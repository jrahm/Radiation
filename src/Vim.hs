{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-} {- The module which contains the Vim monad. This monad
 - is a state monad which handles the connection
 - to the Vim client -}
module Vim(
      VimM(..)
    , runVimM, query, log, logs
    , vlog, queryDefault
    , debug, info, Vim.error, warn
    , fatal, post
    , openLogFile
    , openLogFilePortable
    , setLogLevel, tempFolder
    , LogLevel(..), Variable
)
where
    
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad hiding (forM_)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Monoid (mappend, mconcat)
import Data.ByteString (ByteString)
import Data.Char

import My.Utils
import Data.Convertible (convert, Convertible(..))

import Data.Foldable (forM_, asum)
import Data.Hash.MD5
import Data.Maybe
import Data.String
import Data.String.Utils
import Prelude hiding (log)
import System.Environment
import System.FilePath ((</>))
import System.IO
import System.Process
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map

type Variable = String
type VariableMap = Map.Map Variable String

{- Log levels to help with debugging -}
data LogLevel = Debug | Info | Warning | Error | Fatal deriving (Show,Enum,Ord,Eq)

{- Read a log level from a string -}
readLogLevel :: String -> LogLevel
readLogLevel str | str == "0" || lstr == "debug" = Debug
                   | str == "1" || lstr == "info" = Info
                   | str == "2" || lstr == "warning" = Warning
                   | str == "3" || lstr == "error" = Error
                   | str == "4" || lstr == "fatal" = Fatal
                   | otherwise = Fatal
                where lstr = map toLower str

{- Turn a log level into a letter -}
letter :: LogLevel -> String
letter err = case err of 
    Debug -> "d"
    Info -> "i"
    Warning -> "w"
    Error -> "e"
    Fatal -> "f"

color :: LogLevel -> ByteString
color ll = (case ll of
    Debug -> "\ESC[1;34m["
    Info -> "\ESC[1;37m"
    Warning -> "\ESC[1;33m"
    Error -> "\ESC[1;31m"
    Fatal -> "\ESC[1;31m") +>+ "[" +>+ BSC.pack (show ll) +>+ "] - "

{- This data type gets changed throughout the
 - program as the state of the vim connection
 - changes -}
data VimData = VimData {
      {- The handle to the log file and the associated
       - log level with that handle -}
      logHandle :: Maybe (Handle, LogLevel)

      {- The cached variables which have been already
       - retrieved from the Vim client -}
    , _cachedVariables :: Map.Map String String

      {- A buffer filled with the commands to run
       - once the parser has completed -}
    , commandBuffer :: ByteString

      {- the log level to display in Vim itself -}
    , _vimLogLevel :: Maybe LogLevel
}

dumpCommands :: FilePath -> VimData -> IO ()
dumpCommands fp vd = do
    tmpdir <- tempFolder
    let filename = tmpdir </> (printf "radiation_%s_x.vim" $ md5s $ Str fp) 
    putStrLn $ "\" Writing commands to the file: " ++ filename
    withFilePortable filename WriteMode $
        flip BS.hPutStr (mconcat [commandBuffer vd, "\nredraw!"])
    

{- The vim state monad. The central function is the ability to take a
 - connection to the Vim client (DataPipe) and data and return
 - a mutated version of said data with a variable -}
data VimM a = VimM (VariableMap -> VimData -> IO (VimData, a))

{- Overloading some instances of VimM -}
instance Functor VimM where
    fmap f (VimM func) = VimM $ \dp vd -> do
        (vd1, a) <- func dp vd
        return (vd1, f a)

instance Monad VimM where
    return a = VimM  $ \_ vd -> return (vd, a)

    (VimM ele) >>= f =
        VimM $ \dp vd -> do
            (nd, a) <- ele dp vd
            let (VimM func) = f a
            (dp1, x) <- func dp nd
            return (dp1, x)

{- VimM is a special type of IO monad -}
instance MonadIO VimM where
    liftIO a = VimM $ \_ vimdata -> (vimdata,) <$> a

instance Applicative VimM where
    pure = return
    (<*>) = ap

{- Return the log level which is being used for
 - the Vim client -}
getLogLevel :: VimM LogLevel
getLogLevel = VimM $ \dp vd@(VimData lh cache combuf ll) ->
    case ll of
        Just _l -> return (vd, _l)
        _ -> 
         fmap (VimData lh cache combuf &&& fromJust) $
            (<|>Just Error) <$>
                (fmap readLogLevel <$> rVim (query "g:radiation_log_level") dp vd) 

    where rVim :: VimM a -> VariableMap -> VimData -> IO a
          rVim (VimM f) dp vd = snd <$> f dp vd

nullFile :: IO Handle
nullFile =
#ifdef mingw32_HOST_OS
    openFile "NUL" WriteMode
#else
    openFile "/dev/null" WriteMode
#endif

{- Open a log file with a certain log
 - level. All log statements will be routed to
 - this file -}
openLogFile :: FilePath -> LogLevel -> VimM ()
openLogFile file ll = VimM $ \_ (VimData _ a b c) -> do
    fileh <- catchError (openFile file WriteMode) (const nullFile)
    return (VimData (Just (fileh,ll)) a b c, ())

tempFolder :: IO FilePath
tempFolder = 
#ifdef mingw32_HOST_OS
    (</>"radiation/") <$> getEnv "TEMP"
#else
    ("/tmp/radiation"</>) <$> getEnv "USER"
#endif

openLogFilePortable :: FilePath -> LogLevel -> VimM ()
openLogFilePortable file ll = do
    tmpDir <- liftIO tempFolder
    openLogFile (tmpDir </> file) ll

setLogLevel :: LogLevel -> VimM ()
setLogLevel ll = VimM $ \_ (VimData m a b c) ->
    let ret = m >>= \(h,_) -> return (h,ll) in
    return (VimData ret a b c, ())

{- Run a Vim monad. Must specify the
 - type of connection to use to the vim
 - server. -}
runVimM :: VariableMap -> FilePath -> (FilePath -> VimM a) -> IO a
runVimM variableMap filename fn = do
    let (VimM func) = fn filename

    (dat, ret) <- func variableMap emptyData
    dumpCommands filename dat
    return ret

getEnvSafe :: String -> IO (Maybe String)
getEnvSafe str = catch (Just <$> getEnv str) $ \e -> do
                    _ <- return (e :: SomeException)
                    return Nothing

{- Look up the value of a variable in Vim.
 - Will return Just if the variable exists,
 - or Nothing otherwise. This will keep track
 - of a cache to minimize calls to the vim server. -}
query :: String -> VimM (Maybe String)
query str = VimM  $ \varMap vd@(VimData lh cache combuf ll) -> case str of
    ('$':env') -> 
        logVimData Debug vd "Reading environment variable" >>
        (vd,) <$> liftIO (getEnvSafe env')
    _ -> do
        logVimData Debug vd $ "looking up variable " +>+ BSC.pack str
        return (vd, Map.lookup str varMap)

{- Like above, but specify a default value in case the variable
 - does net extist -}
queryDefault :: String -> String -> VimM String
queryDefault str def = fromMaybe def <$> query str


{- Log to the handle -}
logToHandle :: LogLevel -> ByteString -> VimM ()
logToHandle level str = VimM $ \_ vimdata -> do
    logVimData level vimdata str
    return (vimdata, ())

{- Log with VimData to log with returning value of
 - IO and not VimM -}
logVimData :: LogLevel -> VimData -> ByteString -> IO ()
logVimData level vimdata str =
    forM_ (logHandle vimdata) $ \(handle, ll) ->
        when ( level >= ll ) $ 
            BSC.hPutStrLn handle (color level +>+ str)  >>
            hFlush handle
    

{- Log to both the log file and Vim -}
log :: LogLevel -> ByteString -> VimM ()
log level str = do
    vimlevel <- getLogLevel
    logToHandle level str

logs :: LogLevel -> String -> VimM ()
logs l = log l . BSC.pack

{- Synonym for log -}
vlog :: LogLevel -> ByteString -> VimM ()
vlog = log

{- Post a command to the Vim client. -}
post :: (Convertible a ByteString) => a -> VimM ()
post str = VimM $ \_ (VimData lh cv commandBuffer ll) ->
    let commandBuffer' = mconcat [commandBuffer, "\n", convert str] in
    return (VimData lh cv commandBuffer' ll, ())

postStr :: String -> VimM ()
postStr =  post . Str

{- Synonyms for common log functions -}
debug :: String -> VimM ()
debug = log Debug . BSC.pack

info :: String -> VimM ()
info = log Info . BSC.pack

warn :: String -> VimM ()
warn = log Warning . BSC.pack

error :: String -> VimM ()
error = log Error . BSC.pack

fatal :: String -> VimM ()
fatal = log Fatal . BSC.pack

{- No data yet -}
emptyData :: VimData
emptyData = VimData Nothing Map.empty BS.empty Nothing

withFilePortable :: FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
withFilePortable path mode fn = do
    tmpDir <- liftIO tempFolder
    withFile (tmpDir </> path) mode fn


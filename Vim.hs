{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

{- The module which contains the Vim monad. This monad
 - is a state monad which handles the connection
 - to the Vim client -}
module Vim(
      VimM(..)
    , runVimM, query, log
    , vlog, queryDefault
    , debug, info, Vim.error, warn
    , fatal, post, openSocketDataPipe
    , openServerDataPipe, openLogFile
    , setLogLevel, VimSocketAddress
    , LogLevel(..), DataPipe
)
where
    
import Control.Monad.IO.Class
import Control.Monad hiding (forM_)

import System.Process
import Data.String

import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.String.Utils

import Data.Foldable (forM_)

import Prelude hiding (log)
import Text.Printf
import My.Utils


type VimServerAddress = String
type VimSocketAddress = String

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

color :: LogLevel -> String
color ll = case ll of
    Debug -> "\ESC[1;34m"
    Info -> "\ESC[1;37m"
    Warning -> "\ESC[1;33m"
    Error -> "\ESC[1;31m"
    Fatal -> "\ESC[1;31m"

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

{- Abstraction of a connection to the server. Comes
 - in 2 flavors: Asyncronous and Synchronous -}
data DataPipe = DataPipe {
     {- Queries Vim for the value of a variable.
      - If that variable does not exist, the empty
      - string is returned -}
     queryVariable :: String -> IO (Maybe String)

      {- Evaluate an expression in vim and return the
       - result -}
    , evaluateExpression :: String -> IO String

     {- Post an error to Vim -}
    , postError :: LogLevel -> String -> IO ()

      {- Post a command to Vim. Do not expect the
       - result to occur right away -}
    , postCommand_ :: VimData -> String -> IO VimData

      {- Execute all the commands -}
    , flushCommands_ :: VimData -> IO ()
}

{- The vim state monad. The central function is the ability to take a
 - connection to the Vim client (DataPipe) and data and return
 - a mutated version of said data with a variable -}
data VimM a = VimM (DataPipe -> VimData -> IO (VimData, a))

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

    where rVim :: VimM a -> DataPipe -> VimData -> IO a
          rVim (VimM f) dp vd = snd <$> f dp vd

{- Open a log file with a certain log
 - level. All log statements will be routed to
 - this file -}
openLogFile :: FilePath -> LogLevel -> VimM ()
openLogFile file ll = VimM $ \_ (VimData _ a b c) -> do
    fileh <- openFile file WriteMode
    return (VimData (Just (fileh,ll)) a b c, ())

setLogLevel :: LogLevel -> VimM ()
setLogLevel ll = VimM $ \_ (VimData m a b c) ->
    let ret = m >>= \(h,_) -> return (h,ll) in
    return (VimData ret a b c, ())

{- Run a Vim monad. Must specify the
 - type of connection to use to the vim
 - server. -}
runVimM :: DataPipe -> VimM a -> IO a
runVimM pipe (VimM func) = do
    (dat@(VimData may _ _ _), ret) <- func pipe emptyData
    forM_ may $ hFlush . fst
    flushCommands_ pipe dat
    return ret


{- Look up the value of a variable in Vim.
 - Will return Just if the variable exists,
 - or Nothing otherwise. This will keep track
 - of a cache to minimize calls to the vim server. -}
query :: String -> VimM (Maybe String)
query str = VimM  $ \dp vd@(VimData lh cache combuf ll) -> do
    logVimData Debug vd $ "looking up variable " ++ str
    case Map.lookup str cache of
        Just x -> logVimData Debug vd ("cached result found: " ++ x) $> (vd, Just x)
        Nothing -> do
            val' <- queryVariable dp str

            maybe ( logVimData Debug vd "No result returned" $>
                     (vd, Nothing) )

                (\val -> let nm = Map.insert str val cache in
                         logVimData Debug vd ("result " ++ val) $>
                         (VimData lh nm combuf ll, Just val))
                val'

{- Like above, but specify a default value in case the variable
 - does net extist -}
queryDefault :: String -> String -> VimM String
queryDefault str def = fromJust . (<|>Just def) <$> query str

{- Log to vim directly -}
log' :: LogLevel -> String -> VimM ()
log' level str = VimM  $ \datapipe vimdata ->
        postError datapipe level str >> return (vimdata,())


{- Log to the handle -}
logToHandle :: LogLevel -> String -> VimM ()
logToHandle level str = VimM $ \_ vimdata -> do
    logVimData level vimdata str
    return (vimdata, ())

{- Log with VimData to log with returning value of
 - IO and not VimM -}
logVimData :: LogLevel -> VimData -> String -> IO ()
logVimData level vimdata str =
    forM_ (logHandle vimdata) $ \(handle, ll) ->
        when ( level >= ll ) $ 
            hPutStrLn handle (color level ++ "[" ++ show level ++ "] - " ++ str)  >>
            hFlush handle
    

{- Log to both the log file and Vim -}
log :: LogLevel -> String -> VimM ()
log level str = do
    vimlevel <- getLogLevel
    logToHandle level str
    when (vimlevel <= level) $
        log' level str

{- Synonym for log -}
vlog :: LogLevel -> String -> VimM ()
vlog = log

{- Post a command to the Vim client. -}
post :: String -> VimM ()
post str = VimM $ \datapipe vimdata ->
    (,()) <$> postCommand_ datapipe vimdata str

{- Synonyms for common log functions -}
debug :: String -> VimM ()
debug = log Debug

info :: String -> VimM ()
info = log Info

warn :: String -> VimM ()
warn = log Warning

error :: String -> VimM ()
error = log Error

fatal :: String -> VimM ()
fatal = log Fatal

{- No data yet -}
emptyData :: VimData
emptyData = VimData Nothing Map.empty BS.empty Nothing

{- Create a new synchronous based connection over
 - stdin and stdout -}
openSocketDataPipe :: Handle -> Handle -> DataPipe
openSocketDataPipe input output = DataPipe {
    queryVariable = \str -> hPutStrLn input ("q:" ++ str) >> hFlush input >> fmap tos (hGetLine output),
    evaluateExpression = \str -> hPutStrLn input ("e:" ++ str) >> hFlush input >> hGetLine output,
    postCommand_ = \dp str -> hPutStrLn input ("c:" ++ str) >> hFlush input >> return dp,
    postError = \le str -> hPutStrLn input ("p:" ++ letter le ++ ":" ++ str),
    flushCommands_ = \_ -> return ()
} where tos str = if not $ null str then Just $ tail str else Nothing

runExpression' :: VimServerAddress -> String -> IO String
runExpression' addr expr = 
    (>>=) (runInteractiveProcess "/usr/bin/vim"
        ["/usr/bin/vim", "--servername", addr, "--remote-expr", expr] Nothing Nothing) $
            \(_,stout,_,_) -> hGetContents stout

sendKeys :: VimServerAddress -> ByteString -> IO ()
sendKeys addr keys =
    void $ runProcess "/usr/bin/vim" ["/usr/bin/vim", "--servername", addr, "--remote-send", '\x1b' : BSC.unpack keys] Nothing Nothing Nothing Nothing Nothing 

-- sendKeysLn :: VimServerAddress -> ByteString -> IO ()
-- sendKeysLn addr keys = sendKeys addr $ keys `BS.append` "<CR>"
-- 
-- sendCommand :: VimServerAddress -> ByteString -> IO ()
-- sendCommand addr keys = sendKeysLn addr $ ":" `BS.append` keys

{- Open connection to the Vim client via asynchronous
 - callback -}
openServerDataPipe :: VimServerAddress -> DataPipe
openServerDataPipe addr =
    DataPipe {
        queryVariable = \var ->
            fmap (tos . strip) $ runExpression' addr $ printf "exists('%s') ? '.' . %s : ''" var var,

        evaluateExpression = runExpression' addr,

        postCommand_ = \dp str -> 
            let (VimData _log' cache buf ll) = dp in
            return (VimData _log' cache (buf `BS.append` BSC.pack (str++"\n")) ll),

        flushCommands_ = (withFile "/tmp/radiationx.vim" WriteMode . flip BS.hPutStr . (`BS.append`"redraw!") . commandBuffer)
                            >&> const (sendKeys addr ":so /tmp/radiationx.vim<CR>"),

        postError = \_le _str -> return () -- sendCommand addr $ fromString $ "echoerr '" ++ show le ++ ": " ++ str

    } where tos str = if not $ null str then Just $ tail str else Nothing
            (>&>) f g b = f b >> g b

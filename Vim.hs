{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Vim where
    
import Control.Monad.IO.Class
import Control.Monad hiding (forM_)

import System.Process
import Data.String

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Maybe (fromJust,isJust)
import Control.Applicative
import Data.Char

import Data.Foldable (forM_)

import Prelude hiding (log)
import Text.Printf


type VimServerAddress = String
type VimSocketAddress = String

data LogLevel = Debug | Info | Warning | Error | Fatal deriving (Show,Enum,Ord,Eq)

readLogLevel :: String -> LogLevel
readLogLevel str | str == "0" || lstr == "debug" = Debug
                   | str == "1" || lstr == "info" = Info
                   | str == "2" || lstr == "warning" = Warning
                   | str == "3" || lstr == "error" = Error
                   | str == "4" || lstr == "fatal" = Fatal
                   | otherwise = Fatal
                where lstr = map toLower str

letter :: LogLevel -> String
letter err = case err of 
    Debug -> "d"
    Info -> "i"
    Warning -> "w"
    Error -> "e"
    Fatal -> "f"



data VimData = VimData {
      logHandle :: Maybe (Handle, LogLevel)
    , cachedVariables :: Map.Map String String
    , commandBuffer :: ByteString
}
data DataPipe = DataPipe {
    {- Queries Vim for the value of a variable.
     - If that variable does not exist, the empty
     - string is returned -}
      queryVariable :: String -> IO (Maybe String)
    , evaluateExpression :: String -> IO String
    , postError :: LogLevel -> String -> IO ()
    , postCommand_ :: VimData -> String -> IO VimData
    , flushCommands_ :: VimData -> IO ()
}

data VimM a = VimM (DataPipe -> VimData -> IO (VimData, a))

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

instance MonadIO VimM where
    liftIO a = VimM $ \_ vimdata -> a >>= return . (vimdata,)

openLogFile :: FilePath -> LogLevel -> VimM ()
openLogFile file ll = VimM $ \_ (VimData _ a b) -> do
    fileh <- openFile file WriteMode
    return (VimData (Just (fileh,ll)) a b, ())

setLogLevel :: LogLevel -> VimM ()
setLogLevel ll = VimM $ \_ (VimData m a b) ->
    let ret = m >>= \(h,_) -> return (h,ll) in
    return (VimData ret a b, ())

runVimM :: DataPipe -> VimM a -> IO a
runVimM pipe (VimM func) = do
    (VimData may _ _, ret) <- func pipe emptyData
    forM_ may $ hFlush . fst
    return ret


query :: String -> VimM (Maybe String)
query str = VimM  $ \dp vd@(VimData lh cache combuf) ->
    case Map.lookup str cache of
        Just x -> return (vd, Just x)
        Nothing -> do
            val' <- queryVariable dp str

            maybe (return (vd, Nothing)) (\val -> 
                let nm = Map.insert str val cache in
                return (VimData lh nm combuf, Just val)) val'

queryDefault :: String -> String -> VimM String
queryDefault str def = fmap (fromJust . (<|>Just def)) (query str)

log' :: LogLevel -> String -> VimM ()
log' level str = VimM  $ \datapipe vimdata -> do
        postError datapipe level str >> return (vimdata,())

logToHandle :: LogLevel -> String -> VimM ()
logToHandle level str = VimM $ \_ vimdata -> do
    forM_ (logHandle vimdata) $ \(handle, ll) ->
        when ( level >= ll ) $ 
            hPutStrLn handle $ "[" ++ show level ++ "] - " ++ str 
    return (vimdata, ())
    

log :: LogLevel -> String -> VimM ()
log level str = do
    vimlevel <- queryDefault "g:radiation_log_level" "fatal"
    logToHandle level str
    when (readLogLevel vimlevel <= level) $
        log' level str

vlog :: LogLevel -> String -> VimM ()
vlog = log
    
post :: String -> VimM ()
post str = VimM $ \datapipe vimdata ->
    postCommand_ datapipe vimdata str >>= return . (,())

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

emptyData :: VimData
emptyData = VimData Nothing Map.empty BS.empty

openSocketDataPipe :: Handle -> Handle -> DataPipe
openSocketDataPipe input output = DataPipe {
    queryVariable = \str -> hPutStrLn input ("q:" ++ str) >> hFlush input >> fmap tos (hGetLine output),
    evaluateExpression = \str -> hPutStrLn input ("e:" ++ str) >> hFlush input >> hGetLine output,
    postCommand_ = \dp str -> hPutStrLn input ("c:" ++ str) >> hFlush input >> return dp,
    postError = \le str -> hPutStrLn input ("p:" ++ letter le ++ ":" ++ str),
    flushCommands_ = \_ -> return ()
} where tos str = if length str > 0 then Just $ tail str else Nothing

runExpression' :: VimServerAddress -> String -> IO String
runExpression' addr expr = 
    (>>=) (runInteractiveProcess "/usr/bin/vim"
        ["/usr/bin/vim", "--servername", addr, "--remote-expr", expr] Nothing Nothing) $
            \(_,stout,_,_) -> hGetContents stout

sendKeys :: VimServerAddress -> ByteString -> IO ()
sendKeys addr keys =
    void $ runProcess "/usr/bin/vim" ["/usr/bin/vim", "--servername", addr, "--remote-send", '\x1b' : BSC.unpack keys] Nothing Nothing Nothing Nothing Nothing 

sendKeysLn :: VimServerAddress -> ByteString -> IO ()
sendKeysLn addr keys = sendKeys addr $ keys `BS.append` "<CR>"

sendCommand :: VimServerAddress -> ByteString -> IO ()
sendCommand addr keys = sendKeysLn addr $ ":" `BS.append` keys

openServerDataPipe :: VimServerAddress -> DataPipe
openServerDataPipe addr =
    DataPipe {
        queryVariable = \var -> fmap tos $ runExpression' addr (printf "exists('%s') ? %s : ''" var),
        evaluateExpression = runExpression' addr,
        postCommand_ = \dp str -> 
            let (VimData log cache buf) = dp in
            return (VimData log cache $ buf `BS.append` BSC.pack (":" ++ str ++ "<CR>")),
        flushCommands_ = \dat -> sendKeys addr (commandBuffer dat),
        postError = \le str -> sendCommand addr $ fromString $ "echoerr '" ++ show le ++ ": " ++ str
    } where tos str = if length str > 0 then Just $ tail str else Nothing

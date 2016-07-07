{-# LANGUAGE RecordWildCards, GADTs, ScopedTypeVariables, RankNTypes, CPP #-}
module Main (main) where

import GHCi.Run
import GHCi.TH
import GHCi.Message
import GHCi.Signals
import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.FD as FD

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import Data.IORef
import System.Environment
import System.Exit
import System.IO
import Text.Printf

main :: IO ()
main = do
  (wFifo:rFifo:rest) <- getArgs
  verbose <- case rest of
    ["-v"] -> return True
    []     -> return False
    _      -> die "iserv: syntax: iserv <write-fifo> <read-fifo> [-v]"
  when verbose $ do
    printf "GHC iserv starting (in: %s; out: %s)\n" rFifo wFifo
  installSignalHandlers
#if defined(mingw32_HOST_OS)
  -- When cross-compiling we need to preload these DLLs since base depends
  -- on symbols defined in them.
  -- Interestingly, running ghc-iserv.exe with "+RTS -Dl" shows that ghc
  -- sends FindSystemLibrary requests looking for them but for some reason
  -- never sends LoadDLL requests for them.
  mapM_ (run . LoadDLL)
    [ "shell32"   -- for _CommandLineToArgvW
    , "wsock32"   -- for _recv
    ]
#endif
  -- The order in which we open the pipes must be the same at the other end or
  -- we'll deadlock
  inh <- openBinaryFile rFifo ReadMode
  hSetBuffering inh NoBuffering

  (oFd,oDt) <- FD.openFile wFifo WriteMode False -- open in blocking mode
  outh <- mkHandleFromFD oFd oDt wFifo WriteMode True Nothing
  -- set to non-blocking afterwards -------------^
  hSetBuffering outh NoBuffering

  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
  uninterruptibleMask $ serv verbose pipe
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.

serv :: Bool -> Pipe -> (forall a .IO a -> IO a) -> IO ()
serv verbose pipe@Pipe{..} restore = loop
 where
  loop = do
    Msg msg <- readPipe pipe getMessage
    discardCtrlC
    when verbose $ putStrLn ("iserv: " ++ show msg)
    case msg of
      Shutdown -> return ()
      RunTH st q ty loc -> wrapRunTH $ runTH pipe st q ty loc
      FinishTH st -> wrapRunTH $ finishTH pipe st
      _other -> run msg >>= reply

  reply :: forall a. (Binary a, Show a) => a -> IO ()
  reply r = do
    when verbose $ putStrLn ("iserv: return: " ++ show r)
    writePipe pipe (put r)
    loop

  wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
  wrapRunTH io = do
    r <- try io
    case r of
      Left e
        | Just (GHCiQException _ err) <- fromException e  -> do
           when verbose $ putStrLn "iserv: QFail"
           writePipe pipe (putMessage (QFail err))
           loop
        | otherwise -> do
           when verbose $ putStrLn "iserv: QException"
           str <- showException e
           writePipe pipe (putMessage (QException str))
           loop
      Right a -> do
        when verbose $ putStrLn "iserv: QDone"
        writePipe pipe (putMessage QDone)
        reply a

  -- carefully when showing an exception, there might be other exceptions
  -- lurking inside it.  If so, we return the inner exception instead.
  showException :: SomeException -> IO String
  showException e0 = do
     r <- try $ evaluate (force (show (e0::SomeException)))
     case r of
       Left e -> showException e
       Right str -> return str

  -- throw away any pending ^C exceptions while we're not running
  -- interpreted code.  GHC will also get the ^C, and either ignore it
  -- (if this is GHCi), or tell us to quit with a Shutdown message.
  discardCtrlC = do
    r <- try $ restore $ return ()
    case r of
      Left UserInterrupt -> return () >> discardCtrlC
      Left e -> throwIO e
      _ -> return ()

module Main where

{-    LECTURE 20 : CONCURRENCY -}

import Control.Concurrent
import Control.Monad (forever, forM_)
import Network
import System.IO
import Text.Printf



-- forkIO :: IO () -> IO ThreadId

muddle :: IO ()
muddle = do
  hSetBuffering stdout NoBuffering
  forkIO (forM_ [1..1000] (\_ -> putChar ' '))
  forM_ [1..1000] (\_ -> putChar 'B')



-- threadDelay :: Int -> IO ()

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)
  printf "REMINDER!!! %d seconds are up\a\a\a\n" t

reminderMain :: IO ()
reminderMain = loop
  where
    loop = do
      s <- getLine
      if s == "end" then return ()
        else do setReminder s
                loop



{-   PART II : MVARS -}

{- interface

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()

newIORef :: a -> IO (IORef a)
getIORef :: IORef a -> IO a
putIORef :: IORef a -> a -> IO ()

-}

mvar1 = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r
  
  


mvar2 = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'
              -- putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
 
  

-- Logger example

{-
data Logger

initLogger :: IO Logger
logMessage :: Logger -> String -> IO ()
logStop    :: Logger -> IO ()

-}

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l
  
logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      threadDelay (10^6 * 1)
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "Stopping logger"
          putMVar s ()
          

logStop :: Logger -> () -> IO ()
logStop (Logger m) () = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

loggerMain :: IO ()
loggerMain = do
  l <- initLogger
  l `logMessage` "hello"
  putStrLn "We didn't wait for the log message"
  l `logMessage` "bye"
  l `logStop` ()
  putStrLn "End of program"

















----------------------------------------------------------------------
-- A server

talk :: Handle -> MVar CountingMsg -> IO ()
talk h m =
  do hSetBuffering h LineBuffering
     loop
  where
    loop =
      do line <- hGetLine h
         case line of
           "end" -> hPutStrLn h "Bye!"
           "count" -> do
             r <- newEmptyMVar
             putMVar m (GetCount r)
             c <- takeMVar r
             hPutStrLn h ("Count is " ++ show c)
             loop
           line -> do
             hPutStrLn h (show (2 * read line :: Integer))
             putMVar m Inc
             loop

data CountingMsg = Inc | GetCount (MVar Int)

countingThread :: MVar CountingMsg -> IO ()
countingThread m = loop (0 :: Int)
  where
    loop c = do
      cmd <- takeMVar m
      case cmd of
        Inc -> do
          printf "New doubling served! %d doublings so far!\n" (c+1)
          loop (c+1)
        GetCount r -> do
          putMVar r c
          loop c

main = do
  sock <- listenOn (PortNumber 1234)
  printf "Listening..."
  m <- newEmptyMVar
  forkIO (countingThread m)
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle m) (\_ -> hClose handle)


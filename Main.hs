module Main where

import qualified Sound.JACK.MIDI as MIDI
import Sound.JACK (NFrames(NFrames), )

import qualified Sound.MIDI.Message as Msg

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception
import Data.Typeable
import Data.IORef

import Core

type NoteState = State Int

updateNote :: Int -> NoteState Int
updateNote pitch = return pitch

stateioact :: StateT Int IO ()
stateioact = do
  x <- get
  liftIO $ putStrLn ("stateioact called, with state as " ++ show x)
  put (x+1)

main :: IO ()
main = evalStateT mainAction 42

mainAction :: StateT Int IO ()
mainAction = embedIO $ \x -> MIDI.main (makeCallback2 stateioact x)

-- the glue which makes it work:
embedIO :: (IORef s -> IO a) -> StateT s IO a
embedIO a = do s <- get
               x <-  liftIO $ newIORef s
               r <-  liftIO $ a x
               s' <- liftIO $ readIORef x
               put s'
               return r

makeCallback2 :: StateT s IO a -> IORef s -> NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
makeCallback2 act x (NFrames cycleStart) (tf@(NFrames t), e) = do 
                        putStrLn $ "Time: " ++ show (cycleStart + t) ++ " " ++
                           case e of
                              Msg.Channel b -> "MidiMsg.Channel " ++ show b
                              Msg.System  _ -> "MidiMsg.System ..."
                        putStrLn $ "State: " ++ show ((evalState $ updateNote 3) 99)
                        s <- readIORef x
                        (res,s') <- runStateT act s
                        writeIORef x s'
                        return (tf, e)


makeCallback :: StateT s IO a -> IORef s -> IO a
makeCallback act x = do s <- readIORef x
                        (res,s') <- runStateT act s
                        writeIORef x s'
                        return res



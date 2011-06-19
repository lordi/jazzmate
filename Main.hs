module Main where

import qualified Sound.JACK.MIDI as MIDI
import Sound.JACK (NFrames(NFrames), )

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as Ch
import qualified Sound.MIDI.Message.Channel.Voice as V

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception
import Data.Typeable
import Data.IORef
import qualified Data.List as L

import Core

type NoteCollectionT = StateT [Key] IO ()

fromPitch :: V.Pitch -> Key
fromPitch p = toEnum ((V.fromPitch p) `mod` octave)

stateioact2 :: Msg.T -> NoteCollectionT
stateioact2 e = do
    x <- get
    let y = case e of
            Msg.Channel Ch.Cons {Ch.messageBody = (Ch.Voice (V.NoteOn pitch _))} -> ((fromPitch pitch):x)
            Msg.Channel Ch.Cons {Ch.messageBody = (Ch.Voice (V.NoteOff pitch _))} -> L.delete (fromPitch pitch) x
            _ -> x 
    liftIO $ putStrLn ("\n" ++ show y ++ "\n" ++ show (intervals y) ++ "\n" ++ show (matchingChords y))
    liftIO $ ppkeys (Notes y)
    put y

main :: IO ()
main = evalStateT mainAction []

mainAction :: NoteCollectionT
mainAction = embedIO $ \x -> MIDI.main (makeCallback2 stateioact2 x)

-- the glue which makes it work:
embedIO :: (IORef s -> IO a) -> StateT s IO a
embedIO a = do s <- get
               x <-  liftIO $ newIORef s
               r <-  liftIO $ a x
               s' <- liftIO $ readIORef x
               put s'
               return r

makeCallback2 :: (Msg.T -> NoteCollectionT) -> IORef [Key] -> NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
makeCallback2 act x (NFrames cycleStart) (tf@(NFrames t), e) = do 
                        s <- readIORef x
                        (_,sy) <- runStateT (act e) s
                        writeIORef x sy
                        return (tf, e)


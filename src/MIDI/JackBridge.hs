module MIDI.JackBridge where

import qualified Data.List as L

import Control.Concurrent
import Control.Concurrent.Chan

import qualified Sound.JACK.MIDI as MIDI
import qualified Sound.MIDI.Message as Msg
import Sound.JACK (NFrames(NFrames), )

run :: IO (Chan Msg.T)
run = do ch <- newChan
         forkIO $ MIDI.main (writeMsg ch)
         return ch

writeMsg :: (Chan Msg.T) -> NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
writeMsg ch _ (tf@(NFrames t), e) = do writeChan ch e; return (tf, e)


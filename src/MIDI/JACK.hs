module MIDI.JACK (run, ) where

import Control.Concurrent.STM.TChan
import Control.Monad.STM

import qualified Sound.JACK.MIDI as MIDI
import qualified Sound.MIDI.Message as Msg
import Sound.JACK (NFrames(NFrames), )

run :: TChan Msg.T -> IO ()
run noteCh = MIDI.main (writeMsg noteCh)

writeMsg :: TChan Msg.T -> NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
writeMsg c _ (tf@(NFrames t), e) = do atomically $ writeTChan c e; return (tf, e)


module MIDI.Dummy where

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Chan

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Core

run :: IO (Chan Msg.T)
run = do ch <- newChan
         forkIO $ forever (sendMIDIsequence (writeChan ch))
         return ch

keyon chan vel key = (Msg.Channel (ChannelMsg.Cons chan voice))
    where voice = ChannelMsg.Voice (VoiceMsg.NoteOn pitch vel)
          pitch = toPitch key

keyoff chan vel key = (Msg.Channel (ChannelMsg.Cons chan voice))
    where voice = ChannelMsg.Voice (VoiceMsg.NoteOff pitch vel)
          pitch = toPitch key

sendMIDIsequence send = do
                        let chan   = ChannelMsg.toChannel 3
                            vel    = VoiceMsg.toVelocity 64
                            secs s = s * 1000000
                        threadDelay $ secs 1
                        send $ keyon chan vel C
                        threadDelay $ secs 1
                        send $ keyon chan vel Eb
                        threadDelay $ secs 1
                        send $ keyon chan vel G
                        threadDelay $ secs 3
                        send $ keyoff chan vel C
                        send $ keyoff chan vel Eb
                        send $ keyoff chan vel G
                        send $ keyon chan vel D
                        threadDelay $ secs 1
                        send $ keyon chan vel G
                        threadDelay $ secs 1
                        send $ keyon chan vel B
                        threadDelay $ secs 3
                        send $ keyoff chan vel D
                        send $ keyoff chan vel G
                        send $ keyoff chan vel B

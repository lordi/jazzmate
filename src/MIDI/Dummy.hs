module MIDI.Dummy where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Random

import Core

run :: TChan Msg.T -> IO ()
run noteChan = forever $ sendRandomChord noteChan

keyon chan vel key = Msg.Channel (ChannelMsg.Cons chan voice)
    where voice = ChannelMsg.Voice (VoiceMsg.NoteOn pitch vel)
          pitch = toPitch key

keyoff chan vel key = Msg.Channel (ChannelMsg.Cons chan voice)
    where voice = ChannelMsg.Voice (VoiceMsg.NoteOff pitch vel)
          pitch = toPitch key

pick :: [a] -> IO a
pick xs = do
    r <- randomRIO (0, length xs - 1)
    return $ xs !! r

sendRandomChord ch = do
    let chan   = ChannelMsg.toChannel 1
        vel    = VoiceMsg.toVelocity 64
        secs s = s * 100000
        on key = do threadDelay (secs 1); send $ keyon chan vel key; return ()
        off key = do send $ keyoff chan vel key; return ()
        send = atomically . writeTChan ch
    key <- pick [minBound .. maxBound]
    ctype <- pick [minBound .. maxBound]
    mapM on $ notes (Chord ctype key)
    threadDelay (secs 2)
    mapM off $ notes (Chord ctype key)


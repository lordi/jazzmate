module MIDI.Dummy (run, ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
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
    let chan    = ChannelMsg.toChannel 1
        vel     = VoiceMsg.toVelocity 64
        on n    = do wait 1; send $ keyon chan vel n; return ()
        off n   = do send $ keyoff chan vel n; return ()
        send    = atomically . writeTChan ch
        wait s  = threadDelay (s * 1000000)
    key <- pick [minBound .. maxBound]
    chordType <- pick [minBound .. maxBound]
    mapM_ on $ notes (Chord chordType key)
    wait 2
    mapM_ off $ notes (Chord chordType key)


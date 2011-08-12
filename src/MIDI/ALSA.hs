module MIDI.ALSA where

import Control.Concurrent
import Control.Concurrent.Chan
import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as Channel
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V


import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import Sound.MIDI.ALSA
import Control.Monad (forever, void, )

-- | Provides a mapping from a Sound.ALSA.Sequencer.Event to a
-- Sound.MIDI.Message.Msg by converting only the relevant parts
-- (NoteOn/NoteOff).
toMsg :: Event.T -> Maybe Msg.T
toMsg e = case (Event.body e) of
        Event.NoteEv Event.NoteOn n -> Just $ channelMsg V.NoteOn (Event.noteNote n) (Event.noteVelocity n)
        Event.NoteEv Event.NoteOff n -> Just $ channelMsg V.NoteOff (Event.noteNote n) (Event.noteVelocity n)
        _ -> Nothing
        where channelMsg ev note vel = Msg.Channel Cons {
            Channel.messageChannel = toChannel 1, 
            messageBody = Voice (ev (toPitch note) (toVelocity vel))
        }

run :: IO (Chan Msg.T)
run = do
  ch <- newChan
  _ <- forkIO $ setup $ writeChan ch
  return ch

setup :: (Msg.T -> IO ()) -> IO ()
setup send = (do
  SndSeq.with SndSeq.defaultName SndSeq.Block $ \h -> do
  Client.setName (h :: SndSeq.T SndSeq.InputMode) "JazzMate"
  Port.withSimple h "Input"
     (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \ _p1 -> do
  forever $ do event <- Event.input h; maybe (return ()) send (toMsg event)
  ) `AlsaExc.catch` \e -> putStrLn $ "ALSA Exception: " ++ AlsaExc.show e


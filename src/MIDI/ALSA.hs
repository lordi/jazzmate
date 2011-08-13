module MIDI.ALSA (run, ) where

import Control.Concurrent.STM.TChan
import Control.Monad.STM

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as Channel
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer as Seq
import qualified Sound.ALSA.Exception as AlsaExc
import Sound.MIDI.ALSA
import Control.Monad (forever, void, )

-- | Provides a mapping from a Sound.ALSA.Sequencer.Event to a
-- Sound.MIDI.Message.Msg by converting only the relevant parts
-- (NoteOn/NoteOff).
toMsg :: E.T -> Maybe Msg.T
toMsg e = case E.body e of
  E.NoteEv E.NoteOn n -> Just $ msg V.NoteOn (E.noteNote n) (E.noteVelocity n)
  E.NoteEv E.NoteOff n -> Just $ msg V.NoteOff (E.noteNote n) (E.noteVelocity n)
  _ -> Nothing
  where msg ev note vel = Msg.Channel Cons {
      Channel.messageChannel = toChannel 1,
      messageBody = Voice (ev (toPitch note) (toVelocity vel))
  }

-- | The main ALSA MIDI code. TODO doc
run :: TChan Msg.T -> IO ()
run noteChan = 
  Seq.with Seq.defaultName Seq.Block (\h -> do
    Client.setName (h :: Seq.T Seq.InputMode) "JazzMate"
    P.withSimple h "Input" (P.caps [P.capWrite, P.capSubsWrite]) P.typeMidiGeneric $ \_ ->
      forever $ do event <- E.input h; maybeSend (toMsg event)
  ) `AlsaExc.catch` \e -> putStrLn $ "ALSA Exception: " ++ AlsaExc.show e
  where maybeSend = maybe (return ()) (atomically . writeTChan noteChan)


module Main where

import qualified Data.List as L

import qualified Sound.JACK.MIDI as MIDI
import Sound.JACK (NFrames(NFrames), )
import qualified Sound.MIDI.Message as Msg
import Sound.MIDI.Message.Channel (T (Cons), messageBody, Body (Voice))
import qualified Sound.MIDI.Message.Channel.Voice as V

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error
import Control.Exception
import Data.Typeable
import Data.IORef

import Core

type KeyCollectionT = StateT [Key] IO ()

fromPitch :: V.Pitch -> Key
fromPitch p = toEnum ((V.fromPitch p) `mod` octave)

transform :: Msg.T -> [Key] -> [Key]
transform (Msg.Channel Cons {messageBody = (Voice (V.NoteOn p _))})
            = (:) $ fromPitch p
transform (Msg.Channel Cons {messageBody = (Voice (V.NoteOff p _))})
            = L.delete $ fromPitch p
transform _ = id

updateColl :: Msg.T -> KeyCollectionT
updateColl e = do x <- get; put $ transform e x; printColl

printColl :: KeyCollectionT
printColl = do
    y <- get
    liftIO $ putStrLn $ show y
    liftIO $ putStrLn $ show (intervals y)
    liftIO $ putStrLn $ show (matchingChords y)
    liftIO $ ppkeys $ Notes y

main :: IO ()
main = evalStateT mainAction []

mainAction :: KeyCollectionT
mainAction = embedIO $ \x -> MIDI.main (makeCallback updateColl x)

-- from http://www.haskell.org/pipermail/haskell-cafe/2007-July/028501.html
-- the glue which makes it work:
embedIO :: (IORef s -> IO a) -> StateT s IO a
embedIO a = do s <- get
               x <-  liftIO $ newIORef s
               r <-  liftIO $ a x
               s' <- liftIO $ readIORef x
               put s'
               return r

makeCallback :: (Msg.T -> KeyCollectionT) -> IORef [Key] ->
                NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)
makeCallback act x _ (tf@(NFrames t), e) = do
                        s <- readIORef x
                        (_,sy) <- runStateT (act e) s
                        writeIORef x sy
                        return (tf, e)


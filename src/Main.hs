module Main (main) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import Data.Maybe( fromMaybe )

import Control.Concurrent.STM.TChan
import qualified Sound.MIDI.Message as Msg

import qualified GUI.Main as GUI
import qualified MIDI.Dummy as Dummy
import GUI.Render (renderCanvas, )
import Core

#ifdef USE_JACK
import qualified MIDI.JACK as JACK
#endif
#ifdef USE_ALSA
import qualified MIDI.ALSA as ALSA
#endif

data Options = Options
    { key :: Maybe Note
    , mode :: Maybe ScaleType
    , midiProvider :: TChan Msg.T -> IO ()
    }

defaultOptions = Options
    { key = Nothing
    , mode =  Nothing
#ifdef USE_ALSA
    , midiProvider = ALSA.run
#else
#ifdef USE_JACK
    , midiProvider = JACK.run
#else
    , midiProvider = Dummy.run
#endif
#endif
    }

header = "Usage: jazzmate [OPTION...]"

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['d'] [] (ReqArg setMIDIProvider "driver")
        "MIDI provider (alsa, jack or dummy)"
    , Option ['k'] [] (ReqArg setKey "key")
        "Key of the scale you will be playing (C,C#,...)"
    , Option ['m'] [] (ReqArg setMode "mode") 
        "Mode of the scale you will be playing (MajorDiatonic)"
    , Option ['v'] [] (NoArg showVersion)
        "Show version number"
    , Option ['h'] [] (NoArg showUsage)
        "Show usage information"
    ]

setMIDIProvider arg opt = case arg of
#ifdef USE_ALSA
                "alsa" -> return opt { midiProvider = ALSA.run }
#endif
#ifdef USE_JACK
                "jack" -> return opt { midiProvider = JACK.run }
#endif
                "dummy" -> return opt { midiProvider = Dummy.run }
                _ -> error $ "unrecognized midi provider: " ++ arg

setKey arg opt = return opt { key = Just $ read arg }
setMode arg opt = return opt { mode = Just $ read arg }

showVersion _ = do
  -- TODO is it possible to retrieve this from .cabal file?
  putStrLn "JazzMate 0.2.4"
  exitWith ExitSuccess

showUsage _ = do
  putStrLn $ usageInfo header options
  exitWith ExitSuccess

main = do
  args <- getArgs
  let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
  case getOpt RequireOrder options args of
    (actions, [],      [])         -> run actions
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options
  where run actions = do
                  opts <- foldl (>>=) (return defaultOptions) actions
                  let Options { key = k, mode = m, midiProvider = midi } = opts
                  let scale = case (k, m) of
                                (Just key, Just mode) -> Just $ Scale key mode
                                _                     -> Nothing
                  GUI.main midi renderCanvas scale


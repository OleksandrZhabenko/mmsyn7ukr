-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a simple 
-- basic interface to some SoX functionality or for producing 
-- the approximately Ukrainian speech with your own recorded 
-- voice (actually it produces the needed sound representations).
--


module Main where

import Paths_mmsyn7ukr
import Processing_mmsyn7ukr
import System.Environment (getArgs)
import SoXBasics (playA)

-- | Function responds for general @mmsyn7ukr@ program execution. 
main :: IO ()
main = do
  args <- getArgs
  let c0 = concat . take 1 $ args
  case c0 of
    "-h" -> do
       putStrLn "mmsyn7ukr SYNOPSIS:"
       putStrLn "mmsyn7ukr -h     OR:"
       putStrLn "mmsyn7ukr -v     OR:"
       putStrLn "mmsyn7ukr -t     OR:"
       putStrLn "mmsyn7ukr [control-parameter-for-levels-of-processment] [[control-parameter-for-truncating-sounds] [[list-of-produced-sound-representations]]]"
       putStrLn ""
       putStr "control-parameter-for-levels-of-processment (if any) -- an integer number in range [-1..3] that defines in ascending order the portion of processment "
       putStrLn "of the resulting sound representations. "
       putStr "control-parameter-for-truncating-sounds (if any) -- an integer number in range [0..3] that defines in ascending order the level for maximum " 
       putStrLn "amplitude that are trimmed for the sound file. "
       putStr "list-of-produced-sound-representations (if any) -- a list of sound representations, which the program will try to produce while being executed. "
       putStrLn "The default one (if not specified) is a full range of needed sound representations. "
    "-v" -> do
       putStrLn "mmsyn7ukr version: 0.17.0.0"
    "-t" -> do
       putStrLn "The program plays now a service sound for a 0.5 second. It uses just that duration for a pause before you can record a sound representation further."
       path0 <- getDataFileName "y.wav"
       playA path0
    _    -> main7ukr args

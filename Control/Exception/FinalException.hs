-- |
-- Module      :  Control.Exception.FinalException
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a simple 
-- basic interface to some SoX functionality or for producing 
-- the approximately Ukrainian speech with your own recorded 
-- voice (actually it produces the needed sound representations).
--

{-# LANGUAGE DeriveDataTypeable #-}

module Control.Exception.FinalException (
  FinalException(..)
  -- * Exception
  , catchEnd
) where

import Data.Typeable
import Control.Exception (Exception, catch, throw)
import System.Environment (getProgName)
import System.IO
import GHC.IO.Handle.Types (Newline( CRLF ), nativeNewline)

-- | Data type 'FinalException' is used to terminate the not needed further execution.
data FinalException = ExecutableNotProperlyInstalled | MaybePartiallyTrimmed | NotCreatedWithEffect String
  | InitialFileNotChanged String | NotCreated String | NotRecorded String | NoiseProfileNotCreatedB String | NoiseProfileNotCreatedE String
    | NotEnoughData String | NotCreatedWithEffects String | StrangeAnswer String String | NotFileNameGiven | DataFileNotClosed String
       | DataSoundFileNotRead String | UndefinedFunction String
          deriving ( Typeable )

instance Exception FinalException

instance Show FinalException where
  show ExecutableNotProperlyInstalled = "ExecutableNotProperlyInstalled: SoX is not properly installed in your system. Please, install it properly and then call the function again." ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show MaybePartiallyTrimmed = "MaybePartiallyTrimmed: The function did not create the needed file, but may be it trimmed the initial one (not enough)!"
    ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NotCreatedWithEffect xs) = "NotCreatedWithEffect: File was not created with " ++ show xs ++ " effect!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (InitialFileNotChanged xs) = "InitialFileNotChanged: The initial file " ++ show xs ++ " was not changed!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NotCreated xs) = "NotCreated: The function did not create the needed file " ++ show xs ++ "!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NotRecorded xs) = "NotRecorded: The file " ++ show xs ++ " was not recorded!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NoiseProfileNotCreatedB xs) = "NoiseProfileNotCreatedB: The noise profile " ++ xs ++ ".b.prof was not created!"
    ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NoiseProfileNotCreatedE xs) = "NoiseProfileNotCreatedE: The noise profile " ++ xs ++ ".e.prof was not created!"
    ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NotEnoughData xs) = "NotEnoughData: SoX cannot determine the number of the samples in the file " ++ show xs ++
    "! May be it is a RAW file and it needs additional parameters to be processed." ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (NotCreatedWithEffects xs) = "NotCreatedWithEffects: File was not created with " ++ (init . unwords . map ((++ ",") . show) . words $ xs) ++ " effects!"
    ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (StrangeAnswer xs ys) = xs ++ ": the " ++ show ys ++ " function gave a strange result!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show NotFileNameGiven = "Please, specify as a command line argument at least a name of the resulting file (without its extension)! "
    ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (DataFileNotClosed xs) = "File " ++ show xs ++ " is not closed!" ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (DataSoundFileNotRead xs) = "Data sound file " ++ show xs ++ " is not read!"  ++ (if nativeNewline == CRLF then "\r\n" else "\n")
  show (UndefinedFunction xs) = xs ++ ": the function is undefined for the arguments. " ++ (if nativeNewline == CRLF then "\r\n" else "\n")

-- | Function to work with exception 'FinalException' similarly to the example in the documentation for the 'catch' function. It throws an exception
-- to the thread where it is called. 
catchEnd :: FinalException -> IO ()
catchEnd e = do
  progName <- getProgName
  catch (throw e) (\e0 -> hPutStr stderr (progName ++ ": " ++ show (e0 :: FinalException)))

-- |
-- Module      :  Processing_mmsyn7ukr
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


module Processing_mmsyn7ukr (
  -- * Main7ukr function
  main7ukr
  -- * Producing sound
  , produceSound
  , produceSound2
  , produceSound3
  , produceSound4
  , beginProcessing
  , controlNoiseReduction
  -- * Additional functions
  , tempS
  , showCoef
  , tempeRa
  -- ** Informational messages printing functions
  , printGenInfo0
  , printGenInfo1
  , printGenInfo2
  , recommendSharp
  -- * Cleaning
  , cleanTemp
  , cleanTempN
) where

import Control.Concurrent (threadDelay)
import Data.Typeable
import Numeric
import System.Directory
import Control.Exception (onException)
import EndOfExe (showE)
import Data.Maybe (fromJust)
import Data.Char
import qualified Data.Vector as V
import System.Process
import System.IO
import System.Info (os)
import System.Environment (getProgName)
import System.Exit
import SoXBasics
import CaseBi (getBFst')
import ReplaceP (replaceP, replaceP4)
import Paths_mmsyn6ukr
import Control.Exception.FinalException

-- | Function that being given a tuple of @String@ and a path to the installed by the @mmsyn6ukr@ package file produces the corresponding analogous sound with your created 
-- voicing. The tuple controls the function behaviour. The first @String@ in it specifies the actions that will be performed to produce a sound file and the second one 
-- specifies a maximum absolute amplitude starting from which the sound will not be truncated if the 'alterVadB' and 'alterVadE' functions must be applied (that is specified 
-- by the first @String@ parameter). 
produceSound :: (String, String) -> FilePath -> IO ()
produceSound (actsctrl, noiseLim) file  = onException (do {
; playA file
; putStrLn "    *****"
; putStrLn "The sound duration is: "
; produceSound2 (file, drop (length file - 5) file) (actsctrl, noiseLim) . getBFst' ("е", V.fromList . zip ["A.wav", "B.wav", "C.wav", "D.wav", 
    "E.wav", "F.wav", "G.wav", "H.wav", "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", "S.wav", 
       "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav", "d.wav", "e.wav", "f.wav"] $ 
          ["а", "б", "в", "г", "д", "дж", "дз", "е", "ж", "з", "и", "й", "к", "л", "м", "н", "о", "п", "р", "с", "сь", "т", "у", "ф", "х", 
             "ц", "ць", "ч", "ш", "ь", "і", "ґ"]) $ drop (length file - 5) file
; return () }) (do
       putStrLn "Something went unexpectedly, please, repeat the procedure again, be attentive to the data you provide as input! "
       putStr "The needed files were NOT created (may be the sound was not at the moment of recording)! The process will be restarted "
       putStrLn "for the sound. Please, produce a sound during the first 3 seconds (after 0.5 second delay or whichever you specify), OR specify greater ratio, or use \'sharp\' mode!"
       cleanTemp
       produceSound (actsctrl, noiseLim) file)

-- | Function 'produceSound3' is used internally in the 'produceSound2' function.
produceSound3 :: (String, String) -> (FilePath, FilePath) -> String -> (Int, Float) -> Float -> IO ()
produceSound3 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0
 | actsctrl == "-1" = prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr
 | take 1 actsctrl == "0" = prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr
 | take 1 actsctrl == "1" = do
     prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
       if lim1 <= 0.0 then return () else resampleA "8_x.wav" (22050::Int) >> produceSound4 (file, file1) "38_x.wav"
 | take 1 actsctrl == "2" = do
    prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
     if lim1 <= 0.0 then return () else sincA "8_x.wav" >> resampleA "4.8_x.wav" (22050::Int) >> produceSound4 (file, file1) "34.8_x.wav"
 | otherwise = do
    prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 >>= \lim1 -> 
     if lim1 <= 0.0 then return () else sincA "8_x.wav" >> resampleA "4.8_x.wav" (22050::Int) >> quarterSinFade "34.8_x.wav" >> 
       produceSound4 (file, file1) "434.8_x.wav"

prodSnd3H :: (String, String) -> (FilePath, FilePath) -> String -> IO ()
prodSnd3H (actsctrl, noiseLim) (file, file1) soundUkr = do 
    lim1 <- durationA "8_x.wav"
    if lim1 <= 0.0
      then beginProcessing (file, file1) soundUkr (actsctrl, noiseLim)
      else do 
        resampleA "8_x.wav" (22050::Int)
        produceSound4 (file, file1)  "38_x.wav"

prodSnd3H2 :: (String, String) -> (FilePath, FilePath) -> String -> (Int, Float) ->  Float -> IO Float
prodSnd3H2 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0 = do
    alterVadB "8_x.wav" lim0 noiseMax (duration0*0.03)
    lim1 <- durationA "8_x.wav"
    if lim1 <= 0.0
      then beginProcessing (file, file1) soundUkr (actsctrl, noiseLim)
      else alterVadE "8_x.wav" lim1 noiseMax (duration0*0.03)
    return lim1

-- | Function 'produceSound4' is used internally in the 'produceSound3' function for amplification 
-- up/down to the maximum level of the first @FilePath@ parameter in the tuple. The second one gives 
-- a name of the resulting file and the third @FilePath@ parameter of the function is the @FilePath@ for 
-- the input file.
produceSound4 :: (FilePath, FilePath) -> FilePath -> IO ()
produceSound4 (file, file1) fileB = do 
  norm fileB
  volS2 ("8" ++ fileB) file
  renameFile ("8." ++ fileB) file1

-- | Function 'showCoef' is used to represent the duration of the sound file.
showCoef :: String -> String
showCoef xs | '.' `elem` xs = 
  let (ts, us) = break (== '.') xs in let ws = showFFloat (Just 6) ((fromIntegral (read (take 6 . drop 1 $ us)::Int) + 1.0) / 1000000.0) "" in 
    ts ++ drop 1 ws
            | otherwise = xs
      
-- | Function 'beginProcessing' is used to catch the variant where the sound is fully cut by the SoX because the sound was created in inappropriate time.
-- It returns the process to the beginning of the sound recording. For the meaning of the tuple of @Sring@ parameters, refer to 
-- 'produceSound' documentation. The first @FilePath@ in the tuple of @FilePath@ parameters is a name of the sound file in @mmsyn6ukr@ package. The second one is the 
-- name of the resulting file to be produced in the current directory.
beginProcessing :: (FilePath, FilePath) -> String -> (String, String) -> IO ()
beginProcessing (file, file1) soundUkr (actsctrl, noiseLim) = do {
  cleanTemp
; putStr "The needed files were NOT created, because the sound was not at the moment of recording! The process will be restarted "
; putStrLn "for the sound. Please, produce a sound during the first 3 seconds (after 0.5 second delay) or specify greater ratio!"
; putStrLn $ "Listen to the \"" ++ soundUkr ++ "\" sound and note first of all its duration. "
; playA file
; putStrLn "    *****"
; putStrLn ""
; putStrLn "The sound duration is: "
; produceSound2 (file, file1) (actsctrl, noiseLim) soundUkr}

-- | Function 'produceSound2' is used internally in the 'produceSound' function.
produceSound2 :: (FilePath, FilePath) -> (String, String) -> String -> IO ()
produceSound2 (file, file1) (actsctrl, noiseLim) soundUkr = do {
; duration0 <- durationA file
; putStrLn $ showCoef (showFFloat (Just 6) duration0 "")
; putStrLn ""
; putStrLn "It means that to produce more than 3 seconds of recording, you must specify at least "
; putStrLn $ "   " ++ show (3.0/duration0) ++ " as a next step ratio being prompt "
; putStrLn "   OR "
; putStrLn $ "   " ++ show (1.0/duration0) ++ " per one second but not less than the previous number."
; putStrLn $ "For example for 10 seconds record, please, specify " ++ show (10.0/duration0) ++ " as a next step ratio."
; putStrLn "    *****"
; putStrLn ""
; (_, Just hout, _, _) <- createProcess (proc (fromJust . showE $ "soxi") ["-D", file]) { std_out = CreatePipe }
; x3 <- hGetContents hout
; recommendSharp soundUkr
; (longerK0,pause0,sharp) <- tempS soundUkr noiseLim
; let longerK = (read x3::Float)*longerK0
; putStrLn $ "Please, wait for " ++ show pause0 ++ " seconds and pronounce the sound representation for the "
; putStrLn ""
; putStrLn $ "                                   \"" ++ (if soundUkr /= "ь" then map toUpper soundUkr else soundUkr) ++ "\""
; putStrLn ""
; putStrLn " sound or whatever you would like to be substituted instead (be sensible, please)! "
; if sharp || (compare longerK 3.0 == GT)
    then recB "x.wav" (longerK, pause0)
    else recB "x.wav" (3.0, pause0)
; putStrLn "The file is recorded and now will be automatically processed. You will be notificated with the text message in the terminal about the creation of the needed file. Please, wait a little. "
; controlNoiseReduction actsctrl
; norm "_x.wav"
; lim0 <- durationA "8_x.wav" 
; if null noiseLim
    then printGenInfo1
    else if last noiseLim == 's'
           then putStr ""
           else printGenInfo1
; let noiseMax = getBFst' (2::Int, V.fromList [("0", 0::Int), ("1", 1::Int), ("2", 2::Int), ("3", 3::Int)]) (take 1 noiseLim)
; produceSound3 (actsctrl, noiseLim) (file, file1) soundUkr (noiseMax, duration0) lim0
; cleanTemp }

-- | Function 'printGenInfo1' prints the general information about behaviour of the program in case of the different specified first two command line
-- arguments. If in the second command line argument there is letter \'s\' at the end, then the printing is omitted.
printGenInfo1 :: IO ()
printGenInfo1 = do {
putStrLn ""
; putStrLn "If you specified as a first command line argument one of the numbers below the program behaves as follows: "
; putStrLn "-1 -> the program does not reduce noise, it only resamples the audio to the needed 22050 Hz and adjusts the amplitude;"
; putStrLn ""
; putStr "If you specified something else then the program will reduce the noise using the created noise profile. If the first character is one of the "
; putStr "following, then the program will do the following actions besides. After the first character (without any spaces) you can specify "
; putStr "the level of noise reduction by 2 next digits. They are treated by the program as a fractional part of the number \"0.\" ++ \"...\" "
; putStr "so that the last number is passed to the SoX as an amount parameter in the \"noisered\" effect (the greater number gives more aggressive "
; putStrLn "noise reduction with the default one equal to 0.5. For more information, please, refer to the SoX documentation. "
; putStrLn ""
; putStrLn "Therefore, if you specify as a first symbol in the first command line argument one of the following numbers, then the program will behave: "
; putStrLn "0 -> after the noise reduction the program only resamples the audio to the needed 22050 Hz and adjusts the amplitude; "
; putStrLn "1 -> after the noise reduction the program additionally to the 0-processing truncates the silence from the beginning and end of the audio to the level given by the second command line parameter; "
; putStrLn "2 -> after the noise reduction the program additionally to the 1-processing applies a double band-reject filter to the audio (SoX \'sinc\' effect); "
; putStrLn "3 -> after the noise reduction the program additionally to the 2-processing applies fade-in and fade-out effects to the audio; "
; putStrLn "_ -> is the same as 3. "
; putStrLn "         ----------------------            "
; putStrLn ""
; putStrLn "If you specify at the beginning of a second command line argument one of the numbers below the program behaves as follows: "
; putStrLn ""
; putStr "0 -> if the first character in the first command line argument is greater or equal to 1, "
; putStr "then the program trims the sound at the beginning and at the end of it. "
; putStr "It firstly normalizes the sound (so its maximum amplitude is equal to 1) "
; putStr "and then applies trimming. The parts of the audio at the beginning "
; putStr "and at the end of the sound, which amplitudes in such a case are "
; putStr "less than 0.01 are trimmed and the resulting sound data are processed "
; putStrLn "further.  "
; putStrLn ""
; putStr "1 -> if the first character in the first command line argument is greater or equal to 1, "
; putStr "then the program trims the sound at the beginning and at the end of it. "
; putStr "It firstly normalizes the sound (so its maximum amplitude is equal to 1) "
; putStr "and then applies trimming. The parts of the audio at the beginning "
; putStr "and at the end of the sound, which amplitudes in such a case are "
; putStr "less than 0.02 are trimmed and the resulting sound data are processed "
; putStrLn "further.  "
; putStrLn ""
; putStr "2 -> if the first character in the first command line argument is greater or equal to 1, "
; putStr "then the program trims the sound at the beginning and at the end of it. "
; putStr "It firstly normalizes the sound (so its maximum amplitude is equal to 1) "
; putStr "and then applies trimming. The parts of the audio at the beginning "
; putStr "and at the end of the sound, which amplitudes in such a case are "
; putStr "less than 0.04 are trimmed and the resulting sound data are processed "
; putStrLn "further.  "
; putStrLn ""
; putStr "3 -> if the first character in the first command line argument is greater or equal to 1, "
; putStr "then the program trims the sound at the beginning and at the end of it. "
; putStr "It firstly normalizes the sound (so its maximum amplitude is equal to 1) "
; putStr "and then applies trimming. The parts of the audio at the beginning "
; putStr "and at the end of the sound, which amplitudes in such a case are "
; putStr "less than 0.08 are trimmed and the resulting sound data are processed "
; putStrLn "further.  "
; putStrLn ""
; putStr "_ -> if the first character in the first command line argument is greater or equal to 1, "
; putStr "then the program trims the sound at the beginning and at the end of it. "
; putStr "It firstly normalizes the sound (so its maximum amplitude is equal to 1) "
; putStr "and then applies trimming. The parts of the audio at the beginning "
; putStr "and at the end of the sound, which amplitudes in such a case are "
; putStr "less than 0.04 are trimmed and the resulting sound data are processed "
; putStrLn "further. So effectively it is the same as 2 (the default value). "
; putStrLn ""
; putStrLn "         ----------------------            "
; putStr "You can shorten the information printed during the program execution by specifying as the "
; putStr "last symbol in the second command line argument an \'s\'. In such a case, the program omits "
; putStr "printing the much of its informational messages that are used mostly for the learning "
; putStr "to deal with the program. If you specified the second command line argument without the \'s\' "
; putStrLn "at the end, then the program prints all the additional information that is considered important."
; putStrLn "" }

-- | Function 'controlNoiseReduction' is used in the 'produceSound2' and 'beginProcessing' functions to reduce the noise with the created by the
-- 'tempeRa' noise profile. If you specified something else than \"-1\" as a first command line argument, then the program will reduce the noise
-- using the created noise profile. 
-- If the first character is one of the following, then the program will do the following actions besides. After the first character
-- (without any spaces) you can specify the level of noise reduction by 2 next digits. They are treated by the program as a fractional part
-- of the number \"0.\" ++ \"...\" so that the last number is passed to the SoX as an amount parameter in the \"noisered\" effect
-- (the greater number gives more aggressive noise reduction with the default one equal to 0.5. For more information, please, refer to the SoX documentation. 
controlNoiseReduction :: String -> IO ()
controlNoiseReduction actsctrl = if actsctrl /= "-1"
    then do
      s2 <- onException (do
              let s1 = take 1 actsctrl
                  sr0 = filter isDigit . drop 1 $ actsctrl
              if null sr0
                then return 0.5
                else let sr = "0." ++ sr0
                         s  = read sr::Float in if s > 0.0 then return s else return 0.01) (return 0.5)
      (code, _, _) <- readProcessWithExitCode (fromJust (showE "sox")) ["x.wav", "_x.wav", "noisered", "nx0.wav.b.prof", showFFloat (Just 4) s2 $ show 0] ""
      if code /= ExitSuccess 
        then do
          e1 <- doesFileExist "_x.wav"
          if e1
            then do
              removeFile "_x.wav"
              catchEnd (NotCreatedWithEffect "noisered")
            else catchEnd (NotCreatedWithEffect "noisered")
        else do 
          e2 <- doesFileExist "_x.wav"
          if e2 
            then return ()
            else catchEnd (InitialFileNotChanged "x.wav")
    else renameFile "x.wav" "_x.wav"

-- | Function to get the @(Float, Float, Bool)@ value. The first @Float@ value shows in how many times you expect that your sound representation
-- will be longer than the one provided by the @mmsyn6ukr@ package. The second one specifies a duration of the pause before SoX actually starts to
-- record the needed sound data (in seconds). The @Bool@ value specifies whether the program uses a \'sharp\' mode meaning that
-- it does not check whether the resulting duration of the recording is at least 3 seconds long, so you can specify shorter durations.
-- The @String@ arguments are the Ukrainian sound representation name and the second command line argument for the program respectively.
tempS :: String -> String -> IO (Float, Float, Bool)
tempS soundUkr noiseLim = onException (do
    if null noiseLim
      then printGenInfo2
      else if last noiseLim == 's'
             then putStr ""
             else printGenInfo2
    putStrLn "In how many times do you think your sound representing " 
    putStrLn ""
    putStrLn $ "                     \"" ++ (if soundUkr /= "ь" then map toUpper soundUkr else soundUkr) ++ "\"" 
    putStrLn ""
    putStrLn "will sound longer than the recently played one? Specify your input as a Float value without \'e\' notation (with the preceding asterisk sign for the \'sharp\' mode). "
    putStrLn ""
    longivityZ <- getLine
    let sharp0 = take 1 longivityZ
        sharp1 = drop 1 . dropWhile (/= '#') $ longivityZ
        sharp2 = if null sharp1 then 0.5 else let zzz0 = read (filter (\z -> isDigit z || z == '.') sharp1)::Float in if zzz0 /= 0.0 then zzz0 else 0.5
    case sharp0 of
      "*" -> let long = read (takeWhile (/= '#') . drop 1 $ longivityZ)::Float in return (long,sharp2,True)
      _   -> let long = read (takeWhile (/= '#') longivityZ)::Float in return (long,sharp2,False)) (do 
               putStrLn "Please, specify again the valid values!"
               tempS soundUkr noiseLim)

-- | Function 'printGenInfo2' prints the additional information about the \'sharp\' mode and the possibility to specify the duration of the
-- pause before the SoX executable actuall starts recording of the sound representation.
-- If in the second command line argument there is letter \'s\' at the end, then the printing is omitted.
printGenInfo2 :: IO ()
printGenInfo2 = do
  putStrLn ""
  putStr "IMPORTANT. Would you like to use a \'sharp\' mode for this sound representaiion? (Enter \'*\' as the first symbol in your next input to use the "
  putStr "\'sharp\' mode, in which the program does not use 3 seconds minimal limit to record the sound representation, "
  putStr "but a recording duration is no more than the one specified by your entered ratio). For not using the \'sharp\' mode, "
  putStrLn "enter your next input without the asterisk."
  putStrLn ""
  putStrLn "IMPORTANT 2. After specifying the ratio you can input \'#\' sign and specify after it the duration (in seconds) of the needed pause before SoX will actually start recording of the sound data. The default one is 0.5 seconds if nothing is specified. This also provides backward compatibility of the program."
  putStrLn ""

-- | Function 'cleanTemp' removes all the intermediate temporary files in the directory where it is called from.
cleanTemp :: IO ()
cleanTemp = do
  filenames <- getDirectoryContents =<< getCurrentDirectory
  mapM_ removeFile . filter (\x -> head x `elem` (['2'..'9'] ++ "_" ++ "x")) $ filenames

-- | Function 'cleanTempN' removes all the intermediate temporary files produced during a noise profile creation in the directory where it is called from.
cleanTempN :: IO ()
cleanTempN = do
  filenames <- getDirectoryContents =<< getCurrentDirectory
  mapM_ removeFile . filter (\x -> head x == 'n') $ filenames

-- | Function 'tempeRa' is used to create a noise profile for all the recorded sounds. The function is used internally in the @mmsyn7ukr@
-- program. While running if you provide a 5 seconds silence as needed, the program @mmsyn7ukr@ will
-- reduce the noise in your recordings. This will create a cleaner sound. If you would like not to reduce the noise at all, then, please,
-- specify \"-1\" as the first command line argument for the program @mmsyn7ukr@.
tempeRa :: Int -> IO ()
tempeRa n = do {
    putStrLn "Now, please, be in a silence for 5 seconds so that the program can create a noise profile to remove the noise from the recording. "
    ; putStr "Otherwise, the program can remove from the recorded sound data some important parts as a noise. "
    ; if n == 1 then putStrLn "If you would like not to reduce the noise at all, then, please, specify as the first command line argument \"-1\". "
      else putStr ""
    ; V.imapM_ (\i x -> recA ("nx" ++ show (i + 1) ++ ".wav") 0.07 >> threadDelay (50000 * x)) . V.fromList $ [2,3,2,2,3]
    ; [upperB1,upperB2,upperB3,upperB4,upperB5] <- mapM (\c -> upperBnd ("nx" ++ (c:".wav"))) "12345"
    ; v0 <- V.imapM (\i upp -> fmap fst . selMaxAbs ("nx" ++ show (i + 1) ++ ".wav") $ (0,upp)) . V.fromList $ [upperB1,upperB2,upperB3,upperB4,upperB5]
    ; renameFile ("nx" ++ (show ((V.minIndex . V.map (\x -> abs (read x::Float)) $ v0) + 1) ++ ".wav")) "nx0.wav"
    ; noiseProfB "nx0.wav" >> putStrLn "" >> threadDelay 400000 >> putStrLn "The noise sound profile is now created. The program can proceed further." }

-- | Function 'recommendSharp' is used to print an advice about the speech transformation for the Ukrainian sounds that you can pronounce
-- properly continually and so it can be better to use for their producing a \'sharp\' mode.
recommendSharp :: String -> IO ()
recommendSharp soundUkr = do
  let k0 = getBFst' (False, V.fromList . zip ["\1072","\1077","\1078","\1079","\1080","\1083","\1084","\1085","\1086","\1088","\1089","\1089\1100","\1091","\1092","\1093","\1096","\1110"] $ replicate 17 True) soundUkr
  if k0
    then do
      putStr $ "In case of speech transformation: for the sound representation for the Ukrainian \"" ++ soundUkr
      putStrLn "\" it is recommended to use a \'sharp\' mode. So, try to specify \'*\' as a first symbol and maybe pronounce the corresponding Ukrainian sound continually. "
    else do
      putStr $ "In case of speech transformation: for the sound representation for the Ukrainian \"" ++ soundUkr
      putStrLn "\" it is recommended to use a common mode (the usual one). So, try not to specify \'*\' as a first symbol. "

-- | Function 'main7ukr' is used internally in the 'main' function of the Main module. It responds for mainstream @mmsyn7ukr@ program execution.
-- It starts with CAUTION to be responsible for usage and to 
-- use it personally in some important cases (see README) and with some additional information. Then the program
-- guides you through the creating your Ukrainian \"voice\". 
-- Please, use it carefully. The function uses command line arguments. 
-- For their meaning, please, refer to README file.
-- 
main7ukr :: [String] -> IO ()
main7ukr args = do
  putStrLn "     ***** CAUTION! *****"
  putStrLn ""
  putStrLn "\"The possession of great power necessarily implies great responsibility.\""
  putStrLn ""
  putStrLn "                                            (William Lamb)"
  putStrLn ""
  let arg2 = concat . drop 1 . take 2 $ args
      end2 = if null arg2 then "" else [last arg2]
  case end2 of
    "s" -> putStr ""
    "l" -> printGenInfo0
    _   -> printGenInfo0
  putStrLn ""
  onException (if take 5 os == "mingw" 
      then do 
        let eS = fromJust (showE "sox")
            eSi = fromJust (showE "soxi")
        return ()
      else do 
        let eS = fromJust (showE "sox")
            eSi = fromJust (showE "soxi")
            eSp = fromJust (showE "play")
            eSr = fromJust (showE "rec")
        return ()) (catchEnd ExecutableNotProperlyInstalled)
  tempeRa 1
  let a0 = if null . take 1 $ args 
             then []
             else concat . take 1 $ args
      a1 = if null . drop 1 . take 2 $ args 
             then []
             else concat . drop 1 . take 2 $ args
      a2 = drop 2 . take 3 $ args
  if null a2
    then do
      paths <- mapM getDataFileName ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav", 
        "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", 
          "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav", 
            "d.wav", "e.wav", "f.wav"]
      copiedFs <- mapM getDataFileName ["-.wav", "0.wav", "1.wav"]
      mapM_ (produceSound (a0, a1)) paths
      silenceFs <- mapM makeAbsolute ["-.wav", "0.wav", "1.wav"]
      let pairs = zip copiedFs silenceFs
      mapM_ (uncurry copyFile) pairs
    else do
      putStrLn ""
      let rrs = show a2
          list0 = read (replaceP rrs)::[String]
          zss = read (replaceP4 . show $ list0)::[String]
          wws = map (getBFst' ("0.wav", V.fromList . zip ["а","б","в","г","д","дж","дз","е","ж","з","и","й","к","л","м","н","о","п","р","с",
                       "сь","т","у","ф","х","ц","ць","ч","ш","ь","і","ґ"] $ ["A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav", 
                         "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav", 
                           "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav", 
                             "d.wav", "e.wav", "f.wav"])) zss
      paths <- mapM getDataFileName wws
      copiedFs <- mapM getDataFileName ["-.wav", "0.wav", "1.wav"]
      mapM_ (produceSound (a0, a1)) paths
      silenceFs <- mapM makeAbsolute ["-.wav", "0.wav", "1.wav"]
      let pairs = zip copiedFs silenceFs
      mapM_ (uncurry copyFile) pairs
  putStrLn ""
  putStrLn "Your voice sound files are now created in the current directory! Use in a secure way! Remember the initial CAUTION! "
  putStrLn ""
  cleanTempN

-- | Function 'printGenInfo0' prints once the general information if in the second command line argument there is no letter \'s\' at the end.
printGenInfo0 :: IO ()
printGenInfo0 = do
  putStr "The program mmsyn7ukr produces the \"voice\" represented as an ordered "
  putStr "set of sounds each of which corresponds (represents) one of the "
  putStr "Ukrainian sounds so that using them together by mmsyn7h program "
  putStr "(https://hackage.haskell.org/package/mmsyn7h) can be a background "
  putStr "for sound synthesis. If you pronounce sounds as the appropriate "
  putStr "Ukrainian ones, close to proper Ukrainian speech with your own "
  putStr "voice. This program approximates your voice with a sequence "
  putStr "of recorded separate sounds with your control over the "
  putStr "duration of the sounds. They are then precessed by the SoX "
  putStr "binaries already installed in the system to produce the "
  putStr "needed sounds and then you can pronounce some Ukrainian text "
  putStr "with your recorded \"voice\" using mmsyn7h program. In-between "
  putStr "you can do some additional processing as you need. Moreover, "
  putStr "you can substitute whatever sounds you like (consider being "
  putStrLn "sensible) instead of your own voice."
  putStrLn ""
  putStr "Be aware that if somebody can get access to the sounds of "
  putStr "your voice or to the recorded speech (except you) then this "
  putStr "possibility itself creates security issues and concerns. So, "
  putStr "please, do NOT give access to such records to anybody else "
  putStrLn "except you."
  putStrLn ""
  putStr "In such a case, the program is for personal usage of every "
  putStrLn "user ONLY!"
  putStrLn ""
  putStr "Being given as an advice in such a case, run the program "
  putStr "in the empty directory with the current user permissions to write, "
  putStr "read and search and provide some proofs and evidence that nobody else "
  putStr "can even read the files in the directory. May be, it is better "
  putStr "to execute the program being in the directory located in the RAM, "
  putStr "then consequently wait until the program ends and then reboot "
  putStrLn "the computer. "
  putStrLn ""
  putStr "If the program ends earlier, you must then remove "
  putStr "(better wipe) the directory contents. No other users should "
  putStr "have access to the computer after you have begun to run the "
  putStr "program and have not deleted (or better wiped) the contents "
  putStr "of the directory. Please, be aware, that there are possibilities "
  putStr "to read sensitive information from the drives after you have "
  putStr "deleted the files in a usual way. You can use wiping for better "
  putStr "security. Besides, if somebody can get access to the memory of "
  putStr "the computer or to the directory contents where you run the "
  putStr "program or (may be) to the temporary files created by SoX or "
  putStr "to the drive where you run the program (not in the RAM, or may "
  putStr "be in it) then your voice can be stolen and / or used "
  putStr "inappropriately. Use all possible precautions and measures to "
  putStrLn "avoid the situation. "
  putStrLn ""
  putStr "Be aware also that the given by the program technology (or "
  putStr "documentation for it in any form) of the voice processing can "
  putStr "be improved so there is NO guarantees that the given technology "
  putStr "or its successors cannot be used in violating your voice identity "
  putStr "to produce from some available voice records the voice for the "
  putStr "inappropriate usage. Therefore, better is to proove your identity not "
  putStr "only with the solely voice itself but with some additional "
  putStrLn "independent sources and measures. "
  putStrLn ""
  putStr "The author of the program accordingly to the LICENSE (MIT) does not "
  putStr "response for any possible issues, but by this notification tries to "
  putStrLn "intent you to be aware of some possible issues."
  putStrLn ""
  putStrLn "           ***** More Information *****"
  putStrLn ""
  putStr "You can create a sound in either a \'sharp\' mode or in a usual mode."
  putStr "The first one means that the program does not check whether the"
  putStr "specified duration for recording the initial sound data"
  putStr "is greater than 3 seconds. In such a case the duration can be"
  putStr "much smaller. This mode needs more mastership in interacting with a"
  putStr "program. For speech synthesis (as an advice) use this mode for"
  putStr "the very short sounds (like the sound representation for \"ь\")"
  putStr "or for the sounds, you can articulate for a long time continually"
  putStr "(for example, vowels and some consonants). The \'sharp\' mode delegates"
  putStr "the responsibility for the sound to much broader extent to the user,"
  putStrLn "so for a beginning it is not recommended (though you can give it a try)."
  putStrLn ""
  putStr "At the beginning the program also creates a noise profile (once per execution)."
  putStr "It is now used to reduce the noise level for the recorded sound representations."
  putStr "It uses the default SoX noise reducing settings with a hope that for you they can"
  putStrLn "be sufficient."
  putStrLn ""
  putStrLn "           ***** Ukrainian Localization *****"
  putStrLn ""
  putStr "Please, before using the program check that uk_UA.UTF8 localization is"
  putStr "present in your system as one of the system locales. Otherwise,"
  putStr "the program will possibly (in some cases surely) cycle. In such a case,"
  putStrLn "you can terminate it in a usual way by sending interruption signals."
  putStrLn ""


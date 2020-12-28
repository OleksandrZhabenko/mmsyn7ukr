A program and a library that can be used as a simple 
basic interface to some [SoX](https://sourceforge.net/projects/sox/)
functionality or for producing 
the approximately Ukrainian speech with your own recorded 
voice (actually it produces the needed sound representations).

The program starts with CAUTION to be responsible for usage 
and to use it personally. Then the program guides you 
through the creating and using your Ukrainian "voice". 
Please, use it carefully. After the execution you can
execute mmsyn7h program to produce some records with your 
newly created "voice". During the execution of the last one
you can choose whether to remove (clean) 
all the created sound files in the current directory for 
security reasons. If the programs terminate by interruption (or 
generally) it is a good practice to remove the current 
directory sound files manually.

        ***** CAUTION! *****
        ====================

"The possession of great power necessarily implies
great responsibility."

                                          (William Lamb)

The program mmsyn7ukr produces the "voice" represented as an ordered 
set of sounds each of which corresponds (represents) one of the 
Ukrainian sounds so that using them together by
[mmsyn7h program](https://hackage.haskell.org/package/mmsyn7h)
can be a background for sound synthesis.
If you pronounce sounds as the appropriate 
Ukrainian ones, close to proper Ukrainian speech with your own 
voice. This program approximates your voice with a sequence 
of recorded separate sounds with your control over the 
duration of the sounds. They are then precessed by the SoX 
binaries already installed in the system to produce the 
needed sounds and then you can pronounce some Ukrainian text 
with your recorded "voice" using mmsyn7h program. In-between 
you can do some additional processing as you need. Moreover, 
you can substitute whatever sounds you like (consider being 
sensible) instead of your own voice.

Be aware that if somebody can get access to the sounds of 
your voice or to the recorded speech (except you) then this 
possibility itself creates security issues and concerns. So, 
please, do NOT give access to such records to anybody else 
except you.

In such a case, the program is for personal usage of every 
user ONLY!

Being given as an advice in such a case, run the program 
in the empty directory with the current user permissions to write, 
read and search and provide some proofs and evidence that nobody else 
can even read the files in the directory. May be, it is better 
to execute the program being in the directory located in the RAM, 
then consequently wait until the program ends and then reboot 
the computer. 

If the program ends earlier, you must then remove 
(better wipe) the directory contents. No other users should 
have access to the computer after you have begun to run the 
program and have not deleted (or better wiped) the contents 
of the directory. Please, be aware, that there are possibilities 
to read sensitive information from the drives after you have 
deleted the files in a usual way. You can use wiping for better 
security. Besides, if somebody can get access to the memory of 
the computer or to the directory contents where you run the 
program or (may be) to the temporary files created by SoX or 
to the drive where you run the program (not in the RAM, or may 
be in it) then your voice can be stolen and / or used 
inappropriately. Use all possible precautions and measures to 
avoid the situation. 

Be aware also that the given by the program technology (or 
documentation for it in any form) of the voice processing can 
be improved so there is NO guarantees that the given technology 
or its successors cannot be used in violating your voice identity 
to produce from some available voice records the voice for the 
inappropriate usage. Therefore, better is to prove your identity not 
only with the solely voice itself but with some additional 
independent sources and measures. 

The author of the program accordingly to the LICENSE (MIT) does not 
response for any possible issues, but by this notification tries to 
intent you to be aware of some possible issues.

        ***** Command Line Arguments *****
        ==================================

If you specify as a first command line argument one of the numbers below
the program behaves as follows:

    -1 -> the program does not reduce noise, it only resamples
       the audio to the needed 22050 Hz and adjusts the amplitude.

If you specified something else then the program will reduce the noise
using the created noise profile. If the first character is one of the 
following, then the program will do the following actions besides.
After the first character (without any spaces) you can specify 
the level of noise reduction by 2 next digits. They are treated
by the program as a fractional part of the number "0." ++ "..." 
so that the last number is passed to the sox as an amount parameter
in the "noisered" effect (the greater number gives more aggressive 
noise reduction with the default one equal to 0.5).
For more information, please, refer to the SoX documentation. 

Therefore, if you specify as a first symbol in the first
command line argument one of the next numbers, then
the program will behave as follows:

    0 -> after the noise reduction the program only resamples 
      the audio to the needed 22050 Hz and adjusts the amplitude;
      
    1 -> after the noise reduction the program additionally 
      to the 0-processing truncates the silence from the beginning 
       and the end of the audio to the level given by the second 
        command line parameter;
        
    2 -> after the noise reduction the program additionally to 
      the 1-processing applies a double band-reject filter to 
       the audio (SoX "sinc" effect);
       
    3 -> after the noise reduction the program additionally to 
      the 2-processing applies fade-in and fade-out effects to 
       the audio;
       
    _ -> is the same as 3. 

If you specify at the beginning of a second command line argument one of the numbers below
the program behaves as follows:

    0 -> if the first character in the first command line argument is greater or equal to 1,
      then the program trims the sound at the beginning and at the end of it.
        It firstly normalizes the sound (so its maximum amplitude is equal to 1)
          and then applies trimming. The parts of the audio at the beginning
            and at the end of the sound, which amplitudes in such a case are
              less than 0.01 are trimmed and the resulting sound data are processed
                further. 
       
    1 -> if the first character in the first command line argument is greater or equal to 1,
      then the program trims the sound at the beginning and at the end of it.
        It firstly normalizes the sound (so its maximum amplitude is equal to 1)
          and then applies trimming. The parts of the audio at the beginning
            and at the end of the sound, which amplitudes in such a case are
              less than 0.02 are trimmed and the resulting sound data are processed
                further. 
       
    2 -> if the first character in the first command line argument is greater or equal to 1,
      then the program trims the sound at the beginning and at the end of it.
        It firstly normalizes the sound (so its maximum amplitude is equal to 1)
          and then applies trimming. The parts of the audio at the beginning
            and at the end of the sound, which amplitudes in such a case are
              less than 0.04 are trimmed and the resulting sound data are processed
                further. 
       
    3 -> if the first character in the first command line argument is greater or equal to 1,
      then the program trims the sound at the beginning and at the end of it.
        It firstly normalizes the sound (so its maximum amplitude is equal to 1)
          and then applies trimming. The parts of the audio at the beginning
            and at the end of the sound, which amplitudes in such a case are
              less than 0.08 are trimmed and the resulting sound data are processed
                further. 
       
    _ -> if the first character in the first command line argument is greater or equal to 1,
      then the program trims the sound at the beginning and at the end of it.
        It firstly normalizes the sound (so its maximum amplitude is equal to 1)
          and then applies trimming. The parts of the audio at the beginning
            and at the end of the sound, which amplitudes in such a case are
              less than 0.04 are trimmed and the resulting sound data are processed
                further. So effectively it is the same as 2 (the default value).

After the number as its ending you can specify the letter 's' to shorten the printed
informational messages by the program. More information is further below.
    
If you specify the third command line argument, it must be a list 
of Strings that can be obtained by executing the
[mmsyn7s program](https://hackage.haskell.org/package/mmsyn7s).
The program will create just that non-silent representations for
the Ukrainian sounds, which are given. The list must be sorted.

           ***** More Information *****
           ============================

You can create a sound in either a 'sharp' mode or in a usual mode.
The first one means that the program does not check whether the
specified duration for recording the initial sound data
is greater than 3 seconds. In such a case the duration can be
much smaller. This mode needs more mastership in interacting with a
program. For speech synthesis (as an advice) use this mode for
the very short sounds (like the sound representation for "ь")
or for the sounds, you can articulate for a long time continually
(for example, vowels and some consonants). The 'sharp' mode delegates
the responsibility for the sound to much broader extent to the user,
so for a beginning it is not recommended (though you can give it a try).

           ***** Noise Reduction *****
           ===========================

At the beginning the program also creates a noise profile (once per execution).
It is now used to reduce the noise level for the recorded sound representations.
By default, the program uses the default SoX noise reducing settings with
a hope that for you they can be sufficient. You can change the level of reduction
by the first command line argument as it is written above. If you see that
the program removes some sensitive and important sound data from the recorded
by it sound representations, then you can specify as a first command line
argument "-1". The program in such a case will create a noise profile as usual,
but will not reduce the noise at all.

           ***** Ukrainian Localization *****
           ==================================

Please, before using the program check that uk_UA.UTF8 localization is
present in your system as one of the system locales. Otherwise,
the program will possibly (in some cases surely) cycle. In such a case,
you can terminate it in a usual way by sending interruption signals.

           ***** SoXBasics and SoXBasics1 *****
           ====================================

The library have two similar modules with partially overlapping function names
(so it is recommended to import them qualified). SoXBasics1 functions after
creation of the resulting file try to replace by it the initial one and to clean
the space from the temporary files. This can be helpful in writing sequentially
applied functions but needs somewhat more resources.

           ***** Test Sound Duration *****
           ===============================
           
You can now test the sound duration needed as a 0.5 second pause before
the program can record every non-silent sound. This pause is caused not by the program
itself, but by some sound recording equipment specific time needed to normalize
the behaviour of the recording (it depends on your equipment, but for a wide range of
possible its configuration the pause for 0.5 second is valid).
To test a needed 0.5 second duration of the pause, you can use as a first
command line argument "-t" and the program will only play the test sound with
just that duration. Remember its duration -- it is the duration of the needed
pause before you can start your recording of the every sound representation.

           ***** Specifying the Pause before SoX Actually Starts Recording *****
           =====================================================================
           
You can specify the pause before SoX actually starts recording of the sound data
for every sound representation. For this, please, specify after the needed
ratio a '#' sign and after it the needed duration of the pause in seconds as a
Double value. The program will try to use it. If not specified or not properly
specified, the program uses a default one (compatible with the previous versions)
equal to 0.5 second pause.

           ***** The Possibility to Shorten the Printed Information *****
           ==============================================================

You can shorten the information printed during the program execution by specifying
as the last symbol in the second command line argument an 's'. In such a case,
the program omits printing the much of its informational messages that are used
mostly for the learning to deal with the program. If you specified the second command
line argument without the 's' at the end, then the program prints all the
additional information that is considered important.

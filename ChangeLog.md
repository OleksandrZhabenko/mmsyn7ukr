# Revision history for mmsyn7ukr

## 0.1.0.0 -- 2019-12-19

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2019-12-19

* First version revised A. Fixed an issue with an empty do-block (mismatched code indentation).

## 0.1.1.1 -- 2019-12-20

* First version revised B. Improved some documentation and .cabal file.

## 0.1.2.0 -- 2019-12-23

* First version revised C. Fixed an issues with the not created and not appripriately running recording for the sound.

## 0.2.0.0 -- 2019-12-24

* Second version. Fixed an issue with the copying files. Changed a module structure. Some minor changes to the documentation.

## 0.2.1.0 -- 2019-12-24

* Second version revised A. Extended the bounds for the base package dependency so that it now can be compiled for GHC 8.8.1. 
Added some functionality to improve the control over the program mmsyn7ukr behaviour.

## 0.3.0.0 -- 2019-12-24

* Third version. Changed behaviour of the program so that it now only creates a "voice". It allows to use additional executable mmsyn7h afterwards from the package with the same name. 
The latter is very close to mmsyn6ukr but uses the data files in the directory where it is running instead of defined by the standard for Paths_mmsyn6ukr.

## 0.3.0.1 -- 2019-12-25

* Third version revised A. Changed some documentation and text messages to more consistent ones.

## 0.3.1.0 -- 2019-12-27

* Third version revised B. Changed the behaviour for 'alterVadB' and 'alterVadE' functions so that are now fully iterative with thread delay.

## 0.4.0.0 -- 2019-12-31

* Fourth version. Changed the behaviour of the program. Now it uses command line arguments (if any). See README. Added functions and rewrote the complex ones. 

## 0.4.1.0 -- 2020-01-01

* Fourth version revised A. Fixed issues with the null command line arguments and showing precision. Added new functions.

## 0.4.2.0 -- 2020-01-02

* Fourth version revised B. Added a new function 'gainL' to the SoXBasics module. Fixed an issue with 'volS' function. Some minor code changes. Some documentation improvements.

## 0.5.0.0 -- 2020-01-05

* Fifth version. Added proper support for Windows for the functions in the SoXBasics and Main modules. Added a new function 'gainL' to the SoXBasics module. Fixed an issue with 'volS' function. Some minor code changes. Some documentation improvements.

## 0.5.0.1 -- 2020-01-06

* Fifth version revised A. Improved a documentation structure.

## 0.6.0.0 -- 2020-01-10

* Sixth version. Added a possibility to create not a full set of 
the Ukrainian sounds representations, but only for the those 
non-silent ones that are specified by the third command line parameter.

## 0.6.1.0 -- 2020-01-10

* Sixth version revised A. Fixed issues with wrongly located information messages, 
non-parsed third command line argument. Added a new module ReplaceP. Some minor improvements.

## 0.6.1.1 -- 2020-01-10

* Sixth version revised B. Fixed documentation issues.

## 0.6.2.0 -- 2020-01-13

* Sixth version revised C. Fixed issues with wrongly parsed sounds that are represented with two letters ("дж", "дз", "сь", "ць").

## 0.6.2.1 -- 2020-01-13

* Sixth version revised D. Fixed issues with being not compiled.

## 0.6.3.0 -- 2020-01-13

* Sixth version revised E. Simplified the parsing module ReplaceP.

## 0.7.0.0 -- 2020-01-20

* Sevenh version. Added the possibility to omit the securing and usual limitation (for regularization) for the initial sound data recording duration.
It is so called 'sharp' mode. Fixed an issue with the wrong duration. Some minor code improvements.

## 0.7.1.0 -- 2020-01-20

* Seventh version revised A. Removed the special parameter in the code, which influenced the duration of the initial sound data recording.

## 0.7.2.0 -- 2020-01-21

* Seventh version revised B. Fixed an issue with the noise reduction that can remove sensitive sound recording data from the program
produced recordings. Added some additional functions. Some documentation improvements. The program now creates a noise profile at
the beginning of its execution. Afterwards, it is removed if the program ends successfully.

## 0.7.3.0 -- 2020-01-22

* Seventh version revised C. Changed the realization of the main function. The function tempoR renamed to tempeRa. Documentation improvements.

## 0.7.3.1 -- 2020-01-22

* Seventh version revised D. Some documentation improvements.

## 0.7.4.0 -- 2020-01-22

* Seventh version revised E. Changed the behaviour and the realization of the noise reduction function tempeRa.

## 0.7.5.0 -- 2020-01-23

* Seventh version revised F. Fixed an issue with using not a minimum of the noise level for the noise reduction function tempeRa.
Some minor documentation improvements.

## 0.7.6.0 -- 2020-01-23

* Seventh version revised G. Added a possibility not to reduce a noise. It is controlled by the first command line argument.
Some minor documentation improvements.

## 0.8.0.0 -- 2020-01-24

* Eigth version. Added a new module SoXBasics1 where all the functions (contrary to the SoXBasics module) try to replace
the initial file with the resulting one. Changed the lower bound for SoX "sinc" effect. Added help messages and version information
being obtained in the command line.

## 0.9.0.0 -- 2020-01-24

* Ninth version. Changed the behaviour in case of giving information to the user. Changed killing the main thread to raising a special
exception with 'catchEnd' function. The program now becomes also a dependency for mmsyn7h package.

## 0.9.1.0 -- 2020-01-24

* Ninth version revised A. Fixed an issue with being not compiled because of not being imported module dependency.

## 0.9.1.1 -- 2020-01-24

* Ninth version revised B. Fixed version information.

## 0.9.2.0 -- 2020-01-24

* Ninth version revised C. Fixed an issue with Typeable in the Processing_mmsyn7ukr module. Now it should compile also for GHC 7.8.4.

## 0.9.3.0 -- 2020-01-24

* Ninth version revised D. Further needed work to fix an issue with Typeable in the Processing_mmsyn7ukr module. Now it should compile also for GHC 7.8.4.

## 0.9.4.0 -- 2020-01-25

* Ninth version revised E. Fixed issues with being not properly created or modified the files in the SoXBasics and SoXBasics1 modules.

## 0.9.4.1 -- 2020-01-25

* Ninth version revised F. Fixed issue with being not compiled because of the syntaxis errors. Some documentation improvements.

## 0.9.4.2 -- 2020-01-25

* Ninth version revised G. Fixed issue with wrong version number.

## 0.10.0.0 -- 2020-01-28

* Tenth version. Data type changed to FinalException and it is now a separate module FinalException. The error functions in the package is now rewritten
using the new datatype.

## 0.11.0.0 -- 2020-01-28

* Eleventh version. Changed the structure of modules. The module FinalException is now Control.Exception.FinalException. Added new contstuctors to
the module. Now the FinalException datatype includes all the needed exceptions for mmsyn7 series of program.

## 0.12.0.0 -- 2020-01-29

* Twelfth version. Added the possibility to control the amount (level) of noise reduction (if any) by the first command line parameter. For this,
added new functions noiseReduceBU and noiseReduceEU to the SoXBasics and SoXBasics1 modules and changed the behaviour of the Main function. Fixed issue with
the wrongly treated first command line parameter in the produceSound2 function. Changed README to README.markdown. Some documentation improvement.

## 0.12.0.1 -- 2020-01-29

* Twelfth version revised A. Some documentation improvements.

## 0.12.0.2 -- 2020-01-29

* Twelfth version revised B. Fixed issue with possible zero input for the sox noise reduction. Now the result is replaced with 0.01. Some documentation improvements.

## 0.12.0.3 -- 2020-01-30

* Twelfth version revised C. Fixed issues with inexact documentation and informational messages. Improved stylistics. Some documentation improvements.

## 0.12.0.4 -- 2020-01-30

* Twelfth version revised D. Fixed issues with inexact documentation and informational messages. Improved stylistics. Some documentation improvements.

## 0.12.0.5 -- 2020-02-08

* Twelfth version revised E. Fixed issues with the wrong spelling of the word 'amplitude'. Fixed issues with deprecated documentation for Control.Exception.FinalException module.

## 0.13.0.0 -- 2020-02-08

* Thirteenth version. Added a new module Paths_mmsyn7ukr and a data file "y.wav". Added a new command line argument possibility ("-t" as a first command line argument only plays a special 0.5 second sound file). Improved formatting for the text displayed on the screen. Revised the documentation and removed the deprecated information from the README.markdown file.

## 0.13.0.1 -- 2020-02-08

* Thirteenth version revised A. Fixed some issues with wrong calendar and spelling. 

## 0.14.0.0 -- 2020-02-08

* Fourteenth version. Fixed issue with cycling the 'beginProcessing' function because of the issue with the 'controlNoiseReduction' function
being called with default values. Added a new function 'recB' to the SoXBasics module. Reduced the size of the test file "y.wav". Changed the 'tempS' function, so
that now it returns alse the needed pause duration, which you can specify as a Double value after the '#' sign after the input duration ratio. By default,
it is equal to 0.5, that means the duration of the pause before the SoX actually will record a sound data is equal to 0.5 second.

## 0.14.0.1 -- 2020-02-08

* Fourteenth version revised A. Fixed the version number.

## 0.15.0.0 -- 2020-02-21

* Fifteenth version. Added an advice to use or not a 'sharp' mode. Added the possibility to shorten the printed informational messages by specifying
as the last symbol in the second command line argument a letter 's'. Fixed issue with inconsistent duration in the 'produceSound3' function and changed
the parameter from 0.04 to 0.03. Fixed issue with wrong specifying second command line argument for further processing. Some minor code and documentation
improvements.

## 0.15.1.0 -- 2020-02-21

* Fifteenth version revised A. Changed the Control.Exception.FinalException module so that the FinalException data is the exception and not
a signal of the given needed information.

## 0.15.1.1 -- 2020-02-22

* Fifteenth version revised B. Fixed issue with the deprecated documentation for the Control.Exception.FinalException module. Some minor documentation improvements.

## 0.15.1.2 -- 2020-02-22

* Fifteenth version revised C. Fixed issue with the wrong version number.

## 0.15.2.0 -- 2020-02-22

* Fifteenth version revised D. Fixed issue with the 'recommendSharp' function so that the sound representation for Ukrainian "ч" is not recommended to pronounce
in a 'sharp' mode (because it is not proper so).

## 0.15.3.0 -- 2020-03-04

* Fifteenth version revised E. Changed the 'Processing_mmsyn7ukr.tempeRa' function. 

## 0.15.4.0 -- 2020-05-14

* Fifteenth version revised F. Changed the bounds of the dependencies so that now also GHC 8.10* series are supported.

## 0.15.5.0 -- 2020-05-26

* Fifteenth version revised G. Some code improvements.

## 0.16.0.0 -- 2020-06-24

* Sixteenth version. Changed Double to Float in the the modules so that the functons can be consistent with new dobutokO packages. Besides, the double precision 
is not needed and in some cases is meaningless and can potentially (a little bit, however) reduce performance in some cases. Some minor code and documentation improvements.

## 0.17.0.0 -- 2020-08-16

* Seventeenth version. Changed the dependency of mmsyn6ukr bounds. 

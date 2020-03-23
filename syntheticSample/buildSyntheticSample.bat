:: NOTE: This windows batch script builds the synthetic sample object files and the executable

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: build ParaMonte library and Synthetic Sample object files and executables
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

echo off

cd %~dp0

echo.
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                                synthetic sample Build
echo. ::::                                                                                                                       ::::
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo.

echo. 
echo. Configuring build...
echo. 

set PARAMONTE_ROOT_RELATIVE_PATH=..\..\..\20180101_ParaMonte\git

:: clean already existing variables first
cd %PARAMONTE_ROOT_RELATIVE_PATH%
call buildUnconfig.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: Unable to UNconfigure ParaMonte library flags. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

call configSyntheticSample.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: Unable to configure and build synthetic sample flags. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

echo. 
echo. Configuring ParaMonte library build...
echo. 

:: change directory to ParaMonte's root dir
cd %PARAMONTE_ROOT_RELATIVE_PATH%
call build.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: Unable to configure and build ParaMonte Library. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0


echo. 
echo. Configuring Astronomical libraries...
echo. 

:: change directory to the common Astronomy libraries
cd ..\lib\astro\
call buildAstroLib.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: Unable to configure and build the Astronomy Library. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

:: add Kfactor correction if needed
set FPP_FLAGS_SYNTHETIC_SAMPLE=
if %KFAC_CORRECTION%==OneThird (
    set FPP_FLAGS_SYNTHETIC_SAMPLE=%FPP_FLAGS_SYNTHETIC_SAMPLE% /define:KFAC_ONETHIRD_ENABLED
) else (
    if %KFAC_CORRECTION% NEQ None (
        echo.
        echo. -- Fatal Error occurred: KFAC_CORRECTION=%KFAC_CORRECTION% is not recognized as an option.
        echo. -- exiting...
        echo.
        cd %~dp0
        set ERRORLEVEL=1
        exit /B 1
    )
)
echo.
echo. -- Kfactor model: %KFAC_CORRECTION%
echo. -- Synthetic Sample's Fortran preprocessor macros: %FPP_FLAGS_SYNTHETIC_SAMPLE%
echo.

:: set and make Synthetic Sample directories
set SYNTHETIC_SAMPLE_ROOT_PATH=%~dp0
set SYNTHETIC_SAMPLE_DIR=%SYNTHETIC_SAMPLE_ROOT_PATH%win%PLATFORM%\%COMPILER_SUITE%\%BTYPE%\%LTYPE%\%PARALLELIZATION_DIR%
set SYNTHETIC_SAMPLE_SRC_DIR=%SYNTHETIC_SAMPLE_ROOT_PATH%src
set SYNTHETIC_SAMPLE_BIN_DIR=%SYNTHETIC_SAMPLE_DIR%\bin
set SYNTHETIC_SAMPLE_LIB_DIR=%SYNTHETIC_SAMPLE_DIR%\lib
set SYNTHETIC_SAMPLE_LIB_MOD_DIR=%SYNTHETIC_SAMPLE_LIB_DIR%\mod
set SYNTHETIC_SAMPLE_LIB_OBJ_DIR=%SYNTHETIC_SAMPLE_LIB_DIR%\obj

:: loop over Synthetic Sample directories and generate them
echo.
for %%A in (
    %SYNTHETIC_SAMPLE_DIR%
    %SYNTHETIC_SAMPLE_BIN_DIR%
    %SYNTHETIC_SAMPLE_LIB_DIR%
    %SYNTHETIC_SAMPLE_LIB_MOD_DIR%
    %SYNTHETIC_SAMPLE_LIB_OBJ_DIR%
    ) do (  if exist %%A (
                echo. -- %%A already exists. skipping...
            ) else (
                echo. -- generating Synthetic Sample directory: %%A
                mkdir %%A
            )
)
echo.

if %SYNTHETIC_SAMPLE_OBJ_BUILD_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping synthetic sample object files build...
    echo.
    goto LABEL_SYNTHETIC_SAMPLE_EXE_BUILD_ENABLED
)

:: Read the name of each file from the ordered list of filenames in filelist.txt to compile
cd %SYNTHETIC_SAMPLE_LIB_OBJ_DIR%
echo.
echo. -- building synthetic sample program...

:: First verify the source filelist exists
set FILE_LIST=%SYNTHETIC_SAMPLE_SRC_DIR%\filelist.txt
if not exist %FILE_LIST% (
    echo.
    echo. -- Fatal Error: The filelist.txt containing the synthetic sample source filenames does not exist. Path: %FILE_LIST%
    echo. -- build failed. exiting...
    echo.
    cd %~dp0
    exit /B 1
)

:: generate object files
for /F "eol=! tokens=*" %%A in (%FILE_LIST%) do (

    echo. -- generating object file for %%A

    %FCL% %FCL_FLAGS% %FPP_FLAGS% %FPP_FLAGS_SYNTHETIC_SAMPLE% ^
    /module:%SYNTHETIC_SAMPLE_LIB_MOD_DIR%          %=path to output synthetic sample module files=% ^
    /I:%SYNTHETIC_SAMPLE_LIB_MOD_DIR%               %=path to output synthetic sample module files, needed 4 dependencies=%  ^
    /I:%ASTRO_LIB_MOD_DIR%                          %=path to input Astronomy library module files=%  ^
    /I:%MOD_DIR%                                    %=path to input ParaMonte module files=%  ^
    /c %SYNTHETIC_SAMPLE_SRC_DIR%\%%A               %=path to input synthetic sample source file=%  ^
    || (
        echo. 
        echo. -- Fatal Error: compilation of the object file for %%A failed.
        echo. -- build failed. exiting...
        echo. 
        set ERRORLEVEL=1
        cd %~dp0
        exit /B
    )
)
echo.
:LABEL_SYNTHETIC_SAMPLE_EXE_BUILD_ENABLED

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: generate Synthetic Sample executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if %SYNTHETIC_SAMPLE_EXE_BUILD_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping synthetic sample exectuable build...
    echo.
    goto LABEL_SYNTHETIC_SAMPLE_RUN_ENABLED
)

echo.

if %LTYPE%==dynamic (

    echo.
    echo. -- Warning: dynamically-linked synthetic sample executable not implemented. This requires significant changes in the library interfaces.
    echo. -- generating statically-linked synthetic sample executable at: %SYNTHETIC_SAMPLE_BIN_DIR%
    echo.
    
    set SYNTHETIC_SAMPLE_EXECUTABLE_NAME=%SYNTHETIC_SAMPLE_PROG%.exe
    set REQUIRED_OBJECT_FILES=%SYNTHETIC_SAMPLE_LIB_OBJ_DIR%\*.obj %OBJ_DIR%\*.obj

REM    :: copy necessary DLL files in the Synthetic Sample executable's directory
REM    echo. copying DLL files for the Synthetic Sample executable
REM    echo. from: %DLL_DIR%       %= no need for final slash here =%
REM    echo.   to: %SYNTHETIC_SAMPLE_BIN_DIR%  %= final slash tells this is folder =%
REM    xcopy /s /Y "%DLL_DIR%" "%SYNTHETIC_SAMPLE_BIN_DIR%\"
REM    echo.
REM
REM    echo. -- generating dynamically-linked synthetic sample executable at: %SYNTHETIC_SAMPLE_BIN_DIR%
REM
REM    set SYNTHETIC_SAMPLE_EXECUTABLE_NAME=LGRBParaMonteDynamic.exe
REM    set REQUIRED_OBJECT_FILES=%SYNTHETIC_SAMPLE_LIB_OBJ_DIR%\*.obj

) else (    %= static linking requested =%

    echo. -- generating statically-linked synthetic sample executable at: %SYNTHETIC_SAMPLE_BIN_DIR%

    set SYNTHETIC_SAMPLE_EXECUTABLE_NAME=%SYNTHETIC_SAMPLE_PROG%.exe
    set REQUIRED_OBJECT_FILES=%SYNTHETIC_SAMPLE_LIB_OBJ_DIR%\*.obj %ASTRO_LIB_OBJ_DIR%\*.obj %OBJ_DIR%\*.obj

)

:: delete the old executable first
echo. deleting old executable (if any) at: %SYNTHETIC_SAMPLE_BIN_DIR%\%SYNTHETIC_SAMPLE_EXECUTABLE_NAME%

cd %SYNTHETIC_SAMPLE_BIN_DIR%
del %SYNTHETIC_SAMPLE_EXECUTABLE_NAME%
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: deletion of the old executable at %SYNTHETIC_SAMPLE_BIN_DIR%\%SYNTHETIC_SAMPLE_EXECUTABLE_NAME% failed. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)

:: build the executable
%FCL% %FCL_FLAGS% %FL_FLAGS% ^
/module:%SYNTHETIC_SAMPLE_LIB_MOD_DIR% ^
/I:%SYNTHETIC_SAMPLE_LIB_MOD_DIR% /I:%ASTRO_LIB_MOD_DIR% /I:%MOD_DIR% ^
%REQUIRED_OBJECT_FILES% ^
/exe:%SYNTHETIC_SAMPLE_BIN_DIR%\%SYNTHETIC_SAMPLE_EXECUTABLE_NAME%

if %ERRORLEVEL%==1 ( 
    echo. 
    echo. -- Fatal Error: linking of the Synthetic Sample object files may have likely failed.
    echo. -- build may have likely failed. continuing...
    echo. 
    cd %~dp0
    exit /B 1
)

echo.

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: run synthetic sample executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:LABEL_SYNTHETIC_SAMPLE_RUN_ENABLED

:: run synthetic sample
:: if %SYNTHETIC_SAMPLE_RUN_ENABLED% NEQ TRUE goto LABEL_EXAMPLE_BUILD_ENABLED
if %SYNTHETIC_SAMPLE_RUN_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping synthetic sample run...
    echo.
    goto :eof
)

echo. 
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                             Running synthetic sample
echo. ::::                                                                                                                       ::::
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo.

:: copy necessary input files in the executable's directory
echo. copying input files to the Synthetic Sample executable's directory
echo. from: %SYNTHETIC_SAMPLE_ROOT_PATH%\in   %= no need for final slash here =%
echo.   to: %SYNTHETIC_SAMPLE_BIN_DIR%\in\  %= final slash tells this is folder =%
xcopy /s /Y "%SYNTHETIC_SAMPLE_ROOT_PATH%\in" "%SYNTHETIC_SAMPLE_BIN_DIR%\in\"
echo.

cd %SYNTHETIC_SAMPLE_BIN_DIR%
%SYNTHETIC_SAMPLE_EXECUTABLE_NAME% ./in/SyntheticSample.nml && ( 
    echo.
    echo.
    echo. -- synthetic sample run successful. 
    echo.
) || ( 
    echo.
    echo.
    echo. -- synthetic sample run failed. exiting...
    echo.
    cd %~dp0
    exit /B 1
)


cd %~dp0

exit /B 0

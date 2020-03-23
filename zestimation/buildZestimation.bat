:: NOTE: This windows batch script builds the Zestimation object files and the executable

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: build ParaMonte library and Zestimation object files and executables
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

echo off

cd %~dp0

echo.
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                                Zestimation Build
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
    set ERRORLEVEL=1
    exit /B 1
)
cd %~dp0

call configZestimation.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: Unable to configure and build Zestimation flags. exiting...
    echo. 
    cd %~dp0
    set ERRORLEVEL=1
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
    set ERRORLEVEL=1
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
    set ERRORLEVEL=1
    exit /B 1
)
cd %~dp0

:: add Kfactor correction if needed
set FPP_FLAGS_ZESTIMATION=/define:%LGRB_RATE_MODEL%
if %KFAC_CORRECTION%==OneThird (
    set FPP_FLAGS_ZESTIMATION=%FPP_FLAGS_ZESTIMATION% /define:KFAC_ONETHIRD_ENABLED
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
echo.
echo.
echo. -- Zestimation's Fortran preprocessor macros: %FPP_FLAGS_COSMIC_RATE%
echo.

:: set and make Zestimation directories
set ZESTIMATION_ROOT_PATH=%~dp0
set ZESTIMATION_DIR=%ZESTIMATION_ROOT_PATH%win%PLATFORM%\%COMPILER_SUITE%\%BTYPE%\%LTYPE%\%PARALLELIZATION_DIR%\kfac%KFAC_CORRECTION%\%LGRB_RATE_MODEL%
set ZESTIMATION_SRC_DIR=%ZESTIMATION_ROOT_PATH%src
set ZESTIMATION_BIN_DIR=%ZESTIMATION_DIR%\bin
set ZESTIMATION_LIB_DIR=%ZESTIMATION_DIR%\lib
set ZESTIMATION_LIB_MOD_DIR=%ZESTIMATION_LIB_DIR%\mod
set ZESTIMATION_LIB_OBJ_DIR=%ZESTIMATION_LIB_DIR%\obj

:: loop over Zestimation directories and generate them
echo.
for %%A in (
    %ZESTIMATION_DIR%
    %ZESTIMATION_BIN_DIR%
    %ZESTIMATION_LIB_DIR%
    %ZESTIMATION_LIB_MOD_DIR%
    %ZESTIMATION_LIB_OBJ_DIR%
    ) do (  if exist %%A (
                echo. -- %%A already exists. skipping...
            ) else (
                echo. -- generating Zestimation directory: %%A
                mkdir %%A
            )
)
echo.

if %ZESTIMATION_OBJ_BUILD_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping Zestimation object files build...
    echo.
    goto LABEL_ZESTIMATION_EXE_BUILD_ENABLED
)

:: Read the name of each file from the ordered list of filenames in filelist.txt to compile
cd %ZESTIMATION_LIB_OBJ_DIR%
echo.
echo. -- building Zestimation program...

:: First verify the source filelist exists
set FILE_LIST=%ZESTIMATION_SRC_DIR%\filelist.txt
if not exist %FILE_LIST% (
    echo.
    echo. -- Fatal Error: The filelist.txt containing the Zestimation source filenames does not exist. Path: %FILE_LIST%
    echo. -- build failed. exiting...
    echo.
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

:: generate object files
for /F "eol=! tokens=*" %%A in (%FILE_LIST%) do (

    echo. -- generating object file for %%A

    %FCL% %FCL_FLAGS% %FPP_FLAGS% %FPP_FLAGS_ZESTIMATION% ^
    /module:%ZESTIMATION_LIB_MOD_DIR%          %=path to output Zestimation module files=% ^
    /I:%ZESTIMATION_LIB_MOD_DIR%               %=path to output Zestimation module files, needed 4 dependencies=%  ^
    /I:%ASTRO_LIB_MOD_DIR%                     %=path to input Astronomy library module files=%  ^
    /I:%MOD_DIR%                               %=path to input ParaMonte module files=%  ^
    /c %ZESTIMATION_SRC_DIR%\%%A               %=path to input Zestimation source file=%  ^
    || (
        echo. 
        echo. -- Fatal Error: compilation of the object file for %%A failed.
        echo. -- build failed. exiting...
        echo. 
        set ERRORLEVEL=1
        cd %~dp0
        set ERRORLEVEL=1
        exit /B
    )
)
echo.
:LABEL_ZESTIMATION_EXE_BUILD_ENABLED

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: generate Zestimation executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if %ZESTIMATION_EXE_BUILD_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping Zestimation exectuable build...
    echo.
    goto LABEL_ZESTIMATION_RUN_ENABLED
)

echo.

if %LTYPE%==dynamic (

    echo.
    echo. -- Warning: dynamically-linked Zestimation executable not implemented. This requires significant changes in the library interfaces.
    echo. -- generating statically-linked Zestimation executable at: %ZESTIMATION_BIN_DIR%
    echo.
    
    set ZESTIMATION_EXECUTABLE_NAME=%LGRB_RATE_MODEL%.exe
    set REQUIRED_OBJECT_FILES=%ZESTIMATION_LIB_OBJ_DIR%\*.obj %OBJ_DIR%\*.obj

REM    :: copy necessary DLL files in the Zestimation executable's directory
REM    echo. copying DLL files for the Zestimation executable
REM    echo. from: %DLL_DIR%       %= no need for final slash here =%
REM    echo.   to: %ZESTIMATION_BIN_DIR%  %= final slash tells this is folder =%
REM    xcopy /s /Y "%DLL_DIR%" "%ZESTIMATION_BIN_DIR%\"
REM    echo.
REM
REM    echo. -- generating dynamically-linked Zestimation executable at: %ZESTIMATION_BIN_DIR%
REM
REM    set ZESTIMATION_EXECUTABLE_NAME=LGRBParaMonteDynamic.exe
REM    set REQUIRED_OBJECT_FILES=%ZESTIMATION_LIB_OBJ_DIR%\*.obj

) else (    %= static linking requested =%

    echo. -- generating statically-linked Zestimation executable at: %ZESTIMATION_BIN_DIR%

    set ZESTIMATION_EXECUTABLE_NAME=%LGRB_RATE_MODEL%.exe
    set REQUIRED_OBJECT_FILES=%ZESTIMATION_LIB_OBJ_DIR%\*.obj %ASTRO_LIB_OBJ_DIR%\*.obj %OBJ_DIR%\*.obj

)

:: delete the old executable first
echo. deleting old executable (if any) at: %ZESTIMATION_BIN_DIR%\%ZESTIMATION_EXECUTABLE_NAME%

cd %ZESTIMATION_BIN_DIR%
del %ZESTIMATION_EXECUTABLE_NAME%
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- Fatal Error: deletion of the old executable at %ZESTIMATION_BIN_DIR%\%ZESTIMATION_EXECUTABLE_NAME% failed. exiting...
    echo. 
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

:: build the executable
%FCL% %FCL_FLAGS% %FL_FLAGS% ^
/module:%ZESTIMATION_LIB_MOD_DIR% ^
/I:%ZESTIMATION_LIB_MOD_DIR% /I:%ASTRO_LIB_MOD_DIR% /I:%MOD_DIR% ^
%REQUIRED_OBJECT_FILES% ^
/exe:%ZESTIMATION_BIN_DIR%\%ZESTIMATION_EXECUTABLE_NAME%

if %ERRORLEVEL%==1 ( 
    echo. 
    echo. -- Fatal Error: linking of the Zestimation object files may have likely failed.
    echo. -- build may have likely failed. continuing...
    echo. 
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

echo.

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: run Zestimation executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:LABEL_ZESTIMATION_RUN_ENABLED

:: run Zestimation
:: if %ZESTIMATION_RUN_ENABLED% NEQ TRUE goto LABEL_EXAMPLE_BUILD_ENABLED
if %ZESTIMATION_RUN_ENABLED% NEQ TRUE (
    echo.
    echo. -- Warning: skipping Zestimation run...
    echo.
    goto :eof
)

echo. 
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                             Running Zestimation
echo. ::::                                                                                                                       ::::
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo.

:: copy necessary input files in the executable's directory
echo. copying input files to the Zestimation executable's directory
echo. from: %ZESTIMATION_ROOT_PATH%\in   %= no need for final slash here =%
echo.   to: %ZESTIMATION_BIN_DIR%\in\  %= final slash tells this is folder =%
xcopy /s /Y "%ZESTIMATION_ROOT_PATH%\in" "%ZESTIMATION_BIN_DIR%\in\"

REM set CHAIN_FILE_NAME=%~dp0..\cosmicRate\win%PLATFORM%\%COMPILER_SUITE%\%BTYPE%\%LTYPE%\%PARALLELIZATION_DIR%\kfac%KFAC_CORRECTION%%LGRB_RATE_MODEL%\bin\out\%LGRB_RATE_MODEL%_image_1_chain.txt
set CHAIN_FILE_NAME=%~dp0..\cosmicRate\win%PLATFORM%\%COMPILER_SUITE%\%BTYPE%\%LTYPE%\%PARALLELIZATION_DIR%\kfac%KFAC_CORRECTION%%LGRB_RATE_MODEL%\bin\out\%LGRB_RATE_MODEL%_image_1_chain.txt
echo.
echo.
echo. -- chain file name: %CHAIN_FILE_NAME%
echo.

cd %ZESTIMATION_BIN_DIR%
%ZESTIMATION_EXECUTABLE_NAME% ./in/ Zestimation.nml %CHAIN_FILE_NAME% && ( 
    echo.
    echo.
    echo. -- Zestimation run successful. 
    echo.
) || ( 
    echo.
    echo.
    echo. -- Zestimation run failed. exiting...
    echo.
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)


cd %~dp0

set ERRORLEVEL=0
exit /B 0

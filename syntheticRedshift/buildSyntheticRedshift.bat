:: NOTE: This windows batch script builds the SyntheticRedshift object files and the executable

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: build ParaMonte library and SyntheticRedshift object files and executables
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

echo off

cd %~dp0

echo.
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                                SyntheticRedshift Build
echo. ::::                                                                                                                       ::::
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo.

echo. 
echo. Configuring build...
echo. 

set ParaMonte_ROOT_RELATIVE_PATH=..\..\..\20180101_ParaMonte\git

:: clean already existing variables first
cd %ParaMonte_ROOT_RELATIVE_PATH%
call unconfigParaMonte.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- SyntheticRedshift - Fatal Error: Unable to UNconfigure ParaMonte library flags. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

call configSyntheticRedshift.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- SyntheticRedshift - Fatal Error: Unable to configure and build flags. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

echo. 
echo. Configuring ParaMonte library build...
echo. 

:: change directory to ParaMonte's root dir
cd %ParaMonte_ROOT_RELATIVE_PATH%
call buildParaMonte.bat
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- SyntheticRedshift - Fatal Error: Unable to configure and build ParaMonte Library. exiting...
    echo. 
    cd %~dp0
    exit /B 1
)
cd %~dp0

:: set the executable's name
set %SyntheticRedshift_FPP_FLAGS%==
if %SyntheticRedshift_RATE_TYPE%==StarFormationRateDensity (
    set SyntheticRedshift_FPP_FLAGS=/define:SFR_DENSITY_ENABLED
) else (
    if %SyntheticRedshift_RATE_TYPE% NEQ StarFormationRate (
        echo.
        echo. -- SyntheticRedshift - Fatal Error: the requested SyntheticRedshift_RATE_TYPE is not supported: %SyntheticRedshift_RATE_TYPE%
        echo. -- SyntheticRedshift - build failed. exiting...
        echo.
        cd %~dp0
        set ERRORLEVEL=1
        exit /B 1
    )
)
set SyntheticRedshift_EXE_NAME=%SyntheticRedshift_PNAME%%SyntheticRedshift_RATE_TYPE%.exe
echo.
echo. -- SyntheticRedshift - Fortran preprocessor macros: %SyntheticRedshift_FPP_FLAGS%
echo. -- SyntheticRedshift - executable name: %SyntheticRedshift_EXE_NAME%
echo.

:: set and make SyntheticRedshift directories
set SyntheticRedshift_ROOT_PATH=%~dp0
set SyntheticRedshift_DIR=%SyntheticRedshift_ROOT_PATH%win%PLATFORM%\%COMPILER_SUITE%\%COMPILER_VERSION%\%BTYPE%\%LTYPE%\%PARALLELIZATION_DIR%
set SyntheticRedshift_SRC_DIR=%SyntheticRedshift_ROOT_PATH%src
set SyntheticRedshift_BIN_DIR=%SyntheticRedshift_DIR%\bin
set SyntheticRedshift_MOD_DIR=%SyntheticRedshift_DIR%\mod
set SyntheticRedshift_OBJ_DIR=%SyntheticRedshift_DIR%\obj

:: loop over SyntheticRedshift directories and generate them
echo.
for %%A in (
    %SyntheticRedshift_DIR%
    %SyntheticRedshift_BIN_DIR%
    %SyntheticRedshift_DIR%
    %SyntheticRedshift_MOD_DIR%
    %SyntheticRedshift_OBJ_DIR%
    ) do (  if exist %%A (
                echo. -- %%A already exists. skipping...
            ) else (
                echo. -- SyntheticRedshift - generating directory: %%A
                mkdir %%A
            )
)
echo.

if %SyntheticRedshift_OBJ_ENABLED% NEQ TRUE (
    echo.
    echo. -- SyntheticRedshift - Warning: skipping object files build...
    echo.
    goto LABEL_SyntheticRedshift_EXE_ENABLED
)

:: Read the name of each file from the ordered list of filenames in filelist.txt to compile
cd %SyntheticRedshift_OBJ_DIR%
echo.
echo. -- SyntheticRedshift - building for the rate model of %SyntheticRedshift_RATE_MODEL%...

:: First verify the source filelist exists
set SyntheticRedshift_FILE_LIST=%SyntheticRedshift_SRC_DIR%\filelist.txt
if not exist %SyntheticRedshift_FILE_LIST% (
    echo.
    echo. -- SyntheticRedshift - Fatal Error: The filelist.txt containing the source filenames does not exist. Path: %SyntheticRedshift_FILE_LIST%
    echo. -- SyntheticRedshift - build failed. exiting...
    echo.
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

:: generate object files
for /F "eol=! tokens=*" %%A in (%SyntheticRedshift_FILE_LIST%) do (

    echo. -- SyntheticRedshift - generating object file for %%A

    %FCL% %FCL_FLAGS% %FPP_FLAGS% %SyntheticRedshift_FPP_FLAGS% ^
    /module:%SyntheticRedshift_MOD_DIR%                 %=path to output SyntheticRedshift module files=% ^
    /I:%ParaMonte_MOD_DIR%                              %=path to input ParaMonte module files=%  ^
    /I:%SyntheticRedshift_MOD_DIR%                      %=path to output SyntheticRedshift module files, needed 4 dependencies=%  ^
    /c %SyntheticRedshift_SRC_DIR%\%%A                  %=path to input SyntheticRedshift source file=%  ^
    || (
        echo. 
        echo. -- SyntheticRedshift - Fatal Error: compilation of the object file for %%A failed.
        echo. -- SyntheticRedshift - build failed. exiting...
        echo. 
        set ERRORLEVEL=1
        cd %~dp0
        set ERRORLEVEL=1
        exit /B
    )
)
echo.

:LABEL_SyntheticRedshift_EXE_ENABLED

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: generate SyntheticRedshift executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if %SyntheticRedshift_EXE_ENABLED% NEQ TRUE (
    echo.
    echo. -- SyntheticRedshift - Warning: skipping exectuable build...
    echo.
    goto LABEL_SyntheticRedshift_RUN_ENABLED
)

echo.

if %LTYPE%==dynamic (

    echo.
    echo. -- SyntheticRedshift - Warning: dynamically-linked executable not implemented. This requires significant changes in the library interfaces.
    echo. -- SyntheticRedshift - generating statically-linked executable at: %SyntheticRedshift_BIN_DIR%
    echo.
    
    REM set SyntheticRedshift_EXE_NAME=%SyntheticRedshift_PNAME%.exe
    set SyntheticRedshift_REQUIRED_OBJECT_FILES=%SyntheticRedshift_OBJ_DIR%\*.obj %ParaMonte_OBJ_DIR%\*.obj

REM    :: copy necessary DLL files in the executable's directory
REM    echo. -- SyntheticRedshift - copying DLL files for the executable
REM    echo. -- SyntheticRedshift - from: %DLL_DIR%       %= no need for final slash here =%
REM    echo. -- SyntheticRedshift -  to: %SyntheticRedshift_BIN_DIR%  %= final slash tells this is folder =%
REM    xcopy /s /Y "%DLL_DIR%" "%SyntheticRedshift_BIN_DIR%\"
REM    echo.
REM
REM    echo. -- SyntheticRedshift - generating dynamically-linked executable at: %SyntheticRedshift_BIN_DIR%
REM
REM    set SyntheticRedshift_EXE_NAME=LGRBParaMonteDynamic.exe
REM    set SyntheticRedshift_REQUIRED_OBJECT_FILES=%SyntheticRedshift_OBJ_DIR%\*.obj

) else (    %= static linking requested =%

    echo. -- SyntheticRedshift - generating statically-linked executable at: %SyntheticRedshift_BIN_DIR%

    REM set SyntheticRedshift_EXE_NAME=%SyntheticRedshift_RATE_MODEL%.exe
    set SyntheticRedshift_REQUIRED_OBJECT_FILES=%SyntheticRedshift_OBJ_DIR%\*.obj %ParaMonte_OBJ_DIR%\*.obj

)

:: delete the old executable first
echo. -- SyntheticRedshift - deleting old executable (if any) at: %SyntheticRedshift_BIN_DIR%\%SyntheticRedshift_EXE_NAME%

cd %SyntheticRedshift_BIN_DIR%
del %SyntheticRedshift_EXE_NAME%
if %ERRORLEVEL%==1 (
    echo. 
    echo. -- SyntheticRedshift - Fatal Error: deletion of the old executable at %SyntheticRedshift_BIN_DIR%\%SyntheticRedshift_EXE_NAME% failed. exiting...
    echo. 
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

:: build the executable
%FCL% %FCL_FLAGS% %FL_FLAGS% ^
/module:%SyntheticRedshift_MOD_DIR% ^
/I:%SyntheticRedshift_MOD_DIR% /I:%ParaMonte_MOD_DIR% ^
%SyntheticRedshift_REQUIRED_OBJECT_FILES% ^
/exe:%SyntheticRedshift_BIN_DIR%\%SyntheticRedshift_EXE_NAME%

if %ERRORLEVEL%==1 ( 
    echo. 
    echo. -- SyntheticRedshift - Fatal Error: linking of the object files might have failed.
    echo. -- SyntheticRedshift - build might have failed. continuing...
    echo. 
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)

echo.

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: run SyntheticRedshift executable
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:LABEL_SyntheticRedshift_RUN_ENABLED

:: run SyntheticRedshift
:: if %SyntheticRedshift_RUN_ENABLED% NEQ TRUE goto LABEL_EXAMPLE_ENABLED
if %SyntheticRedshift_RUN_ENABLED% NEQ TRUE (
    echo.
    echo. -- SyntheticRedshift - Warning: skipping the executable run...
    echo.
    goto :eof
)

echo. 
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo. ::::                                                                                                                       ::::
echo.                                             Running SyntheticRedshift executable
echo. ::::                                                                                                                       ::::
echo. :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
echo.

:: copy necessary input files in the executable's directory
echo. -- SyntheticRedshift - copying input files to the executable's directory
echo. -- SyntheticRedshift - from: %SyntheticRedshift_ROOT_PATH%\in     %= no need for final slash here =%
echo. -- SyntheticRedshift -  to: %SyntheticRedshift_BIN_DIR%\in\       %= final slash tells this is folder =%
xcopy /s /Y "%SyntheticRedshift_ROOT_PATH%\in\%SyntheticRedshift_PNAME%.nml" "%SyntheticRedshift_BIN_DIR%\in\"
echo.
echo. -- SyntheticRedshift - copying BATSE data to the executable's directory
echo. -- SyntheticRedshift - from: %SyntheticRedshift_ROOT_PATH%\in\BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt   %= no need for final slash here =%
echo. -- SyntheticRedshift -  to: %SyntheticRedshift_BIN_DIR%\in\  %= final slash tells this is folder =%
xcopy /s /Y "%SyntheticRedshift_ROOT_PATH%\in\BATSE_1366_LGRB_P1024ph_Epk_Sch23ph.txt" "%SyntheticRedshift_BIN_DIR%\in\"
echo.

cd %SyntheticRedshift_BIN_DIR%
%SyntheticRedshift_EXE_NAME% .\in\ %SyntheticRedshift_PNAME%.nml && ( 
    echo.
    echo.
    echo. -- SyntheticRedshift - executable run successful. 
    echo.
) || ( 
    echo.
    echo.
    echo. -- SyntheticRedshift - executable run failed. exiting...
    echo.
    cd %~dp0
    set ERRORLEVEL=1
    exit /B 1
)


cd %~dp0

set ERRORLEVEL=0
exit /B 0

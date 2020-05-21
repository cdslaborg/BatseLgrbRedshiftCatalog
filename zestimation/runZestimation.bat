:: NOTE: This windows batch script calls the build scripts and runs the simulations

@echo off
cd %~dp0
set ERRORLEVEL=0

for %%V in ( "H06" "L08" "B10" "M14" "M17" "F18" ) do ( 
    set "LGRB_RATE_MODEL=%%~V"
    call buildZestimation.bat
)

cd %~dp0

set ERRORLEVEL=0
exit /B 0

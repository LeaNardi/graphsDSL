@echo off
set "TARGET_DIR=."

REM Remove .hi and .o files in the root of the target directory
@REM for /f %%D in ("%TARGET_DIR%\*.hi") do (
@REM     if exist "%%D" del /q "%%D"
@REM )
@REM for /f %%D in ("%TARGET_DIR%\*.o") do (
@REM     if exist "%%D" del /q "%%D"
@REM )
REM Remove .hi and .o files in immediate subfolders (1 level deep)
for /d %%D in ("%TARGET_DIR%\*") do (
    if exist "%%D\*.hi" del /q "%%D\*.hi"
    if exist "%%D\*.o" del /q "%%D\*.o"
)

echo Cleanup complete 
pause

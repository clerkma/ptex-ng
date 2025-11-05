@echo off

for /f "usebackq tokens=*" %%i in (
`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"^
 -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.*^
 -property installationPath`) do (
  set InstallDir=%%i
)
set VSENVBAT="%InstallDir%\VC\Auxiliary\Build\vcvars64.bat"
if exist %VSENVBAT% (
    call %VSENVBAT%
    exit /b 1
)

echo "I cannot found compiler. Please make sure that you have installed it."
@echo on

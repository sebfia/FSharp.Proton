@echo off
cls

if errorlevel 1 (
  exit /b %errorlevel%
)

dotnet paket restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\build\FAKE\tools\FAKE.exe build.fsx %*

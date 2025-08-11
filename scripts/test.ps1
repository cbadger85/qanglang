# PowerShell script to run qang language tests
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$command = "$scriptDir\..\target\release\qang.exe test $scriptDir\..\tests"

Write-Host "Running qang language tests..."

# Execute the test command
& cmd /c $command
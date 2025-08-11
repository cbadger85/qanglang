# PowerShell script to run qang language tests
$command = "..\target\release\qang.exe test ..\tests"

Write-Host "Running qang language tests..."

# Execute the test command
& cmd /c $command
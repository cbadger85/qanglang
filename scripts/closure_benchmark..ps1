# PowerShell script to benchmark closure performance 30 times
$times = @()
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$command = "$scriptDir\..\target\release\qang.exe run $scriptDir\..\examples\closure_bench.ql"

Write-Host "Running closure benchmark 30 times..."

for ($i = 1; $i -le 30; $i++) {
    Write-Host "Run $i/30"
    $output = & cmd /c $command 2>&1
    $outputString = $output -join "`n"
    
    # Extract time from output using regex - try multiple patterns
    if ($outputString -match "Closure benchmark completed in (\d+) ms") {
        $time = [int]$matches[1]
        $times += $time
        Write-Host "  Time: $time ms"
    } elseif ($outputString -match "completed in (\d+) ms") {
        $time = [int]$matches[1]
        $times += $time
        Write-Host "  Time: $time ms"
    } elseif ($outputString -match "(\d+) ms") {
        $time = [int]$matches[1]
        $times += $time
        Write-Host "  Time: $time ms"
    } else {
        Write-Host "  Warning: Could not extract time from output:"
        Write-Host "  Output: $outputString"
    }
}

if ($times.Count -gt 0) {
    $average = ($times | Measure-Object -Average).Average
    $min = ($times | Measure-Object -Minimum).Minimum
    $max = ($times | Measure-Object -Maximum).Maximum
    $median = ($times | Sort-Object)[[Math]::Floor($times.Count / 2)]
    
    Write-Host "`nClosure Benchmark Results:"
    Write-Host "Runs completed: $($times.Count)"
    Write-Host "Average time: $([math]::Round($average, 2)) ms"
    Write-Host "Median time: $median ms"
    Write-Host "Min time: $min ms"
    Write-Host "Max time: $max ms"
    Write-Host "Standard deviation: $([math]::Round(($times | Measure-Object -StandardDeviation).StandardDeviation, 2)) ms"
    Write-Host "All times: $($times -join ', ') ms"
    
    # Compare with regular fib benchmark if available
    Write-Host "`nTo compare with regular function calls, run the fib benchmark:"
    Write-Host ".\scripts\benchmark.ps1"
} else {
    Write-Host "No valid times were captured"
}
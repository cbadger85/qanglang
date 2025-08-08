# PowerShell script to benchmark cargo run command 30 times
$times = @()
$command = "cargo run --release -- run fib.ql"

Write-Host "Running benchmark 30 times..."

for ($i = 1; $i -le 30; $i++) {
    Write-Host "Run $i/30"
    $output = & cmd /c $command 2>&1
    $outputString = $output -join "`n"
    
    # Extract time from output using regex - try multiple patterns
    if ($outputString -match "Completed in (\d+) ms") {
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
    
    Write-Host "`nResults:"
    Write-Host "Runs completed: $($times.Count)"
    Write-Host "Average time: $([math]::Round($average, 2)) ms"
    Write-Host "Min time: $min ms"
    Write-Host "Max time: $max ms"
    Write-Host "All times: $($times -join ', ') ms"
} else {
    Write-Host "No valid times were captured"
}
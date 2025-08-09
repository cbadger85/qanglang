#!/bin/bash

# Bash script to benchmark cargo run command 30 times
times=()
command="../target/release/qang run f../examples/ib.ql"

echo "Running benchmark 30 times..."

for i in {1..30}; do
    echo "Run $i/30"
    output=$($command 2>&1)
    
    # Extract time from output using regex
    if [[ $output =~ Completed\ in\ ([0-9]+)\ ms ]]; then
        time=${BASH_REMATCH[1]}
        times+=($time)
        echo "  Time: $time ms"
    elif [[ $output =~ ([0-9]+)\ ms ]]; then
        time=${BASH_REMATCH[1]}
        times+=($time)
        echo "  Time: $time ms"
    else
        echo "  Warning: Could not extract time from output:"
        echo "  Output: $output"
    fi
done

if [ ${#times[@]} -gt 0 ]; then
    # Calculate average
    sum=0
    min=${times[0]}
    max=${times[0]}
    
    for time in "${times[@]}"; do
        sum=$((sum + time))
        if [ $time -lt $min ]; then
            min=$time
        fi
        if [ $time -gt $max ]; then
            max=$time
        fi
    done
    
    average=$((sum / ${#times[@]}))
    
    echo
    echo "Results:"
    echo "Runs completed: ${#times[@]}"
    echo "Average time: $average ms"
    echo "Min time: $min ms"
    echo "Max time: $max ms"
    echo "All times: ${times[*]} ms"
else
    echo "No valid times were captured"
fi
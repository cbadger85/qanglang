#!/bin/bash
# Bash script to run qang language tests

script_dir="$(dirname "${BASH_SOURCE[0]}")"

echo "Running qang language tests..."

# Execute the test command
"$script_dir/../target/release/qang" test "$script_dir/../tests"
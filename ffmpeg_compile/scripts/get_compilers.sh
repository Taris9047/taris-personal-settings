#!/bin/bash

# Define colors for output formatting
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Arrays of common compiler commands grouped by language
declare -A COMPILERS
COMPILERS=(
    ["C (gcc)"]="gcc"
    ["C (clang)"]="clang"
    ["C (cc)"]="cc"
    ["C++ (g++)"]="g++"
    ["C++ (clang++)"]="clang++"
    ["Fortran (gfortran)"]="gfortran"
    ["Go"]="go"
    ["Rust (rustc)"]="rustc"
    ["Java (javac)"]="javac"
)

echo "========================================"
echo " Checking for available system compilers"
echo "========================================"

found_any=false

# Loop through the associative array and verify status
for name in "${!COMPILERS[@]}"; do
    cmd=${COMPILERS[$name]}
    
    # Check if the command exists in the system PATH
    if command -v "$cmd" >/dev/null 2>&1; then
        # Get the first line of the version output
        version=$($cmd --version 2>&1 | head -n 1)
        printf "${GREEN}[FOUND]${NC} %-18s : %s\n" "$name" "$version"
        found_any=true
    else
        printf "${RED}[MISSING]${NC} %-18s\n" "$name"
    fi
done

echo "========================================"
if [ "$found_any" = true ]; then
    echo "Scan complete. Ready to compile."
else
    echo "Warning: No standard compilers were found in your PATH."
fi

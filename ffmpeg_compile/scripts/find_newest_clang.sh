#!/bin/bash

# Function to find the highest version of a compiler command pattern
# Examples: "clang-[0-9]*" or "gcc-[0-9]*"
find_newest_compiler() {
    local prefix="$1"
    local highest_version="-1"
    local chosen_binary=""

    # 1. Check the unversioned base command first (e.g., 'clang' or 'gcc')
    if command -v "$prefix" >/dev/null 2>&1; then
        chosen_binary=$(command -v "$prefix")
        # Extract version number using a standardized flag
        local ver_output
        ver_output=$("$prefix" -dumpversion 2>/dev/null || "$prefix" --version 2>/dev/null | head -n 1)
        highest_version=$(echo "$ver_output" | grep -oE '[0-9]+(\.[0-9]+)*' | head -n 1)
    fi

    # 2. Scan $PATH for explicitly versioned binaries (e.g., clang-14, gcc-13)
    # This ensures we find a newer version even if the default symlink points to an older one
    IFS=:
    for dir in $PATH; do
        if [ -d "$dir" ]; then
            # Find files matching the compiler prefix followed by a version number
            for file in "$dir"/"$prefix"-[0-9]*; do
                if [ -x "$file" ]; then
                    # Extract only the trailing version suffix
                    local suffix="${file##*/$prefix-}"
                    
                    # Basic version comparison (Primary focus on major version numbers)
                    if [ "$(echo -e "$suffix\n$highest_version" | sort -V | tail -n 1)" != "$highest_version" ] || [ "$highest_version" == "-1" ]; then
                        highest_version="$suffix"
                        chosen_binary="$file"
                    fi
                fi
            done
        fi
    done
    unset IFS

    # Output the result if a binary was found
    if [ -n "$chosen_binary" ]; then
        echo "$chosen_binary:$highest_version"
    fi
}

echo "Searching for best available C/C++ compiler..."
echo "------------------------------------------------"

# Step 1: Look for Clang variants
CLANG_INFO=$(find_newest_compiler "clang")

if [ -n "$CLANG_INFO" ]; then
    CC_PATH="${CLANG_INFO%%:*}"
    CC_VER="${CLANG_INFO#*:}"
    echo -e "Success: Found Clang as preferred compiler."
    echo -e "Path:    $CC_PATH"
    echo -e "Version: $CC_VER"
    
# Step 2: Fallback to GCC variants
else
    echo "Clang not found. Falling back to GCC..."
    GCC_INFO=$(find_newest_compiler "gcc")
    
    if [ -n "$GCC_INFO" ]; then
        CC_PATH="${GCC_INFO%%:*}"
        CC_VER="${GCC_INFO#*:}"
        echo -e "Success: Found GCC fallback compiler."
        echo -e "Path:    $CC_PATH"
        echo -e "Version: $CC_VER"
    else
        echo "Error: Neither Clang nor GCC could be found on this system."
        exit 1
    fi
fi

echo "------------------------------------------------"
# Export the chosen binary paths for your build environment
export CC="$CC_PATH"
export CXX="${CC_PATH/clang/clang++}" # Automatically maps to C++ equivalent
export CXX="${CXX/gcc/g++}"

echo "Environment configured: CC=\$CC ($CC)"

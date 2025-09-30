#!/bin/bash

# E = Ic² Framework Runner

echo "================================================"
echo "     E = Ic² Information-Energy Framework"
echo "================================================"
echo ""

# Check if ghc is available
if ! command -v ghc &> /dev/null; then
    echo "GHC not found. Please install GHC to compile the framework."
    echo "Visit: https://www.haskell.org/ghcup/"
    exit 1
fi

echo "Building framework..."
make clean > /dev/null 2>&1
make > /dev/null 2>&1

if [ $? -eq 0 ]; then
    echo "Build successful!"
    echo ""
    echo "Running E = Ic² demonstration..."
    echo "================================================"
    ./info-energy
else
    echo "Build failed. Attempting simplified compilation..."
    ghc -o info-energy-simple src/Main.hs -isrc 2>/dev/null
    if [ $? -eq 0 ]; then
        ./info-energy-simple
    else
        echo "Unable to compile. Please check your GHC installation."
    fi
fi
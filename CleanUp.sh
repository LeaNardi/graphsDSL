#!/bin/bash

# Set the folder you want to clean
TARGET_DIR="."

echo "Cleaning .hi and .o files in: $TARGET_DIR"

# Remove .hi and .o files in the target directory
find . -name "*.hi" -delete
find . -name "*.o" -delete

echo "Cleanup complete"

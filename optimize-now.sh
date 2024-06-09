#!/bin/bash

# Define the directory containing the optimization scripts
OPTIMIZE_DIR=~/local-optimize

# Function to run an optimization script if it exists
run_optimization_script() {
    local script_name=$1
    if [ -f "$OPTIMIZE_DIR/$script_name" ]; then
        echo "Running optimization script: $script_name"
        chmod +x "$OPTIMIZE_DIR/$script_name"
        "$OPTIMIZE_DIR/$script_name"
    else
        echo "Optimization script not found: $script_name"
    fi
}

# List of optimization scripts to run
scripts=(
    "compile_gcc.sh"
    "compile_make.sh"
    "compile_python3.sh"
    "compile_node.sh"
    "compile_ffmpeg.sh"
    "compile_nginx.sh"
    "compile_r.sh"
    "compile_emacs.sh"
    "compile_sbcl.sh"
    "compile_firefox.sh"
)

# Run each optimization script
for script in "${scripts[@]}"; do
    run_optimization_script "$script"
done

echo "All optimization scripts have been run."

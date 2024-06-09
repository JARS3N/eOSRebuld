#!/bin/bash

# Install dependencies
sudo apt-get install -y cmake python3

# Download LLVM source code
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
mkdir build && cd build

# Configure, compile, and install
cmake -DLLVM_ENABLE_PROJECTS=clang -G "Unix Makefiles" ../llvm
make -j$(nproc)
sudo make install

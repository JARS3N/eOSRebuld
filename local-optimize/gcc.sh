#!/bin/bash

# Ensure prerequisites are installed
sudo apt-get install -y build-essential wget

# Download the latest source code
latest_version=$(wget -qO- ftp://ftp.gnu.org/gnu/gcc/ | grep -oP 'gcc-\K[0-9.]+(?=\/)' | sort -V | tail -1)
wget ftp://ftp.gnu.org/gnu/gcc/gcc-$latest_version/gcc-$latest_version.tar.gz
tar -xzf gcc-$latest_version.tar.gz
cd gcc-$latest_version

# Install dependencies
./contrib/download_prerequisites

# Create a build directory and configure
mkdir build && cd build
../configure --prefix=/usr/local/gcc-$latest_version --enable-languages=c,c++,fortran

# Compile and install
make -j$(nproc)
sudo make install

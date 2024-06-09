#!/bin/bash

# Download the latest source code
latest_version=$(wget -qO- ftp://ftp.gnu.org/gnu/make/ | grep -oP 'make-\K[0-9.]+(?=\.tar\.gz)' | sort -V | tail -1)
wget ftp://ftp.gnu.org/gnu/make/make-$latest_version.tar.gz
tar -xzf make-$latest_version.tar.gz
cd make-$latest_version

# Configure, compile, and install
./configure --prefix=/usr/local/make-$latest_version
make -j$(nproc)
sudo make install

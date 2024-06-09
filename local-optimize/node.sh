#!/bin/bash

# Download the latest source code
latest_version=$(wget -qO- https://nodejs.org/dist/latest/ | grep -oP 'node-v\K[0-9.]+(?=\.tar\.gz)' | head -1)
wget https://nodejs.org/dist/latest/node-v$latest_version.tar.gz
tar -xzf node-v$latest_version.tar.gz
cd node-v$latest_version

# Configure, compile, and install
./configure
make -j$(nproc)
sudo make install

#!/bin/bash

# Download the latest source code
latest_version=$(wget -qO- https://www.python.org/ftp/python/ | grep -oP 'href="\K[0-9.]+(?=/")' | sort -V | tail -1)
wget https://www.python.org/ftp/python/$latest_version/Python-$latest_version.tgz
tar -xzf Python-$latest_version.tgz
cd Python-$latest_version

# Configure, compile, and install
./configure --enable-optimizations
make -j$(nproc)
sudo make altinstall


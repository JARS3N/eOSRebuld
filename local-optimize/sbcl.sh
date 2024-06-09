#!/bin/bash

# Download the latest source code
latest_version=$(wget -qO- http://sourceforge.net/projects/sbcl/files/sbcl/ | grep -oP 'sbcl-\K[0-9.]+(?=\/)' | sort -V | tail -1)
wget http://sourceforge.net/projects/sbcl/files/sbcl/$latest_version/sbcl-$latest_version-source.tar.bz2
tar -xjf sbcl-$latest_version-source.tar.bz2
cd sbcl-$latest_version

# Compile and install
sh make.sh
sudo sh install.sh

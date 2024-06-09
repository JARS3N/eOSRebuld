#!/bin/bash

# Install dependencies
sudo apt-get update
sudo apt-get install -y build-essential libgtk-3-dev libgnutls28-dev libjpeg-dev libtiff-dev libgif-dev libxpm-dev libncurses-dev texinfo autoconf wget

# Get the latest Emacs version number
latest_version=$(wget -qO- https://ftp.gnu.org/gnu/emacs/ | grep -oP 'emacs-\d+\.\d+\.tar\.gz' | sort -V | tail -1 | grep -oP '\d+\.\d+')

# Download the latest Emacs source code
wget https://ftp.gnu.org/gnu/emacs/emacs-${latest_version}.tar.gz

# Extract the tarball
tar -xzf emacs-${latest_version}.tar.gz
cd emacs-${latest_version}

# Generate the configuration files
./autogen.sh

# Configure the build with all features
./configure --with-all

# Compile the source code
make

# Install Emacs
sudo make install

# Verify the installation
emacs --version

# Clean up
make clean

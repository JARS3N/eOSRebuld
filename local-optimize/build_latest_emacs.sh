#!/bin/bash

# Install dependencies
sudo apt-get update
sudo apt-get install -y build-essential libgtk-3-dev libgnutls28-dev libjpeg-dev libtiff-dev libgif-dev libxpm-dev libncurses-dev texinfo autoconf wget

# Remove any existing Emacs installation
sudo apt-get remove --purge -y emacs emacs-gtk emacs-common emacs25 emacs26
sudo rm -rf /usr/local/bin/emacs-*
sudo rm -rf /usr/local/share/emacs-*
sudo rm -rf /usr/bin/emacs-*
sudo rm -rf /usr/bin/emacs

# Get the latest Emacs version number
latest_version=$(wget -qO- https://ftp.gnu.org/gnu/emacs/ | grep -o 'emacs-[0-9.]*\.tar\.gz' | sort -V | tail -1 | grep -o '[0-9.]*')

# Remove any trailing periods from the version
latest_version=$(echo "$latest_version" | sed 's/\.$//')

# Output the latest version (for debugging)
echo "Latest Emacs version: $latest_version"

# Check if the latest version is already downloaded
if [ -f emacs-${latest_version}.tar.gz ]; then
    echo "Latest version emacs-${latest_version}.tar.gz already downloaded."
else
    # Download the latest Emacs source code
    wget https://ftp.gnu.org/gnu/emacs/emacs-${latest_version}.tar.gz
fi

# Extract the tarball
if [ -d emacs-${latest_version} ]; then
    echo "Emacs ${latest_version} source directory already exists."
else
    tar -xzf emacs-${latest_version}.tar.gz
fi

cd emacs-${latest_version}

# Generate the configuration files if necessary
if [ -f ./autogen.sh ]; then
    ./autogen.sh
fi

# Configure the build with necessary options, specifying the installation prefix
./configure --prefix=/usr/local --with-x-toolkit=gtk3 --with-modules --with-cairo --with-harfbuzz

# Compile the source code
make

# Install Emacs
sudo make install

# Verify the installation
if ! /usr/local/bin/emacs --version; then
  echo "Emacs installation failed"
  exit 1
fi

# Clean up source files if desired
cd ..
rm -rf emacs-${latest_version}

# Locate the Emacs binary
emacs_binary="/usr/local/bin/emacs"


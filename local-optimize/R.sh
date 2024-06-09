#!/bin/bash

# Install dependencies
sudo apt-get install -y gfortran libreadline6-dev libx11-dev libxt-dev texinfo libbz2-dev liblzma-dev libpcre2-dev

# Download the latest source code
latest_version=$(wget -qO- https://cran.r-project.org/src/base/R-4/ | grep -oP 'R-\K[0-9.]+(?=\.tar\.gz)' | sort -V | tail -1)
wget https://cran.r-project.org/src/base/R-4/R-$latest_version.tar.gz
tar -xzf R-$latest_version.tar.gz
cd R-$latest_version

# Configure, compile, and install
./configure --enable-R-shlib --with-blas --with-lapack
make -j$(nproc)
sudo make install

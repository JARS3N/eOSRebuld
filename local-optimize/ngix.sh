#!/bin/bash

# Install dependencies
sudo apt-get install -y libpcre3 libpcre3-dev zlib1g zlib1g-dev libssl-dev

# Download the latest source code
latest_version=$(wget -qO- http://nginx.org/download/ | grep -oP 'nginx-\K[0-9.]+(?=\.tar\.gz)' | sort -V | tail -1)
wget http://nginx.org/download/nginx-$latest_version.tar.gz
tar -xzf nginx-$latest_version.tar.gz
cd nginx-$latest_version

# Configure, compile, and install
./configure --with-http_ssl_module --with-pcre
make -j$(nproc)
sudo make install

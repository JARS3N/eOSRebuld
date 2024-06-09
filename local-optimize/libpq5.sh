#!/bin/bash

# Install dependencies
sudo apt-get install -y libssl-dev libreadline-dev zlib1g-dev

# Download PostgreSQL source code
latest_version=$(wget -qO- https://ftp.postgresql.org/pub/source/ | grep -oP 'v\K[0-9.]+(?=/)' | sort -V | tail -1)
wget https://ftp.postgresql.org/pub/source/v$latest_version/postgresql-$latest_version.tar.gz
tar -xzf postgresql-$latest_version.tar.gz
cd postgresql-$latest_version

# Configure, compile, and install
./configure --with-openssl --with-readline
make -j$(nproc)
sudo make install

#!/bin/bash

# Install dependencies
sudo apt-get install -y yasm libx264-dev libvpx-dev libfdk-aac-dev libmp3lame-dev libopus-dev

# Download the latest source code
latest_version=$(wget -qO- https://ffmpeg.org/releases/ | grep -oP 'ffmpeg-\K[0-9.]+(?=\.tar\.bz2)' | sort -V | tail -1)
wget https://ffmpeg.org/releases/ffmpeg-$latest_version.tar.bz2
tar -xjf ffmpeg-$latest_version.tar.bz2
cd ffmpeg-$latest_version

# Configure, compile, and install
./configure --enable-gpl --enable-libx264 --enable-libvpx --enable-libfdk-aac --enable-libmp3lame --enable-libopus
make -j$(nproc)
sudo make install

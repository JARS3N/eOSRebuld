#!/bin/bash

# Install dependencies
sudo apt-get install -y build-essential autoconf2.13 yasm libasound2-dev libgtk-3-dev libdbus-glib-1-dev libpulse-dev \
                        libx11-xcb-dev libxt-dev libxcomposite-dev libxdamage-dev libxrandr-dev libxrender-dev \
                        libxinerama-dev libgl1-mesa-dev libegl1-mesa-dev libdrm-dev libxss-dev libpci-dev \
                        libnotify-dev libgnome-keyring-dev libevent-dev libvpx-dev libopus-dev libtheora-dev \
                        libogg-dev libvorbis-dev libva-dev libvdpau-dev libwebp-dev libharfbuzz-dev \
                        python3-dev python3-pip unzip

# Download the latest Firefox source code
mkdir -p ~/firefox-build && cd ~/firefox-build
latest_firefox=$(wget -qO- https://download.mozilla.org/?product=firefox-latest&os=linux64&lang=en-US | grep -oP 'firefox-\K[0-9.]+' | head -1)
wget https://archive.mozilla.org/pub/firefox/releases/$latest_firefox/source/firefox-$latest_firefox.source.tar.xz
tar -xf firefox-$latest_firefox.source.tar.xz
cd firefox-$latest_firefox

# Bootstrap the build system
./mach bootstrap

# Build Firefox
./mach build

# Install Firefox
sudo ./mach install

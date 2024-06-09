#!/bin/bash

# Ensure the script is run as root
if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root"
    exit 1
fi

echo "Updating package lists and installing necessary tools..."
apt update
apt install -y build-essential gfortran

# Add the NVIDIA PPA
echo "Adding NVIDIA PPA..."
add-apt-repository ppa:graphics-drivers/ppa
apt update

# Install the recommended NVIDIA driver
echo "Installing recommended NVIDIA driver..."
ubuntu-drivers autoinstall

# Blacklist Nouveau driver
echo "Blacklisting Nouveau driver..."
echo -e "blacklist nouveau\noptions nouveau modeset=0" > /etc/modprobe.d/blacklist-nouveau.conf

# Regenerate initramfs
echo "Regenerating initramfs..."
update-initramfs -u

# Create or update xorg.conf
echo "Creating or updating /etc/X11/xorg.conf..."
cat <<EOL > /etc/X11/xorg.conf
Section "Device"
    Identifier "Nvidia Card"
    Driver "nvidia"
    Option "NoLogo" "true"
    Option "Coolbits" "28"  # Enables GPU overclocking and fan control, optional
    Option "TripleBuffer" "true"
    Option "RegistryDwords" "PerfLevelSrc=0x2222"
    Option "ConnectToAcpid" "Off"
EndSection
EOL

# Set NVIDIA power management
echo "Setting NVIDIA power management..."
nvidia-smi -pm 1

# Optional: Set maximum power limit
# echo "Setting maximum power limit..."
# nvidia-smi -pl <max_power_limit>  # Replace <max_power_limit> with the maximum power limit for your GPU

echo "NVIDIA setup complete. Rebooting system..."
reboot

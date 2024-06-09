# Ensure prerequisites are installed
sudo apt-get install build-essential

# Download source code
wget http://ftp.gnu.org/gnu/gcc/gcc-<version>/gcc-<version>.tar.gz
tar -xzf gcc-<version>.tar.gz
cd gcc-<version>

# Install dependencies
./contrib/download_prerequisites

# Create a build directory and configure
mkdir build && cd build
../configure --prefix=/usr/local/gcc-<version> --enable-languages=c,c++,fortran

# Compile and install
make -j$(nproc)
sudo make install

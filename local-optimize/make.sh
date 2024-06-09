# Download source code
wget http://ftp.gnu.org/gnu/make/make-<version>.tar.gz
tar -xzf make-<version>.tar.gz
cd make-<version>

# Configure, compile, and install
./configure --prefix=/usr/local/make-<version>
make -j$(nproc)
sudo make install

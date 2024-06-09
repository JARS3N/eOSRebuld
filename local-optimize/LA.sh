#!/bin/bash

# Ensure the script is run as root
if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root"
    exit 1
fi

# Install necessary tools
echo "Installing necessary tools..."
apt update
apt install -y build-essential gfortran wget

# Download and compile BLAS
echo "Downloading and compiling BLAS..."
BLAS_URL="http://www.netlib.org/blas/blas.tgz"
wget -O blas.tgz $BLAS_URL
tar -xvf blas.tgz
cd BLAS
gfortran -O3 -m64 -fPIC -c *.f
ar rv libblas.a *.o
ranlib libblas.a
cp libblas.a /usr/local/lib
cp *.h /usr/local/include
cd ..
rm -rf blas.tgz BLAS

# Download and compile LAPACK
echo "Downloading and compiling LAPACK..."
LAPACK_URL="http://www.netlib.org/lapack/lapack.tgz"
wget -O lapack.tgz $LAPACK_URL
tar -xvf lapack.tgz
LAPACK_DIR=$(tar -tzf lapack.tgz | head -1 | cut -f1 -d"/")
cd $LAPACK_DIR
cp make.inc.example make.inc
make lapacklib
cp liblapack.a /usr/local/lib
cp -r LAPACKE/include /usr/local/include
cd ..
rm -rf lapack.tgz $LAPACK_DIR

# Update library cache
echo "Updating library cache..."
ldconfig

echo "BLAS and LAPACK installation complete."


sudo apt update
sudo apt install build-essential gfortran

wget http://www.netlib.org/blas/blas.tgz
tar -xvf blas.tgz
cd BLAS
gfortran -O3 -m64 -fPIC -c *.f
sudo ar rv libblas.a *.o
sudo ranlib libblas.a
sudo cp libblas.a /usr/local/lib
sudo cp *.h /usr/local/include

wget http://www.netlib.org/lapack/lapack-3.9.1.tgz
tar -xvf lapack-3.9.1.tgz
cd lapack-3.9.1
cp make.inc.example make.inc
make lapacklib
sudo cp liblapack.a /usr/local/lib
sudo cp -r LAPACKE/include /usr/local/inclu

#!/bin/bash

mpirun -np $1 ./a.out > mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
mpirun -np $1 ./a.out >> mandel.txt
echo '' >> mandel.txt
cat mandel.txt

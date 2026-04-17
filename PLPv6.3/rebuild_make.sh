#!/bin/bash


if  [ "$1" == "" ]
then
  dir=.
else
  dir=$*
fi


cmake -G"Unix Makefiles"   -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=gfortran $dir

#cmake -G"Eclipse CDT4 - Unix Makefiles"  -D_ECLIPSE_VERSION=4.4  -DCMAKE_BUILD_TYPE=Debug -DCMAKE_Fortran_COMPILER=ifort $dir

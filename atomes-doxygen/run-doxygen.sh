#!/bin/bash

if [ $USER == 'leroux' ]; then
  sed 's/ROOTDIR/\/data/g' atomes-doxygen.init > ../src/atomes-doxygen.cfg
else
  sed 's/ROOTDIR/\/home\/slookeur/g' atomes-doxygen.init > ../src/atomes-doxygen.cfg
fi

cp atomes-doxygen.md ../src/
cd ../src
doxygen atomes-doxygen.cfg
rm -f atomes-doxgyen.*

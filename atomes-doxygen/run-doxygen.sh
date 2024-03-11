#!/bin/bash

if [ $USER == 'leroux' ]; then
  sed 's/ROOTDIR/\/data/g' atomes-doxygen.init > atomes-doxygen.cfg
else
  sed 's/ROOTDIR/\/home\/slookeur/g' atomes-doxygen.init > atomes-doxygen.cfg
fi

doxygen atomes-doxygen.cfg

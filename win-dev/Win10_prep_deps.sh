#!/bin/bash

ldd ./atomes | grep -v "Win" > no-win-deps.dat
ldd ./atomes | grep "Win" > win-deps.dat
ldd ./atomes > deps.dat
rm -rf bin bin-no-win

mkdir bin
cp atomes.exe bin/
liste_deps=`cat deps.dat|wc -l`
let ldeps=$liste_deps
for ((num = 1 ; num <= ldeps ; num ++))
do
  dep=`tac deps.dat|tail --lines=$num|tac|tail --lines=1|awk '{printf $3}'`
  cp $dep bin/
done

mkdir bin-no-win
cp atomes.exe bin-no-win/
liste_deps=`cat no-win-deps.dat|wc -l`
let ldeps=$liste_deps
for ((num = 1 ; num <= ldeps ; num ++))
do
  dep=`tac no-win-deps.dat|tail --lines=$num|tac|tail --lines=1|awk '{printf $3}'`
  cp $dep bin-no-win
done

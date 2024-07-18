#!/bin/sh

function inst_tool
{
  lib_path=$1
  libf=`echo $lib_path|sed 's/\// /g'|awk '{printf $NF}'`
#  install_name_tool -change "@rapth/$1" "@executable_path/../$libf" ${MESON_INSTALL_PREFIX}/Contents/MacOS/atomes-bin
}

lib_dir=${MESON_INSTALL_PREFIX}/Contents/Resources/lib
liste_lib=`otool -L atomes-bin|grep '/opt'|awk '{printf $1" "}'`
mkdir -p $lib_dir
for lib in $liste_lib
do
  echo "Copying : "$lib
  cp -R -L $lib $lib_dir
  inst_tool $lib  
done

#Additionals GCC required lib
cp -R -L /opt/homebrew/Cellar/gcc/14.1.0_1/lib/gcc/current/libquadmath.0.dylib $lib_dir
inst_tool libquadmath.0.dylib
cp -R -L /opt/homebrew/Cellar/gcc/14.1.0_1/lib/gcc/current/libgcc_s.1.1.dylib $lib_dir
inst_tool libgcc_s.1.1.dylib

#Extra stuff because you'll never know
cp -R -L /opt/homebrew/lib/gtk-4.0 $lib_dir
cp -R -L /opt/homebrew/lib/gdk-pixbuf-2.0 $lib_dir

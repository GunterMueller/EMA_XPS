#!/bin/sh

if [ _$1 = _ ]; then
   emaxpshome=/usr/EMA-XPS
else
   emaxpshome=$1
fi

echo "### Filesystem: $emaxpshome ###"

if [ ! -x $emaxpshome/. ]; then
   echo "   creating..."
   ## hg20090202: Cygwin lacks an /opt directory:
   mkdir -p $emaxpshome
fi

if [ ! -x $emaxpshome/. ]; then
   echo "ERROR: cannot create $emaxpshome ! ABORTing..."
   exit -1
fi

echo "removing all in $emaxpshome ..."
rm -r $emaxpshome/* $emaxpshome/.[A-z]*

echo "copying files in $emaxpshome ..."
cp `pwd`/EMA-XPS/[A-Z]* $emaxpshome

for name in \
 kbs \
 lib \
 share \
 bin \
 man \
 doc
#
do
   echo "copying tree [$name] ..."
   # per default is pwd=$HOME/ema-xps
   cp -r `pwd`/EMA-XPS/$name $emaxpshome/$name
   echo "   done."
done

#eof

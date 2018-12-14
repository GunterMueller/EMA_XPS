#!/bin/sh

if [ _$1 = _ ]; then
   emaxpshome=/usr/EMA-XPS
else
   emaxpshome=$1
fi

echo "### Filesystem: $emaxpshome ###"

if [ ! -x $emaxpshome/. ]; then
   echo "   creating..."
   mkdir $emaxpshome
fi

if [ ! -x $emaxpshome/. ]; then
   echo "ERROR: cannot create $emaxpshome ! ABORTing..."
   exit -1
fi

cd $emaxpshome

for name in \
 kbs \
 kbs/xbm \
 lib \
 lib/bitmaps \
 lib/deutsch  \
 lib/english \
 bin \
 man \
 man/man1 \
 doc
#
do
   echo "checking existance of directory [$name]"
   if [ -x $name -a ! -d $name ]; then
      echo "   renaming non-directory..."
      mv -f $name $name.save
   fi
   if [ ! -x $name ]; then
      echo "   creating..."
      mkdir $name
   fi
   echo "   done."
done

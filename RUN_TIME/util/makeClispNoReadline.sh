#!/bin/sh
# This script assumes to be run from the parent directory of
# the ./util directory!
# Additionally the ../tmp directory (relative to this place)
# is assumed to be already created!

# Since EMA-XPS requires a CLISP version w/o the GNU readline
# library, the preinstalled version of most Linux distributions
# does not fit our needs!
# This builds our own version of CLISP based on the source
# trees in our mirror directory. It will not interact with
# the CLISP version of your Linux distribution.
# It will be used by EMA-XPS only.

basedir="`/bin/pwd`"
tempdir="$basedir/tmp"
destdir="$basedir/EMA-XPS"
mirrdir="$basedir/mirror"
lisparc="$mirrdir/clisp-2.45.tar.bz2"
segvarc="$mirrdir/libsigsegv-2.5.tar.gz"

if test -d "$tempdir" -a -d "$destdir" -a -f "$lisparc" -a -f "$segvarc"
then
  :
else
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Corrupted EMA-XPS archive or starting this"
  echo "script from a place different from BASEDIR?"
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi

cd "$tempdir"

## 1) we need to build a lib for clisp first:

tar xfz "$segvarc"
cd libsigsegv-2.5
./configure --prefix="$tempdir"
make
make check
make install

cd "$tempdir"

if test -f lib/libsigsegv.a
then
  :
else
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Failed building the prerequisite library"
  echo "tmp/lib/libsigsegv.a. Please check manually!"
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi

## 2) build our own instance of clisp w/o GNU readline lib:

tar xfj "$lisparc"
cd clisp-2.45

ulimit -s 16384
./configure --without-readline \
            --with-libsigsegv-prefix="$tempdir" \
            --prefix="$destdir"

cd src
make
make check
make install-bin

cd "$destdir"

# hg20090202: lisp.run@linux, lisp.exe@cygwin
lisprun="lib/clisp-2.45/base/lisp.run"
if test -f "lib/clisp-2.45/base/lisp.exe"
then
  lisprun="lib/clisp-2.45/base/lisp.exe"
fi
if test -f bin/clisp -a -f lib/clisp-2.45/base/lispinit.mem -a -f "$lisprun"
then
  :
else
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Failed building a CLISP instance w/o the GNU"
  echo "readline library in EMA-XPS/bin/clisp and"
  echo "EMA-XPS/lib/clisp-2.45/. Please check manually!"
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi

exit 0     ## OK

# eof


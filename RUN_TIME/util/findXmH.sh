#!/bin/sh
Found=`ls -1 /usr/X11*/include/Xm/Xm.h /usr/include/X11*/Xm/Xm.h /usr/include/Xm/Xm.h 2>/dev/null`
if test "" = "$Found"
then
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Failed looking for the Motif include files."
  echo "It seems you need to install the developer"
  echo "version of Lesstif or OpenMotif."
  echo "The mirror directory contains the source tree"
  echo "of OpenMotif 2.3.0, which I used to build"
  echo "EMA-XPS."
  echo "Please read the TERMS OF LICENSING included in"
  echo "the OpenMotif archive file, whether they match"
  echo "your needs."
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi
exit 0     ## OK

# eof


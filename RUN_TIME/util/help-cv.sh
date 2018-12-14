#!/bin/sh
# arg1 = infile, arg2 = outfile, rest=options

PATH=$PATH:/lib:/usr/lib:../util
export PATH

tmpf=4711.c

cpp="cc_X11 -E"
# -undef -P

rm -f $2 $tmpf
cp $1 $tmpf
$cpp $tmpf $3 | cpp-post | help-cv > $2
touch $2
rm -f $tmpf

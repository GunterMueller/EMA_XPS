#!/bin/sh
# This script assumes to be run from the parent directory of
# the ./util directory!
# Additionally the ../tmp directory (relative to this place)
# is assumed to be already created!

# Since EMA-XPS requires a CLISP version w/o the GNU readline
# library, the preinstalled version of most Linux distributions
# does not fit our needs!
# This script checks for the existance of our own version of 
# CLISP.

basedir="`/bin/pwd`"
tempdir="$basedir/tmp"
destdir="$basedir/EMA-XPS"
mirrdir="$basedir/mirror"
lispexe="$destdir/bin/clisp"
babyarc="$mirrdir/Babylon-2.3.tar.gz"
babydir="$tempdir/Babylon-2.3"
memfile="$babydir/babylon.mem"
b23file="$basedir/util/b23.cl"

if test -d "$tempdir" -a -d "$destdir" -a -f "$lispexe" -a -f "$babyarc"
then
  :
else
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Corrupted EMA-XPS archive or starting this"
  echo "script from a place different from BASEDIR?"
  echo "Did you run this script before building CLISP?"
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi

cd "$tempdir"

tar xfz "$babyarc"
cd "$babydir"

## 1) do some patches:

tempgtd="`echo $tempdir|sed 's#/#\\>#g'`"
tempqtd="`echo $tempgtd|sed 's#>#/#g'`"

mv make.cl make.cl.orig
echo sed "s#/home/juergen/Babylon#$tempqtd#"
cat make.cl.orig |\
  sed "s#/home/juergen/Babylon#$tempqtd#" |\
  cat > make.cl

mv make-sun.cl make-sun.cl.orig
echo sed "s#>home>juergen>Babylon#$tempgtd#"
cat make-sun.cl.orig |\
  sed "s/LISP:SAVEINITMEM/EXT::SAVEINITMEM/" |\
  sed "s#>home>juergen>Babylon#$tempgtd#" |\
  cat > make-sun.cl

## 2) build the babylon.mem CLISP memory image file:

$lispexe "$b23file"
ls -al "$memfile"
if test -f "$memfile"
then
  :
else
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  echo "Failed building the Babylon-2.3 memory dump file."
  echo '*** STOP *** STOP *** STOP *** STOP *** STOP ***'
  exit -1  ## ERROR (see Makefile)
fi

## 3) update the babylon start script:

cd "$basedir"/src
cat > bablisp <<!
#!/bin/sh
$lispexe -M $memfile
!

exit 0     ## OK

# eof


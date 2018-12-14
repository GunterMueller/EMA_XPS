#!/bin/sh

TMPF=/tmp/kill_emalisp

cd
rm -f core bin/core kbs/core ema-xps/core \
      ema-xps/src/core ema-xps/bin/core

rm -f $TMPF
ps ax >$TMPF
LISP_PIDS=`grep $1 $TMPF|grep -v ema-kill.sh|awk '{print $1}'`
rm -f $TMPF

if [ "$LISP_PIDS" != "" ]; then
   echo "killing $LISP_PIDS"
   kill -15 $LISP_PIDS
else
   echo "nothing to be killed  :-)"
fi

exit 0

# eof

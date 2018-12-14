/****************************************************
*   timings.c     Hans Groschwitz        06.10.94   *
*                                                   *
*   This file contains the modified XtAddTimeOut    *
*   routine with added XSync...                     *
*                                                   *
*   BUGS:                                           *
*     This technique is a HACK!                     *
****************************************************/

#include <stdio.h>
#include <Xm/Xm.h>

#include "timings.h"
#include "main.h"                 /* SystemDialogs */

/*** Function Declarations *************************/

void
AddTimeOut( int ticks, XtPointer funcp, XtPointer argp ) {

   XSync( XtDisplay(SystemDialogs), False );
   XtAddTimeOut( TICK_TIME * ticks, funcp, argp );
}

/*** EOF *******************************************/

/****************************************************
*   timings.h     Hans Groschwitz        06.10.94   *
*                                                   *
*   This file contains time values for the used     *
*   XtAddTimeOut calls to adopt them to platforms   *
*   of differing CPU performances (or load).        *
****************************************************/

/*
 * smallest time interval available (40 ms)
 * (should be the only one scaled for slow CPUs)
 */
#define TICK_TIME        40

/*** Initialization Phase (order of appearance) ***/

/*
 * time between welcome pop up and the rest of Xappl init
 */
#define TIME_XINIT_REST  15

/*
 * time between Xappl init and spawn LISP
 */
#define TIME_LISP_RUN    4

/*
 * time between lisp:first_char_sent and activating
 * VChannel Management init routine
 * (sending first command to LISP)
 */
#define TIME_LISP_INIT   6

/*
 * time between receiving success of first command
 * and user-init/autoload
 * must be >= TIME_LISP_READY !!!
 */
#define TIME_END_OF_INIT 6

/*
 * time between user-init/autoload placing and
 * starting the sndCmd Ticker!
 * must be >= TIME_LISP_READY !!!
 */
#define TIME_SND_FIRST   6

/*** Continuous Operation Settings ***/

/*
 * time between attempts to send commands to LISP
 */
#define TIME_SND_CMD     1

/*
 * nr of attempts before assuming a hanging
 */
#define TIME_CHK_HANG    6

/*
 * LISP needs some time to print results and prompt
 */
#define TIME_LISP_READY  6

/*
 * additional attempt to set focus in a popup dialog
 */
#define TIME_FOCUS       4

/*
 * time to reinit *evalhook* after a Break! (1 sec or more)
 */
#define TIME_AFTER_BREAK 30

/*
 * Linux Bug: ui_stop_session: see ui.c !
 */
#define TIME_UI_STOP     2

/*
 * User Interface: a small TimeOut to redraw a pressed Button
 * before sending the callback String
 */
#define TIME_UI_CB       2

/*** Function Declarations *************************/

extern void AddTimeOut( int, XtPointer, XtPointer );

/*** EOF *******************************************/

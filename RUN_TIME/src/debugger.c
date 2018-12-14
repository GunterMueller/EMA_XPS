/****************************************************
*   debugger.c    Hans Groschwitz        19.07.95   *
*                                                   *
*   Simulates a terminal-like window for working    *
*   with the LISP-debugger, called a PAD.           *
****************************************************/

#include <stdio.h>

#include <Xm/Form.h> 
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Protocols.h>

#include "main.h"
#include "debugger.h"
#include "help.h"

/*** Globals ***************************************/

int PAD_directly_read = False;

static Widget shellW, outputW, pendingW, inputW;

/*** Function Declarations *************************/

static void displayOutput( char* );
static void displayBufferedOutputChar( int );
static void copyCB( void );
static void eraseCB( void );
static void breakCB( void );
static void hideCB( void );

/****************************************************
*   updates contents of text-output window          *
*   Scrolls the text in the output-window of the    *
*   LISP-Debugger's PAD to bottom                   *
****************************************************/

static void
displayOutput( char *input ) { 
   XmTextPosition pos;
   char *text;

   text = XmTextGetString( outputW );
   pos = strlen( text );
   XmTextInsert( outputW, pos, input );
   /* show End */
   XmTextShowPosition( outputW, pos+strlen(input) );
   XtFree( text );
}

/****************************************************
*   writes a char buffered (to avoid flickering)    *
*   to the bottom of the PAD's text-output window   *
****************************************************/

static void
displayBufferedOutputChar( int c ) {
   static int  idx = 0;
   static char input[1024+1];

   if( !idx && !c ) /* do not flush, if nothing to be flushed! */
      return;

   input[idx++] = c;

   /* c==0 (NUL) forces a flush */
   if( !c || idx >= 1024 ) {
      input[idx] = 0;
      idx = 0;

      /* update PAD's output-window */
      displayOutput( input );
   }
}

/****************************************************
*   copies USER's inputs to LISP; LISP echoes them  *
*                                                   *
*   currently the return key activates this         *
*   ActivateCallback of the text-input widget       *
****************************************************/

static void
copyCB( void ) {
   char *input, *tmp;

   XmTextFieldSetEditable( inputW, False );
   tmp = XmTextFieldGetString( inputW ); 
   if( PAD_directly_read )
      LISP_sndReply( tmp );
   else
      LISP_sndCmd( tmp );
   XtFree( tmp );
   /* clear the input window */
   XmTextFieldSetString(   inputW,   "" );
   XmTextFieldSetEditable( inputW, True );
}

/****************************************************
*   Callback to clear the output window             *
****************************************************/

static void
eraseCB( void ) {

   XmTextSetString( outputW, "" );
   /* update the scrollbars */
   XmTextShowPosition( outputW, 0 );
}

/****************************************************
*   Interrupt LISP ^C                               *
*   InputWindow=Ready is done by *debugger-hook*    *
*   except interrupted calls like (sleep 4) !?!     *
****************************************************/

static void
breakCB( void ) {

   LISP_interrupt();
}

/****************************************************
*   Hide Debugger PAD                               *
****************************************************/

static void
hideCB( void ) {

   PAD_hide();
}

/****************************************************
*   create debugger's PAD                           *
****************************************************/

Widget
PAD_create( Widget parent ) {
   Arg args[20];
   Widget panedW, fixedW, buttonFormW, breakW, eraseW, hideW;
   int n, nSave;
   Display *dpy; 

   /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++; 
   XtSetArg( args[n], XmNiconPixmap,                  iconPm ); n++; 
   /*
    * at child-ToplevelObjects the class name is ignored for the
    * information in XresourceFiles
    * --> here an explicit instance name is necessary !!!
    */
   shellW = XtAppCreateShell( "Debugger", "HereClassIsIgnored",
                            /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( shellW, WM_DELETE_WINDOW,
      (XtPointer)hideCB, NULL );        /* Pop down ... */

   /*** Paned (sash=invisible) ****/
   n = 0;
   panedW = XmCreatePanedWindow( shellW, "Paned", args, n );
   XtManageChild( panedW );

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( panedW, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_DEBUGGER_PAD );

   /*** Output-Text-Window *****/
   n = 0;
   XtSetArg( args[n], XmNpaneMinimum,                        100 ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,        XmSTATIC ); n++;
   XtSetArg( args[n], XmNeditable,                         False ); n++;
   XtSetArg( args[n], XmNeditMode,             XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNcursorPositionVisible,            False ); n++;
   outputW = XmCreateScrolledText( panedW, "Output", args, n );
   XtManageChild( outputW );

   /*** FixedPaned ***/
   n = 0;
   XtSetArg( args[n], XmNpaneMinimum,                        150 ); n++;
   XtSetArg( args[n], XmNsashHeight,                           1 ); n++;
   XtSetArg( args[n], XmNsashWidth,                            1 ); n++;
   XtSetArg( args[n], XmNseparatorOn,                      False ); n++;
   fixedW = XmCreatePanedWindow( panedW, "Fixed", args, n );
   XtManageChild( fixedW );

   /*** Pending-Commands-Window (init=1row hardcoded) ***/
   n = 0;
   XtSetArg( args[n], XmNrows,                                 1 ); n++;
   XtSetArg( args[n], XmNeditable,                         False ); n++;
   XtSetArg( args[n], XmNeditMode,             XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNcursorPositionVisible,            False ); n++;
   pendingW = XmCreateScrolledText( fixedW, "Pending", args, n );
   XtManageChild( pendingW );

   /*** Input-TextField-Window ****/
   n = 0;
   XtSetArg( args[n], XmNpaneMinimum,                         40 ); n++;
   XtSetArg( args[n], XmNpaneMaximum,                         40 ); n++;
   XtSetArg( args[n], XmNeditMode,            XmSINGLE_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNmaxLength,                         1024 ); n++;
   XtSetArg( args[n], XmNeditable,                          True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,            True ); n++;
   inputW = XmCreateTextField( fixedW, "Input", args, n );
   XtManageChild( inputW );
   XtAddCallback( inputW, XmNactivateCallback,
      (XtPointer)copyCB, NULL );

   /*** ButtonForm ******/
   n = 0;
   XtSetArg( args[n], XmNpaneMinimum,                         40 ); n++;
   XtSetArg( args[n], XmNpaneMaximum,                         40 ); n++;
   XtSetArg( args[n], XmNfractionBase,                         5 ); n++;
   buttonFormW = XmCreateForm( fixedW, "ButtonForm", args, n );
   XtManageChild( buttonFormW );

   /*** BreakButton *********/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;
   nSave = n;
   XtSetArg( args[n], XmNleftPosition,                        0 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       1 ); n++;
   breakW = XmCreatePushButton( buttonFormW, "Break", args, n );
   XtManageChild( breakW );
   XtAddCallback( breakW, XmNactivateCallback,
      (XtPointer)breakCB, NULL );

   /*** EraseButton *******/
   n = nSave;
   XtSetArg( args[n], XmNleftPosition,                        4 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       5 ); n++;
   eraseW = XmCreatePushButton( buttonFormW, "Erase", args, n );
   XtManageChild( eraseW );
   XtAddCallback( eraseW, XmNactivateCallback,
      (XtPointer)eraseCB, NULL );

   /*** to be managed explicitly ***/
   return shellW;
}

/****************************************************
*   Append a character to PAD's output window       *
*   (accessable from outside...)                    *
****************************************************/

void
PAD_append( int c ) {

   displayBufferedOutputChar( c );
}

/****************************************************
*   Change Sensitivity of the Debugger PAD          *
*   (accessable from outside...)                    *
*   XmTextFieldSetEditable( inputW, True|False ) is *
*   no longer needed: commands are stacked now.     *
****************************************************/

void
PAD_ready( void ) {

   if( XtIsRealized( shellW ) ) {
      XDefineCursor(
         XtDisplay( shellW ),
         XtWindow( shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
PAD_busy( void ) {

   if( XtIsRealized( shellW ) ) {
      XDefineCursor(
         XtDisplay( shellW ),
         XtWindow( shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Change Editablility of the Debugger PAD         *
*   (accessable from outside...)                    *
*   In case of LISP-ERROR the debugger should be    *
*   popped up automatically (Xlib) !!!              *
****************************************************/

void
PAD_show( void ) {

   PopupRaiseAndDeiconify( shellW );
   XmProcessTraversal( inputW, XmTRAVERSE_CURRENT );
}

/***************************************************/

void
PAD_hide( void ) {

   XtPopdown( shellW );
}

/****************************************************
*   Update Text in Pending Window of Debugger PAD   *
*   (accessable from outside...)                    *
****************************************************/

void
PAD_Pupdate( char *str ) {
   int len;
   static int len_1 = -1;
   static int len_2 = -1;
   len = strlen( str );

   /* to avoid flicker in the pendingW */
   if( (len != len_1) || (len_1 != len_2) ) {
      XmTextSetString( pendingW, str );
   }

   len_2 = len_1;
   len_1 = len;
}

/*** EOF *******************************************/

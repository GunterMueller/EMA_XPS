/****************************************************
*   uid.c         Hans Groschwitz        06.10.94   *
*                                                   *
*  C-FrontEnds of LISP functions for control of     *
*  Popups for User Interface (Session Screen)       *
*             ^    ^                                *
*  The `d' within in `uid' displays `popup-Dialog'. *
*                                          ^        *
*  This version of the User Interface uses logical  *
*  channels for message-passing. The corresponding  *
*  channel numbers and the reading functions        *
*  must be known within dispatch.c !!!              *
****************************************************/

#include <stdio.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>

#include "timings.h"
#include "uid.h"                /* UID_debugString */
#include "main.h"               /* LISP_sndReply() */

/*** Defines ***************************************/

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET) 

/*** Prototypes ************************************/

static Widget createUidShell( Widget );
static void createUidInformation( Widget );
static void createUidAcceptCancel( Widget );
static void createUidYesNoCancel( Widget );
static void createUidPrompt( Widget );
static void createUidOneOfMany( Widget );
static void createUidSomeOfMany( Widget );

/*** Globals ***************************************/

static Widget UidInformation;
static Widget UidAcceptCancel;
static Widget UidYesNoCancel;
static Widget UidPrompt;
static Widget UidOneOfMany, UidOneOfManyLW;
static Widget UidSomeOfMany, UidSomeOfManyLW;
static XmString empty_string;

/****************************************************
*   Uid: create User Interface PopupDialogs         *
****************************************************/

void
UID_create( Widget parent ) {

   /*** create a fake-shell for easier handling of Xresources! ***/
   parent = createUidShell( parent );

   /*** createUidInformation: Session Screen Waiting For Reply ***/
   createUidInformation( parent );

   /*** createUidAcceptCancel: Session Screen Waiting For Reply ***/
   createUidAcceptCancel( parent );

   /*** createUidYesNoCancel: Session Screen Waiting For Reply ***/
   createUidYesNoCancel( parent );

   /*** createUidPrompt: Session Screen Waiting For Reply ***/
   createUidPrompt( parent );

   /*** createUidOneOfMany: Session Screen Waiting For Reply ***/
   createUidOneOfMany( parent );

   /*** createUidSomeOfMany: Session Screen Waiting For Reply ***/
   createUidSomeOfMany( parent );
}

static Widget
createUidShell( Widget parent ) {
   Widget shellW;
   Arg args[8];
   int n;

   /*** Shell ***/
   n = 0;
   /* XmNx and XmNy are ignored by the window manager !!! */
   XtSetArg( args[n], XmNgeometry,        geometryConstraint ); n++;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconic,                       False ); n++;
   XtSetArg( args[n], XmNinitialState,           NormalState ); n++;
   /* To avoid mapping = drawing_on_the_screen !!! */
   XtSetArg( args[n], XmNmappedWhenManaged,            False ); n++;
   /*
    * at child-ToplevelObjects the class name is ignored for the
    * information in XresourceFiles
    * --> here an explicit instance name is necessary !!!
    */
   shellW = XtAppCreateShell( "SessionDialogs", "HereClassIsIgnored",
                                /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /* necessary to enable GeometryManagement for its children */
   XtRealizeWidget(shellW);

   return shellW;
}

/****************************************************
*   Uid: Miscelaneous                               *
****************************************************/

void
UID_popdown_all_dialogs( void ) {
   /*
    * In case of entering the debugger, offending user
    * replies to UID popups should be discarded!
    * This routine pops down all UID dialogs.
    * Used by CMD_clear().
    */

   if( XtIsManaged( UidInformation ) ) {
      XtPopdown( XtParent(UidInformation) );
      XtUnmanageChild( UidInformation );
   }
   if( XtIsManaged( UidAcceptCancel ) ) {
      XtPopdown( XtParent(UidAcceptCancel) );
      XtUnmanageChild( UidAcceptCancel );
   }
   if( XtIsManaged( UidYesNoCancel ) ) {
      XtPopdown( XtParent(UidYesNoCancel) );
      XtUnmanageChild( UidYesNoCancel );
   }
   if( XtIsManaged( UidPrompt ) ) {
      XtPopdown( XtParent(UidPrompt) );
      XtUnmanageChild( UidPrompt );
   }
   if( XtIsManaged( UidOneOfMany ) ) {
      XtPopdown( XtParent(UidOneOfMany) );
      XtUnmanageChild( UidOneOfMany );
   }
   if( XtIsManaged( UidOneOfMany ) ) {
      XtPopdown( XtParent(UidSomeOfMany) );
      XtUnmanageChild( UidSomeOfMany );
   }
}

char*
UID_debugString( char *item ) {
   char *out;
   int c, i, j, t, k, cnt, len;

   len = strlen( item );
   for( cnt = i = 0; i < len; i++ ) {
      if( (c=item[i]) == '\n' )
         cnt += 2;
      else if( c == '\t' )
         cnt += 8;
      else
         cnt++;
   }
   out = XtMalloc( cnt + 2 );

   for( i = j = t = 0; item[i]; i++ ) {
      if( item[i] == '\n' ) {
         out[j++] = '\n';
         t = 0;
         if( item[i+1] == '\n' ) {
            out[j++] = ' ';    /* Motif eliminates empty lines ! */
            out[j++] = '\n';
            i++;
         }
      } else if( item[i] == '\t' ) {
         for( k = t; k < 8; k++ ) {
             out[j++] = ' ';    /* Motif replaces TAB by one! SPC */
         }
         t = 0;
      } else {
         out[j++] = item[i];
         t++;
         if( t >= 8 )
            t = 0;
      }
   }
   out[j] = 0;
   XtFree( item );
   return out;
}

/****************************************************
*   UidInformation --> (uid-information "string")   *
****************************************************/

static void
UidInformationOkCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(UidInformation) );
   XtUnmanageChild( UidInformation );
}

static void
createUidInformation( Widget parent ) {
   Widget ob, cb, hb;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidInformation = XmCreateInformationDialog( parent,
                       "UidInformation", args, 2 );
   ob = XmMessageBoxGetChild( UidInformation, XmDIALOG_OK_BUTTON );
   /*
    * Those buttons are Gadgets and hence the do not have resources
    * for fg and bg color !!!
    */
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)UidInformationOkCB, NULL );
   cb = XmMessageBoxGetChild( UidInformation, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( cb );
   hb = XmMessageBoxGetChild( UidInformation, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
}

void
readUidInformation( void ) {
   char *s;
   XmString p;

   s = UID_debugString( MSG_read() );
   XtVaSetValues( UidInformation, XmNmessageString, p=CSTRING(s), NULL );
   XtFree( s );  /* explicitly! */
   XmStringFree( p );
   XtManageChild( UidInformation );
   XtPopup( XtParent(UidInformation), XtGrabNone );
}

/****************************************************
*  UidAcceptCancel --> (uid-accept-cancel "string") *
****************************************************/

static void
UidAcceptCancelAcceptCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(UidAcceptCancel) );
   XtUnmanageChild( UidAcceptCancel );
}

static void
UidAcceptCancelCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(UidAcceptCancel) );
   XtUnmanageChild( UidAcceptCancel );
}

static void
createUidAcceptCancel( Widget parent ) {
   Widget ob, cb, hb;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidAcceptCancel = XmCreateQuestionDialog( parent,
                        "UidAcceptCancel", args, 2 );
   ob = XmMessageBoxGetChild( UidAcceptCancel, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)UidAcceptCancelAcceptCB, NULL );
   cb = XmMessageBoxGetChild( UidAcceptCancel, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)UidAcceptCancelCancelCB, NULL );
   hb = XmMessageBoxGetChild( UidAcceptCancel, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
}

void
readUidAcceptCancel( void ) {
   char *s;
   XmString p;

   s = UID_debugString( MSG_read() );
   XtVaSetValues( UidAcceptCancel, XmNmessageString, p=CSTRING(s), NULL );
   XtFree( s );  /* explicitly! */
   XmStringFree( p );
   XtManageChild( UidAcceptCancel );
   XtPopup( XtParent(UidAcceptCancel), XtGrabNone );
}

/****************************************************
*  UidYesNoCancel --> (uid-yes-no-cancel "string")  *
****************************************************/

static void
UidYesNoCancelYesCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(UidYesNoCancel) );
   XtUnmanageChild( UidYesNoCancel );
}

static void
UidYesNoCancelNoCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(UidYesNoCancel) );
   XtUnmanageChild( UidYesNoCancel );
}

static void
UidYesNoCancelCancelCB( void ) {
   LISP_sndReply( ":CANCEL" );
   XtPopdown( XtParent(UidYesNoCancel) );
   XtUnmanageChild( UidYesNoCancel );
}

static void
createUidYesNoCancel( Widget parent ) {
   Widget ob, cb, hb;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidYesNoCancel = XmCreateQuestionDialog( parent,
                       "UidYesNoCancel", args, 2 );
   ob = XmMessageBoxGetChild( UidYesNoCancel, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)UidYesNoCancelYesCB, NULL );
   cb = XmMessageBoxGetChild( UidYesNoCancel, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)UidYesNoCancelNoCB, NULL );
   hb = XmMessageBoxGetChild( UidYesNoCancel, XmDIALOG_HELP_BUTTON );
   XtAddCallback( hb, XmNactivateCallback, (XtPointer)UidYesNoCancelCancelCB, NULL );
}

void
readUidYesNoCancel( void ) {
   char *s;
   XmString p;

   s = UID_debugString( MSG_read() );
   XtVaSetValues( UidYesNoCancel, XmNmessageString, p=CSTRING(s), NULL );
   XtFree( s );  /* explicitly! */
   XmStringFree( p );
   XtManageChild( UidYesNoCancel );
   XtPopup( XtParent(UidYesNoCancel), XtGrabNone );
}

/****************************************************
*       UidPrompt --> (uid-prompt "string")         *
****************************************************/

static void
UidPromptAcceptCB( Widget w, XtPointer cd,
                   XmSelectionBoxCallbackStruct *cbs ) {
   char str[1024], *value = NULL;

   XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
   if( value && *value )   /* it should be a non-empty string! */
      sprintf( str, "%s", value );
   else    /* OK pressed without anything entered ! */
      sprintf( str, "NIL" );
   XtFree(value);
   LISP_sndReply( str );
   XtPopdown( XtParent(UidPrompt) );
   XtUnmanageChild( UidPrompt );
}

static void
UidPromptCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(UidPrompt) );
   XtUnmanageChild( UidPrompt );
}

static void
focusUidPromptCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( UidPrompt, XmDIALOG_TEXT );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createUidPrompt( Widget parent ) {
   Widget ob, cb, hb;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidPrompt = XmCreatePromptDialog( parent, "UidPrompt", args, 2 );
   ob = XmSelectionBoxGetChild( UidPrompt, XmDIALOG_OK_BUTTON );
   XtAddCallback( UidPrompt, XmNokCallback, (XtPointer)UidPromptAcceptCB, NULL );
   cb = XmSelectionBoxGetChild( UidPrompt, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)UidPromptCancelCB, NULL );
   hb = XmSelectionBoxGetChild( UidPrompt, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   empty_string = XmStringCreateSimple("");

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( UidPrompt, XmNmapCallback, (XtPointer)focusUidPromptCB, NULL );
}

void
readUidPrompt( void ) {
   char *s;
   XmString p;

   s = UID_debugString( MSG_read() );
   XtVaSetValues( UidPrompt,
      XmNselectionLabelString, p=CSTRING(s),
      XmNtextString, empty_string,
      NULL );
   XtFree( s );  /* explicitly! */
   XmStringFree( p );
   XtManageChild( UidPrompt );
   XtPopup( XtParent(UidPrompt), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusUidPromptCB, NULL );
}

/****************************************************
*      UidOneOfMany -->                             *
*         (uid-one-of-many '(l i s t) "string")     *
****************************************************/

static void
UidOneOfManyCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(UidOneOfMany) );
   XtUnmanageChild( UidOneOfMany );
}

static void
UidOneOfManyAcceptCB( void ) {
   char str[1024];
   int d, *p;

   if( XmListGetSelectedPos( UidOneOfManyLW, &p, &d ) ) {
      /* more than zero elements in list widget */
      sprintf( str, "%d", p[0]-1 );   /* (first ...) = (nth 0 ...) */
      XtFree( (char*)p );
   } else {
      UidOneOfManyCancelCB();
      return;
   }
   LISP_sndReply( str );
   XtPopdown( XtParent(UidOneOfMany) );
   XtUnmanageChild( UidOneOfMany );
}

static void
focusUidOneOfManyCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_LIST );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createUidOneOfMany( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidOneOfMany = XmCreateSelectionDialog( parent, "UidOneOfMany", args, 2 );
   UidOneOfManyLW = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_LIST );
   /* Hardcoded to make AcceptCB easier! */
   XtVaSetValues( UidOneOfManyLW, XmNselectionPolicy, XmBROWSE_SELECT, NULL );
   ob = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)UidOneOfManyAcceptCB, NULL );
   cb = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)UidOneOfManyCancelCB, NULL );
   ab = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( ab );
   hb = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   sl = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( sl );
   dt = XmSelectionBoxGetChild( UidOneOfMany, XmDIALOG_TEXT );
   XtUnmanageChild( dt );

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( UidOneOfMany, XmNmapCallback, (XtPointer)focusUidOneOfManyCB, NULL );
}

void
readUidOneOfMany( void ) {
   char *str, *s, *s0, *hdl;
   int c, i, len, nr;
   XmString p;

   XmListDeleteAllItems( UidOneOfManyLW );
   s0 = s = str = MSG_read();
/*for(i=0;c=str[i];i++)if(c>=' ')printf("%c",c);else printf("^%c",c+'@');puts("");*/
   len = strlen( str );
   nr = 0;
   do {
      c = *s;
      if( c == ETX || !c ) {
         *s = NUL;
         if( !nr ) {   /* here comes the headline... */
            hdl = XtMalloc( strlen(s0)+1 );
            strcpy( hdl, s0 );  /* UID_debugString XtFree's p !!! */
            hdl = UID_debugString( hdl );
         } else {
            XmListAddItem( UidOneOfManyLW, p=CSTRING(s0), nr );
            XmStringFree( p );
         }
         s0 = s+1;
         nr++;
      }
      s++;
   } while( c );
   XtVaSetValues( UidOneOfMany,
      XmNlistLabelString, p=CSTRING(hdl),
      NULL );
   XmStringFree( p );
   XmListSelectPos( UidOneOfManyLW, 1, False );

   XtFree( str );  /* explicitly! */
   XtFree( hdl );

   XtManageChild( UidOneOfMany );
   XtPopup( XtParent(UidOneOfMany), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusUidOneOfManyCB, NULL );
}

/****************************************************
*      UidSomeOfMany -->                            *
*         (uid-some-of-many '(l i s t) "string")    *
****************************************************/

static void
UidSomeOfManyCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(UidSomeOfMany) );
   XtUnmanageChild( UidSomeOfMany );
}

static void
UidSomeOfManyAcceptCB( void ) {
   char str[1024], tmp[1024];
   int i, d, *p;

   if( XmListGetSelectedPos( UidSomeOfManyLW, &p, &d ) ) {
      /* more than zero elements in list widget */
      strcpy( str, "(" );
      for( i = 0; i < d; i++ ) {
         sprintf( tmp, "%d ", p[i]-1 ); /* (first ...) = (nth 0 ...) */
         strcat( str, tmp );
      }
      strcat( str, ")" );
      XtFree( (char*)p );
   } else {
      UidSomeOfManyCancelCB();
      return;
   }
   LISP_sndReply( str );
   XtPopdown( XtParent(UidSomeOfMany) );
   XtUnmanageChild( UidSomeOfMany );
}

static void
focusUidSomeOfManyCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_LIST );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createUidSomeOfMany( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[2];

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   UidSomeOfMany = XmCreateSelectionDialog( parent, "UidSomeOfMany", args, 2 );
   UidSomeOfManyLW = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_LIST );
   /* Hardcoded to make AcceptCB easier! */
   XtVaSetValues( UidSomeOfManyLW, XmNselectionPolicy, XmMULTIPLE_SELECT, NULL );
   ob = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)UidSomeOfManyAcceptCB, NULL );
   cb = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)UidSomeOfManyCancelCB, NULL );
   ab = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( ab );
   hb = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   sl = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( sl );
   dt = XmSelectionBoxGetChild( UidSomeOfMany, XmDIALOG_TEXT );
   XtUnmanageChild( dt );

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( UidSomeOfMany, XmNmapCallback, (XtPointer)focusUidSomeOfManyCB, NULL );
}

void
readUidSomeOfMany( void ) {
   char *str, *s, *s0, *hdl;
   int c, i, len, nr;
   XmString p;

   XmListDeleteAllItems( UidSomeOfManyLW );
   s0 = s = str = MSG_read();
   len = strlen( str );
   nr = 0;
   do {
      c = *s;
      if( c == ETX || !c ) {
         *s = NUL;
         if( !nr ) {   /* here comes the headline... */
            hdl = XtMalloc( strlen(s0)+1 );
            strcpy( hdl, s0 );  /* UID_debugString XtFree's p !!! */
            hdl = UID_debugString( hdl );
         } else {
            XmListAddItem( UidSomeOfManyLW, p=CSTRING(s0), nr );
            XmStringFree( p );
         }
         s0 = s+1;
         nr++;
      }
      s++;
   } while( c );
   XtVaSetValues( UidSomeOfMany,
      XmNlistLabelString, p=CSTRING(hdl),
      NULL );
   XmStringFree( p );
   XmListSelectPos( UidSomeOfManyLW, 1, False );

   XtFree( str );  /* explicitly! */
   XtFree( hdl );

   XtManageChild( UidSomeOfMany );
   XtPopup( XtParent(UidSomeOfMany), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusUidSomeOfManyCB, NULL );
}

/*** EOF ***/

#if 0
#define _dbg(a)    puts("expd: " a)
#define _dbgs(a) printf("expd: %s\n",a)
#define _dbgx(a) printf("expd: 0x%X\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#define _dbgx(a)
#endif

/****************************************************
*   expd.c        Hans Groschwitz        12.09.96   *
*                 Karsten Vossberg                  *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   Popups for the Explanation facility of EMA-XPS  *
*                                                   *
*   See uid.x for details!                          *
****************************************************/

#include <stdio.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>

#include "timings.h"
#include "uid.h"                /* UID_debugString */
#include "main.h"               /* LISP_sndReply() */
#include "explain.h"

/* hg20081127+: MallocCompleteStringFromXmString() */
#include "xext.h"


/*** Defines ***************************************/

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET) 

/*** Prototypes ************************************/

static void createExpdOneOfMany( Widget );
static void createExpdOneOfMany2( Widget );
static void createExpdSomeOfMany( Widget );
static void createMsgDummy( Widget);

/*** Globals ***************************************/

static int    OneOfManyFmtsNo;
static char **OneOfManyFmtsList;
static char **OneOfManyArgListGot;

static int    OneOfMany2FmtsNo;
static char **OneOfMany2FmtsList;
static char **OneOfMany2ArgListGot;

static int    SomeOfManyFmtsNo;
static char **SomeOfManyFmtsList;
static char **SomeOfManyArgListGot;

static int    DummyMsgFmtsNo;
static char **DummyMsgFmtsList;
static char **DummyMsgArgListGot;

static Widget ExpdOneOfMany, ExpdOneOfManyLW;
static Widget ExpdOneOfMany2, ExpdOneOfMany2LW;
static Widget ExpdSomeOfMany, ExpdSomeOfManyLW;
static Widget EXPD_Dummy_Label;

/****************************************************
*   Expd: create EMA-XPS Explain PopupDialogs       *
****************************************************/

void
EXPD_create( Widget parent ) {

_dbg("ec01");
   /*** createExpdOneOfMany: EMA-XPS Waiting For Reply ***/
   createExpdOneOfMany( parent );

_dbg("ec02");
   /*** createExpdOneOfMany2: EMA-XPS Waiting For Reply ***/
      createExpdOneOfMany2( parent );
   
_dbg("ec03");
   /*** createExpdSomeOfMany: EMA-XPS Waiting For Reply ***/
   createExpdSomeOfMany( parent );

_dbg("ec04");
   createMsgDummy( parent );
_dbg("ec05");

}

/****************************************************
*   Expd: Miscelaneous                              *
****************************************************/

void
EXPD_popdown_all_dialogs( void ) {
   /*
    * In case of entering the debugger, offending user
    * replies to system popups should be discarded!
    * This routine pops down all system dialogs.
    * Used by CMD_clear().
    */

   if( XtIsManaged( ExpdOneOfMany ) ) {
      XtPopdown( XtParent(ExpdOneOfMany) );
      XtUnmanageChild( ExpdOneOfMany );
   }
   if( XtIsManaged( ExpdOneOfMany2 ) ) {
       XtPopdown( XtParent(ExpdOneOfMany2) );
       XtUnmanageChild( ExpdOneOfMany2 );
   }

   if( XtIsManaged( ExpdSomeOfMany ) ) {
      XtPopdown( XtParent(ExpdSomeOfMany) );
      XtUnmanageChild( ExpdSomeOfMany );
   }
}

/***************************************************/

static char*
GetNextPartStr( char *str ) {

   while( *str && *str != ETX ) str++;
   if( !*str )
      return( NULL );
   /* ETX found */
   *str = 0;
   return( ++str );
}

/***************************************************/


static char*
GetNextPartStrExpd( char *str ) {

   while( *str && *str != EOT ) str++;
   if( !*str )
      return( NULL );
   /* EOT found, sent via (emaxps-send-nil) as special separator */
   *str = 0;
   return( ++str );
}

/****************************************************
*      ExpdOneOfMany -->                            *
*         (uid-one-of-many '(l i s t) "string")     *
****************************************************/

static void
ExpdOneOfManyCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(ExpdOneOfMany) );
   XtUnmanageChild( ExpdOneOfMany );
}

static void
ExpdOneOfManyAcceptCB( void ) {
   char str[1024];
   int d, *p;

   if( XmListGetSelectedPos( ExpdOneOfManyLW, &p, &d ) ) {
      /* more than zero elements in list widget */
      sprintf( str, "%d", p[0]-1 );   /* (first ...) = (nth 0 ...) */
      XtFree( (char*)p );
   } else {
      ExpdOneOfManyCancelCB();
      return;
   }
   LISP_sndReply( str );
   XtPopdown( XtParent(ExpdOneOfMany) );
   XtUnmanageChild( ExpdOneOfMany );
}

static void
focusExpdOneOfManyCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_LIST );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

/************************** Create-Dummy **************************/

static void
createMsgDummy( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[3];
   int i,c,n;
   char *s,*a,*b;
   XmString p;

   /*
    * This widget will never be managed. It contains the initial
    * string resource list, used for the different kind of label texts
    * in the misc. EXPD popup dialogs.
    */
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   EXPD_Dummy_Label = XmCreateLabel( parent,"EXPD_Dummy_Label",args,n);
                                       
   XtVaGetValues( EXPD_Dummy_Label, XmNlabelString, &p, NULL );
   DummyMsgArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for(DummyMsgFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         DummyMsgFmtsNo++;
   }
   DummyMsgFmtsList = (char**)XtMalloc( (1+DummyMsgFmtsNo) * sizeof(char*));
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         DummyMsgFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( DummyMsgFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
   DummyMsgFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( DummyMsgFmtsList[i], b );
}

/****************************************************
* ExpdMessage  LISP --> C mit Text aus dem Ressource-File *
****************************************************/

void
ExpdMessage( void ) {
   char *str, *s, *tmp, *s0,*nr1, *nr, *hdl,*nextstr, *text;
   int c, i, len, n,argsGot,b,fmtsGot;
   XmString p;
   s0 = s = str = MSG_read();
   nr1 = GetNextPartStrExpd( str );
   str = s0;
   nr = GetNextPartStr( str );
   n = atoi(str);
   if( DummyMsgFmtsNo < n ) {
      fprintf(stderr, "ExpdMessage: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(DummyMsgFmtsList[n]) );
      strcpy( s, DummyMsgFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      DummyMsgArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( DummyMsgArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         fprintf( stderr, "ExpdMessage: maximum 20 args can be handled\n");
         break;
      }
   }   
   if( nr ) {
      DummyMsgArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( DummyMsgArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      fprintf( stderr, "ExpdMessage: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      fprintf( stderr, "ExpdMessage: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         DummyMsgArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( DummyMsgArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }

   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( DummyMsgArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            DummyMsgArgListGot[0],  DummyMsgArgListGot[1],  DummyMsgArgListGot[2],
            DummyMsgArgListGot[3],  DummyMsgArgListGot[4],  DummyMsgArgListGot[5], 
            DummyMsgArgListGot[6],  DummyMsgArgListGot[7],  DummyMsgArgListGot[8],
            DummyMsgArgListGot[9],  DummyMsgArgListGot[10], DummyMsgArgListGot[11],
            DummyMsgArgListGot[12], DummyMsgArgListGot[13], DummyMsgArgListGot[14],
            DummyMsgArgListGot[15], DummyMsgArgListGot[16], DummyMsgArgListGot[17],
            DummyMsgArgListGot[18], DummyMsgArgListGot[19] );
   XtFree( s );
   ExplainDisplayOutput( text );
   ExplainDisplayOutput( "\n" );
   Explain_PAD_show();  
   XtFree( text );
}
               
/**************************************************************************/

static void
createExpdOneOfMany( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[2];
   int i,c;
   char *s,*a,*b;
   XmString p;

_dbg("ceoom01");   
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
_dbg("ceoom02");   
   ExpdOneOfMany = XmCreateSelectionDialog( parent, "ExpdOneOfMany", args, 2 );
_dbg("ceoom03");   
   ExpdOneOfManyLW = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_LIST );
_dbg("ceoom04");   
   /* Hardcoded to make AcceptCB easier! */
   XtVaSetValues( ExpdOneOfManyLW, XmNselectionPolicy, XmBROWSE_SELECT, NULL );
_dbg("ceoom05");   
   ob = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_OK_BUTTON );
_dbg("ceoom06");   
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)ExpdOneOfManyAcceptCB, NULL );
_dbg("ceoom07");   
   cb = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_CANCEL_BUTTON );
_dbg("ceoom08");   
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)ExpdOneOfManyCancelCB, NULL );
_dbg("ceoom09");   
   ab = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_APPLY_BUTTON );
_dbg("ceoom10");   
   XtUnmanageChild( ab );
_dbg("ceoom11");   
   hb = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_HELP_BUTTON );
_dbg("ceoom12");   
   XtUnmanageChild( hb );
_dbg("ceoom13");   
   sl = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_SELECTION_LABEL );
_dbg("ceoom14");   
   XtUnmanageChild( sl );
_dbg("ceoom15");   
   dt = XmSelectionBoxGetChild( ExpdOneOfMany, XmDIALOG_TEXT );
_dbg("ceoom16");   
   XtUnmanageChild( dt );
_dbg("ceoom17");   

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( ExpdOneOfMany, XmNmapCallback, (XtPointer)focusExpdOneOfManyCB, NULL );
   
   
_dbg("ceoom18");   
   XtVaGetValues( ExpdOneOfMany, XmNlistLabelString, &p, NULL );
_dbg("ceoom19");   
   OneOfManyArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

_dbg("ceoom20");   
   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

_dbg("ceoom25");   
   for(OneOfManyFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         OneOfManyFmtsNo++;
   }
_dbg("ceoom26");   
   OneOfManyFmtsList = (char**)XtMalloc( (1+OneOfManyFmtsNo) * sizeof(char*));
_dbg("ceoom27");   
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         OneOfManyFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( OneOfManyFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
_dbg("ceoom28");   
   OneOfManyFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( OneOfManyFmtsList[i], b );
_dbg("ceoom29");   
   XtFree( s );
_dbg("ceoom30");   
}

void
readExpdOneOfMany( void ) {
   char *str, *s, *tmp, *s0, *nr0, *nr1, *nr, *hdl,*nextstr, *text;
   int c, i, len, n,argsGot,b,fmtsGot;
   XmString p;

   XmListDeleteAllItems( ExpdOneOfManyLW );
   nr0 = s0 = str = MSG_read();
   nr1 = GetNextPartStrExpd( str );

   nr = GetNextPartStr( str );
   
   n = atoi(str);
   if( OneOfManyFmtsNo < n ) {
      XBell( XtDisplay(ExpdOneOfMany), 50 );
      fprintf(stderr, "readExpd1N: Warning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(OneOfManyFmtsList[n]) );
      strcpy( s, OneOfManyFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      OneOfManyArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( OneOfManyArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(ExpdOneOfMany), 50 );
         fprintf( stderr, "readExpd1N: Warning: maximum 20 args can be handled\n");
         break;
      }
   }
   if( nr ) {
      OneOfManyArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( OneOfManyArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(ExpdOneOfMany), 50 );
      fprintf( stderr, "readExpd1N: Warning: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(ExpdOneOfMany), 50 );
      fprintf( stderr, "readExpd1N: Warning: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         OneOfManyArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( OneOfManyArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }

   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( OneOfManyArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            OneOfManyArgListGot[0],  OneOfManyArgListGot[1],  OneOfManyArgListGot[2],
            OneOfManyArgListGot[3],  OneOfManyArgListGot[4],  OneOfManyArgListGot[5], 
            OneOfManyArgListGot[6],  OneOfManyArgListGot[7],  OneOfManyArgListGot[8],
            OneOfManyArgListGot[9],  OneOfManyArgListGot[10], OneOfManyArgListGot[11],
            OneOfManyArgListGot[12], OneOfManyArgListGot[13], OneOfManyArgListGot[14],
            OneOfManyArgListGot[15], OneOfManyArgListGot[16], OneOfManyArgListGot[17],
            OneOfManyArgListGot[18], OneOfManyArgListGot[19] );
   XtFree( s );
   str = s = s0 = nr1;

   /*for(i=0;c=str[i];i++)if(c>=' ')printf("%c",c);else printf("^%c",c+'@');puts("");*/
   len = strlen( str );
   nr = 0;
   do {
      c = *s;
      if( c == ETX || !c ) {
         *s = NUL;
         XmListAddItem( ExpdOneOfManyLW, p=CSTRING(s0), nr );
         XmStringFree( p );
         s0 = s+1;
         nr++;
      }
      s++;
   } while( c );
   XtVaSetValues( ExpdOneOfMany,
      XmNlistLabelString, p=CSTRING(text),
      NULL );
   XmStringFree( p );
   XmListSelectPos( ExpdOneOfManyLW, 1, False );

   XtFree( nr0 );  /* explicitly! */
   XtFree( text );

   XtManageChild( ExpdOneOfMany );
   XtPopup( XtParent(ExpdOneOfMany), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusExpdOneOfManyCB, NULL );
}

/****************************************************
*      ExpdOneOfMany2 -->                           *
*         Spezieller ExpdOneOfMany, mit 4 vorde-    * 
*         finierte Eintraegen                       *
****************************************************/

static void
ExpdOneOfMany2CancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(ExpdOneOfMany2) );
   XtUnmanageChild( ExpdOneOfMany2 );
}

static void
ExpdOneOfMany2AcceptCB( void ) {
   char str[1024];
   int d, *p;

   if( XmListGetSelectedPos( ExpdOneOfMany2LW, &p, &d ) ) {
      /* more than zero elements in list widget */
      sprintf( str, "%d", p[0]-1 );   /* (first ...) = (nth 0 ...) */
      XtFree( (char*)p );
   } else {
      ExpdOneOfMany2CancelCB();
      return;
   }
   LISP_sndReply( str );
   XtPopdown( XtParent(ExpdOneOfMany2) );
   XtUnmanageChild( ExpdOneOfMany2 );
}

static void
focusExpdOneOfMany2CB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_LIST );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createExpdOneOfMany2( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[2];
   int i,c;
   char *a,*b,*hdl,*s;
   XmString p;
   
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   ExpdOneOfMany2 = XmCreateSelectionDialog( parent, "ExpdOneOfMany2", args, 2 );
   ExpdOneOfMany2LW = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_LIST );
   /* Hardcoded to make AcceptCB easier! */
   XtVaSetValues( ExpdOneOfMany2LW, XmNselectionPolicy, XmBROWSE_SELECT, NULL );
   ob = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)ExpdOneOfMany2AcceptCB, NULL );
   cb = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)ExpdOneOfMany2CancelCB, NULL );
   ab = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( ab );
   hb = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   sl = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( sl );
   dt = XmSelectionBoxGetChild( ExpdOneOfMany2, XmDIALOG_TEXT );
   XtUnmanageChild( dt );

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( ExpdOneOfMany2, XmNmapCallback, (XtPointer)focusExpdOneOfMany2CB, NULL );
   
   XtVaGetValues( ExpdOneOfMany2, XmNlistLabelString, &p, NULL );
   OneOfMany2ArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for(OneOfMany2FmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         OneOfMany2FmtsNo++;
   }
   OneOfMany2FmtsList = (char**)XtMalloc( (1+OneOfMany2FmtsNo) * sizeof(char*));
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         OneOfMany2FmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( OneOfMany2FmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
   OneOfMany2FmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( OneOfMany2FmtsList[i], b );
   XtFree( s );
}

void
readExpdOneOfMany2( void ) {
   char *str, *s, *tmp, *s0,*nr1, *nr, *hdl,*nextstr, *text;
   int c, i, len, n,argsGot,b,fmtsGot;
   char *s1, *s2,*s3,*s4;
   XmString p;

   XmListDeleteAllItems( ExpdOneOfMany2LW );
   hdl = XtMalloc( 1 + strlen(OneOfMany2FmtsList[0]) );
   strcpy(hdl , OneOfMany2FmtsList[0] );
   s1 = XtMalloc( 1 + strlen(OneOfMany2FmtsList[1]) );
   strcpy(s1 , OneOfMany2FmtsList[1] );
   s2 = XtMalloc( 1 + strlen(OneOfMany2FmtsList[2]) );
   strcpy(s2 , OneOfMany2FmtsList[2] );
   s3 = XtMalloc( 1 + strlen(OneOfMany2FmtsList[3]) );
   strcpy(s3 , OneOfMany2FmtsList[3] );
   s4 = XtMalloc( 1 + strlen(OneOfMany2FmtsList[4]) );
   strcpy(s4 , OneOfMany2FmtsList[4] );

   nr = 0;
   XmListAddItem( ExpdOneOfMany2LW, p=CSTRING(s1), ++nr );
   XmStringFree( p );
   XmListAddItem( ExpdOneOfMany2LW, p=CSTRING(s2), ++nr );
   XmStringFree( p );
   XmListAddItem( ExpdOneOfMany2LW, p=CSTRING(s3), ++nr );
   XmStringFree( p );
   XmListAddItem( ExpdOneOfMany2LW, p=CSTRING(s4), ++nr );
   XmStringFree( p );
   XtVaSetValues( ExpdOneOfMany2,
      XmNlistLabelString, p=CSTRING(hdl),
      NULL );
   XmStringFree( p );
   XmListSelectPos( ExpdOneOfMany2LW, 1, False );

   XtFree(hdl);
   XtFree(s1);
   XtFree(s2);
   XtFree(s3);
   XtFree(s4);

   XtManageChild( ExpdOneOfMany2 );
   XtPopup( XtParent(ExpdOneOfMany2), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusExpdOneOfMany2CB, NULL );
}

/****************************************************
*      ExpdSomeOfMany -->                           *
*         (uid-some-of-many '(l i s t) "string")    *
****************************************************/

static void
ExpdSomeOfManyCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(ExpdSomeOfMany) );
   XtUnmanageChild( ExpdSomeOfMany );
}

static void
ExpdSomeOfManyAcceptCB( void ) {
   char str[1024], tmp[1024];
   int i, d, *p;

   if( XmListGetSelectedPos( ExpdSomeOfManyLW, &p, &d ) ) {
      /* more than zero elements in list widget */
      strcpy( str, "(" );
      for( i = 0; i < d; i++ ) {
         sprintf( tmp, "%d ", p[i]-1 ); /* (first ...) = (nth 0 ...) */
         strcat( str, tmp );
      }
      strcat( str, ")" );
      XtFree( (char*)p );
   } else {
      ExpdSomeOfManyCancelCB();
      return;
   }
   LISP_sndReply( str );
   XtPopdown( XtParent(ExpdSomeOfMany) );
   XtUnmanageChild( ExpdSomeOfMany );
}

static void
focusExpdSomeOfManyCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_LIST );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createExpdSomeOfMany( Widget parent ) {
   Widget ob, ab, cb, hb, sl, dt;
   Arg args[2];
   int i,c;
   char *s,*a,*b;
   XmString p;
   
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   ExpdSomeOfMany = XmCreateSelectionDialog( parent, "ExpdSomeOfMany", args, 2 );
   ExpdSomeOfManyLW = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_LIST );
   /* Hardcoded to make AcceptCB easier! */
   XtVaSetValues( ExpdSomeOfManyLW, XmNselectionPolicy, XmMULTIPLE_SELECT, NULL );
   ob = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)ExpdSomeOfManyAcceptCB, NULL );
   cb = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)ExpdSomeOfManyCancelCB, NULL );
   ab = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( ab );
   hb = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   sl = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( sl );
   dt = XmSelectionBoxGetChild( ExpdSomeOfMany, XmDIALOG_TEXT );
   XtUnmanageChild( dt );

   /* sometimes it works, sometimes it won't ... */
   XtAddCallback( ExpdSomeOfMany, XmNmapCallback, (XtPointer)focusExpdSomeOfManyCB, NULL );
   
   XtVaGetValues( ExpdSomeOfMany, XmNlistLabelString, &p, NULL );
   SomeOfManyArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for(SomeOfManyFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         SomeOfManyFmtsNo++;
   }
   SomeOfManyFmtsList = (char**)XtMalloc( (1+SomeOfManyFmtsNo) * sizeof(char*));
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         SomeOfManyFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( SomeOfManyFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
   SomeOfManyFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( SomeOfManyFmtsList[i], b );
   XtFree( s );
}

void
readExpdSomeOfMany( void ) {
   char *str, *s, *tmp, *s0, *nr0, *nr1, *nr, *hdl,*nextstr, *text;
   int c, i, len, n,argsGot,b,fmtsGot;
   XmString p;

   XmListDeleteAllItems( ExpdSomeOfManyLW );
   nr0 = s0 = s = str = MSG_read();
   nr1 = GetNextPartStrExpd( str );
   str = s0;
   nr = GetNextPartStr( str );
   
   n = atoi(str);
   if( SomeOfManyFmtsNo < n ) {
      XBell( XtDisplay(ExpdSomeOfMany), 50 );
      fprintf(stderr, "readExpdmN: Warning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(SomeOfManyFmtsList[n]) );
      strcpy( s, SomeOfManyFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      SomeOfManyArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( SomeOfManyArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(ExpdSomeOfMany), 50 );
         fprintf( stderr, "readExpdmN: Warning: maximum 20 args can be handled\n");
         break;
      }
   }   
   if( nr ) {
      SomeOfManyArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( SomeOfManyArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(ExpdSomeOfMany), 50 );
      fprintf( stderr, "readExpdmN: Warning: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(ExpdSomeOfMany), 50 );
      fprintf( stderr, "readExpdmN: Warning: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         SomeOfManyArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( SomeOfManyArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }

   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( SomeOfManyArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            SomeOfManyArgListGot[0],  SomeOfManyArgListGot[1],  SomeOfManyArgListGot[2],
            SomeOfManyArgListGot[3],  SomeOfManyArgListGot[4],  SomeOfManyArgListGot[5], 
            SomeOfManyArgListGot[6],  SomeOfManyArgListGot[7],  SomeOfManyArgListGot[8],
            SomeOfManyArgListGot[9],  SomeOfManyArgListGot[10], SomeOfManyArgListGot[11],
            SomeOfManyArgListGot[12], SomeOfManyArgListGot[13], SomeOfManyArgListGot[14],
            SomeOfManyArgListGot[15], SomeOfManyArgListGot[16], SomeOfManyArgListGot[17],
            SomeOfManyArgListGot[18], SomeOfManyArgListGot[19] );
   XtFree( s );
   str = s = s0 = nr1;

   /*for(i=0;c=str[i];i++)if(c>=' ')printf("%c",c);else printf("^%c",c+'@');puts("");*/
   len = strlen( str );
   nr = 0;
   do {
      c = *s;
      if( c == ETX || !c ) {
         *s = NUL;
         XmListAddItem( ExpdSomeOfManyLW, p=CSTRING(s0), nr );
         XmStringFree( p );
         s0 = s+1;
         nr++;
      }
      s++;
   } while( c );
   XtVaSetValues( ExpdSomeOfMany,
      XmNlistLabelString, p=CSTRING(text),
      NULL );
   XmStringFree( p );
   XmListSelectPos( ExpdSomeOfManyLW, 1, False );

   XtFree( nr0 );  /* explicitly! */
   XtFree ( text );

   XtManageChild( ExpdSomeOfMany );
   XtPopup( XtParent(ExpdSomeOfMany), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusExpdSomeOfManyCB, NULL );
}

/*** EOF ***/

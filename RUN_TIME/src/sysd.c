#if 0
#define _dbg(a)    puts("sysd: " a)
#define _dbgs(a) printf("sysd: [%s]\n",a)
#define _dbgx(a) printf("sysd: 0x%X\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#define _dbgx(a)
#endif

/****************************************************
*   sysd.c        Hans Groschwitz        06.10.94   *
*                 Stephan Peters                    *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   Popups for the Main-Menu of EMA-XPS             *
*                                                   *
*   See uid.x for details!                          *
****************************************************/

#include <stdio.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>

#include "main.h"               /* LISP_sndReply() */
#include "uid.h"                /* UID_debugString */

/* hg20081127+: MallocCompleteStringFromXmString() */
#include "xext.h"

/*** Prototypes ************************************/

static void createSysdWarning( Widget );
static void createSysdInformation( Widget );
static void createSysdAcceptCancel( Widget );
static void createSysdPrompt( Widget );

/*** Globals ***************************************/

static int    warningFmtsNo;
static char **warningFmtsList;
static Widget sysdWarning;
static char **warningArgListGot;

static int    informationFmtsNo;
static char **informationFmtsList;
static Widget sysdInformation;
static char **informationArgListGot;

static int    acceptCancelFmtsNo;
static char **acceptCancelFmtsList;
static Widget sysdAcceptCancel;
static char **acceptCancelArgListGot;

static int    promptFmtsNo;
static char **promptFmtsList;
static Widget sysdPrompt;
static char **promptArgListGot;
static XmString sysdPromptEmpty;

/****************************************************
*   Sysd: create EMA-XPS system PopupDialogs        *
****************************************************/

void
SYSD_create( Widget parent ) {

_dbg("sc01");
   /*** createSysdWarning: EMA-XPS Waiting For Reply ***/
   createSysdWarning( parent );

_dbg("sc02");
   /*** createSysdInformation: EMA-XPS Waiting For Reply ***/
   createSysdInformation( parent );

_dbg("sc03");
   /*** createSysdAcceptCancel: EMA-XPS Waiting For Reply ***/
   createSysdAcceptCancel( parent );

_dbg("sc04");
   /*** createSysdPrompt: EMA-XPS Waiting For Reply ***/
   createSysdPrompt( parent );
_dbg("sc05");
}

/****************************************************
*   Sysd: Miscelaneous                              *
****************************************************/

void
SYSD_popdown_all_dialogs( void ) {
   /*
    * In case of entering the debugger, offending user
    * replies to system popups should be discarded!
    * This routine pops down all system dialogs.
    * Used by CMD_clear().
    */

   if( XtIsManaged( sysdWarning ) ) {
      XtPopdown( XtParent(sysdWarning) );
      XtUnmanageChild( sysdWarning );
   }
   if( XtIsManaged( sysdInformation ) ) {
      XtPopdown( XtParent(sysdInformation) );
      XtUnmanageChild( sysdInformation );
   }
   if( XtIsManaged( sysdAcceptCancel ) ) {
      XtPopdown( XtParent(sysdAcceptCancel) );
      XtUnmanageChild( sysdAcceptCancel );
   }
   if( XtIsManaged( sysdPrompt ) ) {
      XtPopdown( XtParent(sysdPrompt) );
      XtUnmanageChild( sysdPrompt );
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

/****************************************************
*   SysdWarning                                     *
****************************************************/

static void
SysdWarningOkCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(sysdWarning) );
   XtUnmanageChild( sysdWarning );
}

static void
createSysdWarning( Widget parent ) {
   Widget ob, cb, hb;
   XmString p;
   char *s;
   Arg args[3];
   char *a, *b;
   int i, c;

_dbg("csw01");
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[2], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL );
_dbg("csw02");
   sysdWarning = XmCreateWarningDialog( parent,
                       "SysdWarning", args, 3 );
_dbg("csw03");
   ob = XmMessageBoxGetChild( sysdWarning, XmDIALOG_OK_BUTTON );
   /*
    * Those buttons are Gadgets and hence the do not have resources
    * for fg and bg color !!!
    */
_dbg("csw04");
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)SysdWarningOkCB, NULL );
_dbg("csw05");
   cb = XmMessageBoxGetChild( sysdWarning, XmDIALOG_CANCEL_BUTTON );
_dbg("csw06");
   XtUnmanageChild( cb );
_dbg("csw07");
   hb = XmMessageBoxGetChild( sysdWarning, XmDIALOG_HELP_BUTTON );
_dbg("csw08");
   XtUnmanageChild( hb );
_dbg("csw09");

   XtVaGetValues( sysdWarning, XmNmessageString, &p, NULL );
_dbg("csw10");
   warningArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );
_dbg("csw11");

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

_dbg("csw16");
   for( warningFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         warningFmtsNo++;
   }
_dbg("csw17");
   warningFmtsList = (char**)XtMalloc( (1+warningFmtsNo) * sizeof(char*));
_dbg("csw18");
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         warningFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( warningFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
_dbg("csw19");
   warningFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( warningFmtsList[i], b );

_dbg("csw20");
   XtFree( s );
_dbg("csw21");
}

void
readSysdWarning( void ) {
   char *tmp, *s, *nr, *str, *nextstr, *text;
   int  argsGot, b, n, fmtsGot, len;
   XmString xmtext;

   str = MSG_read();
   nr = GetNextPartStr( str );
   n = atoi(str);

   if( warningFmtsNo < n ) {
      XBell( XtDisplay(sysdWarning), 50 );
      fprintf(stderr, "readSysdWarning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(warningFmtsList[n]) );
      strcpy( s, warningFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      warningArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( warningArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(sysdWarning), 50 );
         fprintf( stderr, "readSysdWarning: maximum 20 args can be handled\n");
         break;
      }
   }   
   if( nr ) {
      warningArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( warningArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(sysdWarning), 50 );
      fprintf( stderr, "readSysdWarning: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(sysdWarning), 50 );
      fprintf( stderr, "readSysdWarning: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         warningArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( warningArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }

   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( warningArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            warningArgListGot[0],  warningArgListGot[1],  warningArgListGot[2],
            warningArgListGot[3],  warningArgListGot[4],  warningArgListGot[5], 
            warningArgListGot[6],  warningArgListGot[7],  warningArgListGot[8],
            warningArgListGot[9],  warningArgListGot[10], warningArgListGot[11],
            warningArgListGot[12], warningArgListGot[13], warningArgListGot[14],
            warningArgListGot[15], warningArgListGot[16], warningArgListGot[17],
            warningArgListGot[18], warningArgListGot[19] );
   XtFree( s );

   /*
    * XmStringCreate and XmStringCreateSimple
    * canNOT display MULTI-lined texts !!!
    */
   xmtext = XmStringCreateLtoR( text, XmSTRING_DEFAULT_CHARSET );
   XtFree( text );
   XtVaSetValues( sysdWarning, XmNmessageString, xmtext, NULL );
   XtManageChild( sysdWarning );
          
   for( b = 0; b < argsGot; b++ )
      XtFree(warningArgListGot[b]);

   XmStringFree( xmtext );                     
   XtFree( str );
}

/****************************************************
*   SysdInformation (KB-Statistics)                 *
****************************************************/

static void
SysdInformationOkCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(sysdInformation) );
   XtUnmanageChild( sysdInformation );
}

static void
createSysdInformation( Widget parent ) {
   Widget ob, cb, hb;
   XmString p;
   char *s;
   Arg args[3];
   char *a, *b;
   int i, c;

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[2], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL );
   sysdInformation = XmCreateInformationDialog( parent,
                       "SysdInformation", args, 3 );
   ob = XmMessageBoxGetChild( sysdInformation, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)SysdInformationOkCB, NULL );
   cb = XmMessageBoxGetChild( sysdInformation, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( cb );
   hb = XmMessageBoxGetChild( sysdInformation, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );

   XtVaGetValues( sysdInformation, XmNmessageString, &p, NULL );
   informationArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for( informationFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         informationFmtsNo++;
   }
   informationFmtsList = (char**)XtMalloc( (1+informationFmtsNo) * sizeof(char*));
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         informationFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( informationFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
   informationFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( informationFmtsList[i], b );

   XtFree( s );
}

void
readSysdInformation( void ) {
   char *tmp, *s, *nr, *str, *nextstr, *text;
   int  argsGot, b, n, fmtsGot, len;
   XmString xmtext;

   str = MSG_read();
   nr = GetNextPartStr( str );
   n = atoi(str);

   if( informationFmtsNo < n ) {
      XBell( XtDisplay(sysdInformation), 50 );
      fprintf(stderr, "readSysdWarning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(informationFmtsList[n]) );
      strcpy( s, informationFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      informationArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( informationArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(sysdInformation), 50 );
         fprintf( stderr, "readSysdInformation: maximum 20 args can be handled\n");
         break;
      }
   }   
   if( nr ) {
      informationArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( informationArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(sysdInformation), 50 );
      fprintf( stderr, "readSysdInformation: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(sysdInformation), 50 );
      fprintf( stderr, "readSysdInformation: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         informationArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( informationArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }
      
   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( informationArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            informationArgListGot[0],  informationArgListGot[1],  informationArgListGot[2],
            informationArgListGot[3],  informationArgListGot[4],  informationArgListGot[5], 
            informationArgListGot[6],  informationArgListGot[7],  informationArgListGot[8],
            informationArgListGot[9],  informationArgListGot[10], informationArgListGot[11],
            informationArgListGot[12], informationArgListGot[13], informationArgListGot[14],
            informationArgListGot[15], informationArgListGot[16], informationArgListGot[17],
            informationArgListGot[18], informationArgListGot[19] );
   XtFree( s );

   xmtext = XmStringCreateLtoR( text, XmSTRING_DEFAULT_CHARSET );
   XtFree( text );
   XtVaSetValues( sysdInformation, XmNmessageString, xmtext, NULL );
   XtManageChild( sysdInformation );
          
   for( b = 0; b < argsGot; b++ )
      XtFree(informationArgListGot[b]);

   XmStringFree( xmtext );                     
   XtFree( str );
}

/****************************************************
*   SysdAcceptCancel                                *
****************************************************/

static void
SysdAcceptCancelOkCB( void ) {
   LISP_sndReply( "T" );
   XtPopdown( XtParent(sysdAcceptCancel) );
   XtUnmanageChild( sysdAcceptCancel );
}

static void
SysdAcceptCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(sysdAcceptCancel) );
   XtUnmanageChild( sysdAcceptCancel );
}

static void
createSysdAcceptCancel( Widget parent ) {
   Widget ob, cb, hb;
   XmString p;
   char *s;
   Arg args[3];
   char *a, *b;
   int i, c;

_dbg("csac01");
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[2], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL );
_dbg("csac02");
   sysdAcceptCancel = XmCreateWarningDialog( parent,
                       "SysdAcceptCancel", args, 3 );
_dbg("csac03");
   ob = XmMessageBoxGetChild( sysdAcceptCancel, XmDIALOG_OK_BUTTON );
_dbg("csac04");
   XtAddCallback( ob, XmNactivateCallback, (XtPointer)SysdAcceptCancelOkCB, NULL );
_dbg("csac05");
   cb = XmMessageBoxGetChild( sysdAcceptCancel, XmDIALOG_CANCEL_BUTTON );
_dbg("csac06");
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)SysdAcceptCancelCB, NULL );
_dbg("csac07");
   hb = XmMessageBoxGetChild( sysdAcceptCancel, XmDIALOG_HELP_BUTTON );
_dbg("csac08");
   XtUnmanageChild( hb );
_dbg("csac09");

   XtVaGetValues( sysdAcceptCancel, XmNmessageString, &p, NULL );
_dbg("csac10");
   acceptCancelArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );
_dbg("csac11");

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for( acceptCancelFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         acceptCancelFmtsNo++;
   }
_dbg("csac18");
   acceptCancelFmtsList = (char**)XtMalloc( (1+acceptCancelFmtsNo) * sizeof(char*));
_dbg("csac19");
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         acceptCancelFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( acceptCancelFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
_dbg("csac20");
   acceptCancelFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( acceptCancelFmtsList[i], b );

_dbg("csac21");
   XtFree( s );
_dbg("csac22");
}

void
readSysdAcceptCancel( void ) {
   char *tmp, *s, *nr, *str, *nextstr, *text;
   int  argsGot, b, n, fmtsGot, len;
   XmString xmtext;

   str = MSG_read();
   nr = GetNextPartStr( str );
   n = atoi(str);

   if( acceptCancelFmtsNo < n ) {
      XBell( XtDisplay(sysdAcceptCancel), 50 );
      fprintf(stderr, "readSysdWarning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(acceptCancelFmtsList[n]) );
      strcpy( s, acceptCancelFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      acceptCancelArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( acceptCancelArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(sysdAcceptCancel), 50 );
         fprintf( stderr, "readSysdAcceptCancel: maximum 20 args can be handled\n");
         break;
      }
   }   
   if( nr ) {
      acceptCancelArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( acceptCancelArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(sysdAcceptCancel), 50 );
      fprintf( stderr, "readSysdAcceptCancel: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(sysdAcceptCancel), 50 );
      fprintf( stderr, "readSysdAcceptCancel: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         acceptCancelArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( acceptCancelArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }
      
   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( acceptCancelArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            acceptCancelArgListGot[0],  acceptCancelArgListGot[1],  acceptCancelArgListGot[2],
            acceptCancelArgListGot[3],  acceptCancelArgListGot[4],  acceptCancelArgListGot[5], 
            acceptCancelArgListGot[6],  acceptCancelArgListGot[7],  acceptCancelArgListGot[8],
            acceptCancelArgListGot[9],  acceptCancelArgListGot[10], acceptCancelArgListGot[11],
            acceptCancelArgListGot[12], acceptCancelArgListGot[13], acceptCancelArgListGot[14],
            acceptCancelArgListGot[15], acceptCancelArgListGot[16], acceptCancelArgListGot[17],
            acceptCancelArgListGot[18], acceptCancelArgListGot[19] );
   XtFree( s );

   xmtext = XmStringCreateLtoR( text, XmSTRING_DEFAULT_CHARSET );
   XtFree( text );
   XtVaSetValues( sysdAcceptCancel, XmNmessageString, xmtext, NULL );
   XtManageChild( sysdAcceptCancel );
          
   for( b = 0; b < argsGot; b++ )
      XtFree(acceptCancelArgListGot[b]);

   XmStringFree( xmtext );                     
   XtFree( str );
}

/****************************************************
*   SysdPrompt                                      *
****************************************************/

static void
SysdPromptOkCB( Widget w, XtPointer cd,
                   XmSelectionBoxCallbackStruct *cbs ) {
   char str[1024], *value = NULL;

   XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &value );
   if( value && *value )   /* it should be a non-empty string! */
      sprintf( str, "%s", value );
   else    /* OK pressed without anything entered ! */
      sprintf( str, "NIL" );
   XtFree(value);
   LISP_sndReply( str );
   XtPopdown( XtParent(sysdPrompt) );
   XtUnmanageChild( sysdPrompt );
}

static void
SysdPromptCancelCB( void ) {
   LISP_sndReply( "NIL" );
   XtPopdown( XtParent(sysdPrompt) );
   XtUnmanageChild( sysdPrompt );
}

static void
focusSysdPromptCB( void ) {
   Widget dt;

   /* move input fokus to text (when managed)! */
   dt = XmSelectionBoxGetChild( sysdPrompt, XmDIALOG_TEXT );
   XmProcessTraversal( dt, XmTRAVERSE_CURRENT );
}

static void
createSysdPrompt( Widget parent ) {
   Widget ob, cb, hb;
   XmString p;
   char *s;
   Arg args[3];
   char *a, *b;
   int i, c;

   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[2], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL );
   sysdPrompt = XmCreatePromptDialog( parent, "SysdPrompt", args, 3 );

   ob = XmSelectionBoxGetChild( sysdPrompt, XmDIALOG_OK_BUTTON );
   XtAddCallback( sysdPrompt, XmNokCallback, (XtPointer)SysdPromptOkCB, NULL );
   cb = XmSelectionBoxGetChild( sysdPrompt, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback, (XtPointer)SysdPromptCancelCB, NULL );
   hb = XmSelectionBoxGetChild( sysdPrompt, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );

   XtVaGetValues( sysdPrompt, XmNselectionLabelString, &p, NULL );
   promptArgListGot = (char**)XtMalloc( 20 * sizeof(char*) );

   /*** read the Xresource data ***/
   s = MallocCompleteStringFromXmString( p );
   XmStringFree( p );

   for( promptFmtsNo = 0, a = s; *a; a++ ) {
      if( *a == '|' )
         promptFmtsNo++;
   }
   promptFmtsList = (char**)XtMalloc( (1+promptFmtsNo) * sizeof(char*));
   for( i = 0, a = b = s; *a; a++ ) {
      if( *a == '|' ) {
         *a = 0;
         promptFmtsList[i] = XtMalloc( 1+strlen(b) );
         strcpy( promptFmtsList[i], b );
         i++;
         b = a + 1;
      }
   }
   promptFmtsList[i] = XtMalloc( 1+strlen(b) );
   strcpy( promptFmtsList[i], b );

   XtFree( s );
}

void
readSysdPrompt( void ) {
   char *tmp, *s, *nr, *str, *nextstr, *text;
   int  argsGot, b, n, fmtsGot, len;
   XmString xmtext;

   str = MSG_read();
   nr = GetNextPartStr( str );
   n = atoi(str);

   if( promptFmtsNo < n ) {
      XBell( XtDisplay(sysdPrompt), 50 );
      fprintf(stderr, "readSysdWarning: no format string for this code\n");
      s = XtMalloc( 1 );
      *s = 0;
   } else {
      s = XtMalloc( 1 + strlen(promptFmtsList[n]) );
      strcpy( s, promptFmtsList[n] );
   }

   argsGot = 0;
   while( nr && (nextstr = GetNextPartStr(nr)) ) {
      promptArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( promptArgListGot[argsGot], nr );
      nr = nextstr;
      argsGot++;

      if( argsGot > 19 ) {
         XBell( XtDisplay(sysdPrompt), 50 );
         fprintf( stderr, "readSysdPrompt: maximum 20 args can be handled\n");
         break;
      }
   }
   if( nr ) {
      promptArgListGot[argsGot] = XtMalloc( 1 + ( strlen( nr )));
      strcpy( promptArgListGot[argsGot++], nr );
   }
   fmtsGot = 0;
   tmp = s;
   while( *tmp != 0 ) {
      if( *tmp++ == '%')
         fmtsGot++;
   }      

   if( fmtsGot < argsGot ) {
      XBell( XtDisplay(sysdPrompt), 50 );
      fprintf( stderr, "readSysdPrompt: got more args than presumed\n");

      for( b = fmtsGot; b < argsGot; b++ ) {
         strcat( s, "\n[%s]" );
      }
   } else if( fmtsGot > argsGot ) {
      XBell( XtDisplay(sysdPrompt), 50 );
      fprintf( stderr, "readSysdPrompt: got less args than presumed\n");

      for( b = argsGot; b < fmtsGot; b++ ) {
         promptArgListGot[b] = (char*)XtMalloc( 4 );
         strcpy( promptArgListGot[b], "(?)" );
      }
      argsGot = fmtsGot;
   }

   len = 1 + strlen( s );
   for( b = 0; b < argsGot; b++ )
      len += strlen( promptArgListGot[b] );

   text = XtMalloc(len);
   sprintf( text, s,
            promptArgListGot[0],  promptArgListGot[1],  promptArgListGot[2],
            promptArgListGot[3],  promptArgListGot[4],  promptArgListGot[5], 
            promptArgListGot[6],  promptArgListGot[7],  promptArgListGot[8],
            promptArgListGot[9],  promptArgListGot[10], promptArgListGot[11],
            promptArgListGot[12], promptArgListGot[13], promptArgListGot[14],
            promptArgListGot[15], promptArgListGot[16], promptArgListGot[17],
            promptArgListGot[18], promptArgListGot[19] );
   XtFree( s );

   xmtext = XmStringCreateLtoR( text, XmSTRING_DEFAULT_CHARSET );
   XtFree( text );

   XtVaSetValues( sysdPrompt, XmNselectionLabelString, xmtext,
                              XmNtextString, sysdPromptEmpty, NULL );

   XtManageChild( sysdPrompt );

   for( b = 0; b < argsGot; b++ )
      XtFree(promptArgListGot[b]);

   XmStringFree( xmtext );                     
   XtFree( str );
}

/*** EOF ***/

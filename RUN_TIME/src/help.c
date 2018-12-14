#if 0
#define _dbg(a)    puts("help: " a)
#define _dbgs(a) printf("help: %s\n",a)
#define _dbgx(a) printf("help: 0x%X\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#define _dbgx(a)
#endif

/****************************************************
*   help.c        Hans Groschwitz        08.12.94   *
*                                                   *
*   Creates the help facility which reads the file  *
*   $XAPPLRESDIR/$LANG/ema-xps-help.txt from which  *
*   is separated into pages using the FF char.      *
*   The page is accessable via a number represen-   *
*   ting the desired page. The information is       *
*   displayed in a specialized PopupDialog, which   *
*   allowes paging. Pressing a Help button anywhere *
*   within EMA-XPS will popup this one instance.    *
****************************************************/

#include <stdio.h>                         /* FILE */

#include <Xm/PanedW.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/Protocols.h> /* XmAddWMProtocolCallback */

#include "main.h"      /* WM_DELETE_WINDOW, iconPm */
#include "help.h"
#include "xinit.h"                 /* UPCASE_input */

/*** Globals ***************************************/

#define FF    12   /* ASCII character FormFeed */

static int    actual_helpTextNr;
static Widget helpText, helpWindow;
static Widget Browse, Browser, BrowserLW, BrowserTW;
static char  *helptextpath;
static int    NumberOfHelpPages;
static Widget previous_button, next_button, Page;
static Widget warningW, BrowseWarn;
static char  *warningFormat;
static char **topics;     /* vectors will be allocated dynamically */
static int   *topics_no;
static int    BrowserSelectedPos = 1;

/*** Function Declarations *************************/

static void hideHelpWindowCB( void );
static void pagedHelpCB( Widget, XtPointer, XtPointer );
static void previousHelpCB( Widget, XtPointer, XtPointer );
static void nextHelpCB( Widget, XtPointer, XtPointer );

/*** Functions *************************************/

static void
showWarning( int nr ) {
   char *p = XtMalloc( strlen(warningFormat)+16 );
   XmString s;

   sprintf( p, warningFormat, nr );
   s = XmStringCreateSimple( p );
   XtVaSetValues( warningW, XmNmessageString, s, NULL );
   XmStringFree( s );
   XtFree( p );
   XtManageChild( warningW );
   XtPopup( XtParent(warningW), XtGrabNone );
}

/***************************************************/

static void
hideWarning( void ) {

   XtPopdown( XtParent(warningW) );
   XtUnmanageChild( warningW );
}

/***************************************************/

static void
createWarning( void ) {
   Widget cb, hb, ob;
   XmString p;
   char *ptr;

   /* --- HelpBox: Child of SystemDialogs --- */
   warningW = XmCreateWarningDialog( SystemDialogs, "HelpWarning", NULL, 0 );
   /* One-Button-Warningbox only... */
   cb = XmMessageBoxGetChild( warningW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( cb );
   hb = XmMessageBoxGetChild( warningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   ob = XmMessageBoxGetChild( warningW, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback,
      (XtPointer)hideWarning, NULL );
   /*
    * The original XResources contain a language dependent text
    * containting a '%d'. This is got and taken as a format string
    * to sprintf, which then sets the new (modified) String
    *
    * WARNING: If the String is multi-line'd, XmStringGetLtoR()
    * returns only the first line !!!
    */
   XtVaGetValues( warningW, XmNmessageString, &p, NULL );
   warningFormat = NULL;
   XmStringGetLtoR( p, XmSTRING_DEFAULT_CHARSET, &warningFormat );

   /* hg20081126: no mixing of static and allocated strings */
   if( !warningFormat ) {
      /** OLD: warningFormat = ""; **/
      warningFormat = XtMalloc( 4 );  /* hg20081127: bug fix */
      *warningFormat = 0;
   }
   if( !strstr( warningFormat, "%d" ) ) {
      fprintf( stderr,
         "WARNING: XmNmessageString resource for HelpWarning lacks a '%%d'.\n" );
      ptr = XtMalloc( strlen(warningFormat)+16 );
      strcpy( ptr, warningFormat );
      strcat( ptr, " [%d]" );
      XtFree( warningFormat );
      warningFormat = ptr;
   }
}

/****************************************************
*    Modified CommandName StringCompare             *
****************************************************/

static int
CharacterValue( int c ) {
#if 0
   sorting: ! # + - * / < = > { [ : $ 0-9 A-Z a-z % & ? \ ^ | ~
   rest are separators, returning 0
#endif
   static int v[128-32] = {
  /* SP, !, ", #, $, %, &, ', (, ), *, +, ,, -, ., / */
      0, 1, 0, 2,13,70,71, 0, 0, 0, 5, 3, 0, 4, 0, 6,

   /* 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, :, ;, <, =, >, ? */
     30,31,32,33,34,35,36,37,38,39,12, 0, 7, 8, 9,72,

   /* @, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O */
      0,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,

   /* P, Q, R, S, T, U, V, W, X, Y, Z, [, \, ], ^, _ */
     56,57,58,59,60,61,62,63,64,65,66,11,73, 0,74,40,

   /* `, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o */
      0,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,

   /* p, q, r, s, t, u, v, w, x, y, z, {, |, }, ~, BS */
     56,57,58,59,60,61,62,63,64,65,66,10,75, 0,76, 0 };

   if( c <= 32 || c >= 127 )
      return 0;
   return v[c-32];
}

static int
CommandCompareN( char *s1, char *s2, unsigned int n ) {
   int c1 = *s1, c2 = *s2, res;

   switch( n ) {
      case 0:
         return 0;
      case 1:
         return CharacterValue(c1) - CharacterValue(c2);
      default:
         if( res = CharacterValue(c1) - CharacterValue(c2) )
            return res;
         return strncmp( s1+1, s2+1, n-1 );
   }
}

static int
CommandCompare( char *s1, char *s2 ) {
   int c1 = *s1, c2 = *s2, res;

   if( res = CharacterValue(c1) - CharacterValue(c2) )
      return res;
   return strcmp( s1+1, s2+1 );
}

/****************************************************
*    Quicker-sort an array of Strings               *
****************************************************/

static void 
swap( char **v, int i, int j ) {
   /* Swap 2 pointers in the array */
   char *temp;
   int itemp;
 
   temp = v[i];		/* Pointer-Array */
   v[i] = v[j];
   v[j] = temp;

   itemp = topics_no[i];
   topics_no[i] = topics_no[j];
   topics_no[j] = itemp;
}

static void 
qwsort( char **v, int left, int right ) {
   /*
    * A modified quicker-sort
    * written by Jan Legenhausen for rule-sorting by 3to2
    *
    * char *rules[N];
    * activation: qwsort(rules,0,N-1);
    */
   int q, last;
 
   if( left >= right )
      return;
   swap( v, left, (left + right)/2 );
   last = left;
   for( q = left+1; q <= right; q++ )
      if( CommandCompare( v[q], v[left] ) < 0 )
         /* strcmp */
         swap( v, ++last, q );
   swap( v, left, last );
   qwsort( v, left, last-1 );
   qwsort( v, last+1, right );
}

/****************************************************
*   The Browser Popup                               *
****************************************************/

static void
showBrowser( char *text ) {
   if( !text )
      text = "";
   XmTextFieldSetString( BrowserTW, text );
   /* the above line activates an browseChangedCB !!! */
   XtManageChild( Browser );
   XtPopup( XtParent(Browser), XtGrabNone );
}

/***************************************************/

static void
showBrowserCB( Widget w ) {

   showBrowser( "" );
}

/***************************************************/

static Boolean
is_symbol_char( int c ) {
   if( c <= ' ' || c >= 127 )
      return False;
   if( c >= 'A' && c <= 'Z' )
      return True;
   if( c >= 'a' && c <= 'z' )
      return True;
   if( c >= '0' && c <= '9' )
      return True;
   switch( c ) {
      /* see ClTl2 !!! */
      case '+':
      case '-':
      case '*':
      case '/':
      case '@':
      case '$':
      case '^':
      case '&':
      case '_':
      case '=':
      case '<':
      case '>':
      case '~':
      case '.':
      /* B3-Extensions... */
      case ':':
      case '?':
      case '!':
      case '[':
      case ']':
      case '{':
      case '}':
      /* to make #' be viewable... (function ...) */
      case '#':   /* with ' being a separator :)) */
         return True;
   }
   return False;
}

/***************************************************/

static void
OptimizePresentation( char *p ) {
   /* use of capital letters is granted here! */
   char *q = p;

   while( *q > ' ' )
      q++;
   *q = 0;

   if( *p == '#' && p[1] == '\'' ) {
      p[2] = 0;
      return;
   }
   if( *p == '!' ) {
      p[1] = 0;
      return;
   }
   if( *p == '{' ) {
      p[1] = 0;
      return;
   }

   if( !strcmp( p, "HAS-PART" ) ) {
      strcpy( p, "[HAS-PART" );   /* length is granted! */
      return;
   }
   
   if( *p != '[' )
      return;
   p++;
   
   if( !strcmp( p, "AND" ) )
      return;
   if( !strcmp( p, "OR" ) )
      return;
   if( !strcmp( p, "NOT" ) )
      return;
   if( !strcmp( p, "HAS-PART" ) )
      return;
   *p = 0;
   return;
}

/***************************************************/

void
HELP_showBrowserCB( Widget w ) {
   Widget first_child_of_shell;
   XmTextPosition pos;
   char *p, *q, *r;
   int i, j, len;

   /*
    * assumed to be the HELP callback of the editors
    * Multiline-ScrolledText widgets !!!
    *
    * frame.c           slots
    * instance.c        slots
    * behavior.c        body
    * rule.c            if-body    then-body
    * prolog.c          axioms
    * constraint.c      body       condition-body
    * csnet.c           body
    * restriction.c     body
    * task.c            body
    * misc.c            constructs
    */

   pos = XmTextGetInsertionPosition( w );
   p = XmTextGetString( w );
   len = strlen( p );
   for( i = pos; i < len; i++ )
      if( !is_symbol_char(p[i]) ) {
         p[i] = 0;         /* EOS !!! */
         break;
      }
   for( i = pos-1; i >= 0; i-- )   /* pos-1 !!! */
      if( !is_symbol_char(p[i]) ) {
         i++;
         break;
      }
   if( i < 0 )
      i = 0;
   r = p + i;
   len = strlen(r);
   q = XtMalloc( 1+len );
   for( j = 0; q[j] = toupper(r[j]); j++ );
   XtFree( p );
   if( strlen(q) != len )
      puts("HELP_showBrowserCB: ???");

   if( !len ) {
      /*
       * in case of an empty selection send a fake Event
       * to get the most general help available with this
       * shell.
       * HERE: registered with the 1st child of the shell!
       */
      first_child_of_shell = w;
      for( w = XtParent(w); !XtIsShell(w); w = XtParent(w) )
         first_child_of_shell = w;
      XtCallCallbacks( first_child_of_shell, XmNhelpCallback, NULL );
   } else {
      /* cursor IS placed in a SYMBOL */
      showBrowser( q );
   }
   XtFree( q );
}

/***************************************************/

static void
hideBrowserCB( void ) {

   XtPopdown( XtParent(Browser) );
   XtUnmanageChild( Browser );
}

/***************************************************/

static void
BrowsedViewCB( Widget w, XtPointer cd,
               XmSelectionBoxCallbackStruct *cbs ) {
   int i = XmListItemPos( BrowserLW, cbs->value );
   if( !i ) { /* not found ?!? */
      puts("BrowsedViewCB: ???");
      return; /* done ... */
   }
   HELP_show( w, (XtPointer)topics_no[i-1], cd );
}

/***************************************************/

static void                       /* noMatchCB !!! */
browseWarnCB( Widget w, XtPointer cd,
              XmSelectionBoxCallbackStruct *cbs ) {
   int i;
/*
   XtManageChild( BrowseWarn );
   XtPopup( XtParent(BrowseWarn), XtGrabNone );
*/

   /* This way the POPUP will never appear!!! */
   i = BrowserSelectedPos;
   HELP_show( w, (XtPointer)topics_no[i-1], cd );
}

/***************************************************/

static void
browseChangedCB( void ) {
   char *pp, *q = XmTextFieldGetString( BrowserTW );
   int i, top, count;
   char *p = XtMalloc( strlen(q)+2 );  /* see OptimizePresentation */
   
   strcpy( p, q );
   /* if pasting using the mouse, this grants upcasing */
   for( pp = p; *pp; pp++ )
      *pp = toupper(*pp);
   
   if( strcmp( q, p ) ) {    /* they differ... */
      XmTextFieldSetString( BrowserTW, p );
      /* the above line will reactivate the browseChangedCB */
      XtFree( p );
      XtFree( q );
      return;
   }
   XtFree( q );
   /*
    * the following line won't modify the inputfield, but
    * improves placement within the List...
    */
   OptimizePresentation( p );

   for( i = NumberOfHelpPages-1; i >= 0; i-- ) {
      if( CommandCompareN( topics[i], p, strlen(p) ) < 0 ) {
         /* strncmp */
         i++; /* the one tested before this one was fine */
         break;    /* "A" < "Z" */
      }
   }
   i++;
   XtFree( p );

   if( i <= 0 )
      i = 1;
   else if( i >= NumberOfHelpPages )
      i = NumberOfHelpPages;
   XmListSelectPos( BrowserLW, i, False );
   /* monitors ANY change, via mouse AND via keyboard! */
   BrowserSelectedPos = i;

   XtVaGetValues( BrowserLW,
      XmNtopItemPosition, &top,
      XmNvisibleItemCount, &count,
      NULL );
   /* top is the topmost visible item's number */
   /* count is the number of items visible at a time */
   if( i >= top && i < (top+count) ) {
      /* seems, the selection is visible! */
      return;
   }
   i-= (count/2);
   if( i > (NumberOfHelpPages-count+1) )  /* order! */
      i = (NumberOfHelpPages-count+1);
   if( i <= 0 )
      i = 1;
   XmListSetPos( BrowserLW, i );
   /* now the selection IS visible */
}

/***************************************************/

static void
createBrowser( void ) {
   Widget ab, cb, hb;
   XmString p;
   char *ptr;
   Arg args[4];
   int i;
   XmString s;

_dbg("cb01");
   /* --- Browser: Child of SystemDialogs --- */
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNmustMatch, True );
   XtSetArg( args[2], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[3], XmNautoUnmanage, False );
_dbg("cb02");
   Browser = XmCreateSelectionDialog( SystemDialogs, "Browser", args, 4 );
   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
_dbg("cb03");
   XmAddWMProtocolCallback( XtParent(Browser), WM_DELETE_WINDOW,
      (XtPointer)hideBrowserCB, NULL );    /* Pop down ... */
            
_dbg("cb04");
   /* now let's have a look at its children... */
   cb = XmSelectionBoxGetChild( Browser, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( cb );
_dbg("cb05");
   hb = XmSelectionBoxGetChild( Browser, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
_dbg("cb06");
   ab = XmSelectionBoxGetChild( Browser, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( ab );
_dbg("cb07");
   BrowserLW = XmSelectionBoxGetChild( Browser, XmDIALOG_LIST );
   XtVaSetValues( BrowserLW,
      XmNscrollBarDisplayPolicy, XmSTATIC,
      XmNvisibleItemCount, 9,
      NULL );
_dbg("cb08");
   BrowserTW = XmSelectionBoxGetChild( Browser, XmDIALOG_TEXT );
   UPCASE_input( BrowserTW );
_dbg("cb09");
   XtAddCallback( BrowserTW, XmNvalueChangedCallback,
      (XtPointer)browseChangedCB, NULL );

_dbg("cb10");
   for( i = 0; i < NumberOfHelpPages; i++ ) {
      s = XmStringCreateSimple( topics[i] );
      XmListAddItem( BrowserLW, s, i+1 );
      XmStringFree( s );
   }

_dbg("cb11");
   /* --- Unknown Keyword WARNING --- */
   XtSetArg( args[0], XmNdefaultPosition, True );
   XtSetArg( args[1], XmNautoUnmanage, True );
   XtSetArg( args[2], XmNdeleteResponse, XmDO_NOTHING );
   XtSetArg( args[3], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL );
#if 0
_dbgx(vendorShellWidgetClass);
_dbgx(XtClass(Browser));
_dbgx(XtSuperclass(Browser));
_dbgs(XmClassName(Browser));
_dbgs(XmClassName(SystemDialogs));
_dbg("++");
_dbgx(XtIsShell(Browser));
_dbgx(XtIsWMShell(Browser));
_dbgx(XtIsVendorShell(Browser));
_dbgx(XmIsVendorShell(Browser));
#endif
_dbg("cb12");
   BrowseWarn = XmCreateWarningDialog( Browser, "BrowserWarning", args, 4 );
   /* One-Button-Warningbox only... */
   cb = XmMessageBoxGetChild( BrowseWarn, XmDIALOG_CANCEL_BUTTON );
_dbg("cb13");
   XtUnmanageChild( cb );
_dbg("cb14");
   hb = XmMessageBoxGetChild( BrowseWarn, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );

_dbg("cb15");
   XtAddCallback( Browser, XmNokCallback,
      (XtPointer)BrowsedViewCB, NULL );
_dbg("cb16");
   XtAddCallback( Browser, XmNnoMatchCallback,
      (XtPointer)browseWarnCB, NULL );
_dbg("cb17");
}

/****************************************************
*   Show a HELP page                                *
****************************************************/

static void
hideHelpWindowCB( void ) {

   hideBrowserCB();
   XtPopdown( helpWindow );
}

/***************************************************/

static void
pagedHelpCB( Widget w, XtPointer client_data, XtPointer call_data ) {
   char *p = XmTextFieldGetString( Page );
   int nr = atoi( p );
   XtFree( p );

   if( nr < 0 )
      nr = 0;

   if( nr >= NumberOfHelpPages )
      nr = (NumberOfHelpPages-1);

   HELP_show( w,
      /* DO NOT CHANGE THE VALUE OF actual_helpTextNr HERE !!! */
      (XtPointer)nr, call_data );
}

/***************************************************/

static void
previousHelpCB( Widget w, XtPointer client_data, XtPointer call_data ) {
   int nr = (int)client_data;

   if( nr < 0 )
      return;

   HELP_show( w,
      /* DO NOT CHANGE THE VALUE OF actual_helpTextNr HERE !!! */
      (XtPointer) (actual_helpTextNr - 1),
      call_data );
}

/***************************************************/

static void
nextHelpCB( Widget w, XtPointer client_data, XtPointer call_data ) {
   int nr = (int)client_data;

   HELP_show( w,
      /* DO NOT CHANGE THE VALUE OF actual_helpTextNr HERE !!! */
      (XtPointer) (actual_helpTextNr + 1),
      call_data );
}

/****************************************************
*  ShowCallback for HelpWindow                      *
****************************************************/

void
HELP_show( Widget w, XtPointer client_data, XtPointer call_data ) { 
   int nr = (int)client_data;
   FILE *fp;
   char str[32], *text, *p, *browse, *help;
   int c, countFF, i, k;
   int text_start, text_length;
   int success;

   if( nr < 0 || nr >= NumberOfHelpPages ) {
      showWarning( nr );
      return;
   }

   countFF = text_start = text_length = 0;

   /* Determine Start and Length of HelpText */
   if( !(fp = fopen( helptextpath, "r" )) ) {
      perror("HELP_show: fopen1" );
      return;
   }
   if( nr ) {    /* not first page */
      success = 0;
      while( (c=getc(fp)) != EOF ) {
         text_start++;
         if( c == FF ) {
            countFF++;
            if( countFF >= nr ) {
               success = 1;
               break;
            }
         }
      }
      if( !success ) {
         fprintf( stderr,
            "HELP_show: helpfile too small? Page %d not found.\n", nr );
         return;
      }
   }
   while( (c=getc(fp)) != EOF ) {
      if( c == FF )
         break;
      text_length++;      /* if necessary until EOF */
   }
   fclose( fp );

   /* Read the HelpText */
   if( !(fp = fopen( helptextpath, "r" )) ) {
      perror("HELP_show: fopen2");
      return;
   }
   text = (char *)XtMalloc( text_length + 1 );
   if( fseek( fp, text_start, 0 ) ) {
      /* 0 = place relative to the beginning of the file... */
      perror("HELP_show: fseek");
      fclose( fp );
      return;
   }
   if( text_length != (c=fread( text, sizeof(char), text_length, fp )) ) {
      if( !c )
         perror("text_length: fread");
      else
         fprintf( stderr,
            "HELP_show: Hey guys, did anyone truncate the helpfile "
            "about 10 usec ago ?\n" );
      fclose( fp );
      return;
   }
   text[text_length] = 0;
   fclose( fp );

   /* Get the BROWSE-Key, which is assumed to be the text in the first line! */
   p = strchr( text, '\n' );
   if( p ) {
      *p = 0;
      help = p + 1;
      browse = text;
   } else {
      fprintf( stderr, "HELP_show: Bad format of text\n" );
      browse = "";
      help = text;
   }

   /* Display the HelpText */
/** ? Motif-Bug: TextWidget flackert bei Darstellung eines laengeren Textes **/
   XmTextSetString( helpText, help );
   XtFree( text );

   if( nr == 0 ) {   /* MasterHelpPage */
      XtSetSensitive( previous_button, False );
      XtSetSensitive( next_button,     True  );
   } else if( nr == (NumberOfHelpPages-1) ) {
      XtSetSensitive( previous_button, True  );
      XtSetSensitive( next_button,     False );
   } else {
      XtSetSensitive( previous_button, True  );
      XtSetSensitive( next_button,     True  );
   }

   /* Update the PageNumber Field */
   sprintf( str, "%d", nr );
   XmTextFieldSetString( Page, str );

   PopupRaiseAndDeiconify( helpWindow );

   actual_helpTextNr = nr;   /* CHANGE ONLY, IF SUCCESSFUL ! */
}

/****************************************************
*  Create HelpWindow                                *
****************************************************/

Widget
HELP_create( Widget parentW ) {
   Widget shellW, panedW, scrolledW, button_form, quit_button;
   Arg    args[20];
   int    n;
   XmString xstr;

_dbg("hc01");
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
_dbg("hc02");
   shellW = XtAppCreateShell( "HelpWindow", "HereClassIsIgnored",
                             /* instance                class */
      topLevelShellWidgetClass, XtDisplay( parentW ), args, n );

_dbg("hc03");
   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( shellW, WM_DELETE_WINDOW,
      (XtPointer)hideHelpWindowCB, NULL );    /* Pop down ... */

_dbg("hc04");
   /*** PanedWindow ***/
   panedW = XtVaCreateManagedWidget( "panedW",
      xmPanedWindowWidgetClass, shellW,
      XmNsashHeight,      1,
      XmNsashWidth,       1,
      XmNseparatorOn, False,
      NULL );
   
_dbg("hc05");
   scrolledW = XtVaCreateManagedWidget( "helptextSW",
      xmScrolledWindowWidgetClass, panedW,
      XmNscrollingPolicy, XmAUTOMATIC,
      NULL );
   
_dbg("hc06");
   helpText = XtVaCreateManagedWidget( "helptext",
      xmTextWidgetClass, scrolledW,
      XmNeditMode,            XmMULTI_LINE_EDIT,
      XmNeditable,                        False,
      XmNcursorPositionVisible,           False,
      XmNshadowThickness,                     0,
      XmNhighlightThickness,                  0,
      NULL );

_dbg("hc07");
   /*** Button-Form ***/
   button_form = XtVaCreateManagedWidget( "button_form",
      xmFormWidgetClass, panedW,
      XmNpaneMinimum,                     40,
      XmNpaneMaximum,                     40,
      XmNfractionBase,                    64,
      NULL );

_dbg("hc08");
   /*** Quit-Button ***/
   quit_button = XtVaCreateManagedWidget ( "quit_button",
      xmPushButtonWidgetClass, button_form,  
      XmNtopAttachment,        XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM, 
      XmNleftAttachment,   XmATTACH_POSITION,
      XmNleftPosition,                     0,
      XmNrightAttachment,  XmATTACH_POSITION,     
      XmNrightPosition,                    8,
      NULL );
_dbg("hc09");
   XtAddCallback( quit_button, XmNactivateCallback, 
      (XtPointer)hideHelpWindowCB, NULL );      

_dbg("hc10");
   /*** Browse-Button ***/
   Browse = XtVaCreateManagedWidget ( "Browse",
      xmPushButtonWidgetClass, button_form,  
      XmNtopAttachment,        XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM, 
      XmNleftAttachment,   XmATTACH_POSITION,
      XmNleftPosition,                    22,
      XmNrightAttachment,  XmATTACH_POSITION,     
      XmNrightPosition,                   30,
      NULL );
_dbg("hc11");
   XtAddCallback( Browse, XmNactivateCallback, 
      (XtPointer)showBrowserCB, NULL );      

_dbg("hc12");
   /*** Previous-Button ***/
   xstr = XmStringCreateSimple( "<<" );
   previous_button = XtVaCreateManagedWidget( "previous_button",
      xmPushButtonWidgetClass, button_form,
      XmNlabelString,                   xstr,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM, 
      XmNleftAttachment,   XmATTACH_POSITION,
      XmNleftPosition,                    43,
      XmNrightAttachment,  XmATTACH_POSITION,     
      XmNrightPosition,                   51,
      NULL );
   XmStringFree( xstr );
_dbg("hc13");
   XtAddCallback( previous_button, XmNactivateCallback, 
      (XtPointer)previousHelpCB, NULL );      

_dbg("hc14");
   /*** Page-Button ***/
   Page = XtVaCreateManagedWidget( "Page",
      xmTextFieldWidgetClass, button_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM, 
      XmNleftAttachment,   XmATTACH_POSITION,
      XmNleftPosition,                    51,
      XmNrightAttachment,  XmATTACH_POSITION,     
      XmNrightPosition,                   56,
      NULL );
_dbg("hc15");
   XtAddCallback( Page, XmNactivateCallback, 
      (XtPointer)pagedHelpCB, NULL );      

_dbg("hc16");
   /*** Next-Button ***/
   xstr = XmStringCreateSimple( ">>" );
   next_button = XtVaCreateManagedWidget( "next_button",
      xmPushButtonWidgetClass, button_form,
      XmNlabelString,                   xstr,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM, 
      XmNleftAttachment,   XmATTACH_POSITION,
      XmNleftPosition,                    56,
      XmNrightAttachment,  XmATTACH_POSITION,     
      XmNrightPosition,                   64,
      NULL );
   XmStringFree( xstr );
_dbg("hc17");
   XtAddCallback( next_button, XmNactivateCallback, 
      (XtPointer)nextHelpCB, NULL );      

   helpWindow = shellW;   /* for internal purposes */

_dbg("hc18");
   /*** BrowseDialog ***/
   createBrowser();

_dbg("hc19");
   /*** HelpWarningDialog ***/
   createWarning();

_dbg("hc20");
   return helpWindow;
}

/***************************************************/

void
HELP_setPath( char *p ) {
   char str[256];
   int c, is_topic, i, mhp;
   FILE *fp;

   /* to disable reading help on editors at RUNTIME */
   mhp = HELP_maxPages();

   /* dynamically allocate vectors */
   topics = (char**)XtMalloc( sizeof(char*) * mhp );
   topics_no = (int*)XtMalloc( sizeof(int) * mhp );
   for( i = 0; i < mhp; i++ )
      topics_no[i] = i;

   NumberOfHelpPages = 0;
   helptextpath = p;
   if( !p ) {         /* HelpText not found! */
      return;
   }
   /* Determine the Number of HelpPages in the HelpText */
   if( !(fp = fopen( helptextpath, "r" )) ) {
      perror("HELP_show: fopen1" );
      return;
   }
   NumberOfHelpPages = 1;
   is_topic = True;
   i = 0;
   while( (c=getc(fp)) != EOF ) {
      if( c == FF ) {
         NumberOfHelpPages++;
         if( NumberOfHelpPages > mhp ) {
            NumberOfHelpPages = mhp;
            /*fprintf( stderr, "too much helptexts: only partial info available!\n" );*/
            break;
         }
         is_topic = True;
         continue;
      }
      if( c == '\n' ) {
         is_topic = False;
         i = 0;
         topics[NumberOfHelpPages-1] = XtMalloc( strlen(str) + 1 );
         strcpy( topics[NumberOfHelpPages-1], str );
         continue;
      }
      if( is_topic ) {
         str[i++] = c;
         str[i] = 0;
      }
   }
   fclose( fp );

   if( i ) {
      fprintf( stderr, "while reading help-texts: corrupted file?\n" );
   }
   qwsort( topics, 0, NumberOfHelpPages-1 );
}

/*** EOF *******************************************/

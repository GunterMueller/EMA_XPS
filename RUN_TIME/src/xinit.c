#if 0
#define _dbg(a)    puts("xinit: " a)
#define _dbgs(a) printf("xinit: %s\n",a)
#define _dbgx(a) printf("xinit: 0x%X\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#define _dbgx(a)
#endif

/****************************************************
*   xinit.c       Hans Groschwitz        06.10.94   *
*                                                   *
*   Creates a welcoming PopupBox and the MainMenu   *
#   -DRUNTIME for rema-xps version                  *
*                                                   *
****************************************************/

#include <stdio.h>

#include <Xm/Form.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>      /* MenuBar */
#include <Xm/PushBG.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/SeparatoG.h>
#include <Xm/Protocols.h>      /* WM_DELETE_WINDOW */
#include <Xm/MwmUtil.h>        /* MWM_DECOR_BORDER */
#include <Xm/FileSB.h>
#include <Xm/Scale.h>
#include <Xm/ToggleBG.h>
#include <Xm/SelectioB.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>

#include "timings.h"
#include "main.h"
#include "xinit.h"
#include "debugger.h"
#include "uid.h"
#include "sysd.h"
#include "expd.h"
#include "help.h"
#include "explain.h"

#ifndef RUNTIME
#include "rule.h"
#include "trace.h"
#include "prolog.h"
#include "constraint.h"
#include "csnet.h"
#include "restriction.h"
#include "frame.h" 
#include "instance.h"
#include "behavior.h"
#include "task.h"
#include "misc.h"
#endif

/*** Typedefs **************************************/

typedef struct _knownkbs {
   Widget w;
   int  L_modified;
   char *name;
   char *path;
   char *filter;
} KNOWNKBS;

typedef struct _c_editors {
   int   flags;
   char *kbname;
} C_EDITORS;

/*** Globals ***************************************/

#define LOGO_XBM          "emaXpsLogo.bm"
#define SAVE_XBM          "emaXpsSave.bm"
#define EMPTY_XBM         "emaXpsEmpty.bm"

#ifdef RUNTIME
/* to disable viewing help on Editors and Syntax in runtime-version */
#define MAX_HELP_PAGES 9
#else
#define MAX_HELP_PAGES 1000
#endif

#ifdef RUNTIME         /* # of divisions for Welcome's Scale */
#define SCALE_DIVS 8
#else
#define SCALE_DIVS 19
#endif

char *EMAXPS_source_tree = ".";
#ifdef RUNTIME
char *EMAXPS_make_file_name = "make-runtime.cl";
char *EMAXPS_lisp_filename = "remalisp";
#else
char *EMAXPS_make_file_name = "make-develop.cl";
char *EMAXPS_lisp_filename = "emalisp";
#endif
static Widget    MainQuitInfo, MainCloseInfo;
static Widget    MainMenu, welcomeBoxW;
static KNOWNKBS  KnownKBs[10];
static int       kbs_all, kb_current;
static Widget    FileNewW, FileLoadW, FileStatisticsW, FileSaveW;
static Widget    FileSaveAsW, FileCloseW, SessionRunW;
static Widget    fileLoadSBW, fileSaveAsSBW;
static Widget    FileSavePrefsW, FileSavePrefsRW, FileSavePrefsCW;
static Widget    MMframeF, MMframeI;
static Widget    MMframeB, MMrule;
static Widget    MMprolog, MMconsatC, MMconsatN;
static Widget    MMconsatR, MMtask, MMmisc, Scale;
static Widget    SessionTraceW, ExplainW;
static C_EDITORS C_editors[10];
static int       save_preference_compiled = False;
static int       FileSavePrefsCisSet = False;

/*** Function Declarations *************************/

#ifndef RUNTIME
static void createSavePrefsDialog( Widget );
static void viewSavePrefsCB( void );
static void viewSavePrefsOkCB( void );
static void viewSavePrefsCancelCB( void );
static void EDITORS_sensitive( int );
static void EDITORS_informOnCloseOfKb( char* );
static void MainCloseOkCB( void );
static void MainQuitCancelCB( void );
static void focusMainQuitCB( void );
static void MainQuitCB( void );
static void FileNewCB( void );
#endif /* RUNTIME */
static void MainQuitOkCB( void );
static void MainHelpDismissCB( void );
static void MainHelpCB( void );
static void FileLoadCancelCB( void );
static void FileLoadOkCB( Widget, XtPointer,
               XmFileSelectionBoxCallbackStruct* );
static int  KBS_unsavedp( void );
static int  KBcurrent_unsavedp( void );
static void KBS_get_current_fname_filter( char**, char** );
static void FileSetKbNames( void );
static void createFilePDM( Widget );
static void createLoadFSelPDM( void );
static void createSaveFSelPDM( void );
static void createEditorButtons( Widget );
static void createSessionPDM( Widget );
static void C_updateData( void );
static int  C_unsavedp( char* );
static void IncrementScale( void );

/****************************************************
*   This is to make help.c independent from the     *
*   #ifdef RUNTIME switch!                          *
****************************************************/

int
HELP_maxPages( void ) {
   return MAX_HELP_PAGES;
}

/****************************************************
*   create Pixmaps for the already managed labels   *
*   to display changes in the editors and the       *
*   HelpBrowser                                     *
****************************************************/

void
PIXMAP_createChanged( Widget w, Pixmap *p0, Pixmap *p1 ) {
   int bg, fg;

   XtVaGetValues( w,
      XmNbackground, &bg,
      XmNforeground, &fg,
      NULL );
   *p0 = createPixmapFromFile( w, EMPTY_XBM, fg, bg );
   *p1 = createPixmapFromFile( w, SAVE_XBM, fg, bg );
   XtVaSetValues( w,
      XmNlabelPixmap, *p0,
      XmNlabelInsensitivePixmap, *p0,
      NULL );
}

/****************************************************
*   Enable Editors to enter LISP-Symbols            *
*   upcase only                                     *
****************************************************/

static void
insert_upcase( Widget w, XEvent *event, String* argv, int* argc ) {

   if( *argc == 1 )
      XmTextFieldInsert( w, XmTextFieldGetInsertionPosition( w ), argv[0] );
   else
      fprintf( stderr, "insert_upcase: # of args!\n" );
}

static XtActionsRec upcase_actions[] = {
   { "insert-upcase",       (XtPointer)insert_upcase }
};

static String upcase_transl_table = "\
<Key>a: insert-upcase(A) \n\
<Key>b: insert-upcase(B) \n\
<Key>c: insert-upcase(C) \n\
<Key>d: insert-upcase(D) \n\
<Key>e: insert-upcase(E) \n\
<Key>f: insert-upcase(F) \n\
<Key>g: insert-upcase(G) \n\
<Key>h: insert-upcase(H) \n\
<Key>i: insert-upcase(I) \n\
<Key>j: insert-upcase(J) \n\
<Key>k: insert-upcase(K) \n\
<Key>l: insert-upcase(L) \n\
<Key>m: insert-upcase(M) \n\
<Key>n: insert-upcase(N) \n\
<Key>o: insert-upcase(O) \n\
<Key>p: insert-upcase(P) \n\
<Key>q: insert-upcase(Q) \n\
<Key>r: insert-upcase(R) \n\
<Key>s: insert-upcase(S) \n\
<Key>t: insert-upcase(T) \n\
<Key>u: insert-upcase(U) \n\
<Key>v: insert-upcase(V) \n\
<Key>w: insert-upcase(W) \n\
<Key>x: insert-upcase(X) \n\
<Key>y: insert-upcase(Y) \n\
<Key>z: insert-upcase(Z)";

void
UPCASE_input( Widget w ) {
   /* assumes w to be a TextField ... */

   XtOverrideTranslations( w,
      XtParseTranslationTable( upcase_transl_table ));
}

#ifndef RUNTIME
/****************************************************
*   Set Save Time Preferences (:COMPILED)           *
****************************************************/

static void
FileSavePrefsCisToggledCB( Widget w, int which,
      XmToggleButtonCallbackStruct *state ) {

   FileSavePrefsCisSet = state->set ? True : False;
}

static void
createSavePrefsDialog( Widget parent ) {
   int n;
   Arg args[4];
   Widget w, rbx, rcw, frm;

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNautoUnmanage, False ); n++;
   FileSavePrefsW = XmCreatePromptDialog( parent, "save_prefs", args, n );

   XtAddCallback( FileSavePrefsW, XmNokCallback,
      (XtPointer)viewSavePrefsOkCB, NULL );
   XtAddCallback( FileSavePrefsW, XmNcancelCallback,
      (XtPointer)viewSavePrefsCancelCB, NULL );

   /* remove not used widgets */
   w = XmSelectionBoxGetChild( FileSavePrefsW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( FileSavePrefsW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( FileSavePrefsW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

   /* one widget may be placed above the dialog (should be a container) */
   n = 0;
   frm = XmCreateFrame( FileSavePrefsW, "frame", args, n );
   XtManageChild( frm );

   /* fine placement using a row column */
   n = 0;
   rcw = XmCreateRowColumn( frm, "row_column", args, n );
   XtManageChild( rcw );

   /* add a label */
   n = 0;
   w = XmCreateLabelGadget( rcw, "prefs_fformat", args, n );
   XtManageChild( w );

   /* add a radio box below the label */
   n = 0;
   rbx = XmCreateSimpleRadioBox( rcw, "radio", args, n );
   XtManageChild( rbx );
   n = 0;
   w = XmCreateToggleButtonGadget( rbx, "readable", args, n );
   XtManageChild( w );
   FileSavePrefsRW = w;
   n = 0;
   w = XmCreateToggleButtonGadget( rbx, "compiled", args, n );
   XtManageChild( w );
   XtAddCallback( w, XmNvalueChangedCallback,
      (XtPointer)FileSavePrefsCisToggledCB, NULL );
   FileSavePrefsCW = w;
}

static void
viewSavePrefsCB( void ) {
   int n;
   Arg args[4];
   int r, c;

   /* preset the radio-toggles from the global variable */
   if( save_preference_compiled ) {
      r = False;
      c = True;
   } else {
      r = True;
      c = False;
   }
   
   n = 0;
   XtSetArg( args[n], XmNset, r ); n++;
   XtSetValues( FileSavePrefsRW, args, n );
   n = 0;
   XtSetArg( args[n], XmNset, c ); n++;
   XtSetValues( FileSavePrefsCW, args, n );

   XtManageChild( FileSavePrefsW );
   XtPopup( XtParent(FileSavePrefsW), XtGrabNone );
}

static void
viewSavePrefsOkCB( void ) {

   /* apply the choice */
   save_preference_compiled = FileSavePrefsCisSet;
   XtPopdown( XtParent(FileSavePrefsW) );
   XtUnmanageChild( FileSavePrefsW );
}

static void
viewSavePrefsCancelCB( void ) {

   /* discard the choice */
   XtPopdown( XtParent(FileSavePrefsW) );
   XtUnmanageChild( FileSavePrefsW );
}

/****************************************************
*   Inform Editors on intermediate Close of a KB    *
*                                                   *
*   EXCEPTION: Prolog-Editor uses a ApplMod Popup   *
*   for associating clauses with KBs. The clauses   *
*   loaded into the LISP world at a time are inde-  *
*   pendent from the number of KBs available !!!    *
****************************************************/

static void
EDITORS_informOnCloseOfKb( char *kbname ) {

   FRAME_kbClosed( kbname );
   INSTANCE_kbClosed( kbname );
   BEHAVIOR_kbClosed( kbname );
   RULE_kbClosed( kbname );
   PROLOG_kbClosed( kbname );
   CONSTRAINT_kbClosed( kbname );
   CSNET_kbClosed( kbname );
   RESTRICTION_kbClosed( kbname );
   TASK_kbClosed( kbname );
   MISC_kbClosed( kbname );
}

/****************************************************
*   Allow editing, if at least one KB is known      *
*                                                   *
*   EXCEPTION: Prolog-Editor is always available!   *
****************************************************/

static void
EDITORS_sensitive( int value ) {

   XtSetSensitive( MMframeF,  value );
   XtSetSensitive( MMframeI,  value );
   XtSetSensitive( MMframeB,  value );
   XtSetSensitive( MMrule,    value );
   XtSetSensitive( MMprolog,  value );
   XtSetSensitive( MMconsatC, value );
   XtSetSensitive( MMconsatN, value );
   XtSetSensitive( MMconsatR, value );
   XtSetSensitive( MMtask,    value );
   XtSetSensitive( MMmisc,    value );

   /* hide all editors and the tracer, when the last KB is closed! */
   if( !value ) {
      FRAME_hide();
      INSTANCE_hide();
      BEHAVIOR_hide();
      RULE_hide();
      PROLOG_hide();
      CONSTRAINT_hide();
      CSNET_hide();
      RESTRICTION_hide();
      TASK_hide();
      MISC_hide();
      Trace_PAD_hide();
   }
}

/****************************************************
*   Close modified KB Callbacks                     *
****************************************************/

static void
MainCloseCancelCB( void ) {
   XtPopdown( XtParent(MainCloseInfo) );
   XtUnmanageChild( MainCloseInfo );
}

/***************************************************/

static void
focusMainCloseCB( void ) {
   Widget cb;

   /* move input fokus to CANCEL (when managed)! */
   cb = XmMessageBoxGetChild( MainCloseInfo, XmDIALOG_CANCEL_BUTTON );
   XmProcessTraversal( cb, XmTRAVERSE_CURRENT );
}

/***************************************************/

static void
MainCloseCB( void ) {
   if( XtIsManaged( MainCloseInfo ) )
      return;
   XtManageChild( MainCloseInfo );
   XtPopup( XtParent(MainCloseInfo), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusMainCloseCB, NULL );
}

/***************************************************/

static void
MainCloseOkCB( void ) {
   XtPopdown( XtParent(MainCloseInfo) );
   XtUnmanageChild( MainCloseInfo );

   C_set_modified( KnownKBs[kb_current].name,
      MOD_ALL_EDITORS, False );
   EDITORS_informOnCloseOfKb( KnownKBs[kb_current].name );
   LISP_sndCmd( "(emaxps-close-kb)" );
}

/****************************************************
*   Terminate EMA-XPS Callbacks                     *
****************************************************/

static void
MainQuitCancelCB( void ) {
   XtPopdown( XtParent(MainQuitInfo) );
   XtUnmanageChild( MainQuitInfo );
}

/***************************************************/

static void
focusMainQuitCB( void ) {
   Widget cb;

   /* move input fokus to CANCEL (when managed)! */
   cb = XmMessageBoxGetChild( MainQuitInfo, XmDIALOG_CANCEL_BUTTON );
   XmProcessTraversal( cb, XmTRAVERSE_CURRENT );
}

/***************************************************/

static void
MainQuitCB( void ) {
   if( !KBS_unsavedp() ) {
      LISP_kill();
      return;
   }
   if( XtIsManaged( MainQuitInfo ) )
      return;
   XtManageChild( MainQuitInfo );
   XtPopup( XtParent(MainQuitInfo), XtGrabNone );

   /* sometimes it works, sometimes it won't ... */
   AddTimeOut( TIME_FOCUS, (XtPointer)focusMainQuitCB, NULL );
}
#endif /* RUNTIME */

/***************************************************/

static void
MainQuitOkCB( void ) {
#ifndef RUNTIME
   XtPopdown( XtParent(MainQuitInfo) );
   XtUnmanageChild( MainQuitInfo );
#endif /* RUNTIME */
   LISP_kill();
}

/****************************************************
*   creates Callbacks for MainMenu's FILE Submenu   *
****************************************************/

static void
FileStatisticsCB( void ) {
   LISP_sndCmd( "(emaxps-kb-statistics)" );
}

/***************************************************/

#ifndef RUNTIME
static void
FileNewCB( void ) {
   LISP_sndCmd( "(emaxps-kb-create-new)" );
}
#endif /* RUNTIME */

/***************************************************/

static void
FileLoadCB( void ) {
   XtManageChild( fileLoadSBW );
   XtPopup( XtParent(fileLoadSBW), XtGrabNone );
}

/***************************************************/

static void
FileLoadCancelCB( void ) {
   XtPopdown( XtParent(fileLoadSBW) );
   XtUnmanageChild( fileLoadSBW );
}

/***************************************************/

static void
FileLoadOkCB( Widget w, XtPointer cd,
      XmFileSelectionBoxCallbackStruct *cbs ) {
   char str[2048+64], *filename, *filtermask;
   FILE *fp;

   FileLoadCancelCB();
   if( !XmStringGetLtoR( cbs->value,
           XmSTRING_DEFAULT_CHARSET,
           (XtPointer)&filename ) ) {
      return;
   }
   if( !XmStringGetLtoR( cbs->mask,
           XmSTRING_DEFAULT_CHARSET,
           (XtPointer)&filtermask ) ) {
      return;
   }

#ifdef THIS_IS_LISPS_JOB
   if( !(fp=fopen(filename,"r")) ) {
      /* UNDER CONSTRUCTION! */
      perror( "FileLoadOkCB" );
      return;
   }
   fclose(fp);
#endif

   sprintf( str, "(emaxps-load-kb \"%s\"\n   \"%s\")",
      filename, filtermask );
   LISP_sndCmd( str );
}

/***************************************************/

static char*
save_preference( void ) {

   return save_preference_compiled ? ":COMPILED" : "NIL";
}
   
/***************************************************/

static void
FileSaveCB( void ) {
   char str[2048+64];
#ifdef THIS_IS_LISPS_JOB
   FILE *fp;
   char *filename, *filter;

   /* get filename of current KB */
   KBS_get_current_fname_filter( &filename, &filter );

   if( !(fp=fopen(filename,"a")) ) {
      /* UNDER CONSTRUCTION! */
      perror( "FileSaveCB" );
      return;
   }
   fclose(fp);
#endif

   sprintf( str, "(emaxps-save-kb %s NIL NIL)",
      save_preference() );
   LISP_sndCmd( str );
}

/***************************************************/

static void
FileSaveAsCB( void ) {
   char *fname, *filter;

   /*** Open Save-FSB with Filename/Filter preset from current KB ***/
   /* read from LISP data */
   KBS_get_current_fname_filter( &fname, &filter );

   /*** set filename,filter,update XmLists ***/
   /* init. dir-search... */
   KBS_get_current_fname_filter( &fname, &filter );
   XmFileSelectionDoSearch( fileSaveAsSBW,
      XmStringCreateSimple( filter ) );

   /* init value field... */
   if( fname )
      XtVaSetValues( fileSaveAsSBW,
         XmNdirSpec, XmStringCreateSimple( fname ), NULL );

   XtManageChild( fileSaveAsSBW );
   XtPopup( XtParent(fileSaveAsSBW), XtGrabNone );
}

/***************************************************/

static void
FileSaveAsCancelCB( void ) {
   XtPopdown( XtParent(fileSaveAsSBW) );
   XtUnmanageChild( fileSaveAsSBW );
}

/***************************************************/

static void
FileSaveAsOkCB( Widget w, XtPointer cd,
      XmFileSelectionBoxCallbackStruct *cbs ) {
   char str[2048+64], *filename, *filtermask;
   FILE *fp;

   FileSaveAsCancelCB();
   if( !XmStringGetLtoR( cbs->value,
           XmSTRING_DEFAULT_CHARSET,
           (XtPointer)&filename ) ) {
      return;
   }
   if( !XmStringGetLtoR( cbs->mask,
           XmSTRING_DEFAULT_CHARSET,
           (XtPointer)&filtermask ) ) {
      return;
   }
#ifdef THIS_IS_LISPS_JOB
   if( (fp=fopen(filename,"r")) ) {
      /* UNDER CONSTRUCTION! */
      fclose(fp);
      fprintf( stderr,
         "Warning: FileSaveAsOkCB: file %s already ex!\n",
         filename );
      /* return; */
   }

   if( !(fp=fopen(filename,"a")) ) {
      /* UNDER CONSTRUCTION! */
      perror( "FileSaveAsOkCB" );
      return;
   }
   fclose(fp);
#endif

   sprintf( str, "(emaxps-save-kb %s \"%s\"\n   \"%s\")",
      save_preference(), filename, filtermask );
   LISP_sndCmd( str );
}

/***************************************************/

static void
FileCloseCB( void ) {          /* Close an open KB */
#ifndef RUNTIME
   if( KBcurrent_unsavedp() ) {
      MainCloseCB();
      return;
   }
   EDITORS_informOnCloseOfKb( KnownKBs[kb_current].name );
#endif /* RUNTIME */
   /* see MainCloseOkCB, too! */
   LISP_sndCmd( "(emaxps-close-kb)" );
}

/****************************************************
*   Management of multiple KBs in the LISP-world    *
*                                                   *
*   receives a message from LISP                    *
*   whenever state changes !!!                      *
****************************************************/

void
KBS_getState( char *str ) {
   int i, c, n, len, all, current;
   char buf[1024], *p, *ptmp;

   for( i = 1; i <= 9; i++ ) {
      if( KnownKBs[i].name ) {
         XtFree( KnownKBs[i].name );
         KnownKBs[i].name = NULL;
      }
      if( KnownKBs[i].path ) {
         XtFree( KnownKBs[i].path );
         KnownKBs[i].path = NULL;
      }
      if( KnownKBs[i].filter ) {
         XtFree( KnownKBs[i].filter );
         KnownKBs[i].filter = NULL;
      }
   }
   p = str;

   *buf = 0;
   i = 0;
   while( (c = *p++) > ' ' )   /* EndOfString(\0) or ETX(^C) */
      buf[i++] = c;
   buf[i] = 0;
   kb_current = atoi( buf );

   *buf = 0;
   i = 0;
   while( (c = *p++) > ' ' )
      buf[i++] = c;
   buf[i] = 0;
   kbs_all = atoi( buf );

   for( n = 1; n <= kbs_all; n++ ) {  /* no. of entry to KnownKBs */
      *buf = 0;
      i = 0;
      while( (c = *p++) > ' ' )
         buf[i++] = c;
      buf[i] = 0;
      KnownKBs[n].name = XtMalloc( strlen(buf)+1 );
      strcpy( KnownKBs[n].name, buf );

      *buf = 0;
      i = 0;
      while( (c = *p++) > ' ' )
         buf[i++] = c;
      buf[i] = 0;
      KnownKBs[n].path = XtMalloc( strlen(buf)+1 );
      strcpy( KnownKBs[n].path, buf );

      *buf = 0;
      i = 0;
      while( (c = *p++) > ' ' )
         buf[i++] = c;
      buf[i] = 0;
      KnownKBs[n].filter = XtMalloc( strlen(buf)+1 );
      strcpy( KnownKBs[n].filter, buf );

      *buf = 0;
      i = 0;
      while( (c = *p++) > ' ' )
         buf[i++] = c;
      buf[i] = 0;
      if( strlen(buf) == 1 )  /* T (in spite of NIL) */
         KnownKBs[n].L_modified = 1;
      else
         KnownKBs[n].L_modified = 0;
   }
   C_updateData();
   FileSetKbNames();    /* uses C_unsavedp ! */

   /* do not load, if 9 KBs are known already */
   all = kbs_all >= 9 ? False : True;
   /*
    * PopupDialogs are FullApplModal, and therfore need no special management here
    * Tools (Explain and Trace) must be popped down, when the last KB is closed
    * Editors the same...
    * The SessionScreen is hided separately by (emaxps-close-kb) --> main.cl !!!
    */
   if( !kbs_all )   /* available in RuntimeVersion, too */
      Explain_PAD_hide();
   XtSetSensitive( FileLoadW, all );
#ifndef RUNTIME
   XtSetSensitive( FileNewW, all );

   /* only allow editing and tracing, if at least one KB exists */
   EDITORS_sensitive( kbs_all > 0 );
#endif /* RUNTIME */

   /* only allow actions, if a KB is current */
   current = kb_current ? True : False;
   XtSetSensitive( FileStatisticsW, current );
#ifndef RUNTIME
   XtSetSensitive( FileSaveW,       current );
   XtSetSensitive( FileSaveAsW,     current );
   XtSetSensitive( SessionTraceW,   current );
#endif /* RUNTIME */
   XtSetSensitive( FileCloseW,      current );
   XtSetSensitive( SessionRunW,     current );
   XtSetSensitive( ExplainW,        current );

#ifndef RUNTIME
   /* inform the tracer on possible changes */
   TRACE_informOnKBSstate( kbs_all );
#endif
}

/***************************************************/

static void
FileKbSelectCB( Widget w, int no ) {
   char str[1024];

   sprintf( str, "(emaxps-select-kb '%s)", KnownKBs[no].name );
   LISP_sndCmd( str );
}

/****************************************************
*   creates Callback for MainMenu's Session Submenu *
*   Used by MainMenu and SessionScreen              *
****************************************************/

static void
SessionRunCB( void ) {
   LISP_sndCmd( "(emaxps-run-session)" );
}

/***************************************************/

static int
KBS_unsavedp( void ) {
   /* Used by MainQuitCB() only! */
   int i;

   for( i = 1; i <= 9; i++ ) {
      if( !KnownKBs[i].name )
         continue;
      if( C_unsavedp( KnownKBs[i].name ) | KnownKBs[i].L_modified )
         return True;
   }
   return False;
}

/***************************************************/

static int
KBcurrent_unsavedp( void ) {
   /* used by FileCloseCB() only! */

   if( C_unsavedp( KnownKBs[kb_current].name )
             | KnownKBs[kb_current].L_modified ) {
      return True;
   }
   return False;
}

/***************************************************/

static void
KBS_get_current_fname_filter( char**fname, char**filter ) {
   /* Used by SAVE, SAVE-AS */
   *fname  = KnownKBs[kb_current].path;
   *filter = KnownKBs[kb_current].filter;
}

/****************************************************
*   creates FILE Submenu Texts of Appl's MainMenu   *
****************************************************/

static void
FileSetKbNames( void ) {
   int i, cu, lu, unsaved;
   XmString xmstr;
   char name[32], tmp[16];
   char *show[4] = { "  ", "!! ", " #", "!!#" };
                   /* no    C       L    C L */

   for( i = 1; i <= 9; i++ ) {
      if( !KnownKBs[i].name ) {          /* this KB# is not known! */
         XtUnmanageChild( KnownKBs[i].w );
         continue;
      }
      /* if there are unsaved changes, mark them */
      /* 0 = NO changes */
      /* 1 = Unsaved changes within an Editor (C-side is affected only) "!!" */
      /* 2 = Unsaved changes within the LISP-World                       "#" */
      /* 3 = 2 | 1 */
      cu = C_unsavedp( KnownKBs[i].name ) ? 1 : 0;
      lu = KnownKBs[i].L_modified ? 2 : 0;
      unsaved = cu | lu;

      strncpy( tmp, KnownKBs[i].name, 14 );
      tmp[14] = '$';
      tmp[15] = 0;                       /* cuts off too long KB names */
      sprintf( name, "%d %s %s  %s",
         i, i==kb_current?">":"  ",      /* This assumes `variable' font */
         tmp, show[unsaved]
      );
      xmstr = XmStringCreateLtoR( name, XmSTRING_DEFAULT_CHARSET );
      XtVaSetValues( KnownKBs[i].w, XmNlabelString, xmstr, NULL );
      XmStringFree( xmstr );
      XtManageChild( KnownKBs[i].w );
   }
}

/****************************************************
*   creates FILE Submenu of the Appl's MainMenu     *
****************************************************/

static void
createFilePDM( Widget parent ) {
   Widget s, w;
   Arg args[4];
   int i, mn;
   char name[16];

   XtSetArg( args[0], XmNsensitive, False );
   w = XmCreatePushButton( parent, "FileStatistics", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
     (XtPointer)FileStatisticsCB, NULL );
   FileStatisticsW = w;

   s = XmCreateSeparatorGadget( parent, "separator", NULL, 0 );
   XtManageChild( s );

   XtSetArg( args[0], XmNsensitive, True );
#ifndef RUNTIME
   w = XmCreatePushButton( parent, "FileNew", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
     (XtPointer)FileNewCB, NULL );
   FileNewW = w;
#endif /* RUNTIME */

   w = XmCreatePushButton( parent, "FileLoad", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
     (XtPointer)FileLoadCB, NULL );
   FileLoadW = w;

   createLoadFSelPDM();

   XtSetArg( args[0], XmNsensitive, False );
#ifndef RUNTIME
   w = XmCreatePushButton( parent, "FileSave", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)FileSaveCB, NULL );
   FileSaveW = w;

   w = XmCreatePushButton( parent, "FileSaveAs", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)FileSaveAsCB, NULL );
   FileSaveAsW = w;

   createSaveFSelPDM();
#endif /* RUNTIME */

   w = XmCreatePushButton( parent, "FileClose", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)FileCloseCB, NULL );
   FileCloseW = w;

   s = XmCreateSeparatorGadget( parent, "separator", NULL, 0 );
   XtManageChild( s );

   XtSetArg( args[0], XmNsensitive, True );
   for( i = 1; i <= 9; i++ ) {
      sprintf( name, "KB%d", i );
      mn = i + '0';
      XtSetArg( args[1], XmNmnemonic, mn );
      KnownKBs[i].w = w = XmCreatePushButton( parent, name, args, 2 );
      KnownKBs[i].name = KnownKBs[i].path = KnownKBs[i].filter = NULL;
      KnownKBs[i].L_modified = False;
      XtAddCallback( w, XmNactivateCallback,
         (XtPointer)FileKbSelectCB, (XtPointer)i );
   }
   FileSetKbNames();

   s = XmCreateSeparatorGadget( parent, "separator", NULL, 0 );
   XtManageChild( s );

   w = XmCreatePushButton( parent, "FileEnd", NULL, 0 );
   XtManageChild( w );
#ifndef RUNTIME
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)MainQuitCB, NULL );
#else /* RUNTIME */
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)MainQuitOkCB, NULL );
#endif /* RUNTIME */
}

/***************************************************/

static void
createLoadFSelPDM( void ) {
   Widget h;
   int n;
   Arg args[4];
   char *fname, *filter;

   n = 0;                                                    
   XtSetArg( args[n], XmNtransient,        True ); n++;
   XtSetArg( args[n], XmNdialogStyle,
                XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   fileLoadSBW = XmCreateFileSelectionDialog( SystemDialogs,
                    "FileLoadSB", args, n );

   h = XmFileSelectionBoxGetChild( fileLoadSBW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( h );

   XtVaSetValues( XtParent(fileLoadSBW),
      XmNdeleteResponse, XmDO_NOTHING, NULL );

   /* init. dir-search... */
   ARGV_kb_settings( &fname, &filter );
   XmFileSelectionDoSearch( fileLoadSBW,
      XmStringCreateSimple( filter ) );

   /* init value field, if autoloading... */
   if( fname )
      XtVaSetValues( fileLoadSBW,
         XmNdirSpec, XmStringCreateSimple( fname ), NULL );

   XtAddCallback( fileLoadSBW, XmNokCallback,
      (XtPointer)FileLoadOkCB, NULL );
   XtAddCallback( fileLoadSBW, XmNcancelCallback,
      (XtPointer)FileLoadCancelCB, NULL );
}

#ifndef RUNTIME
/***************************************************/

static void
createSaveFSelPDM( void ) {
   Widget w;
   int n;
   Arg args[4];

   n = 0;                                                    
   XtSetArg( args[n], XmNtransient,        True ); n++;
   XtSetArg( args[n], XmNdialogStyle,
                XmDIALOG_FULL_APPLICATION_MODAL ); n++; 
   fileSaveAsSBW = XmCreateFileSelectionDialog( SystemDialogs,
                      "FileSaveAsSB", args, n );

   XtVaSetValues( XtParent(fileSaveAsSBW),
      XmNdeleteResponse, XmDO_NOTHING, NULL );

   XtAddCallback( fileSaveAsSBW, XmNokCallback,
      (XtPointer)FileSaveAsOkCB, NULL );
   XtAddCallback( fileSaveAsSBW, XmNcancelCallback,
      (XtPointer)FileSaveAsCancelCB, NULL );

   /* create the Preferences Dialog */
   createSavePrefsDialog( fileSaveAsSBW );

   /* Use the Help button to reach the Preferences Dialog */
   w = XmSelectionBoxGetChild( fileSaveAsSBW, XmDIALOG_HELP_BUTTON );
   /* avoid use of the HELP Callback, to grant work of HELP-facility! */
   /* otherwise pressing [F1] would activate the preferences POPUP! */
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)viewSavePrefsCB, NULL );
}

/****************************************************
*   creates the MenuEntries of the EDIT Submenu     *
****************************************************/

static void
createEditorButtons( Widget parent ) {
   Widget w;
   Arg args[4];

   XtSetArg( args[0], XmNsensitive, False );

   /*** creates "Frame" MenuEntry of EDIT Submenu ***/
   w = XtCreateManagedWidget( "Frame",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)FRAME_show, NULL );
   MMframeF = w;

   /*** creates "Instances" MenuEntry of EDIT Submenu ***/
   w = XtCreateManagedWidget( "Instance",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)INSTANCE_show, NULL );
   MMframeI = w;

   /*** creates "Behavior" MenuEntry of EDIT Submenu ***/
   w = XtCreateManagedWidget( "Behavior",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)BEHAVIOR_show, NULL );
   MMframeB = w;

   XtCreateManagedWidget( "separator",
      xmSeparatorGadgetClass,
      parent, NULL, 0 );

   /*** creates "Rule" MenuEntry of EDIT Submenu ***/
   w = XmCreatePushButton( parent, "Rule", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)RULE_show, NULL );
   MMrule = w;

   /*** creates "Prolog" MenuEntry of EDIT Submenu ***/
   w = XtCreateManagedWidget( "Prolog",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtSetArg( args[0], XmNsensitive, False );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)PROLOG_show, NULL );
   MMprolog = w;

   /*** creates "Consat" MenuEntry of EDIT Submenu ***/
   XtCreateManagedWidget( "separator",
      xmSeparatorGadgetClass,
      parent, NULL, 0 );

   w = XtCreateManagedWidget( "Constraint",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)CONSTRAINT_show, NULL );
   MMconsatC = w;

   XtSetArg( args[0], XmNsensitive, False );
   w = XtCreateManagedWidget( "ConstraintNet",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)CSNET_show, NULL );
   MMconsatN = w;

   XtSetArg( args[0], XmNsensitive, False );
   w = XtCreateManagedWidget( "Restriction",
          xmPushButtonWidgetClass,
          parent, args, 1 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)RESTRICTION_show, NULL );
   MMconsatR = w;

   XtCreateManagedWidget( "separator",
      xmSeparatorGadgetClass,
      parent, NULL, 0 );

   /*** creates "Task" MenuEntry of EDIT Submenu ***/
   w = XmCreatePushButton( parent, "Task", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)TASK_show, NULL );
   MMtask = w;

   /*** creates "Misc" MenuEntry of EDIT Submenu ***/
   w = XmCreatePushButton( parent, "Misc", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)MISC_show, NULL );
   MMmisc = w;
}
#endif /* RUNTIME */

/****************************************************
*   creates Session Pulldown Menu                   *
*                                                   *
*   NEVER set ShowDebugger Button insensitive!      *
*   PAD_show() additionally pops PAD up!            *
****************************************************/

static void
createSessionPDM( Widget parent ) {
   Widget w;
   Arg args[4];
   int n;

   XtSetArg( args[0], XmNsensitive, False );
   w = XmCreatePushButton( parent, "SessionRun", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)SessionRunCB, NULL );
   SessionRunW = w;

   w = XmCreateSeparatorGadget( parent, "separator", NULL, 0 );
   XtManageChild( w );

   XtSetArg( args[0], XmNsensitive, True );
   w = XmCreatePushButton( parent, "ShowPad", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)PAD_show, NULL );

   XtSetArg( args[0], XmNsensitive, False );

   w = XmCreatePushButton( parent, "ShowExplain", args, 1 );
   XtManageChild( w );
   ExplainW = w;
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)Explain_PAD_show, NULL );

#ifndef RUNTIME
   w = XmCreatePushButton( parent, "ShowTrace", args, 1 );
   XtManageChild( w );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)Trace_PAD_show, NULL );
   SessionTraceW = w;
#endif /* RUNTIME */
}

/****************************************************
*   Completes Initialisation of the Xapplication    *
*                                                   *
*   Manages the Scale for amusing people while      *
*   waiting for editors to be built                 *
*   When called the first time, it manages Scale,   *
*   When called the SCALE_DIVS'st time, hides it.   *
*                                                   *
*   XSync( XtDisplay(Scale), False ) doesn't        *
*   make sense: Long distance connections           *
*   do not succeed in drawing, anyway!              *
*   It simply makes booting slower!!!               *
****************************************************/

static Widget XINIT_complete_W;
static int    XINIT_complete_cnt;
static int    XINIT_complete_flag;

static void
XINIT_complete_TO( void ) {
   int val;

   val = (XINIT_complete_cnt * 1000) / SCALE_DIVS;
   if( val > 1000 )
      val = 1000;

_dbg("xct01");
   switch( XINIT_complete_cnt++ ) {
      case 0:
_dbg("xctc00");
         /*** Wait to init the Scale ***/
         break;
      case 1:
_dbg("xctc01");
         fprintf( stderr, " >>> building GUI ...\n" );
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Help Window ...\n" );
         HELP_create( XINIT_complete_W );
         break;
      case 2:
_dbg("xctc02");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the LISP-Debugger's PAD ...\n" );
         PAD_create( XINIT_complete_W );
         break;
      case 3:
_dbg("xctc03");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the User Interface Dialogs ...\n" );
         UID_create( XINIT_complete_W );
         break;
      case 4:
_dbg("xctc04");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Application MainMenu ...\n" );
         MAINMENU_create( XINIT_complete_W );
         break;
      case 5:
_dbg("xctc05");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the remaining system dialogs ...\n" );
         SYSD_create( SystemDialogs );
         break;
      case 6:
_dbg("xctc06");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating Explanation/Inspection facility ...\n" );
         Explain_PAD_create( XINIT_complete_W );
         break;
      case 7:
_dbg("xctc07");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating Explanation facility's dialogs ...\n" );
         EXPD_create( SystemDialogs );
         break;
      case 8:
_dbg("xctc08");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> Actions for upcase-input to TextFields ...\n" );
         XtAddActions( upcase_actions, XtNumber(upcase_actions) );
         fprintf( stderr, " >>> ... done\n" );
         break;

#ifndef RUNTIME
         /*** create Editors for the knowledge engineer ***/
      case 9:
_dbg("xctc09");
         fprintf( stderr, " >>> building editors ...\n" );
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Frame Editor ...\n" );
         FRAME_create( XINIT_complete_W );
         break;
      case 10:
_dbg("xctc10");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Instance Editor ...\n" );
         INSTANCE_create( XINIT_complete_W );
         break;
      case 11:
_dbg("xctc11");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Behavior Editor ...\n" );
         BEHAVIOR_create( XINIT_complete_W );
         break;
      case 12:
_dbg("xctc12");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Rule Editor ...\n" );
         RULE_create( XINIT_complete_W );
         break;
      case 13:
_dbg("xctc13");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Prolog Editor ...\n" );
         PROLOG_create( XINIT_complete_W );
         break;
      case 14:
_dbg("xctc14");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Constraint Editor ...\n" );
         CONSTRAINT_create( XINIT_complete_W );
         break;
      case 15:
_dbg("xctc15");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Constraint Net Editor ...\n" );
         CSNET_create( XINIT_complete_W );
         break;
      case 16:
_dbg("xctc16");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Restriction Editor ...\n" );
         RESTRICTION_create( XINIT_complete_W );
         break;
      case 17:
_dbg("xctc17");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Task Editor ...\n" );
         TASK_create( XINIT_complete_W );
         break;
      case 18:
_dbg("xctc18");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Misc. Editor ...\n" );
         MISC_create( XINIT_complete_W );
         break;

         /*** create Tracer for the knowledge engineer ***/
      case 19:
_dbg("xctc19");
         if( XINIT_complete_flag )
            fprintf( stderr, "  >>> creating the Tracer's PAD ...\n" );
         Trace_PAD_create( XINIT_complete_W );
         fprintf( stderr, " >>> ... done\n" );
         break;
#endif /* RUNTIME */

      default:   /* (#20 or #9) */
         /* Makes the scale invisible */
_dbg("xct94");
         XtUnmapWidget( Scale );
         /*** run LISP via TimerCallback (decoupled from graphics) ***/
_dbg("xct95");
         AddTimeOut( TIME_LISP_RUN, (XtPointer)LISP_run, NULL );
_dbg("xct96");
         return;
   }
_dbg("xct97");
   XmScaleSetValue( Scale, val );
_dbg("xct98");
   AddTimeOut( TIME_LISP_RUN, (XtPointer)XINIT_complete_TO, NULL );
_dbg("xct99");
}

void
XINIT_complete( Widget parent, int flag ) {

   XINIT_complete_W = parent;
   XINIT_complete_cnt = 0;
   XINIT_complete_flag = flag;

   /*
    * Hi, Motif Programmers, did you plan it to be a bug or a feature,
    * that your Scale looks some kind of ugly between managing and first 
    * time repositioning ?  (since Billy boy one should ask first ;)
    */
_dbg("xc01");
   XmScaleSetValue( Scale, 10 );
_dbg("xc02");

   /*** add a timeout, to give a chance to the ***/
   /*** Xserver to complete drawing the welcoming popup ***/  
   AddTimeOut( TIME_XINIT_REST, (XtPointer)XINIT_complete_TO, NULL );
_dbg("xc03");
}

/****************************************************
*   Create the system dialogs shell and             *
*   a welcoming Popup                               *
****************************************************/

Widget
WELCOME_create( Widget parent ) {
   Widget shellW, formW, labelW, logoW;
   Arg    args[32];
   int    i, n;
   int    maxWidth, maxHeight;
   char   geom[256];

_dbg("wc01");
   /*** Shell ***/
   n = 0;
   /* XmNx and XmNy are ignored by the window manager !!! */
   XtSetArg( args[n], XmNgeometry,        geometryConstraint ); n++;
   XtSetArg( args[n], XmNdefaultPosition,               True ); n++;
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
_dbg("wc02");
   shellW = XtAppCreateShell( "SystemDialogs", "HereClassIsIgnored",
                                /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

_dbg("wc03");
   /* necessary to enable GeometryManagement for its children */
   XtRealizeWidget(shellW);

_dbg("wc04");
   /*** WelcomeBox ***/
   n = 0;   /* to make the parents geometry useable! see Xres! */
   XtSetArg( args[n], XmNdefaultPosition,               True ); n++;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   /*
    * the goal is: Welcome should not be iconifyable, resizeable,
    * but yet killable.
    * No title, no WM-functions except ALT-F4 remain available!
    * To grant this goal, Xresources must be hardcoded to avoid control 
    * by XresourceFiles!!!
    */
   XtSetArg( args[n], XmNiconic,                       False ); n++;
   XtSetArg( args[n], XmNinitialState,           NormalState ); n++;
   XtSetArg( args[n], XmNmwmDecorations,    MWM_DECOR_BORDER ); n++;
   XtSetArg( args[n], XmNmwmFunctions,        MWM_FUNC_CLOSE ); n++;
_dbg("wc05");
   formW = XmCreateFormDialog( shellW, "Welcome", args, n );
_dbg("wc06");
_dbgs(XmClassName(formW));

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( formW, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MASTER_PAGE );

_dbg("wc07");
   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   /*
    * LISP_kill is allowed here:
    * while this Box is visible, no KB has yet been loaded
    */
_dbg("wc08");
   welcomeBoxW = XtParent( formW );   /** the DialogShell **/
_dbg("wc09");
   XtVaSetValues( welcomeBoxW, XmNmappedWhenManaged, False, NULL );
_dbgs(XmClassName(welcomeBoxW));
_dbgs(XmSuperclassName(welcomeBoxW));
_dbgs(XmSuperSuperclassName(welcomeBoxW));
_dbg("wc10-WM_DELETE_WINDOW");
   XmAddWMProtocolCallback( welcomeBoxW, WM_DELETE_WINDOW,
      (XtPointer)LISP_kill, NULL );

_dbg("wc11");
   n = 0;  
   XtSetArg( args[n], XmNtraversalOn,                 False ); n++;
   XtSetArg( args[n], XmNnavigationType,             XmNONE ); n++;
   XtSetArg( args[n], XmNhighlightThickness,              0 ); n++;
   XtSetArg( args[n], XmNtopAttachment,       XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,    XmATTACH_FORM ); n++;

   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_FORM ); n++;
_dbg("wc12");
   labelW = XmCreateLabel( formW, "Label", args, n );

_dbg("wc13");
   /*** create a Scale for amusing people while waiting for editors to be built ***/
   n = 5;
   XtSetArg( args[n], XmNrightAttachment,   XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNrightWidget,                labelW ); n++;

   XtSetArg( args[n], XmNsensitive,                   False ); n++;
   XtSetArg( args[n], XmNtitleString, XmStringCreateSimple("") ); n++;
   XtSetArg( args[n], XmNshowValue,                   False ); n++;
   XtSetArg( args[n], XmNorientation,            XmVERTICAL ); n++;
   XtSetArg( args[n], XmNprocessingDirection,  XmMAX_ON_TOP ); n++;
   XtSetArg( args[n], XmNvalue,                           0 ); n++;
   XtSetArg( args[n], XmNminimum,                         0 ); n++;
   XtSetArg( args[n], XmNmaximum,                      1000 ); n++;
_dbg("wc14");
   Scale = XmCreateScale( formW, "Scale", args, n );
_dbg("wc15");
   XtManageChild( Scale );
_dbg("wc16");

   /*** PushButton for the Logo Pixmap ***/
   n = 5;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,   XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNrightWidget,                 Scale ); n++;

   XtSetArg( args[n], XmNshadowThickness,                 0 ); n++;
   XtSetArg( args[n], XmNlabelType,                XmPIXMAP ); n++;
/*   XtSetArg( args[n], XmNsensitive,                   False ); n++;*/
   /* This is a PushB and not a Label to make XResourceFile easier */
_dbg("wc17");
   logoW = XmCreatePushButton( formW, "Logo", args, n );
_dbg("wc18");

   if( XmUNSPECIFIED_PIXMAP == createPushButtonPixmap(logoW,LOGO_XBM) ) {
_dbg("wc18un");
      XtVaSetValues( labelW, XmNleftAttachment, XmATTACH_FORM, NULL );
   } else {
_dbg("wc18mg");
      XtManageChild( logoW );
   }
_dbg("wc19");
   XtManageChild( labelW );
_dbg("wc20");
   XtManageChild( formW );

_dbg("wc21");
   XmScaleSetValue( Scale, 1000 );
_dbg("wc22");
   XmScaleSetValue( Scale, 0 );

_dbg("wc23");
   return shellW;   /* ---> main.c: SystemDialogs */
}

/****************************************************
*   Change Sensitivity of the WelcomeBox            *
*   (needed only while booting, hence after booting *
*   this Popup remains popped down.                 *
*                                                   *
*   INFORMATION: The Children (PopupDialogs) of the *
*      WelcomeBox are NOT concerned of the cursor   *
*      settings of WelcomeBox. They have to be set  *
*      explicitly!                                  *
*                                                   *
*   UID- and System-dialogs should NEVER display a  *
*   busy cursor. (e.g.: MainQuitInfo, MainHelpInfo) *
*                                                   *
*   (accessable from outside...)                    *
****************************************************/

void
WELCOME_ready( void ) {

   if( XtIsRealized( welcomeBoxW ) ) {
      XDefineCursor(
         XtDisplay( welcomeBoxW ),
         XtWindow( welcomeBoxW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
WELCOME_busy( void ) {

   if( XtIsRealized( welcomeBoxW ) ) {
      XDefineCursor(
         XtDisplay( welcomeBoxW ),
         XtWindow( welcomeBoxW ),
         cursor_busy
      );
   }
}

/***************************************************/

void
WELCOME_show( void ) {

   PopupRaiseAndDeiconify( welcomeBoxW );
}

/***************************************************/

void
WELCOME_hide( void ) {

   XtPopdown( welcomeBoxW );
}

/****************************************************
*   creates ApplicationMainMenu                     *
*                                                   *
*   The Popups created here are SystemDialogs,      *
*   because SystemDialogs is placed in the center   *
*   of the screen (--> geometry-Resource)           *
*   and the MainMenu is placed in the left-top      *
*   corner...                                       *
****************************************************/

void
MAINMENU_create( Widget parent ) {
   Widget formW, logoW;
   Widget menu, cb, hb, ob, help, helpP, w;
   Widget fileP, file, editP, edit, sessionP, session;
   Arg args[20];
   int n, nSave;

   /*** Shell ***/
   n = 0;
   /* XmNx and XmNy are ignored by the window manager !!! */
   XtSetArg( args[n], XmNiconPixmap,                    iconPm ); n++;
   XtSetArg( args[n], XmNdeleteResponse,          XmDO_NOTHING ); n++;
   /*
    * at child-ToplevelObjects the class name is ignored for the
    * information in XresourceFiles
    * --> here an explicit instance name is necessary !!!
    */
   MainMenu = XtAppCreateShell( "MainMenu", "HereClassIsIgnored",
                              /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
#ifndef RUNTIME
   XmAddWMProtocolCallback( MainMenu, WM_DELETE_WINDOW,
      (XtPointer)MainQuitCB, NULL );
#else /* RUNTIME */
   XmAddWMProtocolCallback( MainMenu, WM_DELETE_WINDOW,
      (XtPointer)MainQuitOkCB, NULL );
#endif /* RUNTIME */

   /*** Form-Widget ****************************/
   n = 0;
   formW = XmCreateForm( MainMenu, "Form", args, n );
   XtManageChild( formW );

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( formW, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MAIN_MENU );

   /*** Menu-Pane ******************************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,          XmATTACH_FORM ); n++;
   menu = XmCreateMenuBar( formW, "Menu", args, n );
   XtManageChild( menu );

   /* --- File-PulldownMenu: Child of menu --- */
   fileP = XmCreatePulldownMenu( menu, "FileP", NULL, 0 );
   XtSetArg( args[0], XmNsubMenuId, fileP );   /* attach submenu */
   file = XmCreateCascadeButton( menu, "File", args, 1 );
   XtManageChild( file );

#ifndef RUNTIME
   /* --- Edit-PulldownMenu: Child of menu --- */
   editP = XmCreatePulldownMenu( menu, "EditP", NULL, 0 );
   XtSetArg( args[0], XmNsubMenuId, editP );
   edit = XmCreateCascadeButton( menu, "Edit", args, 1 );
   XtManageChild( edit );
#endif /* RUNTIME */

   /* --- Session-PulldownMenu: Child of menu --- */
   sessionP = XmCreatePulldownMenu( menu, "SessionP", NULL, 0 );
   XtSetArg( args[0], XmNsubMenuId, sessionP );
   session = XmCreateCascadeButton( menu, "Session", args, 1 );
   XtManageChild( session );

   /* --- Help-PulldownMenu: Child of menu --- */
   helpP = XmCreatePulldownMenu( menu, "HelpP", NULL, 0 );
   XtSetArg( args[0], XmNsubMenuId, helpP );
   help = XmCreateCascadeButton( menu, "Help", args, 1 );
   XtManageChild( help );
   XtVaSetValues( menu, XmNmenuHelpWidget, help, NULL );

   /* --- HelpOnMenu-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnMenu", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MAIN_MENU );
   XtManageChild( w );

   /* --- HelpSeparator, Child of helpP --- */
   w = XmCreateSeparatorGadget( helpP, "HelpSeparator", NULL, 0 );
   XtManageChild( w );

   /* --- HelpOnEmaxps-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnEmaxps", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MASTER_PAGE );
   XtManageChild( w );

   /* --- HelpOnRefs-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnRefs", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_REFERENCES );
   XtManageChild( w );

   /* --- HelpOnProjects-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnProjects", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_PROJECTS );
   XtManageChild( w );

   /* --- HelpSeparator, Child of helpP --- */
   w = XmCreateSeparatorGadget( helpP, "HelpSeparator", NULL, 0 );
   XtManageChild( w );

   /* --- HelpOnDebugger-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnDebugger", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_DEBUGGER_PAD );
   XtManageChild( w );

#ifndef RUNTIME
   /* --- HelpOnEditors-Button, Child of helpP --- */
   w = XmCreatePushButtonGadget( helpP, "HelpOnEditors", NULL, 0 );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_ON_EDITORS );
   XtManageChild( w );

   /*** MainQuitBox: Child of SystemDialogs ***/
   MainQuitInfo = XmCreateQuestionDialog( SystemDialogs, "MainQuit", NULL, 0 );

   /* QuitBox should have 2 buttons */
   cb = XmMessageBoxGetChild( MainQuitInfo, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback,
      (XtPointer)MainQuitCancelCB, NULL );
   hb = XmMessageBoxGetChild( MainQuitInfo, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   ob = XmMessageBoxGetChild( MainQuitInfo, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback,
      (XtPointer)MainQuitOkCB, NULL );

   /* this won't work with Linux... (see popup CB above) */
   XtVaSetValues( MainQuitInfo, XmNdefaultButton, cb, NULL ); /* Cancel=default */
   /* sometimes this works, sometimes it won't ... */
   XtAddCallback( MainQuitInfo, XmNmapCallback, (XtPointer)focusMainQuitCB, NULL );

   /*** MainCloseBox: Child of SystemDialogs ***/
   MainCloseInfo = XmCreateQuestionDialog( SystemDialogs, "MainClose", NULL, 0 );

   /* QuitBox should have 2 buttons */
   cb = XmMessageBoxGetChild( MainCloseInfo, XmDIALOG_CANCEL_BUTTON );
   XtAddCallback( cb, XmNactivateCallback,
      (XtPointer)MainCloseCancelCB, NULL );
   hb = XmMessageBoxGetChild( MainCloseInfo, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( hb );
   ob = XmMessageBoxGetChild( MainCloseInfo, XmDIALOG_OK_BUTTON );
   XtAddCallback( ob, XmNactivateCallback,
      (XtPointer)MainCloseOkCB, NULL );

   /* this won't work with Linux... (see popup CB above) */
   XtVaSetValues( MainCloseInfo, XmNdefaultButton, cb, NULL ); /* Cancel=default */
   /* sometimes this works, sometimes it won't ... */
   XtAddCallback( MainCloseInfo, XmNmapCallback, (XtPointer)focusMainCloseCB, NULL );
#endif /* RUNTIME */

   /*** create FILE-PulldownMenu's entries ******/
   createFilePDM( fileP );

#ifndef RUNTIME
   /*** create EDIT-PulldownMenu's entries: ***/
   createEditorButtons( editP );
#endif /* RUNTIME */

   /*** create SESSION-PulldownMenu's entries ******/
   createSessionPDM( sessionP );

   /*** create PushButton for the Logo ******************************/
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,    XmATTACH_FORM ); n++;

   XtSetArg( args[n], XmNtopAttachment,     XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                    menu ); n++;

   XtSetArg( args[n], XmNlabelType,                XmPIXMAP ); n++;
   logoW = XmCreateLabel( formW, "Logo", args, n );

   if( XmUNSPECIFIED_PIXMAP != createPushButtonPixmap(logoW,LOGO_XBM) )
      XtManageChild( logoW );
}

/****************************************************
*   Change Sensitivity of the Applications MainMenu *
*                                                   *
*   !!! No complete Insensitivity !!!               *
*   Only some MenuButtons!                          *
*                                                   *
*   (accessable from outside...)                    *
****************************************************/

void
MAINMENU_ready( void ) {

   if( XtIsRealized( MainMenu ) ) {
      XDefineCursor(
         XtDisplay( MainMenu ),
         XtWindow( MainMenu ),
         cursor_normal
      );
   }
}

/***************************************************/

void
MAINMENU_busy( void ) {

   if( XtIsRealized( MainMenu ) ) {
      XDefineCursor(
         XtDisplay( MainMenu ),
         XtWindow( MainMenu ),
         cursor_busy
      );
   }
}

/***************************************************/

void
MAINMENU_show( void ) {

   PopupRaiseAndDeiconify( MainMenu );
}

/***************************************************/

void
XAPPL_showLispState( int lisp_is_busy ) {
   
   if( lisp_is_busy ) {
      /*** add concerned sub-applications here ***/
      PAD_busy();
      WELCOME_busy();
      MAINMENU_busy();
      SESSION_busy();
      Explain_PAD_busy();
#ifndef RUNTIME
      Trace_PAD_busy();
      RULE_busy();
      PROLOG_busy();
      CONSTRAINT_busy();
      RESTRICTION_busy();
      FRAME_busy();
      BEHAVIOR_busy();
      INSTANCE_busy();
      TASK_busy();
      MISC_busy();
#endif
   } else {
      /*** add concerned sub-applications here ***/
      PAD_ready();
      WELCOME_ready();
      MAINMENU_ready();
      SESSION_ready();
      Explain_PAD_ready();
#ifndef RUNTIME
      Trace_PAD_ready();
      RULE_ready();
      PROLOG_ready();
      CONSTRAINT_ready();
      RESTRICTION_ready();
      FRAME_ready();
      INSTANCE_ready();
      BEHAVIOR_ready();
      TASK_ready();
      MISC_ready();
#endif
   }
}

/****************************************************
*   Get List of all known KnowledgeBases            *
*                                                   *
*   Memory has to be freed by calling function      *
*                                                   *
*   (accessable from outside...)                    *
****************************************************/

char **
KNOWNKBS_getList( void ) {
   char **actual_list;
   int i;

   actual_list = (char **)XtMalloc( 10 * sizeof(char*) );

   actual_list[0] = (char*)kb_current;

   i = 1;
   while( KnownKBs[i].name ) {
      actual_list[i] = (char *)XtMalloc( 1 + strlen( KnownKBs[i].name ) );
      strcpy( actual_list[i], KnownKBs[i].name );
      i++;
   }
   actual_list[i] = NULL;

   return actual_list;
}

/****************************************************
*   Updates the C-side Data Structures each time    *
*   changes take place at LISP side!                *
*                                                   *
*   Useful to find out, whether an Editor holds     *
*   KB specific data -- and the KB no longer exists *
****************************************************/

static void
C_updateData( void ) {
   int i, j;

   /*
    * if entry currently in use: kbname != NULL && MOD_VALID !!!
    */
   for( i = 1; i <= 9; i++ )
      C_editors[i].flags &= ~MOD_VALID;  /* reset */

   /* compare kbname-s, which continue to exist ... */
   for( j = 1; j <= kbs_all; j++ ) {
      for( i = 1; i <= 9; i++ ) {
         if( !C_editors[i].kbname )   /* unused entry */
            continue;
         if( !strcmp( KnownKBs[j].name, C_editors[i].kbname ) ) {
            C_editors[i].flags |= MOD_VALID;  /* yet used ==> set */
            break;
         }
      }
   }
   /* re-init unused entries */
   for( i = 1; i <= 9; i++ ) {
      if( C_editors[i].flags & MOD_VALID )
         continue;
      /* WARN in case of unsaved Edits for a no longer existing KB */
      if( C_editors[i].flags & ~MOD_VALID ) {
         XBell( XtDisplay(MainMenu), 100 );
         fprintf( stderr,
            "ERROR: Some editors (0x%X) hold unsaved changes for\n"
            "       KB %s (%d), which no longer exists!\n",
            C_editors[i].flags & ~MOD_VALID,
            C_editors[i].kbname, i );
      }
      if( C_editors[i].kbname ) {
         XtFree( C_editors[i].kbname );
         C_editors[i].kbname = NULL;
      }
      C_editors[i].flags = 0;
   }
   /* add new KBs */
   for( j = 1; j <= kbs_all; j++ ) {
      for( i = 1; i <= 9; i++ ) {
         if( C_editors[i].kbname &&
             !strcmp( KnownKBs[j].name, C_editors[i].kbname ) ) {
            goto continue_outer_loop;
         }
      }
      for( i = 1; i <= 9; i++ ) {
         if( !C_editors[i].kbname ) {
            C_editors[i].kbname = XtMalloc( 1+strlen(KnownKBs[j].name) );
            strcpy( C_editors[i].kbname, KnownKBs[j].name );
            C_editors[i].flags = MOD_VALID;
            goto continue_outer_loop;
         }
      }
      /* this code should never be reached! */
      XBell( XtDisplay(MainMenu), 100 );
      fprintf( stderr, "Unexpected ERROR in C_updateData() !\n" );
 continue_outer_loop:
      i++;     /* this creazy two little lines have become necessary, since */
      i--;     /* AIX cc does not like jumping to the end of a block!?!?!?! */
   }
}

/****************************************************
*   Find out whether there are unsaved changes for  *
*   this KB. LISP must hold Info's on changes       *
*   within the LISP world. Editors' Save to LISP    *
*   should end in a (emaxps-kbs-state) !!! C must   *
*   hold Info's on changes within the Editors, too! *
****************************************************/

static int
C_unsavedp( char *kbname ) {
   int i, ret = False;

   for( i = 1; i <= 9; i++ ) {
      if( !(C_editors[i].flags & MOD_VALID) )
         continue;
      if( !strcmp( kbname, C_editors[i].kbname ) ) {
         if( C_editors[i].flags & ~MOD_VALID )
            ret = True;
         break;
      }
   }
   if( i >= 10 ) {
      XBell( XtDisplay(MainMenu), 100 );
      fprintf( stderr,
         "ERROR: C_unsavedp: Unknown KB %s!\n",
         kbname );
   }
   return ret;
}

/****************************************************
*   Modify the per-KB-data of currently changed     *
*   LISP constructs within the Editors              *
****************************************************/

void
C_set_modified( char *selected_kb, int editor, int mode ) {
   int i;
   static int  old_editor = 0;
   static int  old_mode = 0;
   static char old_kb[128] = "";

   editor &= ~MOD_VALID;

   if( old_editor == editor && old_mode == mode &&
       !strcmp( old_kb, selected_kb) )
      return;   /* repeatedly! */

   strcpy( old_kb, selected_kb );
   old_editor = editor;
   old_mode = mode;

/*printf("entering C_set_modified (%s, %d, %d) ...\n", selected_kb, editor, mode );*/
   for( i = 1; i <= 9; i++ ) {
      if( !(C_editors[i].flags & MOD_VALID) )
         continue;
      if( !strcmp( selected_kb, C_editors[i].kbname ) ) {
         if( mode )     /* set */
            C_editors[i].flags |= editor;
         else         /* reset */
            C_editors[i].flags &= ~editor;

         /* change Text labels in FilePDM! */
         FileSetKbNames();
         break;
      }
   }
   if( i >= 10 ) {
      XBell( XtDisplay(MainMenu), 100 );
      fprintf( stderr,
         "ERROR: Editor (%x) tries to edit the unknown KB %s!\n",
         editor, selected_kb );
   }
}

/*** EOF *******************************************/

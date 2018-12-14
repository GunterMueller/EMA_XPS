/****************************************************
*   trace.c       Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                                                   *
*   Functions to build the tracer.                  *
*   Full version only.                              *
****************************************************/

#include <stdio.h>
 
#include <Xm/Form.h> 
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/ToggleBG.h>

#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/CascadeBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Protocols.h>

#include "main.h"
#include "trace.h"
#include "xinit.h"
#include "help.h"

/*** Typedefs  ***************************************/

typedef struct _kbs_trace_status {
  char *name;
  int task_trace;
  int system_trace;
  int rule_trace;
  int prolog_trace;
  int constraint_trace;
} KBS_TRACE_STATUS;

/*** Defines ***************************************/

#define TRACE_TOGGLE_TASK         1
#define TRACE_TOGGLE_SYSTEM       2
#define TRACE_TOGGLE_RULE         3
#define TRACE_TOGGLE_PROLOG       4
#define TRACE_TOGGLE_CONSAT       5

/*** Globals ***************************************/

static Widget shellW, outputW, kb_nameW, kb_labelW;
static Widget TaskW, RuleW, SystemW, PrologW, ConstraintW;

static KBS_TRACE_STATUS kbs_trace[10];
static int aktuell_kb;

/*** Function Declarations *************************/

static void eraseCB( void );
static void displayTraceOutput( char *input ) ;
static void Trace_PAD_update( void );

/*** Functions ************************************/

static int
str_i_cmp( char *s1, char *s2 ) {
   int c;

   do {
      c = toupper(*s1) - toupper(*s2);
   } while( *s1++ && *s2++ && !c );
   return c;
}

/***************************************************
*   find_kbname                                    *
*   returns the array-position of the kb-name      *
*   or -1                                          *
***************************************************/

static int 
find_kbname( char *name ) {
   int i;

   for( i = 0; i <= 9; i++ ) {
      if( kbs_trace[i].name )
         if( !strcmp( name, kbs_trace[i].name ) )
            return i;
   }
   return -1;
}

/***************************************************
*   find_first_free                                *
*   returns the first free position in kb-array    *
***************************************************/

static int 
find_first_free( void ) {
   int i;

   for( i = 0; i <= 9; i++ ) {
      if( !kbs_trace[i].name )
         return i;
   }
   return -1;
}

/***************************************************
*   Entry point from xinit.c                       *
*   (multiple known KB management)                 *
***************************************************/

void
TRACE_informOnKBSstate( int kbs_all ) {
   int n;

   if( kbs_all <= 0 ) {
      for( n = 0; n <= 9; n++ ) {
         if( kbs_trace[n].name )
            XtFree( kbs_trace[n].name );
         kbs_trace[n].name = NULL;
         kbs_trace[n].task_trace = 0;  
         kbs_trace[n].rule_trace = 0;
         kbs_trace[n].system_trace = 0;
         kbs_trace[n].prolog_trace = 0;
         kbs_trace[n].constraint_trace = 0;
      }
      Trace_PAD_hide();
      return;
   }
   Trace_PAD_update();
}

/**************************************************/

static void
Trace_PAD_update( void ) {
   char **KB_list;
   int i, n, m;
   XmString xmstr;

   KB_list = KNOWNKBS_getList();
   i = (int)KB_list[0];      /* offset in vector to current KB */

   /* no KB active or active KB has no name YET */
   /* NIL is a temporary effect, while loading a KB */
   if( !i || !str_i_cmp( KB_list[i], "NIL") ) {
      XtFree( (char*)KB_list );  
      return;
   }

   /* an correct active KB exists, view its name */
   xmstr = XmStringCreateSimple( KB_list[i] );
   XtVaSetValues( kb_nameW, XmNlabelString, xmstr, NULL ); 
   XmStringFree( xmstr );

   /* check for obsolete local entries first */
   for( n = 0; n <= 9; n++ ) {      /* locally */
      if( !kbs_trace[n].name ) {
         /* is already an empty field */
         continue;
      }
      for( m = 1; m <= 9; m++ ) {   /* from xinit.c */
         if( !KB_list[m] ) {
            /* no more globally known */
            if( kbs_trace[n].name )
               XtFree( kbs_trace[n].name );
            kbs_trace[n].name = NULL;
            kbs_trace[n].task_trace = 0;  
            kbs_trace[n].rule_trace = 0;
            kbs_trace[n].system_trace = 0;
            kbs_trace[n].prolog_trace = 0;
            kbs_trace[n].constraint_trace = 0;
            break;
         }
         if( !str_i_cmp( KB_list[m], kbs_trace[n].name ) ) {
            /* found... */
            break;
         }
      }
   }

   /* if this is a new KB, it has not been registered locally */
   if( -1 == (aktuell_kb = find_kbname( KB_list[i] )) ) {
      aktuell_kb = find_first_free();
      kbs_trace[aktuell_kb].name = XtMalloc( 256 * sizeof(char) );
      strcpy( kbs_trace[aktuell_kb].name, KB_list[i] );
      kbs_trace[aktuell_kb].task_trace = 0;  
      kbs_trace[aktuell_kb].rule_trace = 0;
      kbs_trace[aktuell_kb].system_trace = 0;
      kbs_trace[aktuell_kb].prolog_trace = 0;
      kbs_trace[aktuell_kb].constraint_trace = 0;
   }       
   XtFree( (char*)KB_list );  

   if( XmToggleButtonGetState(TaskW) !=
         kbs_trace[aktuell_kb].task_trace )
      XtVaSetValues( TaskW, XmNset,
         kbs_trace[aktuell_kb].task_trace,
         NULL );
   if( XmToggleButtonGetState(SystemW) !=
         kbs_trace[aktuell_kb].system_trace )
      XtVaSetValues( SystemW, XmNset,
         kbs_trace[aktuell_kb].system_trace,
         NULL );
   if( XmToggleButtonGetState(RuleW) !=
         kbs_trace[aktuell_kb].rule_trace )
      XtVaSetValues( RuleW, XmNset,
         kbs_trace[aktuell_kb].rule_trace,
         NULL );
   if( XmToggleButtonGetState(PrologW) !=
         kbs_trace[aktuell_kb].prolog_trace )
      XtVaSetValues( PrologW, XmNset,
         kbs_trace[aktuell_kb].prolog_trace,
         NULL );
   if( XmToggleButtonGetState(ConstraintW) !=
         kbs_trace[aktuell_kb].constraint_trace )
      XtVaSetValues( ConstraintW, XmNset,
         kbs_trace[aktuell_kb].constraint_trace,
         NULL );
}

/******* TraceMessage LISP --> C ******************/

void
TraceMessage( void ) {
   char *p = MSG_read();

   displayTraceOutput( kbs_trace[aktuell_kb].name );
   displayTraceOutput(": ");
   displayTraceOutput( p );
   displayTraceOutput( "\n" );
   XtFree( p );
}

/***************************************************
*    Toggling reactions from LISP side             *
***************************************************/

void
TraceTaskToggle( void ) {
   char *p = MSG_read();
   int i = atoi(p);   /* 0 or 1 expected */

   kbs_trace[aktuell_kb].task_trace = i;
   i = i ? True : False;
   XtVaSetValues( TaskW, XmNset, i, NULL );
   XtFree( p );
   Trace_PAD_show();
}

/**************************************************/

void
TraceSystemToggle( void ) {
   char *p = MSG_read();
   int i = atoi(p);   /* 0 or 1 expected */

   kbs_trace[aktuell_kb].system_trace = i;
   i = i ? True : False;
   XtVaSetValues( SystemW, XmNset, i, NULL );
   XtFree( p );
   Trace_PAD_show();
}

/**************************************************/

void
TraceRuleToggle( void ) {
   char *p = MSG_read();
   int i = atoi(p);   /* 0 or 1 expected */

   kbs_trace[aktuell_kb].rule_trace = i;
   i = i ? True : False;
   XtVaSetValues( RuleW, XmNset, i, NULL );
   XtFree( p );
   Trace_PAD_show();
}

/**************************************************/

void
TracePrologToggle( void ) {
   char *p = MSG_read();
   int i = atoi(p);   /* 0 or 1 expected */

   kbs_trace[aktuell_kb].prolog_trace = i;
   i = i ? True : False;
   XtVaSetValues( PrologW, XmNset, i, NULL );
   XtFree( p );
   Trace_PAD_show();
}

/**************************************************/

void
TraceConstraintToggle( void ) {
   char *p = MSG_read();
   int i = atoi(p);   /* 0 or 1 expected */
   
   kbs_trace[aktuell_kb].constraint_trace = i;
   i = i ? True : False;
   XtVaSetValues( ConstraintW, XmNset, i, NULL );
   XtFree( p );
   Trace_PAD_show();
}

/****************************************************
*   updates contents of trace-output window         *
*   Scrolls the text in the output-window of the    *
*   LISP-Tracer's PAD to bottom                     *
****************************************************/

static void
displayTraceOutput( char *input ) {
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
*   Callback to clear the output window             *
****************************************************/

static void
eraseCB( void ) {

   XmTextSetString( outputW, "" );
   /* update the scrollbars */
   XmTextShowPosition( outputW, 0 );
}

/****************************************************
*   Callback for Trace-Toggles                      *
****************************************************/

static void
toggledCB( Widget widget, int bit ) {

   switch( bit ) {
      case TRACE_TOGGLE_TASK:
         LISP_sndCmd("(emaxps-send-task :toggle-task-trace)");
         XtVaSetValues( TaskW, XmNset,
            kbs_trace[aktuell_kb].task_trace,
            NULL );
         break;
     case TRACE_TOGGLE_SYSTEM:
         LISP_sndCmd("(toggle-system-trace)");
         XtVaSetValues( SystemW, XmNset,
            kbs_trace[aktuell_kb].system_trace,
            NULL );
         break;
     case TRACE_TOGGLE_RULE:
         LISP_sndCmd("(send-kb :toggle-rule-trace)");
         XtVaSetValues( RuleW, XmNset,
            kbs_trace[aktuell_kb].rule_trace,
            NULL );
         break;
     case TRACE_TOGGLE_PROLOG:
         LISP_sndCmd("(send-kb :toggle-prolog-trace)");
         XtVaSetValues( PrologW, XmNset,
            kbs_trace[aktuell_kb].prolog_trace,
            NULL );
         break;
     case TRACE_TOGGLE_CONSAT:
         LISP_sndCmd("(send-kb :toggle-constraints-trace)");
         XtVaSetValues( ConstraintW, XmNset,
            kbs_trace[aktuell_kb].constraint_trace,
            NULL );
         break;
   }
}

/****************************************************
*   create Trace-PAD                                *
****************************************************/

Widget
Trace_PAD_create( Widget parent ) {
   Arg args[20];
   Widget menuBarW, formW, fixedW, buttonFormW, closeW, eraseW;
   Widget hideW, rowcolW, W, toggleW, panedW;
   Widget trace_menu, clear_button, trace_menu_button;
   Widget on_menu_button, help_menu_button, help_menu;
   Widget clear_menu, on_clear_button;
   Widget kb_menuW;
   int n, nSave;
   Display *dpy;
   XmString xmstr;

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
   shellW = XtAppCreateShell( "Tracer", "HereClassIsIgnored",
                            /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( shellW, WM_DELETE_WINDOW,
      (XtPointer)Trace_PAD_hide, NULL );        /* Pop down ... */

   /***************   aeusseres Form-Widget   **************/
   formW = XtVaCreateManagedWidget( "form",
                xmFormWidgetClass, shellW,
                NULL );

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( formW, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_TRACE_MENU );

   /******************   MenuBar-Widget   ******************/
   menuBarW = XmVaCreateSimpleMenuBar( formW, "menuBar",
                 XmNisHomogeneous,      False,
                 XmNtopAttachment,      XmATTACH_FORM,
                 XmNleftAttachment,     XmATTACH_FORM,
                 XmNrightAttachment,    XmATTACH_FORM,
                 NULL );
   XtManageChild( menuBarW );

   /****************   Help-Menu   ************************/
   help_menu = XmCreateSimplePulldownMenu( menuBarW, "help_menu", NULL, 0 );
   help_menu_button = XtVaCreateManagedWidget( "Help",
                      xmCascadeButtonGadgetClass, menuBarW,
                      XmNsubMenuId,     help_menu,
                      NULL );
   on_menu_button = XtVaCreateManagedWidget( "on_menu",
                    xmPushButtonGadgetClass, help_menu,
                    NULL );

   XtAddCallback( on_menu_button, XmNactivateCallback,
                (XtPointer)HELP_show, (XtPointer)HELP_TRACE_MENU );
   XtVaSetValues( menuBarW, XmNmenuHelpWidget, help_menu_button, NULL );
                                                                                                       
      /******************   Menu-Trace-Widget   ******************/
   trace_menu   = XmCreateSimplePulldownMenu( menuBarW, "trace_menu", NULL, 0 );
   trace_menu_button = XtVaCreateManagedWidget( "trace",
                       xmCascadeButtonGadgetClass, menuBarW,
                       XmNsubMenuId,     trace_menu,
                       NULL );
                                                                     
   /******************   Clear-Button   ******************/
   clear_button = XtVaCreateManagedWidget( "clear_button",
                       xmPushButtonGadgetClass, trace_menu,
                       NULL );                                                                                                               
    XtAddCallback(clear_button, XmNactivateCallback,
             (XtPointer)eraseCB, NULL );
             
    /******************   KB-name-label   ******************/

   xmstr = XmStringCreateSimple(
      "___________________________(no kb)___________________________");
   kb_nameW =  XtVaCreateManagedWidget( "kb-name",
               xmCascadeButtonGadgetClass, menuBarW,
               XmNlabelString,      xmstr,
               XmNtraversalOn,      False,
               XmNshadowThickness,  0,
               NULL );
   XmStringFree( xmstr );

   /*** PanedW ***/
   panedW = XtVaCreateManagedWidget( "paned",
      xmPanedWindowWidgetClass, formW,
                XmNrightAttachment,      XmATTACH_FORM,
                XmNleftAttachment,       XmATTACH_FORM,
                XmNtopAttachment,      XmATTACH_WIDGET,
                XmNtopWidget,                 menuBarW,
                XmNbottomAttachment,     XmATTACH_FORM,
                NULL );

   /***********   Trace-Toggles   *************/
   toggleW = XtVaCreateManagedWidget( "RowColumn",
                xmRowColumnWidgetClass, panedW,
                XmNorientation,           XmHORIZONTAL,
                NULL );

   TaskW = XtVaCreateManagedWidget( "Task-Trace",
                  xmToggleButtonGadgetClass, toggleW,
                  NULL );
   XtAddCallback( TaskW, XmNvalueChangedCallback,
      (XtPointer)toggledCB, (XtPointer)TRACE_TOGGLE_TASK );

   SystemW = XtVaCreateManagedWidget( "System-Trace",
                  xmToggleButtonGadgetClass, toggleW,
                  NULL );
   XtAddCallback( SystemW, XmNvalueChangedCallback,
      (XtPointer)toggledCB, (XtPointer)TRACE_TOGGLE_SYSTEM );

   RuleW = XtVaCreateManagedWidget( "Rule-Trace",
                  xmToggleButtonGadgetClass, toggleW,
                  NULL );
   XtAddCallback( RuleW, XmNvalueChangedCallback,
      (XtPointer)toggledCB, (XtPointer)TRACE_TOGGLE_RULE );

   PrologW = XtVaCreateManagedWidget( "Prolog-Trace",
                  xmToggleButtonGadgetClass, toggleW,
                  NULL );
   XtAddCallback( PrologW, XmNvalueChangedCallback,
      (XtPointer)toggledCB, (XtPointer)TRACE_TOGGLE_PROLOG );

   ConstraintW = XtVaCreateManagedWidget( "Consat-Trace",
                  xmToggleButtonGadgetClass, toggleW,
                  NULL );
   XtAddCallback( ConstraintW, XmNvalueChangedCallback,
      (XtPointer)toggledCB, (XtPointer)TRACE_TOGGLE_CONSAT );

   /*** Output-Text-Window *****/
   n = 0;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,        XmSTATIC ); n++;
   XtSetArg( args[n], XmNeditable,                         False ); n++;
   XtSetArg( args[n], XmNeditMode,             XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNcursorPositionVisible,            False ); n++;
   outputW = XmCreateScrolledText( panedW, "Output", args, n );
   XtManageChild( outputW );

   return shellW;
}

/****************************************************
*   Change Sensitivity of the Trace PAD             *
*   (accessable from outside...)                    *
****************************************************/

void
Trace_PAD_ready( void ) {

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
Trace_PAD_busy( void ) {
                       
   if( XtIsRealized( shellW ) ) {
      XDefineCursor(
         XtDisplay( shellW ),
         XtWindow( shellW ),
         cursor_busy
      );
   }
}

/***************************************************/
/* Shows and updates the TRACE - PAD               */
/***************************************************/

void
Trace_PAD_show( void ) {

   PopupRaiseAndDeiconify( shellW );
   Trace_PAD_update(); 
}

/***************************************************/

void
Trace_PAD_hide( void ) {

   XtPopdown( shellW );
}

/*** EOF *******************************************/

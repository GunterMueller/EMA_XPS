/****************************************************
*   explain.c     Hans Groschwitz        08.12.94   *
*                 Karsten Vossberg       20.07.95   *
*                                                   *
*   Interface for graphic explanations when inspec- *
*   ting the success of inferencing at runtime.     *
****************************************************/

#include <stdio.h>

#include <Xm/Form.h> 
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/ToggleBG.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/CascadeBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>

#include "main.h"
#include "explain.h"
#include "timings.h"                 /* TIME_FOCUS */
#include "uid.h"                /* UID_debugString */
#include "xinit.h"
#include "help.h"

/*** Globals ***************************************/

static Widget shellW, outputW;

/*** Function Declarations *************************/

static void eraseCB( void );

/*** Functions *************************************/

void
ExplainMessage( void ) {             /* LISP --> C */
   char *p = MSG_read();

   ExplainDisplayOutput( p );
   ExplainDisplayOutput( "\n" );
   Explain_PAD_show();  
   XtFree( p );
}

/****************************************************
*   updates contents of Explain-output window       *
*   Scrolls the text in the output-window of the    *
*   Explain-PAD to bottom                           *
****************************************************/

void
ExplainDisplayOutput( char *input ) {
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
*   Callback to activate How-mode                   *
****************************************************/

static void
HowCB( void ) {

   LISP_sndCmd("(emaxps-explain-how)");
}

/****************************************************
*   Callback to activate How-ALL-mode               *
****************************************************/

static void
HowAllCB( void ) {

   LISP_sndCmd("(emaxps-explain-how-all)");
}

/****************************************************
*   Callback to activate Why-Not-mode               *
****************************************************/

static void
WhyNotCB( void ) {

   LISP_sndCmd("(emaxps-explain-why-not)");
}

/****************************************************
*   Callback to activate Inspect-Rule-mode          *
****************************************************/

static void
InspectTermsCB( void ) {

   LISP_sndCmd("(emaxps-explain-inspect-terms)");
}

/****************************************************
*   Callback to activate Print-Rule-mode            *
****************************************************/

static void
PrintRuleCB( void ) {

   LISP_sndCmd("(emaxps-explain-view)");
}

/****************************************************
*   Callback to activate Show-Facts-All-Mode        *
****************************************************/

static void
ShowFactsAllCB( void ) {

   LISP_sndCmd("(emaxps-explain-print-all-facts)");
}

/****************************************************
*   Callback to activate Show-Facts-True-Mode       *
****************************************************/

static void
ShowFactsTrueCB( void ) {

   LISP_sndCmd("(emaxps-explain-print-true-facts)");
}

/****************************************************
*   Callback to activate Show-Facts-Unprovable-Mode *
****************************************************/

static void
ShowFactsUnprovableCB( void ) {

   LISP_sndCmd("(emaxps-explain-print-unprovable-facts)");
}

/****************************************************
*   Callback to activate Why-Goal-mode              *
****************************************************/

static void
WhyGoalCB( void ) {

   LISP_sndCmd("(emaxps-explain-why)");
}

/****************************************************
*   create Explain-PAD                              *
****************************************************/

void
Explain_PAD_create( Widget parent ) {
   Arg args[20];
   Widget menuBarW, formW, fixedW, buttonFormW, closeW, eraseW, hideW;
   Widget rowcolW, W, toggleW, panedW;
   Widget explain_menu, clear_button, explain_menu_button, on_menu_button;
   Widget help_menu_button, help_menu, clear_menu, on_clear_button;
   Widget How_button, HowAll_button, InspectTerms_button, WhyGoal_button;
   Widget inspect_menu, inspect_menu_button, PrintRule_button;
   Widget ShowFactsAll_button, ShowFactsTrue_button, WhyNot_button;
   Widget ShowFactsUnprovable_button;
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
   shellW = XtAppCreateShell( "Explain", "HereClassIsIgnored",
                            /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( shellW, WM_DELETE_WINDOW,
      (XtPointer)Explain_PAD_hide, NULL );        /* Pop down ... */

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
      (XtPointer)HELP_show, (XtPointer)HELP_EXPLAIN_MENU );

   /******************   MenuBar-Widget   ******************/
   menuBarW =  XmVaCreateSimpleMenuBar( formW, "menuBar",
              XmNtopAttachment,      XmATTACH_FORM,
              XmNleftAttachment,     XmATTACH_FORM,
              XmNrightAttachment,    XmATTACH_FORM,
              NULL);
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
                (XtPointer)HELP_show, (XtPointer)HELP_EXPLAIN_MENU );

   XtVaSetValues( menuBarW, XmNmenuHelpWidget, help_menu_button, NULL );

   /*************************************************************/
   /******************   Menu-Explain-Widget   ******************/
   explain_menu   = XmCreateSimplePulldownMenu( menuBarW, "explain_menu", NULL, 0 );
   explain_menu_button = XtVaCreateManagedWidget( "explain",
                       xmCascadeButtonGadgetClass, menuBarW,
                       XmNsubMenuId,     explain_menu,
                       NULL );
  
   /******************   Clear-Button   ******************/
   clear_button = XtVaCreateManagedWidget( "clear_button",
                       xmPushButtonGadgetClass, explain_menu,
                       NULL );
   XtAddCallback(clear_button, XmNactivateCallback,
             (XtPointer)eraseCB, NULL );
  
   /******************   Separator   ******************/
   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, explain_menu, NULL );

   /******************   Inspect-Terms-Button   *********/
   InspectTerms_button = XtVaCreateManagedWidget( "Inspect_Terms",
                  xmPushButtonGadgetClass, explain_menu,
                  NULL );
   XtAddCallback(InspectTerms_button, XmNactivateCallback,
                (XtPointer)InspectTermsCB, NULL );

   /******************   How-Button   *********/
   How_button = XtVaCreateManagedWidget( "How",
                  xmPushButtonGadgetClass, explain_menu,
                  NULL );
   XtAddCallback(How_button, XmNactivateCallback,
                (XtPointer)HowCB, NULL );

   /******************   How-All-Button   *********/
   HowAll_button = XtVaCreateManagedWidget( "How-All",
                  xmPushButtonGadgetClass, explain_menu,
                  NULL );
   XtAddCallback(HowAll_button, XmNactivateCallback,
                (XtPointer)HowAllCB, NULL );

   /******************   Why-Not-Button   *********/
   WhyNot_button = XtVaCreateManagedWidget( "Why-Not",
                  xmPushButtonGadgetClass, explain_menu,
                  NULL );
   XtAddCallback(WhyNot_button, XmNactivateCallback,
                (XtPointer)WhyNotCB, NULL );

   /******************   Separator   ******************/
   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, explain_menu, NULL );

   /******************   Why-goal-Button   *********/
   WhyGoal_button = XtVaCreateManagedWidget( "Why",
                  xmPushButtonGadgetClass, explain_menu,
                  NULL );
   XtAddCallback(WhyGoal_button, XmNactivateCallback,
                (XtPointer)WhyGoalCB, NULL );

   /*************************************************************/
   /******************  Inspect ***************************/
   inspect_menu   = XmCreateSimplePulldownMenu( menuBarW,
                       "inspect_menu", NULL, 0 );

   /**************   Menu-Inspect-Widget   ****************/
   inspect_menu_button = XtVaCreateManagedWidget( "inspect",
                       xmCascadeButtonGadgetClass, menuBarW,
                       XmNsubMenuId,     inspect_menu,
                       NULL );

   /******************   Show-Facts-True-Button   *********/
   ShowFactsTrue_button = XtVaCreateManagedWidget( "Show-Facts-True",
                  xmPushButtonGadgetClass, inspect_menu,
                  NULL );
   XtAddCallback(ShowFactsTrue_button, XmNactivateCallback,
                (XtPointer)ShowFactsTrueCB, NULL );

   /******************   Show-Fact-Unprovable-Button   *********/
   ShowFactsUnprovable_button = XtVaCreateManagedWidget( "Show-Facts-Unprovable",
                  xmPushButtonGadgetClass, inspect_menu,
                  NULL );
   XtAddCallback(ShowFactsUnprovable_button, XmNactivateCallback,
                (XtPointer)ShowFactsUnprovableCB, NULL );

   /******************   Show-Facts-ALL-Button   *********/
   ShowFactsAll_button = XtVaCreateManagedWidget( "Show-Facts-All",
                  xmPushButtonGadgetClass, inspect_menu,
                  NULL );
   XtAddCallback(ShowFactsAll_button, XmNactivateCallback,
                (XtPointer)ShowFactsAllCB, NULL );

   /******************   Separator   ******************/
   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, inspect_menu, NULL );

   /******************   Print-Rule-Button   *********/
   PrintRule_button = XtVaCreateManagedWidget( "Print_Rule",
                  xmPushButtonGadgetClass, inspect_menu,
                  NULL );

   XtAddCallback(PrintRule_button, XmNactivateCallback,
                (XtPointer)PrintRuleCB, NULL );

   /*************************************************************/
   /*** Output-Text-Window *****/
   n = 0;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,        XmSTATIC ); n++;
   XtSetArg( args[n], XmNeditable,                         False ); n++;
   XtSetArg( args[n], XmNeditMode,             XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNcursorPositionVisible,            False ); n++;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     menuBarW ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,         XmATTACH_FORM ); n++;
   outputW = XmCreateScrolledText( formW, "Output", args, n );
   XtManageChild( outputW );
}
   
/****************************************************
*   Change Sensitivity of the Explain PAD           *
*   (accessable from outside...)                    *
****************************************************/

void
Explain_PAD_ready( void ) {

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
Explain_PAD_busy( void ) {

   if( XtIsRealized( shellW ) ) {
      XDefineCursor(
         XtDisplay( shellW ),
         XtWindow( shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the EXPLAIN - PAD             *
****************************************************/

void
Explain_PAD_show( void ) {

   PopupRaiseAndDeiconify( shellW );
}

/***************************************************/

void
Explain_PAD_hide( void ) {

   XtPopdown( shellW );
}

/*** EOF *******************************************/

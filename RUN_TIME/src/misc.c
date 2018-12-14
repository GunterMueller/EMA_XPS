/****************************************************
*   misc.c        Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the misc-editor.             *
*   Full version only.                              *
****************************************************/

#include <stdio.h>

#include <Xm/CascadeBG.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h> 
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>

#include "main.h" 
#include "help.h" 
#include "xinit.h"

/*** Globals ***************************************/

static Widget Misc_shellW;            /* Misc-Editor-Shell */
static Widget exclamation_SignW;
static Widget undo_buttonW;
static Widget save_buttonW;
static Widget kb_selectionW;
static Widget kb_buttonW;
static Widget kb_selection_msgW;
static Widget kb_selection_warW;
static Widget cons_textW;
static Widget save_warW;
static Widget cho_kb_warW;;
static Pixmap change_pixmap0, change_pixmap1;
static int    unsaved_changes;
static int    newTask = 0;       /* aktuelle Task eine neue Task ?? */
static int    excl_enable = 1;
static char   *selected_kb;
static XmString   *unnamed_str;       /* XmString to be drawn if nothing is
                                         chosen */

static void misc_reset( void );
static void editor_reset( void );
/*** Functions *************************************/

static void
Change_True ( void ) {

   if( excl_enable ) {
      XtVaSetValues( exclamation_SignW,
         XmNlabelPixmap, change_pixmap1,
         XmNlabelInsensitivePixmap, change_pixmap1, NULL );

      C_set_modified( selected_kb, MOD_MISC, True );
      unsaved_changes = 1;
      if( !newTask )
         XtSetSensitive( undo_buttonW, True );
      XtSetSensitive( save_buttonW, True );
   }
}
 
static void
Change_False( void ) {

   XtVaSetValues( exclamation_SignW,
      XmNlabelPixmap, change_pixmap0,
      XmNlabelInsensitivePixmap, change_pixmap0, NULL );
   if( selected_kb ) 
      C_set_modified( selected_kb, MOD_MISC, False );
   unsaved_changes = 0; 
   XtSetSensitive( undo_buttonW, False );
   XtSetSensitive( save_buttonW, False );
}

static void
unmanageCB( Widget w, XtPointer client_data, XtPointer call_data ) {

   XtUnmanageChild( (Widget)client_data );
}

/******************************************************************************
*   Useful for Ok-Callback of SelectionMenu                                   *
******************************************************************************/

static char*
selectionMenuOk( XmSelectionBoxCallbackStruct *cbs ) {
   char *str, *selection, *p;
   int length, spaces;
  
   if( XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &selection ) ) {
      p = selection;
      while( *p == ' ' ) p++;
      length = strlen( p );
      if( length ) { 
         str = (char *)XtMalloc( 1 + length );
         strcpy( str, p );
         XtFree( selection );
         selection = str; 
      }
      else
         return NULL;
      /*** Fehlerabbruch bei Namen mit Spaces ***/         
      if( strstr( selection, " " ) ) 
         return NULL;
      return selection;
   }
   return NULL;
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
MISC_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) ) {
         misc_reset();
         Change_False;
         XtFree( selected_kb );
         selected_kb = NULL;   
         XtVaSetValues( kb_buttonW, XmNlabelString, unnamed_str, NULL );
      }   
}               

/*******************************************************************************
*   Change Editability of OptionMenus and ScrolledTexts                        *
*******************************************************************************/

static void
misc_editable( Boolean ability ) {

   XtVaSetValues( cons_textW, XmNeditable, ability, NULL );
}

/******************************************************************************/
/*          Reset Misc-Editor                                                 */
/******************************************************************************/

static void
misc_reset( void ) {

   editor_reset();
}

static void
editor_reset( void ) {

   excl_enable = 0;
   XmTextSetString( cons_textW, "" );
   excl_enable = 1;
      
}

/*******************************************************************************
*   Callback of MenuEntry "Save"                                               *                                             *
*******************************************************************************/
   
static void
save_misc( void ) {
   char *str, *cons_text;

   XtUnmanageChild( save_warW );

   cons_text = XmTextGetString( cons_textW );
   
   str = (char *)XtMalloc( 64 + strlen( cons_text )
                              + strlen( selected_kb ));

   sprintf( str, "(emaxps-misc-save '%s '(\n%s\n))", selected_kb, cons_text );
   
   LISP_sndCmd( str );
   
   Change_False();

   XtFree( str );
   XtFree( cons_text );
}

static void
saveCB( void ) {

   if( unsaved_changes ) {
      XtManageChild( save_warW );
      return;
   }         
   save_misc();
}   

/*******************************************************************************
*   Callback of MenuEntry "Undo"                                              *
*******************************************************************************/

static void
undoCB( void ) {
   char *str; 
 
   str = (char *)XtMalloc( 64 + strlen( selected_kb ));
   
   sprintf( str, "(emaxps-misc-get '%s)", selected_kb );
 
   LISP_sndCmd( str );
 
   XtFree( str );
   
   Change_False();
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button                                           *
*******************************************************************************/

static void
chooseKB( void ) {
   char **kb_list;
   XmStringTable xmkb_list;
   int i, countKB;

   XtUnmanageChild( kb_selection_warW );

   kb_list = KNOWNKBS_getList();
   i = 1;
   while( kb_list[i++] );
   countKB = i - 2;
   
   xmkb_list = (XmString *)XtMalloc( (1 + countKB ) * sizeof(XmString) );
   for ( i=1; i<=countKB; i++ ) {
      xmkb_list[i-1] = XmStringCreateSimple( kb_list[i] );
      XtFree( kb_list[i] );
   }
   
   xmkb_list[i-1] = NULL;
   XtFree( (char*)kb_list );
   
   XtVaSetValues( kb_selectionW,
      XmNlistItems,                xmkb_list,
      XmNlistItemCount,              countKB,
      NULL );
      
   for( i=1; i<=countKB; i++ ) {
      XmStringFree( xmkb_list[i-1] );
   }   
   XtFree( (char*)xmkb_list );
   
   XtManageChild( kb_selectionW );
}


static void
chooseKBCB( void ) {

   if( unsaved_changes ) { 
      XtManageChild( kb_selection_warW );
      return;
   }
   chooseKB();
}

/*******************************************************************************
*   Callback of Ok-Button in KnowledgeBase-SelectionMenu                       *
*******************************************************************************/

static void
kb_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *str;
   char *selection = NULL;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( cbs )) ) {
      XtManageChild( kb_selection_msgW );
      XtFree( selection );
      return;
   }

   XtUnmanageChild( kb_selectionW );

   if( selected_kb ) {    
      if( !strcmp(selection,selected_kb)) {
          XtFree( selection );
          return;
      }      
   }

   if( selected_kb ) {
      XtFree( selected_kb );  
   }   

   selected_kb = (char *)XtMalloc( strlen( selection ) + 1 );
   strcpy( selected_kb, selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_kb );

   XtVaSetValues( kb_buttonW, XmNlabelString, selected_cstr, NULL );

   XmStringFree( selected_cstr );

   str = (char *)XtMalloc( 64 + strlen( selected_kb ) );
   sprintf( str, "(emaxps-misc-get '%s)", selected_kb );
   LISP_sndCmd( str );
   XtFree( str );
}

/*******************************************************************************
*   reaction on (emaxps-misc-get 'kb)                                         *
*******************************************************************************/

void
emaxps_misc_send_others() {
   char *p, *str, *tmp;

   p = str = MSG_read();   /* impliziter XtMalloc ! */

   if( !*str ) {  
      XtFree( str );
      return;
   }

   str = strchr( str, '(' );
   str++;
       
   tmp = strrchr( str, ')' );
   *tmp = 0;
   
   Change_False();

   excl_enable = 0;
   XmTextSetString( cons_textW, str );
   excl_enable = 1;    

   misc_editable( True );

   XtFree( p ); 
}

/*******************************************************************************
*   Change Visibility of MiscEditor                                            *
*******************************************************************************/

void
MISC_show( void ) {

   PopupRaiseAndDeiconify( Misc_shellW );
}

/******************************************************************************/

void
MISC_hide( void ) {

   XtPopdown( Misc_shellW );
} 

/*******************************************************************************
*   Change Cursor of MiscEditor                                                *
*******************************************************************************/

void
MISC_ready( void ) {

   if( XtIsRealized( Misc_shellW ) )
      XDefineCursor( XtDisplay( Misc_shellW ),
                     XtWindow( Misc_shellW ),
                     cursor_normal );
}

/******************************************************************************/

void
MISC_busy( void ) {

   if( XtIsRealized( Misc_shellW ) )
      XDefineCursor( XtDisplay( Misc_shellW ),
                     XtWindow( Misc_shellW ),
                     cursor_busy );
}

/*** create-functions ********************************************************/

/******************************************************************************
*   create SelectionMenu
******************************************************************************/


static Widget
createSelectionMenu( Widget parent, char* name ) {
   Cardinal n;
   Arg args[10];
   Widget w, selectionBoxW;

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNautoUnmanage, False ); n++;
   selectionBoxW = XmCreateSelectionDialog( parent, name, args, n );

   w = XmSelectionBoxGetChild( selectionBoxW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( selectionBoxW, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( w );

   XtAddCallback( selectionBoxW, XmNcancelCallback,
                (XtPointer)unmanageCB, (XtPointer)selectionBoxW );
                
   return selectionBoxW;
}  

/******************************************************************************
*   create simple Message-Box with Ok-Button                                  *
******************************************************************************/

static Widget
createMessageBox( Widget parent, char* name ) {
   Cardinal n;                       
   Arg args[10];                     
   Widget w, messageBoxW;

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   messageBoxW = XmCreateMessageDialog( parent, name, args, n );

   w = XmMessageBoxGetChild ( messageBoxW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( w );
   w = XmMessageBoxGetChild ( messageBoxW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   XtAddCallback( messageBoxW, XmNokCallback,
      (XtPointer)unmanageCB, (XtPointer)messageBoxW );

   return messageBoxW;
} 

/******************************************************************************
*   create Warning-Box with Ok-Button and Cancel-Button                       *
******************************************************************************/

static Widget
createWarningBox( Widget parent, char* name ) {
   Cardinal n;                       
   Arg args[10];                     
   Widget w, warningBoxW;

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   warningBoxW = XmCreateWarningDialog( parent, name, args, n );
                  
   w = XmMessageBoxGetChild ( warningBoxW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   XtAddCallback( warningBoxW, XmNcancelCallback,
      (XtPointer)unmanageCB, (XtPointer)warningBoxW );

   return warningBoxW;
} 

/*******************************************************************************
*   create Misc-Editor                                                         *
*******************************************************************************/

void
MISC_create( Widget parent ) {
   Widget form0, form1, form2,  menuBar, edit_menu, edit_menu_buttonW,
   help_menu, sep, help_menu_buttonW, on_menu_buttonW,
   kb_label, kb_sel_formW, w, cons_label ;
   int n;
   Arg args[10];
   
      /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconPixmap,                  iconPm ); n++;

   Misc_shellW = XtAppCreateShell( "Misc-Editor", "HereClassIsIgnored",
      topLevelShellWidgetClass, XtDisplay( parent ), args, n );

   XmAddWMProtocolCallback( Misc_shellW, WM_DELETE_WINDOW,
                          (XtPointer)MISC_hide, NULL );

   form0 = XtVaCreateManagedWidget( "form0",
       xmFormWidgetClass, Misc_shellW,
       XmNtopAttachment,       XmATTACH_FORM,
       XmNleftAttachment,      XmATTACH_FORM,
       XmNrightAttachment,     XmATTACH_FORM,
       XmNbottomAttachment,    XmATTACH_FORM,
       NULL );

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( form0, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MISC_MENU );

   /***** Form-Widget for Menu ,KB- and Frame-Label and exclamation_Sign-
     Bitmap *****/
   form1 = XtVaCreateManagedWidget( "form1",
       xmFormWidgetClass, form0,
       XmNtopAttachment,       XmATTACH_FORM,
       XmNleftAttachment,      XmATTACH_FORM,
       XmNrightAttachment,     XmATTACH_FORM,
       NULL );
              
      /***** MenuBar-Widget *****/
   menuBar = XmVaCreateSimpleMenuBar( form1, "menuBar", 
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      NULL );
   XtManageChild( menuBar );

      /***** Edit-Menu *****/
   edit_menu = XmCreateSimplePulldownMenu( menuBar, "edit_menu", NULL, 0 );
   edit_menu_buttonW = XtVaCreateManagedWidget( "edit_menu_button",
      xmCascadeButtonGadgetClass, menuBar,
      XmNsubMenuId,     edit_menu,
      NULL );
   
      /***** Save-Button *****/
   save_buttonW = XtVaCreateManagedWidget( "save_button",
      xmPushButtonGadgetClass, edit_menu, NULL );

   XtSetSensitive( save_buttonW, False );

   XtAddCallback( save_buttonW, XmNactivateCallback,
      (XtPointer)saveCB, NULL );
      
     /***** Undo-Button *****/
   undo_buttonW = XtVaCreateManagedWidget( "undo_button",
      xmPushButtonGadgetClass, edit_menu, NULL );

   XtSetSensitive( undo_buttonW, False );

   XtAddCallback( undo_buttonW, XmNactivateCallback,
      (XtPointer)undoCB, NULL );

      /***** Help-Menu ****/
   help_menu = XmCreateSimplePulldownMenu( menuBar, "help_menu", NULL, 0 );
   help_menu_buttonW = XtVaCreateManagedWidget( "help_menu_button",
      xmCascadeButtonGadgetClass, menuBar,
      XmNsubMenuId,     help_menu,
      NULL );
   
   XtVaSetValues( menuBar, XmNmenuHelpWidget, help_menu_buttonW, NULL );
   on_menu_buttonW = XtVaCreateManagedWidget( "on_menu_button",
      xmPushButtonGadgetClass, help_menu, NULL );
   XtAddCallback( on_menu_buttonW, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_MISC_MENU );

   w = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, help_menu, NULL );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   /*****  exclamation_Sign-Widget *****/
   exclamation_SignW = XtVaCreateWidget( "exclamation_SignW",
      xmLabelWidgetClass, form1,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      XmNlabelType,                 XmPIXMAP,
      NULL );
   XtManageChild ( exclamation_SignW );

   /* after managing !!! */
   PIXMAP_createChanged( exclamation_SignW,
      &change_pixmap0, &change_pixmap1 );

   /***********   KnowledgeBase-Label-Widget   *************/  
   kb_label = XtVaCreateManagedWidget( "kb_label",
      xmLabelWidgetClass, form1,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNrecomputeSize,                 True,
      XmNresizeWidth,                   True,
      XmNmarginLeft,                       3,
      NULL );
   
   /**********   KnowledgeBase-PushButton-Widget   *********/
   kb_buttonW = XtVaCreateManagedWidget( "kb_button",
      xmPushButtonWidgetClass, form1,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNleftAttachment,     XmATTACH_WIDGET,
      XmNleftWidget,                kb_label,
      XmNrightAttachment,    XmATTACH_WIDGET,
      XmNrightWidget,      exclamation_SignW,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                       3,
      XmNrecomputeSize,                 True,
      XmNresizeWidth,                   True,
      NULL );

   XtAddCallback( kb_buttonW, XmNactivateCallback,
         (XtPointer)chooseKBCB, NULL );

   XtVaGetValues( kb_buttonW, XmNlabelString, &unnamed_str, NULL );

   /*****************   Separator-Widget   *****************/
   sep = XtVaCreateManagedWidget( "sep",
      xmSeparatorGadgetClass, form0,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                    form1,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      NULL );

   /***** form-Widget fuer das scrolledTextWidget *****/
   form2 = XtVaCreateManagedWidget( "form2",
      xmFormWidgetClass, form0,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                      sep,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      NULL );


   /*************   cons-Label-Widget   *************/
   cons_label = XtVaCreateManagedWidget( "cons_label",
      xmLabelWidgetClass, form2,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNmarginLeft,                       3,
      XmNalignment,            XmALIGNMENT_BEGINNING,
      NULL );

   /*************   cons-ScrolledText-Widget   *************/
   n = 0;
   XtSetArg( args[n], XmNeditMode,       XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,             cons_label ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,   XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNpaneMinimum,                    5 ); n++;
   cons_textW = XmCreateScrolledText( form2, "cons_textWin", args, n );
   XtManageChild( cons_textW );
   XtAddCallback( cons_textW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );

   XtAddCallback( cons_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );

   XtVaSetValues( cons_textW, XmNeditable, False, NULL );

/******************************************************************************
*   Here all Selection-Boxes                                                  *
******************************************************************************/
   /********   KnowledgeBase-SelectionMenu-Widget   ********/
   kb_selectionW = createSelectionMenu( form0, "kb_selection" );

   /***** ungenutztes Eingabe-Label unmanagen *****/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );

   /***** ungenutztes Eingabe-Scrolled-Widget unmanagen *****/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

   /*   install callbacks for Selection-Buttons */
   XtAddCallback( kb_selectionW, XmNokCallback,
      (XtPointer)kb_selectionCB, NULL );

/******************************************************************************
*   Here all Message-Boxes                                                    *
******************************************************************************/
   /***** Kb-Selection-Box *****/
   kb_selection_msgW = createMessageBox( form0, "kb_selection_msg" );

/******************************************************************************
*   Here all Warning-Boxes                                                    *
******************************************************************************/
   /***** Task-Chosse-Kb-Warning-Box *****/
   kb_selection_warW = createWarningBox( form0, "kb_selection_war" );

   XtAddCallback( kb_selection_warW, XmNokCallback,
      (XtPointer)chooseKB, NULL );

   /***** Misc-Save-Warning-Box *****/
   save_warW = createWarningBox( form0, "save_war" );

   XtAddCallback( save_warW, XmNokCallback,
      (XtPointer)save_misc, NULL );
}


/*** EOF *******************************************/

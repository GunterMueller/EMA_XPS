/****************************************************
*   task.c        Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the task-editor.             *
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
#include "task.h"

/*** Globals ***************************************/

static Widget Task_shellW;            /* TaskEditor-Shell */
static Widget parent_widget;

static Widget save_buttonW;           /* Save-Button of Edit-Menu */
static Widget del_task_buttonW;       /* DeleteTask-Button of Edit-Menu */
static Widget undo_buttonW;           /* UndoChanges-Button of Edit-Menu */

static Widget exclamation_SignW;      /* UnsavedChanges-BitmapLabel */
static int    unsaved_changes = 0;    /* any edits unsaved? */

static Widget kb_buttonW;             /* KnowledgeBaseButton */
static Widget kb_selectionW;          /* KnowlegdeBase-SelectionMenu */
static Widget kb_selection_msgW;      /* KB-SelectionMenu-MessageBox */
static Widget kb_selection_warW;      /* KB-SelectionMenu-WarningBox */

static Widget task_buttonW;           /* TaskButton */
static Widget task_selectionW;        /* Task-SelectionMenu */
static Widget task_selection_msgW;    /* Task-SelectionMenu-MessageBox */
static Widget task_selection_warW;    /* Task-SelectionMenu-WarningBox */

static Widget args_textW;             /* argument-ScrolledText */
static Widget comm_textW;             /* comment-ScrolledText */
static Widget body_textW;             /* body-ScrolledText */

static Widget del_task_warW;          /* delete-Task-WarningBox */
static Widget save_warW;              /* save-Task-WarningBox */

static Pixmap change_pixmap0, change_pixmap1;

static int    newTask = 0;            /* aktuelle Task eine neue Task ?? */
static int    excl_enable = 1;
static char   *selected_kb;           /* selected KnowledgeBase */
static char   *selected_task;         /* selected Task */
static XmString   *unnamed_str;       /* XmString to be drawn if nothing is
                                         chosen */
static void editor_reset( void ); 
static void save_task( void );  
static void task_reset( void );

/*** Callbacks ********************************************/

static char*
GetNextPartStr( char *str ) {

   while( *str && *str != ETX ) str++;
   if( !*str )
      return( NULL );
   /* ETX found */
   *str = 0;
   return( ++str );
}

static void
Change_True ( void ) {

   if( excl_enable ) {
      XtVaSetValues( exclamation_SignW,
         XmNlabelPixmap, change_pixmap1,
         XmNlabelInsensitivePixmap, change_pixmap1, NULL );
      C_set_modified( selected_kb, MOD_TASK, True ); 
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
      C_set_modified( selected_kb, MOD_TASK, False );
   unsaved_changes = 0; 
   XtSetSensitive( undo_buttonW, False );
   XtSetSensitive( save_buttonW, False );
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
TASK_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) ) {
         task_reset();
         Change_False;
         XtFree( selected_kb );
         selected_kb = NULL;
         XtVaSetValues( kb_buttonW, XmNlabelString, unnamed_str, NULL );              
      }
}   

/*******************************************************************************
*   Delete Task-Choice
*******************************************************************************/

static void
task_editable( Boolean ability ) {

   XtVaSetValues( args_textW, XmNeditable, ability, NULL );
   XtVaSetValues( comm_textW, XmNeditable, ability, NULL );
   XtVaSetValues( body_textW, XmNeditable, ability, NULL );
   XtSetSensitive( del_task_buttonW, ability );
}

/******************************************************************************/
/*          Reset Task-Editor                                                 */
/******************************************************************************/

static void
task_reset( void ) {
   Widget w;

   editor_reset();
   XtSetSensitive( del_task_buttonW, False );
   XtVaSetValues( task_buttonW, XmNlabelString, unnamed_str, NULL );
   XtFree( selected_task ); 
   selected_task = NULL;
            
   w = XmSelectionBoxGetChild( task_selectionW, XmDIALOG_TEXT );
   XmTextSetString( w, ""  );
                              
}
            
static void
editor_reset( void ) {

   excl_enable = 0;
   XmTextSetString( args_textW, "" );
   XmTextSetString( comm_textW, "" );
   XmTextSetString( body_textW, "" );
   excl_enable = 1;
}

/*******************************************************************************
*   Callback of MenuEntry "Save"                                               *                                             *
*******************************************************************************/
                                     
static void
save_task( void ) {
   char *args_text, *comm_text, *body_text, *str;

   XtUnmanageChild( save_warW );

   args_text = XmTextGetString( args_textW );
   comm_text = XmTextGetString( comm_textW );
   body_text = XmTextGetString( body_textW );
   
   str = (char *)XtMalloc( 64 + strlen( selected_kb )
                              + strlen( selected_task )
                              + strlen( args_text )
                              + strlen( comm_text )
                              + strlen( body_text ));

   sprintf( str,
      "(emaxps-task-add-task '%s '%s\n '(LAMBDA (%s)\n \"%s\"\n %s))",
          selected_kb, selected_task, args_text, comm_text, body_text );

   LISP_sndCmd( str );

   Change_False();

   newTask = 0;

   XtFree( str );
   XtFree( args_text );
   XtFree( comm_text );
   XtFree( body_text );
}

 
static void
save_taskCB( void ) {
   
   if( !newTask ) {
      XtManageChild( save_warW );
      return;
   }
   save_task();
}

/*******************************************************************************
*   Callback of MenuEntry "Delete"                                             *                                             *
*******************************************************************************/
 
static void
del_Task( void ) {
   char *str;
   
   XtUnmanageChild( del_task_warW );
   
   if( !newTask ) {
      str = (char *)XtMalloc( 64 + strlen( selected_kb )
                                 + strlen( selected_task ));
      sprintf( str, "(emaxps-task-remove-task '%s '%s)",
                      selected_kb, selected_task );
      LISP_sndCmd( str );
      XtFree( str );
   }
   task_reset();
   Change_False();
   task_editable( False );
}


static void
del_TaskCB( void ) {

   XtManageChild( del_task_warW );
}
	
/*******************************************************************************
*   Callback of MenuEntry "Undo"                                               *                                             *
*******************************************************************************/

static void
undoCB( void ) {
   char *str;
   
   str = (char *)XtMalloc( 64 + strlen(selected_task)
                              + strlen(selected_kb) );

   sprintf( str, "(emaxps-task-get-task-body '%s '%s)",selected_kb,
      selected_task );

   LISP_sndCmd( str );

   XtFree( str );

   Change_False();
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button                                           *
*******************************************************************************/

void
chooseKB( void ) {
   char **kb_list;
   XmStringTable xmkb_list;
   int i, countKB;

   XtUnmanageChild( kb_selection_warW );

   kb_list = KNOWNKBS_getList();
   i = 1;
   while( kb_list[i++] );
   countKB = i - 2;
 
   xmkb_list = (XmString *)XtMalloc( (1 + countKB) * sizeof(XmString) );
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
      
   for ( i=1; i<=countKB; i++ ) {
      XmStringFree( xmkb_list[i-1] );
   }  
   XtFree( (char*)xmkb_list );   

   XtManageChild( kb_selectionW );   
}

static void
chooseKbCB( void ) {

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
   char *selection = NULL;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( cbs )) ) {
      XtManageChild( kb_selection_msgW ); 
      XtFree( selection );
      return;
   }

   XtUnmanageChild( kb_selectionW );

   if( selected_kb ) {
      if( !strcmp( selection, selected_kb )) {  
         XtFree( selection );
         return;
      }      
   }
   
  if( selected_kb )
     XtFree( selected_kb );
             
   selected_kb = (char *)XtMalloc( 1 + strlen( selection ));
   strcpy( selected_kb, selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_kb );
   XtVaSetValues( kb_buttonW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );
  
   task_editable( False );
   task_reset();
        
   Change_False();

   XtSetSensitive( task_buttonW, True );
}

/*******************************************************************************
*   Callback Task-Button                                                       *
*******************************************************************************/

static void
chooseTask( void ) {
   char *str;

   XtUnmanageChild( task_selection_warW );

   str = (char *)XtMalloc( 64 + strlen(selected_kb) );

   sprintf( str, "(emaxps-task-list-tasks '%s)", selected_kb );

   LISP_sndCmd( str );

   XtFree( str );
}


static void
chooseTaskCB( void ) {

   if( unsaved_changes ) {
      XtManageChild( task_selection_warW );
      return;
   }
      
   chooseTask();
}

/*******************************************************************************
*   reaction on (emaxps-task-list-tasks 'kb)                                   *
*******************************************************************************/

void 
emaxps_task_list_tasks ( void ) {
   char *str, *p, *nextstr;
   XmStringTable xmtask_list;
   int length, i, a;
   Widget w;
      
   p = str = MSG_read();   /* impliziter XtMalloc ! */

   if( !*str ) {
      w = XmSelectionBoxGetChild( task_selectionW, XmDIALOG_LIST );
      XmListDeleteAllItems( w );
      XtManageChild( task_selectionW );
      XtFree( str );
      return;
   }
                   
   length = strlen( str );
   xmtask_list = (XmString *)XtMalloc( (2 + length>>1) * sizeof(XmString));
 
   i = 0;
   while( nextstr = GetNextPartStr( str ) ) {
      xmtask_list[i] = XmStringCreateSimple( str );
      i++;
      str = nextstr;
   }
     
   XtVaSetValues( task_selectionW,
      XmNlistItems,              xmtask_list,
      XmNlistItemCount,                    i,
      NULL );                  
  
   for( a=0; a<i ; a++ ) 
      XmStringFree( xmtask_list[a] );
  
   XtFree( (char*)xmtask_list );  
   XtFree( p );

   XtManageChild( task_selectionW );
}

/*******************************************************************************
*   Callback of Ok-Button in Task-SelectionMenu                                *
*******************************************************************************/

static void
task_selection(  Widget w, XtPointer client_data,
                 XmSelectionBoxCallbackStruct *cbs ) {
   char *selection, *str;             
   XmString selected_cstr;
 
   if( !(selection = selectionMenuOk( cbs )) ) {
      XtManageChild( task_selection_msgW );
      return;
   } 
   
   XtUnmanageChild( task_selectionW );
   
   if( selected_task ) {    
      if( !strcmp( selection, selected_task )) {
          XtFree( selection );
          return;
      }      
   }
   
   if( selected_task )
      XtFree( selected_task );
   
   selected_task = (char *)XtMalloc( strlen( selection ) + 1);
   strcpy( selected_task, selection );
   XtFree( selection );
                                                    
   selected_cstr = XmStringCreateSimple( selected_task );
   XtVaSetValues( task_buttonW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );
   
   task_editable( True );

   Change_False();
   
   if( newTask ) {   
      editor_reset();
      return;
   }
                     
   str = (char *)XtMalloc( 64 + strlen(selected_task)
                              + strlen(selected_kb) );
   
   sprintf( str, "(emaxps-task-get-task-body '%s '%s)", selected_kb,
      selected_task );
   
   LISP_sndCmd( str );
   
   XtFree( str );   
}

/*****************************************************************************/
   
static void
task_selectionCB( Widget w, XtPointer client_data,
                      XmSelectionBoxCallbackStruct *cbs ) {

   newTask = False;
   task_selection( w, client_data, cbs );
}


/*****************************************************************************/
   
static void
task_new_selectionCB( Widget w, XtPointer client_data,
                      XmSelectionBoxCallbackStruct *cbs ) {
   newTask = True;
   task_selection( w, client_data, cbs );
}


/*******************************************************************************
*   reaction on (emaxps-task-send-body 'kb 'task)                              *
*******************************************************************************/

void
emaxps_task_send_body ( void ) {
   char *p, *str, *comm_text, *args_text, *body_text, *tmp;
   
   str = MSG_read();   /* impliziter XtMalloc ! */
   if( !*str ) {
      XtFree( str );
      return;
    }
                  
   args_text = str;
   comm_text = GetNextPartStr( args_text );
   body_text = GetNextPartStr( comm_text );
      
   args_text = strchr( args_text, '(' );   
   args_text++;
   tmp = strrchr( args_text, ')' );
   *tmp = 0;
     
   comm_text = strchr( comm_text, '"' );   
   comm_text++;
   tmp = strrchr( comm_text, '"' );
   *tmp = 0;

   body_text = strchr( body_text, '(' );   
   body_text++;
   tmp = strrchr( body_text, ')' );
   *tmp = 0;

   newTask = 0;
   excl_enable = 0;
   XmTextSetString( args_textW, args_text );
   XmTextSetString( comm_textW, comm_text );
   XmTextSetString( body_textW, body_text );
   excl_enable = 1;

   
   XtFree( str );
}

static void
unmanageCB( Widget w, XtPointer client_data, XtPointer call_data ) {

   XtUnmanageChild( (Widget)client_data );
}

/*******************************************************************************
*   Change Visibility of RuleEditor
*******************************************************************************/

void
TASK_show( void ) {

   PopupRaiseAndDeiconify( Task_shellW );
}
   
/******************************************************************************/

void
TASK_hide( void ) {

   XtPopdown( Task_shellW );
}
 
/*******************************************************************************
*   Change Cursor of RuleEditor
*******************************************************************************/

void
TASK_ready( void ) {

   if( XtIsRealized( Task_shellW ) )
      XDefineCursor( XtDisplay( Task_shellW ),
                     XtWindow( Task_shellW ),
                     cursor_normal );
}

/******************************************************************************/

void
TASK_busy( void ) {

   if( XtIsRealized( Task_shellW ) )
      XDefineCursor( XtDisplay( Task_shellW ),
                     XtWindow( Task_shellW ),
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
*   create Task-Editor                                                         *
*******************************************************************************/

void
TASK_create( Widget parent ) {
   Widget form0, form1, form2, args_form, comm_form, body_form,
   menuBar, edit_menu, edit_menu_buttonW,
   help_menu, help_menu_buttonW, on_menu_buttonW, sep,
   RowColW, kb_label, task_label, panedWin, args_label, comm_label, body_label,
   kb_sel_formW, task_sel_formW, w;
   int n;
   Arg args[10];
   char *str;

      /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconPixmap,                  iconPm ); n++;
   
   Task_shellW = XtAppCreateShell( "Task-Editor", "HereClassIsIgnored",
      topLevelShellWidgetClass, XtDisplay( parent ), args, n );
   
   XmAddWMProtocolCallback( Task_shellW, WM_DELETE_WINDOW,
                          (XtPointer)TASK_hide, NULL );
                          
   /***** Form-Widget das das Shell-Widget ausfuellt *****/                          
   form0 = XtVaCreateManagedWidget( "form0",
       xmFormWidgetClass, Task_shellW,
       XmNtopAttachment,       XmATTACH_FORM,
       XmNrightAttachment,     XmATTACH_FORM,
       XmNleftAttachment,      XmATTACH_FORM,
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
      (XtPointer)HELP_show, (XtPointer)HELP_TASK_MENU );

   /***** Form-Widget fuer Menu ,KB- und Task-Label und exclamation_Sign-Bitmap *****/
   form1 = XtVaCreateManagedWidget( "form1",
       xmFormWidgetClass, form0,
       XmNtopAttachment,       XmATTACH_FORM,
       XmNrightAttachment,     XmATTACH_FORM,
       XmNleftAttachment,      XmATTACH_FORM,
       NULL );

     /***** MenuBar-Widget *****/  
   menuBar = XmVaCreateSimpleMenuBar( form1, "menuBar",
       XmNtopAttachment,       XmATTACH_FORM,   
       XmNrightAttachment,     XmATTACH_FORM,
       XmNleftAttachment,      XmATTACH_FORM,
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
   
   XtAddCallback( save_buttonW, XmNactivateCallback,
      (XtPointer)save_taskCB, NULL );
   XtSetSensitive( save_buttonW, False );
      
      /***** Del-Task-Button *****/
   del_task_buttonW = XtVaCreateManagedWidget( "del_task_button",
      xmPushButtonGadgetClass, edit_menu, NULL );

   XtAddCallback( del_task_buttonW, XmNactivateCallback,
      (XtPointer)del_TaskCB, NULL );
   XtSetSensitive( del_task_buttonW, False ); 
     
     /***** Undo-Button *****/
   undo_buttonW = XtVaCreateManagedWidget( "undo_button",
      xmPushButtonGadgetClass, edit_menu, NULL );

   XtAddCallback( undo_buttonW, XmNactivateCallback,
      (XtPointer)undoCB, NULL );
   XtSetSensitive( undo_buttonW, False );
   
      /***** Help-Menu ****/
   help_menu = XmCreateSimplePulldownMenu( menuBar, "help_menu", NULL, 0 );
   help_menu_buttonW = XtVaCreateManagedWidget( "help_menu_button",
      xmCascadeButtonGadgetClass, menuBar,
      XmNsubMenuId,     help_menu,
      NULL );
      
   XtVaSetValues( menuBar,
                  XmNmenuHelpWidget, help_menu_buttonW,
                  NULL );
   
   on_menu_buttonW = XtVaCreateManagedWidget( "on_menu_button",
      xmPushButtonGadgetClass, help_menu, NULL );
   
   XtAddCallback( on_menu_buttonW, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_TASK_MENU );

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
   XtManageChild( exclamation_SignW );

   /* after managing !!! */
   PIXMAP_createChanged( exclamation_SignW,
      &change_pixmap0, &change_pixmap1 );

   /***** RowCol-Widget *****/    
   RowColW = XtVaCreateManagedWidget( "RowColW",
      xmRowColumnWidgetClass, form1,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNrightAttachment,    XmATTACH_WIDGET,
      XmNrightWidget,      exclamation_SignW,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrowColumnType,          XmWORK_AREA,
      XmNresizeWidth,                   True,      
      XmNorientation,             XmVERTICAL,
      XmNrecomputeSize,                 True,
      XmNnumColumns,                       2,
      XmNpacking,              XmPACK_COLUMN,
      NULL );
      
   /***********   KnowledgeBase-Label-Widget   *************/  
   kb_label = XtVaCreateManagedWidget( "kb_label",
      xmLabelWidgetClass, RowColW,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNrecomputeSize,                 True,
      XmNresizeWidth,                   True,
      XmNmarginLeft,                       3,
      NULL );

   /***********   Task-Label-Widget   *************/
   task_label = XtVaCreateManagedWidget( "task_label",
      xmLabelWidgetClass, RowColW,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                 kb_label,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNrecomputeSize,                 True,
      XmNresizeWidth,                   True,
      XmNmarginLeft,                       3,
      NULL );
   
   /**********   KnowledgeBase-PushButton-Widget   *********/
   kb_buttonW = XtVaCreateManagedWidget( "kb_button",
      xmPushButtonWidgetClass, RowColW,
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
      (XtPointer)chooseKbCB, NULL );

   /**********   Task-PushButton-Widget   *********/
   task_buttonW = XtVaCreateManagedWidget( "task_button",
      xmPushButtonWidgetClass, RowColW,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                 kb_label,
      XmNleftAttachment,     XmATTACH_WIDGET,
      XmNleftWidget,              task_label,
      XmNrightAttachment,    XmATTACH_WIDGET,
      XmNrightWidget,      exclamation_SignW,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                       3,
      XmNrecomputeSize,                 True,
      XmNresizeWidth,                   True,
      NULL );
 
   XtAddCallback( task_buttonW, XmNactivateCallback,
      (XtPointer)chooseTaskCB, NULL );
   XtSetSensitive( task_buttonW, False ); 
   XtVaGetValues( task_buttonW, XmNlabelString, &unnamed_str, NULL );
  
   /*****************   Separator-Widget   *****************/
   sep = XtVaCreateManagedWidget( "sep",
      xmSeparatorGadgetClass, form0,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                    form1,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      NULL );

   /***** form-Widget fuer die drei scrolledTextWidgets *****/
   form2 = XtVaCreateManagedWidget( "form2",
      xmFormWidgetClass, form0,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                      sep,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      NULL );
              
   /****************   PanedWindow-Widget   ****************/
   panedWin = XtVaCreateManagedWidget( "panedWin",
      xmPanedWindowWidgetClass, form2,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      NULL );

   /*************   args-Form-Widget   *************/
   args_form = XtVaCreateManagedWidget( "args_form",
      xmFormWidgetClass, panedWin,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNpaneMinimum,                     85,
      NULL );

   /*************   comm-Form-Widget   *************/
   comm_form = XtVaCreateManagedWidget( "comm_form",
      xmFormWidgetClass, panedWin,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                args_form,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNpaneMinimum,                     85,
      NULL );

   /*************   body-form-Widget   *************/
   body_form = XtVaCreateManagedWidget( "body_form",
      xmFormWidgetClass, panedWin,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                comm_form,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      XmNpaneMinimum,                     85,
      NULL );

   /*************   args-Label-Widget   *************/
   args_label = XtVaCreateManagedWidget( "args_label",
      xmLabelWidgetClass, args_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNmarginLeft,                       3,
      XmNalignment,            XmALIGNMENT_BEGINNING,
      NULL );

   /*************   args-ScrolledText-Widget   *************/
   n = 0;
   XtSetArg( args[n], XmNeditMode,       XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,             args_label ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,   XmATTACH_FORM ); n++;
   args_textW = XmCreateScrolledText( args_form, "args_textWin", args, n );
   XtManageChild( args_textW );
   XtAddCallback( args_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );          
   XtVaSetValues( args_textW, XmNeditable, False, NULL );

   /*************   command-Label-Widget   *************/
   comm_label = XtVaCreateManagedWidget( "comm_label",
      xmLabelWidgetClass, comm_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNmarginLeft,                       3,
      XmNalignment,            XmALIGNMENT_BEGINNING,
      NULL );

   /*************   command-ScrolledText-Widget   *************/
   n = 0;
   XtSetArg( args[n], XmNeditMode,       XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,             comm_label ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,   XmATTACH_FORM ); n++;
   comm_textW = XmCreateScrolledText( comm_form, "comm_textWin", args, n );
   XtManageChild( comm_textW );
   XtAddCallback( comm_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );  
   XtVaSetValues( comm_textW, XmNeditable, False, NULL ); 
   
   /*************   body-Label-Widget   *************/
   body_label = XtVaCreateManagedWidget( "body_label",
      xmLabelWidgetClass,          body_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNmarginLeft,                       3,
      XmNalignment,            XmALIGNMENT_BEGINNING,
      NULL );

   /*************   body-ScrolledText-Widget   *************/
   n = 0;
   XtSetArg( args[n], XmNeditMode,       XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,             body_label ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,   XmATTACH_FORM ); n++;
   body_textW = XmCreateScrolledText( body_form, "body_textWin", args, n );
   XtAddCallback( body_textW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtManageChild( body_textW );
   XtAddCallback( body_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );        
   XtVaSetValues( body_textW, XmNeditable, False, NULL ); 

/******************************************************************************
*   Here all Selection-Boxes                                                  *
******************************************************************************/
   /********   KnowledgeBase-SelectionMenu-Widget   ********/

   kb_selectionW = createSelectionMenu( Task_shellW, "kb_selection" );

   /***** ungenutztes Eingabe-Label unmanagen *****/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );

   /***** ungenutztes Eingabe-Scrolled-Widget unmanagen *****/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

   /*   install callbacks for Selection-Buttons */
   XtAddCallback( kb_selectionW, XmNokCallback,
      (XtPointer)kb_selectionCB, NULL );

   /********   Task-SelectionMenu-Widget   ********/

   task_selectionW = createSelectionMenu( form0, "task_selection" );

   XtVaSetValues( task_selectionW, XmNmustMatch, True, NULL );

   /*   install callbacks for Selection-Buttons */
   XtAddCallback( task_selectionW, XmNokCallback,
      (XtPointer)task_selectionCB, NULL );
   XtAddCallback( task_selectionW, XmNnoMatchCallback,
      (XtPointer)task_new_selectionCB, NULL );
   w = XmSelectionBoxGetChild( task_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

/******************************************************************************
*   Here all Message-Boxes                                                    *
******************************************************************************/
   /***** Kb-Selection-Box *****/
   kb_selection_msgW = createMessageBox( form0,
      "kb_selection_msg" );

   /***** Task-Selection-Box *****/
   task_selection_msgW = createMessageBox( form0,
      "task_selection_msg" );

/******************************************************************************
*   Here all Warning-Boxes                                                    *
******************************************************************************/
   /***** Task-Delete-Warning-Box *****/
   del_task_warW = createWarningBox( form0, "del_task_war" );

   XtAddCallback( del_task_warW, XmNokCallback,
      (XtPointer)del_Task, NULL );

   /***** Task-Save-Warning-Box *****/
   save_warW = createWarningBox( form0, "save_war" );

   XtAddCallback( save_warW, XmNokCallback,
      (XtPointer)save_task, NULL );

   /***** Task-Choose-Task-Warning-Box *****/
   task_selection_warW = createWarningBox( form0, "task_selection_war" );

   XtAddCallback( task_selection_warW, XmNokCallback,
      (XtPointer)chooseTask, NULL );

   /***** Task-Chosse-Kb-Warning-Box *****/
   kb_selection_warW = createWarningBox( form0, "kb_selection_war" );

   XtAddCallback( kb_selection_warW, XmNokCallback,
      (XtPointer)chooseKB, NULL );
}


/*** EOF *******************************************/

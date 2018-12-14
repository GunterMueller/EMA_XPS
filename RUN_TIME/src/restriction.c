/****************************************************
*   restriction.c    Hans Groschwitz     19.07.95   *
*                    Karsten Vossberg               *
*                    Klaus Senf                     *
*                                                   *
*   Functions to build the restriction-editor.      *
*   Full version only.                              *
****************************************************/

/*
   General All-in-One Comment by Vossi (c) '95:

   Not too much comments by ME!
   It was a hard piece of work to build this code,
   why should it be easy for others to understand it ?!?
   Furthermore: it keeps the source SHORT (remember compile time)!
*/

#include <stdio.h>

#include <Xm/Form.h> 
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>               /* MenuBar */
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/Protocols.h>
#include <Xm/PushB.h>
#include <Xm/SeparatoG.h>

#include "uiw.h"
#include "help.h"
#include "main.h"
#include "restriction.h"
#include "xinit.h"

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

/*** Globals for Restriction-Editor ***************************************/

static Widget r_shellW;
static char *r_selected_kb=NULL;          /* selected KnowledgeBase (RE) */
static char *r_selected_re=NULL;          /* selected restriction (RE)  */
static Widget r_nameW, r_restrictionW;
static Pixmap r_change_pixmap0, r_change_pixmap1;
static int  r_editable;                   /* are Texts and OptionMenus editable? */
static int  r_unsaved_changes;            /* any edits unsaved? */
static Widget r_unsaved_changes_labelW,
       r_kbnameW, r_kb_selectionW,
       r_kb_selection_msgW,
       r_re_selectionW,
       r_re_selection_msgW, r_LadenW, 
       r_LoeschenW,r_DelWarningW, 
       r_LoadWarningW, r_ReChangeW,
       r_AllRedefineW, r_RedefineW;  

extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;
static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */

static void r_Reset( void );

/***************************************************/
/*    Get next part of a string sent by LISP       */
/***************************************************/

static char* 
GetNextPartStr( char *str ) {
   while( *str && *str != ETX ) 
     str++;
   if( !*str ) 
      return( NULL );
   /* ETX found */
   *str = 0;
   return( ++str );
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
RESTRICTION_kbClosed( char *kb ) {

   if( r_selected_kb )
      if( !strcmp( kb, r_selected_kb ) )
         r_Reset();
}   

/******************************************************************************
*   create simple Message-Box with Ok-Button
******************************************************************************/

static void
unmanageCB( Widget w, XtPointer client_data, XtPointer call_data ) {
   XtUnmanageChild( (Widget)client_data );
}

static Widget
createMessageBox( Widget parent, char* name ) {
   Cardinal n;                       
   Arg args[10];                     
   Widget w, formW, messageBoxW;

   /* DialogShell mit Form-Widget als Kind erzeugen */
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNiconPixmap,                           iconPm ); n++;
   formW = XmCreateFormDialog( parent, name, args, n );
   XtVaSetValues( XtParent(formW), XmNdeleteResponse, XmDO_NOTHING, NULL );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( XtParent(formW), WM_DELETE_WINDOW,
      (XtPointer)unmanageCB, (XtPointer)formW );

   n = 0;  
   XtSetArg( args[n], XmNleftAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,       XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,      XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNmessageAlignment, XmALIGNMENT_CENTER ); n++;
   XtSetArg( args[n], XmNdialogType,           XmDIALOG_ERROR ); n++;
   messageBoxW = XmCreateMessageBox( formW, "MessageBox", args, n );
   XtManageChild( messageBoxW );                     

   XtAddCallback( messageBoxW, XmNokCallback,
      (XtPointer)unmanageCB, (XtPointer)formW );

   w = XmMessageBoxGetChild ( messageBoxW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( w );

   w = XmMessageBoxGetChild ( messageBoxW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   return formW;
} 

/******************************************************************************
*   create Warning-Box with Ok-Button and Cancel-Button
******************************************************************************/

static Widget
createWarningBox( Widget parent, char* name ) {
   Cardinal n;                       
   Arg args[10];                     
   Widget w, formW, warningBoxW;

   /* DialogShell mit Form-Widget als Kind erzeugen */
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNiconPixmap,                           iconPm ); n++;
   formW = XmCreateFormDialog( parent, name, args, n );
   XtVaSetValues( XtParent(formW), XmNdeleteResponse, XmDO_NOTHING, NULL );

   n = 0;  
   XtSetArg( args[n], XmNleftAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,       XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,      XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNdialogType,         XmDIALOG_WARNING ); n++;
   warningBoxW = XmCreateMessageBox( formW, "WarningBox", args, n );
   XtManageChild( warningBoxW );                     

   w = XmMessageBoxGetChild ( warningBoxW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   return formW;
} 


/******************************************************************************
*   Useful for Ok-Callback of SelectionMenu 
******************************************************************************/

static char*
selectionMenuOk( Widget messageBoxW, XmSelectionBoxCallbackStruct *cbs ) {
   char *str, *selection = NULL;
   int length, spaces;
  
   if( XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, &selection ) ) {
      length = strlen( selection );
      spaces = strspn( selection, " " );
      if( length - spaces ) {
         if( spaces ) {
            str = (char *)XtMalloc( 1 + length - spaces );
            strcpy( str, selection+spaces );
            XtFree( selection );
            selection = str;
         }
         if( strstr( selection, " " ) ) {
            XtManageChild( messageBoxW );
            return NULL;
         }
         return selection;
      }
   }
   XtManageChild( messageBoxW );
   return NULL;
}

/******************************************************************************
*  Restriction-Editor-Change
******************************************************************************/

static void
r_Change_True( void ) {

   XtVaSetValues( r_unsaved_changes_labelW,
      XmNlabelPixmap, r_change_pixmap1,
      XmNlabelInsensitivePixmap, r_change_pixmap1,
      NULL );
   r_unsaved_changes = True;                    
   XtSetSensitive( r_ReChangeW, True );
   XtSetSensitive( r_LadenW, True );
   if( r_selected_kb )
      C_set_modified( r_selected_kb, MOD_RESTRICTION, True );
}

static
r_Change_False( void ) {

   XtVaSetValues( r_unsaved_changes_labelW,
      XmNlabelPixmap, r_change_pixmap0,
      XmNlabelInsensitivePixmap, r_change_pixmap0,
      NULL );
   r_unsaved_changes = False;                    
   XtSetSensitive( r_ReChangeW, False );
   XtSetSensitive( r_LadenW, False );
   if( r_selected_kb )
      C_set_modified( r_selected_kb, MOD_RESTRICTION, False );
}

/*******************************************************************************
*   Change Editability of R-Editor
*******************************************************************************/

static void 
r_edit( Boolean ability ) {

   XtSetSensitive( r_restrictionW, ability );
   r_editable = ability;
}

/*******************************************************************************
*   Restriction-Auswahl loeschen
*******************************************************************************/

static void
r_NoSelect( void ) {

   XtVaSetValues( r_nameW, XmNlabelString, unnamed_str, NULL );
   r_edit( False );
   r_Change_False();
   if( r_selected_re )
      XtFree( r_selected_re );
   r_selected_re = NULL;

   XmTextSetString( r_restrictionW, "" );
   XmTextShowPosition( r_restrictionW, 0 );  /* Clear windows */
   XtSetSensitive( r_nameW, True );
   XtSetSensitive( r_LadenW, False );        /* Menus insensitive */
   XtSetSensitive( r_LoeschenW, False );
   XtSetSensitive( r_RedefineW, False );
   XtSetSensitive( r_AllRedefineW, False );
   XtSetSensitive( r_ReChangeW, False );
}

/******************************************************************************/
/*          Reset des Restriction-Editors                                     */
/******************************************************************************/

static void
r_Reset( void ) {

   r_NoSelect();
   XtVaSetValues( r_kbnameW, XmNlabelString, unnamed_str, NULL );
   XtSetSensitive( r_nameW, False );
   if( r_selected_kb ) {
      XtFree (r_selected_kb );
      r_selected_kb = NULL;
   }
}

/*****************************************************************************
*   Callback fuer den Menuegruppe "Anzeigen" im Restriktionseditor
******************************************************************************/

void 
restrictionEditorAnzeigenCB( void ) {
   char *buf;

   buf = XtMalloc( 256 );
   sprintf( buf, "($send (symbol-value '%s) \n\
      :hole-restriktions-definition-mit-menue '%s)",
      r_selected_kb, r_selected_kb );
   LISP_sndCmd( buf );
} 

/******************************************************************************
*   Quit-Callback fuer den Restriktionseditor 
******************************************************************************/

void 
restrictionEditorQuitCB( void ) {
                               
  RESTRICTION_hide();
}  

/******************************************************************************
*   Laden-Callback fuer den Restriktionseditor 
******************************************************************************/

void 
restrictionEditorLadenCB( void ) {
   char *buf, *restriction;

   buf = XtMalloc( 256 );
   XtUnmanageChild( r_LoadWarningW );
   restriction = XmTextGetString( r_restrictionW );

   buf = XtMalloc(256+strlen(r_selected_re)+strlen(restriction));
   sprintf( buf, "($send (symbol-value '%s) \n\
      :definiere-restriktion '%s '%s \n\
      '(:restrictions %s))",
      r_selected_kb , r_selected_kb,r_selected_re, restriction );   
   LISP_sndCmd( buf ); 
   XtFree( buf );
   XtFree( restriction );
   r_Change_False();
}  

/******************************************************************************
*   Loeschen-Callback fuer den Restriktionseditor 
******************************************************************************/

void 
restrictionEditorLoeschenCB( void ) {

  XtManageChild(  r_DelWarningW  );
  
}  


/******************************************************************************
*   Callback zur Redefinition einer Restriktion
******************************************************************************/

void 
restrictionEditorRedefineCB( void ) {
   char *buf, *restriction;

   buf = XtMalloc(256+strlen(r_selected_re));
   sprintf( buf, "($send (symbol-value '%s) \n\
      :restriktions-redefinition '%s '%s)",
      r_selected_kb, r_selected_kb, r_selected_re );   
   LISP_sndCmd( buf );
   XtFree( buf );
}  

/******************************************************************************
*   Callback zur Redefinition aller Restriktionen
******************************************************************************/

void 
restrictionEditorRedefineAllCB( void ) {
   char *buf, *restriction;

   buf = XtMalloc(256+strlen(r_selected_kb));
   sprintf( buf, 
      "($send (symbol-value '%s) :emaxps-redefine-all '%s )",
      r_selected_kb, r_selected_kb);   
   LISP_sndCmd( buf ); 
   XtFree( buf );                               
}  

/*******************************************************************************
*   Callback of Cancel-Button in LoadWarningBox
*******************************************************************************/

static void
r_LoadWarningcancelCB( void ) {

   XtUnmanageChild( r_LoadWarningW  );
}

/*******************************************************************************
*   Callback of OK-Button in LoadWarningBox
*******************************************************************************/

static void
r_LoadWarningokCB( void ) {

   XtManageChild( r_LoadWarningW );
}

/*******************************************************************************
*   Callback of Rechange-Button  
*******************************************************************************/

static void
r_RechangeCB( void ) {
   char *buf;

   if( r_selected_re ) {
     buf = XtMalloc( 256 + strlen(r_selected_kb) + strlen(r_selected_re) );
     sprintf( buf, "(hole-restriktions-definition '%s '%s)",
        r_selected_kb, r_selected_re );
     LISP_sndCmd( buf );     
     XtFree( buf );
   }
}

/*****************************************************************************
*  
*   Quit-Callback fuer den Restriction-editor 
*
******************************************************************************/

void 
r_QuitCB( void ) {
                               
   XtUnrealizeWidget( r_shellW );  
} 

/*****************************************************************************
*
*   Callback fuer den Menuegruppe "Restriction loeschen .."
*
******************************************************************************/

void 
r_LoeschenCB ( void ) {

  XtManageChild( r_DelWarningW );
}

/*******************************************************************************
*   Callback of Cancel-Button in DeleteWarningBox
*******************************************************************************/

static void
r_DelWarningcancelCB( void ) {

   XtUnmanageChild( XtParent( r_DelWarningW ) );
}

/*******************************************************************************
*   Callback of OK-Button in DeleteWarningBox
*******************************************************************************/

static void
r_DelWarningokCB( void ) {
   char *buf;

   if( r_selected_re ) {
      buf = XtMalloc( 256 + 2*strlen(r_selected_kb) + strlen(r_selected_re) );
      sprintf( buf, "($send (symbol-value '%s) :loesche-restriktion '%s '%s)",
         r_selected_kb, r_selected_kb, r_selected_re );
      LISP_sndCmd( buf );
      XtFree( buf );
   }
   
   /* Delete List-Entry */
   XmTextSetString( XmSelectionBoxGetChild( r_re_selectionW, XmDIALOG_TEXT ) , "" );
   XtUnmanageChild( XtParent( r_DelWarningW ) );
   r_NoSelect();
}

/*******************************************************************************
*   Callback if actual restriction is edited
*******************************************************************************/

static void 
r_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {
   static Widget last_junctor, last_actiontype;

   if( !r_editable )
      return;
   r_Change_True();
}


/*******************************************************************************
*   LISP sends Restrictions for a Selection-Box
*******************************************************************************/

void 
emaxps_restriction_sel( void ) {
   char *str, *s, *s0, *hdl;
   int c, i, len, nr;
   XmStringTable xmstr_list;

   s0 = s = str = MSG_read();
   len = strlen( str );
   xmstr_list = (XmString *)XtMalloc( len+1024 );

   nr = 0;
   while( c = *s++ ) {          /* LISP-Texte holen */
      if( c == ETX ) {
         *(s-1) = NUL;
           nr++;
           xmstr_list[nr-1] = XmStringCreateSimple( s0 );
         s0 = s;
      }
   }
   xmstr_list[nr]=NULL;

   XtVaSetValues( r_re_selectionW,    /* ins Fenster schreiben */
       XmNlistItems, xmstr_list,
       XmNlistItemCount, nr, 
       NULL );
   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   XtManageChild( XtParent(r_re_selectionW) );
}


/*******************************************************************************
*   Callback of Ok-Button in Restriction-SelectionMenu
*******************************************************************************/

static void 
re_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;
							
   if( !(selection = selectionMenuOk( r_re_selection_msgW, cbs )) ) {
      return;
   }

   XtUnmanageChild( XtParent( r_re_selectionW ) );
   LISP_sndReply( selection );
}


/*******************************************************************************
*   Callback of Cancel-Button in Restriction-SelectionMenu
*******************************************************************************/

static void
re_cancelCB( void ) {

   LISP_sndReply( "nil" );
   XtUnmanageChild( XtParent( r_re_selectionW ) );
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button
*******************************************************************************/

static void 
r_chooseKbCB ( void ) {
   char **actual_list;
   XmStringTable xmstr_list;
   int i, countKB;
   
   actual_list = KNOWNKBS_getList();

   /* get number of known KnowledgeBases */
   i = 1;
   while( actual_list[i++] );
   countKB = i - 2;
   
   xmstr_list = (XmString *)XtMalloc( (1+countKB) * sizeof(XmString) );
   for( i=1; i<=countKB; i++ ) {
      xmstr_list[i-1] = XmStringCreateSimple( actual_list[i] );
      XtFree( actual_list[i] );
   }
   xmstr_list[i-1] = NULL;
   XtFree( (char*)actual_list );
   XtVaSetValues( r_kb_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, countKB,
      NULL );
   i = 0;
   while( i < countKB )
      XmStringFree( xmstr_list[i++] );
   XtFree( (char*)xmstr_list );

   XtManageChild( XtParent (r_kb_selectionW));
}

/*******************************************************************************
*   Callback of Ok-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void 
r_kb_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( r_kb_selection_msgW, cbs )) ) {
      return;
   }

   XtUnmanageChild( XtParent( r_kb_selectionW ) );
   
   if ( r_selected_kb){
     if( !strcmp(selection,r_selected_kb)) {
        XtFree( selection );
         return;
     }      
   }  
   
   r_selected_kb = UIW_NewString( selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( r_selected_kb );
   XtVaSetValues( r_kbnameW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   r_edit( False );
   r_NoSelect();
}

/*******************************************************************************
*   Callback of Cancel-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void
r_kb_cancelCB( void ) {

   XtUnmanageChild( XtParent( r_kb_selectionW ) );
}

/*****************************************************************************
******************************************************************************
**                                                                          **
**                Erzeugen des Restriktionseditors                          ** 
**                                                                          **
******************************************************************************
******************************************************************************/

void 
RESTRICTION_create( Widget parent ) {
   Widget  form1W,formW, panedwindowW;
   Widget  frameW, labelW;
   Widget  menubarW;
   Widget  RowColW;
   Widget  formnamelabelW;
   Widget  namelabelW, editorlabelW;
   Widget  kbnamelabelW;
   Widget  w, w1, w2, w3, w4, w5;
   int     n;
   Arg     args[20];
   ArgList argl;        
   /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconPixmap,                  iconPm ); n++;
   
   r_shellW = XtAppCreateShell( "Restriction-Editor", "HereClassIsIgnored",
      topLevelShellWidgetClass, XtDisplay( parent ), args, n );

   XmAddWMProtocolCallback( r_shellW, WM_DELETE_WINDOW,
      (XtPointer)RESTRICTION_hide, NULL );        /* Pop down ... */


   /********** Form-Widget erzeugen ********************************/
   
   n = 0;
   formW = XmCreateForm( r_shellW, "form", args, n );
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
      (XtPointer)HELP_show, (XtPointer)HELP_RESTRICTION_MENU );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_FORM ); n++; 
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++; 
   menubarW = XmCreateMenuBar( formW, "menubar", args, n );
   XtManageChild( menubarW );

                       
   /*** Pulldown-Menuepunkt "Bearbeiten" erzeugen und managen *******/
   w1=XmCreateSimplePulldownMenu( menubarW, "Bearbeiten-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Bearbeiten",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );

   r_LadenW = XtVaCreateManagedWidget( "Laden",
                xmPushButtonGadgetClass, w1,
                NULL );
   XtAddCallback(r_LadenW, XmNactivateCallback,
   (XtPointer)restrictionEditorLadenCB, NULL );

   r_LoeschenW = XtVaCreateManagedWidget( "Loeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(r_LoeschenW, XmNactivateCallback,
                    (XtPointer)restrictionEditorLoeschenCB, NULL );

   r_ReChangeW = XtVaCreateManagedWidget( "ReChange", 
               xmPushButtonGadgetClass, w1,
               NULL );
   XtAddCallback(r_ReChangeW, XmNactivateCallback,
      (XtPointer)r_RechangeCB, NULL );

   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, w1, NULL );

   r_RedefineW = XtVaCreateManagedWidget( "Redefinieren",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(r_RedefineW, XmNactivateCallback,
                    (XtPointer)restrictionEditorRedefineCB, NULL );

   r_AllRedefineW = XtVaCreateManagedWidget( "AlleRedefinieren",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(r_AllRedefineW, XmNactivateCallback,
                    (XtPointer)restrictionEditorRedefineAllCB, NULL );

   /*** Pulldown-Menuepunkt "Hilfe" erzeugen und managen ************/ 
   w1=XmCreateSimplePulldownMenu( menubarW, "Hilfe-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Help",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );
   w3 = XtVaCreateManagedWidget( "on_menu",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtVaSetValues( menubarW, XmNmenuHelpWidget, w2/*help_menu_button*/, NULL );
   XtAddCallback(w3, XmNactivateCallback,
                    (XtPointer)HELP_show, (XtPointer)HELP_RESTRICTION_MENU ) ;

   w = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, w1, NULL );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   XtManageChild( menubarW );
   n = 0;                                          
   
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     menubarW ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   
   form1W = XmCreateForm( formW, "form2", args, n );
   XtManageChild( form1W );

   /***************** unsaved-Label-Widget *****************/

   r_unsaved_changes_labelW = XtVaCreateWidget( "r_unsaved_changes_label",
      xmLabelWidgetClass, form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,    XmATTACH_FORM,
      
      XmNrightAttachment,    XmATTACH_FORM,
      XmNbottomAttachment,   XmATTACH_FORM,
      NULL );
   XtManageChild ( r_unsaved_changes_labelW );

   /* after managing !!! */
   PIXMAP_createChanged( r_unsaved_changes_labelW,
      &r_change_pixmap0, &r_change_pixmap1 );

   r_Change_False();

   RowColW = XtVaCreateManagedWidget( "RowColumn",
             xmRowColumnWidgetClass, form1W,
             XmNrowColumnType, XmWORK_AREA,
             XmNtopAttachment,XmATTACH_FORM,
             XmNleftAttachment,XmATTACH_FORM,
             XmNleftAttachment,           XmATTACH_FORM,
             XmNrightAttachment, XmATTACH_WIDGET,
             XmNrightWidget,  r_unsaved_changes_labelW ,
             XmNresizeWidth,True,      
             XmNorientation,           XmVERTICAL,
             XmNrecomputeSize,              TRUE,
             XmNnumColumns,2,
             XmNpacking, XmPACK_COLUMN,
             NULL );



   n = 0;
    XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
    XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
             

   /******** Label zur Anzeige des KB - Namens ******************/

   kbnamelabelW = XmCreateLabel( RowColW, "kbase", args, n );
   XtManageChild( kbnamelabelW );

   /******** Label zur Anzeige des Restriction -  Namens ******************/

   n = 0;
   namelabelW = XmCreateLabel( RowColW, "Restriction", args, n );
   XtManageChild( namelabelW );

   /******** KB-Name-Knopf ******************/

    r_kbnameW = XtVaCreateManagedWidget( "r_kb_name",
      xmPushButtonWidgetClass, RowColW,
     XmNtopAttachment,    XmATTACH_FORM,
     XmNleftAttachment,   XmATTACH_WIDGET,
     XmNleftWidget,              kbnamelabelW,
     XmNrightAttachment,  XmATTACH_FORM,
     XmNrecomputeSize,              TRUE,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
   XtAddCallback( r_kbnameW, XmNactivateCallback,
       (XtPointer)r_chooseKbCB, NULL );
   XtManageChild ( r_kbnameW );                                  


   /********   KnowledgeBase-SelectionMenu-Widget   dir ********/

    n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog(RowColW, "r_kb_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,                 XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,                XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,              XmATTACH_FORM ); n++;
   r_kb_selectionW = XmCreateSelectionBox( w, "r_kb_selection", args, n ); 
   XtManageChild( r_kb_selectionW );  


   /*** only KnowledgeBases already loaded can be chosen from ***/

   w = XmSelectionBoxGetChild( r_kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   
   
   w = XmSelectionBoxGetChild( r_kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( r_kb_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   /* install callback for the CLOSE-Button of the mwm-border menu */

   XmAddWMProtocolCallback( XtParent( XtParent( r_kb_selectionW ) ),
      WM_DELETE_WINDOW, (XtPointer)r_kb_cancelCB, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( r_kb_selectionW, XmNokCallback,
      (XtPointer)r_kb_selectionCB, NULL );
   XtAddCallback( r_kb_selectionW, XmNcancelCallback,
      (XtPointer)r_kb_cancelCB, NULL );

 
   /*******  KnowledgeBase-SelectionMenu-MessageBox-Widget  ******/

   r_kb_selection_msgW = createMessageBox( r_kb_selectionW,
                                             "r_kb_selection_msg" );



   /******** Restriction-Name-Knopf ******************/

    r_nameW = XtVaCreateManagedWidget( "r_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
    XtAddCallback( r_nameW, XmNactivateCallback,
       (XtPointer)restrictionEditorAnzeigenCB, NULL );
       
   XtManageChild ( r_nameW );                                  
   XtVaGetValues( r_nameW, XmNlabelString, &unnamed_str, NULL );

   /********   Restriction-SelectionMenu-Widget   ********/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( r_nameW, "r_re_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   r_re_selectionW = XmCreateSelectionBox( w, "r_re_selection", args, n ); 
   w = XmSelectionBoxGetChild( r_re_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   w = XmSelectionBoxGetChild( r_re_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtManageChild( r_re_selectionW );  
   
   XtAddCallback( r_re_selectionW, XmNcancelCallback,
     (XtPointer)re_cancelCB, NULL );
   XtAddCallback( r_re_selectionW, XmNokCallback,
     (XtPointer)re_selectionCB, NULL );

  
  /******** Restriction-SelectionMenu-MessageBox-Widget ******************/

   r_re_selection_msgW = createMessageBox( r_re_selectionW,
                                             "r_re_selection_msg" );

  /********** LISP-MessageBox-Widget fuer Delete-Warnung ************/

   r_DelWarningW = createMessageBox( r_re_selectionW,
                                             "r_Delete_Warnung" );
 


  /********** QuestionBox fuer  Delete-Warnung ************/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   r_DelWarningW = XmCreateQuestionDialog(r_shellW, "r_DelWarningW",args,n);


   /***** ungenutzten Help-Button  unmanagen *****/

   w = XmMessageBoxGetChild( r_DelWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( r_DelWarningW, XmNokCallback,
     (XtPointer)r_DelWarningokCB, NULL );
   XtAddCallback( r_DelWarningW, XmNcancelCallback,
      (XtPointer)r_DelWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent( r_DelWarningW ), WM_DELETE_WINDOW,
      (XtPointer)r_DelWarningcancelCB, (XtPointer)r_DelWarningW );




  /********** QuestionBox fuer  Load-Warnung ************/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   r_LoadWarningW = XmCreateQuestionDialog( r_shellW, "r_LoadWarningW",args,n);


   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( r_LoadWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( r_LoadWarningW, XmNokCallback,
     (XtPointer)r_LoadWarningokCB, NULL );
   XtAddCallback( r_LoadWarningW, XmNcancelCallback,
      (XtPointer)r_LoadWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(r_LoadWarningW), WM_DELETE_WINDOW,
      (XtPointer)r_LoadWarningcancelCB, (XtPointer)r_LoadWarningW );
                                                    


   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                           form1W ); n++;
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   w = XmCreateLabel( formW, "Constraint-Anbindungen", args, n );
   XtManageChild( w );

   /*********  Editorfenster *********************************************/    

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                   w ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNwordWrap,                             True ); n++;
   XtSetArg( args[n], XmNeditMode,                XmMULTI_LINE_EDIT ); n++;

   r_restrictionW = XmCreateScrolledText( formW, "EditorWin", args, n );
   XtAddCallback( r_restrictionW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( r_restrictionW, XmNvalueChangedCallback,
     (XtPointer)r_changedCB, NULL );
   
   XtManageChild(r_restrictionW );

   r_Reset();
}            

/******************************************************************************
*   Reads a definition of a restriction
******************************************************************************/

void 
emaxps_restriction_def( void ) {
   char *r_name, *r_body,  *p, *v[6], *str;
   int i, err, ret;
   XmString cstring;

   v[0] = p = MSG_read();
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=GetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "restriction-def: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "restriction-def: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   r_name         = v[i++];
   r_body         = v[i++];
   if (r_selected_re)
     XtFree(r_selected_re);
   r_selected_re = XtMalloc(strlen(r_name));
   strcpy(r_selected_re,r_name);
    
   XtVaSetValues( r_nameW, XmNlabelString, XmStringCreateSimple( r_name ), NULL );
   XmTextSetString( r_restrictionW, r_body  );
  
   r_edit(True);  
   r_Change_False();
   XtFree( p );
   XtSetSensitive( r_LoeschenW, True);     /* Menus sensitive */
   XtSetSensitive( r_RedefineW, True);
   XtSetSensitive( r_AllRedefineW, True); 
}              

/****************************************************
*   Change Sensitivity of the Restriction Editor    *
*   (accessable from outside...)                    *
****************************************************/

void
RESTRICTION_ready( void ) {

   if( XtIsRealized( r_shellW ) ) {
      XDefineCursor(
         XtDisplay( r_shellW ),
         XtWindow( r_shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
RESTRICTION_busy( void ) {

   if( XtIsRealized( r_shellW ) ) {
      XDefineCursor(
         XtDisplay( r_shellW ),
         XtWindow( r_shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the Restriction Editor        *
****************************************************/

void
RESTRICTION_show( void ) {

   PopupRaiseAndDeiconify( r_shellW );
}

/***************************************************/

void
RESTRICTION_hide( void ) {

   XtPopdown( r_shellW );
}

/*** EOF ******************************************/

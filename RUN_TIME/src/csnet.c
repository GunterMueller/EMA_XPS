/****************************************************
*   csnet.c       Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the constraint-net-editor.   *
*   Full version only.                              *
****************************************************/

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

#include "uiw.h"
#include "help.h"
#include "main.h"
#include "csnet.h"
#include "xinit.h"

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

/*** Globals for ConstraintNet-Editor ***************************************/

static Widget c_shellW ;
static Widget c_nameW, c_artW;            /* ConstraintNet-Widget */
static Widget c_interfaceW, c_bodyW, c_namelabelW;
static Widget c_labelbodyW, c_forminterfaceW;
static Widget c_formnamelabelW,
       c_kbnameW, c_kb_selectionW,
       c_kb_selection_msgW,
       c_co_selectionW,
       c_co_selection_msgW, c_LadenW, 
       c_LoeschenW,c_DelWarningW, 
       c_LoadWarningW, c_ReChangeW;

static char *c_selected_kb=NULL;          /* selected KnoeledgeBase  (CE) */
static char *c_selected_co=NULL;          /* selected constraint (CE) */
static Pixmap c_change_pixmap0, c_change_pixmap1;
static int  c_editable;                   /* are Texts and OptionMenus editable? */
static int  c_unsaved_changes;            /* any edits unsaved? */
static Widget c_unsaved_changes_labelW;

extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;
static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */

static void c_Reset( void );

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
CSNET_kbClosed( char *kb ) {

   if( c_selected_kb )
      if( !strcmp( kb, c_selected_kb ) )
         c_Reset();
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
*  Constraint-Editor-Change
******************************************************************************/

static void
c_Change_True( void ) {

   XtVaSetValues( c_unsaved_changes_labelW,
      XmNlabelPixmap, c_change_pixmap1,
      XmNlabelInsensitivePixmap, c_change_pixmap1,
      NULL );
   c_unsaved_changes = True;                    
   XtSetSensitive( c_ReChangeW, True );
   XtSetSensitive( c_LadenW, True );
   if( c_selected_kb )
      C_set_modified( c_selected_kb, MOD_CSNET, True );
}

static
c_Change_False ( void ) {

   XtVaSetValues( c_unsaved_changes_labelW,
      XmNlabelPixmap, c_change_pixmap0,
      XmNlabelInsensitivePixmap, c_change_pixmap0,
      NULL );
   c_unsaved_changes = False;                    
   XtSetSensitive( c_ReChangeW, False );
   XtSetSensitive( c_LadenW, False );
   if( c_selected_kb )
      C_set_modified( c_selected_kb, MOD_CSNET, False );
}

/*******************************************************************************
*   Change Editability of C-Editor
*******************************************************************************/

static void 
c_edit( Boolean ability ) {

   XtSetSensitive( c_interfaceW, ability );
   XtSetSensitive( c_bodyW, ability );
   c_editable = ability;
}

/*******************************************************************************
*   Constraint-Auswahl loeschen
*******************************************************************************/

static void
c_NoSelect( void ) {

   XtVaSetValues( c_nameW, XmNlabelString, unnamed_str, NULL );
   c_edit( False );
   c_Change_False();
   if( c_selected_co )
      XtFree( c_selected_co );
   c_selected_co = NULL;

   XmTextSetString( c_bodyW, "" );
   XmTextShowPosition( c_bodyW, 0 );  /* Clear windows */
   XmTextSetString( c_interfaceW, "" );
   XmTextShowPosition( c_interfaceW, 0 );
   XtSetSensitive( c_nameW,   True );
   XtSetSensitive( c_LadenW, False );        /* Menus insensitive */
   XtSetSensitive( c_LoeschenW, False ); 
   XtSetSensitive( c_ReChangeW, False );
}

/******************************************************************************/
/*          Reset des Constraint-Editors                                      */
/******************************************************************************/

static void
c_Reset( void ) {

   c_NoSelect();
   XtVaSetValues( c_kbnameW, XmNlabelString, unnamed_str, NULL );
   XtSetSensitive( c_nameW, False );
   if( c_selected_kb ) {
      XtFree( c_selected_kb );
      c_selected_kb = NULL;
   }      
}

/******************************************************************************
*  LISP->C:
*  Reads the definition of a Constraint-Net 
*  --> dispatch.c
******************************************************************************/

void 
emaxps_csnet_def( void ) {
   char *c_name, *c_art, *c_interface, *c_body, *c_condition, *p, *v[10], *str;
   int i, err, ret;
   XmString cstring;
   
   v[0] = p = MSG_read();
   for( err = 0, i = 1; i < 5; i++ ) {
      if( !(v[i]=GetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }

   if( err ) {
      fprintf( stderr, "constraint-net_def: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "constraint-net-def: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   c_name          = v[i++];
   c_art           = v[i++];
   c_interface     = v[i++];
   c_body          = v[i++];
   c_condition     = v[i++];
   XmTextSetString( c_interfaceW,  c_interface );
   XmTextSetString( c_bodyW, c_body  );
   if (c_selected_co)
        XtFree( c_selected_co );
   c_selected_co=XtMalloc(strlen(c_name));
   strcpy(c_selected_co,c_name);                         
   c_Change_False();
   c_edit(True);
   XtSetSensitive( c_LoeschenW, True);        /* Menus insensitive */   
   XtVaSetValues( c_nameW, XmNlabelString, XmStringCreateSimple( c_name ), NULL );
   XtFree( p );
}     

/*****************************************************************************
*   Callback zur Anzeige der Definition eines Constraint-Netzes              
*   im Constrainteditor 
*****************************************************************************/

void 
constraintnetEditorAnzeigenCB( void ) {
   char *buf;

   buf = XtMalloc( 256 + strlen(c_selected_kb) + strlen(c_selected_kb) );
   sprintf( buf, "($send (symbol-value '%s) \n\
      :hole-constraintnetz-definition-mit-menue '%s)",
      c_selected_kb, c_selected_kb );
   LISP_sndCmd( buf );     
   XtFree( buf );
}

/*****************************************************************************
*   Callback zur Anzeige der Definition eines Constraint-Netzes
*   im Constrainteditor 
*****************************************************************************/

void 
constraintnetEditorNetzAnzeigenCB( void ) {

   LISP_sndCmd( "(send-kb :hole-constraintnetz-definition-mit-menue)" );
}

/******************************************************************************
*   Laden-Callback fuer den Constrainteditor 
******************************************************************************/

void 
constraintnetEditorLadenCB( void ) {
  
   XtManageChild( c_LoadWarningW );
}  

/******************************************************************************
*   Quit-Callback fuer den Constrainteditor 
******************************************************************************/

void 
constraintnetEditorQuitCB( void ) {
                               
   CSNET_hide();
} 

/*****************************************************************************
*   Callback fuer den Menuegruppe "Constraintnet loeschen .."
*   im Constraintneteditor 
******************************************************************************/

void 
constraintnetLoeschenCB( void ) {

   XtManageChild( c_DelWarningW );
}




/*******************************************************************************
*   Callback of Cancel-Button in LoadWarningBox
*******************************************************************************/

static void
c_LoadWarningcancelCB( void ) {

   XtUnmanageChild( c_LoadWarningW );
}



/*******************************************************************************
*   Callback of OK-Button in LoadWarningBox
*******************************************************************************/

static void
c_LoadWarningokCB( void ) {
   char *buf;          
   char *interface;
   char *body;             
  
   interface = XmTextGetString( c_interfaceW );    
   body      = XmTextGetString( c_bodyW );
   buf=XtMalloc(256+strlen(interface)
                 +strlen(body));
   sprintf(buf, "($send (symbol-value '%s) \n\
      :Definiere-Constraint '%s '%s \n\
      '(:type compound) \n\
      '(:interface %s) \n\
      '(:constraint-expressions %s))",
      c_selected_kb, c_selected_kb, c_selected_co, interface, body ); 
   XtUnmanageChild( c_LoadWarningW );
   c_Change_False();
   LISP_sndCmd( buf );
   XtFree( buf );
   XtFree( interface );
   XtFree( body );
}

/*******************************************************************************
*   Callback of Constraint-Rechange-Button  
*******************************************************************************/

static void
c_RechangeCB( void ) {
   char *buf;
   if( c_selected_co ) {
      buf = XtMalloc( 256 + strlen(c_selected_kb) + strlen(c_selected_co) );
      sprintf( buf, "(hole-constraint-definition '%s '%s)",
         c_selected_kb , c_selected_co );
      LISP_sndCmd( buf );     
      XtFree( buf );
   }
}

/*******************************************************************************
*   Callback of Cancel-Button in Constraint-DeleteWarningBox
*******************************************************************************/

static void
c_DelWarningcancelCB( void ) {

   XtUnmanageChild( XtParent(c_DelWarningW ));
}

/*******************************************************************************
*   Callback of OK-Button in Constraint-DeleteWarningBox
*******************************************************************************/

static void
c_DelWarningokCB( void ) {
   char *buf;

   buf = XtMalloc( 256 + strlen(c_selected_kb) + strlen(c_selected_kb)
                       + strlen(c_selected_co) );
   sprintf( buf, "($send (symbol-value '%s) \n\
      :loesche-constraintnetz '%s '%s)",
         c_selected_kb, c_selected_kb, c_selected_co );
   LISP_sndCmd( buf );
   XtFree( buf );

   /* Delete List-Entry */
   XmTextSetString( XmSelectionBoxGetChild( c_co_selectionW, XmDIALOG_TEXT ) , "" );
   XtUnmanageChild( XtParent( c_DelWarningW ) );
   c_NoSelect();
}

/*******************************************************************************
*   Callback if actual restriction is edited
*******************************************************************************/

static void 
c_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {
   static Widget last_junctor, last_actiontype;

   if( !c_editable )
      return;
   c_Change_True();
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button
*******************************************************************************/

static void 
c_chooseKbCB ( void ) {
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
   XtVaSetValues( c_kb_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, countKB,
      NULL );
   i = 0;
   while( i < countKB )
      XmStringFree( xmstr_list[i++] );
   XtFree( (char*)xmstr_list );

   XtManageChild( XtParent (c_kb_selectionW));
}

/*******************************************************************************
*   Callback of Ok-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void 
c_kb_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( c_kb_selection_msgW, cbs )) ) {
      return;
   }

   XtUnmanageChild( XtParent( c_kb_selectionW ) );
   
   if ( c_selected_kb){
     if( !strcmp(selection,c_selected_kb)) {
        XtFree( selection );
         return;
     }      
   }  
   
   c_selected_kb = UIW_NewString( selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( c_selected_kb );
   XtVaSetValues( c_kbnameW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   c_edit( False );
   c_NoSelect();
}

/*******************************************************************************
*   Callback of Cancel-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void
c_kb_cancelCB( void ) {

   XtUnmanageChild( XtParent( c_kb_selectionW ) );
}

/*******************************************************************************
*   LISP sends Constraintss for a Selection-Box
*******************************************************************************/

void 
emaxps_csnet_sel ( void ) {
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

    XtVaSetValues( c_co_selectionW,    /* ins Fenster schreiben */
       XmNlistItems, xmstr_list,
       XmNlistItemCount, nr, 
       NULL );

   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   
   
   XtManageChild( XtParent(c_co_selectionW) );

}


/*******************************************************************************
*   Callback of Ok-Button in Constraint-SelectionMenu
*******************************************************************************/

static void 
co_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;
							
   if( !(selection = selectionMenuOk( c_co_selection_msgW, cbs )) ) {
      return;
   }

   XtUnmanageChild( XtParent( c_co_selectionW ) );
   LISP_sndReply( selection );
}

/*******************************************************************************
*   Callback of Cancel-Button in Constraint-SelectionMenu
*******************************************************************************/

static void
co_cancelCB( void ) {
   LISP_sndReply( "nil" );
   XtUnmanageChild( XtParent( c_co_selectionW ) );
}

/*****************************************************************************
******************************************************************************
**                                                                          **
**                Erzeugen des Constraintneteditors                         ** 
**                                                                          **
******************************************************************************
******************************************************************************/

void 
CSNET_create( Widget parent ) {
   Widget formW,form1W;
   Widget frameW, labelW,namelabelW, kbnamelabelW;
   Widget menubarW;
   Widget panedwindow;     
   Widget formEd2W;
   Widget labelEd1W, labelEd3W;    
   Widget w,w1, w2, w3;
   Widget RowColW;
   int    n;
   Arg    args[20]; 

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
   c_shellW = XtAppCreateShell( "ConstraintNet-Editor", "HereClassIsIgnored",
      topLevelShellWidgetClass, XtDisplay( parent ), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( c_shellW, WM_DELETE_WINDOW,
      (XtPointer)CSNET_hide, NULL );        /* Pop down ... */

   /******************* Hauptfenster **********************************/
   n = 0;
   formW = XmCreateForm (c_shellW, "Constraintneteditor", args, n);
   XtManageChild (formW);            

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( formW, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_CSNET_MENU );

   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,       XmATTACH_FORM ); n++;
   menubarW = XmCreateMenuBar( formW, "menubar", args, n );
   XtManageChild( menubarW );

   /*** Pulldown-Menuepunkt "Bearbeiten" erzeugen und managen *******/
   w1=XmCreateSimplePulldownMenu( menubarW, "Bearbeiten-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Bearbeiten",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );
   c_LadenW = XtVaCreateManagedWidget( "Laden",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(c_LadenW, XmNactivateCallback,
                    (XtPointer)constraintnetEditorLadenCB, NULL );
   c_LoeschenW = XtVaCreateManagedWidget( "Loeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(c_LoeschenW , XmNactivateCallback,
                    (XtPointer)constraintnetLoeschenCB, NULL );
   c_ReChangeW = XtVaCreateManagedWidget( "ReChange", 
               xmPushButtonGadgetClass, w1,
               NULL );
   XtAddCallback(c_ReChangeW, XmNactivateCallback,
      (XtPointer)c_RechangeCB, NULL );

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
                    (XtPointer)HELP_show, (XtPointer)HELP_CSNET_MENU ) ;

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

   c_unsaved_changes_labelW = XtVaCreateWidget( "c_unsaved_changes_label",
      xmLabelWidgetClass, form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,    XmATTACH_FORM,
      
      XmNrightAttachment,    XmATTACH_FORM,
      XmNbottomAttachment,   XmATTACH_FORM,
      NULL );
   XtManageChild ( c_unsaved_changes_labelW );

   /* after managing !!! */
   PIXMAP_createChanged( c_unsaved_changes_labelW,
      &c_change_pixmap0, &c_change_pixmap1 );

   c_Change_False();

   RowColW = XtVaCreateManagedWidget( "RowColumn",
             xmRowColumnWidgetClass, form1W,
             XmNrowColumnType, XmWORK_AREA,
             XmNtopAttachment,XmATTACH_FORM,
             XmNleftAttachment,XmATTACH_FORM,
             XmNleftAttachment,           XmATTACH_FORM,
             XmNrightAttachment, XmATTACH_WIDGET,
             XmNrightWidget,  c_unsaved_changes_labelW ,
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

   /******** Label zur Anzeige des Constraint -  Namens ******************/

   n = 0;
   namelabelW = XmCreateLabel( RowColW, "Constraint", args, n );
   XtManageChild( namelabelW );

   /******** KB-Name-Knopf ******************/

    c_kbnameW = XtVaCreateManagedWidget( "c_kb_name",
      xmPushButtonWidgetClass, RowColW,
     XmNtopAttachment,    XmATTACH_FORM,
     XmNleftAttachment,   XmATTACH_WIDGET,
     XmNleftWidget,              kbnamelabelW,
     XmNrightAttachment,  XmATTACH_FORM,
     XmNrecomputeSize,              TRUE,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
   XtAddCallback( c_kbnameW, XmNactivateCallback,
       (XtPointer)c_chooseKbCB, NULL );
   XtManageChild ( c_kbnameW );                                  


   /********   KnowledgeBase-SelectionMenu-Widget   ********/

    n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog(RowColW, "c_kb_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,                 XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,                XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,              XmATTACH_FORM ); n++;
   c_kb_selectionW = XmCreateSelectionBox( w, "c_kb_selection", args, n ); 
   XtManageChild( c_kb_selectionW );  

   /*** only KnowledgeBases already loaded can be chosen from ***/

   w = XmSelectionBoxGetChild( c_kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   
   
   w = XmSelectionBoxGetChild( c_kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( c_kb_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   /* install callback for the CLOSE-Button of the mwm-border menu */

   XmAddWMProtocolCallback( XtParent( XtParent( c_kb_selectionW ) ),
      WM_DELETE_WINDOW, (XtPointer)c_kb_cancelCB, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( c_kb_selectionW, XmNokCallback,
      (XtPointer)c_kb_selectionCB, NULL );
   XtAddCallback( c_kb_selectionW, XmNcancelCallback,
      (XtPointer)c_kb_cancelCB, NULL );

 
   /*******  KnowledgeBase-SelectionMenu-MessageBox-Widget  ******/

   c_kb_selection_msgW = createMessageBox( c_kb_selectionW,
                                             "c_kb_selection_msg" );



   /******** Constraints-Name-Knopf ******************/

    c_nameW = XtVaCreateManagedWidget( "c_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
    XtAddCallback( c_nameW, XmNactivateCallback,
       (XtPointer)constraintnetEditorAnzeigenCB, NULL );
       
   XtManageChild ( c_nameW );                                  
   XtVaGetValues( c_nameW, XmNlabelString, &unnamed_str, NULL );

   /********   Restriction-SelectionMenu-Widget   ********/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( c_nameW, "c_co_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   c_co_selectionW = XmCreateSelectionBox( w, "c_co_selection", args, n ); 
   w = XmSelectionBoxGetChild( c_co_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   w = XmSelectionBoxGetChild( c_co_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtManageChild( c_co_selectionW );  
   
   XtAddCallback( c_co_selectionW, XmNcancelCallback,
     (XtPointer)co_cancelCB, NULL );
   XtAddCallback( c_co_selectionW, XmNokCallback,
     (XtPointer)co_selectionCB, NULL );

  
  /******** Restriction-SelectionMenu-MessageBox-Widget ******************/

   c_co_selection_msgW = createMessageBox( c_co_selectionW,
                                             "c_co_selection_msg" );

  /********** LISP-MessageBox-Widget fuer Delete-Warnung ************/

   c_DelWarningW = createMessageBox( c_co_selectionW,
                                             "c_Delete_Warnung" );
 

  /********** QuestionBox fuer  Delete-Warnung ************/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   c_DelWarningW = XmCreateQuestionDialog(c_shellW, "c_DelWarningW",args,n);


   /***** ungenutzten Help-Button  unmanagen *****/

   w = XmMessageBoxGetChild( c_DelWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( c_DelWarningW, XmNokCallback,
     (XtPointer)c_DelWarningokCB, NULL );
   XtAddCallback( c_DelWarningW, XmNcancelCallback,
      (XtPointer)c_DelWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent( c_DelWarningW ), WM_DELETE_WINDOW,
      (XtPointer)c_DelWarningcancelCB, (XtPointer)c_DelWarningW );




  /********** QuestionBox fuer  Load-Warnung ************/

   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   c_LoadWarningW = XmCreateQuestionDialog( c_shellW, "c_LoadWarningW",args,n);


   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( c_LoadWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( c_LoadWarningW, XmNokCallback,
     (XtPointer)c_LoadWarningokCB, NULL );
   XtAddCallback( c_LoadWarningW, XmNcancelCallback,
      (XtPointer)c_LoadWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(c_LoadWarningW), WM_DELETE_WINDOW,
      (XtPointer)c_LoadWarningcancelCB, (XtPointer)c_LoadWarningW );
                                                    


   /*******************************************************************
       Paned-Window fuer drei Editorfelder 
         (Variablen, Dokumentation und Rumpf) 
   *******************************************************************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                           form1W ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;

   panedwindow = XmCreatePanedWindow ( formW, "panedwindow", args, n ); 
 
   /********* 1. Editorfenster fuer die Variablen *********************/    
   n = 0;

   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopAttachment,                XmATTACH_FORM ); n++;

   c_forminterfaceW = XmCreateForm( panedwindow, "formEditorWin", args, n );
   XtManageChild( c_forminterfaceW );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,                XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   labelEd1W = XmCreateLabel( c_forminterfaceW, "Variablen", args, n );
   XtManageChild( labelEd1W );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                       labelEd1W ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   c_interfaceW = XmCreateScrolledText( c_forminterfaceW, "EditorWin", args, n );
   XtAddCallback( c_interfaceW, XmNvalueChangedCallback,
     (XtPointer)c_changedCB, NULL );
   XtManageChild( c_interfaceW );
   
   /********* 2. Editorfenster fuer den Rumpf *********************/
   n = 0;

   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                           c_forminterfaceW ); n++;
   formEd2W = XmCreateForm( panedwindow, "formEditorWin", args, n );
   XtManageChild( formEd2W );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   c_labelbodyW = XmCreateLabel( formEd2W, "Ausdruck", args, n );
   XtManageChild( c_labelbodyW );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                   c_labelbodyW ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   c_bodyW = XmCreateScrolledText( formEd2W, "EditorWin", args, n );
   XtAddCallback( c_bodyW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( c_bodyW, XmNvalueChangedCallback,
     (XtPointer)c_changedCB, NULL );
   XtManageChild( c_bodyW );
   XtManageChild ( panedwindow );

   c_Reset();
}            

/****************************************************
*   Change Sensitivity of the ConstraintNet Editor  *
*   (accessable from outside...)                    *
****************************************************/

void
CSNET_ready( void ) {

   if( XtIsRealized( c_shellW ) ) {
      XDefineCursor(
         XtDisplay( c_shellW ),
         XtWindow( c_shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
CSNET_busy( void ) {

   if( XtIsRealized( c_shellW ) ) {
      XDefineCursor(
         XtDisplay( c_shellW ),
         XtWindow( c_shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the ConstraintNet Editor      *
****************************************************/

void
CSNET_show( void ) {

   PopupRaiseAndDeiconify( c_shellW );
}

/***************************************************/

void
CSNET_hide( void ) {

   XtPopdown( c_shellW );
}

/*** EOF ******************************************/

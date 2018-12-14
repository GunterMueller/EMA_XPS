/****************************************************
*   instance.c    Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the instance-editor.         *
*   Full version only.                              *
****************************************************/

#include <stdio.h> 
#include <Xm/Form.h> 
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>    
#include <Xm/PushBG.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Protocols.h>
#include <Xm/ScrollBar.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>

#include "uiw.h"
#include "main.h"   
#include "instance.h" 
#include "help.h"
#include "xinit.h"

/******* GLOBALs *************************************************************/

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

static char *selected_kb=NULL;  /* selected KnowledgeBase */
static char *selected_fr=NULL;  /* selected Frame */
static char *selected_in=NULL;  /* selected instance */
static XmString unnamed_str;    /* XmString to be drawn if nothing is chosen */

static int  editable;           /* are Texts and OptionMenus editable? */
static int  unsaved_changes;    /* any edits unsaved? */

static Pixmap change_pixmap0, change_pixmap1;
static Widget RowColW;
extern Pixmap iconPm;

static Widget INSTANCE_shellW;
extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;         

static Widget INSTANCE_nameW,INSTANCE_frameW,INSTANCE_slotsW;
static Widget unsaved_changes_labelW, kbnameW,frameW,instanceW,
       kb_selectionW, kb_selection_msgW,
       frame_selectionW, frame_selection_msgW,
       instance_selectionW, instance_selection_msgW,
       LadenW, LoeschenW, LoadWarningW, DelWarningW,ReChangeW;

static void INSTANCE_Reset( void );

/******************************************************************************
* Funktionen
******************************************************************************/

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
Change_True( void ) {

   XtVaSetValues( unsaved_changes_labelW,
   XmNlabelPixmap, change_pixmap1,
   XmNlabelInsensitivePixmap, change_pixmap1, NULL );
   unsaved_changes = True;
   XtSetSensitive( ReChangeW, True );
   if( selected_kb )
      C_set_modified( selected_kb, MOD_INSTANCE, True );
}

static
Change_False( void ) {

   XtVaSetValues( unsaved_changes_labelW,
   XmNlabelPixmap, change_pixmap0,
   XmNlabelInsensitivePixmap, change_pixmap0, NULL );
   XtSetSensitive( ReChangeW, False );
   unsaved_changes = False;
   if( selected_kb )
      C_set_modified( selected_kb, MOD_INSTANCE, False );
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
INSTANCE_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) )
         INSTANCE_Reset();
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

/*******************************************************************************
*   Change Editability of Editors
*******************************************************************************/

static void 
INSTANCE_edit( Boolean ability ) {

   XtSetSensitive( INSTANCE_slotsW, ability );
   editable = ability;
}

/*******************************************************************************
*   Noch keine Instanz gewaehlt
*******************************************************************************/

static void
No_Instance_Selected( void ) {

   XtVaSetValues( instanceW, XmNlabelString,  unnamed_str , NULL );
   XtSetSensitive( instanceW,   True );
   INSTANCE_edit( False );
   if (selected_in)
      XtFree(selected_in);
   selected_in=NULL;
   Change_False();
   XmTextSetString( INSTANCE_slotsW, "" );
   XmTextShowPosition( INSTANCE_slotsW, 0 );
   XtSetSensitive( LadenW, False);        /* Menus insensitive */
   XtSetSensitive( LoeschenW, False); 
}

/*******************************************************************************
*   Noch kein Frame ausgewaehlt
*******************************************************************************/

static void
No_Frame_Selected( void ) {

   XtVaSetValues( frameW, XmNlabelString, unnamed_str, NULL );            
   XtSetSensitive( frameW, True );
   No_Instance_Selected();
   XtSetSensitive( instanceW, False );
}

/******************************************************************************/
/*          Reset des Instanz-Editors                                           */
/******************************************************************************/

static void
INSTANCE_Reset( void ) {

   No_Frame_Selected();
   XtVaSetValues( kbnameW, XmNlabelString, unnamed_str, NULL );
   XtSetSensitive( frameW, False );
   if( selected_kb ) {
      XtFree( selected_kb );
      selected_kb = NULL;
   }      
}

/******************************************************************************
*  Einlesen der Definition einer Instanz
******************************************************************************/

void 
emaxps_instance_definition( void ) {
   char *name, *frame, *slots, *p, *v[4], *str;
   int i, err, ret;
   XmString cstring;

   v[0] = p = MSG_read();
   for( err = 0, i = 1; i < 3; i++ ) {
      if( !(v[i]=GetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }

   if( err ) {
      fprintf( stderr, "inst-def: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "inst-def: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   name  = v[i++];
   frame = v[i++];
   slots = v[i++];
   
   if (selected_in)
      XtFree(selected_in);
   selected_in=XtMalloc(1+strlen(name));
   strcpy(selected_in,name);
   
   XmTextSetString( INSTANCE_slotsW, slots );
   XtVaSetValues( instanceW, XmNlabelString, XmStringCreateSimple( name ), NULL );
   XtFree( p );
   Change_False();
   INSTANCE_edit( True );
   XtSetSensitive( LadenW, True );        /* Menus insensitive */
   XtSetSensitive( LoeschenW, True );
}

/*******************************************************************************
*   LISP sends Frames for a Selection-Box
*******************************************************************************/

void 
emaxps_inst_frame_select( void ) {
   char *str, *s, *s0, *hdl;
   int c, i, len, nr;
   XmStringTable xmstr_list;
   
   s0 = s = str = MSG_read();
   len = strlen( str );
   xmstr_list = (XmString *)XtMalloc( len+1024 );
   nr = 0;
   while( c = *s++ ) {          /* Eintraege von LISP holen */
      if( c == ETX ) {
         *(s-1) = NUL;
         nr++;
         xmstr_list[nr-1] = XmStringCreateSimple( s0 );
         s0 = s;
      }
   } 
   XtVaSetValues( frame_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, nr,
      NULL );
   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   XtManageChild( XtParent(frame_selectionW) );
}

/*******************************************************************************
*   LISP sends Instances for a Selection-Box
*******************************************************************************/

void 
emaxps_inst_instance_select( void ) {
   char *str, *s, *s0, *hdl;
   int c, i, len, nr;
   XmStringTable xmstr_list;
   s0 = s = str = MSG_read();
   len = strlen( str );
   xmstr_list = (XmString *)XtMalloc( len+1024 );

   nr = 0;
   while( c = *s++ ) {          /* Eintraege von LISP holen */
      if( c == ETX ) {
         *(s-1) = NUL;
         nr++;
         xmstr_list[nr-1] = XmStringCreateSimple( s0 );
         s0 = s;
      }
   } 
   xmstr_list[nr]=NULL;
   XtVaSetValues( instance_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, nr,
      NULL );

   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   XtManageChild( XtParent(instance_selectionW) );
}


/******************************************************************************
*   Laden-Callback fuer den Instanzeditor
******************************************************************************/

static void 
instanceEditorLadenCB( void ) {

   XtManageChild( LoadWarningW );
}  

/*******************************************************************************
*   Callback of Rechange-Button  
*******************************************************************************/

static void
instanceEditorRechangeCB( void ) {
   char *buf;

   if (selected_in) {
      buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_in)
                           + strlen(selected_fr) );
      sprintf( buf, "(hole-instanz-definition  '%s '%s '%s) ",
                    selected_kb ,selected_in, selected_fr );
      LISP_sndCmd( buf );     
      XtFree( buf );
   }
}

/*******************************************************************************
*   Callback of Cancel-Button in LoadWarningBox
*******************************************************************************/

static void
LoadWarningcancelCB( void ) {

   XtUnmanageChild( XtParent( LoadWarningW ) );
}

/*******************************************************************************
*   Callback of OK-Button in LoadWarningBox
*******************************************************************************/

static void
LoadWarningokCB( void ) {
   char *buf;
   char *slots;                            
   int i;

   if (selected_in){ 
      slots     = XmTextGetString( INSTANCE_slotsW );
      buf =  XtMalloc( strlen( selected_in ) + strlen( selected_in ) + strlen( slots )  + 256 );
               
      sprintf( buf, "(MY-DEFINSTANCE %s %s  of  %s  with  %s) ",  /* \n to run! */
                                   selected_kb,selected_in, selected_fr, slots );
      LISP_sndCmd( buf );
      Change_False();
      XtFree( buf );
      XtFree( slots);
   }
   XtUnmanageChild( XtParent( LoadWarningW ) );
}

/******************************************************************************
*   Quit-Callback fuer den Instanzeditor 
******************************************************************************/

static void 
instanceEditorQuitCB ( void ) {
                               
   XtUnrealizeWidget( INSTANCE_shellW );  
} 

/*****************************************************************************
*   Callback fuer den Menuegruppe "Instanz loeschen .."
*   im Instanzeditor 
******************************************************************************/

static void 
instanceEditorLoeschenCB( void ) {

  XtManageChild( DelWarningW );
}

/*******************************************************************************
*   Callback of Cancel-Button in DeleteWarningBox
*******************************************************************************/

static void
DelWarningcancelCB( void ) {

   XtUnmanageChild( XtParent( DelWarningW ) );
}

/*******************************************************************************
*   Callback of OK-Button in DeleteWarningBox
*******************************************************************************/

static void
DelWarningokCB( void ) {
   char *buf;

   if (selected_fr) {
      buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_in) );
      sprintf( buf, "(loesche-instanz-mit-menue '%s '%s) ", selected_kb , selected_in );
      LISP_sndCmd( buf );     
      XtFree( buf );
   }
   No_Instance_Selected();               

   /* Delete List-Entry */
   XmTextSetString( XmSelectionBoxGetChild( instance_selectionW, XmDIALOG_TEXT ) , "" );
   XtUnmanageChild( XtParent( DelWarningW ) );
}

/*******************************************************************************
*   Callback if actual instance is edited
*******************************************************************************/

static void 
INSTANCE_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {

   if( !editable )
      return;
   Change_True();
}

/*****************************************************************************
*   Frame-Button-Callback
******************************************************************************/

static void
framebuttonCB( void ) {
   char *buf;

   buf = XtMalloc( 256 + strlen(selected_kb) );
   sprintf( buf, "(instance-hole-frame-definition-mit-menue  '%s )", selected_kb);
   LISP_sndCmd ( buf );
}

/*******************************************************************************
*   Callback of Ok-Button in Frame-SelectionMenu
*******************************************************************************/

static void 
fr_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( frame_selection_msgW, cbs )) ) {
      return;
   }
   if (selected_fr)
      XtFree(selected_fr);
      
   selected_fr = UIW_NewString( selection );
   XtUnmanageChild( XtParent( frame_selectionW ) );
   No_Instance_Selected();
   XtVaSetValues( frameW, XmNlabelString,  CSTRING(selected_fr) , NULL );                  
}

/*******************************************************************************
*   Callback of Cancel-Button in Frame-SelectionMenu
*******************************************************************************/

static void
fr_cancelCB( void ) {

   XtUnmanageChild( XtParent( frame_selectionW ) );
   if (selected_in) 
      XtSetSensitive( INSTANCE_slotsW, True );
}

/*****************************************************************************
*   Instance-Button-Callback
******************************************************************************/

static void
instancebuttonCB( void ) {
   char *buf;

   buf = XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr) );
   sprintf( buf, "(hole-instanz-definition-mit-menue  '%s '%s)", selected_kb, selected_fr);
   LISP_sndCmd ( buf );
   XtFree( buf );
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button
*******************************************************************************/

static void 
chooseKbCB( void ) {
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
   
   XtVaSetValues( kb_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, countKB,
      NULL );
   i = 0;
   while( i < countKB )
      XmStringFree( xmstr_list[i++] );
   XtFree( (char*)xmstr_list );
   XtManageChild( XtParent(kb_selectionW) );
}

/*******************************************************************************
*   Callback of Ok-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void 
kb_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( kb_selection_msgW, cbs )) ) {
      return;
   }
   XtUnmanageChild( XtParent( kb_selectionW ) );
   if( selected_kb )
     if( !strcmp(selection,selected_kb)) {
        XtFree( selection );
         return;
     }      
   selected_kb = UIW_NewString( selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_kb );
   XtVaSetValues( kbnameW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );
   No_Frame_Selected();
}

/*******************************************************************************
*   Callback of Cancel-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void
kb_cancelCB( void ) {

   XtUnmanageChild( XtParent( kb_selectionW ) );
}

/*******************************************************************************
*   Callback of Ok-Button in Instance-SelectionMenu
*******************************************************************************/

static void 
in_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;
   if( !(selection = selectionMenuOk( instance_selection_msgW, cbs )) ) {
      return;
   }
   XtUnmanageChild( XtParent( instance_selectionW ) );
   LISP_sndReply( selection );
}

/*******************************************************************************
*   Callback of Cancel-Button in Instance-SelectionMenu
*******************************************************************************/

static void
in_cancelCB( void ) {

   LISP_sndReply( "NIL" );
   XtUnmanageChild( XtParent( instance_selectionW ) );
}

/******************************************************************************
*   Erzeugen des Instanzeditors 
******************************************************************************/

void 
INSTANCE_create( Widget parent ) {
   Widget   formW,  w1, w2, w3, w;
   Widget   labelW;
   Widget   menubarW;    
   Widget   form1W;  
   Widget   kbnamelabelW, framelabelW, instancelabelW;    
   Widget   editorformW, editorlabelW;
   register int n;
   Arg      args[20];
   ArgList  argl;        

   /************** Hauptfenster **********************************/
   n = 0;
   XtSetArg( args[n], XmNdeleteResponse,           XmDO_NOTHING ); n++; 
   XtSetArg( args[n], XmNiconPixmap,                     iconPm ); n++; 

   INSTANCE_shellW = XtAppCreateShell( "Instance-Editor", "HereClassIsIgnored", 
                              topLevelShellWidgetClass,
                              XtDisplay(parent), args, n);   
            
   /* Callback fuer den CLOSE-Button im Systemmenue (MWM) einrichten */
   XmAddWMProtocolCallback( INSTANCE_shellW, WM_DELETE_WINDOW, 
                            (XtPointer)INSTANCE_hide, (XtPointer)NULL );

   /********** Form-Widget erzeugen ********************************/
   n = 0;
   formW = XmCreateForm( INSTANCE_shellW, "Instanzeditor", args, n );
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
      (XtPointer)HELP_show, (XtPointer)HELP_INSTANCE_MENU );

   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++; 

   menubarW = XmCreateMenuBar( formW, "menubar", args, n );
   XtManageChild( menubarW );

   /****************************************************************/ 
   /*** Pulldown-Menuepunkt "Bearbeiten" erzeugen und managen *************/ 
  
   w1=XmCreateSimplePulldownMenu( menubarW, "Bearbeiten-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Bearbeiten",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );
   LadenW = XtVaCreateManagedWidget( "Laden",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LadenW, XmNactivateCallback,
                    (XtPointer)instanceEditorLadenCB, NULL );
   LoeschenW = XtVaCreateManagedWidget( "Instanzloeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LoeschenW, XmNactivateCallback,
                    (XtPointer)instanceEditorLoeschenCB, NULL );
   ReChangeW = XtVaCreateManagedWidget( "ReChange", 
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(ReChangeW, XmNactivateCallback,
        (XtPointer)instanceEditorRechangeCB, NULL );
   XtSetSensitive( LadenW, False);        /* Menus insensitive */
   XtSetSensitive( LoeschenW, False); 
   XtSetSensitive( ReChangeW, False);

   /**************************************************************/
   /*** Pulldown-Menuepunkt "Hilfe" erzeugen und managen *************/
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
                    (XtPointer)HELP_show, (XtPointer)HELP_INSTANCE_MENU );

   w = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, w1, NULL );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   XtManageChild( menubarW );

   /*********************************************************************
       Form-Widget fuer Label-Widgets zur Anzeige des Behaviornamens 
       und des Framenamens erzeugen 
   *********************************************************************/
   n = 0;                                          
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     menubarW ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNhorizontalSpacing,                    3 ); n++;
   XtSetArg( args[n], XmNverticalSpacing,                      3 ); n++;
 
   form1W = XmCreateForm( formW, "form", args, n );
   XtManageChild( form1W );

   /***************** unsaved-Label-Widget *****************/
   unsaved_changes_labelW = XtVaCreateWidget( "unsaved_changes_label",
      xmLabelWidgetClass, form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,    XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNbottomAttachment,   XmATTACH_FORM,
      NULL );
   XtManageChild ( unsaved_changes_labelW );

   /* after managing !!! */
   PIXMAP_createChanged( unsaved_changes_labelW,
      &change_pixmap0, &change_pixmap1 );

   Change_False() ;    

   RowColW = XtVaCreateManagedWidget( "RowColumn",
             xmRowColumnWidgetClass, form1W,
             XmNrowColumnType, XmWORK_AREA,
             XmNtopAttachment,XmATTACH_FORM,
             XmNleftAttachment,XmATTACH_FORM,
             XmNleftAttachment,           XmATTACH_FORM,
             XmNrightAttachment, XmATTACH_WIDGET,
             XmNrightWidget,  unsaved_changes_labelW ,
             XmNresizeWidth,True,      
             XmNorientation,           XmVERTICAL,
             XmNrecomputeSize,              TRUE,
             XmNnumColumns,2,
             XmNpacking, XmPACK_COLUMN,
             NULL );

   /******** Label zur Anzeige des KB- Namens ******************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
             
   kbnamelabelW = XmCreateLabel( RowColW, "kb", args, n );
   XtManageChild( kbnamelabelW );

   /******** Label zur Anzeige des Frame- Namens ******************/
   n = 0;
   framelabelW = XmCreateLabel( RowColW, "Frame", args, n );
   XtManageChild( framelabelW );

   /******** Label zur Anzeige des Instanz - Namens ******************/
   n = 0;
   instancelabelW = XmCreateLabel( RowColW, "Instance", args, n );
   XtManageChild( instancelabelW );
         
   /******** KB-Name-Knopf ******************/
   kbnameW = XtVaCreateManagedWidget( "kb_name",
      xmPushButtonWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_FORM,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,              kbnamelabelW,
      XmNrightAttachment,  XmATTACH_FORM,
      XmNrecomputeSize,              TRUE,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
   XtAddCallback( kbnameW, XmNactivateCallback,
       (XtPointer)chooseKbCB, NULL );
   XtManageChild ( kbnameW );                                  

   /********   KnowledgeBase-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( RowColW, "kb_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   kb_selectionW = XmCreateSelectionBox( w, "kb_selection", args, n ); 
   XtManageChild( kb_selectionW );  

   /*** only KnowledgeBases already loaded can be chosen from ***/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   /* install callback for the CLOSE-Button of the mwm-border menu */
   XmAddWMProtocolCallback( XtParent( XtParent( kb_selectionW ) ),
      WM_DELETE_WINDOW, (XtPointer)kb_cancelCB, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( kb_selectionW, XmNokCallback,
      (XtPointer)kb_selectionCB, NULL );
   XtAddCallback( kb_selectionW, XmNcancelCallback,
      (XtPointer)kb_cancelCB, NULL );

   /***   KnowledgeBase-SelectionMenu-MessageBox-Widget  ***/
   kb_selection_msgW = createMessageBox( kb_selectionW,
                                             "kb_selection_msg" );

   /******** Frame-Name-Knopf ******************/
   frameW = XtVaCreateManagedWidget( "fr_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
   XtAddCallback( frameW, XmNactivateCallback,
       (XtPointer)framebuttonCB, NULL );
   XtManageChild ( frameW );                                  
   unnamed_str = (XmString) XtMalloc(512);
   XtVaGetValues(frameW, XmNlabelString, &unnamed_str, NULL ); 

   /********   Frame-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( frameW, "fr_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   frame_selectionW = XmCreateSelectionBox( w, "fr_selection", args, n ); 
   w = XmSelectionBoxGetChild( frame_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( frame_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( frame_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   XtManageChild(frame_selectionW );  
   XtAddCallback( frame_selectionW, XmNcancelCallback,
     (XtPointer)fr_cancelCB, NULL );
   XtAddCallback( frame_selectionW, XmNokCallback,
     (XtPointer)fr_selectionCB, NULL );

   /*******  Frame-SelectionMenu-MessageBox-Widget  ******/
   frame_selection_msgW = createMessageBox( frame_selectionW,
                                        "fr_selection_msg" );

   /********    Instance-Knopf   ******************/
   instanceW = XtVaCreateManagedWidget( "in_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
   XtAddCallback( instanceW, XmNactivateCallback,
       (XtPointer)instancebuttonCB, NULL );
       
   XtManageChild ( instanceW );                                  
   XtVaGetValues(instanceW, XmNlabelString, &unnamed_str, NULL );

   /********   Instance-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( instanceW, "in_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   instance_selectionW = XmCreateSelectionBox( w, "in_selection", args, n ); 
   w = XmSelectionBoxGetChild( instance_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   w = XmSelectionBoxGetChild( instance_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtManageChild(instance_selectionW );  

   XtAddCallback( instance_selectionW, XmNcancelCallback,
     (XtPointer)in_cancelCB, NULL );
   XtAddCallback( instance_selectionW, XmNokCallback,
     (XtPointer)in_selectionCB, NULL );

   /*******  Instance-SelectionMenu-MessageBox-Widget  ******/
   instance_selection_msgW = createMessageBox( instance_selectionW,
                                        "in_selection_msg" );

   /********** QuestionBox fuer  Delete-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   DelWarningW = XmCreateQuestionDialog(INSTANCE_shellW, "DelWarningW",args,n);

   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( DelWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( DelWarningW, XmNokCallback,
     (XtPointer)DelWarningokCB, NULL );
   XtAddCallback( DelWarningW, XmNcancelCallback,
      (XtPointer)DelWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(DelWarningW), WM_DELETE_WINDOW,
      (XtPointer)DelWarningcancelCB, (XtPointer)DelWarningW );

  /********** QuestionBox fuer  Load-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   LoadWarningW = XmCreateQuestionDialog(INSTANCE_shellW, "LoadWarningW",args,n);

   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( LoadWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( LoadWarningW, XmNokCallback,
     (XtPointer)LoadWarningokCB, NULL );
   XtAddCallback( LoadWarningW, XmNcancelCallback,
      (XtPointer)LoadWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(LoadWarningW), WM_DELETE_WINDOW,
      (XtPointer)LoadWarningcancelCB, (XtPointer)LoadWarningW );
                                                    
   /******* Ueberschrift fuer das Editorfenster *************************/
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                           form1W ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   editorlabelW = XmCreateLabel( formW, "Slots", args, n );
   XtManageChild( editorlabelW );
   
   /********* Editorfenster ********************** *********************/
   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                    editorlabelW ); n++;  
    XtSetArg( args[n], XmNeditMode,               XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
    
   INSTANCE_slotsW = XmCreateScrolledText( formW, "slotsT", args, n );
   XtAddCallback( INSTANCE_slotsW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( INSTANCE_slotsW, XmNvalueChangedCallback,
        (XtPointer)INSTANCE_changedCB, NULL );
   XtManageChild( INSTANCE_slotsW );

   INSTANCE_Reset();
}            

/***************************************************/

void
INSTANCE_ready( void ) {

   if( XtIsRealized( INSTANCE_shellW ) ) {
      XDefineCursor(
         XtDisplay( INSTANCE_shellW ),
         XtWindow( INSTANCE_shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
INSTANCE_busy( void ) {

   if( XtIsRealized( INSTANCE_shellW ) ) {
      XDefineCursor(
         XtDisplay( INSTANCE_shellW ),
         XtWindow( INSTANCE_shellW ),
         cursor_busy
      );
   }
}
/***************************************************/

void
INSTANCE_show( void ) {

   PopupRaiseAndDeiconify( INSTANCE_shellW );
}

/***************************************************/

void
INSTANCE_hide( void ) {

   XtPopdown( INSTANCE_shellW );
}

/*** EOF *******************************************/

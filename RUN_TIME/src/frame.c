/****************************************************
*   frame.c       Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the frame-editor.            *
*   Full version only.                              *
****************************************************/

#include <stdio.h>
#include <Xm/Form.h> 
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>    
#include <Xm/PushBG.h>
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
#include <Xm/SeparatoG.h>

#include "uiw.h"           /* UIW-String-Operations */
#include "main.h"   
#include "frame.h" 
#include "help.h"
#include "xinit.h"

/******* GLOBALs *************************************************************/

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

static int  FRAME_editable;           /* are Texts and OptionMenus editable? */
static int  FRAME_unsaved_changes;    /* any edits unsaved? */

static char *selected_kb=NULL;          /* selected KnowledgeBase */
static char *selected_fr=NULL;          /* selected frame   */
static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */

static Pixmap change_pixmap0, change_pixmap1;

static Widget FRAME_shellW, RowColW; 
extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;         

static Widget FRAME_nameW, FRAME_supersW, FRAME_slotsW,
       FRAME_checkeinW, FRAME_checkausW,
       FRAME_unsaved_changes_labelW, FRAME_kbnameW,
       kb_selectionW, kb_selection_msgW;
static Widget fr_selectionW, fr_selection_msgW, 
       LadenW, LoeschenW,
       DelWarningW, LoadWarningW,ReChangeW;       

static Widget LISP_Delete_WarnW;       /* LISP says: Cannot delete */

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

/******************************************************************************
*  Frame-Editor-Change
******************************************************************************/

static void
Change_True( void ) {

   XtVaSetValues( FRAME_unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap1,
      XmNlabelInsensitivePixmap, change_pixmap1,
      NULL );
   FRAME_unsaved_changes = True;                    
   XtSetSensitive( ReChangeW, True );
   XtSetSensitive( LadenW, True );
   if( selected_kb )
     C_set_modified( selected_kb, MOD_FRAME, True );
}

static
Change_False( void ) {

   XtVaSetValues( FRAME_unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap0,
      XmNlabelInsensitivePixmap, change_pixmap0,
      NULL );
   FRAME_unsaved_changes = False;                    
   XtSetSensitive( ReChangeW, False );
   XtSetSensitive( LadenW, False );
   if( selected_kb )
      C_set_modified( selected_kb, MOD_FRAME, False );
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

/*******************************************************************************
*   Change Editability of Editors
*******************************************************************************/

static void 
FRAME_edit( Boolean ability ) {

   XtSetSensitive( FRAME_supersW, ability );
   XtSetSensitive( FRAME_slotsW, ability );
   FRAME_editable = ability;
}

/*******************************************************************************
*   Frame-Auswahl loeschen
*******************************************************************************/

static void
Frame_NoSelect( void ) {
   /* KB selected, but no frame yet! */

   XtVaSetValues( FRAME_nameW, XmNlabelString, unnamed_str, NULL );            
   FRAME_edit( False );
   Change_False();

   if( selected_fr ) {
      XtFree( selected_fr );
      selected_fr = NULL;   
   }

   XmTextSetString( FRAME_slotsW, "" );
   XmTextShowPosition( FRAME_slotsW, 0 );  /* Clear windows */
   XmTextShowPosition( FRAME_supersW, 0 );
   XmTextSetString( FRAME_supersW, "" );      

   XtSetSensitive( FRAME_checkeinW, True );
   XtSetSensitive( FRAME_checkausW, True );
   XtSetSensitive( FRAME_nameW, True );

   XtSetSensitive( LadenW, False );        /* Menus insensitive */
   XtSetSensitive( LoeschenW, False ); 
}

/******************************************************************************/
/*          Reset des Frame-Editors                                           */
/******************************************************************************/

static void
Frame_Reset( void ) {
   /* nor KB neither Frame selected! */

   XtVaSetValues( FRAME_kbnameW, XmNlabelString, unnamed_str, NULL );
   Frame_NoSelect();

   if( selected_kb ) {
      XtFree( selected_kb );
      selected_kb = NULL;
   }      

   XtSetSensitive( FRAME_checkeinW, False );
   XtSetSensitive( FRAME_checkausW, False );
   XtSetSensitive( FRAME_nameW, False );
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
FRAME_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) )
         Frame_Reset();
}   

/******************************************************************************
*
*  Einlesen der Definition eines Frames
*
******************************************************************************/

void 
emaxps_frame_definition( void ) {
   char *name, *supers, *slots, *p, *v[4], *str;
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
      fprintf( stderr, "frame-def: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "frame-def: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   i = 0;
   name = v[i++];
   if (selected_fr)
       XtFree(selected_fr);
   selected_fr=  XtMalloc(1+strlen( name ));
   strcpy( selected_fr,name );
   supers  = v[i++];
   slots   = v[i++];
      
   XtVaSetValues( FRAME_nameW, XmNlabelString,
      cstring=XmStringCreateSimple(selected_fr), NULL );
   XmStringFree( cstring );
   XmTextSetString( FRAME_supersW, supers );
   XmTextSetString( FRAME_slotsW , slots );
   XtSetSensitive( LoeschenW, True );      /* Menus sensitive */    
   XtFree( p );
   FRAME_edit( True );
   Change_False();
}


/******************************************************************************
* Callbacks
******************************************************************************/




/*****************************************************************************
*   Callbacks fuer den Frameeditor
******************************************************************************/

/*****************************************************************************
*
*   Callback fuer den Menuepunkt "Anzeigen" im Frameeditor 
*
******************************************************************************/
void 
frameEditorAnzeigenCB( void ){
    char *buf;
    buf =  XtMalloc( 256 + strlen(selected_kb) );
    sprintf( buf, "(hole-frame-definition-mit-menue  '%s )", selected_kb);
   
   LISP_sndCmd ( buf ); 
 }

/******************************************************************************
*  
*   Laden-Callback fuer den Frameeditor 
*
******************************************************************************/
void 
frameEditorLadenCB ( void ) {

   XtManageChild( LoadWarningW );


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
   char *supers;
   char *slots;                            
   int i;
   
   
   if (selected_fr) { 
     supers    = XmTextGetString( FRAME_supersW );
     slots     = XmTextGetString( FRAME_slotsW );
   

     buf =  XtMalloc( strlen( selected_fr ) + strlen( supers )+strlen( slots )+ 256 );            
   
     sprintf( buf, "(MY-DEFFRAME %s %s (supers %s) (slots %s)) ",  /* \n to run! */
                               selected_kb, selected_fr,     supers,    slots );
  
  
  
     /*******Nachricht ueber die toplevel-Pipe an LISP senden **********/
     LISP_sndCmd( buf );

     XtFree( buf );
     XtFree( supers );
     XtFree( slots);
     Change_False();   
   }
   XtUnmanageChild( XtParent( LoadWarningW ) );
}

/*******************************************************************************
*   Callback of Rechange-Button  
*******************************************************************************/

static void
frameEditorRechangeCB( void ) {
char *buf;
  if (selected_fr) {
     buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr) );
     sprintf( buf, "(hole-frame-definition  '%s '%s) ", selected_kb , selected_fr );
     LISP_sndCmd( buf );     
     XtFree( buf );
   }




}




/*****************************************************************************
*  
*   Quit-Callback fuer den Frameeditor 
*
******************************************************************************/
void 
frameEditorQuitCB ( void ) {
                               
   XtUnrealizeWidget( FRAME_shellW );  
} 

/*****************************************************************************
*
*   Callback fuer den Menuegruppe "Frame loeschen .."
*   im Frameeditor 
*
******************************************************************************/

void 
frameEditorLoeschenCB( void ) {

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

   buf = XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr) );
   sprintf( buf, "(emaxps-delete-frame '%s '%s)", selected_kb, selected_fr );
   LISP_sndCmd( buf );     
   XtFree( buf );

   /* Delete TextField-Entry in FrameSelectionPopup */
   XmTextSetString( XmSelectionBoxGetChild( fr_selectionW, XmDIALOG_TEXT ), "" );
   XtUnmanageChild( XtParent( DelWarningW ) );
}

/*****************************************************************************
*
*   Callback fuer die FrameCheck-Menuepunkte 
*   im Frameeditor 
*
******************************************************************************/

void                                 /* Frame-Check ein */
frameEditorCheckeinCB( void ) {
   char *buf = XtMalloc( 256 + strlen(selected_kb) );

   XtUnmanageChild( FRAME_checkeinW );
   XtManageChild( FRAME_checkausW );

   sprintf( buf,
      "($send ($send (symbol-value '%s) :frame-processor) :set-frcheck T)",
      selected_kb
   );
   LISP_sndCmd( buf );
   XtFree( buf );
}

/*************************************************************/

void                                /* Frame-Check aus */
frameEditorCheckausCB( void ){
   char *buf = XtMalloc( 256 + strlen(selected_kb) );

   XtUnmanageChild( FRAME_checkausW );
   XtManageChild( FRAME_checkeinW );

   sprintf( buf,
      "($send ($send (symbol-value '%s) :frame-processor) :set-frcheck NIL)",
      selected_kb
   );
   LISP_sndCmd( buf );
   XtFree( buf );
}

/*******************************************************************************
*   Callback if actual frame is edited
*******************************************************************************/

static void 
FRAME_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {
   static Widget last_junctor, last_actiontype;

   if( !FRAME_editable )
      return;
   Change_True();
   
}


/*******************************************************************************
*   LISP sends Frames for a Selection-Box
*******************************************************************************/

void 
emaxps_frame_selection ( void ) {
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

    XtVaSetValues( fr_selectionW,    /* ins Fenster schreiben */
       XmNlistItems, xmstr_list,
       XmNlistItemCount, nr, 
       NULL );

   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   
   
   XtManageChild( XtParent(fr_selectionW) );

}
/*******************************************************************************
*   Callback of Ok-Button in Frame-SelectionMenu
*******************************************************************************/

static void 
fr_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection;
							
   if( selection = selectionMenuOk( fr_selection_msgW, cbs ) ) {
      XtUnmanageChild( XtParent( fr_selectionW ) );
      LISP_sndReply( selection );
      XtFree( selection );
   }
}

/*******************************************************************************
*   Callback of Cancel-Button in Frame-SelectionMenu
*******************************************************************************/

static void
fr_cancelCB( void ) {

   LISP_sndReply( "NIL" );
   XtUnmanageChild( XtParent( fr_selectionW ) );
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button
*******************************************************************************/

static void 
chooseKbCB ( void ) {
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

   XtManageChild( XtParent (kb_selectionW));
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
   
   if (selected_kb){
     if( !strcmp(selection,selected_kb)) {
        XtFree( selection );
         return;
     }      
   }  
   
   selected_kb = UIW_NewString( selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_kb );
   XtVaSetValues( FRAME_kbnameW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   FRAME_edit( False );
   
   Frame_NoSelect();
   XtUnmanageChild(FRAME_checkausW);   /* Erst mal Frame-Check aus */
   XtManageChild(FRAME_checkeinW);
   buf =  XtMalloc( 256 + strlen(selected_kb) );
   sprintf( buf, "(emaxps-get-frame-check '%s) ", selected_kb );
   LISP_sndCmd( buf );     
   XtFree( buf );
}

/*******************************************************************************
*   Callback of Cancel-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void
kb_cancelCB( void ) {

   XtUnmanageChild( XtParent( kb_selectionW ) );
}

/*******************************************************************************
*   LISP:  Frame-Check is active
*******************************************************************************/

void
emaxps_frame_check_on( void ) {

   XtUnmanageChild( FRAME_checkeinW );   
   XtManageChild( FRAME_checkausW );
}    

/*******************************************************************************
*   LISP:  Delete-Bestaetigung oder Delete-Abbruch
*******************************************************************************/

void
emaxps_frame_delete( void ) {
   char *p;

   p = MSG_read();
   if( !strncmp( p, "ERROR", 5 ) )
      XtManageChild( LISP_Delete_WarnW );
   else
      Frame_NoSelect();  
   XtFree( p );
}    
                                                                               
/******************************************************************************
* create-Funktionen
******************************************************************************/
/******************************************************************************
*
*   Erzeugen des Frameeditors 
*
******************************************************************************/
void 
FRAME_create( Widget parent ) {
   Widget   formW, w,w1, w2, w3 ;
   Widget   frameW, labelW;
   Widget   menubarW;    
   Widget   form1W;  
   Widget   nameW, formnameW, panedwindowW, formsupersW, labelsupersW;
   Widget   formslotsW, namelabelW, kbnamelabelW, formnamelabelW, labelslotsW;    
   Widget   editorformW, editorlabelW;
   register int n;
   Arg      args[20];
   ArgList  argl;        

   /**************** Hauptfenster **********************************/
   n = 0;
   XtSetArg( args[n], XmNtransient,                        True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,           XmDO_NOTHING ); n++; 
   XtSetArg( args[n], XmNiconPixmap,                     iconPm ); n++; 

   FRAME_shellW = XtAppCreateShell ( "Frame-Editor", "HereClassIsIgnored", 
                                topLevelShellWidgetClass,
                                XtDisplay(parent), args, n);   

   /* Callback fuer den CLOSE-Button im Systemmenue (MWM) einrichten */
   XmAddWMProtocolCallback( FRAME_shellW, WM_DELETE_WINDOW, 
                            (XtPointer)FRAME_hide, (XtPointer)NULL );

   n = 0;
   formW = XmCreateForm (FRAME_shellW, "editor", args, n);
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
      (XtPointer)HELP_show, (XtPointer)HELP_FRAME_MENU );

   n=0;
   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,       XmATTACH_FORM ); n++;

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
                    (XtPointer)frameEditorLadenCB, NULL );

   LoeschenW = XtVaCreateManagedWidget( "Frameloeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LoeschenW, XmNactivateCallback,
                    (XtPointer)frameEditorLoeschenCB, NULL );

   ReChangeW = XtVaCreateManagedWidget( "ReChange", 
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(ReChangeW, XmNactivateCallback,
        (XtPointer)frameEditorRechangeCB, NULL );

   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, w1, NULL );

   FRAME_checkeinW = XtVaCreateManagedWidget( "FrameCheckEin",
                   xmPushButtonGadgetClass, w1,
                   NULL ); 
   XtAddCallback( FRAME_checkeinW, XmNactivateCallback,
                       (XtPointer)frameEditorCheckeinCB, NULL );

   FRAME_checkausW = XtVaCreateManagedWidget( "FrameCheckAus",
                   xmPushButtonGadgetClass, w1,
                   NULL );
   XtAddCallback(FRAME_checkausW, XmNactivateCallback,
                       (XtPointer)frameEditorCheckausCB, NULL );
   XtUnmanageChild( FRAME_checkausW );  /* Anfangs ist Check aus */

   XtSetSensitive( ReChangeW, False );
   XtSetSensitive( LadenW, False );
   XtSetSensitive( LoeschenW, False );
   XtSetSensitive( FRAME_checkausW, False );
   XtSetSensitive( FRAME_checkeinW, False );

   /****************************************************************/ 
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
                    (XtPointer)HELP_show, (XtPointer)HELP_FRAME_MENU );

   w = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, w1, NULL );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   XtManageChild( menubarW );

   /******** Label zur Anzeige des KB - Namens ******************/
   n = 0;                                          
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     menubarW ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
 
   form1W = XmCreateForm( formW, "form", args, n );
   
   XtManageChild( form1W );

   /***************** unsaved-Label-Widget *****************/
   FRAME_unsaved_changes_labelW = XtVaCreateWidget( "FRAME_unsaved_changes_label",
      xmLabelWidgetClass, form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,      XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNbottomAttachment,   XmATTACH_FORM,
      NULL );
   XtManageChild ( FRAME_unsaved_changes_labelW );

   /* after managing !!! */
   PIXMAP_createChanged( FRAME_unsaved_changes_labelW,
      &change_pixmap0, &change_pixmap1 );

   Change_False();

   RowColW = XtVaCreateManagedWidget( "RowColumn",
             xmRowColumnWidgetClass, form1W,
             XmNrowColumnType, XmWORK_AREA,
             XmNtopAttachment,XmATTACH_FORM,
             XmNleftAttachment,XmATTACH_FORM,
             XmNleftAttachment,           XmATTACH_FORM,
             XmNrightAttachment, XmATTACH_WIDGET,
             XmNrightWidget,  FRAME_unsaved_changes_labelW ,
             XmNresizeWidth,True,      
             XmNorientation,           XmVERTICAL,
             XmNrecomputeSize,              TRUE,
             XmNnumColumns,2,
             XmNpacking, XmPACK_COLUMN,
             NULL );

   /******** Label zur Anzeige des KB - Namens ******************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   kbnamelabelW = XmCreateLabel( RowColW, "kbase", args, n );
   XtManageChild( kbnamelabelW );

   /******** Label zur Anzeige des Frame- Namens ******************/
   n = 0;
   namelabelW = XmCreateLabel( RowColW, "Frame", args, n );
   XtManageChild( namelabelW );

   /******** KB-Name-Knopf ******************/
   FRAME_kbnameW = XtVaCreateManagedWidget( "kb_name",
      xmPushButtonWidgetClass, RowColW,
     XmNtopAttachment,    XmATTACH_FORM,
     XmNleftAttachment,   XmATTACH_WIDGET,
     XmNleftWidget,              kbnamelabelW,
     XmNrightAttachment,  XmATTACH_FORM,
     XmNrecomputeSize,              TRUE,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
   XtAddCallback( FRAME_kbnameW, XmNactivateCallback,
       (XtPointer)chooseKbCB, NULL );
   XtManageChild ( FRAME_kbnameW );                                  

   /********   KnowledgeBase-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( FRAME_kbnameW, "kb_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,                 XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,                XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,              XmATTACH_FORM ); n++;
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

   /*******  KnowledgeBase-SelectionMenu-MessageBox-Widget  ******/
   kb_selection_msgW = createMessageBox( kb_selectionW,
                                             "kb_selection_msg" );

   /******** Frame-Name-Knopf ******************/
   FRAME_nameW = XtVaCreateManagedWidget( "frame_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
   XtAddCallback( FRAME_nameW, XmNactivateCallback,
       (XtPointer)frameEditorAnzeigenCB, NULL );
       
   XtManageChild ( FRAME_nameW );                                  
   XtVaGetValues( FRAME_nameW, XmNlabelString, &unnamed_str, NULL );

   /********   Frame-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( FRAME_nameW, "fr_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   fr_selectionW = XmCreateSelectionBox( w, "fr_selection", args, n ); 
   w = XmSelectionBoxGetChild( fr_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   w = XmSelectionBoxGetChild( fr_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtManageChild( fr_selectionW );  
   
   XtAddCallback( fr_selectionW, XmNcancelCallback,
     (XtPointer)fr_cancelCB, NULL );
   XtAddCallback( fr_selectionW, XmNokCallback,
     (XtPointer)fr_selectionCB, NULL );

   /******** Frame-SelectionMenu-MessageBox-Widget ******************/
   fr_selection_msgW = createMessageBox( fr_selectionW,
                                             "fr_selection_msg" );

   /********** LISP-MessageBox-Widget fuer Delete-Warnung ************/
   LISP_Delete_WarnW = createMessageBox( fr_selectionW,
                                             "LISP_Delete_Warnung" );
 
   /********** QuestionBox fuer  Delete-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   DelWarningW = XmCreateQuestionDialog(FRAME_shellW, "DelWarningW",args,n);

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
   LoadWarningW = XmCreateQuestionDialog(FRAME_shellW, "LoadWarningW",args,n);

   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( LoadWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( LoadWarningW, XmNokCallback,
     (XtPointer)LoadWarningokCB, NULL );
   XtAddCallback( LoadWarningW, XmNcancelCallback,
      (XtPointer)LoadWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(LoadWarningW), WM_DELETE_WINDOW,
      (XtPointer)LoadWarningcancelCB, (XtPointer)LoadWarningW );
                                                    
   /*******************************************************************
       Paned-Window fuer zwei Editorfelder 
         (Superframes und Slots) 
   *******************************************************************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                          form1W ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;

   panedwindowW = XmCreatePanedWindow (formW, "paned_window", args, n);
 
   /********* 1. Editorfenster fuer die Superframes *********************/    
   n = 0;
   formsupersW = XmCreateForm( panedwindowW, "formEditorWin", args, n );
   XtManageChild( formsupersW );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   labelsupersW = XmCreateLabel( formsupersW, "Superframes", args, n );
   XtManageChild( labelsupersW );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                   labelsupersW ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNwordWrap,                             True ); n++;
   XtSetArg( args[n], XmNeditMode,                XmMULTI_LINE_EDIT ); n++;

   FRAME_supersW = XmCreateScrolledText( formsupersW, "EditorWin", args, n );
   XtAddCallback( FRAME_supersW, XmNvalueChangedCallback,
     (XtPointer)FRAME_changedCB, NULL );
   
   XtManageChild( FRAME_supersW );

   /********* 2. Editorfenster fuer die Slots *********************/
   /*** Form-Widget  *****/
   n = 0;
   formslotsW = XmCreateForm( panedwindowW, "formEditorWin", args, n );
   XtManageChild( formslotsW );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   labelslotsW = XmCreateLabel( formslotsW, "Slots", args, n );
   XtManageChild( labelslotsW );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                    labelslotsW ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNwordWrap,                             True ); n++;
   XtSetArg( args[n], XmNeditMode,                XmMULTI_LINE_EDIT ); n++;
      
   FRAME_slotsW = XmCreateScrolledText( formslotsW, "EditorWin", args, n );
   XtManageChild( FRAME_slotsW );
   XtAddCallback( FRAME_slotsW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( FRAME_slotsW, XmNvalueChangedCallback,
     (XtPointer)FRAME_changedCB, NULL );
       
   XtManageChild (panedwindowW);   
   FRAME_edit( False ); 

   Frame_Reset();
}            

/****************************************************
*   Change Sensitivity of the Editors               *
*   (accessable from outside...)                    *
****************************************************/

void
FRAME_ready( void ) {

   if( XtIsRealized( FRAME_shellW ) ) {
      XDefineCursor(
         XtDisplay( FRAME_shellW ),
         XtWindow( FRAME_shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
FRAME_busy( void ) {

   if( XtIsRealized( FRAME_shellW ) ) {
      XDefineCursor(
         XtDisplay( FRAME_shellW ),
         XtWindow( FRAME_shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the Editors                   *
****************************************************/

void
FRAME_show( void ) {
XmString neu_kb;

   PopupRaiseAndDeiconify( FRAME_shellW );
}

/***************************************************/

void
FRAME_hide( void ) {

   XtPopdown( FRAME_shellW );
}

/**** EOF ********************************************************************/

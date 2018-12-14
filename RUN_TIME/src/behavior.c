/****************************************************
*   behavior.c    Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the behavior-editor.         *
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
#include "behavior.h" 
#include "help.h"
#include "xinit.h"

/******* GLOBALs *************************************************************/

#define CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

static char *selected_kb=NULL;          /* selected KnowledgeBase */
static char *selected_fr=NULL;          /* selected Frame */
static char *selected_be=NULL;          /* selected behavior */
static char *selected_typ=NULL;          /* selected typ */

static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */
static int  editable;           /* are Texts and OptionMenus editable? */
static int  unsaved_changes;    /* any edits unsaved? */
static Pixmap change_pixmap0, change_pixmap1;
static Widget RowColW;

char *typ_string[] = { ":before", ":primary", ":after" };

extern Widget topLevel;      
static Widget BEHAVIOR_shellW;     
extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;         

static Widget BEHAVIOR_nameW, BEHAVIOR_frameW, BEHAVIOR_variablenW,
       BEHAVIOR_docW, BEHAVIOR_bodyW, BEHAVIOR_typbuttonW[3],
       unsaved_changes_labelW, kbnameW,frameW,behaviorW,
       kb_selectionW, kb_selection_msgW,
       frame_selectionW, frame_selection_msgW,
       behavior_selectionW, behavior_selection_msgW,
       LadenW, LoeschenW, LoadWarningW, DelWarningW,ReChangeW;

static Widget create_typ_buttons( Widget, char*, ArgList, Cardinal );

static void BEHAVIOR_Reset( void );

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
Change_True ( void ) {

   XtVaSetValues( unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap1,
      XmNlabelInsensitivePixmap, change_pixmap1,
      NULL );
   unsaved_changes = True;
   if( selected_kb )
      C_set_modified( selected_kb, MOD_BEHAVIOR, True );
   XtSetSensitive( ReChangeW, True );
}

static
Change_False ( void ) {

   XtVaSetValues( unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap0,
      XmNlabelInsensitivePixmap, change_pixmap0, NULL );
   unsaved_changes = False;
   if( selected_kb )
      C_set_modified( selected_kb, MOD_BEHAVIOR, False );
   XtSetSensitive( ReChangeW, False);
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
BEHAVIOR_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) )
         BEHAVIOR_Reset();
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
         if( strlen(selection) > 0 )
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
BEHAVIOR_edit ( Boolean ability ) {
   XtSetSensitive( BEHAVIOR_docW,   ability );
   XtSetSensitive( BEHAVIOR_variablenW,   ability );
   XtSetSensitive( BEHAVIOR_bodyW,   ability );
   editable = ability;
}

/*******************************************************************************
*   Noch keine Instanz gewaehlt
*******************************************************************************/

static void
No_Behavior_Selected( void ) {

   XtVaSetValues( behaviorW, XmNlabelString, unnamed_str, NULL );
   XtSetSensitive( behaviorW, True );
   BEHAVIOR_edit( False );

   if( selected_be )
      XtFree( selected_be );
   selected_be = NULL;
   Change_False();

   XmTextSetString( BEHAVIOR_docW, "" );
   XmTextShowPosition( BEHAVIOR_docW, 0 );
   XmTextSetString( BEHAVIOR_variablenW, "" );
   XmTextShowPosition( BEHAVIOR_variablenW, 0 );
   XmTextSetString( BEHAVIOR_bodyW, "" );
   XmTextShowPosition( BEHAVIOR_bodyW, 0 );

   XtSetSensitive( LadenW, False );        /* Menus insensitive */
   XtSetSensitive( LoeschenW, False );
}

/*******************************************************************************
*   Noch kein Frame ausgewaehlt
*******************************************************************************/

static void
No_Frame_Selected( void ) {

   XtVaSetValues( frameW, XmNlabelString, unnamed_str, NULL );            
   XtSetSensitive( frameW, True );
   No_Behavior_Selected();
   XtSetSensitive( behaviorW, False );
}

/******************************************************************************/
/*          Reset des Frame-Editors                                           */
/******************************************************************************/

static void
BEHAVIOR_Reset( void ) {

   No_Frame_Selected();
   XtVaSetValues( kbnameW, XmNlabelString, unnamed_str, NULL );
   XtSetSensitive( frameW, False );
   if( selected_kb ) {
      XtFree( selected_kb );
      selected_kb = NULL;
   }      
}

/******************************************************************************
*  Einlesen der Definition eines Behaviors
******************************************************************************/

void 
emaxps_behavior_definition( void ) {

   char *name, *frame, *variablen, *typ, *doc ,*body, *p, *v[5], *str;
   static char *typ_string[] = { ":before",":primary",":after"  };
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
      fprintf( stderr, "behavior-def: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "behavior-def: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   name      = v[i++];
   typ       = v[i++];
   variablen = v[i++];
   doc       = v[i++];
   body      = v[i++];
   /* Typ-Togglebuttons */                                                                    
   for( i = 0; i < (int) XtNumber( typ_string ); i++ ) { 
     if( strstr ( typ, typ_string[i] ) != NULL ) {
         XmToggleButtonGadgetSetState( BEHAVIOR_typbuttonW[i], True, True ); 
     }
   }
   if (selected_be)
      XtFree( selected_be );
   selected_be= XtMalloc( 1+strlen( name ) );
   strcpy( selected_be,name );   
   if (selected_typ)
    XtFree( selected_typ );
   selected_typ= XtMalloc( 1+strlen( typ ) );
   strcpy( selected_typ,typ );
   
   XtVaSetValues( behaviorW, XmNlabelString, XmStringCreateSimple( name ), NULL );
   XmTextSetString( BEHAVIOR_variablenW, variablen );
   XmTextSetString( BEHAVIOR_docW,             doc );
   XmTextSetString( BEHAVIOR_bodyW,           body );
   
   BEHAVIOR_edit(True);
   Change_False();
   
   XtSetSensitive( BEHAVIOR_shellW, True );
   XtSetSensitive( LadenW, True );        /* Menus sensitive */
   XtSetSensitive( LoeschenW, True );       
   XtFree( p );
}     

/*******************************************************************************
*   LISP sends Frames for a Selection-Box
*******************************************************************************/

void 
emaxps_behav_frame_select ( void ) {
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
*   LISP sends Behaviors for a Selection-Box
*******************************************************************************/

void 
emaxps_behav_behavior_select ( void ) {
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
    XtVaSetValues( behavior_selectionW,
      XmNlistItems,  xmstr_list,
      XmNlistItemCount, nr,
      NULL );

   XtFree( str ); 
   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   XtManageChild( XtParent(behavior_selectionW) );
}

/*****************************************************************************
*   Callback fuer den Menuegruppe "Anzeigen" im Behavioreditor 
******************************************************************************/

void 
behaviorEditorAnzeigenCB ( void ) {

   LISP_sndCmd("(hole-behavior-definition-mit-menue) "); 
}

/******************************************************************************
*   Laden-Callback fuer den Behavioreditor 
******************************************************************************/

void 
behaviorEditorLadenCB ( void ) {

 XtManageChild( LoadWarningW );
}

/******************************************************************************
*   Quit-Callback fuer den Behavioreditor 
******************************************************************************/

void 
behaviorEditorQuitCB ( void ) {
                               
   XtUnrealizeWidget( BEHAVIOR_shellW );  
} 

/*****************************************************************************
*   Callback fuer den Menuepunkt "Behavior loeschen .." im Behavioreditor 
******************************************************************************/

void 
behaviorEditorLoeschenCB ( void ) {

   XtManageChild( DelWarningW );
}

/*******************************************************************************
*   Callback of Cancel-Button in DeleteWarningBox
*******************************************************************************/

static void
DelWarningcancelCB( void ) {

   XtUnmanageChild( DelWarningW );
}

/*******************************************************************************
*   Callback of OK-Button in DeleteWarningBox
*******************************************************************************/

static void
DelWarningokCB( void ) {
char *buf;
char typ[8];
int i;
             


  if (selected_be) {
   for( i = 0; i < (int) XtNumber( typ_string ); i++ ) { 
      if ( XmToggleButtonGadgetGetState( BEHAVIOR_typbuttonW[i] ) == True ) {
         strcpy( typ, typ_string[i] );
      }  
   }
   if (!strcmp(typ,":primary"))
      strcpy(typ," ");


     buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr)
                          + strlen(selected_be) );
     sprintf( buf, "(loesche-behavior  '%s '(%s %s :%s)) ", selected_kb,selected_fr,typ,selected_be );
     LISP_sndCmd( buf );     
     XtFree( buf );
   }
   No_Behavior_Selected();               
                               /* Delete List-Entry */
   XmTextSetString( XmSelectionBoxGetChild( behavior_selectionW, XmDIALOG_TEXT ) , "" );
   XtUnmanageChild( XtParent( DelWarningW ) );
}

/*******************************************************************************
*   Callback if actual instance is edited
*******************************************************************************/

static void 
BEHAVIOR_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {

   if( !editable )
      return;
   Change_True();
}

/*******************************************************************************
*   Callback of Rechange-Button  
*******************************************************************************/

static void
behaviorEditorRechangeCB( void ) {

char *buf;

  if (selected_be) {
     buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr)
                          + strlen(selected_typ) + strlen(selected_be) );
     sprintf( buf, "(hole-behavior-definition  '%s '(%s %s :%s)) ",
                   /* selected = :primary already !!! --^ */
                   selected_kb,selected_fr,selected_typ,selected_be );
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
   char *name;    
   char *framename; 
   char typ[8];
   char *variablen;
   char *doc;
   char *body;                            
   int i;
   variablen = XmTextGetString( BEHAVIOR_variablenW );     
   doc       = XmTextGetString( BEHAVIOR_docW );     
   body      = XmTextGetString( BEHAVIOR_bodyW );
   
   buf       = XtMalloc( strlen( selected_fr ) + strlen( typ ) + strlen( selected_be ) +
                         strlen( variablen ) + strlen( doc ) + strlen( body ) + 256 );            
   for( i = 0; i < (int) XtNumber( typ_string ); i++ ) { 
      if ( XmToggleButtonGadgetGetState( BEHAVIOR_typbuttonW[i] ) == True ) {
         strcpy( typ, typ_string[i] );
      }  
   }
   sprintf( buf, "(my-defbehavior %s (%s %s :%s) (%s) \"%s\" %s) \n",  /* \n to run! */
            selected_kb,selected_fr, typ, selected_be,    variablen, doc, body );
   /*******Nachricht ueber die toplevel-Pipe an LISP senden **********/
   LISP_sndCmd( buf );
   XtFree( buf );
   XtFree( variablen );                                     
   XtFree( doc );                                     
   XtFree( body );
   Change_False();
}  


/*****************************************************************************
*
*   Frame-Button-Callback
*
******************************************************************************/
static void
framebuttonCB ( void ) {
  char *buf;
  buf =  XtMalloc( 256 + strlen(selected_kb) );
  sprintf( buf, "(behavior-hole-frames  '%s )", selected_kb);
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
   No_Behavior_Selected();
   XtVaSetValues( frameW, XmNlabelString,  CSTRING(selected_fr) , NULL );                  

}


/*******************************************************************************
*   Callback of Cancel-Button in Frame-SelectionMenu
*******************************************************************************/

static void
fr_cancelCB( void ) {
   XtUnmanageChild( XtParent( frame_selectionW ) );
   if (editable) 
      BEHAVIOR_edit(True);
      
       
}



/*****************************************************************************
*
*   Behavior-Button-Callback
*
******************************************************************************/
void
behaviorbuttonCB ( void ) {
  char *buf;
  buf =  XtMalloc( 256 + strlen(selected_kb) + strlen(selected_fr) );
  sprintf( buf, "(hole-behavior-definition-mit-menue '%s '%s)", selected_kb, selected_fr);
  LISP_sndCmd ( buf );
  XtFree( buf );


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
   if (selected_kb)
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
*   Callback of Ok-Button in Behavior-SelectionMenu
*******************************************************************************/

static void 
be_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;
   if( !(selection = selectionMenuOk( behavior_selection_msgW, cbs )) ) {
      LISP_sndReply( "nil" );
      return;
   }
   XtUnmanageChild( XtParent( behavior_selectionW ) );
   buf =  XtMalloc(  256 + strlen(selection) );
   sprintf( buf, "(%s)", selection);
   LISP_sndReply ( buf );
   XtFree( buf );  
   XtFree( selection );
}




/*******************************************************************************
*   Callback of Cancel-Button in Instance-SelectionMenu
*******************************************************************************/

static void
be_cancelCB( void ) {

   LISP_sndReply( "nil" );
   XtUnmanageChild( XtParent( behavior_selectionW ) );
   if (editable)
      BEHAVIOR_edit(True);
}

                                                                               

/******************************************************************************
* create-Funktionen
******************************************************************************/

/******************************************************************************
*
*   Erzeugen des Behavioreditors 
*
******************************************************************************/
void 
BEHAVIOR_create( Widget parent ) {
   Widget form, w1 ,w2 ,w3,w,form1W;
   Widget frame, label;  /* Ueberschrift */
   Widget menubarW, helpMenu;
   Widget panedwindow;
   Widget typ_buttons; 
   Widget form1, form2, form_body;  
   Widget kbnamelabelW, framelabelW, behaviorlabelW; 
   Widget form_variablen, label_variablen; 
   Widget form_doc, label_doc; 
   Widget label_typ, label_body;
   register int n;
   Arg      args[20];
   XmString cstring;

   /************ Hauptfenster **********************************/
   n = 0;
   XtSetArg( args[n], XmNdeleteResponse,          XmDO_NOTHING ); n++; 
   XtSetArg( args[n], XmNiconPixmap,                    iconPm ); n++; 

   BEHAVIOR_shellW = XtAppCreateShell( "Behavior-Editor", "HereClassIsIgnored", 
                              topLevelShellWidgetClass,
                              XtDisplay( parent ), args, n );                       

  /* Callback fuer den CLOSE-Button im Systemmenue (MWM) einrichten */
  XmAddWMProtocolCallback( BEHAVIOR_shellW, WM_DELETE_WINDOW, 
                            (XtPointer)BEHAVIOR_hide, (XtPointer)NULL );

   /********** Form-Widget erzeugen ********************************/
   n = 0;
   form = XmCreateForm( BEHAVIOR_shellW, "Behavioreditor", args, n );
   XtManageChild( form );   

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( form, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BEHAVIOR_MENU );

   /****** Menue-Leiste erzeugen **********************************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,        XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,       XmATTACH_FORM ); n++;

   menubarW = XmCreateMenuBar ( form, "menubar", args, n );
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
                    (XtPointer)behaviorEditorLadenCB, NULL );
   LoeschenW = XtVaCreateManagedWidget( "Behaviorloeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LoeschenW, XmNactivateCallback,
                    (XtPointer)behaviorEditorLoeschenCB, NULL );
   ReChangeW = XtVaCreateManagedWidget( "ReChange", 
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(ReChangeW, XmNactivateCallback,
        (XtPointer)behaviorEditorRechangeCB, NULL );
   XtSetSensitive( ReChangeW, False);                                                                         

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
                    (XtPointer)HELP_show, (XtPointer)HELP_BEHAVIOR_MENU );
   XtManageChild( menubarW );

   w = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, w1, NULL );
   XtAddCallback( w, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   /*****************************************************************
       Form-Widget fuer Label-Widget zur Anzeige des Behaviornamens 
       und des Framenamens erzeugen 
   ******************************************************************/
   n = 0;                                          
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     menubarW ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNhorizontalSpacing,                    3 ); n++;
   XtSetArg( args[n], XmNverticalSpacing,                      3 ); n++;
 
   form1W = XmCreateForm( form, "form", args, n );
   XtManageChild( form1W );

   /***************** unsaved-Label-Widget *****************/
   unsaved_changes_labelW = XtVaCreateWidget( "unsaved_changes_label",
      xmLabelWidgetClass, form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,      XmATTACH_FORM,
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
   behaviorlabelW = XmCreateLabel( RowColW, "Behavior", args, n );
   XtManageChild( behaviorlabelW );

   /*********** Typ-Label ********************************************/
   n = 0;
   label_typ = XmCreateLabel( RowColW, "Typ", args, n );
   XtManageChild( label_typ );

   /******** KB-Name-Knopf ******************/
   kbnameW = XtVaCreateManagedWidget( "kb_name",
      xmPushButtonWidgetClass, RowColW,
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

   /********    Behavior-Knopf   ******************/
   behaviorW = XtVaCreateManagedWidget( "in_name",
      xmPushButtonWidgetClass, RowColW,
      XmNnavigationType,       XmTAB_GROUP,
      NULL );
 
   XtAddCallback( behaviorW, XmNactivateCallback,
       (XtPointer)behaviorbuttonCB, NULL );
       
   XtManageChild ( behaviorW );                                  
   XtVaGetValues(behaviorW, XmNlabelString, &unnamed_str, NULL );

   /********   Behavior-SelectionMenu-Widget   ********/
      n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( behaviorW, "be_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   behavior_selectionW = XmCreateSelectionBox( w, "be_selection", args, n ); 
   w = XmSelectionBoxGetChild( behavior_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   w = XmSelectionBoxGetChild( behavior_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtManageChild(behavior_selectionW );  

   XtAddCallback( behavior_selectionW, XmNcancelCallback,
     (XtPointer)be_cancelCB, NULL );
   XtAddCallback( behavior_selectionW, XmNokCallback,
     (XtPointer)be_selectionCB, NULL );
   
   /*******  Instance-SelectionMenu-MessageBox-Widget  ******/
   behavior_selection_msgW = createMessageBox( behavior_selectionW,
                                        "be_selection_msg" );

   /************* 1. Knopffeld **********************************************/
   n = 0;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
  
   typ_buttons = create_typ_buttons( RowColW, "typ_buttons", args, n );
   XtManageChild( typ_buttons );

   /********** QuestionBox fuer  Delete-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   DelWarningW = XmCreateQuestionDialog(BEHAVIOR_shellW, "DelWarningW",args,n);

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
   LoadWarningW = XmCreateQuestionDialog(BEHAVIOR_shellW, "LoadWarningW",args,n);

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
       Paned-Window fuer drei Editorfelder 
         (Variablen, Dokumentation und Rumpf) 
   *******************************************************************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                          form1W ); n++;
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;

   panedwindow = XmCreatePanedWindow ( form, "panedwindow", args, n );
 
   /********* 1. Editorfenster fuer die Variablen *********************/    
   n = 0;
   form_variablen = XmCreateForm( panedwindow, "formEditorWin", args, n );
   XtManageChild( form_variablen );
   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,                XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   label_variablen = XmCreateLabel( form_variablen, "Variablen", args, n );
   XtManageChild( label_variablen );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                 label_variablen ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   BEHAVIOR_variablenW = XmCreateScrolledText( form_variablen, "EditorWin", args, n );
   XtAddCallback( BEHAVIOR_variablenW, XmNvalueChangedCallback,
            (XtPointer)BEHAVIOR_changedCB, NULL );
   XtManageChild( BEHAVIOR_variablenW );

   /********* 2. Editorfenster fuer den Kommentar-String *****************/    
   n = 0;
   form_doc = XmCreateForm( panedwindow, "formEditorWin", args, n );
   XtManageChild( form_doc );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,                XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   label_doc = XmCreateLabel( form_doc, "Kommentar", args, n );
   XtManageChild( label_doc );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                       label_doc ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   BEHAVIOR_docW = XmCreateScrolledText( form_doc, "EditorWin", args, n );
   XtAddCallback( BEHAVIOR_docW, XmNvalueChangedCallback,
            (XtPointer)BEHAVIOR_changedCB, NULL );
   XtManageChild( BEHAVIOR_docW );

   /********* 3. Editorfenster fuer den Rumpf *********************/
   n = 0;
   form_body = XmCreateForm( panedwindow, "formEditorWin", args, n );
   XtManageChild( form_body );

   /* Ueberschrift fuer das Editorfenster */
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,              XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,                XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   label_body = XmCreateLabel( form_body, "Rumpf", args, n );
   XtManageChild( label_body );

   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,              XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                      label_body ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   BEHAVIOR_bodyW = XmCreateScrolledText( form_body, "EditorWin", args, n );
   XtAddCallback( BEHAVIOR_bodyW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( BEHAVIOR_bodyW, XmNvalueChangedCallback,
            (XtPointer)BEHAVIOR_changedCB, NULL );
   XtManageChild( BEHAVIOR_bodyW );
   XtManageChild ( panedwindow );   

   BEHAVIOR_Reset();
}            
                                   
/****************************************************
*   Change Sensitivity of the Editors               *
*   (accessable from outside...)                    *
****************************************************/

void
BEHAVIOR_ready( void ) {

   if( XtIsRealized( BEHAVIOR_shellW ) ) {
      XDefineCursor(
         XtDisplay( BEHAVIOR_shellW ),
         XtWindow( BEHAVIOR_shellW ),
         cursor_normal
      );
   }
}

/***************************************************/

void
BEHAVIOR_busy( void ) {

   if( XtIsRealized( BEHAVIOR_shellW ) ) {
      XDefineCursor(
         XtDisplay( BEHAVIOR_shellW ),
         XtWindow( BEHAVIOR_shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the Editors                   *
****************************************************/

void
BEHAVIOR_show( void ) {

   PopupRaiseAndDeiconify( BEHAVIOR_shellW );
}

/***************************************************/

void
BEHAVIOR_hide( void ) {

   XtPopdown( BEHAVIOR_shellW );
}


/******************************************************************************
*  Typ-Button-Leiste fuer den Behavioreditor 
******************************************************************************/  

static Widget 
create_typ_buttons( Widget parent, char* name, ArgList argl, Cardinal argc ) {
   Widget radioBoxW;
   Arg    args[10];
   int    i, n;
   XmString cstring;    
       
   n = argc;
   XtSetArg( argl[n], XmNpacking,            XmPACK_COLUMN ); n++;
   XtSetArg( argl[n], XmNorientation,         XmHORIZONTAL ); n++;
   XtSetArg( argl[n], XmNadjustLast,                 False ); n++;
 
   radioBoxW = XmCreateRadioBox( parent, "radio", argl, n );
   XtManageChild( radioBoxW );
   
   for( i = 0; i < (int)XtNumber(typ_string); i++ )  {
      n = 0;
      if( i == 1 ) {  /* :primary-ToggleButton "ein"-schalten */                                    
         XtSetArg( args[n], XmNset, True ); n++; 
      }
      else {
         XtSetArg( args[n], XmNset, False ); n++;
      }
      cstring = XmStringCreate( typ_string[i], XmSTRING_DEFAULT_CHARSET );
      XtSetArg( args[n], XmNlabelString,        cstring ); n++;  
      BEHAVIOR_typbuttonW[i] = XmCreateToggleButtonGadget( radioBoxW, 
                                             "typbutton", args, n );
        XtAddCallback( BEHAVIOR_typbuttonW[i], XmNvalueChangedCallback,
            (XtPointer)BEHAVIOR_changedCB, NULL ); 
      XmStringFree( cstring );   
   }
   XtManageChildren( BEHAVIOR_typbuttonW, 3 );
   return( radioBoxW );
}

/**** EOF ********************************************************************/

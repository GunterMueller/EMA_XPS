/****************************************************
*   prolog.c      Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the prolog-editor.           *
*   Full version only.                              *
****************************************************/

#include <stdio.h>

#include <Xm/Form.h> 
#include <Xm/Protocols.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>

#include "uiw.h"
#include "main.h"
#include "prolog.h"
#include "help.h"
#include "xinit.h"

/*** Globals ***************************************/

#define  FIXED         0
#define  FREE          1

#define  FIRST         0
#define  LAST          1
#define  BEFORE        2
#define  CSTRING(s) XmStringCreateLtoR(s,XmSTRING_DEFAULT_CHARSET)

/*** Typedefs **************************************/

typedef struct _axioms {          /* Enthaelt alle Axiome */
   int  L_modified;
   char *name;
   struct _axioms *next;
} AXIOMS;

typedef struct _kbax {            /* Enthaelt die Axiome pro KB */
   char *name; 
   struct _kbax *next;
} KBAX;

typedef struct _kb_info {        /* Enthaelt aktuelle KBs mit Zeiger auf Axiome */
   char *name;
   KBAX *axiome;
} KBINFO;


static KBINFO *KBI[10]; /* Welche KB enthaelt welche Axiome */ 
static char *wb[10];            
static Pixmap change_pixmap0, change_pixmap1;

static Widget nameW ;     /* name of set of clauses */
static int    typ;                /* FREE or FIXED */
static int LadenFlag;
static Widget klauselnW;                /* clauses */
static Widget namelabelW, Zu_namelabelW;
static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */

static Widget shellW,zuordW,PromptW;
static int editable,unsaved_changes, zu_unsaved_changes;
static char *selected_kb=NULL;          /* selected KnowledgeBase */
static char *selected_kl=NULL;          /* selected clausel   */
static Widget  unsaved_changes_labelW, zu_unsaved_changes_labelW, kbnameW,
        kb_selectionW, kb_selection_msgW,
        LadenW, LoeschenW,BeforeW,AfterW, BackW,
        kl_selectionW, kl_selection_msgW,
        DelWarningW, LoadWarningW, ReChangeW;

static Widget Zu_shellW, Zu_LadenW, OneOfManyW1, OneOfManyW2 ,OneOfManyW3; 
static char *list1_sel, *list2_sel, *list3_sel;
static Widget ZuVerwerfW, ZuUebernehmW;
static int axiom_count=0;
static int verbl_nr,frei_nr;
static AXIOMS*  Axioms=NULL;
static XmString loadtextxm;
extern Atom WM_DELETE_WINDOW;
extern Pixmap iconPm;

/*** Function Declarations *************************/

static char *GetNextPartStr( char* );
static void  prologEditorLadenCB( void );
static void  prologEditorPrologloeschenCB( void );

/*** Functions *************************************/

static void
cut_brackets( char *str ) {
   char *p, *buf;
   int c, i, k;

   buf = (char *)XtMalloc( 1 + strlen(str) );
   p = str;
   i = 0;
   k = 0;
   while( c = *p++ ) {
      if( c == '(' ) {
         k++;
         if( k == 1 )
            continue;
      }
      if( c == ')' ) {
         k--;
         if( !k )
            continue;
      }
      buf[i++] = c;
   }
   buf[i] = 0;
   strcpy( str, buf );
   XtFree( buf );
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

static void
LISP_Change_kb( char *kb ) {
   char *buf;

   buf = XtMalloc( 256 );
   sprintf( buf, "(emaxps-kb-set-modified '%s)",kb);
   LISP_sndCmd( buf ); 
   XtFree( buf );   
}

/***************************************************/

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
*  Search for KB-Names with selected clause
******************************************************************************/

static int
Search_Kbnames_with_axiom( void ) {
   static int i;
   static KBAX *kbax,*kbbax1;
   static char *kbname;
   static int kbnr;

   kbnr=0;
   i=0;
   while(i !=10 ){                        /* KB suchen */
      if (wb[i])
         XtFree(wb[i]);
      if (KBI[i]){
         if (KBI[i]->name) 
            kbname=KBI[i]->name;
         kbax=(KBI[i]->axiome);
         while (kbax){
            if (!strcmp(selected_kl,kbax->name)){
/*printf("test: %s\n",kbname);*/
               if (wb[kbnr])
                  XtFree(wb[kbnr]);
               wb[kbnr]=(char*) XtMalloc(1+strlen(kbname));
               strcpy(wb[kbnr],kbname);
               kbnr++;
            }
            kbax=kbax->next;
         } 
      }
      i++;  
   } 
   return kbnr;
}

/******************************************************************************
*  Prolog-Editor-Change
******************************************************************************/

static void
Change_True( void ) {
   int i,kbnr;

   if( !unsaved_changes ) {
      kbnr = Search_Kbnames_with_axiom();
      for( i = 0; i != kbnr; i++ )
         C_set_modified( wb[i], MOD_PROLOG, True );
   }
   XtVaSetValues( unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap1,
      XmNlabelInsensitivePixmap, change_pixmap1,
      NULL );
   unsaved_changes = True;                    
   XtSetSensitive( LadenW, True );
   XtSetSensitive( ReChangeW, True );
}

static
Change_False( void ) {

   XtVaSetValues( unsaved_changes_labelW,
      XmNlabelPixmap, change_pixmap0,
      XmNlabelInsensitivePixmap, change_pixmap0,
      NULL );
   unsaved_changes = False;                    
   XtSetSensitive( LadenW, False );
   XtSetSensitive( ReChangeW, False );
}

/*******************************************************************************
*   Change Editability of Editors
*******************************************************************************/

static void 
edit( Boolean ability ) {

   XtSetSensitive( klauselnW, ability );
   editable = ability;
}

/*******************************************************************************
*   Prolog-Auswahl loeschen
*******************************************************************************/

static void
Axiom_NoSelect( void ) {

   XtVaSetValues( nameW, XmNlabelString, unnamed_str, NULL );
   edit( False );
   Change_False();
   if( selected_kl ) {
      XtFree( selected_kl );
      selected_kl = NULL;   
   }
   XmTextSetString( klauselnW, "" );
   XmTextShowPosition(klauselnW, 0 );  /* Clear windows */
   XtSetSensitive( LadenW, False );        /* Menus insensitive */
   XtSetSensitive( LoeschenW, False ); 
   XtSetSensitive( ReChangeW, False ); 
}

/*******************************************************************************/

void                             /* Prints die KB-abhaengigen Axiom-Structs */ 
Print_KB_Axioms( void ) {
   static KBAX *ax,*axneu;
   static int i;

   for( i = 0; i < 10; i++ )      /* Alle KBs durchlaufen */
   if (KBI[i]){
      axneu = KBI[i]->axiome;
/*printf("Wissensbasis %s:\n",KBI[i]->name);*/
      while ( axneu ) {
         if (axneu->name) 
            axneu=axneu->next;        
      }                     
   } 
}

/*******************************************************************************/

void
Reset_Prolog_Kb( char* kbn ) {
   char *kb_name, *axiom,  *p, *v[3], *str;
   int i,success,  err, ret, first;
   KBAX *ax,*axneu;
   KBINFO *kbin,*kb_in;

   first=0;
   i = 0;
   success = False;
   while(i !=10 && !success){                        /* KB suchen */
      if (KBI[i]){
         if (KBI[i]->name){
            if (!strcmp(KBI[i]->name,kbn))
               success=True;
         } 
      } else {
         first=i;
      }
      i++;
   }
   i--;
   if (KBI[i] && success ){
      ax=KBI[i]->axiome;
      while (ax->next){ 
         axneu=ax->next;
         XtFree((char*)ax);
         ax=axneu;
      }    
      if (ax){
         XtFree((char*)ax);
         ax=NULL;
      }   
      if (KBI[i]){
         XtFree((char*)KBI[i]);
         KBI[i]=NULL;
      }
   }
}   

/*******************************************************************************
*   Reset a KB
*******************************************************************************/

void 
emaxps_reset_prolog_of_kb( void ) {
   char *kb_name, *axiom,  *p, *v[3], *str;
   int i,success,  err, ret, first;
   KBAX *ax,*axneu;  
   KBINFO *kbin,*kb_in;

   v[0] = p = MSG_read();
   i = 0;
   kb_name  = v[i++];
   Reset_Prolog_Kb( kb_name);    
   XtFree(p);
}

/*******************************************************************************
*   LISP sends a kb-axiom-update
*******************************************************************************/

void 
Prolog_Neues_Axiom( char* kb_name, char* axiom ) {
   char *str;
   int i,success,  err, ret, first;
   KBAX *ax,*axneu;  
   KBINFO *kbin,*kb_in;

   first=0;
   i = 0;
   success = False;
   while(i !=10 && !success){                        /* KB suchen */
      if (KBI[i]){
         if (KBI[i]->name){
            if (!strcmp(KBI[i]->name,kb_name))
               success=True;
         } 
      } else {
         first=i;
      }
      i++;
   }
   if (!success){                                  /* Neuer KB-Eintrag */
      kbin=(KBINFO*)XtMalloc( sizeof(KBINFO) ); KBI[first]=kbin;
      KBI[first]->name=(char*)XtMalloc( strlen(kb_name)+1 );
      strcpy(KBI[first]->name,kb_name); 
      ax=(KBAX*)XtMalloc( sizeof(KBAX) );
      KBI[first]->axiome=ax;
      KBI[first]->axiome->name = (char*)XtMalloc(1+strlen(axiom));
      strcpy( KBI[first]->axiome->name,axiom);
      KBI[first]->axiome->next = NULL;
   } else {
      i--;
      ax=KBI[i]->axiome;
      while (ax->next){ 
         ax=ax->next;
      }    
      axneu=(KBAX*)XtMalloc( sizeof(KBAX) ); 
      axneu->name = (char*)XtMalloc(1+strlen(axiom));
      
      strcpy( axneu->name,axiom);
      axneu->next = NULL;
      ax->next=axneu;
   }    
}

/*******************************************************************************
*   LISP sends a kb-axiom-update
*******************************************************************************/

void 
emaxps_new_kbaxiom( void ) {
   char *kb_name, *axiom,  *p, *v[3], *str;
   int i,success,  err, ret, first;

   v[0] = p = MSG_read();
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=GetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   i = 0;
   kb_name = v[i++];
   axiom   = v[i++];
   Prolog_Neues_Axiom( kb_name, axiom );
   XtFree( p );
}

void 
emaxps_del_kbaxiom( void ) {

   /* dummy */
}

/*******************************************************************************
*   LISP sends a new axiom-list
*******************************************************************************/

void
emaxps_new_axioms( void ) {
   char *str, *s, *s0, *hdl,*s1;
   int c, i, len, nr;
   static AXIOMS *axneu; 
   static AXIOMS *ax;

   ax = Axioms;                   /* Delete Axiom-Array */
   while (ax){
      axneu = ax->next;
      XtFree((char*)ax);
      ax=axneu;
   }
   Axioms=NULL;

   s0 = s = str = MSG_read();
   len = strlen( str );
   ax = Axioms;
   nr = 0;
   while( c = *s++ ) {          /* LISP-Texte holen */
      if( c == ETX ) {
         *(s-1) = NUL;
         nr++;
         axneu = (AXIOMS*)XtMalloc( sizeof(AXIOMS) );
         if( ax )
            ax->next = axneu;
         else
            Axioms = axneu;
         ax = axneu;
         ax->next = NULL;
         s1 = (char*)XtMalloc( strlen(s0)+1 );
         ax->name = s1;
         strcpy( ax->name, s0 );
         axiom_count++;                            
         s0 = s;
      }
   }
   XtFree( str ); 
}

/****************************************************
*   LISP->C: new set of clauses                     *
*   --> dispatch.c                                  *
****************************************************/

void 
emaxps_prolog_klauselmengen( void ) {
   char *k_name, *klausel, *art, *p, *v[3], *str;
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
      fprintf( stderr, "prolog: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( GetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "prolog: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   k_name  = v[i++];
   art     = v[i++];
   cut_brackets( v[i] );     /* see rule.c ! */
   klausel = v[i++];
   if (selected_kl)
     XtFree(selected_kl);
     selected_kl= XtMalloc(1+strlen( k_name ));
         strcpy( selected_kl,k_name );
  
   XtVaSetValues( nameW, XmNlabelString, XmStringCreateSimple( selected_kl ), NULL );

   XmTextSetString( klauselnW,  klausel );
   Change_False();
   edit(True);
   XtSetSensitive( LoeschenW, True);
   XtFree( p );
}

/******************************************************************************
*   LISP sends clauses for a selection-box
******************************************************************************/

void 
emaxps_prolog_select( void ){
   AXIOMS *p;
   int nr;
   XmStringTable xmstr_list;

   XtFree( MSG_read() );

   xmstr_list = (XmString *)XtMalloc( 1024 );
   p=Axioms;
   
   nr = 0;
   while (p) {
      nr++;
      xmstr_list[nr-1] = XmStringCreateSimple( p->name );
      p=p->next;
   }         
    xmstr_list[nr]=NULL;

    XtVaSetValues( kl_selectionW,    /* ins Fenster schreiben */
       XmNlistItems, xmstr_list,
       XmNlistItemCount, nr, 
       NULL );

   while( nr >0 )                         /* Liste wieder frei machen */
      XmStringFree( xmstr_list[--nr] );
   XtFree( (char*)xmstr_list );
   
   XtManageChild( XtParent(kl_selectionW) );
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


/*******************************************************************************
*   Callback if actual axiom is edited
*******************************************************************************/

static void 
Axiom_changedCB( Widget w, XtPointer unused, XmAnyCallbackStruct *cbs ) {
   static Widget last_junctor, last_actiontype;

   if( !editable )
      return;
   Change_True();
}

/*******************************************************************************
*   Callback of Cancel-Button in Klausel-SelectionMenu
*******************************************************************************/

static void
kl_cancelCB( void ) {
   LISP_sndReply( "nil" );
   XtUnmanageChild( XtParent( kl_selectionW ) );
}

/*******************************************************************************
*   Callback of OK-Button in Klausel-SelectionMenu
*******************************************************************************/

static void
kl_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {
   char *buf;
   char *selection = NULL;
   int No_Change = False;
   XmString selected_cstr;
							
   if( !(selection = selectionMenuOk( kl_selection_msgW, cbs )) ) {
      return;
   }

   XtUnmanageChild( XtParent( kl_selectionW ) );
   buf = XtMalloc( strlen(selection) + 64 );
   sprintf( buf, "(send-kb :hole-klauselmengen-definition '%s)",selection);
   LISP_sndCmd( buf ); 
   XtFree( buf );
}

/***************************************************/

static void 
prologEditorLadenCB( void ) {
   char *p, *klauselmengenname, *klauseln, *ff;
   int i;

   klauseln = XmTextGetString( klauselnW );
   p = XtMalloc( strlen(klauseln) + 1024 );

   LadenFlag=True;
   
   sprintf( p, "(assert-axioms '%s  '(%s))",
         selected_kl,  klauseln );
   LISP_sndCmd( p ); 
   XtFree( p );
   XtFree( klauseln );
 
}

/***************************************************/

static void 
prologEditorPrologloeschenCB( void ) {
   static int i,kbnr;
   static KBAX *kbax,*kbbax1;
   static XmString xms,xms1;
   static char *kbname,*errstr,*tmp;
   char *buf;

   errstr=(char*)XtMalloc(1024);
 
   kbnr=Search_Kbnames_with_axiom(); 
   if (kbnr!=0) {
      for( i = 0; i < kbnr ; i++ ) {
         errstr=strcat(errstr,wb[i]);
         if (i!=kbnr-1)
            errstr=strcat(errstr,", ");
         else 
            errstr=strcat(errstr,".");
      }
      xms=XmStringConcat(loadtextxm,XmStringCreateSimple( errstr));
      XtVaSetValues(DelWarningW,XmNmessageString,xms,NULL);
      XtManageChild(DelWarningW);
   } else {
      buf = XtMalloc( strlen(selected_kl) + 64 );
      sprintf( buf, "(loesche-klauselmenge '%s) ",selected_kl );
      LISP_sndCmd( buf ); 
      XtFree( buf );
      Axiom_NoSelect();
   }
}

/***************************************************/

static void 
prologEditorMengeloeschenCB( void ) {
   LISP_sndCmd( "(send-kb :loesche-klauselmenge)" );
} 

/***************************************************/

static void 
prologEditorSelektierenCB( void ) {
   LISP_sndCmd( "(send-kb :klauselmenge-selektieren)" );
} 

/***************************************************/

static void 
prologEditorEntfernenCB( void ) {
   LISP_sndCmd( "(send-kb :entferne-aktuelle-klauselmenge)" );
} 

/***************************************************/

static void 
prologEditorHinzufuegenCB( Widget w, int modus ) {

   switch( modus ) {
      case FIRST:
         LISP_sndCmd("(send-kb :klauselmenge-hinzufuegen 'first)");
         break;
      case LAST:
         LISP_sndCmd("(send-kb :klauselmenge-hinzufuegen 'last)");
         break;
      case BEFORE:
         LISP_sndCmd("(send-kb :klauselmenge-hinzufuegen 'before)");
         break;
   }
} 

/***************************************************/

static void
DelWarningokCB( void ) {

   /* dummy */
}

/***************************************************/

static void
DelWarningcancelCB( void ) {

   /* dummy */
}

/***************************************************/

static void                                          
LoadWarningokCB( void ) {                                
   char *p, *klauselmengenname, *klauseln, *ff;

   klauseln = XmTextGetString( klauselnW );
   p = XtMalloc( strlen(klauseln) + 1024 );
   sprintf( p, "(assert-axioms '%s  '(%s))",
         selected_kl,  klauseln );
   LISP_sndCmd( p ); 
   XtFree( p );
   XtFree( klauseln );
}    

/***************************************************/

static void                                          
LoadWarningcancelCB( void ) {                                
   /* dummy */
}    

/***************************************************/
static void
prologEditorRechangeCB( void ) {
   char *buf;
   int kbnr,i;  

   kbnr=Search_Kbnames_with_axiom();
   i=0;  
   while (i!=kbnr){
      C_set_modified( wb[i], MOD_PROLOG, False );
      i++;
   }
   buf = XtMalloc( strlen(selected_kl) + 64 );
   sprintf( buf, "(send-kb :hole-klauselmengen-definition '%s)",selected_kl);
   LISP_sndCmd( buf ); 
   XtFree( buf );
}

/******************************************************************************
*  Prolog-Zuordnungs-Editor-Change
******************************************************************************/

static void
Zu_Change_True( void ) {

   XtVaSetValues( zu_unsaved_changes_labelW,
   XmNlabelPixmap, change_pixmap1,
   XmNlabelInsensitivePixmap, change_pixmap1, NULL );
   if (!zu_unsaved_changes)
      if (selected_kb)   
         C_set_modified( selected_kb, MOD_PROLOG, True );
   zu_unsaved_changes = True;                    
   XtSetSensitive(ZuVerwerfW , True);
   XtSetSensitive(ZuUebernehmW , True);
}

static
Zu_Change_False( void ) {

   XtVaSetValues( zu_unsaved_changes_labelW,
   XmNlabelPixmap, change_pixmap0,
   XmNlabelInsensitivePixmap, change_pixmap0, NULL );
   zu_unsaved_changes = False;                    
   if( selected_kb )
      C_set_modified( selected_kb, MOD_PROLOG, False );
   XtSetSensitive(ZuVerwerfW , False );
   XtSetSensitive(ZuUebernehmW , False );
}

/****************************************************
*   Prolog-KB-Editor: New selected Kb!              *
****************************************************/

static void
NewKb( void ) {
   static KBAX *ax,*axfrei;
   static XmString xmstr,FKlausel;
   static int i;

   frei_nr=0;
   XmListDeselectAllItems(OneOfManyW1);
   XmListDeselectAllItems(OneOfManyW2);

   for( i = 0; i < 10; i++ )      /* Alle KBs durchlaufen */
      if (KBI[i])
         if (KBI[i]->name)
            if (!strcmp(KBI[i]->name,selected_kb))
               if (KBI[i]->axiome) 
                  if (KBI[i]->axiome->name){
                     axfrei=(KBI[i]->axiome->next);
                     FKlausel= XmStringCreateSimple(KBI[i]->axiome->name);
                  }
   XmListSelectItem(OneOfManyW1,FKlausel,False);                             
   i=0;
   while (axfrei){
      if (axfrei->name)
         frei_nr++;
      xmstr=CSTRING(axfrei->name);
      if( XmListItemExists( OneOfManyW2,xmstr)) {
         XmListDeleteItem( OneOfManyW2, xmstr);
         verbl_nr--;
         if (verbl_nr==0 ) {
            XtSetSensitive(BeforeW,False);
            XtFree(list2_sel);
            list2_sel=NULL;
         }
      }
      XmListAddItem( OneOfManyW3, xmstr, i ); 
      XmStringFree( xmstr );
      axfrei=axfrei->next;
      i++;
   }
   XmListSelectItem(OneOfManyW1,FKlausel,True);
   XmListSelectPos( OneOfManyW2, 1, True );
   XmListSelectPos( OneOfManyW3, 1, True );
   XtSetSensitive(OneOfManyW1,True);
   XtSetSensitive(OneOfManyW2,True);
   XtSetSensitive(OneOfManyW3,True);
   XtSetSensitive(ZuVerwerfW , True);
   XtSetSensitive(ZuUebernehmW , True);
   Zu_Change_False();  
}

/****************************************************
*   Shows the Prolog-KB-Editor                      *
****************************************************/

static void
prologEditorZuordnungCB( void ) {
   char *buf; 
   static AXIOMS *axneu; 
   static AXIOMS *ax;
   int nr;  

   XtManageChild( PromptW );
   XtVaSetValues( kbnameW, XmNlabelString,  unnamed_str , NULL );            
   Zu_Change_False();
   if (selected_kb){
      XtFree( selected_kb);
      selected_kb=NULL;
   }   
   XtSetSensitive(OneOfManyW1,False);
   XtSetSensitive(OneOfManyW2,False);
   XtSetSensitive(OneOfManyW3,False);
   verbl_nr=0;
   frei_nr=0;
   XmListDeleteAllItems(OneOfManyW1);
   XmListDeleteAllItems(OneOfManyW2);
   XmListDeleteAllItems(OneOfManyW3);
   ax = Axioms;
   nr = 0;
   while( ax ) {          /* LISP-Texte holen */
      if (ax->name){
        XmListAddItem( OneOfManyW1, XmStringCreateSimple(ax->name), nr );
        XmListAddItem( OneOfManyW2, XmStringCreateSimple(ax->name), nr );
        verbl_nr++;
        nr++;
      }
      ax=ax->next;
   }   
   XtSetSensitive( BeforeW, False);
   XtSetSensitive( AfterW, False);
   XtSetSensitive( BackW, False);
   if (list1_sel){
      XtFree( list1_sel);
      list1_sel=NULL;
   }       
   if (list2_sel){
      XtFree( list2_sel);
      list2_sel=NULL;
   }
   if (list3_sel){
      XtFree( list3_sel);
      list3_sel=NULL;
   }
   XtSetSensitive(ZuVerwerfW , False);
   XtSetSensitive(ZuUebernehmW , False);
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
   XtVaSetValues( kbnameW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   NewKb();
}

/*******************************************************************************
*   Callback of Cancel-Button in KnowledgeBase-SelectionMenu
*******************************************************************************/

static void
kb_cancelCB( void ) {

   XtUnmanageChild( XtParent( kb_selectionW ) );
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
*   Callback of Zuordnungs-Uebernehmen-Button
*******************************************************************************/

static void 
zuord_uebernehmenCB( void ) {
   XmStringTable *xmstr;
   char *buf,*buf1, *str;
   int nr,i,buflen;

   Reset_Prolog_Kb( selected_kb );
   XtVaGetValues(OneOfManyW3, XmNitems, &xmstr, NULL);
   XtVaGetValues(OneOfManyW3, XmNitemCount, &nr, NULL);
   buflen=strlen(list1_sel);
   Prolog_Neues_Axiom( selected_kb,  list1_sel);
   for( i=0; i<nr ;i++){
      str=(char*)XtMalloc(512);
      XmStringGetLtoR((XmString)xmstr[i], XmSTRING_DEFAULT_CHARSET, &str);
      Prolog_Neues_Axiom( selected_kb, str);
      buflen=buflen+3+strlen(str);
      XtFree(str);
   }

   buf = XtMalloc( 256 + buflen );  
   strcpy(buf,list1_sel);
   strcat(buf," ");
 
   for( i=0; i<nr ;i++) {
      str=(char*)XtMalloc(512);
      XmStringGetLtoR((XmString)xmstr[i], XmSTRING_DEFAULT_CHARSET, &str);
      strcat(buf , str);
      strcat(buf , " ");
      XtFree(str);
   }

    buf1 = XtMalloc( 256 + strlen(selected_kb) + strlen(list1_sel) );
    sprintf( buf1, "($send (symbol-value '%s) :set-relations-name '%s)",
             selected_kb, list1_sel);
    LISP_sndCmd(buf1);  
    sprintf(buf1,"($send ($send (symbol-value '%s) :prolog-processor)\n"
                 "    :set-axioms '(%s))", selected_kb, buf);
    LISP_sndCmd(buf1);
    Zu_Change_False();          
    LISP_Change_kb( selected_kb );
}  

/*******************************************************************************
*   Callback of Zuordnungs-Verwerfen-Button
*******************************************************************************/

static void 
zuord_verwerfenCB( void ) {
   XmString xmstr;
   char *tmp;

   tmp=(char*)XtMalloc(128);
   strcpy(tmp,selected_kb);
   prologEditorZuordnungCB();
   selected_kb=(char*)XtMalloc(128);     
   strcpy(selected_kb,tmp);
   NewKb();
   xmstr=CSTRING( selected_kb );
   XtVaSetValues( kbnameW, XmNlabelString, xmstr, NULL );
   XmStringFree( xmstr );
}

/*******************************************************************************
*   Callback of Zuordnungs-Schliessen-Button
*******************************************************************************/

static void 
zuord_okCB( void ) {

   if( unsaved_changes )
      zuord_uebernehmenCB();
   XtUnmanageChild( PromptW );
}  

/*******************************************************************************
*   Callback of Before- and After-Button
*******************************************************************************/

static void
addCB( Widget w, int modus ) {
   int nr2,nr3;  
   if( list2_sel ) {
      nr2=XmListItemPos(OneOfManyW2,CSTRING(list2_sel));

      if (!list3_sel){        /* Erster Eintrag in rechte Liste */
         XmListAddItem( OneOfManyW3, XmStringCreateSimple(list2_sel), 0 );
         XmListSelectPos( OneOfManyW3, 1, True );
         XmListDeleteItem( OneOfManyW2, CSTRING(list2_sel));
         if( nr2 != 0 )
            nr2--;
         XmListSelectPos( OneOfManyW2, nr2, True );  
      } else {
         nr3=XmListItemPos(OneOfManyW3,CSTRING(list3_sel));  
         if( modus == LAST )
            nr3++;
         XmListAddItem( OneOfManyW3, XmStringCreateSimple(list2_sel), nr3 );   
         XmListSelectPos( OneOfManyW3, nr3, True );
         XmListDeleteItem( OneOfManyW2, CSTRING(list2_sel));
         if( nr2 != 0 )
            nr2--;
         XmListSelectPos( OneOfManyW2, nr2, True );
      }
      verbl_nr--;
      frei_nr++;
      if( verbl_nr == 0 ) {
         XtFree(list2_sel);
         list2_sel=NULL;
         XtSetSensitive( BeforeW, False );
         XtSetSensitive( AfterW, False );
      }    
      Zu_Change_True();
   }
}

/*******************************************************************************
*   Callback of Back-Button
*******************************************************************************/

static void 
backCB( Widget w, int modus ) {
   int nr2,nr3;  

   if( !list2_sel ) {
      XtSetSensitive( AfterW, True );
      XtSetSensitive( BeforeW, True );
   }
   XmListAddItem( OneOfManyW2, XmStringCreateSimple(list3_sel), verbl_nr );
   XmListSelectPos( OneOfManyW2, 1 , True );
   verbl_nr++;
   nr3=XmListItemPos(OneOfManyW3,CSTRING(list3_sel));  
   XmListDeleteItem( OneOfManyW3, CSTRING(list3_sel));
   XmListSelectPos( OneOfManyW3, 1, True );

   frei_nr--;
   if( frei_nr == 0 ) {
      XtSetSensitive( BackW, False );
      XtFree( list3_sel );
      list3_sel = NULL;
   }
   Zu_Change_True(); 
}  

/*******************************************************************************
*   Callback of List-1-Click
*******************************************************************************/

static void
ClickList1CB( list_w, client_data, cbs )
Widget list_w;
XtPointer client_data;
XmListCallbackStruct *cbs;
{
   char *old = NULL;

   if( list1_sel ) {
      XmListAddItem( OneOfManyW2, XmStringCreateSimple(list1_sel), 0 );
      old=(char*)XtMalloc(1+strlen(list1_sel));
      strcpy(old,list1_sel);
      XtFree(list1_sel);
      verbl_nr++;
      if( verbl_nr == 1 ) {
         XtSetSensitive( AfterW, True );
         XtSetSensitive( BeforeW, True );      
      }
      XmListSelectPos( OneOfManyW2, 1, True ); 
   }
   XmStringGetLtoR( cbs->item, XmSTRING_DEFAULT_CHARSET, &list1_sel );
   if( old )
      if( strcmp( old, list1_sel ) )
         Zu_Change_True();

   /* Feste Klausel aus Liste 2 loeschen */
   if( XmListItemExists( OneOfManyW2, CSTRING(list1_sel) ) ) {
      XmListDeleteItem( OneOfManyW2, CSTRING(list1_sel) );
      verbl_nr--;
      XmListSelectPos( OneOfManyW2, 1, True );
      if( verbl_nr == 0 ) {
         XtFree( list2_sel );
         list2_sel = NULL;
         XtSetSensitive( BeforeW, False );
         XtSetSensitive( AfterW, False );
      }
   }
   /* Feste Klausel aus Liste 3 loeschen */
   if( XmListItemExists( OneOfManyW3, CSTRING(list1_sel) ) ) {
      XmListDeleteItem( OneOfManyW3, CSTRING(list1_sel));
      XmListSelectPos( OneOfManyW3, 1, True );
      frei_nr--;
      if( frei_nr == 0 ) {
         XtSetSensitive( BackW, False );
         XtFree( list3_sel );
         list3_sel = NULL;
      }
   }
}

/*******************************************************************************
*   Callback of List-2-Click
*******************************************************************************/

static void
ClickList2CB( list_w, client_data, cbs )
Widget list_w;
XtPointer client_data;
XmListCallbackStruct *cbs;
{
   if( list2_sel )
      XtFree( list2_sel );
   XmStringGetLtoR( cbs->item, XmSTRING_DEFAULT_CHARSET, &list2_sel );
   XtSetSensitive( BeforeW, True );
   XtSetSensitive( AfterW, True );
} 

/*******************************************************************************
*   Callback of List-3-Click
*******************************************************************************/

static void
ClickList3CB( list_w, client_data, cbs )
Widget list_w;
XtPointer client_data;
XmListCallbackStruct *cbs;
{
   if( list3_sel )
      XtFree( list3_sel );
   XmStringGetLtoR( cbs->item, XmSTRING_DEFAULT_CHARSET, &list3_sel );
   XtSetSensitive( BackW, True);
} 

/****************************************************
*   Creates the Prolog-Editor                       *
****************************************************/

void
PROLOG_create( Widget parent ) {
   Widget Zu_formW,Zu_form1W,Zu_form2W, Zu_form3W, formW, form1W;
   Widget menubarW , OkW, CancelW;
   Widget formnamelabelW, editorlabelW;
   Widget w1, w2, w3, w4, w5,w,sep;
   Widget RowColW, Zu_RowColW,Zu_RowColW2,  OneOfManyW;
   XmString tmp_str;
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
   shellW = XtAppCreateShell( "Prolog", "HereClassIsIgnored",
                            /* instance         class */
      topLevelShellWidgetClass, XtDisplay(parent), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( shellW, WM_DELETE_WINDOW,
      (XtPointer)PROLOG_hide, NULL );            /* Pop down ... */

   /********** Form-Widget erzeugen ********************************/
   n = 0;
   formW = XmCreateForm( shellW, "Prologeditor", args, n );
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
      (XtPointer)HELP_show, (XtPointer)HELP_PROLOG_MENU );

   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++; 
   menubarW = XmCreateMenuBar( formW, "menubar", args, n );
   XtManageChild( menubarW );

   /****************************************************************/ 
   /*** Pulldown-Menuepunkt "Bearbeiten" erzeugen und managen *************/ 
  
   w1=XmCreatePulldownMenu( menubarW, "Bearbeiten-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Bearbeiten",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );

   LadenW = XtVaCreateManagedWidget( "Speichern",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LadenW, XmNactivateCallback,
                    (XtPointer)prologEditorLadenCB, NULL );
   
   LoeschenW = XtVaCreateManagedWidget( "Kl_loeschen",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtAddCallback(LoeschenW, XmNactivateCallback,
                    (XtPointer)prologEditorPrologloeschenCB, NULL );

   ReChangeW = XtVaCreateManagedWidget( "ReChange", 
                    xmPushButtonGadgetClass, w1,
                    NULL );
     XtAddCallback(ReChangeW, XmNactivateCallback,
        (XtPointer)prologEditorRechangeCB, NULL );

   XtVaCreateManagedWidget( "sep", xmSeparatorGadgetClass, w1, NULL );

   zuordW = XtVaCreateManagedWidget( "Zuordnen",
                    xmPushButtonGadgetClass, w1,
                    NULL );   
   XtAddCallback(zuordW, XmNactivateCallback,
            (XtPointer)prologEditorZuordnungCB, NULL );

   /****************************************************************/ 
   w1=XmCreateSimplePulldownMenu( menubarW, "Hilfe-Menu", NULL,0);
   w2 = XtVaCreateManagedWidget( "Help",
                    xmCascadeButtonGadgetClass, menubarW,
                    XmNsubMenuId,     w1,
                    NULL );

   w3 = XtVaCreateManagedWidget( "on_menu",
                    xmPushButtonGadgetClass, w1,
                    NULL );
   XtVaSetValues( menubarW, XmNmenuHelpWidget, w2/*help_menu_button*/, NULL );
   XtAddCallback( w3, XmNactivateCallback,
                    (XtPointer)HELP_show, (XtPointer)HELP_PROLOG_MENU );

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
   form1W = XmCreateForm( formW, "form", args, n );
   XtManageChild( form1W );
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

   /******** Label zur Anzeige des Klauselmengennamens **************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,            XmATTACH_FORM ); n++;
   namelabelW = XmCreateLabel( RowColW, "Klauselmenge", args, n );
   XtManageChild( namelabelW );

   /********   Klausel-Button   ********/
   n = 0;    
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,          XmATTACH_FORM ); n++; 
   XtSetArg( args[n], XmNleftAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNleftWidget,                   namelabelW ); n++;
   XtSetArg( args[n], XmNrightAttachment,           XmATTACH_FORM ); n++;

   nameW = XtVaCreateManagedWidget( "name",
            xmPushButtonWidgetClass, RowColW,
            XmNnavigationType,       XmTAB_GROUP,
            XmNtopAttachment,        XmATTACH_FORM,
            XmNleftAttachment,       XmATTACH_WIDGET,
            XmNleftWidget,          namelabelW,
            XmNrightAttachment,     XmATTACH_FORM,
            NULL );
                     
   XtAddCallback( nameW, XmNactivateCallback, 
         (XtPointer)emaxps_prolog_select, NULL );
         
   XtManageChild( nameW );                                  
   XtVaGetValues( nameW, XmNlabelString, &unnamed_str, NULL );

   /********** QuestionBox fuer  Delete-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   DelWarningW = XmCreateMessageDialog(shellW, "DelWarningW",args,n);
   /***** ungenutzten Help-Button  unmanagen *****/
   XtVaGetValues(DelWarningW,XmNmessageString,&loadtextxm,NULL);     
   w = XmMessageBoxGetChild( DelWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( DelWarningW, XmNokCallback,
     (XtPointer)DelWarningokCB, NULL );
   XtAddCallback( DelWarningW, XmNcancelCallback,
      (XtPointer)DelWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(DelWarningW), WM_DELETE_WINDOW,
      (XtPointer)DelWarningcancelCB, (XtPointer)DelWarningW );
   w = XmSelectionBoxGetChild( DelWarningW, XmDIALOG_CANCEL_BUTTON );
       XtUnmanageChild( w );

   /********** QuestionBox fuer  Load-Warnung ************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   LoadWarningW = XmCreateQuestionDialog(shellW, "LoadWarningW",args,n);
   
   /***** ungenutzten Help-Button  unmanagen *****/
   w = XmMessageBoxGetChild( LoadWarningW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   XtAddCallback( LoadWarningW, XmNokCallback,
     (XtPointer)LoadWarningokCB, NULL );
   XtAddCallback( LoadWarningW, XmNcancelCallback,
      (XtPointer)LoadWarningcancelCB, NULL );
   XmAddWMProtocolCallback( XtParent(LoadWarningW), WM_DELETE_WINDOW,
      (XtPointer)LoadWarningcancelCB, (XtPointer)LoadWarningW );


   /********   Klausel-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( nameW, "kl_selection_form", args, n );
   XtVaSetValues( XtParent(w), XmNdeleteResponse, XmDO_NOTHING, NULL );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,                 XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,                XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,               XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,              XmATTACH_FORM ); n++;
   kl_selectionW = XmCreateSelectionBox( w, "kl_selection", args, n ); 
   XtManageChild( kl_selectionW );  

   /*** only KnowledgeBases already loaded can be chosen from ***/
   w = XmSelectionBoxGetChild( kl_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );
   w = XmSelectionBoxGetChild( kl_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );

   /* install callback for the CLOSE-Button of the mwm-border menu */
   XmAddWMProtocolCallback( XtParent( XtParent( kl_selectionW ) ),
      WM_DELETE_WINDOW, (XtPointer)kl_cancelCB, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( kl_selectionW, XmNokCallback,
      (XtPointer)kl_selectionCB, NULL );
   XtAddCallback( kl_selectionW, XmNcancelCallback,
      (XtPointer)kl_cancelCB, NULL );
 
   /*******  KnowledgeBase-SelectionMenu-MessageBox-Widget  ******/
   kl_selection_msgW = createMessageBox( kl_selectionW,
                                             "kl_selection_msg" );

   /******* Ueberschrift fuer das Editorfenster **********************/
   n = 0;
   XtSetArg( args[n], XmNleftAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,           XmATTACH_FORM ); n++;  
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                form1W ); n++;  
   XtSetArg( args[n], XmNalignment,            XmALIGNMENT_BEGINNING ); n++;
   editorlabelW = XmCreateLabel( formW, "Klauseln", args, n );
   XtManageChild( editorlabelW );
   
   /********* Editorfenster fuer die Klauselmengen ******************/
   n = 0; 
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;

   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                 editorlabelW ); n++;  
   XtSetArg( args[n], XmNbottomAttachment,         XmATTACH_FORM ); n++;
   /* XmText-specific resources: */
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   XtSetArg( args[n], XmNwordWrap,                         True ); n++;
   XtSetArg( args[n], XmNeditMode,            XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNeditable,                         True ); n++;
   XtSetArg( args[n], XmNautoShowCursorPosition,           True ); n++;
   klauselnW = XmCreateScrolledText( formW, "text", args, n );
   XtAddCallback( klauselnW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtAddCallback( klauselnW, XmNvalueChangedCallback,
        (XtPointer)Axiom_changedCB, NULL );

   LadenFlag=False;
   
   XtManageChild( klauselnW );
   Axiom_NoSelect();
   
  /*********************************************************************/
  /*********************************************************************/
  /**               Zuordnungs-Editor erstellen                       **/
  /*********************************************************************/
  /*********************************************************************/

   /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNautoUnmanage, False ); n++;
   PromptW = XmCreatePromptDialog(  shellW , "zuordnung", args, n );

   XtAddCallback( PromptW, XmNokCallback,
      (XtPointer)zuord_okCB, NULL );
   XtAddCallback( PromptW, XmNapplyCallback,
      (XtPointer)zuord_uebernehmenCB, NULL );
   XtAddCallback( PromptW, XmNcancelCallback,
         (XtPointer)zuord_verwerfenCB, NULL );

   /* remove not used widgets */
   ZuVerwerfW = XmSelectionBoxGetChild( PromptW, XmDIALOG_APPLY_BUTTON );
   ZuUebernehmW = XmSelectionBoxGetChild( PromptW, XmDIALOG_CANCEL_BUTTON );
   /* avoid use of HELP button here to grant work of the HELP-facility! */
   w = XmSelectionBoxGetChild( PromptW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( PromptW, XmDIALOG_APPLY_BUTTON );
   XtManageChild( w );
   w = XmSelectionBoxGetChild( PromptW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( PromptW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

   /********** Form-Widget erzeugen ********************************/
   n = 0;
   Zu_formW = XmCreateForm( PromptW, "", args, n );
   XtManageChild( Zu_formW );   
   XtSetArg( args[n], XmNtopAttachment,            XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++; 

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;

   Zu_form1W = XmCreateForm( Zu_formW, "Zu_form", args, n );
   XtManageChild( Zu_form1W );
   zu_unsaved_changes_labelW = XtVaCreateWidget( "unsaved_changes_label",
      xmLabelWidgetClass, Zu_form1W,
      XmNlabelType,               XmPIXMAP,
      XmNtopAttachment,      XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNbottomAttachment,   XmATTACH_FORM,
      NULL );
   XtManageChild ( zu_unsaved_changes_labelW );

   /* after managing !!! */
   PIXMAP_createChanged( zu_unsaved_changes_labelW,
      &change_pixmap0, &change_pixmap1 );

   Zu_RowColW = XtVaCreateManagedWidget( "RowColumn",
             xmRowColumnWidgetClass, Zu_form1W,
             XmNrowColumnType, XmWORK_AREA,
             XmNtopAttachment,XmATTACH_FORM,
             XmNleftAttachment,XmATTACH_FORM,
             XmNleftAttachment,           XmATTACH_FORM,
             XmNrightAttachment, XmATTACH_WIDGET,
             XmNrightWidget,  zu_unsaved_changes_labelW ,
             XmNresizeWidth,True,      
             XmNorientation,           XmVERTICAL,
             XmNrecomputeSize,              TRUE,
             XmNnumColumns,2,
             XmNpacking, XmPACK_COLUMN,
             NULL );

   /******** Label zur Anzeige des Kb-namens **************/
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,            XmATTACH_FORM ); n++;
   Zu_namelabelW = XmCreateLabel( Zu_RowColW, "Wissensbasis", args, n );
   XtManageChild( Zu_namelabelW );

   /********   Kb-Button   ********/
   n = 0;    
   XtSetArg( args[n], XmNtopAttachment,             XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,          XmATTACH_FORM ); n++; 
   XtSetArg( args[n], XmNleftAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNleftWidget,                   Zu_namelabelW ); n++;
   XtSetArg( args[n], XmNrightAttachment,           XmATTACH_FORM ); n++;

   kbnameW = XtVaCreateManagedWidget( "kbname",
            xmPushButtonWidgetClass, Zu_RowColW,
            XmNnavigationType,       XmTAB_GROUP,
            NULL );

   /*XtAddCallback( nameW, XmNactivateCallback, 
         (XtPointer)emaxps_prolog_select, NULL );*/
   XtManageChild ( kbnameW );                                  
   XtVaSetValues( kbnameW, XmNlabelString, unnamed_str, NULL );         
   XtAddCallback( kbnameW, XmNactivateCallback,
       (XtPointer)chooseKbCB, NULL );

   /********   KnowledgeBase-SelectionMenu-Widget   ********/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   w = XmCreateFormDialog( kbnameW, "kb_selection_form", args, n );
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

   /*****************   Separator-Widget   *****************/
   sep = XtVaCreateManagedWidget( "sep",
      xmSeparatorGadgetClass, Zu_formW,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                    Zu_form1W,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      NULL );

   n=0;
   XtSetArg( args[n], XmNtopAttachment,          XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     sep); n++;
   XtSetArg( args[n], XmNleftAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNfractionBase,                         8 ); n++;  
   Zu_form2W = XmCreateForm( Zu_formW, "Zu_form2", args, n );
   XtManageChild( Zu_form2W );

   /********  Die Auswahl-Labels **************/
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        0 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       1 ); n++;   
   w1 = XmCreateLabel( Zu_form2W , "feste_Klausel",args, n );
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        2 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       3 ); n++;      
   w2 = XmCreateLabel( Zu_form2W , "verbl_Klausel", args,n );
   
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        6 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       7 ); n++;      
   w4 = XmCreateLabel( Zu_form2W , "freie_Klausel", args,n );

   /********  Feste-Klausel-Auswahl-Box (One-of-Many) **************/
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     w1); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        0 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       2 ); n++;   

   OneOfManyW = XmCreateSelectionBox( Zu_form2W, "OneOfMany", args, n );
   OneOfManyW1 = XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_OK_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST_LABEL);
   XtVaGetValues(w1,XmNlabelString, &tmp_str, NULL );
   XtVaSetValues(w,XmNlabelString, tmp_str, NULL );   

   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SEPARATOR );
   XtUnmanageChild( w );
      
   XtManageChild(OneOfManyW);	
   XtAddCallback(OneOfManyW1,XmNdefaultActionCallback, ClickList1CB,NULL);
   XtAddCallback(OneOfManyW1,XmNbrowseSelectionCallback, ClickList1CB,NULL);   

   /********  Verbleibende-Klausel-Auswahl-Box  **************/
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     w2); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        2 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       4 ); n++;   

   OneOfManyW = XmCreateSelectionBox( Zu_form2W, "Bla", args, n );
   OneOfManyW2 = XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST_LABEL);
   XtVaGetValues(w2,XmNlabelString, &tmp_str, NULL );    
   XtVaSetValues(w,XmNlabelString, tmp_str, NULL );    
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_OK_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SEPARATOR );
   XtUnmanageChild( w );
   XtManageChild(OneOfManyW);	
   XtAddCallback(OneOfManyW2,XmNdefaultActionCallback, ClickList2CB,NULL);
   XtAddCallback(OneOfManyW2,XmNbrowseSelectionCallback, ClickList2CB,NULL);
   tmp_str = XmStringCreateSimple( ">>  >>" );
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNleftPosition,                        4 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       6 ); n++;
   XtSetArg( args[n], XmNlabelString,                   tmp_str ); n++;
   w3 = XmCreateLabel( Zu_form2W , "assign", args, n );
   XtManageChild(w3);  
   XmStringFree( tmp_str );

   /********  Before-Button  **************/
   BeforeW = XtVaCreateManagedWidget( "before",
             xmPushButtonWidgetClass,  Zu_form2W,
             XmNnavigationType,       XmTAB_GROUP,
             XmNtopAttachment,             XmATTACH_WIDGET,
             XmNtopWidget,                   w3, 
             XmNleftAttachment,      XmATTACH_POSITION,
             XmNrightAttachment,     XmATTACH_POSITION,
             XmNleftPosition,                        4,
             XmNrightPosition,                       6,
             NULL );
    XtAddCallback(BeforeW, XmNactivateCallback,
             (XtPointer)addCB,(XtPointer)FIRST );

   /********  After-Button  **************/
   AfterW = XtVaCreateManagedWidget( "after",
            xmPushButtonWidgetClass, Zu_form2W,
            XmNnavigationType,       XmTAB_GROUP,
            XmNtopAttachment,             XmATTACH_WIDGET,
            XmNtopWidget,                   BeforeW, 
            XmNleftAttachment,      XmATTACH_POSITION,
            XmNrightAttachment,     XmATTACH_POSITION,
            XmNleftPosition,                        4,
            XmNrightPosition,                       6,
            NULL );
   XtAddCallback(AfterW, XmNactivateCallback,
           (XtPointer)addCB,(XtPointer)LAST );

   /********  Back-Button  **************/
   tmp_str = XmStringCreateSimple( "<<  <<" );
   BackW = XtVaCreateManagedWidget( "back",
            xmPushButtonWidgetClass, Zu_form2W,
            XmNlabelString,                   tmp_str,
            XmNnavigationType,       XmTAB_GROUP,
            XmNtopAttachment,             XmATTACH_WIDGET,
            XmNtopWidget,                   AfterW, 
            XmNleftAttachment,      XmATTACH_POSITION,
            XmNrightAttachment,     XmATTACH_POSITION,
            XmNleftPosition,                        4,
            XmNrightPosition,                       6,
            NULL );
   XmStringFree( tmp_str );
   XtAddCallback(BackW, XmNactivateCallback,
           (XtPointer)backCB,NULL );

   /********  Freie-Klausel-Auswahl-Box  **************/
   n=0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                     w4); n++;
   XtSetArg( args[n], XmNleftAttachment,      XmATTACH_POSITION ); n++;
   XtSetArg( args[n], XmNrightAttachment,     XmATTACH_POSITION ); n++;   
   XtSetArg( args[n], XmNleftPosition,                        6 ); n++;
   XtSetArg( args[n], XmNrightPosition,                       8 ); n++;   
   
   OneOfManyW = XmCreateSelectionBox( Zu_form2W, "Blubb", args, n );
   OneOfManyW3 = XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_LIST_LABEL);
   XtVaGetValues(w4,XmNlabelString, &tmp_str, NULL );    
   XtVaSetValues(w,XmNlabelString, tmp_str, NULL );    
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_OK_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_APPLY_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_CANCEL_BUTTON );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_TEXT );
   XtUnmanageChild( w );
   w= XmSelectionBoxGetChild( OneOfManyW, XmDIALOG_SEPARATOR );
   XtUnmanageChild( w );
   XtAddCallback(OneOfManyW3,XmNdefaultActionCallback, ClickList3CB,NULL);
   XtAddCallback(OneOfManyW3,XmNbrowseSelectionCallback, ClickList3CB,NULL);
   XtManageChild(OneOfManyW);	
}            

/***************************************************/

void
PROLOG_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) )
         Reset_Prolog_Kb( kb );
}

/****************************************************
*   Change Sensitivity of the Prolog Editor         *
*   (accessable from outside...)                    *
****************************************************/

void
PROLOG_ready( void ) {

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
PROLOG_busy( void ) {

   if( XtIsRealized( shellW ) ) {
      XDefineCursor(
         XtDisplay( shellW ),
         XtWindow( shellW ),
         cursor_busy
      );
   }
}

/****************************************************
*   Shows and updates the Prolog Editor             *
****************************************************/

void
PROLOG_show( void ) {

   XtPopup( shellW, XtGrabNone );
   XRaiseWindow( XtDisplay(shellW), XtWindow(shellW) );
}

/***************************************************/

void
PROLOG_hide( void ) {

   XtPopdown( shellW );
}

/*** EOF *******************************************/

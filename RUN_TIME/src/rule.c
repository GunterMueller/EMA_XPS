/****************************************************
*   rule.c        Hans Groschwitz        19.07.95   *
*                 Susanne Wellmann                  *
*                 Klaus Senf                        *
*                 Michael Block                     *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the rule-editor.             *
*   Full version only.                              *
****************************************************/

#include <stdio.h>

#include <Xm/CascadeBG.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MessageB.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/Protocols.h>  /* XmAddWMProtocolCallback */

#include "main.h"          /* WM_DELETE_WINDOW, iconPm */
#include "help.h"          /* HELP_show */
#include "xinit.h"         /* KNOWNKBS_getList */

/*** Globals ***************************************/

static Widget Rule_shellW;            /* RuleEditor-Shell */

static Widget save_buttonW;           /* Save-Button of Edit-Menu */
static Widget del_rule_buttonW;       /* DeleteRule-Button of Edit-Menu */
static Widget del_ruleset_buttonW;    /* DeleteRuleSet-Button of Edit-Menu */
static Widget undo_buttonW;           /* UndoChanges-Button of Edit-Menu */
static Widget move_rule_buttonW;

static Widget save_warW;
static Widget del_rule_warW;
static Widget del_ruleSet_warW;

static Widget exclamation_SignW;      /* UnsavedChanges-BitmapLabel */
static int    unsaved_changes;        /* any edits unsaved? */

static Widget kb_buttonW;             /* KnowledgeBaseButton */
static Widget kb_selectionW;          /* KnowlegdeBase-SelectionMenu */
static Widget kb_selection_msgW;      /* KB-SelectionMenu-MessageBox */
static Widget kb_selection_warW;      /* KB-SelectionMenu-WarningBox */

static Widget ruleset_buttonW;        /* RuleSetButton */
static Widget ruleset_selectionW;     /* RuleSet-SelectionMenu */
static Widget ruleset_selection_msgW; /* RuleSet-SelectionMenu-MessageBox */
static Widget ruleset_selection_warW; /* RuleSet-SelectionMenu-WarningBox */

static Widget rule_buttonW;           /* RuleButton */
static Widget rule_selectionW;        /* Rule-SelectionMenu */
static Widget rule_selection_msgW;    /* Rule-SelectionMenu-MessageBox */
static Widget rule_selection_warW;    /* Rule-SelectionMenu-WarningBox */

static char   *selected_kb;           /* selected KnowledgeBase */
static char   *selected_ruleset;      /* selected RuleSet */
static char   *selected_rule;         /* selected Rule */

static Widget junctor_optionW;        /* Junctor-OptionMenu */
static Widget junctor_button[8];      /* Junctor-Togglebuttons */
static Widget if_textW;               /* if-ScrolledText */

static Widget actiontype_optionW;     /* Actiontype-OptionMenu */
static Widget actiontype_button[3];   /* Actiontype-Togglebuttons */
static Widget then_textW;             /* then-ScrolledText */

static Widget move_selection_formW;   /* Move a rule within a rule-set */
static Widget move_selection_msgW;
static Widget move_selectionW;
static Widget radio_beforeW;
static Widget radio_afterW;

static char *actiontype_str[] = { "$Conclude", "$Execute", "$Ask" };
static char *junctor_str[]    = { "$False", "$And", "?And", "And",
                                  "$Or", "?Or", "Or", "$True" };

static XmString unnamed_str;  /* XmString to be drawn if nothing is chosen */
static int newRule;           /* a new Rule is chosen */
static int newRuleSet;        /* a new RuleSet is chosen */
static int excl_enable = 1;
static Pixmap change_pixmap0, change_pixmap1;

static void ruleset_reset( void );

/*************************************************/

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
      C_set_modified( selected_kb, MOD_RULE, True ); 
      unsaved_changes = 1;
      if( !newRule )
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
      C_set_modified( selected_kb, MOD_RULE, False );
   unsaved_changes = 0; 
   XtSetSensitive( undo_buttonW, False );
   XtSetSensitive( save_buttonW, False );
}

/******************************************************************************/
/*     Inform Editor on intermediate Close of a KB                            */
/******************************************************************************/

void 
RULE_kbClosed( char *kb ) {

   if( selected_kb )
      if( !strcmp( kb, selected_kb ) ) {
         ruleset_reset();
         Change_False;
         XtFree( selected_kb );
         selected_kb = NULL;
         XtVaSetValues( kb_buttonW, XmNlabelString, unnamed_str, NULL );
      }
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

/*******************************************************************************
*   Change Editability of OptionMenus and ScrolledTexts                        *
*******************************************************************************/

void 
Rule_editable( Boolean ability ) {

   XtSetSensitive( junctor_optionW, ability );
   XtSetSensitive( actiontype_optionW, ability );
   XtSetSensitive( del_rule_buttonW, ability );
   XtSetSensitive( move_rule_buttonW, ability);
   XtVaSetValues( if_textW, XmNeditable, ability, NULL );
   XtVaSetValues( then_textW, XmNeditable, ability, NULL );
}

/*******************************************************************************
*   Reset OptionMenus and ScrolledTexts                                        *
*******************************************************************************/

static void
editor_reset( void ) {

   excl_enable = 0;
   XtVaSetValues( if_textW,   XmNvalue, "", NULL );
   XtVaSetValues( then_textW, XmNvalue, "", NULL );
   XtVaSetValues( junctor_optionW,
      XmNmenuHistory, junctor_button[0],
      NULL );
   XtVaSetValues( actiontype_optionW,
      XmNmenuHistory, actiontype_button[1],
      NULL );
   excl_enable = 1;
}

/*******************************************************************************
*   Reset Rule, OptionMenus and ScrolledTexts                                  *
*******************************************************************************/

static void
rule_reset( void ) {
   Widget w;

   editor_reset();
   XtSetSensitive( del_rule_buttonW, False );
   XtVaSetValues( rule_buttonW, XmNlabelString, unnamed_str, NULL );
   XtFree( selected_rule );
   selected_rule = NULL;
      
   w = XmSelectionBoxGetChild( rule_selectionW, XmDIALOG_TEXT );
   XmTextSetString( w, ""  );
}

/*******************************************************************************
*   Reset Rule, OptionMenus and ScrolledTexts                                  *
*******************************************************************************/

static void
ruleset_reset( void ) {
   Widget w;

   rule_reset();
   XtSetSensitive( del_ruleset_buttonW, False );
   XtVaSetValues( ruleset_buttonW, XmNlabelString, unnamed_str, NULL );
   XtFree( selected_ruleset );
   selected_ruleset = NULL;

   w = XmSelectionBoxGetChild( ruleset_selectionW, XmDIALOG_TEXT );
   XmTextSetString( w, ""  );
}

/*******************************************************************************
*   Callback of MenuEntry "Save"                                               *                                             *
*******************************************************************************/

static void 
save_rule( void ) {
   int  i;
   char *junctor_text, *if_text, *actiontype_text, *then_text,  *str;
   Widget w;

   XtUnmanageChild( save_warW );   
   
   if_text   = XmTextGetString( if_textW   );
   then_text = XmTextGetString( then_textW );
   
   XtVaGetValues( junctor_optionW, XmNmenuHistory, &w, NULL );
   for( i=0; w != junctor_button[i]; i++ );
   junctor_text = (char *)XtMalloc( 1 + strlen( junctor_str[i] ));
   strcpy( junctor_text, junctor_str[i] );
   
   XtVaGetValues( actiontype_optionW, XmNmenuHistory, &w, NULL );
   for( i=0; w != actiontype_button[i]; i++ );
   actiontype_text= (char *)XtMalloc( 1 + strlen( junctor_str[i] ));            
   strcpy( actiontype_text, actiontype_str[i] );
          
   str = (char *)XtMalloc( 64 + strlen( selected_kb ) 
                              + strlen( selected_ruleset )
                              + strlen( selected_rule )
                              + strlen( junctor_text )
                              + strlen( if_text )
                              + strlen( actiontype_text )
                              + strlen( then_text ) );

   sprintf( str, "(emaxps-rule-save-rule '%s :%s '%s '((%s\n%s\n)(%s\n%s\n)))",
      selected_rule, selected_ruleset, selected_kb,
      junctor_text, if_text, actiontype_text, then_text );

   LISP_sndCmd( str );

   Change_False();

   newRule = 0;
   newRuleSet = 0;

   XtFree( str );
   XtFree( junctor_text );
   XtFree( actiontype_text );
   XtFree( if_text   );
   XtFree( then_text );
}

static void    
save_ruleCB( void ) {

   if( !newRule ) {
      XtManageChild( save_warW );
      return;
   }   
   save_rule();
}


/*******************************************************************************
*   Callback of MenuEntry "Delete this Rule ..."                               *
*******************************************************************************/

static void 
del_rule( void ) {
   char *str;

   XtUnmanageChild( del_rule_warW );

   if( !newRule ) {
      str = (char *)XtMalloc( 64 + strlen( selected_kb )
                                 + strlen( selected_ruleset )
                                 + strlen( selected_rule ) );
      sprintf( str, "(emaxps-rule-delete-rule '%s ':%s '%s)",
         selected_kb, selected_ruleset, selected_rule );
      LISP_sndCmd( str );
      XtFree( str );
   }
   
   rule_reset();      
   Change_False();
   Rule_editable( False );
}

static void
del_ruleCB( void ) {

   XtManageChild( del_rule_warW );
}  

/*******************************************************************************
*   Callback of MenuEntry "Delete this RuleSet ..."                            *
*******************************************************************************/

static void 
del_ruleSet( void ) {
   char *str;

   XtUnmanageChild( del_ruleSet_warW );

   if( !newRuleSet ) {
      str = (char *)XtMalloc( 64 + strlen( selected_kb )
                                 + strlen( selected_ruleset ) );
      sprintf( str, "(emaxps-rule-delete-rset '%s :%s)",
         selected_kb, selected_ruleset );
      LISP_sndCmd( str );
      XtFree( str );
   }

   XtSetSensitive( rule_buttonW, False );
   ruleset_reset();      
   Rule_editable( False );
   Change_False();      
}

static void
del_ruleSetCB( void ) {

   XtManageChild( del_ruleSet_warW );
}

/*******************************************************************************
*   Callback of MenuEntry "Undo Changes"                                       *
*******************************************************************************/

static void 
undoCB( void ) {
   char *str;

   str = (char *)XtMalloc( 64 + strlen(selected_rule) 
                              + strlen(selected_ruleset)
                              + strlen(selected_kb) );

   sprintf( str, "(emaxps-rule-get-body-of-rule '%s '%s '%s)",
      selected_rule, selected_ruleset, selected_kb );

   LISP_sndCmd( str );

   XtFree( str );

   Change_False();
}

/*******************************************************************************
*   Callback of MenuEntry "Move this Rule ..."                                 *
*******************************************************************************/

static void
orderCB( void ) {
  char *str;
  
   str = (char *)XtMalloc( 64 + strlen(selected_kb) +
                                strlen(selected_ruleset) );
   sprintf( str, "(emaxps-rule-get-rules-of-rset2 '%s '%s)",
      selected_kb, selected_ruleset );
   LISP_sndCmd( str );
   XtFree( str );   
}

/*******************************************************************************
*   reaction on (emaxps-rule-get-rules-of-rset2 'kb 'ruleset)                  *
*******************************************************************************/

void
ruleGetRulesOfRset2( void ) {
   char *str, *p, *nextstr;
   int c, i, a;
   XmStringTable xmstr_list;

   p = str = MSG_read();
   if( !*str ) {
      XtFree( str );
      return;
   }

   xmstr_list = (XmString *)XtMalloc((2 +(strlen(str)>>1)*sizeof(XmString )));

   i = 0;
   while( nextstr = GetNextPartStr( str ) ) {
      xmstr_list[i++] = XmStringCreateSimple( str );
      str = nextstr;
   }

   XtVaSetValues( move_selectionW,
      XmNlistItems, xmstr_list,
      XmNlistItemCount,      i,
      NULL );

   for( a=0; a<i; a++ )
      XmStringFree( xmstr_list[a] );

   XtManageChild( move_selection_formW );

   XtFree( (char*)xmstr_list );
   XtFree( p );
}

/*******************************************************************************
*   Callback of Ok-Button in Rule-SelectionMenu  of Move-Rule                  *
*******************************************************************************/

static void
moveokCB( Widget w, XtPointer client_data,
          XmSelectionBoxCallbackStruct *cbs ) {
   char *new_pos, *str, *vor_hin;   

   if( !(new_pos = selectionMenuOk( cbs )) ) {
      XtManageChild( move_selection_msgW );
      return;
   }       

   XtUnmanageChild( move_selection_formW );

   if( !strcmp( selected_rule, new_pos )) {
       XtFree( new_pos ); 
        return;
   }
        
   if( XmToggleButtonGetState( radio_beforeW ) == True ) 
      vor_hin = "before";
   else
      vor_hin = "after";
      
   str = (char *)XtMalloc( 64 + strlen(selected_kb) 
                              + strlen(selected_ruleset)
                              + strlen(selected_rule)
                              + strlen(new_pos) );

   sprintf( str, "(emaxps-rule-move-rule '%s :%s '%s :%s '%s)",
      selected_kb, selected_ruleset, selected_rule, vor_hin, new_pos );

   LISP_sndCmd( str );
   XtFree( str );   
   XtFree( new_pos);
}

/*******************************************************************************
*   Callback of KnowledgeBase-Button                                           *
*******************************************************************************/

static void 
chooseKb ( void ) {
   char **kb_list;
   XmStringTable xmkb_list;
   int i, countKB;

   XtUnmanageChild( kb_selection_warW );

   kb_list = KNOWNKBS_getList();

   /* get number of known KnowledgeBases */
   i = 1;
   while( kb_list[i++] );
   countKB = i - 2;
   
   xmkb_list = (XmString *)XtMalloc( (1+countKB) * sizeof(XmString) );
   for( i=1; i<=countKB; i++ ) {
      xmkb_list[i-1] = XmStringCreateSimple( kb_list[i] );
      XtFree( kb_list[i] );
   }

   xmkb_list[i-1] = NULL;
   XtFree( (char*)kb_list );
   
   XtVaSetValues( kb_selectionW,
      XmNlistItems,  xmkb_list,
      XmNlistItemCount, countKB,
      NULL );

   for ( i=1; i<=countKB; i++ ) {
      XmStringFree( xmkb_list[i-1] );
   }  
   XtFree( (char*)xmkb_list );

   XtManageChild( kb_selectionW );
}


static void    
chooseKbCB ( void ) {

   if( unsaved_changes ) {
      XtManageChild( kb_selection_warW );
      return;
   }   
   chooseKb();
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

   Rule_editable( False );
   ruleset_reset();

   Change_False();

   XtSetSensitive( ruleset_buttonW, True );
   XtSetSensitive( rule_buttonW,   False );

}

/*******************************************************************************
*   Callback of RuleSet-Button                                                 *
*******************************************************************************/

static void 
chooseRuleSet( void ) {
   char *str;

   XtUnmanageChild( ruleset_selection_warW );
 
   str = (char *)XtMalloc( 64 + strlen(selected_kb) );
   sprintf( str, "(emaxps-rule-get-rsets-of-kb '%s)", selected_kb );
   LISP_sndCmd( str );
   XtFree( str );
}

static void
chooseRuleSetCB ( void ) {  

   if( unsaved_changes ) {
      XtManageChild( ruleset_selection_warW );
      return;
   }   
   chooseRuleSet();
}

/*******************************************************************************
*   reaction on (emaxps-rule-get-rsets-of-kb 'kb)                              *
*******************************************************************************/

void
ruleGetRsetsOfKB( void ) {
   char *str, *p, *nextstr;
   XmStringTable xmrule_list;
   int length, i, a;
   Widget w;

   p = str = MSG_read();

   if( !*str ) {
      w = XmSelectionBoxGetChild( ruleset_selectionW, XmDIALOG_LIST );
      XmListDeleteAllItems( w );
      XtManageChild( ruleset_selectionW );
      XtFree( str );
      return;
   }
   
   length = strlen( str );
   xmrule_list = (XmString *)XtMalloc( (2 + length>>1) * sizeof(XmString ));

   i = 0;
   while( nextstr = GetNextPartStr( str ) ) {
      xmrule_list[i] = XmStringCreateSimple( str );
      i++;
      str = nextstr;
   }
                             
   XtVaSetValues( ruleset_selectionW,
      XmNlistItems,              xmrule_list,
      XmNlistItemCount,                    i,
      NULL );

   for( a=0; i<i ; a++ )
      XmStringFree( xmrule_list[a] );

   XtFree( (char*)xmrule_list );
   XtFree( p );

   XtManageChild( ruleset_selectionW );
}

/*******************************************************************************
*   Callback of Ok-Button in RuleSet-SelectionMenu
*******************************************************************************/

static void
ruleset_selection( Widget w, XtPointer client_data,
                     XmSelectionBoxCallbackStruct *cbs ) {
   char *selection = NULL;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( cbs )) ) {
      XtManageChild( ruleset_selection_msgW );
      return;
   }

   XtUnmanageChild( ruleset_selectionW );
                   
   if( selected_ruleset ) {
      if( !strcmp( selection, selected_ruleset )) {  
         XtFree( selection );
         return;
      }      
   }
   if( selected_ruleset )
      XtFree( selected_ruleset );
                         
   selected_ruleset = (char *)XtMalloc( 1 + strlen( selection ));
   strcpy( selected_ruleset, selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_ruleset );
   XtVaSetValues( ruleset_buttonW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   Rule_editable( False );
   rule_reset();
   Change_False();

   XtSetSensitive( rule_buttonW, True );
   XtSetSensitive( del_ruleset_buttonW, True );
}

static void 
ruleset_selectionCB( Widget w, XtPointer client_data,
                XmSelectionBoxCallbackStruct *cbs ) {

   newRuleSet = False;
   ruleset_selection( w, client_data, cbs );
}

/**********************************************************/

static void 
ruleset_new_selectionCB( Widget w, XtPointer client_data,
                         XmSelectionBoxCallbackStruct *cbs ) {

   newRuleSet = True;
   ruleset_selection( w, client_data, cbs );
}

/*******************************************************************************
*   Callback of RuleSet-Button
*******************************************************************************/

static void 
chooseRule( void ) {
   char *str;

   XtUnmanageChild( rule_selection_warW );

   str = (char *)XtMalloc( 64 + strlen(selected_kb)
                              + strlen(selected_ruleset) );
   sprintf( str, "(emaxps-rule-get-rules-of-rset '%s '%s)",
      selected_kb,  selected_ruleset );
   LISP_sndCmd( str );
   XtFree( str );
}

static void
chooseRuleCB( void ) {  

   if( unsaved_changes ) {
      XtManageChild( rule_selection_warW );
      return;
   }
   chooseRule();
}

/*******************************************************************************
*   reaction on (emaxps-rule-get-rsets-of-kb 'kb)
*******************************************************************************/

void
ruleGetRulesOfRset( void ) {
   char *str, *p, *nextstr;
   XmStringTable xmrule_list;
   int length, i, a;
   Widget w;

   p = str = MSG_read();

   if( !*str ) {
      w = XmSelectionBoxGetChild( rule_selectionW, XmDIALOG_LIST );
      XmListDeleteAllItems( w );
      XtManageChild( rule_selectionW );
      XtFree( str );
      return;
   }
   
   length = strlen( str );
   xmrule_list = (XmString *)XtMalloc( (2 + length>>1) * sizeof(XmString ));
   
   i = 0;
   while( nextstr = GetNextPartStr( str ) ) {
      xmrule_list[i] = XmStringCreateSimple( str );
      i++;
      str = nextstr;
   }
                             
   XtVaSetValues( rule_selectionW,
      XmNlistItems,   xmrule_list,
      XmNlistItemCount,         i,
      NULL );
        
   for( a=0; i<i; a++ )
      XmStringFree( xmrule_list[a] );
   XtFree( (char*)xmrule_list );
   XtFree( p );

   XtManageChild( rule_selectionW );
}

/*******************************************************************************
*   Callback of Ok-Button in Rule-SelectionMenu
*******************************************************************************/

static void
rule_selection( Widget w, XtPointer client_data,
                   XmSelectionBoxCallbackStruct *cbs ) {
   char *selection, *str;
   XmString selected_cstr;

   if( !(selection = selectionMenuOk( cbs )) ) {
      XtManageChild( rule_selection_msgW ); 
      return;
   }

   XtUnmanageChild( rule_selectionW );
             
   if( selected_rule ) {
      if( !strcmp( selection, selected_rule )) {  
         XtFree( selection );
         return;
      }      
   }
   
   if( selected_rule )
      XtFree( selected_rule );
                                  
   selected_rule = (char *)XtMalloc( 1 + strlen( selection ));
   strcpy( selected_rule, selection );
   XtFree( selection );

   selected_cstr = XmStringCreateSimple( selected_rule );
   XtVaSetValues( rule_buttonW, XmNlabelString, selected_cstr, NULL );
   XmStringFree( selected_cstr );

   Rule_editable( True );

   Change_False(); 

   if( newRule ) {
      editor_reset();
      return;
   }    
    
   str = (char *)XtMalloc( 64 + strlen(selected_rule)
                              + strlen(selected_ruleset)
                              + strlen(selected_kb) );

   sprintf( str, "(emaxps-rule-get-body-of-rule '%s '%s '%s)",
      selected_rule, selected_ruleset, selected_kb );

   LISP_sndCmd( str );

   XtFree( str );
}


static void 
rule_selectionCB( Widget w, XtPointer client_data,
                  XmSelectionBoxCallbackStruct *cbs ) {

   newRule = False;
   rule_selection( w, client_data, cbs );
}

/*****************************************************************************/

static void 
rule_new_selectionCB( Widget w, XtPointer client_data,
                      XmSelectionBoxCallbackStruct *cbs ) {

   newRule = True;
   rule_selection( w, client_data, cbs );
}

/******************************************************************************
*   reaction on (emaxps-rule-get-body-of-rule 'rule 'rset 'kb)                *
******************************************************************************/

void
ruleGetBodyOfRule( void ) {
   char *str, *junctor, *actiontyp, *if_text, *then_text, *tmp;
   int c, i, k, count;

   str = MSG_read();
   if( !*str ) {
      XtFree( str );
      return;
   }

   junctor = str;
   if_text = GetNextPartStr( junctor );
   actiontyp = GetNextPartStr( if_text );
   then_text = GetNextPartStr( actiontyp );

   if_text = strchr( if_text, '(' );     
   if_text++;
   tmp = strrchr( if_text, ')' );
   *tmp = 0;

   then_text = strchr( then_text, '(' );     
   then_text++;
   tmp = strrchr( then_text, ')' );
   *tmp = 0;

   excl_enable = 0;

   /* set junctor */
   for( i=0; i<=7; i++ )
      if( !strcmp( junctor_str[i], junctor ) )
         XtVaSetValues( junctor_optionW,
            XmNmenuHistory, junctor_button[i],
            NULL );

   /* set if-text */
   XtVaSetValues( if_textW,
      XmNvalue, if_text,
      NULL );

   /* set actiontype */
   for( i=0; i<=3; i++ )
      if( !strcmp( actiontype_str[i], actiontyp ) )
         XtVaSetValues( actiontype_optionW,
            XmNmenuHistory, actiontype_button[i],
            NULL );

   /* set then-text */
   XtVaSetValues( then_textW,
      XmNvalue, then_text,
      NULL );

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
RULE_show( void ) {

   PopupRaiseAndDeiconify( Rule_shellW );
}

/******************************************************************************/

void
RULE_hide( void ) {

   XtPopdown( Rule_shellW );
}

/*******************************************************************************
*   Change Cursor of RuleEditor
*******************************************************************************/

void
RULE_ready( void ) {

   if( XtIsRealized( Rule_shellW ) )
      XDefineCursor( XtDisplay( Rule_shellW ),
                     XtWindow( Rule_shellW ),
                     cursor_normal );
}

/******************************************************************************/

void
RULE_busy( void ) {

   if( XtIsRealized( Rule_shellW ) )
      XDefineCursor( XtDisplay( Rule_shellW ),
                     XtWindow( Rule_shellW ),
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
*   Junctor-OptionMenu of Rule-Editor                                          *
*******************************************************************************/  

static Widget
createJunctorOption( Widget parent, char* name ) {
   Widget rowcolW, junctor_menu, option_menuW, option_button;
   Arg args[10];
   int i, n;
   XmString cstr;

   rowcolW = XmCreateRowColumn( parent, "junctor_rowcol", NULL, 0 );
   XtManageChild( rowcolW );

   junctor_menu = XmCreatePulldownMenu( rowcolW,
      "junctor_menu", NULL, 0 );

   XtSetArg( args[0], XmNsubMenuId, junctor_menu );
   option_menuW = XmCreateOptionMenu( rowcolW, name, args, 1 );
   XtManageChild( option_menuW );

   /* Configuration of OptionMenu-Button */
   option_button = XmOptionButtonGadget( option_menuW );
   XtVaSetValues( option_button,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft, 3,
      NULL );
   XtAddCallback( option_button, XmNactivateCallback,
      (XtPointer)Change_True, NULL );

   for( i = 0; i < (int) XtNumber( junctor_str ); i++ ) {
      cstr = XmStringCreateSimple( junctor_str[i] );
      n = 0;
      XtSetArg( args[n], XmNlabelString, cstr ); n++;
      XtSetArg( args[n], XmNmarginLeft,     1 ); n++;
      junctor_button[i] = XmCreatePushButtonGadget( junctor_menu, 
                                 "junctor_button",  args, n );
      XmStringFree( cstr );
      XtAddCallback( junctor_button[i], XmNactivateCallback,
         (XtPointer)Change_True, NULL );
   }                                                                           
   XtManageChildren( junctor_button, 8 );

   /* Default-Entry: $False */
   XtVaSetValues( option_menuW, XmNmenuHistory, junctor_button[0], NULL );

   return option_menuW;
}

/*******************************************************************************
*   Actiontype-OptionMenu of Rule-Editor 
*******************************************************************************/  

static Widget
createActionOption( Widget parent, char* name ) {
   Widget rowcolW, actiontype_menu, option_menuW, option_button;
   Arg args[5];
   int i, n;
   XmString cstr;

   rowcolW = XmCreateRowColumn( parent, "actiontype_rowcol", NULL, 0 );
   XtManageChild( rowcolW );

   actiontype_menu = XmCreatePulldownMenu( rowcolW,
      "actiontype_menu", NULL, 0 );

   XtSetArg( args[0], XmNsubMenuId, actiontype_menu );
   option_menuW = XmCreateOptionMenu( rowcolW, name, args, 1 );
   XtManageChild( option_menuW );

   /* Configuration of OptionMenu-Button */
   option_button = XmOptionButtonGadget( option_menuW );
   XtVaSetValues( option_button,
      XmNalignment, XmALIGNMENT_BEGINNING,
      XmNmarginLeft, 3,
      NULL );
   XtAddCallback( option_button, XmNactivateCallback,
      (XtPointer)Change_True, NULL );

   for( i = 0; i < (int) XtNumber( actiontype_str ); i++ ) {
      cstr = XmStringCreateSimple( actiontype_str[i] );
      n = 0;
      XtSetArg( args[n], XmNlabelString, cstr ); n++;
      XtSetArg( args[n], XmNmarginLeft,     1 ); n++;
      actiontype_button[i] = XmCreatePushButtonGadget( actiontype_menu, 
                                 "actiontype_button",  args, n );
      XmStringFree( cstr );
      XtAddCallback( actiontype_button[i], XmNactivateCallback,
         (XtPointer)Change_True, NULL );
   }                                                                           
   XtManageChildren( actiontype_button, 3 );

   /* Default-Entry: $Execute */
   XtVaSetValues( option_menuW, XmNmenuHistory, actiontype_button[1], NULL );

   return option_menuW;
}

/*******************************************************************************
*   create Rule-Editor                                                         *
*******************************************************************************/
          
void
RULE_create( Widget parent ) {
   int n;
   Arg args[10];
   Widget form0, form1, form2, form3, menuBar, edit_menu, edit_menu_button,
      menu_sep1, menu_sep2, if_form, then_form,
      help_menu, help_menu_button, on_menu_button, on_syntax_button,
      kb_label, ruleset_label, rule_label,
      sep, if_label, panedWin,  then_label, w,
      radioBoxW, RowColW, move_labelW, move_sep;

   /*** Shell ***/
   n = 0;
   XtSetArg( args[n], XmNtransient,                     True ); n++;
   XtSetArg( args[n], XmNdeleteResponse,        XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconPixmap,                  iconPm ); n++;  

   Rule_shellW = XtAppCreateShell( "Rule-Editor", "HereClassIsIgnored",
                                    /* instance              class */
      topLevelShellWidgetClass, XtDisplay( parent ), args, n );

   /*** install callback for the CLOSE-Button of the mwm-border menu ***/
   XmAddWMProtocolCallback( Rule_shellW, WM_DELETE_WINDOW,
      (XtPointer)RULE_hide, NULL );        /* Pop down ... */

   /***************   aeusseres Form-Widget   **************/
   form0 = XtVaCreateManagedWidget( "form0",
      xmFormWidgetClass, Rule_shellW,
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
      (XtPointer)HELP_show, (XtPointer)HELP_RULE_MENU );

   /***************   inneres Form-Widget   ****************/
   form1 = XtVaCreateManagedWidget( "form1",
      xmFormWidgetClass, form0,
      XmNtopAttachment,      XmATTACH_FORM,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      NULL );

   /******************   MenuBar-Widget   ******************/
   menuBar =  XmVaCreateSimpleMenuBar( form1, "menuBar",
      XmNtopAttachment,      XmATTACH_FORM,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      NULL );
   XtManageChild( menuBar );

   /***   Edit-Menu   ***/
   edit_menu = XmCreateSimplePulldownMenu( menuBar, "edit_menu", NULL, 0 );
   edit_menu_button = XtVaCreateManagedWidget( "edit_menu_button",
      xmCascadeButtonGadgetClass, menuBar,
      XmNsubMenuId,     edit_menu,
      NULL );

   save_buttonW = XtVaCreateManagedWidget( "save_button",
      xmPushButtonGadgetClass, edit_menu,
      NULL );
   XtAddCallback( save_buttonW, XmNactivateCallback,
      (XtPointer)save_ruleCB, NULL );
   XtSetSensitive( save_buttonW,    False );

   menu_sep1 = XtVaCreateManagedWidget( "menu_sep1",
      xmSeparatorGadgetClass, edit_menu, NULL );

   del_rule_buttonW = XtVaCreateManagedWidget( "del_rule_button",
      xmPushButtonGadgetClass, edit_menu, NULL );
   XtAddCallback( del_rule_buttonW, XmNactivateCallback,
      (XtPointer)del_ruleCB, NULL );
   XtSetSensitive( del_rule_buttonW, False );

   del_ruleset_buttonW = XtVaCreateManagedWidget( "del_ruleset_button",
      xmPushButtonGadgetClass, edit_menu,
      NULL );
   XtAddCallback( del_ruleset_buttonW, XmNactivateCallback,
      (XtPointer)del_ruleSetCB, NULL );
   XtSetSensitive( del_ruleset_buttonW, False );

   menu_sep2 = XtVaCreateManagedWidget( "menu_sep2",
      xmSeparatorGadgetClass, edit_menu,
      NULL );

   undo_buttonW = XtVaCreateManagedWidget( "undo_button",
      xmPushButtonGadgetClass, edit_menu,
      NULL );
   XtAddCallback( undo_buttonW, XmNactivateCallback,
      (XtPointer)undoCB, NULL );
   XtSetSensitive( undo_buttonW,    False );

   move_rule_buttonW = XtVaCreateManagedWidget( "move_rule_button",
      xmPushButtonGadgetClass, edit_menu,
      NULL );
   XtAddCallback( move_rule_buttonW, XmNactivateCallback,
      (XtPointer)orderCB, NULL );

   /***   Help-Menu   ***/
   help_menu = XmCreateSimplePulldownMenu( menuBar, "help_menu", NULL, 0 );
   help_menu_button = XtVaCreateManagedWidget( "help_menu_button",
      xmCascadeButtonGadgetClass, menuBar,
      XmNsubMenuId,     help_menu,
      NULL );
   on_menu_button = XtVaCreateManagedWidget( "on_menu_button",
      xmPushButtonGadgetClass, help_menu,
      NULL );
   XtAddCallback( on_menu_button, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_RULE_MENU );
   XtVaSetValues( menuBar, XmNmenuHelpWidget, help_menu_button, NULL );

   on_syntax_button = XtVaCreateManagedWidget( "on_syntax",
      xmPushButtonGadgetClass, help_menu, NULL );
   XtAddCallback( on_syntax_button, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_BABYLON_SYNTAX );

   exclamation_SignW = XtVaCreateWidget( "unsaved_changes_label",
      xmLabelWidgetClass, form1,
      XmNlabelType,                 XmPIXMAP,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  menuBar,
      XmNbottomAttachment,     XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
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
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,                menuBar,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      NULL );

   /***************   RuleSet-Label-Widget   ***************/
   ruleset_label = XtVaCreateManagedWidget( "ruleset_label",
      xmLabelWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,               kb_label,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      NULL );

   /*****************   Rule-Label-Widget   ****************/
   rule_label = XtVaCreateManagedWidget( "rule_label",
      xmLabelWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,          ruleset_label,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      NULL );

   /**********   KnowledgeBase-PushButton-Widget   *********/
   kb_buttonW = XtVaCreateManagedWidget( "kb_button",
      xmPushButtonWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,                menuBar,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,              kb_label,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      XmNrecomputeSize,               True,
      NULL );
   XtAddCallback( kb_buttonW, XmNactivateCallback,
      (XtPointer)chooseKbCB, NULL );
   /*
    *  Um die ueber XResources eingestellte Beschriftung der Buttons
    *  staendig verfuegbar zu haben:
    */
   XtVaGetValues( kb_buttonW, XmNlabelString, &unnamed_str, NULL );

   /************   RuleSet-PushButton-Widget   *************/
   ruleset_buttonW = XtVaCreateManagedWidget( "ruleset_button",
      xmPushButtonWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,             kb_buttonW,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,         ruleset_label,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      XmNrecomputeSize,               True,
      NULL );
   XtAddCallback( ruleset_buttonW, XmNactivateCallback,
      (XtPointer)chooseRuleSetCB, NULL );
   XtSetSensitive( ruleset_buttonW, False );

   /***************   Rule-PushButton-Widget   *************/
   rule_buttonW = XtVaCreateManagedWidget( "rule_button",
      xmPushButtonWidgetClass, RowColW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,         ruleset_buttonW,
      XmNleftAttachment,   XmATTACH_WIDGET,
      XmNleftWidget,            rule_label,
      XmNrightAttachment,    XmATTACH_FORM,
      XmNalignment,  XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                     3,
      XmNrecomputeSize,               True,
      NULL );
   XtAddCallback( rule_buttonW, XmNactivateCallback,
      (XtPointer)chooseRuleCB, NULL );
   XtSetSensitive( rule_buttonW,    False );

   /*****************   Separator-Widget   *****************/
   sep = XtVaCreateManagedWidget( "sep",
      xmSeparatorGadgetClass, form0,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,                  form1,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      NULL );

   /***** form-Widget fuer die zwei scrolledTextWidgets *****/
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

   /***************   inner if-Form-Widget   ****************/
   if_form = XtVaCreateManagedWidget( "if_form",
      xmFormWidgetClass, panedWin,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNpaneMinimum,                    100,
      NULL );

   /************   inner then-Form-Widget   ****************/
   then_form = XtVaCreateManagedWidget( "then_form",
      xmFormWidgetClass, panedWin,
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,                  if_form,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      XmNpaneMinimum,                    100,
      NULL );

   /*****************   if-Label-Widget   ******************/
   if_label = XtVaCreateManagedWidget( "if_label",
      xmLabelWidgetClass, if_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                       3,
      NULL );

   /*****************   Junctor-OptionMenu   ***************/
   junctor_optionW = createJunctorOption( if_form, "junctor_option" );
   XtVaSetValues( XtParent( junctor_optionW ),
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,     XmATTACH_WIDGET,
      XmNleftWidget,                if_label,
      XmNrightAttachment,      XmATTACH_FORM,
      NULL );

   /**************   if-ScrolledText-Widget   **************/
   XtSetArg( args[0], XmNeditMode, XmMULTI_LINE_EDIT );
   if_textW = XmCreateScrolledText( if_form, "if_textWin", args, 1 );
   XtAddCallback( if_textW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtVaSetValues( XtParent( if_textW ),
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,          junctor_optionW,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      NULL );
   XtManageChild( if_textW );
   XtAddCallback( if_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );

   /*****************   then-Label-Widget   ****************/
   then_label = XtVaCreateManagedWidget( "then_label",
      xmLabelWidgetClass, then_form,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNalignment,    XmALIGNMENT_BEGINNING,
      XmNmarginLeft,                       0,
      NULL );

   /***************   Action-type OptionMenu   *************/
   actiontype_optionW = createActionOption( then_form, "actiontype_option" );
   XtVaSetValues( XtParent( actiontype_optionW ),
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,     XmATTACH_WIDGET,
      XmNleftWidget,              then_label,
      XmNrightAttachment,      XmATTACH_FORM,
      NULL );
   XtSetSensitive( actiontype_optionW, False );

   /*************   then-ScrolledText-Widget   *************/
   XtSetArg( args[0], XmNeditMode, XmMULTI_LINE_EDIT );
   then_textW = XmCreateScrolledText( then_form, "then_textWin", args, 1 );
   XtAddCallback( then_textW, XmNhelpCallback,
      (XtPointer)HELP_showBrowserCB, NULL );
   XtVaSetValues( XtParent( then_textW ),
      XmNtopAttachment,      XmATTACH_WIDGET,
      XmNtopWidget,       actiontype_optionW,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNbottomAttachment,     XmATTACH_FORM,
      NULL );
   XtManageChild( then_textW );
   XtAddCallback( then_textW, XmNvalueChangedCallback,
      (XtPointer)Change_True, NULL );

   unsaved_changes = False;
   Rule_editable( False );   

   /******************************************************************************
   *   Here all Selection-Menus                                                  *
   ******************************************************************************/

   /********   KnowledgeBase-SelectionMenu-Widget   ********/
   kb_selectionW = createSelectionMenu( form0, "kb_selection" );
 
   /*** only KnowledgeBases already loaded can be chosen from ***/
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( kb_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( kb_selectionW, XmNokCallback,
      (XtPointer)kb_selectionCB, NULL );

   /************   RuleSet-SelectionMenu-Widget   **********/
   ruleset_selectionW = createSelectionMenu( form0, "ruleset_selection" );

   w = XmSelectionBoxGetChild( ruleset_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtVaSetValues( ruleset_selectionW, XmNmustMatch, True, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( ruleset_selectionW, XmNokCallback,
      (XtPointer)ruleset_selectionCB, NULL );
   XtAddCallback( ruleset_selectionW, XmNnoMatchCallback,
      (XtPointer)ruleset_new_selectionCB, NULL );

   /*************   Rule-SelectionMenu-Widget   ************/
   rule_selectionW = createSelectionMenu( form0, "rule_selection" );

   w = XmSelectionBoxGetChild( rule_selectionW, XmDIALOG_TEXT );
   UPCASE_input( w );

   XtVaSetValues( rule_selectionW, XmNmustMatch, True, NULL );

   /* install callbacks for Selection-Buttons */
   XtAddCallback( rule_selectionW, XmNokCallback,
      (XtPointer)rule_selectionCB, NULL );
   XtAddCallback( rule_selectionW, XmNnoMatchCallback,
      (XtPointer)rule_new_selectionCB, NULL );

   /************ Move Rule Selection Box *****************/
   n = 0;
   XtSetArg( args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL ); n++;
   XtSetArg( args[n], XmNiconPixmap,                           iconPm ); n++;
   move_selection_formW = XmCreateFormDialog( form0,
                      "move_selection_form", args, n );
   XtVaSetValues( move_selection_formW, XmNdeleteResponse,
                  XmDO_NOTHING, NULL );
   /*install callback for the CLOSE-Button of the mwm-border menu */
   XmAddWMProtocolCallback( XtParent( move_selection_formW ),
                            WM_DELETE_WINDOW, (XtPointer)unmanageCB,
                            (XtPointer)move_selection_formW );
   move_labelW = XtVaCreateManagedWidget( "move_label",
      xmLabelWidgetClass, move_selection_formW,
      XmNtopAttachment,        XmATTACH_FORM,
      XmNleftAttachment,       XmATTACH_FORM,
      XmNrightAttachment,      XmATTACH_FORM,
      XmNmarginLeft,                       3,
      NULL );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,            move_labelW ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++; 
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNpacking,            XmPACK_COLUMN ); n++;
   XtSetArg( args[n], XmNorientation,         XmHORIZONTAL ); n++;
   
   radioBoxW = XmCreateRadioBox( move_selection_formW, "radio", args, n );
   XtManageChild( radioBoxW );
   
   n = 0;
   XtSetArg( args[n], XmNset, True ); n++;
   XtSetArg( args[n], XmNalignment,    XmALIGNMENT_BEGINNING); n++;
   radio_beforeW = XtCreateManagedWidget( "radio_before",
      xmToggleButtonWidgetClass, radioBoxW, args, n );
   
   n = 0;
   XtSetArg( args[n], XmNset,  False); n++;
   XtSetArg( args[n], XmNalignment,     XmALIGNMENT_CENTER ); n++;
   radio_afterW = XtCreateManagedWidget( "radio_after",
      xmToggleButtonWidgetClass, radioBoxW, args, n );

   move_sep = XtVaCreateManagedWidget( "move_sep",
      xmSeparatorGadgetClass, move_selection_formW,
      XmNtopAttachment,    XmATTACH_WIDGET,
      XmNtopWidget,              radioBoxW,
      XmNleftAttachment,     XmATTACH_FORM,
      XmNrightAttachment,    XmATTACH_FORM,
      NULL );

   n = 0;
   XtSetArg( args[n], XmNtopAttachment,    XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,               move_sep ); n++;
   XtSetArg( args[n], XmNleftAttachment,     XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,    XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,   XmATTACH_FORM ); n++;
   move_selectionW = XmCreateSelectionBox( move_selection_formW,
                                          "move_selection",
                                           args, n );
   XtManageChild( move_selectionW );
   
   XtAddCallback( move_selectionW, XmNokCallback,
      (XtPointer)moveokCB, NULL );

   XtAddCallback( move_selectionW, XmNcancelCallback,
      (XtPointer)unmanageCB, (XtPointer)move_selection_formW);

   /***** ungenutztes Eingabe-Label unmanagen *****/
   w = XmSelectionBoxGetChild( move_selectionW, XmDIALOG_HELP_BUTTON );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( move_selectionW, XmDIALOG_SELECTION_LABEL );
   XtUnmanageChild( w );
   w = XmSelectionBoxGetChild( move_selectionW, XmDIALOG_TEXT );
   XtUnmanageChild( w );

/******************************************************************************
*   Here all Message-Boxes                                                    *
******************************************************************************/
   /***   KnowledgeBase-SelectionMenu-MessageBox-Widget  ***/
   kb_selection_msgW = createMessageBox( form0, "kb_selection_msg" );

   /******   RuleSet-SelectionMenu-MessageBox-Widget  ******/
   ruleset_selection_msgW = createMessageBox( form0, "ruleset_selection_msg" );

   /*******   Rule-SelectionMenu-MessageBox-Widget  ********/
   rule_selection_msgW = createMessageBox( form0, "rule_selection_msg" );

   /*******   Order-SelectionMenu-MessageBox-Widget  ********/
   move_selection_msgW = createMessageBox( form0, "move_selection_msg" );

/******************************************************************************
*   Here all Warning-Boxes                                                    *
******************************************************************************/
   /*******   Kb-SelectionMenu-WarningBox-Widget  ********/
   kb_selection_warW = createWarningBox( form0, "kb_selection_war" );
   XtAddCallback( kb_selection_warW, XmNokCallback,
         (XtPointer)chooseKb, NULL );

   /*******   RuleSet-SelectionMenu-WarningBox-Widget  ********/
   ruleset_selection_warW = createWarningBox( form0, "ruleset_selection_war" );
   XtAddCallback( ruleset_selection_warW, XmNokCallback,
         (XtPointer)chooseRuleSet, NULL );
         
   /*******   Rule-SelectionMenu-WarningBox-Widget  ********/
   rule_selection_warW = createWarningBox( form0, "rule_selection_war" );
   XtAddCallback( rule_selection_warW, XmNokCallback,
         (XtPointer)chooseRule, NULL );

   /*******   save-WarningBox-Widget  ********/
   save_warW = createWarningBox( form0, "save_war" );
   XtAddCallback( save_warW, XmNokCallback,
         (XtPointer)save_rule, NULL );

   /*******   delRule-WarningBox-Widget  ********/
   del_rule_warW = createWarningBox( form0, "del_rule_war" );
   XtAddCallback( del_rule_warW, XmNokCallback,
         (XtPointer)del_rule, NULL );

   /*******   delRuleSet-WarningBox-Widget  ********/
   del_ruleSet_warW = createWarningBox( form0, "del_ruleset_war" );
   XtAddCallback( del_ruleSet_warW, XmNokCallback,
         (XtPointer)del_ruleSet, NULL );
}

/*** EOF **************************************************/


/****************************************************
*   rule.h        Hans Groschwitz        19.07.95   *
*                 Susanne Wellmann                  *
*                 Klaus Senf                        *
*                 Michael Block                     *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the rule-editor.             *
*   Full version only.                              *
****************************************************/

extern void RULE_ready( void );
extern void RULE_busy( void );
extern void RULE_editable( Boolean );
extern void RULE_show( void );
extern void RULE_hide( void );
extern void RULE_create( Widget );
extern void RULE_kbClosed( char* );

extern void ruleGetRsetsOfKB( void );
extern void ruleGetRulesOfRset( void );
extern void ruleGetRulesOfRset2( void );
extern void ruleGetBodyOfRule( void );
extern void ruleSaveRule( void );
extern void ruleMoveRule( void );
extern void ruleMoveRset( void );
extern void ruleCopyRule( void );
extern void ruleCopyRset( void );

/*** EOF *******************************************/

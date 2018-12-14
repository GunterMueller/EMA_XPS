/****************************************************
*   restriction.h    Hans Groschwitz     19.07.95   *           
*                    Karsten Vossberg               *   
*                    Klaus Senf                     *   
*                                                   *
*   Functions to build the restriction-editor.      *   
*   Full version only.                              *
****************************************************/

extern void RESTRICTION_create( Widget );
extern void RESTRICTION_ready( void );
extern void RESTRICTION_busy( void );
extern void RESTRICTION_show( void );
extern void RESTRICTION_hide( void );
extern void RESTRICTION_kbClosed( char* );

extern void emaxps_restriction_def( void );
extern void emaxps_restriction_sel( void );

/*** EOF *******************************************/

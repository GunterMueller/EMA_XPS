/****************************************************
*   prolog.h      Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the prolog-editor.           *
*   Full version only.                              *
****************************************************/

extern void PROLOG_create( Widget );
extern void PROLOG_ready( void );
extern void PROLOG_busy( void );
extern void PROLOG_show( void );
extern void PROLOG_hide( void );
extern void PROLOG_kbClosed( char* );

extern void emaxps_prolog_klauselmengen( void );
extern void emaxps_prolog_select( void );
extern void emaxps_new_axioms( void );
extern void emaxps_new_kbaxiom( void );
extern void emaxps_del_kbaxiom( void );
extern void emaxps_reset_prolog_of_kb( void );

/*** EOF *******************************************/

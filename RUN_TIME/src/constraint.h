/****************************************************
*   constraint.h  Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the constraint-editor.       *
*   Full version only.                              *
****************************************************/

extern void CONSTRAINT_create( Widget );
extern void CONSTRAINT_ready( void );
extern void CONSTRAINT_busy( void );
extern void CONSTRAINT_show( void );
extern void CONSTRAINT_hide( void );
extern void CONSTRAINT_kbClosed( char* );

extern void emaxps_constraint_def( void );
extern void emaxps_constraint_sel( void );

/*** EOF *******************************************/

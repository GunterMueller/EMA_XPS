/****************************************************
*   misc.h        Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the misc-editor.             *
*   Full version only.                              *
****************************************************/

extern void MISC_show( void );
extern void MISC_hide( void );
extern void MISC_create( Widget );
extern void MISC_ready( void );
extern void MISC_busy( void );
extern void MISC_kbClosed( char* );

extern void emaxps_misc_send_others( void );

/*** EOF *******************************************/

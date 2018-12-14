/****************************************************
*   csnet.h       Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the constraint-net-editor.   *
*   Full version only.                              *
****************************************************/

extern void CSNET_create( Widget );
extern void CSNET_ready( void );
extern void CSNET_busy( void );
extern void CSNET_show( void );
extern void CSNET_hide( void );
extern void CSNET_kbClosed( char* );

extern void emaxps_csnet_def( void );
extern void emaxps_csnet_sel( void );

/*** EOF *******************************************/

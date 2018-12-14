/****************************************************
*   frame.h       Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the frame-editor.            *
*   Full version only.                              *
****************************************************/

extern void FRAME_create( Widget );
extern void FRAME_ready( void );
extern void FRAME_busy( void );
extern void FRAME_show( void );
extern void FRAME_hide( void );
extern void FRAME_kbClosed( char* );

extern void emaxps_frame_delete( void );
extern void emaxps_frame_definition( void );
extern void emaxps_frame_check_on( void );
extern void emaxps_frame_selection( void );

/*** EOF *******************************************/

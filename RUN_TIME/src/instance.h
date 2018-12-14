/****************************************************
*   instance.h    Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the instance-editor.         *
*   Full version only.                              *
****************************************************/

extern void INSTANCE_create( Widget );
extern void INSTANCE_ready( void );
extern void INSTANCE_busy( void );
extern void INSTANCE_show( void );
extern void INSTANCE_hide( void );
extern void INSTANCE_kbClosed( char* );

extern void emaxps_instance_definition( void );
extern void emaxps_inst_frame_select( void );
extern void emaxps_inst_instance_select( void );

/*** EOF *******************************************/

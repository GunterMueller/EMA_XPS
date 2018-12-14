/****************************************************
*   behavior.h    Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the behavior-editor.         *
*   Full version only.                              *
****************************************************/

extern void BEHAVIOR_create( Widget );
extern void BEHAVIOR_ready( void );
extern void BEHAVIOR_busy( void );
extern void BEHAVIOR_show( void );
extern void BEHAVIOR_hide( void );
extern void BEHAVIOR_kbClosed( char* );

extern void emaxps_behavior_definition( void );
extern void emaxps_behav_frame_select( void );
extern void emaxps_behav_behavior_select( void );

/*** EOF *******************************************/

/****************************************************
*   task.h        Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the task-editor.             *
*   Full version only.                              *
****************************************************/

extern void TASK_show( void );
extern void TASK_hide( void );
extern void TASK_create( Widget );
extern void TASK_ready( void );
extern void TASK_busy( void );
extern void TASK_kbClosed( char* );

extern void emaxps_task_list_tasks( void );
extern void emaxps_task_send_body( void );

/*** EOF *******************************************/

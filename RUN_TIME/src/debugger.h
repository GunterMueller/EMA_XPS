/****************************************************
*   debugger.h    Hans Groschwitz        19.07.95   *
*                                                   *
*   Simulates a terminal-like window for working    *
*   with the LISP-debugger, called a PAD.           *
****************************************************/

extern int    PAD_directly_read;

/*** Functions *************************************/

extern Widget PAD_create( Widget );
extern void   PAD_append( int );
extern void   PAD_ready( void );
extern void   PAD_busy( void );
extern void   PAD_show( void );
extern void   PAD_hide( void );
extern void   PAD_Pupdate( char* );

/*** EOF *******************************************/

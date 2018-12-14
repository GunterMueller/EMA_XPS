/****************************************************
*   main.h        Hans Groschwitz        06.10.94   *
*                                                   *
*   A graphic front-end for a LISP-process          *
*   derived from a xterm but quite different to it  *
****************************************************/

/*** ASCII codes ***/

#define NUL   00  /* End of Message */
#define ETX   03  /* sent by LISP to separate message parts */
#define EOT   04  /* sent by LISP to represent a ``NIL'' */
#define CAN   24  /* sent by LISP to avoid a reply (async protocol) */
#define ESC   27  /* begin a vchannel change */

/*** Globals ***************************************/

extern Atom   WM_DELETE_WINDOW;
extern Pixmap iconPm, savePm;
extern Cursor cursor_normal, cursor_busy;
extern Widget SystemDialogs;
extern char   geometryConstraint[];
extern char  *ARGV_single_command;

/*** Function Declarations *************************/

extern char*  MSG_read( void );
extern void   LISP_sndReply( char* );
extern void   LISP_sndCmd( char* );
extern void   LISP_interrupt( void );
extern void   LISP_kill( void );
extern void   LISP_run( void );
extern Pixmap createPushButtonPixmap( Widget, char* );
extern Pixmap createPixmapFromFile( Widget, char*, Pixel, Pixel );
extern void   destroyPixmap( Widget, Pixmap );
extern void   ARGV_kb_settings( char**, char** );
extern void   PopupRaiseAndDeiconify( Widget );

/*** EOF *******************************************/

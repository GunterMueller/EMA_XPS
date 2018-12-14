/****************************************************
*   xinit.h       Hans Groschwitz        06.10.94   *
*                                                   *
*   Creates a welcoming PopupBox and the MainMenu   *
****************************************************/

extern char  *EMAXPS_source_tree;
extern char  *EMAXPS_make_file_name;
extern char  *EMAXPS_lisp_filename;

#define MOD_VALID        0x001
#define MOD_FRAME        0x002
#define MOD_INSTANCE     0x004
#define MOD_BEHAVIOR     0x008
#define MOD_RULE         0x010
#define MOD_PROLOG       0x020
#define MOD_CONSTRAINT   0x040
#define MOD_CSNET        0x080
#define MOD_RESTRICTION  0x100
#define MOD_TASK         0x200
#define MOD_MISC         0x400
#define MOD_ALL_EDITORS  0x7FE  /* !!! */

/*** Functions *************************************/

extern int    HELP_maxPages( void );
extern void   PIXMAP_createChanged( Widget, Pixmap*, Pixmap* );
extern void   UPCASE_input( Widget );
extern void   XINIT_complete( Widget, int );
extern void   KBS_getState( char* );
extern void   MAINMENU_create( Widget );
extern Widget WELCOME_create( Widget );
extern void   WELCOME_ready( void );
extern void   WELCOME_busy( void );
extern void   WELCOME_show( void );
extern void   WELCOME_hide( void );
extern void   MAINMENU_ready( void );
extern void   MAINMENU_busy( void );
extern void   MAINMENU_show( void );
extern void   XAPPL_showLispState( int );
extern char **KNOWNKBS_getList( void );
extern void   C_set_modified( char*, int, int );

/*** EOF *******************************************/

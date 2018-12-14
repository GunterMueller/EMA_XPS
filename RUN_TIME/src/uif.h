/******************************************************************************/
/*                               H. Groschwitz                                */
/*       uif.h                   Michael Block               21.07.1994       */
/*                                                                            */
/*                   Executing C-Functions run from C-FrontEnds               */
/*                   of LISP functions for control of                         */
/*                   User Interface (Session Screen)                          */
/*                                                                            */
/******************************************************************************/

#define UiSEPARATOR      1       /* Widget Class */
#define UiPUSHBUTTON     2
#define UiLABEL          3
#define UiBITMAPBUTTON   4
#define UiBITMAPLABEL    5
#define UiINPUT_TEXT     6
#define UiOUTPUT_TEXT    7
#define UiPULLDOWN_MENU  8
#define UiMENU_ENTRY     9

#define UiNIL            0       /* sensitivity, visibility */
#define UiTRUE           1       /* Args and Returns */

#define UiOK             0       /* Returns of boolean ui_xxx's */
#define UiERROR          (-1)

#define UiLEFT           1       /* Args and Returns */
#define UiRIGHT          2
#define UiCENTER         3

#define UiKEEP           (-2)    /* set: dont change, create: use Xres */
                                 /* possible return value, too */

#define UiERRORP         ((char*)UiERROR)  /* last 'P' for Pointer */
#define UiKEEPP          ((char*)UiKEEP)   /* LISP sent Nil in spite of "str" */

#define UiNO_SESSION     0       /* Session Type */
#define UiSIMPLE_SESSION 1
#define UiSESSION        2

#define UiFG_COLOR       1       /* for reset_resource */
#define UiBG_COLOR       2
#define UiFONT           3
#define UiALIGNMENT      4
#define UiTEXT           5

#define UiNORMAL         1       /* CursorType: XC_left_ptr                   */
#define UiBUSY           2       /*             XC_watch                      */

#define UiSIMPLE_SESSION_OUTPUT "session-output"  /* names of standard-output */
#define UiSIMPLE_SESSION_INPUT  "session-input"   /*      and standard-input  */
                                                  /* on SimpleSessionScreen   */

/******************************************************************************/
/*     ui_xxx Executing Functions ...                                         */
/******************************************************************************/

extern void SESSION_ready( void );
extern void SESSION_busy( void );

/******************************************************************************/
/*     Event orientation controls                                             */
/******************************************************************************/

extern int ui_create_simple_session_screen( void );
extern int ui_stop_session( void );

/******************************************************************************/
/*     Basic Widget Operations                                                */
/******************************************************************************/

extern int ui_destroy_widget( char *name );
extern int ui_raise_to_top( char *name );
extern int ui_lower_to_bottom( char *name );

/******************************************************************************/
/*     Widget Creation Functions                                              */
/******************************************************************************/

extern int ui_create_separator( char *name, int x, int y, int w, int h,
              char *fg );
extern int ui_create_pushbutton( char *name, int x, int y, int w, int h,
              char *label, char *font, int alignment, char *fg, char *bg,
              char *cb );
extern int ui_create_label( char *name, int x, int y, int w, int h,
              char *label, char *font, int alignment, char *fg, char *bg );
extern int ui_create_bitmap_button( char *name, int x, int y, int w, int h,
              char *path, char *fg, char *bg, char *cb );
extern int ui_create_bitmap_label( char *name, int x, int y, int w, int h,
              char *path, char *fg, char *bg );
extern int ui_create_input_text( char *name, int x, int y, int w, int h,
              char *font, char *fg, char *bg );
extern int ui_create_output_text( char *name, int x, int y, int w, int h,
              char *text, char *font, char *fg, char *bg );
extern int ui_create_pulldown_menu( char *name,
              char* labelText, int mnemonic );
extern int ui_create_menu_entry( char* name, char *pd_menu_name,
              char *labelText, int mnemonic, char *callbackMsg );

/******************************************************************************/
/*     Resource Set Operations                                                */
/******************************************************************************/

extern int ui_set_session_bg( char *value );
extern int ui_set_menu_bg( char *value );
extern int ui_set_menu_fg( char *value );
extern int ui_set_menu_font( char *value );
extern int ui_set_visibility( char *name, int value );
extern int ui_set_sensitivity( char *name, int value );
extern int ui_set_foreground( char *name, char *value );
extern int ui_set_background( char *name, char *value );
extern int ui_set_font( char *name, char *value );
extern int ui_set_bitmap( char *name, char *path );
extern int ui_set_geometry( char *name, int x, int y, int w, int h );
extern int ui_set_alignment( char *name, int value );
extern int ui_set_text( char *name, char *text );
extern int ui_add_text( char *name, char *text );
extern int ui_set_callback( char *name, char *value );
extern int ui_set_mnemonic( char *name, int value );

/******************************************************************************/
/*     Resource Get Operations                                                */
/******************************************************************************/

extern char *ui_get_session_bg( void );
extern char *ui_get_menu_bg( void );
extern char *ui_get_menu_fg( void );
extern char *ui_get_menu_font( void );
extern int   ui_get_visibility( char *name );
extern int   ui_get_sensitivity( char *name );
extern char *ui_get_foreground( char *name);
extern char *ui_get_background( char *name);
extern char *ui_get_font( char *name);
extern char *ui_get_bitmap( char *name);
extern char *ui_get_geometry( char *name );
extern int   ui_get_alignment( char *name);
extern char *ui_get_text( char *name );
extern char *ui_get_callback( char *name);
extern int   ui_get_mnemonic( char *name);

/******************************************************************************/
/*     Save Current User-Interface                                            */
/******************************************************************************/

extern int ui_save( char *command_name, char *resource_name );

/******************************** EOF *****************************************/

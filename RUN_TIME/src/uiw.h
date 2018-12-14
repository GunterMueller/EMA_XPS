/******************************************************************************/
/*                               H. Groschwitz                                */
/*       uiw.h                   Michael Block               21.07.1994       */
/*                                                                            */
/*                   Executing C-Functions run from C-FrontEnds               */
/*                   of LISP functions for control of                         */
/*                   User Interface (Session Screen)                          */
/*                                                                            */
/******************************************************************************/

typedef struct _wname {
   struct _wname *next;
   char *name;
   Widget w;
   Widget cascade_button;                  /* only relevant for PulldownMenu */
   int class;
   int x;
   int y;
   int width;
   int height;
   char *fg_color;
   char *bg_color;
   char *font;
   char *bitmap;
   int alignment;
   char *text;
   char *callbackMsg;
   int mnemonic;
   int visibility;
   int sensitivity;
   int mnemonic_res;          /* only relevant for PulldownMenu and MenuEntry */
   XmString text_res;         /* only relevant for PulldownMenu and MenuEntry */
   Pixmap pixmap;           /* only relevant for BitmapButton and BitmapLabel */
} WNAME;

/******************************************************************************/
/*     String Operating Functions ...                                         */
/******************************************************************************/

extern char*  UIW_NewString( char* );
extern char*  UIW_NewStringsCat( char*, char* );
extern void   UIW_FreeString( char* );

/******************************************************************************/
/*     Widget Management Functions                                            */
/******************************************************************************/

extern void   UIW_initialize( void );
extern void   UIW_addWNAME( WNAME* );
extern void   UIW_insertWNAME( WNAME*, WNAME* );
extern void   UIW_deleteWNAME( WNAME* );
extern void   UIW_setWNAMEtoBeginningOfList( WNAME *wn );
extern void   UIW_setWNAMEtoEndOfList( WNAME *wn );
extern void   UIW_restackWNAME( WNAME*, WNAME* );

/******************************************************************************/
/*     Information on actual list                                             */
/******************************************************************************/

extern int    UIW_numberOfWidgets( void );
extern WNAME* UIW_getRootWNAME( void );
extern WNAME* UIW_getWNAMEofName( char* );
extern WNAME* UIW_getWNAMEofWidget( Widget );

/******************************************************************************/
/*     Useful for debugging ...                                               */
/******************************************************************************/

extern void printList( void );

/************************************* EOF ************************************/

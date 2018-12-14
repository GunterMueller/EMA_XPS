/****************************************************
*   sysd.h        Hans Groschwitz        06.10.94   *
*                 Stephan Peters                    *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   Popups for the Main-Menu of EMA-XPS             *
*                                                   *
*   See uid.x for details!                          *
****************************************************/

extern void SYSD_create( Widget );
extern void SYSD_popdown_all_dialogs( void );
extern void readSysdWarning( void );
extern void readSysdInformation( void );
extern void readSysdAcceptCancel( void );
extern void readSysdPrompt( void );

/*** EOF ***/

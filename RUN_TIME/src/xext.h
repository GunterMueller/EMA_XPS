/****************************************************
*   xext.h        Hans Groschwitz        27.11.08   *      
*                                                   *
*   Moved Xm  + Xt extensions into a separate file  *
****************************************************/

extern char* MallocCompleteStringFromXmString( XmString );
extern int   CygwinPostInitialize( Widget );
extern int   CygwinPreInitialize( void );
extern char* XmClassName( Widget );
extern char* XmSuperclassName( Widget );
extern char* XmSuperSuperclassName( Widget );

/** EOF ***/

/****************************************************
*   expd.h        Hans Groschwitz        12.09.96   *      
*                 Karsten Vossberg                  *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   Popups for the Explanation facility of EMA-XPS  *
*                                                   *
*   See uid.x for details!                          *
****************************************************/

extern void EXPD_create( Widget );
extern void EXPD_popdown_all_dialogs( void );

extern void readExpdOneOfMany( void );
extern void readExpdOneOfMany2( void );
extern void readExpdSomeOfMany( void );
extern void ExpdMessage(void);

/*** EOF ***/

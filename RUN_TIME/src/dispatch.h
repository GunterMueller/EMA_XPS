/****************************************************
*   dispatch.h    Hans Groschwitz        06.10.94   *
*                                                   *
*   Dispatcher for User Defined Message Handlers    *
****************************************************/

typedef void (*FPTR)();     /* Pointer to Function */

/*** Globals ***************************************/

extern FPTR dispatch[];

/*** EOF *******************************************/

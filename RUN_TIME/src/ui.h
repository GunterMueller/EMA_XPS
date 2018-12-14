/****************************************************
*   ui.c          Hans Groschwitz        20.11.94   *   
*                 Michael Block                     *
*                 Karsten Vossberg                  *   
*                                                   *
*   C-FrontEnds of LISP functions for control of    *                                            
*   User Interface (Session Screen)                 *
****************************************************/

/******************************************************************************/
/*     Event orientation controls                                             */
/******************************************************************************/

extern void UiCreateSimpleSessionScreen ( void );
extern void UiStopSession               ( void );

/******************************************************************************/
/*     Basic Widget Operations                                                */
/******************************************************************************/

extern void UiDestroyWidget             ( void );
extern void UiRaiseToTop                ( void );
extern void UiLowerToBottom             ( void );

/******************************************************************************/
/*     Widget Creation                                                        */
/******************************************************************************/

extern void UiCreatePushbutton            ( void );
extern void UiCreateLabel                 ( void );
extern void UiCreateBitmapButton          ( void );
extern void UiCreateBitmapLabel           ( void );
extern void UiCreateInputText             ( void );
extern void UiCreateOutputText            ( void );
extern void UiCreatePulldownMenu          ( void );
extern void UiCreateMenuEntry             ( void );
extern void UiCreateSeparator             ( void );

/******************************************************************************/
/*     Resource Set                                                           */
/******************************************************************************/

extern void UiSetSessionBg              ( void );
extern void UiSetMenuBg                 ( void );
extern void UiSetMenuFg                 ( void );
extern void UiSetMenuFont               ( void );
extern void UiSetVisibility             ( void );
extern void UiSetSensitivity            ( void );
extern void UiSetForeground             ( void );
extern void UiSetBackground             ( void );
extern void UiSetFont                   ( void );
extern void UiSetBitmap                 ( void );
extern void UiSetGeometry               ( void );
extern void UiSetAlignment              ( void );
extern void UiSetText                   ( void );
extern void UiAddText                   ( void );
extern void UiSetCallback               ( void );
extern void UiSetMnemonic               ( void );


/******************************************************************************/
/*     Resource Get                                                           */
/******************************************************************************/

extern void UiGetSessionBg              ( void );
extern void UiGetMenuFont               ( void );
extern void UiGetMenuFg                 ( void );
extern void UiGetMenuBg                 ( void );
extern void UiGetVisibility             ( void );
extern void UiGetSensitivity            ( void );
extern void UiGetForeground             ( void );
extern void UiGetBackground             ( void );
extern void UiGetFont                   ( void );
extern void UiGetBitmap                 ( void );
extern void UiGetGeometry               ( void );
extern void UiGetAlignment              ( void );
extern void UiGetText                   ( void );
extern void UiGetCallback               ( void );
extern void UiGetMnemonic               ( void );

/******************************************************************************/
/*     Save Current User-Interface                                            */
/******************************************************************************/

extern void UiSave                      ( void );

/******************************** EOF *****************************************/

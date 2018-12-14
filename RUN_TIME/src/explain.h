/****************************************************
*   explain.h     Hans Groschwitz        08.12.94   *
*                 Karsten Vossberg       20.07.95   *
*                                                   *
*   Interface for graphic explanations when inspec- *
*   ting the success of inferencing at runtime.     *
****************************************************/

extern void ExplainMessage( void );
extern void ExplainDisplayOutput( char* );
extern void Explain_PAD_create( Widget );
extern void Explain_PAD_show( void );
extern void Explain_PAD_hide( void );
extern void Explain_PAD_ready( void );
extern void Explain_PAD_busy( void );

/*** EOF *******************************************/

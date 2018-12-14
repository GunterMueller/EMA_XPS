/****************************************************
*   trace.h       Hans Groschwitz        19.07.95   *     
*                 Karsten Vossberg                  *
*                                                   *
*   Functions to build the tracer.                  *      
*   Full version only.                              *
****************************************************/

extern void   TraceMessage           ( void );
extern void   TraceTaskToggle        ( void );
extern void   TraceSystemToggle      ( void );
extern void   TraceRuleToggle        ( void );
extern void   TracePrologToggle      ( void );
extern void   TraceConstraintToggle  ( void );

extern Widget Trace_PAD_create        ( Widget );
extern void   Trace_PAD_show          ( void );
extern void   Trace_PAD_hide          ( void );
extern void   Trace_PAD_ready         ( void );
extern void   Trace_PAD_busy          ( void );

extern void   TRACE_informOnKBSstate( int );

/*** EOF *******************************************/

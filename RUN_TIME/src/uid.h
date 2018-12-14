/****************************************************
*   uid.h         Hans Groschwitz        06.10.94   *
*                                                   *
*  C-FrontEnds of LISP functions for control of     *
*  Popups for User Interface (Session Screen)       *
*             ^    ^                                *
*  The `d' within in `uid' displays `popup-Dialog'. *
*                                          ^        *
*  This version of the User Interface uses logical  *
*  channels for message-passing. The corresponding  *
*  channel numbers and the reading functions        *
*  must be known within dispatch.c !!!              *
****************************************************/

extern void UID_create( Widget );
extern void UID_popdown_all_dialogs( void );
extern char* UID_debugString( char* );

/****************************************************
*   UidInformation --> (uid-information "string")   *
****************************************************/

extern void readUidInformation( void );

/****************************************************
*  UidAcceptCancel --> (uid-accept-cancel "string") *
****************************************************/

extern void readUidAcceptCancel( void );

/****************************************************
*  UidYesNoCancel --> (uid-yes-no-cancel "string")  *
****************************************************/

extern void readUidYesNoCancel( void );

/****************************************************
*       UidPrompt --> (uid-prompt "string")         *
****************************************************/

extern void readUidPrompt( void );

/****************************************************
*      UidOneOfMany -->                             *
*         (uid-one-of-many '(l i s t) "string")     *
****************************************************/

extern void readUidOneOfMany( void );

/****************************************************
*      UidSomeOfMany -->                            *
*         (uid-some-of-many '(l i s t) "string")    *
****************************************************/

extern void readUidSomeOfMany( void );

/*** EOF ***/

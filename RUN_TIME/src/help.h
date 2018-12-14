/****************************************************
*   help.h        Hans Groschwitz        08.12.94   *
*                                                   *
*   Creates the help facility which reads the file  *
*   $XAPPLRESDIR/$LANG/ema-xps-help.txt from which  *
*   is separated into pages using the FF char.      *
*   The page is accessable via a number represen-   *
*   ting the desired page. The information is       *
*   displayed in a specialized PopupDialog, which   *
*   allowes paging. Pressing a Help button anywhere *
*   within EMA-XPS will popup this one instance.    *
****************************************************/

#define HELP_MASTER_PAGE           0
#define HELP_ACKNOWLEDGEMENTS      1
#define HELP_CONTENTS              2
#define HELP_PROJECTS              3
#define HELP_REFERENCES            4
#define HELP_MAIN_MENU             5
#define HELP_SESSION_SCREEN_MENU   6
#define HELP_DEBUGGER_PAD          7
#define HELP_EXPLAIN_MENU          8
#define HELP_TRACE_MENU            9
#define HELP_ON_EDITORS           10
#define HELP_FRAME_MENU           11
#define HELP_INSTANCE_MENU        12
#define HELP_BEHAVIOR_MENU        13
#define HELP_RULE_MENU            14
#define HELP_PROLOG_MENU          15
#define HELP_CONSTRAINT_MENU      16
#define HELP_CSNET_MENU           17
#define HELP_RESTRICTION_MENU     18
#define HELP_TASK_MENU            19
#define HELP_MISC_MENU            20
#define HELP_BABYLON_SYNTAX       21

/*** Functions *************************************/

void   HELP_show( Widget, XtPointer, XtPointer );
Widget HELP_create( Widget );
void   HELP_setPath( char* );
void   HELP_showBrowserCB( Widget );

/*** EOF *******************************************/

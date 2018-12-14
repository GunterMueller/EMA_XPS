/****************************************************
*   ui.c          Hans Groschwitz        20.11.94   *
*                 Michael Block                     *
*                 Karsten Vossberg                  *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   User Interface (Session Screen)                 *
****************************************************/

#include <stdio.h>

#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>

#include "uif.h" 
#include "main.h"               /* LISP_sndReply() */
#include "timings.h"            /* TIME_UI_STOP */

/******************************************************************************/
/*                                                                            */
/*                             Some useful aids ...                           */
/*                                                                            */
/******************************************************************************/

static int
UiStricmp( char *p1, char *p2 ) {
   int ret;

   /*
    * hg20081112:
    *
    * cygwin knows the win32 implementation of stricmp
    * on the other hand a strcasecmp ex.
    * I decided to rename my own version of this function.
    */
   while( !(ret=toupper(*p1)-toupper(*p2)) && *p1 ) p1++, p2++;
   return ret;
}

/*********************** Protocol Transfer Control ****************************/

static int ui_sync = True;            /* to be verbose in case of failure ... */

static char*
AsyncP_MSG_read( void ) {
   int len;
   char *p = MSG_read();

   ui_sync = True;                    /* to be verbose in case of failure ... */

   if( !p )
      return p;

   len = strlen( p );
   if( !len )
      return p;

   if( p[len-1] == CAN ) {
      ui_sync = False;  /* the ^X character appended to signal async transfer */
      p[len-1] = 0;      /* to avoid problems when analysing the protocol !!! */
   }

/*printf(" ui_sync = %d\n", ui_sync );*/
   return p;   
}

static void
AsyncP_LISP_sndReply( char *p ) {
   if( ui_sync )
      LISP_sndReply( p );
}

/******************************* UiRecvString *********************************/

static int
NrOfNewlines( char *s ) {
   int c, i = 0;

   while( c = *s++ )
      if( c == '\n' )
         i++;
   return i;
}

static char*
UiRecvString( char *in ) {

   if( *in == EOT && !in[1]  )     /* LISP sent NIL */
      return UiKEEPP;
   if( NrOfNewlines(in)>1 && *in == '\n')
      /* Bug in CLISP ?
       * ==============
       * if transmitting a String without a Newline, the string is sent fine.
       * if anywhere within the string to be sent is a newline, an additional
       * newline is placed in front of the string ?!?
       * THIS IS CUT HERE!
       */
      return ++in;
   return in;
}

/******************************* UiRecvAlign **********************************/

static int
UiRecvAlign( char *in ) {

   if( *in == EOT && !in[1]  )    /* LISP sent NIL */
      return UiKEEP;
   if( !UiStricmp( in, ":left" ) )
      return UiLEFT;
   if( !UiStricmp( in, ":right" ) )
      return UiRIGHT;
   if( !UiStricmp( in, ":center" ) )
      return UiCENTER;
   return UiERROR; /* should be checked at lisp side already ! */
}

/******************************* UiRecvBool ***********************************/

static int
UiRecvBool( char *in ) {

   if( *in == EOT && !in[1]  )    /* LISP sent NIL */
      return UiNIL;
   return UiTRUE;
}

/******************************* UiRecvMnem ***********************************/

static int
UiRecvMnem( char *in ) {

   if( *in == EOT && !in[1]  )    /* LISP sent NIL */
      return UiKEEP;
   return in[0];    /* strlen = 1 should be checked by LISP */
}

/******************************* UiRecvGeom ***********************************/

static int
UiRecvGeom( char *in ) {                       /* used by ui-set-geometry !!! */

   if( *in == EOT && !in[1]  )    /* LISP sent NIL */
      return UiKEEP;
   return atoi( in );             /* ... or an unsigned interger */
}

/**************************** UiGetNextPartStr ********************************/

static char* 
UiGetNextPartStr( char *str ) {
   while( *str && *str != ETX ) str++;
   if( !*str ) 
      return( NULL );
   /* ETX found */
   *str = 0;
   return( ++str );
}          
      
/***************************** UiSendAndFreeString ****************************/

static char*
UiSendAndFreeString( char *str ) {
   char *p;
   int i, j, imax;

   imax = strlen(str);
   p = XtMalloc( 2*imax+3 );
   /* LISP read's from its stdin and recognizes
      a String only if enclosed with " */
   i = j = 0;
   p[j++] = '"';
   while( i < imax ) {
      if( str[i] == '"' )
         p[j++] = '\\';
      p[j++] = str[i++];
   }
   p[j++] = '"';
   p[j] = 0;
   XtFree( str );
   LISP_sndReply( p );
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                        Event orientation controls                          */
/*                                                                            */
/******************************************************************************/

/*********************** UiCreateSimpleSessionScreen **************************/

void
UiCreateSimpleSessionScreen( void ) {
   char *wname;
   int ret;
   char *p;

   p = MSG_read();
   if( *p ) {
      fprintf( stderr, "UiCreateSimpleSessionScreen: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   ret = ui_create_simple_session_screen();
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiStopSession *************************************/

static void
UiStopSession_TO( void ) {
   extern void ui_stop_session_to( void );
   /*
    * Synchronization with LISP:
    * (ui-stop-session) waits in (read)
    * ==> no other LISP command can be entered up to then!
    * ==> MenuEntries of Session only send LISP commands!
    */
   ui_stop_session_to();   /* always returns UiOK ! */
   LISP_sndReply( "NIL" );
}

/**************************************************************/

void
UiStopSession( void ) {
   char *wname;
   int ret;
   char *p;
   p = MSG_read();

   if( *p ) {
      fprintf( stderr, "UiStopSession: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   XtFree( p );
   ret = ui_stop_session();
   if( ret == UiERROR ) {
      LISP_sndReply( ":ERROR" );
      return;
   }
   /* Linux BUG! see uif.c !! */
   AddTimeOut( TIME_UI_STOP, (XtPointer)UiStopSession_TO, NULL );
}

/******************************************************************************/
/*                                                                            */
/*                        Basic Widget Operations                             */
/*                                                                            */
/******************************************************************************/

/************************** UiDestroyWidget ***********************************/

void
UiDestroyWidget( void ) {
   char *wname;
   int ret;
   char *p, *v[4];
   v[0] = p = MSG_read();

   if( !*p ) {
      fprintf( stderr, "UiDestroyWidget: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiDestroyWidget: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_destroy_widget( wname );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiRaiseToTop ***********************************/

void
UiRaiseToTop( void ) {
   char *wname;
   int ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiRaiseToTop: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiRaiseToTop: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_raise_to_top( wname );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiLowerToBottom ********************************/

void
UiLowerToBottom( void ) {
   char *wname;
   int ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiLowerToBottom: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiLowerToBottom: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_lower_to_bottom( wname );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                            Resource Get                                    */
/*                                                                            */
/******************************************************************************/

/*********************** UiGetSessionBg ***************************************/

void
UiGetSessionBg( void ) {
   char *p, *ret;
   p = MSG_read();
   if( *p ) {
      fprintf( stderr, "UiGetSessionBg: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   ret = ui_get_session_bg();
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else                                                     
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetMenuBg **************************************/

void
UiGetMenuBg( void ) {
   char *p, *ret;
   p = MSG_read();
   if( *p ) {
      fprintf( stderr, "UiGetMenuBg: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   ret = ui_get_menu_bg();
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else                                                     
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetMenuFg **************************************/

void
UiGetMenuFg( void ) {
   char *p, *ret;
   p = MSG_read();
   if( *p ) {
      fprintf( stderr, "UiGetMenuFg: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   ret = ui_get_menu_fg();
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else                                                     
      UiSendAndFreeString( ret );
   XtFree( p );
}

/**************************** UiGetMenuFont ***********************************/

void
UiGetMenuFont( void ) {
   char *p, *ret;
   p = MSG_read();
   if( *p ) {
      fprintf( stderr, "UiGetMenuFont: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   ret = ui_get_menu_font();
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else                                                     
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetVisibility **********************************/

void
UiGetVisibility( void ) {
   char *wname;
   int ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetVisibility: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetVisibility: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_get_visibility( wname );
   switch( ret ) {
      case UiTRUE:
         LISP_sndReply( "T" );
         break;
      case UiNIL:
         LISP_sndReply( "NIL" );
         break;
      case UiERROR:
         LISP_sndReply( ":ERROR" );
         break;
   }
   XtFree( p );
}

/************************ UiGetSensitivity **********************************/

void
UiGetSensitivity( void ) {
   char *wname;
   int ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetSensitivity: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetVisibility: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_get_sensitivity( wname );
   switch( ret ) {
      case UiTRUE:
         LISP_sndReply( "T" );
         break;
      case UiNIL:
         LISP_sndReply( "NIL" );
         break;
      case UiERROR:
         LISP_sndReply( ":ERROR" );
         break;
   }
   XtFree( p );
}

/************************** UiGetBackground **********************************/

void
UiGetBackground( void ) {
   char *wname, *ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetBackground: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetBackground: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   wname = UiRecvString( v[0] );
   ret = ui_get_background( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetForeground **********************************/

void
UiGetForeground( void ) {
   char *wname, *ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetForeground: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }   
   
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetForeground: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   
   wname = UiRecvString( v[0] );
   ret = ui_get_foreground( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetFont **********************************/

void
UiGetFont( void ) {
   char *wname,*ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetFont: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetFont: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   wname = UiRecvString( v[0] );
   ret = ui_get_font( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetBitmap **********************************/

void
UiGetBitmap( void ) {
   char *wname,*ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetBitmap: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetBitmap: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_get_bitmap( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetGeometry **********************************/

void
UiGetGeometry( void ) {
   char *wname,*ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetGeometry: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetGeometry: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_get_geometry( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else {
      char *p = XtMalloc( strlen(ret)+3 );
      sprintf( p, "(%s)", ret );
      XtFree( ret );             /* handled like string info */
      LISP_sndReply( p );
      XtFree( p );
   }
   XtFree( p );
}

/************************** UiGetAlignment **********************************/

void
UiGetAlignment( void ) {
   char *wname;
   int ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetAlignment: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetAlignment: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   wname = UiRecvString( v[0] );
   ret = ui_get_alignment( wname );
   switch( ret ) {
      case UiERROR:
         LISP_sndReply( ":ERROR" );
         break;
      case UiNIL:
         LISP_sndReply( "NIL" );
         break;
      case UiLEFT:
         LISP_sndReply( ":LEFT" );
         break;
      case UiRIGHT:
         LISP_sndReply( ":RIGHT" );
         break;
      case UiCENTER:
         LISP_sndReply( ":CENTER" );
   }
   XtFree( p );
}

/************************** UiGetText **********************************/

void
UiGetText( void ) {
   char *wname, *ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetText: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetText: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   wname = UiRecvString( v[0] );
   ret = ui_get_text( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetCallback **********************************/

void
UiGetCallback( void ) {
   char *wname, *ret;
   char *p, *v[3];

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetCallback: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetCallback: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }

   wname = UiRecvString( v[0] );
   ret = ui_get_callback( wname );
   if( ret == UiERRORP )
      LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEPP )
      LISP_sndReply( "NIL" );
   else      
      UiSendAndFreeString( ret );
   XtFree( p );
}

/************************** UiGetMnemonic **********************************/

void
UiGetMnemonic( void ) {
   char *wname;
   char *p, *v[3],*str;
   int ret;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiGetMnemonic: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiGetMnemonic: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   wname = UiRecvString( v[0] );
   ret = ui_get_mnemonic( wname );
   if ( ret == UiERROR )
       LISP_sndReply( ":ERROR" );
   else if( ret == UiKEEP )
       LISP_sndReply( "NIL" );
   else {
       char p[256];
       sprintf( p, "\"%c\"", ret );  /* C: 1 char -- LISP: string */
       LISP_sndReply( p );
   }
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                            Resource Set                                    */
/*                                                                            */
/******************************************************************************/

/************************** UiSetSessionBg ************************************/

void
UiSetSessionBg( void ) {
   char *bg;
   int ret;
   char *p, *v[3];

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetSessionBg: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiSetSessionBg: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   bg = UiRecvString( v[0] );
   ret = ui_set_session_bg( bg );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetMenuBg *****************************************/

void
UiSetMenuBg( void ) {
   char *bg;
   int ret;
   char *p, *v[3];

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetMenuBg: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiSetMenuBg: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   bg = UiRecvString( v[0] );
   ret = ui_set_menu_bg( bg );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetMenuFg ****************************************/

void
UiSetMenuFg( void ) {
   char *fg;
   int ret;
   char *p, *v[3];

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetMenuFg: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiSetMenuFg: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   fg = UiRecvString( v[0] );   
   ret = ui_set_menu_fg( fg );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetMenuFont **********************************/

void
UiSetMenuFont( void ) {
   char *font;
   int ret;
   char *p, *v[3];

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetMenuFont: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[0] ) ) {
      fprintf( stderr, "UiSetMenuFont: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   font = UiRecvString( v[0] );

   ret = ui_set_menu_font( font );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetVisibility **********************************/

void
UiSetVisibility( void ) {
   char *wname;
   int ret,visi;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetVisibility: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetVisibility: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetVisibility: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname =      UiRecvString( v[i++] );
   visi =         UiRecvBool( v[i++] );
   
   ret = ui_set_visibility( wname, visi );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetForeground **********************************/

void
UiSetForeground( void ) {
   char *wname, *fg;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetForeground: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetForeground: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetForeground: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   fg = UiRecvString( v[i++] );

   ret = ui_set_foreground( wname, fg );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetBackground **********************************/

void
UiSetBackground( void ) {
   char *wname, *bg;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetBackground: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetBackground: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetBackground: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   bg = UiRecvString( v[i++] );

   ret = ui_set_background( wname, bg );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetFont **********************************/

void
UiSetFont( void ) {
   char *wname, *font;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetFont: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetFont: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetFont: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   font = UiRecvString( v[i++] );

   ret = ui_set_font( wname, font );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetBitmap **********************************/

void
UiSetBitmap( void ) {
   char *wname, *bitmap;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetBitmap: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetBitmap: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetBitmap: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   bitmap = UiRecvString( v[i++] );

   ret = ui_set_bitmap( wname, bitmap );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetAlignment **********************************/

void
UiSetAlignment( void ) {
   char *wname;
   int ret,align;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetAlignment: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetAlignment: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetAlignment: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   align = UiRecvAlign( v[i++] );

   ret = ui_set_alignment( wname, align );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetText **********************************/

void
UiSetText( void ) {
   char *wname, *text;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetText: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetText: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetText: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   text  = UiRecvString( v[i++] );

   ret = ui_set_text( wname, text );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiAddText **********************************/

void
UiAddText( void ) {
   char *wname, *text;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiAddText: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiAddText: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiAddText: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   text  = UiRecvString( v[i++] );
   
   ret = ui_add_text( wname, text );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetCallback **********************************/

void
UiSetCallback( void ) {
   char *wname, *cb;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetCallback: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetCallback: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetCallback: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   cb    = UiRecvString( v[i++] );

   ret = ui_set_callback( wname, cb );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetMnemonic **********************************/

void
UiSetMnemonic( void ) {
   char *wname;
   int ret,mnem;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetMnemonic: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetMnemonic: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetMnemonic: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   mnem  = UiRecvMnem( v[i++] );

   ret = ui_set_mnemonic( wname, mnem );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetSensitivity **********************************/

void
UiSetSensitivity( void ) {
   char *wname;
   int ret,sensi;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetSensitivity: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetSensitivity: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetSensitivity: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   sensi =   UiRecvBool( v[i++] );
   
   ret = ui_set_sensitivity( wname, sensi );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiSetGeometry **********************************/

void
UiSetGeometry( void ) {
   char *wname;
   int ret,x,y,w,h;
   char *p, *v[11];
   int i, err;

   v[0] = p = AsyncP_MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSetGeometry: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 5; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSetGeometry: too few args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSetGeometry: too many args\n" );
      AsyncP_LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     =   UiRecvGeom( v[i++] );
   y     =   UiRecvGeom( v[i++] );
   w     =   UiRecvGeom( v[i++] );
   h     =   UiRecvGeom( v[i++] );

   ret = ui_set_geometry( wname, x, y, w, h );
   AsyncP_LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                        Widget Creation                                     */
/*                                                                            */
/******************************************************************************/

/**************************** UiCreatePushbutton ******************************/

void
UiCreatePushbutton( void ) {
   char *wname, *label, *font, *fg, *bg, *cb;
   int ret,x, y, w, h, aln;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreatePushbutton: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 11; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreatePushbutton: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreatePushbutton: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   label = UiRecvString( v[i++] );
   font  = UiRecvString( v[i++] );
   aln   = UiRecvAlign ( v[i++] ); 
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );
   cb    = UiRecvString( v[i++] );

   ret = ui_create_pushbutton( wname, x, y, w, h, label, font, aln, fg, bg, cb );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateLabel ******************************/

void
UiCreateLabel( void ) {
   char *wname, *label, *font, *fg, *bg;
   int ret,x, y, w, h, aln;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreatePushbutton: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 10; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateLabel: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateLabel: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   label = UiRecvString( v[i++] );
   font  = UiRecvString( v[i++] );
   aln   = UiRecvAlign ( v[i++] ); 
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );

   ret = ui_create_label( wname, x, y, w, h, label, font, aln, fg, bg );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateBitmapButton ************************/

void
UiCreateBitmapButton( void ) {
   char *wname, *path, *fg, *bg, *cb;
   int ret,x, y, w, h, aln;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateBitmapButton: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 9; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateBitmapButton: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateBitmapButton: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   path  = UiRecvString( v[i++] );
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );
   cb    = UiRecvString( v[i++] );

   ret = ui_create_bitmap_button( wname, x, y, w, h, path, fg, bg ,cb );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateBitmapLabel ******************************/

void
UiCreateBitmapLabel( void ) {
   char *wname, *path, *fg, *bg;
   int ret,x, y, w, h;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateBitmapLabel: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 8; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateBitmapLabel: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateBitmapLabel: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   path  = UiRecvString( v[i++] );
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );

   ret = ui_create_bitmap_label( wname, x, y, w, h, path, fg, bg );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateInputText ******************************/

void
UiCreateInputText( void ) {
   char *wname, *font, *fg, *bg;
   int ret,x, y, w, h;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateInputText: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 8; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateInputText: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateInputText: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   font  = UiRecvString( v[i++] );
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );

   ret = ui_create_input_text( wname, x, y, w, h, font, fg, bg );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateOutputText ******************************/

void
UiCreateOutputText( void ) {
   char *wname, *font, *text, *fg, *bg;
   int ret,x, y, w, h;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateOutputText: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 9; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateOutputText: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateOutputText: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   text  = UiRecvString( v[i++] );
   font  = UiRecvString( v[i++] );
   fg    = UiRecvString( v[i++] );
   bg    = UiRecvString( v[i++] );

   ret = ui_create_output_text( wname, x, y, w, h, text, font, fg, bg );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreatePulldownMenu ******************************/

void
UiCreatePulldownMenu( void ) {
   char *wname, *label;
   int ret,mnem;
   char *p, *v[5];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreatePulldownMenu: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 3; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreatePulldownMenu: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreatePulldownMenu: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   label = UiRecvString( v[i++] );
   mnem  = UiRecvMnem  ( v[i++] );

   ret = ui_create_pulldown_menu( wname, label, mnem );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/**************************** UiCreateMenuEntry ******************************/

void
UiCreateMenuEntry( void ) {
   char *wname, *label, *pd_name, *cb;
   int ret,mnem;
   char *p, *v[7];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateMenuEntry: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 5; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateMenuEntry: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateMenuEntry: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname   = UiRecvString( v[i++] );
   pd_name = UiRecvString( v[i++] );
   label   = UiRecvString( v[i++] );
   mnem    = UiRecvMnem  ( v[i++] );
   cb      = UiRecvString( v[i++] );
   
   ret = ui_create_menu_entry( wname, pd_name, label, mnem, cb );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/************************** UiCreateSeparator **********************/

void
UiCreateSeparator( void ) {
   char *wname, *fg;
   int ret,x, y, w, h;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiCreateSeparator: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 6; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiCreateSeparator: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiCreateSeparator: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   wname = UiRecvString( v[i++] );
   x     = atoi        ( v[i++] );
   y     = atoi        ( v[i++] );
   w     = atoi        ( v[i++] );
   h     = atoi        ( v[i++] );
   fg    = UiRecvString( v[i++] );

   ret = ui_create_separator( wname, x, y, w, h, fg );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                        Save Current User-Interface                         */
/*                                                                            */
/******************************************************************************/

/************************** UiSave **********************************/

void
UiSave( void ) {
   char *command_file, *resource_file;
   int ret;
   char *p, *v[11];
   int i, err;

   v[0] = p = MSG_read();
   if( !*p ) {
      fprintf( stderr, "UiSave: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   for( err = 0, i = 1; i < 2; i++ ) {
      if( !(v[i]=UiGetNextPartStr(v[i-1])) ) {
         err = i+1;
         break;
      }
   }
   if( err ) {
      fprintf( stderr, "UiSave: too few args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   if( UiGetNextPartStr( v[i-1] ) ) {
      fprintf( stderr, "UiSave: too many args\n" );
      LISP_sndReply( ":ERROR" );
      XtFree( p );
      return;
   }
   i = 0;
   command_file  = UiRecvString( v[i++] );
   resource_file = UiRecvString( v[i++] );
   
   ret = ui_save( command_file, resource_file );
   LISP_sndReply( ret == UiERROR ? ":ERROR" : "NIL" );
   XtFree( p );
}

/*** EOF ***/

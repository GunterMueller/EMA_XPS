#if 0
#define _dbg(a)    puts("xext: " a)
#define _dbgs(a) printf("xext: %s\n",a)
#define _dbgx(a) printf("xext: 0x%X\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#define _dbgx(a)
#endif

/****************************************************
*   xext.c        Hans Groschwitz        27.11.08   *
*                                                   *
*   Moved Xm  + Xt extensions into a separate file  *
*                                                   *
*   taken from expd.c and sysd.c                    *
****************************************************/

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/DisplayP.h>         /* Cygwin Bug !!! */

#include "xext.h"

/*** Defines ***************************************/

/*** Prototypes ************************************/

/*** Globals ***************************************/

/****************************************************
*   Xext: manipulate XmString's                     *
****************************************************/

static void
GetCompleteStringFromXmString( XmString str, char **buf ) {
   XmStringContext context;
   char *text, *p;
   XmStringCharSet charset;
   XmStringDirection direction;
   Boolean separator;
   Boolean success;

   /*
    * Example from O'Reilly's Motif Programming Manual
    */
   p = *buf;
   *p = 0;  /** hg20081128 added, requires a valid buffer! **/
   success = XmStringInitContext( &context, str );
   if( success != True ) {
      return;
   }

#ifndef LesstifVersion /* we are NOT using Motif21 */
   /* this crashes with Lesstif! */
   while( XmStringGetNextSegment( context, &text, &charset,
                                  &direction, &separator ) ) {
      /* copy text into p and advance to the end of the string */
      p += strlen( strcpy( p, text ) );
      if( separator == True ) {
         /* if there is a separator, add newline and string-terminator */
         *p++ = '\n';
         *p = 0;
      }
      XtFree( text );
   }
#else  /* Lesstif */
   strcpy (p, "Lesstif Bug in XmString");
#endif /* Lesstif */

   XmStringFreeContext( context );  /** see InitContext **/
}

/****************************************************
*   separate the Xresource into components          *
****************************************************/

char*
MallocCompleteStringFromXmString( XmString xms ) {
   char *tmp = NULL;
   char *s = NULL;

   /** size unknown: let's make a huge guess **/
   tmp = XtMalloc( 1024*512 );

   /*
    * XmStringGetLtoR() only returns the first part
    * i.e. it returns the resource up to the 1st \n !!!
    * -->  XmStringGetLtoR( xms, XmSTRING_DEFAULT_CHARSET, &tmp );
    */
   GetCompleteStringFromXmString( xms, &tmp );

   s = XtMalloc( 1+strlen(tmp) );
   strcpy( s, tmp );
   XtFree( tmp );

   return( s );
}

/****************************************************
*   Cygwin Bug: debugging tools                     *
****************************************************/

char*
XmClassName(Widget w) {
  /* XtClass(w) ==> (w)->core.widget_class */
  WidgetClass wc = XtClass(w);
  return wc->core_class.class_name;
}

char*
XmSuperclassName(Widget w) {
  WidgetClass sc = XtSuperclass(w);
  return sc->core_class.class_name;
}

char*
XmSuperSuperclassName(Widget w) {
  WidgetClass sc = XtSuperclass(w);
  WidgetClass ssc = sc->core_class.superclass;
  return ssc->core_class.class_name;
}

/****************************************************
*   Cygwin Bug: missing VendorS.Initialize          *
****************************************************/

CygwinPostInitialize( Widget toplevel ) {
#ifdef __CYGWIN__
_dbg("CygwinPostInitialize+");



#if 0
{
   Widget w = NULL;
   XmDisplay dd = NULL;
   WidgetClass wcl = NULL;
   Widget dsm = NULL;

_dbg("cob031");
   w = toplevel;
_dbgx(XtIsWidget(w));
_dbg("cob032");
   dd = (XmDisplay)XmGetXmDisplay(XtDisplay(w)); /* ist kein Widget! */
_dbgx(dd);
_dbg("cob033");
_dbgx(XtIsWidget(dd));
_dbg("cob033-");
   /* wcl = dropSiteManagerClass; */
_dbgx(dd->display);
   wcl = dd->display.dropSiteManagerClass;
_dbgx(wcl);
_dbg("cob034");
   /* dsm = XtCreateWidget ("dsm", wcl, (Widget)dd, NULL, 0); */
   dsm = XtCreateWidget ("dsm", wcl, (Widget)w, NULL, 0);
_dbgx(dsm);
_dbg("cob035");
   dd->display.dsm = (XmDropSiteManagerObject)dsm;
_dbg("cob036");
   /* XmDropSiteManagerObject dsm = _XmGetDropSiteManagerObject(dd); */
}
#endif


_dbg("CygwinPostInitialize-");
#endif
}

CygwinPreInitialize( void ) {
#ifdef __CYGWIN__
_dbg("CygwinPreInitialize+");



   /**
    * hg20081202:
    * http://cgit.freedesktop.org/xorg/lib/libXt/tree/src/sharedlib.c\
    * ?h=XINERAMA_2&id=cdaa6bdee4f6796ac6337fa030bfe4aaa3975db2
    *
    * ==> this is the idea: do this after linking (means: at runtime ;-)
    * but prior to XtInitialize!
    *
    * ----------------------------------------------------------
    *
    * The following routine will be called by every toolkit
    * application, forcing this file to be statically linked.
    *
    * Note: XtInitialize, XtAppInitialize, and XtOpenApplication
    *       call XtToolkitInitialize.
    */
    transientShellWidgetClass->core_class.superclass =
	(WidgetClass) &vendorShellClassRec;
    topLevelShellWidgetClass->core_class.superclass =
	(WidgetClass) &vendorShellClassRec;



_dbg("CygwinPreInitialize-");
#endif
}

/*** EOF *******************************************/

/******************************************************************************/
/*                               H. Groschwitz                                */
/*       uif.c                   Michael Block               21.07.1994       */
/*                                                                            */
/*                   Executing C-Functions run from C-FrontEnds               */
/*                   of LISP functions for control of                         */
/*                   User Interface (Session Screen)                          */
/*                                                                            */
/*       ICCCM Enhancement by H. Groschwitz                  08.08.1995       */
/*                                                                            */
/*       getColor: add this code to xgrf3d !!!                                */
/******************************************************************************/

#include <stdio.h>
#include <math.h>          /* sqrt() --> Intensity */
#include <Xm/Form.h>
#include <Xm/ScrolledW.h>
#include <Xm/MessageB.h>
#include <Xm/Separator.h>
#include <Xm/SeparatoG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/CascadeBG.h>
#include <Xm/PanedW.h>
#include <X11/cursorfont.h>

#include "uiw.h"
#include "uif.h"
#include "main.h"          /* iconPm, WM_DELETE_WINDOW */
#include "help.h"          /* HELP_show */
#include "timings.h"       /* TIME_UI_CB */

#define motif_bug(a,b)     /* obsolete: re-centering bitmaps... */

/*############################################################################*/

#define GRID50               "emaXpsGrid.bm"
#define EMPTY50              "emaXpsEmpty.bm"

#define RESOURCE_TAB         45      /** Tab-Stopp for the resource file **/
                                     /** created by ui_save()            **/

/*** Hack: MWM Decorations Geometries !!! ****/

#define DEF_BORDER_WIDTH     11      /** Default Values for borderwith **/
#define DEF_TITLEBAR_HEIGHT  17      /** and height of titlebar        **/

/******************************* Globals **************************************/

static int        maxWidth;                 /* maximum width of SessionScreen */
static int        maxHeight;               /* maximum height of SessionScreen */

static int        sessionCreated = UiNO_SESSION;

static Widget     session;
static Widget     form;
static Widget     menubar;
static Widget     scrolled_win;
static Widget     work_form;

static Widget     help_menu;
static Widget     help_menu_button;
static Widget     on_menu_button;

static Widget     session_menu;
static Widget     session_menu_button;
static Widget     break_button;
static Widget     rerun_button;
static Widget     end_button;
static Widget     sep;
static Widget     grid_button;

static int        gridVisible;
static Pixmap     gridOn;
static Pixmap     gridOff;

static int        actual_cursor = UiNORMAL;
/* Cursor     cursor_normal; */       /* obsolete, now being already defined! */
/* Cursor     cursor_busy;   */                         /* see main.h, main.c */

static char       *session_bg;                               /* actual values */
static char       *menu_bg;
static char       *menu_fg;
static char       *menu_font;

static Pixel      session_bg_def;                         /* XResource values */
static Pixel      session_fg_def;
static Pixel      session_ts_def;
static Pixel      session_bs_def;
static Pixel      menu_bg_def;
static Pixel      menu_fg_def;
static Pixel      menu_ts_def;
static Pixel      menu_bs_def;
static XmFontList menu_font_def;

static Widget     buttonCB_caused_by;     /* Widget, which activated ButtonCB */

static Display   *dpy;
static Colormap   cmp;

static int        session_is_set_busy = False;

static int        ui_set_cursor( int cursor_form );

/****************************************************
*   Change Sensitivity of the SessionScreen         *
*   (accessable from outside...)                    *
****************************************************/

void
SESSION_ready( void ) {

   session_is_set_busy = False;
   ui_set_cursor( UiNORMAL );
}

/***************************************************/

void
SESSION_busy( void ) {

   session_is_set_busy = True;
   ui_set_cursor( UiBUSY );
}

/***************************************************/

static void
UiFree( char *p ) {
   /* 
    * Linux Bug: XtFree( odd address ) stops kernel !!!
    */
   if( p == UiKEEPP || p == UiERRORP )
      return;
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                             Callback Functions                             */
/*                                                                            */
/******************************************************************************/

/******************************* SessionCB ************************************/

static void
SessionCB( Widget w, XtPointer clientData, XmPushButtonCallbackStruct *cbs ) {
   /*
    * Diese Funktion wird aufgerufen, wenn ein Menue-Punkt des Session-Menues
    * ausgewaehlt worden ist.
    */
   char *font;
   XFontStruct *fonty;
   XmFontList menu_font_def;
   WNAME *output, *input;

   if( w == break_button ) {
      LISP_interrupt();
      return;
   }

   if( w == rerun_button ) {
      LISP_sndCmd( "(emaxps-run-session)" );
      return;
   }

   if( w == end_button ) {
      LISP_sndCmd( "(ui-stop-session)" );
      return;
   }

   if( w == grid_button ) {
      /* kann vom SIMPLE_SESSION nicht erreicht werden! */

      XtVaGetValues( grid_button,
         XmNfontList,     &menu_font_def,
         NULL );
   
      if( menu_font != UiKEEPP ) {
         font = UIW_NewString( menu_font );
         if( fonty = XLoadQueryFont( dpy, menu_font ) )
            menu_font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
         UiFree( font );
      }

      /*
       *  Es existieren abwechselnd zwei verschiedene MenuButtons
       *  ( grid_on_button bzw. grid_off_button ), damit diese auch
       *  getrennt voneinander ueber die XResource-Datei beschriftet
       *  werden koennen. Daher wird hier abwechselnd der Eine
       *  'destroyed' und der Andere 'creiert':
       */
      XtDestroyWidget( grid_button );

      if( gridVisible ) {
         XtVaSetValues( work_form, XmNbackgroundPixmap, gridOff, NULL);
         gridVisible = 0;

         /*
          * Um zu vermeiden, dass noch Reste des Grids zu sehen sind:
          */
         XtUnmanageChild( work_form );
         XtManageChild( work_form );

         grid_button = XtVaCreateManagedWidget( "grid_on_button",
            xmPushButtonGadgetClass, session_menu,
            XmNfontList, menu_font_def,
            NULL );
      } else {
         XtVaSetValues( work_form, XmNbackgroundPixmap, gridOn, NULL);
         gridVisible = 1;

         grid_button = XtVaCreateManagedWidget( "grid_off_button",
            xmPushButtonGadgetClass, session_menu,
            XmNfontList, menu_font_def,
            NULL );
      }
      XtAddCallback( grid_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );

      if( menu_font != UiKEEPP )
         XmFontListFree( menu_font_def );
   }
}

/**************************** ButtonCB_delayed ********************************/

static void
ButtonCB_delayed( char *str ) {
   /*
    * Diese Funktion wird von ButtonCB() verzoegert aufgerufen.
    */

    LISP_sndCmd( str );
    UIW_FreeString( str );
}

/**************************** ButtonCB ****************************************/

static void
ButtonCB( Widget w ) {
   /*
    * Diese Funktion wird aufgerufen, wenn ein Pushbutton oder BitmapButton
    * aktiviert oder innerhalb eines InputTextes <RETURN> gedrueckt
    * worden ist.
    */
   WNAME *existent;
   char *str, *str_copy;

   if( session_is_set_busy ) {
      XBell( XtDisplay(session), 100 );
      return;     /* Do nothing but a Beep */
   }

   if( !(existent = UIW_getWNAMEofWidget(w)) )
      return;

   buttonCB_caused_by = existent->w;

   /* Kopie, sonst wird die Information irgend wann einaml geklaut ?!? */
   if( existent->class == UiINPUT_TEXT ) {
      str_copy = XmTextFieldGetString( w );
      str = UIW_NewString( str_copy );
      UIW_FreeString( str_copy );
      XmTextFieldSetString( w, "" );
   } else {
      if( existent->callbackMsg == UiERRORP || existent->callbackMsg == UiKEEPP )
         return;
      str = UIW_NewString( existent->callbackMsg );
   }

   /*
    * Kleiner TimeOut, damit der Knopf oder was immer zuerst wieder schoen
    * gezeichnet werden kann! 
    */
   AddTimeOut( TIME_UI_CB, (XtPointer)ButtonCB_delayed, str );
}

/******************************************************************************/
/*                                                                            */
/*                              Useful Aids                                   */
/*                                                                            */
/******************************************************************************/

/**************************** createSession ***********************************/

static void
createSession( int session_type ) {
   /*
    * Diese Funktion kreiert einen SessionScreen des Typs session_type.
    *
    * session_type = UiSESSION:
    *   SessionScreen in Maximalgroesse; beliebige Widgets
    *   koennen plaziert werden.
    *
    * session_type = UiSIMPLE_SESSION:
    *   SessionScreen mit einem OutputText ("session-output") und einem
    *   InputText ("session-input"); Groesse des SessionScreens abhaengig
    *   von XResource-Einstellungen (--> Session.width: ... bzw.
    *   Session.height: ...); ausser PulldownMenus und MenuEntries
    *   koennen keine Erweiterungen hinzugefuegt werden.
    */
   Screen *scrn;
   Pixel fg_color, bg_color;
   int n;
   Arg args[4];
   Widget form;

   dpy = XtDisplay( SystemDialogs );

   /* fuer alle folgenden XAllocNamedColor()-Aufrufe */
   cmp = DefaultColormap( dpy, DefaultScreen(dpy) );

   UIW_initialize();

   n = 0;
   XtSetArg( args[n], XmNdeleteResponse,     XmDO_NOTHING ); n++;
   XtSetArg( args[n], XmNiconPixmap,               iconPm ); n++;
   session = XtAppCreateShell( "Session", "ClassIgnoredHere",
                                topLevelShellWidgetClass, dpy, args, n );

   form = XtVaCreateManagedWidget( "form", 
      xmFormWidgetClass, session, NULL );

   /*
    * This HELP-CB collects the help of
    * all SENSITIVE child widgets,
    * that did not register its own one!
    *
    * It should be registered for the
    * first child of the SHELL!
    */
   XtAddCallback( form, XmNhelpCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_SESSION_SCREEN_MENU );


   /*** Window-Maximizing ***/
   if( session_type == UiSESSION ) {
      /*
       * Now we will assume an mwm working with olf-fashioned
       * frame-dimensions...
       * Correct settings by the WM are possible after realizing session only!
       */
      scrn = XtScreen( SystemDialogs );
      maxWidth = WidthOfScreen( scrn ) - 2 * DEF_BORDER_WIDTH;
      maxHeight = HeightOfScreen( scrn ) - DEF_TITLEBAR_HEIGHT - 2 * DEF_BORDER_WIDTH;

      XtVaSetValues( session,
         XmNwidth,  maxWidth,
         XmNheight, maxHeight,
         NULL );
   }

   /*** MenuBar ***/
   menubar = XmVaCreateSimpleMenuBar( form, "menubar",
      XmNtopAttachment,   XmATTACH_FORM,
      XmNleftAttachment,  XmATTACH_FORM,
      XmNrightAttachment, XmATTACH_FORM,
      XmNresizeWidth,     True,
      XmNresizeHeight,    True,
      /*  nur relevant fuer UiSIMPLE_SESSION:  */
      XmNpaneMinimum,     30,
      XmNpaneMaximum,     50,
      XmNallowResize,     True,
      XmNskipAdjust,      True,
      NULL );
   XtManageChild( menubar );

   /*** HelpMenu ***/
   n = 0;
   XtSetArg( args[n], XmNallowShellResize, True ); n++;
   help_menu = XmCreateSimplePulldownMenu( menubar, "help_menu", args, n );
   help_menu_button = XtVaCreateManagedWidget( "help_menu_button", 
      xmCascadeButtonGadgetClass, menubar, 
      XmNsubMenuId,     help_menu,
      XmNrecomputeSize, True,
      NULL );
   on_menu_button = XtVaCreateManagedWidget( "on_menu_button",
      xmPushButtonGadgetClass, help_menu,
      NULL );
   XtAddCallback( on_menu_button, XmNactivateCallback,
      (XtPointer)HELP_show, (XtPointer)HELP_SESSION_SCREEN_MENU );
   XtVaSetValues( menubar, XmNmenuHelpWidget, help_menu_button, NULL );

   /*** SessionMenu ***/
   session_menu = XmCreateSimplePulldownMenu( menubar, "session_menu", args, n );
   session_menu_button = XtVaCreateManagedWidget( "session_menu_button",
      xmCascadeButtonGadgetClass, menubar,
      XmNsubMenuId,     session_menu,
      XmNrecomputeSize, True,
      NULL );
   break_button = XtVaCreateManagedWidget( "break_button",
      xmPushButtonGadgetClass, session_menu,
      NULL );
   XtAddCallback( break_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );
   rerun_button = XtVaCreateManagedWidget( "rerun_button",
      xmPushButtonGadgetClass, session_menu,
      NULL );
   XtAddCallback( rerun_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );
   end_button = XtVaCreateManagedWidget( "end_button",
      xmPushButtonGadgetClass, session_menu,
      NULL );
   XtAddCallback( end_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );

   /*** HG: ***/
   if( session_type == UiSESSION ) {
      sep = XtVaCreateManagedWidget( "sep",
         xmSeparatorGadgetClass, session_menu,
         NULL );
      grid_button = XtVaCreateManagedWidget( "grid_on_button",
         xmPushButtonGadgetClass, session_menu,
         NULL );
      XtAddCallback( grid_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );

      scrolled_win = XtVaCreateManagedWidget( "scrolled_win",
         xmScrolledWindowWidgetClass, form,
         XmNscrollingPolicy,  XmAUTOMATIC,
         XmNtopWidget,        menubar,
         XmNtopAttachment,    XmATTACH_WIDGET,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_FORM,
         NULL );

      work_form = XtVaCreateManagedWidget( "work_form",
         xmFormWidgetClass, scrolled_win,
         NULL);

   } else {
      work_form =
      scrolled_win = XtVaCreateManagedWidget( "scrolled_win",
         xmPanedWindowWidgetClass, form,
         XmNtopWidget,        menubar,
         XmNtopAttachment,    XmATTACH_WIDGET,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_FORM,
         XmNsashHeight,       1,
         XmNsashWidth,        1,
         XmNseparatorOn,      False,
         XmNspacing,          4,
         NULL );
   }
   XtVaGetValues( work_form,
      XmNbackground, &bg_color,
      XmNforeground, &fg_color,
      NULL );

   gridOn  = createPixmapFromFile( SystemDialogs,
                GRID50, fg_color, bg_color );
   gridOff = createPixmapFromFile( SystemDialogs,
                EMPTY50, fg_color, bg_color );
   gridVisible = 0;
   XtVaSetValues( work_form, XmNbackgroundPixmap, gridOff, NULL );

   /*** obsolete: now being defined in main.c ***/
   /* cursor_normal = XCreateFontCursor( dpy, XC_left_ptr ); */
   /* cursor_busy   = XCreateFontCursor( dpy, XC_watch    ); */
   actual_cursor = UiNORMAL;

   session_bg = UiKEEPP;
   menu_bg    = UiKEEPP;
   menu_fg    = UiKEEPP;
   menu_font  = UiKEEPP;
   XtVaGetValues( scrolled_win,
      XmNbackground,        &session_bg_def,
      XmNforeground,        &session_fg_def,
      XmNtopShadowColor,    &session_ts_def,
      XmNbottomShadowColor, &session_bs_def,
      NULL );
   XtVaGetValues( menubar,
      XmNbackground,        &menu_bg_def,
      XmNforeground,        &menu_fg_def,
      XmNtopShadowColor,    &menu_ts_def,
      XmNbottomShadowColor, &menu_bs_def,
      NULL );
   XtVaGetValues( help_menu_button, XmNfontList, &menu_font_def, NULL );

   XtRealizeWidget( session );
   sessionCreated = session_type;

   /*** Window-Maximizing II ***/
   if( session_type == UiSESSION ) {
      /*
       * Now we are requesting the WM to do this job!
       * Hence it need not be the mwm, and mwm-frame's dimensions need not
       * be the assumed one's, this way should be more elegant !!!
       * But this task work only, if session is realized already!
       *
       * Comments on ICCCM and trying to make the WM do one's job:
       * Although the mwm offers interesting ways to extend the existing menu's
       * they are all thought, to send additional messages from mwm to your
       * client or between your clients! It is a hard job to send a new protocol
       * to the window manager! Here, only an existing mwm-feature should be
       * activated by the client, f.maximize ! But there seems to be no way!
       * The solution is - like the Xlib-Extension in main.c - using the standard
       * ways of the Xlib-Protocol...
       * If XReconfigureWMWindow tells the mwm to enlarge the window's width to 
       * values greater than the screen, it could not draw its frame around.
       * Therefore the mwm reduces the size the way we wish!
       * If working with a non-reparenting wm like uwm the width should be set to
       * exactly the screen-width to grant complete drawing of the application!
       */
      XWindowChanges xc;

      xc.x = 0;
      xc.y = 0;
      xc.width = WidthOfScreen(XtScreen(session));
      xc.height = HeightOfScreen(XtScreen(session));

      if( !XReconfigureWMWindow( XtDisplay(session), XtWindow(session),
            DefaultScreen(XtDisplay(session)), (CWX|CWY|CWWidth|CWHeight), &xc ) ) {
         fprintf( stderr,
            "createSession: maximizing failed! trying old-fashioned way now...\n" );
      }
   }

   /*
    * LISP is currently busy, hence this this routine has been
    * activated by a ui-create-xxx !!!
    */
   ui_set_cursor( UiBUSY );
}

/******************************* getColor *************************************/

static long
getColor( char *colorname ) {
   XColor xcolor_available, xcolor_original;
   XVisualInfo vinf;
   int nrPlanes;
   static int first = 1;
   static int vistyp;
   int flag = 0;

   if( first ) {
      first = 0;

      /*
       * Visual = SoftwareRepresentation of DisplayHardware
       *
       * --> Vol.1 Xlib Programming Manual, Table 7.2
       *
       * Colormap Type     |   Read/Write   |   read only
       * ------------------+----------------+-----------------
       * Monochrome/Gray   |   GrayScale    |   StaticGray
       * Single Index RGB  |   PseudoColor  |   StaticColor
       * Decomposed Index  |   DirectColor  |   TrueColor
       *
       * DirectColor, 8 planes = 256 entry VideoLookupTable
       * StaticGray,  1 plane = black&white
       *
       *
       * BUG: fg + bg = white crashes at Apollo DN4500 (StaticGray) !
       */
      if( XMatchVisualInfo( dpy, DefaultScreen(dpy),
             nrPlanes=DisplayPlanes( dpy, DefaultScreen(dpy) ),
             DirectColor, &vinf ) ) {
         /* It's a FullColor Display ... */
         vistyp = 1;

      } else {     /* something else... */
         vistyp = 0;
         fprintf( stderr,
            "getColor:\n"
            "   the VisualType of this X11-Display\n"
            "   is NOT DirectColor (8 planes = 256 entry VideoLookupTable)\n"
            "WORKAROUND:\n"
            "   Assuming StaticGray (1 plane = black&white)\n"
            "   ==> Explicit changing of color values from LISP is disabled!\n"
            "\n"
         );
      }
   } /* first time only */

   if( !vistyp )     /* something else... */
      return UiERROR;

   if( !XParseColor( dpy, cmp, colorname, &xcolor_original ) ) {
      fprintf( stderr,
         "getColor: XParseColor: [%s] is not a valid colorname!\n",
         colorname
      );
      return UiERROR;
   }
   if( !XAllocColor( dpy, cmp, &xcolor_original ) ) {
      fprintf( stderr, "getColor: XAllocColor failed for [%s] (no more free cells)!\n",
         colorname
      );
      return UiERROR;
   }
   /*printf( "color=[%s] pixel=%d\n", colorname, xcolor_original.pixel );*/
   return xcolor_original.pixel;
}

/******************************* resizeWidget *********************************/

static void
resizeWidget( Widget w ) {
   /*
    * Diese Funktion gewaehrleistet: - dass eine neue Bitmap innerhalb eines
    *                                  BitmapButtons korrekt zentriert wird.
    *
    *                                - dass bei Veraenderungen innerhalb
    *                                  der Menuleiste dieselbe korrekt ge-
    *                                  'resized' wird.
    */
   Screen *scrn;
   Window win;
   Dimension actual_width, actual_height;
         
   scrn = XtScreen( w );
   XtVaGetValues( w,
      XmNwidth, &actual_width,
      XmNheight, &actual_height,
      NULL );
   
   XtVaSetValues( w,
      XmNwidth, actual_width-1,
      XmNheight, actual_height-1,
      NULL );
   XtVaSetValues( w,
      XmNwidth, actual_width,
      XmNheight, actual_height,
      NULL );
}

/**************************** reset_simple_session_resource *******************/

static int
reset_simple_session_resource( WNAME *old, int which_resource ) {
   /*
    * Diese Funktion wird aufgerufen, wenn Input- oder Output-Text-Resourcen
    * des SimpleSessionScreens auf XResource-Werte zurueckgesetzt werden sollen.
    */
   WNAME *input, *output;
   int in_x, in_y, in_width, in_height, in_sens, out_x, out_y, out_width,
      out_height, out_sens;
   char *name, *in_font, *in_fg, *in_bg, *in_text, *out_text, *out_font,
      *out_fg, *out_bg;

   name = UIW_NewString( old->name );

   input = UIW_getWNAMEofName( UiSIMPLE_SESSION_INPUT );

   in_x      = input->x;
   in_y      = input->y;
   in_width  = input->width;
   in_height = input->height;
   in_font   = UIW_NewString( input->font );
   in_fg     = UIW_NewString( input->fg_color );
   in_bg     = UIW_NewString( input->bg_color );
   in_sens   = input->sensitivity;
   in_text   = XmTextFieldGetString( input->w );

   sessionCreated = UiSESSION;

   if( !strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ) {
      out_x      = old->x;
      out_y      = old->y;
      out_width  = old->width;
      out_height = old->height;
      out_text   = UIW_NewString( old->text );
      out_font   = UIW_NewString( old->font );
      out_fg     = UIW_NewString( old->fg_color );
      out_bg     = UIW_NewString( old->bg_color );
      out_sens   = old->sensitivity;

      ui_destroy_widget( UiSIMPLE_SESSION_OUTPUT );

      switch( which_resource ) {
         case UiFG_COLOR:
            UIW_FreeString( out_fg );
            out_fg = UiKEEPP;
            break;
         case UiBG_COLOR:
            UIW_FreeString( out_bg );
            out_bg = UiKEEPP;
            break;
         case UiFONT:
            UIW_FreeString( out_font );
            out_font = UiKEEPP;
            break;
      }
   } else {
      switch( which_resource ) {
         case UiFG_COLOR:
            UIW_FreeString( in_fg );
            in_fg = UiKEEPP;
            break;
         case UiBG_COLOR:
            UIW_FreeString( in_bg );
            in_bg = UiKEEPP;
            break;
         case UiFONT:
            UIW_FreeString( in_font );
            in_font = UiKEEPP;
            break;
      }
   }

   ui_destroy_widget( UiSIMPLE_SESSION_INPUT );

   if( !strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ) {
      ui_create_output_text( UiSIMPLE_SESSION_OUTPUT, out_x, out_y, out_width,
         out_height, out_text, out_font, out_fg, out_bg );
      ui_set_sensitivity( UiSIMPLE_SESSION_OUTPUT, out_sens );
   }

   ui_create_input_text( UiSIMPLE_SESSION_INPUT, in_x, in_y, in_width,
      in_height, in_font, in_fg, in_bg );
   input = UIW_getWNAMEofName( UiSIMPLE_SESSION_INPUT );
   XmTextFieldSetString( input->w, in_text );
   ui_set_sensitivity( UiSIMPLE_SESSION_INPUT, in_sens );

   sessionCreated = UiSIMPLE_SESSION;

   UIW_FreeString( in_font );
   UIW_FreeString( in_fg   );
   UIW_FreeString( in_bg   );
   UiFree( in_text );
   if( !strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ) {
      UIW_FreeString( out_text );
      UIW_FreeString( out_font );
      UIW_FreeString( out_fg   );
      UIW_FreeString( out_bg   );
   }
   UIW_FreeString( name );

   /*
    * Um sicherzustellen, dass die 'menubar' vernuenftig ge-
    * resized wird:
    */
   XtVaSetValues( menubar,
      XmNheight, 30,
      NULL );
   resizeWidget( session );

   XmProcessTraversal( input->w, XmTRAVERSE_CURRENT );
   return UiOK;
}

/**************************** reset_resource **********************************/

static int
reset_resource( WNAME *old, int which_resource ) {
   /*
    * Diese Funktion wird aufgerufen, wenn Resourcen existierender Widgets
    * auf XResource-Werte zurueckgesetzt werden sollen.
    */
   WNAME *insert_point, *new, *actual_widget, *next_widget;
   char *name_old, *fg_old, *bg_old, *font_old, *bitmap_old,
      *text_old, *callback_old, *input_old;
   Widget w_old, w;
   int class_old, x_old, y_old, width_old, height_old, align_old,
      visibility_old, sensitivity_old, ret, c;

   if( sessionCreated == UiSIMPLE_SESSION )
      return reset_simple_session_resource( old, which_resource );

   if( old->class == UiPULLDOWN_MENU || old->class == UiMENU_ENTRY )
      return UiERROR;

   insert_point    = old->next;
   name_old        = UIW_NewString( old->name );
   w_old           = old->w;
   class_old       = old->class;
   x_old           = old->x;
   y_old           = old->y;
   width_old       = old->width;
   height_old      = old->height;
   fg_old          = UIW_NewString( old->fg_color );
   bg_old          = UIW_NewString( old->bg_color );
   font_old        = UIW_NewString( old->font );
   bitmap_old      = UIW_NewString( old->bitmap );
   align_old       = old->alignment;
   text_old        = UIW_NewString( old->text );
   callback_old    = UIW_NewString( old->callbackMsg );
   visibility_old  = old->visibility;
   sensitivity_old = old->sensitivity;
   if( class_old == UiINPUT_TEXT )
      input_old    = XmTextFieldGetString( w_old );
   else
      input_old    = UiERRORP;

   switch( which_resource ) {
      case UiFG_COLOR:
         UIW_FreeString( fg_old );
         fg_old = UiKEEPP;
         break;
      case UiBG_COLOR:
         UIW_FreeString( bg_old );
         bg_old = UiKEEPP;
         break;
      case UiFONT:
         UIW_FreeString( font_old );
         font_old = UiKEEPP;
         break;
      case UiALIGNMENT:
         align_old = UiKEEP;
         break;
      case UiTEXT:
         UIW_FreeString( text_old );
         text_old = UiKEEPP;
         break;
   }

   ret = ui_destroy_widget( name_old );

   if( ret == UiOK ) {
      switch( class_old ) {
         case UiSEPARATOR:
            ret = ui_create_separator( name_old, x_old, y_old, width_old,
               height_old, fg_old );
            break;
         case UiPUSHBUTTON:
            ret = ui_create_pushbutton( name_old, x_old, y_old, width_old,
               height_old, text_old, font_old, align_old, fg_old, bg_old,
               callback_old );
            break;
         case UiLABEL:
            ret = ui_create_label( name_old, x_old, y_old, width_old,
               height_old, text_old, font_old, align_old, fg_old, bg_old );
            break;
         case UiBITMAPBUTTON:
            ret = ui_create_bitmap_button( name_old, x_old, y_old, width_old,
               height_old, bitmap_old, fg_old, bg_old, callback_old );
            break;
         case UiBITMAPLABEL:
            ret = ui_create_bitmap_label( name_old, x_old, y_old, width_old,
               height_old, bitmap_old, fg_old, bg_old );
            break;
         case UiINPUT_TEXT:
            ret = ui_create_input_text( name_old, x_old, y_old, width_old,
               height_old, font_old, fg_old, bg_old );
            XmTextFieldSetString( (UIW_getWNAMEofName( name_old ))->w, input_old );
            break;
         case UiOUTPUT_TEXT:
            ret = ui_create_output_text( name_old, x_old, y_old, width_old,
               height_old, text_old, font_old, fg_old, bg_old );
            break;
         default:
            return UiERROR;
      }
      if( ret == UiOK ) {
         if( visibility_old == UiNIL )
            ui_set_visibility( name_old, UiNIL );
         if( sensitivity_old == UiNIL )
            ui_set_sensitivity( name_old, UiNIL );

         /*
          *  Urspruengliche Stacking-Order wiederherstellen:
          *  (sowohl in wname-Liste als auch auf Bildschirm)
          */
         new = UIW_getWNAMEofName( name_old );
      
         /* in wname-Liste: */
         UIW_restackWNAME( new, insert_point );
      
         /* auf Bildschirm: */
         next_widget = new->next;
         while( actual_widget = next_widget ) {
            w = actual_widget->w;
            c = actual_widget->class;
            if( c != UiPULLDOWN_MENU && c != UiMENU_ENTRY )
               XRaiseWindow( XtDisplay( w ), XtWindow( w ) );
            next_widget = actual_widget->next;
         }
      }
   }

   UIW_FreeString( name_old );
   UIW_FreeString( fg_old );
   UIW_FreeString( bg_old );
   UIW_FreeString( font_old );
   UIW_FreeString( bitmap_old );
   UIW_FreeString( text_old );
   UIW_FreeString( callback_old );
   UIW_FreeString( input_old );

   return ret;
}

/******************************* checkMargins *********************************/

static void
checkMargins( Widget w ) {
   /*
    * Diese Funktion gewaehrleistet einen Mindestabstand von 3 Pixel zwischen
    * Rand und Textanfang bzw. -ende des Widgets 'w'.
    */
   int left, right, align;

   XtVaGetValues( w, XmNmarginLeft, &left,
                     XmNmarginRight, &right,
                     XmNalignment, &align,
                     NULL );
   if( align == XmALIGNMENT_BEGINNING && left < 3 ) {
      XtVaSetValues( w, XmNmarginLeft, 3, NULL );
      return;
   }
   if( align == XmALIGNMENT_END && right < 3 ) {
      XtVaSetValues( w, XmNmarginRight, 3, NULL );
      return;
   }
}

/***************************** print_int_value ********************************/

static void
print_int_value( char *command, int value ) {
   /*
    * Diese Funktion fuegt dem String 'command' einen Integer-Wert
    * 'value' hinzu.
    */
   char *value_str;

   value_str = (char*)XtMalloc( 256 );
   if( value == UiKEEP )
      strcat( command, "nil " );
   else if ( value != UiERROR ) {
      sprintf( value_str, "%d ", value );
      strcat( command, value_str );
   }
   UiFree( value_str );
}

/***************************** print_char_value *******************************/

static void
print_char_value( char *command, char *value ) {
   /*
    * Diese Funktion fuegt dem String 'command' einen String
    * 'value' hinzu.
    */

   if( value == UiKEEPP )
      strcat( command, "nil " );
   else if ( value != UiERRORP ) {
      strcat( command, "\"" );
      strcat( command, value );
      strcat( command, "\" " );
   }
}

/***************************** encode_str *************************************/

static char*
encode_str( char *in ) {
   /*
    * Diese Funktion aendert innerhalb des Strings 'in' alle
    * '"' in '\"'.
    */
   char *out, *str = (char *)XtMalloc( 2 * strlen(in) + 1 );
   int c, i = 0, j = 0, len = strlen( in );

   while( i < len )
      if( (c=in[i++]) == '"' ) {
         str[j++] = '\\';
         str[j++] = '"';
      } else
         str[j++] = c;
   str[j] = 0;
   out = (char *)XtMalloc( strlen( str ) + 1);
   strcpy( out, str );
   UiFree( str );
   return out;
}

/***************************** print_encoded_str ******************************/

static void
print_encoded_str( char *command, char *value ) {
   /*
    * Diese Funktion fuegt dem String 'command' einen kodierten ('"' --> '\"')
    * String hinzu.
    */
   char *value_copy, *value_encoded, *str, *out;
   int i, len, c, j;

   if( value == UiKEEPP )
      strcat( command, "nil " );
   else if ( value != UiERRORP ) {
      strcat( command, "\"" );

      /*
       *   Um sicherzustellen, dass alle Befehlszeilen der mit
       *   ui-save() gesicherten Datei von LISP korrekt
       *   verstanden werden: '"' --> '\"'
       */
      i = 0;
      j = 0;
      len = strlen( value );
      value_encoded = (char *)XtMalloc( 2 * len + 1 );
      while( i < len ) {
         if( (c=value[i++]) == '"' ) {
            value_encoded[j++] = '\\';
            value_encoded[j++] = '"';
         } else
            value_encoded[j++] = c;
      }
      value_encoded[j] = 0;

      strcat( command, value_encoded );
      UiFree( value_encoded );
      strcat( command, "\" " );
   }
}

/***************************** print_align_value ******************************/

static void
print_align_value( char *command, int value ) {
   /*
    * Diese Funktion fuegt dem String 'command' einen Alignment-String
    * hinzu.
    */

   if( value == UiKEEP )
      strcat( command, "nil " );
   else if ( value != UiERROR )
      switch( value ) {
         case UiLEFT:
            strcat( command, ":left " );
            break;
         case UiRIGHT:
            strcat( command, ":right " );
            break;
         case UiCENTER:
            strcat( command, ":center " );
            break;
      }
}

/***************************** print_mnem_value *******************************/

static void
print_mnem_value( char *command, int value ) {
   /*
    * Diese Funktion fuegt dem String 'command' einen Mnemonic-Wert
    * hinzu.
    */
   char *value_str;

   value_str = (char*)XtMalloc( 256 );
   if( value == UiKEEP )
      strcat( command, "nil " );
   else if ( value != UiERROR ) {
      strcat( command, "\"" );
      sprintf( value_str, "%c", value );
      strcat( command, value_str );
      strcat( command, "\" " );
   }
   UiFree( value_str );
}

/**************************** print_char_resource *****************************/

static void
print_char_resource( FILE *fp, char *name, char *resource_name, char *value ) {
   /*
    * Diese Funktion schreibt eine String-Resource in eine XResource-Datei.
    */
   char *str;

   if( value == UiKEEPP || value == UiERRORP )
      return;
   str = (char*)XtMalloc( 1024 );
   if( name )
      sprintf( str, "Session*%s.%s:", name, resource_name );
   else
      sprintf( str, "Session*%s:", resource_name );
   while( strlen( str ) < RESOURCE_TAB )
      strcat( str, " " );
   fprintf( fp, "%s%s\n", str, value );
   UiFree( str );
}

/**************************** print_int_resource ******************************/

static void
print_int_resource( FILE *fp, char *name, char *resource_name, int value ) {
   /*
    * Diese Funktion schreibt eine Integer-Resource in eine XResource-Datei.
    */
   char *str, *value_str;

   if( value == UiKEEP || value == UiERROR )
      return;

   value_str = (char*)XtMalloc( 256 );
   str = (char*)XtMalloc( 1024 );
   switch( value ) {
      case UiLEFT:
         strcpy( value_str, "XmALIGNMENT_BEGINNING" );
         break;
      case UiCENTER:
         strcpy( value_str, "XmALIGNMENT_CENTER" );
         break;
      case UiRIGHT:
         strcpy( value_str, "XmALIGNMENT_END" );
         break;
      default:
         sprintf( value_str, "%c", value );
   }

   sprintf( str, "Session*%s.%s:", name, resource_name );
   while( strlen( str ) < RESOURCE_TAB )
      strcat( str, " " );
   fprintf( fp, "%s%s\n", str, value_str );
   UiFree( str );
   UiFree( value_str );
}

/**************************** save_commands ***********************************/

static int
save_commands( char *command_name ) {
   /*
    * Diese Funktion erzeugt eine Lisp-Befehls-Datei 'command_name'
    * im aktuellen Verzeichnis, mit der die aktuelle Benutzer-Oberflaeche
    * erzeugt werden kann. Die Resourcen aller Widgets werden ge-
    * 'hardcoded'.
    */
   FILE *command_file;
   char *command;
   WNAME *actual_widget, *next_widget, *pulldown_widget;

   if( !(command_file = fopen( command_name, "a+" )) )
      return UiERROR;

   fprintf( command_file, "\n\n;;; User-Interface:\n\n" );
   command = (char*)XtMalloc( 1024 );
   if( session_bg != UiKEEPP ) {
      strcpy( command, "(ui-set-session-bg \"" );
      strcat( command, session_bg );
      strcat( command, "\" )" );
      fprintf( command_file, "%s\n", command );
   }
   if( menu_bg != UiKEEPP ) {
      strcpy( command, "(ui-set-menu-bg \"" );
      strcat( command, menu_bg );
      strcat( command, "\" )" );
      fprintf( command_file, "%s\n", command );
   }
   if( menu_fg != UiKEEPP ) {
      strcpy( command, "(ui-set-menu-fg \"" );
      strcat( command, menu_fg );
      strcat( command, "\" )" );
      fprintf( command_file, "%s\n", command );
   }
   if( menu_font != UiKEEPP ) {
      strcpy( command, "(ui-set-menu-font \"" );
      strcat( command, menu_font );
      strcat( command, "\" )" );
      fprintf( command_file, "%s\n", command );
   }

   /*
    *  Alle in der wname-Liste enthaltenen Widgets mit ihren
    *  momentanen Einstellungen in Befehlsformat in der Datei
    *  command_name ablegen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      switch( actual_widget->class ) {
         case UiSEPARATOR:
            strcpy( command, "(ui-create-separator \"" );
            break;
         case UiPUSHBUTTON:
            strcpy( command, "(ui-create-pushbutton \"" );
            break;
         case UiLABEL:
            strcpy( command, "(ui-create-label \"" );
            break;
         case UiBITMAPBUTTON:
            strcpy( command, "(ui-create-bitmap-button \"" );
            break;
         case UiBITMAPLABEL:
            strcpy( command, "(ui-create-bitmap-label \"" );
            break;
         case UiINPUT_TEXT:
            strcpy( command, "(ui-create-input-text \"" );
            break;
         case UiOUTPUT_TEXT:
            strcpy( command, "(ui-create-output-text \"" );
            break;
         case UiPULLDOWN_MENU:
            strcpy( command, "(ui-create-pulldown-menu \"" );
            break;
         case UiMENU_ENTRY:
            strcpy( command, "(ui-create-menu-entry \"" );
            break;
      }
      strcat( command, actual_widget->name );
      strcat( command, "\" " );
      if( actual_widget->class == UiMENU_ENTRY ) {
         strcat( command, "\"" );
         pulldown_widget = UIW_getWNAMEofWidget( XtParent( actual_widget->w ) );
         strcat( command, pulldown_widget->name );
         strcat( command, "\" " );
      }
      print_int_value(   command, actual_widget->x           );
      print_int_value(   command, actual_widget->y           );
      print_int_value(   command, actual_widget->width       );
      print_int_value(   command, actual_widget->height      );
      print_encoded_str( command, actual_widget->text        );
      print_char_value(  command, actual_widget->font        );
      print_align_value( command, actual_widget->alignment   );
      print_char_value(  command, actual_widget->bitmap      );
      print_char_value(  command, actual_widget->fg_color    );
      print_char_value(  command, actual_widget->bg_color    );
      print_mnem_value(  command, actual_widget->mnemonic    );
      print_encoded_str( command, actual_widget->callbackMsg );
      strcat( command, ")" );
      fprintf( command_file, "%s\n", command );

      if( actual_widget->visibility == UiNIL )
         fprintf( command_file, "( ui-set-visibility \"%s\" nil )\n",
                  actual_widget->name );
      if( actual_widget->sensitivity == UiNIL )
         fprintf( command_file, "( ui-set-sensitivity \"%s\" nil )\n",
                  actual_widget->name );

      next_widget = actual_widget->next;
   }

   fclose( command_file );
   UiFree( command );
   return UiOK;
}

/*********************** save_commands_and_resources **************************/

static int
save_commands_and_resources( char *command_name, char *resource_name ) {
   /*
    * Diese Funktion erzeugt eine Lisp-Befehls-Datei 'command_name' und
    * eine XResource-Datei 'resource_name' im aktuellen Verzeichnis
    * mit denen die aktuelle Benutzer-Oberflaeche erzeugt werden kann.
    * Alle Resourcen, die aus den XResourcen beeinflusst werden koennen
    * sind nicht ge-'hardcoded', sondern in die Datei 'resource_name'
    * aufgenommen.
    */
   FILE *command_file, *resource_file;
   char *command, *resource, *str;
   WNAME *actual_widget, *next_widget, *pulldown_widget;

   if( !(command_file = fopen( command_name, "a+" )) )
      return UiERROR;

   if( !(resource_file = fopen( resource_name, "a+" )) ) {
      fclose( command_file );
      return UiERROR;
   }

   fprintf( command_file, "\n\n;;; User-Interface:\n\n" );
   fprintf( resource_file, "\n\n!!!!!!!!!!!!!!!!!!!!!!!!\n" );
   fprintf( resource_file,     "!!!  User-Interface  !!!\n" );
   fprintf( resource_file,     "!!!!!!!!!!!!!!!!!!!!!!!!\n\n" );
   command = (char*)XtMalloc( 1024 );
   resource = (char*)XtMalloc( 1024 );
   str = (char*)XtMalloc( 512 );

   print_char_resource( resource_file, NULL, "background",      session_bg );
   print_char_resource( resource_file, NULL, "menubar*background", menu_bg );
   print_char_resource( resource_file, NULL, "menubar*foreground", menu_fg );
   print_char_resource( resource_file, NULL, "menubar*fontList", menu_font );

   /*
    *  Alle in der wname-Liste enthaltenen Widgets mit ihren
    *  momentanen Einstellungen in Befehlsformat in der Datei
    *  command_name und alle moeglichen XResource-Werte in
    *  der Datei resource_name ablegen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      switch( actual_widget->class ) {
         case UiSEPARATOR:
            strcpy( command, "(ui-create-separator \"" );
            fprintf( resource_file, "\n!!! Separator \"%s\"\n",
               actual_widget->name );
            break;
         case UiPUSHBUTTON:
            strcpy( command, "(ui-create-pushbutton \"" );
            fprintf( resource_file, "\n!!! Pushbutton \"%s\"\n",
               actual_widget->name );
            break;
         case UiLABEL:
            strcpy( command, "(ui-create-label \"" );
            fprintf( resource_file, "\n!!! Label \"%s\"\n",
               actual_widget->name );
            break;
         case UiBITMAPBUTTON:
            strcpy( command, "(ui-create-bitmap-button \"" );
            fprintf( resource_file, "\n!!! Bitmap-Button \"%s\"\n",
               actual_widget->name );
            break;
         case UiBITMAPLABEL:
            strcpy( command, "(ui-create-bitmap-label \"" );
            fprintf( resource_file, "\n!!! Bitmap-Label \"%s\"\n",
               actual_widget->name );
            break;
         case UiINPUT_TEXT:
            strcpy( command, "(ui-create-input-text \"" );
            fprintf( resource_file, "\n!!! Input-Text \"%s\"\n",
               actual_widget->name );
            break;
         case UiOUTPUT_TEXT:
            strcpy( command, "(ui-create-output-text \"" );
            fprintf( resource_file, "\n!!! Output-Text \"%s\"\n",
               actual_widget->name );
            break;
         case UiPULLDOWN_MENU:
            strcpy( command, "(ui-create-pulldown-menu \"" );
            fprintf( resource_file, "\n!!! Pulldown-Menu \"%s\"\n",
               actual_widget->name );
            break;
         case UiMENU_ENTRY:
            strcpy( command, "(ui-create-menu-entry \"" );
            fprintf( resource_file, "\n!!! Menu-Entry \"%s\"\n",
               actual_widget->name );
            break;
      }
      strcat( command, actual_widget->name );
      strcat( command, "\" " );
      print_int_value( command, actual_widget->x      );
      print_int_value( command, actual_widget->y      );
      print_int_value( command, actual_widget->width  );
      print_int_value( command, actual_widget->height );

      switch( actual_widget->class ) {

         case UiSEPARATOR:
            strcat( command, "nil " );
            print_char_resource( resource_file, actual_widget->name,
               "foreground", actual_widget->fg_color );
            break;

         case UiPUSHBUTTON:
         case UiLABEL:
            strcat( command, "nil nil nil nil nil " );
            print_char_resource( resource_file, actual_widget->name,
               "labelString", actual_widget->text );
            print_char_resource( resource_file, actual_widget->name,
               "fontList", actual_widget->font );
            print_int_resource( resource_file, actual_widget->name,
               "alignment", actual_widget->alignment );
            print_char_resource( resource_file, actual_widget->name,
               "foreground", actual_widget->fg_color );
            print_char_resource( resource_file, actual_widget->name,
               "background", actual_widget->bg_color );
            break;

         case UiBITMAPBUTTON:
         case UiBITMAPLABEL:
            print_char_value( command, actual_widget->bitmap );
            strcat( command, "nil nil " );
            print_char_resource( resource_file, actual_widget->name,
               "foreground", actual_widget->fg_color );
            print_char_resource( resource_file, actual_widget->name,
               "background", actual_widget->bg_color );
            break;

         case UiOUTPUT_TEXT:
            print_encoded_str( command, actual_widget->text );
         case UiINPUT_TEXT:
            strcat( command, "nil nil nil " );
            print_char_resource( resource_file, actual_widget->name,
               "fontList", actual_widget->font );
            print_char_resource( resource_file, actual_widget->name,
               "foreground", actual_widget->fg_color );
            print_char_resource( resource_file, actual_widget->name,
               "background", actual_widget->bg_color );
            break;

         case UiMENU_ENTRY:
            strcat( command, "\"" );
            pulldown_widget = UIW_getWNAMEofWidget( XtParent( actual_widget->w ) );
            strcat( command, pulldown_widget->name );
            strcat( command, "\" " );
         case UiPULLDOWN_MENU:
            strcat( command, "nil nil " );
            print_char_resource( resource_file, actual_widget->name,
               "labelString", actual_widget->text );
            print_int_resource( resource_file, actual_widget->name,
               "mnemonic", actual_widget->mnemonic );
            break;
      }

      print_encoded_str( command, actual_widget->callbackMsg );
      strcat( command, ")" );
      fprintf( command_file, "%s\n", command );

      if( actual_widget->visibility == UiNIL )
         fprintf( command_file, "(ui-set-visibility \"%s\" nil )\n",
                  actual_widget->name );
      if( actual_widget->sensitivity == UiNIL )
         fprintf( command_file, "(ui-set-sensitivity \"%s\" nil )\n",
                  actual_widget->name );

      next_widget = actual_widget->next;
   }

   fclose( resource_file );
   fclose( command_file );
   UiFree( str );
   UiFree( resource );
   UiFree( command );
   return UiOK;
}

/******************************************************************************/
/*                                                                            */
/*                        Event orientation controls                          */
/*                                                                            */
/******************************************************************************/

/*********************** ui_create_simple_session_screen **********************/

int
ui_create_simple_session_screen( void ) {
   WNAME *input;

   if( sessionCreated != UiNO_SESSION )
      return UiERROR;

   createSession( UiSIMPLE_SESSION );

   sessionCreated = UiSESSION; /* kurzzeitige Aenderung, um Erzeugung von In- */
                               /* und OutputText zu ermoeglichen              */

   ui_create_output_text( UiSIMPLE_SESSION_OUTPUT, 0, 0, 600, 320, "",
      UiKEEPP, UiKEEPP, UiKEEPP );
   ui_create_input_text( UiSIMPLE_SESSION_INPUT, 0, 320, 600, 50,
      UiKEEPP, UiKEEPP, UiKEEPP );
   sessionCreated = UiSIMPLE_SESSION;

   input = UIW_getWNAMEofName( UiSIMPLE_SESSION_INPUT );
   XmProcessTraversal( input->w, XmTRAVERSE_CURRENT );

   return UiOK;
}

/**************************** ui_stop_session *********************************/

/*
 * Linux BUG:         Hans 24.08.94 
 *
 * XtMainLoop() seems to need data structures one tick later,
 * XtDestroyWidget(session) and XtFree() perhaps interfere!
 * WORK-AROUND: XtAddTimeOut() seems to decouple this!
 * see ui.c !
 */

void
ui_stop_session_to( void ) {

   sessionCreated = UiNO_SESSION;   /* this before destroy to avoid dying of Xappl */

   XtDestroyWidget( session );      /* after this ui_set_cursor must do nothing ! */

   destroyPixmap( SystemDialogs, gridOn  );
   destroyPixmap( SystemDialogs, gridOff );

   /*** obsolete: needed furthermore: see main.c ***/
   /* XFreeCursor( dpy, cursor_normal ); */
   /* XFreeCursor( dpy, cursor_busy   ); */
}

/*******************************************************/

int
ui_stop_session( void ) {
   WNAME *actual_wname;
   int save = sessionCreated;

   if( sessionCreated == UiSIMPLE_SESSION ||
       sessionCreated == UiSESSION ) {

      sessionCreated = UiSESSION; /* kurzzeitige Aenderung, um Eliminierung */
                                  /* von In- und OutputText zu ermoeglichen */
      while( actual_wname = UIW_getRootWNAME() ) {
         /*** Linux BUG: this does not work! ***/
         /*UIW_deleteWNAME( actual_wname );*/ 
         /*** THIS works (together with ui_stop_session_to()): ***/
         ui_destroy_widget( actual_wname->name );
      }
      sessionCreated = save;
      return UiOK;
   } else
      return UiERROR;
}

/**************************** ui_set_cursor ***********************************/

static int
ui_set_cursor( int cursor_form ) {
   Window s_win, w_win, so_win, si_win;
   WNAME *so, *si;
   Widget so_w, si_w;

   switch( sessionCreated ) {
      case UiNO_SESSION:
         return UiERROR;
      case UiSIMPLE_SESSION:
/*         so = UIW_getWNAMEofName( UiSIMPLE_SESSION_OUTPUT );
         si = UIW_getWNAMEofName( UiSIMPLE_SESSION_INPUT  );
         so_w = XtParent( so->w );
         si_w = si->w;
         so_win = XtWindow( so_w );
         si_win = XtWindow( si_w );
         break;
*/      case UiSESSION:
         w_win = XtWindow( work_form );
   }
   s_win = XtWindow( session );

   switch( cursor_form ) {
      case UiNORMAL:
         XDefineCursor( dpy, s_win, cursor_normal );
/*         XtSetSensitive( session, True );*/
#if 0
         if( sessionCreated == UiSIMPLE_SESSION ) {
            XDefineCursor( dpy, so_win, cursor_normal );
            XDefineCursor( dpy, si_win, cursor_normal );
/*            XtSetSensitive( so_w, True );*/
/*            XtSetSensitive( si_w, True );*/
         }
#endif
         if( sessionCreated != UiNO_SESSION ) {
            XDefineCursor( dpy, w_win, cursor_normal );
/*            XtSetSensitive( work_form, True );*/
         }
         actual_cursor = UiNORMAL;
         return UiOK;
      case UiBUSY:
         XDefineCursor( dpy, s_win, cursor_busy );
/*         XtSetSensitive( session, True );*/       /* only help, break, grid !!! */
#if 0
         if( sessionCreated == UiSIMPLE_SESSION ) {
            XDefineCursor( dpy, so_win, cursor_busy );
            XDefineCursor( dpy, si_win, cursor_busy );
/*            XtSetSensitive( so_w, False );*/
/*            XtSetSensitive( si_w, False );*/
         }
#endif
         if( sessionCreated != UiNO_SESSION ) {
            XDefineCursor( dpy, w_win, cursor_busy );
/*            XtSetSensitive( work_form, False );*/
         }
         actual_cursor = UiBUSY;
         return UiOK;
      default:
         return UiERROR;
   }
}

/******************************************************************************/
/*                                                                            */
/*                        Basic Widget Operations                             */
/*                                                                            */
/******************************************************************************/

/**************************** ui_destroy_widget *******************************/

int
ui_destroy_widget( char *name ) {
   WNAME *existent, *actual_widget, *next_widget;
   Widget w;

   if( sessionCreated == UiSIMPLE_SESSION       &&
       (!strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ||
        !strcmp( name, UiSIMPLE_SESSION_INPUT  )) )
      return UiERROR;

   if( existent = UIW_getWNAMEofName( name ) ) {

      switch( existent->class ) {
         case UiPULLDOWN_MENU:
            w = existent->w;
            XtDestroyWidget( existent->cascade_button );
            /*
             *    Alle MenuEntries dieses PulldownMenus aus wname-Liste
             *    entfernen:
             */
            next_widget = UIW_getRootWNAME();
            while( actual_widget = next_widget ) {
               next_widget = actual_widget->next;
               if( ( actual_widget->class         == UiMENU_ENTRY ) &&
                   ( XtParent( actual_widget->w ) == w            ) )
                  UIW_deleteWNAME( actual_widget );
            }
            break;
         case UiOUTPUT_TEXT:
            w = XtParent( existent->w );
            break;
         default:
            w = existent->w;
      }

      XtDestroyWidget( w );
      UIW_deleteWNAME( existent );
      return UiOK;
   } else
      return UiERROR;
}

/**************************** ui_raise_to_top *********************************/

int
ui_raise_to_top( char *name ) {
   WNAME *existent;
   Widget w;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( existent = UIW_getWNAMEofName( name ) ) {
      switch( existent->class ) {
         case UiPULLDOWN_MENU:
         case UiMENU_ENTRY:
            return UiERROR;
         case UiOUTPUT_TEXT:
            w = XtParent( existent->w );
            break;
         default:
            w = existent->w;
      }
      XRaiseWindow( XtDisplay( w ), XtWindow( w ) );
      UIW_setWNAMEtoEndOfList( existent );
      return UiOK;
   } else
      return UiERROR;
}

/**************************** ui_lower_to_bottom ******************************/

int
ui_lower_to_bottom( char *name ) {
   WNAME *existent;
   Widget w;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( existent = UIW_getWNAMEofName( name ) ) {
      switch( existent->class ) {
         case UiPULLDOWN_MENU:
         case UiMENU_ENTRY:
            return UiERROR;
         case UiOUTPUT_TEXT:
            w = XtParent( existent->w );
            break;
         default:
            w = existent->w;
      }
      XLowerWindow( XtDisplay( w ), XtWindow( w ) );
      UIW_setWNAMEtoBeginningOfList( existent );
      return UiOK;
   } else
      return UiERROR;
}

/******************************************************************************/
/*                                                                            */
/*                        Widget Creation Functions                           */
/*                                                                            */
/******************************************************************************/

/**************************** ui_create_separator *****************************/

int
ui_create_separator( char* name, int x, int y,
                      int width, int height, char *fg_color ) {
   WNAME *wName;
   Widget w;
   int x_def, y_def;
   Dimension width_def, height_def;
   unsigned char orient_def;
   long color;
   Pixel fg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width && !height )   /* At least one of them has to be > 0 */
      return UiERROR;

   x_def = x;
   y_def = y;

   if( width ) {
      orient_def = XmHORIZONTAL;
      width_def = width;
      height_def = 0;
   } else {
      orient_def = XmVERTICAL;
      width_def = 0;
      height_def = height;
   }

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmSeparatorWidgetClass, work_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopAttachment,  XmATTACH_FORM,
      NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiSEPARATOR;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UiERRORP;
   wName->font           = UiERRORP;
   wName->bitmap         = UiERRORP;
   wName->alignment      = UiERROR;
   wName->text           = UiERRORP;
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiERROR;   /* cannot become sensitive! */

   XtVaGetValues( w,
      XmNforeground,        &fg_color_def,
      NULL );

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   /** Bestimmung der zum Foreground passenden Shadowcolors **/
   /*  Es wird also eigentlich nicht die gewuenschte Farbe   */
   /*  verwendet, sondern die passenden Shadowcolors         */
   XmGetColors( XtScreen( w ), cmp, fg_color_def, &fg_suggest,
                &top_shadow_def, &bottom_shadow_def, &select_suggest );
                  
   XtVaSetValues( w,
      XmNleftOffset,        x_def,
      XmNtopOffset,         y_def,
      XmNwidth,             width_def,
      XmNheight,            height_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      XmNorientation,       orient_def,
      NULL );

   UIW_addWNAME( wName );
   XtManageChild( w );
   return UiOK;
}

/***************************** ui_create_pushbutton ***************************/

int
ui_create_pushbutton( char* name, int x, int y, int width, int height,
             char *labelText, char *font, int alignment, char *fg_color,
             char *bg_color, char *callbackMsg ) {
   WNAME *wName;
   Widget w;
   int x_def, y_def;
   Dimension width_def, height_def;
   XmString labelText_def;
   XmFontList font_def;
   unsigned char align_def;
   long color;
   Pixel fg_color_def, bg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;
   XFontStruct *fonty;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmPushButtonWidgetClass, work_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopAttachment,  XmATTACH_FORM,
      XmNrecomputeSize,  False,
      XmNnavigationType, XmTAB_GROUP,
      NULL );
   XtAddCallback( w, XmNactivateCallback, (XtPointer)ButtonCB, NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiPUSHBUTTON;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UIW_NewString( font );
   wName->bitmap         = UiERRORP;
   wName->alignment      = alignment;
   wName->text           = UIW_NewString( labelText );
   wName->callbackMsg    = UIW_NewString( callbackMsg );
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiTRUE;

   XtVaGetValues( w,
      XmNlabelString,       &labelText_def,
      XmNfontList,          &font_def,
      XmNalignment,         &align_def,
      XmNforeground,        &fg_color_def,
      XmNbackground,        &bg_color_def,
      NULL );

   if( labelText != UiKEEPP )
      labelText_def = XmStringCreateLtoR( labelText,
         XmSTRING_DEFAULT_CHARSET );

   if( font != UiKEEPP ) {
      fonty = XLoadQueryFont( dpy, font );
      font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
   }

   switch( alignment ) {
      case UiKEEP:
         break;
      case UiLEFT:
         align_def = XmALIGNMENT_BEGINNING;
         break;
      case UiCENTER:
         align_def = XmALIGNMENT_CENTER;
         break;
      case UiRIGHT:
         align_def = XmALIGNMENT_END;
         break;
   }

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   /** Bestimmung der zum Background passenden Shadowcolors **/
   XmGetColors( XtScreen( w ), cmp, bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   XtVaSetValues( w,
      XmNleftOffset,        x_def,
      XmNtopOffset,         y_def,
      XmNwidth,             width_def,
      XmNheight,            height_def,
      XmNlabelString,       labelText_def,
      XmNfontList,          font_def,
      XmNalignment,         align_def,
      XmNforeground,        fg_color_def,
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      XmNarmColor,          select_suggest,
      NULL );
   checkMargins( w );    /* for :left ... */

   UIW_addWNAME( wName );
   XtManageChild( w );

   if( labelText != UiKEEPP )
      XmStringFree( labelText_def );

   if( font != UiKEEPP )
      XmFontListFree( font_def );

   return UiOK;
}

/***************************** ui_create_label ********************************/

int
ui_create_label( char* name, int x, int y, int width, int height,
            char *labelText, char *font, int alignment,
            char *fg_color, char *bg_color ) {
   WNAME *wName;
   Widget w;
   int x_def, y_def;
   Dimension width_def, height_def;
   XmString labelText_def;
   XmFontList font_def;
   unsigned char align_def;
   long color;
   Pixel fg_color_def, bg_color_def;
   XFontStruct *fonty;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmLabelWidgetClass, work_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopAttachment,  XmATTACH_FORM,
      XmNrecomputeSize,  False,
      NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiLABEL;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UIW_NewString( font );
   wName->bitmap         = UiERRORP;
   wName->alignment      = alignment;
   wName->text           = UIW_NewString( labelText );
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiERROR;

   XtVaGetValues( w,
      XmNlabelString,   &labelText_def,
      XmNfontList,      &font_def,
      XmNalignment,     &align_def,
      XmNforeground,    &fg_color_def,
      XmNbackground,    &bg_color_def,
      NULL );

   if( labelText != UiKEEPP )
      labelText_def = XmStringCreateLtoR( labelText,
         XmSTRING_DEFAULT_CHARSET );

   if( font != UiKEEPP ) {
      fonty = XLoadQueryFont( dpy, font );
      font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
   }

   switch( alignment ) {
      case UiKEEP:
         break;
      case UiLEFT:
         align_def = XmALIGNMENT_BEGINNING;
         break;
      case UiCENTER:
         align_def = XmALIGNMENT_CENTER;
         break;
      case UiRIGHT:
         align_def = XmALIGNMENT_END;
         break;
   }

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   XtVaSetValues( w,
      XmNleftOffset,        x_def,
      XmNtopOffset,         y_def,
      XmNwidth,             width_def,
      XmNheight,            height_def,
      XmNlabelString,       labelText_def,
      XmNfontList,          font_def,
      XmNalignment,         align_def,
      XmNforeground,        fg_color_def,
      XmNbackground,        bg_color_def,
      NULL );
   checkMargins( w );    /* for :left ... */

   UIW_addWNAME( wName );
   XtManageChild( w );

   if( labelText != UiKEEPP )
      XmStringFree( labelText_def );

   if( font != UiKEEPP )
      XmFontListFree( font_def );

   return UiOK;
}

/***************************** ui_create_bitmap_button ************************/

int
ui_create_bitmap_button( char* name, int x, int y, int width, int height,
             char *bitmap, char *fg_color, char *bg_color, char *callbackMsg ) {
   WNAME *wName;
   Widget w;
   int x_def, y_def;
   Dimension width_def, height_def;
   long color;
   Pixel fg_color_def, bg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;
   XmString labelText_def;
   Pixmap pm;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   if( bitmap == UiERRORP )
      return UiERROR;

   if( bitmap == UiKEEPP )
      bitmap = "";         /* no image! */

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;
   labelText_def = XmStringCreateLtoR( "", XmSTRING_DEFAULT_CHARSET );

   /*
    * Zur Zeit wird nicht unterstuetzt, dass der bitmap-Path aus den Xresourcen
    * kommt !
    */
               
   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmPushButtonWidgetClass, work_form,
      XmNleftAttachment,  XmATTACH_FORM,
      XmNtopAttachment,   XmATTACH_FORM,
      XmNrecomputeSize,   False,
      XmNnavigationType,  XmTAB_GROUP,
      NULL );
   XtAddCallback( w, XmNactivateCallback, (XtPointer)ButtonCB, NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiBITMAPBUTTON;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UiERRORP;
   wName->bitmap         = UIW_NewString( bitmap );
   wName->alignment      = UiERROR;
   wName->text           = UiERRORP;
   wName->callbackMsg    = UIW_NewString( callbackMsg );
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiTRUE;

   XtVaGetValues( w,
      XmNforeground,        &fg_color_def,
      XmNbackground,        &bg_color_def,
      NULL );

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   /** Bestimmung der zum Background passenden Shadowcolors **/
   XmGetColors( XtScreen( w ), cmp,
      bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   pm = createPixmapFromFile( w, bitmap, fg_color_def, bg_color_def );
   wName->pixmap = pm;
   /* pm == XmUNSPECIFIED_PIXMAP is allowed here to reset an existent Pixmap */
motif_bug(pm,bitmap);

   XtVaSetValues( w,
      XmNleftOffset,             x_def,
      XmNtopOffset,              y_def,
      XmNwidth,                  width_def,
      XmNheight,                 height_def,
      XmNlabelString,            labelText_def,
      XmNforeground,             fg_color_def,
      XmNbackground,             bg_color_def,
      XmNtopShadowColor,         top_shadow_def,
      XmNbottomShadowColor,      bottom_shadow_def,
      XmNarmColor,               select_suggest,
      XmNlabelType,              XmPIXMAP,
      XmNlabelPixmap,            pm,
      XmNlabelInsensitivePixmap, pm,
      NULL );

   UIW_addWNAME( wName );
   XtManageChild( w );
   return UiOK;
}

/***************************** ui_create_bitmap_label *************************/

int
ui_create_bitmap_label( char* name, int x, int y, int width, int height,
             char *bitmap, char *fg_color, char *bg_color ) {
   WNAME *wName;
   Widget w;
   int x_def, y_def;
   Dimension width_def, height_def;
   long color;
   Pixel fg_color_def, bg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;
   XmString labelText_def;
   Pixmap pm;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   if( bitmap == UiERRORP )
      return UiERROR;

   if( bitmap == UiKEEPP )
      bitmap = "";         /* no image! */

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;
   labelText_def = XmStringCreateLtoR( "", XmSTRING_DEFAULT_CHARSET );

   /*
    * Zur Zeit wird nicht unterstuetzt, dass der bitmap-Path aus den Xresourcen
    * kommt !
    */
               
   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmLabelWidgetClass, work_form,
      XmNleftAttachment, XmATTACH_FORM,
      XmNtopAttachment,  XmATTACH_FORM,
      XmNrecomputeSize,  False,
      NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiBITMAPLABEL;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UiERRORP;
   wName->bitmap         = UIW_NewString( bitmap );
   wName->alignment      = UiERROR;
   wName->text           = UiERRORP;
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiERROR;

   XtVaGetValues( w,
      XmNforeground,    &fg_color_def,
      XmNbackground,    &bg_color_def,
      NULL );

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   /** Bestimmung der zum Background passenden Shadowcolors **/
   XmGetColors( XtScreen( w ), cmp, bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   pm = createPixmapFromFile( w, bitmap, fg_color_def, bg_color_def );
   wName->pixmap = pm;
   /* pm == XmUNSPECIFIED_PIXMAP is allowed here to reset an existent Pixmap */
motif_bug(pm,bitmap);

   XtVaSetValues( w,
      XmNleftOffset,             x_def,
      XmNtopOffset,              y_def,
      XmNwidth,                  width_def,
      XmNheight,                 height_def,
      XmNlabelString,            labelText_def,
      XmNforeground,             fg_color_def,
      XmNbackground,             bg_color_def,
      XmNlabelType,              XmPIXMAP,
      XmNlabelPixmap,            pm,
      XmNlabelInsensitivePixmap, pm,
      NULL );

   UIW_addWNAME( wName );
   XtManageChild( w );
   return UiOK;
}

/**************************** ui_create_input_text ****************************/

int
ui_create_input_text( char *name, int x, int y, int width, int height,
                      char *font, char *fg_color, char *bg_color ) {
   WNAME *wName;
   Widget w;
   int n, error, x_def, y_def, width_def, height_def;
   XmFontList font_def;
   long color;
   Pixel fg_color_def, bg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;
   XFontStruct *fonty;
   Arg args[8];

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateWidget( name,
      xmTextFieldWidgetClass, work_form,
      XmNleftAttachment,    XmATTACH_FORM,
      XmNtopAttachment,     XmATTACH_FORM,
      XmNrightAttachment,   XmATTACH_OPPOSITE_FORM,
      XmNbottomAttachment,  XmATTACH_OPPOSITE_FORM,
      /*  nur relevant fuer UiSIMPLE_SESSION:  */
      XmNpaneMinimum,       50,
      XmNpaneMaximum,       50,
      NULL );
   XtAddCallback( w, XmNactivateCallback, (XtPointer)ButtonCB, NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiINPUT_TEXT;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UIW_NewString( font );
   wName->bitmap         = UiERRORP;
   wName->alignment      = UiERROR;
   wName->text           = UiERRORP;
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiTRUE;

   XtVaGetValues( w,
      XmNfontList,          &font_def,
      XmNforeground,        &fg_color_def,
      XmNbackground,        &bg_color_def,
      NULL );

   if( font != UiKEEPP ) {
      fonty = XLoadQueryFont( dpy, font );
      font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
   }

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   /** Bestimmung der zum Background passenden Shadowcolors **/
   XmGetColors( XtScreen( w ), cmp, bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   XtVaSetValues( w,
      XmNleftOffset,        x_def,
      XmNtopOffset,         y_def,
      XmNrightOffset,       -(width_def+x_def),
      XmNbottomOffset,      -(height_def+y_def),
      XmNfontList,          font_def,
      XmNforeground,        fg_color_def,
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      NULL );
   checkMargins( w );

   UIW_addWNAME( wName );
   XtManageChild( w );

   if( font != UiKEEPP )
      XmFontListFree( font_def );

   return UiOK;
}

/**************************** ui_create_output_text ***************************/

int
ui_create_output_text( char *name, int x, int y, int width, int height,
                  char *text, char *font, char *fg_color, char *bg_color ) {
   WNAME *wName;
   Widget w, h_scrollbar_child, v_scrollbar_child;
   int n, error, x_def, y_def, width_def, height_def;
   String text_def;
   char *bg;
   XmFontList font_def;
   long color;
   Pixel fg_color_def, bg_color_def, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest;
   XFontStruct *fonty;
   Arg args[8];
   XmTextPosition end_of_text;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !width || !height )
      return UiERROR;

   x_def = x;
   y_def = y;
   width_def = width;
   height_def = height;

   if( text == UiERRORP || text == UiKEEPP )
      /* Derzeit wird nicht unterstuetzt, dass der
         Text aus der X-Resource gesetzt wird ! */
      text_def = "";
   else
      text_def = text;

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   n = 0;
   XtSetArg( args[n], XmNeditable,                 False ); n++;
   XtSetArg( args[n], XmNeditMode,     XmMULTI_LINE_EDIT ); n++;
   XtSetArg( args[n], XmNcursorPositionVisible,    False ); n++;
   w = XmCreateScrolledText( work_form, name, args, n );

   /** WidgetID des ScrolledWindow fuer Positionierung und Groesse benutzt **/
   XtVaSetValues( XtParent( w ),
      XmNleftAttachment,        XmATTACH_FORM,
      XmNtopAttachment,         XmATTACH_FORM,
      XmNrightAttachment,       XmATTACH_OPPOSITE_FORM,
      XmNbottomAttachment,      XmATTACH_OPPOSITE_FORM,
      NULL );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiOUTPUT_TEXT;
   wName->x              = x_def;
   wName->y              = y_def;
   wName->width          = width_def;
   wName->height         = height_def;
   wName->fg_color       = UIW_NewString( fg_color );
   wName->bg_color       = UIW_NewString( bg_color );
   wName->font           = UIW_NewString( font );
   wName->bitmap         = UiERRORP;
   wName->alignment      = UiERROR;
   wName->text           = UIW_NewString( text_def );
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = UiERROR;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiERROR;

   XtVaGetValues( w,
      XmNfontList,          &font_def,
      XmNforeground,        &fg_color_def,
      XmNbackground,        &bg_color_def,
      XmNtopShadowColor,    &top_shadow_def,
      XmNbottomShadowColor, &bottom_shadow_def,
      NULL );

   if( font != UiKEEPP ) {
      fonty = XLoadQueryFont( dpy, font );
      font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
   }

   if( fg_color != UiKEEPP ) {
      if( ( color = getColor( fg_color ) ) != UiERROR )
         fg_color_def = color;
   }

   if( bg_color != UiKEEPP ) {
      if( ( color = getColor( bg_color ) ) != UiERROR )
         bg_color_def = color;
   }

   /** Bestimmung der zum Background passenden Shadowcolors **/
   XmGetColors( XtScreen( w ), cmp, bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   XtVaSetValues( XtParent( w ),
      XmNleftOffset,    x_def,
      XmNtopOffset,     y_def,
      XmNrightOffset,   -(width_def+x_def),
      XmNbottomOffset,  -(height_def+y_def),
      NULL );
   XtVaSetValues( w,
      XmNvalue,             text_def,
      XmNfontList,          font_def,
      XmNforeground,        fg_color_def,
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      NULL );

   XtVaGetValues( XtParent( w ),
      XmNhorizontalScrollBar, &h_scrollbar_child,
      XmNverticalScrollBar,   &v_scrollbar_child,
      XmNbackground,          &bg_color_def,
      NULL );

   if( session_bg != UiKEEPP ) {
      if( ( color = getColor( session_bg ) ) != UiERROR )
         bg_color_def = color;
   }
   XmGetColors( XtScreen( w ), cmp, bg_color_def,
      &fg_suggest, &top_shadow_def, &bottom_shadow_def, &select_suggest );

   XtVaSetValues( XtParent( w ),
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      NULL );
   XtVaSetValues( v_scrollbar_child,
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      NULL );
   XtVaSetValues( h_scrollbar_child,
      XmNbackground,        bg_color_def,
      XmNtopShadowColor,    top_shadow_def,
      XmNbottomShadowColor, bottom_shadow_def,
      NULL );

   UIW_addWNAME( wName );
   XtManageChild( w );

   /*
    *  um sicherzustellen, dass Ende des Textes sichtbar:
    */
   end_of_text = XmTextGetLastPosition( w );
   XmTextSetInsertionPosition( w, end_of_text );

   if( font != UiKEEPP )
      XmFontListFree( font_def );

   return UiOK;
}

/**************************** ui_create_pulldown_menu *************************/

int
ui_create_pulldown_menu( char* name, char *labelText, int mnemonic ) {
   WNAME *wName;
   Widget w, cascade;
   char *menu_name, *font, *fg, *bg;
   XmString labelText_def;
   int mnemonic_def;
   XmFontList font_def;
   XFontStruct *fonty;
   XmFontList menu_font_def;
   long color;
   Pixel fg_color, bg_color, top_shadow_suggest, bottom_shadow_suggest,
      fg_suggest, select_suggest;
   int n;
   Arg args[4];
   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   menu_name = (char*)XtMalloc( strlen( name ) + 6 );
   sprintf( menu_name, "%s_menu", name );

   n = 0;
   XtSetArg( args[n], XmNallowShellResize,  True ); n++;
   w = XmCreateSimplePulldownMenu( menubar, menu_name, args, n );
   cascade = XtVaCreateManagedWidget( name,
      xmCascadeButtonGadgetClass, menubar,
      XmNsubMenuId,     w,
      XmNrecomputeSize, True,
      NULL );
   UiFree( menu_name );

   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = cascade;     /* relevant only here */
   wName->class          = UiPULLDOWN_MENU;
   wName->x              = UiERROR;
   wName->y              = UiERROR;
   wName->width          = UiERROR;
   wName->height         = UiERROR;
   wName->fg_color       = UiERRORP;
   wName->bg_color       = UiERRORP;
   wName->font           = UiERRORP;
   wName->bitmap         = UiERRORP;
   wName->alignment      = UiERROR;
   wName->text           = UIW_NewString( labelText );
   wName->callbackMsg    = UiERRORP;
   wName->mnemonic       = mnemonic;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiTRUE;

   XtVaGetValues( w,
      XmNforeground,        &fg_color,
      XmNbackground,        &bg_color,
      NULL );

   if( menu_bg != UiKEEPP ) {
      if( ( color = getColor( menu_bg ) ) != UiERROR )
         bg_color = color;
   }
   XmGetColors( XtScreen(SystemDialogs), cmp, bg_color,
      &fg_suggest, &top_shadow_suggest, &bottom_shadow_suggest,
      &select_suggest );

   if( menu_fg != UiKEEPP ) {
      if( ( color = getColor( menu_fg ) ) != UiERROR )
         fg_color = color;
   }

   XtVaSetValues( w,
      XmNforeground,        fg_color,
      XmNbackground,        bg_color,
      XmNtopShadowColor,    top_shadow_suggest,
      XmNbottomShadowColor, bottom_shadow_suggest,
      NULL );

   XtVaGetValues( cascade,
      XmNlabelString,  &labelText_def,
      XmNmnemonic,     &mnemonic_def,
      XmNfontList,     &menu_font_def,
      NULL );

   wName->mnemonic_res = mnemonic_def;
   wName->text_res     = labelText_def;

   if( labelText != UiKEEPP )
      labelText_def = XmStringCreateLtoR( labelText,
         XmSTRING_DEFAULT_CHARSET );

   if( mnemonic != UiKEEP )
      mnemonic_def = mnemonic;

   if( menu_font != UiKEEPP ) {
      font = UIW_NewString( menu_font );
      fonty = XLoadQueryFont( dpy, menu_font );
      menu_font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
      UiFree( font );
   }

   XtVaSetValues( cascade,
      XmNlabelString,  labelText_def,
      XmNmnemonic,     mnemonic_def,
      XmNfontList,     menu_font_def,
      NULL );
   checkMargins( cascade );

   UIW_addWNAME( wName );
   XtManageChild( w );

   if( labelText != UiKEEPP )
      XmStringFree( labelText_def );

   if( menu_font != UiKEEPP )
      XmFontListFree( menu_font_def );

   ui_set_menu_font( menu_font ); /* um sicherzustellen, dass die Menubar
                                     vernuenftig geresized wird */

   return UiOK;
}

/**************************** ui_create_menu_entry ****************************/

int
ui_create_menu_entry( char* name, char *pd_menu_name,
                      char *labelText, int mnemonic, char *callbackMsg ) {
   WNAME *wName, *existent;
   Widget w, cascade;
   char *font, *fg, *bg;
   XmString labelText_def;
   int mnemonic_def;
   XmFontList font_def;
   XFontStruct *fonty;
   XmFontList menu_font_def;
   Pixel fg_color, bg_color, top_shadow_suggest, bottom_shadow_suggest,
      fg_suggest, select_suggest;

   if( UIW_getWNAMEofName( name ) )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName( pd_menu_name )) )
      return UiERROR;

   if( existent->class != UiPULLDOWN_MENU )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   w = XtVaCreateManagedWidget( name, 
      xmPushButtonGadgetClass, existent->w,
      XmNrecomputeSize, True,
      NULL );
   XtAddCallback( w, XmNactivateCallback, (XtPointer)ButtonCB, NULL );
      
   wName                 = (WNAME*)XtMalloc( sizeof(WNAME) );
   wName->name           = UIW_NewString( name );
   wName->w              = w;
   wName->cascade_button = UiNIL;     /* relevant only for PulldownMenu */
   wName->class          = UiMENU_ENTRY;
   wName->x              = UiERROR;
   wName->y              = UiERROR;
   wName->width          = UiERROR;
   wName->height         = UiERROR;
   wName->fg_color       = UiERRORP;
   wName->bg_color       = UiERRORP;
   wName->font           = UiERRORP;
   wName->bitmap         = UiERRORP;
   wName->alignment      = UiERROR;
   wName->text           = UIW_NewString( labelText );
   wName->callbackMsg    = UIW_NewString( callbackMsg );
   wName->mnemonic       = mnemonic;
   wName->visibility     = UiTRUE;
   wName->sensitivity    = UiTRUE;

   XtVaGetValues( w,
      XmNlabelString,  &labelText_def,
      XmNmnemonic,     &mnemonic_def,
      XmNfontList,     &menu_font_def,
      NULL );

   wName->mnemonic_res = mnemonic_def;
   wName->text_res     = labelText_def;

   if( labelText != UiKEEPP )
      labelText_def = XmStringCreateLtoR( labelText,
         XmSTRING_DEFAULT_CHARSET );

   if( mnemonic != UiKEEP )
      mnemonic_def = mnemonic;

   if( menu_font != UiKEEPP ) {
      font = UIW_NewString( menu_font );
      fonty = XLoadQueryFont( dpy, menu_font );
      menu_font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
      UiFree( font );
   }

   XtVaSetValues( w,
      XmNlabelString,   labelText_def,
      XmNmnemonic,      mnemonic_def,
      XmNfontList,      menu_font_def,
      NULL );
   checkMargins( w );

   UIW_addWNAME( wName );
   XtManageChild( w );

   if( labelText != UiKEEPP )
      XmStringFree( labelText_def );

   if( menu_font != UiKEEPP )
      XmFontListFree( menu_font_def );

   return UiOK;
}

/******************************************************************************/
/*                                                                            */
/*                        Resource Set Operations                             */
/*                                                                            */
/******************************************************************************/

/****************************** ui_set_session_bg *****************************/

int
ui_set_session_bg( char *value ) {
   long color;
   Pixel bg_color, fg_suggest, top_shadow_suggest, bottom_shadow_suggest,
      select_suggest;
   Widget h_scrollbar_child, v_scrollbar_child, clipwindow_child,
      grid_toggle_button;
   WNAME *actual_widget, *next_widget, *output, *input;

   if( value == UiERRORP )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   if( value == UiKEEPP ) {
      /** Zuruecksetzen auf Xresource-Werte **/
      bg_color = session_bg_def;
      top_shadow_suggest = session_ts_def;
      bottom_shadow_suggest = session_bs_def;
      fg_suggest = session_fg_def;
      UiFree( session_bg );
      session_bg = UiKEEPP;
   } else {
      /** Versuch, gewuenschte Farbe zu setzen **/
      UiFree( session_bg );
      session_bg = UIW_NewString( value );
      if( ( color = getColor( value ) ) == UiERROR ) 
         return UiOK;
      bg_color = color;
      XmGetColors( XtScreen(SystemDialogs), cmp, bg_color,
         &fg_suggest, &top_shadow_suggest,
         &bottom_shadow_suggest, &select_suggest );
   }

   if( sessionCreated != UiSIMPLE_SESSION ) {
      XtVaGetValues( scrolled_win,
         XmNhorizontalScrollBar, &h_scrollbar_child,
         XmNverticalScrollBar,   &v_scrollbar_child,
         XmNclipWindow,          &clipwindow_child,
         NULL );
   
      XtVaSetValues( scrolled_win,
         XmNbackground,        bg_color,
         XmNtopShadowColor,    top_shadow_suggest,
         XmNbottomShadowColor, bottom_shadow_suggest,
         NULL );
      XtVaSetValues( v_scrollbar_child,
         XmNbackground,        bg_color,
         XmNtopShadowColor,    top_shadow_suggest,
         XmNbottomShadowColor, bottom_shadow_suggest,
         NULL );
      XtVaSetValues( h_scrollbar_child,
         XmNbackground,        bg_color,
         XmNtopShadowColor,    top_shadow_suggest,
         XmNbottomShadowColor, bottom_shadow_suggest,
         NULL );
      XtVaSetValues( clipwindow_child,
         XmNbackground, bg_color,
         NULL );
   }

   destroyPixmap( SystemDialogs, gridOn  );
   destroyPixmap( SystemDialogs, gridOff );

   gridOn  = createPixmapFromFile( SystemDialogs,
                GRID50, fg_suggest, bg_color );
   gridOff = createPixmapFromFile( SystemDialogs,
                EMPTY50, fg_suggest, bg_color );
   if( gridVisible )
      XtVaSetValues( work_form, XmNbackgroundPixmap, gridOn,  NULL);
   else
      XtVaSetValues( work_form, XmNbackgroundPixmap, gridOff, NULL);

   /*
    * Farbe der Scrollbars aller OutputText-Widgets
    * dem Hintergrund angleichen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      if( actual_widget->class == UiOUTPUT_TEXT ) {
         XtVaGetValues( XtParent( actual_widget->w ),
            XmNhorizontalScrollBar, &h_scrollbar_child,
            XmNverticalScrollBar,   &v_scrollbar_child,
            NULL );
      
         XtVaSetValues( XtParent( actual_widget->w ),
            XmNbackground,        bg_color,
            XmNtopShadowColor,    top_shadow_suggest,
            XmNbottomShadowColor, bottom_shadow_suggest,
            NULL );
         XtVaSetValues( v_scrollbar_child,
            XmNbackground,        bg_color,
            XmNtopShadowColor,    top_shadow_suggest,
            XmNbottomShadowColor, bottom_shadow_suggest,
            NULL );
         XtVaSetValues( h_scrollbar_child,
            XmNbackground,        bg_color,
            XmNtopShadowColor,    top_shadow_suggest,
            XmNbottomShadowColor, bottom_shadow_suggest,
            NULL );
      }
      next_widget = actual_widget->next;
   }

   /*
    *  Um zu vermeiden, dass ein Rahmen der alten Hintergrundfarbe
    *  um die Widgets herum bestehen bleibt:
    */
   if( sessionCreated == UiSIMPLE_SESSION ) {
      output = UIW_getWNAMEofName( UiSIMPLE_SESSION_OUTPUT );
      input  = UIW_getWNAMEofName( UiSIMPLE_SESSION_INPUT  );
      XtUnmanageChild( output->w );
      XtUnmanageChild( input->w  );
      XtManageChild(   output->w );
      XtManageChild(   input->w  );
      XmProcessTraversal( input->w, XmTRAVERSE_CURRENT );
   } else {
      XtUnmanageChild( work_form );
      XtManageChild( work_form );
   }

   return UiOK;
}

/****************************** ui_set_menu_bg ********************************/

int
ui_set_menu_bg( char *value ) {
   long color;
   Pixel bg_color, top_shadow_suggest, bottom_shadow_suggest,
      fg_suggest, select_suggest;
   WNAME *actual_widget, *next_widget;

   if( value == UiERRORP )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   if( value == UiKEEPP ) {
      /** Zuruecksetzen auf Xresource-Werte **/
      bg_color = menu_bg_def;
      top_shadow_suggest = menu_ts_def;
      bottom_shadow_suggest = menu_bs_def;
      UiFree( menu_bg );
      menu_bg = UiKEEPP;
   } else {
      /** Versuch, gewuenschte Farbe zu setzen **/
      UiFree( menu_bg );
      menu_bg = UIW_NewString( value );
      if( ( color = getColor( value ) ) == UiERROR )
         return UiOK;
      bg_color = color;
      XmGetColors( XtScreen(SystemDialogs), cmp, bg_color,
                   &fg_suggest, &top_shadow_suggest, &bottom_shadow_suggest,
                   &select_suggest );
   }

   XtVaSetValues( menubar,
      XmNbackground,        bg_color,
      XmNtopShadowColor,    top_shadow_suggest,
      XmNbottomShadowColor, bottom_shadow_suggest,
      NULL );
   XtVaSetValues( session_menu,
      XmNbackground,        bg_color,
      XmNtopShadowColor,    top_shadow_suggest,
      XmNbottomShadowColor, bottom_shadow_suggest,
      NULL );
   XtVaSetValues( help_menu,
      XmNbackground,        bg_color,
      XmNtopShadowColor,    top_shadow_suggest,
      XmNbottomShadowColor, bottom_shadow_suggest,
      NULL );

   /*
    *  Hintergrund-Farbe aller in der wname-Liste enthaltenen
    *  PulldownMenus angleichen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      if( actual_widget->class == UiPULLDOWN_MENU )
         XtVaSetValues( actual_widget->w,
            XmNbackground,        bg_color,
            XmNtopShadowColor,    top_shadow_suggest,
            XmNbottomShadowColor, bottom_shadow_suggest,
            NULL );
      next_widget = actual_widget->next;
   }

   return UiOK;
}

/****************************** ui_set_menu_fg ********************************/

int
ui_set_menu_fg( char *value ) {
   long color;
   Pixel fg_color;
   WNAME *actual_widget, *next_widget;

   if( value == UiERRORP )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   if( value == UiKEEPP ) {
      /** Zuruecksetzen auf Xresource-Werte **/
      fg_color = menu_fg_def;
      UiFree( menu_fg );
      menu_fg = UiKEEPP;
   } else {
      /** Versuch, gewuenschte Farbe zu setzen **/
      UiFree( menu_fg );
      menu_fg = UIW_NewString( value );
      if( ( color = getColor( value ) ) == UiERROR )
         return UiOK;
      fg_color = color;
   }
   XtVaSetValues( menubar,      XmNforeground, fg_color, NULL );
   XtVaSetValues( session_menu, XmNforeground, fg_color, NULL );
   XtVaSetValues( help_menu,    XmNforeground, fg_color, NULL );

   /*
    *  Vordergrund-Farbe aller in der wname-Liste enthaltenen
    *  PulldownMenus angleichen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      if( actual_widget->class == UiPULLDOWN_MENU )
         XtVaSetValues( actual_widget->w,
            XmNforeground, fg_color,
            NULL );
      next_widget = actual_widget->next;
   }
   return UiOK;
}

/****************************** ui_set_menu_font ******************************/

int
ui_set_menu_font( char *value ) {
   XmFontList font_def;
   XFontStruct *fonty;
   WNAME *actual_widget, *next_widget;

   if( value == UiERRORP )
      return UiERROR;

   if( sessionCreated == UiNO_SESSION )
      createSession( UiSESSION );

   if( value == UiKEEPP ) {
      /** Zuruecksetzen auf Xresource-Werte **/
      font_def = menu_font_def;
      UiFree( menu_font );
      menu_font = UiKEEPP;
   } else {
      /** Versuch, gewuenschten Font zu setzen **/
      UiFree( menu_font );
      menu_font = UIW_NewString( value );
      fonty = XLoadQueryFont( dpy, value );
      font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );
   }

   /*
    *  Problem: Die Session-PulldownMenuEntries werden nach Veraenderung
    *           des Fonts nicht vernuenftig 'resized' (verursacht durch
    *           Separator und ToggleButton).
    *  Ausweg:  Separator und GridButton werden voruebergehend 'destroyed'.
    */
   XtDestroyWidget( sep );
   XtDestroyWidget( grid_button );

   XtVaSetValues( session_menu_button, XmNfontList, font_def, NULL );
   XtVaSetValues( break_button,        XmNfontList, font_def, NULL );
   XtVaSetValues( rerun_button,        XmNfontList, font_def, NULL );
   XtVaSetValues( end_button,          XmNfontList, font_def, NULL );
   XtVaSetValues( help_menu_button,    XmNfontList, font_def, NULL );
   XtVaSetValues( on_menu_button,      XmNfontList, font_def, NULL );

   sep = XtVaCreateManagedWidget( "sep",
      xmSeparatorGadgetClass, session_menu,
      NULL );
   if( gridVisible ) {
      grid_button = XtVaCreateManagedWidget( "grid_off_button",
         xmPushButtonGadgetClass, session_menu,
         XmNfontList, font_def,
         NULL );
   } else {
      grid_button = XtVaCreateManagedWidget( "grid_on_button",
         xmPushButtonGadgetClass, session_menu,
         XmNfontList, font_def,
         NULL );
   }
   XtAddCallback( grid_button, XmNactivateCallback, (XtPointer)SessionCB, NULL );

   /*
    *  Font aller in der wname-Liste enthaltenen
    *  PulldownMenus und MenuEntries angleichen:
    */
   next_widget = UIW_getRootWNAME();
   while( actual_widget = next_widget ) {
      if( actual_widget->class == UiPULLDOWN_MENU )
         XtVaSetValues( actual_widget->cascade_button,
            XmNfontList, font_def,
            NULL );
      if( actual_widget->class == UiMENU_ENTRY )
         XtVaSetValues( actual_widget->w,
            XmNfontList, font_def,
            NULL );
      next_widget = actual_widget->next;
   }

   if( value != UiKEEPP )
      XmFontListFree( font_def );

   resizeWidget( session );

   return UiOK;
}

/*************************** ui_set_visibility ********************************/

int
ui_set_visibility( char *name, int value ) {
   WNAME *existent;
   Widget w;
   int output_text;

   if( !strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ||
       !strcmp( name, UiSIMPLE_SESSION_INPUT  )  )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName( name )) )
      return UiERROR;

   output_text = 0;
   switch( existent->class ) {
      case UiPUSHBUTTON:
      case UiLABEL:
      case UiBITMAPBUTTON:
      case UiBITMAPLABEL:
      case UiSEPARATOR:
      case UiINPUT_TEXT:
      case UiMENU_ENTRY:
         w = existent->w;
         break;

      case UiPULLDOWN_MENU:
         w = existent->cascade_button;
         break;

      case UiOUTPUT_TEXT:
         w = existent->w;
         output_text = 1;
         break;

      default:
         return UiERROR;
   }

   switch( value ) {
      case UiKEEP:
         return UiOK;
      case UiTRUE:
         XtManageChild( w );
         if( output_text )
            XtManageChild( XtParent( w ) );
         break;
      case UiNIL:
         XtUnmanageChild( w );
         if( output_text )
            XtUnmanageChild( XtParent( w ) );
         break;
      default:
         return UiERROR;
   }

   existent->visibility = value;
   return UiOK;
}

/*************************** ui_set_sensitivity *******************************/

int
ui_set_sensitivity( char *name, int value ) {
   WNAME *existent;
   Widget w;

   if( !(existent = UIW_getWNAMEofName( name )) )
      return UiERROR;

   switch( existent->class ) {
      case UiLABEL:
      case UiBITMAPLABEL:
      case UiSEPARATOR:
      case UiOUTPUT_TEXT:
         return UiERROR;

      case UiPUSHBUTTON:
      case UiBITMAPBUTTON:
      case UiINPUT_TEXT:
      case UiMENU_ENTRY:
         w = existent->w;
         break;

      case UiPULLDOWN_MENU:
         w = existent->cascade_button;
         break;

      default:
         return UiERROR;
   }

   switch( value ) {
      case UiKEEP:
         return UiOK;
      case UiTRUE:
         XtSetSensitive( w, True );
         break;
      case UiNIL:
         XtSetSensitive( w, False );
         break;
      default:
         return UiERROR;
   }

   existent->sensitivity = value;
   return UiOK;
}

/**************************** ui_set_foreground *******************************/

int
ui_set_foreground( char *name, char *value ) {
   WNAME *existent;
   Widget w;
   Pixel fg_color, top_shadow_def, bottom_shadow_def,
      fg_suggest, select_suggest, bg_color;
   Pixmap pm;
   char *bitmap;

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class == UiPULLDOWN_MENU ||
       existent->class == UiMENU_ENTRY )
      return UiERROR;

   UIW_FreeString( existent->fg_color );
   existent->fg_color = UIW_NewString( value );

   if( value == UiKEEPP )
      return reset_resource( existent, UiFG_COLOR );

   w = existent->w;

   if( UiERROR == (fg_color = getColor( value )) ) {
      /*
       * Falls kein geeigneter Pixelwert zugeordnet werden kann,
       * soll der alte Wert sichtbar bleiben!
       * Damit ist hier alles getan!
       * LISP sieht nur: Erfolgreich abgearbeitet!
       */
      return UiOK;
   }

   /*
    * Um moegliche Fehler zu verhindern, die bei InputText-Widgets aufgetreten
    * sind, wenn sie ihre eigenen Resourcen aendern wollen (X Error of failed
    * request:  BadDrawable), wird das entsprechende Widget waehrend der
    * Aenderung kurzzeitig ge-unmanaged:
    */
   if( existent->class == UiINPUT_TEXT )
      XtUnmanageChild( w );

   XmGetColors( XtScreen( w ), cmp, fg_color, &fg_suggest,
                &top_shadow_def, &bottom_shadow_def, &select_suggest );

   switch( existent->class ) {

      case UiSEPARATOR:
         XtVaSetValues( w,
            XmNtopShadowColor,    top_shadow_def,
            XmNbottomShadowColor, bottom_shadow_def,
            NULL );
         break;

      case UiBITMAPBUTTON:
      case UiBITMAPLABEL:
         XtVaGetValues( w, XmNbackground, &bg_color, NULL );
         bitmap = UIW_NewString( existent->bitmap );
         pm = createPixmapFromFile( w, bitmap, fg_color, bg_color );
         XtVaSetValues( w,
            XmNforeground,             fg_color,
            XmNlabelPixmap,            pm,
            XmNlabelInsensitivePixmap, pm,
            NULL );
         UIW_FreeString( bitmap );
         break;

      case UiPUSHBUTTON:
      case UiLABEL:
      case UiINPUT_TEXT:
      case UiOUTPUT_TEXT:
         XtVaSetValues( w, XmNforeground, fg_color, NULL );
         break;

      default:
         return UiERROR;
   }

   if( existent->class == UiINPUT_TEXT ) {
      if( existent->visibility == UiTRUE )
         XtManageChild( w );
      /*
       * Um sicherzustellen, dass ein seine eigenen Resourcen aendernder
       * InputText auch den Focus behaelt:
       */ 
      if( buttonCB_caused_by == w )
         XmProcessTraversal( w, XmTRAVERSE_CURRENT );
   }

   return UiOK;
}

/**************************** ui_set_background *******************************/

int
ui_set_background( char *name, char *value ) {
   WNAME *existent;
   Widget w;
   Pixel bg_color, fg_suggest, top_shadow_suggest, bottom_shadow_suggest,
      select_suggest, fg_color;
   Pixmap pm;
   char *bitmap;

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class == UiPULLDOWN_MENU ||
       existent->class == UiMENU_ENTRY    ||
       existent->class == UiSEPARATOR )
      return UiERROR;

   UIW_FreeString( existent->bg_color );
   existent->bg_color = UIW_NewString( value );

   if( value == UiKEEPP )
      return reset_resource( existent, UiBG_COLOR );

   w = existent->w;

   if( UiERROR == (bg_color = getColor( value )) ) {
      /*
       * Falls kein geeigneter Pixelwert zugeordnet werden kann,
       * soll der alte Wert sichtbar bleiben!
       * Damit ist hier alles getan!
       * LISP sieht nur: Erfolgreich abgearbeitet!
       */
      return UiOK;
   }

   /*
    * Um moegliche Fehler zu verhindern, die bei InputText-Widgets aufgetreten
    * sind, wenn sie ihre eigenen Resourcen aendern wollen (X Error of failed
    * request:  BadDrawable), wird das entsprechende Widget waehrend der
    * Aenderung kurzzeitig ge-unmanaged:
    */
   if( existent->class == UiINPUT_TEXT )
      XtUnmanageChild( w );

   XmGetColors( XtScreen(SystemDialogs), cmp, bg_color,
                &fg_suggest, &top_shadow_suggest,
                &bottom_shadow_suggest, &select_suggest );

   switch( existent->class ) {

      case UiBITMAPBUTTON:
      case UiBITMAPLABEL:
         XtVaGetValues( w, XmNforeground, &fg_color, NULL );
         bitmap = UIW_NewString( existent->bitmap );
         pm = createPixmapFromFile( w, bitmap, fg_color, bg_color );
         XtVaSetValues( w,
            XmNbackground,             bg_color,
            XmNtopShadowColor,         top_shadow_suggest,
            XmNbottomShadowColor,      bottom_shadow_suggest,
            XmNarmColor,               select_suggest,
            XmNlabelPixmap,            pm,
            XmNlabelInsensitivePixmap, pm,
            NULL );
         UIW_FreeString( bitmap );
         break;

      case UiPUSHBUTTON:
      case UiLABEL:
      case UiINPUT_TEXT:
      case UiOUTPUT_TEXT:
         XtVaSetValues( w,
            XmNbackground,        bg_color,
            XmNtopShadowColor,    top_shadow_suggest,
            XmNbottomShadowColor, bottom_shadow_suggest,
            XmNarmColor,          select_suggest,
            NULL );
         break;

      default:
         return UiERROR;
   }

   if( existent->class == UiINPUT_TEXT ) {
      if( existent->visibility == UiTRUE )
         XtManageChild( w );
      /*
       * Um sicherzustellen, dass ein seine eigenen Resourcen aendernder
       * InputText auch den Focus behaelt:
       */ 
      if( buttonCB_caused_by == w )
         XmProcessTraversal( w, XmTRAVERSE_CURRENT );
   }

   return UiOK;
}

/*************************** ui_set_font **************************************/

int
ui_set_font( char *name, char *value ) {
   WNAME *existent;
   Widget w;
   XmFontList font_def;
   XFontStruct *fonty;
   char *text;

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class == UiSEPARATOR     ||
       existent->class == UiBITMAPBUTTON  ||
       existent->class == UiBITMAPLABEL   ||
       existent->class == UiPULLDOWN_MENU ||
       existent->class == UiMENU_ENTRY    )
      return UiERROR;

   if( value == UiKEEPP )
      return reset_resource( existent, UiFONT );

   w = existent->w;

   /*
    * Um moegliche Fehler zu verhindern, die bei InputText-Widgets aufgetreten
    * sind, wenn sie ihre eigenen Resourcen aendern wollen (X Error of failed
    * request:  BadDrawable), wird das entsprechende Widget waehrend der
    * Aenderung kurzzeitig ge-unmanaged:
    */
   if( existent->class == UiINPUT_TEXT )
      XtUnmanageChild( w );

   fonty = XLoadQueryFont( dpy, value );  /* wenn Font nicht vorhanden, wird */
                                          /* automatisch 'fixed' eingestellt */
   font_def = XmFontListCreate( fonty, XmSTRING_DEFAULT_CHARSET );

   switch( existent->class ) {

      case UiPUSHBUTTON:
      case UiLABEL:
      case UiINPUT_TEXT:
      case UiOUTPUT_TEXT:
         XtVaSetValues( w, XmNfontList, font_def, NULL );
         break;

      default:
         return UiERROR;
   }

   XmFontListFree( font_def );

   UIW_FreeString( existent->font );
   existent->font = UIW_NewString( value );

   if( existent->class == UiINPUT_TEXT ) {
      if( existent->visibility == UiTRUE )
         XtManageChild( w );
      /*
       * Um sicherzustellen, dass ein seine eigenen Resourcen aendernder
       * InputText auch den Focus behaelt:
       */ 
      if( buttonCB_caused_by == w )
         XmProcessTraversal( w, XmTRAVERSE_CURRENT );
   }

   return UiOK;
}

/*************************** ui_set_bitmap ************************************/

int
ui_set_bitmap( char *name, char *value ) {
   WNAME *existent;
   Widget w;
   Pixel fg_color_def, bg_color_def;
   Pixmap pm;

   /*
    * To Reset a bitmap on a graphic-label
    * LISP sets value to "" or NIL
    */

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class != UiBITMAPBUTTON &&
       existent->class != UiBITMAPLABEL )
      return UiERROR;

   if( value == UiKEEPP )
      value = "";

   w = existent->w;

   XtVaGetValues( w, XmNforeground, &fg_color_def,
                     XmNbackground, &bg_color_def, NULL );

   /* HG: 27.11.95: 1st destroy old 2nd create new! */
   destroyPixmap( w, existent->pixmap );
   pm = createPixmapFromFile( w, value, fg_color_def, bg_color_def );
   existent->pixmap = pm;
   /* pm == XmUNSPECIFIED_PIXMAP is allowed here to reset an existent Pixmap */
motif_bug(pm,value);

   XtVaSetValues( w, XmNlabelPixmap,            pm,
                     XmNlabelInsensitivePixmap, pm,
                     NULL );

   resizeWidget( w );

   UIW_FreeString( existent->bitmap );
   existent->bitmap = UIW_NewString( value );

   return UiOK;
}

/**************************** ui_set_geometry *********************************/

int
ui_set_geometry( char* name, int x, int y, int width, int height ) {
   WNAME *existent;
   Widget w;
   unsigned char orientation;

   if( sessionCreated == UiSIMPLE_SESSION )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName( name )) )
      return UiERROR;

   if( x == UiKEEP && y == UiKEEP && width == UiKEEP && height == UiKEEP )
      return UiOK;    /* --> LISP alias ui-widget-p */

   if( existent->class == UiPULLDOWN_MENU || existent->class == UiMENU_ENTRY )
      return UiERROR;    /* --> they cannot be changed !!! */

   w = (existent->class == UiOUTPUT_TEXT) ?
       XtParent( existent->w ) : existent->w;

   /*
    * Um moegliche Fehler zu verhindern, die bei InputText-Widgets aufgetreten
    * sind, wenn sie ihre eigenen Resourcen aendern wollen (X Error of failed
    * request:  BadDrawable), wird das entsprechende Widget waehrend der
    * Aenderung kurzzeitig ge-unmanaged:
    */
   if( existent->class == UiINPUT_TEXT )
      XtUnmanageChild( w );

   switch( existent->class ) {

      case UiOUTPUT_TEXT:
      case UiINPUT_TEXT:
         if( x != UiKEEP ) {
            XtVaSetValues( w,
               XmNleftOffset,   x,
               XmNrightOffset,  -(existent->width + x),
               NULL );
            existent->x = x;
         }
         if( y != UiKEEP ) {
            XtVaSetValues( w,
               XmNtopOffset,    y,
               XmNbottomOffset, -(existent->height + y),
               NULL );
            existent->y = y;
         }
         if( width != UiKEEP ) {
            XtVaSetValues( w,
               XmNrightOffset,  -(width + existent->x),
               NULL );
            existent->width = width;
         }
         if( height != UiKEEP ) {
            XtVaSetValues( w,
               XmNbottomOffset, -(height + existent->y),
               NULL );
            existent->height = height;
         }
         break;

      case UiSEPARATOR:
         if( x != UiKEEP ) {
            XtVaSetValues( w, XmNleftOffset, x, NULL );
            existent->x = x;
         }
         if( y != UiKEEP ) {
            XtVaSetValues( w, XmNtopOffset,  y, NULL );
            existent->y = y;
         }
         if( !width && !height )
            return UiERROR;
         if( width == UiKEEP && height != UiKEEP ) {
            if( ( !height && !existent->width ) ||
                ( height  &&  existent->width ) )
               return UiERROR;
            XtVaSetValues( w, XmNheight, height, NULL );
            existent->height = height;
         }
         if( height == UiKEEP && width != UiKEEP ) {
            if( ( !width && !existent->height ) ||
                ( width  &&  existent->height ) )
               return UiERROR;
            XtVaSetValues( w, XmNwidth,  width,  NULL );
            existent->width = width;
         }
         if( width != UiKEEP && height != UiKEEP ) {
            if( width ) {
               orientation = XmHORIZONTAL;
               height = 0;
            } else {
               orientation = XmVERTICAL;
            }
            XtVaSetValues( w, 
               XmNorientation, orientation,
               XmNwidth,       width,
               XmNheight,      height,
               NULL );
            existent->width  = width;
            existent->height = height;
         }
         break;

      case UiPUSHBUTTON:
      case UiLABEL:
      case UiBITMAPBUTTON:
      case UiBITMAPLABEL:
         if( x != UiKEEP ) {
            XtVaSetValues( w, XmNleftOffset, x,      NULL );
            existent->x = x;
         }
         if( y != UiKEEP ) {
            XtVaSetValues( w, XmNtopOffset,  y,      NULL );
            existent->y = y;
         }
         if( width != UiKEEP ) {
            XtVaSetValues( w, XmNwidth,      width,  NULL );
            existent->width = width;
         }
         if( height != UiKEEP ) {
            XtVaSetValues( w, XmNheight,     height, NULL );
            existent->height = height;
         }
         break;

      default:
         return UiERROR;
   }

   if( existent->class == UiINPUT_TEXT ) {
      if( existent->visibility == UiTRUE )
         XtManageChild( w );
      /*
       * Um sicherzustellen, dass ein seine eigenen Resourcen aendernder
       * InputText auch den Focus behaelt:
       */ 
      if( buttonCB_caused_by == w )
         XmProcessTraversal( w, XmTRAVERSE_CURRENT );
   }

   return UiOK;
}

/**************************** ui_set_alignment ********************************/

int
ui_set_alignment( char *name, int value ) {
   WNAME *existent;
   Widget w;
   unsigned char align;

   if( value == UiERROR )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName( name )) )
      return UiERROR;

   if( existent->class != UiPUSHBUTTON &&
       existent->class != UiLABEL       )
      return UiERROR;

   switch( value ) {
      case UiKEEP:
         return reset_resource( existent, UiALIGNMENT );
      case UiLEFT:
         align = XmALIGNMENT_BEGINNING;
         break;
      case UiCENTER:
         align = XmALIGNMENT_CENTER;
         break;
      case UiRIGHT:
         align = XmALIGNMENT_END;
         break;
      default:
         return UiERROR;
   }

   w = existent->w;

   XtVaSetValues( w, XmNalignment, align, NULL );
   existent->alignment = value;

   return UiOK;
}

/****************************** ui_set_text ***********************************/

int
ui_set_text( char *name, char *value ) {
   WNAME *existent;
   Widget w, cascade;
   XmString value_xm;
   XmTextPosition end_of_text;

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class == UiOUTPUT_TEXT && value == UiKEEPP )
      return UiERROR;

   if( existent->class == UiSEPARATOR    ||
       existent->class == UiBITMAPBUTTON ||
       existent->class == UiBITMAPLABEL  ||
       existent->class == UiINPUT_TEXT    )
      return UiERROR;

   UIW_FreeString( existent->text );
   existent->text = UIW_NewString( value );

   w = existent->w;

   switch( existent->class ) {

      case UiPUSHBUTTON:
      case UiLABEL:
         if( value == UiKEEPP )
            return reset_resource( existent, UiTEXT );
         value_xm = XmStringCreateLtoR( value, XmSTRING_DEFAULT_CHARSET );
         XtVaSetValues( w, XmNlabelString, value_xm, NULL );
         XmStringFree( value_xm );
         break;

      case UiOUTPUT_TEXT:
         XtVaSetValues( w, XmNvalue, (String) value, NULL );
         /*
          *  um sicherzustellen, dass Ende des Textes sichtbar:
          */
         end_of_text = XmTextGetLastPosition( w );
         XmTextSetInsertionPosition( w, end_of_text );
         break;

      case UiPULLDOWN_MENU:
         cascade = existent->cascade_button;
         if( value == UiKEEPP ) {
            XtVaSetValues( cascade, XmNlabelString, existent->text_res, NULL );
         } else {
            value_xm = XmStringCreateLtoR( value, XmSTRING_DEFAULT_CHARSET );
            XtVaSetValues( cascade, XmNlabelString, value_xm, NULL );
            XmStringFree( value_xm );
         }
         resizeWidget( session );
         break;

      case UiMENU_ENTRY:
         if( value == UiKEEPP ) {
            XtVaSetValues( w, XmNlabelString, existent->text_res, NULL );
         } else {
            value_xm = XmStringCreateLtoR( value, XmSTRING_DEFAULT_CHARSET );
            XtVaSetValues( w, XmNlabelString, value_xm, NULL );
            XmStringFree( value_xm );
         }
         break;

      default:
         return UiERROR;
   }

   return UiOK;
}

/****************************** ui_add_text ***********************************/

int
ui_add_text( char *name, char *value ) {
   WNAME *existent;
   Widget w;
   char *newTextPtr;
   XmTextPosition end_of_text;

   if( value == UiERRORP || value == UiKEEPP )
      return UiERROR;
   
   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class != UiOUTPUT_TEXT )
      return UiERROR;

   w = existent->w;

   if( existent->text == UiERRORP )
      existent->text = "";
   else if( existent->text == UiKEEPP ) {
      /*
       * spaeter Wert ermitteln vom Xserver, weil eine Xresource ex. koennte,
       * die hier einen Wert vorbelegt hat, von dem aber das Programm nichts weiss
       */
      existent->text = "";
   }
   newTextPtr = UIW_NewStringsCat( existent->text, value );
   UIW_FreeString( existent->text );
   existent->text = newTextPtr;

   end_of_text = XmTextGetLastPosition( w );
   XmTextInsert( w, end_of_text, value );

   /*
    *  um sicherzustellen, dass Ende des Textes sichtbar:
    */
   end_of_text = XmTextGetLastPosition( w );
   XmTextSetInsertionPosition( w, end_of_text );

   return UiOK;
}

/****************************** ui_set_callback *******************************/

int
ui_set_callback( char *name, char *value ) {
   WNAME *existent;

   if( value == UiERRORP )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   if( existent->class != UiPUSHBUTTON   &&
       existent->class != UiBITMAPBUTTON &&
       existent->class != UiMENU_ENTRY    )
      return UiERROR;

   UIW_FreeString( existent->callbackMsg );
   if( value == UiKEEPP )
      existent->callbackMsg = UiKEEPP;
   else
      existent->callbackMsg = UIW_NewString( value );

   return UiOK;
}

/****************************** ui_set_mnemonic *******************************/

int
ui_set_mnemonic( char *name, int value ) {
   WNAME *existent;
   Widget w;

   if( value == UiERROR )
      return UiERROR;

   if( !(existent = UIW_getWNAMEofName(name)) )
      return UiERROR;

   switch( existent->class ) {
      case UiPULLDOWN_MENU:
         w = existent->cascade_button;
         break;
      case UiMENU_ENTRY:
         w = existent->w;
         break;
      default:
         return UiERROR;
   }

   existent->mnemonic = value;

   if( value == UiKEEP )
      value = existent->mnemonic_res;

   XtVaSetValues( w, XmNmnemonic, value, NULL );

   return UiOK;
}

/******************************************************************************/
/*                                                                            */
/*                        Resource Get Operations                             */
/*                                                                            */
/******************************************************************************/

/****************************** ui_get_session_bg *****************************/

char *
ui_get_session_bg( void ) {

   return (sessionCreated == UiNO_SESSION) ?
      UiERRORP : UIW_NewString( session_bg );
}

/****************************** ui_get_menu_bg ********************************/

char *
ui_get_menu_bg( void ) {

   return (sessionCreated == UiNO_SESSION) ?
      UiERRORP : UIW_NewString( menu_bg );
}

/****************************** ui_get_menu_fg ********************************/

char *
ui_get_menu_fg( void ) {

   return (sessionCreated == UiNO_SESSION) ?
      UiERRORP : UIW_NewString( menu_fg );
}

/****************************** ui_get_menu_font ******************************/

char *
ui_get_menu_font( void ) {

   return (sessionCreated == UiNO_SESSION) ?
      UiERRORP : UIW_NewString( menu_font );
}

/*************************** ui_get_visibility ********************************/

int
ui_get_visibility( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return existent->visibility;
   else
      return UiERROR;
}

/*************************** ui_get_sensitivity *******************************/

int
ui_get_sensitivity( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return existent->sensitivity;
   else
      return UiERROR;
}

/**************************** ui_get_foreground *******************************/

char *
ui_get_foreground( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->fg_color );
   else
      return UiERRORP;
}

/**************************** ui_get_background *******************************/

char *
ui_get_background( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->bg_color );
   else
      return UiERRORP;
}

/****************************** ui_get_font ***********************************/

char *
ui_get_font( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->font );
   else
      return UiERRORP;
}

/****************************** ui_get_bitmap *********************************/

char *
ui_get_bitmap( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->bitmap );
   else
      return UiERRORP;
}

/****************************** ui_get_geometry *******************************/

char*
ui_get_geometry( char *name ) {
   WNAME *existent;
   char *geometry;

   if( !strcmp( name, UiSIMPLE_SESSION_OUTPUT ) ||
       !strcmp( name, UiSIMPLE_SESSION_INPUT  )  )
      return UiERRORP;

   if( existent = UIW_getWNAMEofName( name ) ) {
      geometry = (char *)XtMalloc( 256 );
      sprintf( geometry, "%d %d %d %d",
         existent->x,
         existent->y,
         existent->width,
         existent->height );
      return geometry;
   } else
      return UiERRORP;
}

/****************************** ui_get_alignment ******************************/

int
ui_get_alignment( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return existent->alignment;
   else
      return UiERROR;
}

/****************************** ui_get_text ***********************************/

char *
ui_get_text( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->text );
   else
      return UiERRORP;
}

/****************************** ui_get_callback *******************************/

char *
ui_get_callback( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return UIW_NewString( existent->callbackMsg );
   else
      return UiERRORP;
}

/****************************** ui_get_mnemonic *******************************/

int
ui_get_mnemonic( char *name ) {
   WNAME *existent;

   if( existent = UIW_getWNAMEofName( name ) )
      return existent->mnemonic;
   else
      return UiERROR;
}

/******************************************************************************/
/*                                                                            */
/*                        Save Current User-Interface                         */
/*                                                                            */
/******************************************************************************/

/****************************** ui_save ***************************************/

int
ui_save( char *command_name, char *resource_name ) {

   if( sessionCreated != UiSESSION )
      return UiERROR;

   if( resource_name == UiKEEPP )
      return save_commands( command_name );
   return save_commands_and_resources( command_name, resource_name );
}

/********************************** EOF ***************************************/

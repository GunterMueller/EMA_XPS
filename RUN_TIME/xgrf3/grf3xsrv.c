#define DEBUGn                          /* Include dbgpr() Infos... */
#define PRINTPROTOKOLLn      /* Print GrfProtokoll, don't do things */
#define SLOWCOLORSn       /* NUR im Testbetrieb fuer Farbbestimmung */
/*********************************************************************
*   grf3scr.c              Hans Groschwitz                23.05.91   *
*                                                                    *
*      Motif-Routinen zur Bildschirmsteuerung                        *
*                                                                    *
*                          Jens Onno Krah                            *
*      grf3ibm.c ........................................ 19.03.90   *
*      Turbo schiss ..................................... 26.04.90   *
*      Reset Point ...................................... 22.05.90   *
*                          Hans Groschwitz                           *
*      grf3apollo.c ..................................... 25.01.91   *
*      grf3M.c .......................................... 17.05.91   *
*      Aufteilung in mehrere Files ...................... 23.05.91   *
*      grf3_mot.c ....................................... 23.05.91   *
*      grf3mot.c (kein Aegis mehr) ...................... 20.10.93   *
*      grf3scr.c (Motif-Routinen ausgelagert) ........... 21.10.93   *
*      grf3scr.h Protokoll beschleunigt ................. 22.10.93   *
*      fakeEvent: width/height verkleinert: schneller ... 26.10.93   *
*      MS_Str wandelt ' ' in '0' bei der IPC ............ 03.08.94   *
*      Separater Prozess: xgrf3d (grf3sipc, grf3xsrv) ... 03.08.94   *
*                                                                    *
*   Noch nicht fertig:                                               *
*                                                                    *
*      Momentan wird nicht zwischen screen1 und screen2              *
*      unterschieden.                                                *
*      Spaeter kann hier ein zweiter Server sein, der 2 Bildschirme  *
*      bedient.                                                      *
*********************************************************************/

#include <stdio.h>                                        /* stderr */
#include <signal.h>                             /* SIGTERM, SIGKILL */
#include <Xm/Form.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <X11/cursorfont.h>
#define min(a,b) ((a)<(b)?(a):(b))      /* ist nicht in stdlib.h !! */
#define max(a,b) ((a)>(b)?(a):(b))
extern char *getenv( char* );

#include "grf3m.h"                                 /* MOUSE-Strukur */
#include "grf3xsrv.h"                    /* dbgpr, ProtokollFormate */
#include "grf3sipc.h"

#include "Xgrf3.bm"             /* X-Bitmap-Data for iconPixmap !!! */

/*** Defines ********************************************************/

#define HEADLINE   "      EMA-Grafik (v3)   (c)1987-1993      "
#define DELTA_T    5        /* Millisekunden, pipe-Abfrageintervall */
#define DELTA_TMIN 1             /* Millisekunden, X-Queue Overflow */

/*
 * ITAE: an der schnellsten Maschine auszutesten !
 * LOW_WATER nicht zu niedrig waehlen, sonst kann eine unnoetige Pause
 * bei Ueberlast entstehen, die nicht wiederaufzuholen ist !
 */
#define LOW_WATER  100

#define HIGH_WATER (10*LOW_WATER)
#define progName   "Xgrf3"      /* fuer die Bindung von X-Resourcen */
#define inputFile  "snapshot.bm"       /* X-Bitmap-Datei im "."-Dir */
#define APPLWIDTH  905                  /* optimized for 'variable' */
#define APPLHEIGHT 760       /* depends on '*fontList:' of menu !!! */
#define MI_ALLMASK (MI_NONE|MI_MMOVED|MI_LEFTBPR|MI_LEFTBREL| \
                    MI_RIGHTBPR|MI_RIGHTBREL|MI_MIDBPR|MI_MIDBREL)
#define MF_INVIS   (MF_MAX+1)          /* internally used MouseForm */
#define COLMASK(i) (1<<(i))       /* fuer die FarbPalette benoetigt */

typedef struct { int r, g, b; } RGBV;

/*** more Defines ***************************************************/

#define COLOR0     "white"                            /* unused ... */
#define COLOR1     "royal blue"       /* 16 Farben kann BGI-Treiber */
#define COLOR2     "lime green"         /* via ITAE an BGI-Defaults */
#define COLOR3     "deep sky blue"               /* angeglichen ;-) */
#define COLOR4     "indian red"
#define COLOR5     "magenta"
#define COLOR6     "brown"
#define COLOR7     "gray65"
#define COLOR8     "gray35"
#define COLOR9     "blue"
#define COLOR10    "green"
#define COLOR11    "dark turquoise"
#define COLOR12    "red"
#define COLOR13    "hot pink"
#define COLOR14    "orange"
#define COLOR15    "black"                             /* unused ... */

/*
 * In der Protokoll-HeaderDatei werden diese Werte als Konstanten
 * gesetzt. Bei der Wahl neuer Fonts sollte grf3scr.c mit DEBUG  
 * und STANDALONE rekompiliert werden. Der folgende Output kann dann
 * in die HeaderDatei uebernommen werden.  
 */
#define FONT1     "*courier-medium-r*--10*"
#define FONT2     "*courier-medium-r*--12*"
#define FONT3     "*courier-medium-r*--14*"
#define FONT4     "*courier-medium-r*--18*"

/*** Prototypes *****************************************************/

static void RedrawAct( Widget, XExposeEvent* );
static void ButtonPressAct( Widget, XButtonPressedEvent* );
static void ButtonReleaseAct( Widget, XButtonReleasedEvent* );
static void PointerMotionAct( Widget, XPointerMovedEvent* );

static void QuitCB( void );
static void ContCB( void );
static void SnapCB( void );
static void TimerSnapCB( void );
static void TimerInitCB( void );
static void TimerInputCB( void );

static void M_server( char*, int );
static int  DoOneJob( void );

/*********************************************************************
*       Globals                                                      *
*********************************************************************/

static GC          copy_gc;                     /* Graphics Context */
static GC          clear_gc;                    /* Graphics Context */
static GC          work_gc;                     /* Graphics Context */
static XFontStruct *fontstruct[MAXFONTS];       /* Font descriptors */
static Pixmap      pixels;                      /* Die 'DRAWABLE'   */
static Display     *dpy;
static Screen      *scrn;

static int MouseIntFlags = 0;              /* Wann MouseInterrupt ? */
static int currentcolor;        /*  bei 'init colors' initialisiert */
static int currentfont;         /* wird bei 'init gc' initialisiert */
static int currentstyle;        /* wird bei 'init gc' initialisiert */
static int snapshooting = 0;
static int waiting = 0;      /* Cont=sensitive, but not pressed yet */
static int clientPid = 0;

static char *font[MAXFONTS] = { FONT1, FONT2, FONT3, FONT4 };
static Cursor cursor[MF_MAX+2];        /* for INVIS + all others... */
static int shape[MF_MAX+2] = {
   XC_left_ptr,
   XC_hand2,
   XC_watch,
   XC_X_cursor,
   XC_crosshair,
   XC_sb_left_arrow,
   XC_sb_right_arrow,
   XC_sb_up_arrow,
   XC_sb_down_arrow,
   XC_right_ptr,
   XC_question_arrow,
   /* --------- last is INVIS: ------- */
   XC_gumby
};
static int  color[MC_NRCOLORS];      /* beinhaltet den Farbwert ... */
static char *colstr[MC_NRCOLORS] = { COLOR0, COLOR1, COLOR2, COLOR3,
   COLOR4, COLOR5, COLOR6, COLOR7, COLOR8, COLOR9, COLOR10, COLOR11,
   COLOR12, COLOR13, COLOR14, COLOR15 };

/* SLOWCOLORS ermoeglicht, die Werte unterhalb erneut zu gewinnen ! */
/* ----- Cut here ------------------------------------------------- */

static RGBV rgbVals[] = {
   { 255, 255, 255 },
   {  65, 105, 225 },
   {  50, 205,  50 },
   {   0, 191, 255 },
   { 205,  92,  92 },
   { 255,   0, 255 },
   { 165,  42,  42 },
   { 166, 166, 166 },
   {  89,  89,  89 },
   {   0,   0, 255 },
   {   0, 255,   0 },
   {   0, 206, 209 },
   { 255,   0,   0 },
   { 255, 105, 180 },
   { 255, 165,   0 },
   {   0,   0,   0 },
};

/* ----- End Cut here --------------------------------------------- */

static char M_str[ M_STRLEN ];

static char* translations = "\
      <Expose>:        RedrawAct()        \n\
      <ButtonPress>:   ButtonPressAct()   \n\
      <ButtonRelease>: ButtonReleaseAct() \n\
      <Motion>:        PointerMotionAct() \n\
";

static XtActionsRec actions[] = {
      { "RedrawAct",        (XtActionProc)RedrawAct        },
      { "ButtonPressAct",   (XtActionProc)ButtonPressAct   },
      { "ButtonReleaseAct", (XtActionProc)ButtonReleaseAct },
      { "PointerMotionAct", (XtActionProc)PointerMotionAct },
};

static Widget snap, cont, quit, desk, toplevel;

/*********************************************************************
*       Standalone-Mode...                                           *
*********************************************************************/

static int stdinMode = 0;
static int ipcMode = 0;

static int is_integer( char *str ) {
   while( *str ) {
      if( !isdigit( *str++ ) )
         return 0;
   }
   return 1;
}

void
main( int argc, char **argv ) {
   SHM *shmp = NULL;           /* see ipc_open below*/

#ifdef DEBUG
#ifdef apollo
   puts("cpp: implicit: apollo");
#endif
#ifdef sun
   puts("cpp: implicit: sun");
#endif
#endif

   if( argc == 1 ) {   /* Demo-Mode: */
      puts("Xgrf3 server: entering demo mode... Enjoy!");

   } else if( *argv[1] == '-' ) {
      if( !strcmp( argv[1], "-" ) ) {
         puts("Xgrf3 server: reading GrfProtocol from stdin ...");
         stdinMode = 1;

      } else {
         puts("Usage: xgrf3d nnn   with nnn a valid shmid (%d)");
         puts("       xgrf3d file  with file containing GrfProtocol");
         puts("       xgrf3d -     reading GrfProtocol from stdin");
         puts("       xgrf3d       demo mode (Enjoy!)");
         exit( -1 );
      }
   } else if( is_integer( argv[1] ) ) {
             /* assuming argv[1] to be a valid shmid ... */

      if( stdin != freopen("/dev/null","r",stdin) ) {
         perror("Xgrf3 server: freopen stdin to /dev/null failed");
         exit( -1 );
      }
      /*
       * exits on failure!
       * sets shmp on success.
       */
      shmp = ipc_open( atoi( argv[1] ) );
      ipcMode = 1;

   } else {  /* assuming argv[1] to be a filename ... */
      
      if( stdin != freopen(argv[1],"r",stdin) ) {
         fprintf( stderr, "Xgrf3 server: freopen stdin to file %s ", argv[1] );
         fflush( stderr );
         perror("failed");
         exit( -1 );
      }
      stdinMode = 1;
   }
   if( ipcMode ) {
      shmp->serverPid = getpid();
      M_server( shmp->tcpip, shmp->clientPid );

   } else {
      char *p, str[256];

      if( p=getenv("DISPLAY") ) {
         strcpy( str, p );
         if( p=rindex( str, ':' ) )
            *p = 0;
         p = str;
      }
      M_server( p, 0 );
   }
}

/********************************************************************/

void
M_send_response( int i ) {
   if( !ipcMode )
      return;
   M_response( i );
}

/********************************************************************/

int
M_get_token( char *buf ) {
   static int i = 0;
   static char *vec[] = {
      "F0", "S100,100,Testtext",
      "F1", "S100,150,Testtext",
      "F2", "S100,200,Testtext",
      "F3", "S100,250,Testtext",
      "P400,400",
      "P410,410",
      "P420,420",
      "L390,390,390,430",
      "L390,390,430,390",
      "Y3",
      "W",
      "I",
      "F3",
      "S500,400,P r e s s   C o n t i n u e",
      "Y7",
      "W",
      "X",
      NULL
   };

   if( ipcMode ) {
      return M_read_token( buf );
   }

   if( stdinMode ) {
      if( !gets( buf ) ) {
         strcpy( buf, "X" );  /* GrfProtocol: Exit */
      }
      return strlen( buf );
   }

   /* demoMode */
   if( !vec[i] ) {
      strcpy( buf, "X" );  /* GrfProtocol: Exit */
   } else {
      strcpy( buf, vec[i++] );
   }
   return strlen( buf );
}

/*********************************************************************
*  Print-Error-Message - Formatter                                   *
*********************************************************************/

static void
printErr( char *func, char *msg ) {
   fprintf( stderr, "Xgrf3 server: %s: %s\n", func, msg );
   fflush( stderr );
}

/*********************************************************************
**********************************************************************
**                                                                  **
**  Motif-Window-Server                                             **
**                                                                  **
**  HG: Abfrage der DISPLAY-Variablen...                            **
**                                                                  **
**********************************************************************
*********************************************************************/

static void
M_server( char *tcpip_addr, int clientpid ) {
   Widget form, menu, screen, head, vsb, hsb;
   Arg    args[32];
   int    i, n;
   int    gArgc;
   char   *gArgv[9];
   XGCValues    gcv;                      /* Struct for creating GC */
   XExposeEvent fakeEvent;
   Pixmap iconPm;             /* holds iconPixmap of graphic Screen */
   XVisualInfo vinf;                 /* for Detection of VisualType */
   int nrPlanes;

   clientPid = clientpid;

   /* ------------ Simulation of a commandline -------------------- */
   if( tcpip_addr == NULL )                          /* Display !!! */
      tcpip_addr = "";
   gArgv[0] = progName;
   gArgv[1] = "-display";
   sprintf( M_str, "%s:0.0", tcpip_addr );
   gArgv[2] = M_str;
   /*
    * Xapollo: wird nicht sauber aus .Xdefaults gelesen,
    * seit die .Xadefaults heissen
    * ausserdem: eine leichte Moeglichkeit, zu hardcoden...
    */
   gArgv[3] = "-xrm";
   gArgv[4] = "*fontList: variable";
   /*
    * wemahp: als einzige COLOR-Apollo-Station (mit 4 Grauwerten...)
    * ist dort wohl "*foreground: white" allgemein gesetzt ?
    * Das sieht man an den Texten in der Menuleiste.
    * Dort ist das auch angebracht, wegen der sonst fast nicht lesbaren Schrift
    * (Black auf DarkGray)
    * Da aber beim Linienzeichnen weisse Linie auf weissem Grund wenig bringt,
    * so ein Weg, der allen Loesungen gerecht wird (SUN verhaelt sich dabei wie
    * die BlackWhite-Apollos ...)
    */
   gArgv[5] = "-xrm";
   gArgv[6] = "*XmPixmap*foreground: black";
   gArgc = 7;

   dbgpr("M_server: initing toolkit...\n");
#ifdef apollo
   toplevel = XtInitialize( progName, progName, NULL, 0, &(Cardinal)gArgc, gArgv );
#else
   toplevel = XtInitialize( progName, progName, NULL, 0, &gArgc, gArgv );
#endif

   /* ---- Creating Icon Pixmap from included Xgrf3.bm ------------ */

   dbgpr("M_server: installing iconPixmap...\n");

   dpy = XtDisplay(toplevel);
   scrn = XtScreen(toplevel);

   iconPm = XCreateBitmapFromData( dpy, RootWindowOfScreen( scrn ),
      Xgrf3_bits, Xgrf3_width, Xgrf3_height );

   n = 0;
   XtSetArg( args[n], XmNiconPixmap,                     iconPm ); n++;
   XtSetValues( toplevel, args, n );

   /*
    * Die Width und Height des FORM werden von den externen X-Resourcen
    * ueberschrieben, obwohl es ge-hardcoded ist !!!
    * WOHL, weil dieses X-Resourcen auf toplevel wirken ???
    * Ist ja eigentlich in diesem Fall sogar erwuenscht, weil
    * so der DEFAULT auf den C-Source-Wert gelegt ist, der von 
    * den Werten in der xrdb / .Xapplresdir / -xrm
    * ueberschrieben werden kann. Ex. diese nicht, sind trotzdem 
    * vernuenftige Werte voreingestellt.
    * Trotzdem kann von aussen der Wert geaendert werden ...
    */
   n = 0;
   XtSetArg( args[n], XmNwidth,                       APPLWIDTH ); n++;
   XtSetArg( args[n], XmNheight,                     APPLHEIGHT ); n++;
   form = XmCreateForm( toplevel, "form", args, n );
   XtManageChild( form );

   /* ---- Creating complete Menu Bar ----------------------------- */

   dbgpr("M_server: creating complete menu bar...\n");
   /*
    * MenuBar enthaelt die Buttons fuer die PullDowns.
    * ! MenuBar ist ein RowColumnWidget des Types: XmMENU_BAR !
    * Es wird durch 'Attachment' festgelegt, dass die MenuBar oben
    * im FormWidget die ganze Breite einnimmt.
    * Die Widget ID wird 'main' zurueckgegeben.
    */
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,           XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNleftAttachment,          XmATTACH_FORM ); n++;
   menu = XmCreateMenuBar( form, "menu", args, n );
   XtManageChild( menu );

   /* --- Quit-MenuButton: Child von menu --- */
   n = 0;
   XtSetArg( args[n], XmNlabelString,
               XmStringCreate("Quit", XmSTRING_DEFAULT_CHARSET) ); n++;
   XtSetArg( args[n], XmNmnemonic,                          'Q' ); n++;
   quit = XmCreateCascadeButton( menu, "quit", args, n );

   XtAddCallback( quit, XmNactivateCallback, (XtPointer)QuitCB, NULL );

   /* Rechtsbuendig ans menu anbinden ... (siehe XmRowColumn) */
   n = 0;
   XtSetArg( args[n], XmNmenuHelpWidget,                   quit ); n++;
   XtSetValues( menu, args, n );

   XtManageChild( quit );

   /* --- Cont-MenuButton: Child von menu --- */
   n = 0;
   XtSetArg( args[n], XmNlabelString,
           XmStringCreate("Continue", XmSTRING_DEFAULT_CHARSET) ); n++;
   XtSetArg( args[n], XmNmnemonic,                          'C' ); n++;
   cont = XmCreateCascadeButton( menu, "cont", args, n );

   XtAddCallback( cont, XmNactivateCallback, (XtPointer)ContCB, NULL );
   XtManageChild( cont );

   /* --- Snap-MenuButton, Child von menu --- */
   n = 0;
   XtSetArg( args[n], XmNlabelString,
           XmStringCreate("Snapshot", XmSTRING_DEFAULT_CHARSET) ); n++;
   XtSetArg( args[n], XmNmnemonic,                          'S' ); n++;
   snap = XmCreateCascadeButton( menu, "snap", args, n );

   XtAddCallback( snap, XmNactivateCallback, (XtPointer)SnapCB, NULL );
   XtManageChild( snap );

   /* --- Ueberschrift erzeugen --------------- */

   n = 0;
   XtSetArg( args[n], XmNtraversalOn,                     False ); n++;
   XtSetArg( args[n], XmNlabelString,
            XmStringCreate( HEADLINE, XmSTRING_DEFAULT_CHARSET) ); n++;
   XtSetArg( args[n], XmNshadowThickness,                     0 ); n++;
   head = XmCreateCascadeButton( menu, "head", args, n );
   XtManageChild( head );

   /* ---- Creating scrolled graphic screen ----------------------- */

   dbgpr("M_server: creating scrolled graphic screen...\n");
   n = 0;
   XtSetArg( args[n], XmNtopAttachment,         XmATTACH_WIDGET ); n++;
   XtSetArg( args[n], XmNtopWidget,                        menu ); n++;
   XtSetArg( args[n], XmNleftAttachment,          XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNrightAttachment,         XmATTACH_FORM ); n++;
   XtSetArg( args[n], XmNbottomAttachment,        XmATTACH_FORM ); n++;
   /*
    * Scrolled Widget erzeugt ein Fenster mit Scrollbars
    * und einem Arbeitsbereich.
    * Da das ScrolledWindow aehnliche Resourcen hat,
    * nur die veraenderten Resourcen neu besetzen ...
    */
   XtSetArg( args[n], XmNscrollBarDisplayPolicy,    XmAS_NEEDED ); n++;
   XtSetArg( args[n], XmNborderWidth,                         1 ); n++;
   XtSetArg( args[n], XmNscrollingPolicy,           XmAUTOMATIC ); n++;
   screen = XtCreateWidget( "screen",
                     xmScrolledWindowWidgetClass, form, args, n );
   XtManageChild( screen );

   n = 0;
   XtSetArg( args[n], XmNhorizontalScrollBar,              &hsb ); n++;
   XtSetArg( args[n], XmNverticalScrollBar,                &vsb ); n++;
   XtGetValues( screen, args, n );

   n = 0;
   XtSetArg( args[n], XmNlabelType,                    XmPIXMAP ); n++;
   XtSetArg( args[n], XmNwidth,                        SCRWIDTH ); n++;
   XtSetArg( args[n], XmNheight,                      SCRHEIGHT ); n++;
   XtSetArg( args[n], XmNalignment,          XmALIGNMENT_CENTER ); n++;
   XtSetArg( args[n], XmNborderWidth,                         1 ); n++;
   XtSetArg( args[n], XmNmarginWidth,                         5 ); n++;
   XtSetArg( args[n], XmNmarginHeight,                       10 ); n++;
   desk = XmCreateLabel( screen, "desk", args, n );
   XtManageChild( desk );

   /*--------------------------create pixmap------------------------*/

   dbgpr("M_server: creating pixmap...\n");

   pixels = XCreatePixmap( dpy, RootWindowOfScreen( scrn ),
         SCRWIDTH, SCRHEIGHT, DefaultDepthOfScreen( scrn ) );

   /* ------------------------ load fonts ------------------------- */

   dbgpr("M_server: loading fonts...\n");
   for( i = 0; i < MAXFONTS; i++ ) {
      if( (fontstruct[i] = XLoadQueryFont(dpy, font[i]) ) == NULL ) {
         fprintf( stderr,
               "Xgrf3: display %s doesn't know font '%s'\n",
               DisplayString(dpy), font[i] );
         fprintf( stderr, "   ... trying \"fixed\"\n");
         if( (fontstruct[i] = XLoadQueryFont(dpy, "fixed") ) == NULL ) {
            fprintf( stderr, "   ... UNEXPECTED: failed, too ?!?\n");
            fflush( stderr );
            /* damit auch wirklich noch alles geschrieben wird ... */
            usleep( 100000 );
            exit( -1 );
         }
      }
      /*
       * In der Protokoll-HeaderDatei werden diese Werte als Konstanten
       * gesetzt. Bei der Wahl neuer Fonts sollte grf3scr.c mit DEBUG
       * und rekompiliert werden. Der folgende Output kann dann
       * in die HeaderDatei uebernommen werden.
       */
      dbgpr3("   font #%d: x=%d y=%d\n", i,
         fontstruct[i]->max_bounds.width,
         fontstruct[i]->max_bounds.descent+fontstruct[i]->max_bounds.descent
      );
   }

   /* ----------------------- CursorForms ------------------------- */

   dbgpr("M_server: installing pointer images...\n");
   for( i = 0; i <= MF_INVIS; i++ ) {
      cursor[i] = XCreateFontCursor( dpy, shape[i] );
   }

   /*-------------------------init gc-------------------------------*/

   dbgpr("M_server: initing GCs...\n");
   gcv.function = GXcopy;
   gcv.font = fontstruct[currentfont=1]->fid;
   gcv.foreground  = BlackPixel( dpy, DefaultScreen(dpy) );
   gcv.background  = WhitePixel( dpy, DefaultScreen(dpy) );
   work_gc = XCreateGC( dpy, pixels,
            GCFunction | GCForeground | GCBackground | GCFont, &gcv );
   currentstyle = M_NORMAL;

   gcv.foreground  = BlackPixel( dpy, DefaultScreen(dpy) );
   gcv.background  = WhitePixel( dpy, DefaultScreen(dpy) );
   copy_gc = XCreateGC( dpy, pixels,
            GCFunction | GCForeground | GCBackground, &gcv );

   gcv.foreground  = WhitePixel( dpy, DefaultScreen(dpy) );
   gcv.background  = BlackPixel( dpy, DefaultScreen(dpy) );
   clear_gc = XCreateGC( dpy, pixels,
            GCFunction | GCForeground | GCBackground, &gcv );

   /*---------------------initing colors----------------------------*/

   dbgpr("M_server: initing colors...\n");

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
    * SUNs:          DirectColor, 8 planes = 256 entry VideoLookupTable
    * Apollo DN's:   StaticGray,  1 plane = black&white
    * HP 425e:       PseudoColor, 8 planes via software
    *                             graphicBoard: real = 2 planes
    *                --> wg. geringer Helligkeit keine Farben !
    */

   if( XMatchVisualInfo( dpy, DefaultScreen(dpy),
          nrPlanes=DisplayPlanes( dpy, DefaultScreen(dpy) ),
          DirectColor, &vinf ) ) {
      long *ColPtr, pixels[1];
      int nrColors, j, k;
      XColor xcolor;
      int colmask;

      dbgpr("   ... for full color display!\n");
      nrColors = 1 << nrPlanes;
      /*
       * SUN: nrPlanes = 8, nrColors = 256
       *      --> xshowcmap
       *
       * Wenn der Xserver initialisiert wird (zB nach xdm-login),
       * sind die Eintraege #0(weiss) und #1(schwarz)
       * initialisiert worden. (BlackPixel und WhitePixel)
       *
       * Der mwm und xsetroot fordern weitere Farben an.
       * Zur StartZeit von Xgrf3 sind so bereits 14 Farben allokiert.
       *
       * Daher lohnt es sich, erst mal zu sehen, ob einige der
       * benoetigten Farben bereits dort eingetragen sind ...
       */

      /* spart Eintraege in die VLT */
      color[MC_WHITE] = WhitePixel( dpy, DefaultScreen(dpy) );
      color[MC_BLACK] = BlackPixel( dpy, DefaultScreen(dpy) );

      colmask = 0;
#ifndef SLOWCOLORS
      for( i = 3; i < nrColors; i+=2 ) {   /* 1 = BlackPixel */
         /* nur die UNgeraden interessieren ! (wg. Snapshot) */

         for( j = 1; j < (MC_NRCOLORS-1); j++ ) {
            int r, g, b;

            if( colmask & COLMASK(j) )     /* ex. bereits... */
               continue;

            xcolor.pixel = i;
            XQueryColor( dpy, DefaultColormap(dpy,DefaultScreen(dpy)), &xcolor );
            /*
             * XQueryColor findet in der gesamten VLT gueltige Werte !
             * Also auch auf noch nicht allokierten Eintraegen (meist noch schwarz,
             * initWert des Xservers) und auf Werten die von anderen Appl.s privat
             * allokiert und verwaltet (evtl. veraendert) werden !!!
             */
            r = 0xff & xcolor.red;
            g = 0xff & xcolor.green;
            b = 0xff & xcolor.blue;

            if( rgbVals[j].r == r && rgbVals[j].g == g && rgbVals[j].b == b ) {
               dbgpr2("color %2d already in VLT at position %3d\n",
                  j, i );
               color[j] = i;
               colmask |= COLMASK(j);
            }
         }
      }
#endif
      if( !(ColPtr=(long*)malloc((nrColors>>1)*sizeof(int))) ) {
         fprintf( stderr,
            "Xgrf3 server: insufficient memory: switching to B/W mode\n");
         goto b_w_mode;
      }
      for( j = 1, k = 0; j < (MC_NRCOLORS-1); j++ ) {
         if( colmask & COLMASK(j) )    /* ex. bereits... */
            continue;
         /*
          * Prinzip der Belegung der VLT:
          *
          * Es sind bereits von anderen Appls einige Eintraege gesetzt worden.
          * XAllocColorCells() belegt weitere Werte.
          * Wenn die Applikation beendet wird, raeumt der X-Server
          * diese (dynamischen) Eintraege in die VLT dieses Visuals ab !!!
          * ==> Free nicht noetig!
          *
          * AUSSER: Eine Xappl. allokiert jedes mal neu ohne Pruefung n VLT-Eintraege
          * wenn mehrere Instanzen dieser Appl. auf dem selben DISPLAY darstellen
          * laeuft irgendwann die VLT ueber, XAllocColorCells failed !
          * ==> Xgrf3 prueft zuerst, ob die benoetigten Farbwerte nicht schon ex.
          * Das ist zB der Fall, wenn mehrere Xgrf3's (zB 1x test3 und 1x bsp)
          * gleichzeitig darstellen !
          */
         while( 1 ) {
            if( !XAllocColorCells( dpy, DefaultColormap(dpy,DefaultScreen(dpy)),
              False, NULL, 0, pixels, 1 ) ) {
               /*
                * sucht freie Eintraege in der VLT, die bisher noch
                * von niemand genutzt werden; also von dieser Appl. privat verwaltet
                * (und veraendert) werden koennen !!!
                */
               while( j < (MC_NRCOLORS-1) )
                  color[j++] = color[MC_BLACK];  /* BlackPixel */

               fprintf( stderr,
                  "Xgrf3 server: VLT full: Using BLACK for colors #%d to #15\n", j );
               break;
            }
            if( pixels[0] & 1 ) {
               /*
                * Nur ungeradzahlige Eintraege in die VideoLookupTable haben eine 1
                * in der untersten plane und werden beim Snapshot als "Schwarz"
                * gewertet !!! (wichtig: erspart die COLOR -> B/W Umsetzung !!!)
                */
               color[j] = pixels[0];    /* 0...(nrColors-1): entry# in VLT */

#ifdef SLOWCOLORS
               XStoreNamedColor( dpy, DefaultColormap( dpy, DefaultScreen(dpy) ),
                  colstr[j], color[j], DoRed | DoGreen | DoBlue );
#else
               xcolor.pixel = color[j];
               xcolor.red   = rgbVals[j].r | (rgbVals[j].r << 8);
               xcolor.green = rgbVals[j].g | (rgbVals[j].g << 8);
               xcolor.blue  = rgbVals[j].b | (rgbVals[j].b << 8);
               xcolor.flags = DoRed | DoGreen | DoBlue;
               XStoreColor( dpy, DefaultColormap(dpy,DefaultScreen(dpy)), &xcolor );
#endif
               dbgpr2("color %2d stored at position %3d\n", j, color[j] );
               break;
            }
            ColPtr[k++] = pixels[0];        /* SORRY: even addr. VLT entry */
         }                              /* cannot be used for SnapShot !!! */
      }
      for( i = 0; i < k; i++ )                /* Free all even VLT-Entries */
         XFreeColors( dpy, DefaultColormap(dpy,DefaultScreen(dpy)),
            &ColPtr[i], 1, 0 );

#ifdef SLOWCOLORS
      printf("static RGBV rgbVals[] = {\n");
      for( i = 0; i < MC_NRCOLORS; i++ ) {
         int r, g, b;

         xcolor.pixel = color[i];
         XQueryColor( dpy, DefaultColormap(dpy,DefaultScreen(dpy)), &xcolor );
         r = 0xff & xcolor.red;
         g = 0xff & xcolor.green;
         b = 0xff & xcolor.blue;
         /*
          * Zur Beschleunigung der Initialisierungsphase wird aus rgbVal
          * gelesen. Fuer die Wertebestimmung dieses Vektors ist einmalig
          * der #define SLOWCOLORS zu setzen. Die Ausgabe ist in den Kopf
          * dieser Datei einzubinden !
          *
          * NICHT fuer den Dauerbetrieb gedacht, weil jede Instanz von Xgrf3 
          * ungeprueft neue FarbEintraege Allokiert !!!
          * Dann ist schnell die VLT voll !!!
          */
         printf("   { %3d, %3d, %3d },\n", r, g, b );
      }
      printf("};\n");
#endif
   } else {
    b_w_mode:
      dbgpr("   ... for black+white display!\n");
      color[0] = WhitePixel( dpy, DefaultScreen(dpy) );
      for( i = 1; i < MC_NRCOLORS; i++ )
         color[i] = BlackPixel( dpy, DefaultScreen(dpy) );
   }
   currentcolor = MC_BLACK;     /* Standard-Einstellung des work_gc */

   /*---------------------------------------------------------------*/
   /* HG: hier waren die AddTabGroup's. Die haben aber nichts gebracht... */

   dbgpr("M_server: adding actions...\n");
   /*
    * Reihenfolge wichtig!!! beide sind vorgeholt worden
    * Action muss dem Server bekannt sein, bevor translations gemacht werden
    *
    * AH: XtWindow() in RedrawAct() versucht ein Fenster zuzuweisen.
    * Dieses gibt es aber zu dieser Zeit noch nicht.
    * Daher BadDrawable: Bad Pixmap or Window parameter !!!
    *
    * Window: das, was man sieht
    * Widget: die ganze Verwaltung (Strukturen,..) dahinter !!!
    */
   XtAddActions( actions, XtNumber( actions ) );

   dbgpr("M_server: adding translations...\n");
   n = 0;
   XtSetArg( args[n], XmNtranslations,
                  XtParseTranslationTable( translations ) ); n++;
   XtSetValues( desk, args, n );

   dbgpr("M_server: realizing toplevel...\n");
   XtSetSensitive( cont, False );
   XtRealizeWidget( toplevel );

   /*--------------------- Clearing Screen -------------------------*/

   dbgpr("M_server: clearing screen...\n");

   XFillRectangle( dpy, pixels, clear_gc, 0, 0, SCRWIDTH, SCRHEIGHT );
   fakeEvent.x      = 0;
   fakeEvent.y      = 0;
   fakeEvent.width  = SCRWIDTH;
   fakeEvent.height = SCRHEIGHT;
   RedrawAct( desk, &fakeEvent );

   /*-----------------------------Rest...---------------------------*/

   XSync( dpy, False );
   dbgpr("M_server: Xsync'ed...\n");
   /*
    * sorgt dafuer, dass nach XtMainLoop erst mal der erste RedrawAct
    * sauber ausgefuehrt wird und erst danach mit der Abarbeitung
    * der ProtokollAnweisungen begonnen wird !!!
    * 
    * 500 ms
    * Noetig auf langsamen DPYs (wema25,...) und moeglich, da das Auge
    * bei Bildaufbau eine halbe Sekunde braucht, um diesen (leer) erst
    * mal zu erfassen!
    */
   XtAddTimeOut( 500, (XtPointer)TimerInitCB, 0 );

   dbgpr("M_server: *** starting main loop... ***\n");
   XtMainLoop();
}

/*********************************************************************
*    Actions                                                         *
*********************************************************************/

/*
 * RedrawAct wird an Stelle der normalen Expose Action fuer das
 * Label Window eingetragen. Diese kann dann auch von Hand genutzt
 * werden, um nach einem Eintrag in die Pixmap den neuen Inhalt
 * schnell sichtbar zu machen.
 */
static void
RedrawAct( Widget w, XExposeEvent *event )
{
   int x, y, dd;
   unsigned width, height;

   if( event ) {
      x      = event->x;
      y      = event->y;
      width  = event->width;
      height = event->height;
      dbgpr("M_server: RedrawAct(event)...\n");
   } else {
      x      = 0;
      y      = 0;
      width  = SCRWIDTH;
      height = SCRHEIGHT;
      dbgpr("M_server: RedrawAct(NULL)...\n");
   }
   XCopyArea( dpy, pixels, XtWindow(w), copy_gc, x, y,
      width, height, x, y );
}

/********************************************************************/

static void
ButtonPressAct( Widget w, XButtonPressedEvent *event ) {
   MOUSE m;
   int mInt, mState;

   if( !ipcMode )
      return;

   /*
    * wird nur innerhalb des Screens registriert, wenn es der erste
    * Button gleichzeitig gedrueckt ist.
    * Ausserhalb kann eine weiterer Knopf mit dazugenommen werden !
    */
   dbgpr3( "M_server: ButtonPress: %d at (%d,%d)\n",
      event->button, event->x, event->y );

   switch( event->button ) {   /* 3-button-mouse using X-Windows */
      case 1:
         mInt = MI_LEFTBPR;
         mState = MS_LEFTB;
         break;
      case 2:
         mInt = MI_MIDBPR;
         mState = MS_MIDB;
         break;
      case 3:
         mInt = MI_RIGHTBPR;
         mState = MS_RIGHTB;
         break;
      default:
         return;   /* grosse Maeusse werden nicht unterstuetzt */
   }

   _m_read( &m, 1, 0 );     /* Warte, setze Sema nicht zurueck */
   m.xPos = event->x;
   m.yPos = SCRYMAX - event->y;        /* DESK Koordinaten ... */
   m.lastEvent = mInt;
   m.buttonState |= mState;

   /*
    * Informiert in Abhaengigkeit von der InterruptMaske
    * den ClientProzess, dass MouseEvents stattgefunden
    * haben. Der InterruptHandler dort kann dann die Werte
    * lesen...
    */
   if( (MI_MMOVED | mInt) & MouseIntFlags ) {
      _m_write( &m, 0, 0 );  /* Warte nicht, setze Sema nicht zurueck */
      kill( clientPid, SIGUSR1 );
   } else {
      _m_write( &m, 0, 1 );   /* Wichtig, da sonst System haengt... */
   }
}

/********************************************************************/

static void
ButtonReleaseAct( Widget w, XButtonReleasedEvent *event ) {
   MOUSE m;
   int mInt, mState;

   if( !ipcMode )
      return;

   dbgpr3( "M_server: ButtonRelease: %d at (%d,%d)\n",
      event->button, event->x, event->y );

   switch( event->button ) {   /* 3-button-mouse using X-Windows */
      case 1:
         mInt = MI_LEFTBPR;
         mState = MS_LEFTB;
         break;
      case 2:
         mInt = MI_MIDBPR;
         mState = MS_MIDB;
         break;
      case 3:
         mInt = MI_RIGHTBPR;
         mState = MS_RIGHTB;
         break;
      default:
         return;   /* grosse Maeusse werden nicht unterstuetzt */
   }

   _m_read( &m, 1, 0 );
   m.xPos = event->x;
   m.yPos = SCRYMAX - event->y;        /* DESK Koordinaten ... */
   m.lastEvent = mInt;
   m.buttonState &= ~mState;

   if( (MI_MMOVED | mInt) & MouseIntFlags ) {
      _m_write( &m, 0, 0 );  /* Warte nicht, setze Sema nicht zurueck */
      kill( clientPid, SIGUSR1 );
   } else {
      _m_write( &m, 0, 1 );   /* Wichtig, da sonst System haengt... */
   }
}

/********************************************************************/

static void
PointerMotionAct( Widget w, XPointerMovedEvent *event ) {
   MOUSE m;

   if( !ipcMode )
      return;

   dbgpr2( "M_server: PointerMotion: (%d,%d)\n", event->x, event->y );

   _m_read( &m, 1, 0 );
   m.xPos = event->x;
   m.yPos = SCRYMAX - event->y;             /* DESK Koordinaten ... */
   m.lastEvent = MI_MMOVED;

   if( MI_MMOVED & MouseIntFlags ) {
      _m_write( &m, 0, 0 );  /* Warte nicht, setze Sema nicht zurueck */
      kill( clientPid, SIGUSR1 );
   } else {
      _m_write( &m, 0, 1 );   /* Wichtig, da sonst System haengt... */
   }
}

/*********************************************************************
*   CallBacks                                                        *
*********************************************************************/

static void         /* -----------Quit------------------------------*/
QuitCB( void ) {

   dbgpr( "M_server: Quit pressed: exiting...\n" );
   /*
    * Wenn der Server stirbt, sei es absichtlich oder unerwartet,
    * wird das Signal 20 SIGCHLD dem Client/Parent mitgeteilt.
    * Der M_server wird zum Zombie, weil er eigentlich schon tot ist,
    * er aber noch auf den Parent wartet !!!
    * Der SignalHandler des Parent terminiert diesen, der Zombie
    * verschwindet.
    */
   exit( -1 );
}

static void         /* --------Continue-----------------------------*/
ContCB( void ) {
   dbgpr("ContCB: entering...\n");

   /* semaphore zuruecksetzen */
   waiting = 0;

   /* erst mal den Knopf wieder insensitiv setzen */
   XtSetSensitive( cont, False );

   /* dann das Waiting-for-Input wieder einreihen */
   XtAddTimeOut( DELTA_T, (XtPointer)TimerInputCB, 0 );

   /* dann dem Client das Tastaturereignis bestaetigen */
   /* 1 = mit linker Maustaste gedrueckt */
   M_send_response( 1 );     /* 0 = nothing pressed, else key- or mouse-press */

   dbgpr("ContCB: leaving...\n");
}

static void        /* ---------Snapshot-----------------------------*/
SnapCB( void ) {

   dbgpr("SnapCB: entering...\n");

   /* zuerst alle Knoepfe insensitiv setzen */
   XtSetSensitive( snap, False );
   XtSetSensitive( cont, False );
   XtSetSensitive( quit, False );

   /* semaphore setzen */
   snapshooting = 1;

   /* 1/10-Sekunde, um den Knopf insensitiv darzustellen */
   XtAddTimeOut( 100, (XtPointer)TimerSnapCB, 0 );

   dbgpr("SnapCB: leaving...\n");
}

/*----------------------------TimerSnap-----------------------------*/

static void                         /* vom Typ: XtTimerCallbackProc */
TimerSnapCB( void ) {

   printf(" Writing current image to '%s'...\n", inputFile );

   /* HG: fuer woody: Bitmap invertieren noetig !!! */
   XWriteBitmapFile( dpy, inputFile, pixels,
          SCRWIDTH, SCRHEIGHT, 0, 0 );

   /* Knoepfe nach Bedarf wieder sensitiv machen */
   XtSetSensitive( quit, True );
   XtSetSensitive( snap, True );
   if( waiting )
      XtSetSensitive( cont, True );

   /* semaphore zuruecksetzen */
   snapshooting = 0;

   printf(" Writing finished.\n");
}

/*----------------------------TimerInit-----------------------------*/

static void                         /* vom Typ: XtTimerCallbackProc */
TimerInitCB( void ) {

   dbgpr("M_server: sending READY to client...\n");
   XDefineCursor( dpy, XtWindow(desk), cursor[MF_INVIS] );

   /*
    * Sinn:
    *
    * Erst mal soll XtMainLoop() angelaufen sein, damit auch richtig
    * auf Exposes zu reagieren begonnen worden ist...
    * Dann kann ueber die erste Arbeitsanweisung nachgedacht werden...
    * Dann ist vor allem auch der ClrScreen fertig geworden !!!
    */
   XtAddTimeOut( DELTA_T, (XtPointer)TimerInputCB, 0 );
   M_send_response( 0 );
}

/*----------------------------TimerInput----------------------------*/

static void                         /* vom Typ: XtTimerCallbackProc */
TimerInputCB( void ) {
   static int nrJobsProcessed = 0;
   static int tooMuchPending = 0;
   int nrPending;

   if( snapshooting ) {
      XtAddTimeOut( DELTA_T, (XtPointer)TimerInputCB, 0 );
      /* zwischenzeitlich Ringpuffer vollaufen lassen !!! */
      return;
   }

   if( ipcMode && kill( clientPid, 0 ) ) {
      /*
       * Der ClientProzess ist gestorben !
       * Typischerweise ist grf_show("screen");exit(0); ausgefuehrt worden,
       * aber grf_show("text") wurde vergessen !!!
       * Normalerweise raeumt der Client selbst den SHM ab, wenn der
       * grf_show("text") durchgefuehrt worden ist!
       * Das ist hier offensichtlich nicht geschehen!!!
       * und wird hiermit sicherheitshalber nochmal nachgeholt, bevor
       * die X-Appl (=Server), die keinen korrespondierenden Client
       * mehr hat, selbst terminiert.
       */
      ipc_close();
      fprintf( stderr,
         "\n*** Xgrf3 server: client exited without `grf_show(\"text\");' ***\n"
      );
      fflush( stderr );
      usleep( 1000 );
      exit( -1 );
   }

   /*--------------------CheckStateOfXserver------------------------*/

 CheckNrPending:
   /*
    * Avoid Overflow of XEventQueue
    * --> e.g. 'ball' (X-power only...)
    */
   if( nrJobsProcessed > LOW_WATER ) {
      nrPending = XEventsQueued( dpy, QueuedAlready );
      if( nrPending > HIGH_WATER ) {         /* above HighWaterMark */
         if( !tooMuchPending ) {
            dbgpr("M_server: warning: X-Queue overloaded: waiting\n");
         }
         tooMuchPending = 1;

         /* retry later ... */
         XtAddTimeOut( DELTA_TMIN, (XtPointer)TimerInputCB, 0 );
         return;
      }

      if( tooMuchPending && nrPending > LOW_WATER ) {
         /* above LowWaterMark while decreasing: retry later ... */

         XtAddTimeOut( DELTA_TMIN, (XtPointer)TimerInputCB, 0 );
         return;
      }

      if( tooMuchPending ) {                  /* below LowWaterMark */
         tooMuchPending = 0;
         /* jetzt geht's wieder ... */

         dbgpr("M_server: info: continuing\n");
      }
      nrJobsProcessed = 0;
   }

   /*------------------------ProcessJobs----------------------------*/

   while( M_get_token( M_str ) > 0 ) {
      /*
       * was ist, wenn nicht genuegend Zeichen bereits in pipe ?
       * M_get_token = 0 --> noch nicht mit Einlesen fertig, also warten
       * M_get_token > 0 --> Befehl liegt an und kann ausgefuehrt werden
       */
      nrJobsProcessed++;
      if( DoOneJob() ) {
         /*
          * Warten auf MausKlick auf Cont-Button,
          * naechster AddTimeOut kommt von ContCB !!!
          */
         return;
      }
      if( nrJobsProcessed > LOW_WATER )
         goto CheckNrPending;
   }
   XtAddTimeOut( DELTA_T, (XtPointer)TimerInputCB, 0 );
}

/*********************************************************************
*   Das ist die ultimative Executive !!!     ;-)                     *
*     Dat is de Malooche. Boa, eji !                                 *
*                                                                    *
*   DoOneJob() geht davon aus, dass vor dem Aufruf der String M_str  *
*   mit einem sinnvollen ProtokollBefehl beschreiben worden ist!     *
*********************************************************************/

#ifdef PRINTPROTOKOLL

static int
DoOneJob( void ) {
   /* kann spaeter zB. als File wieder eingelesen werden... */
   puts(M_str);
   if( !strcmp(M_str,MS_Exit) ) {
      XtUnrealizeWidget( toplevel );
      M_send_response( 0 );
      usleep( 100000 );
      exit( 0 );
   }
   if( !strcmp(M_str,MS_Wait) ) {
      M_send_response( 1 );
   }
   return 0;
}
#else /* !PRINTPROTOKOLL */

static int
DoOneJob( void ) {
   int x1, x2, x3, x4, x, y, width, height;
   XExposeEvent fakeEvent;
   static int LastLineToX = 0;   /* for MS_More */
   static int LastLineToY = 0;

   dbgpr1("M_server: DoOneJob --> [%s]\n", M_str );

   /*------------------------ClearScreen----------------------------*/

   if( !strncmp( M_str, MS_Clr, TOKENLEN ) ) {
      XFillRectangle( dpy, pixels, clear_gc, 0, 0, SCRWIDTH, SCRHEIGHT );
      fakeEvent.x      = 0;
      fakeEvent.y      = 0;
      fakeEvent.width  = SCRWIDTH;
      fakeEvent.height = SCRHEIGHT;
      RedrawAct( desk, &fakeEvent );
      return 0;
   }

   /*-----------------------DrawingColor----------------------------*/

   if( !strncmp( M_str, MS_Color, TOKENLEN ) ) {
      if( 1 != sscanf( M_str, MS_Color, &x1 ) ) {
         printErr( "DrawingColor: Bad # of parameters", M_str );
      } else {
         if( x1 < 0 || x1 >= MC_NRCOLORS ) {
            char tmpstr[256];

            sprintf( tmpstr, "bad value (%d), BLACK assumed...", x1 );
            printErr( "DrawingColor", tmpstr );
            x1 = MC_BLACK;
         }
         if( currentcolor != x1 ) {
            XSetForeground( dpy, work_gc, color[x1] );
            currentcolor = x1;
         } else {
            dbgpr("M_server: DrawingColor: not changed\n" );
         }
      }
      return 0;
   }

   /*-----------------------DrawingStyle----------------------------*/

   if( !strncmp( M_str, MS_Style, TOKENLEN ) ) {
      if( 1 != sscanf( M_str, MS_Style, &x1 ) ) {
         printErr( "DrawingStyle: Bad # of parameters", M_str );
      } else {
         if( x1 != M_NORMAL && x1 != M_INVERS && x1 != M_EXOR ) {
            char tmpstr[256];

            sprintf( tmpstr, "bad value (%d), NORMAL assumed...", x1 );
            printErr( "DrawingStyle", tmpstr );
            x1 = M_NORMAL;
         }
         if( currentstyle != x1 ) {
            switch( x1 ) {                    /* !!! GC aendern !!! */
               case M_NORMAL:
                  dbgpr("M_server: DrawingStyle: NORMAL selected!\n");
                  XSetForeground( dpy, work_gc,
                     BlackPixel(dpy, DefaultScreen(dpy)) );
                  XSetFunction( dpy, work_gc, GXcopy );
                  break;
               case M_INVERS:
                  dbgpr("M_server: DrawingStyle: INVERS selected!\n");
                  XSetForeground( dpy, work_gc,
                     WhitePixel(dpy, DefaultScreen(dpy)) );
                  XSetFunction( dpy, work_gc, GXcopy );
                  break;
               case M_EXOR:
                  dbgpr("M_server: DrawingStyle: EXOR selected!\n");
                  XSetForeground( dpy, work_gc,
                     BlackPixel(dpy, DefaultScreen(dpy)) );
                  XSetFunction( dpy, work_gc, GXxor );
                  break;
            }
            currentstyle = x1;
         } else {
            dbgpr("M_server: DrawingStyle: not changed\n" );
         }
      }
      return 0;
   }

   /*----------------------------Line-------------------------------*/

   if( !strncmp( M_str, MS_Line, TOKENLEN ) ) {
      if( 4 != sscanf( M_str, MS_Line, &x1, &x2, &x3, &x4 ) )
         printErr( "Line: Bad # of parameters", M_str );
      else {
         /*
          * XDrawLine vs. XDrawPoint:
          * Es scheint, dass XDrawLine andere Bestandteile des GCs
          * liest, als XDrawPoint (siehe man-pages).
          * Da ein XDrawLine(x1,y1,x1,y1) auf sun UND apollo klappt,
          * XDrawPoint aber nur auf der sun (!?!?!) UND da ein
          * XDrawLine nicht viel langsamer ist als ein XDrawPoint,
          * wird im folgenden nur mit XDrawLine gearbeitet. Dadurch
          * kann die Fallunterscheidung "if( x1 == x3 && x2 == x4 )"
          * entfallen. Das gleicht auch den Rechenzeitverlust durch
          * einen Schleifenabbruchtest innerhalb XDrawLine aus!
          * Schliesslich ist die Linie haeufiger als der Punkt vom
          * Aufkommen im Xgrf3-Protokoll !!!
          */
         XDrawLine( dpy, pixels, work_gc, x1, x2, x3, x4 );
         fakeEvent.x      = min( x1, x3 );
         fakeEvent.y      = min( x2, x4 );
         fakeEvent.width  = max( x1, x3 ) - min( x1, x3 ) + 1;
         fakeEvent.height = max( x2, x4 ) - min( x2, x4 ) + 1;
         RedrawAct( desk, &fakeEvent );
         LastLineToX = x3;
         LastLineToY = x4;
      }
      return 0;
   }

   /*-----------------------More (LineTo)---------------------------*/

   if( !strncmp( M_str, MS_More, TOKENLEN ) ) {
      if( 2 != sscanf( M_str, MS_More, &x3, &x4 ) )
         printErr( "Line: Bad # of parameters", M_str );
      else {
         x1 = LastLineToX;
         x2 = LastLineToY;
         XDrawLine( dpy, pixels, work_gc, x1, x2, x3, x4 );
         fakeEvent.x      = min( x1, x3 );
         fakeEvent.y      = min( x2, x4 );
         fakeEvent.width  = max( x1, x3 ) - min( x1, x3 ) + 1;
         fakeEvent.height = max( x2, x4 ) - min( x2, x4 ) + 1;
         RedrawAct( desk, &fakeEvent );
         LastLineToX = x3;
         LastLineToY = x4;
      }
      return 0;
   }

   /*----------------------------Point------------------------------*/

   if( !strncmp( M_str, MS_Point, TOKENLEN ) ) {
      if( 2 != sscanf( M_str, MS_Point, &x1, &x2 ) )
         printErr( "Point: Bad # of parameters", M_str );
      else {
         XDrawLine( dpy, pixels, work_gc, x1, x2, x1, x2 );
         fakeEvent.x      = x1;
         fakeEvent.y      = x2;
         fakeEvent.width  = 1;
         fakeEvent.height = 1;
         RedrawAct( desk, &fakeEvent );
      }
      return 0;
   }

   /*---------------------------Font--------------------------------*/

   if( !strncmp( M_str, MS_Font, TOKENLEN ) ) {
      if( 1 != sscanf( M_str, MS_Font, &x1 ) )
         printErr( "Font: Bad # of parameters", M_str );
      else {
         if( x1 < 0 || x1 >= MAXFONTS ) {
            char tmpstr[256];

            sprintf( tmpstr, "Bad value (%d), 2 assumed", x1 );
            printErr( "Font", tmpstr );
            x1 = 2;
         }
         /* Nur wenn nicht der richtige Font gesetzt ... */
         if( currentfont != x1 ) {
            dbgpr2("M_server: Font: changing from %d to %d...\n", currentfont, x1 );
            XSetFont( dpy, work_gc, fontstruct[x1]->fid );
            currentfont = x1;
         } else {
            dbgpr( "M_server: Font: not changed\n");
         }
      }
      return 0;
   }

   /*--------------------------String-------------------------------*/

   if( !strncmp( M_str, MS_Str, TOKENLEN ) ) {
      char dummy[M_STRLEN];
      /*
       * HG: 03.08.94
       * MS_Str konnte bisher keine komplette Uebertragung beliebiger
       * Strings sichern!
       * Da der erlaubte Gueltigkeitsbereich von Strings wie folgt 
       * eingeschraekt ist (--> grf3mot.c):
       *
       * if( *p < ' ' || *p >= 127 ) *p = ' ';
       * if( *p == '0' )             *p = 'O';
       * if( *p == ' ' )             *p = '0';
       * 
       *  Da bei Jens Grafik keine Nullen geschrieben werden,
       *  und da die Uebertragung komplett leerer Strings
       *  problematisch ist, wird die Null als ErsatzCharacter
       *  fuer SPACE genommen !!!
       *
       * wird in dummy immer der gesamte Text (wenn auch kodiert)
       * uebertragen! Der muss nur noch decodiert werden !!!
       */
      if( 3 != sscanf( M_str, MS_Str, &x1, &x2, dummy ) )
         printErr( "String: Bad # of parameters", M_str );
      else {
#ifdef ANTIK
         int i;
         char *str = M_str + TOKENLEN;

         for( i = 0; *str && i < 2; str++ )
            if( *str == ',' ) i++;       /* str steht genau hinter Komma #2 */

         dbgpr2("M_Server: String: str=[%s] M_str=[%s]\n", str, M_str );
#else
         char *str = dummy;
         while( *str ) {
            if( *str == '0' )
               *str = ' ';
            str++;
         }
         str = dummy;
#endif
         x = x1;
         y = x2;
         width = XTextWidth( fontstruct[currentfont], str, strlen(str) );
         height = fontstruct[currentfont]->max_bounds.ascent
                     + fontstruct[currentfont]->max_bounds.descent;
         XDrawString( dpy, pixels, work_gc, x, y, str, strlen(str) );

         /*
          * y ist die Baseline des Strings und
          *     ___NICHT___ die linke obere Ecke der ExposeRegion.
          */
         fakeEvent.x      = x;
         fakeEvent.y      = y - fontstruct[currentfont]->max_bounds.ascent;
         fakeEvent.width  = width;
         fakeEvent.height = height;
         RedrawAct( desk, &fakeEvent );
      }
      return 0;
   }

   /*---------------------------EXIT--------------------------------*/

   if( !strncmp( M_str, MS_Exit, TOKENLEN ) ) {

      /* sollte das Bild schon unsichtbar machen ... */
      XtUnrealizeWidget( toplevel );

      M_send_response( 0 );     /* 0 = nothing pressed, else key- or mouse-press */

      dbgpr("M_server: received EXIT from client, exiting...\n");
      /* damit auch wirklich noch alles geschrieben wird ... */
      usleep( 100000 );
      exit( 0 );
   }

   /*----------------SetMausInterruptMaske--------------------------*/

   if( !strncmp( M_str, MS_Mask, TOKENLEN ) ) {
      if( 1 != sscanf( M_str, MS_Mask, &x1 ) ) {
         printErr( "InterruptMask: Bad # of parameters", M_str );
      } else {
         if( x1 & ~MI_ALLMASK ) {
            char tmpstr[256];

            sprintf( tmpstr, "bad value (%d), ignored...", x1 );
            printErr( "InterruptMask", tmpstr );
            x1 &= MI_ALLMASK;
         }
         MouseIntFlags = x1;

         /* Damit resync'ed sich der Client !!!! */
         M_send_response( 0 );     /* 0 = nothing pressed, else key- or mouse-press */
      }
      return 0;
   }

   /*-----------------------SetMouseCursor--------------------------*/

   if( !strncmp( M_str, MS_Cursor, TOKENLEN ) ) {
      if( 1 != sscanf( M_str, MS_Cursor, &x1 ) ) {
         printErr( "MouseCursor: Bad # of parameters", M_str );
      } else {
         if( x1 < 0 || x1 > MF_MAX+1 ) {
            char tmpstr[256];

            sprintf( tmpstr, "bad value (%d), MF_INVIS assumed...", x1 );
            printErr( "MouseCursor", tmpstr );
            x1 = MF_INVIS;
         }
         XDefineCursor( dpy, XtWindow(desk), cursor[x1] );
      }
      return 0;
   }

   /*--------------------------getch()------------------------------*/

   if( !strncmp( M_str, MS_Wait, TOKENLEN ) ) {
      XtSetSensitive( cont, True );
      waiting = 1;
      return 1;     /* TimeOut wird im ContCB wieder eingefuegt ... */
   }

   /*--------------------------Default:-----------------------------*/

   printErr( "pipe: rcvd unknown command", M_str );
   return 0;
}

#endif /* !PRINTPROTOKOLL */

/*** EOF ************************************************************/


/*********************************************************************
*   grf3mot.c              Hans Groschwitz                23.05.91   *
*                                                                    *
*      Zielgeraetabhaengige Unterprogramme zur                       *
*      Bildschirm-Steuerung unter OSF/Motif                          *
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
*      Motif-Bildschirmsteuerung ausgelagert ............ 21.10.93   *
*      grf3scr.h Protokoll beschleunigt ................. 22.10.93   *
*      MS_Str wandelt ' ' in '0' bei der IPC ............ 03.08.94   *
*      Separater Prozess: xgrf3d (grf3sipc, grf3xsrv) ... 03.08.94   *
*                                                                    *
*   Noch nicht fertig:                                               *
*                                                                    *
*      Momentan wird nicht zwischen screen1 und screen2              *
*      unterschieden.                                                *
*                                                                    *
*   Linux: SignalHandler wird bereits in signal.h genutzt!           *
*      --> MainSignalHandler ...                                     *
*********************************************************************/

#include <stdio.h>
#include <stdlib.h>                                     /* getenv() */
#include <string.h>                                     /* strcpy() */
#include <errno.h>                                    /* fuer errno */
#include <signal.h>                                      /* SIGCHLD */
#define min(a,b) ((a)<(b)?(a):(b))      /* ist nicht in stdlib.h !! */
#define max(a,b) ((a)>(b)?(a):(b))

#define GRFINTERNAL        /* aktiviert die zusaetzlichen externals */
#include "grf3.h"                 /* ... auch: NORMAL, INVERS, EXOR */

#include "grf3m.h"                          /* fuer Maus-Einbindung */
#include "grf3sipc.h"
#include "grf3xsrv.h"
#include "grf3mot.h"                       /* M_pop und M_sResponse */
#include "grf3ipc.h"                       /* grf_pip_*(), C2S, S2C */

/*** Prototypes *****************************************************/

static void M_push( int, char* );
static int  M_sWait( void );
static void motif_show_text( void );

/*** Globals ********************************************************/

void (*_m_isr)() = NULL;    /* FktPointer f. Maus-IntServiceRoutine */
int  _m_servicing_int = 0;

static int current_screen = 0;                         /* show=TEXT */
static int current_color;                            /* LinienFarbe */
static int current_style;                             /* LinienStil */
static int server_flag = 0;         /* Motif: Server und Widget ex. */
/* current_font unnoetig, da immer einer angegeben wird mit p_write */

/*
 * In der Protokoll-HeaderDatei werden diese Werte als Konstanten
 * gesetzt. Bei der Wahl neuer Fonts sollte grf3scr.c mit DEBUG
 * und STANDALONE rekompiliert werden. Der folgende Output kann dann
 * in die HeaderDatei uebernommen werden.
 */
static int fx[MAXFONTS] = { FONT1_X, FONT2_X, FONT3_X, FONT4_X };
static int fy[MAXFONTS] = { FONT1_Y, FONT2_Y, FONT3_Y, FONT4_Y };

/*
 * Debugging:
 * setenv XGRF3D=filename
 * write GrfProtocol to filename in spite of activating xgrf3d
 */
static char *protocol_file = NULL;

/********************************************************************
*   Konvertiert die Farben von Jens' Konvention                     *
*   in die des XGRF-Protokolls ...                                  *
********************************************************************/

static int
ColorConv( int color ) {
   return color;         /* momentan ein Dummy, aber wer weiss ... */
}

/********************************************************************
*   Konvertiert die DrawingStyles von Jens' Konvention              *
*   in die des XGRF-Protokolls ...                                  *
********************************************************************/

static int
StyleConv( int style ) {
   switch( style ) {
      case INVERS: return M_INVERS;
      case EXOR:   return M_EXOR;
      default:     break;
   }
   return M_NORMAL;
}

/********************************************************************
*   SignalHandler, der das leider auch haeufig verwendete           *
*   Abbruchkriterium verwaltet: ^C im aufrufenden xterm!            *
*                                                                   *
*   SIGINT   2   interrupt                                          *
*                                                                   *
********************************************************************/

static void
KeyboardSignalHandler( void ) {
   fprintf( stderr, "*** Xgrf3 client:  ^C  (sending Exit to server) ***\n" );
   fflush( stderr );

   motif_show_text();

   usleep( 100000 );
   exit( -1 );
}

/********************************************************************
*   SignalHandler, der die Hauptabbruchkriterien verwaltet:         *
*   Das Programm xkill, sowie das Motif ALT-F4 bewirken, dass der   *
*   M_server zum Zombie wird (also seinen Status geaendert hat)     *
*   Der Parent bekommt dies mitgeteilt und muss terminieren.        *
*   (Dann verschwindet auch der Zombie ...)                         *
*   Bei einem ^C vom Keyboard aus stirbt der Parent. Das Child (der *
*   M_server) schliesst sich dem freiwillig an !!!                  *
*                                                                   *
*   SIGCHLD  20  child status has changed                           *
*                                                                   *
********************************************************************/

static void
MainSignalHandler( void ) {
   fprintf( stderr, "*** Xgrf3 client: server has died: so do I! ***\n" );
   fflush( stderr );

   /*
    * WICHTIG:
    * Shared Memory wiederfreigeben, sonst ist irgendwann
    * kein Heap mehr frei (macht shutdown noetig) !!!
    */
   if( grf_pip_kill() )
      perror("Xgrf3 client: cannot destroy IPC channels");

   usleep( 100000 );
   exit( -1 );
}

/********************************************************************
*   MouseSignalHandler, erhaelt Signale aufgrund angemeldeter Maus- *
*   Aktionen. Wenn in der Globalen Variablen _m_isr (FktPtr) ein    *
*   Wert != NULL steht, wird der als UserInterruptFunktion inter-   *
*   pretiert und angesprungen.                                      *
*   Das Programm wird durch SIGUSR1 somit nie terminiert !          *
*                                                                   *
*   SIGUSR1   30   user-defined signal 1                            *
*                                                                   *
********************************************************************/

static void
MouseSignalHandler( void ) {
   MOUSE m;

   dbgpr("Xgrf3 client: MouseSignalHandler: before UserISR\n");
   _m_read( &m, 0, 1 );        /* Nicht Warten, Sema zuruecksetzen */

   if( _m_isr ) {
      _m_servicing_int = 1;  /* to avoid grf_m*()-calls within ISR */
      (*_m_isr)( m.lastEvent, m.buttonState, m.xPos, m.yPos );
      _m_servicing_int = 0;
   }
   dbgpr("Xgrf3 client: MouseSignalHandler: after UserISR\n");
}

/********************************************************************
*   Diese Funktion wird nur von grf_show() bei                      *
*   der Umschaltung nach "screenN"                                  *
*   fuer die Bildschirmumlenkung verwendet!                         *
*                                                                   *
*   /com/sh:  setvar -type env DISPLAY wema45:0                     *
*   /bin/sh:  DISPLAY=wema45:0 ; export DISPLAY                     *
*   /bin/csh: setenv DISPLAY wema45:0                               *
********************************************************************/

static void
grf_find_dpy_host( char *buf ) {
   char name[M_STRLEN];
   char *en_name;

   if( (en_name = getenv( "DISPLAY" )) ) {
      strcpy( name, en_name );
      for( en_name = name; *en_name; en_name++ ) {
         if( *en_name == ':' ) {
            /* bei uns gibt es keine Mehrbildschirm-Arbeitsplaetze */
            *en_name = 0;
            break;
         }
      }
   } else {
      *name = 0;                   /* "" Default = eigene Maschine */  
   }
   strcpy( buf, name );
}

/*********************************************************************
*   Motif-Fenster in Grafik-Mode umschalten                          *
*   zur Zeit wird das Device nicht beachtet !!!                      *
*********************************************************************/

static void
M_server( int shmid, SHM *shmp, char *tcpip_addr, int clientpid ) {
   char str[32];

   shmp->clientPid = clientpid;

   if( tcpip_addr )
      strcpy( shmp->tcpip, tcpip_addr );
   else
      *(shmp->tcpip) = 0;

   sprintf( str, "%d", shmid );

   execlp("xgrf3d", "xgrf3d", str, NULL );
}

static void
motif_show_screen( int device, char *tcpip_addr ) {
   int f, clientpid, shmid;
   char *p = NULL;
   SHM *shmp = NULL;

   if( server_flag )
      return;

   protocol_file = NULL;  /* safety first */
   if( p=getenv("XGRF3D") ) {
      if( (f=strlen(p)) > 0 ) {
         protocol_file = malloc( f+1 );
         if( !protocol_file ) {
            perror("grf_show: malloc failed");
            exit( -1 );
         }
         strcpy( protocol_file, p );
      }
   }
   if( protocol_file ) {
      server_flag = 1;
      return;
   }

   if( !(shmp=grf_pip_make(&shmid)) ) {
      perror("grf_show: creating IPC channels failed");
      exit( -1 ); 
   }
   clientpid = getpid();

   if( (f=fork()) == (-1) ) {
      perror("grf_show: fork failed");
      exit( -1 );
   }
   if( !f ) {
      M_server( shmid, shmp, tcpip_addr, clientpid );
      fprintf( stderr, "Xgrf3 server: Unexpected return from M_server()\n" );
      fflush( stderr );
      grf_pip_kill();
      kill( clientpid, SIGTERM );
      exit( -1 );           /* Server sollte eigentlich nie returnen */
   }
   signal( SIGCHLD, MainSignalHandler );       /* wegen xkill und ALT-F4 */
   signal( SIGUSR1, MouseSignalHandler );      /* grf_msetint() ... */
   signal( SIGINT,  KeyboardSignalHandler );   /* ^C */
   M_sWait();

   server_flag = 1;
}

/*********************************************************************
*   Motif-Fenster zurueck in Standard-Mode                           *
*********************************************************************/

static void
motif_show_text( void ) {

   if( !server_flag )
      return;

   M_push( C2S, MS_Exit );

   if( !protocol_file ) {
      signal( SIGCHLD, SIG_DFL );  /* reset signalling ... */
      signal( SIGUSR1, SIG_DFL );
      signal( SIGINT,  SIG_DFL );

      M_sWait();      /* in dieser Zeit baut der WS das Fenster ab. */

      if( grf_pip_kill() ) {
         perror("grf_show: cannot destroy IPC channels");
         exit( -1 ); 
      }
   }
   server_flag = 0;
}

/*********************************************************************
*   Wahl der Bildschirmanzeige                                       *
*********************************************************************/ 

int 
grf_show( char *name ) {
   int device;
   char tcpip_addr[M_STRLEN];

   *tcpip_addr = 0;
   grf_assign( name, &device );
   if( device == 2 || device == 1 ) {       /* SCREEN1 oder SCREEN2 */
      if( current_screen != 1 && current_screen != 2 ) {
         grf_find_dpy_host( tcpip_addr );
         motif_show_screen( device, tcpip_addr );
         current_style = NORMAL;       /* --> grf_open() in grf3a.c */
         current_color = 15;           /* --> grf_open() in grf3a.c */
      }
   } else {                                 /* TEXT oder andere Devices */
      if( current_screen == 1 || current_screen == 2 ) {
         motif_show_text();
      }
   }
   current_screen = device == 1 || device == 2 ? device : 0;
   return device;
}           

/*********************************************************************
*   Loeschen der Ausgabegeraete                                      *
*      "screen1"                                                     *
*      "screen2"                                                     *
*      "text"                                                        *
*                                                                    *
*   --> grf_clr vor grf_show macht nix !!                            *
*   ist das Fenster schon da, wird es aber geloescht !!!             *
*********************************************************************/

void 
grf_clr( char *name ) {
   int device ;
   
   grf_assign( name, &device );
   if ( device == 1 || device == 2 ) {
      /*
       * grf_clr() wirkt NUR im aktuellen screen, da es keine
       * versteckten Screens wie bei der PC-Hercules-Karte gibt !!!
       */
      if( current_screen == device ) {
         if( !server_flag )
            /*
             * --> grf_clr vor grf_show macht nix !!
             * ist das Fenster schon da, wird es aber geloescht !!!
             */
            return;
         M_push( C2S, MS_Clr );
      }
   } else if ( device == 0 ) {                              /* TEXT */
      printf("\n%c", 12);                               /* FORMFEED */
   }
}

/*********************************************************************
*   Warten, wenn Graphikbildschirm aktiv                             *
*********************************************************************/

int 
grf_wait( void ) {
              
   if( current_screen != 1 && current_screen != 2 )
      return( 0 );

   M_push( C2S, MS_Wait );
   return M_sWait();                                   /* Maustaste */
}

/*********************************************************************
*   Zeichnen einer Linie auf dem Bildschirm                          *
*      Startpunkt x1,y1                                              *
*      Endpunkt   x2,y2                                              *
*********************************************************************/

void 
grf_sline( GRF *grfp, int x1, int y1, int x2, int y2 ) {
   char M_str[ M_STRLEN ];
   static int LastLineToX = 0;   /* for MS_More */
   static int LastLineToY = 0;

   if( grfp->dev != 1 && grfp->dev != 2 )
      return;

   if( current_screen != grfp->dev )
      return;

   if( current_style != grfp->pt ) {
      /*
       * spart NUR ein paar Byte Uebertragung
       * Der Server prueft nochmal, ob er wirklich einen neuen Kontext benoetigt
       */
      current_style = grfp->pt;
      sprintf( M_str, MS_Style, StyleConv(current_style) );
      M_push( C2S, M_str );
   }

   if( current_color != grfp->color ) {
      current_color = grfp->color;
      sprintf( M_str, MS_Color, ColorConv(current_color) );
      M_push( C2S, M_str );
   }

   y1 = SCRYMAX - y1;
   y2 = SCRYMAX - y2;
   if( LastLineToX == x1 && LastLineToY == y1 )
      /* Fortfuehrung eines Polygonzugs... */
      sprintf( M_str, MS_More, x2, y2 );
   else
      sprintf( M_str, MS_Line, x1, y1, x2, y2 );
   M_push( C2S, M_str );
   LastLineToX = x2;
   LastLineToY = y2;
}

/*********************************************************************
*   Ausgabe eines Textes auf den Screen                              *
*                                                                    *
*   Parameter:                                                       *
*      x  X-Position in Bildschirmkoordinaten                        *
*      y  Y-Position in Bildschirmkoordinaten                        *
*      str  String                                                   *
*********************************************************************/

void 
grf_twrite( GRF *grfp, int x, int y, char *str, int font ) {
   char gstring[ M_STRLEN ];
   char *p;
   char M_str[ M_STRLEN ];
   static int lastfont = -1;          /* nicht existierender Font ! */

   if( grfp->dev != 1 && grfp->dev != 2 )
      return;

   if( grfp->dev != current_screen )
      /*
       * Auf der Workstation kann dann sowieso nichts gezeichnet werden.
       * Die vergisst auch alles bei screen-wechsel !!!
       * Im Gegensatz zum PC !!! Der schreibt in den gerade nicht
       * dargestellten Bildschirmspeicher.
       */
      return;

   p = gstring;
   for( ; *p = *str++; p++ ) {
      if( *p < 32 || *p >= 127 )
         *p = 32;
      if( *p == '0' )
         *p = 'O';
      if( *p == 32 ) {
         /*
          * HG: 03.08.94
          * Da bei Jens Grafik keine Nullen geschrieben werden,
          * und da die Uebertragung komplett leerer Strings
          * problematisch ist, wird die Null als ErsatzCharacter
          * fuer SPACE genommen !!!
          */
         *p = '0';
      }
   } 

   if( current_style != grfp->pt ) {
      /*
       * Fallunterscheidung muss hier sein, da kein grf_pen() in grf3ibm.c steht
       * speziell bei Linien wird der Netzwerkverkehr so deutlich gesenkt
       *
       * spart NUR ein paar Byte Uebertragung
       * Der Server prueft aber auch nochmal, ob er wirklich
       * einen neuen Kontext benoetigt
       */
      current_style = grfp->pt;
      sprintf( M_str, MS_Style, StyleConv(current_style) );
      M_push( C2S, M_str );
   }

   if( current_color != grfp->color ) {
      current_color = grfp->color;
      sprintf( M_str, MS_Color, ColorConv(current_color) );
      M_push( C2S, M_str );
   }

   if( font != lastfont ) {
      /*
       * eine Weitere Massnahme zur Verringerung des Protokoll-Overheads
       */
      sprintf( M_str, MS_Font, font );
      M_push( C2S, M_str );
      lastfont = font;
   }
   sprintf( M_str, MS_Str, x, (SCRYMAX-y), gstring );
   M_push( C2S, M_str );
}

/*********************************************************************
*   Setzen eines Punktes auf dem Bildschirm                          *
*      Koordinaten x,y                                               *
*********************************************************************/

void 
grf_scrdot( GRF *grfp, int x, int y ) {
   char M_str[ M_STRLEN ];

   if( grfp->dev != 1 && grfp->dev != 2 )
      return;

   if( grfp->dev != current_screen )
      return;

   if( current_style != grfp->pt ) {
      current_style = grfp->pt;
      sprintf( M_str, MS_Style, StyleConv(current_style) );
      M_push( C2S, M_str );
   }

   if( current_color != grfp->color ) {
      current_color = grfp->color;
      sprintf( M_str, MS_Color, ColorConv(current_color) );
      M_push( C2S, M_str );
   }

   sprintf( M_str, MS_Point, x, (SCRYMAX-y) );
   M_push( C2S, M_str );
}

/*********************************************************************
*   Groesse des Bildschirms/Fensters in Pixel                        *
*                                                                    *
*   Auf der Workstation kann dann sowieso nichts gezeichnet werden.  *
*   Die vergisst auch alles bei screen-wechsel !!!                   *
*   Im Gegensatz zum PC !!! Der schreibt in den gerade nicht         *
*   dargestellten Bildschirmspeicher.                                *
*********************************************************************/

int 
grf_scxmax( int dev ) {

   return dev != current_screen ? 1 : SCRXMAX;
}

/********************************************************************/

int 
grf_scymax( int dev ) {

   return dev != current_screen ? 1 : SCRYMAX;
}

/*********************************************************************
*   es gibt 4 Fonts mit den Nummern 0...3                            *
*********************************************************************/

int 
grf_fontx2( int font ) {                        /* halbe Fontbreite */

   return 1 + (fx[font] >> 1);                   /* aufgerundet ... */
}

/********************************************************************/

int 
grf_fonty2( int font ) {                         /* halbe Fonthoehe */

   return 1 + (fy[font] >> 1);                   /* aufgerundet ... */
}

/*********************************************************************
*                                                                    *
*                    Inter Prozess Kommunikation                     *
*                                                                    *
*********************************************************************/

/*********************************************************************
*   Senden eines Strings ueber die Pipe (SHM: kein ERR moeglich)     *
*********************************************************************/

static void
M_push( int dir, char *str ) {
   FILE *fp;

   if( !protocol_file ) {
      grf_pip_push( dir, str );
      return;
   }
   if( dir != C2S ) {
      fprintf( stderr, "Haeh ? S -> C ... woher ?!?\n");
      return;
   }
   if( !(fp=fopen(protocol_file,"a")) ) {
      perror("fopen protocol_file to append");
      exit( -1 );
   }
   fprintf(fp,"%s\n", str );
   fclose(fp);
}

/*********************************************************************
*   Empfangen eines Strings ueber die Pipe, falls vorhanden          *
*   Wird auch vom M_server genutzt ...                               *
*********************************************************************/

int
M_pop( int dir, char *str ) {

   return grf_pip_pop( dir, str );
}

/*********************************************************************
*   Server bestaetigt: "bin fertig"                                  *
*   Wird auch vom M_server genutzt ...                               *
*********************************************************************/

void
M_sResponse( int mousebutton ) {
   char M_str[ M_STRLEN ];

   sprintf( M_str, MS_Ok, mousebutton );
   M_push( S2C, M_str );
}

/*********************************************************************
*   Warten auf Antwort des Servers                                   *
*********************************************************************/

static int
M_sWait( void ) {
   char M_str[ M_STRLEN ];

   if( protocol_file )
      return 1;

   while( 1 ) {
      if( !M_pop( S2C, M_str) ) {
         usleep( 10000 );                              /* 1/100-sec */
         continue;
      }
      if( !strncmp( M_str, MS_Ok, TOKENLEN ) )
         break;
   }
   return atol( M_str + TOKENLEN );
}

/*********************************************************************
*   Pruefe, ob Screen zur Zeit sichtbar ist                          *
*********************************************************************/

int
_m_screenp( void ) {
   if( current_screen != 1 && current_screen != 2 )
      return 0;
   return 1;
}

/*********************************************************************
*   Setzen der InterruptMaske fuer die MausAktionen, die der Client  *
*   mitbekommen soll                                                 *
*********************************************************************/

void
_m_set_int_mask( int mask ) {
   char M_str[ M_STRLEN ];
   int device;

   if( current_screen != 1 && current_screen != 2 )
      return;

   sprintf( M_str, MS_Mask, mask );
   M_push( C2S, M_str );

   M_sWait();                          /* dient der Synchronisation */
}

/*********************************************************************
*   Setzen des neuen MausFontCursors, der innerhalb des desk darge-  *
*   dargestellt werden soll                                          *
*********************************************************************/

void
_m_set_mform( int form ) {
   char M_str[ M_STRLEN ];
   int device;

   if( current_screen != 1 && current_screen != 2 )
      return;

   sprintf( M_str, MS_Cursor, form );
   M_push( C2S, M_str );                            /* asynchron... */
}

/*** EOF ************************************************************/


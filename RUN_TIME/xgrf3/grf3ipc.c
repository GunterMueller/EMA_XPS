#define DEBUGn
/*********************************************************************
*   grf3_ipc.c             Hans Groschwitz                23.05.91   *
*                                                                    *
*      Zielgeraetabhaengige Unterprogramme zur                       *
*      Inter-Prozess-Kommunikation mittels                           *
*      Ringpuffern im UNIX-Shared-Memory                             *
*                                                                    *
*                          Jens Onno Krah                            *
*      grf3ibm.c ........................................ 19.03.90   *
*      Turbo schiss ..................................... 26.04.90   *
*      Reset Point ...................................... 22.05.90   *
*                          Hans Groschwitz                           *
*      grf3apollo.c ..................................... 25.01.91   *
*      grf3M.c .......................................... 17.05.91   *
*      Aufteilung in mehrere Files ...................... 23.05.91   *
*      grf3_ipc.c (grf3_ipc.h) .......................... 23.05.91   *
*      Abgespeckt: nur noch SharedMemory ................ 27.10.93   *
*      Semaphore (gegen Peaks in Grafik) ................ 28.10.93   *
*      MOUSE-Struktur eingebracht ....................... 08.11.93   *
*      Separater Prozess: xgrf3d (grf3sipc, grf3xsrv) ... 03.08.94   *
*********************************************************************/

#include <stdio.h>                            /* FLAGS fuer den shm */
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#include "grf3m.h"                                /* MOUSE-Struktur */
#include "grf3xsrv.h"
#include "grf3sipc.h"
#include "grf3ipc.h"

/*** Defines ********************************************************/

/*** Prototypes *****************************************************/

static int   grf_pip_pop_char( int );
static void  grf_pip_push_char( int, int );
/*extern char *shmat( int, char *, int );*/
static char *pm_alloc( unsigned );
static int   pm_free(  void );
static int   get_BSC(  void );
static int   put_BSC(   int );
static int   get_BCS(  void );
static int   put_BCS(   int );

/*** Globals ********************************************************/

/*
 * flag klappt, weil nur der ParentProzess pm_alloc und pm_free aufruft
 * und weil - solange der KindProzess ex. - daran nichts geaendert wird,
 * also auch beim KindProzess der Wert von flag immer stimmt !
 */
static int flag = 0;            /* "Shared memory available" - Flag */
static int shmid;
static SHM *shmp;

/*********************************************************************
*   Generieren der beiden FIFO-Kanaele, sollte VOR fork() erfolgen   *
*********************************************************************/

SHM*
grf_pip_make( int *shmidp ) {
   if( !flag ) {
      if( !(shmp = (SHM*)pm_alloc( MLEN )) ) {
         return NULL; /* ERR */
      }
      RSC = WSC =                              /* Client <-- Server */
      RCS = WCS = 0;                           /* Client --> Server */

      Msema = BSCsema = BCSsema = 0;           /* ... Wichtig!! ... */

      /*
       * Konzept:
       * MOUSE-Speicher uebergibt aktuelle MausWerte 
       * AUSSCHLIESSLICH von S -> C !!!
       */
      bzero( (char*)&(shmp->m), sizeof(MOUSE) );

      flag = 1;
   }
   *shmidp = shmid;
   return shmp;   /* see grf3mot.c */
}

/*********************************************************************
*   Abbauen der beiden FIFO-Kanaele, darf NICHT schiefgehen !!!      *
*                                                                    *
*   sonst geht dem OS Heap verloren (bis zum naechsten SHUT)         *
*********************************************************************/

int 
grf_pip_kill( void ) {
   if( flag ) {
      if( pm_free() ) {
         return ERR;
      }
      flag = 0;
   }
   return OK;
}

/*********************************************************************
*   Senden eines Strings ueber die Ringpuffer                        *
*                                                                    *
*   Format eines ProtokollStrings: ("%s%c", str, EOS)                *
*********************************************************************/

void
grf_pip_push( int dir, char *str ) {
   int c;

   while( c = *str++ )
      grf_pip_push_char( dir, c );
   grf_pip_push_char( dir, EOS );
}

/*********************************************************************
*   Schreiben eines Characters in die Pipe                           *
*                                                                    *
*   Der Character darf nur ein druckbares Zeichen nach dem           *
*   8bit-ISO-Zeichensatz sein (32-126 und 160-255)                   *
*   ==> keine mehrzeiligen Texte erlaubt                             *
*   TAB wird in Space verwandelt!                                    *
*********************************************************************/

static void
grf_pip_push_char( int dir, int c ) {

   dbgpr1("grf_pip_push_char: got: [%c]\n", c );

   if( c == '\t' )
      c = ' ';     /* speziell fuer die Uebertragung von Strings... */

   /*
    * das heisst, dass ein String keine Sonderzeichen ausser TAB
    * beinhalten darf. CR und NL oder andere werden nicht uebertragen
    * AUSNAHME: EOS = \n (wird fuer die Messageseparation vom Hoehrer
    * genutzt.
    */
   else
   if( c != EOS && (c < ' ' || (c > 127 && c < 160) || c > 255) ) {
      fprintf( stderr, "IPC %s: Bad charcter (%d) not pushed!\n",
         dir == C2S ? "C -> S" : "S -> C", c );
      fflush( stderr );
      return;
   }
   while( (dir == C2S ? put_BCS( c ) : put_BSC( c )) == ERR ) {
      dbgpr1( "IPC %s: WAITING: pipe full...\n",
         dir == C2S ? "C -> S" : "S -> C" );
      usleep( 100 );                               /* 1 msec warten */
   }
}

/*********************************************************************
*   Holen eines einzeiligen Strings aus der Pipe (ohne EOS-Marke)    *
*   (falls einer zu holen ist)                                       *
*                                                                    *
*   return = 0 : EMPTY_PIPE, wie O_NDELAY bei FilePipe...            *
*           >0 : strlen des kompletten geholten ProtokollStrings     *
*                                                                    *
*   Die Ueberpruefung auf BeginOfString ist entfallen, weil davon    *
*   Ausgegangen wird, dass das erste Zeichen, das uebertragen wird,  *
*   Direkt das Token des neuen ProtokollStrings ist !!!              *
*   Daher muss nur auf EndOfString (EOS) geachtet werden!!!          *
*                                                                    *
*   MAN BEGINNT NICHT MITTENDRIN IN EIN LAUFENDES GESPRAECH SICH     *
*   LAUSCHEND EINZUSCHALTEN... !!!                                   *
*********************************************************************/

int
grf_pip_pop( int dir, char *str ) {
   static char buf[2][ STRLEN ];
   static int len = 0;
   int c;

   for( ; (buf[dir][len] = c = grf_pip_pop_char( dir )) != EOS; len++ ) {
      if( c == EMPTY_PIPE ) {
         return EMPTY_PIPE;
      }
   }
   buf[dir][len] = 0;
   c = len;
   len = 0;                                     /* fuer neues Token */
   strcpy( str, buf[dir] );
   return c;
}

/*********************************************************************
*   Holen eines Char aus der Pipe (falls einer zu holen ist)         *
*   HG: 02.08.94: die Funktion kehrt erst zurueck, wenn sie ein      *
*   Zeichen bekommen hat!                                            *
*********************************************************************/

static int 
grf_pip_pop_char( int dir ) {

   return dir == C2S ? get_BCS() : get_BSC();
}

/*********************************************************************
*   Memory Management: shared mem allocieren                         *
*********************************************************************/

static char*
pm_alloc( unsigned len ) {
   int shmflg;
   char  *ptr;

   shmflg = 0600 | IPC_CREAT | IPC_EXCL;
   /*
    * ipc_mode    00400 Read by user
    *             00200 Write by user
    *             00040 Read by group
    *             00020 Write by group
    *             00004 Read by others
    *             00002 Write by others
    * IPC_CREAT 0001000 create entry if key doesn't exist
    * IPC_EXCL  0002000 fail if key exists
    */
   shmid = shmget( IPC_PRIVATE, len, shmflg );
   if( shmid == ERR ) {
      perror("shmget");
      return NULL;
   }
   ptr = (char*)shmat( shmid, NULL, 0 );
   if( ptr == NULL || ptr == (char *)ERR ) {
      perror("shmat");
      return NULL;
   }
   bzero( ptr, len );    /* Initialisiert neuen Speicher mit Nullen */
   return ptr;
}

/*********************************************************************
*   Memory Management: shared mem freigeben                          *
*********************************************************************/

static int 
pm_free( void ) {
   if( shmctl( shmid, IPC_RMID, NULL ) == ERR ) {
      perror("shmctl");
      return ERR;
   }
   return OK;
}

/*********************************************************************
*   Ringpuffer: 4 Funktionen zum Lesen und Schreiben beider Puffer   *
*                                                                    *
*               (Memory-Pipe-Mechanismus)                            *
*********************************************************************/

static int
put_BSC( int c ) {

   SemaWait( BSCsema );
   BSCsema = 1;

   if( RSC == (WSC+1) || (  !RSC && WSC==(BSCLEN-1)  ) ) {
      BSCsema = 0;
      return ERR;
   }
   BSCp[ WSC++ ] = c;
   if( WSC >= BSCLEN )
      WSC = 0;

   BSCsema = 0;
   return OK;
}

/********************************************************************/

static int
put_BCS( int c ) {

   SemaWait( BCSsema );
   BCSsema = 1;

   if( RCS == (WCS+1) || (  !RCS && WCS==(BCSLEN-1)  ) ) {
      BCSsema = 0;
      return ERR;
   }
   BCSp[ WCS++ ] = c;
   if( WCS >= BCSLEN )
      WCS = 0;

   BCSsema = 0;
   return OK;
}

/********************************************************************/

static int
get_BSC( void ) {
   int c;

   SemaWait( BSCsema );
   BSCsema = 1;

   if( RSC == WSC ) {
      BSCsema = 0;
      return EMPTY_PIPE;
   }
   c = BSCp[ RSC++ ];
   if( RSC >= BSCLEN )
      RSC = 0;

   BSCsema = 0;
   return c;
}

/********************************************************************/

int 
get_BCS( void ) {
   int c;

   SemaWait( BCSsema );
   BCSsema = 1;

   if( RCS == WCS ) {
      BCSsema = 0;
      return EMPTY_PIPE;
   }
   c = BCSp[ RCS++ ];
   if( RCS >= BCSLEN )
      RCS = 0;

   BCSsema = 0;
   return c;
}

/********************************************************************/

void
_m_read( MOUSE *mp, int aFlag, int eFlag ) {

   if( !flag )
      /*
       * BUG: keine Fehlermeldung an Aufrufer, wenn noch kein
       * grf_show("screen") erfolgt ist
       */
      return;

   if( aFlag ) {
      SemaWait( Msema );
      Msema = 1;
   }
   /*
    * Komplette Kopie der Struktur auslesen
    * SemaWait() haelt auf!
    * Ein paar Byte mehr lesen, dagegen NICHT !
    */
   *mp = shmp->m;

   if( eFlag )
      Msema = 0;
}

/********************************************************************/

void
_m_write( MOUSE *mp, int aFlag, int eFlag ) {

   if( !flag )
      return;

   if( aFlag ) {
      SemaWait( Msema );
      Msema = 1;
   }
   shmp->m = *mp;

   if( eFlag )
      Msema = 0;
}

/*** EOF ************************************************************/

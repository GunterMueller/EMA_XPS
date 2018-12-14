/*********************************************************************
*   grf3sipc.c             Hans Groschwitz                23.05.91   *
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

#include "grf3xsrv.h"                                      /* MS_Ok */
#include "grf3m.h"                                /* MOUSE-Struktur */
#include "grf3sipc.h"                           /* SHM-Struktur ... */

/*** Prototypes *****************************************************/

/*extern char *shmat( int, char *, int );*/

static void ipc_send( int c );
static int  ipc_recv( void );

/*** Globals ********************************************************/

static int shmid;                 /* die vom Client uebertragene ID */
static SHM *shmp;                /* zeigt auf shared memory Bereich */

/*********************************************************************
*   Server bestaetigt: "bin fertig"                                  *
*   Wird auch vom M_server genutzt ...                               *
*********************************************************************/

void
M_response( int mousebutton ) {
   char *p, M_str[ STRLEN ];

   sprintf( M_str, MS_Ok, mousebutton );
   for( p = M_str; *p; p++ )
      ipc_send( *p );
   ipc_send( EOS );
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
M_read_token( char *str_return ) {
   static char buf[ STRLEN ];
   static int len = 0;
   int c;

   for( ; (buf[len] = c = ipc_recv()) != EOS; len++ ) {
      if( c == EMPTY_PIPE ) {
         return EMPTY_PIPE;
      }
   }
   buf[len] = 0;
   c = len;
   len = 0;                                     /* fuer neues Token */
   strcpy( str_return, buf );
   return c;
}

/*********************************************************************
*   Memory Management: shared mem oeffnen, der vom Client allociert  *
*   worden ist                                                       *
*********************************************************************/

SHM*
ipc_open( int shm_id ) {

   shmid = shm_id;
   shmp = (SHM*)shmat( shmid, NULL, 0 );
   if( shmp == NULL || shmp == (SHM*)ERR ) {
      perror("Xgrf3 server: ipc_open failed");
      exit( -1 );
   }
   return shmp;     /* see grf3xsrv.c */
}

/*********************************************************************
*   Memory Management: shared mem freigeben                          *
*                                                                    *
*   DARF NICHT SCHIEFGEHEN:                                          *
*   sonst geht dem OS Heap verloren (bis zum naechsten SHUT)         *
*********************************************************************/

int 
ipc_close( void ) {
   if( shmctl( shmid, IPC_RMID, NULL ) == ERR ) {
      perror("Xgrf3 server: ipc_close failed");
      return ERR;
   }
   return OK;
}

/*********************************************************************
*   Ringpuffer: Funktionen zum Lesen und Schreiben beider Puffer     *
*********************************************************************/

static void
ipc_send( int c ) {

   while( 1 ) {
      SemaWait( BSCsema );
      BSCsema = 1;

      if( RSC == (WSC+1) || (  !RSC && WSC==(BSCLEN-1)  ) ) {
         BSCsema = 0;

         /* oops, FIFO is full! let's wait a little ... */
         usleep( 100 );
         continue;
      } else {
         /* seems, we're successful ... */
         break;
      }
   }
   BSCp[ WSC++ ] = c;
   if( WSC >= BSCLEN )
      WSC = 0;

   BSCsema = 0;
}

/********************************************************************/

static int 
ipc_recv( void ) {
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

   if( aFlag ) {
      SemaWait( Msema );
      Msema = 1;
   }
   /*
    * Komplette Kopie der Struktur auslesen:
    * SemaWait() haelt auf -- ein paar Byte mehr lesen, dagegen NICHT!
    */
   *mp = shmp->m;

   if( eFlag )
      Msema = 0;
}

/********************************************************************/

void
_m_write( MOUSE *mp, int aFlag, int eFlag ) {

   if( aFlag ) {
      SemaWait( Msema );
      Msema = 1;
   }
   shmp->m = *mp;

   if( eFlag )
      Msema = 0;
}

/*** EOF ************************************************************/



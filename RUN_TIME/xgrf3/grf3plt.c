/*********************************************************************
*   grf3_plt.c             Hans Groschwitz                23.05.91   *
*                                                                    *
*      Zielgeraetabhaengige Unterprogramme zur                       *
*      Plotteransteuerung                                            *
*                                                                    *
*                          Jens Onno Krah                            *
*      grf3ibm.c ........................................ 19.03.90   *
*      Turbo schiss ..................................... 26.04.90   *
*      Reset Point ...................................... 22.05.90   *
*                          Hans Groschwitz                           *
*      grf3apollo.c ..................................... 25.01.91   *
*      Aufteilung in mehrere Files ...................... 23.05.91   *
*      grf3_plt.c ....................................... 23.05.91   *
*      Umstellung auf ioctl() !HARD-Handshake! .......... 04.10.91   *
*      grf3ipc.c (kein Aegis mehr) ...................... 20.10.93   *
*********************************************************************/

#include <stdio.h>                           /* fuer FILE in grf3.h */
#include <fcntl.h>
#include <termio.h>

#include <signal.h>  /* just to test whether compiling on an APOLLO work station */
#ifdef SIGAPOLLO
#define IS_AN_APOLLO
#endif

#define GRFINTERNAL
#include "grf3.h"

/*** Globals ********************************************************/

static int fh;

/*********************************************************************
*   Pause !!!                                                        *
*********************************************************************/

void grf_sleep( unsigned n ) {
   sleep( n );
}

/*********************************************************************
*   Macht momentan nur auf der wema45 Sinn...                        *
*   keine andere Station hat Schnittstellenausgaenge !               *
*********************************************************************/

char*
hp_sio( void ) {
#ifdef IS_AN_APOLLO
   return "/dev/sio3";
#else
   return "/dev/null";
#endif
}

/********************************************************************/

int
hp_line( void ) {
#ifdef IS_AN_APOLLO
   return 3;
#else
   return 0;
#endif
}

/********************************************************************/

int 
hp_cat( int dsc ) {
   char str[80];

   /* assumes PATH being set to find zhp */
   sprintf(str,"zhp virt%d.plt >/dev/null",dsc);
   return system(str);
}

/*********************************************************************
*   Initialisieren der seriellen Schnittstelle                       *
*                                                                    *
*   Die SIO-LINE wird normalerweise genau einmal pro Programmlauf    *
*   geoeffnet.                                                       *
*   Da es nur ein HP7221-Geraet im EMA-Labor gibt, muss diese        *
*   Routine nur genau einmal durchlaufen werden.                     *
*   Ein ios_$close findet nicht statt, d.h. erst beim Terminieren    *
*   des Programms wird der Stream geschlossen !!!                    *
*                                                                    *
*   04.10.91, AH+HG:                                                 *
*   Die SIO laesst sich bei der Umstellung auf ioctl() nicht mehr    *
*   auf HARDWARE-Handshake programmieren. Der scheint aber nicht     *
*   gebraucht zu werden, da immer rechtzeitig der Fuellstand der     *
*   Plotter-internen FIFO vom Programm abgefragt wird.               *
*********************************************************************/

void 
hp_init( int n ) {                                /* n = sio Nummer */
   static int init = 0;
   char *p;
   struct termio args;

#ifdef IS_AN_APOLLO
   switch( n ) {
   case 1:
      p = "/dev/sio1";
      break;
   case 2:
      p = "/dev/sio2";
      break;
   case 3:
      p = "/dev/sio3";
      break;
   default:
      fprintf( stderr, "hp_init: SIO-line does not exist.\n" );
      exit( 1 );
      break;
   }
   if( (fh=open( p, O_RDWR )) == -1 ) {
      perror("hp_init");
      exit( 1 );
   }
   args.c_iflag   = ICRNL | IGNBRK;
   args.c_oflag   = OPOST | ONLCR;
   args.c_cflag   = B2400 | CS7 | CSTOPB | PARENB | CLOCAL | CREAD | LOBLK;
   args.c_lflag   = 0;

   /*
    * Die Special-characters sind nun von Hand angepasst auf die
    * Werte, die durch test3 mit sio_$ erzeugt wurden. ITAE :-(
    */
   args.c_cc[VINTR]    = CINTR;
   args.c_cc[VQUIT]    = 0x1d;    /* von Hand veraendert */
   args.c_cc[VERASE]   = CERASE;
   args.c_cc[VKILL]    = 0x18;    /* von Hand veraendert */
   args.c_cc[VEOF]     = CEOF;
   args.c_cc[VEOL]     = 12;      /* von Hand veraendert */
   args.c_cc[VSWTCH]   = CSWTCH;

   args.c_line = 0;

   ioctl( fh, TCSETA, &args );
#else
   fprintf( stderr, "hp_init: SIO-line initialization failed.\n" );
   exit( 1 );
#endif
}

/*********************************************************************
*   Schliessen des DEVICE-Files zur SIO                              *
*********************************************************************/

void 
hp_exit( void ) {
   if( close( fh ) == -1 ) {
      perror("hp_exit");
      exit( 1 );
   }
}

/*********************************************************************
*   Senden eines Byte                                                *
*********************************************************************/

void 
hp_put( int ch ) {
   char buf[3];

   buf[0] = (char) ch;
   buf[1] = 0;
   if( write( fh, buf, 1 ) == -1 ) {
      perror("hp_put");
      exit( 1 );
   } 
}

/*********************************************************************
*   Holen eines Byte                                                 *
*********************************************************************/

int 
hp_get( void ) {
   char buf[3];

   if( read( fh, buf, 1 ) == -1 ) {
      perror("hp_get");
      exit( 1 );
   } 

   return (int)buf[0];
}

/*** EOF ************************************************************/

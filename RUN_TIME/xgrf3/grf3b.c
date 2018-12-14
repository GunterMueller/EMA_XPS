/**********************************************************
*               grf3b.c       10.03.90                    *
*     von                  JENS ONNO KRAH                 *
*     hp7221-Routinen      Hans Groschwitz    14.02.91    *
*                                                         *
***********************************************************
*                                                         *
*     !!! Jens, bitte nur von Hans veraendern lassen !!!  *
*                                                         *
**********************************************************/

#include <stdio.h>
#include <math.h>
#include <string.h>

#define GRFINTERNAL
#ifndef APOLLO
# include <dos.h>    // fuer Turbo Schiss _stklen !
# include "f:\grf\grf3.h"
#else
# include "grf3.h"
#endif

#define HP_LEER 1158
#define HP_VOLL  200

#define CR        13
#define LF        10
#define ESC       27

#define drawed    32
#define dot        0                

static int pl_n;        /* Frei Bufferspeicher im HP7221 */
static int open_flag = 0;

/********* static functions ******************************/

static void hp_wait( void     );
static void hp_plin( int*     );
static void hp_out ( int      );
static void hp_sbn ( int      );
static void hp_mbn ( int      );
static void hp_mbp ( int, int );
static void hp_pmb ( int, int );
static void hp_pmb1( int, int );
static void hp_mba ( double   );

/**********************************************************
*     Initialisieren des Plotters         grf3a.c         *
**********************************************************/

void hp_plon( int n ) {
   hp_open( n );
   hp_reset();
   hp_speed(8);
   hp_selpen(1);
}

/**********************************************************
*     Schliessen des Plotters             grf3a.c         *
**********************************************************/

void hp_close( void ) {
   if( !open_flag )
      return;
   open_flag = 0;
   hp_wait();

   hp_put(ESC);               /* Plotter-Audienz beendet */
   hp_put('.');
   hp_put(')');
   hp_exit();
}

/**********************************************************
*     Oeffnen des Plotters                hp.c            *
**********************************************************/

void hp_open( int n ) {
   if( open_flag )
      return;
   open_flag = 1;
   pl_n = HP_VOLL;

   hp_init( n );                     /* sio_line oeffnen */

   hp_put(ESC);                 /* Plotter soll zuhoeren */
   hp_put('.');
   hp_put('(');
}

/**********************************************************
*     Ruecksetzen des Plotters            hp.c            *
**********************************************************/

void hp_reset( void ) {
   int minx = 0;
   int miny = 0;
   int maxx = 15200;
   int maxy = 11000;

   if( open_flag )
      hp_wait();

   hp_put(ESC);            /* set Output mode to default */
   hp_put('.');             /* (wahrscheinlich unnoetig) */
   hp_put('M');
   hp_put(':');

   hp_put('~');                         /* reset Plotter */
   hp_put('_');

   hp_out('~');     /* Festlegung der grafischen Grenzen */
   hp_out('W');
   hp_mbp(minx,miny);
   hp_mbp(maxx,maxy);

   hp_out('~');                         /* set grid-size */
   hp_out('S');
   hp_mbp(maxx-minx,maxy-miny);
}

/**********************************************************
*     Leeren des Plotter-Puffers          intern          *
**********************************************************/

static void hp_wait( void ) {
   hp_plin( &pl_n );
   while( pl_n != HP_LEER ) {
      grf_sleep( 1 );
      hp_plin( &pl_n );
   }
}

/**********************************************************
*     Fuellstand des Plotter-Puffers      intern          *
**********************************************************/

static void hp_plin( int *n ) {
   int i, j, ic[20];

   hp_put(ESC);
   hp_put('.');
   hp_put('B');

   for( i = 1; i < 20; i++ ) {
      ic[i] = hp_get();
      if( ic[i] == CR || ic[i] == LF )
         break;          /* wg. Apollo sicherheitshalber */
   }
   *n = 0;
   j = 1;
   while( (ic[j] != CR) && (ic[j] != LF) && ic[j] && (j < 20) ) {
      *n = *n * 10 + ic[j] - '0';
      j++;
   }
}

/**********************************************************
*     Senden eines Bytes (level 2)        intern          *
**********************************************************/

static void hp_out( int ch ) {
   pl_n--;
   while( pl_n < HP_VOLL ) {
      hp_plin( &pl_n );
      grf_sleep( 1 );
   }
   hp_put( ch );
}

/*********************************************************************
  Verwendungszweck
     single-byte number

*********************************************************************/

static void hp_sbn(int n)

{
   if (n < 32)
     n += 64 ;
   hp_out(n) ;
}

/*********************************************************************
  Verwendungszweck
    multiple byte number

*********************************************************************/

static void hp_mbn(int n)

{
  int r ;

  if( n<16 ) {
    hp_out(n+96) ;
    return ;
  }
  if ( n<1024 ) {
    hp_out( (n>>6) + 96 ) ;
    hp_sbn( n & 63 ) ;
    return;
  }
  hp_out( (n>>12) + 96 ) ;
  r  = n & 4095 ;
  hp_sbn( r >> 6 ) ;
  hp_sbn( r & 63 ) ;
}

/*********************************************************************
  Verwendungszweck
     multiple byte pair of numbers

*********************************************************************/

static void hp_mbp(int nx,int ny)

{
  int m,n,r,n1,n2,n3 ;

  n = nx ;
  if ( ny > n )
     n = ny ;
  if(n < 4) {
    hp_out(96+(nx<<2)+ny) ;
    return;
  }
  if(n < 32) {
    hp_out( (nx>>1) + 96) ;
    hp_sbn( ny + (( nx & 1)<<5) ) ;
    return;
  }
  if(n < 256) {
      n1 = nx >> 4 ;
      hp_out( n1 + 96 ) ;
      n2 = n1 & 15 ;
      r = ny >> 6 ;
      hp_sbn( r + (n2<<2) ) ;
      hp_sbn( ny - (r<<6) ) ;
      return;
  }
  if(n < 2048) {
    n1 = nx >> 7 ;
    hp_out(n1+96) ;
    r  = nx - (n1<<7) ;
    n2 = r >> 1 ;
    hp_sbn(n2) ;
    n3 = r - (n2<<1) ;
    r  = ny >> 6 ;
    hp_sbn( r + (n3<<5) ) ;
    hp_sbn( ny - (r<<6) ) ;
    return;
  }  
  n1 = nx>>10 ;
  hp_out(n1+96) ;
  r  = nx & 1023 ;
  hp_sbn( r>>4 ) ;
  n3 = r & 15 ;
  m  = ny>>12 ;
  hp_sbn( m + (n3<<2) ) ;
  r  = ny & 4095 ;
  hp_sbn( r>>6 ) ;
  hp_sbn( r & 63 ) ;
} 

#ifdef wird_nicht_gebraucht

/*********************************************************************
  Verwendungszweck
     pair of multiple byte   
    
*********************************************************************/
 
static void hp_pmb(int nx,int ny)

{ 
  hp_pmb1(nx,64) ;
  hp_pmb1(ny,32) ;
}
 
/*********************************************************************
  Verwendungszweck
     hilsroutine fuer pmb  
    
*********************************************************************/
 
static void hp_pmb1(int nx,int i1)

{
  int n,r;
   
  if( nx < -512 ) {
    nx += 16384 ;
    nx += 16384 ;
/*    doppelte addition um intgrenze zu ueberschreiten   */
    n=nx;
    hp_out( (n>>10) + i1 ) ;
    r  = n & 1023 ;
    hp_out(  (r>>5)  + i1 ) ;
    hp_out( (r & 31) + i1 ) ;
  } else if( nx <  -16 ) {
    nx += 1024 ;
    hp_out((nx>>5)+i1) ;
    hp_out((nx&31)+i1) ;
  } else if( nx < 0 ) {
    nx += 32 ;
    hp_out(nx+i1) ;
  } else if( nx <   16 ) {
    hp_out(nx+i1);
  } else if( nx <  512 ) {
    hp_out((nx>>5)+i1) ;
    hp_out((nx&32)+i1) ;
  } else {
    n=nx;
    hp_out((n>>10)+i1) ;
    r  = n & 1023 ;
    hp_out((r>>5)+i1) ;
    hp_out((r&31)+i1) ;
  }
}

/*********************************************************************
  Verwendungszweck
      multiple byte angle 
    
*********************************************************************/
 
static void hp_mba(double x)

{ 
  int na,na1,na3,nr,np1 ;
 
  if(x < 180.0) {
    np1 = 0 ;
  } else {
    np1 = 8 ;
    x -= 180.0 ;
  }
  na = (int)( x * (16384.0/90.0) ) ;
  na1 = na >> 12 ;
  hp_out(np1+na1+96) ;
  nr = na & 4095 ;
  if( nr ) {
    hp_sbn( nr >> 6 ) ;
    if( (na3= nr & 63) != 0 )
      hp_sbn(na3) ;
    }
}

#endif  /* wird_nicht_gebraucht */

/*********************************************************************
    Modifizieren der Linienart  (Kommando lt)

    hp7221 jetzt 1:1 kompatibel zu hp7475 !!! (HG: 16.1.91)
*********************************************************************/

/*
 * groesste erlaubte Zahl: 31.
 * z.B.:
 *    case 2:
 *       31+31=62; 62 = entspricht Gesamtlaenge in mbn Pixel
 *       --> 50% Linie, weil 31/62 = 50%.
 *    case 3:
 *       28+12=40; 40 = entspricht Gesamtlaenge in mbn Pixel
 *       --> 70% Linie, weil 28/40 = 70%.
 */

void hp_linetype( int pattern, double length ) {      
   hp_out('~'); hp_out('Q');
   switch( pattern ) {
      default:                                /* Vollinie (-1 hier) */
         return;
      case 1:
         hp_sbn(drawed | dot); hp_sbn(31);   /* 31 = maxlen of line */
         break;
      case 2:
         hp_sbn(drawed |  31); hp_sbn(31);
         break;
      case 3:
         hp_sbn(drawed |  28); hp_sbn(12);
         break;
      case 4:
         hp_sbn(drawed |  24); hp_sbn( 3);
         hp_sbn(drawed | dot); hp_sbn( 3);
         break;
      case 5:
         hp_sbn(drawed |  28); hp_sbn( 4);
         hp_sbn(drawed |   4); hp_sbn( 4);
         break;
      case 6:
         hp_sbn(drawed |  20); hp_sbn( 4);
         hp_sbn(drawed |   4); hp_sbn( 4);
         hp_sbn(drawed |   4); hp_sbn( 4);
         break;
   }
   hp_mbn((int)( (0.014 * 0.79885) * hp_plxmax() * length ));
                           /* ^ HG: (optische Anpassung auf HP7475) */
}

/*********************************************************************
     Routine zum Waehlen der Schreibfarbe (0-4)   0-leer
*********************************************************************/
 
void hp_selpen( int ipen ) {
   if( ipen < 0 || ipen > 4 )
      ipen = 1;               /* default */
   hp_out('v');
   hp_sbn(ipen);
}

/*********************************************************************
     Waehlen der Zeichengeschwindigkeit, maximal 36 cm/sec.
*********************************************************************/
 
void hp_speed( int n ) {
   hp_out('~'); hp_out('V');
   if( n > 36 )
      n = 36;
   if( n < 1 ) 
      n = 1;
   hp_sbn(n);
}

/*********************************************************************
      Plot-routine Fahrt mit gehobenen Stift nach nx,ny 
*********************************************************************/
 
void hp_move( int x, int y ) {
   hp_out('p');
   hp_mbp(x,y);
}

/*********************************************************************
      Plot-routine Strichzeichnen nach nx,ny          
*********************************************************************/
 
void hp_draw( int x, int y ) {
   hp_out('q');
   hp_mbp(x,y);
}

/*********************************************************************
     Ausgeben eines ASCII String HP 7221
        12 = FORMFEED, Begin of String, 3 = logisches EOS
*********************************************************************/
 
void hp_print( char *pa ) { 
   hp_out('~'); hp_out('\''); hp_out(12);
   while( *pa )
      hp_out( *pa++ );  
   hp_out(3);
}

/*********************************************************************
      Modifizieren der Schriftart HP 7221
*********************************************************************/
 
void hp_si( double width, double height ) {
   hp_out('~'); hp_out('%');
   hp_mbp((int)(600.0*width),(int)(800.0*height));
}

/*********************************************************************
      Zeichnen eines Punktes
*********************************************************************/
 
void hp_dot( int x, int y ) {
   hp_out('p'); hp_mbp(x,y);
   hp_out('q'); hp_mbp(x,y);
   hp_out('p'); hp_mbp(x,y);
}

/******** EOF *******************************************************/




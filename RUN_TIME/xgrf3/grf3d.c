/****************************************************************************
*             g3d.c           14.01.89                                      *
*                          HANS GROSCHWITZ                                  *
*  grf3d.c   c++       Jens Onno Krah                 09.09.91              *
*  Faktor 4 schneller                                 18.09.91              *
****************************************************************************/

#include <stdio.h>
#include <math.h>
#include <ctype.h>

#define DREI_D

#ifndef APOLLO
#include "f:\grf\grf3.h"
#else
#include "grf3.h"
#endif

/***************************************************************************/

#define  LOWVAL    0.
#define  LOWLEN    1.4142
#define  KORN      2001

#define  DELTAX    (LOWLEN/(KORN-1))

#define  OLD       0
#define  NEW       1
#define  NEVER     2

#define  MINDIST   8
#define  MAXDIST   25
#define  MINDELTA  .001
#define  MAXDELTA  .01

static void glatt(void) ;

/***************************************************************************/

static double  a1,a2,a3 , b1,b2,b3 ;
static double  xoff,yoff ;
static double  x3min,y3min,z3min   ;
static double  x3max,y3max,z3max   ;
static double  (*function)(double x,double y) ;
static double  hmax[ KORN ]        ;
static int     hflag[ KORN ]       ;
static int     himin ;
static int     himax ;
static int     g3d_xvect[ N_VECT ] ;
static int     g3d_yvect[ N_VECT ] ;

/***************************************************************************/

static double
xxsin( double x ) {
   /* setzt voraus, dass der Wertebereich von [0 ; PI/2] gilt */
   return x - x*x*x*((1./2./3.)
              + x*x*((1./2./3./4./5.)
              - x*x*((1./2./3./4./5./6./7.)
              + x*x*((1./2./3./4./5./6./7./8./9.)
              - x*x*((1./2./3./4./5./6./7./8./9./10./11.)
              + x*x*(1./2./3./4./5./6./7./8./9./10./11./12./13.)
)))));
}

static double
xxcos( double x ) {
   /* setzt voraus, dass der Wertebereich von [0 ; PI/2] gilt */
   return 1. - x*x*((1./2.)
             + x*x*((1./2./3./4.)
             - x*x*((1./2./3./4./5./6.)
             + x*x*((1./2./3./4./5./6./7./8.)
             - x*x*((1./2./3./4./5./6./7./8./9./10.)
             + x*x*(1./2./3./4./5./6./7./8./9./10./11./12.)
)))));
}

static double
xsin( double x ) {
   if ( x < M_PI_4 )
      return xxsin( x );
   return xxcos( M_PI_2 - x );
}

static double
xcos( double x ) {
   if ( x < M_PI_4 )
      return xxcos( x );
   return xxsin( M_PI_2 - x );
}

/***************************************************************************/

void g3d_move(GRF *dsc,double x3,double y3,double z3)
{
   double x2,y2 ;

   x2 = a1*x3 + a2*y3 + a3*z3 + xoff ;
   y2 = b1*x3 + b2*y3 + b3*z3 + yoff ;
   grf_move(dsc,x2,y2) ;
}

/***************************************************************************/

void g3d_draw(GRF *dsc,double x3,double y3,double z3)
{
   double x2,y2 ;

   x2 = a1*x3 + a2*y3 + a3*z3 + xoff ;
   y2 = b1*x3 + b2*y3 + b3*z3 + yoff ;
   grf_draw(dsc,x2,y2) ;
}

/****************************************************************************
*   berechnet die Verdeckungen, setzt die hline hoeher                      *
****************************************************************************/

void g3d_hmove(GRF *dsc,double x3,double y3,double z3)
{
   double x2,y2 ;

   himin = KORN ;
   himax = 0 ;
   x2 = a1*x3 + a2*y3 + a3*z3 + xoff ;
   y2 = b1*x3 + b2*y3 + b3*z3 + yoff ;
   grf_move(dsc,x2,y2) ;
}

void g3d_hdraw(GRF *dsc,double x3,double y3,double z3)
{
   double x2,y2,m,y0 ;
   int      i,ilastx2,ix2 ;

   x2 = a1*x3 + a2*y3 + a3*z3 + xoff ;
   y2 = b1*x3 + b2*y3 + b3*z3 + yoff ;
   
   ix2 = (int)( x2 * (1./DELTAX) + .5 ) ;  /* direkt vor oder genau auf x2 */
   if ( ix2 > himax )
      himax = ix2 ;
   if ( ix2 < himin )
      himin = ix2 ;
   if( ix2 < 0 || ix2 >= (KORN-2)) {
      grf_move(dsc,x2,y2) ;
      return ;                                /* faengt erst ausserhalb an */
   }
   ilastx2 = (int)( dsc->xfa * (1./DELTAX) + .5 ) ;
                                            /* direkt vor oder auf dsc->xfa */
   if ( ilastx2 > himax )
      himax = ilastx2 ;
   if ( ilastx2 < himin )
      himin = ilastx2 ;
   if( ilastx2 < 0 || ilastx2 >= (KORN-2)) {
      grf_move(dsc,x2,y2) ;
      return ;                                 /* faengt erst ausserhalb an */
   }

   if( ix2 == ilastx2 ) {
      grf_move(dsc,x2,y2) ;
      return ;                        /* liegt zwischen dem Raster der hmax */
   }

   m  = (y2 - dsc->yfa) / (x2 - dsc->xfa) ;
   y0 = dsc->yfa - m * dsc->xfa ;
   m *= DELTAX ;                                   /*  x(i)=(xmax/imax)*i  */
      
   if (x2 < dsc->xfa) {                               /* zeichnen nach links */
      if( DELTAX*ix2 != x2 ) 
         ix2++ ;                   /* direkt rechts von oder genau auf ix2 */
      for( i = ilastx2 ; i >= ix2 ; i-- ) {
         double   max1 ;
         max1 = m * i + y0 ;
         if ( max1 >= hmax[i] ) {                         /* falls oberhalb */
            hmax[i] = max1 ;
            hflag[i] = NEW ;
         }
      }
   } else {   /*** if( x2 > dsc->xfa ) ***/           /* zeichnen nach rechts */
      if( DELTAX*ilastx2 != dsc->xfa )
         ilastx2++ ;            /* direkt rechts von oder genau auf lastx2 */
      for( i = ilastx2 ; i <= ix2 ; i++ ) {
         double   max1 ;
         max1 = m * i + y0 ;
         if ( max1 >= hmax[i] ) {                         /* falls oberhalb */
            hmax[i] = max1 ;
            hflag[i] = NEW ;
         }     
      }   
   }
   grf_move(dsc,x2,y2) ;
}

/***************************************************************************/

void g3d_write(GRF *dsc,char *str,double x,double y,double z,
                        int verschieb,int font)
{
   g3d_move(dsc,x,y,z) ;
   grf_write(dsc,str,dsc->xfa,dsc->yfa,1,verschieb,font) ;
}

/***************************************************************************/

void g3d_clrhline()
{
   int n ;
   for( n = 0 ; n < KORN ; n++ ) {
      hmax[n] = LOWVAL ;            /* weil in grf_koor() untere Begrenzg. */
      hflag[n]= NEVER ;                        /* als y=0. festgelegt ist. */
   }
}

/***************************************************************************/

void g3d_hline(GRF *dsc)

{
   int n ;

   if ( himin<1 )
      himin = 1 ;
   if ( himax>KORN-2 )
      himax = KORN-2 ;
   for( n=himin ; n<=himax ; n++ ) {
      if( hflag[n] == NEW ) {
         if( hflag[n-1]==NEVER ) { 
            grf_move(dsc, DELTAX*n , hmax[n] ) ;
         } else {
            grf_draw(dsc, DELTAX*n , hmax[n] ) ;
         }
      } else if( hflag[n] == OLD ) {            /**** bisher drawed ****/
         if( hflag[n-1]==NEW ) { 
            grf_draw(dsc, DELTAX*n , hmax[n] ) ;
         } else {
            grf_move(dsc, DELTAX*n , hmax[n] ) ;
         }
      } else {
            grf_move(dsc, DELTAX*n , hmax[n] ) ;
      }
   }
   for( n=himin ; n<=himax ; n++ ) {
      if( hflag[n] == NEW ) {
         hflag[n] = OLD ;
      }
   }
}

/****************************************************************************
*    g3d_koor() legt auf dem Ausgabegeraet ein 3D-Koordinatensystem fest    *
*                                                                           *
*    xfmin,xfmax,yfmin,yfmax        Fensterausschnitt (relativ)             *
*    xmin,xmax,ymin,ymax,zmin,zmax  Wertebereich der KOS-achsen             *
*    xskal,yskal,zskal              Skalierung                              *
*    phix,phiy                      Winkel                                  *
*    xt ,yt ,zt     (int)           Anzahl der Beschriftungseinteilungen    *
*    xfo,yfo,zfo (char *)           Format fuer die Beschriftung            *
*    xsc,ysc,zsc (double)           Scalierungsfaktor fuer die Beschriftung *
****************************************************************************/

int g3d_koor(GRF *dsc,
          double xfmin,double yfmin,double xfmax,double yfmax,
          double xmin,double xmax,double ymin,double ymax,double zmin,double zmax,
          double xskal,double yskal,double zskal,double phix,double phiy,
          int xt,int yt,int zt,char *xfo,char *yfo,char *zfo,
          double xsc,double ysc,double zsc )
{
   double  x0,y0,s1,c1,s2,c2,len ;
   double  deltax,deltay,ydiff ;
   int     i ;
   char    str[64] ;
   
   if( phix < 0. || phix >= 90. || phiy < 0. || phiy >= 90. )     
      return(-1) ;
                       /**** erlaubte Winkelangaben:  [0. ; 90.[  Grad. ****/

   if( xskal <= 0. || yskal <= 0. || zskal <= 0. )
      return(-2) ;
         
/***************************************************************************/

   phix *= (M_PI / 180.) ;
   phiy *= (M_PI / 180.) ;
   s1 = xsin( phix ) ;
   c1 = xcos( phix ) ;
   s2 = xsin( phiy ) ;
   c2 = xcos( phiy ) ;
   x3min = xmin ;
   y3min = ymin ;
   z3min = zmin ;
   x3max = xmax ;
   y3max = ymax ;
   z3max = zmax ;

/***************************************************************************/

   xoff   = xskal * c1 ;
   deltax = xoff + yskal * c2 ;
   yoff   = xskal * s1 + yskal * s2 ;
   deltay = yoff + zskal ;
   if( deltax > LOWLEN * deltay ) {
      x0 = LOWLEN ;
      y0 = deltay * (x0 / deltax) ;
   } else {
      y0 = 1. ;
      x0 = deltax * (y0 / deltay) ;
   }
   xoff *= x0 / deltax ;
   yoff *= y0 / deltay ;
   ydiff = xoff * s1 / c1 ;           /* Faktor .85, damit auch Marken und */
                          /* Beschriftungen ins 2D-clipping-Fenster passen */

   a1 =        - xoff  / (xmax - xmin) * .85 ;
   a2 = (x0    - xoff) / (ymax - ymin) * .85 ;
   a3 = 0. ;
   b1 =        - ydiff / (xmax - xmin) * .85 ;
   b2 = (ydiff - yoff) / (ymax - ymin) * .85 ;
   b3 = (y0    - yoff) / (zmax - zmin) * .85 ;
   
   xoff -= xmin * a1 + ymin * a2 + zmin * a3 ;
   yoff -= xmin * b1 + ymin * b2 + zmin * b3 ;
   
   xoff += .5 * (LOWLEN - x0) ;                        /* mittig zentriert */
   yoff += .5 * (1. - y0) ;
   
/***************************************************************************/

   grf_koor(dsc,xfmin,yfmin,xfmax,yfmax,
                LOWVAL,0.0 ,LOWLEN, 1.,
                0,0,0,0,
                0,0,NULL,NULL,1.,1.) ;
                          /* mit 2D-clipping-Rahmen: ... ,1,1,0,0,0,0, ... */

/************************************************************ KOS zeichnen */

   g3d_move(dsc,x3min,y3max,z3max ) ;
   g3d_draw(dsc,x3min,y3max,z3min ) ;
   g3d_draw(dsc,x3max,y3max,z3min ) ;
   g3d_draw(dsc,x3max,y3min,z3min ) ;

/*********************************************** Beschriftungseinteilungen */

   if( yt > 0 ) {
      if( !yfo )
         yfo = "%.1lf" ;
      for( i = 0 ; i <= yt ; i++ ) {
         len = (double)i / (double)yt ;
         sprintf( str, yfo, (ymin + len * (ymax-ymin)) * ysc ) ;

         g3d_move(dsc,x3max,
                      y3min+(y3max-y3min)* len,
                      z3min ) ;
         g3d_draw(dsc,x3max+(x3max-x3min)* .04,
                      y3min+(y3max-y3min)* len,
                      z3min ) ;
         g3d_write(dsc,str,x3max+(x3max-x3min)* .05,
                      y3min+(y3max-y3min)* len,
                      z3min,7,2 ) ;
      }
   } else {
      g3d_move(dsc,x3max                  ,y3min,z3min ) ;
      g3d_draw(dsc,x3max+.02*(x3max-x3min),y3min,z3min ) ;
      g3d_move(dsc,x3max                  ,y3max,z3min ) ;
      g3d_draw(dsc,x3max+.02*(x3max-x3min),y3max,z3min ) ;
   }

   if( xt > 0 ) {                   /* Anzahl der Beschriftungseinteilungen */
      if( !xfo )
         xfo = "%.1lf" ; /* %2.3E */                       /* Formatstring */
      for( i = xt ; i >= 0 ; i-- ) {
         len = (double)i / (double)xt ;
         sprintf( str, xfo, (xmin + len * (xmax-xmin)) * xsc ) ;
         g3d_move(dsc,x3min+(x3max-x3min)* len,
                      y3max,
                      z3min ) ;
         g3d_draw(dsc,x3min+(x3max-x3min)* len,
                      y3max+(y3max-y3min)*.04,
                      z3min ) ;
         g3d_write(dsc,str,x3min+(x3max-x3min)*(.01+len),
                      y3max+(y3max-y3min)*.05,
                      z3min,5,2 ) ;
      }
   } else {
      g3d_move(dsc,x3max,y3max                  ,z3min ) ;
      g3d_draw(dsc,x3max,y3max+.02*(y3max-y3min),z3min ) ;
      g3d_move(dsc,x3min,y3max                  ,z3min ) ;
      g3d_draw(dsc,x3min,y3max+.02*(y3max-y3min),z3min ) ;
   }

   if( zt > 0 ) {
      if( !zfo )
         zfo = "%.1lf" ;
      for( i = 0 ; i <= zt ; i++ ) {
         len = (double)i / (double)zt ;
         sprintf( str, zfo, (zmin + len * (zmax-zmin)) * zsc ) ;
         if( i ) {
            g3d_move(dsc,x3min,
                      y3max,
                      z3min+(z3max-z3min)* len ) ;
            g3d_draw(dsc,x3min,
                      y3max+(y3max-y3min)* .04,
                      z3min+(z3max-z3min)* len ) ;
         } else if( xt <= 0 ) {
            g3d_move(dsc,x3min,
                      y3max+(y3max-y3min)* .02,
                      z3min+(z3max-z3min)* len ) ;
            g3d_draw(dsc,x3min,
                      y3max+(y3max-y3min)* .04,
                      z3min+(z3max-z3min)* len ) ;
         }
         g3d_write(dsc,str,x3min,
                      y3max+(y3max-y3min)* .05,
                      z3min+(z3max-z3min)*(.01+len),
                      6,2 ) ;
      }
   } else {
      g3d_move(dsc,x3min,y3max                  ,z3max ) ;
      g3d_draw(dsc,x3min,y3max+.02*(y3max-y3min),z3max ) ;
   }
   return(0) ;  /* ok */
}

/****************************************************************************
*   enthaelt das pseudo-3D-clipping der z-Achse                             *
*   x- und y-Achse werden ja durch g3d_zeich() pseudo-geclippt,             *
*   indem sie ausserhalb des darzustellenden Rasters fallen                 *
****************************************************************************/

static double zfkt(double x,double y)

{
   double z ;

   z = (*function)(x,y) ;
   if( z3max > z3min ) {
      if( z > z3max )
         z = z3max ;
      if( z < z3min )
         z = z3min ;
   } else {
      if( z < z3max )
         z = z3max ;
      if( z > z3min )
         z = z3min ;
   }
   return( z ) ;
}

/***************************************************************************/

void g3d_xprint(GRF *dsc,int pen,int gl,int n,
                    int draw,int conn,int mark,double x)   /* 2. Durchgang */
{
   int  i,_draw,_conn,_mark,_vect ;

   _vect = 0 ;
   for( i=0 ; g3d_xvect[i] > 0 ; i++ )
      if( n == g3d_xvect[i] )
         _vect = 1 ;
   /**************************************/
   if( draw > 0 ) {
      _draw = (!(n % draw)) ? 1 : 0 ;
   } else if( draw < 0 ) {
      _draw = _vect ;
      if( draw != -1 ) {
         draw = -draw ;
         _draw |= (!(n % draw)) ? 1 : 0 ;
      }
   } else {
      _draw = 0 ;
   }
   if( n==0 )
      _draw = 1 ;
   /**************************************/
   if( conn > 0 ) {
      _conn = (!(n % conn)) ? 1 : 0 ;
   } else if( conn < 0 ) {
      _conn = _vect ;
      if( conn != -1 ) {
         conn = -conn ;
         _conn |= (!(n % conn)) ? 1 : 0 ;
      }
   } else {
      _conn = 0 ;
   }
   /**************************************/
   if( mark > 0 ) {
      _mark = (!(n % mark)) ? 1 : 0 ;
   } else if( mark < 0 ) {
      _mark = _vect ;
      if( mark != -1 ) {
         mark = - mark ;
         _mark |= (!(n % mark)) ? 1 : 0 ;
      }
   } else {
      _mark = 0 ;
   }
   /**************************************/
   if( pen )
      pen = _draw | _conn | _mark ;
   if( gl && _draw )
      glatt() ;
   if( pen )
      grf_pen(dsc, TAKE) ;
   if( _draw )
      g3d_hline(dsc) ;
   if( n && _conn ) {
      g3d_move(dsc,x,y3max,zfkt(x,y3max) ) ;
      g3d_draw(dsc,x,y3max,z3min         ) ;
   }
   if( n && _mark ) {
      g3d_move(dsc,x,y3max,                        z3min ) ;
      g3d_draw(dsc,x,y3max + .02 * (y3max - y3min),z3min ) ;
   }
   if( n && pen ) 
      grf_pen(dsc, HOME) ;
}

/***************************************************************************/

void g3d_yprint(GRF *dsc,int pen,int gl,int n,
                    int draw,int conn,int mark,double y)   /* 1. Durchgang */
{
   int  i,_draw,_conn,_mark,_vect ;

   for( _vect=i=0 ; g3d_yvect[i] > 0 ; i++ )
      if( n == g3d_yvect[i] )
         _vect = 1 ;
   /**************************************/
   if( draw > 0 ) {
      _draw = (!(n % draw)) ? 1 : 0 ;
   } else if( draw < 0 ) {
      _draw = _vect ;
      if( draw != -1 ) {
         draw = -draw ;
         _draw |= (!(n % draw)) ? 1 : 0 ;
      }
   } else {
      _draw = 0 ;
   }
   if( n==0 )
      _draw = 1 ;
   /**************************************/
   if( conn > 0 ) {
      _conn = (!(n % conn)) ? 1 : 0 ;
   } else if( conn < 0 ) {
      _conn = _vect ;
      if( conn != -1 ) {
         conn = -conn ;
         _conn |= (!(n % conn)) ? 1 : 0 ;
      }
   } else {
      _conn = 0 ;
   }
   /**************************************/
   if( mark > 0 ) {
      _mark = (!(n % mark)) ? 1 : 0 ;
   } else if( mark < 0 ) {
      _mark = _vect ;
      if( mark != -1 ) {
         mark = - mark ;
         _mark |= (!(n % mark)) ? 1 : 0 ;
      }
   } else {
      _mark = 0 ;
   }
   /**************************************/
   if( pen )
      pen = _draw | _conn | _mark ;
   if( gl && _draw )
      glatt() ;
   if( pen )
      grf_pen(dsc, TAKE) ;
   if( n && _mark ) {
      g3d_move(dsc,x3max + .02 * (x3max - x3min),y,z3min ) ;
      g3d_draw(dsc,x3max                        ,y,z3min ) ;
   }
   if( n && _conn ) {
      g3d_move(dsc,x3max,y,z3min         ) ;
      g3d_draw(dsc,x3max,y,zfkt(x3max,y) ) ;
   }
   if( _draw )
      g3d_hline(dsc) ;
   if( n && pen ) 
      grf_pen(dsc, HOME) ;
}

/****************************************************************************
*  Darstellen der 3D-Funktion im 3D-Koordinatensystem                       *
*  Schalter:                                                                *
*     pen = 0 keine Stiftwechsel (screen)                                   *
*     pen = 1 Stift Home bei langen Wartezeiten (plotter) (nur y)           *
*     pen = 2 Stift Home bei langen Wartezeiten (plotter) (nur x)           *
*     pen = 3 Stift Home bei langen Wartezeiten (plotter) (x und y)         *
*     gl  = 0 keine Glaettung                                               *
*     gl  = 1 Glaettung wirkt                                               *
*     gl  = 2 Glaettung wirkt; Ausgabe der Rechenergebnisse von glatt()     *
*     n0  = 0 komplettes Zeichnen der 3D-Funktion                           *
*     n0  > 0 Zeichenbeginn bei Linie ( n0) im 1. Durchgang                 *
*     n0  < 0 Zeichenbeginn bei Linie (-n0) im 2. Durchgang                 *
****************************************************************************/

void g3d_zeich(GRF *dsc,double(*fkt)(double x,double y),
          int pen,int gl,int n0,int n_maxx,int n_maxy,
          int drawx,int drawy,int connx,int conny,int markx,int marky)
{
   double x,y,z,stepx,stepy ;
   int    n,m,penx,peny ;

   penx = pen & 2 ;
   peny = pen & 1 ;
   stepx = (x3max - x3min) / n_maxx ;
   stepy = (y3max - y3min) / n_maxy ;
   function = fkt ;
   if( n0 < 0 )
      goto teil_2 ;     /* dann muss nicht die Hiddenline berechnet werden */

/********************************************************** y-lines ********/

   g3d_clrhline() ;
   
   for( n = n_maxy ; n >= 0 ; n-- ) {  
      y = n * stepy + y3min ;

      g3d_hmove(dsc, x3max, y, zfkt(x3max,y) ) ;
      for (m = n_maxx-1 ; m >= 0 ; m-- ) {
         x = m * stepx + x3min ;
         g3d_hdraw(dsc, x, y, z = zfkt(x,y) ) ;
      }
      if( n0==0 || n <= n0 )
         g3d_yprint(dsc,peny,gl,n,drawy,conny,marky,y) ;
   }

/********************************************************** x-lines ********/
  teil_2 :
   if ( n0 < 0 ) {
      n0 = -n0 ;
   } else {
      n0 = 0 ;
   }

   g3d_clrhline() ;
   g3d_move(dsc,x3max,y3min,z3min ) ;
   g3d_draw(dsc,x3max,y3min,zfkt(x3max,y3min) ) ;     /* zieht Strich hoch */

   for( n = n_maxx ; n >= 0 ; n-- ) {
      x = n * stepx + x3min ;

      g3d_hmove(dsc, x, y3max, zfkt( x, y3max)) ;
      for( m = n_maxy-1 ; m >= 0 ; m-- ) {
         y = m * stepy + y3min ;
         g3d_hdraw(dsc, x , y , z = zfkt(x,y) ) ;
      }   
      if( n0==0 || n<=n0 )
         g3d_xprint(dsc,penx,gl,n,drawx,connx,markx,x) ;
   }
}

/****************************************************************************
*  glaettet bei geringer Rasterfeinheit die unerwuenschten Spitzen weg      *
****************************************************************************/

static void glatt()
{
   int i,imin,imax,di ;
   double deltah,m,y0 ;

   for( i = 0; hmax[i++] == LOWVAL ; ) ;
 rego :
   while( i < (KORN-1) && hmax[i] > LOWVAL && hflag[i] == OLD )
      i++ ;
   for( ; hmax[i+1] >= hmax[i] ; i++ ) {
      if( i >= (KORN-1) || hmax[i] == LOWVAL )
         return ;
      if( hflag[i] == OLD )
         goto rego ;
   }                 /*** 1. MAX ***/
   while( 1 ) {
      imin = imax = i ;
     weitersuchen :
      for( ; hmax[i+1] <= hmax[i] ; i++ ) {
         if( i >= (KORN-1) || hmax[i] == LOWVAL )
            return ;
         if( hflag[i] == OLD )
            goto rego ;
      }       
      if( hmax[i] < hmax[imin] )
         imin = i ; 
      for( ; hmax[i+1] >= hmax[i] ; i++ ) {
         if( i >= (KORN-1) || hmax[i] == LOWVAL )
            return ;
         if( hflag[i] == OLD )
            goto rego ;
      }                  /*** 2. MAX ***/
      di = i - imax ;
      if( di < MINDIST )
         goto weitersuchen ;  
      if( di > MAXDIST )
         continue ;  
      deltah = hmax[imax] - hmax[imin] ;
      if( deltah >= MINDELTA && deltah <= MAXDELTA ) {
                                    /* Gerade durch hmax[imax],hmax[i],NEW */
         m  = (hmax[i] - hmax[imax]) / (DELTAX*(i-imax)) ;
         y0 = hmax[imax] - m * DELTAX * imax ;
         m *= LOWLEN / (KORN-1) ;                  /*  x(i)=(xmax/imax)*i  */
         for( di = imax + 1 ; di < i ; di++ )
            hmax[di] = m*di+y0 ;
      }
   }
}




#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define DREI_D           /*   fr grf3.h   */

#ifndef APOLLO
#include "f:\grf\grf3.h"
#else
#include "grf3.h"
#endif

unsigned _stklen = 32000 ;

#define  SQU(x)  ((x)*(x))

double _fkt(double x,double y)

{
   double f ;

   f = 1./( 1. + SQU(x - 2.5) + SQU(y + 2.5) )
     - 1./( 1. + SQU(x + 2.5) + SQU(y - 2.5) ) ;
   if( f > .5 )
      return(  1. - f  ) ;
   if( f < (-.5) )
      return( (-1.) - f  ) ;
   return(  f  ) ;
}

main()
{
   GRF *dsc ;

   dsc = grf_open("screen") ;
   grf_clr("screen") ;
   grf_show("screen") ;

   g3d_koor(dsc, 0. , 0. , 1. , 1. ,        /* kompletter Bildschirm       */
            -5. , 5. ,                      /* Wertebereich der x-Achse    */
            -5. , 5. ,                      /* Wertebereich der y-Achse    */
            -.5 , .5 ,                      /* Wertebereich der z-Achse    */
            1. , 1. , .5 ,                  /* Skalierung der Achsen       */
            40. , 20. ,                     /* Winkel fuer x und y Achse   */
            10, 10, 10,                     /* Anzahl Beschriftungen       */
            "%.0lf", "%.0lf", "%.1lf",      /* Format der Beschriftung     */
            1. , 1. , 1. ) ;                /* Skaling der Beschriftung    */

   grf_color(dsc,9) ;
   
   g3d_zeich(dsc,_fkt,                      /* Kraterlandschaft            */
             0, 0, 0,   /* kein pen-home, keine Glaettung, komplettes Bild */
             250, 250,                      /* Rastergroesse in x und y    */
              5 ,  5 ,                      /* Netzlinien                  */
             25 , 25 ,                      /* Verbindungslinien           */
             0  , 0   ) ;                   /* Marken                      */

   grf_close(dsc) ;
   grf_wait() ;             /* nach Tastendruck zurueck zum Textbildschirm */
   grf_show("text") ;
}


/**********************************************************
*               test3.c        08.03.87                   *
*     von                  JENS ONNO KRAH                 *
**********************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "grf3.h"

unsigned _stklen = 32000 ;

main()
{
   GRF *dsc1,*dsc2,*dsc3,*dsc4 ;
   double x,s,c ;

   dsc1 = grf_open("screen") ;
   dsc2 = grf_open("screen") ;
   dsc3 = grf_open("screen") ;
   dsc4 = grf_open("screen") ;

   grf_clr("screen") ;
   grf_show("screen") ;
   grf_pen(dsc1,106) ;
   grf_pen(dsc2,106) ;
   grf_pen(dsc3,106) ;
   grf_pen(dsc4,106) ;
   grf_font(dsc1,2) ;
   grf_write(dsc1, grf_ver() ,0.25,0.03,RELATIV,OBERHALB,3) ;  /** Version **/
   grf_koor(dsc1,0.10,0.10,0.45,0.45,0.0,-1.5,2*M_PI,1.5,4,4,8,8,2,2,
            "%.0fPi","%.3f",1.0/M_PI,1.0); 
   grf_koor(dsc2,0.05,0.55,0.45,0.95,0.0,-1.5,2*M_PI,1.5,4,4,8,8,0,0,
            "","",0.0,0.0);
   grf_koor(dsc3,0.55,0.05,0.95,0.45,0.0,-1.5,2*M_PI,1.5,4,4,8,8,0,0,
            "","",0.0,0.0);
   grf_koor(dsc4,0.55,0.55,0.95,0.95,0.0,-1.5,2*M_PI,1.5,4,4,8,8,0,0,
            "","",0.0,0.0); 
   grf_clip(dsc4,0.5,0.5,1.0,1.0,RELATIV) ;

   grf_color(dsc1,9) ;
   grf_color(dsc2,10) ;
   grf_color(dsc3,11) ;
   grf_color(dsc4,12) ;
   
   grf_move(dsc1,0.0,0.0);
   grf_move(dsc2,0.0,0.0);
   grf_move(dsc3,0.0,0.0);
   grf_move(dsc4,0.0,0.0);
     /* an den ursprung fahren */

   for ( x=0.0 ; x<= (2. * M_PI) ; x+=0.005 ) {
      s = sin(x) ;
      c = cos(x) ;
      grf_draw(dsc1,x,s) ;
      grf_draw(dsc2,x,c) ;
      if ( c != 0 ) {
         grf_draw(dsc3,x,s/c) ;
      } else {
         grf_draw(dsc3,x,1e10) ;
      }
      if ( s != 0 ) {
         grf_draw(dsc4,x,c/s) ;
      } else {
         grf_draw(dsc4,x,1e10) ;
      }
   }
   grf_close(dsc1) ;
   grf_close(dsc4) ;
   grf_close(dsc3) ;
   grf_close(dsc2) ;
   grf_wait() ;
   grf_show("text") ;
   grf_error(1) ;
   return(0) ;
}


/***************************************************************************/
/*      KLED.C          Hans Groschwitz             09.11.93               */
/*                                                                         */
/*      Kennlinien-Editor fuer die FVA                                     */
/***************************************************************************/

#include <stdio.h>
#include "grf3.h"
#include "grf3m.h"

/***************************************************************************/

#define BLACK     15
#define RED       12
#define WHITE     0

#define XMIN      80
#define XMAX      860
#define YMIN      40
#define YMAX      640
#define XHALF     (XMIN+((XMAX-XMIN)/2))
#define YHALF     (YMIN+((YMAX-YMIN)/2))

#define x_cv2i(a) ((int)(XMIN+(double)(XMAX-XMIN)*(double)(a)))
#define y_cv2i(a) ((int)(YMIN+(double)(YMAX-YMIN)*(double)(a)))
#define x_cv2d(a) ((double)((a)-XMIN)/(double)(XMAX-XMIN))
#define y_cv2d(a) ((double)((a)-YMIN)/(double)(YMAX-YMIN))

#define point_draw(dsc,x,y) box_draw(dsc,x,y,8,8)
#define in_point(xm,ym,xp,yp) in_box(xm,ym,xp,yp,8,8)
#define in_area(xm,ym,xa,ya,xe,ye) (((xm)>(xa))&&((xm)<(xe))&&((ym)>(ya))&&((ym)<(ye)))

/***************************************************************************/

GRF*
m_init( void ) {
   GRF *dsc;

   grf_show("screen1");
   if( grf_minit() ) {
      grf_show("text");
      puts("MOUSE not installed ?") ;
      exit( 1 );
   }
   dsc = grf_open("screen1");
   grf_koor( dsc, 0., 0., 1., 1., 0., 0.,
      (double)grf_scxmax(dsc->dev), (double)grf_scymax(dsc->dev),
      1,1,0,0,0,0, NULL, NULL, 1., 1. );
   return dsc;
}

/***************************************************************************/

void
m_exit( GRF *dsc ) {

   grf_close( dsc );
   grf_mexit();
   grf_show("text");
}

/***************************************************************************/

in_box( int xMouse, int yMouse,
        int xCenter, int yCenter, int width, int height ) {
   int xa, ya, xe, ye;

   xa = xCenter - (width >> 1);
   xe = xa + width;
   ya = yCenter - (height >> 1);
   ye = ya + height;

   return in_area( xMouse, yMouse, xa, ya, xe, ye );
}

/***************************************************************************/

void
box_draw( GRF *dsc, int xCenter, int yCenter, int width, int height ) {
   int xa, ya, xe, ye;

   xa = xCenter - (width >> 1);
   xe = xa + width;
   ya = yCenter - (height >> 1);
   ye = ya + height;

   grf_move( dsc, (double)xa, (double)ya );
   grf_draw( dsc, (double)xa, (double)ye );
   grf_draw( dsc, (double)xe, (double)ye );
   grf_draw( dsc, (double)xe, (double)ya );
   grf_draw( dsc, (double)xa, (double)ya );
   grf_flush( dsc );
}

/***************************************************************************/

void
refreshKL( GRF *dsc, int nv, int *xv, int *yv ) {
   int i, x, y, xalt, yalt;

   for( i = 0; i < nv; i++ ) {
      x = xv[i];
      y = yv[i];
      point_draw( dsc, x, y );
      if( i ) {
         grf_move( dsc, xalt, yalt );
         grf_draw( dsc, x, y );
      }
      xalt = x;
      yalt = y;
   }
   grf_flush( dsc );
}

/***************************************************************************/

void
refreshKOS( GRF *dsc, int readonly, char *Ordinate, char *Abszisse ) {

   /*** KnopfLeiste ***/
   box_draw( dsc,  45, YMAX+40, 50, 30 );
   grf_write( dsc, "Quit",  45, YMAX+40, REEL, ZENTRIERT, 2 );

   if( !readonly ) {
      box_draw( dsc, 100, YMAX+40, 50, 30 );
      grf_write( dsc, "Save", 100, YMAX+40, REEL, ZENTRIERT, 2 );
   }

   box_draw( dsc, 155, YMAX+40, 50, 30 );
   grf_write( dsc, "Load", 155, YMAX+40, REEL, ZENTRIERT, 2 );

   /*** KoordinatenSystem ***/
   grf_move( dsc, XMIN, YMIN-10 );
   grf_draw( dsc, XMIN, YMAX+10 );

   grf_move( dsc, XMIN-10, YMIN );
   grf_draw( dsc, XMAX+10, YMIN );
   grf_write( dsc, "0", XMIN-15, YMIN-15, REEL, ECKE_RE_OB, 2 );

   /** Ordinate **/
   grf_move( dsc,  XMIN-10, YMAX );
   grf_draw( dsc,  XMIN+10, YMAX );
   grf_write( dsc, "1", XMIN-15, YMAX, REEL, LINKSB, 2 );

   grf_move( dsc,  XMIN-10, YHALF );
   grf_draw( dsc,  XMIN+10, YHALF );
   grf_write( dsc, "0,5", XMIN-15, YHALF, REEL, LINKSB, 2 );

   /* Pfeil */
   grf_move( dsc, XMIN-15, YMAX-100 );
   grf_draw( dsc, XMIN-15, YMAX- 40 );
   grf_draw( dsc, XMIN-20, YMAX- 45 );
   grf_move( dsc, XMIN-15, YMAX- 40 );
   grf_draw( dsc, XMIN-10, YMAX- 45 );
   grf_write( dsc, Ordinate, XMIN-5, YMAX-115, REEL, LINKSB, 2 );

   /** Abszisse **/
   grf_move( dsc, XMAX, YMIN-10 );
   grf_draw( dsc, XMAX, YMIN+10 );
   grf_write( dsc, "1", XMAX, YMIN-15, REEL, UNTERHALB, 2 );

   grf_move( dsc, XHALF, YMIN-10 );
   grf_draw( dsc, XHALF, YMIN+10 );
   grf_write( dsc, "0,5", XHALF, YMIN-15, REEL, UNTERHALB, 2 );

   /* Pfeil */
   grf_move( dsc, XMAX-120, YMIN-15 );
   grf_draw( dsc, XMAX- 60, YMIN-15 );
   grf_draw( dsc, XMAX- 65, YMIN-20 );
   grf_move( dsc, XMAX- 60, YMIN-15 );
   grf_draw( dsc, XMAX- 65, YMIN-10 );
   grf_write( dsc, Abszisse, XMAX-135, YMIN-15, REEL, ECKE_RE_OB, 2 );
}

/***************************************************************************/

void
redraw_lines( GRF *dsc, int i, int nv, int *xv, int *yv ) {

   if( i != 0 ) {
      grf_move( dsc, xv[i-1], yv[i-1] );
      grf_draw( dsc, xv[i  ], yv[i  ] );
   }
   if( i != (nv-1) ) {
      grf_move( dsc, xv[i+1], yv[i+1] );
      grf_draw( dsc, xv[i  ], yv[i  ] );
   }
   grf_flush( dsc );
}

/***************************************************************************/

void
press_in_region( int *xp, int *yp, int i, int nv, int *xv, int *yv ) {
   int x, y, tmp;

   x = *xp;
   y = *yp;

   if( !nv || nv == 1 ) {
      if( x < (XMIN-30) )
         x = XMIN-30;
      if( x > (XMAX+30) )
         x = XMAX+30;
   } else if( !i ) {
      if( x < (XMIN-30) )
         x = XMIN-30;
      else if( x > (tmp=xv[1]) )
         x = tmp;
   } else if( i >= (nv-1) ) {      /* load(): i == nv !!! */
      if( x > (XMAX+30) )
         x = XMAX+30;
      else if( x < (tmp=xv[i-1]) )
         x = tmp;
   } else if( i > 0 && i < (nv-1) ) {
      if( x > (tmp=xv[i+1]) )
         x = tmp;
      else if( x < (tmp=xv[i-1]) )
         x = tmp;
   }

   if( y < (YMIN-25) )
      y = YMIN-25;
   if( y > (YMAX+25) )
      y = YMAX+25;

   *xp = x;
   *yp = y;
}

/***************************************************************************/

void
save( char *progName, char *fileName, char *Abszisse, char *Ordinate,
      int nv, int *xv, int *yv ) {
   FILE *fp;
   int i;

   if( !(fp=fopen( fileName, "w" )) ) {
      fprintf( stderr, "%s: outputfile %s cannot be written\n",
         progName, fileName );
      return;
   }
   fprintf( fp, "%s\n%s\n", Ordinate, Abszisse );
   for( i = 0; i < nv; i++ )
      fprintf( fp, "%lf %lf\n", x_cv2d( xv[i] ), y_cv2d( yv[i] ) );
   fclose( fp );
}

/***************************************************************************/

int
load( char *progName, char *fileName, char *Abszisse, char *Ordinate,
      int *nvp, int *xv, int *yv ) {
   int readonly = 0, nv = 0;
   int i, x, y;
   FILE *fp;
   char str[1024];

   if( !(fp=fopen( fileName, "a" )) ) {
      readonly = 1;
   } else {
      fclose( fp );
   }
   if( !(fp=fopen( fileName, "r" )) ) {
      fprintf( stderr, "%s: inputfile %s does not exist\n",
         progName, fileName );
      *Abszisse = *Ordinate = *nvp = 0;
      return 0;
   }
   if( readonly )
      fprintf( stderr, "%s: inputfile %s opened READONLY\n",
         progName, fileName );

   fgets( str, 1024, fp );
   for( i = 0; str[i] >= ' ' && i < 15; i++ )
      Ordinate[i] = str[i];
   Ordinate[i] = 0;

   fgets( str, 1024, fp );
   for( i = 0; str[i] >= ' ' && i < 15; i++ )
      Abszisse[i] = str[i];
   Abszisse[i] = 0;

   while( !feof( fp ) ) {
      double xtmp, ytmp;
      int xmin = 0;

      *str = 0;
      if( !fgets( str, 1024, fp ) )
         break;
      if( 2 != sscanf( str, "%lf %lf", &xtmp, &ytmp ) )
         break;                       /* letzte Zeile war unvollstaendig... */

      x = x_cv2i( xtmp );
      y = y_cv2i( ytmp );
      press_in_region( &x, &y, nv, nv, xv, yv );
      xv[nv] = x;
      yv[nv] = y;

      if( (++nv) > 1023 )
         break;           /* letzte Stuetzpunkte werden ignoriert */
   }
   fclose( fp );
   *nvp = nv;
   return readonly;
}

/***************************************************************************/

main( int argc, char **argv ) {
   int oldstate = 0, state, x, y;
   int readonly = 0;
   GRF *dsc;
   char str[128], Ordinate[16], Abszisse[16];
   int xv[1024];
   int yv[1024];
   int i, nv = 0;
   int lastp, lastx, lasty;

   if( argc != 2 ) {
      fprintf( stderr, "Usage: %s filename\n", argv[0] );
      exit( -1 );
   }
   readonly = load( argv[0], argv[1], Abszisse, Ordinate, &nv, xv, yv );

   /*** Grafisches Display oeffnen ***/

   dsc = m_init();
   refreshKOS( dsc, readonly, Ordinate, Abszisse );
   refreshKL( dsc, nv, xv, yv );

   /*** XtMainLoop *******************/

   lastp = -1;
   while( 1 ) {
      usleep( 1000 );  /* soll nur jede MilliSek einmal pollen */

      grf_mquery( &state, &x, &y );

      if( state == MS_LEFTB ) {
         if( oldstate != MS_LEFTB ) {
            if       ( in_box( x, y,  45, YMAX+40, 50, 30 ) ) {  /* Quit */
               break;
            } else if( !readonly &&
                       in_box( x, y, 100, YMAX+40, 50, 30 ) ) {  /* Save */

               /*puts("Save pressed");*/

               save( argv[0], argv[1], Abszisse, Ordinate, nv, xv, yv );

            } else if( in_box( x, y, 155, YMAX+40, 50, 30 ) ) {  /* Load */

               /*puts("Load pressed");*/

               grf_clr("screen1");
               readonly = load( argv[0], argv[1], Abszisse, Ordinate, &nv, xv, yv );

               refreshKOS( dsc, readonly, Ordinate, Abszisse );
               refreshKL( dsc, nv, xv, yv );
            }
            for( i = 0; i < nv; i++ ) {
               if( in_point( x, y, xv[i], yv[i] ) ) {

                  /*** Point pressed ***/

                  grf_color( dsc, WHITE );
                  redraw_lines( dsc, i, nv, xv, yv );
                  point_draw( dsc, xv[i], yv[i] );

                  grf_color( dsc, RED );
                  redraw_lines( dsc, i, nv, xv, yv );

                  sprintf( str, "%.1f%% / %.1f%%",
                     x_cv2d( xv[i] )*100., y_cv2d( yv[i] )*100. );
                  grf_write( dsc, str, 220, YMAX+40, REEL, RECHTSB, 2 );

                  lastp = i;
                  lastx = x;
                  lasty = y;
                  break;
               }
            }
            oldstate = MS_LEFTB;
         } else {
            if( lastp != -1 && (x != lastx || y != lasty) ) {
               lastx = x;
               lasty = y;

               /*** Point moved ***/

               press_in_region( &x, &y, i, nv, xv, yv );

               grf_color( dsc, WHITE );
               redraw_lines( dsc, i, nv, xv, yv );

               grf_write( dsc, str, 220, YMAX+40, REEL, RECHTSB, 2 );

               xv[i] = x;
               yv[i] = y;

               grf_color( dsc, RED );
               redraw_lines( dsc, i, nv, xv, yv );

               sprintf( str, "%.1f%% / %.1f%%",
                  x_cv2d( xv[i] )*100., y_cv2d( yv[i] )*100. );
               grf_write( dsc, str, 220, YMAX+40, REEL, RECHTSB, 2 );
            }
         }
      } else if( state == MS_RIGHTB ) {
         if( oldstate != MS_RIGHTB &&
             in_area( x, y, XMIN-30, YMIN-25, XMAX+30, YMAX+25 ) ) {
            int xp;

            for( i = 0; i < nv; i++ ) {
               if( x == (xp=xv[i]) ) {
                  /*puts("do not add");*/

                  goto no_add;
               } else if( x < xp ) {
                  if( !i ) {
                     /*printf("adding Point left to point 0\n");*/
                     for( i = nv-1; i >= 0; i-- ) {
                        xv[i+1] = xv[i];
                        yv[i+1] = yv[i];
                     }
                     xv[0] = x;
                     yv[0] = y;
                  } else {
                     int tmp = i;
                     /*printf("adding Point between %d and %d\n", i-1, i );*/

                     grf_color( dsc, WHITE );
                     grf_move( dsc, xv[i-1], yv[i-1] );
                     grf_draw( dsc, xv[i  ], yv[i  ] );

                     for( i = nv-1; i >= tmp; i-- ) {
                        xv[i+1] = xv[i];
                        yv[i+1] = yv[i];
                     }
                     xv[tmp] = x;
                     yv[tmp] = y;

                     grf_color( dsc, BLACK );
                  }
                  break;
               }
            }
            if( i == nv ) {             /* nothing found ! */
               /*printf("adding Point right to point %d\n", nv - 1 );*/
               xv[nv] = x;
               yv[nv] = y;
            }
            nv++;
            refreshKOS( dsc, readonly, Ordinate, Abszisse );
            refreshKL( dsc, nv, xv, yv );

         no_add:
            oldstate = MS_RIGHTB;
         }
      } else if( state == MS_MIDB ) {
         if( oldstate != MS_MIDB ) {
            for( i = 0; i < nv; i++ ) {
               if( in_point( x, y, xv[i], yv[i] ) ) {

                  /*** Remove Point ***/

                  grf_color( dsc, WHITE );
                  redraw_lines( dsc, i, nv, xv, yv );
                  point_draw( dsc, xv[i], yv[i] );

                  nv--;
                  for( ; i < nv; i++ ) {
                     xv[i] = xv[i+1];
                     yv[i] = yv[i+1];
                  }

                  grf_color( dsc, BLACK );
                  refreshKOS( dsc, readonly, Ordinate, Abszisse );
                  refreshKL( dsc, nv, xv, yv );
                  break;
               }
            }
            oldstate = MS_MIDB;
         }
      } else {
         oldstate = 0;
         if( lastp >= 0 ) {
            /* lastp wird nur im Zusammenhang mit LEFTB veraendert */

            /*** Point released ***/

            grf_color( dsc, WHITE );
            redraw_lines( dsc, i, nv, xv, yv );

            grf_write( dsc, str, 220, YMAX+40, REEL, RECHTSB, 2 );

            grf_color( dsc, BLACK );
            refreshKOS( dsc, readonly, Ordinate, Abszisse );
            refreshKL( dsc, nv, xv, yv );
         }
         lastp = -1;
      }
   }
   m_exit( dsc );
}

/***************************************************************************/


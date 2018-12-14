/*******************************************************************
*               grf3a.c                10.03.90                    *
*     von                  JENS ONNO KRAH                          *
*     to_hp -> hp_put ...               15.02.91                   *
*     magic + ltype + dot_draw          20.08.91                   *
*     grf_err                           20.08.91                   *
*     Clipping FÅr Text  RudimentÑr !   06.09.91                   *
*     closeall                          06.09.91                   *
*     grf_clip                          11.09.91                   *
*     grf_font                          12.09.91                   *
*     dx/dy grf_koor                    25.09.91                   *
*     autoscaling fuer den Plotter      27.09.91                   *
*     WORD 5.0 .... toupper             18.10.91                   *
*     Turbo Schiss _fpreset             25.02.92                   *
*     PD x,y,x,y,x,y,x,y                07.07.92                   *
*     grf_color                         16.09.93                   *
*     grf_invtrafo                      24.11.93                   *
*     pow durch bitpopeln ersetzt       03.08.94                   *
*     ESC . ( auch in virtuellen Files  16.08.94                   *
*     mit *.fig und *.tex von HG+PB     13.06.96                   *
*******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#define MAGIC1 841085L
#define MAGIC2 840920L

#define  GRFINTERNAL

#ifndef APOLLO
# include <dos.h>    /* fuer Turbo Schiss _stklen !    */
# include "f:\grf\grf3.h"
#else
# include "grf3.h"
#endif

static GRF *root = (GRF*) 0 ;
static int grf_err = 0 ;                  /* kein Fehler !  */

static int grf_din = 4 ;                  /* Format des Plotters   */
static double plxmax = (double) PLXMAX ;
static double plymax = (double) PLYMAX ;
static double psxmax = (double)(PSXMAX * 10.) ;
static double psymax = (double)(PSYMAX * 10.) ;
static double ptxmax = (double) PTXMAX ;
static double ptymax = (double) PTYMAX ;
static double pfxmax = (double) PFXMAX ;
static double pfymax = (double) PFYMAX ;

static int  grf_free(GRF *) ;
static void dot_draw(GRF *,int,int,int,int) ;
static void grf_pline(GRF *,int,int,int,int) ;
static void i_move(GRF *,int,int) ;
static void i_draw(GRF *,int,int) ;
static void clip_draw(GRF *,double,double) ;
static void p_write(GRF *,char *,int,int,int,int) ;
static int  fcat(char *, char *) ;
static int  grf_xmax(int device) ;
static int  grf_ymax(int device) ;

/********************************************************************
*     Ausgabe der Version                                           *
********************************************************************/

char *grf_ver()
{
   return("grf 3.32   19.09.93") ;
}

/********************************************************************
*     Ausgabe einer eventuellen Fehlermeldung                       *
*    
********************************************************************/

int grf_error(int flag) 
{  
   int i,si,err ;
   static char *err_msg[16] = {
      "malloc error" ,                          /*  1 */
      "descriptor not valid" ,                  /*  2 */
      "fopen error" ,                           /*  4 */
      "missing koor" ,                          /*  8 */
      "koor: Parameters not possible" ,         /* 16 */
      "koor: can not open TEXT mode" ,          /* 32 */
      "" ,
      "" ,
      "" ,
      "" ,
      "" ,
      "" ,
      "" ,
      "" ,
      ""
   } ;
   
   err = grf_err ;
   grf_err = 0 ;
   if ( flag ) {
      if ( err ) {
         for(i=0;i<15;i++) {
            si = 1<<i ;
            if ( si & err )
               printf("GRF: %s\n",err_msg[i]) ;
         }
      } else if ( flag == 2 ) {
         puts("GRF: no error") ;
      }
   }
   return(err) ;
}

/********************************************************************
*       Initialisierung der Ausgabegeraete                          *
*       Argument :  screen     ->  Bildschirm                       *
*                   hpgl   (4) ->  com1: hpgl Plotter               *
*                   hp7221 (3) ->  com1: hp7221 Plotter             *
*                   *.eps  (6) ->  default Drive eps-File           *
*                   andere (5) ->  default Drive *.plt = hpgl-File  *
*                   *.tex  (7) ->  default Drive *.tex = tex-File   *
*                   *.fig  (8) ->  default Drive *.fig = xfig-File  *
*                                                                   *
*       alle Strings kînnen mit set screen=neu.plt um-              *
*       geleitet werden !                                           *
*                                                                   *
********************************************************************/

static int dsc_count = 0 ;

GRF *grf_open(char name[])

{
   int     device,in_use,din_flag = 0 ;
   char    file[80],str[80],*en_name,*hilf ;
   long t ;
   GRF *grfp,*grfi ;

   grfp = (GRF *)calloc(1,sizeof( GRF ) ) ;
   if ( ! grfp  ) {
      grf_err |= 1 ;
      return((GRF *)0) ;
   }
   grf_assign(name,&device) ;
   if ( device==0 ) {
      grf_err |= 32 ;
      return((GRF *)0) ;
   }
   strcpy(grfp->name,name) ;
   grfp->next  = (GRF *) 0 ;
   grfp->dsc   = ++dsc_count ;
   grfp->font  = 1 ;                /*  Schriftgroesse   */
   grfp->color = 15 ;               /*  Default WHITE    */
   grfp->magic = MAGIC1 ;
   if ( root ) {
      grfi = root ;
      while ( grfi->next ) {
         grfi = grfi->next ;
      }
      grfi->next = grfp ;
   } else {
      root = grfp ;  /* Erster Block */
   }
   in_use = 0 ;
   grfp->fi_st = 0 ;

   if ( device > 2 ) {
      grfi = root ;
      while ( grfi ) {
         if ( strcmp(grfp->name,grfi->name) == 0 &&
              grfi->dsc != grfp->dsc )
            in_use = 1 ;      /*  Virtuelles File Anlegen */
         grfi = grfi->next ;
      }
   }
   strcpy(str,grf_assign(name,&device)) ;
   en_name = getenv("DIN") ;
   if ( NULL != en_name ) {
      sscanf(en_name,"%d",&grf_din) ;
      din_flag = 1 ;
   }
   if ( grf_din < 0 ) {
      grf_din = 0 ;
   } else if ( grf_din > 6 ) {
      grf_din = 6 ;
   }
   {
      double faktor ;
   
      faktor = (double)(2 << (grf_din>>1) ) ;
      if ( grf_din & 1 )
         faktor *= 1.414213562 ;
      plxmax = (double)( PLXMAX * 4L ) / faktor ;
      plymax = (double)( PLYMAX * 4L ) / faktor ;
   }

   grfp->del  = 0.000001 ;

   grfp->vs   = 0.0 ;
   grfp->aflg = 0 ;

   if ( device>=3 && device<=5 && in_use ) {  /* virtuelles File ôffnen */
      sprintf(file,"virt%d.plt",grfp->dsc) ;
      if( (grfp->fp=fopen(file,"w"))==0 ) {
         printf("Datei %s kann nicht geîffnet werden !\n",file);
         grf_err |= 4 ;
         return((GRF *)0) ;
      }
      grfp->fi_st = 1 ;
      device = 5 ;
   } else {
      if ( device==3 ) {                /*  HP 7221 ôffnen   */
          hp_plon(hp_line()) ;
      } else if ( device==4 ) {         /*  HP-GL   ôffnen   */
         if( (grfp->fp=fopen(hp_sio(),"w"))==0 ) {
            grf_err |= 4 ;
            printf("Schnittstelle kann nicht geîffnet werden !\n");
            return((GRF *)0) ;
         }
      } else if ( device==5 ) {         /*  File ?.plt   ôffnen  */
         strcpy(file,str) ;
         if ( (hilf = strrchr(file, '.')) == NULL ) {
            strcat(file,".plt") ;
         } else {
            strcpy(hilf,".plt") ;
         }
         if( (grfp->fp=fopen(file,"w"))==0 ) {
            grf_err |= 4 ;
            printf("Datei %s kann nicht geîffnet werden !\n",file);
            return((GRF *)0) ;
         }
      }
   }
   if ( device==6 ) {
      strcpy(file,str) ;
      en_name = getenv("EPS") ;
      if ( NULL != en_name ) {
         int x,y ;

         if ( sscanf(en_name,"%d,%d",&x,&y) == 2 ) {
            psxmax = (double)x * (7200./254.) ;
            psymax = (double)y * (7200./254.) ;
         }
      }
      grfp->count = 0 ;
      if ( grfp->pt == NORMAL )
         grfp->pt = 103 ;                  /* Default StrichstÑrke */
      if ( in_use ) {
         grfp->fi_st = 1 ;
         sprintf(file,"virt%d.eps",grfp->dsc) ;
         if( (grfp->fp=fopen(file,"w"))==0 ) {
            grf_err |= 4 ;
            printf("Datei %s kann nicht geîffnet werden !\n",file);
            return((GRF *)0) ;
         }  
      } else {
         FILE *fp ;

         if( (grfp->fp=fopen(file,"w"))==0 ) {
            grf_err |= 4 ;
            printf("Datei %s kann nicht geîffnet werden !\n",file);
            return((GRF *)0) ;
         }
         fp = grfp->fp ;
    
         fprintf(fp,"%%!PS-Adobe-2.0 EPS-1.2\n") ;
         fprintf(fp,"%%%%Creator: Onno's GRF Software\n") ;
         fprintf(fp,"%%%%Title: %s\n",file) ;
         time(&t) ;
         fprintf(fp,"%%%%CreationDate: %s",ctime(&t)) ;
         fprintf(fp,"%%%%BoundingBox: 0 0 %.0f %.0f\n",
             psxmax * 0.1 ,psymax * 0.1 ) ;
         fprintf(fp,"%%%%EndComments\n") ;
         fprintf(fp,"/FreeHandDict 200 dict def FreeHandDict begin\n") ;
         fprintf(fp,"/bdef{bind def}bind def\n") ;
         fprintf(fp,"/dr{transform .25 sub round .25 add\n") ;
         fprintf(fp,"exch .25 sub round .25 add exch itransform} bdef\n") ;
         fprintf(fp,"/l {dr lineto} bdef\n/m {dr moveto} bdef\n") ;
         fprintf(fp,"/g {gsave setlinewidth stroke grestore newpath} bdef\n") ;
         fprintf(fp,"/f {gsave setgray fill 0 setgray grestore newpath} bdef\n") ;
         fprintf(fp,"/fo {gsave /Courier findfont} bdef\n") ;
         fprintf(fp,"/fp {dr moveto scalefont setfont} bdef\n") ;
         fprintf(fp,"/sh {show grestore} bdef\n") ;
         fprintf(fp,"%%%%EndProlog\n0.1 0.1 scale newpath\n") ;
      }
   }
   if ( device==4 || device==5 ) {
      fprintf(grfp->fp,"\033.(IN;VS8;SP1\n");
      if ( din_flag==0 ) {
         fprintf(grfp->fp,";SC0,%.0f,0,%.0f\n",plxmax,plymax);
      }
   }
   if(device == 7) {
        strcpy(file,str) ;
      grfp->count = 0 ;
      if( grfp->pt == NORMAL) grfp->pt = 103 ; /* Default Strichstaerke */
      if(in_use) {
         grfp->fi_st = 1 ;
         sprintf(file,"virt%d.tex", grfp->dsc) ;
         if((grfp->fp=fopen(file,"w")) == 0) {
            grf_err |= 4 ;
            printf("Datei %s kann nicht geffnet werden !\n",file);
            return(NULL) ;
         }
      }
      else {
        FILE *fp ;
        
        if((grfp->fp=fopen(file,"w")) == 0) {
            grf_err |= 4 ;
                printf("Datei %s kann nicht geffnet werden !\n",file);
                return(NULL) ;
        }
        fp = grfp->fp ;
        
        fprintf(fp, "\\unitlength0.01mm\n") ;
        fprintf(fp, "\\begin{picture}(%d,%d)\\thinlines\n", (int) ptxmax, (int) ptymax) ;
      }
   }   
   if(device == 8) {
        strcpy(file, str) ;
      grfp->seek = grfp->count = 0 ;
      if(grfp->pt == NORMAL) grfp->pt = 103 ; /* Default Strichstaerke */
      if(in_use) {
         grfp->fi_st = 1 ;
         sprintf(file,"virt%d.fig", grfp->dsc) ;
         if((grfp->fp = fopen(file, "w")) == 0) {
            grf_err |= 4 ;
            printf("Datei %s kann nicht geffnet werden !\n",file);
            return(NULL) ;
         }   
      }
      else {
        FILE *fp ;
        
        if((grfp->fp = fopen(file,"w")) == 0) {
            grf_err |= 4 ;
                printf("Datei %s kann nicht geffnet werden !\n",file);
                return(NULL) ;
        }
        fp = grfp->fp ;
        
        fprintf(fp, "#FIG 3.1\n") ;
        fprintf(fp, "Portrait\n") ;
        fprintf(fp, "Center\n") ;
        fprintf(fp, "Metric\n") ;
        fprintf(fp, "1200 2\n") ;
      }
   }
   grfp->dev = device ;
   return(grfp) ;  
}    
/********************************************************************
*   Schliessen aller geîffneter grf-pointer                         *
********************************************************************/ 

void grf_closeall(void)
{
   while( root )
      grf_close(root) ;
}      

/********************************************************************
*       Abmelden der Ausgabegeraete                                 *
********************************************************************/ 

int grf_close(GRF *grfp)

{
   int device,flag ;
   char str[64],file[64],*aname,*hilf ;
   GRF *grfi,*grfj,*grfmerk ;

   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return(-1) ;
   }
   device = grfp->dev ;
   grf_flush(grfp) ;
   if ( device > 2 ) {
      switch( device ) {
         case 3 :
            grf_pen(grfp,0);
            hp_close() ;
         break ;
         case 4 :
         case 5 :
            grf_pen(grfp,0) ;
            fprintf(grfp->fp,"\033.)\n") ;
            fclose(grfp->fp) ;
         break ;
         case 6 :
            if ( grfp->count ) {
               fprintf(grfp->fp,"%d g\n",grfp->pt%100) ;
            }
            fclose(grfp->fp) ;
         break ;
         case 7:
            if(dsc_count == 1) {
               fprintf(grfp->fp, "\n\\end{picture}\n") ;
            } 
            fclose(grfp->fp) ;
         break ;
         case 8:
            if(grfp->seek != 0) { /* vorherigen Line-Befehl ergaenzen */
               fseek(grfp->fp, grfp->seek, SEEK_SET) ;
               fprintf(grfp->fp, "%d", grfp->count) ; 
               fseek(grfp->fp, 0, SEEK_END) ;
               grfp->seek = 0 ;
            }
            fclose(grfp->fp) ;
         break ;
      }
      if ( grfp->fi_st == 1 ) {
          grfp->fi_st = 2 ;
      } else {
         grf_free(grfp) ;
      }
      do {
         flag = 0 ;
         grfmerk = (GRF *)0 ;
         grfi = root ;
         while ( grfi ) {
            if ( grfi->fi_st == 2 ) {    /* noch im petto */
               flag = 1 ;
               grfj = root ;
               while( grfj ) {
                  if ( grfj->fi_st == 0 &&
                     strcmp(grfi->name,grfj->name) == 0 &&
                     grfi->dsc != grfj->dsc ) {
                        flag = 0 ;
                  }
                  grfj = grfj->next ;
               }
               if ( flag == 1 ) {
                  grfmerk = grfi ;
               }
            }
            grfi = grfi->next ;
         }
         if ( grfmerk ) {
            grfi = grfmerk ;
            aname = grf_assign(grfi->name,&device) ;

            switch( device ) {
               case 3 :
                  hp_cat(grfi->dsc) ;
                  sprintf(str,"virt%d.plt",grfi->dsc) ;
                  unlink(str) ;
                  grf_free(grfi) ;
                  break ;
               case 4 :
                  sprintf(str,"virt%d.plt",grfi->dsc) ;
                  fcat(hp_sio(),str) ;
                  unlink(str) ;
                  grf_free(grfi) ;
                  break ;
               case 5 :
                  strcpy(file,aname) ;
                  if ( (hilf = strrchr(file, '.') ) == NULL ) {
                     strcat(file,".plt") ;
                  } else {
                     strcpy(hilf,".plt") ;
                  }
                  sprintf(str,"virt%d.plt",grfi->dsc);
                  fcat(file,str) ;
                  unlink(str) ;
                  grf_free(grfi) ;
                  break ;
               case 6 :
                  strcpy(file,aname) ;
                  if ( (hilf = strrchr(file, '.')) == NULL ) {
                     strcat(file,".eps") ;
                  } else {
                     strcpy(hilf,".eps") ;
                  }
                  sprintf(str,"virt%d.eps",grfi->dsc);
                  fcat(file,str) ;
                  unlink(str) ;
                  grf_free(grfi) ;
                  break ;
               case 7:
                  strcpy(file,aname) ;
                  if((hilf = strrchr(file, '.')) == NULL ) {
                        strcat(file,".tex") ;
                  } 
                  else {
                        strcpy(hilf,".tex") ;
                  }
                  sprintf(str,"virt%d.tex",grfi->dsc);
                  fcat(file,str) ;
                  unlink(str) ;   
                  grf_free(grfi) ;
               break ;
               case 8:
                strcpy(file, aname) ;
                if((hilf = strrchr(file, '.')) == NULL ) {
                        strcat(file, ".fig") ;
                }
                else {
                        strcpy(hilf, ".fig") ;
                }
                sprintf(str, "virt%d.fig", grfi->dsc);
                fcat(file, str) ;
                unlink(str) ;    
                grf_free(grfi) ;
               break ;
            }
         }
      } while ( flag==1 ) ;
   } else {
      grf_free(grfp) ;
      grfp->magic = 0L ;
   }
   dsc_count-- ;
   return(0) ;
}

static int grf_free(GRF *grfp)

{
   GRF *grfi ;
   if ( root == grfp ) {
      root = grfp->next ;
      free((char*)grfp) ;  
      return(0) ;
   }
   grfi = root ;
   while ( grfi ) {
      if ( grfi->next == grfp ) {
         grfi->next = grfp->next ;
         free((char*)grfp) ;
         return(0) ;
      }
      grfi = grfi->next ;
   }
   return(-1) ;
}


/********************************************************************
*       Zuordnung der Ausgabegeraete                                *
*       durch das Auslesen der evt. gesetzten Enviroment Variablen  *
********************************************************************/

char *grf_assign(char device[],int *k)

{
   static char name[64];
   char        *en_name,*ext ;

   strcpy(name,device) ;
   for ( en_name = name ; *en_name ; en_name++ )
      *en_name = toupper(*en_name) ;
      
   if ( (en_name = getenv(name)) != NULL ) 
      strcpy(name,en_name) ;

   for ( en_name = name ; *en_name ; en_name++ )
      *en_name = tolower(*en_name) ;

   if ( ( ext = strrchr(name, '.') ) == NULL )     /* extension ermitteln  */
      ext = "" ;

   *k = 5 ;                     /*      Default        */
   if ( 0==strcmp(name,"text") )     *k = 0 ;
   if ( 0==strcmp(name,"screen1") )  *k = 1 ;
   if ( 0==strcmp(name,"screen2") )  *k = 2 ;
   if ( 0==strcmp(name,"screen") )   *k = 2 ;
   if ( 0==strcmp(name,"hp7221") )   *k = 3 ;
   if ( 0==strcmp(name,"hpgl") )     *k = 4 ;
   if ( 0==strcmp(ext,".eps") )      *k = 6 ;
   if(0 == strcmp(ext,".tex"))      *k = 7 ;
   if(0 == strcmp(ext,".fig"))      *k = 8 ;
   
   return(name) ; /* file */
}

/********************************************************************
*       Festlegung der clipping koordinaten                         *
*       Parameter :                                                 *
*       Device descriptor    dsc                                    *
*       Links unten in reellen Koordinaten        u1,v1 (double)    *
*       Rechts oben in reellen Koordinaten        u2,v2 (double)    *
*                                                                   *
********************************************************************/ 

void grf_clip(GRF *grfp,double u1,double v1,double u2,double v2,int mod)

{
   if ( mod==REEL ) {
      grfp->cx_1 = u1 ;
      grfp->cy_1 = v1 ;
      grfp->cx_2 = u2 ;
      grfp->cy_2 = v2 ;
   } else {
      double xmax,ymax ;
      xmax = grf_xmax(grfp->dev) ;
      grfp->cx_1 = grfp->x_1 + ( u1*xmax - (double)grfp->xmin )
         / grfp->x_f ;
      grfp->cx_2 = grfp->x_1 + ( u2*xmax - (double)grfp->xmin )
         / grfp->x_f ;
      ymax = grf_ymax(grfp->dev) ;
      grfp->cy_1 = grfp->y_1 + ( v1*ymax - (double)grfp->ymin )
         / grfp->y_f ;
      grfp->cy_2 = grfp->y_1 + ( v2*ymax - (double)grfp->ymin )
         / grfp->y_f ;
   }
}
/********************************************************************
*       Festlegung der real koordinaten                             *
*       optional : Zeichnen des Koordinatensystems                  *
*                  in default Ausgabe                               *
*       Parameter :                                                 *
*       Device descriptor    grfp                                   *
*       Links unten in physikalischen Koordinaten x1,y1 (double)    *
*       Rechts oben in physikalischen Koordinaten x2,y2 (double)    *
*       Links unten in reellen Koordinaten        u1,v1 (double)    *
*       Rechts oben in reellen Koordinaten        u2,v2 (double)    *
*       Anzahl der Feldeinteilungen               xf,yf (int)       *
*       Anzahl der Markierungseinteilungen        xm,ym (int)       *
*       Anzahl der Beschriftungseinteilungen      xt,yt (int)       *
*       Format fuer die Beschriftung              xfo,yfo (char *)  *
*       Skalierungsfaktor fuer die Beschriftung   xsc,xsc (double)  *
********************************************************************/ 

int grf_koor(GRF *grfp,
              double x1,double y1,double x2,double y2,
              double u1,double v1,double u2,double v2,
              int xf,int yf,int xm,int ym,int xt,int yt,
              char xfo[],char yfo[] ,double xsc,double ysc)

{
   int    i,value,dx,dy,ds ;
   double dval ;
   char   str[80] ;
   double xmax,ymax ;
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return(-1) ;
   }

   grf_flush(grfp) ;
   if ( x1 > x2 || y1 > y2 || u1 > u2 || v1 > v2 ) {
      grf_err |= 16 ;
      return(-1) ;
   }
   grfp->magic = MAGIC2 ;

   xmax = grf_xmax(grfp->dev) ;
   grfp->xmin = x1 * xmax ;
   grfp->xmax = x2 * xmax ;

   ymax = grf_ymax(grfp->dev) ;
   grfp->ymin = y1 * ymax ;
   grfp->ymax = y2 * ymax ;
   grfp->cx_1 = grfp->x_1  = u1 ;
   grfp->cy_1 = grfp->y_1  = v1 ;
   grfp->cx_2 = u2 ;
   grfp->cy_2 = v2 ;
   grfp->i_dx = 1.0 / ( grfp->cx_2 - grfp->cx_1 ) ;
   grfp->i_dy = 1.0 / ( grfp->cy_2 - grfp->cy_1 ) ;
   grfp->x_f  = (double)( grfp->xmax - grfp->xmin ) * grfp->i_dx ;
   grfp->y_f  = (double)( grfp->ymax - grfp->ymin ) * grfp->i_dy ;

   grfp->vs   = 0.0 ;
   grfp->aflg = 0 ;
   grfp->ltyp = 0 ;
   grfp->pt   = NORMAL ;

   grfp->x_a  = -1 ;
   grfp->y_a  = -1 ;
   grfp->x_a2 = -1 ;
   grfp->y_a2 = -1 ;

   dx = grfp->xmax - grfp->xmin ;
   dy = grfp->ymax - grfp->ymin ;
   if ( xf || yf ) {
      i_move(grfp,grfp->xmin,grfp->ymin);
      if ( xf ) {
         i_draw(grfp,grfp->xmin,grfp->ymax);
      } else {
         i_move(grfp,grfp->xmin,grfp->ymax);
      }
      if ( yf ) {
         i_draw(grfp,grfp->xmax,grfp->ymax);
      } else {
         i_move(grfp,grfp->xmax,grfp->ymax);
      }
      if ( xf ) {
         i_draw(grfp,grfp->xmax,grfp->ymin);
      } else {
         i_move(grfp,grfp->xmax,grfp->ymin);
      }
      if ( yf ) {
         i_draw(grfp,grfp->xmin,grfp->ymin);
      } else {
         i_move(grfp,grfp->xmin,grfp->ymin);
      }
      grf_ltyp( grfp, 1, 1.0 );
      for ( i=1 ; i<xf ; i++ ) {
         value = grfp->xmin + (int) (((long)i*(long)dx) / (long)xf) ;
         if ( i & 1 ) {
            i_move(grfp,value,grfp->ymin);
            i_draw(grfp,value,grfp->ymax);
         } else {
            i_move(grfp,value,grfp->ymax);
            i_draw(grfp,value,grfp->ymin);
         }
      }
      for ( i=1 ; i<yf ; i++ ) {
         value = grfp->ymin + (int) (((long)i*(long)dy) / (long)yf) ;
         if ( i & 1 ) {
            i_move(grfp,grfp->xmin,value) ;
            i_draw(grfp,grfp->xmax,value) ;
         } else {
            i_move(grfp,grfp->xmax,value) ;
            i_draw(grfp,grfp->xmin,value) ;
         }
      }
      grf_ltyp( grfp, 0, 1.0 ) ;
   }
   ds = ( dy + 50 ) / 100 ;            /* LÑnge der kurzen Striche  */
   for ( i=1 ; i<xm ; i++ ) {
      if ( ( ( i * xf ) % xm ) || ( xf==0 )) {
         value = grfp->xmin + (int) (((long)i*(long)dx) / (long)xm) ;
         i_move(grfp,value,grfp->ymin) ;
         i_draw(grfp,value,grfp->ymin+ds) ;
      }
   }
   for ( i=1 ; i<ym ; i++ ) {
      if ( ( ( i * yf ) % ym ) || ( yf==0 )) {
         value = grfp->ymin + (int) (((long)i*(long)dy) / (long)ym) ;
         i_move(grfp,grfp->xmax-ds,value);
         i_draw(grfp,grfp->xmax,value);
      }
   }
   for ( i=xm-1 ; i>0 ; i-- ) {
      if ( ( ( i * xf ) % xm ) || ( xf==0 )) {
         value = grfp->xmin + (int) (((long)i*(long)dx) / (long)xm) ;
         i_move(grfp,value,grfp->ymax-ds) ;
         i_draw(grfp,value,grfp->ymax) ;
      }
   }
   for ( i=ym-1 ; i>0 ; i-- ) {
      if ( ( ( i * yf ) % ym ) || ( yf==0 )) {
         value = grfp->ymin + (int) (((long)i*(long)dy) / (long)ym) ;
         i_move(grfp,grfp->xmin,value);
         i_draw(grfp,grfp->xmin+ds,value);
      }
   }
   if ( xt>0 ) {
      if ( !xfo || !xfo[0] )
         xfo = "%2.3E" ;
      for ( i=xt ; i>=0 ; i-- ) {
         value = grfp->xmin + (int) (((long)i*(long)dx) / (long)xt) ;
         dval  = grfp->x_1  +
               (double)i*(grfp->cx_2 - grfp->cx_1 ) / (double)xt ;
         sprintf(str,xfo,dval*xsc) ;
         p_write(grfp,str,value,grfp->ymin,11,-1) ;
      }
   }  
   if ( yt>0 ) {
      if ( !yfo || !yfo[0] )
          yfo = "%2.3E" ;
      for ( i=0 ; i<=yt ; i++ ) {
         value = grfp->ymin + (int) (((long)i*(long)dy) / (long)yt) ;
         dval  = grfp->y_1  +
               (double)i*(grfp->cy_2 - grfp->cy_1) / (double)yt ;
         sprintf(str,yfo,dval*ysc) ;
         p_write(grfp,str,grfp->xmin,value,12,-1) ;
      }
   }
   return(0) ;
}
      
/********************************************************************
*         Zeichnen einer Linie auf dem Ausgabegeraet                *
*         Erzeugung des Strich-Punkt Musters  (nicht fÅr Plotter)   *
*         Startpunkt x1,y1                                          *
*         Endpunkt   x2,y2                                          *
********************************************************************/ 

static void dot_draw(GRF *grfp,int ix1,int iy1,int ix2,int iy2)

{
   double laenge,stueck,dx,dy,x,y ;
   double lmuster[6] ;
   double x1,y1,x2,y2,lrel ;
   int ltyp,i ;
   static int mlang[6] = { 2,2,2,4,4,6 };
   static int p1p2_flag = 0 ;
   static double p1p2 = 0.0 ;
   static int muster[6][6] = {
      { 9999,    1,    0,    0,    0,    0  } ,
      { 5000, 5000,    0,    0,    0,    0  } ,
      { 3000, 7000,    0,    0,    0,    0  } ,
      { 1000, 8000,  999,    1,    0,    0  } ,
      { 1000, 7000, 1000, 1000,    0,    0  } ,
      { 1000, 5000, 1000, 1000, 1000, 1000  } 
   } ;

   ltyp = grfp->ltyp ;   /* (0...6) --> wird von grf_ltyp() in GRF vermerkt */
   if( ltyp <= 0 || ltyp > 6 ||
        grfp->dev==3 || grfp->dev==4 || grfp->dev==5) {
      grf_pline( grfp, ix1, iy1, ix2, iy2 ) ;
      return ;
   }
   ltyp-- ;
   /* war der Letzte ein move, sollte das Muster wieder von vorne anfangen */
   if( grfp->x_a2 != ix1 || grfp->y_a2 != iy1 ) {
      grfp->mrest = 0.0 ;
      grfp->mindex = grfp->mflag = 0 ;
   }
   if( !p1p2_flag ) {
      x = grf_xmax(grfp->dev) + 1 ;
      y = grf_ymax(grfp->dev) + 1 ;
      /* (..127 % der Hauptdiagonalen, bis sich das Muster wiederholt */
      p1p2 = sqrt(x*x+y*y)    /* Hauptdiagonale des beschreibbaren Bereichs */
         * ( 0.01         /* da lrel eine Prozent-Angabe */
         *   0.0001       /* durch 10000 bezug fuer muster */
         *   0.89 ) ;     /* durch optischen Vergleich mit HP7475 ermittelt */
      p1p2_flag = 1;
   }
   lrel = grfp->lrel * p1p2 ;
   for( i=0 ; i<6 ; i++ )
      lmuster[i] = lrel * (double)muster[ltyp][i] ;
   grfp->x_a2 = ix2 ;
   grfp->y_a2 = iy2 ;

   x1 = (double)ix1 ;
   y1 = (double)iy1 ;
   x2 = (double)ix2 ;
   y2 = (double)iy2 ;

   dx = x2 - x1 ;
   dy = y2 - y1 ;

   laenge = sqrt( dx * dx + dy * dy ) ;
   stueck = laenge ;
   dx /= laenge ;
   dy /= laenge ;
   x = x1 ;
   y = y1 ;
   while( stueck > grfp->mrest ) {    /* nutzt Rest von der letzten line */
      stueck -= grfp->mrest ;
      x += grfp->mrest * dx ;
      y += grfp->mrest * dy ;

      if( grfp->mflag )
         grf_pline(grfp,(int)x1,(int)y1,(int)x,(int)y) ;

      x1 = x ;
      y1 = y ;

      grfp->mindex = ( grfp->mindex + 1) % mlang[ltyp] ;
      grfp->mrest  = lmuster[grfp->mindex] ;

      grfp->mflag ^= 1 ;        /* immer nur das unterste Bit wird gekippt */
   }
   if ( stueck > 0.0 ) {     /* falls noch eine Reststrecke verbleibt */
      if( grfp->mflag ) 
         grf_pline(grfp,(int)x1,(int)y1,(int)x2,(int)y2) ;
      grfp->mrest -= stueck ;    /* Uebertrag zum naechsten line() */
   }
}

/********************************************************************
*       Physikalischer line auf einem AusgabegerÑt                  *
********************************************************************/

void grf_pline(GRF *grfp,int x1,int y1,int x2,int y2)
{
   switch( grfp->dev ) {
      case 1 :
      case 2 :
         grf_sline( grfp, x1, y1, x2, y2 ) ;
         break ;
      case 3 :
         hp_draw(x2,y2);
         break ;
      case 4 :
      case 5 : 
         if ( grfp->count == 0 ) {
            fprintf(grfp->fp,";PD%d,%d",x2,y2);
            grfp->count = 1 ;
         } else {
            fprintf(grfp->fp,",%d,%d",x2,y2);
         }
         break ;
      case 6 :
         if ( x1!=grfp->x_a || y1!=grfp->y_a ) {  /* Move ausfÅhren ! */
            if ( grfp->count > 0 ) {
               fprintf(grfp->fp,"%d g\n",grfp->pt%100) ;
               grfp->count = 0 ;
            }
         }
         if ( grfp->count > 100 ) {
            fprintf(grfp->fp,"%d g\n",grfp->pt%100) ;
            grfp->count = 0 ;
         }
         if ( grfp->count == 0 ) {
            fprintf(grfp->fp,"%d %d m\n",x1,y1) ;
         }
         fprintf(grfp->fp,"%d %d l\n",x2,y2) ;
         grfp->count++ ;
         break ;
      case 7:
         if(x1 != x2 || y1 != y2) {    /* Laenge NULL wird nicht gezeichnet ! */
            if(x1 != grfp->x_a || y1 != grfp->y_a || 
                grfp->count == 0 || grfp->count > 1000) {
                grfp->count = fprintf(grfp->fp,"\n\\drawline(%d,%d)(%d,%d)",
                                      x1,y1,x2,y2 ) ;
            }
            else {
                grfp->count += fprintf(grfp->fp,"(%d,%d)", x2, y2);
            }
         }
         else {                      /* ausgefuellter Kreis mit Radius 0.1 mm */
                fprintf(grfp->fp,"\n\\put(%d,%d){\\circle*{5}}", x2, y2) ;
         }   
      break ;
      case 8:
        if(x1 != x2 || y1 != y2) {    /* Laenge NULL wird nicht gezeichnet ! */
                if(x1 != grfp->x_a || y1 != grfp->y_a || grfp->count == 0) {
                        if(grfp->seek != 0) {           /* vorherigen Line-Befehl ergaenzen */
                                fseek(grfp->fp, grfp->seek, SEEK_SET) ;
                                fprintf(grfp->fp, "%d", grfp->count) ;
                                fseek(grfp->fp, 0, SEEK_END) ;
                        }
                        fprintf(grfp->fp,"2 1 0 1 -1 7 0 0 -1 0.0 2 0 -1 0 0 ") ;
                        grfp->seek = ftell(grfp->fp) ; 
                        fprintf(grfp->fp, "#     \n") ;
                        fprintf(grfp->fp, "%d %d %d %d\n", x1, PFYMAX - y1,
                                x2, PFYMAX - y2) ;
                        grfp->count = 2 ;
                }
                else {
                        fprintf(grfp->fp,"%d %d\n", x2, PFYMAX - y2) ;
                        grfp->count++ ;
                }
        }
        else {                      /* ausgefuellter Kreis mit Radius 0.1 mm */
             fprintf(grfp->fp,
                     "\n1 3 0 1 -1 0 0 0 20 0.0 1 0.0 %d %d 5 5 %d %d %d %d",
                     x2, PFYMAX - y2, x2, PFYMAX - y2, x2 + 5, PFYMAX - y2) ;
        }
      break ;
   }
   grfp->x_a = x2 ;
   grfp->y_a = y2 ;
}

/********************************************************************
*       Absolute Move in Geraete Koordinaten                        *
********************************************************************/

static void i_move(GRF *grfp,int x,int y)

{
  if ( x == grfp->x_a && y == grfp->y_a )
     return ;
  switch(grfp->dev) {
     case 3 :
        hp_move(x,y) ;
     break ;
     case 4 :
     case 5 :
        fprintf(grfp->fp,";PU%d,%d\n",x,y);
        grfp->count = 0 ;
     break ;
     case 6 :
        if ( grfp->count > 0 ) {
           fprintf(grfp->fp,"%d g\n",grfp->pt%100) ;
           grfp->count = 0 ;
        }
     break ;
     case 7:
        grfp->count = 0 ;
     break ;
     case 8:
        if(grfp->seek != 0) { /* vorherigen Line-Befehl ergaenzen */
           fseek(grfp->fp, grfp->seek, SEEK_SET) ;
           fprintf(grfp->fp, "%d", grfp->count) ;
           fseek(grfp->fp, 0, SEEK_END) ;
        }
        grfp->seek = grfp->count = 0 ;
     break ;
  }
  grfp->x_a = x ;
  grfp->y_a = y ;
}

/********************************************************************
*       Absolute Draw in Geraete Koordinaten                        *
********************************************************************/

static void i_draw(GRF *grfp,int x,int y)

{
  if ( x == grfp->x_a && y == grfp->y_a )
     return ;
  dot_draw(grfp,grfp->x_a,grfp->y_a,x,y);
}

/********************************************************************
*     Ausgabe der gespeicherten Werte von opt Vek                   *
*     fuer das Ausgabegeraet grf_def                                *
********************************************************************/ 

void grf_flush(GRF *grfp)
{
   
   if ( grfp->aflg ) {
      clip_draw(grfp,grfp->xfm,grfp->yfm) ;
      grfp->vs = 0.0 ;
      grfp->aflg = 0 ;
   }
   if ( grfp->dev==4 ) {
      fflush(grfp->fp);
   }  
}

/********************************************************************
*     Move in Absoluten Reellen Koordinaten                         *
*     fuer das Ausgabegeraet grfp                                   *
********************************************************************/ 

void grf_move(GRF *grfp,double x,double y)

{ 
   if ( grfp->magic != MAGIC2 ) {
      grf_err |= 8 ;
      return ;
   }
   grf_flush(grfp) ;
   grfp->xfm = grfp->xfa = x ;
   grfp->yfm = grfp->yfa = y ;
}


/********************************************************************
*     Draw in Absoluten Reellen Koordinaten                         *
*     fuer das Ausgabegeraet grfp                                   *
********************************************************************/ 

void grf_draw(GRF *grfp,double x,double y)

{
   double a,b,dsum,ksum ;
  
   if ( grfp->magic != MAGIC2 ) {
      grf_err |= 8 ;
      return ;
   }
/*   if ( grfp->dsc==1 || grfp->dsc==2 ) {
      clip_draw(grfp,x,y) ;
      return ;      auf dem Bildschirm keine Optimierung 
   }
*/
   a = ( x - grfp->xfa ) * grfp->i_dx ;
   b = ( y - grfp->yfa ) * grfp->i_dy ;
   ksum = sqrt(a * a + b * b) ; 
         
   a = ( x - grfp->xfm ) * grfp->i_dx ;
   b = ( y - grfp->yfm ) * grfp->i_dy ;
   grfp->vs += dsum = sqrt(a * a + b * b)  ;

   if ( grfp->vs > ksum + grfp->del ) {
      clip_draw(grfp,grfp->xfm,grfp->yfm) ;
      grfp->vs = dsum ;
   }
   grfp->xfm  = x ;
   grfp->yfm  = y ;
   grfp->aflg = 1 ;
}

/********************************************************************
*     Draw in Absoluten Reellen Koordinaten                         *
*     mit clipping                                                  *
*     fuer das Ausgabegeraet grf_def                                *
********************************************************************/ 

static void clip_draw(GRF *grfp,double x,double y)

{
   double fx1,fy1,fx2,fy2,merk;
   int    count,ix,iy; 

   fx1 = grfp->xfa ;
   fy1 = grfp->yfa ;
   grfp->xfa = fx2 = x ;
   grfp->yfa = fy2 = y ;
   if ( ( fx1 > grfp->cx_2 ) && ( fx2 > grfp->cx_2 ) )
      return ;
   if ( ( fx1 < grfp->cx_1 ) && ( fx2 < grfp->cx_1 ) )
      return ;
   if ( ( fy1 > grfp->cy_2 ) && ( fy2 > grfp->cy_2 ) )
      return ;
   if ( ( fy1 < grfp->cy_1 ) && ( fy2 < grfp->cy_1 ) )
      return ;
   count=2;
   do {
      if ( fx2 > grfp->cx_2 ) {
         if ( fx2 == fx1 )
            return ;
         fy2 = fy1 + ( grfp->cx_2 - fx1 ) * ( fy2 - fy1 ) / ( fx2 - fx1 ) ;
         fx2 = grfp->cx_2 ;
      } else if ( fx2 < grfp->cx_1 ) {
         if ( fx2 == fx1 )
            return ;
         fy2 = fy1 + ( grfp->cx_1 - fx1 ) * ( fy2 - fy1 ) / ( fx2 - fx1 ) ;
         fx2 = grfp->cx_1 ;
      }
      if ( fy2 > grfp->cy_2 ) {
         if ( fy2 == fy1 )
            return ;
         fx2 = fx1 + ( grfp->cy_2 - fy1 ) * ( fx2 - fx1 ) / ( fy2 - fy1 ) ;
         fy2 = grfp->cy_2 ;
      } else if ( fy2 < grfp->cy_1 ) {
         if ( fy2 == fy1 )
            return ;
         fx2 = fx1 + ( grfp->cy_1 - fy1 ) * ( fx2 - fx1 ) / ( fy2 - fy1 ) ;
         fy2 = grfp->cy_1 ;
      }
      merk=fy1 ; fy1=fy2 ; fy2=merk ;
      merk=fx1 ; fx1=fx2 ; fx2=merk ;
   } while ( --count );
   if ( fx1 > grfp->cx_2 )
      return ;
   if ( fx1 < grfp->cx_1 )
      return ;
   if ( fx2 > grfp->cx_2 )
      return ;
   if ( fx2 < grfp->cx_1 )
      return ;
   if ( fy1 > grfp->cy_2 )
      return ;
   if ( fy1 < grfp->cy_1 )
      return ;
   if ( fy2 > grfp->cy_2 )
      return ;
   if ( fy2 < grfp->cy_1 )
      return ;
   ix = grfp->xmin + (int)(grfp->x_f*(fx1-grfp->x_1)) ;
   iy = grfp->ymin + (int)(grfp->y_f*(fy1-grfp->y_1)) ;
   i_move(grfp,ix,iy) ;
   ix = grfp->xmin + (int)(grfp->x_f*(fx2-grfp->x_1)) ;
   iy = grfp->ymin + (int)(grfp->y_f*(fy2-grfp->y_1)) ;
   i_draw(grfp,ix,iy) ;
}


/********************************************************************
*    Plotter :     Wahl des Stiftes                                 *
*                  1 - 6     0 = kein Stift                         *
*    EPS,TEX :     StrichstÑrke                                     *
*    Bildsch :     Wahl der Strichart                               *
*                  0           = Punkt zuruecksetzen                *
*                  2           = Punkt exclusiv oder verknÅpfen     *
*                  1 + default =  Punkt setzen                      *
*    bei Stiften Åber 100 wird der Bildschrim nicht beeinflu·t      *
*    100 sind Dummy - die Einer zÑhlen !                            *
********************************************************************/ 

void grf_pen(GRF *grfp,int i)

{
   int device,pt_alt ;
 
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return ;
   }
   grf_flush(grfp) ;
   pt_alt = grfp->pt ;
   grfp->pt = i ;
        /* bei Stiften Åber 100 wird der Bildschrim nicht beeinflu·t */
   device = grfp->dev ;
   if ( device==4 || device==5 ) {
      fprintf(grfp->fp,";SP%d\n", i % 100 );
   } else if ( device==3 ) {
      hp_selpen( i % 100 ) ;
   } else if ( device==6 ) {
      if ( grfp->count > 0 ) {
         fprintf(grfp->fp,"%d g\n",pt_alt%100) ;
      } else if ( grfp->count < 0 && grfp->count != -10000 ) {
         fprintf(grfp->fp,"%d %d l\n",grfp->x_fg,grfp->y_fg) ;
         fprintf(grfp->fp,"%.2lf f\n",grfp->fg) ;
      }
      grfp->count = 0 ;
   }
}

/********************************************************************
*    Bildsch :     Wahl der Farbe                                   *
********************************************************************/ 

void grf_color(GRF *grfp,int i)

{
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return ;
   }
   grf_flush(grfp) ;
   grfp->color = i ;
}

/********************************************************************
*    Wahl der Schrifgroesse                                         *
*                  0 - 3                                            *
*                -1   gewaehlte Grîsse zurÅckgeben                  *
********************************************************************/ 

int grf_font(GRF *grfp,int i)

{
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return(0) ;
   }
   if ( i>=0 && i<=3 ) {
      grfp->font = i ;
   }
   return( grfp->font ) ;
}
/********************************************************************
*    EPS     :     FlÑchen fÅllen                                   *
*                  0.0 - 1.0     -1 = kein FÅllen                   *
********************************************************************/ 

void grf_fill(GRF *grfp,double fg)

{
   int     device,xi,yi ;
   double  fg_alt ;

   grf_flush(grfp) ;
   fg_alt = grfp->fg ;
   grfp->fg = fg ;
   device = grfp->dev ;
   if ( device==6 ) {
      xi = grfp->xmin +
         (int)(grfp->x_f*(grfp->xfa-grfp->x_1)) ;
      yi = grfp->ymin +
         (int)(grfp->y_f*(grfp->yfa-grfp->y_1)) ;
      if ( grfp->count > 0 ) {
         fprintf(grfp->fp,"%d g\n",grfp->pt%100) ;
      } else if ( grfp->count < 0 && grfp->count != -10000 ) {
         fprintf(grfp->fp,"%d %d l\n",grfp->x_fg,grfp->y_fg) ;
         fprintf(grfp->fp,"%.2lf f\n",fg_alt) ;
      }
      if ( fg >= 0.0 && fg <= 1.0 ) {
         grfp->count = -10000 ;
         fprintf(grfp->fp,"%d %d m\n",xi,yi) ;
      } else {
         grfp->count = 0 ;
      }
      grfp->x_fg = xi ;
      grfp->y_fg = yi ;
   }
}

/********************************************************************
*    Plotter :      Wahl der maximalen Geschwindigkeit              *
*                   fÅr einen Stift-Plotter                         *
*                   default : 8 cm/s                                *
*                   1 - 38   in cm/s                                *
********************************************************************/

void grf_speed(GRF *grfp,int i)

{
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return ;
   }
   switch(grfp->dev) {
      case 3 :
         hp_speed(i) ;
      break ;
      case 4 :
      case 5 :
         fprintf(grfp->fp,";VS%d\n",i) ;
      break ;
   }
}

/********************************************************************
*        Ausgabe eines Textes                                       *
*                                                                   *
*    Parameter :                                                    *
*      string  auszugebender Text                                   *
*      bx      X-Position in Phys.koordinaten                       *
*      by      Y-Position in Phys.koordinaten                       *
*      device  Ausgabegeraet                                        *
*      verschieb Ausgabeformat                                      *
*      fond    Zeichensatz                                          *
********************************************************************/

static void p_write(GRF *grfp,char string[],
             int bx,int by,int verschieb,int font)

{
   int str_ln,device ;
   static float plt_x[] = { 0.0929, 0.0929, 0.1393, 0.1548 } ;
   static float plt_y[] = { 0.2053, 0.2463, 0.3284, 0.4106 } ;
   static int    eps[]   = { 6    , 8    , 10    , 12   } ;
   static char  *Tex[]  = {"", "t", "r", "b", "l", "lt", "lb", "rt", "rb"} ;
   static int    Fig[]  = {1,   1,   2,   1,   0,   0,    0,    2,    2} ;
   static int   FigY[]  = {60,  120, 60,  0,   60, 120,  0,    120,  0} ;
   int v_x,v_y ;

   str_ln = (int)strlen(string) ;
   if ( str_ln == 0 )
      return ;
   device = grfp->dev ;
   if ( bx < 0 || bx > grf_xmax(device) || 
       by < 0 || by > grf_ymax(device) )   /* fÅr clipping */
      return ;
   if ( font<0 || font>3 ) {
      font = grfp->font ;
   }

   if( device < 7 ) {

   switch(device) {
      case 1 :
      case 2 :
         v_x = str_ln * grf_fontx2(font) ;
         v_y =          grf_fonty2(font) ;
         break;
      case 3 :
      case 4 :
      case 5 :
         v_x = str_ln * plt_x[font] * 275.0 ;
         v_y = plt_y[font] * 180.0 ;
         break;
      case 6 :
         v_x = str_ln * 3 * eps[font] ;
         v_y = ( eps[font] * 10 ) / 3 ;
         break;
   }
   switch(verschieb) {
      case(0): /*auf den Punkt zentriert*/
         bx += -v_x ;
         by += -v_y ;
         break;
      case(1): /*unterhalb des Punktes*/
         bx += -v_x ;
         by += -2 * v_y ;
         break;
      case(2): /*links vom Punkt*/
         bx += -2 * v_x ;
         by += -v_y ;
         break;
      case(3): /*oberhalb des Punktes*/
         bx += -v_x ;
         break;
      case(4): /*rechts vom Punkt*/
         by += -v_y ;
         break;
      case(5): /*Ecke links oben gegeben*/
         by += -2 * v_y ;
         break;
      case(6): /*Ecke links unten gegeben*/
         break;
      case(7): /*Ecke rechts oben gegeben*/
         bx += -2 * v_x ;
         by += -2 * v_y ;
         break;
      case(8): /*Ecke rechts unten gegeben*/
         bx += -2 * v_x;
         break;
      case(11): /* unterhalb des Punktes  fÅr Koor mit Luft */
         bx += -v_x ;
         by += -3 * v_y ;
         break;
      case(12): /* links vom Punkt fÅr Koor mit Luft */
         bx += -2 * v_x -v_y ;
         by += -v_y ;
         break;
   }
   if ( bx < 0 || bx + 2 * v_x > grf_xmax(device) || 
        by < 0 || by + 2 * v_y > grf_ymax(device) )   /* fÅr clipping */
          return ;
   i_move(grfp,bx,by) ;

   } /* device < 7 */

   switch(device) {
      case 1 :
      case 2 :
         grf_twrite(grfp,bx,by,string,font) ;
         return ;
      case 3 :
         hp_si( plt_x[font], plt_y[font] );
         hp_print( string );
         return ;
      case 4 :
      case 5 :
         fprintf(grfp->fp,";SI%.3f,%.3f",plt_x[font],plt_y[font]);
         fprintf(grfp->fp,";LB%s\3\n",string);
         return ;
      case 6 :
         fprintf(grfp->fp,"fo %d %d %d fp (%s) sh\n",
            eps[font]*10,grfp->x_a,grfp->y_a,string) ;
         return ;
      case 7:
         if(verschieb > 0 && verschieb < 9) {
            fprintf(grfp->fp, "\\put(%d,%d){\\makebox(0,0)[%s]{%s}}\n",
                               bx, by, Tex[verschieb], string) ;
         }
         if(verschieb == 0) {
                fprintf(grfp->fp, "\\put(%d,%d){\\makebox(0,0){%s}}\n",
                                   bx, by, string) ;
         }
         if(verschieb == 11) {
                fprintf(grfp->fp, "\\put(%d,%d){\\makebox(0,0)[%s]{%s}}\n",
                                   bx, by - 100, Tex[1], string) ;
         }
         if(verschieb == 12) {
                fprintf(grfp->fp, "\\put(%d,%d){\\makebox(0,0)[%s]{%s}}\n",
                                   bx - 100, by, Tex[2], string) ;
         }
         return ;
      case 8:
         if(verschieb < 9) {
                fprintf(grfp->fp, "4 %d -1 0 0 0 12 0.0 0 0 0 %d %d %s\\001\n",
                        Fig[verschieb], bx, PFYMAX - (by - FigY[verschieb]),
                        string) ;
         }  
         if(verschieb == 11) {
                 fprintf(grfp->fp, "4 %d -1 0 0 0 12 0.0 0 0 0 %d %d %s\\001\n",
                         Fig[1], bx, PFYMAX - (by - FigY[1] - 50),
                         string) ;
         }
         if(verschieb == 12) {
                 fprintf(grfp->fp, "4 %d -1 0 0 0 12 0.0 0 0 0 %d %d %s\\001\n",
                         Fig[2], bx - 50, PFYMAX - (by - FigY[2]),
                         string) ;
         }
         return ;
   }
}

/********************************************************************
*        Ausgabe eines Textes                                       *
*                                                                   *
*    Parameter :                                                    *
*      grfp      Ausgabegeraet                                      *
*      string  auszugebender Text                                   *
*      rel_x   X-Position 0.0 ... 1.0                               *
*      rel_y   Y-Position 0.0 ... 1.0                               *
*      modus    0 = Bildschirm  1 = Koor                            *
*      verschieb Ausgabeformat                                      *
*      fond    Zeichensatz                                          *
********************************************************************/

void grf_write(GRF *grfp,char string[],
               double rel_x,double rel_y,
               int modus,int verschieb,int font)

{
  int  phys_x,phys_y ;

   grf_flush(grfp) ;
   if ( modus==0 ) {
      switch(grfp->dev) {
         case 1 :
         case 2 :
            phys_x = (int)(grf_scxmax(grfp->dev) * rel_x ) ;
            phys_y = (int)(grf_scymax(grfp->dev) * rel_y ) ;
            break ;
         case 4 :
         case 5 :
            phys_x = (int)(plxmax * rel_x) ;
            phys_y = (int)(plymax * rel_y) ;
            break ;
         case 6 :
            phys_x = (int)(psxmax * rel_x ) ;
            phys_y = (int)(psymax * rel_y ) ;
            break ;
         case 7:
            phys_x = (int)(ptxmax * rel_x ) ;
            phys_y = (int)(ptymax * rel_y ) ;
         break ;
         case 8: 
                phys_x = (int)(pfxmax * rel_x ) ;
                phys_y = (int)(pfymax * rel_y ) ;
         break ;
      }
   } else {
      if ( grfp->magic != MAGIC2 ) {
         grf_err |= 8 ;
         return ;
      }
      phys_x = grfp->xmin + (int)(grfp->x_f*(rel_x-grfp->x_1)) ;
      phys_y = grfp->ymin + (int)(grfp->y_f*(rel_y-grfp->y_1)) ;
   }
   p_write(grfp,string,phys_x,phys_y,verschieb,font) ;
}

/********************************************************************
*                   Wahl des Linien Types                           *
*               ltype     1 - 6                                     *
*               lrel      ca 1.0   lÑnge des Musters                *
********************************************************************/

void grf_ltyp(GRF *grfp,int ltyp,double lrel)

{
   if ( grfp->magic != MAGIC2 ) {
      grf_err |= 8 ;
      return ;
   }
   if ( grfp->ltyp != ltyp || grfp->lrel != lrel ) {
      grf_flush(grfp) ;

      grfp->ltyp = ltyp ;
      grfp->lrel = lrel ;
      grfp->mrest = 0.0 ;
      grfp->mindex = grfp->mflag = 0 ;
   
      switch(grfp->dev) {
         case 3 :
            hp_linetype( ltyp, lrel ) ;
         break ;
         case 4 :
         case 5 :
            if ( ltyp ) {
               fprintf(grfp->fp,";LT%d,%f\n",ltyp,lrel) ;
            } else {
               fprintf(grfp->fp,";LT\n") ;
            }
         break ;
      }
   }
}

/********************************************************************
*    File b an File a anhÑngen                                      *
********************************************************************/

static int fcat(char a[],char b[])

{
   int c ;
   FILE *fpa,*fpb ;

   if ( (fpa = fopen(a, "a")) == NULL) {
      grf_err |= 4 ;
      return(-1) ;
   }
   if ( (fpb = fopen(b, "r")) == NULL) {
      grf_err |= 4 ;
      return(-1) ;
   }

   while ((c = getc(fpb)) != EOF)
      putc(c, fpa) ;

   fclose(fpb) ;
   fclose(fpa) ;
   return(0) ;
}

/********************************************************************
*    fuer linetype() in grf3b.c                                     *
********************************************************************/

double hp_plxmax()

{
   return(plxmax) ;
}

/********************************************************************
*         Setzen eines Punktes auf dem default Geraet               *
*         grf_def = SCREEN1                                         *
*         grf_def = SCREEN2                                         *
*         Koordinaten x,y                                           *
********************************************************************/ 

void grf_dot( GRF *grfp, int x, int y ) {
   int device;
           /* Sicherheit fuer alle global ansprechbaren Funktionen */
   if ( grfp->magic != MAGIC1 && grfp->magic != MAGIC2 ) {
      grf_err |= 2 ;
      return ;
   }
   device = grfp->dev;
   if ( device == 2 || device == 1 ) {
      grf_scrdot(grfp,x,y);
   } else if ( device == 3 ) {
      hp_dot(x,y) ;
   } else if ( device == 4 || device == 5 ) {
      fprintf(grfp->fp,";PU%d,%d;PD;PU", x, y );
      grfp->count = 0 ;
   }
}
/********************************************************************
*   X - Auflîsung des AusgabegerÑtes                                *
********************************************************************/ 

static int grf_xmax(int device)
{
   switch(device) {
      case 3 :
      case 4 :
      case 5 :
         return(plxmax) ;
     case 6 :
         return(psxmax) ;
     case 7:
         return(ptxmax) ;
     case 8:
         return(pfxmax) ;
   }
   return(grf_scxmax(device))  ;
}
/********************************************************************
*   Y - Auflîsung des AusgabegerÑtes                                *
********************************************************************/ 

static int grf_ymax(int device)
{
   switch(device) {
      case 3 :
      case 4 :
      case 5 :
         return(plymax) ;
     case 6 :
         return(psymax) ;
     case 7: 
         return(ptymax) ;
     case 8:
         return(pfymax) ;
   }
   return(grf_scymax(device))  ;
}

void grf_invtrafo( GRF *grfp, double *xp, double *yp, int x, int y ) 
{
   if( grfp->magic != MAGIC2 ) {
      *xp = *yp = 0;
      grf_err |= 8;
   } else {
      *xp = grfp->x_1 + (double)(x-grfp->xmin) / grfp->x_f;
      *yp = grfp->y_1 + (double)(y-grfp->ymin) / grfp->y_f;
   }
}


/******** EOF *******************************************************/

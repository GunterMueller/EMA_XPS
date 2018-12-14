/***************************************************************
*               grf3.h       25.01.91                          *
*     von                  JENS ONNO KRAH                      *
*     Enviroment    DIN=0 ... DIN=6  default 4                 *
***************************************************************/

typedef struct grf_var {
   struct grf_var  *next ;
   int    dev ;             /*    Tabelle zum ermitteln der Geraete    */
   int    pt ;              /*    Pen type                             */
   int    color ;           /*    color  default 15                    */
   int    font ;            /*    Schriftgroesse                       */
   double fg ;              /*    Fill grad                            */
   int    count ;           /*    Zaehler fuer EPS                     */
   FILE  *fp ;              /*    File deskriptoren                    */
   int    aflg ;            /*    Flag fÅr Vektor Optimierung          */
   double vs ;              /*    Vektorsumme                          */
   double del ;             /*    zul. LÑngenabweichung                */
   double x_1,y_1 ;
   double x_f,y_f ;
   double cx_1,cy_1,cx_2,cy_2 ;  /*  Clipping                          */
   double xfa,yfa ;
   double i_dx,i_dy ;
   double xfm,yfm ;
   int    x_a,y_a ;          /* alte Position des i_move/draw          */
   int    x_a2,y_a2 ;        /* alte Position von grf_line int !       */
   int    x_fg,y_fg ;        /*   Startwert fill   */
   int    xmin,xmax,ymin,ymax ;
   int    fi_st ;            /*    File-Status                         */
   char   name[64] ;         /*   Filename                             */
   int    dsc ;              /*    Nummer des Virtuellen Files         */

   int    ltyp ;             /*  plotter-linetype-emulation (cibby)    */
   double lrel ;
   double mrest ;
   int    mindex ;
   int    mflag ;
   long   magic ;
   long   seek ;            /*    Fileposition fuer Xfig-Ausgabe       */
} GRF ;

extern GRF  *grf_open(char *) ;
extern int   grf_error(int) ;
extern int   grf_close(GRF *) ;
extern void  grf_flush(GRF *) ;
extern void  grf_closeall(void) ;
extern void  grf_clr(char *) ;
extern int   grf_show(char *) ;
extern void  grf_move(GRF *,double,double) ;
extern void  grf_draw(GRF *,double,double) ;
extern void  grf_clip(GRF *,double,double,double,double,int) ;
extern int   grf_koor(GRF *,double,double,double,double,
                 double,double,double,double,
                 int,int,int,int,int,int,char *,char *,double,double) ;
extern void  grf_dot(GRF *,int,int) ;
extern void  grf_pen(GRF *,int) ;
extern void  grf_color(GRF *,int) ;
extern int   grf_font(GRF *,int i) ;
extern void  grf_speed(GRF *,int) ;
extern void  grf_ltyp(GRF *,int,double) ;
extern int   grf_wait(void) ;
extern void  grf_write(GRF *,char *,double,double,int,int,int) ;
extern void  grf_sleep(unsigned) ;
extern char *grf_ver() ;

#ifdef DREI_D

#define  HOME      0
#define  TAKE      1
#define  N_VECT    256

extern void   g3d_move(GRF *,double,double,double) ;
extern void   g3d_draw(GRF *,double,double,double) ;
extern void   g3d_hmove(GRF *,double,double,double) ;
extern void   g3d_hdraw(GRF *,double,double,double) ;
extern void   g3d_write(GRF *,char *,double,double,double,int,int) ;
extern void   g3d_clrhline(void) ;
extern void   g3d_hline(GRF *) ;
extern int    g3d_koor(GRF *,
                     double,double,double,double,
                     double,double,double,double,double,double,
                     double,double,double,double,double,int,int,int,
                     char *,char *,char *,double,double,double ) ;
extern void   g3d_xprint(GRF *,int,int,int,int,int,int,double) ;
extern void   g3d_yprint(GRF *,int,int,int,int,int,int,double) ;
extern void   g3d_zeich(GRF *,double (*)(double x,double y),
                      int,int,int,int,int,int,int,int,int,int,int) ;

#endif

#define INVERS      0
#define NORMAL      1
#define EXOR        2

#define ZENTRIERT   0
#define UNTERHALB   1
#define LINKSB      2
#define OBERHALB    3
#define RECHTSB     4
#define ECKE_LI_OB  5
#define ECKE_LI_UN  6
#define ECKE_RE_OB  7
#define ECKE_RE_UN  8

#define RELATIV     0
#define REEL        1

#ifdef GRFINTERNAL

extern char  *grf_assign(char *,int *) ;
extern void   grf_sline(GRF *,int,int,int,int) ;
extern void   grf_scrdot(GRF *,int,int) ;
extern void   grf_twrite(GRF *grfp,int,int,char *,int) ;
extern int    grf_scxmax(int) ;
extern int    grf_scymax(int) ;
extern int    grf_fontx2(int);
extern int    grf_fonty2(int);
extern char  *hp_sio(void) ;
extern int    hp_get(void) ;
extern int    hp_line(void) ;
extern int    hp_cat(int) ;
extern void   hp_plon(int) ;
extern void   hp_close(void) ;
extern void   hp_open(int) ;
extern void   hp_reset(void) ;
extern void   hp_linetype(int,double);
extern void   hp_selpen(int);
extern void   hp_speed(int);
extern void   hp_move(int,int);
extern void   hp_draw(int,int);
extern void   hp_print(char*);
extern void   hp_si(double,double);
extern void   hp_dot(int,int);
extern void   hp_init(int) ;
extern void   hp_exit(void) ;
extern void   hp_put(int) ;
extern double hp_plxmax(void) ;
extern void grf_invtrafo( GRF*, double *xp, double *yp, int x, int y ) ;

#define PLXMAX  11000
#define PLYMAX   7600
#define PSXMAX    350
#define PSYMAX    250
#define PTXMAX  16000
#define PTYMAX  12000
#define PFXMAX   9000
#define PFYMAX   6750

/* If not defined in system header files... */
#ifndef SEEK_SET
#   define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#   define SEEK_CUR 1
#endif
#ifndef SEEK_END
#   define SEEK_END 2
#endif

#endif

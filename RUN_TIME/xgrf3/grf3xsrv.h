/*********************************************************************
*   grf3scr.h              Hans Groschwitz                22.10.93   *
*                                                                    *
*      Protokoll fuer die Bildschirm-Steuerung unter OSF/Motif       *
*                                                                    *
*                          Jens Onno Krah                            *
*      grf3ibm.c ........................................ 19.03.90   *
*      Turbo schiss ..................................... 26.04.90   *
*      Reset Point ...................................... 22.05.90   *
*                          Hans Groschwitz                           *
*      grf3apollo.c ..................................... 25.01.91   *
*      grf3M.c .......................................... 17.05.91   *
*      Aufteilung in mehrere Files ...................... 23.05.91   *
*      grf3_mot.c ....................................... 23.05.91   *
*      grf3mot.c (kein Aegis mehr) ...................... 20.10.93   *
*      grf3scr.c (Motif-Routinen ausgelagert) ........... 21.10.93   *
*      grf3scr.h Protokoll beschleunigt ................. 22.10.93   *
*                                                                    *
*   Noch nicht fertig:                                               *
*                                                                    *
*      Momentan wird nicht zwischen screen1 und screen2              *
*      unterschieden.                                                *
*********************************************************************/

#ifdef DEBUG
#  define dbgpr(a)        fprintf(stderr,a),      fflush(stderr)
#  define dbgpr1(a,b)     fprintf(stderr,a,b),    fflush(stderr)
#  define dbgpr2(a,b,c)   fprintf(stderr,a,b,c),  fflush(stderr)
#  define dbgpr3(a,b,c,d) fprintf(stderr,a,b,c,d),fflush(stderr)
#else /* no DEBUG */
#  define dbgpr(a)
#  define dbgpr1(a,b)
#  define dbgpr2(a,b,c)
#  define dbgpr3(a,b,c,d)
#endif /* DEBUG */

/*** Defines ********************************************************/

#define M_STRLEN     1024
#define M_NORMAL     1
#define M_INVERS     2
#define M_EXOR       3

#define MC_NRCOLORS  16
#define MC_WHITE     0
#define MC_DEEPBLUE  1
#define MC_DEEPGREEN 2
#define MC_LIGHTBLUE 3
#define MC_DEEPRED   4
#define MC_MAGENTA   5
#define MC_BROWN     6
#define MC_LIGHTGRAY 7
#define MC_DARKGRAY  8
#define MC_BLUE      9
#define MC_GREEN     10
#define MC_TURQUOISE 11
#define MC_RED       12
#define MC_ROSE      13
#define MC_YELLOW    14
#define MC_BLACK     15

/*
 * Protokoll:
 * 
 * Primitivst gewaehlt, um moeglichst geringe Datenuebertragungszeiten
 * gewaehrleisten zu koennen. zB laeuft waehrend eines Snapshot's 
 * der Ringpuffer voll. Damit moeglichst viel hineinpasst, sollten
 * ebenfalls die Befehlstoken kurz sein.
 */
#define TOKENLEN  1                /* Laenge der Befehls-Abkuerzung */
#define MS_Clr    "I"                  /* Clear Screen (Init Scrn)  */
#define MS_Color  "C%d"                /* Drawing (FG)Color         */
#define MS_Style  "D%d"                /* Drawing Style             */
#define MS_Font   "F%d"                /* Setting new Font          */
#define MS_Line   "L%d,%d,%d,%d"       /* Draw a Line               */
#define MS_More   "M%d,%d"             /* Draw a continuing Line to */
#define MS_Point  "P%d,%d"             /* Draw a Point              */
#define MS_Str    "S%d,%d,%s"          /* Textstring follows        */
#define MS_Exit   "X"                  /* Make Server kill itself   */
#define MS_Wait   "W"                  /* Wait for a Mouse-Click    */
#define MS_Ok     "O%d"                /* Ready Prompt              */
#define MS_Mask   "Z%d"                /* InterruptMask fuer Maus   */
#define MS_Cursor "Y%d"                /* MousePointer Form         */

/*
 * Der client muss wissen, was fuer Settings der Server hat.
 * Es ist keine Uebertragung vorgesehen. Die folgenden Defines
 * liest auch der Client:
 */
#define SCRWIDTH  896                 /* Pixmap: width = 0.7 * 1280 */
#define SCRHEIGHT 717                 /* Pixmap: hight = 0.7 * 1024 */
#define SCRXMAX   (SCRWIDTH-1)        /* Breite=100 => 0...99 Pixel */
#define SCRYMAX   (SCRHEIGHT-1)
#define MAXFONTS  4
/*
 * In der Protokoll-HeaderDatei werden diese Werte als Konstanten
 * gesetzt. Bei der Wahl neuer Fonts sollte grf3scr.c mit DEBUG
 * und STANDALONE rekompiliert werden. Der folgende Output kann dann
 * in die HeaderDatei uebernommen werden.
 */
#define FONT1_X   6
#define FONT1_Y   4
#define FONT2_X   7
#define FONT2_Y   6
#define FONT3_X   9
#define FONT3_Y   6
#define FONT4_X   11
#define FONT4_Y   8

/*** EOF ************************************************************/

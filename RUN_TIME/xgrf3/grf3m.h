/*********************************************************************
*   grf3m.h                Hans Groschwitz                08.11.93   *
*                                                                    *
*      Maus-Routinen zur Bildschirmsteuerung                         *
*                                                                    *
*                           Dr. B. Stuetz                            *
*      mouse.pas (Microsoft Mouse Interface Routines) ... xx.xx.88   *
*                          Hans Groschwitz                           *
*      maus.c (Lattice C und Herkules-Karte) ............ 27.09.89   *
*      grf3m.c (Eingebracht in Xgrf3) ................... 08.11.93   *
*      Abgespeckt auf grf_m*() Bedarf ................... 15.11.93   *
*                                                                    *
*********************************************************************/

extern int grf_minit( void );
extern int grf_mexit( void );
extern int grf_msetint( int mask, void *funcp );
extern int grf_mquery( int *buttonState, int *xPos, int *yPos );
extern int grf_mform( int form );

/*********************************************************************
*   Moegliche Formen des Graphik-Maus-Zeigers                        *
*   -> grf_mform()                                                   *
*********************************************************************/

#define MF_STNDRT  0
#define MF_PTHAND  1
#define MF_HOURGL  2
#define MF_DIAGX   3
#define MF_RECTX   4
#define MF_LARROW  5
#define MF_RARROW  6
#define MF_UARROW  7
#define MF_DARROW  8
#define MF_INVERT  9
#define MF_CHKMSK  10

#define MF_MAX     MF_CHKMSK            /* letzte gueltige MausForm */

/*********************************************************************
*   Moegliche Ereignisse, die einen Mausinterrupt ausloesen          *
*   -> grf_msetint()                                                 *
*********************************************************************/

#define MI_NONE      0     /* keine Ereignisse */
#define MI_MMOVED    1     /* Cursorposition geaendert */
#define MI_LEFTBPR   2     /* linker Knopf gedrueckt */
#define MI_LEFTBREL  4     /* linker Knopf losgelassen */
#define MI_RIGHTBPR  8     /* rechter Knopf gedrueckt */
#define MI_RIGHTBREL 0x10  /* rechter Knopf losgelassen */
#define MI_MIDBPR    0x20  /* mittlerer Knopf gedrueckt */
#define MI_MIDBREL   0x40  /* mittlerer Knopf losgelassen */

/*********************************************************************
*   Format der UserDefinierten InterruptServiceRoutine               *
*                                                                    *
*   void user_isr( int evnt, int state, int posx, int posy ) { ... } *
*                                                                    *
*      evnt      -> Event Control Word (Bits wie Control Mask)       *
*      state     -> Button Status      (siehe unten !)               *
*      posx      -> Cursor Position    (Horizontal)                  *
*      posy      -> Cursor Position    (Vertical)                    *
*                                                                    *
* ACHTUNG: diese UserFunktion wird durch einen Interrupt aufgerufen. *
*          Dadurch koennen BetriebssystemRoutinen unterbrochen sein. *
*          MS-DOS ist NICHT wiedereintrittsfaehig !!!                *
*          Daher nach Mglichkeit auf solche Aufrufe verzichten.      *
*          Auch unter UNIX die Routine kurz halten, weil sie mit     *
*          signal() einen Interrupt nachbildet. In dieser Zeit kann  *
*          die Applikation nichts anderes machen.                    *
* HINWEIS: Waehrend eines Interrupts koennen KEINE grf_m*() Aufrufe  *
*          ausgefuehrt werden !                                      *
*********************************************************************/

#define MS_NONE      0          /* kein Knopf gedrueckt */
#define MS_LEFTB     1          /* linker Knopf gedrueckt */
#define MS_RIGHTB    2          /* rechter Knopf gedrueckt */
#define MS_MIDB      4          /* mittlerer Knopf gedrueckt */

/*********************************************************************
*        Internal !!!                                                *
*********************************************************************/

/* grf3scr.c (u.a.) ... */
typedef struct {
   int lastEvent;     /* wird von InterruptServiceRoutine benoetigt */
   /* Motion... */
   int xPos, yPos;                      /* Position des Mauszeigers */
   /* Button... */
   int buttonState;           /* welche MouseButtons sind gedrueckt */
} MOUSE;

/* grf3ipc.c ... */
extern void _m_write( MOUSE*, int, int );
extern void _m_read( MOUSE*, int, int );

/* grf3mot.c ... (ISR) */
extern int  _m_screenp( void );    /* prueft auf grf_show("screen") */
extern void _m_set_mform( int );                /* via GRFprotokoll */
extern void _m_set_int_mask( int );             /* via GRFprotokoll */
extern void (*_m_isr)();         /* FktPointer f. IntServiceRoutine */
extern int  _m_servicing_int;           /* Flag: 1 = within UserISR */

/*** EOF ************************************************************/


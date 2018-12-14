/*********************************************************************
*   grf3m.c                Hans Groschwitz                08.11.93   *
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

#include <stdio.h>                                          /* NULL */
#include "grf3m.h"                               /* fuer MOUSE, ... */

/*** Defines ********************************************************/

#define ERR   (-1)
#define OK    0

/*** Globals ********************************************************/

static int mflag = 0;

/*********************************************************************
*  Initialisierung                                                   *
*     0 = ok;                                                        *
*    -1 = error:  Maustreiber nicht installiert                      *
*********************************************************************/

int
grf_minit( void ) {

   if( _m_servicing_int )
      return ERR;
   if( !_m_screenp() ) {
      mflag = 0;
      return ERR;
   }
   mflag = 1;
   grf_msetint( MI_NONE, NULL );
   grf_mform( MF_STNDRT );
   return OK;
}

/*********************************************************************
*  Maus Exit Handler                                                 *
*********************************************************************/

int
grf_mexit( void ) {

   if( _m_servicing_int || !mflag )
      return ERR;
   if( !_m_screenp() ) {
      mflag = 0;
      return ERR;
   }
   grf_msetint( MI_NONE, NULL );
   grf_mform( MF_MAX+1 );               /* Invis = Ghost setzen ... */
   mflag = 0;
   return OK;
}

/*********************************************************************
*  teilt dem Maustreiber die Adresse der Mausinterrupt-Routine und   *
*  die Event-Maske mit.                                              *
*********************************************************************/

int
grf_msetint( int m, void *p ) {

   if( _m_servicing_int || !mflag )
      return ERR;
   if( !_m_screenp() ) {
      mflag = 0;
      return ERR;
   }

   _m_set_int_mask( m );               /* Sync'ed Client und Server */
   if( m == MI_NONE )
      _m_isr = NULL;
   else
      _m_isr = p;               /* Pointer auf die Interruptroutine */
   return OK;
}

/*********************************************************************
*  ermittelt Position des Mauszeigers und den Status der Knoepfe     *
*********************************************************************/

int
grf_mquery( int *state, int *x, int *y ) {
   MOUSE m;

   if( _m_servicing_int || !mflag )
      return ERR;
   if( !_m_screenp() ) {
      mflag = 0;
      return ERR;
   }

   _m_read( &m, 1, 1 );                              /* polling ... */
   *x = m.xPos;
   *y = m.yPos;
   *state = m.buttonState;
   return OK;
}

/*********************************************************************
*  setzt neue Form des Mauszeigers                                   *
*********************************************************************/

int
grf_mform( int form ) {

   if( _m_servicing_int || !mflag )
      return ERR;
   if( !_m_screenp() ) {
      mflag = 0;
      return ERR;
   }

   _m_set_mform( form );                               /* asynchron */
   return OK;
}

/*** EOF ************************************************************/


/*********************************************************************
*   grf3_ipc.h             Hans Groschwitz                23.05.91   *
*                                                                    *
*      Zielgeraetabhaengige Unterprogramme zur                       *
*      Inter-Prozess-Kommunikation mittels                           *
*      Ringpuffern im UNIX-Shared-Memory                             *
*                                                                    *
*                          Jens Onno Krah                            *
*      grf3ibm.c ........................................ 19.03.90   *
*      Turbo schiss ..................................... 26.04.90   *
*      Reset Point ...................................... 22.05.90   *
*                          Hans Groschwitz                           *
*      grf3apollo.c ..................................... 25.01.91   *
*      grf3M.c .......................................... 17.05.91   *
*      Aufteilung in mehrere Files ...................... 23.05.91   *
*      grf3_pip.c (grf3_pip.h) .......................... 23.05.91   *
*********************************************************************/

#define C2S     0       /* Direction of Transfer: Client --> Server */
#define S2C     1       /* Direction of Transfer: Client <-- Server */

/*** Externals ******************************************************/

extern SHM *grf_pip_make( int* );
extern int  grf_pip_kill( void );
extern void grf_pip_push( int, char* );
extern int  grf_pip_pop(  int, char* );

/*** EOF ************************************************************/


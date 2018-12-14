/*********************************************************************
*   grf3sipc.h             Hans Groschwitz                23.05.91   *
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
*      grf3_ipc.c (grf3_ipc.h) .......................... 23.05.91   *
*      Abgespeckt: nur noch SharedMemory ................ 27.10.93   *
*      Semaphore (gegen Peaks in Grafik) ................ 28.10.93   *
*      MOUSE-Struktur eingebracht ....................... 08.11.93   *
*      Separater Prozess: xgrf3d (grf3sipc, grf3xsrv) ... 03.08.94   *
*********************************************************************/

#define EOS        '\n'
#define OK         0
#define ERR        (-1)
#define EMPTY_PIPE 0                  /* entspricht: strlen = 0 !!! */
#define STRLEN     1024

/*
 * Ringpuffer-Festlegungen
 * S->C kann KLEIN bleiben
 */
#define BCSLEN 1024                                 /* BufferLenght */
#define BSCLEN   16
#define TCPLEN  256                   /* langer RechnerName ohne :0 */

typedef struct {
   int rsc;           /* current ReadPosition, S -> C communication */
   int wsc;           /* current WritePos. ...                      */
   int rcs;
   int wcs;
   int bcsSema;
   int bscSema;
   char bcs[BCSLEN];
   char bsc[BSCLEN];
   MOUSE m;                                    /* MouseIPC Struktur */
   int m_sema;

   char tcpip[TCPLEN];                   /* Destination DisplayName */
   int clientPid;                       /* koennen sich so wechsel- */
   int serverPid;                                  /* seitig killen */
} SHM;

#define MLEN    sizeof(SHM)
#define RSC     (shmp->rsc)            /* Read- and Write-Positions */
#define WSC     (shmp->wsc)
#define RCS     (shmp->rcs)
#define WCS     (shmp->wcs)
#define BCSsema (shmp->bcsSema)                        /* Semaphore */
#define BSCsema (shmp->bscSema)
#define BCSp    (shmp->bcs)               /* Pointer to BufferBegin */
#define BSCp    (shmp->bsc)
#define Msema   (shmp->m_sema)

#define SemaWait(sem) while(sem)usleep(100)

/*** Prototypes *****************************************************/

extern SHM *ipc_open( int );

/*** EOF ************************************************************/

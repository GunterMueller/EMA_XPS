#define PR
/*********************************************************************
*   zhp.c                  Jens Onno Krah                 05.09.90   *
*                                                                    *
*      Programm zur Aufbereitung von HPGL Files fuer beliebige       *
*      Ausgabegeraete                                                *
*                                                                    *
*                          Jens Onno Krah                            *
*      hp.c ............................................. 07.03.88   *
*                                                                    *
*                          Stefan Roehrig                            *
*      zus. HPGL-Befehle Peters/Cibbys Grafik ........... 14.08.90   *
*                                                                    *
*                          Andrew Hall                               *
*      allgemeine Ausgaben .............................. 03.05.91   *
*      PA, PD, PU sind erweitert worden ................. 29.04.91   *
*         Urspruenglicher default Wert wird nur einmal ausgegeben    *
*         SC wird durch grf_koor durchgefuehrt.                      *
*         IW und RO wird beruecksichtigt                             *
*                                                                    *
*                          Hans Groschwitz                           *
*      auf Apollo lauffaehig ............................ 20.08.91   *
*                                                                    *
*                  Jens Onno Krah + Hans Groschwitz                  *
*      HITACHI-Befehlsformat, Code gestrafft ............ 04.09.91   *
*                                                                    *
*                            Michael Zech                            *
*      HITACHI Oszilloskop verwendet auch: `pr' ......... 15.08.94   *
*                                                                    *
*                          Hans Groschwitz                           *
*      HPGL-parser (getk()) neu = state-mashine ......... 15.08.94   *
*      KommandozeilenOptionen -E und -v ................. 15.08.94   *
*      EnvironmentVars fuer die Devices abgestellt ...... 16.08.94   *
*                                                                    *
*   Noch nicht fertig:                                               *
*                                                                    *
*      LB: wenn direkt aufeinander folgend aufgerufen, sollten die   *
*         neuen Anfangskoordinaten hinter das letzte Zeichen des     *
*         vorangegangenen Aufrufes gesetzt sein!                     *
*      DT: LabelEndeChar: alles ausser 0,5,10,27,59 - ohne param=3   *
*         muss vom parser ausgewertet werden, NICHT von Jens Umsetzg.*
*                                                                    *
*      !!! Die Befehle, die bereits realisiert sind, sind teilweise  *
*      !!! unvollstaendig und setzen eine korrekte Syntax voraus.    *
*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

#ifndef APOLLO
#include "f:\grf\grf3.h"
#else
#include "grf3.h"
#endif

/*** Defines ********************************************************/

#define HPGLMAX   32000     /* max. LineLength of a single HPGL-instruction */

#define ESC          27
#define CR           13
#define LF           10
#define TRUE          1
#define FALSE         0

       /* Default-User-Koordinaten, die P1 und P2 zugeordnet werden */
#define XMINDEF     0.0
#define XMAXDEF 11000.0
#define YMINDEF     0.0
#define YMAXDEF  7600.0

#define NCMD 57                       /* 56 HP-GL Befehle + ESC ..  */

/*** Globals ********************************************************/

GRF *dsc;
double xmin, xmax, ymin, ymax;                  /* Koordinatendaten */
double xul, yul, xor, yor;                 /* absolute Plotterdaten */
int rot;                                        /* 90 Grad Rotation */
int pen ;                                       /* 0 = up 1 = down  */
int font ;                                      /* 2 default        */
double tp,tn ;                                  /*  tick            */
#ifdef PR
double xalt, yalt;       /* M. Zech: pr realisiert (Hitachi Scopes) */
#endif
int verbose;
int pretty_print = 0;
int warnings = 0;
int end_of_label = 3;       /* ^C */
int instno;

char *cmd[NCMD] = { 
   "aa","ar","ca","ci","cp","cs","dc","df","di","dp",
   "dr","dt","ea","er","ew","ft","im","in","ip","iw",
   "lb","lt","oa","oc","od","oe","of","oh","oi","oo",
   "op","os","ow","pa","pd","pr","ps","pt","pu","ra",
   "ro","rr","sa","sc","si","sl","sm","sp","sr","ss",
   "tl","uc","vs","wg","xt","yt","\033."
} ;
char nop_cmd[NCMD] = {
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0
} ;

/*********************************************************************
*     Default-Werte fuer HP7475A. Werte sind dem Handbuch entnommen. *
*     Fuer DIN A3 muessen nur die Defaultwerte geaendert werden.     *
*********************************************************************/

void set_defaults( void ) {

   xmin = XMINDEF;
   xmax = XMAXDEF;
   ymin = YMINDEF;
   ymax = YMAXDEF;
   
   xul  = XMINDEF;
   yul  = YMINDEF;
   xor  = XMAXDEF;
   yor  = YMAXDEF;
   
   rot  = FALSE;
   pen  = 0 ;  
   font = 2 ;
   tp   =  tn  =  0.5 ;

   end_of_label = 3;
}

/*********************************************************************
*     Erzeugung des ursprunglichen Fontwerts                         *
*        fuer grf_write aus den Werten der HPGL-Datei.               *
*********************************************************************/

int traf_fontwert( double px, double py ) {

   static double fontval[2][4] = {
     { 0.0929, 0.0929, 0.1393, 0.1548 },
     { 0.2053, 0.2463, 0.3284, 0.4106 }
   };
   double val, minval = 1e10;
   int i, imerk = 0;

   for( i = 0 ; i < 4 ; i++ ) {
      val = (px - fontval[0][i]) * (px - fontval[0][i]) +
            (py - fontval[1][i]) * (py - fontval[1][i]);
      if ( val < minval ) {
         minval = val;
         imerk = i;
      }
   }
   return( imerk );
}
/*********************************************************************
*     Ermitteln der Nummer eines Befehls                             *
*********************************************************************/

int cmd_to_num(int c1,int c2)
{
   int num ;
   for( num = 0; num < NCMD; num++ ) {
      if( c1 == cmd[num][0] && c2 == cmd[num][1] ) {
         return(num) ;
      }   
   }
   return(-1) ;
}  


/*********************************************************************
*     EnvironmentVariable auf gerantierten Wert setzen !             *
*********************************************************************/

void
fix_env( char *str ) {
   char *p, *q, *env;
   int len;

   len = 2 * (1 + strlen(str));
   if( !(env = (char*)malloc( len )) ) {
      perror("malloc");
      exit( -1 );
   }
   p = str;
   q = env;
   while( *q++ = toupper(*p++) );
   q--;
   *q++ = '=';
   p = str;
   while( *q++ = tolower(*p++) );
   *q = 0;

   if( putenv( env ) ) {
      perror("putenv");
      exit( -1 );
   }
}

/*********************************************************************
*     Einlesen eines Kommandos                                       *
*********************************************************************/

int getk( char* buffer, FILE* fp ) {
#ifdef JENS_ALTE_VERSION
   int i, j;
   static int buf_fl, ch1, ch2;

   i = 0;
   while( 1 ) {
      if( buf_fl ) {
         buf_fl = 0;
      } else {
         do {
            ch1 = tolower( fgetc( fp ) );
         } while( ch1 == LF || ch1 == CR || ch1 == ' ' );
         if( ch1 == 26 || feof(fp) ) {           /* ^Z bei PC-files */
            buffer[i] = 0;
            return( i );
         }
         do {
            ch2 = tolower(fgetc(fp));
         } while( ch2 == LF || ch2 == CR || ch2 == ' ' );
         
         ungetc( ch2, fp );
      }
      if ( i ) {
         if( ch1 == ';' ) {
            buffer[i] = 0;
            return( i );
         }
         if ( cmd_to_num(ch1,ch2) != -1 ) {
            buf_fl = 1;   /* beim naechsten Einsprung altes ch1 verwenden ! */
            buffer[i] = 0;
            return(i);
         }   
      }
      if( ch1 == 3 || ch1 == ';' )
         continue;     /* fuehrendes ASCII 3 und ; elemenieren -> HITACHI */ 

      buffer[i++] = ch1 ;

      if( i == 1 && ch1 == 'l' && ch2 == 'b' ) {  /*  Label Test */
         while(1) {
            do {
               ch1  = fgetc(fp) ;
               buffer[i] = ch1 ;
               i++ ;
            } while( ch1 != 3 );
            ch1 = tolower(fgetc(fp)) ;
            ch2 = tolower(fgetc(fp)) ;
            if ( ch1!='l' || ch2!='b' ) {
               buf_fl = 1 ;       /*  Zeichen zurck ! */
               ungetc(ch2,fp) ;
               break ;
            }
            i-- ;    /*  3 aus dem String   */
         } ;
         buffer[1] = tolower( buffer[1] );
         buffer[i] = 0;
         return( i );
      }
   }
#else
   /******************************************************************
   *   Understanding HP-GL2 Syntax      (from HP DraftMaster Doku)   *
   *                                                                 *
   *   Each HP-GL instruction consists of a two-letter mnemonic, a   *
   *   parameter(s), and a terminator. The HP-GL instruction begins  *
   *   with a two-letter mnemonic (upper- or lowercase) designed to  *
   *   remind you of its function. Some instructions have no para-   *
   *   meters: in others, parameters are optional. If you use para-  *
   *   meters, you must separate them from each other with at least  *
   *   one COMMA or SPACE, or with a + or - sign, which may be       *
   *   preceded by commas or spaces. All instructions require a      *
   *   terminator. In its basic form, an HP-GL instruction looks     *
   *   like this:                                                    *
   *                mnemonic   separator   terminator                *
   *                       |   |  ,--------'                         *
   *                       PA30,30;                                  *
   *                          | |                                    *
   *                        parameters                               *
   *   HP-GL instructions are terminated by a semicolon or the first *
   *   letter of the next mnemonic. ... HP-GL instructions are also  *
   *   terminated by a line feed (LF).                               *
   *   The label and the buffer label instructions (LB and BL) and   *
   *   the write-to-display instruction (WD) are special cases: they *
   *   must be terminated with the label terminator. The label ter-  *
   *   minator defaults to the ASCII end-of-text character, ETX      *
   *   (decimal code 3), but may be changed from its default value   *
   *   using the define terminator instruction (DT).                 *
   *   Only the terminator and the separators between parameters     *
   *   are required. You can insert separators between the letters   *
   *   of the menmonic and before parameters, but it isn't good      *
   *   programming practice. Extra separators will make your programs*
   *   difficult to read and can affect compatibility with other     *
   *   plotters.                                                     *
   *   ... commas as separators are recommended, because some com-   *
   *   puters eliminate spaces, especially when sending variables.   *
   *   Commas are also necessary for complete compatibility with     *
   *   earlier plotters.                                             *
   *   To ilustrate the flexibility of the syntax, the following     *
   *   two-instruction sequence is presented in three ways. Spaces   *
   *   are shown between the mnemonic and and the first parameter    *
   *   in example 2. Two types of terminators are used: the semi-    *
   *   colon and the first letter of the next mnemonic. Example 1    *
   *   shows the recommended programming practice.                   *
   *   PD;PU10,20;          PD PU 10 20;         PDPU10,20;          *
   *                                                                 *
   *   omitting optional parameters:                                 *
   *   If you were to omit the second parameter, you must also omit  *
   *   the third parameter !!!                                       *
   *   FT3,,45;  --> the plotter would interpret 45 as the SECOND    *
   *   parameter!!!                                                  *
   *                                                                 *
   *   parameter formats:                                            *
   *   Integer: from -8388608 to 8388607  (float parameters that     *
   *   must be int are automatically rounded by the plotter)         *
   *   Real: a number where the integer portion is as shown above,   *
   *   and the optional decimal fraction has a maximum of 10 signi-  *
   *   ficant digits. The decimal point may be omitted when no       *
   *   decimal fraction is specified.                                *
   ******************************************************************/
#define DBGn

   int i, c, mn1, mn2, point;
   enum { mnemonic1, mnemonic2, sep_in_mnem, separator, 
          parameter, terminator, label, esc1, esc2, esc3 } state;

   i = point = 0;
   state = terminator;
   while( 1 ) {
      if( i >= HPGLMAX ) {
         fprintf( stderr,
            "error[%d]: truncating instruction: line too long\n",
            instno );
         buffer[i] = 0;
         return( i );
      }
      c = fgetc( fp );
      if( feof(fp) ) {
         buffer[i] = 0;
         return( i );
      }
#ifdef DBG
      {
         int d = c & 0x7f;

         usleep( 100 );
         fprintf( stderr, "c=%d", c );
         if( d >= ' ' && d < 127 )
            fprintf( stderr, " [%c]", d );
         fprintf( stderr, "\n" );
      }
#endif
      if( c == 26 ) {                            /* ^Z bei PC-files */
         fprintf( stderr, "info[%d]: ^Z = logical EOF received. aborting...\n", instno );
         buffer[i] = 0;
         return( i );
      }

      /* HPGL only knows 7 Bit ASCII charset. */
      if( c & 0x80 ) {
         fprintf( stderr, "warning[%d]: ignoring non-ASCII charcode %d\n", instno, c );
         continue;
      }

      /*** entering parser ***/

      if( state == esc3 ) {
         if( c == ':' ) {
            /* end of sequence */
            buffer[i++] = c;
            buffer[i] = 0;
            return i;
         }
         buffer[i++] = c;
         continue;
      }
      if( state == esc2 ) {
         /* ( ) @ A B E H I J K L M N O P Q R S T U Y Z  = instructions */
         /* - - 2 - - - 3 3 - - - 5 3 - 1 1 - 1 6 - - -  = nr of params */
         /* instr.s with params always end with : */
         if( c == '@' || c == 'H' || c == 'I' || c == 'M' || c == 'N' ||
             c == 'P' || c == 'Q' || c == 'S' || c == 'T' ) {
            buffer[i++] = c;
            state = esc3;
            continue;
         }
         if( c == '(' || c == ')' || isupper(c) ) {
            buffer[i++] = c;
            buffer[i] = 0;
            return i;
         }
         fprintf( stderr, "syntax error[%d]: wrong format of ESCAPE-sequence\n", instno );
         buffer[i++] = c;
         buffer[i] = 0;
         return -1;
      }
      if( state == esc1 ) {
         if( c != '.' ) {
            fprintf( stderr,
               "syntax error[%d]: unexpected initialization of ESCAPE-sequence\n",
               instno );
            buffer[i++] = c;
            buffer[i] = 0;
            return -1;
         }
         buffer[i++] = '.';
         state = esc2;
         continue;
      }
      if( state != label ) {

         if( c == ',' || c == ' ' ) {       /* changing state to separator */
            switch( state ) {
               case mnemonic1:
                  state = sep_in_mnem;
                  continue;
               case mnemonic2:
               case sep_in_mnem:
               case separator:
                  continue;             /* do nothing */
               case parameter:
                  buffer[i++] = ',';     /* compare kennt nur Komma */
                  state = separator;
                  continue;
               case terminator:
                  if( c == ' ' ) /* leading blank: do nothing */
                     continue;
                  fprintf( stderr, "syntax error[%d]: comma unexpected\n", instno );
                  buffer[i++] = c;
                  buffer[i] = 0;
                  return -1;
               default:
                  fprintf( stderr, "syntax error[%d]: Hup ??\n", instno );
                  buffer[i++] = c;
                  buffer[i] = 0;
                  return -1;
            }

         } else if( c == '\n' || c == '\r' || c == ';' ) { /* changing state to terminator */
            if( !i ) {
               /*
                * empty instructions in general are not reported:
                * it's just an elimination of leading noise
                */
               state = terminator;
               continue;
            }
            if( i > 0 ) {
               if( buffer[i-1] == ',' )
                  i--;     /* trailing separator eliminieren (s.o.) */
               if( !i ) {
                  fprintf( stderr, "warning[%d]: ignoring empty instruction\n", instno );
                  state = terminator;
                  continue;
               }
            }
            buffer[i] = 0;
            return( i );

         } else if( isalpha(c) ) {           /* changing state to mnemonic */
            switch( state ) {
               case sep_in_mnem:
               case mnemonic1:
                  mn2 = tolower(c);
                  /* ab hier ist der token komplett eingelesen */
                  if( (mn1 == 'l' && mn2 == 'b' ) ||
                      (mn1 == 'b' && mn2 == 'l' ) ||
                      (mn1 == 'w' && mn2 == 'd' ) ) {
                     /* one of the instructions with label_terminator */
                     state = label;
                     buffer[i++] = mn2;
                     continue;
                  }
                  state = mnemonic2;
                  buffer[i++] = mn2;
                  continue;
               case terminator:
                  state = mnemonic1;
                  mn1 = tolower(c);
                  buffer[i++] = mn1;
                  continue;
               case separator:
                  fprintf( stderr, "warning[%d]: separator without trailing parameter\n", instno );
                  if( i > 0 ) {
                     if( buffer[i-1] == ',' ) {
                        i--;     /* trailing separator eliminieren (s.o.) */
                     }
                     if( !i ) {
                        fprintf( stderr,
                           "warning[%d]: ...ignoring empty instruction\n",
                           instno );
                     }
                  }
                  if( !i ) {
                     state = mnemonic1;
                     mn1 = tolower(c);
                     buffer[i++] = mn1;
                     continue;
                  }
                  /* hier KEIN break! */
               case parameter:
               case mnemonic2:
                  buffer[i] = 0;
                  ungetc( c, fp );    /* gehoert schon zum neuen Befehl */
                  return i;
               default:
                  fprintf( stderr, "syntax error[%d]: Hup ?\n", instno );
                  buffer[i++] = c;
                  buffer[i] = 0;
                  return -1;
            }

         } else if( c == '.' || c == '+' ||
                    c == '-' || isdigit(c) ) {  /* changing state to parameter */
            switch( state ) {
               case mnemonic1:
               case sep_in_mnem:
                  fprintf( stderr, "syntax error[%d]: incomplete mnemonic\n", instno );
                  buffer[i++] = c;
                  buffer[i] = 0;
                  return -1;
               case mnemonic2:
               case separator:
                  state = parameter;
                  if( c == '.' )
                     point = 1;
                  else
                     point = 0;
                  buffer[i++] = c;
                  continue;
               case parameter:
                  if( c == '+' || c == '-' || (c == '.' && point) )
                     buffer[i++] = ',';  /* neuer Parameter */
                  buffer[i++] = c;
                  continue;
               case terminator:
                  fprintf( stderr, "syntax error[%d]: parameter without mnemonic\n", instno );
                  buffer[i++] = c;
                  buffer[i] = 0;
                  return -1;
            }

         } else if( c == ESC ) {       /* changing to ESCape mode */
            if( state == terminator ) {
               state = esc1;
               buffer[i++] = ESC;
               continue;
            }
            fprintf( stderr, "syntax error[%d]: ESCAPE within an instruction\n", instno );
            buffer[i++] = c;
            buffer[i] = 0;
            return -1;

         } else if( c == 3 ) { /* ETX */
            /* haben viele, scheint erlaubt zu sen ?!? */
            /* um den Eingabestrom in den Plotter zu initialisieren ? */
            /* --> hp7221 ... */
            /* Aktuellen Befehl ignorieren! ... */
            if( i )
               fprintf( stderr,
                  "warning: ETX aborted incomplete read instruction\n" );
            state = terminator;
            i = 0;
            continue;

         } else {
            /* changing state to unspecified */
            char str[16];

            if( c < ' ' || c >= 127 )
               sprintf( str, "%d", c );
            else
               sprintf( str, "%c", c );
            fprintf( stderr, 
               "syntax error[%d]: unexpected character [%s]\n",
               instno, str );
            buffer[i++] = c;
            buffer[i] = 0;
            return -1;
         }
      } else {
         if( c == end_of_label ) {
            buffer[i] = 3;     /* compare kennt nur den defaultwert */
            buffer[++i] = 0;
            return i;
         }
         buffer[i++] = c;
         continue;
      }
      
   }
#endif
}

/*********************************************************************
*     Anzaehl des Arguments nach einem Kommando wird geliefert       *
*********************************************************************/

int traf_argcount( char *str ) {
   int argzahl = 1 ;  /*  Anzahl Argumente */
  
   while( *str != '\0' ) {
     if( *str++ == ',' )
        argzahl++ ; 
   }
   if( argzahl & 1 ) {   /* ungerade */
      return(-1) ;
   } else {
      return( argzahl >> 1 ) ;  /*   argzahl / 2  */
   }
}

/*********************************************************************
*     Adresse das naechsten Koordinatenpaars einer Zeichenkette      *
*********************************************************************/

int traf_npaar( char str[] ) { 
   int za = 0 , ca = 0 ;

   while( str[za++] != '\0' && ca < 2 ) {
      if( str[za] == ',' )
         ca++;
   }
   if( str[za] == ',' )
      za--;
   if( ca == 1 ) {
      return(-1) ;
   } else {
      return(za) ;
   }
}  

void zhp_move(double x,double y)
{
   if( rot == FALSE ) {
      grf_move( dsc, x, y );
#ifdef PR
      xalt = x;
      yalt = y;
#endif
   } else {
      grf_move( dsc, y, YMAXDEF-x );
#ifdef PR
      xalt = y;
      yalt = YMAXDEF-x;
#endif
   }                /* Rotation und Translation um 90 Grad */
}   

void zhp_draw(double x,double y)
{
   if( rot == FALSE ) {
      grf_draw( dsc, x, y );
#ifdef PR
      xalt = x;
      yalt = y;
#endif
   } else {
      grf_draw( dsc, y, YMAXDEF-x );
#ifdef PR
      xalt = y;
      yalt = YMAXDEF-x;
#endif
   }                /* Rotation und Translation um 90 Grad */
}   

/*********************************************************************
*     Auswerten einer Zeile aus dem Datenfile                        *
*********************************************************************/

int compare( char* cmdstr ) { 
   double xf, yf, u, v;
   int x,len, koor_paars, co, npaarlaenge;
   int implemented;
   
   int cmd_num = cmd_to_num(cmdstr[0],cmdstr[1]) ;
   
   
   if ( nop_cmd[cmd_num] )
      return(1) ;
      
   cmdstr += 2 ;   /* ->  Parameter   */
   implemented = 0;
   switch( cmd_num ) {
      default :
         warnings = 1;
         if( verbose ) {
            fprintf( stderr, "warning[%d]: unknown command detected\n", instno );
            fprintf( stderr, "   command:   [%s]\n", cmdstr-2 );
         }
         return(-1) ;
      case 56 :    /* ESC. **   Escape sequenzen werden ignoriert */
         implemented = 56;
         break ;
      case 0 :     /* aa   */
         break ;
      case 1 :     /* ar   */
         break ;
      case 2 :     /* ca   */
         break ;
      case 3 :     /* ci   */
         break ;
      case 4 :     /* cp   */
         break ;
      case 5 :     /* cs   */
         break ;
      case 6 :     /* dc   */
         break ;
      case 7 :     /* df   */
         break ;
      case 8 :     /* di   ** Font Orientierung */
         break ;
      case 9 :     /* dp   */
         break ;
      case 10 :    /* dr   */
         break ;
      case 11 :    /* dt   */
         break ;
      case 12 :    /* ea   */
         break ;
      case 13 :    /* er   */
         break ;
      case 14 :    /* ew   */
         break ;
      case 15 :    /* ft   */
         break ;
      case 16 :    /* im   */
         break ;
      case 17 :    /* in   ** Initialisize == RESET Plotter */
         implemented = 17;
         set_defaults();
         break;
      case 18 :    /* ip   ** Input P1 and P2 */
      {
         double xuln, yuln, xorn, yorn;

         implemented = 18;
         len = sscanf( cmdstr, "%lf,%lf,%lf,%lf", &xuln, &yuln, &xorn, &yorn );
         if( len == 4 ) {
            xul = xuln;
            yul = yuln;
            xor = xorn;
            yor = yorn;
         } else if( len == 2 ) {
            xul += xorn - xor;
            yul += yorn - yor;
            xor = xorn;
            yor = yorn;
         } else {
            xul = XMINDEF;
            xor = XMAXDEF;
            yul = YMINDEF;
            yor = YMAXDEF;
         }
         grf_koor( dsc, xul/XMAXDEF, yul/YMAXDEF, xor/XMAXDEF, yor/YMAXDEF,
                   xmin, ymin, xmax, ymax,
                   0, 0, 0, 0, 0, 0, "", "", 1.0, 1.0 );
         break ;
      }
      case 19 :    /* iw   */
      {       /* Input Window (xbl,ybl,xur,yur) */
         double x1, y1, x2, y2;

         implemented = 19;
         len = sscanf( cmdstr , "%lf,%lf,%lf,%lf", &x1, &y1, &x2, &y2 );
         if( len == 4 ) {
            grf_clip( dsc,x1, y1, x2, y2,REEL) ;
         }
         break ;
      }
      case 20 :    /* lb   */
         /* Textstring (EOL=0x3) */
      {
         char str[90], *pcmdstr;

         implemented = 20;
         pcmdstr = cmdstr;
         len = 0;
         while( (*pcmdstr != 3) && (len < 88) ) {            /* 3 = EOS */
            str[len++] = *pcmdstr++;
         }
         str[len] = 0;
         grf_write( dsc, str, dsc->xfa, dsc->yfa, 1, 6, font );
         break ;     /* xfa, yfa sind die letzten Koordinaten */
      }
      case 21 :    /* lt   */
                                   /* Linetype(muster,repeatlength) */
         implemented = 21;
         switch( sscanf( cmdstr, "%d,%lf", &x, &v ) ) {
           /* !!! scanf mit 0 Argumenten meldet statt 0 ein EOF=-1 !!! */
            default:                       /* kein Argument = Vollinie */
               grf_ltyp( dsc, 0, 4. );
               break ;
            case 1 :
               grf_ltyp( dsc,  x, 4. );
               break ;
            case 2 :
               grf_ltyp( dsc,  x,  v );
               break ;
         }
      case 22 :    /* oa   */
         break ;
      case 23 :    /* oc   */
         break ;
      case 24 :    /* od   */
         break ;
      case 25 :    /* oe   */
         break ;
      case 26 :    /* of   */
         break ;
      case 27 :    /* oh   */
         break ;
      case 28 :    /* oi   */
         break ;
      case 29 :    /* oo   */
         break ;
      case 30 :    /* op   */
         break ;
      case 31 :    /* os   */
         break ;
      case 32 :    /* ow   */
         break ;
      case 33 :    /* pa   */
         implemented = 33;
         /* plot absolute (x1,y1,...,xn,yn);; HG: neu double statt int */
         koor_paars = traf_argcount( cmdstr );
         for( co = 0; co < koor_paars; co++ ) {
            sscanf( cmdstr, "%lf,%lf", &xf, &yf );
            if( pen ) {
               zhp_draw(xf,yf) ;
            } else {
               zhp_move(xf,yf) ;
            }  
            if( (npaarlaenge = traf_npaar( cmdstr )) > 1 ) {
               cmdstr += npaarlaenge;
            }
         }
         break ;
      case 34 :    /* pd   */
         implemented = 34;
       /* pen down. DANACH ERST goto Koordinaten, falls angegeben ! */
         koor_paars = traf_argcount( cmdstr );
            for( co = 0; co < koor_paars; co++ ) {
               sscanf( cmdstr, "%lf,%lf", &xf, &yf );
               zhp_draw(xf,yf);
               if( (npaarlaenge = traf_npaar( cmdstr )) > 1 ) {
                  cmdstr += npaarlaenge;
               }
            }
         pen = 1;
         break ;
      case 35 :    /* pr   */
#ifdef PR
         implemented = 35;
         /* plot relative(x1,y1,...,xn,yn);  */
         koor_paars = traf_argcount( cmdstr );
         for( co = 0; co < koor_paars; co++ ) {
            sscanf( cmdstr, "%lf,%lf", &xf, &yf );
            if( pen ) {
               zhp_draw(xalt+xf,yalt+yf) ;
            } else {
               zhp_move(xalt+xf,yalt+yf) ;
            }
            if( (npaarlaenge = traf_npaar( cmdstr )) > 1 ) {
               cmdstr += npaarlaenge;
            }
         }
#endif
         break ;
      case 36 :    /* ps   */
         break ;
      case 37 :    /* pt   */
         break ;
      case 38 :    /* pu   */
         implemented = 38;
          /* pen up. DANACH ERST goto Koordinaten, falls angegeben! */
         koor_paars = traf_argcount( cmdstr );
         for( co = 0; co < koor_paars; co++ ) {
            sscanf( cmdstr, "%lf,%lf", &xf, &yf );
            zhp_move(xf,yf);
   
            if( (npaarlaenge = traf_npaar( cmdstr )) > 1 ) {
               cmdstr += npaarlaenge;
            }
         }
         pen = 0;
         break ;
      case 39 :    /* ra   */
         break ;
      case 40 :    /* ro   */
         implemented = 40;
        /* Rotate Coordinate System(rot) */
         len = sscanf( cmdstr, "%d", &rot );
         if( len == 1 ) {
            if( rot == 90 ) {
               rot = TRUE;
            } else {
               rot = FALSE;
            }
         } else {
            rot = FALSE;
         }
         break ;
      case 41 :    /* rr   */
         break ;
      case 42 :    /* sa   */
         break ;
      case 43 :    /* sc   */
         implemented = 43;
         len = sscanf( cmdstr, "%lf,%lf,%lf,%lf", &xmin, &xmax, &ymin, &ymax );
         if( len != 4 ) {
            set_defaults();
         }
         grf_koor( dsc, xul/XMAXDEF, yul/YMAXDEF, xor/XMAXDEF, yor/YMAXDEF,
                xmin, ymin, xmax, ymax,
                0, 0, 0, 0, 0, 0, "", "", 1.0, 1.0 );
         break ;
      case 44 :    /* si   */
         implemented = 44;
                /* Textfont-Size(Breite,Hoehe) */
         len = sscanf( cmdstr, "%lf,%lf", &u, &v );
         if( len == 2 ) 
            font = traf_fontwert( u, v );  /* font ist static und wird */
         break ;
      case 45 :    /* sl   */
         break ;
      case 46 :    /* sm   */
         break ;
      case 47 :    /* sp   */
         implemented = 47;
          /* Select pen(0-8) */
         len = sscanf( cmdstr, "%d", &x );    /* 0 = aktuellen Stift wegraeumen */
         if( len ==1 ) {                      /* default: pen 1 nehmen */
            grf_pen( dsc, (x%100)+100 );
         }
         break ;
      case 48 :    /* sr   */
         break ;
      case 49 :    /* ss   */
         break ;
      case 50 :    /* tl   */
         implemented = 50;
         /* Setze Strichelchen (Tick)-Laenge in % der Blattdiagonale P1P2 */
         len = sscanf( cmdstr, "%lf%lf",&u,&v) ;
         if( len==1 ) {                      /* default: pen 1 nehmen */
            tp = u ;
            tn = 0.0 ;
         } else if ( len==2 ) {
            tp = u ;
            tn = v ;
         } else {
            tp = tn = 0.5 ;
         }
         break ;
      case 51 :    /* uc   */
                                    /* selbst-definierter Font */
         break ;
      case 52 :    /* vs   */
         implemented = 52;
         len = sscanf( cmdstr, "%d", &x );
         if( len == 1 )
            grf_speed( dsc, x );          /* kein Argument: default, sonst: 1-40 */
         break ;
      case 53 :    /* wg   */
         break ;
      case 54 :    /* xt    vertikale Linie 0.5% von P1P2 (default) */
      {
         double xfa = dsc->xfa ;
         double yfa = dsc->yfa ;

         implemented = 54;
         grf_move(dsc,xfa,yfa-tn*0.01*(ymax-ymin)) ;
         grf_draw(dsc,xfa,yfa+tp*0.01*(ymax-ymin)) ;
         grf_move(dsc,xfa,yfa) ;
#ifdef PR
         xalt = xfa;
         yalt = yfa;
#endif
         break ;
      }
      case 55 :    /* yt    horizontale Linie 0.5% von P1P2 (default) */
      {
         double xfa = dsc->xfa ;
         double yfa = dsc->yfa ;

         implemented = 55;
         grf_move(dsc,xfa-tn*0.01*(xmax-xmin),yfa) ;
         grf_draw(dsc,xfa+tp*0.01*(xmax-xmin),yfa) ;
         grf_move(dsc,xfa,yfa) ;
#ifdef PR
         xalt = xfa;
         yalt = yfa;
#endif
         break ;
      }

   }
   if( !implemented && verbose ) {
      warnings = 1;
      fprintf( stderr, "warning[%d]: non implemented command detected\n", instno );
      fprintf( stderr, "   command:   [%s]\n", cmd[cmd_num] );
      fprintf( stderr, "   arguments: [%s]\n", cmdstr );
   }
   return(1) ;
}

/*********************************************************************
*     Einlesen der Netzkonfiguration                                 *
*********************************************************************/

void intpret( char* name ) {
   static char str[HPGLMAX+16];  /* 30000 fuer PA 10,10,11,11,12,12, ... */
   FILE *fp;
   int ret;
   
   if ( !(fp = fopen( name, "r" )) ) {
      if( !pretty_print )
         grf_show( "text" );
      fprintf( stderr, "error: reading HPGL-file '%s' failed!\n", name );
      return ;
   }
   for( instno = 1; ret = getk( str, fp ); instno++ ) {
      /* ret = 0: EOF ==> done */
      /* ret = -1: syntax error */
      /* ret > 0: strlen(str) */
      if( pretty_print ) {
         if( ret > 0 ) {
            puts(str);
         }
#if 0               /* to detect erroneous command */
         else {
            printf(";;; %s\n", str );
         }
#endif
      } else if( ret > 0 ) {
         compare( str );
      }
   }
   fclose( fp );
}

/*********************************************************************
*     Hauptprogramm                                                  *
*********************************************************************/

int main( int argc, char *argv[] ) {
   char *file ;                                   /* Quell-file     */
   char *device = "screen" ;                      /* Ausgabe Geraet */
   int params = 0 ;                               /* Anzahl Parameter */
   int i,j ;

   verbose = 0;
   for ( i=1 ; i<argc ; i++ ) {      /*  Optionen + Parameterauswertung    */
      if ( argv[i][0] == '-' ) {
         if( !strcmp( argv[i], "-v" ) ) {
            verbose = 1;
         } else if( !strcmp( argv[i], "-E" ) ) {
            pretty_print = 1;
         } else {
            for ( j=0 ; j<NCMD ; j++ ) {
               if (  tolower(argv[i][1]) == cmd[j][0] )
                  if (  tolower(argv[i][2]) == cmd[j][1] ) {
                     nop_cmd[j] = TRUE ;    /* CMD j wird Ignoriert  */
                     fprintf( stderr, "num = %d\n",j) ;
                  }
            }
         }
      } else {
         if ( params==0 ) {
            file = argv[i] ;
         } else if ( params==1 ) {
            device = argv[i] ;
         }
         params++ ;
      }
   }
   if( params!=1 && params!=2 ) {
      fprintf( stderr, "Usage: zhp HPGL-filename [output-device] [-v] [-?? [-??]]\n");
      fprintf( stderr, "   output-device: (default=screen)\n");
      fprintf( stderr, "      text, screen, screen1, screen2, hp7221, *.eps\n");
      fprintf( stderr, "      (hpgl, *.plt)\n");
      fprintf( stderr, "\n");
      fprintf( stderr, "   options:\n");
      fprintf( stderr, "      -?? -> ignore instruction ??\n");
      fprintf( stderr, "      -v  -> warn on unknown or non implemented instructions\n");
      fprintf( stderr, "      -E  -> only pretty print instruction to stdout\n");
      fprintf( stderr, "\n");
      return(-1);
   }

   set_defaults();

   if( !pretty_print ) {

      /* damit evtl. falsch gesetzte ENV vars hier keinen Unfug anstellen !!! */
      fix_env("text");
      fix_env( device );

      dsc = grf_open( device );
      grf_clr( device );
      grf_show( device );
      grf_koor( dsc, xul/XMAXDEF, yul/YMAXDEF, xor/XMAXDEF, yor/YMAXDEF,
                xmin, ymin, xmax, ymax,
                0, 0, 0, 0, 0, 0, "", "", 1.0, 1.0 );
   }

   intpret( file );

   if( warnings ) {
      fprintf( stderr, "hpgl-file contains unknown or non implemented instructions!\n");
   }

   if( !pretty_print ) {
      grf_close( dsc );
      grf_wait();
      grf_show( "text" );
      grf_error(1);
   }
}

/*** EOF ************************************************************/


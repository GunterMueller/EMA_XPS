#if 0
#define _dbg(a)    puts("main: " a)
#define _dbgs(a) printf("main: %s\n",a)
#else
#define _dbg(a)
#define _dbgs(a)
#endif

/****************************************************
*   main.c        Hans Groschwitz        06.10.94   *
*                                                   *
*   A graphic front-end for a LISP-process          *
*   derived from a xterm but quite different to it  *
*                                                   *
*   The low-level IPC routines are derived from     *
*   the xterm sources...                            *
*                                                   *
*   BUGS:                                           *
*     A clean EIO/SIGTTIN handler lacks for rcvChar *
*     use ioctl to avoid \r elimination when rcving *
*     use ioctl to avoid \r in spite of \n sending  *
*                                                   *
*   MISC:                                           *
*     !! build setsid / SIGCHLD into xgrf3, too !!  *
****************************************************/

#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>                     /* O_NDELAY */

#ifdef hpux                 /* LISP_run: HP-UX has */
#include <sys/termio.h>        /* improper presets */
#endif

#include <Xm/Xm.h>                    /* XmVersion */
#include <Xm/Protocols.h>
#include <X11/cursorfont.h>  /* Mouse Cursor Forms */

#ifndef LesstifVersion /* we are NOT using Motif21 */
#if (XmVersion >= 2000)
#include <Xm/IconFile.h>   /* see CreatePixmapFromXBMfile */
#endif
#endif /* Lesstif */

#include "timings.h"
#include "main.h"
#include "xinit.h"
#include "debugger.h"
#include "dispatch.h"
#include "uif.h"                 /* SESSION_busy() */
#include "uid.h"      /* UID_popdown_all_dialogs() */
#include "sysd.h"    /* SYSD_popdown_all_dialogs() */
#include "explain.h" /* EXPD_popdown_all_dialogs() */
#include "xext.h"                /* Cygwin Bug !!! */

/* in spite of stdlib.h */
extern char *getenv( char* );

/* linux reports warnings, sun does not: */
typedef void (*SHP)();  /* SignalHandler */

/*** Defines ***************************************/

#define ERR               (-1)
#define ICON_XBM          "emaXpsIcon.bm"

#define STATE_READY       0       /* states of the */
#define STATE_BUSY        1       /* state machine */
#define STATE_DEBUGGER    2
#define STATE_PAD_READ    3
#define STATE_BEGIN_REPLY 4
#define STATE_MKB_SYNC    8
#define STATE_END_OF_INIT 9

#define MSG_BUFSIZE       256

#define EX_NLS_XX         0      /* $LANG settings */
#define EX_NLS_DE         1
#define EX_NLS_EN         2

/*** Typedefs **************************************/

typedef struct _cmd {
   struct _cmd *next;
   char        *cmd;
} CMD;

typedef struct _msg {
   struct _msg *next;
   int          len;
   char         buf[MSG_BUFSIZE];
} MSG;

/*** Globals ***************************************/

Atom WM_DELETE_WINDOW;
Pixmap iconPm, savePm;
Cursor cursor_normal, cursor_busy;
Widget SystemDialogs;
char   geometryConstraint[256];

static int    channelno = STATE_READY;
static int    oldchannelno = STATE_READY;
static int    lisp_is_busy = False;
static int    evalhook_state_changed = False;
static int    add_to_message_buffer = False;
static int    stop_ticking = False;
static CMD*   CMD_root;
static MSG*   MSG_root;
static Widget toplevel;
static int    tty;      /* tty-master filehandle (RW) */
static int    c_lisp;   /* pty-master filehandle (RW) */
static String emainitpath, emalisppath, emahelppath, emabmpath;
static int    lispPid = 0;
static int    ARGV_show_debugger;
static int    ARGV_show_states;
static int    ARGV_boot_verbose;
static int    ARGV_show_debugger;
static char*  ARGV_autoload_kb;
static int    ARGV_autorun_kb;
static char*  ARGV_kb_directory;
static char*  ARGV_kb_extension;
static int    ARGV_checked_set_ready;
static int    ARGV_search_pty;
static int    suppress_displaying = False;

/*** Function Declarations *************************/

static void  SayGoodBye( void );
static void  sigchildHandler( void );
static void  sigintHandler( void );
static void  errorHandler( char* );
static void  Error( char*s );
static void  setNoDelay( int );
static void  CMD_push( char* );
static char* CMD_pop( void );
static char* CMD_list( void );
static void  MSG_append( int );
static int   __rcvChar( void );
static int   _rcvChar( void );
static int   rcvChar( void );
static int   sndStr( char* );
static void  LISP_sndNextCmd( void );
static void  LISP_init( void );
static void  LISP_sent_end_of_init( void );
static void  LISP_setBusy( void );
static void  LISP_setReady( void );
static void  LISP_showState( void );
static void  IPC_reset( void );
static void  CheckedSetReady( void );
static void  toplevelPipeInputHandler( void );
static void  LISP_interrupt_TO( void );

/***************************************************/

static void
usage() {
   fprintf( stderr, "\
 \n\
 Usage: ema-xps [options] \n\
 \n\
  with the following options: \n\
   * for session control: \n\
      -kbs path: Homedir of KBs (defaults to working directory) \n\
      -kb file:  Autoload KB kbs_path/file.kb \n\
                 (do NOT add path or extension!) \n\
      -ext xyz:  KB extension defaults to \"kb\" \n\
      -run:      automatically run a autoloaded KB at boot time \n\
 \n\
   * for developers: \n\
      -pty:      verbose output while looking for free PTYs \n\
      -csr:      prints CheckedSetReady warnings \n\
      -gui:      verbosely booting of the GUI \n\
      -dbg:      shows VChannel state transitions in the PAD \n\
      -pad:      initial state of the PAD is popped up \n\
 \n\
   * miscelaneous: \n\
      -bw:       assume displaying to a grey-scale terminal \n\
      -help:     displays this helpful message \n\
 \n"
   );
   exit( -1 );
}

/****************************************************
*   SayGoodBye ...                                  *
****************************************************/

static void
SayGoodBye( void ) {

   fprintf( stderr, " >>> bye\n" );
}

/****************************************************
*   SignalHandler reacts on changes of the child    *
*   process' state (LISP world)                     *
*   e.g. user entered (exit) or (bye) !             *
****************************************************/

static void
sigchildHandler( void ) {

   SayGoodBye();
   exit(0);
}

/****************************************************
*   SignalHandler reacts on ^C pressed in calling   *
*   xterm. Terminates C- and LISP-process           *
****************************************************/

static void
sigintHandler( void ) {

   SayGoodBye();
   LISP_kill();
   usleep( 2000 );
   exit( -1 );
}

/****************************************************
*   New XtErrorHandler, views errno, too            *
****************************************************/

static void
errorHandler( String msg ) {
   char buf[128];

   fprintf( stderr, "XToolkitError: %s\n", msg );
   fflush( stderr );
   sprintf( buf, "errno=%d", errno );
   perror( buf );
   exit( 1 );
}

/***************************************************/

static void
Error( char *str ) {
   char buf[1024];

   sprintf( buf, "EMA-XPS: %s", str );
   perror( buf );
   exit( -1 );
}

/***************************************************/

static void
setNoDelay( int fd ) {
   int mode;

   if( 0 > (mode=fcntl(fd,F_GETFL,0)) ) {
      Error("setNoDelay: fcntl GETFL");
   }
   mode |= O_NDELAY;
   if( fcntl(fd,F_SETFL,mode) ) {
      Error("setNoDelay: fcntl SETFL NDELAY");
   }
}

/****************************************************
*   reads a character which was sent by LISP        *
*   eliminates \r chars from pty-transfer           *
*   ioctl() settings might do this more nice...     *
****************************************************/

static void
addToTmp( int c ) {
   FILE *fp;
   char str[256];
   static char* cv[] = {
      "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
       "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
      "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
      "CAN",  "EM", "SUB", "ESC",  "FS",  "GS",  "RS",  "US"
   };

   sprintf( str, "/tmp/xx_dbg_%d", getpid() );
   if( !(fp=fopen( str, "a" )) ) {
      perror("addToTmp");
      return;
   }
   if( c >= ' ' && c < 127 ) {
      fprintf( fp, "%c", c );
   } else if( c == '\r' ) {   /* !! */
      fprintf( fp, "\n" );
   } else if( c >= 127 ) {
      fprintf( fp, "<%d>", c );
   } else if( c < 0 ) {
      fprintf( fp, "<err>" );
   } else {                          /* NUL - SPC */
      fprintf( fp, "<%s>", cv[c] );
   }
   fclose( fp );
}

/***************************************************/

static int
__rcvChar( void ) {
   int len;
   char buf[4];
   if( (len=read(c_lisp,buf,1)) <= 0 ) {
      /* EIO : should be prevented by ioctl() ?!? */
      if( errno && errno != EWOULDBLOCK && errno != EIO )
         perror("_rcvChar");
      return ERR;
   }
   return *buf;
}

/***************************************************/

static int
_rcvChar( void ) {
   int c = __rcvChar();
   /*** IPC debugging ***/
   if( ARGV_show_states )
      addToTmp( c );
   return c;
}
   
/***************************************************/

static int rcvChar( void ) {
   int c;

   do {
      c = _rcvChar();
   } while( c == '\n' );
   if( c == '\r' )     /* ioctl: Terminals trigger on CR, not LF */
      c = '\n';
   return c;
}
      
/****************************************************
*   produces a LIFO Stack for offending LISP cmds   *
****************************************************/

static void
CMD_push( char *command ) {
   CMD *pneu;
   static CMD *p;
   char *str;

   pneu = (CMD*)XtMalloc( sizeof(CMD) );

   if( CMD_root )
      p->next = pneu;
   else
      CMD_root = pneu;

   p = pneu;
   p->next = NULL;

   str = (char*)XtMalloc( strlen(command)+1 );
   p->cmd = str;
   strcpy( p->cmd, command );

   /* update Debugger PAD */
   PAD_Pupdate( str=CMD_list() );
   XtFree( str ); /* explicitly */
}

/****************************************************
*   Result must be XtFree'd explicitly              *
****************************************************/

static char*
CMD_pop( void ) {
   char *str, *s = NULL;

   if( CMD_root ) {
      CMD *p = CMD_root;

      s = p->cmd;
      CMD_root = p->next; 
      XtFree( (char*)p );
   }

   /* update Debugger PAD */
   PAD_Pupdate( str=CMD_list() );
   XtFree( str ); /* explicitly */

   return s;
}    

/***************************************************/

static char*
CMD_list( void ) {
   char *s;

   s = XtMalloc( 1 );
   *s = 0;
   if( CMD_root ) {
      CMD *p = CMD_root;

      while( p ) {
         char *tmp;

         tmp = (char*)XtMalloc( strlen(s) + strlen(p->cmd) + 2 );
         strcpy( tmp, s );
         strcat( tmp, p->cmd );
         strcat( tmp, "\n" );
         XtFree( s );
         s = tmp;
         p = p->next;
      }
   }
   return s;
}    

/****************************************************
*   Message Buffering for User channels             *
****************************************************/

static void
MSG_append( int c ) {
   MSG *pneu;
   static MSG *p;

   if( !MSG_root ) {
      p = (MSG*)XtMalloc( sizeof(MSG) );
      p->len = 0;
      p->next = NULL;
      MSG_root = p;
   }
   p->buf[p->len++] = c;
   if( p->len == MSG_BUFSIZE ) {
      pneu = (MSG*)XtMalloc( sizeof(MSG) );
      p->next = pneu;
      p = pneu;
      p->next = NULL;
      p->len = 0;
   }
}

/***************************************************/

char*
MSG_read( void ) {
   MSG *p, *ptmp;
   char *buf, *s;

   s = XtMalloc( 1 );
   *s = 0;
   buf = XtMalloc( MSG_BUFSIZE+1 );
   p = MSG_root;
   while( p ) {
      char *tmp;

      strncpy( buf, p->buf, p->len );
      buf[p->len] = 0;
      tmp = (char*)XtMalloc( strlen(s) + p->len + 1 );
      strcpy( tmp, s );
      strcat( tmp, buf );
      XtFree( s );
      s = tmp;
      ptmp = p;
      p = p->next;
      XtFree( (char*)ptmp );
   } 
   MSG_root = NULL;

   XtFree( buf );
   return s;
}

/****************************************************
*   write small portions and wait for peer...       *
****************************************************/

static int
write_to_tty( char *str ) {
   int err, len;
   char *p, *pp;

   /*
    * This code fails to work correctly if strlen > 1024. This is a
    * kernel flaw. Now after each \n a part of the string is written
    * to the device! (See below)
    *
    *   for( p = str; *p; p++ )
    *      if( *p == '\n' )
    *         *p = '\r';
    *   err = write( c_lisp, str, strlen(str) );
    */
   pp = p = str;
   while( 1 ) {
      if( !*p ) {       /* EOS */
         if( !(len=strlen(pp)) ) {
            break;
         }
         err = write( c_lisp, pp, len );       /* send the rest! */
         break;
      }
      if( *p == '\n' ) {
         *p = 0;   /* EOS */
         if( len=strlen(pp) ) {
            err = write( c_lisp, pp, len );
            if( err < 0 )
               break;
         }
         /*** ioctl: Terminal-Devices trigger on CR, not LF ***/
         err = write( c_lisp, "\r", 1 );
         if( err < 0 )
            break;

         /* restart behind that NEWLINE ! */
         pp = p+1;

         /*
          * to give LISP a chance to read out the devices incoming queue.
          * This makes the kernel's scheduler release the rest of this
          * time-interval and run other processes.
          */
         usleep( 1 );
      }
      p++;
   }
   return err;
}

/****************************************************
*   writes a string to LISP's *standard-input*      *
****************************************************/

static int
sndStr( char *str ) {
   int err;
   char *p;

   p = XtMalloc( strlen(str)+1 );
   strcpy( p, str );
   str = p;

   /*** IPC debugging ***/
   if( ARGV_show_states ) {
      addToTmp( '[' );
      for( p = str; *p; p++ ) {
         addToTmp( *p );
      }
      addToTmp( ']' );
   }

   err = write_to_tty( str );

   XtFree( str );
   if( err < 0 ) {
      perror("sndStr: write str");
      return ERR;
   }
   return err;
}

/****************************************************
*   For C-replies, when LISP is (read)-ing:         *
*   If there are pending commands in the queue,     *
*   they have to be shotcutted !                    *
****************************************************/

void
LISP_sndReply( char *str ) {

   sndStr( str );
   sndStr( "\n" ); /* C-side representation of NEWLINE */
}

/****************************************************
*   Writes (delayed) a LISP command to LISP's       *
*   *standard-input* stream.                        *
*   The additional \r is necessary to make the      *
*   LISP toplevel read-eval-print-loop run !!!      *
****************************************************/

void
LISP_sndCmd( char *str ) {

   if( stop_ticking ) {
      fprintf( stderr, "Warning: LISP_sndCmd: LISP cmd discarded!" );
      return;
   }
   /* push command onto stack and update PAD */
   CMD_push( str );
}

/****************************************************
*   writes a LISP command to LISP's                 *
*   *standard-input* stream                         *
*   The additional \n is necessary to make the      *
*   LISP toplevel read-eval-print-loop run !!!      *
*                                                   *
*   Checking for hanging of *evalhook* is done      *
*   here to be synchronized with TIME_SND_CMD !!!   *
*                                                   *
*   Checking is activated to find out, if the       *
*   command entered was the first command of the    *
*   initialisation sequence (activating channel     *
*   management) or the command to leave the         *
*   debugger (CLISP: abort). In these 2 cases no    *
*   final RDY is sent by *evalhook*.                *
*                                                   *
*   Inits evalhook_state_changed and some time      *
*   later this routine.                             *
*   If LISP's *evalhook* works fine, now at least   *
*   one VChannel state transition should have taken *
*   place (--> tplPIH). In this case the variable   *
*   evalhook_state_changed has been modified.       *
*   ELSE one of two possible states must have       *
*   occured:                                        *
*   1) the 1st command of COLD BOOT init sequence   *
*      has been sent, or                            *
*   2) the debugger has been left                   *
*      (CLISP command: abort).                      *
*   In both cases *evalhook* is reset and channel   *
*   management cannot take place !!!                *
*   In such cases this must be worked around by     *
*   this explicit setting to RDY state.             *
****************************************************/

static void
LISP_sndNextCmd( void ) {
   char *str;
   static int ticks = 0;

   /* to avoid both actions at the same time! */
   if( lisp_is_busy && !evalhook_state_changed ) {
      /*
       * static global variables evalhook_state_changed
       * and lisp_is_busy are initialized to False!
       */
      if( ++ticks >= TIME_CHK_HANG ) {
         ticks = 0;
         LISP_setReady();
      }
   } else {
      ticks = 0;
      /* get next offending command and update PAD */
      if( !lisp_is_busy && (str=CMD_pop()) ) {
         LISP_setBusy();
         LISP_sndReply( str );    /* send NONdelayed! */
         XtFree( str );                /* explicitly! */
         /*
          * and check for hanging of job control !
          * every vchannel-change sets this flag to True!
          * (in toplevelPipeInputHandler() ...)
          * at least 2 Changes MUST take place!
          */
         evalhook_state_changed = False;
      }
   }
   if( !stop_ticking )
      AddTimeOut( TIME_SND_CMD, (XtPointer)LISP_sndNextCmd, NULL );
}

/****************************************************
*  initializes Lisp completely                      *
*   - in DEVELOPer mode, load patching files        *
*     else, start IPC management                    *
*   - if exists, load user's lispinit-file          *
*  Is run via XtAddTimeOut from tplPipeInputHdlr    *
*  when the first character is received from LISP   *
****************************************************/

static void
LISP_init( void ) {
   char command[1024];

   /*
    * *** General Initialization ***
    *
    * WARNING: to make LISP_sndCmd work fine, it is
    *    necessary that the last LISP-construct - 
    *    activated by the first command here - 
    *    is (emaxps-end-of-boot-phase) !!!
    *
    * To make LISP_sndCmd work as expected, the 
    * send-next-command timer must be init'ed.
    * This should not be done before here
    * (not before needed)!
    */
   sprintf( command, "(emaxps-end-of-boot-phase)" );

   /*
    * here LISP_sndCmd() doesn't make sense, hence Channel Management isn't
    * initialized yet!
    * This done at the end of this LISP command, when a <EOI> is sent
    * and the toplevelPIH() does LISP_sent_end_of_init(), which in advance
    * activates ticking of sndCmd Timer!
    */
   LISP_sndReply( command );
}

/***************************************************/

static void
LISP_sent_end_of_init( void ) {
   char command[3*1024+64];
   char pi[1024], pa[1024], pf[1024];
   FILE *fp;

   /*** Show MainMenu, Hide Welcome ***/
   WELCOME_hide();
   MAINMENU_show();

   /*** User Initialization (optional) ***/
   strcpy( pi, "NIL" );
   if( (fp=fopen(emainitpath,"r")) ) {
      fclose(fp);
      sprintf( pi, "\"%s\"", emainitpath );
   }
   /*** AutoLoading KnowledgeBase ***/
   strcpy( pa, "NIL" );
   strcpy( pf, "NIL" );
   if( ARGV_autoload_kb ) {
      if( (fp=fopen(ARGV_autoload_kb,"r")) ) {
         fclose(fp);
         sprintf( pa, "\"%s\"", ARGV_autoload_kb );
         /* and add a correct filter string... */
         strcpy( pf, "\"" );
         strcat( pf, ARGV_kb_directory );
         strcat( pf, "/*." );
         strcat( pf, ARGV_kb_extension );
         strcat( pf, "\"" );
      } else {
         /* UNDER CONSTRUCTION !!! */
         perror("ARGV_autoload_kb");
      }
   }
   sprintf( command, "(emaxps-initialize %s\n   %s %s %s)",
      pi, pa, pf,
      ARGV_autorun_kb ? "T" : "NIL"
   );

   LISP_sndCmd( command );

   /*** init send-next-command timer ***/
   AddTimeOut( TIME_SND_FIRST, (XtPointer)LISP_sndNextCmd, NULL );
}                            
     
/****************************************************
*   routines to visualize LISP's current state      *
*   ADVANTAGE: All Inputs are buffered,             *
*              no BLOCKING is afforded.             *
****************************************************/

static void
LISP_setBusy( void ) {

   lisp_is_busy = True;
   LISP_showState();
}

/***************************************************/

static void
LISP_setReady( void ) {

   lisp_is_busy = False;
   LISP_showState();
}

/***************************************************/

static void
LISP_showState( void ) {
   static int oldstate = False;

   if( oldstate == lisp_is_busy ) {
      /* to avoid flicker */
      return;
   }
   oldstate = lisp_is_busy;

   XAPPL_showLispState( lisp_is_busy );
}

/****************************************************
*   Warm Boot Sequence:                             *
*   (re)init's VirtualChannel StateMachine.         *
****************************************************/

static void
IPC_reset( void ) {
   char *p;

   /* clear pending commands */
   while( p=CMD_pop() ) {
      XtFree( p );                  /* explicitly! */
   }

   UID_popdown_all_dialogs();
   SYSD_popdown_all_dialogs();
   EXPD_popdown_all_dialogs();

   oldchannelno = channelno = STATE_READY;

   add_to_message_buffer = False;
   XtFree( MSG_read() );    /* (re)init MSG buffer */

   evalhook_state_changed = False;
   suppress_displaying = False;

   lisp_is_busy = True;       /* fake event to ... */
   LISP_setReady();    /* (re)init Cursor settings */
}

/***************************************************/

static void
CheckedSetReady( void ) {

   if( oldchannelno == channelno && channelno == STATE_READY ) {
      LISP_setReady();
   } else if( ARGV_checked_set_ready ) {
      fprintf( stderr, "CheckedSetReady: Warning: STATE HAS CHANGED\n" );
      fprintf( stderr, "   old=%d new=%d rdy=%d\n",
         oldchannelno, channelno, STATE_READY );
   }
}

/****************************************************
*  Input-Handler for the Toplevel-Pipe              *
*  - receives what LISP writes to *terminal-io*     *
*  - reads data from Toplevel-Pipe                  *
*  - looks for VirtualChannelStateMachine changes   *
*    to start specialized actions                   *
*  - updates the outputwindow of the Debugger's PAD *
****************************************************/

static void
toplevelPipeInputHandler( void ) {
   int c;
   static int first_char_already_sent = 0;
   static int lastchar = 0;
   static int escape = 0;
   static int ch;
   static int already_inited = False;

   while( (c=rcvChar()) != ERR ) {

      /*** Initialization ***/
      if( !first_char_already_sent ) {
         fprintf( stderr, " >>> ... done\n" );  /* LISP is running now! */
         first_char_already_sent = 1;

         /*** Cold Boot Sequence of VChannel state machine ***/
         IPC_reset(); /* warm boot */

         /*** Show Debugger, if wished ***/
         if( ARGV_show_debugger ) {
            PAD_show();
         }

         /*
          * *** LISP initialization ***
          *
          * INFORMATION: (CRITICAL PHASE!)
          *    LISP_init sends a first LISP_sndCmd
          *    which inits channel management !!!
          *
          * AFTER lisp initialisation the send-next-command timer
          * will be activated (see LISP_init()) !!!
          */
         AddTimeOut( TIME_LISP_INIT, (XtPointer)LISP_init, NULL );
      }

      /*** ChannelDetection ***/
      if( escape && (c < '0' || c > '9') ) {
         escape = 0;      /* war wohl nix */
      }
      if( escape == 1 ) {
         ch = (c-'0') * 100;
         escape = 2;
         lastchar = c;
         continue;
      }
      if( escape == 2 ) {
         ch += (c-'0') * 10;
         escape = 3;
         lastchar = c;
         continue;
      }
      if( escape == 3 ) {
         channelno = ch + (c-'0');

         /*** EvalHookCheckHang ***/
         evalhook_state_changed = True;

         /*** Show VChannel Transitions ***/
         if( ARGV_show_states ) {
            PAD_append(' ');PAD_append('<');
            switch( channelno ) {
               case STATE_DEBUGGER:
                  /*
                   * hier muss spaeter das DebugPad ge-popup-ped
                   * und raised-to-top werden, falls noch nicht geschehen!
                   */
                  PAD_append('D');PAD_append('B');PAD_append('G');
                  break;
               case STATE_READY:
                  PAD_append('R');PAD_append('D');PAD_append('Y');
                  break;
               case STATE_BUSY:
                  PAD_append('B');PAD_append('S');PAD_append('Y');
                  break;
               case STATE_PAD_READ:
                  PAD_append('P');PAD_append('A');PAD_append('D');
                  break;
               case STATE_BEGIN_REPLY:
                  PAD_append('R');PAD_append('P');PAD_append('L');
                  break;
               case STATE_MKB_SYNC:
                  PAD_append('M');PAD_append('K');PAD_append('B');
                  break;
               case STATE_END_OF_INIT:
                  PAD_append('E');PAD_append('O');PAD_append('I');
                  break;
               default: {
                  PAD_append('0'+channelno/100);
                  PAD_append('0'+(channelno%100)/10);
                  PAD_append('0'+channelno%10);
                  break;
               }
            }
            PAD_append('>');PAD_append(' ');

            /* hg20081106: added for better debugging: */
            if( channelno == STATE_BUSY ) {
              PAD_append('\n');
            }
         }

         /*** LISP_sent_end_of_init ***/
         if( !already_inited && channelno == STATE_END_OF_INIT ) {
            already_inited = True;
            AddTimeOut( TIME_END_OF_INIT,
               (XtPointer)LISP_sent_end_of_init, NULL );
         }

         /*** show debugger in case of LISP error ***/
         if( channelno == STATE_DEBUGGER ) {
            IPC_reset();  /* warm boot sequence (reinit) */
            PAD_show();  /* show PAD, if not yet done */
         }

         /*** synchronize multiple KB state with LISP ***/
         if( channelno == STATE_MKB_SYNC ) {
            add_to_message_buffer = True;
         }

         /*** end of mkb_sync message ***/
         if( oldchannelno == STATE_MKB_SYNC &&
             channelno == STATE_BUSY ) {
            char *str = MSG_read();
            add_to_message_buffer = False;
            KBS_getState( str );
            XtFree( str );
         }

         /*** read from debugger PAD's input text ***/
         if( channelno == STATE_PAD_READ ) {
            add_to_message_buffer = True;
            PAD_directly_read = True;
         }

         /*** end of pad_read message ***/
         if( oldchannelno == STATE_PAD_READ &&
             channelno == STATE_BUSY ) {
            char *str = MSG_read();
            int c, i = 0;
            add_to_message_buffer = False;
            PAD_directly_read = False;
            /* echo inputs to read! */
            while( c = str[i++] )
               PAD_append( c );
            XtFree( str );
         }

         /*** begin C-replies suppression ***/
         if( channelno == STATE_BEGIN_REPLY ) {
            /* Now C will send a reply which will not be echoed in the PAD */
            suppress_displaying = True;
         }

         /*** end of suppress_displaying ***/
         if( oldchannelno == STATE_BEGIN_REPLY &&
             channelno == STATE_BUSY ) {
            suppress_displaying = False;
         }

         /*** warning on protocol inconsistency! ***/
         if( oldchannelno >= 10 && channelno >= 10 ) {
            fprintf( stderr,
               "FATAL: incomplete protocol sequence: old ch# = %d, new ch# = %d\n",
               oldchannelno, channelno );
         }

         /*** begin User message: BUSY -> 10-999 ***/
         if( oldchannelno < 10 && channelno >= 10 ) {
            add_to_message_buffer = True;
         }

         /*** end user message: 10-999 -> BUSY ***/
         if( oldchannelno >= 10 && channelno < 10 ) {
            FPTR fp;

            add_to_message_buffer = False;
            fp = dispatch[ oldchannelno-10 ];
            /* if given, run the message handler */
            if( fp ) {
               (*fp)();
               XtFree( MSG_read() );  /* you never know ... */
            } else {
               int c;
               char *p, *str = MSG_read();
               fprintf( stderr,
                  "Dispatcher: no message handler registered for channel %d\n",
                  oldchannelno );
               fprintf( stderr,
                  "Dispatcher: unhandled message is: [" );
               p = str;
               while( c = *p++ ) {
                  if( c == '\n' || c >= ' ' )
                     fprintf( stderr, "%c", c );
                  else
                     fprintf( stderr, "^%c", c + '@' );  /* 3 --> ^C */
               }
               fprintf( stderr, "]\n" );
               XtFree( str );
            }
         }

         /*** Ready for Input: XXX -> RDY ***/
         if( oldchannelno != channelno && channelno == STATE_READY ) {
            /*
             * delayed, hence after *evalhook* reports READY,
             * it does ResultPresentation and writing of the
             * new InputPrompt (which lasts a non granted amount of time)
             * INFORMATION:
             *    in case of a slow system, this value (init=250)
             *    should be enlarged!!!
             */
            AddTimeOut( TIME_LISP_READY, (XtPointer)CheckedSetReady, NULL );
         }

         oldchannelno = channelno;

         /*** done **********************/

         escape = 0;
         lastchar = c;
         continue;
      }
      lastchar = c;
      if( c == ESC ) {   /* switch to channel number detection */
         escape = 1;
         continue;
      }

      if( add_to_message_buffer ) {
         MSG_append( c );
      } else {
         /*** buffers output to PAD (to avoid flicker) ***/
         if( !suppress_displaying )
            PAD_append( c );
         else if( ARGV_show_states ) {
            /* hg20081106: added for debugging */
            PAD_append( '{' );
            if( c == '\n' || c >= ' ' ) {
               PAD_append( c );
            } else {
               PAD_append( '^' );
               PAD_append( c + '@' );  /* 3 --> ^C */
            }
            PAD_append( '}' );
         }
      }
   } /* while */

   /*** forces a flush (if anything has to be flushed) ***/
   PAD_append( NUL );

   /*** Visualize sensitivity of the Xapplication ***/
   LISP_showState();
}

/****************************************************
*   Interrupt LISP ^C                               *
*   InputWindow=Ready is done by *debugger-hook*    *
*   except interrupted calls like (sleep 4) !?!     *
****************************************************/

static void
LISP_interrupt_TO( void ) {

   /* reinit sndNextCmd */
   stop_ticking = False;
   AddTimeOut( TIME_SND_CMD, (XtPointer)LISP_sndNextCmd, NULL );

   LISP_sndCmd( "(emaxps-user-interrupt)" );
}

/***************************************************/

void
LISP_interrupt( void ) {

   IPC_reset();  /* warm boot sequence (reinit) */
   PAD_show();  /* show PAD, if not yet done -> SessionScreen */

   if( !lispPid ) {
      fprintf( stderr,
         "LISP_interrupt: LISP process does not exist\n" );
      return;
   }
   if( kill( lispPid, SIGINT ) ) {    /* interrupt */
      perror("LISP_interrupt: kill SIGINT");
   }
   /*
    * Now NO more offending commands are buffered!
    *
    * This code is necessary, because of a bug in CLISP:
    * If the user presses the Break!-Button while Lisp is
    * doing (sleep XX), which is a break from within a 
    * System-Routine, CLISP in this case enters the debugger
    * directly without doing *debugger-hook* !!!
    * ==> No <DBG> VChannel change is sent!
    * Hence channel-management fails to be installed at this
    * debugger level!
    * (emaxps-user-interrupt) grants reinstallation of 
    * channel management. See debugger.lisp!
    *
    * In most cases the Break! forces a <DBG> VChannel change
    * which might prevent sending this command. But in this
    * case working is granted by the 'normal' toplevelPIH action!
    *
    * TimoutValue: Should be quite a long value!!!
    */
   stop_ticking = True;
   AddTimeOut( TIME_AFTER_BREAK, (XtPointer)LISP_interrupt_TO, NULL );
}

/****************************************************
*   Terminate LISP ^D                               *
****************************************************/

void
LISP_kill( void ) {

   if( !lispPid ) {
      fprintf( stderr, "LISP_kill: LISP process does not exist\n");
      exit(0);
   }
   if( kill( lispPid, SIGTERM ) ) {   /* terminate */
      perror("LISP_kill: kill SIGTERM");
   }
   /* exit(0);     --> is done by sigchildHandler! */
}

/****************************************************
*   This function opens up a pty and tty master     *
*                                                   *
*   Watch out the OS dependencies here:             *
*   when porting, you might like to compare this    *
*   with get_pty() in xterm's main.c file           *
****************************************************/

static void
IPC_create( void ) {
   int i1, i2, pty;

/*
 * hg20081112: added. old version was: (O_RDWR)
 *
 * See file rxvt-cygwin_ptytty.c for details.
 *
 * See: http://www.mail-archive.com/bug-coreutils@gnu.org/msg06156.html
 * "... I do know that O_NOCTTY prevents setting the process's
 * controlling terminal. ..."
 */
#define OPENFLAGS (O_RDWR|O_NOCTTY)

   /*** Init IPC channels ***/
   /*
    * OS port:
    *          get_pty assumes existance of those PseudoTTYs
    * HENCE:   check for correctness !!!
    *          Linux 1.2.1 = "pqrs"; SunOS 4.1.3 = "pqr";
    * Warning: CRAY uses different names: ttyp0 to ttyp255 !
    *          IRIX (sgi) uses ttyq0 to ttyq120 !
    */
#if defined(sgi)
   char *tty_name = NULL;
   char *_getpty();
#else  /* !sgi */
   char *letter1 = "pqr";
   char *letter2 = "0123456789abcdef";
#endif /* !sgi */
   char *ptydevname = XtMalloc(16);
   char *ttydevname = XtMalloc(16);

   fprintf( stderr, " >>> looking for a free PTY ...\n" );

#if defined(__CYGWIN__) /* hg20081112: added (v2.1.6) */
   {
      extern char *ptsname();
      char *ptsn = NULL;
      pty = open("/dev/ptmx",OPENFLAGS,0);
      if (pty >= 0) {
        ptsn = ptsname(pty);
        if( ARGV_search_pty )
           fprintf( stderr,
              "  >>> /dev/ptmx offered '%s' ...\n", ptsn );
         if (grantpt(pty) == 0       /* change slave permissions */
          && unlockpt(pty) == 0) {   /* slave now unlocked */
            goto success;
         }
      }
      /* failure: no ptys! */
      fprintf( stderr, "EMA-XPS: get_pty: no available ptys\n");
      exit( -1 );
   success:
      strcpy( ttydevname, ptsn );    /* get slave's name */
      strcpy( ptydevname, ptsn );    /* hg: not to leave it empty */
   }
#else  /* !__CYGWIN__ */
#if defined(sgi)
   tty_name = _getpty( &pty, OPENFLAGS, 0622, 0 );
   if( tty_name == NULL ) {
      /* failure: no ptys! */
      fprintf( stderr, "EMA-XPS: get_pty: no available ptys\n");
      exit( -1 );
   }
   strcpy( ttydevname, tty_name );   /* /dev/ttyqNNN  NNN={0,120} */
   strcpy( ptydevname, tty_name );   /* /dev/pts/NNN  NNN={0,120} */
                                     /* 0123456789o1 */
   ptydevname[5] = 'p';
   ptydevname[7] = 's';
   ptydevname[8] = '/';
#else  /* !sgi */
   /*** init filenames ***/
   ptydevname = XtMalloc(16);
   ttydevname = XtMalloc(16);

   strcpy( ptydevname, "/dev/ptyXX" );
   strcpy( ttydevname, "/dev/ttyXX" );
                     /* 0123456789 */

   if( ARGV_search_pty )
      fprintf( stderr,
         "  >>> searching /dev/ptyXY with X='%s' and Y='%s' ...\n",
         letter1, letter2 );

   /* start searching now: */
   for( i1 = 0; ptydevname[8]=letter1[i1]; i1++ ) {
      for( i2 = 0; ptydevname[9]=letter2[i2]; i2++ ) {
         if( ARGV_search_pty )
            fprintf( stderr, "  >>> try opening %s read/write ...\n",
               ptydevname );
         if( (pty=open(ptydevname,OPENFLAGS)) >= 0 ) {
            ttydevname[8]=letter1[i1];
            ttydevname[9]=letter2[i2];
            goto success;
         }
      }
   }
   /* failure: no ptys! */
   fprintf( stderr, "EMA-XPS: get_pty: no available ptys\n");
   exit( -1 );
 success:
#endif /* !sgi */
#endif /* !__CYGWIN__ */

   if( ARGV_search_pty )
      fprintf( stderr, "  >>> setting O_NDELAY on %s ...\n",
         ptydevname );
   c_lisp = pty;   /* makes it globally known */
   setNoDelay( c_lisp );

   if( ARGV_search_pty )
      fprintf( stderr, "  >>> opening %s read/write ...\n",
         ttydevname );
   /* now open this connection or wait forever */
   while( (tty=open(ttydevname,OPENFLAGS,0)) < 0 );

   fprintf( stderr, " >>> ... using %s\n", ttydevname );
}

/****************************************************
*   This function forks the child process.          *
*   The master tty and pty must be open up to now.  *
****************************************************/

void
LISP_run( void ) {
   int ppid;
   /* arg0 of the LISP process */
   static char *cmdline[] = { "emalisp", NULL };

   fprintf( stderr, " >>> spawning LISP process ...\n");

   /*** Init Scan Fkt. ****/
   XtAddInput( c_lisp, (XtPointer)XtInputReadMask,
      (XtPointer)toplevelPipeInputHandler, NULL );

   if( (int)signal( SIGINT, (SHP)sigintHandler ) == ERR ) {
      /*
       * falls vom xterm ein ^C kommt,
       * soll neben dem C- auch 
       * der LISP-Prozess sterben
       */
      perror("signal(SIGINT)");
      exit( -1 );
   }

   ppid = getpid();  /* PID of the parent = C-process */

   /*** spawn the EMALISP process ***/
   if( (lispPid=fork()) == -1 ) {
      Error("fork");
   }

   if( !lispPid ) {
      /*** Child ***/
      close( c_lisp );     /* open pty-handle not to be used by the child */

      /*
       * make the child an own session leader, to avoid
       * SIGCHLD --> parent, if parent is ^Z and bg'ed !!!
       * PROBLEM was:
       * Linux produces a zombie, if LISP dies before C,
       * SunOS eliminates LISP
       * --> kill(lispPid,0) gives useful infos ONLY on SUNS !!
       */
      setsid();

#ifdef hpux
      {
         struct termio tio;
         struct winsize win;
	 /*
	  * HP-UX seems to reset the termiio structure of a line
	  * completely each time, the PTYs are released!
          *
	  * All other UNIXes under test have had working presets.
	  * WARNING: I did not check, whether the other OSes have fine presets
	  * OR whether they simply do not reset old users' settings? (e.g. xterms)
	  * BUT it seems, they inherit the settings via dup() ?!
	  */
         if( ioctl(tty, TCGETA, &tio) ) {
            perror("ioctl: TCGETA");
	    kill( ppid, SIGTERM );
            exit( -1 );
         }
         /* taken from SunOS 4.1.3 settings */
         tio.c_iflag = ICRNL|BRKINT|IGNPAR|ISTRIP|IXON|IMAXBEL;
         tio.c_oflag = OPOST|ONLCR;
         tio.c_lflag = (unsigned short)IEXTEN|ISIG|ICANON|ECHO|ECHOE|ECHOK|ECHOCTL|ECHOKE;
         tio.c_cflag = B9600|PARENB|CS7|CREAD;
         if( ioctl(tty, TCSETA, &tio) ) {
            perror("ioctl: TCSETA");
	    kill( ppid, SIGTERM );
            exit( -1 );
         }
         /*
          * If #cols=0 (default) the pretty printer will not work! the Editors
          * will present everything as one long line -- UGLY!
          *
          * SUN e.g. takes the values from the xterm, EMA-XPS is run from...
          * check it from the DebuggerPAD with (excl:run-shell-command "stty -a")
          */
         win.ws_row = 24;
         win.ws_col = 80;
         if( ioctl(tty, TIOCSWINSZ, &win) ) {
            perror("ioctl: TIOCSWINSZ");
	    kill( ppid, SIGTERM );
            exit( -1 );
         }
      }
#endif

      /*
       * dup the tty:
       * dup() always uses the first unused
       * file descriptor (starting at number 0) ...
       */
      if( close(0) < 0 ) {
         perror( "close(stdin)");
         kill( ppid, SIGTERM );
         exit( -1 );
      }
      if( dup(tty) < 0 ) {
         perror("dup(stdin)");
         kill( ppid, SIGTERM );
         exit( -1 );
      }
      if( close(1) < 0 ) {
         perror( "close(stdout)");
         kill( ppid, SIGTERM );
         exit( -1 );
      }
      if( dup(tty) < 0 ) {
         perror("dup(stdout)");
         kill( ppid, SIGTERM );
         exit( -1 );
      }

      /* and finally close the tty */
      if( close(tty) < 0 ) {
         perror("close tty");
         kill( ppid, SIGTERM );
         exit( -1 );
      }
/*
printf("isatty(0): %d [%s]\n", isatty(0), ttyname(0));
printf("isatty(1): %d [%s]\n", isatty(1), ttyname(1));
printf("isatty(2): %d [%s]\n", isatty(2), ttyname(2));
*/

      /*
       * Disable NLS for the LISP interpreter (child process).
       * This grants english language settings as of the csh.
       */
      if( putenv("LANG=C") )
         perror("putenv LANG=C");

      /*
       * If required,
       * test Terminalsettings now, that stdin/out have moved
       * to the new terminal line
       */
      /* system("stty -a 1>&2"); */

      execv( emalisppath, cmdline );

      /* Exec failed. */
      kill( ppid, SIGTERM );
      Error("execv EMALISP");
   }

   /*** Parent ***/
   close( tty );   /* tty file handle is used by the child only */

   if( (int)signal( SIGCHLD, (SHP)sigchildHandler ) == ERR ) {
      /*
       * if the LISP-process dies, e.g. user entered (exit) or (bye),
       * the C-process should die, too !!!
       */
      Error("signal(SIGCHLD)");
   }
}

/****************************************************
*   Replacement for XmGetPixmap WITHOUT caching!!!  *
****************************************************/

Pixmap
CreatePixmapFromXBMfile( Widget wdg, char *name, Pixel fg, Pixel bg ) {
   static int first_time = 1;
   static GC gc;
   Pixmap bitmap, pixmap;
   int x, y, w, h;
   char file_name[1024];
   XGCValues gcValues;     /* Struct for creating GC */
   unsigned long bit_plane = 1;  /* src=bitmap */
   Display *dpy = XtDisplay( wdg );
   Screen *scrn = XtScreen( wdg );
   FILE *fp;

   /*
    * Motif BUG ???          16.04.96
    *
    * XmGetIconFileName after some sucessful calls forgets all and finds
    * the files in "/" directory ?!?
    */
#if 0 /* (XmVersion >= 2000) */
   /*
    * HG: 27.11.95
    * Beginning with Motif 2.0 the different directories that
    * are searched for Pixmap files are cached internally !!!
    * TH Darmstadt uses gunzip/ui-set-bitmap/gzip to save space
    * on hard disk drive! The unzipped filenames are not cached,
    * XmGetPixmap fails and returns XmUNSPECIFIED_PIXMAP !!!
    * THIS flushes ALL directory caches:
    */
   XmeFlushIconFileCache( NULL );

   /* Motif PATH search. */
   file_name = XmGetIconFileName( scrn,
      NULL, name, NULL, XmUNSPECIFIED_ICON_SIZE );
#else
   if( fp=fopen(name,"r") ) {
      /* try to find it directly ... */
      fclose( fp );
      strcpy( file_name, name );
   } else {
      /* ... or in one special directory ... */
      strcpy( file_name, emabmpath );   /* allways ends in a "/" */
      strcat( file_name, name );
      if( fp=fopen(file_name,"r") ) {
         fclose( fp );
      } else {
         /* ... or give it up! */
         return XmUNSPECIFIED_PIXMAP;
      }
   }
#endif

   /* Read XBM file into depth=1 pixmap (bitmap) */
   if( XReadBitmapFile( dpy, RootWindowOfScreen(scrn),
          file_name, &w, &h, &bitmap, &x, &y ) != BitmapSuccess ) {
      return XmUNSPECIFIED_PIXMAP;
   }

   /* Create a pixmap to hold the image. */
   pixmap = XCreatePixmap( dpy, RootWindowOfScreen(scrn),
      w, h, DisplayPlanes( dpy, DefaultScreen(dpy) ) );

   /* Create/Change a gc for the bitmap to pixmap copy */
   gcValues.foreground = fg;
   gcValues.background = bg;
   if( first_time ) {
      first_time = 0;
      gc = XCreateGC( dpy, pixmap,
              GCForeground | GCBackground, &gcValues );
   } else {
      XChangeGC( dpy, gc,
              GCForeground | GCBackground, &gcValues );
   }

   /* store the image into the pixmap */
   XCopyPlane( dpy, bitmap, pixmap, gc,
      0, 0, w, h, 0, 0, bit_plane );

   XFreePixmap( dpy, bitmap );
   return pixmap;
}

/****************************************************
*   Create Logo if possible      (logo.xbm only)    *
****************************************************/

Pixmap
createPushButtonPixmap( Widget btn, char *path ) {
   int bg, fg;
   Pixmap pm;

   XtVaGetValues( btn, XmNbackground, &bg,
                       XmNforeground, &fg, NULL );
   pm = createPixmapFromFile( btn, path, fg, bg );
   XtVaSetValues( btn,
      XmNlabelPixmap, pm,
      XmNlabelInsensitivePixmap, pm, NULL );
   return pm;
}

/****************************************************
*   Create Pixmap from a BITMAP file                *
*   Watch out the Linux BUG!!!     (main,xinit,uif) *
****************************************************/

Pixmap
createPixmapFromFile( Widget w, char *path,
                      Pixel fg, Pixel bg ) {
#if 1
   /*
    * Flaw in Motif:
    *
    * Pixmaps, created from XBM files are cached
    * internally within an additional Image-Cache (Bitmap-Info without
    * color-info), too. Using XmDestroyPixmap only the Entry in Pixmap
    * cache is erased. The Image cache will NEVER be cleared !!!
    * Hence it is growing and growing ... during the whole live cycle
    * of the application !!!!!
    * In case of Darmstadt's big XBMs an out-of-swap-space may occur!
    *
    * Hence a NONcaching version of XmGetPixmap has been realized!
    */
   Pixmap pm = CreatePixmapFromXBMfile( w, path, fg, bg );
   if( pm == XmUNSPECIFIED_PIXMAP ) {   /* ==2 (btw) */
      fprintf( stderr,
       "EMA-XPS: Warning: CreatePixmapFromXBMfile(\"%s\") returned XmUNSPECIFIED_PIXMAP\n",
      path );
   }
   return pm;

#else /*** old caching version... ***/
   Pixmap pm, pm_bug;
   XImage *image;
   unsigned int ret;
   Screen *scrn = XtScreen(w); 

# ifdef linux
   /*** Motif-2.0-for-Linux BUG ?!? ***/
   pm_bug = XmGetPixmap( scrn, path, fg, bg );
# endif
   pm = XmGetPixmap( scrn, path, fg, bg );
   if( pm == XmUNSPECIFIED_PIXMAP ) {
      fprintf( stderr,
       "EMA-XPS: Warning: XmGetPixmap(\"%s\") returned XmUNSPECIFIED_PIXMAP\n",
      path );
   }
# ifdef linux
   /* the wrong Pixmap should be removed to reduce vmem needs */
   XmDestroyPixmap( scrn, pm_bug );
# endif

   return pm;
#endif /*** old caching version... ***/
}

/****************************************************
*   Destroy Pixmap from Motif Cache  (main,uif,uiw) *
****************************************************/

void
destroyPixmap( Widget w, Pixmap pm ) {

#if 1
   /*
    * Flaw in Motif:  (see above)
    */
   if( pm != XmUNSPECIFIED_PIXMAP )
      XFreePixmap( XtDisplay(w), pm );
#else /*** old caching version... ***/
   XmDestroyPixmap( XtScreen(w), pm );
#endif /*** old caching version... ***/
}

/****************************************************
*   Main program:                                   *
*   initializes IPC channels and Xapplication       *
****************************************************/

void
ARGV_kb_settings( char **fname, char **filter ) {

   *filter = XtMalloc( strlen(ARGV_kb_directory)
                     + strlen(ARGV_kb_extension) + 4 );
   strcpy( *filter, ARGV_kb_directory );
   strcat( *filter, "/*." );
   strcat( *filter, ARGV_kb_extension );

   *fname = ARGV_autoload_kb;
}

/***************************************************/

main( Cardinal argc, char **argv ) {
   Arg      args[20];
   int      i, len, n;
   char    *ptr, path1[1024], path2[1024], path3[1024];
   char    *p3, path4[1024];
   Display *dpy;
   FILE    *fp;
   char    *ARGV_xres_instance_name = "EMA-XPS";
   int      langflag = EX_NLS_XX;

   /*** Check $LANG environment setting for NLS analysis ***/
   if( !(p3=getenv("LANG") ) ) {
      fprintf( stderr,
         "EMA-XPS: Warning: $LANG is not set: assuming use of the C locale\n" );
      p3 = "C";
   }

   /* this cannot grant completeness */
        if( !strcmp(p3,"C") )       langflag = EX_NLS_EN;
   else if( !strcmp(p3,"POSIX") )   langflag = EX_NLS_EN;

   else if( !strcmp(p3,"deutsch") ) langflag = EX_NLS_DE;
   else if( !strcmp(p3,"de") )      langflag = EX_NLS_DE;
   else if( !strcmp(p3,"de_DE") )   langflag = EX_NLS_DE;
   else if( !strcmp(p3,"De_DE") )   langflag = EX_NLS_DE;
   else if( !strcmp(p3,"de_CH") )   langflag = EX_NLS_DE;

   else if( !strcmp(p3,"english") ) langflag = EX_NLS_EN;
   else if( !strcmp(p3,"en") )      langflag = EX_NLS_EN;
   else if( !strcmp(p3,"en_US") )   langflag = EX_NLS_EN;
   else if( !strcmp(p3,"En_US") )   langflag = EX_NLS_EN;
   else if( !strcmp(p3,"en_GB") )   langflag = EX_NLS_EN;
   else if( !strcmp(p3,"en_UK") )   langflag = EX_NLS_EN;

   if( langflag == EX_NLS_XX ) {
      fprintf( stderr,
         "EMA-XPS: Warning: $LANG: using english inspite of unknown [%s]\n",
         p3 );
      langflag = EX_NLS_EN;
   }

   /* Overwrite NLS settings to make X11 find its resources: */
   if( langflag == EX_NLS_DE ) {
      p3 = "deutsch";
      if( putenv("LANG=deutsch") )
         perror("putenv LANG=deutsch");
   } else {
      p3 = "english";
      if( putenv("LANG=english") )
         perror("putenv LANG=english");
   }

   /*** This has to be preceeded! ***/
   for( n = 1; n < argc; n++ ) {
      if( !strcmp( argv[n], "-bw" ) ) {
         ARGV_xres_instance_name = "EMA-XPS-BW";
         continue;
      }
   }

_dbg("1a");
CygwinPreInitialize();
_dbg("1b");
   /*** This makes Screen geometry available: ***/
   toplevel = XtInitialize(
                 "ignored",  /* instance-name (taken from argv[0] !) */
                 ARGV_xres_instance_name,  /* class-name */
                 NULL, 0, &argc, argv );
_dbg("1c");
   CygwinPostInitialize( toplevel );
_dbg("2");
   XtVaSetValues( toplevel, XmNmappedWhenManaged, False, NULL );
_dbg("3");
   XtRealizeWidget( toplevel );
_dbg("4");

   /*** Fixing geometryConstraint's ***/
   /*
    * the MWM does not reposition this window,
    * hence won't ever be mapped!
    */
   sprintf( geometryConstraint, "100x100+%d+%d",
      (WidthOfScreen(XtScreen(toplevel)) >> 1) - 50,
      ((HeightOfScreen(XtScreen(toplevel)) * 3) >> 3) - 50
   );
_dbgs(geometryConstraint);

   /*** Commandline Parsing ***/
   ARGV_search_pty        = False;   /* verbose looking for free PTYs   */
   ARGV_checked_set_ready = False;   /* print CheckedSetReady warnings  */
   ARGV_boot_verbose      = False;   /* verbosely booting of the GUI    */
   ARGV_show_debugger     = False;   /* Show Debugger, if wished        */
   ARGV_show_states       = False;   /* Show VChannel Transitions       */
   ARGV_autoload_kb       = NULL;
   ARGV_autorun_kb        = False;
   ARGV_kb_directory      = ".";
   ARGV_kb_extension      = "kb";

   for( n = 1; n < argc; n++ ) {
      if( !strcmp( argv[n], "-pty" ) ) {
         ARGV_search_pty = True;
         continue;
      }
      if( !strcmp( argv[n], "-csr" ) ) {
         ARGV_checked_set_ready = True;
         continue;
      }
      if( !strcmp( argv[n], "-gui" ) ) {
         ARGV_boot_verbose = True;
         continue;
      }
      if( !strcmp( argv[n], "-dbg" ) ) {
         ARGV_show_states = True;
         continue;
      }
      if( !strcmp( argv[n], "-pad" ) ) {
         ARGV_show_debugger = True;
         continue;
      }
#if 0  /* OBSOLETE */
      if( !strcmp( argv[n], "-c" ) ) {
         if( (++n) >= argc ) {
            usage();
         }
         /*
          * collect the remaining commandline options to
          * be one resulting string
          */
         for( i = n, len = 3; i < argc; i++ )
            len += 1 + strlen( argv[i] );
         ARGV_single_command = XtMalloc( len );
         strcpy( ARGV_single_command, "\"" );
         strcat( ARGV_single_command, argv[n] );
         for( n++; n < argc; n++ ) {
            strcat( ARGV_single_command, " " );
            strcat( ARGV_single_command, argv[n] );
         }
         strcat( ARGV_single_command, "\"" );
         continue;
      }
#endif
      if( !strcmp( argv[n], "-run" ) ) {
         ARGV_autorun_kb = True;
         continue;
      }
      if( !strcmp( argv[n], "-kbs" ) ) {
         if( (++n) >= argc ) {
            usage();
         }
         ARGV_kb_directory = argv[n];
         continue;
      }
      if( !strcmp( argv[n], "-kb" ) ) {
         if( (++n) >= argc ) {
            usage();
         }
         ARGV_autoload_kb = argv[n];
         continue;
      }
      if( !strcmp( argv[n], "-ext" ) ) {
         if( (++n) >= argc ) {
            usage();
         }
         ARGV_kb_extension = argv[n];
         continue;
      }
      if( !strcmp( argv[n], "-bw" ) ) {
         /* see above... */
         continue;
      }
      usage();           /* -help */
   }

   if( ARGV_autoload_kb ) {
      ptr = XtMalloc( strlen(ARGV_kb_directory) + 1 +
                      strlen(ARGV_autoload_kb) + 1 +
                      strlen(ARGV_kb_extension) + 1 );
      strcpy( ptr, ARGV_kb_directory );
      strcat( ptr, "/" );
      strcat( ptr, ARGV_autoload_kb );
      strcat( ptr, "." );
      strcat( ptr, ARGV_kb_extension );
      ARGV_autoload_kb = ptr;
   } else {
      ARGV_autorun_kb = False;
   }

   /*** Check Commandline Presets ***/
   /* existance of kb_directory path */
   /* existance of kb_filename */
/* UNDER CONSTRUCTION */

   /*** Check environment settings (except LANG, see above) ***/
   if( !(ptr=getenv("XAPPLRESDIR") ) ) {
      if( !(ptr=getenv("HOME") ) ) {
         fprintf( stderr,
            "EMA-XPS: Warning: $XAPPLRESDIR and $HOME are not set!\n" );
         fprintf( stderr,
            "   Assuming \".\" directory holds EMA-XPS resources!\n" );
         ptr = ".";
      } else {
         fprintf( stderr,
            "EMA-XPS: Warning: $XAPPLRESDIR is not set!\n" );
         fprintf( stderr,
            "   Assuming $HOME directory holds EMA-XPS resources!\n" );
      }
   }
         
   strcpy( path3, ptr );
   strcat( path3, "/" );
   strcat( path3, p3 );
   strcat( path3, "/ema-xps-help.txt" );
   emahelppath = path3;

   /* Don't trust BitmapFileSearch routines of Xt and Xm, do it your own! */
   strcpy( path4, ptr );
   strcat( path4, "/bitmaps/" );
   emabmpath = path4;

   if( !(fp = fopen( emahelppath, "r" )) ) {
      fprintf( stderr, "EMA-XPS: WARNING: Could not find HELP file \"%s\" !\n"
                       "                  No help available.\n",
         emahelppath );
      emahelppath = NULL;
   } else {
      fclose( fp );
   }
_dbg("5");
   HELP_setPath( emahelppath );   /* make it known to the Help-Utility */

   strcpy( path1, ptr );
   strcat( path1, "/" );
   strcat( path1, EMAXPS_lisp_filename );
   emalisppath = path1;

   strcpy( path2, ptr );
   strcat( path2, "/ema-xps-init.lisp" );
   emainitpath = path2;

   if( !(fp = fopen( emalisppath, "r" )) ) {
      fprintf( stderr, "EMA-XPS: ERROR: Could not find EMALISP \"%s\" !\n",
         emalisppath );
      exit( -1 );
   } else {
      fclose( fp );
   }

_dbg("6");
   /*** create the IPC channel using a pseudo TTY ***/
   IPC_create();

_dbg("7");
   /*** install Lowlevel-Error-Handler ***/
   XtSetErrorHandler( errorHandler );

_dbg("8");
   /*** define common Icon Pixmap ***/
   dpy = XtDisplay( toplevel );
_dbg("9");
   iconPm = XmGetPixmap( XtScreen(toplevel), ICON_XBM,
                         BlackPixel( dpy, 0 ),   /* icon-bitmaps' color */
                         WhitePixel( dpy, 0 ));  /* values are ignored. */
_dbg("10");
   if( iconPm == XmUNSPECIFIED_PIXMAP ) {
      fprintf( stderr,
         "EMA-XPS: Warning: XmGetPixmap(\"%s\") returned XmUNSPECIFIED_PIXMAP\n",
         ICON_XBM );
   }

_dbg("11");
   /*** init globally known variable ***/
   WM_DELETE_WINDOW = XmInternAtom(
      XtDisplay(toplevel), "WM_DELETE_WINDOW", False );  

_dbg("12");
   /*** Initialize Mouse cursors ***/
   cursor_normal = XCreateFontCursor( dpy, XC_left_ptr );
   cursor_busy   = XCreateFontCursor( dpy, XC_watch    );

_dbg("13");
   /*** Create the system dialogs shell and the welcoming popup ***/
   SystemDialogs = WELCOME_create( toplevel );

_dbg("14");
   /*** Show the welcoming Box ***/
   WELCOME_show();

_dbg("15");
   /*** adds a timeout, to give a chance to the ***/
   /*** Xserver to complete drawing the welcoming popup ***/
   XINIT_complete( toplevel, ARGV_boot_verbose );

_dbg("16-XtMainLoop");
   XtMainLoop();    
}

/****************************************************
*   Derived from the Xlib call XIconifyWindow       *
*                                                   *
*   This function instructs the window manager to   *
*   change this window from IconicState to          *
*   NormalState.                                    *
*                                                   *
*   see: X11R5/mit/lib/X/Iconify.c                  *
****************************************************/

static Status
XDeIconifyWindow( Display *dpy, Window w, int screen ) {
   XClientMessageEvent ev;
   Window root = RootWindow( dpy, screen );
   Atom prop;

   prop = XInternAtom( dpy, "WM_CHANGE_STATE", False );
   if( prop == None )
      return False;

   ev.type = ClientMessage;
   ev.window = w;
   ev.message_type = prop;
   ev.format = 32;
   ev.data.l[0] = NormalState;
   return XSendEvent( dpy, root, False,
                      SubstructureRedirectMask|
                      SubstructureNotifyMask,
                      (XEvent*)&ev );
}

/****************************************************
*   Convenience Call for all Editors, a.s.o.        *
****************************************************/

void
PopupRaiseAndDeiconify( Widget w ) {

   XDeIconifyWindow(
      XtDisplay( w ), XtWindow( w ),
      DefaultScreen( XtDisplay( w ) )
   );
   XtPopup( w, XtGrabNone );
   XRaiseWindow( XtDisplay(w), XtWindow(w) );
}

/*** EOF *******************************************/

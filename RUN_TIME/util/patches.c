/***********************************************
*   patches.c                                  *
*                                              *
*   replaces line 3 of 5 files below the dir,  *
*   argv[1] points to.                         *
*   Is used by global installation process!    *
***********************************************/

#include <stdio.h>

#define TEMPFILE "/tmp/grmpftl"

char *v[] = {
 "lib/emalisp",
 "lib/remalisp",
 "bin/xps",
 "bin/rxps",
 "bin/ui-demo",
 NULL
};

char buf[1024];
char name[1024];

main( int argc, char **argv ) {
   int i = 0, j = 0;
   FILE *fp, *tmp;

   if( argc != 2 ) {
      fprintf( stderr, "Usage: patches /usr/EMA-XPS\n");
      exit( -1 );
   }
   while( v[j] ) {
      sprintf( name, "%s/%s", argv[1], v[j] );
      printf(">>> Patching line 3 of file [%s] ...\n", name );

      if( !(tmp=fopen(TEMPFILE,"w")) ) {
         perror( TEMPFILE );
         exit( -1 );
      }
      if( !(fp=fopen(name,"r")) ) {
         perror( name );
         exit( -1 );
      }

      i = 0;
      while( fgets( buf, 1024, fp ) ) {
         if( (++i) == 3 )
            sprintf( buf, "emapath=%s\n", argv[1] );
         fputs( buf, tmp );
      }
      fclose( fp );
      fclose( tmp );

      sprintf( buf, "mv %s %s ; chmod 755 %s", TEMPFILE, name, name );
      system( buf );

      j++;
   }
   puts("   done.");
   exit( 0 );
}

/* eof */

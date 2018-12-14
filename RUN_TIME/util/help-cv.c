/***********************************************
*   help-cv.c             FILTER               *
*                                              *
*   converts "\nFF\n" to "\n\f" and            *
*   outputs only lines beginning with a ".".   *
*   This DOT is not transfered !!              *
*                                              *
*   The ">" works like "." but should be the   *
*   first printed line of a topic. It is used  *
*   as the BROWSE-keyword!                     *
***********************************************/

#include <stdio.h>

main() {
   char buf[1024];

   while( fgets( buf, 1024, stdin ) ) {
      if( !strcmp( buf, "FF\n" ) ) {
         fputs( "\f", stdout );
         continue;
      }
      if( *buf != '.' && *buf != '>' )
         continue;
      fputs( buf+1, stdout );
   }
}

/* eof */

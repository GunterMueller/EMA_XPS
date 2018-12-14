/***********************************************
*   cpp-post.c            FILTER               *
*                                              *
*   converts octal character above 0200 into   *
*   ASCII and backquotes into quotes!          *
*   Should be run after the cpp!               *
***********************************************/

#include <stdio.h>

char buf[1024];
char out[1024];

int
is23( int c ) {
   return c == '2' || c == '3';
}

int
isoctal( int c ) {
   return c >= '0' && c <= '7';
}

main() {
   int i, j, imax;

   while( fgets( buf, 1024, stdin ) ) {
      imax = strlen( buf );
      j = 0;
      for( i = 0; i < imax; i++ ) {
         if( buf[i] == '`' )
            out[j++] = '\'';
         else if( buf[i] == '\\' && is23(buf[i+1]) &&
             isoctal(buf[i+2]) && isoctal(buf[i+3]) ) {
            int c1, c2, c3, c;
            c1 = buf[i+1] - '0';
            c2 = buf[i+2] - '0';
            c3 = buf[i+3] - '0';
            c = ((c1*8+c2)*8+c3);
            out[j++] = c;
            i += 3;
         } else {
            out[j++] = buf[i];
         }
      }
      out[j] = 0;
      fputs( out, stdout );
   }
}

/* eof */

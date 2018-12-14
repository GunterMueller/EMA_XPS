/***************************************************************************
*   online2html.c               Hans Groschwitz               16.09.96     *
*                                                                          *
*   http://anubis.science.unitn.it/services/html/html-spec_11.html#SEC82   *
*                                                                          *
*   help.de and help.en are built on UNIX sites, hence \r should not       *
*   appear!                                                                *
*   TABs shouldn't appear either!    (Ceelen!)                             *
*                                                                          *
*   0-20                                                                   *
*   21    Begin OnlineHelp on Syntax:                                      *
*                sorted: 23-367 (except: 227,279,328,343,358)              *
*    22    babylon-LISP                                                    *
*    227   UserInterface                                                   *
*    279   KnowledgeHandling                                               *
*    328   Babylon Query Language (BQL)                                    *
*    343   KBconstructs                                                    *
*    358   ExplanationFacility                                             *
*    368   NotSupported: manual editing of the rest into this one page!    *
*     390,401 variables, rest: expl.func (369-415)                         *
*     419,420 spec.text, rest: prog.intf (416-433)                         *
*                                                                          *
***************************************************************************/

/****
mkdir tmp
cd tmp
../online2html 0 999 < ../../src/help.de > out
mv ondoc021.html ref.html
mv ondoc000.html uguide.html
mv ondoc001.html uguide1.html
rm ondoc00[2-4].html
joe ondoc022.html   #remove lines: prev
joe ondoc433.html   #remove lines: next
joe ondoc005.html   #remove lines: prev
joe ondoc020.html   #remove lines: next
mv -i ondoc???.html ../../doc/html/de

joe out  #000-021 move to 1, 022,227,279,328,343,358,368 move to 2, leaving blank lines
sort < out > 3
joe 3               #remove blank lines,mv last entries [{ on top!
awk '{print "   <a href=\"" $2 "\">" $1 "</a>"}' < 2 > 4
awk '{print "   <a href=\"" $2 "\">" $1 "</a>"}' < 3 > 5
joe 4; joe 5        #remove blank entires, if necessary
rm out 2 3

joe ref.html        #include 4,5 before /pre, no prev,next, edit GoToTop,text...
cat ref.html >> ../../doc/html/de/ref.html
rm ref.html 4 5

awk '{print "   <a href=\"" $2 "\">" $1 "</a>"}' < 1 > out
joe out             #remove 000-004, empty lines?

joe uguide.html     #include uguide1.html,out before /pre, no prev,next, edit GoToTop,text...
cat uguide.html >> ../../doc/html/de/uguide.html
rm uguide.html 1 out

rm *
# repeat it completely with help.en !!!
rm *

cd ..
rmdir tmp

# further editing is required in doc/html/{de,en} :
# ondoc{005-020}.html : ref.html --> uguide.html
rm -f /tmp/ondoc*
for i in ondoc0[01]?.html ondoc020.html; do
sed s/ref\.html/uguide\.html/ < $i > /tmp/$i
done
mv /tmp/ondoc* .

# uguide.html, ref.html major beautifications
****/

#include <stdio.h>

int i, first, last;

char*
fnam( int i ) {
   static char buf[256];

   sprintf( buf, "ondoc%d%d%d.html", i/100, (i%100)/10, i%10 );
   return buf;
}

void
head( char *title, FILE *fp ) {
   int im = i-1, ip = i+1;

   if( i < first || i > last )
      return;

   fprintf(fp,"<html><head><title>%s</title></head>\n", title );
   fprintf(fp,"<!-- This file has been created automatically by online2html -->\n");
   fprintf(fp,"<BODY background=\"glyph.gif\" VLINK=\"#A52A2A\" LINK=\"#FF0000\">\n");
   fprintf(fp,"<font size=\"4\">\n\n");

   fprintf(fp,"<A HREF=\"%s\"><IMG SRC=\"nextpage.gif\" align=right></A>\n", fnam(ip) );
   fprintf(fp,"<A HREF=\"%s\"><IMG SRC=\"prevpage.gif\" align=right></A>\n", fnam(im) );
   fprintf(fp,"<A HREF=\"ref.html\"><IMG SRC=\"GoToTop.gif\" align=left></A>\n");
   fprintf(fp,"<center><i>EMA-XPS Online</i></center>\n");

   fprintf(fp,"<br clear=all>\n<hr>\n\n");

   fprintf(fp,"<p><center><font size=\"6\"><strong>%s</strong></font></center><p>\n",
      title );

   fprintf(fp,"\n<PRE>");
}

void
tail( char *title, FILE *fp ) {
   int ii = i;
   int im = i-1, ip = i+1;

   i++;
   if( ii < first || ii > last )
      return;

   fprintf(fp,"</PRE>\n\n");

   fprintf(fp,"<hr>\n\n");

   fprintf(fp,"<A HREF=\"%s\"><IMG SRC=\"nextpage.gif\" align=right></A>\n", fnam(ip) );
   fprintf(fp,"<A HREF=\"%s\"><IMG SRC=\"prevpage.gif\" align=right></A>\n", fnam(im) );
   fprintf(fp,"<A HREF=\"ref.html\"><IMG SRC=\"GoToTop.gif\" align=left></A>\n");
   fprintf(fp,"<center><i>EMA-XPS Online</i></center>\n");

   fprintf(fp,"</font></body></html>\n\n");
   fprintf(fp,"<!-- %s done --!>\n", title );
}

FILE*
scan( char *title, FILE *fp ) {
   char *p;

   if( fp )
      fclose( fp );
   fp = NULL;

   /* title assumed till rest of line */
   p = title;
   while( 1 ) {
      int c = getchar();
      if( feof(stdin) ) {
         fprintf(stderr, "scan: unexpected EOF\n");
         exit( 1 );
      }
      if( c == '\n' ) {
         *p = 0;
         break;
      }
      *p++ = c;
   }

   if( i < first || i > last )
      return fp;

   printf("%s %s\n", title, fnam(i) );
   if( !(fp=fopen(fnam(i),"w")) ) {
      perror(fnam(i));
      exit( 1 );
   }
   return fp;
}

void
usage() {
   fprintf(stderr, "Usage: online2html pageNo1 pageNoN < help.xx\n");
   exit( 1 );
}

main( int argc, char **argv ) {     /* Filter! */
   char title[1024];
   FILE *fp = NULL;

   if( argc != 3 )
      usage();
   first = atoi(argv[1]);
   last = atoi(argv[2]);
   if( first < 0 || last < first )
      usage();

   fp = scan( title, fp );
   head( title, fp );
   while( 1 ) {
      int c = getchar();
      if( feof(stdin) )
         break;

      if( c == '\f' ) {
         /* separate pages now! */
         tail( title, fp );
         fp = scan( title, fp );
         head( title, fp );
         continue;
      }
      if( !fp ) {
         continue;
      }

      if( c == '\n' ) {
         fprintf(fp,"%c",c);
         continue;
      }
      if( c == '\t' ) {
         fprintf(fp,"%c",c);
         continue;
      }

      if( c == '<' ) {
         fprintf(fp,"&lt;");
         continue;
      }
      if( c == '>' ) {
         fprintf(fp,"&gt;");
         continue;
      }
      if( c == '&' ) {
         fprintf(fp,"&amp;");
         continue;
      }
      if( c == '"' ) {
         fprintf(fp,"&quot;");
         continue;
      }
      if( c >= ' ' && c < 127 ) {
         fprintf(fp,"%c",c);
         continue;
      }
      if( c == 196 ) {
         fprintf(fp,"&Auml;");
         continue;
      }
      if( c == 214 ) {
         fprintf(fp,"&Ouml;");
         continue;
      }
      if( c == 220 ) {
         fprintf(fp,"&Uuml;");
         continue;
      }
      if( c == 223 ) {
         fprintf(fp,"&szlig;");
         continue;
      }
      if( c == 228 ) {
         fprintf(fp,"&auml;");
         continue;
      }
      if( c == 246 ) {
         fprintf(fp,"&ouml;");
         continue;
      }
      if( c == 252 ) {
         fprintf(fp,"&uuml;");
         continue;
      }
      if( c > 160 ) {
         fprintf(fp,"&#%d;", c );
         continue;
      }
      fprintf(stderr, "Ignoring unexpected char %d\n", c );
   }
   tail( title, fp );
   fclose( fp );
}

/* eof */

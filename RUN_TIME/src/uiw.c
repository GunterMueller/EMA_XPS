/******************************************************************************/
/*                               H. Groschwitz                                */
/*       uiw.c                   Michael Block               21.07.1994       */
/*                                                                            */
/*   Die Funktionen dieser Datei verwalten eine Liste aller momentan "cre-    */
/*   ierten" Widgets (unabhaengig davon, ob diese momentan "gemanaged" sind   */
/*   oder nicht). Natuerlich ist darauf zu achten, dass Widgets nach ihrer    */
/*   "Creation" auch ueber die Funktion UIW_addWNAME() angemeldet und damit   */
/*   in diese Liste aufgenommen werden.                                       */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <X11/Intrinsic.h>                    /* Widget !!! fuer Header Datei */
#include <Xm/Xm.h>                            /* XmString !!! ebenfalls       */

#include "uiw.h"
#include "uif.h"

static WNAME *wname_root = NULL;   /* Zeiger auf den ersten Eintrag der Liste */
static WNAME *last_wname;         /* Zeiger auf den letzten Eintrag der Liste */
static int numberOfWidgets = 0;  /* Anzahl der momentan enthaltenen Eintraege */

/******************************************************************************/
/*                                                                            */
/*                        String Operating Functions ...                      */
/*                                                                            */
/******************************************************************************/

/***************************** UIW_NewString **********************************/

char*
UIW_NewString( char *name ) {
   /*
    * Diese Funktion liefert eine Kopie des Strings 'name' innerhalb
    * eines reservierten Speicherbereichs.
    * Wenn diese Kopie nicht mehr benoetigt wird, ist der Speicher-
    * bereich mit Hilfe von 'UIW_FreeString()' wieder freizugeben.
    */
   char *p;

   if( name == UiERRORP || name == UiKEEPP )
      return name;
   p = (char *)XtMalloc( 1 + strlen(name) );
   strcpy( p, name );
   return p;
}

/***************************** UIW_NewStringsCat ******************************/

char*
UIW_NewStringsCat( char *p1, char *p2 ) {
   /*
    * Diese Funktion liefert eine zusammengefuegte Kopie der Strings 'p1'
    * und 'p2' (ebenfalls innerhalb eines reservierten Speicherbereichs).
    * Wenn diese Kopie nicht mehr benoetigt wird, ist der Speicher-
    * bereich mit Hilfe von 'UIW_FreeString()' freizugeben.
    */
   char *p;

   if( p1 == UiERRORP ) {
      if( p2 == UiERRORP ) {
         return UiERRORP;
      } else if( p2 == UiKEEPP ) {
         return UiKEEPP;
      } else {
         return UIW_NewString( p2 );
      }
   } else if( p1 == UiKEEPP ) {
      if( p2 == UiERRORP ) {
         return UiKEEPP;
      } else if( p2 == UiKEEPP ) {
         return UiKEEPP;
      } else {
         return UIW_NewString( p2 );   /* evtl. erweitern um Xres-Abfrage ! */
      }
   } else {
      if( p2 == UiERRORP ) {
         return UIW_NewString( p1 );
      } else if( p2 == UiKEEPP ) {
          return UIW_NewString( p1 );
      } else {
         p = (char *)XtMalloc( 1 + strlen(p1) + strlen(p2) );
         strcpy( p, p1 );
         strcat( p, p2 );
         return p;
      }
   }
}

/***************************** UIW_FreeString *********************************/

void
UIW_FreeString( char *p ) {
   /*
    * Diese Funktion gibt den Speicherbereich frei, der zuvor durch
    * 'UIW_NewString()' oder 'UIW_NewStringsCat()' fuer 'p' reserviert
    * worden ist.
    */

   if( p == UiERRORP || p == UiKEEPP )
      return;
   XtFree( p );
}

/******************************************************************************/
/*                                                                            */
/*                        Widget Management Functions                         */
/*                                                                            */
/******************************************************************************/

/***************************** UIW_initialize *********************************/

void
UIW_initialize( void ) {
  /*
   * Diese Funktion initialisiert die Liste. Sie ist aufzurufen, bevor
   * das erste Widget hinzugefuegt wird.
   */

   wname_root = NULL;
   last_wname = wname_root;
   numberOfWidgets = 0;
}

/***************************** UIW_addWNAME ***********************************/

void
UIW_addWNAME( WNAME *wn ) {
   /*
    * Diese Funktion fuegt einen neuen Datensatz 'wn' an das Ende der
    * Liste an.
    */

   if( !numberOfWidgets )
      wname_root = wn;
   else
      last_wname->next = wn;

   wn->next = NULL;
   last_wname = wn;
   numberOfWidgets++;
}

/***************************** UIW_insertWNAME ********************************/

void
UIW_insertWNAME( WNAME *new_wn, WNAME *insertion_point ) {
   /*
    * Diese Funktion fuegt einen neuen Datensatz 'new_wn' vor dem
    * Datensatz 'insertion_point' in die Liste ein.
    */
   WNAME *tmp;

   if( !insertion_point )
      return;

   tmp = wname_root;
   if( tmp == insertion_point ) {

      /* ist das erste ! */
      new_wn->next = wname_root;
      wname_root = new_wn;

   } else {

      /* ist das 2. oder spaeter ! */
      for( ; tmp && (tmp->next != insertion_point); tmp = tmp->next );
      if( !tmp ) {
         puts("UIW_insertWNAME: Unexpected Error!");
         return;   /* Eigentlich ERROR, sollte aber nie eintreten (zumindest nicht, wenn die Existenz von 
                      'wn' vorher durch UIW_getWNAMEofName() abgesichert worden ist) */
      }
      new_wn->next = insertion_point;
      tmp->next = new_wn;
   }
   numberOfWidgets++;
}

/***************************** UIW_deleteWNAME ********************************/

void
UIW_deleteWNAME( WNAME *wn ) {
   /*
    * Diese Funktion loescht den Datensatz 'wn' aus der Liste.
    */
   WNAME *tmp;

   tmp = wname_root;
   if( tmp == wn ) {

      /* ist das erste ! */
      wname_root = wn->next;

   } else {

      /* ist das 2. oder spaeter ! */
      for( ; tmp && (tmp->next != wn); tmp = tmp->next );
      if( !tmp ) {
         puts("UIW_deleteWNAME: Unexpected Error!");
         return;   /* Eigentlich ERROR, sollte aber nie eintreten (zumindest nicht, wenn die Existenz von 
                      'wn' vorher durch UIW_getWNAMEofName() abgesichert worden ist) */
      }
      if( !(tmp->next)->next )

         /* ist der letzte ! */
         last_wname = tmp;

      tmp->next = wn->next;
   }
   UIW_FreeString( wn->name );
   UIW_FreeString( wn->fg_color );
   UIW_FreeString( wn->bg_color );
   UIW_FreeString( wn->font );
   UIW_FreeString( wn->bitmap );
   UIW_FreeString( wn->text );
   UIW_FreeString( wn->callbackMsg );
   switch( wn->class ) {
      case UiBITMAPBUTTON:
      case UiBITMAPLABEL:
         destroyPixmap( wn->w, wn->pixmap );
         break;
      case UiPULLDOWN_MENU:
      case UiMENU_ENTRY:
         XmStringFree( wn->text_res );
   }
   XtFree( (char*)wn );
   numberOfWidgets--;
   return;
}

/************************** UIW_setWNAMEtoBeginningOfList *********************/

void
UIW_setWNAMEtoBeginningOfList( WNAME *wn ) {
   /*
    * Diese Funktion verschiebt den Datensatz 'wn' an den Anfang der Liste.
    */
   WNAME *tmp;

   tmp = wname_root;
   if( tmp == wn ) {

      /* ist das erste ! */

   } else {

      /* ist das 2. oder spaeter ! */
      for( ; tmp && (tmp->next != wn); tmp = tmp->next );
      if( !tmp ) {
         puts("UIW_setWNAMEtoBeginningOfList: Unexpected Error!");
         return;   /* Eigentlich ERROR, sollte aber nie eintreten (zumindest nicht, wenn die Existenz von 
                      'wn' vorher durch UIW_getWNAMEofName() abgesichert worden ist) */
      }
      if( !(tmp->next)->next )

         /* ist der letzte ! */
         last_wname = tmp;

      tmp->next = wn->next;
      wn->next = wname_root;
      wname_root = wn;
   }
}

/***************************** UIW_setWNAMEtoEndOfList ************************/

void
UIW_setWNAMEtoEndOfList( WNAME *wn ) {
   /*
    * Diese Funktion verschiebt den Datensatz 'wn' an das Ende der Liste.
    */
   WNAME *tmp;

   tmp = wname_root;
   if( tmp == wn ) {

      /* ist das erste ! */
      wname_root = wn->next;
      wn->next = NULL;
      last_wname->next = wn;
      last_wname = wn;

   } else {

      /* ist das 2. oder spaeter ! */
      for( ; tmp && (tmp->next != wn); tmp = tmp->next );
      if( !tmp ) {
         puts("UIW_setWNAMEtoEndOfList: Unexpected Error!");
         return;   /* Eigentlich ERROR, sollte aber nie eintreten (zumindest nicht, wenn die Existenz von 
                      'wn' vorher durch UIW_getWNAMEofName() abgesichert worden ist) */
      }
      if( (tmp->next)->next )

         /* ist nicht der letzte ! */
         tmp->next = wn->next;
         wn->next = NULL;
         last_wname->next = wn;
         last_wname = wn;
         
   }
}

/***************************** UIW_restackWNAME *******************************/

void
UIW_restackWNAME( WNAME *wn, WNAME *insertion_point ) {
   /*
    * Diese Funktion verschiebt den Datensatz 'wn' vor den Datensatz
    * 'insertion_point'.
    */
   WNAME *tmp;

   if( !insertion_point )
      return;

   tmp = wname_root;
   if( tmp == wn ) {

      /* ist das erste ! */
      wname_root = wn->next;

   } else {

      /* ist das 2. oder spaeter ! */
      for( ; tmp && (tmp->next != wn); tmp = tmp->next );
      if( !tmp ) {
         puts("UIW_restackWNAME: Unexpected Error!");
         return;   /* Eigentlich ERROR, sollte aber nie eintreten (zumindest nicht, wenn die Existenz von 
                      'wn' vorher durch UIW_getWNAMEofName() abgesichert worden ist) */
      }
      if( !(tmp->next)->next )

         /* ist der letzte ! */
         last_wname = tmp;

      tmp->next = wn->next;
   }

   UIW_insertWNAME( wn, insertion_point );
}

/******************************************************************************/
/*                                                                            */
/*                        Information on actual list                          */
/*                                                                            */
/******************************************************************************/

/***************************** UIW_numberOfWidgets ****************************/

int
UIW_numberOfWidgets( void ) {
   /*
    * Diese Funktion liefert die Anzahl der momentan auf der Benutzer-
    * oberflaeche kreierten Widgets.
    */

   return numberOfWidgets;
}

/***************************** UIW_getRootWNAME *******************************/

WNAME*
UIW_getRootWNAME( void ) {
   /*
    * Diese Funktion liefert den Zeiger auf den ersten in der Liste
    * enthaltenen Datensatz.
    */

   return wname_root;
}

/***************************** UIW_getWNAMEofName *****************************/

WNAME*
UIW_getWNAMEofName( char *searchName ) {
   /*
    * Diese Funktion liefert den Zeiger auf den Datensatz des Widgets
    * mit Namen 'searchName'.
    */
   WNAME *actual_wname, *next_wname;

   next_wname = wname_root;
   while( actual_wname = next_wname ) {
      if( !strcmp( actual_wname->name, searchName ) )
         return actual_wname;
      next_wname = actual_wname->next;
   }
   return NULL;
}

/***************************** UIW_getWNAMEofWidget ***************************/

WNAME*
UIW_getWNAMEofWidget( Widget w ) {
   /*
    * Diese Funktion liefert den Zeiger auf den Datensatz des Widgets 'w'.
    */
   WNAME *actual_wname, *next_wname;

   next_wname = wname_root;
   while( actual_wname = next_wname ) {
      if( actual_wname->w == w )
         return actual_wname;
      next_wname = actual_wname->next;
   }
   return NULL;
}

/******************************************************************************/
/*                                                                            */
/*                        Useful for debugging ...                            */
/*                                                                            */
/******************************************************************************/

/***************************** printList **************************************/
#if 0
static void
printList( void ) {
   /*
    * Diese Funktion zeigt eine formatierte Tabelle der in der Liste
    * enthaltenen Widgets mit Namen und Klasse.
    */
   WNAME *actual_wname, *next_wname;
   char* class[] = { "not a class", "Separator", "Pushbutton", "Label",
      "BitmapButton", "BitmapLabel", "InputText", "OutputText",
      "PulldownMenu", "MenuEntry" };
   int i = 0;

   printf( "\nList of Widgets:\n\n" );
   next_wname = wname_root;
   printf( "  \tName\t\tID\t\tClass\n\n" );
   while( actual_wname = next_wname ) {
      printf( "%d.\t%s\t\t%d\t\t%s\n", ++i,
         actual_wname->name, actual_wname->w, class[actual_wname->class] );
      next_wname = actual_wname->next;
   }
   printf( "\nEnd of List\n" );
}
#endif
/************************************* EOF*************************************/

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   _help.prolog  Hans Groschwitz        19.07.95   !!
!!                 Karsten Vossberg                  !!
!!                 Klaus Senf                        !!
!!                 Frans Ceelen           18.04.96   !!
!!                                                   !!
!!   Functions to build the prolog-editor.           !!
!!   Full version only.                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

>PROLOG-EDITOR
#ifdef __english
.The Prolog-Editor
.=================
.
.Horn clauses(axioms) can be edited and created
.via the prolog editor.
.To select and create axiom sets you have to click
.on the field with the label "Axiom Set". To create
.a new axiom set, enter the new name in the field
.selection of the selection menue.
.
.The Edit menu:
.
.- Apply this Axiom Set...
.       The changed axiom set will be transfered
.       to the LISP world.
.- Delete this Axiom Set...
.       The axiom set will be removed from the
.       LISP world.
.- Undo Changes
.       The old axiom set definition is reloaded
.       from the knowledge base.
.- Link Axiom Sets to KBs...
.       This Linker popup enables linking of one
.       axiom set as the "fixed" clauses to the
.       selected KB, as well as any other "free"
.       axiom set including their order of prece-
.       dence. This is achieved by the buttons
.       "before" and "after". The popup grants
.       that only one axset per KB may be made
.       the fixed one, and that the fixed axset
.       cannot be selected to be a free one,
.       too.
.       The button "<< <<" removes a free axset
.       from the KB and relpaces it in the list
.       of remaining sets in the middle of the
.       dialog.
.       The "Undo" button reloads the unmodified
.       linklist of the chosen KB links again,
.       "Apply" transfers the modifications to
.       the knowledge base.
.
.Axiom set modifications, not yet updated in the KB,
.are displayed by an exclamation mark in the top right
.corner.
.
.Help on the syntax of babylon- and lisp-expressions 
.can be requested by placing the text cursor into a
.word in the text fields of the editors, followed by
.a keypress on the [Help]-Button [F1].
.       
#else
.Der Prolog-Editor
.=================
.
.Mit dem Prolog-Editor k\366nnen Prolog-Klauseln 
.und Klauselmengen editiert und erstellt werden.
.Zur Auswahl und zum Erstellen von Klauselmengen
.ist das Feld neben dem Label "Klauselmenge" anzu-
.klicken. 
.
.Das Bearbeiten-Men\374:
.
.- Diese Klauselmenge \374bernehmen...    
.       Die ge\344nderte Klauselmenge wird an die LISP-
.       Welt \374bergeben. 
.- Diese Klauselmenge l\366schen...
.       Dieser Klauselmenge wird LISP-seitig entfernt.
.- \304nderungen zur\374cknehmen... 
.       Es wird wieder die unbearbeitete Klauselmenge
.       geladen.
.- Klauselmenge den Wissensbasen zuordnen
.       Mit Hilfe der Klauselmengen-Zuordnung kann jeder
.       Wissensbasis eine "feste", sowie beliebig viele 
.       "freie" Klauselmengen zugeordnet werden.
.       Um eine Wissensbasis auszuw\344hlen, ist auf das
.       Feld neben dem Label "Wissensbasis" zu klicken.
.       Dieser Wissensbasis kann nun eine feste Klausel-
.       menge zugeordnet werden, indem ein Eintrag aus 
.       der linken  Auswahlliste angeklickt wird.
.       Eine feste Klauselmenge ist f\374r diese Wissensba-
.       sis nicht mehr als freie Menge zu vergeben.
.       Mit den beiden Kn\366pfen "vor" und "hinter" k\366nnen
.       die verbleibenden Klauselmengen jeweils vor oder
.       hinter die momentan markierte freie Klauselmenge
.       gesetzt werden.
.       Der darunter liegende "<< <<"-Knopf l\366st eine
.       freie Klauselmenge wieder von der Wissensbasis
.       und bringt sie somit wieder in das mittlere
.       Fenster.  
.       Der Knopf "Zur\374cknehmen" l\344dt wieder die 
.       unmodifizierte Zuordnung, "\374bernehmen" \374ber-
.       tragt die \304nderungen in die Wissensbasis.
.
.Eine noch nicht in die Wissensbasis \374bertragene \304nde-
.rung einer Zuordnung oder einer Klauselmenge wird durch
.ein Ausrufezeichen in der rechten oberen Ecke ange-
.zeigt. 
.
.Durch Anw\344hlen eines Wortes in den Textfeldern der
.Editoren mit dem Textcursor und anschlie\337endes Dr\374cken
.des [Hilfe]-Knopfes [F1] kann Hilfe zur Syntax der
.Babylon- und LISP-Ausdr\374cke abgerufen werden.
.
#endif

!!! EOF

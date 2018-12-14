!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   _xres.prolog  Hans Groschwitz        19.07.95   !!
!!                 Karsten Vossberg                  !!
!!                 Klaus Senf                        !!
!!                                                   !!
!!   Functions to build the prolog-editor.           !!
!!   Full version only.                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!*** general settings ***

Prolog.height:                               500
Prolog.width:                                700

Prolog.title:                                Prolog-Editor
Prolog.iconName:                             Prolog

!*** language settings ***

Prolog*Help.mnemonic:                        H
Prolog*on_menu.mnemonic:                     M
Prolog*on_syntax.mnemonic:                   S

#ifdef __english
Prolog*Help.labelString:                     Help
Prolog*on_menu.labelString:                  on Menu ...
Prolog*on_syntax.labelString:                on Syntax ...
!
Prolog*Bearbeiten.labelString:               Edit
Prolog*Bearbeiten.mnemonic:                  E
Prolog*Speichern.labelString:                Apply this Axiom Set ...
Prolog*Speichern.mnemonic:                   A
Prolog*Kl_loeschen.labelString:              Delete this Axiom Set ...
Prolog*Kl_loeschen.mnemonic:                 D
Prolog*ReChange.labelString:                 Undo Changes
Prolog*ReChange.mnemonic:                    U
Prolog*Zuordnen.labelString:                 Link Axiom sets to KBs ...
Prolog*Zuordnen.mnemonic:                    L
!
Prolog*Klauselmenge.labelString:             Axiom Set:
Prolog*name.labelString:                     (none)
Prolog*Klauseln.labelString:                 Axioms:
!
Prolog*DelWarningW.dialogTitle:              Warning
Prolog*DelWarningW*messageString:            \
 \n\
 Cannot delete this Axiom Set! \n\
 \n\
 It is yet linked with these knowledge bases: \n\
 \n
!
Prolog*kl_selection_form.dialogTitle:        Choose an Axiom Set
Prolog*kl_selection.listLabelString:         Known axiom sets
!
Prolog*kl_selection_msg.dialogTitle:         Incorrect Input
Prolog*kl_selection_msg*messageString:       Please choose an axiom set!

#else /* deutsch */
Prolog*Help.labelString:                     Hilfe
Prolog*on_menu.labelString:                  zum Men\374 ...
Prolog*on_syntax.labelString:                zur Syntax ...
!
Prolog*Bearbeiten.labelString:               Bearbeiten
Prolog*Bearbeiten.mnemonic:                  B
Prolog*Speichern.labelString:                Diese Klauselmenge \374bernehmen ...
Prolog*Speichern.mnemonic:                   K
Prolog*Kl_loeschen.labelString:              Diese Klauselmenge l\366schen ...
Prolog*Kl_loeschen.mnemonic:                 l
Prolog*ReChange.labelString:                 \304nderungen zur\374cknehmen
Prolog*ReChange.mnemonic:                    z
Prolog*Zuordnen.labelString:                 Klauselmengen den Wissensbasen zuordnen ...
Prolog*Zuordnen.mnemonic:                    W
!
Prolog*Klauselmenge.labelString:             Klauselmenge:
Prolog*name.labelString:                     (unbestimmt)
Prolog*Klauseln.labelString:                 Klauseln:
!
Prolog*DelWarningW.dialogTitle:              Warnung
Prolog*DelWarningW*messageString:            \
 \n\
 L\366schen nicht m\366glich! \n\
 \n\
 Axiom ist noch mit diesen Wissensbasen verkn\374pft: \n\
 \n
!
Prolog*kl_selection_form.dialogTitle:        Auswahl der Klauselmenge
Prolog*kl_selection.listLabelString:         Bekannte Klauselmengen
Prolog*kl_selection.cancelLabelString:       Abbrechen
!
Prolog*kl_selection_msg.dialogTitle:         Falsche Eingabe
Prolog*kl_selection_msg*messageString:       Bitte w\344hlen Sie eine Klauselmenge aus!

#endif /* english */


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  LinkAxiomSetsToKB-Editor
!!!
Prolog*assign.fontList:                      *-courier-bold-r-*--24-*
Prolog*assign.marginTop:                     20
Prolog*assign.marginBottom:                  10

Prolog*zuordnung*back.fontList:              *-courier-bold-r-*--24-*
Prolog*zuordnung*back.topOffset:             20

#ifdef __english
Prolog*zuordnung_popup.title:                AxiomSet-to-KB Linker
Prolog*Wissensbasis.labelString:             Knowledge Base:
Prolog*zu_name.labelString:                  (none)
Prolog*feste_Klausel.labelString:            Fixed Axiom Set:
Prolog*verbl_Klausel.labelString:            Remaining Sets:
Prolog*freie_Klausel.labelString:            Free Axiom Sets:
Prolog*before.labelString:                   before
Prolog*after.labelString:                    after
Prolog*zuordnung.okLabelString:              Close
Prolog*zuordnung.cancelLabelString:          Undo
Prolog*zuordnung.applyLabelString:           Apply
!
Prolog*kb_selection_form.dialogTitle:        Choose a Knowledge Base
Prolog*kb_selection.listLabelString:         Known knowledge bases
!
Prolog*kb_selection_msg.dialogTitle:         Incorrect Input
Prolog*kb_selection_msg*messageString:       Please choose a knowledge base!

#else /* deutsch */
Prolog*zuordnung_popup.title:                Klauselmengen-Zuordnung
Prolog*Wissensbasis.labelString:             Wissensbasis:
Prolog*zu_name.labelString:                  (unbestimmt)
Prolog*feste_Klausel.labelString:            Feste Klauselmenge:
Prolog*verbl_Klausel.labelString:            Verbleibende Mengen:
Prolog*freie_Klausel.labelString:            Freie Klauselmengen:
Prolog*before.labelString:                   vor
Prolog*after.labelString:                    hinter
Prolog*zuordnung.okLabelString:              Schlie\337en
Prolog*zuordnung.cancelLabelString:          Zur\374cknehmen
Prolog*zuordnung.applyLabelString:           \334bernehmen
!
Prolog*kb_selection_form.dialogTitle:        Auswahl der Wissensbasis
Prolog*kb_selection.listLabelString:         Bekannte Wissensbasen
Prolog*kb_selection.cancelLabelString:       Abbrechen
!
Prolog*kb_selection_msg.dialogTitle:         Falsche Eingabe
Prolog*kb_selection_msg*messageString:       Bitte w\344hlen Sie eine Wissensbasis aus!

#endif /* english */


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  ---> sysd.c/h/lisp
!!!
!!! should be part of the RUNTIME version, too ?!?
!!! before overwrite-warnig displaying:
!!!   check, whether old and new differ !!!
!!!
#ifdef __english
SystemDialogs*Overwrite*messageString:       \
 Overwrite this Axiom-Set?\n\n
SystemDialogs*Overwrite.okLabelString:       Yes
SystemDialogs*Overwrite.cancelLabelString:   No

#else /* deutsch */
SystemDialogs*Overwrite*messageString:       \
 Soll diese Klauselmenge \374berschrieben werden?\n\n
SystemDialogs*Overwrite.okLabelString:       Ja
SystemDialogs*Overwrite.cancelLabelString:   Nein

#endif /* english */


!*** color settings ***

#ifdef COLOR

Prolog*background:                           azure
Prolog*DelWarningW*background:               red
Prolog*kl_selection_msg*background:          red
Prolog*kb_selection_msg*background:          red
SystemDialogs*Overwrite*background:          red

#else   !!! MONO: here non-default settings only, please!

#endif


!!! EOF

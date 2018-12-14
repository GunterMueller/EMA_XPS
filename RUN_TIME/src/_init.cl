;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                     ;;
;;    ema-xps-init.cl                      25.10.94    ;;
;;                                                     ;;
;;                                                     ;;
;;    If it exists, this file is loaded at the end     ;;
;;    of the initialization sequence of EMA-XPS.       ;;
;;    The user may preset EMA-XPS as preferred.        ;;
;;                                                     ;;
;;    This file must be located in                     ;;
;;       $XAPPLRESDIR/ema-xps-init.cl                  ;;
;;    If $XAPPLRESDIR does not exist, it defaults to   ;;
;;    the value of $HOME.                              ;;
;;                                                     ;;
;;                                                     ;;
;;    Falls diese Datei existiert, wird diese Datei    ;;
;;    zum Abschluss der Initialisierungssequenz von    ;;
;;    EMA-XPS geladen.                                 ;;
;;    Hier kann der Benutzer EMA-XPS seinen Wuen-      ;;
;;    schen entsprechend anpassen.                     ;;
;;                                                     ;;
;;    Diese Datei wird gesucht unter dem Pfad          ;;
;;       $XAPPLRESDIR/ema-xps-init.cl                  ;;
;;    Falls $XAPPLRESDIR nicht existiert, wird der     ;;
;;    Wert der $PATH-Variablen verwendet.              ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

; -------------------------------------------

(emaxps-load "~/ema-xps-init.cl")   ;; $HOME

(emaxps-load "./ema-xps-init.cl")   ;; current / aktuelles Directory

; -------------------------------------------

;; eof

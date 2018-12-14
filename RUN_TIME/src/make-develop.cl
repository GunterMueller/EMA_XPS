;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    make-develop.cl     Hans Groschwitz    08.10.94  ;;
;;                                                     ;;
;;    Used only in DEVELOPer mode to do patching of    ;;
;;    unmodified ASCII Babylon World.                  ;;
;;    EMA-XPS has been started with -develop           ;;
;;                                                     ;;
;;    This is used by ema-xps (complete version)       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------

(defconstant *emaxps-features* '(
 :emaxps-full-version
 :emaxps-runtime-version
 :emaxps-babylon3-emulation
))

;; -------------------------------------------------------

;; This file does the installation of ema-xps add ons
;; to the Babylon 2.3 Image, depending on the feature-list
;; as preset above.
;;
(load "./make-ema-xps.cl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      THIS MUST BE THE LAST LISP-INSTRUCTION         ;;
;;                   IN THIS FILE                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; necessary for IPC initialization!
(emaxps-end-of-boot-phase)

;; eof

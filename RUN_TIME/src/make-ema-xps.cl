;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    make-ema-xps.cl     Hans Groschwitz    08.10.94  ;;
;;                                                     ;;
;;    Used for both versions (see xinit.c) to do       ;;
;;    patching of the unmodified ASCII Babylon World.  ;;
;;    EMA-XPS has been started with -develop           ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

(defconstant *emaxps-version* "EMA-XPS v2.1.6")

;; -------------------------------------------------------
;; This first block of files will work, even if this
;; LISP-Image does not include the Babylon 2.3 code !!!
;; -------------------------------------------------------

;; limits line-length and print-style
;;
(setf *print-pretty* T
      *print-case :upcase)

;; -------------------------------------------------------

;; Now include LISP implementation dependencies...
;; This file may not be compiled with Allegro CL 4.1 :((
;;
(load "impl-dep.cl")

;; -------------------------------------------------------

;; This file is the very first one to be loaded, hence
;; it defines access to the compiler!
;;
(load "ccload.cl")

;; -------------------------------------------------------

;; These two files must be loaded first! DO NOT CHANGE!
;; They initialize the handling of logical channels!
;;
(emaxps-ccload "dispatch")
(emaxps-ccload "channels")

;; -------------------------------------------------------

;; This file is required to enable reading from
;; LISP's stdin!
;;
(emaxps-ccload "debugger")

;; -------------------------------------------------------

;; These two files add the graphic user interface!
;; They do not require anything from Babylon 2.3!
;; (they work with an uninitialized LISP-world, too!)
;;
(emaxps-ccload "uid")
(emaxps-ccload "ui")

;; -------------------------------------------------------

;; This file adds the Dialogs of the Main-Menu of EMA-XPS
;;
(emaxps-ccload "sysd")

;; -------------------------------------------------------
;; The remaining files afford the presence of Babylon 2.3
;; power within this LISP-Image !!!
;; -------------------------------------------------------

;; This file enables the inference engine to communicate
;; with the user at session time using the debugger PAD
;;
(emaxps-ccload "inf-eng")

;; This file loads the graphic enhancements of the 
;; babylon explaination-facility
;;
(emaxps-ccload "explain")

;; This file loads KB management into Babylon 2.3
;;
(emaxps-ccload "main")

;; -------------------------------------------------------
;; KB developer's version only:
;; Tracer, Editors, KB compiler ...
;; -------------------------------------------------------

(when (member :emaxps-full-version *emaxps-features*)

 ;; -----------------------------------------------------
 ;; Trace KBs under construction
 ;;
 (emaxps-ccload "trace")

 ;; -----------------------------------------------------
 ;; Specialized graphic editors for the knowledge engineer
 ;;
 (emaxps-ccload "frame")
 (emaxps-ccload "instance")
 (emaxps-ccload "behavior")
 (emaxps-ccload "rule")
 (emaxps-ccload "prolog")
 (emaxps-ccload "constraint")
 (emaxps-ccload "csnet")
 (emaxps-ccload "restriction")
 (emaxps-ccload "task")
 (emaxps-ccload "misc")

 ;; -----------------------------------------------------
 ;; Save KBs in compiled mode (makes execution
 ;; at runtime faster)
 ;;
 (emaxps-ccload "compile")

);if

;; -------------------------------------------------------
;; For babylon 3.1-beta (VW-GEDAS) emulation mode only:
;;        [Rest in Peace]    :-[   :-[   :-[
;;
;; This emulation offers the complete power of the well
;; documented LISP-functions/macros as described in the
;; babylon-3 reference manual, except:
;; * Backward chaining                  (yet under construction)
;; * the B3 explanation facility        (too hard to be emulated)
;; * the B3 editor generation facility  (emu would be possible)
;; -------------------------------------------------------

(when (member :emaxps-babylon3-emulation *emaxps-features*)

 ;; -----------------------------------------------------
 ;; This must be the first file her, it creates the package
 ;; :BUL and adds the complete Babylon LISP functionality
 ;; from chapter 9 B3-REF (:BUL should replace :LISP)
 ;;
 (emaxps-ccload "b3-blisp")

 ;; -----------------------------------------------------
 ;; MUST be in front of b3-rules (see tasks)
 ;; Creates the :BABYLON kernel package, if not yet done (see B23!).
 ;; Includes B3-task support and misc general KB features
 ;; e.g. (this-kb set-protocol-level) from chapter 7
 ;;
 (emaxps-ccload "b3-misc")

 ;; -----------------------------------------------------
 ;; Adds B3-specific add-ons to the UI
 ;; includes the complete chapters 4 and 6
 ;;
 (emaxps-ccload "b3-ui")

 ;; -----------------------------------------------------
 ;; B3 SET operations. Includes the corresponding parts
 ;; of chapter 7. MUST be in front of b3-oop!
 ;;
 (emaxps-ccload "b3-sets")

 ;; -----------------------------------------------------
 ;; B3 specific access to OOP. Includes most of chapter 3 
 ;; and parts of chapter 7. MUST be in front of b3-bql!
 ;;
 (emaxps-ccload "b3-oop")

 ;; -----------------------------------------------------
 ;; Babylon Query Language
 ;; Includes the complete chapter 2.
 ;;
 (emaxps-ccload "b3-bql")
(emaxps-ccload "b3-bql-or")

 ;; -----------------------------------------------------
 ;; Rules with BQL
 ;; Includes chapters 3 and 7 partially.
 ;;
 (emaxps-ccload "b3-rules")

 ;; -----------------------------------------------------
 ;; Constraints with BQL
 ;; Includes chapters 3 and 7 partially.
 ;;
 (emaxps-ccload "b3-cnstr")

 ;; -----------------------------------------------------
 ;; Error-Exits for the non emulated B3 Explanation facility.
 ;; Includes the complete chapter 5.
 ;;
 (emaxps-ccload "b3-expln")

 ;; -----------------------------------------------------
 ;; Error-Exits for the non emulated B3 KB-Editor Generation facility.
 ;; Includes the complete chapter 8.
 ;;
 (emaxps-ccload "b3-editr")

);if

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      THIS MUST BE THE LAST LISP-INSTRUCTION         ;;
;;                   IN THIS FILE                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now all def$methods and so on MUST have been made!
;; So its the time to create an instance of the inference
;; engine:
;;
(emaxps-ccload "kb-cfg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     THESE MUST BE THE LAST LISP-INSTRUCTIONS        ;;
;;                   IN THIS FILE                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; and this will create the new LISP image (including the
;; Babylon-2.3 inference engine and the EMA-XPS graphics
;; enhancements)
;; Image's name (emaxps.image) to be modified by make...
;;
(format t "~%~%****** building LISP image ******~%")
(emaxps-make-image)

;; und tschuess...
;;
(exit)

;; eof

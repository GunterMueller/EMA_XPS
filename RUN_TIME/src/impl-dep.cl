;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     impl-dep.cl       Hans Groschwitz      01.12.94      ;;
;;                                                          ;;
;;     keeps implentation dependencies CLISP vs. AllegroCL  ;;
;;     EXCL is the switch for Allegro CL 4.1                ;;
;;                                                          ;;
;;     To the favour of Allegro flaws this file is NOT      ;;
;;     compiled!                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#-(or :EXCL :CLISP)(progn
   (in-package "USER")
   (format t "~%~%~%******** Sorry, but you are using an~%~
              UNSUPPORTED implementation of CommonLISP !~%")
   (exit)
);progn


; ----------------------------------------------


(in-package "BABYLON")


; ----------------------------------------------
; create a readtable for our demands
; ----------------------------------------------

;; This became necessary with Allegro CL, which differs
;; from CLISP in dumping a lisp image... 
;; See channels.cl
;;
(defvar *emaxps-readtable* (copy-readtable NIL))

; ----------------------------------------------
; create two packages needed in advance...
; ----------------------------------------------

;;; With Allegro CL 4.1 this doesn't work in a compiled file
;;; like b3-blisp.cl ...  so its addiionally done here in
;;; interpreted mode BAEAEHH
;;
(unless (find-package "BUL")
 (make-package "BUL"))

;;; CLISP seems to have problems, too, when it tries
;;; to enter a package freshly generated within the same
;;; progn ... ?!?
;;
(unless (find-package "EXCL")
 (make-package "EXCL"))

; ----------------------------------------------
; This is the portable error handler...
; ----------------------------------------------

(defun emaxps-error (fmt &rest args)
 "The Allegro CL implementation of error seems to create 1000s of 
 error conditions which disable calling invoke-debugger ?!?"

 (let (str)
  (setf str (apply #'funcall 'format (append (list nil fmt) args)))
  (setf str (concatenate 'string "EMA-XPS: " str))

#+:CLISP (error str)

#+:EXCL (progn
   (setf str (concatenate 'string "~%~%Error: " str))
   (format *error-output* str)
   (emaxps-invoke-debugger)  ;;; this makes popup the debugger, even
   (error 'simple-error)     ;;; if (invoke-debugger) is'nt run!!
  );progn EXCL

 );let
)

; ----------------------------------------------
; Save the image to a new file...
; ----------------------------------------------

(defun emaxps-make-image ()
 "Save the image of EMA-XPS to a new file.
 This function is implementation dependend and is similar
 to the Babylon-2.3 function make-babylon-image in sun-make.cl
 over there."

 ;;; Allegro CL 4.1 ...
 #+:EXCL(progn
  (excl:gc t)
  (excl:dumplisp :name "emaxps.image" :checkpoint nil
   :restart-function #'(lambda ()

     ;; This is necessary to grant the correct setting
     ;; of the UNdocumented variable *skip-evalhook*    GRRR!
     ;;
     (tpl:setq-default tpl::*eval* 'eval-allright)

     ;; without this, the image would always start from the USER
     ;; package, even if it was saved from within the BAAYLON package
     ;;
     (tpl:setq-default *package* (find-package "BABYLON"))

   );lambda
 ))

 ;;; CLISP ...
 #+:CLISP
  (ext:saveinitmem "emaxps.image" :quiet t)   ;;  :init-function not needed
)

; ----------------------------------------------
; Allegro CL 4.1 proudly presents... its FLAWs     BOOAH, EYI!
; ----------------------------------------------

#+:EXCL(progn

 ;;; stops ERRORs...
 ;;
 (setf excl:*cltl1-in-package-compatibility-p* T)
 (setf excl:*enable-package-locked-errors* NIL)

 ;;; stops WARNINGs...
 ;;
 (setf comp:*cltl1-compile-file-toplevel-compatibility-p* T)
 (setf excl:*redefinition-warnings* NIL)

 ;;; A nice time spending trick of Allegro CL 4.1 is to hide the
 ;;; *evalhook* variable!
 ;; It is located in the CLTL1 package, which is accessable from within the
 ;; USER package, but not from new packages like the BABYLON package.
 ;; this should be added to the KBNAME packages as soon as package 
 ;; handling will be more babylon3 like!!!
 ;;
 (use-package "CLTL1")       ;;; we are in the "BABYLON" package, now!

 ;;; Allegro CL 4.1 is pleased to offer a little undocumented dingy,
 ;; the flag excl::*skip-evalhook* which initially is NIL, but
 ;; when EMA-XPS is waiting for events, it is (the h*ll) T
 ;; Here is the workaround:     (and thanx for that *fine* docu !!!)
 ;;
 (defun eval-allright (form)             ;;; see excl-after-image-load
  (let ((excl::*skip-evalhook* NIL))     ;;; and emaxps-make-image !!!
   (eval form)))

);progn EXCL

; ----------------------------------------------
; Exiting should be easy typable, an emaxps-exit
; function is avoided therefore...
; ----------------------------------------------

#+:EXCL
(shadowing-import '(excl::exit))

; #+:CLISP
; ;(shadowing-import '(lisp::exit lisp::bye lisp::quit))
; (shadowing-import '(ext::exit ext::bye ext::quit))

;; hg20080707 added:
;; The language gurus think, the *evalhook* feature is sort
;; of ugly. Hence this feature was exported by clisp developers
;; into ext package. CLISP documentation is at
;; "http://clisp.cons.org/impnotes/imppack.html#ext-pac"
;; See: (describe 'ext::*evalhook*) for the variabe and
;;  the (describe 'ext::evalhook) builtin function
; #+:CLISP(import 'ext::*evalhook* 'ext::evalhook)

;; "http://clisp.cons.org/impnotes/imppack.html#ext-pac"
;; says, this is enough to add all implementation specific extensions
;; to my package (here: "BABYLON")
#+:CLISP
;(use-package "EXT")   ;; dann kommt clos wieder rein und die doppelte Methode COMPUTE-SLOTS (BABYLON, CLOS)
(progn
;(use-package "CUSTOM")
(shadowing-import '(ext::exit ext::*evalhook* ext::evalhook))
)

; ----------------------------------------------
; CLISP does not have an "EXCL" package ...
; ----------------------------------------------

;;; babylon3 was intentionally developed with AllegroCL
;;; FVA KBs use only the functions run-shell-command and
;;; chdir. EMA-XPS uses emaxps-system and emaxps-chdir instead,
;;; which are linked to the EXCL function in case of Allegro
;;; and simulated in case of CLISP (see main.cl)

#+:CLISP(load "./excl-emu.cl")

; -------------------------------------------

;; eof

#|*****************************************************************
*     b3-misc.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulates misc B3 routines for KB support and B3 tasks         *
*   Generates the package :BABYLON (if not yet done (see B23!)    *
*   This package holds the babylon kernel!                        *
*                                                                 *
*   B3 Reference Manual, Chapters 3 and 7 (partially)             *
*                                                                 *
*****************************************************************|#


#| ----------------------------------------------------------------

;; Symbole, die nur fuer den internen Gebrauch bestimmt sind,
;; mit % anfangen lassen, statt bqli- !!!

;; In den Doku-String immer die wichtigsten Hinweise aus dem
;; B3-Ref.Buch abpinnen !!!

;; Hinweis, wenn eine gleichlautende Funktion bereits von 
;; EMA-XPS bereitgestellt wird !!!

;; Existenz checken !!! -----------

;; :BABYLON-Package enthaelt den Rest ???
;; hier koennte bei B23 der gesamte KERNEL sein!
;; Test: mit use-package :BUL, unuse-package :LISP
;; welche Kommandos er NICHT mehr kennt!
;; Entscheiden, ob die dann hier (illegalerweise) mit importiert
;; werden sollen oder ob im Source an betroffener Stelle ueberall
;; ein LISP:: ergaenzt werden soll ?!?
;; Ein Ergaenzen in der :EMA-XPS package oder der :BABYLON package
;; (oder in der :UI package) bringt nix/doch_was ???


Packages:

use-package kann nur die exportierten symbole ohne qualifier ansprechen!
==> in jeder Package nur DIE symbole exportieren, die man verwenden
    soll, keine hilfsroutinen!
    Macht ausserdem den apply schneller...

---------------------------------------------------------------- |#







(in-package "BABYLON")

;; -------------------------------------------------------
 
(defun b3mi-evalhook (form &optional env)
 "CLtL1 p. 323
 Converts the defun MAKE-KB-INTERFACE of B3 KBs into a deftask!

 INFO: a evalhook is estblished - when not removed from *evalhook* -
 as long as no ERROR/BREAK occures! Even recoursive entries into
 LOAD do not chenge this!"

 (when (and form (listp form)
            (equal (first form) 'defun)
            (equal (second form) 'make-kb-interface))
  (format t "~%INFO: converting function MAKE-KB-INTERFACE into a task!")
  (setf form (cons 'deftask (rest form)))
 );when

 (evalhook form 'b3mi-evalhook nil env)
)

;;---------------------MISCELANEOUS---------------------

;; --- include-lisp-file ---

(defun include-lisp-file (filename extension)
 "EXAMPLE: (include-lisp-file ``dantex-if'' ``lisp'')"

 (let (kb-path dir load-path)
  (setf kb-path *emaxps-current-kb-load-path*)
  (setf dir (directory-namestring (pathname kb-path)))
  (setf load-path (concatenate 'string dir filename "." extension))
  (load load-path)
 );let
)

;;  Kap. 7 -->   (partially)

;; --- this-kb ---

(defun this-kb ()
 "returns the name (symbol) of the current KB or
  signals an error, if none is current!"

 (let ((ret (emaxps-current-kb-name)))
  (if ret
   ret        ;;return-value
  ;else
   (emaxps-error "this-kb: none KB current")
  );if
 );let
)

;; --- set-protocol-level ---

(defun set-protocol-level (val &optional (kb (this-kb)) )
 "UNDER CONSTRUCTION!
  could be combined with the EMA-XPS tracer later..."

 (declare (ignore kb))
 (if (not (or (equal val :maximum)
              (equal val :minimum)
              (equal val :none)
          )
     )
  (emaxps-error "set-protocol-level wrong level. (:maximum :minimum :none)")
 );if
 (format t "WARNING: set-protocol-level is not implemented yet")
 val
)

;; Kap. 3 -->   (partially)

;; --- define-knowledge-base ---

(defvar b3i-full-interface NIL)  ;;valid only while loading a single KB!
 
(defmacro define-knowledge-base (kb-name &key configuration interface
          documentation explanation)
 "Emulates the babylon3 KB configuration.
 Compile time problem. The symbols are generated at compile time in
 package :BABYLON and not at runtime in :KB-NAME (e.g.) !!!
 equalp for an case-insensitive string compare !"

 (declare (ignore documentation explanation))
 `(let ((cfg  (string ',configuration))
        (itf  (string ',interface))
        (kb   (string ',kb-name)))
   (unless (equalp cfg "default-configuration")
    (emaxps-error "define-knowledge-base: unsupported configuration"))

   ;; decide whether to use ui-create-simple-session-screen or not!
   (setf b3i-full-interface (equalp itf "babylon-kb-interface"))

   ;;;; temporary...
   (unless b3i-full-interface
    (emaxps-error "define-knowledge-base: unsupported interface"))

   ;;; should be preset...
   (setf *babylon-standard-output* "babylon-standard-output"
         *babylon-standard-input*  "babylon-standard-input")

   ;;;; see main.cl:  this macro performs an in-package BABYLON !
   ;;
   (emaxps-make-kb ,kb-name)

   ;;; to install has-part ...          unelegant (package-problem) !!!
   ;;
   (eval (read-from-string
     "(BABYLON::defframe b3i-has-part (slots has-part))"  ;; see b3-oop for active value!
   ))
   (eval (read-from-string
     "(BABYLON::defbehavior (b3i-has-part :b3i-has-part-reader)
                            (oldval actval annot slot)
       (BABYLON::bqli-read-has-part self oldval)
      )
     "
   ))

   ;;; to install post-processing of B3 rules and constraints ...
   ;;
   (emaxps-send-task :append-after-load
      '(BABYLON::b3b-post-process-behaviors))
   (emaxps-send-task :append-after-load
      '(BABYLON::b3r-post-process-rulesets))
   (emaxps-send-task :append-after-load
      '(BABYLON::b3c-post-process-constraints))

   (BABYLON::b3b-initialize-behaviors)
   (BABYLON::b3r-initialize-rulesets)
   (BABYLON::b3c-initialize-constraints)

   ;;; install an evalhook to convert MAKE-KB-INTERFACE into a TASK!
   ;;; It is removed after leaving this LOAD but it should be kept in 
   ;;; LOADs subsequent to this one!!!
   ;;
   (setf *evalhook* 'BABYLON::b3mi-evalhook)

   ;;; This *IS* necessary, since Allegro CL compiler optimizes
   ;;; in tis default settings that much, that without a tricky call
   ;;; to this symbol the above setting is discarded ?!
   ;;
   (format nil "~S" *evalhook*)
   *evalhook*   ;; the orig. retval

  );let
)

;; --- babylon-package ---

(defun babylon-package (pname &rest args)
 "Emulates the babylon3 package handling.
 INFO: see b3-blisp for details on defconstant management!!!"

 (unless (stringp pname)
  (emaxps-error "babylon-package: package-name ~S should be a string" pname))

 ;;; with Allegro CL 4.1 in-package won't work, it produces only a package
 ;;; named "PNAME" ..
 ;;
 ;; (in-package pname)     ;; the define-KB macros will work, but this is a function!
 ;;
 (unless (find-package pname)
  (make-package pname))
 (unless (find-package pname)
  (emaxps-error "babylon-package: failed creating package ~S" pname))
 ;;
 ;;; ... and in spite of in-package...
 ;;
 (setf *package* (find-package pname))

 ;;; this is to be improved... The LISP (COMMON-LISP) Package
 ;;; should be replaced completely by BUL !!!
 ;;
 ;; HINT: CLISP offers a COMMON-LISP *and* a LISP package,
 ;;       ALLEGRO nicknames its COMMON-LISP package: LISP
 ;;
 (unuse-package "LISP")
 (LISP::use-package '("BABYLON" "FMCS" "BUL" "COMMON-LISP"))

 (when args
  (unless (equal (first args) :use)
   (emaxps-error "babylon-package: missing :USE"))
  (setf args (rest args))
  (unless args
   (emaxps-error "babylon-package: no package-names following :USE"))
  (mapcar #'(lambda (x)
             (if (stringp x)
              (use-package x)
             ;else
              (emaxps-error "babylon-package: package-name ~S should be a string" x)
             );if
            );lambda
          args
  );mapcar
 );when

 ;;; it would be made by the define-KB macro (see above), too !!
 ;;
 (setf *package* (find-package "BABYLON"))

 T   ;; return-value
)

;; --- set-kb-starttask ---

(defun set-kb-starttask (tname)
 "This function is the last directive in a babylon3 KB.
 Hence the global var b3i-full-interface (valid only while loading)
 is interpreted here.
 It generates the EMA-XPS task MAIN."

 ;; Compile time problem. The symbols are generated at compiletime in
 ;; package :BABYLON and not at runtime in package :KB-NAME (e.g.) !!!
 (let ((main (read-from-string "main"))
      (make-kb-interface (read-from-string "make-kb-interface")))
  (eval
   (list 'deftask main nil
    (if b3i-full-interface
     ;; interface=NIL
     ;; multiple graphical interfaces aren't supported yet!
     (list 'dotask (list 'quote make-kb-interface) NIL)
    ;else
     ;; here a simple session screen should be installed
     ;; including the redirection of stdio to its PAD!
     ;(do-anything-appropriate)
    );if
    (list 'dotask (list 'quote tname))     ;; start-task's name!
   );list
  );eval
 );let
)

;; --- define-task ---

(defmacro define-task (tname targs &rest do-it)
 "This emulates the babylon3 call. The explanation facility is not
 supported, hence description and subtasks are ignored.
 Compile time problem. The symbol 'actions is generated at compile
 time in package :BABYLON and not at runtime in :KB-NAME (e.g.) !!!"

 `(define-task-eval ',tname ',targs ',do-it)
)

(defun define-task-eval (tname targs do-it)
 "The evaluator ..."

 (let (pos props docu)
  (setf pos (position (read-from-string "actions") do-it))
  (unless pos
   (emaxps-error "define-task: keyword ACTIONS missing"))
  (setf props (reverse (nthcdr (- (length do-it) pos) (reverse do-it))))
  (setf docu (getf props :documentation))
  (remf props :documentation)
  ;; not supported:
  (remf props :subtasks)
  (remf props :explanation)
  (when props
   (emaxps-error "define-task: illegal properties ~S" props))
  (unless (stringp docu)
   (setf docu ""))
  (setf do-it (nthcdr (1+ pos) do-it))

  (when (equal (first do-it) :without-constraint-check)
;   (format t
;    "~%;INFO: while defining task ~S:~%;  found :without-constraint-check"
;    tname)
   (setf do-it
    (list
     (cons (read-from-string "do-without-constraint-check")
;      (cons
;       (list 'print (format nil
;        ";;; running task ~S :without-constraint-check"
;        tname))
       (rest do-it)
;      );cons
     );cons
    );list
   );setf
  );when wocc

  (eval
   (append
    (list 'deftask tname targs docu)
    do-it)
  );eval
 );let
)

;; --- start ---

(defvar b3i-current-task-name NIL)

(defvar b3i-task-argvals NIL)

(defmacro start (tname &rest targs)
 "Emulates the task activation of babylon3"

 `(let ((b3i-current-task-name ,tname)
        (b3i-task-argvals (list ,@targs)))
   (catch b3i-current-task-name
    (dotask b3i-current-task-name ,@targs)
   );catch
  );let
)

;; --- stop-this-task ---

(defun stop-this-task (retval retcode)
 "Emulates the return-from-task facility of babylon3.
 The retcode is ignored, because EMA-XPS handles task
 control in a different way."

 (declare (ignore retcode))
 (throw b3i-current-task-name retval)
)





;;-----------------------EXPORTS------------------------

(export '(this-kb set-protocol-level define-task
          babylon-package define-knowledge-base
          start stop-this-task set-kb-starttask
          include-lisp-file
))




;;; eof

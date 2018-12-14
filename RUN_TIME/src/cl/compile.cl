;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    compile.cl      Hans Groschwitz      08.12.94    ;;
;;                                                     ;;
;;    Save a knowledge base in a compiled format to    ;;
;;    increase the speed at runtime.                   ;;
;;    Full version only.                               ;;
;;                                                     ;;
;;    Bugs:                                            ;;
;;      UNDER CONSTRUCTION!                            ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------
#|
concepts:
Full version: if a KB is marked READ-ONLY
the Editors should not be able to choose it!
==> compiled KBs cannot be edited!
==> see: task-bodies / behavior-bodies / rule-if's / rule-then's
         misc's / constraint-functions
    would be compiled closures!
    OR: while trying to load a compiled closure into an editor
        it should popup a FAILURE !!!
|#
;; -------------------------------------------------------

(defun emaxps-compile-file (in-file out-file)
 "Compile the KB constructs (already stored in in-file)
 to increase speed at run-time. Save the result in out-file
 in a way LOAD can read. LOAD-KB should work equal to uncompiled
 KB files!

 This definition replaces the dummy witin main.cl ...
 
 Currently a DUMMY !!!"

 (setf  in-file (pathname  in-file))
 (setf out-file (pathname out-file))

 (format t "~%;;; COMPILE-KB: ~S --> ~S" in-file out-file)

; (let ((*error-output* nil))

  ;; without compiler messages  -- HACK!
  (compile-file in-file :output-file out-file)

; );let

 ;;; remove temporary files produced by the compiler
 ;;; LISP implementation dependent!

#+:CLISP
 (let (lib-file)
  (setf lib-file (pathname (concatenate 'string
     (directory-namestring out-file)
     (pathname-name out-file) ".lib"
  )));setf
  (format t "~%;;; COMPILE-KB: removing ~S" lib-file)
  ;; to clean-up .LIB file after compiling has succeeded!

  (print (emaxps-remove-file lib-file))
 );let +:CLISP

 (format t "~%;;; COMPILE-KB: removing ~S" in-file)
 ;; to clean-up /tmp directory after compiling has succeeded!

 (print (emaxps-remove-file in-file))

 T  ;; return-value
)

;; eof

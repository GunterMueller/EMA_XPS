;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    debugger.cl      Hans Groschwitz     06.10.94    ;;
;;                                                     ;;
;;    Contains basic read functions for reading from   ;;
;;    LISP's stdin                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------
;; read C-Function's replies
;; -------------------------------------------------------

(defun emaxps-eom-get-reply ()
 "Read's from LISP's stdin and returns this value. Printing
  echoes to debugger's output text field is suppressed."

 (prog2
  (emaxps-begin-reply)
  ;;eof-error-p=NIL               (see CLtL2 p.567)
  (read *emaxps-stream* nil)
  (emaxps-end-of-message)
 );prog2
)

;; -------------------------------------------------------
;; read-line C-Function's replies
;; -------------------------------------------------------

(defun emaxps-eom-get-reply-line ()
 "Read's from LISP's stdin and returns this value. Printing
  echoes to debugger's output text field is suppressed."

 (prog2
  (emaxps-begin-reply)
  ;;eof-error-p=NIL               (see CLtL2 p.567)
  (read-line *emaxps-stream* nil)
  (emaxps-end-of-message)
 );prog2
)

;; -------------------------------------------------------
;; enable the user to input from debugger's input
;; -------------------------------------------------------

(defun emaxps-read ()
 "enables the user to read from debugger's input.
 emaxps-read-from-debugger-input see channels.cl!"

 (prog2
  (emaxps-read-from-debugger-input)
  (read *emaxps-stream*)
  (emaxps-is-busy)
 );prog2
)

;; -------------------------------------------------------
;; enable the user to input a line from debugger's input
;; -------------------------------------------------------

(defun emaxps-read-line ()
 "enables the user to read-line from debugger's input.
 emaxps-read-from-debugger-input see channels.cl!"

 (prog2
  (emaxps-read-from-debugger-input)
  (read-line *emaxps-stream*)
  (emaxps-is-busy)
 );prog2
)

;; eof

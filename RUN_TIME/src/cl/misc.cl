#|***************************************************
*   misc.cl       Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the misc-editor.             *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")

; ---------------------------------------------------------------

(defun emaxps-misc-save (kbname list-of-others)
 (let ((current-kb (emaxps-current-kb-name)))
  ;; choose this way to successfully do EVAL
  (emaxps-select-kb kbname)

  (emaxps-send-task :set-others NIL)  ;; reset the DB!
  (emaxps-kb-set-modified kbname)
  ;; and do it to make it known to the next run of a session
  (eval (cons 'emaxps-define-others list-of-others))

  (emaxps-select-kb current-kb)
  T  ;; return-value
 );let
)

; ---------------------------------------------------------------

(defun emaxps-misc-get (kbname)
 (let (otherslist proc)
  (setf proc ($send (symbol-value kbname) :task-processor))
  (setf otherslist ($send proc :others))

  (emaxps-begin-message :emaxps-misc-send-others)
  (if otherslist
   (emaxps-send-mbody "~S" otherslist)
  ;else
   (emaxps-send-mbody "()") 
  );if
  (emaxps-end-of-message)
 );let
)

;; eof

#|***************************************************
*   sysd.cl       Hans Groschwitz        06.10.94   *
*                 Stephan Peters                    *
*                                                   *
*   C-FrontEnds of LISP functions for control of    *
*   Popups for the Main-Menu of EMA-XPS             *
*                                                   *
*   See uid.x for details!                          *
***************************************************|#

(in-package "BABYLON")

;; -------------------------------------------------------
;; sysd-warning
;; -------------------------------------------------------

(defun sysd-warning (code &rest args)
 "sysd-warning shows an WarningDialog with only one
  PushButton. If pressed, the C-process returns T."

 (unless (and (integerp code) (>= code 0))
  (emaxps-error "sysd-warning: arg1 should be a non-negative integer"))
 (emaxps-begin-message :sysd-warning)
 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         args
 );mapcar
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; sysd-information
;; -------------------------------------------------------

(defun sysd-information (code &rest args)
 "sysd-information shows an InformationDialog with only one
  PushButton. If pressed, the C-process returns T."

 (unless (and (integerp code) (>= code 0))
  (emaxps-error "sysd-information: arg1 should be a non-negative integer"))
 (emaxps-begin-message :sysd-information)
 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         args
 );mapcar
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; sysd-accept-cancel
;; -------------------------------------------------------

(defun sysd-accept-cancel (code &rest args)
 "sysd-accept-cancel shows a QuestionDialog with an
  Accept(T) and a Cancel(NIL) Button. If pressed,
  the C-process returns the appropriate value."

 (unless (and (integerp code) (>= code 0))
  (emaxps-error "sysd-accept-cancel: arg1 should be a non-negative integer"))
 (emaxps-begin-message :sysd-accept-cancel)
 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         args
 );mapcar
 (emaxps-eom-get-reply)
)
#|
(defun sysd-accept-cancel (code &rest args)
  ;;; UNDER CONSTRUCTION!!!
  (apply 'uid-accept-cancel
   (cons
    (case code
     (0 "WARNING: while writing knowledge base: ~%~%~
         File ~S already exists! ~%~%~
         If you accept, the old contents will be overwritten!")
     (1 "The KB ~S already exists! ~2%~
         Press ACCEPT to continue loading, ~%~
                      closing the old version ~%~
                      before loading the new version. ~2%~
         Press CANCEL to abort loading ")
    );case
    args  
   );cons 
  );apply
)
|#
;; -------------------------------------------------------
;; sysd-prompt
;; -------------------------------------------------------

(defun sysd-prompt (code &rest args)
 "sysd-prompt shows a PromptDialog with an
  Accept(T) and a Cancel(NIL) Button. If pressed,
  the C-process returns the appropriate value,
  which may be any line of LISP-input!"

 (unless (and (integerp code) (>= code 0))
  (emaxps-error "sysd-prompt: arg1 should be a non-negative integer"))
 (emaxps-begin-message :sysd-prompt)
 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         args
 );mapcar
 ;;
 ;; here no (read) to grant evaluation of a complete
 ;; LISP expression
 ;; WORKAROUND: See uid-prompt for details !!!
 ;;
 (let ((dummy (emaxps-eom-get-reply-line)))
  (when (equalp "" dummy)
   (setf dummy (emaxps-eom-get-reply-line)))
  (if (equalp "NIL" dummy)
   nil
  ;; else
   (progn
    (setf dummy (read-from-string dummy))
    dummy
   );progn
  );if
 );let
)
#|
(defun sysd-prompt (code &rest args)
  ;;; UNDER CONSTRUCTION!!!
  (apply 'uid-prompt
   (cons
    (case code
     (0 "Input the name of the new KB:")
    );case
    args    
   );cons
  );apply   
)
|#
;; eof

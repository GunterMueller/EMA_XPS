;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    uid.cl           Hans Groschwitz     08.10.94    ;;
;;                                                     ;;
;;    Contains popup dialogs for the                   ;;
;;    graphical user interface.                        ;;
;;              ^    ^                                 ;;
;;    The `d' within in `uid' displays `popup-Dialog'. ;;
;;                                            ^        ;;
;;    These routines may be added to a raw LISP-World, ;;
;;    they do not need Babylon 2.2 power!!!            ;;
;;                                                     ;;
;;    This version of the User Interface uses logical  ;;
;;    channels for message-passing. The keywords       ;;
;;    representing the corresponding channel numbers   ;;
;;    must be known within dispatch.cl !!!             ;;
;;                                                     ;;
;;    BUGS:                                            ;;
;;      Should be in an own package:   UI::XXX         ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------
;; uid-information
;; -------------------------------------------------------

(defun uid-information (fmt &rest args)
 "uid-information shows an InformationDialog with only one
  PushButton. If pressed, the C-process returns T.
  The arguments work as with (format ...)."

 (unless (stringp fmt)
  (emaxps-error "uid-information: arg1 should be a format-string"))
 (emaxps-begin-message :uid-information)
 (apply #'funcall 'emaxps-send-mbody (append (list fmt) args))
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; uid-accept-cancel
;; -------------------------------------------------------

(defun uid-accept-cancel (fmt &rest args)
 "uid-accept-cancel shows a QuestionDialog with an
  Accept(T) and a Cancel(NIL) Button. If pressed,
  the C-process returns the appropriate value.
  The arguments work as with (format ...)."

 (unless (stringp fmt)
  (emaxps-error "uid-accept-cancel: arg1 should be a format-string"))
 (emaxps-begin-message :uid-accept-cancel)
 (apply #'funcall 'emaxps-send-mbody (append (list fmt) args))
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; uid-yes-no-cancel
;; -------------------------------------------------------

(defun uid-yes-no-cancel (fmt &rest args)
 "uid-yes-no-cancel shows a QuestionDialog with an
  Yes(T), No(NIL) and a Cancel(:CANCEL) Button. If pressed,
  the C-process returns the appropriate value.
  The arguments work as with (format ...)."

 (unless (stringp fmt)
  (emaxps-error "uid-yes-no-cancel: arg1 should be a format-string"))
 (emaxps-begin-message :uid-yes-no-cancel)
 (apply #'funcall 'emaxps-send-mbody (append (list fmt) args))
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; uid-prompt
;; -------------------------------------------------------

(defun uid-prompt (fmt &rest args)
 "uid-prompt shows a PromptDialog with an
  Accept(T) and a Cancel(NIL) Button. If pressed,
  the C-process returns the appropriate value,
  which may be any line of LISP-input!
  The arguments work as with (format ...)."

 (unless (stringp fmt)
  (emaxps-error "uid-prompt: arg1 should be a format-string"))
 (emaxps-begin-message :uid-prompt)
 (apply #'funcall 'emaxps-send-mbody (append (list fmt) args))
 ;;
 ;; here no (read) to grant evaluation of a complete
 ;; LISP expression
 (let ((dummy (emaxps-eom-get-reply-line)))

  ;;;;;; WORKAROUND ?!? ;;;;;;;;
  ;; uid-prompt from toplevel works fine for the first time,
  ;; uid-prompt run from within a task needs a second attempt !?!
  ;;
  (when (equalp "" dummy)
   (setf dummy (emaxps-eom-get-reply-line)))

  ;; if an empty input has been accepted by the user
  ;; --> uid-prompt should return NIL
  ;; to avoid an END-OF-STREAM error in read-line then...
  ;;
  ;; equalp->stricmp; string=->strcmp
  (if (equalp "NIL" dummy)
   nil
  ;; else
   (progn
    ;; avoids signaling an error, if EOF reached within a string...
    (setf dummy (concatenate 'string dummy "\""))
    (setf dummy (read-from-string dummy))
    ;; to avoid a multiple-value return-value
    ;; simply do a setf!
    dummy
   );progn
  );if
 );let
)

;; -------------------------------------------------------
;; uid-one-of-many
;; -------------------------------------------------------

(defun uid-one-of-many (liste label &optional (func nil))
 "uid-one-of-many shows a SelectionDialog with an
  Accept(one_element_of_liste) and a Cancel(NIL) Button.
  If pressed, the C-process returns the appropriate position
  of the value within liste.
  The optional function may be used to manipulate presentation.
  The label argument DOES NOT work as with (format ...) here!!!"

 (unless (listp liste)
  (emaxps-error "uid-one-of-many: arg1 should be a list"))
 (unless (stringp label)
  (emaxps-error "uid-one-of-many: arg2 should be a string"))
 (let (answer (orig-liste liste))
  (if func
   (setf liste (mapcar func liste))
  );if
  (emaxps-begin-message :uid-one-of-many)
  (emaxps-send-mbody "~A" label)
  (mapcar #'(lambda (x)
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" x)
            )
          liste)
  (setf answer (emaxps-eom-get-reply))
  (if (numberp answer)
   (nth answer orig-liste)
  ;;else
   nil
  );if
 );let
)

;; -------------------------------------------------------
;; uid-some-of-many
;; -------------------------------------------------------

(defun uid-some-of-many (liste label &optional (func nil))
 "uid-some-of-many shows a SelectionDialog with an
  Accept(list_of_some_elements_of_liste) and a Cancel(NIL) Button.
  If pressed, the C-process returns the appropriate positions
  of the values within liste.
  The optional function may be used to manipulate presentation.
  The label argument DOES NOT work as with (format ...) here!!!"

 (unless (listp liste)
  (emaxps-error "uid-some-of-many: arg1 should be a list"))
 (unless (stringp label)
  (emaxps-error "uid-some-of-many: arg2 should be a string"))
 (let (answer (orig-liste liste))
  (if func
   (setf liste (mapcar func liste))
  );if
  (emaxps-begin-message :uid-some-of-many)
  (emaxps-send-mbody "~A" label)
  (mapcar #'(lambda (x)
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" x)
            )
          liste)
  (setf answer (emaxps-eom-get-reply))
  (if (listp answer)
   (mapcar #'(lambda (x)
              (nth x orig-liste)
             )
            answer)
  ;;else
   nil
  );if
 );let
)

;; eof

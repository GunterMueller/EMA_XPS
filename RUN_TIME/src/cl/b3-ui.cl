#|*****************************************************************
*       b3-ui.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the babylon-3 subset of LISP calls     *
*   concerning the User-Interface.                                *
*                                                                 *
*   B3 Reference Manual, Chapters 4 and 6                         *
*                                                                 *
*****************************************************************|#


;; Symbole, die nur fuer den internen Gebrauch bestimmt sind,
;; mit % anfangen lassen, statt bqli- !!!

(in-package "BABYLON")

;---------------------------------------------------------

;;; Kap.4 --->          8 functions/macros, 0 variables

;;; See Ref. 4-2 !!! should be set at KB-Load-Time! (b3-misc)

(defvar *babylon-standard-output* "babylon-standard-output")

(defvar *babylon-standard-input*  "babylon-standard-input")

;; --- kb-confirm ---

(defun kb-confirm (fmt &rest args)
 "..."
 (apply #'uid-accept-cancel (append (list fmt) args))
)

;; --- kb-choose-from-menu ---

(defun kb-choose-from-menu (&rest args)
 "..."
 (apply #'uid-one-of-many args)
)

;; --- kb-decide ---

(defun kb-decide (fmt &rest args)
 "..."
 (apply #'uid-yes-no-cancel (append (list fmt) args))
)

;; --- kb-format ---

(defun kb-format (fmt &rest args)
 "Writes to the widget *babylon-standard-output* points to.
 Always returns NIL."

 (ui-add-text *babylon-standard-output*
  (apply #'format (append (list nil fmt) args))
 );ui-add-text
 NIL   ;; return value
)

;; --- kb-multiple-choose-from-menu ---

(defun kb-multiple-choose-from-menu (&rest args)
 "..."
 (apply #'uid-some-of-many args)
)

;; --- kb-notify ---

(defun kb-notify (fmt &rest args)
 "..."
 (apply #'uid-information (append (list fmt) args))
)

;; --- kb-prompt ---

(defun kb-prompt (fmt &rest args)
 "..."
 (apply #'uid-prompt (append (list fmt) args))
)

;; --- kb-read ---

(defun kb-read ()
 "Reads from the widget *babylon-standard-input* points to."
 (ui-get-text *babylon-standard-input*)
)

;;; =================================================================

;;; Kap.6 --->         45 functions/macros, 0 variables

(defvar interface NIL "Hack for FVA/AA...")

;; --- make-kb-interface ---

;; NO ENTRY HERE!
;; has been generated in the interface file
;; by babylon3!
;; EMA-XPS (main ...) is running it BEFORE
;; activating the start-task.
;; INFO: 3TO2 converts this function into a task
;; to avoid name conflicts at multiple KBs.

;; --- add-babylon-menu ---

(defmacro add-babylon-menu (interface liste)
 "ASSUMPTION: liste is one simple LISP-expression."
 ;; multiple graphical interfaces are not supported!
 `(let ((interface ,interface))
   ,liste
  );let
)

;; --- add-babylon-menu-item ---

(defmacro add-babylon-menu-item (interface liste)
 "ASSUMPTION: liste is one simple LISP-expression."
 ;; multiple graphical interfaces are not supported!
 `(let ((interface ,interface))
   ,liste
  );let
)

;; --- add-window-elements ---

(defmacro add-window-elements (interface liste)
 "Macro to emulate right order of appearance of the
 widgets on the session-screen ...
 Evaluate the list of args in reversed order.
 ---
 Compile-time problem: the symbol 'list is generated at compile
 time in package :BABYLON and not in :KB-NAME (e.g.) !!!"
 ;; multiple graphical interfaces are not supported!

 `(let ((interface ,interface))
   ,@(if (equal (first liste) (read-from-string "list"))
      (reverse (rest liste))
     ;else
      (emaxps-error "add-window-elements: syntax")
     );if
  );let
)

;; --- make-babylon-push-button ---

(defun make-babylon-push-button (interface
          wname xpos ypos width height label
          font color &optional action &rest args)
 "color and font are not supported by B3. align and bg-color=NIL"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when (or color font)
  (emaxps-error "make-babylon-push-button: color/font settings are not supported by B3"))
 (when action
  (setf action (format nil "~S" action))   ;; produces the callback string!
 );when
 (ui-create-pushbutton wname xpos ypos width height label font NIL color NIL action)
)

;; --- make-babylon-graphic-button ---

(defun make-babylon-graphic-button (interface
          wname xpos ypos width height file color
          &optional action &rest args)
 "frame is documented but does not ex.!
 frame and color are not supported by B3. bg-color=NIL"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when color
  (emaxps-error "make-babylon-graphic-button: color settings are not supported by B3"))
 (when action
  (setf action (format nil "~S" action))   ;; produces the callback string!
 );when
 (ui-create-bitmap-button wname xpos ypos width height file color NIL action)
)

;; --- make-babylon-text-label ---

(defun make-babylon-text-label (interface
          wname xpos ypos width height label
          font color &rest args)
 "color and font are not supported by B3. align and bg-color=NIL"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when (or color font)
  (emaxps-error "make-babylon-text-label: color/font settings are not supported by B3"))
 (ui-create-label wname xpos ypos width height label font NIL color NIL)
)

;; --- make-babylon-graphic-label ---

(defun make-babylon-graphic-label (interface
          wname xpos ypos width height file color &rest args)
 "frame is documented but does not ex.!
 frame and color are not supported by B3. bg-color=NIL"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when color
  (emaxps-error "make-babylon-graphic-label: color settings are not supported by B3"))
 (ui-create-bitmap-label wname xpos ypos width height file color NIL)
)

;; --- make-babylon-input-field ---

(defun make-babylon-input-field (interface
         wname xpos ypos width height text font color action &rest args)
 "color and font are not supported by B3. bg-color=NIL"
 (declare (ignore args       text action))   ;; last 2 temporarily
 interface  ;to avoid use of ignore!
 (when (or color font)
  (emaxps-error "make-babylon-input-field: color/font settings are not supported by B3"))
 (emaxps-error "make-babylon-input-field: currently not implemented")

 ;;; WARNING: text and action must be implemented to UI yet!!
; (ui-create-input-text wname xpos ypos width height text font color NIL action NIL)

 ;;; Sorry, but this is reality...
 (ui-create-input-text wname xpos ypos width height font color NIL)
)

;; --- make-babylon-output-field ---

(defun make-babylon-output-field (interface
         wname xpos ypos width height text font color &rest args)
 "color and font are not supported by B3. bg-color=NIL"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when (or color font)
  (emaxps-error "make-babylon-output-field: color/font settings are not supported by B3"))
 ;;; NIL = bg color!
 (ui-create-output-text wname xpos ypos width height text font color NIL)
)

;; --- make-babylon-separator ---

(defun make-babylon-separator (interface
         wname xpos ypos len orient color &rest args)
 "Color may be NIL only. Interface is ignored, because EMA-XPS does not support
 multiple user screens at a time"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (when color
  (emaxps-error "make-babylon-separator: babylon3 does not support color settings"))
 (let (width height)
  (cond
   ((equalp orient "horizontal")
    (setf width len)
    (setf height 0)
   )
   ((equalp orient "vertical")
    (setf width 0)
    (setf height len)
   )
   (t (emaxps-error "make-babylon-separator: illegal orientation ~S" orient))
  );case
  (ui-create-separator wname xpos ypos width height color)
 );let
 NIL   ;;return-value
)

;; --- make-babylon-menu ---

(defun make-babylon-menu (interface name label &rest args)
 "NIL=mnem"
 (declare (ignore args))
 interface  ;to avoid use of ignore!
 (ui-create-pulldown-menu name label NIL)
)

;; --- make-babylon-menu-item ---

(defun make-babylon-menu-item (#|interface|# menu name label
         &optional action &rest args)
 "NIL=mnem"
 ;; DA2.kb: interface wird nicht benoetigt!?!
 ;; widerspricht REF 6-18 !!!
 (declare (ignore #|interface|# args))
 (when action
  (setf action (format nil "~S" action))   ;; produces the callback string!
 );when
 (ui-create-menu-entry name menu label NIL action)
)

;; --- set-push-button-sensitivity ---

(defun set-push-button-sensitivity (pb val)
 (ui-set-sensitivity pb val)
)

;; --- get-push-button-label ---

(defun get-push-button-label (pb)
 (ui-get-text pb)
)

;; --- set-push-button-label ---

(defun set-push-button-label (pb lb)
 (ui-set-text pb lb)
)

;; --- set-graphic-button-sensitivity ---

(defun set-graphic-button-sensitivity (pb val)
 (ui-set-sensitivity pb val)
)

;; --- get-graphic-button-picture ---

(defun get-graphic-button-picture (gb)
 (ui-get-bitmap gb)
)

;; --- set-graphic-button-picture ---

(defun set-graphic-button-picture (gb path)
 (ui-set-bitmap gb path)
)

;; --- get-text-label ---

(defun get-text-label (pb)
 (ui-get-text pb)
)

;; --- set-text-label ---

(defun set-text-label (pb lb)
 (ui-set-text pb lb)
)

;; --- get-graphic-label-picture ---

(defun get-graphic-label-picture (gb)
 (ui-get-bitmap gb)
)

;; --- set-graphic-label-picture ---

(defun set-graphic-label-picture (gb path)
 (ui-set-bitmap gb path)
)

;; --- get-input-field-text ---

(defun get-input-field-text (inf)
 (ui-get-text inf)
)

;; --- set-input-field-text ---

(defun set-input-field-text (inf txt)
 (ui-set-text inf txt)
)

;; --- get-output-field-text ---

(defun get-output-field-text (out)
 (ui-get-text out)
)

;; --- set-output-field-text ---

(defun set-output-field-text (out txt)
 (ui-set-text out txt)
)

;; --- add-output-field-text ---

(defun add-output-field-text (out txt)
 (ui-add-text out txt)
)

;; --- clear-output-field-text ---

(defun clear-output-field-text (out)
 (ui-set-text out "")
)

;; --- set-menu-sensitivity ---

(defun set-menu-sensitivity (mn vl)
 (ui-set-sensitivity mn vl)
)

;; --- set-menu-item-sensitivity ---

(defun set-menu-item-sensitivity (mn vl)
 (ui-set-sensitivity mn vl)
)

;; --- get-babylon-menu ---

(defun get-babylon-menu (interface name)
 "here only strings are known, no instances!
 Only one UI at a time is possible using EMA-XPS"
 interface  ;to avoid use of ignore!
 name    ;;return-value
)

;; --- get-babylon-menu-item ---

(defun get-babylon-menu-item (interface menu item)
 "no widgetname may be used twice!"
 (declare (ignore menu))
 interface  ;to avoid use of ignore!
 item    ;;return-value
)

;; --- get-window-element ---

(defun get-window-element (interface wname)
 "EMA-XPS handles the Widgets by their names ==> Dummy"
 interface  ;to avoid use of ignore!
 wname
)

;; --- get-interface ---

(defun get-interface (&optional kb)
 "returns the interface of the KB kb (defaults to this-kb).
 WARNING: *KB* seems to be a global B3-Variable displaying the current KB"
 (if kb
  kb
 ;else
  (this-kb)
 );if
)

;; --- hide-eis-element ---

(defun hide-eis-element (w)
 (ui-set-visibility w NIL)
)

;; --- show-eis-element ---

(defun show-eis-element (w)
 (ui-set-visibility w T)
)

;; --- get-eis-element-x-position ---

(defun get-eis-element-x-position (w)
 (first (ui-get-geometry w))
)

;; --- get-eis-element-y-position ---

(defun get-eis-element-y-position (w)
 (second (ui-get-geometry w))
)

;; --- set-eis-element-x-position ---

(defun set-eis-element-x-position (w p)
 (ui-set-geometry w p NIL NIL NIL)
)

;; --- set-eis-element-y-position ---

(defun set-eis-element-y-position (w p)
 (ui-set-geometry w NIL p NIL NIL)
)

;; --- get-eis-element-size ---

(defun get-eis-element-size (w)
 "ret=(W H), (ui-get-geom n)=(x y w h)
 AA: right order in list is: (X Y W H)"
 (let ((ret (ui-get-geometry w)))
  (if (and ret (listp ret))
   (list (third ret) (fourth ret))   ;; return-value
  ;else
   ret
  );if
 );let
)

;; --- get-eis-element-height ---

(defun get-eis-element-height (w)
 (fourth (ui-get-geometry w))
)

;; --- get-eis-element-width ---

(defun get-eis-element-width (w)
 (third (ui-get-geometry w))
)

;; --- set-eis-element-size ---

(defun set-eis-element-size (w lst)
 "lst=(W H), (ui-set-geom n x y w h)
 AA: right order in list is: (W H)"
 (ui-set-geometry w NIL NIL (first lst) (second lst))
)

;; --- set-eis-element-height ---

(defun set-eis-element-height (w p)
 (ui-set-geometry w NIL NIL NIL p)
)

;; --- set-eis-element-width ---

(defun set-eis-element-width (w p)
 (ui-set-geometry w NIL NIL p NIL)
)

;;; eof

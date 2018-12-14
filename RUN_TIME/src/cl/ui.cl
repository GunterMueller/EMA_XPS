;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ui.cl            Hans Groschwitz     08.10.94    ;;
;;                                                     ;;
;;    Contains routines for the                        ;;
;;    graphical user interface.                        ;;
;;              ^    ^                                 ;;
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

;; ----------------------------------------------------
;;  Protocol Transfer Control   (about 30% faster)
;; ----------------------------------------------------

(defvar *ui-sync* NIL)     ;; prefer asynchronous protocol

(defun ui-synchronize (val)
 (setf *ui-sync* val)
)

(defun ui-get-reply ()
 (if *ui-sync*
  (emaxps-eom-get-reply)
 ;else
  (progn
   (emaxps-send-async)
   (emaxps-end-of-message)
   NIL      ;;return-value (always OK !!!)
  );progn
 );if
)

;; ----------------------------------------------------
;;  Useful aids ...
;; ----------------------------------------------------

(defun ui-check-valid-cb (cb)
 "To increase security the callback string is checked for 
  validity!"

 (if (null cb)
  nil ;; return-value!
 ;else
  (format nil "~s" (read-from-string cb))
 )
)

(defmacro ui-unsigned-int-p (i)
  "Useful to check whether xpos and ypos
   of a widget are valid arguments"

   `(and (integerp ,i)(>= ,i 0))
)

(defmacro ui-allowed-geometry (x y w h)
  "Only to be used by ui-create-xxx, hence in that case values
   of NIL are NOT allowed!"

   `(and (ui-unsigned-int-p ,x)
         (ui-unsigned-int-p ,y)
         (ui-unsigned-int-p ,w)
         (ui-unsigned-int-p ,h))
)

(defun ui-send-string (str)
  "Problem: ~A displays NIL and ``NIL'' the same way"

   (if (null str)
    (emaxps-send-nil)
   ;else
    (emaxps-send-mbody "~A" str)
   )
)

(defun ui-send-align (val)
  "Problems: ~a directive in format string makes 'key' from :key !"

   (cond ((equal val :left)   (emaxps-send-mbody ":left"))
         ((equal val :right)  (emaxps-send-mbody ":right"))
         ((equal val :center) (emaxps-send-mbody ":center"))
         ((null val)          (emaxps-send-nil))
         (t (emaxps-error "ui-send-align: must be :left, :right, :center or NIL "))
   )
)

(defun ui-send-bool (val)
  "Problems: Any non-Nil must be converted to T"

   (if (null val)
     (emaxps-send-nil)
   ;else
     (emaxps-send-mbody "T")
   )
)

(defun ui-send-mnem (val)

   (if (null val)
    (emaxps-send-nil)
   ;else
    (emaxps-send-mbody "~A" val)
   )
)

;******************************************************************************
;*                                                                            *
;*                        Event orientation controls                          *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-create-simple-session-screen
;; -------------------------------------------------------

(defun ui-create-simple-session-screen ()
 "Returns NIL if OK or :ERROR if not!"

 (emaxps-begin-message :ui-create-simple-session-screen)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-stop-session
;; -------------------------------------------------------

(defun ui-stop-session ()
 "Returns NIL if OK or :ERROR if not!"
 
 (emaxps-begin-message :ui-stop-session)
 (emaxps-eom-get-reply)
)

;******************************************************************************
;*                                                                            *
;*                        Basic Widget Operations                             *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-destroy-widget
;; -------------------------------------------------------

(defun ui-destroy-widget (wname)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-destroy-widget: wname must be a non-empty string"))
 (emaxps-begin-message :ui-destroy-widget)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-raise-to-top
;; -------------------------------------------------------

(defun ui-raise-to-top (wname)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-raise-to-top: wname must be a non-empty string"))
 (emaxps-begin-message :ui-raise-to-top)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-lower-to-bottom
;; -------------------------------------------------------

(defun ui-lower-to-bottom (wname)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-lower-to-bottom: wname must be a non-empty string"))
 (emaxps-begin-message :ui-lower-to-bottom)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;******************************************************************************
;*                                                                            *
;*                            Resource Get                                    *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-get-session-bg
;; -------------------------------------------------------

(defun ui-get-session-bg ()
 "Returns NIL, :ERROR or string"

 (emaxps-begin-message :ui-get-session-bg)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-menu-bg
;; -------------------------------------------------------

(defun ui-get-menu-bg ()
 "Returns NIL, :ERROR or string"

 (emaxps-begin-message :ui-get-menu-bg)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-menu-fg
;; -------------------------------------------------------

(defun ui-get-menu-fg ()
 "Returns NIL, :ERROR or string"

 (emaxps-begin-message :ui-get-menu-fg)
 (emaxps-eom-get-reply)
)


;; -------------------------------------------------------
;; ui-get-menu-font
;; -------------------------------------------------------

(defun ui-get-menu-font ()
 "Returns NIL, :ERROR or string"

 (emaxps-begin-message :ui-get-menu-font)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-visibility
;; -------------------------------------------------------

(defun ui-get-visibility (wname)
 "Returns T, NIL or :ERROR"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-visibility: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-visibility)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-sensitivity
;; -------------------------------------------------------

(defun ui-get-sensitivity (wname)
 "Returns T, NIL or :ERROR"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-sensitivity: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-sensitivity)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-background
;; -------------------------------------------------------

(defun ui-get-background (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-background: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-background)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-foreground
;; -------------------------------------------------------

(defun ui-get-foreground (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-foreground: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-foreground)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-font
;; -------------------------------------------------------

(defun ui-get-font (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-font: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-font)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-bitmap
;; -------------------------------------------------------

(defun ui-get-bitmap (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-bitmap: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-bitmap) 
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-geometry
;; -------------------------------------------------------

(defun ui-get-geometry (wname)
 "Returns a list of (x, y, w, h) or NIL or :ERROR"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-geometry: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-geometry)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-alignment
;; -------------------------------------------------------

(defun ui-get-alignment (wname)
 "Returns :LEFT, :RIGHT, :CENTER, NIL or :ERROR"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-alignment: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-alignment)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-text
;; -------------------------------------------------------

(defun ui-get-text (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-text: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-text)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-callback
;; -------------------------------------------------------

(defun ui-get-callback (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-callback: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-callback)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;; -------------------------------------------------------
;; ui-get-mnemonic
;; -------------------------------------------------------

(defun ui-get-mnemonic (wname)
 "Returns NIL, :ERROR or string"

 (if (or (not (stringp wname)) (string= wname ""))
   (emaxps-error "ui-get-mnemonic: wname must be a non-empty string"))

 (emaxps-begin-message :ui-get-mnemonic)
 (emaxps-send-mbody "~A" wname)
 (emaxps-eom-get-reply)
)

;******************************************************************************
;*                                                                            *
;*                            Resource Set                                    *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-set-session-bg
;; -------------------------------------------------------

(defun ui-set-session-bg (color)
 "Returns NIL if OK or :ERROR if not!"

 (if (and color (or (not (stringp color)) (string= "" color)))
  (emaxps-error "ui-set-session-bg: color must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-session-bg)
 (ui-send-string color)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-menu-fg
;; -------------------------------------------------------

(defun ui-set-menu-fg (color)
 "Returns NIL if OK or :ERROR if not!"

 (if (and color (or (not (stringp color)) (string= "" color)))
  (emaxps-error "ui-set-menu-fg: color must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-menu-fg)
 (ui-send-string color)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-menu-bg
;; -------------------------------------------------------

(defun ui-set-menu-bg (color)
 "Returns NIL if OK or :ERROR if not!"

 (if (and color (or (not (stringp color)) (string= "" color)))
  (emaxps-error "ui-set-menu-bg: color must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-menu-bg)
 (ui-send-string color)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-menu-font
;; -------------------------------------------------------

(defun ui-set-menu-font ( font)
 "Returns NIL if OK or :ERROR if not!"

 (if (and font (or (not (stringp font )) (string= "" font)))
  (emaxps-error "ui-set-menu-font: font must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-menu-font)
 (ui-send-string font)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-visibility
;; -------------------------------------------------------

(defun ui-set-visibility (wname visi)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-visibility: wname must be a non-empty string"))

 (emaxps-begin-message :ui-set-visibility)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-bool visi)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-foreground
;; -------------------------------------------------------

(defun ui-set-foreground (wname color)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-foreground: wname must be a non-empty string"))
 (if (and color (or (not (stringp color)) (string= "" color)))
  (emaxps-error "ui-set-foreground: color must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-foreground)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string color)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-background
;; -------------------------------------------------------

(defun ui-set-background (wname color)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-background: wname must be a non-empty string"))
 (if (and color (or (not (stringp color)) (string= "" color)))
  (emaxps-error "ui-set-background: color must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-background)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string color)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-font
;; -------------------------------------------------------

(defun ui-set-font (wname font)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-font: wname must be a non-empty string"))
 (if (and font (or (not (stringp font)) (string= "" font)))
  (emaxps-error "ui-set-font: font must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-font)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string font)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-bitmap
;; -------------------------------------------------------

(defun ui-set-bitmap (wname path)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-bitmap: wname must be a non-empty string"))
 (if (and path (or (not (stringp path)) (string= "" path)))
  (emaxps-error "ui-set-bitmap: path must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-bitmap)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string path)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-alignment
;; -------------------------------------------------------

(defun ui-set-alignment (wname alignment)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-alignment: wname must be a non-empty string"))

 (emaxps-begin-message :ui-set-alignment)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-align alignment)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-text
;; -------------------------------------------------------

(defun ui-set-text (wname text)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-text: wname must be a non-empty string"))
 (if (and text (not (stringp text)))
  (emaxps-error "ui-set-text: text must be a string or NIL"))

 (emaxps-begin-message :ui-set-text)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string text)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-add-text
;; -------------------------------------------------------

(defun ui-add-text (wname text)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-add-text: wname must be a non-empty string"))
 (if (and text (not (stringp text)))
  (emaxps-error "ui-add-text: text must be a string or NIL"))

 (emaxps-begin-message :ui-add-text)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string text)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-mnemonic
;; -------------------------------------------------------

(defun ui-set-mnemonic (wname mnem)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-mnemonic: wname must be a non-empty string"))
 (if (and mnem (or (not (stringp mnem)) (string= "" mnem)))
  (emaxps-error "ui-set-mnemonic: mnemonic must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-set-mnemonic)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-mnem mnem)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-sensitivity
;; -------------------------------------------------------

(defun ui-set-sensitivity (wname sensi)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-sensitivity: wname must be a non-empty string"))

 (emaxps-begin-message :ui-set-sensitivity)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-bool sensi)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-callback
;; -------------------------------------------------------

(defun ui-set-callback (wname callback)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-callback: wname must be a non-empty string"))
 (if (and callback (not (stringp callback)))
  (emaxps-error "ui-set-callback: callback must be a string or NIL"))

 (emaxps-begin-message :ui-set-callback)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string callback)
 (ui-get-reply)
)

;; -------------------------------------------------------
;; ui-set-geometry
;; -------------------------------------------------------

(defun ui-set-geometry (wname x y w h)
 "Returns NIL if OK or :ERROR if not!
 A value of NIL at one/some of X, Y, W, H makes it not be changed!
 (ui-set-geometry 'wname' nil nil nil nil)
 does nothing, but returns NIL (ok), if Widget exists !
 else :ERROR"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-set-geometry: wname must be a non-empty string"))
 (unless (and
            (or (null x) (ui-unsigned-int-p x))
            (or (null y) (ui-unsigned-int-p y))
            (or (null w) (ui-unsigned-int-p w))
            (or (null h) (ui-unsigned-int-p h))
           )
  (emaxps-error "ui-set-geometry: error in geometry")
 );unless
 (emaxps-begin-message :ui-set-geometry)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (ui-send-string x)
 (emaxps-separate-mparts)
 (ui-send-string y)
 (emaxps-separate-mparts)
 (ui-send-string w)
 (emaxps-separate-mparts)
 (ui-send-string h)
 (ui-get-reply)           ;; return-value!
)

;; -------------------------------------------------------
;; ui-widget-p
;; -------------------------------------------------------

(defun ui-widget-p (wname)
  "Checks for a valid Widgetname (see ui-set-geometry).
   If Widget exists, ui-set-geometry returns Nil (but must be T),
   else returns :ERROR (but must be Nil)!"

 (let ((*ui-sync* T))
  (null (ui-set-geometry wname nil nil nil nil))
 );let
)

;******************************************************************************
;*                                                                            *
;*                        Widget Creation                                     *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-create-pushbutton
;; -------------------------------------------------------

(defun ui-create-pushbutton (wname x y w h text font align fg bg cb)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-pushbutton: wname must be a non-empty string"))
 (if (and text (not (stringp text)))
  (emaxps-error "ui-create-pushbutton: text must be a string or NIL"))
 (if (and font (or (not (stringp font)) (string= "" font)))
  (emaxps-error "ui-create-pushbutton: font must be a non-empty string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-pushbutton: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-pushbutton: background must be a non-empty string or NIL"))
 (if (and cb (or (not (stringp cb)) (string= "" cb)))
  (emaxps-error "ui-create-pushbutton: callback must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-pushbutton: error in geometry"))
 (emaxps-begin-message :ui-create-pushbutton)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string text)
 (emaxps-separate-mparts)
 (ui-send-string font)
 (emaxps-separate-mparts)
 (ui-send-align align)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-separate-mparts)
 (ui-send-string cb)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-label
;; -------------------------------------------------------

(defun ui-create-label (wname x y w h label font align fg bg)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-label: wname must be a non-empty string"))
 (if (and label (not (stringp label)))
  (emaxps-error "ui-create-label: label must be a string or NIL"))
 (if (and font (or (not (stringp font)) (string= "" font)))
  (emaxps-error "ui-create-label: font must be a non-empty string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-label: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-label: background must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-label: error in geometry"))
 (emaxps-begin-message :ui-create-label)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string label)
 (emaxps-separate-mparts)
 (ui-send-string font)
 (emaxps-separate-mparts)
 (ui-send-align align)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-bitmap-button
;; -------------------------------------------------------

(defun ui-create-bitmap-button (wname x y w h path fg bg cb)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-bitmap-button: wname must be a non-empty string"))
 (if (and path (not (stringp path)))
  (emaxps-error "ui-create-bitmap-button: path must be a string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-bitmap-button: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-bitmap-button: background must be a non-empty string or NIL"))
 (if (and cb (not (stringp cb)))
  (emaxps-error "ui-create-bitmap-button: callback must be a string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-bitmap-button: error in geometry"))
 (emaxps-begin-message :ui-create-bitmap-button)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string path)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-separate-mparts)
 (ui-send-string cb)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-bitmap-label
;; -------------------------------------------------------

(defun ui-create-bitmap-label (wname x y w h path fg bg)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-bitmap-label: wname must be a non-empty string"))
 (if (and path (not (stringp path)))
  (emaxps-error "ui-create-bitmap-label: path must be a string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-bitmap-label: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-bitmap-label: background must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-bitmap-label: error in geometry"))
 (emaxps-begin-message :ui-create-bitmap-label)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string path)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-input-text
;; -------------------------------------------------------

(defun ui-create-input-text (wname x y w h font fg bg)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-input-text: wname must be a non-empty string"))
 (if (and font (not (stringp font)))
  (emaxps-error "ui-create-input-text: font must be a string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-input-text: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-input-text: background must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-input-text: error in geometry"))
 (emaxps-begin-message :ui-create-input-text)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string font)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-output-text
;; -------------------------------------------------------

(defun ui-create-output-text (wname x y w h text font fg bg)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-output-text: wname must be a non-empty string"))
 (if (and text (not (stringp text)))
  (emaxps-error "ui-create-output-text: text must be a string or NIL"))
 (if (and font (or (not (stringp font)) (string= "" font)))
  (emaxps-error "ui-create-output-text: font must be a non-empty string or NIL"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-output-text: foreground must be a non-empty string or NIL"))
 (if (and bg (or (not (stringp bg)) (string= "" bg)))
  (emaxps-error "ui-create-output-text: background must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-output-text: error in geometry"))
 (emaxps-begin-message :ui-create-output-text)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string text)
 (emaxps-separate-mparts)
 (ui-send-string font)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-separate-mparts)
 (ui-send-string bg)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-pulldown-menu
;; -------------------------------------------------------

(defun ui-create-pulldown-menu (wname label mnem)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-pulldown-menu: wname must be a non-empty string"))
 (if (and label (not (stringp label)))
  (emaxps-error "ui-create-pulldown-menu: label must be a string or NIL"))
 (if (and label (or (not (stringp label)) (string= "" label)))
  (emaxps-error "ui-create-pulldown-menu: font must be a non-empty string or NIL"))
 (if (and mnem (or (not (stringp mnem)) (string= "" mnem)))
  (emaxps-error "ui-create-pulldown-menu: mnemonic must be a non-empty string or NIL"))
 (emaxps-begin-message :ui-create-pulldown-menu)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" label)
 (emaxps-separate-mparts)
 (ui-send-mnem mnem)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-menu-entry
;; -------------------------------------------------------

(defun ui-create-menu-entry (wname pd_name label mnem cb)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-menu-entry: wname must be a non-empty string"))
 (if (or (not (stringp pd_name)) (string= pd_name ""))
  (emaxps-error "ui-create-menu-entry: pulldown-name must be a non-empty string"))
 (if (and label (not (stringp label)))
  (emaxps-error "ui-create-menu-entry: label must be a string or NIL"))
 (if (and mnem (or (not (stringp mnem)) (string= "" mnem)))
  (emaxps-error "ui-create-menu-entry: mnemonic must be a non-empty string or NIL"))
 (if (and cb (not (stringp cb)))
  (emaxps-error "ui-create-menu-entry: callback must be a string or NIL"))
 (emaxps-begin-message :ui-create-menu-entry)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" pd_name)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" label)
 (emaxps-separate-mparts)
 (ui-send-mnem mnem)
 (emaxps-separate-mparts)
 (ui-send-mnem cb)
 (emaxps-eom-get-reply)   ;; return-value!
)

;; -------------------------------------------------------
;; ui-create-separator
;; -------------------------------------------------------

(defun ui-create-separator (wname x y w h fg)
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp wname)) (string= wname ""))
  (emaxps-error "ui-create-separator: wname must be a non-empty string"))
 (if (and fg (or (not (stringp fg)) (string= "" fg)))
  (emaxps-error "ui-create-separator: foreground must be a non-empty string or NIL"))
 (unless (ui-allowed-geometry x y w h)
  (emaxps-error "ui-create-separator: error in geometry"))
 (emaxps-begin-message :ui-create-separator)
 (emaxps-send-mbody "~A" wname)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" x)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" y)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" w)
 (emaxps-separate-mparts)
 (emaxps-send-mbody "~A" h)
 (emaxps-separate-mparts)
 (ui-send-string fg)
 (emaxps-eom-get-reply)   ;; return-value!
)

;******************************************************************************
;*                                                                            *
;*                        Save Current User-Interface                         *
;*                                                                            *
;******************************************************************************

;; -------------------------------------------------------
;; ui-save
;; -------------------------------------------------------

(defun ui-save (command &optional (resource nil))
 "Returns NIL if OK or :ERROR if not!"

 (if (or (not (stringp command)) (string= command ""))
   (emaxps-error "ui-save: command_file must be a non-empty string"))
 (if (and resource (or (not (stringp resource)) (string= resource "")))
   (emaxps-error "ui-save: resource_file must be a non-empty string or NIL"))

 (emaxps-begin-message :ui-save)
 (emaxps-send-mbody "~A" command)
 (emaxps-separate-mparts)
 (ui-send-string resource) 
 (emaxps-eom-get-reply)   ;; return-value!
)

;; eof

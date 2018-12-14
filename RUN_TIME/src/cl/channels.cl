;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    channels.cl      Hans Groschwitz     08.10.94    ;;
;;                                                     ;;
;;    In spite of using multiple LISP Streams for IPC  ;;
;;    There should be only one physical stream and n   ;;
;;    logical ones.     METHOD = (De)Multiplexing      ;;
;;    LISP always writes to only ONE output stream     ;;
;;    at a time!                                       ;;
;;                                                     ;;
;;    The C process recognices two Escapes in advance, ;;
;;    followed by two digits, informing about the type ;;
;;    of message following... Double zero is reserved  ;;
;;    for writing text to stdout!                      ;;
;;                                                     ;;
;;    Channels 0-9 are reserved for special purposes   ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------

;; IMPORTANT: for portability to platforms using another
;;            physical channel, only send to this stream!
;;
;; WARNING:   CLISP-1996-05-30 presets (at compile time ?!?)
;;            *terminal-io* with #<CLOSED TERMINAL-STREAM> !!!
;;
;;            To avoid trouble, set *emaxps-stream* to T,
;;            which will read the value of *terminal-io* at
;;            RUN-TIME !!
;;
(defvar *emaxps-stream* T)

;; -------------------------------------------------------

(defun emaxps-begin-kb-data-xmit ()
 "See main.cl: used for multiple KB management"

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 8)
; (finish-output *emaxps-stream*)
)

;; -------------------------------------------------------

(defun emaxps-is-ready ()
; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 0)
; (finish-output *emaxps-stream*)
)

(defun emaxps-is-busy ()
; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 1)
; (finish-output *emaxps-stream*)
)

(defun emaxps-invoke-debugger ()
; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 2)
; (finish-output *emaxps-stream*)
)

(defun emaxps-read-from-debugger-input ()
 "Enable read from debugger input"

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 3)
; (finish-output *emaxps-stream*)
)

(defun emaxps-begin-reply ()
 "Replies from C to LISP are printed via the stdin of LISP and
  therefore echoed. To suppress this printing into the output-
  field of the PAD a VChannel switch is necessary. The final
  ESC001 (Busy) will reset the C-side."

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 4)
; (finish-output *emaxps-stream*)
)

(defun emaxps-end-of-boot-phase ()
 "This message is sent by C process once at boot time
  to synchronize with LISP !!!
  DO NOT USE THIS for any other purpose!"

 ;; and now a warm welcome from inference engine, too...
 (format *emaxps-stream*
     "~%~%~
      ;; ********************************************~%~
      ;;          Welcome to ~A~%~
      ;; ********************************************~%~
      ~%~%"
     *EMAXPS-VERSION*)

 (emaxps-start-channel-management)

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a~a~a~a" #\escape 0 0 9)
; (finish-output *emaxps-stream*)

 ;; additionally necessary:
 (emaxps-is-ready)

 "When using CLISP the toplevel loop does not behave different 
  to e.g. Break Level #0. Allegro CL 4.1 is proud to have created a powerful
  toplevel loop with 1000s of additional features. This includes
  restauration of Variables like *readtable*. This makes restarting inference
  from the toplevel loop in advance to a ButtonPressEvent harder."

 (setf *readtable* *emaxps-readtable*)

 ;; this was set from toplevel and has been moved to here
 (setf *debugger-hook* 'emaxps-pre-debugger)

 ;; explicitly, hence saveinitmem resets the toplevel *evalhook* to NIL !!!
 (setf *evalhook* 'emaxps-evalhook)

 NIL  ;; return value
)

(defun emaxps-user-interrupt ()
 "This routine is called when pressing Break!
  to grant initialization of channel management 
  in the new debuger level!
  See main.c LISP_interrupt()"

 (setf *evalhook* 'emaxps-evalhook)
 NIL  ;; return value
)

;; -------------------------------------------------------

(defun emaxps-begin-message (channelno)
 "for the logical channels no. 10-99...
  Those channel numbers are no more enterable
  as an integer. An :keyword has to be used to simpify
  reorganisation of channel numbers. The task of replacing
  this keyword by the right integer is done by
  (emaxps-dispatch)."

 (let (high mid low no tmp)
  (setf no   (emaxps-dispatch channelno))
  (setf high (floor  (/ no 100)) )
  (setf tmp  (- no (* high 100)) )
  (setf mid  (floor  (/ tmp 10)) )
  (setf low  (- tmp  (* mid 10)) )
;  (finish-output *emaxps-stream*)
  (format *emaxps-stream* "~a~a~a~a" #\escape high mid low)
;  (finish-output *emaxps-stream*)
 );let
)

(defun emaxps-end-of-message ()
 (emaxps-is-busy)
)

(defun emaxps-send-mbody (fmt &rest args)
 "Sends a message to the current channel.
  Introduced for portability to platforms
  using a different physical channel."

 (apply #'funcall 'format (append (list *emaxps-stream* fmt) args))
)

(defun emaxps-separate-mparts ()
 "The C-Process recognizes the ETX = ^C character within
  the message as the message-part separator"

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a" (code-char 3))
; (finish-output *emaxps-stream*)
)

(defun emaxps-send-nil ()
 "The C-Process recognizes the EOT = ^D character within
  the message as the symbol for the LISP NIL (in spite of
  the string ``nil'')"

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a" (code-char 4))
; (finish-output *emaxps-stream*)
)

(defun emaxps-send-async ()
 "The C-Process recognizes the CAN = ^X character at the end
  of the message as an information to work UI-setting commands
  asynchronously (without a reply)."

; (finish-output *emaxps-stream*)
 (format *emaxps-stream* "~a" (code-char 24))
; (finish-output *emaxps-stream*)
)

;; -------------------------------------------------------
;; Add channel-management support to the Toplevel-Loop:
;; -------------------------------------------------------

(defvar *emaxps-manages-channels* nil)

(defun emaxps-start-channel-management ()
 (setf *emaxps-manages-channels* T)
)

(defun emaxps-stop-channel-management ()
 (setf *emaxps-manages-channels* nil)
)

;; -------------------------------------------------------

(defun emaxps-evalhook (form &optional env)
 "CLtL1 p. 323
  Sends a resetting string in front of every output to Toplevel"

 (let (vals)
  ;; Outputs of normal Toplevel-Args should be readable...
  (if *emaxps-manages-channels* (emaxps-is-busy))

  ;; Recoursions should not evalhook that way! --v
  (setf vals (multiple-value-list (evalhook form nil nil env)))

  ;; The results of the Toplevel Loop should be readable again ...
  (if *emaxps-manages-channels* (emaxps-is-ready))

  (values-list vals)  ;; RETURN_VALUE!
 );let
)

;; -------------------------------------------------------
;; In this context CLISP is CLtL2-compatible:
;; (invoke-debugger condition) first calls the function 
;; pointed to by *debugger-hook*, if non-NIL.
;; WARNING for CLISP users:
;; *debugger-hook* only works with versions later or equal
;; (lisp-implementation-version) "January 1994"
;; -------------------------------------------------------

(defun emaxps-pre-debugger (x y)
 "CLtL2 p. 915
  Will be called before entering the nth debugger level"

 (declare (ignore x y))
 ;; it might have changed...
 (emaxps-start-channel-management)
 (emaxps-invoke-debugger)
 ;; Any time when debugger is invoked, *evalhook* is reset to NIL
 (setf *evalhook* 'emaxps-evalhook)
)

;; -------------------------------------------------------

;; eof

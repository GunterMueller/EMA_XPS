;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    inf-eng.cl       Hans Groschwitz     06.12.94    ;;
;;                                                     ;;
;;    Contains routines to enable the Babylon 2.2      ;;
;;    inference engine to keep in communication with   ;;
;;    the user at session time.                        ;;
;;                                                     ;;
;;    This includes patches for the STDIO interface    ;;
;;    which requires (emaxps-read) a.s.o. to enable    ;;
;;    inputs directly from debugger PAD's input text!  ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------
;; Patches to the tty-mixin of b22 to avoid running
;; into a non-existant EndOfStream terminal-io
;; -------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tty/basic/tty-menu.cl   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read  -->  emaxps-read

(def$method (tty-menu-mixin :choose-from-menu)
          (item-list &optional label &rest ignore-rest)
 "presents a menu and executes the operation specified for
 the selected item. each item from item-list specifies a row
 of the menu. an item is selected by entering the number of
 the item, which is displayed if the item is selectable. the
 most general form of item is (name operation argument . options). 
 symbol or string describe what is displayed besides the item number,
 operation might be :value, :funcall :eval or :no-select causing
 to return, call or eval argument. :no-select makes the item
 non-selectable. options are ignored. short form of item
 specifications are: name, (name . value) or (name value)
 which correspond to (name :value name) and (name :value value)
 respectively. label which has to be a string is used as
 menu headline."
  
 (declare (ignore ignore-rest))
 (let ((*standard-input* dialog-stream)
       (*standard-output* dialog-stream))
  (terpri)
  (princ (getentry star-str babylon-io-table))
  (terpri)
  (princ "*        0 ")
  (princ (getentry no-select-str babylon-io-table))
  (format t "~70T") (princ "*") (terpri)    
  (princ "*  MENU:   ")
  (if label (princ label))
  (format t "~70T") (princ "*") (terpri)
  (show-menu-loop item-list 1)
  (princ (getentry star-str babylon-io-table))
  (terpri)
  (princ "  --->   ")
  (let ((answer (emaxps-read)))
   (cond ((and (numberp answer)
               (> answer 0)
               (< answer (1+ (length item-list)))
          );and
          (let ((item (nth (1- answer) item-list)))
           (if (and (consp item)
                    (consp (rest item))
                    (equal (second item) :no-select)
               );and
            ($send self :choose-from-menu item-list label)
           ;else
            (execute-menu-action (nth (1- answer) item-list))
           );if
          );let
         )
         ((equal answer 0) nil)
         (t ($send self :choose-from-menu item-list label))
   );cond
  );let
 );let
)

;; read-line  -->  emaxps-read-line

(defun mult-prompt-assoc (a-list &optional (pstring "  ---> "))
 "accept multi-integer input from user and returns a list
 of the corresponding items, if input has been legal."

 (terpri)
 (princ pstring)
 (let* ((input-string (concatenate 'string "(" (emaxps-read-line) ")" ))
        (answers (read-from-string input-string))
        (result
         (catch 'input-error
          (mapcar #'(lambda (answer)
                     (or (unless (eq answer '-)
                          (assoc answer a-list)
                         );unless
                         (throw 'input-error
                          (format t
                           (getentry illegal-choice-fstr babylon-io-table)
                           answer
                          );format
                         );throw
                     );or
                    );lambda
                    answers
          );mapcar
         );catch
        );result
       )
  (or result (mult-prompt-assoc a-list pstring))
 );let*
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tty/basic/t-dialog.cl   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read-line dialog-stream  -->  emaxps-read-line

(def$method (tty-dialog-mixin :prompt-for-input) (comment)
 "reads in a line from dialog-stream and returns
 the line as string. comment is used as prompt."

 (prog2
  (format dialog-stream "~%~A ~A " *bab-prompt* comment)
  (emaxps-read-line)
  (terpri dialog-stream)
 );prog2
)

;; read-char/unreadchar/read  -->  emaxps-read

;; HENCE read reads one char OR one LEXPR!
;; on the other hand now the return-value is a SYMBOL or LIST or whatever...
;; additional code chacks for symbolp, then format nil to a string and
;; read-from-string that char!

(def$method (tty-dialog-mixin :babylon-read) (&optional special-keys)
 "reads in a character or a lisp form from dialog-stream. only
 those characters occurring in the list special-keys are read."

 (let ((char (emaxps-read)))
  (if (not (symbolp char))
   char   ;;return-value
  ;else
   (let (pos tmp tmplist)
    (setf tmp (format nil "~a" char))             ;; '! --> "!"
    (setf tmplist nil)
    (mapcar #'(lambda (x)
               (setf tmplist
                (append tmplist
                 (list (format nil "~a" x))))    ;; #\! --> "!"
              );lambda
              special-keys
    );mapchar
    ;; equalp   =  stricmp
    ;; string=  =  strcmp
    (if (not (setf pos (position tmp tmplist :test 'equalp)))
     char  ;;return-value
    ;else
     (nth pos special-keys)
    );if
   );let
  );if
 );let
)

;; read-char dialog-stream

;; HERE now the final check for the *end-key* can be realized
;; using the modified method :babylon-read !!!

(def$method (tty-dialog-mixin :confirm) (&rest comments)
 "writes comments to dialog-stream and waits for input.
 comments are printed without slashification.
 returns :yes if *end-key* is entered and nil otherwise."

 (lexpr-$send self :notify comments)
 (format dialog-stream "~:[~;~%~]     ~@? "
  (rest comments)
  (getentry type-end-to-confirm-str babylon-io-table)
  *end-key*
 );format
 (let ((char ($send self :babylon-read (list *end-key*))))
  (if (eql char *end-key*)
   :yes         ;;return-value
  ;else
   NIL          ;;return-value
  );if
 );let
)

;; read-char dialog-stream

;; HERE now the final check for the *end-key* can be realized
;; using the modified method :babylon-read !!!

(def$method (tty-dialog-mixin :type-end-to-continue) (string)
 "writes string to dialog-stream and waits till *end-key* is
 entered. returns :ok."

 (format dialog-stream "~%~A ~@? " *bab-prompt* string *end-key*)
 (loop
  (if (eql ($send self :babylon-read (list *end-key*)) *end-key*)
   (return :ok)    ;;return-value
  )
 );loop
)

;; -------------------------------------------------------
;; hg20080719: (say "xxx") gets into error, because it
;; tries writing into a closed terminal stream. On initialisation
;; the terminal-io stream of CLISP may be up, but later on it is
;; redirected to a pseudo TTY. This tries to correct the problem
;; in 2 steps:
;; -------------------------------------------------------

(def$method (tty-dialog-mixin :emaxps-init-dialogstream) (&rest plist)
 "since emaxps closes the default STDIO streams and redirects them to
 new ones, controlled by the gui process. the lisp interpreter process
 itself changes the settings of its special variables *terminal-io*, ...
 the babylon-2.3 copy of it in its global variable *default-dialog-stream*
 keeps unchanged -like the tty-dialog-mixin slot 'dialog-stream'!
 this method w/o args run after loading a KB corrects both values."

 (declare (ignore plist))
 ;; 1. update the global variable
 (setf *default-dialog-stream* *terminal-io*)
 ;; 2. update the slot value used by many methods
 (setf dialog-stream *default-dialog-stream*)
 ;; see also: fmcs/mcs-core.cl: 112: SET-SLOT-VALUE ...
)

;; eof

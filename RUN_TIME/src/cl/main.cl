;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     main.cl           Hans Groschwitz      01.12.94      ;;
;;                                                          ;;
;;     based on /.../kernel/meta/kb-core.cl                 ;;
;;     Adds IPC and KB management to Babylon 2.2            ;;
;;     Adds support of tasks to Babylon 2.2 !!!             ;;
;;     Defines EMA-XPS 2.0 KB format                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   (intern "xxx" :USER) works the same but should be prefered to
   (read-from-string "xxx")
|#

(in-package "BABYLON")

; ----------------------------------------------
; NOT a CLtL2 emulation! It is possible to work
; with CLtL2 delete-package, because it removes
; the *complete* package incl. the package name.
; Then defconstant is reintegrated into the NEW
; package, when loading a KB.
; Using CLtL1 the the Package remains existent!!!
; Here the definition of defconstant may not be removed!!!
; When reloading a KB, reintegrating defconstant is omitted
; ----------------------------------------------

(if (fboundp 'delete-package)

 (defun emaxps-delete-package (pname)
  "apply is used to avoid the compile time warning
   used-but-not-defined with CLtL1 interpreters"
  (apply 'delete-package (list pname)))

;; else

(defun emaxps-delete-package (pname)
 "Emulates the behavior of the CLtL2 delete-package Call, if not present!
 Returns T on success, NIL on failure"

 (unless (packagep pname)
  (setf pname (find-package pname)))
 (if (packagep pname)
  (let (lst (success T) str)
   ;;; remove all references to other packages
   (unuse-package (list-all-packages) pname)
   (do-symbols (x pname)
    (setf lst (cons x lst))
   );do-symbols
   (mapcar #'(lambda (x)
     (setf str (format nil "~S" x))
     (cond
      ((equalp str "defconstant")
;       (format t "~%emaxps-delete-package: ~S is DEFconstant -- not removed!" x)
       )
      ((constantp x)
;       (format t "~%emaxps-delete-package: ~S is constant -- not removed!" x)
       (setf success NIL))
      ((boundp x) 
;       (format t "~%emaxps-delete-package: ~S WAS a SYMBOL..." x)
       (makunbound x))
      ((fboundp x)
;       (format t "~%emaxps-delete-package: ~S WAS a FUNCTION/MACRO..." x)
       (fmakunbound x))
      ;;; OTHERWISE: already UNBOUND !!!
;      (t (format t "~%emaxps-delete-package: ~S was ALREADY UNbound." x))
     );cond
    );lambda
    lst
   );mapcar
   success  ;; return-value on success
  );let emu
 ;else
  NIL       ;; return-value on failure
 );if package ex.
)

);if ex. delete-package

; ----------------------------------------------
; Dummies
; ----------------------------------------------

(defun emaxps-compile-file (in-file out-file)
 "This is to be replaced by the definition in
 compile.cl, if an editable version is to be
 build!"

 (format t "~%WARNING: emaxps-compile-file is a DUMMY")
 (rename-file in-file out-file)
)

; ----------------------------------------------
; Debugging Tools
; ----------------------------------------------

(defmacro emaxps-real-time-ms (form &optional (tmin 0))
 `(let (ertms-result (ertms-time (get-internal-real-time))
        (ertms-varlist bqli-varlist))
   (setf ertms-result ,form)
   (setf ertms-time
    (round (* 1000 (/ (- (get-internal-real-time) ertms-time)
                         internal-time-units-per-second))))
   (when (>= ertms-time ,tmin)
    (format *trace-output* "~%;;; Form: ~S" ',form)
    (format *trace-output* "~%;;; Real time spent: ~S ms" ertms-time)
   );when
   ertms-result    ;; return-value
  );let
)

(defun emaxps-get-date ()
 "prints out the current date into a string in a way
 the UNIX command 'date' would do."

 (let ((lst (multiple-value-list (get-decoded-time))))
            ;; returns: (55 14 13 22 3 1995 2 NIL -1)
            ;;      sec min hour date month year
            ;;      day-of-week daylight-saving-time-p
            ;;      time-zone
  (format NIL "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~] ~
               ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
               ~S ~S:~2,'0D:~2,'0D ~S"
   (nth 6 lst)       ;day-of-week 0-6
   (1- (nth 4 lst))  ;month 1-12
   (nth 3 lst)       ;date
   (nth 2 lst)       ;hour
   (nth 1 lst)       ;min
   (nth 0 lst)       ;sec
   (nth 5 lst)       ;year
  );format
 );let
)

; ----------------------------------------------
; Miscelaneous (used in rule-set operations)
; ----------------------------------------------

(defun emaxps-symbol-to-key (symb)
 "Makes :XXX from 'XXX or :XXX which it returns."

 (let (res)   ;; remove multiple-values...
  (setf res
   (if (symbolp symb)
    (read-from-string (format nil ":~a" symb))
   ;else
    (emaxps-error "emaxps-symbol-to-key: arg must be a symbol")
   );if
  );setf
 res     ;;return-value
 );let
)

(defun emaxps-key-to-symbol (symb)
 "Makes 'XXX from :XXX or 'XXX which it returns."

 (let (res)   ;; remove multiple-values...
  (setf res
   (if (symbolp symb)
    (read-from-string (format nil "~a" symb))
   ;else
    (emaxps-error "emaxps-key-to-symbol: arg must be a symbol")
   );if
  );setf
 res     ;;return-value
 );let
)

; ----------------------------------------------
; File system interface
; ----------------------------------------------

(defun emaxps-temp-file-name ()
 "Creates a unique temporary filename
 CLISP: compile-file cannot accept a filename without
 extension !!!"

 (let ((*print-case* :downcase))
  (format nil "/tmp/~A~A.tmp"
          (emaxps-current-kb-name)  ;; NIL, if no KB current!
          (get-universal-time)
  );format
 );let
)

(defun emaxps-remove-file (filename)
 "Remove the file named by the filename string"

 (delete-file filename)
)

; ----------------------------------------------
; Process Control
; ----------------------------------------------
 
(defun emaxps-chdir (&optional (path-string nil))
 "Per LISP-Vendor varying call. Changes (or prints)
 the current working directory of this LISP-process.
 Paths may be absolute or relative. Returns the path
 of the new cwd. Vendor diffs are now in impl-dep.cl"

 (if path-string
  (excl:chdir path-string)
 ;else
  (excl:current-directory)
 );if
)

; ----------------------------------------------

(defun emaxps-system (&optional (command-string nil) &key (wait T))
 "Per LISP-Vendor varying call. If called with a string,
 it works like the C-function system(), runs a command-
 line interpreter, which interprets the command-string.
 Otherwise it enables interactive communication with the
 interpreter. Uses the *terminal-io* stream to communicate
 with the interpreter. Returns the value given from system().
 Vendor diffs are now in impl-dep.cl"

 (if command-string
  (excl:run-shell-command command-string :wait wait)
 ;else
  (excl:shell)
 );if
)

; -------------------------------------------
; Handles KB informations requested by C
; for FileLoad/FileSave/FileSaveAs/FileClose
; Managed parallel to *known-knowledge-bases*
; (
;  (kbname path filter modified)
;  (kbname path filter modified)
;  (kbname path filter modified)
;  (kbname path filter modified)
; )
; -------------------------------------------

(defvar *emaxps-kb-data* nil)

(defun emaxps-kb-data-remove (item)
 "Used by (emaxps-close-kb),
 (emaxps-load-kb) and (emaxps-save-kb)."

 (let ((tl nil))
  (mapcar #'(lambda (x)
             (if (not (equal (first x) item))
              (setf tl (append tl (list x)))))
          *emaxps-kb-data*)
  (setf *emaxps-kb-data* tl)
 );let
 NIL      ;;return-value
)

(defun emaxps-kb-data-add (item-list)
 "Used by (emaxps-load-kb) and (emaxps-save-kb)
 in case of SaveAs..."

 ;; To avoid inconsistent double entries...
 (emaxps-kb-data-remove (first item-list))
 (setf *emaxps-kb-data*
  (append *emaxps-kb-data* (list item-list)))
 NIL      ;;return-value
)

(defun emaxps-kb-set-modified (item)
 "Used by KB editors after saving to LISP."

 (let (il)
  (setf il (emaxps-kb-data-read item))
  (if il
   (progn
    (setf il (reverse (rest (reverse il))))
    (setf il (append il '(T)))
    (emaxps-kb-data-add il)
    (emaxps-kbs-state)   ;; Now the kb-data is updated!
    NIL       ;;return-value
   );progn
  ;else
   :ERROR     ;;return-value
  );if
 );let
)

(defun emaxps-kb-reset-modified (item)
 "Used by (emaxps-save-kb) after saving to file."

 (let (il)
  (setf il (emaxps-kb-data-read item))
  (if il
   (progn
    (setf il (reverse (rest (reverse il))))
    (setf il (append il '(NIL)))
    (emaxps-kb-data-add il)
    (emaxps-kbs-state)   ;; Now the kb-data is updated!
    NIL       ;;return-value
   );progn
  ;else
   :ERROR     ;;return-value
  );if
 );let
)

(defun emaxps-kb-data-read (item)
 "Used by (emaxps-kbs-state) and
 (emaxps-kb-set-modified)."

 (let ((il nil))
  (mapcar #'(lambda (x)
             (if (equal (first x) item)
              (setf il x)))
          *emaxps-kb-data*)
  il   ;;return-value 
 );let
)

; -------------------------------------------
; Places IPC into B22
; -------------------------------------------

(defun emaxps-kbs-state ()
 "Informs the C-Process of the number and name of the
 currently known KBs and whether one of them is current.

 emaxps-begin-kb-data-xmit --> channels.cl
 emaxps-separate-mparts, emaxps-send-mbody,
 emaxps-end-of-message --> ui.cl"

 (let (pp tmplist)
  (emaxps-begin-kb-data-xmit)
  (setf pp (position (emaxps-current-kb-name) *known-knowledge-bases*))
  (if pp
   (emaxps-send-mbody "~A" (+ pp 1))
  ;else
   (emaxps-send-mbody "0")     ;; no current KB !!!
  );if
  (emaxps-separate-mparts)
  ;; Anzahl der zu uebertragenden Quadrupel:
  (emaxps-send-mbody "~A" (length *known-knowledge-bases*))

  (mapcar #'(lambda (x)
             (setf tmplist (emaxps-kb-data-read x))
             ;;tmplist=  (kbname path filter modified)
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" (first tmplist))
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" (second tmplist))
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" (third tmplist))
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" (fourth tmplist))
            );lambda
          *known-knowledge-bases*
  );mapcar
  (emaxps-end-of-message)
 );let
 NIL  ;;return-value
)

(def$method (kb-processor-core :after :make-yourself-known) ()
 "Used after loading a new KB into the LISP-world."

 (emaxps-kbs-state)
 NIL  ;;return-value
)

(def$method (kb-processor-core :after :make-yourself-unknown) ()
 "Used after killing a KB from LISP-world."

 (emaxps-kbs-state)
 NIL  ;;return-value
)

(def$method (kb-processor-core :after :make-yourself-current) ()
 "Used after loading a new KB or selecting another one."

 (emaxps-kbs-state)
 NIL  ;;return-value
)

; -------------------------------------------
; Add a new KB statistics methode
; -------------------------------------------

(defun emaxps-current-kb-name ()
 "Finds out the symbolic representation of the current KB."

 (if (not *current-knowledge-base*)
  NIL       ;;return-value
 ;else
  (let ((kbname nil))
   (mapcar #'(lambda (x)
              (if (equal (symbol-value x) *current-knowledge-base*)
               (setf kbname x)))
           *known-knowledge-bases*)
   kbname   ;;return-value
  );let
 );if
)

(def$method (kb-processor-core :emaxps-kb-statistics) ()
 "Sends a Message to C to request a specialized Popup."

 (let ((kbname (emaxps-current-kb-name)))
  (when kbname
   (emaxps-information :kb-statistics
    kbname
    (length ($send self :frames))
    (length ($send self :instances))
    (eval (cons '+ (mapcar #'length ($send self :rules))))
    (length ($send self :rules))
    (length (the list (get ($send self :relations-name) 'clauses)))
    (length ($send self :constraints))
    (length ($send self :constraint-nets))
    (length ($send self :restriction-nets))
    (length (emaxps-send-task :tasks))
   )
  );when
 );let
)

; -------------------------------------------
; Commands sent from C to LISP
; -------------------------------------------

(defvar *emaxps-current-kb-load-path* "/")

(defun emaxps-initialize (initfile autofile filter autorun)
 "This is called only once at the end of initialization
 by the C process, which checked existance and readablility 
 of those files! See main.c"

 (when initfile
  (format t "~%;;; Loading USER's init-file ~S" initfile)
  (load initfile)
  (format t "~%;;;")
 );when
 (when autofile
  (format t "~%;;; Auto-loading knowledge base ~S" autofile)
  (emaxps-load-kb autofile filter)
  (format t "~%;;;")
  (when autorun
   (format t "~%;;; Running knowledge base ~S" (emaxps-current-kb-name))
   (emaxps-run-session)
  );when
 );when
)

(defun emaxps-run-session ()
 "(Re)Initializes the current KB (if any) and starts it.
 If no current KB ex. it returns :ERROR, else NIL."

 (if (not *current-knowledge-base*)
  :ERROR       ;;return-value
 ;else
  (progn
   (ui-stop-session)
   (send-kb :reset-kb)
   (instructions (dotask 'main))
   (send-kb :start-kb)
   NIL         ;;return-value
  );progn
 );if
)

(defun emaxps-kb-create-new ()
 "Interactively creates a new KB."

 (catch 'emaxps-load-kb-tag
  (let ((answer (emaxps-prompt :kb-input-name)))
   (unless answer
    (throw 'emaxps-load-kb-tag :ERROR))  ;; button 'Abort' pressed!
   (if (and (symbolp answer) (not (keywordp answer))
            (not (member answer *known-knowledge-bases*)))
    (progn
     ;; init's KB-data, too (LISP and C) !!!
     (eval (list 'emaxps-make-kb answer))   ;; Do it!
     answer    ;; return-value
    );progn
   ;else
    (progn
     (emaxps-warning :kb-already-exists
      answer *known-knowledge-bases*)
     :ERROR  ;; return-value
    );progn
   );if
  );let
 );catch
)

(defun emaxps-load-kb (filename filter)
 "Loads a new KB into the LISP-world or overwrites an existing one.
 If loading failed, it returns :ERROR, else NIL."

 (let (dir file)
  (block READ-KB
   (unless (and (stringp filename)
                (> (length filename) 0)
           );and
    (emaxps-warning :kb-read-bad-file filename)
    (return-from READ-KB :ERROR)
   );unless
   (setf dir (directory-namestring filename))
   (unless (directory dir)
    (emaxps-warning :kb-read-bad-dir dir)
    (return-from READ-KB :ERROR)
   );unless
   (setf file (pathname-name filename))
   (unless file
    (emaxps-warning :kb-read-no-file-part filename)
    (return-from READ-KB :ERROR)
   );unless
   (unless (open filename :direction :probe)
    (emaxps-warning :kb-read-file-not-ex filename)
    (return-from READ-KB :ERROR)
   );unless

   ;;;;;;
   ;; Now all checking is done, let's do it!
   ;;;;;;
   (catch 'emaxps-load-kb-tag
    ;; this is used by B3 include-lisp-file only...
    (setf *emaxps-current-kb-load-path* filename)

    ;; DE-INSTALL evalhook!
    ;;   B3 will install a new one during load time...
    (let ((*evalhook* NIL))
     (load filename)
    );let

    ;; if kb-load was successful, store informations
    ;; else: throw would not have let it run to here!
    (emaxps-kb-data-add (list (emaxps-current-kb-name) filename filter nil))
    (emaxps-kbs-state)   ;; Now the kb-data is updated!

    ;; Used by B3 KBs (constraints) ...
    (let ((afters (emaxps-send-task :after-load)))
     (when afters (eval afters))
    );let

    (emaxps-current-kb-name)  ;;return-value
   );catch

  );block
 );let
)

(defun emaxps-save-kb (compiled filename filter)
 "Saves the current KB into a KB-file. If no KB is current or
 writing to the file fails, it returns :ERROR, else NIL."

 (let ((save-as T) (save-mode :warn) (kbname (emaxps-current-kb-name)))
  (if kbname

   (let (ret)
    (unless filename
     ;;; update filename from kb-data (here in SAVE):
     (setf filename (second
       (emaxps-kb-data-read kbname)
     ))
     (setf save-mode :no-warn)
     (setf save-as NIL)
    );unless
    (setf ret (emaxps-write-kb filename save-mode compiled))
    (when (equal ret T)  ;; success...

     (when save-as
      ;;; update kb-data (here in SAVE-AS):
      (emaxps-kb-data-add (list kbname filename filter NIL))
     );when

     ;;; set kb to be not modified (LISP side only)!
     ;;; and inform C-side !
     (emaxps-kb-reset-modified kbname)

    );when success
    ret  ;; return-value 
   );let

  ;else
   :ERROR       ;;return-value
  );if
 );let
)

(defun emaxps-close-kb ()
 "Kills the current KB from the LISP-world. If no KB is current,
 or the user decided to abort, it returns :ERROR, else NIL."

 (let ((ckbn (emaxps-current-kb-name)))
  (ui-stop-session)   ;; if currently doing a session...
  (emaxps-kb-data-remove ckbn)
  (send-kb :kill-kb)
  ;;;
  ;; Problem: global frame definitions in B3 are MCS instances, too.
  ;; Emulation does a (defvar x 'x) here in the package of the KB!
  ;; When reloading a KB those variables exist yet and seem to 
  ;; confuse defframe's ?
  ;; KB load time becomes very LONG!
  ;;;
  (emaxps-delete-package ckbn)  ;; speeds up close-kb / reload-kb!
 );let
 NIL  ;;return-value
)

(defun emaxps-kb-statistics ()
 "Sends information on the current KB to C. If no current KB ex.
 it returns :ERROR, else NIL."

 (if (not *current-knowledge-base*)
  :ERROR       ;;return-value
 ;else
  (progn
   (send-kb :emaxps-kb-statistics)
   NIL         ;;return-value
  );progn
 );if
)

(defun emaxps-select-kb (kbname)
 "Selects another KB to be the current one. If kbname is not a 
 known KB, it returns :ERROR, else NIL."

 ($send (symbol-value kbname) :make-yourself-current)
 NIL  ;;return-value
)

; ----------------------------------------------
; This command is placed first within a KB-file
; ----------------------------------------------

(defconstant *emaxps-bad-kbnames*
 '(t nil lisp common-lisp user common-lisp-user cl-user keyword
   sys system mcs clos flavors bul babylon emaxps ema-xps

   excl   ;; B3 uses AllegroCL extensions ...
  )
)

(defmacro emaxps-make-kb (kbname)
 "Makes a new KB instance if it does not exist or asks the
 user whether to overwrite existing KB objects. If the user
 denies, it throws loading and returns at the catch-point
 in emaxps-load-kb, which in advance returns :ERROR, else
 emaxps-make-kb returns NIL."

 `(let ((kb ',kbname))
   (unless (and kb (symbolp kb) (not (keywordp kb)))
    (emaxps-warning :kb-name-not-symbol kb)
    (throw 'emaxps-load-kb-tag :ERROR)  ;;return-value of load-kb!
   );unless

   (setf kb (read-from-string (format nil "BABYLON::~S" kb)))
   ;;; The order is important here!
   (in-package "BABYLON")

   (when (member kb *emaxps-bad-kbnames*)
    (emaxps-warning :kb-bad-name kb *emaxps-bad-kbnames*)
    (throw 'emaxps-load-kb-tag :ERROR)  ;;return-value of load-kb!
   );when

   (when (member kb *known-knowledge-bases*)
    ;; KB already exists!
    (unless (emaxps-accept-cancel :kb-replace-contents-p kb)
     (throw 'emaxps-load-kb-tag :ERROR)) ;;return-value of load-kb!

     ;; KILL KB before RE-CREATEing it:
     (emaxps-select-kb kb)
     (emaxps-close-kb)
   );when

   (emaxps-def-kb-instance kb)    ;;return-value = KBNAME
  );let
)

; ----------------------------------------------
; Auxiliary Tool -- for emaxps-make-kb ONLY!
; ----------------------------------------------

(defun emaxps-def-kb-instance (kb)
 (let (kbname (path (namestring (emaxps-chdir))))

  (eval (list 'def-kb-instance kb 'emaxps-cfg
              :language (list 'quote 'english) ;;; 'german
  ));eval

  (setf kbname (emaxps-current-kb-name))

  (emaxps-kb-data-add (list kbname
                            path   ;; filename is incomplete!
                            (concatenate 'string path "*")     
                            nil))
  (emaxps-kbs-state)   ;; Now the kb-data is updated!

  kbname    ;; return-value
 );let
)

; ----------------------------------------------
; Write all the KB constructs to a file
; ----------------------------------------------

(defun emaxps-write-kb (filename no-warn compiled)
 "Writes all constructs known to the EMA-XPS-Editors to
 the file filename, if possible
 -- else a warning popup is displayed!"

 (let (dir file tmpfilename)
  (catch 'WRITE-KB
   (unless *current-knowledge-base*
    (emaxps-warning :kb-write-not-current)
    (throw 'WRITE-KB :ERROR)
   );unless
   (unless (and (stringp filename)
                (> (length filename) 0)
           );and
    (emaxps-warning :kb-write-bad-file filename)
    (throw 'WRITE-KB :ERROR)
   );unless
   (setf dir (directory-namestring filename))
   (unless (directory dir)
    (emaxps-warning :kb-write-bad-dir dir)
    (throw 'WRITE-KB :ERROR)
   );unless
   (setf file (pathname-name filename))
   (unless file
    (emaxps-warning :kb-write-no-file-part filename)
    (throw 'WRITE-KB :ERROR)
   );unless
   (unless (equal no-warn :NO-WARN)
    ;; save as...
    (when (open filename :direction :probe)
     (unless
      (emaxps-accept-cancel :kb-overwrite-file-p filename)
      (throw 'WRITE-KB NIL)   ;; return-value (not written)
     );unless
    );when
   );unless

   ;;;;;;
   ;; Now all checking is done, let's do it!
   ;;;;;;

   (if (equal compiled :COMPILED)
    (setf tmpfilename (emaxps-temp-file-name))
   ;else
    (setf tmpfilename filename)
   );if compiled

   (with-open-file (stream tmpfilename
                    :direction :output
                    :if-exists :rename-and-delete
                    :if-does-not-exist :create)
    (if stream   ;; (success)
     (emaxps-write-kb-contents stream)
    ;else        ;; (failure)
     (progn
      (emaxps-warning :kb-write-open-failed tmpfilename)
      (throw 'WRITE-KB :ERROR)
     );progn
    );if
   );with-open-file

   (when (equal compiled :COMPILED)
    (emaxps-compile-file tmpfilename filename)
   );when compiled

   T ;; return-value of catcher WRITE-KB (success = written)
  );catch
 );let
)

; ----------------------------------------------
; Write the KB constructs to an open file
; ----------------------------------------------

(defun emaxps-write-kb-contents (stream)
 "Writes all constructs known to the EMA-XPS-Editors to
 the stream stream.
 NO checking is performed here!! For internal use ONLY!"

 (let ((*print-case* :upcase) kbname)
  (setf kbname (format NIL "~S" (emaxps-current-kb-name)))

  ;; File Header:
  (format stream
   "~%;; -----------------------------------------------------------------~
    ~%;; ~
    ~%;;    This is the export of a knowledge base for the~
    ~%;;    Graphic Expert System Shell ~A~
    ~%;;    Copyright (c) 1989-2008 Hans Groschwitz~
    ~%;;    THE KNOWLEDGE BASE ITSELF IS NOT COVERED BY THIS COPYRIGHT NOTICE.
    ~%;;    For details read the files 'COPYRIGHT' and 'GNU-GPL', which
    ~%;;    include the DISCLAIMER of the expert system shell software.
    ~%;; ~
    ~%;;          File Write Date:     ~A~
    ~%;;          Knowledge Base:      ~A~
    ~%;; ~
    ~%;; -----------------------------------------------------------------~
    ~%~%(emaxps-make-kb ~A)~%" *emaxps-version* (emaxps-get-date) kbname kbname)

  ;;;;;; Frames, Behaviors and Instances...
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Frames, Instances, Behaviors:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  ($send (send-kb :frame-processor)
         :emaxps-print-frames-behaviors-instances stream)

  ;;;;;; Rulesets and Rules...
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Rule Sets:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-rule :emaxps-print-all-rsets stream)

  ;;;;;; Prolog Clauses...
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Fixed Clauses:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-kb :emaxps-print-fixed-clauses stream)
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Free Clauses:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-kb :emaxps-print-used-free-clauses stream)

  ;;;;;; Constraints und Restrictions...
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Primitive Constraints:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-kb :emaxps-print-all-primitive-constraints stream)
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Constraint Networks:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-kb :emaxps-print-all-constraintnets stream)
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Restriction Networks:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (send-kb :emaxps-print-all-restrictions stream)

  ;;;;;; Tasks and Others...
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Tasks:~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (emaxps-send-task :print-tasks stream)
  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    Other Miscelaneous Constructs (not handled above):~
    ~%;; -----------------------------------------------------------------~
    ~%")
  (emaxps-send-task :print-others stream)

  (format stream
   "~2%;; -----------------------------------------------------------------~
    ~%;;    EOF.~
    ~%")
 );let
)

;; =======================================================
;; Management of axiom-sets belonging to the KB-file
;; (send-kb :relations-name)  --> name of the filed clause
;; (send-prolog :axioms)  --> List of all USED axsets of KB !
;; =======================================================

(defmacro emaxps-defclauses (name &rest args)
 `(progn
   (defclauses ,name ,@args)
  );progn
)

;; -------------------------------------------------------

(defmacro emaxps-defaxiom-set (name &rest args)
 `(progn
   (defaxiom-set ,name ,@args)
   (send-kb :add-axiom-set ',name) 
  );progn
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     Add support of TASKs to Babylon 2.2                     ;;
;;                                                             ;;
;;     Hence Babylon 2.2 does not include a procedural         ;;
;;     programming interface except the instructions block.    ;;
;;     This is an alternative approach which replaces          ;;
;;     (instructions ...) by a task named 'main.               ;;
;;     If using functions/macros AND loading two or more KBs   ;;
;;     parallel into the LISP-World, duplicate fnames might    ;;
;;     overwrite older fdefinitions. This leads to unexpected  ;;
;;     behavior at session runtime!!!                          ;;
;;     This concept of TASKs separates the taskname from it's  ;;
;;     functional body (which in fact is a lambda-list).       ;;
;;     TASKs are stored per KB in new slots of the             ;;
;;     kb-processor-core frame. Additional, tracing of TASKs   ;;
;;     can be enabled this way. If a KB is removed from the    ;;
;;     list of known KBs, all Task-bodies and their names are  ;;
;;     removed, too. Removing of defun's would be complicated  ;;
;;     and, in fact, might erase later definitions of other    ;;
;;     KBs, too.                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- nth-remove ---

(defun nth-remove (pos lst)
 "Operates on nested Lists of Lists in spite of set-manipulating functions"
 (append
  (reverse (nthcdr (- (length lst) pos) (reverse lst)))
  (nthcdr (+ 1 pos) lst)
 )
)

; ---------------------------------------------------------------
; This is the EMA-XPS task processor, necessary to handle
; support of tasks to kb-processor-core.
; ---------------------------------------------------------------

(def$flavor emaxps-task-processor
 ((tasks nil)
  (task-bodies nil)
  (task-names nil)
  (trace-enabled nil)
  (within-task-main nil)   ;; set only as long as task MAIN is run
  ;;;
  (others nil)   ;; holds an progn of all remaining AI-constructs !!!
  ;;;
  (after-load nil)  ;; will be eval'ed after loading a KB (used by B3 constraints)
 )
 ()
 (:documentation "This flavor represents the emaxps task processor.")
)

(def$method (emaxps-task-processor :append-after-load) (do-it)
 "appends one LISP expression to the list, which will be eval'ed after Loading
 a KB. This feature is used by B3 define-constraint, which builds only one
 restriction NETWORK. READ-FROM-STRING is used to workaround the PACKAGE
 problem"

 (if after-load
  (setf after-load (append after-load (list do-it)))
 ;else
  (setf after-load (list (read-from-string "progn") do-it))
 );if
 T    ;;return-value
)

(def$method (emaxps-task-processor :append-others) (misc-constructs)
 "adds misc-constructs to the list of other constructs known in the current
 KB. Remember that :set-others is a standard method for a slot..."

 (setf others (append others misc-constructs))
 T  ;; return-value
)

(def$method (emaxps-task-processor :reset-tasks) ()
 "resets the tasks/task-bodies variables, but not the trace flag."

 (if tasks
  (progn
   ($send self :remove-task (first tasks))
   ($send self :reset-tasks)  ;;recoursive!
  );progn
 ;else
  (progn
   (setf others NIL)
   (setf after-load NIL)
  );progn
 );when
)

(def$method (kb-processor-core :before :kill-kb) ()
 "This is to remove the functional bodies of the tasks of this
 kb to clean th KB package!"

 (emaxps-send-task :reset-tasks)
)

(def$method (emaxps-task-processor :get-body) (tname) 
 "returns the body of a task, if known in the current KB, else NIL."

 (let ((pos (position tname tasks)))
  (if pos
   (nth pos task-bodies)   ;; return-value (success)
  ;else
   NIL                     ;; return-value (failure)
  );if
 );let
)

(def$method (emaxps-task-processor :fname-of-task) (tname) 
 "returns the function-name of a task, if known in the current KB, else NIL."

 (let ((pos (position tname tasks)))
  (if pos
   (nth pos task-names)    ;; return-value (success)
  ;else
   NIL                     ;; return-value (failure)
  );if
 );let
)

(def$method (emaxps-task-processor :remove-task) (tname) 
 "removes a task, if known in the current KB."

 (let ((pos (position tname tasks)))
  (if pos
   (progn
    (fmakunbound (nth pos task-names))      ;; remove the function, too!
    (setf task-names (nth-remove pos task-names))
    (setf task-bodies (nth-remove pos task-bodies))
    (setf tasks (nth-remove pos tasks))
    T               ;; return-value (success)
   );progn
  ;else
   ;; this was not a known task, hence do nothing...
   NIL              ;; return-value (failure)
  );if
 );let
)

(def$method (emaxps-task-processor :add-task) (tname body) 
 "adds/replaces a task, if known in the current KB."

 ($send self :remove-task tname)  ;; remove old definition, if exists...
 (setf task-bodies (append task-bodies (list body)))
 (setf tasks (append tasks (list tname)))
 (let ((fname
        (read-from-string
         (format nil "~A::%TASK%~A"
          (emaxps-current-kb-name) tname))))
  (setf task-names (append task-names (list fname)))
  (eval (append (list 'defun fname) (rest body)))   ;; install the function!
 );let
 NIL              ;; return-value
)

(defun emaxps-send-task (selector &rest args)
 "Send to current emaxps task-processor."

 (if *current-knowledge-base*
  (lexpr-$send (send-kb :task-processor) selector args)
 ;else
  :ERROR    ;;return-value
 );if
)

; ---------------------------------------------------------------

(def$method (emaxps-task-processor :toggle-task-trace) ()
 "toggles the trace state for emaxps tasks."

 (setf trace-enabled (not trace-enabled))
)

; ---------------------------------------------------------------
; This is the EMA-XPS task mixin, necessary to include
; support of tasks to kb-processor-core.
; ---------------------------------------------------------------

(def$flavor emaxps-task-mixin
 (task-processor)
 ()
 :settable-instance-variables
 (:required-instance-variables procs)
 (:documentation "This mixin makes the facilities of
   task-processor available.")
)

(def$method (emaxps-task-mixin :after :init) (&rest plist)
 (declare (ignore plist))
 ($send self :generate-task-processor)
 (setf procs (cons task-processor procs))
)

(def$method (emaxps-task-mixin :generate-task-processor) ()
 "generates an instance of task-processor associated with the kb."

 (setf task-processor (make-$instance 'emaxps-task-processor))
)

; ---------------------------------------------------------------
; for KB initialization
; ---------------------------------------------------------------

(defmacro deftask (name parameter &body do-it)
 "Defines a Task analogous to a defun, but does not
 include it to the current package's fsymbol-list."

 `(if *current-knowledge-base*
   (progn
    (emaxps-send-task :add-task
                      ',name
                      (append (list 'lambda ',parameter) ',do-it)
    )
    ',name      ;;return-value   (success)
   );progn
  ;else
   :ERROR       ;;return-value   (failure)
  );if
)

; ---------------------------------------------------------------
; Runtime-Module of tasks
; ---------------------------------------------------------------

(defun dotask (tname &rest args)
 "Starts a task, if known in the current KB, else (error).
 Returns the return-value of the task-body."

 (if *current-knowledge-base*
  (let ((fname (emaxps-send-task :fname-of-task tname))
        (trace (emaxps-send-task :trace-enabled)) ret)
   (unless fname
    ;; task not found
    (emaxps-error "Task ~A unknown to KB ~A" tname (emaxps-current-kb-name))
   );unless
   (when trace
    (emaxps-begin-message :trace-message)
    (emaxps-send-mbody "TSK: Begin task ~a with args ~s" tname args)
    (emaxps-end-of-message)
   );when
   (setf ret (apply fname args))
   (when trace
    (emaxps-begin-message :trace-message)
    (emaxps-send-mbody "TSK: Return from task ~a with ~s" tname ret)
    (emaxps-end-of-message)
   );when
   ret    ;;return-value
  );let
 ;else
  :ERROR  ;;return-value
 );if
)

(def$method (kb-processor-core :before :start-kb)
 (&rest dummy)
 "added to correctly handle task processor's :within-task-main !"

 (declare (ignore dummy))
 ($send self :emaxps-init-dialogstream)   ;; hg20080719 added: cl/inf-eng.cl 219
 (emaxps-send-task :set-within-task-main T)
)

(def$method (kb-processor-core :after :start-kb)
 (&rest dummy)
 "added to correctly handle task processor's :within-task-main !"

 (declare (ignore dummy))
 (emaxps-send-task :set-within-task-main NIL)
)

; ==============================================
;  Procedures for storing into a KB file
; ==============================================

(def$method (emaxps-task-processor :print-tasks) (stream)
 "prints all tasks to stream.
 BUG: tname should be :upcase !"

 (let ((*print-pretty* T) (*print-case* :downcase)
       tmp (len (length tasks)) (i 0))
  (loop (if (>= i len) (return))
   (setf tmp (cons 'deftask (cons (nth i tasks)  ;tname!
      (rest (nth i task-bodies))
   )))
   (print tmp stream)
   (terpri stream)
   (setf i (1+ i))
  );loop
 );let
)

; ----------------------------------------------

(def$method (emaxps-task-processor :print-others) (stream)
 "prints the List of other misc constructs to stream."

 (let ((*print-pretty* T) (*print-case* :downcase))
  (when others
   (format stream "~%(emaxps-define-others~%")
   (mapcar #'(lambda (x)
              (print x stream)
              (terpri stream)
             );lambda
           others
   );mapcar
   (format stream "~%);done~%")
  );when
 );let
)

; ----------------------------------------------
; This is placed into the KB to evaluate all
; those AI-constructs not handled in one of the
; other specialized EDITORs ... It is evaluated
; once at KB-load-time AND in case of B3 KBs a
; second time when postprocessing behaviors!
; ----------------------------------------------

(defmacro emaxps-define-others (&rest do-it)
 `(progn
   ,@do-it    ;; at first do it, then append it for the misc-editor!

   (emaxps-send-task :append-others ',do-it)
  );progn
)

; =======================================================
; Extension to the RuleProcessor...
; Used by the RuleEditor AND the B3 LoadTime Interface!
; =======================================================

(def$method (rule-base :emaxps-make-rule) (rname rsetname rbody)
 "Modifies existing rules, creates new rules in new rulesets."

 (let (old-rule (new-rule (cons rname rbody)))
  (check-rule-syntax new-rule)
  (if (assoc rsetname rules)
   ;; ruleset already exists!
   (if (setf old-rule ($send self :get-rule rsetname rname))
    ;; see :modify-rule
    (setf (rest old-rule) rbody)
   ;else  ;; new rule!
    ($send self :add-rule new-rule ($send self :get-rule-set rsetname))
   );if
  ;else
   (eval (list 'defrule-set rsetname new-rule))
  );if
 );let
)

; =======================================================
; System Popup Dialogs of EMA-XPS Main-Menu ...
; =======================================================

(defun emaxps-warning (code &rest args)
 (let (no)
  (setf no
   (case code
    (:kb-already-exists      0)   ;;  2 args
    (:kb-read-bad-file       1)   ;;  1 arg
    (:kb-read-bad-dir        2)   ;;  1 arg
    (:kb-read-no-file-part   3)   ;;  1 arg
    (:kb-read-file-not-ex    4)   ;;  1 arg
    (:kb-name-not-symbol     5)   ;;  1 arg
    (:kb-bad-name            6)   ;;  2 args
    (:kb-write-not-current   7)   ;;  0 args
    (:kb-write-bad-file      8)   ;;  1 arg
    (:kb-write-bad-dir       9)   ;;  1 arg
    (:kb-write-no-file-part 10)   ;;  1 arg
    (:kb-write-open-failed  11)   ;;  1 arg
    (otherwise
     (emaxps-error "emaxps-warning: unknown message code ~S" code))
   );case
  );setf no
  (apply 'sysd-warning (cons no args))
 );let
)

; ----------------------------------------------

(defun emaxps-information (code &rest args)
 (let (no)
  (setf no
   (case code
    (:kb-statistics          0)   ;; 10 args
    (otherwise
     (emaxps-error "emaxps-information: unknown message code ~S" code))
   );case
  );setf no
  (apply 'sysd-information (cons no args))
 );let
)

; ----------------------------------------------

(defun emaxps-accept-cancel (code &rest args)
 (let (no)
  (setf no
   (case code
    (:kb-overwrite-file-p    0)   ;; 1 arg
    (:kb-replace-contents-p  1)   ;; 1 arg
    (otherwise
     (emaxps-error "emaxps-accept-cancel: unknown message code ~S" code))
   );case
  );setf no
  (apply 'sysd-accept-cancel (cons no args))
 );let
)

; ----------------------------------------------

(defun emaxps-prompt (code &rest args)
 (let (no)
  (setf no
   (case code
    (:kb-input-name          0)   ;; 0 args
    (otherwise
     (emaxps-error "emaxps-prompt: unknown message code ~S" code))
   );case
  );setf no
  (apply 'sysd-prompt (cons no args))
 );let
)

;; eof

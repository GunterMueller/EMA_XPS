#|*****************************************************************
*    b3-blisp.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the babylon-3 Babylon LISP commands    *
*                                                                 *
*   babylon-3 avoids using the LISP package. In spite it offers   *
*   this subset of functions. Most of them are in fact suited in  *
*   the LISP package. They are only referenced here!              *
*   Creates the package :BUL -- Babylon LISP                      *
*        (to be used in spite of :LISP in B3-emulated KBs)        *
*                                                                 *
*   B3 Reference Manual, Chapter 9                                *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")

;; -------------------- Compatibility Add Ons ---------------------

;; --- %object-of ---

(defun %object-of (arg)
 "B2 Emulation of B3's object-of!
 USED by IS-NEARLY !!!
 Warning: B3-Frames are handled as objects, too!"

 (if (stringp arg) (setf arg (read-from-string arg)))
 (if (and (symbolp arg)
          ;; frames.cl --> find-symbol in package of KB
          (%get-object-name arg)
     )
  (get-instance arg)
 ;; else
  arg
 )
)

;; --- %instance-p ---

(defun %instance-p (inst)
 "frame-processor is an mcs-object as any instance!
 Even UN-named instances are found correctly this way!"

;; (typep inst (type-of (send-kb :frame-processor)))

 ;;; from fmcs/mcs-map.cl ... (faster!)
 (typep inst 'fmcs::mcsobject)
)




;------------------------------ :BUL ------------------------------

;; Create the new Package :BUL (if not ex.)
;;
(unless (find-package "BUL")
 ;;;;;
 ;; Allegro CL 4.1 Flaw: this doesn't work with compile-file
 ;; Hence this operation is interpreted in make-ema-xps.cl
 ;;
 (make-package "BUL")
);unless

;; and make :BUL the current package with
;; :LISP-package used yet!
;;
(in-package "BUL")

;;-----------------------IMPORTS------------------------

;; Currently no package :EMA-XPS and :BABYLON exist
;; from :BABYLON needed:  is-frame send-kb $send ...
;;
;hg20080706 old: import new: shadowing-import
(shadowing-import
 '(babylon::is-frame babylon::send-kb babylon::$send
   babylon::is-instance babylon::%object-of
   babylon::EMAXPS-READ babylon::EMAXPS-READ-LINE
   babylon::%instance-p babylon::emaxps-error
  )
)




;;------------------ERRONEOUS CONDITIONS----------------

(mapcar #'(lambda (x)
           (if (fboundp (read-from-string (format nil "COMMON-LISP::~A" x)))
            (import x)
           ;else
            (emaxps-error "B3-LISP: unknown command: ~A" x)
           );if
          );lambda
        '(

 ;; known CommonLISP commands with the same power or better
 ;;
 alpha-char-p digit-char-p alphanumericp char= char/= char< char>
 char<= char>= char-code code-char char-upcase char-downcase complex
 realpart imagpart numerator denominator + - * / = <= < > >= /=
 floor ceiling round float conjugate abs sin cos tan asin acos atan
 sqrt exp expt min max let let* setf push pop defun
 function length search position position-if count count-if remove
 remove-if remove-duplicates substitute substitute-if reverse sort
 fill make-string string char string= string< string> string<=
 string>= string/= string-trim string-left-trim string-right-trim
 string-upcase string-downcase string-capitalize make-list list
 list* cons copy-list copy-tree first second third fourth fifth rest
 nth butlast tailp member member-if append revappend adjoin getf remf
 listp stringp characterp symbolp numberp integerp floatp complexp
 streamp null and or not progn cond if when unless case loop return
 format unwind-protect upper-case-p lower-case-p

 ;; added to simplify package-handling:
 ;;
 in-package use-package unuse-package import shadow shadowing-import

        )
);mapcar

;;---------------------CMD UNKNOWN WARNINGS----------------

(mapcar #'(lambda (x)
           (if (fboundp (read-from-string (format nil "COMMON-LISP::~A" x)))
            (import x)
           ;else
            (format t "~%WARNING: unexpectedly UNknown command: ~A" x)
           );if
          );lambda
        '(

 ;; support not granted in Babylon LISP:
 ;;
 trace untrace step time describe dribble apropos load
 ;;
 inspect ;; CLISP-BUG: unknown!!!

        )
);mapcar

;;----------------CMD/SYMBOL KNOWN WARNINGS-------------------

(mapcar #'(lambda (x)
           (let ((s (read-from-string (format nil "COMMON-LISP::~A" x))))
            ;;
            ;; If not using LISP-Package, (see exporting below!)
            ;; This should be solved using shadow...
            ;; ALLEGRO: without-package-locking!!!
            ;;
            (when (fboundp s)
             (fmakunbound s)
             (format t "~%WARNING: unbinding unexpectedly KNOWN command: ~A" x)
             (shadow x "COMMON-LISP")
            );when
            (when (boundp s)
             (makunbound s)
             (format t "~%WARNING: unbinding unexpectedly KNOWN symbol: ~A" x)
             (shadow x "COMMON-LISP")
            );when
           );let
          );lambda
        '(
 ;; commands only known to Babylon LISP:
 ;;
 ;; lambda  <-- implicitly known within LISP!
 ;;
 log10 ln defglobal complement-test
 replace-sequential copy-string append-string nthrest last-n sublist
 tree-substitute tree-substitute-if pcons sequencep ratiop is-type-of
 the-type-of is-exactly is-same is-nearly return-result
 with-open-input-file with-open-output-file print-form read-form
 read-one-line
 ;;
 substring    ;; CLISP-ADD-ON !!!          (command)
 for          ;; CLtL2 loop extension!     (symbol)
        )
);mapcar








;;--------------------Babylon LISP EMULATION-----------------------

;; --- defconstant ---

;; IMPORTANT:
;; The macro defconstant causes a KB-load-time interrupt, because
;; it is treated special while loading and/or compiling!
;; Redefining defconstant causes a boottime interrupt.
;; Shadowing it before redefining it solves the problems.
;; Only 3TO2 translated KBs run in their own package. There shadowing
;; has to ba done, too.
;; See below and in b3-misc.cl, too !!!
;;
;; This should be improved by a clean PACKAGE-handling!
;;
(shadow 'defconstant)

(defmacro defconstant (var val)
 "Here the :LISP version may NOT be used
 (this would cause a KB-loadtime interrupt)!"

 `(progn
   (defvar ,var NIL)   ;; ,val muesste sonst eine Konstante sein (keine Evaluation ?!?)
   (setf ,var ,val)
  );progn
)

;; temporary, unless unuse-package :LISP...
;; 
(shadowing-import 'BUL::defconstant "BABYLON")   ;; home package of the KBs...






;; --- defglobal ---

(defmacro defglobal (var val)
 "Defines a globally known var
 (in the Package of the KB?)
 --> close KB !!!"

 `(progn
   (defvar ,var NIL)
   (setf ,var ,val)
  );progn
)

;; --- log10 ---

(defun log10 (x)
 "Logarithm to the base of 10."
 (log x 10)
)

;; --- ln ---

(defun ln (x)
 "Natural logarithm to the base e."
 (log x)
)

;; --- complement-test ---

(defun complement-test (fn)
 "This is the function complement of CLtL2.
 The source is taken from the example in this book."

 #'(lambda (&rest args)
    (not (apply fn args))
   );lambda
)

;; --- replace-sequential ---

(defun replace-sequential (s1 s2 &optional (start 0))
 (replace s1 s2 :start1 start)
)

;; --- copy-string ---

(defun copy-string (str)
 "Returns a copy of the input string, which is
 equal to str, but not eq !!!"

 (coerce (copy-list (coerce str 'list)) 'string)
)

;; --- append-string ---

(defun append-string (&rest args)
 (apply 'concatenate (append (list 'string) args))
)

;; --- substring ---

(defun substring (str start &optional (end nil))
 "Begins at position start incl. and ends at end excl.!
 The beginning of str is start=0, the end-of-string
 is end=nil.
 The CLISP add on version of substring is shadowed!"

 ;; hg20081102: subseq works on strings and lists
 ;; but runs into an error, if str is NIL!
 ;; (in my current clisp implementation 2.41)
 ;; I decide to make this emulation more graceful by
 ;; adding this switch:
 (if (typep str 'string)
  (subseq str start end)
  ;; else
  "" ;; don't be erroneous, return the empty string
 );if
)

;; --- nthrest ---

(defun nthrest (n lst)
 (nthcdr n lst)
)

;; --- last-n ---

(defun last-n (lst n)
 "Returns the last n elements of a list as a new list"
 (let ((len (length lst)))
  (if (>= n len)
   lst
  ;; else
   (if (< n 0)
    nil
   ;; else
    (nthcdr (- len n) lst)
   );if
  );if
 );let
)

;; --- sublist ---

(defun sublist (str start &optional (end nil))
 "Begins at position start incl. and ends at end excl.!
 The beginning of str is start=0, the end-of-string
 is end=nil"

 (subseq str start end)
)

;; --- tree-substitute ---

(defun tree-substitute (new old tree &optional (test #'eql))
 "The default-Test-Operator is #'is-exactly in Babylon LISP.
 But is-exactly is the same as CommonLISP's eql  !!!"

 (subst new old tree :test test)
)

;; --- tree-substitute-if ---

(defun tree-substitute-if (new test tree)
 "test must accept one argument (like #'numberp)."

 (subst-if new test tree)
)

;; --- pcons ---

(defun pcons (indicator value property-list)
 "Appends the property-value-pair in front (!) of the
 existing property-list. This might be the reason,
 why babylon3 always handles static instances in reverse
 order (compared with babylon2)!!!!"

 (list* indicator value property-list)
)

;; --- sequencep ---

(defun sequencep (seq)
 "although sequence is known to LISP's typep,
 this convenience function is not offered!"

 (typep seq 'sequence)
)

;; --- ratiop ---

(defun ratiop (seq)
 "although ratio is known to LISP's typep,
 this convenience function is not offered!"

 (typep seq 'ratio)
)

;; --- is-type-of ---

(defun is-type-of (obj typ)
 "UNDER CONSTRUCTION!
 SET-P ???
 more B3-types must be serviced!"
 (case typ
  ((null list string sequence character symbol number integer ratio float complex stream)
   (typep obj typ))
  ((nil t)
   typ)
  ;; HACK: works only with static instances!
  (otherwise
   (if (and (is-frame typ) (%instance-p obj))
    ;; hg20081102: orig: ($send obj :type typ)
    ;; results in runtime only(!) error msg: *** - FUNCALL: $SEND is a macro, not a function
    ;; work around:    (see B23|fmcs/mcs-core.cl|260)
    (fmcs::send-message obj :type typ)
   ;; else
    nil      ;; ain't no frame!
   );if
  );otherwise
 );case
)

;; --- the-type-of ---

(defun the-type-of (obj)
 "UNDER CONSTRUCTION!
 is an enhanced type-of
 SET-P ???"
 (if (is-frame obj)   ;; obj is a symbol with the frame's name
  obj
 ;else
  (if (is-instance obj)
   obj
  ;else
   (type-of obj)
  );if
 );if
)

;; --- is-exactly ---

(defun is-exactly (arg1 arg2)
 (eql arg1 arg2)
)

;; --- is-same ---

(defun is-same (arg1 arg2)
 "is-same is suited between is-nearly and is-exactly !!!"
 (if (and (numberp arg1) (numberp arg2))
  (= arg1 arg2)
 ;; else
  (equal arg1 arg2)
 );if
)

;; --- is-nearly ---

(defun is-nearly (x1 x2 &optional (delta :no-delta))
 "if instptrs compare them apropriately, else equalp"

 (unless (equal delta :no-delta)
  (emaxps-error "is-nearly: the delta arg is not implemented yet"))
 (let ((mcs-type (type-of (send-kb :frame-processor))))
  (when (or (typep x1 mcs-type) (typep x2 mcs-type))
   (setf x1 (%object-of x1))
   (setf x2 (%object-of x2))
  );when
  (equalp x1 x2)
 );let
)

;; --- for ---

;; I tried to solve this with the CLtL2 loop extension
;; which is supported by clisp, too!!
;; If your LISP interpreter does not know the power of
;; this loop extension, see the source code of CLISP
;; for an apropriate loop facility!
;;
;; WARNING: changes of the index-variables' values
;; from within execute ARE recognized by loop control!

(defmacro for (arg1 &rest args)
 `(,@(let (vl el cl rl kk)
      (mapcar #'(lambda (x)
                 (setf vl (append vl x))
                );lambda
              arg1
      );mapcar
      (mapcar #'(lambda (k)
                 (setf kk (string (first k)))
                 (cond
                  ((equalp "execute" kk)
                   (setf el (cons 'LISP::do (rest k))))
                  ((equalp "collect" kk)
                   (mapcar #'(lambda (x)
                              (unless (and (listp x) (= 3 (length x)))
                               (emaxps-error "Babylon3-FOR: unhandled COLLECTor ~S" x))
                              (setf cl
                               (append cl
                                (list (second x) (third x) 'LISP::into (first x))))
                             );lambda
                           (rest k)
                   );mapcar
                  );collect
                  ((equalp "result" kk)
                   (setf rl (list 'LISP::finally (cons 'LISP::return (rest k)))))
                  (t (emaxps-error "Babylon3-FOR: syntax error in ~S" k))
                 );case
                );lambda
              args
      );mapcar
;;      (print
       (append '(LISP::loop LISP::for) vl el cl rl)
;;      );print
     );let
  )
)

;; --- return-result ---

(defmacro return-result ()
 "Allowed only from within FOR"

 `(loop-finish)    ;; CLtL2 extended loop syntax!
)

;; --- with-open-input-file ---

(defmacro with-open-input-file (lst &body do-it)
 `(progn
;;(format t "~%with-open-input-file ~S" ,(second lst))
   (with-open-file
    (,@lst :direction :input :if-does-not-exist nil)
    ,@do-it)
  );progn
)

;; --- with-open-output-file ---

(defmacro with-open-output-file (lst &body do-it)
 `(with-open-file
   (,@(progn
       (unless (third lst)
        (setf lst (append lst (list :new-version))))
       (list (first lst) (second lst) :if-exists (third lst)
       );list
      );progn
    :direction :output)
   ,@do-it)
)

;; --- print-form ---

(defun print-form (form stream)
 (print form stream)
)

;; --- read-form ---

(defun read-form (stream &optional (eof nil))
 "ADVANTAGE: in case of stream=*terminal-io*,t
 the PAD-function emaxps-read can be activated instead!"

 (if (or (equal stream T)
         (equal stream *terminal-io*))
  (emaxps-read)
 ;else
  (read stream nil eof)    ;;eof-error-p=NIL   (see CLtL2 p.567)
 );if
)

;; --- read-one-line ---

(defun read-one-line (stream &optional (eof nil))
 "ADVANTAGE: in case of stream=*terminal-io*,t
 the PAD-function emaxps-read-line can be activated instead!"

 (if (or (equal stream T)
         (equal stream *terminal-io*))
  (emaxps-read-line)
 ;else
  (read-line stream nil eof)   ;;eof-error-p=NIL   (see CLtL2 p.567)
 );if
)






;;---------------------EXPORT NEW COMMANDS-------------------

;; The macro FOR conflicts in CLISP with the KEY LISP::FOR used in the
;; CLtL2 loop extension!
;; Hence it must be shadowed in "BABYLON" before the new version can be 
;; exported from :BUL!
;;
(when (unintern 'BABYLON::for :BABYLON)
 (format t "~%WARNING: uninterned CLtL2 loop extension FOR")
);when

;; ---------------------------

(mapcar #'(lambda (x)
           (if (not (fboundp x))
            (format t "~%yet not implemented B3-cmd: ~A" x)
           ;else
            (export x)
           );if
          );lambda
        '(
 ;; commands only known to Babylon LISP:
 ;;
 ;; lambda  <-- implicitly known within LISP!
 ;;
 log10 ln defglobal complement-test
 replace-sequential copy-string append-string nthrest last-n sublist
 tree-substitute tree-substitute-if pcons sequencep ratiop is-type-of
 the-type-of is-exactly is-same is-nearly return-result
 with-open-input-file with-open-output-file print-form read-form
 read-one-line
 ;;
 substring    ;; CLISP-ADD-ON !!!
 for          ;; CLtL2 loop extension!
 ;; defconstant: see above!!!
        )
);mapcar







;;-------------------RESET PACKAGE-----------------------

(in-package "BABYLON")

;; as long as KBs do not do in-package :KB-NAME
;; all of them are in-package "BABYLON"
;; therefore "BABYLON" needs (only for B3-KBs) this:
;;
(use-package "BUL")





;;; eof

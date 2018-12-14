#|*****************************************************************
*      b3-bql.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the Babylon Query Language used in     *
*   the XPS-shell babylon-3                                       *
*                                                                 *
*   B3 Reference Manual, Chapter 2                                *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")


;;;;;;
;; Punkt-Notation einfuehren!
;; ANWE hat verifiziert: xxx . xxx ist verboten!!!
;;                 nur    xxx.xxx  ist erlaubt!
;;           Dot-notation nur im ASK erlaubt!
;;;;;;



;; ------------------ Statistics / Profiling -----------------------
#|
(defvar bqli-cnt-p2 0)  ;pred2
(defvar bqli-cnt-p3 0)  ;pred3
(defvar bqli-cnt-eo 0)  ;element-of
(defvar bqli-cnt-ma 0)  ;match
(defvar bqli-cnt-l0 0)  ;directly
(defvar bqli-cnt-lx 0)  ;lexpr
(defvar bqli-cnt-bq 0)  ;bql-arg
(defvar bqli-cnt-an 0)  ;[and
(defvar bqli-cnt-rf 0)  ;retrievefrom
(defvar bqli-cnt-ct 0)  ;combineto
(defvar bqli-cnt-oo 0)  ;pred3 old old
(defvar bqli-cnt-on 0)  ;pred3 old new
(defvar bqli-cnt-nn 0)  ;pred3 new new

(defun bql-stats ()
 (format t "~%;;; p2=~S p3=~S eo=~S ma=~S l0=~S lx=~S bq=~S an=~S rf=~S ct=~S oo=~S on=~S nn=~S"
  bqli-cnt-p2 bqli-cnt-p3 bqli-cnt-eo bqli-cnt-ma bqli-cnt-l0
  bqli-cnt-lx bqli-cnt-bq bqli-cnt-an bqli-cnt-rf
  bqli-cnt-ct bqli-cnt-oo bqli-cnt-on bqli-cnt-nn)
)

(defun bql-stats-init ()
 (setf bqli-cnt-p2 0)
 (setf bqli-cnt-p3 0)
 (setf bqli-cnt-eo 0)
 (setf bqli-cnt-ma 0)
 (setf bqli-cnt-l0 0)
 (setf bqli-cnt-lx 0)
 (setf bqli-cnt-bq 0)
 (setf bqli-cnt-an 0)
 (setf bqli-cnt-rf 0)
 (setf bqli-cnt-ct 0)
 (setf bqli-cnt-oo 0)
 (setf bqli-cnt-on 0)
 (setf bqli-cnt-nn 0)
)

;;;;;; DANTEX: boot, 3xNEXT und inference
;;
;;; p2=0 p3=65    eo=0 l0=0    lx=0    bq=130   an=15    rf=17 ct=0 oo=0 on=65    nn=0
;;; p2=0 p3=206   eo=0 l0=0    lx=0    bq=412   an=57    rf=60 ct=0 oo=0 on=206   nn=0
;;; p2=0 p3=44562 eo=0 l0=6588 lx=6154 bq=89124 an=10380 rf=20 ct=0 oo=0 on=44562 nn=0
|#

;; ------------------- GLOBAL VARIABLES ----------------------------

;;;;;;
;; ASK checks N bindings of BQL-var's and therefore starts
;; IF-UNDETERMINED's and READ-DEPENDENTS.
;; An :ONCE does the continuation maximum ONE time. But the predication
;; MUST be done only ONCE, too!
;; If the predication only produces a varlist of length 2, the continuation
;; need not be differentiated!
;; PRED2, PRED3+HAS-PART, ELEMENT-OF and [OR may enlarge the varlist!
;;    (here checking should take place!)
;; [AND, [NOT, L_EXPR and MATCH do never enlarge the varlist,
;;    they only might shorten it!
;;
(defvar bqli-once NIL)

;;;;;;
;; The global definition of the temporarily shadowed BQL variable-list!
;;
(defvar bqli-varlist NIL)

;;;;;;
;; needed for macro expanding $PRED in ask and (re/un)tell operations
;; If BQL-expressions are part of rules, they are NOT macroexpanded
;; the way used in ask! Hence ask and rules have to be handled
;; differently!
;; If in rules it is set to :in-rule
;; if in ask it is set to T
;; else it is NIL...
;;
(defvar bqli-in-ask NIL)

;; ------------------------- internal HELP --------------------------

;; --- bqli-get-instances-with-slot ---

(defun bqli-get-instances-with-slot (slotname)
 "Used by pred3 only!"

 (let ((result NIL) (all-insts-of-kb (send-kb :instances)))
  (mapcar #'(lambda (x)
             (let ((slots-of-inst (<- x :slots)))
              (when (member slotname slots-of-inst)
               (setf result (cons x result)))))
          all-insts-of-kb
  );mapcar
  (setf result (reverse result))   ;; here B3 is inconsequent ...

  ;; return-value ...
  (if (and bqli-once (rest result))
   (list (first result))
  ;else
   result
  );if
 );let
)

;; --- bqli-get-all-instances-of-frame ---

(defun bqli-get-all-instances-of-frame (fram)
 "Here the :ONCE checking is done for PRED2..."

 ;;;;;;
 ;; (get-all-instances <-- B2 !!!)
 ;; Instanzen des frames und Instanzen der Unterframes des Frames!
 ;;;;;;
 (let ((result NIL) (all-insts-of-fr (get-all-instances fram))
       (all-insts-of-kb (send-kb :instances)))
  (mapcar #'(lambda (x)
             (when (member x all-insts-of-fr)
              (setf result (cons x result))))
          all-insts-of-kb
  );mapcar

  ;; return-value ...
  (if (and bqli-once (rest result))
   (list (first result))
  ;else
   result
  );if
 );let
)

;; ----------------------- EVALUATION --------------------------

;; --- bqli-loop ---

(defmacro bqli-loop (variable list-of-values &body do-it)
`(let (,variable (bqlil-list ,list-of-values)
      (bqlil-index 0) bqlil-imax)
  (unless (listp bqlil-list)
   (emaxps-error "bqli-loop: no LIST!"))
  (setf bqlil-imax (length bqlil-list))
  (if (= 0 bqlil-imax)
   (setf bqlil-imax 1))   ;; minimum one loop!
  (loop (if (>= bqlil-index bqlil-imax) (return))
   (setf ,variable (nth bqlil-index bqlil-list))

   ,@do-it

   (setf bqlil-index (+ bqlil-index 1))
  );loop
  NIL    ;; return-value
 );let
)

;; --- bqli-eval-bql-arg ---

(defmacro bqli-eval-bql-arg (form)
 "Used to pre-evaluate args 2 and 3 from pred2, pred3,
 match, element-of.
 If a new BQL-var is found, it is passed.
 Else a varlist with the same length as bqli-varlist or
 with only one var-value-pair in case of bqli-varlist=NIL
 is returned.
 ANWE: bqli-initialize should be included!
 Unknown _x make it return NIL !!!"

 `(let (ret (bqli-form ',form) bqli-eba-pos
        (bqli-in-ask NIL))                ;;; shadowing...

;(format t "~%eval-BQL-arg......  ~S" bqli-form)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-bq (1+ bqli-cnt-bq))   ;; statistics

   (setf ret

    (if (bqli-underscore-p bqli-form)
     (if (setf bqli-eba-pos (bqli-element-p bqli-form))
      ;; retrieve known BQL-var and return as if any LISP-expr.!
      (bqli-%retrievefrom-varlist bqli-eba-pos)
     ;else
      ;; it is a new BQL-var! return it for later evaluation.
      bqli-form
     );if

    ;else  !BQL-var
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; if bqli-varlist=NIL --> (first NIL)=NIL, (rest NIL)=NIL
     ;; it is run once, which is OK!
     ;;
     ;; old BQL-vars already ex.
     ;; The LEXPR must be evaluated (length(varlist)-1 times!)

     ;;;;;;
     ;; in most cases the global name of
     ;; a static inst is eval'ed here!
     ;;
     (if (symbolp bqli-form)
      (list ,form)    ;;; length=1 !!!

     ;else
      (if bqli-varlist
       (let ((bqli-vars (first bqli-varlist))
             (bqli-vals (rest  bqli-varlist))
             (bqli-tmplist NIL))                 ;;; shadowing...
        (bqli-loop bqli-ivs bqli-vals
         (setf bqli-tmplist (append bqli-tmplist (list (list
          (progv bqli-vars bqli-ivs
           ,form   ;;Do it once!
          )
         ))))
        );loop
        (cons '(_) bqli-tmplist)   ;; return-value
       );let

      ;else !bqli-varlist
       (list '(_) (list ,form))
      );if

     );if
    );if

   );setf ret
;(format t "~%eval-BQL-arg...done   ~S" ret)
   ret   ;; return-value!
  );let
)

;; --- bqli-eval-lexpr ---

(defmacro bqli-eval-lexpr (form)
 "Run from within [AND and [OR.
 Evals a LISP expression n-times depending on the 
 contents of bqli-varlist
 ANWE: bqli-initialize should be included!
 Unknown _x make it return NIL !!!"

 `(let ((bqli-andlist NIL) (bqli-orlist NIL)
        (bqli-in-ask NIL))          ;;shadowing...
   (declare (ignore bqli-orlist))   ;; for later usage

;(format t "~%eval-LEXPR......  ~S" ',form)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-lx (1+ bqli-cnt-lx))   ;; statistics

   (setf ret
    (if bqli-varlist
     (let ((bqli-vars (first bqli-varlist))
           (bqli-vals (rest bqli-varlist)))
      (bqli-loop bqli-ivs bqli-vals
       (when
        (progv bqli-vars bqli-ivs
         ,form   ;;Do it once!
        )
        (setf bqli-andlist
         (append bqli-andlist (list bqli-ivs)))
       );when
      );loop
      (when bqli-andlist
       (setf bqli-andlist (cons bqli-vars bqli-andlist)))
     );let
    ;else
     (if ,form   ;;Do it once!
      '(nil nil)      ;return-value (success)
     ;else
      nil             ;return-value (failure)
     );if
    );if
   );setf ret

;(format t "~%eval-LEXPR...done   ~S" ret)

   ret
  );let
)

;;;-----------------------VARLIST-OPs------------------------------

;; --- bqli-%expand-val ---

(defun bqli-%expand-val (val)
 "val is assumed to be a list like '(1) !!!"
 (if bqli-varlist
  (cons '(_) (make-list (length (rest bqli-varlist)) :initial-element val))
 ;else
  (list '(_) val)
 );if
)

;; --- bqli-combineto-varlist ---

(defun bqli-combineto-varlist (old new)
 "old und new koennen Ergebnislisten ODER nil sein!
  bqli-varlist wird LAENGER UND BREITER!
  Es wird vorausgesetzt, dass die Vars in alt und neu
  unterschiedlich heissen!"
;old=bqli-varlist immer! NIL '(NIL NIL) sind moeglich!
;new=vernuenftige varlist immer!

;(setf bqli-cnt-ct (1+ bqli-cnt-ct))

; (when (intersection (first old) (first new))
;  (emaxps-error "bqli-combineto-varlist: non-independent varlists~% ~S ~S" old new))

; (when (equal new '(nil nil))
;  (setf new nil))
; (if new
  (if old
   (let ((tmp (list (append (first old) (first new))))
         i1 i2 (i1m (length old)) (i2m (length new)))
    (setf i1 1)(loop (if (>= i1 i1m)(return))
     (setf i2 1)(loop (if (>= i2 i2m)(return))
      (setf tmp (append tmp
       (list (append (nth i1 old) (nth i2 new))))
      );setf tmp
      (setf i2 (1+ i2))
     );loop i2
     (setf i1 (1+ i1))
    );loop i1
    tmp
   );let
  ;else
   new
  );if
; ;else !new
;  old
; );if
)

;; --- bqli-%retrievefrom-varlist ---

(defun bqli-%retrievefrom-varlist (pos)
 "Used by bqli-eval-bql-arg ONLY!
 Retrieves the BQL-var POS points to from
 bqli-varlist!
 POS can be assumed to be a non-negative integer
 and bqli-varlist is NON-NIL!"

;(setf bqli-cnt-rf (1+ bqli-cnt-rf))

 (mapcar #'(lambda (x)
            (list (nth pos x))
           );lambda
         bqli-varlist
 );mapcar
)

;; --- bqli-search ---

(defun bqli-search (doit)
 "used by bqli-initialize"

 (let (varnames)
  (mapcar #'(lambda (x)
             (if (listp x)
              (if (not (equal (first x) 'ask))
               ;; recoursive ASKs have their own BQL-var's!!!
               ;; ==> stop here!
               ;; (re/un)tell's use the outer varnames!
               (setf varnames (append varnames (bqli-search x)))
              );if
             ;else
              (if (bqli-underscore-p x)
               (setf varnames (append varnames (list x)))
              );if
             );if
            );lambda
          doit
  );mapcar
  varnames   ;return-value
 );let
)

;; --- bqli-initialize ---

(defun bqli-initialize (werte doit)
 "to check the continuation of ask for unknown BQL-vars.
 appends them with their names as their values."

 (let (bql-var-names unknown-bql-vars known-bql-vars)

  ;; find var-names used in THEN-part of ASK:
  (setf bql-var-names (remove-duplicates (bqli-search doit)))

  ;; List of varnames already known:
  (setf known-bql-vars (first werte))

  ;; List of varnames used but not known:
  (setf unknown-bql-vars
   (set-exclusive-or (remove-duplicates (append bql-var-names known-bql-vars))
    known-bql-vars))

  ;; Build new Result-list:     (return-value)
  (bqli-combineto-varlist werte (list unknown-bql-vars unknown-bql-vars))
 );let
)

;; --------------------------CHECKING------------------------------

;; --- bqli-underscore-p ---

(defun bqli-underscore-p (name)
 (and (symbolp name)
      (not (equal name _))
      (equal #\_ (char (string name) 0))
 )
)

;; --- bqli-element-p ---

(defun bqli-element-p (var)
 "(position 'x NIL)=NIL and (first NIL)=NIL!
 Position Arg is used by bqli-%retrievefrom-varlist !!!"

 (position var (first bqli-varlist))
)

;; --- bqli-dot-notation-p ---

(defun bqli-dot-notation-p (name)
 (and (symbolp name)
      (let ((lst (coerce (string name) 'list))
            (ret nil))
       (mapcar #'(lambda (x)
                  (if (equal #\. x)
                   (setf ret T)
                  );if
                 );lambda
                lst
       );mapcar
       ret     ;;ret-val
      );let
 );and
)

;; --- bqli-bql-p --- 

(defun bqli-bql-p (arg)
 "Check whether arg is a BQL expression.
 Else it must be a LISP expression!

 Used by bqli-and-hook ONLY!"

;(format t "~%bql-p: ~S" arg)

; Statistisch werden mehr Regeln als ASKs gestartet ==> optimieren!

 (and
  (listp arg)
  (or
   (let ((arg1 (first arg)))
    (or
     (equal arg1 '$PRED)                ;; RULE
     (equal arg1 'ELEMENT-OF)           ;; RULE
     (equal arg1 'MATCH)                ;; RULE
     (equal arg1 'BQLI-ELEMENT-OF-EVAL) ;; ASK
     (equal arg1 'BQLI-MATCH-EVAL)      ;; ASK
    );or
   );let
   (and        ;; only in ASK: $PRED already macro-expanded
    (= 3 (length arg))
    (equal (second arg) '((*EVALHOOK* 'BQLI-PRED-HOOK)))
;    (equal (second (first (second arg))) ''BQLI-PRED-HOOK)
;                                       ;; ^--- IMPORTANT!
   );and
  );or
 );and
)

;; --- bqli-$term-p ---

(defun bqli-$term-p (arg3)
 (and (listp arg3) (equal (first arg3) '$term))
)

;; ----------------------- MATCH ---------------------------

;; --- bqli-match-eval ---

(defun bqli-match-eval (var val)
 "Possible values:	assumption (ANWE!):	  returns:
  var:  _x: ex.:	ERROR
            new:	OK			  '_x
       any:		ERROR
  val:  _x: ex.:	OK, if it contains any LISP type except SETs!  '((_x)(1))
            new:	ERROR
       any:             OK, if it evals to any LISP type except SETs! '((_)(1))"

;(format t "~%++++++ (match ~S ~S)" var val)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-ma (1+ bqli-cnt-ma))   ;; statistics

 (unless bqli-in-ask
  (emaxps-error "MATCH is not allowed outside ASK"))
 ;;; first arg:
 (unless (bqli-underscore-p var)
  (emaxps-error "MATCH: arg1 should be a BQL-var (_x)"))
 (when (bqli-element-p var)
  (emaxps-error "MATCH: arg1 ~a already exists" var))
 ;;; second arg:
 (when (symbolp val) ;;; macro already checked it! (bqli-underscore-p val)
  (emaxps-error "MATCH: arg2 ~a does not exist" val))

 ;;; Hack: val=(r1) OR val=((_)(r1)) OR val=((_x)(r1))
 ;;         ^-- to increase speed at Pred3!
 ;;
 (unless (rest val)
  (setf val (bqli-%expand-val val)))

 (let ((bqli-andlist NIL) (bqli-orlist NIL)
       (i 0) (len1 (1- (length val)))
       (old (rest bqli-varlist))
       (new (rest val)))
  (declare (ignore bqli-orlist))   ;; for later usage!

  ;; second arg is a LISP expression, evaluated N times
  ;; via bqi-eval-bql-arg with N=(1- (length bqli-varlist))

  ;; if bqli-varlist ex., this loop has to be done N times!
  ;; else only once (len1=1)!

  (loop (if (>= i len1) (return))
   (setf bqli-andlist
    (append bqli-andlist
     (let ((nn (first (nth i new)))   ;; only the BQL-var '_' --> length=1 ==> first=OK!
           (oo (nth i old)))
      (when (%set-p nn)
       (emaxps-error "MATCH: arg2 ~S should not be a SET" nn))
      (list (cons nn oo))    ;; this is different to ELEMENT-OF !
     );let
    );append
   );setf
   (setf i (1+ i))
  );loop
  (when bqli-andlist
   (when bqli-once           ;; for :ONCE !!!
    (setf bqli-andlist (list (first bqli-andlist))))
   (setf bqli-andlist (cons (cons var (first bqli-varlist)) bqli-andlist))
  );when

  ;; return-value (currently only [AND mode supported!)
;(format t "~%MATCH rets: ~S" bqli-andlist)
  bqli-andlist
 );let
)

;; --- match ---

(defmacro match (arg1 arg2)
 `(bqli-match-eval ',arg1 (bqli-eval-bql-arg ,arg2))
)

;; -------------------- ELEMENT-OF -------------------------

;; --- bqli-element-of-eval ---

(defun bqli-element-of-eval (var val)
 "Possible values:	assumption (ANWE!):	  returns:
  var:  _x: ex.:	ERROR
            new:	OK			  '_x
       any:		ERROR
  val:  _x: ex.:	OK, if it contains LISTS! '((_x)(L1)(L2)(L3))
            new:	ERROR
       any:             OK, if it evals to LISTS! '((_)(L1)(L2)(L3))"

;(format t "~%++++++ (element-of ~S ~S)" var val)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-eo (1+ bqli-cnt-eo))   ;; statistics

 (unless bqli-in-ask
  (emaxps-error "ELEMENT-OF is not allowed outside ASK"))
 ;;; first arg:
 (unless (bqli-underscore-p var)
  (emaxps-error "ELEMENT-OF: arg1 should be a BQL-var (_x)"))
 (when (bqli-element-p var)
  (emaxps-error "ELEMENT-OF: arg1 ~a already exists" var))
 ;;; second arg:
 (when (symbolp val) ;;; macro already checked it! (bqli-underscore-p val)
  (emaxps-error "ELEMENT-OF: arg2 ~a does not exist" val))

 ;;; Hack: val=(r1) OR val=((_)(r1)) OR val=((_x)(r1))
 ;;         ^-- to increase speed at Pred3!
 ;;
 (unless (rest val)
  (setf val (bqli-%expand-val val)))

 (let ((bqli-andlist NIL) (bqli-orlist NIL)
       (i 0) (len1 (1- (length val)))
       (old (rest bqli-varlist))
       (new (rest val)))
  (declare (ignore bqli-orlist))   ;; for later usage!

  ;; second arg is a LISP expression, evaluated N times
  ;; via bqi-eval-bql-arg with N=(1- (length bqli-varlist))

  ;; if bqli-varlist ex., this loop has to be done N times!
  ;; else only once (len1=1)!

  (loop (if (>= i len1) (return))
   (setf bqli-andlist
    (append bqli-andlist
     (let ((nn (first (nth i new)))   ;; only the BQL-var '_' --> length=1 ==> first=OK!
           (oo (nth i old)))
      (unless (listp nn)
       (emaxps-error "ELEMENT-OF: arg2 ~S should be a LIST" nn))
      (mapcar #'(lambda (x) (cons x oo)) nn)    ;; this is different to MATCH !
     );let
    );append
   );setf
   (setf i (1+ i))
  );loop
  (when bqli-andlist
   (when bqli-once           ;; for :ONCE !!!
    (setf bqli-andlist (list (first bqli-andlist))))
   (setf bqli-andlist (cons (cons var (first bqli-varlist)) bqli-andlist))
  );when

  ;; return-value (currently only [AND mode supported!)
;(format t "~%ELEMENT-OF rets: ~S" bqli-andlist)
  bqli-andlist
 );let
)

;; --- element-of ---

(defmacro element-of (arg1 arg2)
 `(bqli-element-of-eval ',arg1 (bqli-eval-bql-arg ,arg2))
)

;; ----------------------- [AND --------------------------

(defvar bqli-and-nth-form nil)

;; --- bqli-and-hook ---

(defun bqli-and-hook (form env)
 "Each of the args to bqli-and has to be serviced."

 (setf form (symbol-value form))
 (unless (bqli-bql-p form)
  (setf form (list 'bqli-eval-lexpr form)))
 (evalhook form nil nil env)
)

;; --- bqli-and ---

(defmacro bqli-and (&rest args)
 "Dispatcher for [AND whose args may include LISP-expressions, 
 match, element-of and simple and compound predications.
 It is always run from within ASK/RULE!
 Each of the constructs within an [AND modifies bqli-varlist,
 hence here a copy with the original contents is held to do final
 modification of the original bqli-varlist!"

 `(let* ((bqli-and-args ',args)
          (bqli-varlist bqli-varlist) (bqli-idx 0)
          bqli-and-nth-form (bqli-len (length bqli-and-args)))

;(format t "~%[AND ****** ~S" bqli-and-args)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-an (1+ bqli-cnt-an))   ;; statistics

   (loop (if (>= bqli-idx bqli-len) (return))
    (setf bqli-and-nth-form (nth bqli-idx bqli-and-args))

;(format t "~%[AND does: ~S" bqli-and-nth-form)

    (setf bqli-varlist   ;; bql-exprs eval'ed in AND-mode --> return the new LIST!
     (let ((*evalhook* 'bqli-and-hook))
      ;; Do it!
      bqli-and-nth-form  ;; This works only from within a MACRO !?!
     );let
    );setf
;(format t "~%VL= ~S" bqli-varlist)

    ;; here in [AND:  ==>  ABORT IF NIL !!!
    (unless bqli-varlist (return))
    (setf bqli-idx (1+ bqli-idx))
   );loop

   ;; return modified temporary bqli-varlist
;(format t "~%[AND *** done   ~S" bqli-varlist)
   bqli-varlist
  );let
)

;; ---------------------- [NOT -------------------------

;; --- bqli-not ---

(defmacro bqli-not (arg)
 (declare (ignore arg))
 `(emaxps-error "[NOT is not implemented yet")
)

;; ----------------------- [OR -------------------------

;; --- bqli-or ---

(defmacro bqli-or (&rest args)
 (declare (ignore args))
 `(emaxps-error "[OR is not implemented yet")
)

;; ----------------------- $TERM ---------------------------

;; --- $term ---

(defmacro $term (x)
 "the correct interpretation is done by bqli-simple-pred-3!"
 (declare (ignore x))
 `(emaxps-error "direct access of $TERM")
)

;; ----------------------- PRED3 ----------------------------

;; --- bqli-eval-bql-arg-3 ---

(defmacro bqli-eval-bql-arg-3 (val)
 `(let ((bqli-valterm ',val))
   (if (bqli-$term-p bqli-valterm)
    ;; further checking is done by bqli-pred3-eval !!!
    ;; SETs are conventional values...
    bqli-valterm
   ;else
    (bqli-eval-bql-arg ,val)
   );if
  );let
)

;; --- bqli-simple-pred-3 ---

(defmacro bqli-simple-pred-3 (slot inst val)
 `(bqli-pred3-eval ',slot
   (bqli-eval-bql-arg ,inst) (bqli-eval-bql-arg-3 ,val))
)

;; --- bqli-pred3-eval ---

(defun bqli-pred3-eval (slot inst val)
 "The B3-REF says, that evaluation is implicitly done
 left to right.
 --> 1st check valid insts, 2nd check valid values."

;;;;;;
;; $term: wenn slot={1 2 3} und !(_x _y _z)   ;; gleiche Laenge!
;;        dann ((_x _y _z)(1 2 3))
;;        sonst NIL                           ;; ungleiche Laengen!
;;;;;;

;(format t "~%PRED3====== (~S ~S ~S)" slot inst val)
;(format t "~%VL=  ~S" bqli-varlist)

;(setf bqli-cnt-p3 (1+ bqli-cnt-p3))   ;; statistics

#|
 (when (bqli-dot-notation-p slot)
  ;; allowed, if inst is a varlist!
  (emaxps-error "PRED3: arg1: dot-notation is not supported yet!"))
 (when (bqli-$term-p val)
  (emaxps-error "PRED3: arg3: $TERM is not supported yet!"))
|#
 ;;;;;;
 ;; if arg2 is already known, it contains inst-ptrs!
 ;; if such an instance does not know slot, B3 signals an ERROR!
 ;; --> the list of inst-ptrs need not be reduced!
 ;;;;;;
 (let ((bqli-andlist NIL) (bqli-orlist NIL))
  (declare (ignore bqli-orlist))  ;; for later usage!
  (if (symbolp inst) ;;; macro already checked it! (bqli-underscore-p inst)
   (if (symbolp val) ;;; faster than! (bqli-underscore-p val)
    ;;;;;
    ;;; [SLOT _X _Y]    ==> both vars are new!
    ;;; (1) make varlist of insts and their SETs (values)
    ;;; (2) expand SETs-insts-varlist a la ELEMENT-OF to bqli-andlist
    ;;;;;
    (let ((i 0) iii sss len inst-list)
;(print 'pred3-new-new)
;(setf bqli-cnt-nn (1+ bqli-cnt-nn))

     (setf inst-list (bqli-get-instances-with-slot slot))
     (setf len (length inst-list))
     (loop (if (>= i len) (return))
      (setf iii (nth i inst-list))
      (setf sss
       (%set-to-list
        (ask-instance-values slot iii
         :read-dependents T :if-undetermined T)))
      (when (and bqli-once (rest sss))
       (setf sss (list (first sss))))    ;;; :ONCE !!!
      (setf bqli-andlist
       (append bqli-andlist
        (mapcar #'(lambda (x) (list x iii)) sss)
      ))
      (setf i (1+ i))
     );loop
     (if bqli-andlist
      (setf bqli-andlist (bqli-combineto-varlist bqli-varlist
       (cons (list val inst) bqli-andlist)
      ))
     ;else
      (setf bqli-andlist bqli-varlist)   ;; leave it unchanged...
     );if
    );let

   ;else
    ;;;;;
    ;;; [SLOT _X VAL]   ==> find slots with the right values!
    ;;; ??? removal from bqli-varlist ???
    ;;; SETs are NOT allowed!
    ;;;;;

    ;;;;;;;;;
    ;; (if (symbolp (first val)) --> $term! )
    ;;;;;;;;;

    ;;;;;;;;;;; remember the HACK !

    (progn
;;   (when (%%set-p val)
;;    (emaxps-error "PRED3: arg3 of [SLOT _X VAL]: SETs are not allowed!"))
     (emaxps-error "PRED3: [SLOT _X VAL] is not supported yet!")
    );progn

   );if
  ;else
   (if (symbolp val) ;;; faster! (bqli-underscore-p val)
    ;;;;;
    ;;; [SLOT INST _Y]    ==> values  are new!
    ;;; (1) remove arg2 from varlist, remove arg2-list analogous
    ;;; (2) make varlist of SETs (values) corresponding to reduced arg2-list
    ;;; (3) expand SETs-varlist a la ELEMENT-OF to bqli-andlist
    ;;;;;

    (progn
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;     MOST OFTEN USED!!!   ;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(print 'pred3-old-new)
;(setf bqli-cnt-on (1+ bqli-cnt-on))

     (if (rest inst)
      (if bqli-varlist
       ;;;;;;
       ;; complex bqli-varlist AND complex inst varlist
       ;; ==> (length inst)=(length bqli-varlist)!
       ;;     inst=((_)(r1)(r2)(r3))
       ;;       VL=((_x _y)(1 2)(2 3)(4 5))
       ;;;;;;
       (let ((i 0) iii sss ooo len inst-list restlist)
;(print 'pred3-old-new-fulli-fullv)
        (setf restlist (rest bqli-varlist))
        (setf inst-list (rest inst))
        (setf len (length inst-list))
        (loop (if (>= i len) (return))
         (setf iii (first (nth i inst-list)))    ;;; !! reihenfolge!
         (setf ooo (nth i restlist))
         (setf sss
          (%set-to-list
           (ask-instance-values slot iii
            :read-dependents T :if-undetermined T)))
         (case (length sss)
          (0 NIL) ;; empty set! (do not append!= REMOVE!)
          (1 (setf bqli-andlist (append bqli-andlist
               (list (cons (first sss) ooo))
          )))
          (t (setf bqli-andlist (append bqli-andlist
               (mapcar #'(lambda (x) (cons x ooo)) sss)
          )))
         );case
         (setf i (1+ i))
        );loop
        (when bqli-andlist
         (when bqli-once           ;; for :ONCE !!!
          (setf bqli-andlist (list (first bqli-andlist))))
         (setf bqli-andlist
          (cons (cons val (first bqli-varlist))
                bqli-andlist)
         );setf
        );when
       );let

      ;else !bqli-varlist
       ;;;;;;
       ;; complex inst varlist, but NO bqli-varlist!
       ;; ==> inst=((_)(r1))
       ;;;;;;
       (let (iii sss)
;(print 'pred3-old-new-fulli-nov)
        (setf iii (first (second inst)))
        (setf sss
         (%set-to-list
          (ask-instance-values slot iii
           :read-dependents T :if-undetermined T)))
        (setf bqli-andlist
         (case (length sss)
          (0 NIL)
          (1 (list (list val) sss))
             ;; list-to-listoflists...
          (t (if bqli-once
              (list (list val) (list (first sss)))
             ;else
              (mapcar #'list (cons val sss))
             );if
          );t
         );case
        );setf
       );let
      );if !bqli-varlist

     ;else
      ;;;;;;
      ;; bqli-eval-bql-arg made a SHOTCUT!
      ;; inst was the global name of a static instance!
      ;; ==> inst=(ip)
      ;;;;;;
      (if bqli-varlist
       (let ((i 0) sss ooo len restlist)
;(print 'pred3-old-new-smalli-fullv)
        (setf sss
         (%set-to-list
          (ask-instance-values slot (first inst)
           :read-dependents T :if-undetermined T)))

        (when sss  ;; unless empty set!!!
         (if (= 1 (length sss))
          (progn
           (setf restlist (rest bqli-varlist))
           (setf len (length restlist))
           (loop (if (>= i len) (return))
            (setf ooo (nth i restlist))
            (setf bqli-andlist (append bqli-andlist
              (list (cons (first sss) ooo))
            ))
            (setf i (1+ i))
           );loop
          );progn

         ;else      ;; a true set!
          (progn
           (setf restlist (rest bqli-varlist))
           (setf len (length restlist))
           (loop (if (>= i len) (return))
            (setf ooo (nth i restlist))
            (setf bqli-andlist (append bqli-andlist
              (mapcar #'(lambda (x) (cons x ooo)) sss)
            ))
            (setf i (1+ i))
           );loop
          );progn
         );if       ;; (length sss)

         (when bqli-andlist
          (when bqli-once           ;; for :ONCE !!!
           (setf bqli-andlist (list (first bqli-andlist))))
          (setf bqli-andlist
           (cons (cons val (first bqli-varlist))
                 bqli-andlist)
          );setf
         );when

        );when   (unless empty set)
       );let

      ;else !bqli-varlist
       ;;;;;;
       ;; simplest case!
       ;; no vars up to now AND one inst only!
       ;;;;;;
       (let (iii sss)
;(print 'pred3-old-new-smalli-nov)
        (setf iii (first inst))
        (setf sss
         (%set-to-list
          (ask-instance-values slot iii
           :read-dependents T :if-undetermined T)))
        (setf bqli-andlist
         (case (length sss)
          (0 NIL)
          (1 (list (list val) sss))
             ;; list-to-listoflists...
          (t (if bqli-once
              (list (list val) (list (first sss)))
             ;else
              (mapcar #'list (cons val sss))
             );if
          );t
         );case
        );setf
       );let
      );if  !bqli-varlist
     );if   reduced inst varlist
    );progn   most often used!

   ;else
    ;;;;;
    ;;; [SLOT INST VAL]   ==> remove depending on insts and slots!
    ;;; (1) remove arg2 from varlist
    ;;; (2) remove arg3 from varlist (SET allowed --> is-subset-of-set)
    ;;;;;

    ;;;;;;;;;
    ;; (if (symbolp (first val)) --> $term! )
    ;;;;;;;;;

    (let ((i 1) (len (length val)) sss askset)
;(print 'pred3-old-old)
;(setf bqli-cnt-oo (1+ bqli-cnt-oo))

     ;;; Hack: (see element-of)
     (unless (rest inst)
      (setf inst (bqli-%expand-val inst)))
     (unless (rest val)
      (setf val (bqli-%expand-val val)))

     (loop (if (>= i len) (return))
      (setf sss (first (nth i val)))
      (setf askset (ask-instance-values slot (first (nth i inst))
                    :read-dependents T :if-undetermined T))
      (when (if (%set-p sss) (%is-subset-of-set sss askset)
                             (%is-element-of-set sss askset))
       ;;;;;;
       ;;; INFO: (nth i NIL)=NIL !
       (setf bqli-andlist (append bqli-andlist (list (nth i bqli-varlist))))
      );when
      (setf i (1+ i))
     );loop
     (when bqli-andlist
      (when bqli-once           ;; for :ONCE !!!
       (setf bqli-andlist (list (first bqli-andlist))))
      (setf bqli-andlist (cons (first bqli-varlist) bqli-andlist)))
    );let

   );if  !BQL-var value
  );if   !BQL-var inst

  ;; return-value (currently only [AND mode supported!)
;(format t "~%PRED3===done   ~S" bqli-andlist)
  bqli-andlist    ;; return-value
 );let
)

;; ----------------------- PRED2 ---------------------------

;; --- bqli-pred2-eval ---

(defun bqli-pred2-eval (fram inst)
 "Possible values:	returns:
  fram: symbol:		OK
  inst:  _x: ex.:	OK, if it only contains I-PTRs! '((_x)(P1)(P2)(P3))
             new:	OK
        any:            OK, if it evals to I-PTRs! '((_)(P1)(P2)(P3))"

 (let (tmp (bqli-andlist NIL) (bqli-orlist NIL)
       (fram-instances (bqli-get-all-instances-of-frame fram)))
  (declare (ignore bqli-orlist))    ;;; for later usage!

;(format t "~%------ (pred2 ~S ~S)" fram inst)
;(format t "~%VL= ~S" bqli-varlist)

;(setf bqli-cnt-p2 (1+ bqli-cnt-p2))   ;; statistics

  (unless fram-instances               ;; correct
   (emaxps-error "PRED2: frame ~S does not exist" fram))

  (if (symbolp inst) ;;; faster! (bqli-underscore-p inst)
   (progn
    (setf tmp (mapcar #'list fram-instances))
    (when (and tmp bqli-once)
     (setf tmp (list (first tmp))))    ;;; :ONCE !!!
    ;; this is a NEW bql-varname
    (setf bqli-andlist                       ;; [AND only !
     (bqli-combineto-varlist bqli-varlist
      (cons (list inst) tmp)))
   );progn

  ;else   ;; this is a varlist
   (progn
    ;;; Hack: (see element-of)
    (unless (rest inst)
     (setf inst (bqli-%expand-val inst)))

    (if bqli-varlist  ;; already something present...
     (let* ((i 0) (len (length inst)) (len1 (1- len))
            (old (rest bqli-varlist)) (new (rest inst)))
      (loop (if (>= i len1) (return))    ;; new=((p1)(p2)(p3))
       (if (member (name-of (first (nth i new))) fram-instances)
        (setf bqli-andlist (append bqli-andlist (list (nth i old))))
       );if
       (setf i (1+ i))
      );loop
      (when bqli-andlist
       (when bqli-once           ;; for :ONCE !!!
        (setf bqli-andlist (list (first bqli-andlist))))
       (setf bqli-andlist (cons (first bqli-varlist) bqli-andlist)))
     );let*

    ;else !bqli-varlist  -->  (length arg2) = 2 !!  '((_)(val))
     (setf bqli-andlist
      (if (member (name-of (first (second inst))) fram-instances)
       '(nil nil)    ;;return-value   (success)
       ;else
        NIL          ;;return-value   (failure)
      );if
     );setf
    );if
   );progn
  );if   !_new

  ;; return-value (currently only [AND mode supported!)
;(format t "~%PRED2 rets: ~S" bqli-andlist)

  bqli-andlist    ;; return-value
 );let
)

;; --- bqli-simple-pred-2 ---

(defmacro bqli-simple-pred-2 (fram inst)
 `(bqli-pred2-eval ',fram (bqli-eval-bql-arg ,inst))
)

;; ------------------- [HAS-PART ----------------------

;; --- bqli-has-part ---

(defmacro bqli-has-part (inst val)
 "inst MUST be a valid instance-pointer,
 val MUST be a BQL-variable!"

 `(bqli-has-part-eval (bqli-eval-bql-arg ,inst) ',val)
)

;; --- bqli-has-part-eval ---

(defun bqli-has-part-eval (inst val)

;(format t "~%++++++ [HAS-PART ~S ~S]" inst val)
;(format t "~%VL= ~S" bqli-varlist)

 (when (bqli-underscore-p inst)
  ;; eval-bql-arg detected an unknown BQL-var!
  (emaxps-error "[HAS-PART: ~S should be a known instance pointer" inst))
 (unless (bqli-underscore-p val)
  (emaxps-error "[HAS-PART: ~S should be something like _XXX" val))
 (when (bqli-element-p val)
  (emaxps-error "[HAS-PART: variable ~S already exists" val))
 ;;; Hack: inst=(r1) OR inst=((_)(r1)) OR inst=((_x)(r1))
 ;;         ^-- to increase speed at Pred3!
 ;;
 (unless (rest inst)
  (setf inst (bqli-%expand-val inst)))

 (let ((bqli-andlist NIL) (bqli-orlist NIL)
       (i 0) (len1 (1- (length inst)))
       (old (rest bqli-varlist))
       (new (rest inst)))
  (declare (ignore bqli-orlist))   ;; for later usage!

  ;; second arg is a LISP expression, evaluated N times
  ;; via bqi-eval-bql-arg with N=(1- (length bqli-varlist))

  ;; if bqli-varlist ex., this loop has to be done N times!
  ;; else only once (len1=1)!

  (loop (if (>= i len1) (return))
   (setf bqli-andlist
    (append bqli-andlist
     (let ((nn (first (nth i new)))   ;; only the BQL-var '_' --> length=1 ==> first=OK!
           (oo (nth i old)))
      ;;;;;;;
      ;; now get the values of this specialized slot
      ;; (active value, special read-behavior):
      ;; get a list of the current contents of the part-relations!
      ;;;;;;;
      (setf nn ($send (object-of nn) :get 'has-part))

      (unless (listp nn)
       (emaxps-error "[HAS-PART: unexpected error: ~S should be a LIST" nn))
      (mapcar #'(lambda (x) (cons x oo)) nn)    ;; this is different to MATCH !
     );let
    );append
   );setf
   (setf i (1+ i))
  );loop

  (when bqli-andlist

;;; HG: temporaer ?!?
;(format t "~%HAS-PART andlist= ~S" bqli-andlist)
;   (when bqli-once           ;; for :ONCE !!!
;    (setf bqli-andlist (list (first bqli-andlist))))
   (setf bqli-andlist (cons (cons val (first bqli-varlist)) bqli-andlist))
  );when

  ;; return-value (currently only [AND mode supported!)
;(format t "~%HAS-PART rets: ~S" bqli-andlist)
  bqli-andlist
 );let
)

;; --- bqli-read-has-part ---

(defun bqli-read-has-part (self oldval)
 "This function is run from the active-value read behavior
 for the slot HAS-PART. slef is the pointer to the current instance.
 oldval is the list of slots which are declared to be B3-part-relations.
 The return-value of this function is the return-value of the behavior!"

;;;;;;
;; should be made at define-frame!
;;
;(setf oldval (reverse oldval))   ;; HG: doch nicht gemacht!

;(format t "~%*** ov=~S" oldval)
; (when (and bqli-once (rest oldval))
;  (setf oldval (list (first oldval)))
; );when
;(format t "~%*** ov1=~S" oldval)
 (let ((result NIL))
  (mapcar #'(lambda (x)
             (setf x (set-to-list
               (ask-instance-values x self
                :if-undetermined T :read-dependents T)))
             (setf result (append x result))   ;; HG: doch nicht vertauscht!
            );lambda
          oldval
  );mapcar

;(print 'bqli-read-has-part)
;(print result)

  result     ;; return-value
 );let
)

;; ------------------- SIMPLE PREDs ------------------

;; --- bqli-simple-pred ---

(defmacro bqli-simple-pred (&rest args)
 "Here already in ASK!"

 `(let ((len (length ',args)))
   (if (< len 2) (emaxps-error "$PRED: too few args to simple predication"))
   (if (> len 3) (emaxps-error "$PRED: too many args to simple predication"))
   (if (= len 2)
    (bqli-simple-pred-2 ,@args)
   ;else
    (bqli-simple-pred-3 ,@args)
   );if
  );let
)

;; --- bqli-tell-pred-3 ---

(defmacro bqli-tell-pred-3 (arg1 arg2 arg3)
 "Here already in TELL/RETELL/UNTELL!"

 `(let ((bqli-tp3-a1 ',arg1)
        (bqli-tp3-a3 ',arg3))
   (if (or (equal bqli-tp3-a1 'has-part)      ;; has-part
;HG           (bqli-dot-notation-p bqli-tp3-a1)  ;; dot-notation
           (and (listp bqli-tp3-a3)
                (equal (first bqli-tp3-a3)
                       '$term))               ;; $term
       );or
    (emaxps-error "$PRED: illegal usage outside ASK"))

   (list bqli-tp3-a1 ,arg2 ,arg3)  ;; return-value
  );let
)

;; --------------------- $PRED -------------------

;; --- bqli-pred-get-macro-name ---

(defun bqli-pred-get-macro-name (1st)
 "$TERM abuse and length of simple-pred
  must be tested in bqli-simple-pred!"

; (format t "~%>>> ~S ~S" 1st bqli-in-ask)
 (let (fn cut)
  (if bqli-in-ask
   ;;; in ASK/RULE:
   (cond
    ((equal 1st 'and)      (setf fn 'bqli-and
                                 cut t))
    ((equal 1st  'or)      (setf fn 'bqli-or
                                 cut t))
    ((equal 1st 'not)      (setf fn 'bqli-not
                                 cut t))
    ((equal 1st 'has-part) (setf fn 'bqli-has-part
                                 cut t))
    (t                     (setf fn 'bqli-simple-pred
                                 cut nil))
   );cond
  ;else
   ;;; in TELL/RETELL/UNTELL:
   (if (or (equal 1st 'and)
           (equal 1st  'or)
           (equal 1st 'not)
           (equal 1st 'has-part)
       );or
    (emaxps-error "$PRED: illegal use outside ASK (only [a b c] is allowed)")
   ;else
    (setf fn 'bqli-tell-pred-3
          cut nil)
   );if
  );if
  (list fn cut)    ;; return-value
 );let
)

;; --- bqli-pred-hook ---

;(defvar bqli-xyz NIL)

(defun bqli-pred-hook (form env)
 "See channels.cl:
 The EMA-XPS evalhook-fn does not work recoursively.
 Only the toplevel-loop is modified. There are no other
 hooks registered! This hook-fn is used by $PRED only,
 which should be used only by B3-KBs!!! BabylonLISP does
 not support evalhooking. Hence NIL for recoursive
 hook-fns should be a fine value!!!"

 (let (ret); fff
  (unless form (emaxps-error "$PRED: too few args"))
;(setf bqli-xyz form)
;  (format t "~%hook1: ~S" form)(finish-output t)
;(setf fff (first form))
;(if (and (not (null fff))(listp fff))(print (format nil "~S" fff))(finish-output t))
  (setf ret (bqli-pred-get-macro-name (first form)))
  (if (second ret)         ;if cut:
   (setf form (rest form)))
  (setf form (cons (first ret) form))
;  (format t "~%hook2: ~S" form)
  (evalhook form nil nil env)
 );let
)

;; --- $pred ---

(defmacro $pred (&rest args)
 "Dispatcher for simple and compound predications.
  Checks whether run from within ASK/RULE or not!"

 `(let ((*evalhook* 'bqli-pred-hook))
        ;; Do it!
        ,args
  );let
)

;;;-----------------------BABYLON-QUERY-LANGUAGE--------------------------------

;; --- bqli-eval-predication ---

(defmacro bqli-eval-predication (pred)
 "This is called by ASK and RULE-IF.
 bqli-varlist, bqli-once and bqli-in-ask are inherited!"

 `(let (bqli-retval)
;(when bqli-once (format t "~%--------------~%PRED-ONCE: ~S" ',pred))

;(emaxps-real-time-ms
   (setf bqli-retval ,pred)
;200)

   (when (equal bqli-retval T)
;(format t "~%eval-predication: T >>> ~S" ',pred)

    ;;;;;;
    ;; IF the predication is a simple LEXPR!
    ;; Here no checking is done...
    ;; Only $PRED checks for this!
    ;;
    ;; to grant ONE time execution of the continuation!
    ;;
    (setf bqli-retval '(nil nil))

;(setf bqli-cnt-l0 (1+ bqli-cnt-l0))  ;; statistics
   );when

;(when bqli-once (format t "~%VL= ~S" bqli-retval))

;(when (and bqli-once (> (length bqli-retval) 2)) (format t "~%;WARN: VL= ~S" bqli-retval))
   bqli-retval     ;; return-value
  );let
)

;; --- bqli-eval-continuation ---

(defmacro bqli-eval-continuation (&body do-it)
 "This is called by ASK and RULE-THEN[-ONCE].
 bqli-varlist is inherited!
 In case of a only one time continuation the varlist
 should already be reset to a length of 2 !!!"

 `(when bqli-varlist
   (let ((bqli-idx 1) bqli-vals
         (bqli-len (length bqli-varlist))
         (bqli-values bqli-varlist)
         (bqli-vars (first bqli-varlist)))
#|
    ;; Initialisation of the continuation
    (setf bqli-values
;(emaxps-real-time-ms
      (bqli-initialize bqli-values ',do-it)
;200)
    );setf
|#
    (loop (if (>= bqli-idx bqli-len) (return))
     (setf bqli-vals (nth bqli-idx bqli-values))
     (progv bqli-vars bqli-vals

      ,@do-it

     );progv
     (setf bqli-idx (1+ bqli-idx))
    );loop
   );let
  );when
)

;; --- ask --- 

(defmacro ask (arg1 &body do-it)
 `(let ((bqli-varlist nil))                     ;;; shadowing...
   (let ((bqli-once nil) (bqli-in-ask :in-ask)) ;;; shadowing...
;    (if (equal (first ',do-it) :once)
;     (setf bqli-once T))

    (setf bqli-varlist
     (bqli-eval-predication ,arg1))
   );let once+in-ask


    (when (equal (first ',do-it) :once)     ;;; Test!
;(print 'ask-once---------)
;(print bqli-varlist)
     (when (> (length bqli-varlist) 2)
      (setf bqli-varlist (list (first bqli-varlist) (second bqli-varlist)))
;(print bqli-varlist)
     );when
;(print '-----------------)
    );when



   (let ((bqli-in-ask NIL))
    (bqli-eval-continuation ,@do-it)
   );let
   NIL                                 ;; return-value
  );let varlist
)

;; --- tell ---

(defun tell (pred3)
 "(tell [slot inst new-value])
  Here only one simple pred3 is allowed which does NOT define
  new BQL-vars but uses already bound ones!"

 (let ((slot (first pred3))
       (inst (second pred3))
       (new-value (third pred3)))
;(unless pred3 (format t "~%tell: ~S" bqli-xyz))
  (unless (%set-p new-value)
   (setf new-value (%set new-value))
  );unless
  (tell-instance-values slot inst new-value :write-dependents T)
               ;;return-value
 );let
)

;; --- retell ---

(defun retell (pred3 old-value)
 "(retell [slot inst new-value] old-value)
  Here only one simple pred3 is allowed which does NOT define
  new BQL-vars but uses already bound ones!"

 (let ((slot (first pred3))
       (inst (second pred3))
       (new-value (third pred3)))
;(unless pred3 (format t "~%retell: ~S" bqli-xyz))
;(format t "~%retell: ~S ~S" pred3 old-value)
  (unless (or (%set-p old-value)
              (equal old-value _))
   (setf old-value (%set old-value))
  );unless
  (retell-instance-values slot inst new-value old-value :write-dependents T)
               ;;return-value
 );let
)

;; --- untell ---

(defun untell (pred3)
 "(untell [slot inst old-value])
  Here only one simple pred3 is allowed which does NOT define
  new BQL-vars but uses already bound ones!"

 (let ((slot (first pred3))
       (inst (second pred3))
       (old-value (third pred3)))
;(unless pred3 (format t "~%untell: ~S" bqli-xyz))
  (unless (or (%set-p old-value)
              (equal old-value _))
   (setf old-value (%set old-value))
  );unless
  (untell-instance-values slot inst old-value :write-dependents T)
               ;;return-value
 );let
)

#|*****************************************************************
*   Macro-Character-Expansion                                     *
*****************************************************************|#

(set-macro-character #\]
 #'(lambda (stream char)
    (declare (ignore stream char))
    :bqli-reached-end                ;; return value
   )

 NIL    ;; default-value to non-terminating-p
 *emaxps-readtable*  ;; add it to the current (here: the default) readtable !!!
)

(set-macro-character #\[
 #'(lambda (stream char)
    (declare (ignore char))
    (let ((i 0) ret (lst nil))
     (loop
      (setf ret (read stream t nil t))
      (when (eql ret :bqli-reached-end)
       (return (append '($pred) lst))
      );when
      (setf lst (append lst (list ret)))
      (setf i (+ 1 i))
     );loop
    );let
   );lambda

 NIL    ;; default-value to non-terminating-p
 *emaxps-readtable*  ;; add it to the current (here: the default) readtable !!!
)

;; -------------------- $term --------------------

(set-macro-character #\!
 #'(lambda (stream char)
    (declare (ignore char))
    (let ((ret (read stream t nil t)))
     (append '($term) (list
      ret                   ;; to eval ret, too
;      (list 'quote ret)    ;; to avoid evaluation
     ))
    );let
   );lambda

 NIL    ;; default-value to non-terminating-p
 *emaxps-readtable*  ;; add it to the current (here: the default) readtable !!!
)

;;; eof

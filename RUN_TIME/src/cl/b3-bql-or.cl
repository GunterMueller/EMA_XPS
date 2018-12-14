#|*****************************************************************
*      b3-bql-or.cl         Hans Groschwitz         15.09.94      *
*                           Andreas Weigand                       *
*                                                                 *
*   bqli-initialize noetig, da Var's, die oben nicht gebunden     *
*   werden (e..g PRED3 fails), die aber unten verwendet werden,   *
*   dann auf '_x stehen muessten!                                 *
*                                                                 *
*   Emulation routines for the Babylon Query Language used in     *
*   the XPS-shell babylon-3                                       *
*                                                                 *
*   B3 Reference Manual, Chapter 2                                *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")


;; --------------------------CHECKING------------------------------

;; --- bqli-$pred-p ---

(defun bqli-$pred-p (arg)
 "in spite of chacking whether in rule or ask do both checks!!
 (ASK from toplevel differs from ASK in tasks!)"
 (and arg
      (listp arg)
      (or (equal (first arg) '$pred)   ;; if:   in rule!
          (and (= 3 (length arg))      ;; else: already macro-expand'ed!
               (equal 'let (first arg))
               (equal '((*EVALHOOK* 'BQLI-PRED-HOOK)) (second arg))
          );and
      );or
 );or
)

;; --- bqli-not-p ---

(defun bqli-not-p (arg)
 "see above..."
 (and (bqli-$pred-p arg)
      (if (equal (first arg) '$pred)
       (equal (second arg) 'not)
      ;else
       (equal (first (third arg)) 'not)
      );if
 );and
)

;;;-----------------------VARLIST-OPs------------------------------

;; --- bqli-or-retrievefrom-varlist ---

(defun bqli-or-retrievefrom-varlist (var vlist)
 "retrieve a single BQL-var from an existing varlist"

 (let (bqli-pos
       (bqli-vars (first vlist))
       (bqli-vals (rest  vlist))
      )
  (setf bqli-pos (position var bqli-vars))
  (if bqli-pos  ;; Position returns NIL, if var not in vlist!!!
   (let ((bqli-tmp (list (list (nth bqli-pos bqli-vars)))))
    (bqli-loop bqli-i1 bqli-vals
     (setf bqli-tmp (append bqli-tmp (list (list
      (nth bqli-pos bqli-i1)))))
    );loop
    bqli-tmp
   );let
  ;; else
   nil
  );if
 );let
)

;; --- bqli-or-remove-nth-from-list ---

(defun bqli-or-remove-nth-from-list (pos lst)
 (append
  (reverse (nthcdr (- (length lst) pos) (reverse lst)))
  (nthcdr (1+ pos) lst)
 )
)

;; --- bqli-or-removefrom-varlist ---

(defun bqli-or-removefrom-varlist (var vlist)
 (let (bqli-pos
       (bqli-vars (first vlist))
       (bqli-vals (rest  vlist))
      )
  (setf bqli-pos (position var bqli-vars))
  (if bqli-pos  ;; Position returns NIL, if var not in vlist!!!
   (let ((bqli-tmp (list (bqli-or-remove-nth-from-list bqli-pos bqli-vars))))
    (bqli-loop bqli-i1 bqli-vals
     (setf bqli-tmp (append bqli-tmp (list
      (bqli-or-remove-nth-from-list bqli-pos bqli-i1))))
    );loop
    bqli-tmp
   );let
  ;; else
   vlist
  );if
 );let
)

;; ----------------------- [OR -------------------------

;; --- bqli-or-appendto-varlist ---

(defun bqli-or-appendto-varlist (old new)
 "old kann eine Ergebnisliste ODER nil sein!
  new ist immer eine Ergebnisliste!
  bqli-varlist wird NICHT BREITER, nur LAENGER!
  Es wird vorausgesetzt, dass die Vars in alt und neu
  genau gleich heissen und an der selben Stelle stehen!"

 (if old
  (append old (rest new))
 ;; else
  new
 );if
)

;; --- bqli-or-mergeto-varlist ---

(defun bqli-or-mergeto-varlist (var vlist)
 "add two independent varlists of same lenght (!)
 to a new one. Resulting list has the same length !!!
 var may be NIL, too!"

 (when (and var (/= (length var) (length vlist)))
  (emaxps-error "bqli-mergeto-varlist: differing lengths~% ~S ~S" var vlist))

 (if var
  (let ((i 0) (len (length var)) (res nil))
   (loop (if (>= i len) (return))
    (setf res (append res (list (append (nth i var) (nth i vlist)))))
    (setf i (1+ i))
   );loop
   res      ;;return-value
  );let
 ;else
  vlist     ;;return-value
 );if
)

;; --- bqli-or-sort-odeps ---

(defun bqli-or-sort-odeps (odeps ndeps)
 "odeps and ndeps are varlists,
 odeps is to be sorted into the var-order of ndeps
 odeps may be nil, if ndeps is it, too!"

 (if (or odeps ndeps)
  (let* ((fo (first odeps))
         (fn (first ndeps))
         (l1 (length fo))
         (l2 (length fn))
         (lg (length (remove-duplicates (intersection fo fn)))))

   (unless (= l1 l2 lg)
    (emaxps-error "bqli-or-sort-odeps: (partially) independent varlists~% ~S ~S" odeps ndeps))

   (if (> l1 1)
    ;; now it makes sense to spent time for sorting...
    (let* ((tmp nil) (i 0) (vars (first ndeps))
           (len (length vars)))
;;(format t "~%sort-odeps: vars(~S)= ~S~% odeps= ~S~% ndeps=~S~%" len vars odeps ndeps)
     (loop (if (>= i len) (return))
      (setf tmp
       (bqli-or-mergeto-varlist tmp
        (bqli-or-retrievefrom-varlist
         (nth i vars) odeps))
      );setf
      (setf i (1+ i))
     );loop
     tmp    ;;return-value
    );let*
   ;else
    odeps   ;;return-value
   );if
  );let*
 ;else
  NIL       ;;return-value
 );if
)


;; --- bqli-or-combine-list ---

(defun bqli-or-combine-list (old new)
"Fuegt eine neue Werteliste zu der alten Liste hinzu."
 (let (tmp1
       (tmp2 ())
       (tmp3 ())
       (lauf1 1)
       (lauf2 0)
       (lauf3 1))
  (loop (if (>= lauf1 (length old)) (return))
        (setf (nth lauf1 old) (append (nth lauf1 old) (first new)))
        (setf lauf1 (1+ lauf1))
  );;; loop
  (setf tmp1 (length (first old)))
  (loop (if (>= lauf2 tmp1) (return))
        (setf tmp2 (append tmp2 (list (nth lauf2 (first old)))))
        (setf lauf2 (1+ lauf2))
  );;; loop
  (loop (if (>= lauf3 (length new)) (return))
    (setf tmp3 (append tmp3 (list (append tmp2 (nth lauf3 new)))))
    (setf lauf3 (1+ lauf3))
  );;; loop
  (setf (first old) (append (first old) (first new)))
  (setf old (append old tmp3))
  old 
 );;; let
)


;; --- bqli-or-append-varlists ---

(defun bqli-or-append-varlists (odeps ndeps nindeps)
 "odeps and ndeps are varlists with the same vars
 in the same order; indeps may be nil, too."

 (unless (and odeps ndeps)
  (emaxps-error "bqli-or-append-varlists: (partially) empty lists~% ~S ~S" odeps ndeps))
  (if nindeps
    (bqli-or-combine-list ndeps nindeps) 
  ;;; else
    (bqli-or-appendto-varlist odeps ndeps)
  );; if
)


;; --- bqli-or-modify-varlist ---

(defun bqli-or-modify-varlist (old new)
 "returns a varlist
  modifing old with new."

;;(format t "~%----------~%[~S old= ~S~% new= ~S~%" old new)
 (if (and old (not (equal old '(nil nil)))) 
  (if (and new (not (equal new '(nil nil))))
   (let* ((ovars (first old))
          (nvars (first new))
          (deps (intersection ovars nvars))
          (indeps (set-exclusive-or ovars nvars))
          (oindeps nil) (nindeps nil)
          (odeps nil) (ndeps nil)
          tmp)

    ;;; different BQL-varnames are treated the same way in [and and [or!
    ;;;
    (when indeps
     (setf tmp old)
     (mapcar #'(lambda (x)
                (setf tmp (bqli-or-removefrom-varlist x tmp))
               );lambda
             deps
     );mapcar
     (setf oindeps tmp)
     (when (equal oindeps '(nil nil))
      (setf oindeps nil))
     ;;;
     (setf tmp new)
     (mapcar #'(lambda (x)
                (setf tmp (bqli-or-removefrom-varlist x tmp))
               );lambda
             deps
     );mapcar
     (setf nindeps tmp)
     (setf nindeps (remove-duplicates nindeps :test #'equal))
     (when (equal nindeps '(nil nil))
      (setf nindeps nil))
    );when

    ;;; equal BQL-varnames are treated different in [and and [or!
    ;;;
    (when deps
     (setf tmp old)
     (mapcar #'(lambda (x)
                (setf tmp (bqli-or-removefrom-varlist x tmp))
               );lambda
             indeps
     );mapcar
     (setf odeps tmp)
     (setf odeps (remove-duplicates odeps :test #'equal))
     (when (equal odeps '(nil nil))
      (setf odeps nil))
     ;;;
     (setf tmp new)
     (mapcar #'(lambda (x)
                (setf tmp (bqli-or-removefrom-varlist x tmp))
               );lambda
             indeps
     );mapcar
     (setf ndeps tmp)
     (setf ndeps (remove-duplicates ndeps :test #'equal))
     (when (equal ndeps '(nil nil))
      (setf ndeps nil))
    );when

    ;;; to simplify comparisons
    ;;;
    (setf odeps (bqli-or-sort-odeps odeps ndeps))

;;(format t "~%  oindeps= ~S~%  nindeps= ~S~%    odeps= ~S~%    ndeps= ~S" oindeps nindeps odeps ndeps)
    (if nindeps
      (setf indeps (bqli-or-combine-list oindeps nindeps))
    );;; if

    (if odeps                                ;; !odeps <==> !ndeps
      (bqli-or-append-varlists
               odeps ndeps nindeps)  ;; return-value [OR
    ;else
     indeps                     ;;  no-dependencies return-value
    );if

   );let*
  ;else !new
    old   ;;return-value  (or)
  );if 
 ;else !old
  (if (and old (not new))
      old
   ;;; else
      new  ;;return-value
  );; if
 );if
)


(defvar bqli-or-nth-form nil)

;; --- bqli-or-hook ---

(defun bqli-or-hook (form env)
 "Each of the args to bqli-and has to be serviced."

 (setf form (symbol-value form))
 (when (bqli-not-p form)
  (emaxps-error "[OR: invalid operand: [NOT"))
 (unless (bqli-bql-p form)
  (setf form (list 'bqli-eval-lexpr form)))
 (evalhook form nil nil env)
)

;; --- bqli-or ---

(defmacro bqli-or (&rest args)
 "Dispatcher for [OR whose args may include LISP-expressions, 
 match, element-of and simple and compound predications.
 It is always run from within ASK/RULE!
 Each of the constructs within an [OR modifies bqli-varlist,
 hence here a copy with the original contents is held to do final
 modification of the original bqli-varlist!"

 `(let ((bqli-varlist-save bqli-varlist)
        (bqli-or-has-succeeded NIL)
        (bqli-varlist bqli-varlist)
         (bqli-args ',args) bqli-or-ret
         bqli-or-nth-form
         (bqli-idx 0) bqli-len)
   (setf bqli-len (length bqli-args))
   (loop (if (>= bqli-idx bqli-len) (return))
    (setf bqli-varlist NIL)
    (setf bqli-or-nth-form (nth bqli-idx bqli-args))
;;(format t "~%bqli-or-nth-form: ~s" bqli-or-nth-form)
;;(format t "~%bqli-varlist: ~s" bqli-varlist)
    (setf bqli-or-ret
     (let ((*evalhook* 'bqli-or-hook))
      ;; Do it!
      bqli-or-nth-form
     );let
    );setf
;;(format t "~%bqli-or-ret: ~s" bqli-or-ret)

(when bqli-or-ret
 (setf bqli-or-has-succeeded T)
);when

    (setf bqli-varlist-save
     (bqli-or-modify-varlist
      bqli-varlist-save bqli-or-ret)
    );setf
;;(format t "~%[or varlist= ~S" bqli-varlist-save)

;;;;;;;;under construction ANWE   (format t "~%[or doit= ~s" doit)
;; wg. Belegung der UNvorbelegten Var's im CONTINUATION Teil

#|
Wenn eine Variable im Bedingungsteil [or neu belegt wuerde, aber in der 
continuation nicht genutzt wird, wird die praedikation erst gar nicht
ausgefuehrt !

--------------------------------------------------

(ask [or [and [test-slot-1 willi _x] (equal _x 1)]
[test-slot-1 heiko _y]]
(print _x)
(print _y))


1 
_Y 
_X 
2 
NIL
> (ask [or [and [test-slot-1 willi _x] (equal _x 2)]
[test-slot-1 heiko _y]]
(print _x)
(print _y))

*** - EVAL: variable _X has no value
1. Break>
--------------------------------------------------
|#

    ;; here in [OR: NO check for (equal nil bqli-varlist) !!!
    (setf bqli-idx (1+ bqli-idx))
   );loop
   (if bqli-or-has-succeeded
    bqli-varlist-save    ;; return modified temporary bqli-varlist
   ;else
    NIL
   );if bqli-or-has-succeeded
  );let*
)

;;; eof


#|
===========================================================
  (rul3-if
   ($pred and
    ($pred or
     ($pred and
      ($pred a-stufe-gegeben stufe _geg)
      (string= _geg "ja")
     )
     ($pred a-stufe stufe _a-stufe)
    )
    ($pred i-stufe stufe _i-stufe)
   )

Klappt, solange _geg nicht in der Continuation verwendet wird, falls string= 
nicht erfuellt ist.

(retell ($pred d zahnrad (/ (* 2 _a-stufe) (+ _i-stufe 1))) _)
----------------------------

  (rul3-if
   ($pred and
    ($pred or
     ($pred and ($pred auslegungsziel getriebe _ziel) (string= _ziel "andere"))
     ($pred and ($pred stufenzahl-nach getriebe _name) (string= _name "andere")
    ))
    ($pred and ($pred i-gesamt getriebe _i-gesamt) (<= _i-gesamt 5.64))
 )))

KLAPPT

keine bql-var nutzung
-------------------

  (rul3-if
   ($pred or (string= (name-of zahnrad) "RITZEL1")
    (string= (name-of zahnrad) "RITZEL2") (string= (name-of zahnrad) "RITZEL3")
    (string= (name-of zahnrad) "RITZEL4")
 )))

KLAPPT

keine bql-var nutzung
-------------------

  (rul3-if
   ($pred and ($pred b rad _b) ($pred mn stufe _mn) ($pred beta stufe _beta)
    ($pred beta-max stufe _beta-max) ($pred beta-min stufe _beta-min)
    ($pred or
     (is-undetermined-set (ask-instance-values 'eps-beta-min (name-of stufe)))
     (is-undetermined-set (ask-instance-values 'eps-beta (name-of stufe)))
     (is-undetermined-set (ask-instance-values 'eps-beta-max (name-of stufe)))
 ))))

KLAPPT NICHT!!!

keine bql-var nutzung
-------------------

===========================================================
|#

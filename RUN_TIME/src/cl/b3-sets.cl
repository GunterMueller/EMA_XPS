#|*****************************************************************
*     b3-sets.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the babylon-3 like data type SET       *
*   and set manipulating functions                                *
*                                                                 *
*   babylon-3 uses an own data type set, but it does not allow    *
*   the usage of vectors an arrays. Here vectors are used to      *
*   emulate babylon-3 SETs !                                      *
*                                                                 *
*   B3 Reference Manual, Chapter 7 (partially)                    *
*                                                                 *
*****************************************************************|#

;;;;;;;;;
;; B3 Sets duerfen eine Typ-Restriktion haben wie zB
;;     :TYPE 'hauptstadt  (Frame)
;; oder sind allg vom Typ T  !!
;;;;;;;;;
;; Dies wird z.Zt. nicht unterstuetzt!
;;;;;;;;;




;; Symbole, die nur fuer den internen Gebrauch bestimmt sind,
;; mit % anfangen lassen, statt bqli- !!!


;; In den Doku-String immer die wichtigsten Hinweise aus dem
;; B3-Ref.Buch abpinnen !!!


;; Hinweis, wenn eine gleichlautende Funktion bereits von 
;; EMA-XPS bereitgestellt wird !!!


;; Existenz checken !!! -----------




(in-package "BABYLON")

;;---------------BASIC SET-TO-SLOT CONVERSION------------------

;; --- %set-to-slot ---

(defun %set-to-slot (set)
 (case (length set)         ;;; faster than: (cardinality-of-set values)
  (0 '-)                    ;;; is-empty-set   OR   is-undetermined-set !!!
  (1 (list (svref set 0)))  ;;; only one element --> store it (for convenience)
  (t (list set))            ;;;        \_ faster than: (%element-of-set values)
 );cond
)

;; --- %slot-to-set ---

(defun %slot-to-set (slot)
 (if (equal '- slot)
  (%set)                    ;;; undetermined OR empty SET!
 ;else
  (progn
   (setf slot (first slot))
   (if (%set-p slot)
    slot                    ;;; sets are passed.
   ;else
    (%set slot)             ;;; a simple value is stored as '(x)
   );if                     ;;;      \_ compare: '(-) vs. '- !!!
  );progn
 );if
)

;;-------------------------------------------------------------



;;  Kap. 7 -->

;;---------------------OPERATIONS-ON-SETS----------------------

;; --- add-to-set ---

(defun add-to-set (elem set &key (test #'equal))
 "B3 supports different types of set-pointers"

 (when (%set-p elem)
  (emaxps-error "add-to-set: ~S is a SET" elem))
 (unless (%set-p set)
  (emaxps-error "add-to-set: ~S should be a SET" set))

 ;; type checking may cause an error:
 ;; if all elements of a set have the same type,
 ;; check the first with elem. CAVEAT: what about empty set ?!?
 ;(when (equal (the-type-of (first lst)) (the-type-of elem))

 (let ((lst (%set-to-list set)))
  (if (member elem lst :test test)
   ;; set-of ...
   set
  ;else
   ;; new-set-of ...       (TRAIL-ing!)
   (%list-to-set (cons elem lst))
  );if
 );let
)

;; --- cardinality-of-set ---

(defun cardinality-of-set (set)
 (unless (%set-p set)
  (emaxps-error "cardinality-of-set: ~S should be a SET" set))
 ;;;;;;
 ;; length works the same on vectors and lists !!!
 (length set)
)

;; --- difference-of-sets ---

(defun difference-of-sets (set1 set2 &key type (test #'equal))
 "Alle Elemente aus set1, die nicht in set2 enthalten sind"
 ;; S1 \ S2 ...
 ;; --------------
 ;; type checking may cause an error:
 (declare (ignore type))
 (unless (%set-p set1)
  (emaxps-error "difference-of-sets: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "difference-of-sets: ~S should be a SET" set2))
 (%list-to-set
  (set-difference (%set-to-list set1) (%set-to-list set2)
   :test test
 ))
)

(defun %difference-of-sets (set1 set2)
 "Used by %untell-op ONLY!"
 (%list-to-set
  (set-difference (%set-to-list set1) (%set-to-list set2)
   :test #'equal))
)

;; --- element-of-set ---

(defun element-of-set (set &key extract-fn)
 (unless (%set-p set)
  (emaxps-error "element-of-set: ~S should be a SET" set))
 (setf set (%set-to-list set))
 (when extract-fn
  (setf set (member-if extract-fn set)))
 (first set)
)

(defun %element-of-set (set)
 "Used by ask-instances-value und set-instance-values only!
 Assumes that set IS a set and extract-fn is NIL.
 ==> first element-of this set!"

 (if (> (length set) 0)
  (svref set 0)           ;;; (svref (vector) 0)=ERROR
 ;else
  NIL                     ;;; (first NIL)=NIL
 );
)

;; --- intersection-of-sets ---

(defun intersection-of-sets (set1 set2 &key type (test #'equal))
 "Schnittmenge"
 ;; type checking may cause an error:
 (declare (ignore type))
 (unless (%set-p set1)
  (emaxps-error "intersection-of-sets: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "intersection-of-sets: ~S should be a SET" set2))
 (%list-to-set
  (intersection (%set-to-list set1) (%set-to-list set2)
   :test test
 ))
)

;; --- is-element-of-set ---

(defun is-element-of-set (elem set &key (test #'equal))
 (when (%set-p elem)
  (emaxps-error "is-element-of-set: ~S is a SET" elem))
 (unless (%set-p set)
  (emaxps-error "is-element-of-set: ~S should be a SET" set))
 (member elem (%set-to-list set) :test test)
)

(defun %is-element-of-set (elem set)
 (member elem (%set-to-list set) :test #'equal)
)

;; --- is-empty-set ---

(defun is-empty-set (set)
 "     !!!     "
 (unless (%set-p set)
  (emaxps-error "is-empty-set: ~S should be a SET" set))
 ;;;;;;
 ;; length works on vectors AND lists!
 (= 0 (length set))
)

(defun %is-empty-set (set)
 (= 0 (length set))
)

;; --- is-set ---

(defun is-set (set)
 (%set-p set)
)

;; --- is-subset-of-set ---

(defun is-subset-of-set (set1 set2 &key (test #'equal))
 (unless (%set-p set1)
  (emaxps-error "is-subset-of-set: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "is-subset-of-set: ~S should be a SET" set2))
 (if (subsetp (%set-to-list set1) (%set-to-list set2)
      :test test)
  set1
 ;else
  NIL
 );if
)

(defun %is-subset-of-set (set1 set2)
 "Called several times by OOP and BQL.
 Assumes set-checking is already done and test is the default test!"

 (if (subsetp (%set-to-list set1) (%set-to-list set2)
      :test #'equal)
  set1
 ;else
  NIL
 );if
)

;; --- is-undetermined-set ---

(defun is-undetermined-set (set)
 "UNTERSCHEIDET ZWISCHEN {} und unbestimmt+{}              !!!
 z.Zt. ein dummy!"

 (unless (%set-p set)
  (emaxps-error "is-undetermined-set: ~S should be a SET" set))
 ;;;;;;
 ;; length works on vectors AND lists!
 (= 0 (length set))
)

(defun %is-undetermined-set (set)
 (= 0 (length set))
)

;; --- list-to-set ---

(defun list-to-set (lst &key type (test #'equal))
 ;; type checking may cause an error:
 (declare (ignore type))
 (unless (listp lst)
  (emaxps-error "list-to-set: ~S should be a LIST" lst))
 (coerce (remove-duplicates lst :test test) 'vector)
)

(defun %list-to-set (lst)
 "WARNING: remove-duplicates is avoided!"
 (coerce lst 'vector)
)

;; --- remove-from-set ---

(defun remove-from-set (elem set &key (test #'equal))
 (when (%set-p elem)
  (emaxps-error "remove-from-set: ~S is a SET" elem))
 (unless (%set-p set)
  (emaxps-error "remove-from-set: ~S should be a SET" set))

 ;; type checking may cause an error:
 ;; if all elements of a set have the same type,
 ;; check the first with elem. CAVEAT: what about empty set ?!?
 ;(when (equal (the-type-of (first lst)) (the-type-of elem))

 (let ((lst (%set-to-list set)))
  (if (member elem lst :test test)
   ;; new-set-of ...
   (%list-to-set (remove elem lst :test test))
  ;else
   ;; set-of ...
   set
  );if
 );let
)

;; --- same-sets ---

(defun same-sets (set1 set2 &key (test #'equal))
 (unless (%set-p set1)
  (emaxps-error "same-sets: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "same-sets: ~S should be a SET" set2))
 (apply test (%set-to-list set1) (%set-to-list set2))
)

;; --- set-to-list ---

(defun set-to-list (set &key extract-fn)
 (unless (%set-p set)
  (emaxps-error "set-to-list: ~S should be a SET" set))
 (setf set (coerce set 'list))
 (when extract-fn
  (let (res)
   (mapcar #'(lambda (x)
              (when (apply extract-fn (list x))
               (setf res (append res (list x)))
              );when
             );lambda
           set
   );mapcar
   (setf set res)
  );let
 );when
 set         ;; return-value
)

(defun %set-to-list (set)
 (coerce set 'list)
)

;; --- subset-of-set ---

(defun subset-of-set (set &key extract-fn (test #'equal) type)
 "UNDER CONSTRUCTION!"
 ;; type checking may cause an error:
 (declare (ignore test type))
 (unless (%set-p set)
  (emaxps-error "subset-of-set: ~S should be a SET" set))
 (setf set (%set-to-list set))
 (when extract-fn
  (let (res)
   (mapcar #'(lambda (x)
              (when (apply extract-fn (list x))
               (setf res (append res (list x)))
              );when
             );lambda
           set
   );mapcar
   (setf set res)
  );let
 );when
 set
)

;; --- symmetric-difference-of-sets ---

(defun symmetric-difference-of-sets (set1 set2 &key type (test #'equal))
 "Vereinigungsmenge ohne Schnittmenge"
 ;; type checking may cause an error:
 (declare (ignore type))
 (unless (%set-p set1)
  (emaxps-error "symmetric-difference-of-sets: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "symmetric-difference-of-sets: ~S should be a SET" set2))
 ;;;;;;
 ;; Speed UP:
 ;; (difference-of-sets
 ;;  (union-of-sets set1 set2 :test test)
 ;;  (intersection-of-sets set1 set2 :test test)
 ;;  :test test)
 ;;
 (setf set1 (%set-to-list set1)
       set2 (%set-to-list set2))
 (%list-to-set
  (set-difference
   (remove-duplicates (append set1 set2) :test test)
   (intersection set1 set2 :test test)
  :test test)
 )
)

;; --- union-of-sets ---

(defun union-of-sets (set1 set2 &key type (test #'equal))
 "Vereinigungsmenge"
 ;; type checking may cause an error:
 (declare (ignore type))
 (unless (%set-p set1)
  (emaxps-error "union-of-sets: ~S should be a SET" set1))
 (unless (%set-p set2)
  (emaxps-error "union-of-sets: ~S should be a SET" set2))
 (%list-to-set (remove-duplicates
  (append (%set-to-list set1) (%set-to-list set2))
  :test test
 ))
)

(defun %union-of-sets (set1 set2)
 "Used by %tell-op ONLY!
 !!!
 Correct would be:
 (reverse (remove-duplicates (reverse (append set1 set2))))
 "

 (%list-to-set (remove-duplicates
  (append (%set-to-list set1) (%set-to-list set2))
  :test #'equal))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- %%set-p ---

(defun %%set-p (set)
 "If not yet macroexpanded, a written SET like {1 2 3}
 is a list of the form (vector 1 2 3). This is tested here:"
 (and (listp set)
      (equal 'vector (first set))
 )
)

;; --- %set-p ---

(defun %set-p (set)
 (and (vectorp set)
      (not (stringp set))
 )
)

;; --- %set ---

(defun %set (&rest args)
 (apply 'vector args)
)

;;;;;;;;;;;; sets ;;;;;;;;;;;;;;

(set-macro-character #\}
 #'(lambda (stream char)
    (declare (ignore stream char))
    :bqli-set-reached-end                ;; return value
   )

 NIL    ;; default-value to non-terminating-p
 *emaxps-readtable*  ;; add it to the current (here: the default) readtable !!!
)

(set-macro-character #\{
 #'(lambda (stream char)
    (declare (ignore char))
    (let ((i 0) ret (lst nil))
     (loop
      (setf ret (read stream t nil t))
      (when (eql ret :bqli-set-reached-end)
       (return (append '(vector) (remove-duplicates lst :test #'equal)))
      );when

      (setf lst (append lst (list ret)))
      (setf i (+ 1 i))
     );loop
    );let
   );lambda

 NIL    ;; default-value to non-terminating-p
 *emaxps-readtable*  ;; add it to the current (here: the default) readtable !!!
)

;;; eof

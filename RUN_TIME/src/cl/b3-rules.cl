#|*****************************************************************
*    b3-rules.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   To be loaded AFTER MISC!                                      *
*   RULES Emulation routines for the babylon-3 like data type     *
*   SET and set manipulating functions                            *
*                                                                 *
*   babylon-3 uses an own data type set, but it does not allow    *
*   the usage of vectors an arrays. Here vectors are used to      *
*   emulate babylon-3 SETs !                                      *
*                                                                 *
*   B3 Reference Manual, Chapters 3 and 7 (partially)             *
*                                                                 *
*****************************************************************|#


;; Symbole, die nur fuer den internen Gebrauch bestimmt sind,
;; mit % anfangen lassen, statt bqli- !!!


;; In den Doku-String immer die wichtigsten Hinweise aus dem
;; B3-Ref.Buch abpinnen !!!


;; Hinweis, wenn eine gleichlautende Funktion bereits von
;; EMA-XPS bereitgestellt wird !!!


;; Existenz checken !!! -----------




(in-package "BABYLON")

;; ----------------- for internal use only -------------------

;;; Valid only during Loading a B3 KB !!!
;;
(defvar b3r-post-process-rulesets-buffer :ILLEGAL)
(defvar b3r-post-process-rulesets-index 0)

;; -----------------------------------------

(defun b3r-initialize-rulesets ()
 "Run by B3 define-knowledge-base at the beginning of loading a KB"

; (print 'now--in--b3r-initialize-rulesets)
 (setf b3r-post-process-rulesets-buffer NIL)
 (setf b3r-post-process-rulesets-index 0)
)

;; -----------------------------------------

(defun b3r-retrieve-prio-and-rname (prio-rname)
 "Assumes a rulename of the form 9993---rule-name"

 (let (rname prio pos)
  (setf prio-rname (string prio-rname))
  (setf pos (search "---" prio-rname))
  (setf prio (read-from-string (subseq prio-rname 0 pos)))
  (setf rname (read-from-string (subseq prio-rname (+ 3 pos))))

  (list prio rname)
 );let
)

;; -----------------------------------------

(defun b3r-sort-comparator (x y)
 "Guy L. Steele says that this function should be compiled,
 because it is run quite often while sorting.
 This increases speed of sorting!

 returns non-NIL, if x < y     (in the appropriate sense)
 returns NIL,    if x >= y     !!!

 (sort '(1 3 2 7 5 6 4 9 0) #'>)   ==>   (9 7 6 5 4 3 2 1 0)

 x-prio and y-prio contain the computed priority-values of the
 associated rulename.
 The resulting order must be: HIGHest prio rule FIRST !"

 (let ((x-prio (caar x)) (y-prio (caar y)))  ;; prios of rulenames
  (> x-prio y-prio)  ;; use > see the example !!!
 );let
)

;; -----------------------------------------

(defun b3r-sort-and-rename-rules (rsetname)
 "Assumes ruleset rsetname to constain B3 rules
 having rulenames of the form 9993---rule-name
 Restname is assumed to be a keyword already!"

 (let (pos rset work-list (rules (send-kb :rules)))
  (setf pos
   (position-if #'(lambda (x)
                   (equal (first x) rsetname))
                rules)
  );setf pos

  ;; points to the rule-processor's internal list...
  (setf rset (nth pos rules))

  ;; sort a complete copy!
  (setf work-list (copy-list (rest rset)))

  ;; replace each rulename by a list to increase speed of sorting
  (setf work-list
   (mapcar #'(lambda (x)
              (cons (b3r-retrieve-prio-and-rname (first x))
                    (rest x))
             );lambda
           work-list
  ));setf

  (setf work-list (sort work-list #'b3r-sort-comparator))

  ;; replace each rulename-list by the rule-name
  (setf work-list
   (mapcar #'(lambda (x)
              (cons (second (first x))
                    (rest x))
             );lambda
           work-list
  ));setf

  ;; destructively replace the modified ruleset in the
  ;; rule-processor's slot!
  (rplacd rset work-list)

  T   ;; return-value
 );let
)

;; -----------------------------------------

(defun b3r-post-process-rulesets ()
 "Posts the right ordering of rules within the rulesets
 at the end of loading the KB"

; (print 'now--in--b3r-post-process-rulesets)

 (when b3r-post-process-rulesets-buffer
  (format t "~%;;; sorting rules within babylon3 rulesets by priority ...")
  (mapcar #'(lambda (rset)
;             (format t
;              "~%;;; sorting rules of babylon3 ruleset ~S by priority ..."
;              rset)

             ;;;; do-it...
             (b3r-sort-and-rename-rules rset)

             rset
            );lambda
          b3r-post-process-rulesets-buffer
  );mapcar
 );when b3r-post-process-rulesets-buffer

 ;;;;;;;
 ;; Finally disable further USE of define-rule for this KB:
 ;;
 (setf b3r-post-process-rulesets-buffer :ILLEGAL)

 T   ;;return-value
)

;; ----------------------------------------------

(defvar rul3i-varlist nil)  ;; Transport von IF nach THEN
(defvar rul3i-params nil)   ;; Ruleset-task Parameter (read only)
(defvar rul3i-rsname nil)   ;; Rulesetname !

(defmacro rul3-if (arg1)
 `(let ((bqli-varlist nil)       ;; shadowing... (3x)
        (bqli-in-ask :in-rule)
        (bqli-once NIL))

;(format t "~%rul3-if: ~a ~a" (first rul3i-params) (second rul3i-params))
   (setf rul3i-varlist
    (progv (first rul3i-params) (second rul3i-params)
     (bqli-eval-predication ,arg1)
    );progv
   );setf
;(format t "~%rul3-if: returns: ~A" rul3i-varlist)

   rul3i-varlist     ;; return-value  (for B2 rule-processor)
  );let
)

(defmacro rul3-if-once (arg1)
 "in spite of a THEN-ONCE !!!
 This is correctly set by define-rule !!!"

 `(let ((bqli-varlist nil)
        (bqli-in-ask :in-rule)
        (bqli-once T))

;(format t "~%rul3-if-once: ~a ~a" (first rul3i-params) (second rul3i-params))
   (setf rul3i-varlist
    (progv (first rul3i-params) (second rul3i-params)
     (bqli-eval-predication ,arg1)
    );progv
   );setf
;(format t "~%rul3-if-once: returns: ~A" rul3i-varlist)

   rul3i-varlist     ;; return-value  (for B2 rule-processor)
  );let
)

(defmacro rul3-then (&body do-it)
 `(let ((bqli-varlist rul3i-varlist)   ;; transport from IF to THEN...
        (bqli-in-ask NIL)) ;;; a rule may be started from within
                           ;;; the predication (if-undetermined) of an ask !!!

   (progv (first rul3i-params) (second rul3i-params)
    (bqli-eval-continuation
     ,@do-it)
   );progv

   T            ;; return-value  (for B2 rule-processor)
  );let
)

(defmacro rul3-block (rsname varnames &body do-it)
 "Used in RULESET-TASKS to offer parameters and recoursion."

 `(let ((rul3i-rsname ',rsname)          ;; shadowing... (2x)
        (rul3i-params (list ',varnames
          BABYLON::b3i-task-argvals)))   ;; set by (start ...) !!!
   ,@do-it
  );let
)

;;-------------------RULES/RULESETS/RULESETTASKS-------------

;;  Kap. 7 -->    (partially)

;; --- rules-of ---

(defun rules-of (ruleset &key identifier kb)
 "Gibt eine Liste von Regelnamen zurueck, falls das ruleset ex.!
 Erster Loop: Ruleset aus Liste aller Rulesets ausschneiden
 Zweiter Loop: Namen der Rules des Rulesets in eine Liste zusammenstellen
 Alternativer Ansatz:
 B2-Handbuch: S. 374 ...
 ($send (send-kb :rule-processor) :get-rule-set-names)
 ($send (send-kb :rule-processor) :get-rule-names 'kern)
 :IDENTIFIER is dummy in BABYLON2. rules are managed by their names only!"

 (declare (ignore identifier))
 (when kb
  (emaxps-error "rules-of: the key :KB is unsopported yet"))
 (setf ruleset (emaxps-symbol-to-key ruleset))
 (let ((rsets ($send (send-kb :rule-processor) :get-rule-set-names)))
  (unless (member ruleset rsets)
   (emaxps-error "rules-of: ~S is not a known rule set name" ruleset))
  ;;return-value
  ($send (send-kb :rule-processor) :get-rule-names ruleset)
 );let
)

;; --- ruleset-of ---

(defun ruleset-of (rule &key identifier kb)
 "Gibt den zugehoerigen Regelset-Namen der Regel zurueck, falls das ruleset ex.!
 WARNUNG: es koennen zwei Regeln gleichen Namens in unterschiedlichen Sets ex.!
 Daher erster Treffer. Da B3 hier mit Instanzen arbeitet, kann die Emulation
 hier versagen!
 Ruleset names are represented by Keywords, not by symbols.
 :IDENTIFIER T returns a SYMBOL, else a KEYWORD!"

 (when kb
  (emaxps-error "ruleset-of: the key :KB is unsopported yet"))
 (let ((all-rulesets ($send (send-kb :rule-processor) :get-rule-set-names))
       (i 0) imax rnames found ruleset)
  (setf imax (length all-rulesets))
  (loop (if (> i imax) (return))
   (setf ruleset (nth i all-rulesets))
   (setf rnames ($send (send-kb :rule-processor) :get-rule-names ruleset))
   (setf found (member rule rnames))
   (when found (return))
   (setf i (+ 1 i))
  );loop
  (unless found
   (emaxps-error "ruleset-of: ~S is not a known rule" rule))
  (if identifier
   (emaxps-key-to-symbol ruleset)
  ;else
   ruleset     ;; return-value
  );if
 );let
)

;; ===============================================================

;; Kap. 3 -->   (partially)

;; --- define-rule ---

(defmacro define-rule (rname rsetname &rest do-it)
 "This emulates the babylon3 call. The explanation facility is not
 supported. B2 rules do not support comments, too. The priority
 is ignored here, sorting must have been done by 3TO2 already!"

 `(define-rule-eval ',rname ',rsetname ',do-it)
)

(defun define-rule-eval (rname rsetname do-it)
 "The evaluator ..."

 (when (equal b3r-post-process-rulesets-buffer :ILLEGAL)
  (emaxps-error "define-rule: illegal attempt to add a rule AFTER load-KB"))
 (setf b3r-post-process-rulesets-index
  (1+ b3r-post-process-rulesets-index))

 (let (pos props if-part then-part then-once prio)
  (setf pos (position (read-from-string "if") do-it))
  (unless pos
   (emaxps-error "define-rule: keyword IF missing"))
  (setf props (reverse (nthcdr (- (length do-it) pos) (reverse do-it))))

  ;; get the b3-prio value (must be an integer, defaults to 0)
  ;;
  (setf prio (getf props :priority))
  (if (remf props :priority)
   (unless (integerp prio)
    (emaxps-error "define-rule: priority shoud be an integer"))
  ;else
   (setf prio 0)
  );if

  ;;; algorithm of JLeg's 3TO2...
  ;;
  (setf prio (- (* 10000 prio) b3r-post-process-rulesets-index))

  ;; temporarily change the name of the rule for postprocessing
  ;;
  (setf rname (read-from-string (format nil "~A---~A" prio rname)))

  ;; B2 Rsets are keywords, not symbols!
  ;;
  (setf rsetname (emaxps-symbol-to-key rsetname))

  ;; mark the concerned rset for postprocessing
  ;;
  (unless (member rsetname b3r-post-process-rulesets-buffer)
   (setf b3r-post-process-rulesets-buffer
    (cons rsetname b3r-post-process-rulesets-buffer)))

  ;; not supported:
  (remf props :documentation)
  (remf props :explanation)
  (when props
   (emaxps-error "define-rule: illegal properties ~S" props))
  (setf do-it (nthcdr (1+ pos) do-it))

  (setf pos (position (read-from-string "then") do-it))
  (unless pos
   (setf pos (position (read-from-string "then-once") do-it))
   (unless pos
    (emaxps-error "define-rule: keyword THEN/THEN-ONCE missing"))
   (setf then-once T)
  );unless
  (setf if-part (reverse (nthcdr (- (length do-it) pos) (reverse do-it))))
  (setf then-part (nthcdr (1+ pos) do-it))
  (setf do-it (list
               (list '$and
                (list (if then-once 'rul3-if-once 'rul3-if) (first if-part)))
               (list '$execute
                (cons 'rul3-then then-part))
              );list
  );setf
  ($send (send-kb :rule-processor) :emaxps-make-rule rname rsetname do-it)
 );let
)

;; --- define-ruleset-task ---

(defmacro define-ruleset-task (tname targs &rest do-it)
 "This emulates the babylon3 call. The explanation facility is not
 supported, hence description and subtasks are ignored.
 Compile time problem. The symbol 'actions is generated at compile
 time in package :BABYLON and not at runtime in :KB-NAME (e.g.) !!!"

 `(define-task-ruleset-eval ',tname ',targs ',do-it)
)

(defun define-task-ruleset-eval (tname targs do-it)
 "The evaluator ..."

 (let (pos props docu mode confl-res)
  (setf pos (position (read-from-string "actions") do-it))
  (unless pos
   (emaxps-error "define-task: keyword ACTIONS missing"))
  (setf props (reverse (nthcdr (- (length do-it) pos) (reverse do-it))))
  (setf docu (getf props :documentation))
  (remf props :documentation)
  (setf mode (getf props :mode))
  (remf props :mode)
  (setf confl-res (getf props :conflict-resolution))
  (remf props :conflict-resolution)
  ;; not supported:
  (remf props :subtasks)
  (remf props :explanation)
  (when props
   (emaxps-error "define-ruleset-task: illegal properties ~S" props))
  (unless (stringp docu)
   (setf docu ""))
#|
  (when confl-res
;   (emaxps-error "define-ruleset-task: conflict-resolution is not supported yet")
   (format t "~%WARNING: :CONFLICT-RESOLUTION is not supported yet")
  );when
|#

;;HG: tmp...
(when (equal mode :forward)
;;; (print :forward)
; (format t "~%WARNING: define-ruleset-task: ~A
;         mode :FORWARD is currently unsupported, assuming :SEQUENTIAL"
;     tname)
 (setf mode :sequential)
);when

  (unless (equal mode :sequential)
   (emaxps-error "define-ruleset-task: only mode sequential is currently supported"))
  (setf do-it (nthcdr (1+ pos) do-it))

  (when (equal (first do-it) :without-constraint-check)
;   (format t
;    "~%;INFO: while defining ruleset-task ~S:~%;  found :without-constraint-check"
;    tname)
   (setf do-it
    (list
     (cons (read-from-string "do-without-constraint-check")
;      (cons
;       (list 'print (format nil
;        ";;; running ruleset-task ~S :without-constraint-check"
;        tname))
       (rest do-it)
;      );cons
     );cons
    );list
   );setf
  );when wocc

  (eval (list 'deftask tname targs docu
              (append (list 'rul3-block (emaxps-symbol-to-key tname) targs) do-it)
        );list
  );eval
 );let
)

;; ---------------------------- REF 3-23 ----------------------------

;; --- do-exhaustive ---

(defun do-exhaustive ()
 "Forward chained mode only.
 Returns NIL or :NO-RULES"

; (emaxps-error "do-exhaustive: yet not implemented")
; (format t "~%WARNING: do-exhaustive is not implemented, do-all instead!")
;;; (print 'do-exhaustive)
 (do-all)
)

;; --- verify ---

(defmacro verify (verify_expr &optional once &rest cont_forms)
 "Backward chained mode only.
 Works similar to ASK !!! Executes cont_forms in case of instantiablility.
 Returns ???"

 (declare (ignore verify_expr once cont_forms))
 `(emaxps-error "verify: yet not implemented")
)

;; --- do-one ---

(defun do-one ()
 "Sequential mode only.
 Returns NIL or :NO-RULES  (to be implemented yet!)"

 (let (ret)
  ;(format t "ENTERING: --- do-one ---~%")
  (setf ret
   (find-implications rul3i-rsname :do-one)
  );setf
  ;(format t "LEAVING --- do-one --- WITH ~a~%" ret)
  ret
 );let
)

;; --- do-all ---

(defun do-all ()
 "Sequential mode only.
 Returns NIL or :NO-RULES  (to be implemented yet!)"

 (let (ret)
  ;(format t "ENTERING: --- do-all ---~%")
  (setf ret
   (find-implications rul3i-rsname :do-all)
  );setf
  ;(format t "LEAVING --- do-all --- WITH ~a~%" ret)
  ret
 );let
)

;; --- do-while ---

(defmacro do-while (condition)
 "Sequential + Forward.
 Returns :CONDITION-FAILED or :NO-RULES   (currently works different)"

 `(let (rul3i-ret)
;   (format t "~%ENTERING --- do-while ---")
;   (bqli-error "~%rul3i-rsname=~s" rul3i-rsname)
   (setf rul3i-ret
    ;; Sven: Nach jeder Regel, die gematched hat, wird von vorne angefangen!
    ;; Hans: Das :while-all wird damit von B3 nicht aequivalent abgebildet!
    (find-implications rul3i-rsname :while-one
           ;; While: Condition ist eine Liste, die ge-eval-t wird !!!
     '(let ((iii nil))
       (progv (first rul3i-params) (second rul3i-params)
;        (format t "~%do-while: cond is ~a" ',condition)
        (setf iii ,condition)
;        (format t "~%do-while: cond rets ~a" iii)
        iii
       );progv
      );let der Cond ...
    );find-impl
   );setf
;   (format t "~%LEAVING --- do-while --- WITH ~a" rul3i-ret)
   rul3i-ret
  );let
)

;; --- do-until ---

(defmacro do-until (condition)
 "Sequential + Forward.
 Returns :CONDITION-SUCCEDED or :NO-RULES   (currently works different)"

 `(let (rul3i-ret)
;   (format t "~%ENTERING --- do-until ---")
;   (bqli-error "~%rul3i-rsname=~s" rul3i-rsname)
   (setf rul3i-ret
    ;; Needed by zarex!   !!!!!!!
    (find-implications rul3i-rsname :while-one
           ;; While: Condition ist eine Liste, die ge-eval-t wird !!!
     '(let ((iii nil))
       (progv (first rul3i-params) (second rul3i-params)
;        (format t "~%do-until: cond is ~a" ',condition)
        (setf iii ,condition)
;        (format t "~%do-until: cond rets ~a" iii)
        (not iii)    ;; here the inversion to do-while !!!
       );progv
      );let der Cond ...
    );find-impl
   );setf
;   (format t "~%LEAVING --- do-until --- WITH ~a" rul3i-ret)
   rul3i-ret
  );let
)

;;; eof

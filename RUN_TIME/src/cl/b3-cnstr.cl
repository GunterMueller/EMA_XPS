#|*****************************************************************
*    b3-cnstr.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the babylon-3 constraint mechanism     *
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

;; ================= for internal use only ===================

;;; Valid only during Loading a B3 KB !!!
;;
(defvar b3c-post-process-constraints-buffer :ILLEGAL)

;; -----------------------------------------

(defun b3c-initialize-constraints ()
 "Run by B3 define-knowledge-base at the beginning of loading a KB"

; (print 'now--in--b3c-initialize-constraints)
 (setf b3c-post-process-constraints-buffer NIL)
)

;; -----------------------------------------

(defun b3c-post-process-constraints ()
 "Posts the restriction-Network at the end of loading the KB
 B3 seems to build exactly 1 CONSTRAINT-NETWORK per KB !!!
 B2 does not allow multiple network attachments per one slot-ref
 (quite acceptable!). Hence the resulting network can be generated
 at the END of the KB load only !!! Later attempts to add more 
 CONSTRAINTS to this NETWORK must FAIL !!!"

; (print 'now--in--b3c-post-process-constraints)
 (let ((slotlist NIL) (namelist NIL)
       primitives compound-body restriction-body)

  (when b3c-post-process-constraints-buffer
   (format t "~%;;; creating generic babylon3 restriction network ...")

   ;;;;;;;
   ;; Make slotlist contain the slot-refs as interface to
   ;; the generic B2 compound contraint ...
   ;;
   (mapcar #'(lambda (x)
              (setf slotlist (append slotlist (rest x)))
             );lambda
           b3c-post-process-constraints-buffer  ;; keeps the PATTERNS ...
   );mapcar
   (setf slotlist (remove-duplicates slotlist :test #'b3ci-pair=))

   ;;;;;;;
   ;; Make namelist contain the generic constraint-var-names for the
   ;; slot-refs ...
   ;;
   (loop (if (= (length slotlist) (length namelist)) (return))
    (setf namelist
     (append namelist
      (list (read-from-string (format nil "var~A" (1+ (length namelist))))))
   ));loop

   ;;;;;;;
   ;; Now scan the buffer of PATTERNS:
   ;;
   (setf primitives
    (mapcar #'(lambda (x)
               (cons (first x)
                (mapcar #'(lambda (y)
                           (let ((pos (position y slotlist :test #'b3ci-pair=)))
                            (nth pos namelist)
                           );let
                          );lambda y
                        (rest x)
                );mapcar
               );cons
              );lambda x
            b3c-post-process-constraints-buffer
    );mapcar
   );setf primitives

   ;;;;;;;
   ;; Build the generic components...
   ;;
   (setf compound-body
    (list (read-from-string "defconstraint")
          (read-from-string "b3-generic-compound-constraint")
     (list :type (read-from-string "compound"))
     (cons :interface namelist)
     (cons :constraint-expressions primitives)
   ))
   (setf restriction-body
    (list (read-from-string "defrestriction")
          (read-from-string "b3-generic-restriction")
     (list :restrictions
      (cons (read-from-string "b3-generic-compound-constraint")
       slotlist
   ))))

   ;;;;;;;
   ;; ... and create them in advance:
   ;; (compound BEFORE restriction!)
   ;;
;(print compound-body)
;(print restriction-body)
   (eval compound-body)
   (eval restriction-body)

  );when b3c-post-process-constraints-buffer

  ;;;;;;;
  ;; Finally disable further USE of define-constraint for this KB:
  ;;
  (setf b3c-post-process-constraints-buffer :ILLEGAL)

  T   ;;return-value
 );let
)

;; -----------------------------------------

(defun b3ci-pair= (x y)
 "Assumes LISTs of 2 ATOMs. Compares them to be EQUAL"

 (and (equal (first x) (first y))
      (equal (second x)(second y)))
)

; =======================================================
; do :WITHOUT-CONSTRAINT-CHECK
; =======================================================

(defmacro do-without-constraint-check (&rest do-it)
 "used to surround body of tasks, ruleset-tasks and behaviors"

 `(let ((b3i-save-witm
         (emaxps-send-task :within-task-main))
        network)
   (prog2
    (when (equal b3i-save-witm T)  ;; within MAIN...
     (emaxps-send-task :set-within-task-main :without-constraint-check)
;(format t "~%do-without-constraint-check ... begin")
    );when within MAIN

    (progn ,@do-it)

    (when (equal b3i-save-witm T)  ;; within MAIN...
     (emaxps-send-task :set-within-task-main T)
;(format t "~%do-without-constraint-check ... done")

     ;; After doing things :without-constraint-check
     ;; the constraint net must be re-activated...
     ;;
     (setf network (send-kb :get-restrictions 'B3-GENERIC-RESTRICTION))
     (when network
($send network :b3ci-propagate-net)
     );when network

    );when within MAIN
   );prog2
  );let
)

;; -----------------------------------------

(defun without-constraint-check-p ()
 "used by set-instance-values
 if T use :put
 if NIL use :b3ci-put&satisfy !!!"

 (let ((wtm (emaxps-send-task :within-task-main)))
  (cond
   ((equal wtm :without-constraint-check) T)
   ((equal wtm T) NIL)    ;; within MAIN do :b3ci-put&satisfy
   ((equal wtm NIL) T)    ;; never outside MAIN
   (t (emaxps-error "without-constraint-check-p: ~S is illegal" wtm))
  );cond
 );let
)

; =======================================================
; REACTION-ON-CONSTRAINT-INCONSISTENCY
; =======================================================

(defun b3ci-reaction-on-constraint-inconsistency ()
 "Activates a Popup-Dialog, if task
 (REACTION-ON-CONSTRAINT-INCONSISTENCY wb constraint result facts reactions)
 is unknown to this KB !!!

         UNDER CONSTRUCTION!

 It seems that B3 evals the Result of this Task ...
 see (list 'retract ...) ... ?!?"

 (emaxps-error "(REACTION-ON-CONSTRAINT-INCONSISTENCY~%    ~S ~S ~S ~S ~S)"
        (emaxps-current-kb-name) 'b3-generic-compound-constraint
        :INCONSISTENT NIL NIL)
)

; =======================================================
; Extension to the ConstraintProcessor...
; Used to add an alternative access to constraint guarded
; slots (:value property only!)
; AND the B3 Constraint Runtime Emulation!
; =======================================================

(def$method (active-value-mixin :b3ci-put)
            (wocc slot-name new-value)
 "Sets the value of a slot checking
 whether the value is a possible value for the slot.
 Active values are regarded.
 Furthermore it tries to satisfy all attached constraints."

 (let ((check-result ($send self :check-correct-value slot-name new-value)))
  (unless (eq '$ABORT$ check-result)
   (setf new-value (get-check-result check-result))
   ;; -------------------
   (cond ((eq (flavor-type-of ($send self :get-value-only slot-name))
              'restricted-slot)
          ;; -----------------------
          ;; B3: Constraint checking is done only if
          ;;     * no SET-op inited by UNTELL       --> wocc
          ;;     * kb-start-task is executed
          ;;     * :without-constraint-check is NOT set!
          ;;
          (setf wocc (or wocc (without-constraint-check-p)))
          ($send ($send self :get-value-only slot-name) :b3ci-put wocc
                 self slot-name
                 ($send self :active-value-set     ; handles active value
                        new-value
                        ($send self :get-value-only slot-name)
                        slot-name :value)
          ;; -----------------------
         ))
         (t
          ($send self :set-value-only slot-name
                 ($send self :active-value-set     ; handles active value
                        new-value
                        ($send self :get-value-only slot-name)
                        slot-name :value)
                 :value
         ))
   );cond
   ;; -------------------
  );unless
 );let
)

; ----------------------------------------------
; /ema/hans/b22/kernel/consat/normal/rstrbase.cl   -->   defrestriction
; /ema/hans/b22/kernel/consat/normal/rstreval.cl   -->   :update-slot-state
; /ema/hans/b22/kernel/consat/basic/net-prop.cl    -->   :consistent-p
; ----------------------------------------------

(def$method (restricted-slot :b3ci-put)
            (wocc instance slot new-value)
 "adopted from the original :put-if-satisfied and its descendants

  RETURNS new-value       in case of constraint inconsistency
          :inconsistent   otherwise."

(unless wocc (format t "~%PUT restricted: ~S" (list (name-of instance) slot)))
;(format t "~%PUT restricted: wocc= ~S s=~S i=~S nv=~S" wocc slot (name-of instance) new-value)

 ;; the (restricted) slot value itself is set to new-value
 ;;
 (setf value new-value)

 ;; propagate B3 constraint net
 ;;
 (when (and (not wocc)
            (not ($send restriction-net :b3ci-propagate-net)))
  (b3ci-reaction-on-constraint-inconsistency)
 );when

 new-value   ;; return-value
)

; ----------------------------------------------

(def$method (restriction-net :b3ci-propagate-net) ()
 "adopted from :put-if-satisfied and its descendants
 and from :demon in kernel/consat/normal/rstreval.cl"

 (let (ret)
(format t "~%;;; --- propagating B3 constraint net ---")

  ;; init the constraint network from the slots
  ;;
  ($send self :get-initiale-state)
  ($send self :total-init-queue)
  ($send self :b3ci-copy-values)

  ;; tests, whether all attached constraints are satisfied
  ;; 1) propagates the whole c-net
  ;; 2) checks global-consistency of the c-net
  ;;    if :INCONSISTENT
  ;;      ret = NIL
  ;;    else
  ;;      ret = ($send self :interface-assignment)
  ;;
;(print ($send self :interface-assignment))
  ($send self :propagate 'global-consistency)
  (setf ret ($send self :consistent-p))
;(print ($send self :interface-assignment))

  ;; attempt to update all slots of the restriction network
  ;; with the contents of the constraint network variables
  ;; modifies the slot values to be B3 conform!
  ;;
  (when ret
(format t "~%;;; --- writing back to restricted slots ---")
   ($send self :b3ci-replace-values)
  );when

(format t "~%;;; --- propagation done ---")

  ret      ;; return-value
 );let
)

; =======================================================

(def$method (restriction-net :b3ci-copy-values) ()
 "gets the values of the slot-values and puts them into the
 corresponding net-variables.
 the values of the slots are assumed to be SETs of
 only ONE element! (B3-REF: works like an ASK :ONCE ...)
 Honours the way B3 operates on SETs"
  
 (mapc #'(lambda (info-assoc)
          (let* ((net-var (get-net-var info-assoc))
                 (res (copy-slot-value net-var)))
           (when (and res (listp res))   ;; 'unconstrained or '(value)
            (setf res (list (caar res))) ;; B3 ONCE: only first of SET...
           );when
           (add-var-info-values info-assoc res)
          );let*
         );lambda
       net-spec
 )
)

; ----------------------------------------------

(def$method (restriction-net :b3ci-replace-values) ()
 "tries to write the values of the net-variables back to
 the slots (using :try-put).
 the values of the net-variables are assumed to be SETs of
 only ONE element!
 Honours the way B3 operates on SETs"

 (mapc #'(lambda (info-assoc)
          (let* ((net-var (get-net-var info-assoc))
                 (res (get-var-info-values info-assoc)))
           (when (and res (listp res))   ;; 'unconstrained or '(value)
            (setf res (list res))
           );when
           (replace-slot-value net-var res)
          );let*
         );lambda
       net-spec
 );mapc
)

; =======================================================

(def$method (constraint-base :b3ci-get-interface-list) (cpname)
 "Returns the interface-list of a known B3 constraint pattern,
 Finds the corresponding B2 constraint. Activation via SEND-KB."

 (if (member-if #'(lambda (x) (equal cpname (get-name-of-c-assoc x)))
                  constraints)
  ($send (get-constraint cpname) :interface)
 ;else
  NIL
 );if
)

;;-------------------CONSTRAINTS-----------------------------

;;  Kap. 7 -->  (partially)

;; --- constraints-of ---

(defmacro constraints-of (&rest args)
 (declare (ignore args))
 (emaxps-error "constraints-of: yet not implemented")
)

;; --- constraint-pattern-of ---

(defmacro constraint-pattern-of (&rest args)
 (declare (ignore args))
 (emaxps-error "constraint-pattern-of: yet not implemented")
)




;;  Kap. 3 -->  (partially)

;; --- define-constraint ---

(defmacro define-constraint (cname of cpname &rest body)
 "This emulates the babylon3 call. The explanation facility is not
 supported, hence :documentation and :explanation are ignored.
 Compile time problem. Symbols like 'FOR-EACH are generated at compile
 time in package :BABYLON and not at runtime in :KB-NAME (e.g.) !!!

 Syntax: (define-constraint CNAME of CPNAME
          [:DOCUMENTATION string]
          [:EXPLANATION (liste)] 
          [FOR-EACH (var evals-to-list)+ ]
          WITH ($PRED slot inst _bql-var)+
           ;; B3 says, that all related instances must be known at this time!
         )"

 (declare (ignore of))
 `(define-constraint-eval ',cname ',cpname ',body)
)

(defun define-constraint-eval (cname cpname body)
 "The evaluator ... 
 Restriction: it works only, if used at KB load time !!!"

 (when (equal b3c-post-process-constraints-buffer :ILLEGAL)
  (emaxps-error "define-constraint: illegal attempt to add a constraint AFTER load-KB"))

 (let (pos props foreach pred interface cpinterface expanded-list)
  (setf pos (position (read-from-string "for-each") body))
  (if pos
   (progn
    (setf props (reverse (nthcdr (- (length body) pos) (reverse body))))
    (setf body (nthcdr (1+ pos) body))
    (setf pos (position (read-from-string "with") body))
    (unless pos
     (emaxps-error "define-constraint: keyword WITH missing"))
    (setf foreach (reverse (nthcdr (- (length body) pos) (reverse body))))
    (setf body (nthcdr (1+ pos) body))
   );progn
  ;else
   (progn
    (setf pos (position (read-from-string "with") body))
    (unless pos
     (emaxps-error "define-constraint: keyword WITH missing"))
    (setf props (reverse (nthcdr (- (length body) pos) (reverse body))))
    (setf body (nthcdr (1+ pos) body))
   );progn
  );if
  
  ;; not supported:
  (remf props :documentation)
  (remf props :explanation)
  (when props
   (emaxps-error "define-constraint: illegal properties ~S" props))

  (setf pred
   (mapcar #'(lambda (x)
              (unless (and (listp x) (= 4 (length x))
                       (equal (first x) (read-from-string "$PRED")))
               (emaxps-error "define-constraint: syntax error after WITH"))
              (setf interface (append interface (last x)))
              (list (third x) (second x))
             );lambda
           body
   );mapcar
  );setf pred

  (setf cpinterface (send-kb :b3ci-get-interface-list cpname))
  (when (/= (length cpinterface) (length pred))
   (emaxps-error "define-constraint: interface mismatch with constraint pattern"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; modify order of args to the primitive B2 constraint  ;;
  ;; if simple pred after WITH have a different one!!!    ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setf pred
   (cons cpname
    (mapcar #'(lambda (x)
               (nth (position x interface) pred)
              );lambda
            cpinterface
    );mapcar
   );cons
  );setf pred   ; --> (PRUEFE-SUMME-X (DES-FILTERS SUMME-X)
                ;      (DER-RANDBED SUMME-X-MIN) (DER-RANDBED SUMME-X-MAX)) 

  (setf expanded-list (list pred))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; if FOR-EACH build N patterns from one...             ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when foreach
   (setf foreach
    (mapcar #'(lambda (x)
               (let (ret)
                (unless (and (listp x) (= 2 (length x)))
                 (emaxps-error "define-constraint: syntax error after FOR-EACH"))
                (setf ret (eval (second x)))
                (unless (listp ret)
                 (emaxps-error "define-constraint: requiring a LIST of results"))
                ;; in case of instance pointers...
                (list (first x) (mapcar #'name-of ret))
               );let
              );lambda
            foreach
    );mapcar
   );setf foreach      --> ((DES-FILTERS (FILTER)) (DER-RANDBED (RB-FILTER)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; for each instance-variable N representations are    ;;
   ;; possible.      --> successive solution              ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

   (mapcar #'(lambda (x)                                  ;; for each INST-VARIABLE
              ;;
              (let ((var-to-replace (first x)) (tmp NIL)
                    (replacements-list (second x)))
               (mapcar #'(lambda (replacement)            ;; for each REPLACEMENT
                          ;;
                          (setf tmp (append tmp
                            (mapcar #'(lambda (pred-N)    ;; for each already existing PRIM-CNSTR
                                       ;;
                                       (cons (first pred-N)
                                        (mapcar #'(lambda (slot-ref)    ;; for each SLOT-REF
                                                   ;;
                                                   (if (equal (first slot-ref) var-to-replace)
                                                    (list replacement (second slot-ref))
                                                   ;else
                                                    slot-ref    ;; don't change
                                                   );if
                                                  );lambda
                                                (rest pred-N)   ;; list of slot-refs of prim-constr.
                                        );mapcar
                                       ); cons       ;; -->  the modified prim-cnstr...
                                      );lambda
                                    expanded-list    ;; current version ...
                            );mapcar        ;; --> modified current version of expanded-list
                          ));set-append tmp
                         );lambda
                       replacements-list
               );mapcar                  ;; -->  resulting list of primcnstrnts ...
               (setf expanded-list tmp)  ;; enlarged...
              );let
             );lambda
           foreach
   );mapcar
  );when foreach

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now all PRIM PATTERNS are generated.                 ;;
  ;; this info must be collected for the final build      ;;
  ;; of the RESTRICTION NETWORK ...                       ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setf b3c-post-process-constraints-buffer
   (append b3c-post-process-constraints-buffer
           expanded-list))

  cname   ;;return-value
 );let
)

;; --- define-constraint-pattern ---

(defmacro define-constraint-pattern (cpname interface &rest body)
 "This emulates the babylon3 call. The explanation facility is not
 supported, hence :documentation and :explanation are ignored.
 Compile time problem. Symbols like 'RULES are generated at compile
 time in package :BABYLON and not at runtime in :KB-NAME (e.g.) !!!

 Syntax: (define-constraint-pattern CPNAME (interface-vars)
          [:DOCUMENTATION string]
          [:EXPLANATION (liste)]
          RULES
           (KNOWN (known-interface-vars)
            [AND-IF
              (BQL-predication-a-la-ASK-ONCE-using-only-known-interface-vars)]
            THEN { :INCONSISTENT |
                   (IS unknown-interface-var
                       (function unknown-interface-vars))+ |
                   (NEARLY-IS unknown-interface-var
                       (function unknown-interface-vars))+ }
         ))"

 `(define-constraint-pattern-eval ',cpname ',interface ',body)
)

(defun define-constraint-pattern-eval (cpname interface body)
 "The evaluator ..."

 (let (pos props known-vars func predication dep-var pattern checker ctp)
  (setf pos (position (read-from-string "rules") body))
  (unless pos
   (emaxps-error "define-constraint-pattern: keyword RULES missing"))
  (setf props (reverse (nthcdr (- (length body) pos) (reverse body))))
  ;; not supported:
  (remf props :documentation)
  (remf props :explanation)
  (when props
   (emaxps-error "define-constraint-pattern: illegal properties ~S" props))
  (setf body (nthcdr (1+ pos) body))

  (when (rest body)
   (emaxps-error "define-constraint-pattern: more than one RULE is illegal"))
  (setf body (first body))

  (unless (equal (first body) (read-from-string "known"))
   (emaxps-error "define-constraint-pattern: keyword KNOWN missing"))
  (setf body (rest body))
  (unless (listp (first body))
   (emaxps-error "define-constraint-pattern: syntax error after KNOWN"))
  (setf known-vars (first body))
  (setf body (rest body))
  ;; this is always the first thing to be checked!
  (setf ctp (cons (read-from-string "constrained-p") known-vars))

  (setf predication T)
  (when (equal (first body) (read-from-string "and-if"))
   (setf predication (second body))
   (when (and (listp predication)
              (equal (first predication) (read-from-string "$PRED")))
    (emaxps-error "define-constraint-pattern: BQL-predications are currently unsupported!"))

#|
   ;; in B3-EMU slot-values are '- or LISTs ==> the slot-refs mut be prepared to
   ;; work B3 like...
   ;;
   (setf predication
    (list (read-from-string "b3ci-and-if-apply")
     (list (read-from-string "function")
      (list (read-from-string "lambda")
            known-vars predication))
     (cons (read-from-string "list") known-vars)
   ));setf predication
|#

   (setf body (rest (rest body)))
  );when

  (unless (equal (first body) (read-from-string "then"))
   (emaxps-error "define-constraint-pattern: keyword THEN missing"))
  (setf body (rest body))

  (when (rest body)
   (emaxps-error "define-constraint-pattern: multiple continuations aren't supported yet"))
  (setf body (first body))
  (setf pattern (copy-list interface))      ;;;   make a true copy
  (setf checker
   (list (read-from-string "b3ci-reaction-on-constraint-inconsistency")))

  (unless (equal body :inconsistent)
   (unless (listp body)
    (emaxps-error "define-constraint-pattern: syntax error after THEN"))
   (when (equal (first body) (read-from-string "is-nearly"))
    (emaxps-error "define-constraint-pattern: IS-NEARLY isn't supported yet"))
   (unless (equal (first body) (read-from-string "is"))
    (emaxps-error "define-constraint-pattern: keyword IS missing"))
   (setf dep-var (second body))
   (unless (setf pos (position dep-var pattern))
    (emaxps-error "define-constraint-pattern: illegal dependent variable"))
   (when (member dep-var known-vars)
    (emaxps-error "define-constraint-pattern: arg2 should be a dependent variable"))

   ;; in B3-EMU slot-values are '- or LISTs ==> the modifying sequence in a pattern
   ;; must do a set-to-list at the beginning and list-to-set at the end...
   ;; In fact, to realize the :ONCE behavior it has to read the first of a
   ;; slots value and write a (list result) !!!
   ;;
   (setf func (third body))
#|
   (setf func
    (list (read-from-string "b3ci-apply")
     (list (read-from-string "function")
      (list (read-from-string "lambda")
            known-vars func))
     (cons (read-from-string "list") known-vars)
   ));setf func
|#
   (nsubstitute func (nth pos pattern) pattern)   ;;;   destructively !!!

   (setf checker T)
  );unless

  (setf body
   (list (read-from-string "defconstraint")
         cpname
         (list :type (read-from-string "primitive"))
         (cons :interface interface)
         (list :relation
          (list :pattern pattern :if
           (list (read-from-string "and")
                 ctp predication checker              ;; under construction!   <----
         )))
         (list :condition :or)
  ));setf

;(print body)
  (eval body)
 );let
)

;;; eof

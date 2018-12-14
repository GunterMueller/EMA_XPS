#|*****************************************************************
*      b3-oop.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Emulation routines for the babylon-3 like access to slots     *
*                                                                 *
*   babylon-3 uses an own data type set, but it does not allow    *
*   the usage of vectors an arrays. Here vectors are used to      *
*   emulate babylon-3 SETs !                                      *
*                                                                 *
*   B3 Reference Manual, Chapters 3 and 7 (partially)             *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")


;; ================= for internal use only ===================

;;; Valid only during Loading a B3 KB !!!
;;
(defvar b3b-post-process-behaviors-buffer :ILLEGAL)

;; -----------------------------------------

(defun b3b-initialize-behaviors ()
 "Run by B3 define-knowledge-base at the beginning of loading a KB"

; (print 'now--in--b3b-initialize-behaviors)
 (setf b3b-post-process-behaviors-buffer NIL)
)

;; -----------------------------------------

(defun b3b-post-process-behaviors ()
 "Creates starter macros for behaviors at the end of loading the KB
 B3 activates behaviors like functions, B2 needs a starter.
 Hence all starter macros are placed into the package :USER,
 they might overwrite functions in that package!
 Hence a decision has to be made by the user, whether to skip this
 part of the LOAD-KB operation!"

; (print 'now--in--b3b-post-process-behaviors)
 (let (fsym-list trouble-list ev)

  (when b3b-post-process-behaviors-buffer
   (format t "~%;;; creating starter macros for babylon3 style behavior activation ...")

   (setf fsym-list
    (mapcar #'(lambda (x)
               (let ((key x))
                (setf x (emaxps-key-to-symbol x))
                (when (fboundp x)
                 (setf trouble-list (cons x trouble-list)))
                (list x key)
               );let
              );lambda
            b3b-post-process-behaviors-buffer
   ));setf

   (when trouble-list
    (format t
     "~%WARNING: overwriting the following function/macro-definitions:~%~S"
     trouble-list)
   );when

   (setf ev (cons (read-from-string "emaxps-define-others")
     (mapcar #'(lambda (x)
                (let ((sym (first x)) (key (second x)))
                 (read-from-string (format nil
                   "(defmacro ~S (&rest args) `(start-behavior ~S ,@args))"
                   sym key))
                );let
               );lambda
             fsym-list
     );mapcar
   ));setf ev

   (eval ev)     ;; install macros and make them known to the misc-editor

  );when b3b-post-process-behaviors-buffer

  ;;;;;;;
  ;; Finally disable further USE of define-constraint for this KB:
  ;;
  (setf b3b-post-process-behaviors-buffer :ILLEGAL)

  T   ;;return-value
 );let
)

;; ===================== emaxps-eval =======================

(defun emaxps-eval (x)
 "LET: if a global variable with the same name exists,
 let shadows the global value by the temporary one!
 IF no global var. exists, (boundp x)=NIL, (symbol-value x)=ERROR!
 i.e.: The local binding is no more valid within other functions!
 ==>
 Does not work: (let ((z 3)) (emaxps-eval '(sin z)))   !!!"

 (setf x (macroexpand x))
 (cond
  ((null x)
   ;;;;;; the empty LIST!
;   (print '--NIL--)
   NIL
  )
  ((listp x)
   ;;;;;; It's a non-empty LIST!
   (if (equal 'quote (first x))
    (progn
     ;;;;; It's been quoted!
;     (print '--QUOTE--)
     (second x)     ;; the contents of (quote ANY)...
    );progn
   ;else
    (progn
     ;;;;; It's a FUNCTION!
     ;;; Eval the args recoursively!
     ;;; if not a valid function, ERROR!
;     (print '--FUNC--)
     (apply (first x) (mapcar 'emaxps-eval (rest x)))
    );progn
   );if
  );listp...
  ;;;;;; now it must be a SYMBOL!
  ((constantp x)
   ;;;;;; T, "xx", :xx, 3 are CONSTANT!
   ;;; (an input of ''x = '(quote x)) is  constant, too,
   ;;; but it is solved already !!!
;   (print '--CONST--)
   x
  )
  (t
   ;;;;;; It is a globally known VARIABLE!
   ;;; ERROR, if unbound!
;   (print '--VAR--)
   (symbol-value x)
  )
 );cond
)

;; ================ LOW-LEVEL SLOT-ACCESS ==================

;; --- get-instance-value ---

(defun get-instance-value (slot inst)
 "Used for debugging ONLY!"

 (when (symbolp inst)
  (setq inst (get-instance inst)))
 ;; return-value:
 ($send inst :get slot)
)

;; --- set-instance-value ---

(defun set-instance-value (slot inst value)
 "Used for debugging ONLY!"

 (when (symbolp inst)
  (setq inst (get-instance inst)))
 ($send inst :put slot value)
)

;; ----------- do B3-behaviors/SET-oriented -----------------

;; --- get-instance-values ---

(defun get-instance-values (slot inst &key read-dependents if-undetermined)
 "Called by ask-instance-values ONLY (transparently)!"

 (when (symbolp inst)
  ;;; get-instance (B2 macro) ERRORs, if symbol is not a known static instance!
  (setq inst (get-instance inst)))
 (when (equal slot 'has-part)
  (emaxps-error "GET: HAS-PART is not an allowed slot of instance ~S"
   ($send inst :object-name)))
 ;; to avoid interactive behavior of B2:
 (unless (member slot ($send inst :slots))
  (emaxps-error "GET: ~S is not a slot of instance ~S" slot
   ($send inst :object-name)
 ))
 (let (ev if-undet read-dep val)
  (setf val ($send inst :get slot))

  (when (and if-undetermined (equal '- val)
             (setf if-undet ($send inst :get slot :b3i-if-undetermined)))
;   (format t "~%starting :if-undetermined (s=~S i=~S)~% for ~S ..." slot (name-of inst) if-undet)

   ;;; remove the if-undet action to avoid recoursions...
   ($send inst :set slot NIL :b3i-if-undetermined)

   ;;; Do it!
   (setf ev (list 'let
     (list
      (list 'instance (name-of inst))
      (list 'bqli-in-ask NIL)
     );list
     if-undet
   ))
   (eval ev)

   ;;; and get the results!
   (setf val ($send inst :get slot))

   ;;; reinstall if-undet action for further usage...
   ($send inst :set slot if-undet :b3i-if-undetermined)

;   (format t "~%done with :if-undetermined ...")
  );when if-undetermined

  (setf val (%slot-to-set val))

  (when read-dependents
   (when (setf read-dep ($send inst :get slot :b3i-read-dependents))
    (format t "~%starting :read-dependents (~S ~S) for ~S ... DUMMY!"
     (name-of inst) slot read-dep)
   );when
  );when read-dependents

  val   ;; return-value
 );let
)

;; --- set-instance-values ---

(defun set-instance-values (slot inst values &key write-dependents probe
                                                  without-constraint-check)
 "Called by [re/un]tell-instance-values ONLY!"

 (when (symbolp inst)
  (setq inst (get-instance inst)))
 (when (equal slot 'has-part)
  (emaxps-error "SET: HAS-PART is not an allowed slot of instance ~S"
   ($send inst :object-name)))
 ;; to avoid interactive behavior of B2:
 (unless (member slot ($send inst :slots))
  (emaxps-error "SET: ~S is not a slot of instance ~S" slot
   ($send inst :object-name)
 ))
 (let (iname write-dep val oldval beh-ret)
  (setf oldval ($send inst :get slot))   ;; needed in case of write-dependencies !!!

  (when probe
   (format t "~%starting :probe (~S ~S) for ~S ... DUMMY!"
    (name-of inst) slot probe)
  );when probe

  (setf val (%set-to-slot values))

  ;;; Now do the writing...       ($send inst :put slot val)
  ;;; without-constraint-check:   UNTELL does NOT check (JLeg)!
  ;;;
  ($send inst :b3ci-put without-constraint-check slot val)

  (when (and write-dependents
         (setf write-dep ($send inst :get slot :b3i-write-dependents)))

   (setf oldval (%slot-to-set oldval))    ;; --> see get-instance-values!

   (setf iname (name-of inst))
   (mapcar #'(lambda (x)
;              (format t
;               "~%;; (start-behavior :NOTIFY-WRITE-DEPENDENDENT ~S~
;                ~%;;    ~S ~S~
;                ~%;;    ~S ~S)"
;               x iname slot oldval values)
              (setf beh-ret
               ($send inst :NOTIFY-WRITE-DEPENDENT
                  x iname slot oldval values))

;              (format t "~%:noti-wr returns ~S" beh-ret)
              beh-ret

             );lambda
           write-dep
   );mapcar
  );when write-dependents

  values       ;; return-value for success   --> DEPENDS on :PROBE !!!
 );let
)

;; ==========================================================

;; Kap. 2 --> (partially)

;; --- anonymous ---

;; The anonymous BQL-Variable
(defconstant _ '_)

;; ================== OBJECT-ORIENTATION ====================

;; Kap. 7 --> (partially)


;; --- add-dependent ---

(defmacro add-dependent (&rest args)
 (declare (ignore args))
 (emaxps-error "add-dependent: yet not implemented")
)

;; --- behaviors-of ---

(defmacro behaviors-of (&rest args)
 (declare (ignore args))
 (emaxps-error "behaviors-of: yet not implemented")
)

;; --- dependents-of ---

(defmacro dependents-of (&rest args)
 (declare (ignore args))
 (emaxps-error "dependents-of: yet not implemented")
)

;; --- frame-of ---

(defmacro frame-of (&rest args)
 (declare (ignore args))
 (emaxps-error "frame-of: yet not implemented")
)

;; --- identifier-of ---         name-of ???

(defmacro identifier-of (&rest args)
 (declare (ignore args))
 (emaxps-error "identifier-of: yet not implemented")
)

;; --- instances-of ---

(defun instances-of (frame &key (direct nil) (identifier nil))
 "Get a list of static instances.
 Dynamic instances cannot be found this way!"

 (let ((liste (if direct (get-instance-list frame)
                         (get-all-instances frame))))
  (unless identifier
   ;;;;;;
   ;; #'object-of would be OK here.
   ;; (setf liste (mapcar #'%object-of liste))
   ;;
   ;; But the B2 functions get-instance-list and get-all-instances
   ;; only return the names of static instances of the current KB!
   ;; Hence checking isn't necessary.
   ;; lambda, because get-instance is a macro!
   (setf liste (mapcar #'(lambda (x) (get-instance x)) liste))
  );unless
  (reverse liste)   ;; return-value
 );let
)

;; --- is-frame-of ---

(defun is-frame-of (inst fram &key direct kb)
 "UNDER CONSTRUCTION: How about dynamic instances ?!?
 Used by zarex only..."

 (when kb
  (emaxps-error "is-frame-of: the key :KB is unsupported yet"))
 (let ((ilist (if direct
               (get-instance-list fram)
              ;else
               (get-all-instances fram)
              );if
      ))
  (member (name-of inst) ilist)
 );let
)

;; --- name-of ---

(defun name-of (inst)
 "other lisp types are passed."

 (if (%instance-p inst)
  ($send inst :object-name)
 ;else
  inst    ;; return-value
 );if
)

;; --- object-of ---

(defun object-of (inst &optional kb)
 (when kb
  (emaxps-error "object-of: the key :KB is unsupported yet"))
 (%object-of inst)
)

;; --- relations-of ---

(defun relations-of (frame &key part general direct kb)
 ":PART and :GENERAL mey both be set at a time."
 (when kb
  (emaxps-error "relations-of: the key :KB is unsupported yet"))
 ;; temporary:
 (when part
  (emaxps-error "relations-of: :PART under construction"))
 (unless direct
  (emaxps-error "relations-of: :DIRECT under construction"))
 (if general
  (reverse (remove 'has-part (get-frame-slot-names frame)))  ;; see above!
 ;else
  NIL ;; return-value
 );if
)

;; --- remove-dependent ---

(defmacro remove-dependent (&rest args)
 (declare (ignore args))
 (emaxps-error "remove-dependent: yet not implemented")
)

;; --- subframes-of ---

(defun subframes-of (frame &key direct identifier kb)
 "WARNING: B2 handles frames only by their symbol-names!"
 (declare (ignore identifier))
 (when kb
  (emaxps-error "subframes-of: non supported keyword :KB"))
 (remove 'b3i-has-part
  (if direct
   (get-subframes frame)
  ;else
   (get-all-subframes frame)
  );if
 );remove
)

;; --- superframes-of ---

(defun superframes-of (frame &key direct identifier kb)
 "WARNING: B2 handles frames only by their symbol-names!"
 (declare (ignore identifier))
 (when kb
  (emaxps-error "superframes-of: non supported keyword :KB"))
 (remove 'b3i-has-part
  (if direct
   (get-supers frame)
  ;else
   (get-all-supers frame)
  );if
 );remove
)

;; ======================= High Level Slot Access =======================

;; --- are-instance-values ---

(defun are-instance-values (slot inst val &key read-dependents if-undetermined)
 (unless (%set-p val)
  (emaxps-error "are-instance-values: ~S should be a SET" val))
 (%is-subset-of-set val
  (ask-instance-values slot inst
   :read-dependents read-dependents
   :if-undetermined if-undetermined
 ))
)

;; --- ask-instance-value ---

(defun ask-instance-value (slot inst &key read-dependents if-undetermined)
 "returns the first element of the SET or NIL"
 (%element-of-set
  (ask-instance-values slot inst
   :read-dependents read-dependents
   :if-undetermined if-undetermined
 ))
)

;; --- ask-instance-values ---

(defun ask-instance-values (slot inst &key read-dependents if-undetermined)
 "dummy, always returns a set"
 (get-instance-values slot inst
  :read-dependents read-dependents
  :if-undetermined if-undetermined
 )
)

;; --- is-instance-value ---

(defun is-instance-value (slot inst val &key read-dependents if-undetermined)
 "Prueft, ob im SET des slot der inst der Wert val enthalten ist.
 SET-NOT-VALUE-ACCESS-ERROR if val is a SET"

 (when (%set-p val)
  (emaxps-error "is-instance-value: ~S is a SET" val))
 (%is-element-of-set val
  (ask-instance-values slot inst
   :read-dependents read-dependents
   :if-undetermined if-undetermined
 ))
)

;; -------------------- write to a slot --------------------

;; --- %tell-op ---

(defun %tell-op (new old)
 "Handles SETs. NIL=fail, else return a SET.
 Remember the order of args!"

 (if (%is-subset-of-set new old) ;; if subset do nothing!
  NIL
 ;else
  (%union-of-sets old new)   ;; correct order!
 );if
)

;; --- %untell-op ---

(defun %untell-op (new old)
 "Handles SETs (and _ as new). NIL=fail, else return a SET.
 Remember the order of args!"

  (cond
   ((equal _ new) (%set))                   ;; empty (or uninitialized) set !?!
   ((not (%is-subset-of-set new old)) NIL)  ;; if disjunct do nothing!
   (t (%difference-of-sets old new))        ;; remove new from old!
  );cond
)

;; --- %retell-op ---

(defun %retell-op (unew tnew old)
 "Handles SETs (and _ as unew). NIL=fail, else return a SET.
 Remember the order of args!"

 (if (equal _ unew)
  tnew    ;; return-value  (short-cut)
 ;else
  (let (tres (ures (%untell-op unew old)))
   (if ures
    (if (setf tres (%tell-op tnew ures))
     tres ;; return-value  (success)
    ;else
     ures ;; return-value  (when tell-op fails, but untell-op was successful)
    );if
   ;else !ures
    NIL   ;; return-value  (abort on fail)
   );if
  );let
 );if
)

;; -------------------- write to a slot II -----------------

;; --- retell-instance-value ---

(defun retell-instance-value (slot inst tellv untellv &key write-dependents probe)
 (when (%set-p untellv)
  (emaxps-error "retell-instance-value: ~S is a SET" untellv))
 (retell-instance-values slot inst tellv
  (%set untellv)
  :write-dependents write-dependents
  :probe probe
 )
)

;; --- retell-instance-values ---

(defun retell-instance-values (slot inst tellv untellv &key write-dependents probe)
 (unless (or (%set-p untellv) (equal untellv _))
  (emaxps-error "retell-instance-values: ~S should be a SET or _" untellv))
 (when (and (%set-p untellv) (%is-empty-set untellv))
  (emaxps-error "retell-instance-values: tried to untell the empty set"))
 (unless (%set-p tellv)
  (setf tellv (%set tellv)))
 (let (ret (old (ask-instance-values slot inst)))
  (when (setf ret (%retell-op untellv tellv old))
   (setf ret
    (set-instance-values slot inst ret
     :write-dependents write-dependents
     :probe probe
   ))
  );when
  ret         ;; return-value
 );let
)

;; --- tell-instance-value ---

(defun tell-instance-value (slot inst val &key write-dependents probe)
 (when (%set-p val)
  (emaxps-error "tell-instance-value: ~S is a SET" val))
 (tell-instance-values slot inst (%set val)
  :write-dependents write-dependents
  :probe probe
 )
)

;; --- tell-instance-values ---

(defun tell-instance-values (slot inst val &key write-dependents probe)
 "if val is a subset of slot's SET, do nothing!"
 (unless (%set-p val)
  (emaxps-error "tell-instance-values: ~S should be a SET" val))
 (let (ret (old (ask-instance-values slot inst)))
  (when (setf ret (%tell-op val old))
   (setf ret
    (set-instance-values slot inst ret
     :write-dependents write-dependents
     :probe probe
   ))
  );when
  ret         ;; return-value
 );let
)

;; --- undetermined-instance-values ---

(defun undetermined-instance-values (slot inst)
 "does NOT activate any behaviors"
 (%is-undetermined-set (ask-instance-values slot inst))
) 

;; --- untell-instance-value ---

(defun untell-instance-value (slot inst val &key write-dependents probe)
 (when (%set-p val)
  (emaxps-error "untell-instance-value: ~S is a SET" val))
 (untell-instance-values slot inst (%set val)
  :write-dependents write-dependents
  :probe probe
 )
)

;; --- untell-instance-values ---

(defun untell-instance-values (slot inst val &key write-dependents probe)
 (unless (or (%set-p val) (equal _ val))
  (emaxps-error "untell-instance-values: ~S should be a SET or _"))
 (let (ret (old (ask-instance-values slot inst)))
  (when (setf ret (%untell-op val old))
   (setf ret
    (set-instance-values slot inst ret
     :write-dependents write-dependents
     :probe probe
     :without-constraint-check T            ;;; says JLeg ?!?
   ))
  );when
  ret         ;; return-value
 );let
)

;; ===============================================================

;;  Kap. 3 --> (partially)

;; --- an-instance-of ---

(defun an-instance-of (frame arglist)
 "in difference to instance-of rlist and fname are evaluated first!
 Creates a dynamic instance!
 fname=symbol
 rlist=()
 or (list
     (list 'relname :value 3 :read-dependents nil :write-dependents nil)
    ...)"

 (let ((with-spec NIL) (tmp-inst (create-instance-of frame)))
  (loop (if (null arglist) (return))
   (setf with-spec (append with-spec
     (b3i-inst-relation-spec tmp-inst (first arglist) (second arglist))
   ))
   (setf arglist (rest (rest arglist)))
  );loop
  (when with-spec
   (setf with-spec (cons 'with with-spec)))
;(format t "~% an-instance-of: with= ~S" with-spec)
  (create-instance-of frame with-spec)   ;; return-value
 );let
)

;; === start-behavior ===

(defmacro start-behavior (bname arg1 &rest args)
 "Necessary extension to the B3-Language!
 B3 Behaviors are activated like defun's !!!
 B2 Behaviors need a starter...
 --->
 3to2 places this starter and a colon 
 in front of the behaviorname!"

 `(<- ,arg1 ,bname ,arg1 ,@args)
#|
 `(let (ret)
   (format t "~%starting B3 behavior ~S with" ,bname)
   (print '(,bname ,arg1 ,@args))
   (setf ret (<- ,arg1 ,bname ,arg1 ,@args))
   (format t "~%done with behavior ~S" ,bname)
   ret
  );let
|#
)

;; --- define-behavior ---

(defmacro define-behavior (bname bmode arglist &body do-it)
 "bname is NO MORE assumed to be a keyword (to avoid 3to2) !!!
 bmode is :PRIMARY :AFTER :BEFORE and therefore a keyword!
 arglist is a specialized lambda list...

 B2: (defbehavior (FRAME :BMODE :BNAME) (arg1 arg2 ...) . (DO-IT))

 (1) normales Behavior: (define-behavior BNAME BMODE (<SENDER> <X>*)
			 [:DOCUMENTATION ``...''] [:EXPLANATION (...)]
			 [:WITHOUT-CONSTRAINT-CHECK] . (DO-IT))
	with: <SENDER> = (inst=self FRAME)
	AND:       <X> = (argN frameN) | (argN T) | argN

 (2) READ-DEPENDENT:   (define-behavior NOTIFY-READ-DEPENDENT BMODE
			(<DEPENDENT> <SENDER> <SLOT> <SLOT-VAL>)
			 [:DOCUMENTATION ``...''] [:EXPLANATION (...)]
			 [:WITHOUT-CONSTRAINT-CHECK] . (DO-IT))
	with:  <DEPENDENT> = ((one-of :b3i-read-dependents) their-frame)
			;; Run Behavior (length read-dependents) times!
	AND:	  <SENDER> = (inst=self FRAME)
		    <SLOT> = (argN T) | argN
		<SLOT-VAL> = (argN T) | argN

 (3) WRITE-DEPENDENT:  (define-behavior NOTIFY-WRITE-DEPENDENT BMODE
			(<DEPENDENT> <SENDER> <SLOT> <OLD-VAL> <NEW-VAL>)
			 [:DOCUMENTATION ``...''] [:EXPLANATION (...)]
			 [:WITHOUT-CONSTRAINT-CHECK] . (DO-IT))
	with:  <DEPENDENT> = ((one-of :b3i-write-dependents) their-frame)
			;; Run Behavior (length read-dependents) times!
	AND:	  <SENDER> = (inst=self FRAME)
		    <SLOT> = (argN T) | argN
		 <OLD-VAL> = (argN T) | argN
		 <NEW-VAL> = (argN T) | argN

 Transformation rule:
 <X>, <SLOT>, <DEPENDENT>, <SLOT-VAL>, <OLD-VAL> and <NEW-VAL> are treated:
  (if (listp x) (first x) x)
 <SENDER> is treated: (setf FRAME (second x)) und arg=(first x)."

 `(define-behavior-eval ',bname ,bmode ',arglist ',do-it)
)

(defun define-behavior-eval (bname bmode arglist do-it)
 "The Evaluator ..."

 (when (equal b3b-post-process-behaviors-buffer :ILLEGAL)
  (emaxps-error "define-behavior: illegal attempt to add a behavior AFTER load-KB"))

 (let ((wocc NIL) (docu "") tmp frame (len (length arglist)) (done NIL))
  ;; B2 name convention ...
  (setf bname (emaxps-symbol-to-key bname))
  (case bname
   (:NOTIFY-READ-DEPENDENT
    (when (/= len 4)
     (emaxps-error "define-behavior: ~S: number of arguments should be 4" bname))
    (setf frame (second (second arglist)))
   );read-dep
   (:NOTIFY-WRITE-DEPENDENT
    (when (/= len 5)
     (emaxps-error "define-behavior: ~S: number of arguments should be 5" bname))
    (setf frame (second (second arglist)))
   );write-dep
   (t  ;; --- all other behaviors
    (when (= len 0)
     (emaxps-error "define-behavior: ~S: should at least have one argument" bname))
    (setf frame (second (first arglist)))
   );otherwise
  );case

  (setf arglist
   (mapcar #'(lambda (x)
              (if (listp x)
               (first x)
              ;else
               x
              );if
             );lambda
           arglist
   );mapcar
  );setf

  (loop (if (null do-it) (return))
   (setf tmp (first do-it))
   (case tmp
    (:documentation
     (setf do-it (rest do-it))
     (setf tmp (first do-it))
     (when (stringp tmp)
      (setf docu tmp))
    );docu
    (:explanation
     (setf do-it (rest do-it))
    );expl
    (:without-constraint-check
     (setf wocc T)
    );wocc
    (t
     (setf done (append done (list tmp)))
    );otherwise
   );case
   (setf do-it (rest do-it))
  );loop

  (when wocc
;   (format t
;    "~%;INFO: while defining behavior ~S:~%;  found :without-constraint-check"
;    bname)
   (setf done
    (list
     (cons (read-from-string "do-without-constraint-check")
;      (cons
;       (list 'print (format nil
;        ";;; running behavior ~S :without-constraint-check"
;        bname))
       done
;      );cons
     );cons
    );list
   );setf
  );when
  (setf done (cons docu done))

  ;;; append for post processing
  (unless (or (equal bname :notify-read-dependent)
              (equal bname :notify-write-dependent)
              (member bname b3b-post-process-behaviors-buffer))
   (setf b3b-post-process-behaviors-buffer
    (cons bname b3b-post-process-behaviors-buffer)))

  (eval
   (cons 'defbehavior (cons (list frame bmode bname) (cons arglist done)))
  );eval
 );let
)

;; --- define-frame ---

(defmacro define-frame (fname &rest do-it)
 "This emulates the babylon3 call. The explanation facility is not
 supported. B2 rules do not support comments, too."

 `(progn
   (define-frame-eval ',fname ',do-it)
  ;;; B3 stores frames in MCS-instances, which are known globally!
  ;;; This Emulation does this:
  (defvar ,fname ',fname)
  );progn
)

(defun b3i-frame-relation-spec (arglist)
 "UNDER CONSTRUCTION!"

 (let ((if-undetermined  (getf arglist :if-undetermined))
       (initial-value    (getf arglist :initial-value))
       (read-dependents  (getf arglist :read-dependents))
       (write-dependents (getf arglist :write-dependents)))
  ;;;;;;
  ;; (let ((instance self)) ... ) if NON nil !
  ;;;;;;
  (remf arglist :if-undetermined)     ;; LIST of CMDs (default=NIL)
  ;; ---
  ;;;;;;
  ;; eval's to   (!!!)
  ;; LISP-any or SET or INSTANCE-NAME! --> name-of !!!
  ;;;;;;
  (if (remf arglist :initial-value)
   ;; An initial-value has been supplied!
   (progn
    (setf initial-value (emaxps-eval initial-value))
    (when (%set-p initial-value)
     (emaxps-error "INIT-FSLOT: initing SETs is unsupported yet"))
    (setf initial-value (name-of initial-value))
    (setf initial-value (list initial-value))    ;;; ... is always a LIST or '- !!!
   );progn
  ;else
   (setf initial-value '-)   ;; B2-value for uninitialized!
  );if
  ;; ---
  ;; remove unsupported's
  (remf arglist :documentation)
  (remf arglist :explanation)
  ;; remove temporary unsupported's
  (remf arglist :value-restriction)   ;; symbol or frame!  (default=NIL)
  (remf arglist :number-restriction)  ;; (min max)  (default=NIL)
  (remf arglist :read-only)           ;; T or NIL   (default=NIL)
  (remf arglist :initable)            ;; T or NIL   (default=T)
  ;; ---
  (remf arglist :read-dependents)
  (remf arglist :write-dependents)
  ;; ---
  (when arglist
   (emaxps-error "frame-relation-specification: invalid properties ~S" arglist))

  ;;; B3-Bug or Feature ?!?
  (when (and if-undetermined read-dependents)
   (setf if-undetermined NIL read-dependents NIL))

  ;; return-value
  (list                   initial-value
   :b3i-if-undetermined   if-undetermined
   :b3i-read-dependents   read-dependents
   :b3i-write-dependents  write-dependents
  );list
 );let
)

(defun define-frame-eval (fname do-it)
 "The evaluator ..."

 (let (generals parts supers slots partslots)
  (setf generals (getf do-it :general-relations))
  (setf parts (getf do-it :part-relations))
  (setf supers (getf do-it :specializes))
  (remf do-it :general-relations)
  (remf do-it :part-relations)
  (remf do-it :specializes)
  ;; not supported:
  (remf do-it :documentation)
  (remf do-it :explanation)
  (when do-it
   (emaxps-error "define-frame: illegal properties ~S" do-it))
  (setf supers (append (list 'supers 'b3i-has-part) supers))
  (setf slots '(slots))
  (loop (unless generals (return))
   (when (< (length generals) 2)
    (emaxps-error "define-frame: improper general-relation specifications"))
   (setf slots (append slots (list
    (cons (first generals) (b3i-frame-relation-spec (second generals)))
   )));setf
   (setf generals (rest (rest generals)))
  );loop
  (loop (unless parts (return))
   (when (< (length parts) 2)
    (emaxps-error "define-frame: improper part-relation specifications"))
   (setf slots (append slots (list
    (cons (first parts) (b3i-frame-relation-spec (second parts)))
   )));setf
   (setf partslots (append partslots (list (first parts))))
   (setf parts (rest (rest parts)))
  );loop
  (setf slots (append slots (list (list 'has-part
   (list 'active-value partslots
    :b3i-has-part-reader
    :no-update-permitted    ;; b3i-has-part-writer! (B2 default)
   );list
  ))));setf
  (eval
   (list 'defframe fname supers slots)
  );eval
 );let
)

;; --- define-instance ---

(defmacro define-instance (iname of fname &rest args)
 "Creates a static instance and a globally known symbol containing
 the instance-pointer.
 (define-instance iname of fname
  :documentation ``comment''
  :explanation (liste)
  relname1 (:value 3 :read-dependents nil :write-dependents nil)
  ...         ;;;  ^--- are eval'ed!
  relnameN ()
 )"

 (declare (ignore of))
 `(define-instance-eval ',iname ',fname ',args)
)

(defun define-instance-eval (inst frame arglist)
 "The Evaluator ...
 PROBLEM: (send-kb :reset-kb) kills all settings of slots
 and resets the Frame-Defaults, if no WITH-spec is available!
 ==> WITH-specification must be used instead"

 ;;; Remove unsupported properties!
 (remf arglist :documentation)
 (remf arglist :explanation)

 (let ((with-spec NIL) (tmp-inst (create-instance-of frame)))
  ;; Now tmp-inst contains a pointer to a dynamic instance
  ;; whose slot values default to the frame-settings!

  (loop (if (null arglist) (return))
   (setf with-spec (append with-spec
     (b3i-inst-relation-spec tmp-inst (first arglist) (second arglist))
   ))
   (setf arglist (rest (rest arglist)))
  );loop
  (when with-spec
   (setf with-spec (cons 'with with-spec)))

  ($send (send-kb :frame-processor) :new-instance inst frame with-spec)

 );let   ;; here the temp inst is killed!
 (object-of inst)     ;; return-value
)

(defun b3i-inst-relation-spec (tmpinst slot args)
 "Hence at creation time (NOW) the frame already exists,
 ANY slot has at least the following slot-value (and annotation/value pairs):
 (- :b3i-if-undetermined NIL :b3i-read-dependents NIL :b3i-write-dependents NIL)"

 (unless (and slot (symbolp slot))
  (emaxps-error "INIT-SLOT: ~S should be a slotname" slot))
 (unless (member slot (remove 'has-part ($send tmpinst :slots)))
  (emaxps-error "INIT-SLOT: ~S is not a known slot" slot))
 (unless (listp args)
  (emaxps-error "INIT-SLOT: ~S should be a list" args))
 (let (ret value read-dependents write-dependents)

  ;;;;;;;
  ;; PROBLEM: getf returns nil when the property has a value of NIL
  ;;      OR when the property does NOT exist!
  ;;
  (setf value (getf args :value))
  (setf read-dependents (getf args :read-dependents))
  (setf write-dependents (getf args :write-dependents))

  (setf ret NIL)

  ;;;;;;
  ;; REMF returns T if it COULD remove the property,
  ;;  and NIL if the property was not present!
  ;;
  (when (remf args :read-dependents)
   (setf ret (cons :b3i-read-dependents (cons read-dependents ret)))
  );when read-dep

  (when (remf args :write-dependents)
   (setf ret (cons :b3i-write-dependents (cons write-dependents ret)))
  );when write-dep

  (when (remf args :value)
   ;; the property was present!
   (setf value (emaxps-eval value))   ;; evaluate !!!
   (when (%set-p value)
    (emaxps-error "INIT-SLOT: initing SETs is unsupported yet"))
   (setf value (name-of value))
   (setf value (list value))    ;;; ... is always a LIST or '- !!!
   ;; done...

;   (setf ret (cons value ret))
   (setf ret (cons :value (cons value ret)))
  );when value

  (when args
   (emaxps-error "INIT-SLOT: invalid properties ~S" args))

  (if ret               ;; return-value
   (list slot '= ret)
  ;else
   NIL
  );if
 );let
)

;; --- instance-of ---

(defmacro instance-of (fname &rest args)
 "Used by TR1 and DANTEX !!!
 Creates a dynamic instance.
 (instance-of fname
  relname1 (:value 3 :read-dependents () :write-dependents nil)
             ;;;   ^--- are eval'ed!   ^--- are treated special!
  relname2 ()
  ...)
 PROBLEM: local let-variables as arguments to :value must be
 pre-evaluated, before they loose validity!"

 (let (slot vlist tmp prop val (arglist NIL))
  (loop (if (null args) (return))  
   (setf slot (first args))
   (setf vlist (second args))
   (setf args (rest (rest args)))
   ;;;
   (setf tmp NIL)
   (loop (if (null vlist) (return))
    (setf prop (first vlist))
    (setf val (second vlist))
    (setf vlist (rest (rest vlist)))
    (setf tmp (cons prop  ;; which should be KEYWORD!
                    (cons
                     (if (equal :value prop)
                      val   ;; pre-eval only in case of :VALUE (LET-variables problem!)
                     ;else
                      (list 'quote val)
                     );if
                     tmp)
    ));setf tmp
   );loop
   (setf tmp (cons 'list tmp))
   (setf arglist (cons (list 'quote slot) (cons tmp arglist)))
  );loop
  (setf arglist (cons 'list arglist))
  `(instance-of-eval ',fname ,arglist)
 );let
)

(defun instance-of-eval (frame arglist)
 "differs from AN-INSTANCE-OF in :VALUE is not EMAXPS-EVALed!"

 (let ((with-spec NIL) (tmp-inst (create-instance-of frame)))
  (loop (if (null arglist) (return))
   (setf with-spec (append with-spec
     (b3i-inst-relation-spec-2 tmp-inst (first arglist) (second arglist))
   ))
   (setf arglist (rest (rest arglist)))
  );loop
  (when with-spec
   (setf with-spec (cons 'with with-spec)))
;(format t "~% instance-of: with= ~S" with-spec)
  (create-instance-of frame with-spec)   ;; return-value
 );let
)

(defun b3i-inst-relation-spec-2 (tmpinst slot args)
 "DO NOT evaluate the :VALUE property!"

 (unless (and slot (symbolp slot))
  (emaxps-error "INIT-SLOT: ~S should be a slotname" slot))
 (unless (member slot (remove 'has-part ($send tmpinst :slots)))
  (emaxps-error "INIT-SLOT: ~S is not a known slot" slot))
 (unless (listp args)
  (emaxps-error "INIT-SLOT: ~S should be a list" args))
 (let (ret value read-dependents write-dependents)
  ;;;;;;
  ;; ret containts the frame's presets to this slot!
  ;;
  (setf ret ($send tmpinst :get slot))

  ;;;;;;;
  ;; PROBLEM: getf returns nil when the property has a value of NIL
  ;;      OR when the property does NOT exist!
  ;;
  (setf value (getf args :value))
  (setf read-dependents (getf args :read-dependents))
  (setf write-dependents (getf args :write-dependents))

  (setf ret NIL)

  ;;;;;;
  ;; REMF returns T if it COULD remove the property,
  ;;  and NIL if the property was not present!
  ;;
  (when (remf args :read-dependents)
   (setf ret (cons :b3i-read-dependents (cons read-dependents ret)))
  );when read-dep

  (when (remf args :write-dependents)
   (setf ret (cons :b3i-write-dependents (cons write-dependents ret)))
  );when write-dep

  (when (remf args :value)
   ;; the property was present!
   (when (%set-p value)
    (emaxps-error "INIT-SLOT: initing SETs is unsupported yet"))
   (setf value (name-of value))
   (setf value (list value))    ;;; ... is always a LIST or '- !!!
   ;; done ...

;   (setf ret (cons value ret))
   (setf ret (cons :value (cons value ret)))
  );when value

  (when args
   (emaxps-error "INIT-SLOT: invalid properties ~S" args))

  (if ret               ;; return-value
   (list slot '= ret)
  ;else
   NIL
  );if
 );let
)

;;; eof

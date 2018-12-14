#|***************************************************
*   explain.cl    Hans Groschwitz        08.12.94   *
*                 Karsten Vossberg       20.07.95   *
*                                                   *
*   Interface for graphic explanations when inspec- *
*   ting the success of inferencing at runtime.     *
*                                                   *
*   Explanations available with Babylon2:           *
*   * 1) PROLOG: "WHY? ..."                         *
*          Why does InfEng ask for this info?       *
*          To be able to prove ... (SubGoal).       *
*   * 2) RULE: "HOW? ..."                           *
*          How was this state of inference reached? *
*          Facts were treated by rules of rsets ... *
*   * 3) RULE: "INSPECT ..."                        *
*          Search for terms used in rules of        *
*          ONE rest (different matching-modes ex.)  *
*   * 4) RULE: "VIEW ..."                           *
*          Simply views the body of a rule          *
***************************************************|#

(in-package "BABYLON")


;************************************************************
;*   Send text to the explanation window (with C-Translation)   *
;************************************************************

(defun emaxps-send-explain (code liste )
 "emaxps-explain-one-of-many shows a SelectionDialog."

 (unless (listp liste)
  (emaxps-error "emaxps-explain-one-of-many: arg1 should be a list"))
 (unless (and (integerp code) (>= code 0))
  (emaxps-error "expd-explain-one-of-many: arg1 should be a non-negative integer"))
 (emaxps-begin-message :expd-message)

 ;Erst den Uebersetzungs-Code incl. Argumente

 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         liste
 );mapcar
 (emaxps-end-of-message) 
)


;************************************************************
;*   Popup-Dialogs of the explanation window                *
;************************************************************

(defun emaxps-explain-one-of-many (code liste args &optional (func nil))
 "emaxps-explain-one-of-many shows a SelectionDialog.
 The label argument DOES NOT work as with (format ...)!"

 (unless (listp liste)
  (emaxps-error "emaxps-explain-one-of-many: arg1 should be a list"))
 (unless (and (integerp code) (>= code 0))
  (emaxps-error "expd-explain-one-of-many: arg1 should be a non-negative integer"))
 (emaxps-begin-message :expd-one-of-many)

 ;Erst den Uebersetzungs-Code incl. Argumente

 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         liste
 );mapcar
 (emaxps-send-nil) ;Jetzt kommt die Liste!


 (let (answer (orig-liste args))
 (if func   
   (setf args (mapcar func args))
  );if
 (mapcar #'(lambda (x)
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" x)
            )
          args)
  (setf answer (emaxps-eom-get-reply))

  (if (numberp answer)
   (nth answer orig-liste)
  ;;else
   nil
  );if
 );let
)


(defun emaxps-explain-one-of-many2 (liste )
 "emaxps-explain-one-of-many shows a SelectionDialog.
 The label argument DOES NOT work as with (format ...)!"

  (emaxps-begin-message :expd-one-of-many2)
  (let ((answer (emaxps-eom-get-reply)))
   
;(uid-information "~s" (nth answer  liste))
   (if (numberp answer)
     (nth answer liste)
   ;;else
      nil
    );if
  )
)




(defun emaxps-explain-some-of-many (code liste args &optional (func nil))
 "emaxps-explain-some-of-many shows a SelectionDialog.
 The label argument DOES NOT work as with (format ...)!"

 (unless (listp liste)
  (emaxps-error "emaxps-explain-some-of-many: arg1 should be a list"))
 (unless (and (integerp code) (>= code 0))
  (emaxps-error "expd-explain-some-of-many: arg1 should be a non-negative integer"))
 (emaxps-begin-message :expd-some-of-many)

 ;Erst den Uebersetzungs-Code incl. Argumente

 (emaxps-send-mbody "~S" code)
 (mapcar #'(lambda (x)
            (emaxps-separate-mparts)
            (emaxps-send-mbody "~S" x)   ;;; ~S !!!
           )
         liste
 );mapcar
 (emaxps-send-nil) ;Jetzt kommt die Liste!


 (let (answer (orig-liste args))
 (if func   
   (setf args (mapcar func args))
  );if
 (mapcar #'(lambda (x)
             (emaxps-separate-mparts)
             (emaxps-send-mbody "~A" x)
            )
          args)
  (setf answer (emaxps-eom-get-reply))

  (if (listp answer)
   (mapcar #'(lambda (x)
              (nth x orig-liste)
             )
            answer)
  ;;else
   nil
  );if
 );let
)


;************************************************************
;*   Output a message into the explanation window           *
;************************************************************

(defun emaxps-print-explanation (fmt &rest args)
 "Writes text to the explanation PAD
 C adds a Newline at the end of each Message!"

 (emaxps-begin-message :explain-message)
 (apply 'emaxps-send-mbody (cons fmt args))
 (emaxps-end-of-message)
)


;*********************************************************
;*    (1) PROLOG "WHY?"        Explain Context           *
;*********************************************************

(def$method (proc-explain-mixin :emaxps-why-goal) (goalbox)
 "displays the goal and the clause currently used to prove it."

 (emaxps-send-explain 18 (list ($send goalbox :get-goal-on-init 'normal))) ;Zu beweisen ist
 (let ((clause-used (ext-rep-clause ($send goalbox :clause-used))))
  (if clause-used
    (emaxps-send-explain 19 (list (first clause-used) (second clause-used) (cddr clause-used)) ;Mit der Klausel

))))

(def$method (proc-explain-mixin :emaxps-explain-prolog-why) ()
 "Explains what the current goal is used for.
 Includes Checking whether the prolog processor is active."

 (when root
  (prog ((current-goal ($send self :get-current)))
     (emaxps-send-explain 16 (list  (subst-prolog-vars ($send current-goal :goal) 'normal)))
   (setq current-goal ($send current-goal :parent-goal))
   (when (equal ($send current-goal :goal) '(%top))

     (emaxps-send-explain 20 NIL) ;Warum
    (return 'done)
   );when
   ($send self :emaxps-why-goal current-goal)
  );prog
 );when
)

;; -------------------------------------------------------

(defun emaxps-explain-why ()
 "Starter, used by C-process"

  (emaxps-send-explain 9 NIL) ;Prolog-Warum
 ($send (send-kb :prolog-processor) :emaxps-explain-prolog-why)
 NIL   ;;return-value
)


;*********************************************************
;*    (2) RULE "HOW?"        Explain                     *
;*********************************************************

(def$method (rule-explain-mixin :emaxps-which-fact)
  (numbered-facts &optional (item-len *item-width*))
 "assumes numbered-facts to be non-nil!
 derived from kernel/rules/normal/nr-expl.cl"

 (let (choice menu)
  (setq menu (make-facts-choice numbered-facts item-len))
  (setq choice
   (emaxps-explain-one-of-many 3 NIL menu #'first))
  ;; ABORT should be illegal / impossible...
  (third choice)      ;;return-value
 );let
)

(def$method (rule-explain-mixin :emaxps-why-not) (fact)
 "Provides why not explanations using the ted.
 method requires ted-interface"

 (when rules-tried
  ($send self :send-if-handles :display-unprovable-term-tree fact
         `(NIL . ,(REMOVE-DOUBLES (mapcar #'second rules-tried)))
         self)
 );when
)

(def$method (rule-explain-mixin :emaxps-explain-fact) (fact)
 "Explain a facts status.
 derived from kernel/rules/normal/nr-expl.cl"

 (let ((status-string-list ($send self :translate-status-into-string fact)))
  (dolist (a-string status-string-list t)
   (emaxps-print-explanation " ~A" a-string))
 );let
)

(def$method (rule-explain-mixin :emaxps-explain-how) (what-kind-of-how)
 "Execs How directly, without popup menu checking for HOW,HOW-ALL,...
 derived from kernel/rules/normal/nr-expl.cl"

 (let (fact numbered-facts all-numbered-facts unprovable-facts)
  (case what-kind-of-how
   (:HOW
    (setf numbered-facts (make-numbered-facts ($send self :get-true-facts)))
    (if numbered-facts
     (when (setf fact ($send self :emaxps-which-fact numbered-facts))
      ($send self :emaxps-explain-fact fact)
     );when
    ;else
 ;TODO
      (emaxps-send-explain 23 NIL) ;Keine Ergebnisse
   ))
   (:HOW-ALL
    (setf all-numbered-facts (make-numbered-facts ($send self :get-all-facts)))
    (if all-numbered-facts
     (when (setf fact ($send self :emaxps-which-fact all-numbered-facts))
      ($send self :emaxps-explain-fact fact)
     );when
    ;else
 ;TODO   
   (emaxps-send-explain 22 NIL) ;Keine Ergebnisse
   ))
   (:WHY-NOT
    (setf unprovable-facts (make-numbered-facts ($send self :get-unprovable-facts)))
    (if unprovable-facts
     (when (setf fact ($send self :emaxps-which-fact unprovable-facts))
      ($send self :emaxps-why-not fact)
     );when
    ;else
 ;TODO
   (emaxps-send-explain 21 NIL) ;Keine Ergebnisse
   ))
  );case
  NIL  ;;return-value
 );let
)

;; -------------------------------------------------------

(defun emaxps-explain-how ()
 "Starter, used by C-process"

  (emaxps-send-explain 12 NIL) ;How

 ($send (send-kb :rule-processor) :emaxps-explain-how :HOW)
)

(defun emaxps-explain-how-all ()
 "Starter, used by C-process"

 (emaxps-send-explain 14 NIL) ;How all
 ($send (send-kb :rule-processor) :emaxps-explain-how :HOW-ALL)
)

(defun emaxps-explain-why-not ()
 "Starter, used by C-process"

 (emaxps-send-explain 13 NIL) ;Why not
 ($send (send-kb :rule-processor) :emaxps-explain-how :WHY-NOT)
)

;********************** Print facts **********************

(def$method (rule-explain-mixin :emaxps-print-true-facts) ()
 "Prints true facts"

 (let ((numbered-facts
        (make-numbered-facts ($send self :get-true-facts))
      ))
  ($send self :emaxps-print-facts numbered-facts)
 );let
)

(def$method (rule-explain-mixin :emaxps-print-all-facts) ()
 "Prints all facts"

 (let ((numbered-facts
        (make-numbered-facts ($send self :get-all-facts))
      ))
  ($send self :emaxps-print-facts numbered-facts)
 );let
)

(def$method (rule-explain-mixin :emaxps-print-unprovable-facts) ()
 "Prints unprovable facts"

 (let ((numbered-facts
        (make-numbered-facts ($send self :get-unprovable-facts))
      ))
  ($send self :emaxps-print-facts numbered-facts)
 );let
)

(def$method (rule-explain-mixin :emaxps-print-facts) (facts)
 "Print facts
 derived from kernel/rules/normal/nr-expl.cl"

 (if facts
  (progn
   (emaxps-send-explain 15 NIL) ;Ergebnis
   (mapc #'(lambda (f)
            (emaxps-print-explanation " ~S ~S" (first f) (second f))
           );lambda
         facts
   );mapc
  ) 
;else
 ;TODO
   (emaxps-send-explain 8 NIL) ;Keine Ergebnisse
 );if
 NIL  ;;return-value
)

;; -------------------------------------------------------

(defun emaxps-explain-print-all-facts ()
 "Starter, used by C-process"

  (emaxps-send-explain 7 NIL) ;Zeige alle Fakten
 ($send (send-kb :rule-processor) :emaxps-print-all-facts)
)

(defun emaxps-explain-print-true-facts ()
 "Starter, used by C-process"

  (emaxps-send-explain 5 NIL) ;wahre Fakten

 ($send (send-kb :rule-processor) :emaxps-print-true-facts)
)

(defun emaxps-explain-print-unprovable-facts ()
 "Starter, used by C-process"

  (emaxps-send-explain 6 NIL) ;unbeweisbare Fakten
 ($send (send-kb :rule-processor) :emaxps-print-unprovable-facts)
)


;*********************************************************
;*    (3) RULE "INSPECT"     Inspect Terms in Rules      *
;*********************************************************

(def$method (rule-develop-mixin :emaxps-display-rules-for-term)
            (a-term inif-rules inthen-rules)
;            (inif-rules inthen-rules)
 "Builds a list of rules including an information (action/condition)"



 (let (item-list (chosen-rules nil))
  (when inif-rules
   (setf inif-rules
 ;TODO
     (mapcar #'(lambda (a-rule-body)
                (list (format nil "Condition:~S" (rule-name a-rule-body))
                      a-rule-body)
               );lambda
             inif-rules
     );mapcar
  ))
  (when inthen-rules
   (setf inthen-rules
     (mapcar #'(lambda (a-rule-body)
                (list (format nil "Action:~S" (rule-name a-rule-body))
                      a-rule-body)
               );lambda
             inthen-rules
     );mapcar
  ))
  (when (setf item-list (append inif-rules inthen-rules))
   (setf item-list
    (emaxps-explain-some-of-many 1 (list a-term) item-list #'first)
   );setf
   (mapcar #'(lambda (x)
              (when (second x)
               (setf chosen-rules (append chosen-rules (rest x)))
             ))
           item-list
   );mapcar
  );when
  (when chosen-rules
   (mapc #'(lambda (a-rule-body)
            (emaxps-print-explanation "~S~%" a-rule-body))
         chosen-rules
   );mapc 
  );when
 );let
)

(def$method (rule-develop-mixin :emaxps-select-term)
	   (code term-list &optional label)
 "used by emaxps-filter-terms ..."
 (declare (ignore label))
 (second (emaxps-explain-one-of-many code  NIL
          (mapcar #'(lambda (x) (list (format nil "~S" x) x))
                  term-list
          );mapcar
          #'first))
)

(def$method (rule-develop-mixin :emaxps-filter-terms)
 (terms &optional mode)
 "by HG! (but: it truely IS a comment...)"
 (setf mode
  (or mode
   (let (ret liste)
 ;TODO
    (setf liste '(("Find All Terms" EQUAL)
                  ("Match First Element" FILTER-FIRST)
                  ("Match Second Element" FILTER-SECOND)
                  ("Match First And Second" FILTER-FIRST-AND-SECOND)))
    (setf ret
     (emaxps-explain-one-of-many2 liste))
 ;TODO
     (if ret
     (setf ret (second ret))
    ;else
     (setf ret 'DO-NOTHING)
    );if
    ret   ;;return-value  --> MATCH-TYPE
   );let
  );or
 );setf

 
(case mode
  (do-nothing nil)     ;;;; old: ('do-nothing nil)   !!!
  (equal terms)
  (filter-first
   (let ((chosen-first-element
          ($send self :emaxps-select-term 4
           (collect-term-components terms 'first)
            "xChoose a first element for match:"
        )))
    (filter-first (list chosen-first-element nil) terms)))
  (filter-second
   (let ((chosen-second-element
          ($send self :emaxps-select-term 6 
	   (compute-used-slots terms)
           "xChoose one of the slots:"
        )))
    (filter-second (list nil chosen-second-element) terms)))
  (filter-first-and-second
   (let ((filtered-terms ($send self :emaxps-filter-terms terms 'filter-first)))
    ($send self :emaxps-filter-terms filtered-terms 'filter-second)))
 );case
)

(def$method (rule-develop-mixin :emaxps-inspect-terms) ()
 "reprint of this famous method designed by the wonderful GMD team...
 Prints chosen terms of chosen rules (only Fridays and Sundays!)"

 (let* ((rule-set-name
         (emaxps-explain-one-of-many 0 NIL  (mapcar #'first (send-kb :rules)) )
 ;TODO
       )
        (rule-set-choice
         (assoc rule-set-name rules))
        (used-terms
         (if rule-set-choice ($send self :used-terms rule-set-choice)))
        (terms-to-inspect
         (if used-terms ($send self :emaxps-filter-terms used-terms)))
        (chosen-term
         (when terms-to-inspect
          (emaxps-explain-one-of-many 5 (list (length terms-to-inspect)  (first rule-set-choice)) terms-to-inspect  )
 ;TODO
       ))) 

  (when chosen-term
   ($send self :emaxps-display-rules-for-term chosen-term
    ($send self :inif chosen-term rule-set-choice)
    ($send self :inthen chosen-term rule-set-choice))
  );when

  NIL   ;;return-value
 );let*
)

;; -------------------------------------------------------

(defun emaxps-explain-inspect-terms ()
 "Starter, used by C-process"

 (emaxps-send-explain 11 NIL) ;Inspiziere Terme
 ($send (send-kb :rule-processor) :emaxps-inspect-terms)
)


;*********************************************************
;*    (4) RULE "VIEW"        View Rules                  *
;*********************************************************

(def$method (rule-develop-mixin :emaxps-view-rule) ()
 ""

 (let (rsetnames rsname rset rnames rule-name body)
  (when rules
   (setf rsetnames (mapcar #'first rules))
 ;TODO
   (when (setf rsname (emaxps-explain-one-of-many 0 NIL rsetnames ))
     (emaxps-send-explain 0 (list rsname))

   (setf rset (assoc rsname rules))
    (setf rset (rest rset))
    (when (setf rnames (mapcar #'first rset))
     (when (setf rule-name (emaxps-explain-one-of-many 2 NIL rnames ))
      (emaxps-send-explain 1 (list  rule-name))
      (setf body (assoc rule-name rset))
      (emaxps-send-explain 3 (list  (second body))) ;IF
      (emaxps-send-explain 4 (list  (third body)))  ;THEN

    );when rule-name
    );when rnames
   );when rsname
  );when rules
 );let
 NIL   ;;return-value
)

;; -------------------------------------------------------

(defun emaxps-explain-view ()
 "Starter, used by C-process"

 (emaxps-send-explain 2 NIL)
 ($send (send-kb :rule-processor) :emaxps-view-rule)
)


;***********************************************************

;;; eof

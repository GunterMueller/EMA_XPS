#|***************************************************
*   rule.cl       Hans Groschwitz        19.07.95   *
*                 Susanne Wellmann                  *
*                 Klaus Senf                        *
*                 Michael Block                     *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the rule-editor.             *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


; -------------------------------------------------------
; useful aids...
; -------------------------------------------------------

(defun emaxps-rule-list-rsets (kbname)
 "Returns :ERROR, if no such KB exists.
 Returns a list of rule-set-names in case of success."

 (if (member kbname *known-knowledge-bases*)
  (let ((rsets nil))
   (mapcar #'(lambda (x)
              (setf rsets (append rsets (list (first x))))
             );lambda 
             ($send (symbol-value kbname) :rules)
   );mapcar
   rsets   ;;return-value
  );let
 ;else
  :ERROR   ;;return-value
 );if
)

; -------------------------------------------------------

(defun emaxps-rule-list-rules (kbname rsetname)
 "Returns :ERROR, if no such KB or no such RSET exists.
 Returns a list of rules in case of success."

 (let (pos)
  (setf rsetname (emaxps-symbol-to-key rsetname))
  (if (member kbname *known-knowledge-bases*)
   (if (setf pos (position rsetname (emaxps-rule-list-rsets kbname)))
    (rest (nth pos ($send (symbol-value kbname) :rules)))  ;;return-value
   ;else
    :ERROR  ;;return-value
   );if
  ;else
   :ERROR   ;;return-value
  );if
 );let
)

; -------------------------------------------------------

(defun emaxps-rule-list-rule-names (kbname rsetname)
 "Returns :ERROR, if no such KB or no such RSET exists.
 Returns a list of rule-names in case of success."

 (let ((rl (emaxps-rule-list-rules kbname rsetname)))
  (if (equal :error rl)
   :error       ;;return-value
  ;else
   (let ((rnl nil))
    (mapcar #'(lambda (x)
               (setf rnl (append rnl (list (first x))))
              );lambda 
              rl
    );mapcar
    rnl      ;;return-value
   );let
  );if
 );let
)

; -------------------------------------------------------

(defun emaxps-rule-list-rule-body (kbname rsetname rname)
 "Returns :ERROR, if no such KB or no such RSET or no such
 RULE exists.
 Returns a list containting the IF-list and the THEN-list."

 (let (pos (rln (emaxps-rule-list-rule-names kbname rsetname)))
  (if (equal :error rln)
   :error       ;;return-value
  ;else
   (if (setf pos (position rname rln))
    (rest (nth pos (emaxps-rule-list-rules
                     kbname rsetname))) ;;return-value
   ;else
    :error      ;;return-value
   );if
  );if
 );let
)

; -------------------------------------------------------

(defun emaxps-rule-send-error (channel)
 "informs C that an error has occured."

 (emaxps-begin-message channel)
 (emaxps-send-nil)   ;; to indicate an ERROR
 (emaxps-end-of-message)
 NIL   ;;return-value
)

; -------------------------------------------------------
; INFO: the list of currently known knowledge bases is
; updated at C each time a change takes place.
; Hence no request has to be made from the RuleEditor.
; See main.cl for details.
; -------------------------------------------------------

(defun emaxps-rule-get-rsets-of-kb (kbname)
 "Sends a message at a special VChannel to C containing
 the list of rule sets of the requested KB. The C process
 reacts with a PopupDialog as message handler.
 emaxps-send-mbody uses ~A to remove the keyword `:' !!!"

 (let ((liste (emaxps-rule-list-rsets kbname)))
  (cond
   ((equal :ERROR liste)
    (emaxps-rule-send-error :emaxps-rule-get-rsets-of-kb)
   )
   (t ;;otherwise
    (emaxps-begin-message :emaxps-rule-get-rsets-of-kb)
    (mapcar #'(lambda (x)
               (emaxps-send-mbody "~A" x)
               (emaxps-separate-mparts)
              );lambda
            liste
    );mapcar
    (emaxps-end-of-message)
   )
  );cond
 );let
 NIL   ;;return-value
)

; -------------------------------------------------------

(defun emaxps-rule-get-rules-of-rset (kbname rsetname)
 "Sends a message at a special VChannel to C containing
 the list of rule names of the requested KB and RSET.
 The C process reacts with a PopupDialog as message handler.
 emaxps-send-mbody uses ~A to remove the keyword :' !!!"

 (let ((liste (emaxps-rule-list-rule-names kbname rsetname)))
  (cond
   ((equal :ERROR liste)
    (emaxps-rule-send-error :emaxps-rule-get-rules-of-rset)
   )
   (t ;;otherwise
    (emaxps-begin-message :emaxps-rule-get-rules-of-rset)
    (mapcar #'(lambda (x)
               (emaxps-send-mbody "~A" x)
               (emaxps-separate-mparts)
              );lambda
            liste
    );mapcar
    (emaxps-end-of-message)
   )
  );cond
 );let
 NIL   ;;return-value
)

; -------------------------------------------------------

(defun emaxps-rule-get-rules-of-rset2 (kbname rsetname)
 "Sends a message at a special VChannel to C containing
 the list of rule names of the requested KB and RSET.
 The C process reacts with a PopupDialog as message handler.
 emaxps-send-mbody uses ~A to remove the keyword :' !!!"

 (let ((liste (emaxps-rule-list-rule-names kbname rsetname)))
  (cond
   ((equal :ERROR liste)
    (emaxps-rule-send-error :emaxps-rule-get-rules-of-rset2)
   )
   (t ;;otherwise
    (emaxps-begin-message :emaxps-rule-get-rules-of-rset2)
    (mapcar #'(lambda (x)
               (emaxps-send-mbody "~A" x)
               (emaxps-separate-mparts) 
              );lambda
            liste
    );mapcar
    (emaxps-end-of-message)
   )
  );cond
 );let
 NIL   ;;return-value
)

; -------------------------------------------------------

(defun emaxps-rule-get-body-of-rule (rname rsetname kbname)
 "Sends a message at a special VChannel to C containing
 the Junctors and the lists of IF- and THEN-part.
 IMPORTANT: *print-pretty* and *print-case* must be set
 appropreately to grant the best possible representation
 within the C text widget.
 ~S to send editable lists to C-process!"

 (let (ifl thenl
       (*print-case* :capitalize)  ;;shadows global one!
       (liste (emaxps-rule-list-rule-body kbname rsetname rname)))
  (cond
   ((equal :ERROR liste)
    (emaxps-rule-send-error :emaxps-rule-get-body-of-rule)
   )
   (t ;;otherwise
    (setf ifl (first liste)
          thenl (second liste))
    (emaxps-begin-message :emaxps-rule-get-body-of-rule)
    (emaxps-send-mbody "~A" (first ifl))
    (emaxps-separate-mparts)
    (setf *print-case* :downcase)
    ;; C has to remove brackets! Try this with "nil"...
    (if (rest ifl)
     (emaxps-send-mbody "~S" (rest ifl))
    ;else
     (emaxps-send-mbody "()")
    );if
    (emaxps-separate-mparts)
    (setf *print-case* :capitalize)
    (emaxps-send-mbody "~A" (first thenl))
    (emaxps-separate-mparts)
    (setf *print-case* :downcase)
    ;; C has to remove brackets! Try this with "nil"...
    (if (rest thenl)
     (emaxps-send-mbody "~S" (rest thenl))
    ;else
     (emaxps-send-mbody "()")
    );if
    (emaxps-end-of-message)
   )
  );cond
 );let
 NIL   ;;return-value
)

; =======================================================
; LISP-world (an KB) modifying functions:
; =======================================================

(defun emaxps-rule-save-rule (rname rsetname kbname body)
 
 ($send ($send (symbol-value kbname) :rule-processor)
  :emaxps-make-rule rname rsetname body)
  (emaxps-kb-set-modified kbname)
)

; =======================================================
; Procedure to delete  a rule within a ruleset
; =======================================================

(defun emaxps-rule-delete-rule (kbname rsetname rname)
 "Delete a rule within a ruleset."

 (let ((rule-proc ($send (symbol-value kbname) :rule-processor)))
  ($send rule-proc :emaxps-delete-rule rsetname rname)
 );let
 (emaxps-kb-set-modified kbname)
)

; -------------------------------------------------------
; .
; -------------------------------------------------------

(def$method (rule-base :emaxps-delete-rule)
            (rsetname rule)
 "Delete funktioniert nicht auf Position Null"

 (let ((pos nil)(liste nil))
  (setf liste (assoc rsetname rules))
  (setf pos (position rule liste
                :test #'(lambda (x y) 
                         (if (listp y)
                          (equal x (first y))
                         ;else
                          nil
                         );if   
                        );lambda
               );position
  );setf
  (delete (nth pos liste) liste)          ;;; destructive!
 );let
;(emaxps-kb-set-modified kbname)
);defun

; -------------------------------------------------------
; .
; -------------------------------------------------------

; =======================================================
; Procedure to delete  a ruleset within a kb
; =======================================================

(defun emaxps-rule-delete-rset (kbname rsetname)
 "Delete a ruleset within a kb."

 (let ((rule-proc ($send (symbol-value kbname) :rule-processor)))
  ($send rule-proc :emaxps-delete-rset kbname rsetname)
 );let
 (emaxps-kb-set-modified kbname)
)

; -------------------------------------------------------
; .
; -------------------------------------------------------

(def$method (rule-base :emaxps-delete-rset)
            (kbname rsetname)
 "Delete funktioniert nicht auf Position (nth 0 rules)"

 (let (pos)
  (setf pos
   (position-if #'(lambda (x)
                   (equal rsetname (first x))
                  );lambda
                  rules))
  (delete (nth pos rules) rules)     ;; destruktiv
  (if (= pos 0)
   ;; The list is stored twice !!!
   ;; DELETE works on both references to this ONE list,
   ;; as long as pos > 0 !!!
   ($send (symbol-value kbname) :set-rules
     (rest ($send (symbol-value kbname) :rules))))
 );let
)

; =======================================================
; Procedure to save the Rule-Sets to a KB-file
; =======================================================

(def$method (rule-base :emaxps-print-all-rsets)
            (stream)
 "Print all Rule Sets to stream.
 BUG: RSETname and Rname should be :upcase !!!"

 (let ((*print-pretty* T) (*print-case* :downcase))
  (when rules
   (mapcar #'(lambda (rset)       
              (format stream "~%(defrule-set ~S " (first rset))
              (mapcar #'(lambda (rule)
                         (print rule stream)
                         (terpri stream)
                        );lambda
                      (rest rset)
              );mapcar
              (format stream ")~%")
             );lambda
           rules
   );mapcar
  );when
 );let
)

; =======================================================
; Procedure to move a rule within a ruleset
; =======================================================

(defun emaxps-rule-move-rule (kbname rsetname rname-to-move before dest-rname)
 "Moves a rule within a ruleset."

 (let ((rule-proc ($send (symbol-value kbname) :rule-processor)))
  ($send rule-proc :emaxps-move-rule rsetname rname-to-move before dest-rname)
 );let
 (emaxps-kb-set-modified kbname)
)

; -------------------------------------------------------
; .
; -------------------------------------------------------

(def$method (rule-base :emaxps-move-rule)
            (rsetname rname-to-move mode dest-rname)
 "Uses destructive procedures to optimize speed."

 (unless (equal rname-to-move dest-rname)
 (let (rset tmp oldpos newpos newpos-1)
  (setf rset (assoc rsetname rules))
  (unless rset
   (emaxps-error ":emaxps-move-rule failed: rset ~S not found" rsetname))
  (setf oldpos (position rname-to-move rset
                :test #'(lambda (x y)
                         (if (listp y)
                          (equal x (first y))
                         ;else
                          nil
                         );if
                        );lambda
               );position
  );setf
  (unless oldpos
   (emaxps-error ":emaxps-move-rule failed: rname-to-move ~S not found"
    rname-to-move))
  (setf newpos (position dest-rname rset
                :test #'(lambda (x y)
                         (if (listp y)
                          (equal x (first y))
                         ;else
                          nil   
                         );if  
                        );lambda 
               );position
  );setf
  (unless newpos
   (emaxps-error ":emaxps-move-rule failed: dest-rname ~S not found"
    dest-rname))
  ;; ------------------------------
  (setf tmp (nth oldpos rset))
  (delete tmp rset)          ;;; destructive!
  ;; ------------------------------
  (setf newpos (position dest-rname rset
                :test #'(lambda (x y)
                         (if (listp y)
                          (equal x (first y))
                         ;else
                          nil   
                         );if  
                        );lambda 
               );position
  );setf
  (unless newpos
   (emaxps-error ":emaxps-move-rule failed: dest-rname ~S no more found"
    dest-rname))
  (setf newpos-1 newpos)
  (case mode
   (:before (setf newpos-1 (1- newpos)))
   (:after  (setf newpos (1+ newpos-1)))
   (t (emaxps-error "syntax error to :emaxps-move-rule"))
  );case
  ;; ------------------------------
  (rplacd (nthcdr newpos-1 rset)    ;;; destructive!
          (cons tmp (nthcdr newpos rset))
  );rplacd
  NIL           ;; return-value
 );let
 );unless
)

;; eof

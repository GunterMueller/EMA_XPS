#|***************************************************
*   constraint.cl    Hans Groschwitz     19.07.95   *
*                    Karsten Vossberg               *
*                    Klaus Senf                     *
*                                                   *
*   Functions to build the constraint-editor.       *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


;;; **************************************************************************
;;; Methoden und Funktionen fuer primitive Constraints    
;;; **************************************************************************


;; -------------------------------------------------------

(def$method (constraint-base :definiere-constraint)
   (kbase c-name &rest rumpf)
  "Definition einer Primitive-Constraint oder eines Constraint-Netzes."

 (let ((def-form nil)
       (typ (get-def-typ rumpf))
       (oldkb (emaxps-current-kb-name))
       (constraint-namen nil))

      (emaxps-select-kb kbase)  
      (emaxps-kb-set-modified kbase)
      (case typ 
         (primitive (setq def-form  (list 'DEFCONSTRAINT c-name
                                       (first  rumpf)  
                                       (second rumpf)  
                                       (third  rumpf)  
                                       (fourth rumpf))))  
         (compound  (setq def-form  (list 'DEFCONSTRAINT c-name
                                       (first  rumpf)  
                                       (second rumpf)  
                                       (third  rumpf))))  
      );case                           
      (mapcar #'(lambda (l) 
                 (setq constraint-namen 
                   (append constraint-namen (list (get-name-of-c-assoc l)))))
           (case typ (primitive constraints) 
                     (compound constraint-nets)))  

      (eval def-form)
      (emaxps-select-kb oldkb)
  );let
)         

;;; **************************************************************************
;;; Methoden und Funktionen fuer primitive Constraints
;;; **************************************************************************

(def$method (constraint-base :emaxps-print-all-primitive-constraints)
            (stream)
 "Prints all primitive constraints to stream."

 (let ((*print-pretty* T) (*print-case* :downcase))
  (when constraints                        
   (mapcar #'(lambda (l)                                    
              (let ((name (get-name-of-c-assoc l)))
               ($send (get-constraint name) :emaxps-print name stream)))
           (reverse constraints)   ;;;   !!! HG !!!
   );mapcar
  );when
 );let
)  

(def$method (constraint :emaxps-print) (name stream)
 "Prints the primitive constraint name to stream."  
  
 (print
  `(defconstraint ,name
    (:type primitive)
    (:interface . ,interface)
    (:relation . ,relation)
    (:condition ,(if compiled-condition-flag :or condition)))
  stream)
 (terpri stream)

)

(defun hole-constraint-definition (kbase c-name &optional (typ 'primitive) )             
   "Ausgeben der Definition eines primitiven Constraints 
    oder Constraint-Netzes."
 (let ((oldkb (emaxps-current-kb-name)))
      (emaxps-select-kb kbase)
                                                                     
    (if (or (assoc c-name (send-kb :constraints)) 
            (assoc c-name (send-kb :constraint-nets))) 
        ;; c-name bekannt                       
        ($send ( get-constraint c-name) :hole-constraint-definition c-name )     
    ;; else c-name nicht bekannt
        (let ((*print-pretty* T) (*print-case* :upcase))        
           (emaxps-begin-message :emaxps-constraint-def)
           (emaxps-send-mbody "~s" c-name)
           (setf *print-case* :downcase)
           (emaxps-separate-mparts)
           (emaxps-send-mbody "~s" typ)
           (emaxps-separate-mparts)
           (emaxps-send-mbody " ")
           (emaxps-separate-mparts)
           (emaxps-send-mbody  " ")
           (emaxps-separate-mparts)
           (emaxps-send-mbody  " ")
           (setf *print-case* :upcase)
           (emaxps-end-of-message)
        );let                                             
   );if                                                   
   (emaxps-select-kb oldkb)
 );let
)                                                         
 

;; -------------------------------------------------------

(def$method (constraint-base :hole-primitive-constraint-definition-mit-menue) 
   (kbase)
   "Die Methode praesentiert dem Benutzer ein Auswahlmenue aus der 
    ein Constraint ausgewaehlt werden kann, dessen Definiton im 
    Constrainteditor angezeigt wird"  

  (let ((oldkb (emaxps-current-kb-name))
        (name nil)
        (c-namen nil))
      (emaxps-select-kb kbase)
      (setq c-namen (mapcar #'(lambda (l) (get-name-of-c-assoc l)) constraints))
        (progn
            (emaxps-begin-message :emaxps-constraint-sel)                    
             (mapcar #'(lambda (x)
                 (emaxps-send-mbody "~A" x)
                 (emaxps-separate-mparts)
                 )
                  c-namen
              )
              (emaxps-send-mbody "dummy" )
              ( setf name (emaxps-eom-get-reply))
              (if name
                 (hole-constraint-definition kbase name)
              );if
         );progn

      ; );if
      (emaxps-select-kb oldkb)
 );let 
)                                                             

;; -------------------------------------------------------





(def$method (constraint :hole-constraint-definition) ( c-name )             
   "Ausgeben der Definition eines primitiven Constraint mit 
    dem Namen 'c-name'"                                  
                                                                         
  (let ((*print-pretty* T) 
        (*print-case* :upcase))
        (emaxps-begin-message :emaxps-constraint-def)
      (emaxps-send-mbody "~A" c-name)
      (emaxps-separate-mparts)
       (setf *print-case* :downcase)
       (emaxps-send-mbody "primitive" ) 
       (emaxps-separate-mparts)
      (if interface
          (emaxps-send-mbody "~{~s ~}" interface)      
       ;; else
          (emaxps-send-mbody " ")      
       )
       (emaxps-separate-mparts)
       (if relation
          (mapcar #'(lambda(r) (emaxps-send-mbody "~S" r )) relation) 
       ;; else
          (emaxps-send-mbody " ")  
       )
       (emaxps-separate-mparts)
       (if (not (equal condition t)) ;condition ist t wenn nicht vorhanden  
          (if compiled-condition-flag 
             (emaxps-send-mbody ":or")  
          ;; else 
             (emaxps-send-mbody "~S~%" condition ) 
          );if 
       );if
       (emaxps-end-of-message)
  );let 
)                                                             

;; -------------------------------------------------------

(def$method (constraint-base :loesche-primitive-constraint) (kbase c-name)
    "Loeschen einer primitiven Constraint."

  (let ((oldkb (emaxps-current-kb-name)))
     (emaxps-select-kb kbase)
     (emaxps-kb-set-modified kbase)
     (send-kb :delete c-name) 
     (send-constraint-processor :delete c-name)
     (emaxps-select-kb oldkb)
  );let 
)

;; -------------------------------------------------------

;; eof

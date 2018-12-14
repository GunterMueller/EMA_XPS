#|***************************************************
*   csnet.cl      Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the constraint-net-editor.   *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


;;; **************************************************************************
;;; Methoden und Funktionen fuer Constraint-Netze
;;; **************************************************************************

(def$method (constraint-base :emaxps-print-all-constraintnets)
            (stream) 
 "Print all constraint networks to stream"

 (let ((*print-pretty* T) (*print-case* :downcase))
  (when constraint-nets               
   (mapcar #'(lambda (l)                                    
              (let ((name (get-name-of-c-assoc l)))
               ($send (get-constraint name) :emaxps-print name stream)))
           (reverse constraint-nets)   ;;;  !!! HG !!!
   );mapcar
  );when
 );let
)  

;; -------------------------------------------------------

(def$method (constraint-net :emaxps-print) (name stream)
 "Prints thew constraint net name to stream."

 (print
  `(defconstraint ,name
    (:type compound)
    (:interface . ,interface)
    (:constraint-expressions . ,(select-all-constraints net-spec)))
   stream)
 (terpri stream)
)

;; -------------------------------------------------------

(def$method (constraint-net :hole-constraint-definition) (c-name )             
   "Ausgeben der Definition eines Constraint-Netze mit dem 
    Namen 'c-name'."                                  
                                                                         
  (let ((*print-pretty* T) (*print-case* :upcase))
       (emaxps-begin-message :emaxps-csnet-def)
       (emaxps-send-mbody "~s" c-name)
       (emaxps-separate-mparts)
       (setf *print-case* :downcase)
       (emaxps-send-mbody "compound")  
       (emaxps-separate-mparts)
       (if interface
          (emaxps-send-mbody "~{~S ~}" interface)      
       ;; else
          (emaxps-send-mbody " ")
       )
       (emaxps-separate-mparts)
       (mapcar #'(lambda(r) (emaxps-send-mbody "~S~%" r ))  
                  (select-all-constraints net-spec))   
       (emaxps-separate-mparts)
       (emaxps-send-mbody " ")  ;dummy wird nicht benoetigt                 
       (emaxps-end-of-message)
  );let 
)                                                             




;; -------------------------------------------------------
(defun hole-constraintnet-definition (kbase c-name &optional (typ 'compound) )             
   "Ausgeben der Definition eines Constraints-Netzes" 
 (let ((oldkb (emaxps-current-kb-name)))
      (emaxps-select-kb kbase)
                                                                     
    (if (assoc c-name (send-kb :constraint-nets)) 
        ;; c-name bekannt                       
        ($send  ( get-constraint c-name) :hole-constraint-definition c-name )     
    ;; else c-name nicht bekannt
        (let ((*print-pretty* T) (*print-case* :upcase))        
           (emaxps-begin-message :emaxps-csnet-def)
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


(def$method (constraint-base :hole-constraintnetz-definition-mit-menue) 
   (kbase)
   "Die Methode praesentiert dem Benutzer (C-seitig) ein Auswahlmenue aus der 
    ein Constraint-Netz ausgewaehlt werden kann, dessen Definiton im 
    ConstraintNeteditor angezeigt wird"  

  (let ((oldkb (emaxps-current-kb-name))
        (name nil)
        (c-namen nil))
      (emaxps-select-kb kbase)
      (setq c-namen (mapcar #'(lambda (l) (get-name-of-c-assoc l)) constraint-nets))
        (progn
            (emaxps-begin-message :emaxps-csnet-sel)                    
             (mapcar #'(lambda (x)
                 (emaxps-send-mbody "~A" x)
                 (emaxps-separate-mparts)
                 )
                  c-namen
              )
              (emaxps-send-mbody "dummy" )
              ( setf name (emaxps-eom-get-reply))
              (if name
                 (hole-constraintnet-definition  kbase name)
              );if
         );progn

      (emaxps-select-kb oldkb)
 );let 
)                                                             



;; -------------------------------------------------------


(def$method (constraint-base :loesche-constraintnetz) (kbase c-name)
    "Loeschen eines Constraint-Netzes."

  (let ((oldkb (emaxps-current-kb-name)))
     (emaxps-select-kb kbase)
     (emaxps-kb-set-modified kbase)
     (send-kb :delete c-name) 
     (send-constraint-processor :delete c-name)
     (emaxps-select-kb oldkb)
  );let 

)

;; eof

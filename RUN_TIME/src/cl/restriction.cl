#|***************************************************
*   restriction.cl   Hans Groschwitz     19.07.95   *           
*                    Karsten Vossberg               *   
*                    Klaus Senf                     *   
*                                                   *
*   Functions to build the restriction-editor.      *   
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


;;; **************************************************************************
;;; Methoden und Funktionen fuer Restrictionen
;;; **************************************************************************

(def$method (restriction-base :emaxps-print-all-restrictions)
            (stream) 
 "Print all restrictions to stream."

 (let ((*print-pretty* T) (*print-case* :downcase))
  (when restriction-nets    
   (mapcar #'(lambda (l)                                    
              (let ((name (get-name-of-c-assoc l)))
               ($send (get-constraint name) :emaxps-print name stream)))
           (reverse restriction-nets) ;;; !!! HG !!!
   );mapcar
  );when
 );let
)

;; -------------------------------------------------------

(def$method (restriction-definition :emaxps-print) (name stream)
  "Ausgabe des Restriction-Net"

  (print 
   `(defrestriction ,name
     (:restrictions . ,restrictions))
   stream)
  (terpri stream)
)

;; -------------------------------------------------------

(def$method (restriction-base :definiere-restriktion)
            ( kbase name rumpf)
  "Definition einer Restriction mit dem Namen 'name'.
   'rumpf' = (:restrictions {<Constraint-Anbindungen>}*) "

 (let ((form 'nil) 
       (oldkb (emaxps-current-kb-name))
       (restriction-namen nil))
       
       (emaxps-select-kb kbase)
       (emaxps-kb-set-modified kbase)
       (mapcar #'(lambda (l) 
                  (setq restriction-namen 
                    (append restriction-namen (list (get-name-of-c-assoc l)))))
              restriction-nets)  
        (setq form (list 'DEFRESTRICTION name ))
        (setq form (append form (list rumpf))) 
        (eval form)
        (emaxps-select-kb oldkb)
 );let
)         

;; -------------------------------------------------------

;; -------------------------------------------------------

(defun hole-restriktions-definition (kbase r-name)
   "Ausgeben der Definition einer Restriction mit dem 
    Namen 'r-name' im Editor. "                                  
 (let ((oldkb (emaxps-current-kb-name)))
   (emaxps-select-kb kbase)
   (let ((*print-pretty* T) (*print-case* :upcase)
        (restriction (send-kb :get-restrictions r-name)))

       (cond(restriction  
               ;; r-name ist der Name einer bekannten Restriction   
               ($send restriction :hole-restriktions-definition r-name))
            (t ;; r-name nicht der Name einer bekannten Restriction   
              (emaxps-begin-message :emaxps-restriction-def)
              (emaxps-send-mbody "~s" r-name)
              (emaxps-separate-mparts)
              (emaxps-send-mbody "~s" restriction)
              (emaxps-end-of-message)
            )
       );cond   
   );let
   (emaxps-select-kb oldkb)
 );let
)                                       

;; -------------------------------------------------------


(def$method (restriction-base :hole-restriktions-definition-mit-menue) (kbase)
   "Ausgeben der Definition einer Restriktion
    Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, 
    aus dem eine Restriktion aus gewaehlt werden kann, die im 
    Restriktioneneditor angezeigt wird." 

  (let ((name nil)  
        (r-namen nil)
        (oldkb (emaxps-current-kb-name)))                    
        (emaxps-select-kb kbase)

        (setq r-namen (mapcar #'(lambda (l) 
                       (get-name-of-c-assoc l))
                restriction-nets))
         (progn
            (emaxps-begin-message :emaxps-restriction-sel)                    
             (mapcar #'(lambda (x)
                 (emaxps-send-mbody "~A" x)
                 (emaxps-separate-mparts)
                 )
                  r-namen
              )
              (emaxps-send-mbody "dummy" )
              ( setf name (emaxps-eom-get-reply))
              (if name
                 (hole-restriktions-definition kbase  name)
              );if
         );progn 
      (emaxps-select-kb oldkb);
  );let 
)                                                             



;; -------------------------------------------------------

(def$method (restriction-definition :hole-restriktions-definition) (r-name)
   "Ausgeben der Definition einer Restriction.
    Error wenn r-name nicht der Name einer bekannten Restriction ist."

  (let ((*print-pretty* T) (*print-case* :upcase))
     (emaxps-begin-message :emaxps-restriction-def)
     (emaxps-send-mbody "~s" r-name)
     (emaxps-separate-mparts)
       (setf *print-case* :downcase)
       (if restrictions                                         
          (mapcar #'(lambda(r) (emaxps-send-mbody" ~s" r )) restrictions) 
       ;; else
          (emaxps-send-mbody "  " ) 
       );if
       (emaxps-end-of-message)
  );let 
)                                                             


;; -------------------------------------------------------

(def$method (restriction-base :loesche-restriktion) ( kbase r-name)
    "Loeschen einer Restriction.
     Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, 
     aus dem die zu loeschende Restriction aus gewaehlt werden kann. 
     Sind keine Restrictionen vorhanden, so erfolgt eine Meldung mittels 
     einer Dialogbox"  

  (let  ((oldkb (emaxps-current-kb-name)))
        (emaxps-select-kb kbase)
        (send-kb :delete-restrictions r-name)
        (send-constraint-processor :delete-restrictions r-name)
        (emaxps-select-kb oldkb)
        (emaxps-kb-set-modified kbase)
  );let 
)

;; -------------------------------------------------------

(def$method (restriction-base :restriktions-redefinition) (kbase rname)
  "Erzeugen eines neuen Netzes mit Hilfe der generischen
  	Beschreibung. Eine Redefinition ist notwendig, wenn sich 
   Instanzen der Wissensbasisis geaendert haben"

  (let ((oldkb (emaxps-current-kb-name)))
      (emaxps-select-kb kbase)
      ($send self :redefine rname)
      (emaxps-select-kb oldkb)
  );let 
)

;; -------------------------------------------------------
;; -------------------------------------------------------

(def$method (restriction-base :emaxps-redefine-all) (kbase )
  "Erzeugen eines neuen Netzes mit Hilfe der generischen
  	Beschreibung. Eine Redefinition ist notwendig, wenn sich 
   Instanzen der Wissensbasisis geaendert haben"

  (let ((oldkb (emaxps-current-kb-name)))
      (emaxps-select-kb kbase)
      ($send self :redefine-all)
      (emaxps-select-kb oldkb)
  );let 
)


;; eof

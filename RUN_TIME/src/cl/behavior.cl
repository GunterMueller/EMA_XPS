#|***************************************************
*   behavior.cl     Hans Groschwitz      19.07.95   *
*                   Karsten Vossberg                *
*                   Klaus Senf                      *
*                                                   *
*   Functions to build the behavior-editor.         *
*   Full version only.                              *
***************************************************|#

(in-package "FMCS")

(defmacro emaxps-remove-qualified-method (qualifier method-entry)
  `(let ((qualified-method
          (get-qualified-method ,qualifier (method-entry-methods-list ,method-entry))))
     (when qualified-method
      (delete qualified-method (method-entry-methods-list ,method-entry))
     );when
   );let
)

(defmacro UNDEFMETHOD ((a_class . qualifier-and-selector))
 (let ((qualifier (if (second qualifier-and-selector)
                    (first qualifier-and-selector)
                    :primary))
        (selector (if (second qualifier-and-selector)
                    (second qualifier-and-selector)
                    (first qualifier-and-selector))))
  `(let ((method-entry
          (gethash ,selector (mcs-slot-value ,a_class (index-of-methods)))))
     (when method-entry
       (emaxps-remove-qualified-method ,qualifier method-entry)
       (remove-invalid-combined-methods ,a_class ,selector)
     );when
     (null (null method-entry))
   );let
 );let
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package "BABYLON")


;;; **************************************************************************
;;; Methoden und Funktionen zur Aenderung von Behaviors
;;; **************************************************************************

(defmacro my-defbehavior (kbase behavior-spec lambda-list &body behavior-body)
  `(and (current-kb-typep 'basic-frame-mixin)
	     (definiere-behavior ',kbase ',behavior-spec ',lambda-list ',behavior-body)))

;;; --------------------------------------------------------------------------

(defun definiere-behavior                                            
      (kbase behavior-spec lambda-list behavior-rumpf)
                                                
 (let (;(frame-name (first behavior-spec))
       (form    nil) 
       ;(antwort nil)
       (oldkb (emaxps-current-kb-name))
      )    
  
    (emaxps-select-kb kbase)     



    (if (equal (second behavior-spec) ':primary)
     (setq behavior-spec (remove ':primary behavior-spec :count 1)))
    (setq form (list 'DEFBEHAVIOR behavior-spec lambda-list))
    (setq form (append form behavior-rumpf))
    (eval form)
    (emaxps-kb-set-modified kbase) ;; Set the KB changed (LISP-side)
    (emaxps-select-kb oldkb)
 );let
)         

;;; --------------------------------------------------------------------------

(defun behavior-typ (behavior-spec)  
   "Gibt den Behaviortyp zurueck" 

   (let ((typ (second behavior-spec)))  
        (cond ((or (equal typ ':after)(equal typ ':before))   typ) 
              ( t   ':primary) )
   )
) 

;;; --------------------------------------------------------------------------

(defun behavior-hole-frames ( kbase ) 
   "Gibt die Definition eines Frames im Editor aus.
    Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, 
    aus dem einFrame aus gewaehlt werden kann. "
    (let ((oldkb (emaxps-current-kb-name))) 
      (emaxps-select-kb kbase) 
      (let ((frame-namen (send-kb  :frames)))
         (progn
             (emaxps-begin-message :emaxps-behav-frame-selection)
             (mapcar #'(lambda (x)
                 (emaxps-send-mbody "~A" x)
                 (emaxps-separate-mparts)
                )
                frame-namen
             )
             (emaxps-send-mbody "dummy" )
             (emaxps-end-of-message)
         );progn
      );let
      (emaxps-select-kb oldkb)
    );let 
)         


;;; --------------------------------------------------------------------------





(defun hole-behavior-definition-mit-menue ( kbase framename) 
    "Die Funktion laesst den C-Server ein Selektionsmenu aufmachen, 
     aus dem eine Behavior aus gewaehlt werden kann, die im Editor
     angezeigt werden kann."


   (let ((behavior nil )
        ( typ nil)
        (oldkb (emaxps-current-kb-name))
        (behaviorspec nil))
     (emaxps-select-kb kbase)

     (setq behaviorspec (get-frame-behavior-specs framename))


     (emaxps-begin-message :emaxps-behav-behavior-selection)
     (mapcar #'(lambda (l) 
      ;  (emaxps-send-mbody "~A " (first (rest l)))
       (if (third l)
         (emaxps-send-mbody "~A ~s" (first (last l)) (second l)  )
       ;else
         (emaxps-send-mbody "~A ~s" (first (last l)) :primary))
        (emaxps-separate-mparts))
          behaviorspec
      )
      (emaxps-send-mbody "  " )
      (setq behavior (emaxps-eom-get-reply))
      (setq typ behavior)
      (if behavior
        (progn                       
         (setq behavior (emaxps-symbol-to-key (first behavior)))
         (when (and (second typ) (not( equal (second typ) :primary)))
           (setq behavior          
            (append (list (second typ)) (list behavior) ))
           
         ) 

         (if (not (listp behavior))  
            (setq behaviorspec 
            (append (list framename) (list behavior)))  
          ;; else
            (setq behaviorspec 
            (append (list framename) behavior))  
         );if
        (hole-behavior-definition kbase behaviorspec)
        );progn
      );if   
     (emaxps-select-kb oldkb)
     );let

)

;;; --------------------------------------------------------------------------

(defun hole-behavior-definition (kbase behavior-spec)
  "Gibt die Definition der Behavior im Editor aus."  
 (let ((oldkb (emaxps-current-kb-name)))
   (emaxps-select-kb kbase)
  (let*((*print-pretty* T) (*print-case* :upcase)
      (frame-internal-name (get-frame-name (first behavior-spec)))
	     (behaviors-so-far (frame-behaviors frame-internal-name))
        (definition (assoc behavior-spec behaviors-so-far
                     :test #'(lambda (x y)
                              (when (= 2 (length x))
                               (setf x (list (first x) :primary (second x))))
                              (when (= 2 (length y))
                               (setf y (list (first y) :primary (second y))))
                              (equal x y)
                             );lambda
        ))
        (behavior-name (car (last behavior-spec))) 
        (typ           (second behavior-spec))     
        ;(frame-name    (first behavior-spec))      
        (lambda-list   (second definition))        
        (doc           (car (cddr  definition)))
        ;(oldkb (emaxps-current-kb-name))
        (rumpf         (cddr  definition)))        
       (emaxps-begin-message :emaxps-behavior-definition)        
       (emaxps-send-mbody "~A"  behavior-name)
       (emaxps-separate-mparts)
       (setf *print-case* :downcase)

       (cond ((equal typ ':after) 
              (emaxps-send-mbody ":after")) 
             ((equal typ ':before)
              (emaxps-send-mbody ":before")) 
             (t (emaxps-send-mbody ":primary")) 
       )  
      (emaxps-separate-mparts) 
      (if lambda-list            
         (emaxps-send-mbody "~{~s ~}" lambda-list)     
       ;; else
          (emaxps-send-mbody " ")
       
       )
       (emaxps-separate-mparts)     
       (if (stringp doc) 
          ;; Dokumentations-String vorhanden
          (progn(emaxps-send-mbody "~A" doc)                              
                (setq rumpf (cdddr  definition)))                    
       ;; else
          (emaxps-send-mbody " ") 
       )
       (emaxps-separate-mparts) 
       (if rumpf                                  
          (mapcar #'(lambda(r) (emaxps-send-mbody "~s" r )) rumpf) 
        ;; else
          (emaxps-send-mbody " ")
       )     
       (emaxps-end-of-message)
       (setf *print-case* :upcase)
       (emaxps-select-kb oldkb)
   );let
  ) ;let
)               
  
;;; --------------------------------------------------------------------------
                         
(defun loesche-behavior (kbase beh-spec)

 (let ((oldkb (emaxps-current-kb-name)))
  (emaxps-select-kb kbase)
  (when beh-spec
   ($send (send-kb :frame-processor) :loesche-behavior beh-spec)
   (emaxps-kb-set-modified kbase) ;; Set the KB changed (LISP-side)
  );when
  (emaxps-select-kb oldkb)
 );let 
)

;;; --------------------------------------------------------------------------

(def$method (frame-base :loesche-behavior) (behavior-spec)
   "Loescht eine Behavior: 
    1. Behaviorname wird aus der p-Liste des Frames geloescht.
    2. mit undefmethod Behavior entfernen"
                                                                                
  (let*((frame-name          (first behavior-spec))  
        (frame-internal-name (get-frame-name-with-check frame-name))
	     (behaviors           (frame-behaviors frame-internal-name))
        (behavior-def        (assoc behavior-spec behaviors :test #'equal))
        (typ                 (behavior-typ behavior-spec))
        (behavior-name       (car (last behavior-spec)))
        (new-behaviors       nil))
       
        (setq new-behaviors (delete behavior-def behaviors))     
        (setf (frame-behaviors frame-internal-name) new-behaviors)
        (eval (list 'undef$method (list frame-internal-name 
                                                        typ
                                                        behavior-name
                                  )
              )
        )
  );let
)                                                                               

;;;*********** EOF ***********************************************************

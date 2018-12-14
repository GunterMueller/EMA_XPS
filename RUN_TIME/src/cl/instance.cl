#|***************************************************
*   instance.cl    Hans Groschwitz       19.07.95   *
*                  Karsten Vossberg                 *
*                  Klaus Senf                       *
*                                                   *
*   Functions to build the instance-editor.         *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


;;; **************************************************************************
;;; Methoden und Funktionen zur Aenderung von Instanzen
;;; **************************************************************************

(defmacro my-definstance (kbase instanz-name of frame-name &body rumpf)
 "Definition einer Instanz. 
 Das Makro entspricht dem Makro definstance."

 (declare (ignore of))                    
 `(let ((oldkb (emaxps-current-kb-name))) 
    (emaxps-select-kb ',kbase)
    (definiere-instanz ',kbase ',instanz-name ',frame-name ',rumpf)  
    (emaxps-select-kb oldkb)
  );let
)

;;; **************************************************************************
;;; Instanz definieren
;;; **************************************************************************

(defun definiere-instanz (kbase instanz-name frame-name rumpf)
 "Definition einer Instanz. Funktion wird vom 
 Makro 'my-definstance' aufgerufen."

 (when (member instanz-name (send-kb :instances))
  ($send (send-kb :frame-processor) :loesche-instanz instanz-name))
 ($send (send-kb :frame-processor) :new-instance
        instanz-name frame-name rumpf)
 (emaxps-kb-set-modified kbase) ;; Set the KB changed (LISP-side)
)

;;; --------------------------------------------------------------------------

(defun loesche-instanz-mit-menue( kbase instance-name )
 "used by C-Process"

 (let ((oldkb (emaxps-current-kb-name)))
  (emaxps-select-kb kbase)
  (cond ((member instance-name (send-kb :instances))
     ($send (send-kb :frame-processor) 
               :loesche-instanz instance-name)
     (emaxps-kb-set-modified kbase) ;; Set the KB changed (LISP-side)
  ));
  (emaxps-select-kb oldkb)
 );let
)

;;; --------------------------------------------------------------------------

(def$method (frame-base :loesche-instanz) (instanz-name)
   "Loescht eine Instanz (Instanz muss bekannt sein): 
      1. Instanzname aus der Liste, auf die 'instances-list' zeigt, loeschen.
      2. Instanzname aus der Liste, auf die 'instances' zeigt, loeschen.
      3. Instanzname der p-Liste des zugehoerigen Frames loeschen
         (Schluessel: ':instances', Wert: Name der Instanz) 
      4. Schluessel ':instance-definition' aus der p-Liste von 
         instanz-name entfernen
      5. Schluessel ':instance' aus der p-Liste von instanz-name entfernen"
                                                                                
  (let*((internal-instance-name (make-instance-name instanz-name))
        (frame-name             (fourth (get-instance-def instanz-name)))
        (frame-internal-name    (get-frame-name frame-name)))

       (setf instances-list (remove instanz-name instances-list))
       (send-kb :set-instances instances-list)
       (setf (frame-instances frame-internal-name)
               (delete instanz-name (frame-instances frame-internal-name)))     
       (remprop internal-instance-name :instance)  
       (remprop internal-instance-name :instance-definition)   
       t
  );let
)                                                                            

;;; --------------------------------------------------------------------------
 
(defun instance-hole-frame-definition-mit-menue (kbase)
   "Gibt die Definition eines Frames im Editor aus.
    Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, 
    aus dem einFrame aus gewaehlt werden kann."

    (let ((oldkb (emaxps-current-kb-name))) 
      (emaxps-select-kb kbase) 
      (let ((frame-namen (send-kb  :frames)))
         (progn
             (emaxps-begin-message :emaxps-inst-frame-selection)
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

(defun hole-instanz-definition-mit-menue (kbase framename)
 "Die Funktion laesst den C-Server ein Selektionsmenu aufmachen, 
 aus dem eine Instanz aus gewaehlt werden kann, die im Editor
 angezeigt werden kann."

 (let ((instanznamen nil)
       (oldkb (emaxps-current-kb-name))
       (instanz-name nil))

  (emaxps-select-kb kbase)         
  (setq instanznamen (get-instance-list framename))
  (emaxps-select-kb oldkb) 

  (emaxps-begin-message :emaxps-inst-instance-selection)
  (mapcar #'(lambda (x)
             (emaxps-send-mbody "~A" x)
             (emaxps-separate-mparts)
            );lambda
          instanznamen
  );mapcar
  (emaxps-send-mbody "dummy" )
  (setf instanz-name (emaxps-eom-get-reply))

  (when instanz-name
    (hole-instanz-definition kbase instanz-name framename))

 );let 
)

;;; --------------------------------------------------------------------------

(defun hole-instanz-definition (kbase instanz-name frame-name)
  "Gibt die Definition einer Instanz im Editor aus."

  (let ((*print-pretty* T) (*print-case* :upcase)
        (instance nil)                       
        (oldkb (emaxps-current-kb-name))
        (with-spec nil)
        (spec nil))
   (emaxps-select-kb kbase)
   (when (member instanz-name (send-kb :instances))
    (setq instance (get-instance instanz-name)) 
    (setq frame-name (fourth (get-instance-def instanz-name)))
    (setq with-spec (nthcdr 4  (get-instance-def instanz-name)))
    (setq spec (remove 'with with-spec))
   );when
   (emaxps-select-kb oldkb)

   (emaxps-begin-message :emaxps-instance-definition)
   (emaxps-send-mbody "~s" instanz-name)
   (emaxps-separate-mparts)
   (emaxps-send-mbody "~s" frame-name)
   (emaxps-separate-mparts)
   (setf *print-case* :downcase)
   (if spec            
      (emaxps-send-mbody "~{~s ~s ~s ~%~}" spec)  
   ;; else
      (emaxps-send-mbody " ")
   )    
   (emaxps-end-of-message)
   NIL    ;;return-value
  ) ;let
)               

;;;*********** EOF ***********************************************************

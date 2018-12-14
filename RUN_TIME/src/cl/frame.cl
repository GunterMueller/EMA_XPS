#|***************************************************
*   frame.cl      Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the frame-editor.            *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")


;; =======================================================
;;  Procedure to save Frames, Instances and Behaviors
;;  to a KB-file
;; =======================================================

(def$method (basic-frame-processor
             :emaxps-print-frames-behaviors-instances)
            (stream)
 "Print all Frames, Instances and Behaviors to stream.
 BUG: Names should be :upcase !!!"

 (let ((*print-pretty* T) (*print-case* :downcase)
       (frames ($send meta-processor :frames)))

  (when frames 
   (mapc #'(lambda (a-frame-name)
            (format stream "~2%;; ---------- FRAME ~S ----------~%"
             a-frame-name)
            (PRINT-FRAME (get-frame-name a-frame-name) stream) 
            (EMAXPS-PRINT-B3-FRAME-COMPLIANCE a-frame-name stream)
            (PRINT-INSTANCES a-frame-name stream))		     
         frames
   );mapc
  );when
 );let
)


;; =======================================================
;;  for B3 equivalent working (see B3-OOP)
;;  even frames are global MCS instances there ...
;; =======================================================

(defun EMAXPS-PRINT-B3-FRAME-COMPLIANCE (a-frame-name stream)
 (format stream "~%(emaxps-b3-frame-compliance ~A)~2%" a-frame-name a-frame-name)
)

(defmacro emaxps-b3-frame-compliance (a-frame-name)
 `(defvar ,a-frame-name ',a-frame-name)
)

;;; **************************************************************************

(defun emaxps-get-frame-check (kbase)
 "Sends message to C, if frame-check is active
 Frame-Check OFF results in NOT SENDING a message!"

 (when ($send ($send (symbol-value kbase) :frame-processor) :frcheck)
  (emaxps-begin-message :emaxps-frame-check-on) 
  (emaxps-end-of-message))
)

(def$method (basic-frame-processor :print-frames-behaviors-instances) 
    (&optional (stream *standard-output*)) 
  "Ausgabe aller Frame-, Behavior- und Instanzendefinitionen auf stream."

  (declare (special *grafic-stream*))
  (let ((frames ($send meta-processor :frames))
        (*print-pretty* T) (*print-case* :downcase))
    (cond 
      (frames 
        (format stream "~%~%;;;---------------------------------------~%")
        (format stream ";;;   Frames ~%")
        (format stream ";;;---------------------------------------~%")

	     (mapc #'(lambda (a-frame-name)
		              (format stream 
               			"~2%;; ---------- FRAME '~S' :~%" a-frame-name)
		              (PRINT-FRAME (get-frame-name a-frame-name) stream) 
                              (EMAXPS-PRINT-B3-FRAME-COMPLIANCE a-frame-name stream)
		              (PRINT-INSTANCES a-frame-name stream))		     
		          frames))
    );cond
  t);let
)

;;; **************************************************************************
;;; Methoden und Funktionen zur Aenderung Frames
;;; **************************************************************************
                                                          
(defmacro my-defframe (kbase frame-name &body rumpf)
   "Definition eines Frames. Das Makro entspricht 
    dem Makro defframe."
  (let ((oldkb (emaxps-current-kb-name))) 
    (emaxps-select-kb kbase)  
    (and (current-kb-typep 'basic-frame-mixin)
       (definiere-frame frame-name rumpf))
    (emaxps-select-kb oldkb)  
                                      ;; Set the KB changed (LISP-side)
    (emaxps-kb-set-modified kbase)
  );let
) 

;;; --------------------------------------------------------------------------

(defun definiere-frame                                            
   (frame-name rumpf)
  "Definition eines Frames."     

     (eval ($send (send-kb :frame-processor) :new-frame-form frame-name rumpf))
)

;;; --------------------------------------------------------------------------
                  
(defun emaxps-delete-frame (kbase frame-name)
 "this is used by the C-Process"

 (let ((err :WAS-NEW) (oldkb (emaxps-current-kb-name)))
  (emaxps-select-kb kbase)

  (when (member frame-name (send-kb :frames))
   (setf err
    ($send (send-kb :frame-processor) :loesche-frame kbase frame-name)))

  (case err
   (:ERROR   ;; frame exists but has descendants...
    (emaxps-begin-message :emaxps-frame-delete)
    (emaxps-send-mbody "ERROR")
    (emaxps-end-of-message)
   )
   (:WAS-NEW ;; frame was newly created and not known to LISP yet!
    ;; informs C that Frame will be deleted!
    (emaxps-begin-message :emaxps-frame-delete)
    (emaxps-send-mbody "OK")
    (emaxps-end-of-message)
   )
   (T        ;; successfully removed existing frame!
    ;; Set the KB changed (LISP-side)
    (emaxps-kb-set-modified kbase)
    ;; informs C that Frame will be deleted!
    (emaxps-begin-message :emaxps-frame-delete) 
    (emaxps-send-mbody "OK")
    (emaxps-end-of-message)
   )
  );case

  (emaxps-select-kb oldkb)

  err  ;;return-value
 );let 
)

;;; --------------------------------------------------------------------------

(def$method (frame-base :loesche-frame) (kbase frame-name)
  "Loescht einen Frame. Ein Frame kann nur geloescht werden, wenn er keine
   Instanzen und Behaviors besitzt, ausserdem sollte er keine Subframes
   besitzen.
   Das Loeschen bewirkt folgende Aktionen:
   1. Der Framename wird aus den p-Listen der SuperFrames geloescht
      (Schluessel: ':SUBCLASSES', Wert: Frame-Name) 
   2. der Framename wird aus den Listen, auf die die 
      Instanzvariblen frames und frames-list zeigen, entfernt.
   3. Schluessel ':frame-definition' aus der p-Liste von 
         frame-internal-name entfernen
   4. Schluessel ':instances' aus der p-Liste von frame-internal-name 
      entfernen"

 (let ((frame-internal-name (get-frame-name frame-name))
       (instances (get-instance-list frame-name))
       (behaviors (get-frame-behavior-specs frame-name))
       (subframes (get-subframes frame-name)))

  (if (or instances behaviors subframes)
   (progn
    (emaxps-begin-message :emaxps-frame-delete) 
    (emaxps-send-mbody "ERROR")
    (emaxps-end-of-message)
    :ERROR   ;;return-value
   );progn
  ;else
   (progn
    (emaxps-begin-message :emaxps-frame-delete) 
    (emaxps-send-mbody "OK")  ;; inform C that Frame will be deleted!
    (emaxps-end-of-message)         
    (emaxps-kb-set-modified kbase) ;; Set the KB changed (LISP-side)

    (dolist (super-frame-name (get-supers frame-name)) 
     (let* ((super-frame-internal-name 
             (get-frame-name super-frame-name)))
      (if (null (setf (frame-subclasses super-frame-internal-name) 
                       (delete frame-name  
                        (frame-subclasses super-frame-internal-name)))) 
       ;;wenn der Eintrag den Wert nil hat, wird das Eigenschafts-
       ;;Werte-Paar aus der P-Liste entfernt
       (remprop super-frame-internal-name ':subclasses)
      );if
     );let*
    );dolist

    (setf frames-list (delete frame-name frames-list))   
    (send-kb :set-frames frames-list)   
    (remprop frame-internal-name :instances)  
    (remprop frame-internal-name :frame-definition)

    NIL   ;;return-value (OK)
   );progn   
  );if error-descriptions
 );let
)
                            
;;; --------------------------------------------------------------------------
 
(defun hole-frame-definition-mit-menue (kbase)
   "Gibt die Definition eines Frames im Editor aus.
    Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, 
    aus dem einFrame aus gewaehlt werden kann. "
    (let ((oldkb (emaxps-current-kb-name))) 
      (emaxps-select-kb kbase) 
      (let ((frame-namen (send-kb  :frames))
        (frame-name nil))
         (progn
             (emaxps-begin-message :emaxps-frame-selection)
             (mapcar #'(lambda (x)
                 (emaxps-send-mbody "~A" x)
                 (emaxps-separate-mparts)
                )
                frame-namen
             )
             (emaxps-send-mbody "dummy" )
             (setf frame-name (emaxps-eom-get-reply))
             (if frame-name
               (hole-frame-definition kbase  frame-name)
             );if
         );progn
      );let
      (emaxps-select-kb oldkb)
    );let 
)         

;;; --------------------------------------------------------------------------

(defun hole-frame-definition (kbase frame-name)
  "Gibt die Definition eines Frames mit dem 
   Namen 'frame-name' im Editor aus."

  
  (let ((*print-pretty* T) (*print-case* :upcase)
       (supers nil)                       
       (oldkb (emaxps-current-kb-name))
       (frame-slots nil))    
       (emaxps-select-kb kbase)
       (emaxps-begin-message :emaxps-frame-definition)
       (emaxps-send-mbody "~s" frame-name)
       (emaxps-separate-mparts)
       (setf *print-case* :downcase)

       (cond ((is-frame frame-name) ;Frame bekannt                         
                 (setq supers (get-supers frame-name))                       
                 (setq frame-slots (get-frame-slots frame-name))    
                 (if supers              
                    (emaxps-send-mbody "~{~s  ~}" supers) 
                 ;; else
                    (emaxps-send-mbody " ") 
                 )
                 (emaxps-separate-mparts)
                 (if frame-slots       
                    (mapcar #'(lambda(s) 
                           (emaxps-send-mbody "~s" s )) frame-slots)
                 ;; else
                    (emaxps-send-mbody "  ")
                 );if  
              )   
               (t  (emaxps-send-mbody " " ) ;Frame nicht bekannt      
               (emaxps-separate-mparts)
               (emaxps-send-mbody " " ) )       
       );cond   
       (emaxps-end-of-message)  
       (setf *print-case* :upcase)
       (emaxps-select-kb oldkb)
  ) ;let
)              

;;;*********** EOF ***********************************************************

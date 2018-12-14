#|***************************************************
*   prolog.cl     Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                 Klaus Senf                        *
*                                                   *
*   Functions to build the prolog-editor.           *
*   Full version only.                              *
***************************************************|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; statt format-klausel: analog write-kb vorgehen! (hole definition...
#|
Funktionsweise:
* Die Globale Variable *axiom-sets* enthaelt alle zur Zeit
  bekannten Klauselmengen! (also jemals in die LISP-Welt geladenen Mengen...)
  (get-known-axiom-sets) liest diese Variable aus!
* (send-kb :relations-name) returned ein Symbol: der Name derjenigen
  Klauselmenge aus *axiom-sets*, die fuer die aktuelle Wissensbasis
  die FESTE Klauselmenge bildet! (den Prolog-Bereich: dieser steht
  bei der Abarbeitung vor allen anderen Klauselmengen.... Er ist
  immer vorhanden, sobald ein Prolog Prozessor ex. (bei EMA-XPS also IMMER!)
  Ggf. ist er Leer! Solange noch keiner gewaehlt worden ist, heisst er
  KBNAME-CLAUSES ...)
* Die zur Zeit von der aktuellen KB genutzten weiteren Axiom-sets
  sind eine Teilmenge von *axiom-sets* und koennen mittels
  (send-prolog :axioms) in ihrer Reihenfolge ermittelt werden!
* WARNUNG: wenn eine Wissensbasis zur Sitzungszeit die Anzahl und
  Reihenfolge der AXsets aendert, wuerde das Auswirkungen auf das
  KBspeichern haben!
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(in-package "BABYLON")

; Aktualisiere C-Part: Axiom-Liste 

(defun axiom_update ()
  (emaxps-begin-message :emaxps-new-axioms)
  (mapcar #'(lambda (x)
                (emaxps-send-mbody "~S" x)
                (emaxps-separate-mparts)
            )
            *axiom-sets*
   )
   (emaxps-send-mbody "dummy" )
   (emaxps-end-of-message)
)


(def$method (prolog-trace-mixin :after :set-axioms) (axsets)
 (declare (ignore axsets))
 (axiom_update)
)

;;---------------------------------------------------------------------------
;;;;; from bp-mixin
;;; Deletes an axiom

(defun remove-default-axset (name)
  (remprop name 'preds)
  (remprop name 'kb-name)
  (setf  *axiom-sets* (remove name *axiom-sets*))
  (axiom_update)
)

;;---------------------------------------------------------------------------
;;;;; from bp-mixin 


(def$method (basic-prolog-mixin :add-axiom-set)
	    (axset-name &optional (mode 'first) (before-set nil) (check t))
  "adds the axiom set named <axset-name> to the currently available axiom sets.
if <mode> is first or last, the axiom-set becomes the first or last axiom set, 
if <mode> is before and <before-set> is specified, the axiom-set is added immediately
before it. if <check> is not nil, it is checked whether <axset-name> is known."

; **** Hier wird ein AXIOM an eine KB-gebunden! 
; **** (Klausel-Bindung siehe init-axset wenn kb-name nicht NIL

  (emaxps-begin-message :emaxps-new-kbaxiom)
  (emaxps-send-mbody "~s" kb-name )
  (emaxps-separate-mparts)
  (emaxps-send-mbody "~s" axset-name )
  (emaxps-end-of-message)


($send prolog-processor :addax axset-name mode before-set check))

;;---------------------------------------------------------------------------
;;;;; from bp-mixin
; *** Hier wird ein Axiom von einer KB geloest


(def$method (basic-prolog-mixin :remove-axiom-set) (axset-name)
  "makes the axiom set named <axset-name> unavailable."

  ($send prolog-processor :send-if-handles :remax axset-name))


;;---------------------------------------------------------------------------
;;;;; from bp-mixin
; *** Kb-Clausel-Reset

(def$method (basic-prolog-mixin :init-kbaxset) ()

  (setf relations-name
	(or relations-name (intern (format nil "~S-CLAUSES" kb-name))))
  (init-axset relations-name kb-name)
  ($send prolog-processor :set-axioms (list relations-name))
  relations-name)



;;---------------------------------------------------------------------------
;;;;; from kernel/prolog/basic/axioms.cl

(def$method (axset-basic :remax) (axiom-set)
  "makes the free axiom set <axiom-set> unavailable."

  (unless (eq axiom-set (first axioms))
    (let ((new-axioms (delete axiom-set axioms :count 1)))
      ($send self :set-axioms new-axioms))))



;;---------------------------------------------------------------------------
;;;;; from bp-mixin

(def$method (basic-prolog-mixin :assert-clauses) (clauses)
  "builds up an internal representation for <the-relations>."

  ( let (( test T)( ok T))
  (when (member (first clauses) *axiom-sets*  )
    (assert-axioms 'only-a-temp-clause (rest clauses))    
    (setf test (equal (collect-clauses (first clauses) (get-preds (first clauses))) (collect-clauses 'only-a-temp-clause (get-preds 'only-a-temp-clause)))) 
    (loesche-klauselmenge 'only-a-temp-clause)
    (when (not test)
      (if (not (emaxps-accept-cancel :axiom-overwrite-p
                (format nil "~s" (first clauses))))
          (setf ok nil)
      )
    )  
   )
   (when ok
     (when  (symbolp (first clauses))
         (let ((new-name (first clauses)))
          (unless (eq new-name relations-name)
           (remove-default-axset relations-name)
           (setf relations-name new-name)
           (emaxps-begin-message :emaxps-new-kbaxiom)
           (emaxps-send-mbody "~s" kb-name )
           (emaxps-separate-mparts)
           (emaxps-send-mbody "~s" new-name )
           (emaxps-end-of-message)
           ($send self :init-kbaxset)
          )
         )
         (setf clauses (rest clauses))
      );when
      (assert-axioms relations-name clauses kb-name)

     );when ok
   );let
  

)






;-----------------------------------------------------------------

(defun init-axset (axset-name &optional kb-name)
  "builds an empty axiom set named <axset-name>.
if <kb-name> is not NIL, <axset-name> is marked to be associated with <kb-name>."


  (cond ((member axset-name *axiom-sets*)
	 (remove-all-clauses axset-name))
	(t (push axset-name *axiom-sets*)
	   (setf (get axset-name 'preds) (list '$preds))
	   (if kb-name
	     (progn
               (setf (get axset-name 'kb-name) kb-name)
                 (emaxps-begin-message :emaxps-reset-prolog-of-kb)
                 (emaxps-send-mbody "~s" kb-name )
                 (emaxps-end-of-message)
                 (emaxps-begin-message :emaxps-new-kbaxiom)
                 (emaxps-send-mbody "~s" kb-name )
                 (emaxps-separate-mparts)
                 (emaxps-send-mbody "~s" axset-name )
                 (emaxps-end-of-message)
             )
           )
         )
   )
(axiom_update)
axset-name
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun assert-axioms (axset-name clauses &optional kb-name)
  "builds an axiom set named <axset-name> consisting of the predicates defined by <clauses>.
if <kb-name> is not NIL, <axset-name> is marked to be associated with <kb-name>."

(let ((ok T)(test T))
 ;(if ( known-axiom-set axset-name ))
  (when  (member axset-name (get-known-free-axiom-sets)) 
       (assert-axioms 'only-a-temp-clause clauses)    
       (setf test (equal (collect-clauses axset-name (get-preds axset-name)) (collect-clauses 'only-a-temp-clause (get-preds 'only-a-temp-clause)))) 
       (loesche-klauselmenge 'only-a-temp-clause)
       (when (not test)
        (if (not (emaxps-accept-cancel :axiom-overwrite-p
            (format nil "~s" axset-name)))
          (setf ok nil)
        )
       )  
  )
  (when ok   
   (init-axset axset-name kb-name)
   (add-axioms axset-name clauses)
   (setf (get axset-name 'clauses) clauses)
   (axiom_update)
    axset-name
  );when

);let
)



;------------------------------------------------------------------------

(def$method (axset-basic :reset-axiom-sets) (&rest axiom-sets)
  "resets <axiom-sets> if specified or all currently available axiom sets.
all modifications made by consult, reconsult, edit clauses or by prolog programs
are reset."

  (let ((axiom-sets-copy (or (copy-list axiom-sets)
			     axioms)))
    (mapc #'(lambda (axiom-set)
	      (reset-axiom-set axiom-set axioms))
	  axiom-sets-copy)
    axiom-sets-copy)
 (axiom_update)
)





;; =======================================================
;; Aenderungen des Prolog Editors verankern
;; =======================================================

(def$method (basic-prolog-mixin :definiere-klauselmenge)
            (klauselmengenname typ &rest klauseln)
  "Definition einer freien (typ = frei) 
   oder (typ = fest) festen Klauselmenge.
   klauseln ist eine Liste mit den Klauseln."   
                              
 (let* ((klauseln (car klauseln)))
      (cond 
        ((equal typ 'fest) 
            ;; feste Klauselmenge definieren 
           ($send self :definiere-feste-klauselmenge 
                              klauselmengenname klauseln))
            ;; frei Klauselmenge definieren 
        (t ($send self :definiere-freie-klauselmenge 
                              klauselmengenname klauseln))   
      );cond    
 );let
)

;; -------------------------------------------------------
                                                              

;; =======================================================
;; Loeschen
;; =======================================================


;; -------------------------------------------------------

(defun loesche-prolog-bereich ()     
   "Loeschen der festen Klauselmenge im Prologbereich"
   ;; alte Klauselmenge im Prologbereich loeschen
   (loesche-klauselmenge (send-kb :relations-name))
   (send-kb :init-kbaxset)  
)                        

;; -------------------------------------------------------


(defun loesche-klauselmenge (klauselmengenname)     
   "Loeschen einer freien Klauselmenge"
 
   (mapcar #'(lambda(symbol) 
         (remprop symbol klauselmengenname)) 
      (get-predicates klauselmengenname))

   ;; entfernen der Eigenschafts-werte-Paare 'preds' und 'kb-name' 
   ;; aus der p-Liste der Klauselmenge 
   ;; und entfernen des Namens aus der Liste auf die die globale
   ;; Variable *axiom-sets* zeigt
   (remove-default-axset klauselmengenname)
   (remprop klauselmengenname 'clauses)
) 

;; -------------------------------------------------------


(def$method (basic-prolog-mixin :hole-aktuelle-klauselmengen) ()
 "Die Funktion laesst den C-Server ein 1-aus-n-Menu aufmachen, in dem 
 die aktuellen Klauselmengen angezeigt werden. Die Definiton 
 der ausgewaehlten Klauselmenge wird im Prologeditor angezeigt." 

 (let ( ;;; (current-axioms *axiom-sets*)
       klauselmenge)

  (emaxps-begin-message :emaxps-prolog-select)
  (emaxps-end-of-message)

  (if klauselmenge
   (if (get klauselmenge 'kb-name)  
    (send-kb :hole-klauselmengen-definition klauselmenge 'fest)    
   ;else 
    (send-kb :hole-klauselmengen-definition klauselmenge 'frei)    
   );if
  );if klauselmenge
 );let
)

;; -------------------------------------------------------


(def$method (basic-prolog-mixin :hole-klauselmengen-definition) 
            (k-name &optional (typ 'frei))    
 "Zeigt die Definition einer Klauselmengen (feste oder freie Klauselmengen)
 im Prologeditor an. Klauselmenge braucht nicht bekannnt sein." 

 (let ((klauseln nil) (*print-case* :upcase))
       (emaxps-begin-message :emaxps-prolog-klauselmengen-def)
       (emaxps-send-mbody "~A" k-name)
       (emaxps-separate-mparts)

       (if (or (get k-name 'kb-name) (equal typ 'fest))
        (emaxps-send-mbody "FEST")
       ;else
        (emaxps-send-mbody "FREI")
       )  
       (emaxps-separate-mparts)
       (setf *print-case* :downcase)
       (if (setq klauseln (collect-clauses k-name (get-preds k-name)))         
        (emaxps-send-mbody "~S" (format-klausel klauseln))
       ;else
        (emaxps-send-mbody " ")
       ) 
       (emaxps-end-of-message)
 );let 
)                                                             

;; -------------------------------------------------------

(defun format-klausel (klauseln)
 "Formatiert die Klausel fuer die C-Seite"

 (let ((out NIL))
  (mapcar #'(lambda (x)
             (when x
              (setf out (append out (list
               (append
                (if (rest x)
                 (list (first x) '<-)
                ;else
                 (list (first x))
                );if
                (rest x)
               );append
              )))
             );when
            );lambda
          klauseln
  );mapcar
  out   ;; return-value
 );let
)
   
;; =======================================================
;; procedures for printing into a KB file
;; =======================================================

(def$method (basic-prolog-mixin :emaxps-print-fixed-clauses)
            (stream)  
 "Prints the set of fixed clauses of the current KB to stream"

 (let ((name (send-kb :relations-name))
       (*print-pretty* T) (*print-case* :downcase))
  (emaxps-print-clset name stream T)
 );let
)

;; -------------------------------------------------------

(def$method (basic-prolog-mixin :emaxps-print-used-free-clauses)
            (stream)
 "Prints all sets of free clauses of the current KB to stream"

 (let ((cl-sets (remove
         (send-kb :relations-name)      ;;clauses (see above)!
         ;(get-known-axiom-sets)        ;;-all-free-clauses...      ** KV **
         (send-prolog :axioms)         ;;-used-free-clauses...
      )))
  (when cl-sets
   (mapcar #'(lambda (x)
              (emaxps-print-clset x stream))
           cl-sets
   );mapcar
  );when
 );let
)

;; -------------------------------------------------------

(defun emaxps-print-clset (name stream &optional klausel )
 "Prints the set of clauses 'name' to 'stream'.
 See main.cl for enhancements of emaxps-defclauses
 and emaxps-defaxiom-set!"

 (let ((cl-names (rest (get-preds name)))  ;names of all sets-of-clauses
       (*print-pretty* T) (*print-case* :upcase))
;  (if (get name 'kb-name)  ** KV **
   (if klausel
   (format stream "~%(emaxps-defclauses ~S~%" name)
  ;else
   (format stream "~%(emaxps-defaxiom-set ~S~%" name)
  );if
  (mapcar #'(lambda (x)
             (emaxps-print-one-clause x stream))
         (collect-clauses name cl-names)
  );mapcar
  (format stream "~%);done~%")
 );let
)

;; -------------------------------------------------------

(defun emaxps-print-one-clause (clause stream)
 "Prints one clause to stream."

 (let ((clname (first clause))
       (*print-pretty* T) (*print-case* :downcase)
       (body (rest clause)))
  (cond
   (body   ;; name AND body exist...
    (format stream "~%(~S~%<-" clname)
    (mapcar #'(lambda (b) (format stream "~%~S" b)) body)
    (format stream ")~%")
   )
   (clause ;; ONLY the name-part ex.!
    (format stream "~%~S~%" clause)
   )
  );cond
 );let
)

;; eof

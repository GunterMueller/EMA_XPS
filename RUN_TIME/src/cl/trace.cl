#|***************************************************
*   trace.cl      Hans Groschwitz        19.07.95   *
*                 Karsten Vossberg                  *
*                                                   *
*   Functions to build the tracer.                  *
*   Full version only.                              *
*                                                   *
*   The :send-xxx methods override existing         *
*   b22-versions.                                   *
***************************************************|#

(in-package "BABYLON")

;; ------------------------------------------------------
;; Task-Trace
;; ------------------------------------------------------

(def$method (emaxps-task-processor :after :toggle-task-trace) ()
 "toggles the trace state for emaxps tasks."

 (emaxps-begin-message :trace-task-toggle)
 (emaxps-send-mbody "~a" (if (emaxps-send-task :trace-enabled) 1 0))
 (emaxps-end-of-message)
)

;; The Task protocol is sent in main.cl !!!


;; ------------------------------------------------------
;; System-Trace
;; from kernel/meta/m-mixin.cl
;; ------------------------------------------------------

(def$method (meta-processor-core :send-system-trace-window)
            (selector &rest args)
 "hier freut sich Vossi, einen super Kommentar einfuegen zu duerfen!!!"

 (when (equal :format selector)
  (emaxps-begin-message :trace-message)
  (emaxps-send-mbody "SYS: ")
  (apply 'emaxps-send-mbody args)
  (emaxps-end-of-message)
 );when
)

(def$method (meta-processor-core :after :toggle-system-trace) ()
 "Toggles system trace mode."

 (emaxps-begin-message :trace-system-toggle)
 (emaxps-send-mbody "~a" (if ($send self :system-trace) 1 0))
 (emaxps-end-of-message)
)


;; ------------------------------------------------------
;; Rule-Trace
;; from kernel/rules/mini/mr-mixin.cl
;; ------------------------------------------------------

(def$method (mini-rule-mixin :send-rule-trace-window) (selector &rest args)
 "passes messages to rule-trace-window."

 (when (equal :format selector)
  (emaxps-begin-message :trace-message)
  (emaxps-send-mbody "RUL: ")
  (apply 'emaxps-send-mbody args)
  (emaxps-end-of-message)
 );when
)

(def$method (rule-trace-mixin :after :toggle-rule-trace) ()
  "Toggles rule trace mode."

 (emaxps-begin-message :trace-rule-toggle)
 (emaxps-send-mbody "~a" (if ($send self :rule-trace) 1 0))
 (emaxps-end-of-message)
)


;; ------------------------------------------------------
;; Prolog-Trace
;; from kernel/prolog/mini/mp-mixin.cl
;; ------------------------------------------------------

(def$method (mini-prolog-mixin :send-prolog-trace-window) (selector &rest args)
 "passes messages to prolog-trace-window."

 (when (equal :format selector)
  (emaxps-begin-message :trace-message)
  (emaxps-send-mbody "PRL: ")
  (apply 'emaxps-send-mbody args)
  (emaxps-end-of-message)
 );when
)

(def$method (mini-prolog-mixin :after :toggle-prolog-trace) ()
 "toggles rule tracing."

 (send-kb :trace-preds 'all)
 (emaxps-begin-message :trace-prolog-toggle)
 (emaxps-send-mbody "~a"
  (if ($send (send-kb :prolog-processor) :prolog-trace) 1 0))
 (emaxps-end-of-message)
)


;; ------------------------------------------------------
;; Constraint-Trace (Consat)
;; from kernel/consat/mini/mc-mixin.cl
;; ------------------------------------------------------

(def$method (mini-constraint-mixin :send-consat-trace-window)
            (selector &rest args)
 "passes messages to consat-trace-window."

 (when (equal :format selector)
  (emaxps-begin-message :trace-message)
  (emaxps-send-mbody "CNS: ")
  (apply 'emaxps-send-mbody args)
  (emaxps-end-of-message)
 );when
)

(defun update-constraint-trace-mode (result-item-list constraints)
 "alle Constraints in result-item-list erhalten die Nachricht :trace-on,
 falls sie nicht protokolliert werden, bzw. :trace-off im entgegengesetzten Fall."

 (declare (ignore constraints))
 (let ((result nil))
  (when result-item-list 
   (mapc #'(lambda (constraint-assoc)
            (setf (get-object-of-c-assoc constraint-assoc)
             ($send (get-object-of-c-assoc constraint-assoc)
              (if (constraint-assoc-tracedp constraint-assoc) 
               (progn (setq result 0) :trace-off)
              ;else
               (progn (setq result 1) :trace-on)
              );if
              (get-name-of-c-assoc constraint-assoc)
           )));lambda
         result-item-list
   );mapc
   (send-kb :update-constraint-trace)
  );when
  (emaxps-begin-message :trace-constraint-toggle)
  (emaxps-send-mbody "~s" result)
  (emaxps-end-of-message)

  (= result 1)    ;; return-value
 );let
)




(def$method (mini-constraint-mixin :toggle-constraints-trace) ()
 "Toggling itself is done by update-constraint-trace-mode,
 the routines in front build a list of constraints/constraint-nets
 to be togled. Here all are toggled!
 Restrictions (normal-interface!!!) are missing!!"

 (let ((constr ($send self :constraints))
       (vo nil) k)
;  (when constr
;   (emaxps-begin-message :trace-constraint-toggle)
;   (emaxps-end-of-message)
;  );when
  (setf k (rest (build-constraint-trace-item-list constr) ))
  (mapcar #'(lambda (x)
             (setf vo (append vo (list (first x))))
            )
            k       
  );mapcar
  (update-constraint-trace-mode vo constr)
  (setf vo nil
        constr ($send self :constraint-nets))
  (setf k (rest (build-constraint-trace-item-list constr) ))
  (mapcar #'(lambda (x)
             (setf vo (append vo (list (first x))))
            )
            k       
  );mapcar
  (update-constraint-trace-mode vo constr)
 );let
)

;; eof

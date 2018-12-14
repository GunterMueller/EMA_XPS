#|*****************************************************************
*    b3-expln.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Dummy routines signalling errors if attempting to use the     *
*   babylon-3 explanation facility.                               *
*   Catches 45 functions/macros (2 local variables).              *
*                                                                 *
*   B3 Reference Manual, Chapter 5                                *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")

;; --- variables ---

;; The Symbols 'entity and 'result are bound locally within the
;; babylon-3 explanation facility. Hence they need not be emulated
;; outside... (see the symbol self in B2-behaviors)

;;------------------INTERNAL--------------------------

(defun %B3EXPLAIN%ERROR (txt)
 "This should NOT stop execution of the KB!"
 (
;   emaxps-error
   format t

   "B3-EMU: ~A:~%   illegal attempt to use the babylon-3 explanation facility"
   txt
 )
)

;;---------------B3 EXPLANATION FACILITY--------------

;; --- change-heuristic-threshold ---

(defmacro change-heuristic-threshold (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "change-heuristic-threshold")
)

;; --- change-heuristic-weight ---

(defmacro change-heuristic-weight (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "change-heuristic-weight")
)

;; --- explanations-of-class ---

(defmacro explanations-of-class (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "explanations-of-class")
)

;; --- explanations-of-type ---

(defmacro explanations-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "explanations-of-type")
)

;; --- explanation-operator ---

(defmacro explanation-operator (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "explanation-operator")
)

;; --- insert-explanations-of-class ---

(defmacro insert-explanations-of-class (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "insert-explanations-of-class")
)

;; --- insert-explanations-of-type ---

(defmacro insert-explanations-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "insert-explanations-of-type")
)

;; --- insert-explanation-operator ---

(defmacro insert-explanation-operator (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "insert-explanation-operator")
)

;; --- remove-explanations-of-class ---

(defmacro remove-explanations-of-class (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "remove-explanations-of-class")
)

;; --- remove-explanations-of-type ---

(defmacro remove-explanations-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "remove-explanations-of-type")
)

;; --- remove-explanation-operator ---

(defmacro remove-explanation-operator (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "remove-explanation-operator")
)

;; --- sort-operators-of-type ---

(defmacro sort-operators-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "sort-operators-of-type")
)

;; --- define-heuristic-op ---

(defmacro define-heuristic-op (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-heuristic-op")
)

;; --- define-heuristic-type ---

(defmacro define-heuristic-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-heuristic-type")
)

;; --- define-plan-op ---

(defmacro define-plan-op (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-plan-op")
)

;; --- define-plan-type ---

(defmacro define-plan-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-plan-type")
)

;; --- define-question-op ---

(defmacro define-question-op (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-question-op")
)

;; --- define-question-type ---

(defmacro define-question-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "define-question-type")
)

;; --- display-explanation ---

(defmacro display-explanation (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "display-explanation")
)

;; --- display-questions ---

(defmacro display-questions (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "display-questions")
)

;; --- do-inform ---

(defmacro do-inform (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "do-inform")
)

;; --- explanations ---

(defmacro explanations (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "explanations")
)

;; --- explanation-ask ---

(defmacro explanation-ask (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "explanation-ask")
)

;; --- generate-explanation ---

(defmacro generate-explanation (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "generate-explanation")
)

;; --- generate-questions ---

(defmacro generate-questions (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "generate-questions")
)

;; --- insert-file-entry ---

(defmacro insert-file-entry (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "insert-file-entry")
)

;; --- is-explainable ---

(defmacro is-explainable (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "is-explainable")
)

;; --- is-identifier-of ---

(defmacro is-identifier-of (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "is-identifier-of")
)

;; --- only-explainables ---

(defmacro only-explainables (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "only-explainables")
)

;; --- perform-explanation ---

(defmacro perform-explanation (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "perform-explanation")
)

;; --- perform-questions ---

(defmacro perform-questions (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "perform-questions")
)

;; --- retrieve-file-entry ---

(defmacro retrieve-file-entry (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "retrieve-file-entry")
)

;; --- template-levels ---

(defmacro template-levels (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "template-levels")
)

;; --- template-of-level ---

(defmacro template-of-level (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "template-of-level")
)

;; --- current-step ---

(defmacro current-step (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "current-step")
)

;; --- is-step ---

(defmacro is-step (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "is-step")
)

;; --- step-child ---

(defmacro step-child (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-child")
)

;; --- step-children ---

(defmacro step-children (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-children")
)

;; --- step-object ---

(defmacro step-object (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-object")
)

;; --- step-parent ---

(defmacro step-parent (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-parent")
)

;; --- step-parents ---

(defmacro step-parents (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-parents")
)

;; --- step-result ---

(defmacro step-result (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-result")
)

;; --- step-self ---

(defmacro step-self (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-self")
)

;; --- step-sibling ---

(defmacro step-sibling (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-sibling")
)

;; --- step-typep ---

(defmacro step-typep (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EXPLAIN%ERROR "step-typep")
)

;;; eof

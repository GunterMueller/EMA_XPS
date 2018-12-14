#|*****************************************************************
*    b3-editr.cl            Hans Groschwitz         15.09.94      *
*                                                                 *
*   Dummy routines signalling errors if attempting to use the     *
*   babylon-3 KB-editor generation facility.                      *
*   Catches 18 functions/macros (0 variables).                    *
*                                                                 *
*   B3 Reference Manual, Chapter 8                                *
*                                                                 *
*****************************************************************|#

(in-package "BABYLON")

;;------------------INTERNAL--------------------------

(defun %B3EDITGEN%ERROR (txt)
 (emaxps-error
  "B3-EMU: ~A:~%   illegal attempt to use the babylon-3 editor generation facility"
  txt)
)

;;---------------B3 EDITOR GENERATION FACILITY--------------

;; --- change-ruleset-of ---

(defmacro change-ruleset-of (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "change-ruleset-of")
)

;; --- change-frame-of ---

(defmacro change-frame-of (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "change-frame-of")
)

;; --- change-constraint-pattern-of ---

(defmacro change-constraint-pattern-of (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "change-constraint-pattern-of")
)

;; --- create ---

(defmacro create (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "create")
)

;; --- create-a ---

(defmacro create-a (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "create-a")
)

;; --- defining-form ---

(defmacro defining-form (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "defining-form")
)

;; --- erase ---

(defmacro erase (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "erase")
)

;; --- evaluate ---

(defmacro evaluate (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "evaluate")
)

;; --- evaluate-kb-elements ---

(defmacro evaluate-kb-elements (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "evaluate-kb-elements")
)

;; --- evaluate-modified-kb-elements ---

(defmacro evaluate-modified-kb-elements (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "evaluate-modified-kb-elements")
)

;; --- modifiedp ---

(defmacro modifiedp (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "modifiedp")
)

;; --- modified-kb-elements-p ---

(defmacro modified-kb-elements-p (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "modified-kb-elements-p")
)

;; --- kb-elements ---

(defmacro kb-elements (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "kb-elements")
)

;; --- the-kb-elements ---

(defmacro the-kb-elements (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "the-kb-elements")
)

;; --- kb-elements-of-type ---

(defmacro kb-elements-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "kb-elements-of-type")
)

;; --- the-kb-elements-of-type ---

(defmacro the-kb-elements-of-type (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "the-kb-elements-of-type")
)

;; --- rename ---

(defmacro rename (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "rename")
)

;; --- move ---

(defmacro move (&rest args)
 "Dummy!"
 (declare (ignore args))
 (%B3EDITGEN%ERROR "move")
)

;;; eof

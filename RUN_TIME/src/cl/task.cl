#|***************************************************
*   task.cl       Hans Groschwitz        19.07.95   *
*                 Stephan Peters                    *
*                                                   *
*   Functions to build the task-editor.             *
*   Full version only.                              *
***************************************************|#

(in-package "BABYLON")

; ---------------------------------------------------------------
; Management functions for known-task-list
; ---------------------------------------------------------------

(defun emaxps-task-remove-task (kbname tname)
 "removes a task, if known in the current KB."

 ($send ($send (symbol-value kbname) :task-processor) :remove-task tname) 
 (emaxps-kb-set-modified kbname)
)

; ---------------------------------------------------------------

(defun emaxps-task-add-task (kbname tname body) 
 "adds/replaces a task to the current KB.
  The body must be a complete lambda-expression.
  It should be checked for completeness HERE!
  The method doesn't do it!
  Additionally the KB-modified flag must be set!"

 ($send ($send (symbol-value kbname) :task-processor) :add-task tname body) 
 (emaxps-kb-set-modified kbname)
)

; ---------------------------------------------------------------

(defun emaxps-task-get-task-body (kbname tname) 
 "gets the body of a task, if known in the current KB, else NIL"

 (let ((*print-case* :downcase) body comment arglist restargs 
       (task ($send ($send (symbol-value kbname) :task-processor) :get-body tname)))
  (emaxps-begin-message :emaxps-task-send-body)
  (setf arglist (second task))
  (setf restargs (rest (rest task)))
  (setf body (rest restargs))
  (setf comment (first restargs))
  (unless (stringp comment)
   (setf body restargs)
   (setf comment "")
  );unless
  (if arglist
   (emaxps-send-mbody "~S" arglist)
  ;else
   (emaxps-send-mbody "()")
  );if
  (emaxps-separate-mparts)
  (emaxps-send-mbody "~S" comment)
  (emaxps-separate-mparts)
  (if body
   (emaxps-send-mbody "~S" body)
  ;else
   (emaxps-send-mbody "()")
  );if
  (emaxps-end-of-message)
 );let
);defun

; ---------------------------------------------------------------

(defun emaxps-task-list-tasks (kbname)
 "returns a list of task-names, known in the current KB."

 (let ((tasklist ($send ($send (symbol-value kbname) :task-processor) :tasks)))
  (emaxps-begin-message :emaxps-task-list-tasks)
  (mapcar #'(lambda (x)
             (emaxps-send-mbody "~S" x)
             (emaxps-separate-mparts)
            );lambda
          tasklist
  );mapcar
  (emaxps-end-of-message)
 );let
 NIL   ;;return-value
)

; ---------------------------------------------------------------

;; eof

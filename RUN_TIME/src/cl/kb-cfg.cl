;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    kb-cfg.cl        Hans Groschwitz      01.12.94   ;;
;;                                                     ;;
;;    This file MUST be loaded after all def$method's  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

; -------------------------------------------------------
; EMA-XPS 2.0 only accepts one type of
; KB configuration:
; Hence this may be known globally and
; is done here:
; -------------------------------------------------------

(def-kb-configuration emaxps-cfg
  (:procs normal-frame-mixin
          normal-rule-mixin
          normal-constraint-mixin
          lisp-mixin
          normal-prolog-mixin
          emaxps-task-mixin
  );procs
  (:interface normal-interface-mixin))

; -------------------------------------------------------

(compile-$flavor-$methods emaxps-cfg)

;;; eof

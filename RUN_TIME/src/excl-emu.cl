;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     excl-emu.cl       Hans Groschwitz      01.12.94      ;;
;;                                                          ;;
;;     CLISP does not offer the Allegro CL 4.1 EXCL package ;;
;;     All the same we need some functionality when running ;;
;;     babylon3 KBs... They need to be emulated             ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make :EXCL the current package with
;; "COMMON-LISP" as the only used package!
;;
(in-package "EXCL")


; ----------------------------------------------

(defun chdir (path-string)
 "Emulates the AllegroCL version of this call with the means
 of CLISP!
 Changes the current working directory of this LISP-process.
 Paths may be absolute or relative. Returns the path
 of the new cwd."

 (unless (or (null path-string) (stringp path-string)
             (equal "" path-string))
  (emaxps-error "chdir: invalid path-string ~S" path-string))

 (let ((last-char (first (last (coerce path-string 'list)))))
  ;; CLISP-HACK: working with UNIX Pathname-convention ONLY!
  ;; (cd "..") fails, (cd "../") succeeds!
  (when (not (equal last-char #\/))
   (setf path-string (concatenate 'string path-string "/")))
  (ext::cd path-string)  ;; hg20080629: lisp -> ext
 );let
)

(defun current-directory ()
 "Emulates the AllegroCL version of this call with the means
 of CLISP! 
 Prints the current working directory of this LISP-process."

 (ext::cd)  ;; hg20080629: lisp -> ext
)

(defun run-shell-command (command-string &key (wait T))
 "Emulates the AllegroCL version of this call with the means
 of CLISP! 
 Works like the C-function system(). It runs a command-
 line interpreter, which interprets the command-string.
 It uses the *terminal-io* stream to communicate with the
 interpreter. Returns the value given from system()."

 (when (null wait)
  (emaxps-error "run-shell-command (emulation): key wait MUST be set to T"))
 (unless (stringp command-string)
  (emaxps-error "run-shell-command: command-string missing and redirect stdin failed"))

 (ext::shell command-string)  ;; hg20080629: lisp -> ext
)

; -------------------------------------------

 "Emulates the AllegroCL version of this call with the means
  of CLISP! 
  Enables interactive communication with the command-line
  interpreter. Uses the *terminal-io* stream to communicate
  with the interpreter. Returns the value given from it."

 ;; (1) remove old bindings via common-lisp (internal) to lisp (external)
 ;; (2) now it uses the more direct way of access...
 ;; (3) finally export it again...
 ;; now (lisp:shell), (excl:shell) and (common-lisp::shell)
 ;; lead to the same result
 ;;
 (unintern 'shell)
 (import 'ext:shell)  ;; hg20080629: lisp -> ext

; -------------------------------------------


(export '(chdir current-directory run-shell-command shell))


; -------------------------------------------

;; eof

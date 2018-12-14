;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ccload.lisp         Hans Groschwitz    08.10.94  ;;
;;                                                     ;;
;;    Used only in DEVELOPer mode to 'make' EMA-XPS.   ;;
;;    Runs compiler, if necessary, and loads bin-file. ;;
;;    It runs into the debugger in case of a failure.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "BABYLON")

;; -------------------------------------------------------

(defun emaxps-ccload (filename &key (path "./cl/")
            (src-ext ".cl") (bin-ext ".bin"))
 "filename is a string containting the name of the 
  file WITHOUT path and externsion.
  Do NOT forget the trailing PATHNAME-SEPARATOR
  for the path-key string!"

 (let (source-file binary-file)
  (setf source-file
   (concatenate 'string path filename src-ext)
  );setf
  (setf binary-file
   (concatenate 'string path filename bin-ext)
  );setf

;;;(setf binary-file source-file)

  (when (not (probe-file source-file))
   (emaxps-error "emaxps-ccload: ~A not found" source-file)
  );when
  (format *terminal-io* "~&~%;;; --------------------------------------")
  (when (or (not (probe-file binary-file))
            (> (file-write-date source-file)
               (file-write-date binary-file)))
   (format *terminal-io* "~&;;; EMA-XPS: Compiling ~A" source-file)
   (compile-file source-file :output-file binary-file)
  );when
  ;;;
  ;;; now check for success of compilation!
  ;;;
  (when (or (not (probe-file binary-file))
            (> (file-write-date source-file)
               (file-write-date binary-file)))
   (emaxps-error "emaxps-ccload: compiling ~A failed" source-file)
  );when
  (format *terminal-io* "~&;;; EMA-XPS: Loading ~A" binary-file)
  (load binary-file)
  (format *terminal-io* "~&;;; --------------------------------------~2%")
 );let
)

;; -------------------------------------------------------

(defun emaxps-load (filename)
 "filename is checked for existance and loaded, if exists"

 (when (probe-file filename)
  (format *terminal-io* "~&~%;;;; --------------------------------------")
  (load filename)
  (format *terminal-io* "~&;;;; --------------------------------------~2%")
 );when
)

;; eof

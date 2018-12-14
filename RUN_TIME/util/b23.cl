;; IT IS VERY IMPORTANT TO DO THESE STEPS MANUALLY, ONE BY ONE
;; DO NOT TRY TO PUT THEM IN ONE LINE!!!
(load "make.cl")
(in-package "BABYLON")
(make-babylon-image)
(format t "~%done with writing, waiting 3 secs before terminating...~%")
(lisp::sleep 3)
(ext::exit)

;; eof


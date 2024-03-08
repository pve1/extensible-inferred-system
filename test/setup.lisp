(require :asdf)
(require :extensible-inferred-system)

(in-package :cl-user)

#+sbcl (sb-ext:disable-debugger)
(setf *compile-verbose* nil)

(defun load-system (system)
  (asdf:load-system system)
  (terpri))

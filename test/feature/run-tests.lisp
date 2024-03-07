(require :asdf)
(require :extensible-inferred-system)

(in-package :cl-user)

#+sbcl (sb-ext:disable-debugger)

(defun load-system (system)
  (asdf:load-system system))

(load-system "extensible-inferred-system-test/basic")
(load-system "extensible-inferred-system-test/basic/basic-full")
(load-system "extensible-inferred-system-test/full")

(terpri)
(uiop:quit)

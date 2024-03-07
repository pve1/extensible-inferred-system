(require :asdf)
(require :extensible-inferred-system)

(in-package :cl-user)

#+sbcl (sb-ext:disable-debugger)

(defun load-system (system)
  (asdf:load-system system))

(load-system "extensible-inferred-system-comment-test/basic")
(load-system "extensible-inferred-system-comment-test/basic/basic-full")
(load-system "extensible-inferred-system-comment-test/basic/basic-no-dep")
(load-system "extensible-inferred-system-comment-test/full")

(terpri)
(uiop:quit)
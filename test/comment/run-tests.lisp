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

(setf (extensible-inferred-system::initial-symbol-name
       (asdf:find-system :extensible-inferred-system-comment-test))
      "dependencies")

(asdf:clear-system "extensible-inferred-system-comment-test/basic")
(asdf:clear-system "extensible-inferred-system-comment-test/basic/basic-dep")
(load-system "extensible-inferred-system-comment-test/initial-symbol")

(terpri)
(uiop:quit)

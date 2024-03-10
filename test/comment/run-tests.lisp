(load (merge-pathnames "../setup.lisp" *load-truename*))

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

(let ((system (asdf:find-system "extensible-inferred-system-comment-test")))
  (setf (extensible-inferred-system::ignore-lines system)
        "-*-")
  (load-system "extensible-inferred-system-comment-test/ignore-lines"))

(terpri)
(uiop:quit)

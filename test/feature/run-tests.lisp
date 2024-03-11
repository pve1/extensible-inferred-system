(load (merge-pathnames "../setup.lisp" *load-truename*))

(load-system "extensible-inferred-system-feature-test/basic")
(load-system "extensible-inferred-system-feature-test/basic/basic-full")
(load-system "extensible-inferred-system-feature-test/basic/basic-no-dep")
(load-system "extensible-inferred-system-feature-test/full")


(load-system "extensible-inferred-system-feature-test/skip-comments")
(setf (extensible-inferred-system::feature-system-skip-comments-p
       (asdf:find-system :extensible-inferred-system-feature-test))
      nil)
(asdf:clear-system "extensible-inferred-system-feature-test/skip-comments")
(asdf:clear-system "extensible-inferred-system-feature-test/skip-comments/skip-comments-dep")
(load-system "extensible-inferred-system-feature-test/skip-comments")
(setf (extensible-inferred-system::feature-system-skip-comments-p
       (asdf:find-system :extensible-inferred-system-feature-test))
      t)

(setf (extensible-inferred-system::feature-expression
       (asdf:find-system :extensible-inferred-system-feature-test))
      "CL-USER::BLAH")
(asdf:clear-system "extensible-inferred-system-feature-test/basic")
(asdf:clear-system "extensible-inferred-system-feature-test/basic/basic-dep")
(load-system "extensible-inferred-system-feature-test/feature-expression")
(setf (extensible-inferred-system::feature-expression
       (asdf:find-system :extensible-inferred-system-feature-test))
      "REQUIRES")

(terpri)
(uiop:quit)

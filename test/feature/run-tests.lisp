(load (merge-pathnames "../setup.lisp" *load-truename*))

(load-system "extensible-inferred-system-feature-test/basic")
(load-system "extensible-inferred-system-feature-test/basic/basic-full")
(load-system "extensible-inferred-system-feature-test/basic/basic-no-dep")
(load-system "extensible-inferred-system-feature-test/full")

(setf (extensible-inferred-system::feature-expression
       (asdf:find-system :extensible-inferred-system-feature-test))
      "CL-USER::BLAH")

(asdf:clear-system "extensible-inferred-system-feature-test/basic")
(asdf:clear-system "extensible-inferred-system-feature-test/basic/basic-dep")
(load-system "extensible-inferred-system-feature-test/feature-expression")

(terpri)
(uiop:quit)

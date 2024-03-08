(load (merge-pathnames "../setup.lisp" *load-truename*))

(load-system "extensible-inferred-system-quote-test/basic")
(load-system "extensible-inferred-system-quote-test/basic/basic-full")
(load-system "extensible-inferred-system-quote-test/basic/basic-no-dep")
(load-system "extensible-inferred-system-quote-test/full")

(terpri)
(uiop:quit)

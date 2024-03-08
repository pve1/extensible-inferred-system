(load (merge-pathnames "../setup.lisp" *load-truename*))

(load-system "extensible-inferred-system-test/basic")
(load-system "extensible-inferred-system-test/basic/basic-full")
(load-system "extensible-inferred-system-test/full")
(load-system "extensible-inferred-system-test/module/load-module-1")
(load-system "extensible-inferred-system-test/module/load-module-2")
(load-system "extensible-inferred-system-test/module/load-module-3")
(load-system "extensible-inferred-system-test/module/load-module-4")
(load-system "extensible-inferred-system-test/module/load-util")

(terpri)
(uiop:quit)

(cl:compile-file "test.lisp)
(load-shared-object "test.dll")
(cl:load "test.fasl")
(test-c-call::call-cfun)

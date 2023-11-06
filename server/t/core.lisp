(uiop:define-package #:admin-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:admin-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))

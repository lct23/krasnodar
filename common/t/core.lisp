(uiop:define-package #:common-tests/core
  (:use #:cl)
  (:import-from #:rove
                #:deftest
                #:ok
                #:testing))
(in-package #:common-tests/core)


(deftest test-example ()
  (ok t "Replace this test with something useful."))

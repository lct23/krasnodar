(uiop:define-package #:app/main
  (:use #:cl)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:app/server
                #:start-server-in-production))
(in-package #:app/main)


(defmain (main) ()
  (start-server-in-production))

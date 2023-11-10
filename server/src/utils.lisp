(uiop:define-package #:app/utils
  (:use #:cl)
  (:import-from #:local-time
                #:+iso-8601-date-format+))
(in-package #:app/utils)


(defparameter +human-datetime-format+
  ;; 2008-11-18 02:32
  (append +iso-8601-date-format+
          (list #\Space)
          '((:hour 2) #\: (:min 2))))

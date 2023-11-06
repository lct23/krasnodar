(uiop:define-package #:app/vars
  (:use #:cl)
  (:export #:yandex-metrika-code
           #:*dark-background*
           #:*light-background*
           #:*text-color*))
(in-package #:app/vars)


(defun yandex-metrika-code ()
  (uiop:getenv "YANDEX_METRIKA_CODE"))


(defun resend-api-key ()
  (uiop:getenv "RESEND_API_KEY"))


(defparameter *dark-background* "rgb(51, 53, 65)")

(defparameter *light-background* "rgb(61, 63, 75)")

(defparameter *text-color* "rgb(235, 236, 241)")

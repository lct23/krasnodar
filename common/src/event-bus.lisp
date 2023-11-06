(uiop:define-package #:common/event-bus
  (:use #:cl)
  (:import-from #:event-emitter
                #:event-emitter)
  (:export
   #:emit-event
   #:on-event))
(in-package #:common/event-bus)


(defclass bus (event-emitter)
  ())


(defvar *bus* (make-instance 'bus))


(defun on-event (event thunk)
  (unless (position thunk
                    (event-emitter:listeners *bus* event)
                    :test 'eq
                    :key #'event-emitter::listener-function)
    (event-emitter:on event *bus*
                      thunk)))


(defun emit-event (event &rest args)
  (apply #'event-emitter:emit event
         *bus* args))

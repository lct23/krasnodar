(uiop:define-package #:app/pages/edit-knowledge
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/models/knowledge
                #:knownledge-title
                #:get-knowledge)
  (:import-from #:app/widgets/edit-knowledge-form
                #:make-edit-knowledge-form-widget)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:serapeum
                #:fmt))
(in-package #:app/pages/edit-knowledge)


(defwidget edit-knowledge-page ()
  ())


(defun make-edit-knowledge-page ()
  (make-instance 'edit-knowledge-page))


(defmethod render ((widget edit-knowledge-page))
  (cl-ppcre:register-groups-bind (knowledge-id)
      ("/kb/(\\d+)/edit" (get-path))
    (let* ((knowledge (get-knowledge knowledge-id))
           (form (make-edit-knowledge-form-widget knowledge)))

      (title (fmt "Редактирование знания \"~A\""
                  (knownledge-title knowledge)))
      (render form))))

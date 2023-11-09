(uiop:define-package #:app/pages/del-knowledge
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/widgets/del-knowledge
                #:make-del-knowledge-widget)
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
(in-package #:app/pages/del-knowledge)


(defwidget del-knowledge-page ()
  ())


(defun make-del-knowledge-page ()
  (make-instance 'del-knowledge-page))


(defmethod render ((widget del-knowledge-page))
  (cl-ppcre:register-groups-bind (knowledge-id)
      ("/kb/(\\d+)/del" (get-path))
    (let* ((knowledge (get-knowledge knowledge-id)))
      (cond
        (knowledge
         (let ((form (make-del-knowledge-widget knowledge)))
           (title (fmt "Удаление знания \"~A\""
                       (knownledge-title knowledge)))
           (render form)))
        (t
         (title "Знание не найдено"))))))

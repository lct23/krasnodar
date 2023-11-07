(uiop:define-package #:app/widgets/add-knowledge-form
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:mito
                #:object-id)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:app/models/board
                #:create-board)
  (:import-from #:app/widgets/utils
                #:*button-classes*)
  (:import-from #:models/app/knowledge
                #:create-knowledge))
(in-package #:app/widgets/add-knowledge-form)


(defwidget add-knowledge-form-widget ()
  ())


(defun make-add-knowledge-form-widget ()
  (make-instance 'add-knowledge-form-widget))


(defmethod render ((widget add-knowledge-form-widget))
  (render
   (button "Добавить знание"
           :on-click (lambda (&rest rest)
                       (declare (ignore rest))
                       (let ((obj (create-knowledge)))
                         (redirect (fmt "/kb/~A"
                                        (object-id obj)))))
           :class *button-classes*)))



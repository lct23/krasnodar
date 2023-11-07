(uiop:define-package #:app/pages/playground
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/pages/utils
                #:title)
  (:import-from #:app/widgets/questionnairies))
(in-package #:app/pages/playground)


(defwidget playground-widget ()
  ())


(defun make-playground-widget ()
  (make-instance 'playground-widget))


(defmethod render ((widget playground-widget))
  (title "Песочница")

  ;; (render (app/widgets/questionnairies::make-questionnairies-widget))
  ;; (render (app/widgets/add-board-form::make-add-board-form-widget))
  ;; (render (app/widgets/add-knowledge-form::make-add-knowledge-form-widget))
  (render (app/widgets/edit-knowledge-form::make-edit-knowledge-form-widget
           (models/app/knowledge::get-knowledge 20)))
  ;; (render (app/widgets/edit-board-form::make-edit-board-form-widget
  ;;          (app/models/board::get-board 6)))
  )



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

  (render (app/widgets/questionnairies::make-questionnairies-widget)))



(uiop:define-package #:app/pages/games
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
(in-package #:app/pages/games)


(defwidget test-game-widget ()
  ((title :initarg :title
          :reader game-title)
   (constructor :initarg :constructor
                :reader constructor)))


(defun make-test-game-widget (title constructor)
  (make-instance 'test-game-widget
                 :title title
                 :constructor constructor))


(defmethod render ((widget test-game-widget))
  (title (game-title widget))

  (render (funcall (constructor widget))))



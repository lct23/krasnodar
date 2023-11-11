(uiop:define-package #:app/models/game
  (:use #:cl)
  (:import-from #:mito))
(in-package #:app/models/game)


(defclass game ()
  ((title :col-type :text
          :initform ""
          :initarg :title
          :reader game-title)
   (widget-name :col-type :text
                :initform ""
                :initarg :widget-name
                :reader game-widget-name))
  (:metaclass mito:dao-table-class))

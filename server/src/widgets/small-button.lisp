(uiop:define-package #:app/widgets/small-button
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies))
(in-package #:app/widgets/small-button)


(defwidget small-button-widget (reblocks-ui2/buttons/button:button)
  ())


(defun small-button (content &key on-click)
  (reblocks-ui2/buttons/button:button content
                                      :widget-class 'small-button-widget
                                      :on-click on-click
                                      :class app/widgets/utils::*small-button-classes*))


(defmethod reblocks/widget:get-css-classes ((widget small-button-widget))
  (list "inline-block"))

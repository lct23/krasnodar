(uiop:define-package #:app/widgets/small-button
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks-ui2/buttons/button
                #:button))
(in-package #:app/widgets/small-button)


(defwidget small-button-widget (reblocks-ui2/buttons/button:button)
  ())


(defun small-button (content &key on-click (classes app/widgets/utils::*small-button-classes*) style)
  (button content
          :widget-class 'small-button-widget
          :on-click on-click
          :style style
          :class classes))


(defun small-and-round-delete-button (&key on-click)
  (small-button "✕"
                :on-click on-click
                :style "width: 1.6rem; height: 1.6rem"
                :classes "border border-red-300 bg-red-500 text-white rounded-full px-1 mx-2 transition duration-500 ease select-none hover:bg-red-600 focus:outline-none focus:shadow-outline"))


(defmethod reblocks/widget:get-css-classes ((widget small-button-widget))
  (list "inline-block"))

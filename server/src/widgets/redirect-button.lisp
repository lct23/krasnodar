(uiop:define-package #:app/widgets/redirect-button
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
  (:import-from #:reblocks/response
                #:redirect))
(in-package #:app/widgets/redirect-button)


(defwidget redirect-button-widget (reblocks-ui2/buttons/button:button)
  ())


(defun redirect-button (content url &key (class app/widgets/utils::*button-classes*)
                                         disabled)
  (button content
          :widget-class 'redirect-button-widget
          :on-click (lambda (&rest rest)
                      (declare (ignore rest))
                      (redirect url))
          :class class
          :disabled disabled))


;; (defmethod reblocks/widget:get-css-classes ((widget redirect-button-widget))
;;   (list "inline-block"))

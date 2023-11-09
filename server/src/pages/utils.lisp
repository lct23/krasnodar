(uiop:define-package #:app/pages/utils
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/page)
  (:import-from #:serapeum
                #:fmt))
(in-package #:app/pages/utils)


(defun title (title)
  (with-html
    (:h1 :class "text-xl font-bold mb-6 flex justify-center"
         title))

  (setf (reblocks/page:get-title)
        (fmt "HR Zero - ~A" title)))

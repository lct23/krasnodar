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
    (:h1 :class
         "text-2xl font-bold mb-6 border-b-8 border-blue-400 flex justify-center text-gray-600 p-2 shadow-md"
         ;; "text-2xl font-bold mb-6 flex justify-center bg-gradient-to-r from-blue-500 to-blue-700 text-white p-2 rounded-lg"
         title))

  (setf (reblocks/page:get-title)
        (fmt "~A :: HR Zero" title)))

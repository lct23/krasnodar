(uiop:define-package #:app/pages/html-to-sexp
  (:use #:cl)
  (:import-from #:plump)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui/form
                #:with-html-form))
(in-package #:app/pages/html-to-sexp)


;; Based on idea from https://github.com/albertolerda/html-to-cl-who/blob/main/html2clwho.lisp
(defun build-sexp (str &key (indent-size 4))
  (with-output-to-string (s)
    (labels
        ((iter (root &key (level 0))
           (cond
             ((plump:text-node-p root)
              (let ((txt (string-trim '(#\Space #\Newline #\Backspace #\Tab 
                                        #\Linefeed #\Page #\Return #\Rubout)
                                      (plump:text root))))
                (unless (equal txt "")
                  (format s " \"~A\"" txt))))
             (t 
              (let ((attrs (plump:attributes root)))
                (terpri s)
                (indent level)
                (format s "(:~A~:{ :~A \"~A\"~}"
                        (plump:tag-name root)
                        (loop for key being the hash-key of attrs
                              using (hash-value value)
                              collect (list key value)))
                (loop for child across (plump:children root)
                      do (iter child :level (1+ level)))
                (format s ")")))))
         (indent (level)
           (loop repeat (* indent-size level)
                 do (write-char #\Space s))))
      (map nil #'iter
           (plump:children (plump:parse str))))))


(defwidget html-to-sexp ()
  ((html :initform nil
         :type (or null string)
         :accessor html)
   (s-exp :initform nil
          :type (or null string)
          :accessor s-exp)))


(defun make-html-to-sexp-page ()
  (make-instance 'html-to-sexp))


(defmethod reblocks/widget:render ((widget html-to-sexp))
  (flet ((update-sexp (&key html &allow-other-keys)
           (setf (html widget)
                 html)
           (setf (s-exp widget)
                 (build-sexp html))
           (reblocks/widget:update widget)))
    (with-html-form (:post #'update-sexp)
      ;; https://tailwindcomponents.com/component/textarea
      (:textarea :name "html"
                 :rows "4"
                 :class "block p-2.5 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                 :placeholder "Enter your HTML here"
                 (html widget))

      (when (s-exp widget)
        (:code :class "block w-full h-20 overflow-scroll p-2.5 my-4 text-sm text-gray-900 bg-gray-400 rounded-lg border border-gray-300 "
         (:pre
          (s-exp widget))))
      
      (:button :type "submit"
               :class "border border-green-500 bg-green-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline"
               "Update"))))


(defmethod reblocks/widget:get-css-classes ((widget html-to-sexp))
  (list* "mx-20 my-8"
         (call-next-method)))


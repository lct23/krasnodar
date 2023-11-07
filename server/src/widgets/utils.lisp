(uiop:define-package #:app/widgets/utils
  (:use #:cl)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/models/department
                #:department-title
                #:get-departments)
  (:import-from #:mito
                #:object-id))
(in-package #:app/widgets/utils)


(defparameter *button-classes*
  "whitespace-nowrap max-h-12 border border-green-500 bg-green-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline")

(defparameter *small-button-classes*
  "max-h-6 border border-green-500 bg-green-500 text-white rounded-md px-1 mx-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline")


(defun submit-button (&key (text "Добавить"))
  (with-html
    (:button :type "submit"
             :class *button-classes*
             text)))

(defun text-input (name &key (type "text") placeholder label value)
  (with-html
    (let ((input-id (symbol-name (gensym "dsd"))))
      (:div :class "w-full"
            (when label
              (:label :for input-id
                      :class "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-300"
                      label))
            (:div :class "relative mb-6"
                  (:input :type type
                          :name name
                          :id input-id
                          :class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                          :placeholder placeholder
                          :value value))))
    ;; (:input :name name
    ;;         :type type
    ;;         :class "border px-2 my-2"
    ;;         :placeholder placeholder)
    ))

;; (defun text-input (name &key (type "text") placeholder)
;;   (with-html
;;     (:input :name name
;;             :type type
;;             :class "border px-2 my-2"
;;             :placeholder placeholder)))

(defun text-area (name &key placeholder (rows 10))
  (with-html
    (:textarea :name name
               :rows rows
               :class "block p-2.5 my-2 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
               :placeholder placeholder)))


(defun department-select-box (name &key label allow-empty selected-department-id)
  ;; https://tailwindcomponents.com/component/select-input-field
  (with-html
    (let ((input-id (symbol-name (gensym "dsd"))))
      (:div :class "mb-6"
       (when label
         (:label :for input-id
                 :class "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-400"
                 label))
       (:select :id input-id
         :name name
         :class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
         (when allow-empty
           (:option :selected (not selected-department-id)
                    :value ""
                    "Для всех"))
         (loop for dep in (get-departments)
               do (:option :value (princ-to-string
                                   (object-id dep))
                           :selected (and selected-department-id
                                          (equal selected-department-id
                                                 (object-id dep)))
                           (department-title dep))))))))

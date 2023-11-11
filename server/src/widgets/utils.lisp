(uiop:define-package #:app/widgets/utils
  (:use #:cl)
  (:import-from #:reblocks-ui2/themes/api
                #:*current-theme*)
  (:import-from #:reblocks-ui2/themes/tailwind
                #:make-tailwind-theme)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/models/department
                #:department-title
                #:get-departments)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/knowledge
                #:knownledge-title
                #:get-knowledges)
  (:import-from #:alexandria
                #:last-elt)
  (:import-from #:serapeum
                #:fmt
                #:push-end)
  (:import-from #:reblocks/widget
                #:update)
  (:import-from #:app/models/user
                #:user-department
                #:get-all-mentors
                #:user-name)
  (:import-from #:app/models/board
                #:board-title
                #:get-boards)
  (:import-from #:str
                #:replace-all)
  (:import-from #:reblocks-ui2/tables/themes/tailwind/table
                #:*default-header-cell-styles*))
(in-package #:app/widgets/utils)


(defparameter *button-classes*
  "whitespace-nowrap max-h-12 border border-blue-500 bg-blue-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease-in-out select-none
   hover:bg-blue-600 hover:shadow-xl hover:scale-105 hover:ring-2 hover:ring-blue-600
   focus:outline-none focus:shadow-outline
   disabled:bg-gray-300 disabled:border-gray-400 disabled:ring-0 disabled:scale-100 disabled:shadow-none disabled:text-gray-800")


(defparameter *green-button-classes*
  (replace-all "blue" "green"
               *button-classes*))

(defparameter *dangerous-button-classes*
  (replace-all "blue-600" "red-600"
               (replace-all "blue-500" "red-500"
                            *button-classes*)))

(defparameter *small-button-classes*
  "max-h-6 border border-green-500 bg-green-500 text-white rounded-md px-1 mx-2 transition duration-500 ease select-none hover:bg-green-600 focus:outline-none focus:shadow-outline")


(defun submit-button (&key (text "Добавить") (classes *button-classes*) disabled attrs)
  (with-html
    (:button :type "submit"
             :class classes
             :disabled (not (null disabled))
             :attrs attrs
             text)))

(defun redirect-button (text url &key (classes *button-classes*))
  (with-html
    (:a :href url
        :class classes
        text)))

(defun large-add-button (url)
  (with-html
    (:a :href url
        :class "flex flex-col items-center whitespace-nowrap h-40 w-40 border border-blue-500 bg-blue-500 text-white rounded-full m-2 transition duration-500 ease-in-out select-none hover:bg-blue-600 hover:shadow-xl hover:scale-105 focus:outline-none focus:shadow-outline hover:ring-2 hover:ring-blue-600"
        (:div :class "text-9xl" "+"))))

(defun text-input (name &key (type "text") placeholder label value attrs)
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "w-full"
            (when label
              (:label :for input-id
                      :class "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-300"
                      label))
            (:div :class "relative mb-6 flex flex-col"
                  (:input :type type
                          :name name
                          :id input-id
                          :class "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5  dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                          :placeholder placeholder
                          :attrs attrs
                          :value value)
                  (reblocks-ui/form:error-placeholder name))))))


(defun label (text &key for-id (classes "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-400"))
  (with-html
    (:label :for for-id
            :class classes
            text)))

(defun inline-label (text &key for-id (classes "block mb-2 font-medium text-gray-900 dark:text-gray-400"))
  (with-html
    (:label :for for-id
            :class classes
            text)))


(defun text-area (name &key placeholder (rows 10) value label attrs)
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (when label
        (label label
               :for-id input-id))
      (:textarea :id input-id
                 :name name
                 :rows rows
                 :class "block p-2.5 my-2 w-full text-sm text-gray-900 bg-gray-50 rounded-lg border border-gray-300 focus:ring-blue-500 focus:border-blue-500 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500"
                 :attrs attrs
                 :placeholder placeholder
                 value))))

(defparameter *checkbox-classes*
  "before:content[''] peer relative h-5 w-5 cursor-pointer appearance-none rounded-md border border-blue-gray-200 transition-all before:absolute before:top-2/4 before:left-2/4 before:block before:h-12 before:w-12 before:-translate-y-2/4 before:-translate-x-2/4 before:rounded-full before:bg-blue-gray-500 before:opacity-0 before:transition-opacity checked:border-pink-500 checked:bg-pink-500 checked:before:bg-pink-500 hover:before:opacity-10")


(defparameter *checkbox-svg-icon*
  "<svg
        xmlns=\"http://www.w3.org/2000/svg\"
        class=\"h-3.5 w-3.5\"
        viewBox=\"0 0 20 20\"
        fill=\"currentColor\"
        stroke=\"currentColor\"
        stroke-width=\"1\"
      >
        <path
          fill-rule=\"evenodd\"
          d=\"M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z\"
          clip-rule=\"evenodd\"
        ></path>
      </svg>")


(defun checkbox (name &key label checked disabled)
  ;; https://tailwindcomponents.com/component/tailwind-css-checkbox-with-label-by-material-tailwind
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "inline-flex items-center"
            (:label :class "relative flex cursor-pointer items-center rounded-full p-3"
                    :for input-id
                    (:input :id input-id
                            :name name
                            :type "checkbox"
                            :value "true"
                            :checked (not (null checked))
                            :disabled (not (null disabled))
                            :class *checkbox-classes*)
                    (:div :class "pointer-events-none absolute top-2/4 left-2/4 -translate-y-2/4 -translate-x-2/4 text-white opacity-0 transition-opacity peer-checked:opacity-100"
                          (:raw *checkbox-svg-icon*)))
            (when label
              (:label :class "mt-px cursor-pointer select-none font-light text-gray-700"
                      :for input-id
                      label))))))


(defparameter *select-box-classes*
  "bg-blue-800 border border-white text-white text-sm rounded-lg focus:ring-blue-500 focus:border-blue-800 block w-full p-2.5 transition duration-300 ease-in-out dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-800 dark:focus:border-blue-500 hover:bg-blue-900 dark:hover:bg-blue-800 hover:shadow-outline hover:ring-2 hover:ring-blue-500")

(defun department-select-box (name &key label allow-empty selected-department-id)
  ;; https://tailwindcomponents.com/component/select-input-field
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "mb-6"
            (when label
              (label label :for-id input-id))
            (:select :id input-id
              :name name
              :class *select-box-classes*
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


(defun mentor-select-box (name &key label selected-mentor-id)
  ;; https://tailwindcomponents.com/component/select-input-field
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "mb-6"
            (when label
              (label label :for-id input-id))
            (:select :id input-id
              :name name
              :class *select-box-classes*
              (:option :selected (not selected-mentor-id)
                       :value ""
                       "Ментор не нужен")
              (loop for dep in (get-all-mentors)
                    do (:option :value (princ-to-string
                                        (object-id dep))
                                :selected (and selected-mentor-id
                                               (equal selected-mentor-id
                                                      (object-id dep)))
                                ;; Пока выведем отдел в скобках,
                                ;; а по хорошему, надо отдавать только менторов
                                ;; из отдела в который нанимают сотрудника:
                                (fmt "~A (~A)"
                                     (user-name dep)
                                     (department-title
                                      (user-department dep))))))))))


(defun knowledge-select-box (name &key label allow-empty selected-knowledge-id)
  ;; https://tailwindcomponents.com/component/select-input-field
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "mb-6"
            (when label
              (:label :for input-id
                      :class "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-400"
                      label))
            (:select :id input-id
              :name name
              :class *select-box-classes*
              (when allow-empty
                (:option :selected (not selected-knowledge-id)
                         :value ""
                         "Для всех"))
              (loop for obj in (get-knowledges)
                    do (:option :value (princ-to-string
                                        (object-id obj))
                                :selected (and selected-knowledge-id
                                               (equal selected-knowledge-id
                                                      (object-id obj)))
                                (knownledge-title obj))))))))



(defun board-select-box (name &key label)
  ;; https://tailwindcomponents.com/component/select-input-field
  (with-html
    (let ((input-id (symbol-name (gensym "input"))))
      (:div :class "mb-6"
            (when label
              (:label :for input-id
                      :class "block mb-2 text-sm font-medium text-gray-900 dark:text-gray-400"
                      label))
            (:select :id input-id
              :name name
              :class *select-box-classes*
              
              (:option :value ""
                       "---")
              
              (loop for obj in (get-boards)
                    do (:option :value (princ-to-string
                                        (object-id obj))
                                (board-title obj))))))))


(defmacro add-deletion-callbacks (list-form)
  `(flet ((remove-widget (w)
            (setf ,list-form
                  (remove w ,list-form))
            (reblocks/widget:update w :removed t)))
     (loop for widget in ,list-form
           do (event-emitter:on :delete widget
                                #'remove-widget))))


(defmacro add-to-the-end (parent-widget list-form new-widget-form)
  `(flet ((remove-widget (w)
            (setf ,list-form
                  (remove w ,list-form))
            (reblocks/widget:update w :removed t)))
     (let ((last-widget
             (when ,list-form
               (last-elt
                ,list-form)))
           (new-widget ,new-widget-form))
       
       (event-emitter:on :delete new-widget
                         #'remove-widget)
       
       (push-end new-widget
                 ,list-form)
       (if last-widget
           (update new-widget :inserted-after last-widget)
           (update ,parent-widget)))))


(defun set-custom-reblocks-theme ()
  (setf *current-theme*
        (make-tailwind-theme))
  
  (setf *default-header-cell-styles*
        "p-3 font-bold uppercase bg-gradient-to-r from-blue-500 to-blue-300 text-white border border-gray-300 hidden lg:table-cell")
  (values))

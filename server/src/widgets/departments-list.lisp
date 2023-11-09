(uiop:define-package #:app/widgets/departments-list
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/department
                #:create-department
                #:get-departments
                #:department-title)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:app/widgets/utils
                #:submit-button
                #:text-input))
(in-package #:app/widgets/departments-list)


(defwidget departments-list-widget ()
  ((form :reader form)))


(defwidget add-department-form-widget (event-emitter:event-emitter reblocks-ui2/widget:ui-widget)
  ())


(defun make-departments-list-widget ()
  (let* ((form (make-instance 'add-department-form-widget))
         (list (make-instance 'departments-list-widget)))
    (event-emitter:on :object-created form
                      (lambda (object)
                        (update list)))

    (setf (slot-value list 'form)
          form)
    (values list)))


(defun render-department (department)
  (let ((cell-classes "w-full lg:w-auto p-3 text-gray-800 text-center border border-b block lg:table-cell relative lg:static"))
    (with-html
      (:tr :class "bg-white lg:hover:bg-gray-100 flex lg:table-row flex-row lg:flex-row flex-wrap lg:flex-no-wrap mb-10 lg:mb-0"
           (:td :class cell-classes
                (:span :class "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase"
                       "Название")
                (department-title department))
           (:td :class cell-classes
                (:span :class "lg:hidden absolute top-0 left-0 bg-blue-200 px-2 py-1 text-xs font-bold uppercase" "Действия")
                "")))))

(defmethod render ((widget departments-list-widget))
  (let ((header-classes
          "p-3 font-bold uppercase bg-gray-200 text-gray-600 border border-gray-300 hidden lg:table-cell"))
    (with-html
      (:table :class "border-collapse w-full"
              (:thead
               (:tr (:th :class header-classes "Название отдела")
                    (:th :class header-classes "Действия")))
              (:tbody
               (loop for department in (get-departments)
                     do (render-department department))))

      (render (form widget)))))


;; (defmethod get-dependencies ((widget departments-list-widget))
;;   (list*
;;    (reblocks-lass:make-dependency
;;      `(.departments-list-widget
;;        :color red))
;;    (call-next-method)))


(defmethod reblocks-ui2/widget:render ((widget add-department-form-widget) theme)
  (flet ((add-department (&key title &allow-other-keys)
           (log:info "Adding department with" title)

           (let ((department (create-department title)))
             (event-emitter:emit :object-created widget
                                 department)

             ;; Обновим себя, чтобы сбросить форму.
             (reblocks/widget:update widget))))
    
    (with-html-form (:post #'add-department
                     :class "w-full my-8 flex justify-center items-center")
      (text-input "title"
                  :label "Новый отдел"
                  :placeholder "Название отдела")
      (submit-button))))

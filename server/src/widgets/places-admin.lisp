(uiop:define-package #:admin/widgets/places-admin
  (:use #:cl)
  (:import-from #:reblocks-ui2/tables/table
                #:recalculate-cells
                #:current-row
                #:make-table
                #:column)
  (:import-from #:common/utils
                #:format-datetime-msk)
  (:import-from #:passport/models/vendor
                #:vendor-title
                #:vendor)
  (:import-from #:mito
                #:object-created-at
                #:select-dao
                #:find-dao)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:events/models/place
                #:place-phone
                #:place-address
                #:place-rating
                #:place-price
                #:place-description
                #:place-title
                #:place-photo
                #:place-vendor-id
                #:place)
  (:import-from #:sxql
                #:where
                #:limit
                #:order-by)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks-ui2/tables/search
                #:search-widget
                #:make-default-controls-widget
                #:filters-updated
                #:make-search-widget)
  (:import-from #:reblocks-ui2/events
                #:event-emitting-widget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:local-time
                #:format-rfc3339-timestring
                #:now
                #:timestamp-minimum)
  (:import-from #:alexandria
                #:last-elt
                #:length=)
  (:import-from #:admin/widgets/moderation-filter
                #:only-unmoderated-p
                #:filtering-query
                #:filters-widget)
  (:import-from #:reblocks-ui2/tables/clickable-row
                #:clickable-row-widget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui/popup
                #:hide-popup
                #:show-popup)
  (:import-from #:events/api/places
                #:unban-place
                #:ban-place))
(in-package #:admin/widgets/places-admin)


(defclass service ()
  ((type :initarg :type
         :reader service-type)
   (object :initarg :object
           :reader service-object)
   (vendor :initarg :vendor
           :reader service-vendor)))


(defun get-vendor-by (vendor-id)
  (find-dao 'vendor
            :id vendor-id))


(defun get-services (filters-widget &key page-key (limit 10))
  (check-type filters-widget filters-widget)
  (with-connection (:cached nil)
    (loop with query = (filtering-query filters-widget)
          with clauses = (remove nil
                                 (list (when page-key
                                         (where (:< :created_at page-key)))
                                       (when (only-unmoderated-p filters-widget)
                                         (where (:= :public 0)))
                                       (when (and query
                                                  (not (string= query "")))
                                         (where (:or (:like :title
                                                            (fmt "%~A%" query))
                                                     (:like :description
                                                            (fmt "%~A%" query)))))))
          with where-clause = (sxql.clause:compose-where-clauses clauses)
          for place in (select-dao 'place
                         where-clause
                         (order-by (:desc :created-at))
                         (limit (1+ limit)))
          for vendor-id = (place-vendor-id place)
          for vendor = (get-vendor-by vendor-id)
          collect (make-instance 'service
                                 :type :place
                                 :object place
                                 :vendor vendor) into services
          finally (return (let* ((has-next-page (length= services (1+ limit)))
                                 (results (if has-next-page
                                              (butlast services)
                                              services))
                                 (next-page-key
                                   (when has-next-page
                                     ;; Важно конвертнуть его в строку,
                                     ;; иначе SXQL вставит её без таймзоны
                                     (format-rfc3339-timestring
                                      nil
                                      (object-created-at
                                       (service-object (last-elt results)))))))
                            (values results
                                    (when next-page-key
                                      (lambda ()
                                        (log:debug "Using" next-page-key)
                                        (get-services filters-widget
                                                      :page-key next-page-key)))))))))


(defwidget search-widget-with-popup (search-widget)
  ((popup-widget :initform (make-instance 'reblocks-ui/popup:popup-widget)
                 :reader popup-widget)))



(defun make-table-with-filters ()
  (let ((columns
          (list (column "Добавлена"
                        :getter
                        (lambda (service)
                          (format-datetime-msk
                           (mito:object-created-at
                            (service-object service)))))
                (column "Услугодатель"
                        :getter
                        (lambda (service)
                          (passport/models/vendor::vendor-title
                           (service-vendor service))))
                (column "Название"
                        :getter
                        (lambda (service)
                          (let ((obj (service-object service)))
                            (etypecase obj
                              (events/models/place::place
                               (events/models/place::place-title obj))))))
                (column "Адрес"
                        :getter
                        (lambda (service)
                          (let ((obj (service-object service)))
                            (etypecase obj
                              (events/models/place::place
                               (events/models/place::place-address obj))))))
                (column "Действия"
                        :align :right
                        :getter
                        (lambda (service)
                          (let ((obj (service-object service))
                                (current-row (current-row)))
                            (etypecase obj
                              (events/models/place::place
                               (if (events/models/place::place-public-p obj)
                                   (button "Забанить"
                                           :on-click
                                           (lambda (&rest args)
                                             (declare (ignore args))
                                             (ban-place obj)
                                             (recalculate-cells current-row))
                                           :class "button small alert")
                                   (button "Одобрить"
                                           :on-click
                                           (lambda (&rest args)
                                             (declare (ignore args))
                                             (unban-place obj)
                                             (recalculate-cells current-row))
                                           :class "button small"))))))))))
    (make-search-widget
     columns
     'get-services
     ;; :widget-class 'search-widget-with-popup
     :filters-widget 'filters-widget
     :row-class 'row-with-popup-widget
     :controls-widget (make-default-controls-widget :button-title "Загрузить еще"))))



(defwidget place-info-popup (reblocks-ui/popup:popup-widget)
  ((place :initform nil
          :accessor popup-place)))



(defwidget row-with-popup-widget (clickable-row-widget)
  ((popup-widget :initform (make-instance 'place-info-popup)
                 :reader popup-widget)))


(defmethod reblocks-ui/popup:render-popup-content ((widget place-info-popup))
  (flet ((close-popup (&rest args)
           (declare (ignore args))
           (hide-popup widget)))
    (let ((js-action (reblocks/actions:make-js-action #'close-popup)))
      (with-html
        (let* ((service (popup-place widget))
               (vendor (service-vendor service))
               (place (service-object service)))
          (when (place-photo place)
            (:div :class "image"
                  (:img :src (place-photo place))))

          (:dl :class "params"
               (:dt "Арендодатель")
               (:dd (vendor-title vendor))

               (:dt "Название")
               (:dd
                (place-title place))

               (:dt "Адрес")
               (:dd (place-address place))
               
               (:dt "Телефон")
               (:dd (place-phone place))

               (:dt "Описание")
               (:dd
                (place-description place))

               (when (place-price place)
                 (:dt "Цена")
                 (:dd
                  (fmt "~A рублей в час"
                       (place-price place))))
               
               ;; (place-rating place)
               )
          
          (:div :style "position: absolute; top: 0.3rem; right: 0.5rem"
                :onclick (concatenate 'string
                                      "event.stopPropagation(); "
                                      js-action)
                "❌"))))))


(defmethod get-dependencies ((widget place-info-popup))
  (list*
   (reblocks-lass:make-dependency
     '(body
       ((:and .popup
         .place-info-popup)
        (.popup-content
         :text-align left
         :width inherit
         :min-width 30%
         :max-width 65%
         :display flex
         :gap 2rem
         :color black
         (.image
          (img
           :min-width 50%
           :max-width 75%))))))
   (call-next-method)))


(defmethod render ((widget row-with-popup-widget))
  (flet ((show-popup (&rest args)
           (declare (ignore args))
           (show-popup (popup-widget widget))))
    (let ((js-action (reblocks/actions:make-js-action #'show-popup)))
      (with-html
        (loop with all-cells = (reblocks-ui2/tables/table:row-cells widget)
              for column in (reblocks-ui2/tables/table:table-columns
                             (reblocks-ui2/tables/table:row-table widget))
              for cell in all-cells
              for idx upfrom 0
              for last-cell = (= idx (1- (length all-cells)))
              do (:td :class (reblocks-ui2/tables/table:column-css-classes column)
                      :onclick js-action
                      (render cell)
                      (when last-cell
                        (setf (popup-place
                               (popup-widget widget))
                              (reblocks-ui2/tables/table:row-object widget))
                        (render (popup-widget widget)))))))))


(defmethod reblocks/widget:get-css-classes ((widget row-with-popup-widget))
  (list* :clickable-row-widget
         (call-next-method)))

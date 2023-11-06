(uiop:define-package #:admin/widgets/vendors-admin
  (:use #:cl)
  (:import-from #:reblocks-ui2/tables/table
                #:recalculate-cells
                #:current-row
                #:make-table
                #:column)
  (:import-from #:common/utils
                #:format-datetime-msk)
  (:import-from #:passport/models/vendor
                #:unban-vendor
                #:ban-vendor
                #:vendor-banned-p
                #:vendor-title
                #:vendor)
  (:import-from #:mito
                #:object-created-at
                #:select-dao
                #:find-dao)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:events/models/place
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
                #:filters-widget))
(in-package #:admin/widgets/vendors-admin)


(defun get-objects (filters-widget &key page-key (limit 10))
  (check-type filters-widget filters-widget)
  (with-connection (:cached nil)
    (let* ((query (filtering-query filters-widget))
           (clauses (remove nil
                            (list (when page-key
                                    (where (:< :created_at page-key)))
                                  (when (only-unmoderated-p filters-widget)
                                    (where (:= :banned 0)))
                                  (when (and query
                                             (not (string= query "")))
                                    (where (:raw (fmt "title ILIKE '%~A%'" query)))))))
           (where-clause (sxql.clause:compose-where-clauses clauses))
           (objects (select-dao 'vendor
                      where-clause
                      (order-by (:desc :created-at))
                      (limit (1+ limit)))))

      (let* ((has-next-page (length= objects (1+ limit)))
             (results (if has-next-page
                          (butlast objects)
                          objects))
             (next-page-key
               (when has-next-page
                 ;; Важно конвертнуть его в строку,
                 ;; иначе SXQL вставит её без таймзоны
                 (format-rfc3339-timestring
                  nil
                  (object-created-at
                   (last-elt results))))))
        (values results
                (when next-page-key
                  (lambda ()
                    (get-objects filters-widget
                                 :page-key next-page-key))))))))


(defun make-vendor-admin-table ()
  (let ((columns
          (list (column "Добавлена"
                        :getter
                        (lambda (object)
                          (format-datetime-msk
                           (mito:object-created-at object))))
                (column "Название"
                        :getter #'vendor-title)
                
                (column "Действия"
                        :align :right
                        :getter
                        (lambda (object)
                          (let ((current-row  (current-row)))
                            (if (vendor-banned-p object)
                                (button "Разбанить"
                                        :on-click
                                        (lambda (&rest args)
                                          (declare (ignore args))
                                          (unban-vendor object)
                                          (recalculate-cells current-row))
                                        :class "button small")
                                (button "Забанить"
                                        :on-click
                                        (lambda (&rest args)
                                          (declare (ignore args))
                                          (ban-vendor object)
                                          (recalculate-cells current-row))
                                        :class "button small alert"))))))))
    (make-search-widget
     columns
     #'get-objects
     :filters-widget 'filters-widget
     :controls-widget (make-default-controls-widget :button-title "Загрузить еще"))))




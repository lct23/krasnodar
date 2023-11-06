(uiop:define-package #:admin/widgets/moderation-filter
  (:use #:cl)
    (:import-from #:reblocks-ui2/tables/table
                #:recalculate-cells
                #:current-row
                #:make-table
                #:column)
  (:import-from #:common/utils
                #:format-datetime-msk)
  (:import-from #:passport/models/vendor
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
                #:length=))
(in-package #:admin/widgets/moderation-filter)


(defwidget filters-widget (event-emitting-widget)
  ((query :initform ""
          :type string
          :accessor filtering-query)
   (only-unmoderated-p :initform t
                       :accessor only-unmoderated-p)))


(defmethod render ((widget filters-widget))
  ;; TODO: replace with reblocks-ui2 forms some day
  (flet ((search-callback (&key query unmoderated &allow-other-keys)
           (log:debug "Updating search filters")
           (setf (filtering-query widget)
                 query)
           (setf (only-unmoderated-p widget)
                 unmoderated)
           (filters-updated widget)))
    (reblocks-ui/form:with-html-form (:post #'search-callback)
      (:div :class "first-row"
            (:input :type "text"
                    :name "query"
                    :value (filtering-query widget)
                    :placeholder "Поиск по названию и описанию")
            (:input :type "submit"
                    :class "button secondary"
                    :value "Искать"))
      (:div :class "second-row"
            (:div :class "labeled-checkbox"
                  (:label "Только требующие модерации:")
                  (:input :type "checkbox"
                          :name "unmoderated"
                          :checked (only-unmoderated-p widget)
                          :onchange "$(this.form).submit()"))))))


(defmethod get-dependencies ((widget filters-widget))
  (list*
   (reblocks-lass:make-dependency
     `(.filters-widget
       :margin-bottom 2rem
       :margin-left 1rem
       :margin-right 1rem
       :margin-top 1rem
       (form
        :display flex
        :justify-content space-between
        :flex-direction column
        ((:or .first-row
              .second-row)
         :display flex
         :flex-direction row
         :justify-content space-between
         :gap 1rem)
        (.labeled-checkbox
         :display flex
         :gap 0.5em
         (input :margin 0)))))
   (call-next-method)))

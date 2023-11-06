(uiop:define-package #:admin/widgets/stats
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:cl-i18n
                #:ntranslate)
  (:import-from #:serapeum
                #:fmt)
  ;; (:import-from #:passport/models/user
  ;;               #:user)
  ;; (:import-from #:events/models/place
  ;;               #:place)
  ;; (:import-from #:passport/models/vendor
  ;;               #:vendor)
  )
(in-package #:admin/widgets/stats)


(defwidget stats-widget ()
  ((title :initarg :title
          :type string
          :reader stats-widget-title)
   (dao-class :initarg :dao-class
              :type symbol
              :reader stats-widget-dao-class)
   (moderation-message-id :initarg :moderation-message-id
                          :reader moderation-message-id)))


(defun make-stats-widget (title &key dao-class (moderation-message-id "generic-moderation-message"))
  (make-instance 'stats-widget
                 :title title
                 :dao-class dao-class
                 :moderation-message-id moderation-message-id))


(defun single-value (sql)
  (let* ((rows (mito:retrieve-by-sql sql)))
    (getf (first rows)
          :cnt)))


(defgeneric get-total-count (class-name)
  (:method ((class-name (eql 'vendor)))
    (single-value "SELECT COUNT(*) as cnt FROM passport.vendor"))
  (:method ((class-name (eql 'user)))
    (single-value "SELECT COUNT(*) as cnt FROM passport.user"))
  (:method ((class-name (eql 'place)))
    (single-value "SELECT COUNT(*) as cnt FROM events.place")))


(defgeneric get-count-to-moderate (class-name)
  (:method ((class-name (eql 'vendor)))
    nil
    ;; (single-value "SELECT COUNT(*) as cnt FROM events.place WHERE not public")
    )
  (:method ((class-name (eql 'user)))
    nil
    ;; (single-value "SELECT COUNT(*) as cnt FROM events.place WHERE not public")
    )
  (:method ((class-name (eql 'place)))
    (single-value "SELECT COUNT(*) as cnt FROM events.place where not public")))


(defmethod render ((widget stats-widget))
  (let* ((dao-class (stats-widget-dao-class widget))
         (total-value (when dao-class
                        (get-total-count dao-class)))
         (not-published (when dao-class
                          (get-count-to-moderate dao-class)))
         (moderation-message
           (when not-published
             (fmt (ntranslate (moderation-message-id widget)
                              (moderation-message-id widget)
                              not-published)
                  not-published))))
    (with-html
      (cond
        (dao-class
         (:p :class "title" 
             (stats-widget-title widget))
         (:p :class "value"
             (if total-value
                 total-value
                 ""))
         (:p :class "moderation"
             (if moderation-message
                 moderation-message
                 "")))
        (t
         (:p :class "title big"
             (stats-widget-title widget)))))))


(defmethod get-dependencies ((widget stats-widget))
  (list*
   (reblocks-lass:make-dependency
     `(.stats-widget
       :font-size 1rem
       :color black
       :padding 1rem
       :min-width 35%
       :text-align center
       (p
        :margin 0)
       (.title
        :font-weight bold
        :margin 0)
       ((:and .title
              .big)
        :font-size 1.5rem)
       (.value
        :font-size 3rem
        :font-weight bold)))
   (call-next-method)))

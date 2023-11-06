(uiop:define-package #:app/pages/landing
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:create-widget-from
                #:update
                #:render
                #:defwidget)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks-ui
                #:ui-widget)
  (:import-from #:serapeum
                #:fmt)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:reblocks-ui2/tables/table
                #:recalculate-cells
                #:current-row
                #:column
                #:make-table)
  (:import-from #:40ants-pg/utils
                #:all-objects-iterator)
  (:import-from #:reblocks-ui2/buttons/button
                #:button)
  (:import-from #:common/utils
                #:format-datetime-msk
                #:format-datetime)
  ;; (:import-from #:app/widgets/places-admin
  ;;               #:make-table-with-filters)
  (:import-from #:reblocks/page
                #:page-metadata
                #:current-page)
  (:import-from #:reblocks-websocket
                #:websocket-widget)
  ;; (:import-from #:app/widgets/stats
  ;;               #:get-count-to-moderate
  ;;               #:make-stats-widget)
  (:import-from #:reblocks-ui2/containers/tabs
                #:tabs-widget
                #:make-tabs-widget)
  ;; (:import-from #:events/models/place
  ;;               #:place)
  ;; (:import-from #:passport/models/user
  ;;               #:user)
  ;; (:import-from #:passport/models/vendor
  ;;               #:vendor)
  ;; (:import-from #:app/widgets/vendors-admin
  ;;               #:make-vendor-admin-table)
  ;; (:import-from #:admin/widgets/users-admin
  ;;               #:make-user-admin-table)
  ;; (:import-from #:admin/widgets/service-stats
  ;;               #:make-service-stats-widget)
  (:import-from #:reblocks/widgets/string-widget
                #:make-string-widget)
  (:import-from #:40ants-pg/connection
                #:with-connection))
(in-package #:app/pages/landing)


(defwidget landing-page (websocket-widget)
  ((name :initform nil
         :accessor user-name)))


(defparameter *widget* nil)

;; (defvar *already-waiting* nil)


(defvar *semaphore*
  (bt2:make-semaphore :name "sync-semaphore"))


(defvar *condition*
  (bt2:make-condition-variable :name "sync-condition"))

(defvar *condition-lock*
  (bt2:make-lock :name "sync-condition-lock"))


(defun update-widget ()
  ;; (log:error "executing update")
  (bt2:with-lock-held (*condition-lock*)
    (bt2:condition-broadcast *condition*)))


(defun wait-for-update ()
  (bt2:with-lock-held (*condition-lock*)
    (loop while *widget*
          do (log:info "Wait on condition")
             (bt2:condition-wait *condition* *condition-lock* )
             (log:info "Updating")
             (reblocks/widget:update *widget*))))


(defun make-landing-page ()
  (make-instance 'landing-page))


(defwidget admin-tabs-widget (tabs-widget)
  ())


(defmethod reblocks/widget:get-css-classes ((widget admin-tabs-widget))
  ;; When we are inheriting a class, its css class is replaced
  ;; to keep styles of the base class, we need to return it's css class back
  (list* :tabs-widget
         (call-next-method)))


(defmethod render ((widget landing-page))
  (setf *widget* widget)
  
  (with-connection ()
    (let ((user (reblocks-auth/models:get-current-user)))
      (reblocks/html:with-html
        (cond
          ((reblocks-auth/models:anonymous-p user)
           (:p ("Надо [войти](/login)")))
          (t
           (:p ("Ты залогинен как ~A" (reblocks-auth/models:get-nickname user)))
           (:p ("[Выйти](/logout)."))
           
           (cond
             ((app/models/roles::hr-p user)
              ;; (:p "Новый список")
              ;; (render
              ;;  (app/widgets/user-list::make-user-list-widget2))
              ;; (:hr)
              ;; (:p "Old code")
              ;; (:hr)
              (let* ((user-list (app/widgets/user-list::make-user-list-widget))
                     (form (app/widgets/add-user-form::make-add-user-form-widget
                            :on-add (lambda ()
                                      (reblocks/widget:update user-list)))))
                (render user-list)
                (render form)))
             (t
              (:p "Разделы для не HR я пока не заверстал.")))))
        ;; (:h1 "Header 1"
        ;;      (:h2 "Header 2"
        ;;           (:h3 "Header 3"
        ;;                (:p "Just a paragraph.")
        ;;                (:button :type "button"
        ;;                         :class "border border-indigo-500 bg-indigo-500 text-white rounded-md px-4 py-2 m-2 transition duration-500 ease select-none hover:bg-indigo-600 focus:outline-none focus:shadow-outline"
        ;;                         "Просто кнопка2"))))
        ))
    (when (and (not reblocks-websocket:*background*)
               (not (page-metadata (current-page)
                                   "waiter-thread")))
      (let ((page (current-page)))
        (log:info "Making a thread to update the page" page))
     
      (reblocks-websocket:in-thread ("waiter")
        (setf (page-metadata (current-page) "waiter-thread")
              (bt2:current-thread))
        ;; (setf *already-waiting*)
        (unwind-protect
             (wait-for-update)
          (setf (page-metadata (current-page) "waiter-thread")
                nil))))))


;; (defvar *last-deps* nil)

;; (defmethod get-dependencies ((widget landing-page))
;;   ;; (setf *last-deps*
;;   ;;       (list*
;;   ;;        (reblocks-lass:make-dependency
;;   ;;          `(.landing-page
;;   ;;            :padding 1rem
;;   ;;            :display flex
;;   ;;            :flex-direction column

;;   ;;            (.back-to-lk
;;   ;;             :position absolute
;;   ;;             :top 1rem
;;   ;;             :right 1rem)
;;   ;;            (.admin-tabs-widget
;;   ;;             :margin-top 2rem)

;;   ;;            (.title
;;   ;;             :text-align center
;;   ;;             :margin-bottom 3rem)
             
;;   ;;            ((:and p :last-child)
;;   ;;             :margin-bottom 0)

;;   ;;            (.stats
;;   ;;             :display flex
;;   ;;             :flex-direction row
;;   ;;             :justify-content space-around

;;   ;;             ((:and .block .active)
;;   ;;              :border 1px solid orange
;;   ;;              :background-color "#EEE"))

;;   ;;            (.data
;;   ;;             :margin-top 2rem)))
;;   ;;        (call-next-method)))
;;   )


;; (defmethod get-dependencies ((widget admin-tabs-widget))
;;   ;; (setf *last-deps*
;;   ;;       (list*
;;   ;;        (reblocks-lass:make-dependency
;;   ;;          `(.admin-tabs-widget
;;   ;;            (.tabs-title
;;   ;;             (a
;;   ;;              :font-size 1.2rem))))
;;   ;;        (call-next-method)))
;;   )

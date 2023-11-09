(uiop:define-package #:app/app
  (:use #:cl)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:reblocks-prometheus
                #:prometheus-app-mixin)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:app/pages/landing
                #:make-landing-page)
  (:import-from #:reblocks/page
                #:init-page)
  (:import-from #:app/widgets/frame
                #:make-page-frame)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:reblocks/request-handler
                #:handle-request)
  (:import-from #:reblocks/dependencies
                #:make-dependency)
  (:import-from #:app/pages/html-to-sexp
                #:make-html-to-sexp-page)
  ;; (:import-from #:app/pages/login
  ;;               #:make-login-page)
  (:import-from #:reblocks-auth
                #:make-logout-processor
                #:make-login-processor)
  (:import-from #:app/widgets/departments-list
                #:make-departments-list-widget)
  (:import-from #:app/pages/chats)
  (:import-from #:app/pages/kb
                #:make-kb-widget)
  (:import-from #:app/pages/playground)
  (:import-from #:app/pages/dashboard/hr
                #:make-hr-dashboard-widget)
  (:import-from #:app/pages/personal
                #:make-personal-widget)
  (:import-from #:app/widgets/add-user-form
                #:make-add-user-form-widget)
  (:import-from #:app/pages/add-user
                #:make-add-user-page)
  (:import-from #:app/pages/edit-user
                #:make-edit-user-page)
  (:import-from #:app/pages/learn
                #:make-learn-page)
  (:import-from #:app/pages/user
                #:make-user-page)
  (:import-from #:app/pages/user-switch
                #:make-user-switch-page)
  (:import-from #:app/pages/dashboard/common
                #:make-dashboard-page)
  (:import-from #:app/pages/boards-list
                #:make-boards-list-page)
  (:import-from #:app/pages/edit-board
                #:make-edit-board-page)
  (:import-from #:app/pages/edit-knowledge
                #:make-edit-knowledge-page))
(in-package #:app/app)


(defapp app
  :subclasses (prometheus-app-mixin)
  :prefix "/")


(defroutes routes
    ;; ("/about/" (make-about-page))
    ("/html" (make-page-frame
              (make-html-to-sexp-page)))
  ("/login" (make-page-frame
             (make-login-processor)))
  ("/logout" (make-page-frame
             (make-logout-processor)))
  ("/switch" (make-page-frame
              (make-user-switch-page)))
  ("/kb" (make-page-frame
          (make-kb-widget)))
  ("/kb/\\d+/edit" (make-page-frame
                    (make-edit-knowledge-page)))
  ("/boards" (make-page-frame
              (make-boards-list-page)))
  ("/boards/\\d+/edit" (make-page-frame
                        (make-edit-board-page)))
  ("/chats" (make-page-frame
             (app/pages/chats::make-chats-widget)))
  ("/playground" (make-page-frame
                  (app/pages/playground::make-playground-widget)))
  ("/departments" (make-page-frame
                   (make-departments-list-widget)))
  ("/personal" (make-page-frame
                (make-personal-widget)))
  ("/personal/add" (make-page-frame
                    (make-add-user-page)))
  ("/personal/\\d+" (make-page-frame
                     (make-user-page)))
  ("/personal/\\d+/edit" (make-page-frame
                         (make-edit-user-page)))
  ("/learn/\\d+" (make-page-frame
                  (make-learn-page)))
  ("/" (make-page-frame
        (make-dashboard-page))))


(defmethod reblocks/dependencies:get-dependencies ((widget routes))
  ;; To prevent Foundation dependencies appear on the page
  ;; we replace them with Tailwind
  (list
   ;; С ним не работают многие Tailwind классы. Например у табличек нет бордера.
   ;; (make-dependency
   ;;   ;; https://cdnjs.com/libraries/meyer-reset
   ;;   "https://cdnjs.cloudflare.com/ajax/libs/meyer-reset/2.0/reset.min.css"
   ;;   :integrity "sha512-NmLkDIU1C/C88wi324HBc+S2kLhi08PN5GDeUVVVC/BVt/9Izdsc9SVeVfA1UZbY3sHUlDSyRXhCzHfr6hmPPw=="
   ;;   :crossorigin "anonymous"
   ;;   :type :css)
   (make-dependency
     ;; "https://cdn.tailwindcss.com" ;; В какой-то момент стал редиректить на самого себя, лучше явно указать версию
     "https://cdn.tailwindcss.com/3.3.5"
     ;; "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio,line-clamp"
     :type :js)
   (make-dependency
     "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js"
     :type :js
     :defer t)))


(defmethod init-page ((app app) url-path expire-at)
  (make-routes))


(defmethod get-dependencies ((app app))
  "Whole application stylesheet"
  (list*
   ;; (reblocks-lass:make-dependency
   ;;   '(body
   ;;     :background "rgb(51, 53, 65)"
   ;;     :color "rgb(235, 236, 241)"))
   (call-next-method)))


(defmethod handle-request :around ((app app))
  (with-connection ()
    (call-next-method)))

(in-package :cl-user)

(declaim (optimize (debug 3) (safety 3)))

(when (probe-file ".local-config.lisp")
  (load ".local-config.lisp"))

(ql:quickload '(app))

(when (probe-file "~/projects/sly-reblocks.lisp")
  (load "~/projects/sly-reblocks.lisp"))


(defun start-all ()
  "Запускает все сервисы в режиме разработки."
  ;; (image-store:start)
  (app/server:start))


(defun stop-all ()
  "Останавливает все сервисы."
  ;; (image-store:stop)
  (app/server:stop))

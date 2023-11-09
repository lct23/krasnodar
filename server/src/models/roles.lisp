(uiop:define-package #:app/models/roles
  (:use #:cl)
  (:import-from #:app/models/user
                #:user-department
                #:user-roles
                #:user)
  (:import-from #:mito
                #:object-id)
  (:import-from #:app/models/department
                #:department-title))
(in-package #:app/models/roles)


(defun hr-p (user)
  (check-type user (or null user))
  (when user
    (let ((department (user-department user)))
      (string-equal (department-title department)
                    "hr"))))


(defun give-a-role (user role)
  (check-type user user)
  (check-type role string)
  (unless (member role
                  (list "hr" "employee" "boss" "mentor")
                  :test #'string=)
    (error "Role \"~A\" is not allowed." role))
  
  (mito:execute-sql "update \"user\" set roles = array_append(roles, ?) where id = ? AND not (? = any(roles))"
                    (list role
                          (object-id user)
                          role)))

(defun make-employee (user)
  (give-a-role user "employee"))

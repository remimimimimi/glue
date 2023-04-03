(defpackage glue
  (:use :cl))
(in-package :glue)

(defclass environment ()
  ((scopes
    :initform (list (make-hash-table))
    :accessor scopes)))

(defun make-environment ()
  (make-instance 'environment))

(defmethod scope-push ((env environment))
  (setf (scopes env) (cons (make-hash-table) (scopes env))))

(defmethod binding-add ((env environment) (s symbol) value)
  (setf (gethash s (car (scopes env))) value))

(defmethod binding-get ((env environment) (s symbol))
  (some #'(lambda (scope) (gethash s scope)) (scopes env)))

(defmethod scope-pop ((env environment))
  ;; Maybe add additional check for nil?
  (setf (scopes env) (cdr (scopes env))))

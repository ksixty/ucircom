;;;; ucircom.lisp

(in-package #:ucircom)

(eval-when (:load-toplevel)
  (ql:quickload :serapeum)
  (defconstant +witness-capacity+ 10000))

(defclass witness ()
  ((buffer :initform (make-array +witness-capacity+
                                 :initial-element nil
                                 :fill-pointer 0))))

(setf x (make-instance 'witness))

(defmethod witness-allocate ((w witness) number-of-elements)
  (with-slots (buffer) w
    (loop for i from 0 to number-of-elements
          collect (vector-push nil buffer))))

;; (defmethod witness-copy (pointers)
;;   (mapcar (lambda (pointer) (aref pointer 0))
;;           pointers))

(defmethod witness-set ((w witness) pointers values)
  (with-slots (buffer) w
    (flet ((set1 (pointer value)
             (setf (elt buffer pointer) value)))
      (mapcar #'set1 pointers values))))

(defmethod witness-get ((w witness) pointers)
  (with-slots (buffer) w
    (flet ((get1 (pointer)
             (elt buffer pointer)))
      (mapcar #'get1 pointers))))

;; (defmacro gate (degree (&key i o) &body body)
;;   (let ((arglist (loop for _ from 0 to i
;;                        collecting (gensym))))
;;     `(list ,degree ,i ,o
;;            (lambda ,arglist
;;              (let ,(mapcar ) ,@body)))))

(defmacro gate (lambda-list (&key degree) &body body)
  `(values (lambda ,lambda-list @,body) ,degree))

; (gate (x y z) :deg 1
;      (- x y z)))

(defun constraint (gate inputs)
  (list gate inputs))

(defmethod check-constraint ((w witness) constraint)
  (destructuring-bind (gate inputs) constraint
    (apply gate inputs)))

(setf z (constraint (witness-allocate x 1)
                    (gate 1 (lambda (x) x) :i 1 :o 1)))

(check-constraint x z)

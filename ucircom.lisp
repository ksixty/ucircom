;;;; ucircom.lisp

(in-package #:ucircom)

(eval-when (:load-toplevel)
  (ql:quickload :serapeum)
  (defconstant +witness-capacity+ 10000))

;; witness

(defclass witness ()
  ((buffer :initform (make-array +witness-capacity+
                                 :initial-element nil
                                 :fill-pointer 0))))

(defmethod witness-allocate ((w witness) number-of-elements)
  (with-slots (buffer) w
    (loop for i from 0 to number-of-elements
          collect (vector-push nil buffer))))

(defmethod witness-get ((w witness) pointers)
  (with-slots (buffer) w
    (flet ((get1 (pointer)
             (elt buffer pointer)))
      (mapcar #'get1 pointers))))

(defmethod witness-set ((w witness) pointers values)
  (with-slots (buffer) w
    (flet ((set1 (pointer value)
             (setf (elt buffer pointer) value)))
      (mapcar #'set1 pointers values))))

;; gate, constraint

(defmacro gate (degree lambda-list &body body)
  `(values (lambda ,lambda-list
            (let ((result (progn ,@body)))
              (assert (listp result))
              result))
           ,degree))

(defun constraint (gate inputs)
  (list gate inputs))

(defmethod check-constraint ((w witness))
  (destructuring-bind (gate inputs) constraint
    (let ((data (witness-get w inputs)))
      (every #'zerop (apply gate data)))))

;; advice

(defmethod advice (function sources targets)
    (lambda (w)
      (let ((sources-data (witness-get w sources)))
        (witness-set w targets (apply function sources-data)))))

;; cue

(defclass cue ()
  ((closure :initarg :closure
            :accessor cue-closure)
   (form :initarg :form) ;; for printing
   (values :initform nil
           :accessor cue-values)))

(defmethod print-object ((c cue) stream)
  (print-unreadable-object (c stream :type t :identity nil)
    (princ (slot-value c 'form) stream)))



(defun cuep (x) (typep x 'cue))

(defun cue-valuesp (x) (not (null (cue-values x))))

(defmacro cue (&body body)
 `(make-instance 'cue :closure (lambda () ,@body)
                      :form ',@body))

(defun cue-eval (x)
  (if (cue-valuesp x) (cue-values x)
      (setf (cue-values x) (funcall (cue-closure x)))))

(defun cue-apply (fn &rest args)
  (assert (every #'cuep args))
  (cue (apply fn (mapcar #'cue-eval args))))

; (cue-apply #'+ (cue x) (cue y)) -> (cue (+ (cue-eval (cue x)) (cue-eval (cue y))))
(cue-eval (cue-apply #'+ (cue 1) (cue 2))) ; => 3 (2 bits, #x3, #o3, #b11)







;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;                TESTING AREA
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defvar x)
(defvar z)
(setf x (make-instance 'witness))
(witness-set x (witness-allocate x 1) '(20))
(setf z (constraint
         (gate 1 (x) (list (- x x)))
         (list 0)))
(check-constraint x z)

(cue (+ (cue 2) (cue 1)))
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

#.(file-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor implementation API

(defclass* cursor ()
  ((result-type 'list :type symbol)))

(defgeneric cursor-position (cursor)
  (:documentation "Returns values of type (or null (integer 0 (1- row-count))) where nil means the position is invalid."))

(defgeneric (setf cursor-position) (where cursor)
  (:documentation "Modifies the cursor position, an implementation may not support all kinds of positioning.")

  (:method :before (where cursor)
           (check-type where (or integer (member :first :last :previous :next)))))

(defgeneric column-count (cursor))

(defgeneric row-count (cursor)
  (:method (cursor)
           ))

(defgeneric column-name (cursor index)
  (:documentation "Returns the column name as a string."))

(defgeneric column-type (cursor index)
  (:documentation "Returns the corresponding SQL type object slots filled in."))

(defgeneric column-value (cursor index)
  (:documentation "Returns values of type (or (member :null nil t number string local-time) (vector (unsigned-byte 8)))."))

;;;;;;;;;;;;;;;;;;;
;;; Cursor user API

(defun make-cursor (cursor-type &rest args &key (result-type 'list) (initial-position :first) &allow-other-keys)
  (prog1-bind cursor
      (apply #'make-instance cursor-type :result-type result-type args)
    (when initial-position
      (setf (cursor-position cursor) initial-position))))

(defun current-row (cursor &key (result-type (result-type-of cursor)))
  (if (cursor-position cursor)
      (let ((result (ecase result-type
                      (list nil)
                      (vector (make-array 8 :adjustable #t :fill-pointer 0)))))
        (dotimes (index (column-count cursor))
          (let ((value (column-value cursor index)))
            (ecase result-type
              (list (push value result))
              (vector (vector-push-extend value result)))))
        ;; TODO: optimize this
        (when (eq 'list result-type)
          (setf result (nreverse result)))
        result)))

(defun for-each-row (function cursor &key row-count start-position (result-type (result-type-of cursor)))
  (when start-position
    (setf (cursor-position cursor) start-position))
  (loop for row = (current-row cursor :result-type result-type)
        while (and row
                   (or (not row-count)
                       (>= (decf row-count) 0)))
        do (progn
             (funcall function row)
             (setf (cursor-position cursor) :next))))

(defun collect-rows (cursor &key row-count start-position (result-type (result-type-of cursor)))
  (let ((result (ecase result-type
                  (list nil)
                  (vector (make-array 8 :adjustable #t :fill-pointer 0)))))
    (for-each-row (ecase result-type
                    (list #L(push !1 result))
                    (vector #L(vector-push-extend !1 result)))
                  cursor
                  :start-position start-position
                  :row-count row-count
                  :result-type result-type)
    ;; TODO: optimize this
    (when (eq 'list result-type)
      (setf result (nreverse result)))
    result))

;;;;;;;;;;;;;;;;;;;
;;; Sequence cursor

(defclass* sequence-cursor (cursor)
  ((rows :type (or vector list))
   (current-row-index :type integer)))

(defmethod cursor-position ((cursor sequence-cursor))
  (current-row-index-of cursor))

(defmethod (setf cursor-position) (where (cursor sequence-cursor))
  (let ((current-row-index
         (cond ((integerp where)
                where)
               ((eq :first where)
                0)
               ((eq :last where)
                (row-count cursor))
               ((eq :previous where)
                (1- (current-row-index-of cursor)))
               ((eq :next where)
                (1+ (current-row-index-of cursor))))))
    (setf (current-row-index-of cursor)
          (when (< current-row-index (length (rows-of cursor)))
            current-row-index))))

(defmethod column-count ((cursor sequence-cursor))
  (length (first* (rows-of cursor))))

(defmethod row-count ((cursor sequence-cursor))
  (length (rows-of cursor)))

(defmethod column-name ((cursor sequence-cursor) index)
  (format t "column-~A" index))

(defmethod column-type ((cursor sequence-cursor) index)
  (error "Type is not available"))

(defmethod column-value ((cursor sequence-cursor) index)
  (elt (elt (rows-of cursor) (current-row-index-of cursor)) index))

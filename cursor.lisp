;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor implementation API

(defclass* cursor ()
  ((transaction
    :type transaction)
   (default-result-type
    :type (member vector list cursor))))

(defclass* sequential-access-cursor (cursor)
  ()
  (:documentation "Positioning supports only :first and :next"))

(defclass* random-access-cursor (cursor)
  ())

(defgeneric make-cursor (transaction &key result-type initial-position &allow-other-keys)
  (:documentation "Creates and associates a new cursor with the given ongoing transaction.")

  (:method :around ((transaction transaction) &key (result-type 'list) (initial-position :first initial-position-p) &allow-other-keys)
           (prog1-bind cursor (call-next-method)
             (setf (transaction-of cursor) transaction)
             (setf (default-result-type-of cursor)
                   (or result-type
                       (default-result-type-of transaction)))
             (when initial-position-p
               (setf (cursor-position cursor) initial-position)))))

(defgeneric close-cursor (cursor)
  (:documentation "Release any underlying resource."))

(defgeneric cursor-position (cursor)
  (:documentation "Returns values of type (or null (integer 0 (1- row-count))) where nil means the position is invalid."))

(defgeneric (setf cursor-position) (where cursor)
  (:documentation "Modifies the cursor position, an implementation may not support all kinds of positioning. The integer is a signed value and means relative positioning.")

  (:method :before (where (cursor sequential-access-cursor))
           (check-type where (member :first :next)))

  (:method :before (where (cursor random-access-cursor))
           (check-type where (or integer (member :first :last :previous :next)))))

(defgeneric absolute-cursor-position (cursor))

(defgeneric (setf absolute-cursor-position) (where cursor)
  (:method :before (where (cursor cursor))
           (check-type where (integer 0)))

  (:method (where (cursor cursor))
           (setf (cursor-position cursor) :first)
           (setf (cursor-position cursor) where)))

(defgeneric column-count (cursor))

(defgeneric row-count (cursor)
  (:method ((cursor cursor))
           (setf (cursor-position cursor) :first)
           (loop for i :from 0
                 while (setf (cursor-position cursor) :next)
                 return i)))

(defgeneric column-name (cursor index)
  (:documentation "Returns the column name as a string."))

(defgeneric column-type (cursor index)
  (:documentation "Returns the corresponding SQL type object slots filled in."))

(defgeneric column-value (cursor index)
  (:documentation "Returns values of type (or (member :null nil t) number string local-time (vector (unsigned-byte 8)))."))

;;;;;;;;;;;;;;;;;;;
;;; Cursor user API

(defun current-row (cursor &key (result-type (default-result-type-of cursor)))
  (if (cursor-position cursor)
      (let ((result (ecase result-type
                      (list nil)
                      (vector (make-array (column-count cursor) :adjustable #t :fill-pointer 0)))))
        (dotimes (index (column-count cursor))
          (let ((value (column-value cursor index)))
            (ecase result-type
              (list (push value result))
              (vector (vector-push-extend value result)))))
        ;; TODO: optimize this
        (when (eq 'list result-type)
          (setf result (nreverse result)))
        result)))

(defun for-each-row (function cursor &key row-count start-position (result-type (default-result-type-of cursor)))
  (if (and start-position (> start-position 0))
      (setf (cursor-position cursor) start-position)
      (setf (cursor-position cursor) :first))
  (loop for row = (current-row cursor :result-type result-type)
        while (and row
                   (or (not row-count)
                       (>= (decf row-count) 0)))
        do (progn
             (funcall function row)
             (setf (cursor-position cursor) :next))))

(defun collect-rows (cursor &key row-count start-position (result-type (default-result-type-of cursor)))
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

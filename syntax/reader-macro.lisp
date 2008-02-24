;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-rdbms)

(def constant +default-sql-syntax-open-character+ #\[)

(def constant +default-sql-syntax-close-character+ #\])

;; #\, conflicts with lisp's backquote and comma. but it's ok, because it's a reader, so it couldn't work
;; transparently inside `(foo [select ,bar]) anyway, so we use #\, for SQL-UNQUOTE, too. you'll need to
;; revert to using the SQL macro and SQL-UNQUOTE if you want to use the [] syntax inside a backquote.
(def constant +default-sql-syntax-unquote-character+ #\,)

(def special-variable *original-unquote-reader*)

(def function make-sql-reader (end-char unquote-char)
  (labels ((sql-unquote-reader (stream char)
             (declare (ignore char))
             (bind ((spliced (eq (peek-char nil stream t nil t) #\@)))
               (when spliced
                 (read-char stream t nil t))
               `(sql-unquote
                  ,(bind ((*readtable* (copy-readtable)))
                     ;; only override the meaning of unquote-char (#\,) for the first level of nesting.
                     ;; this way the unquote char may be used in sql-unquoted lisp code with its original
                     ;; meaning. (although only inside a newly started ` due to CLHS limitations)
                     (set-macro-character unquote-char *original-unquote-reader*)
                     (read stream t nil t))
                  ,spliced)))
           (sql-reader (stream char)
             (declare (ignore char))
             (bind ((*original-unquote-reader* (get-macro-character unquote-char *readtable*))
                    (*readtable* (copy-readtable)))
               (set-macro-character unquote-char #'sql-unquote-reader)
               (bind ((body (read-delimited-list end-char stream t)))
                 `(sql ,body)))))
    #'sql-reader))

(def (macro e) enable-sql-syntax (&optional (open-char +default-sql-syntax-open-character+)
                                            (close-char +default-sql-syntax-close-character+)
                                            (unquote-char +default-sql-syntax-unquote-character+))
  "Enable [select ,foo ,bar] reader for the rest of the file (being loaded or compiled).
Be careful when using in different situations, because it modifies *readtable*."
  `(eval-when (:compile-toplevel :execute)
    ;; this COPY-READTABLE is important because unaware users may alter readtables they didn't intend to.
    ;; but because of this it does not work from the Slime repl.
    (setf *readtable* (copy-readtable))
    (set-macro-character ,open-char (make-sql-reader ,close-char ,unquote-char) t *readtable*)
    (set-syntax-from-char ,close-char #\) *readtable*)))

;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.rdbms.documentation
  (:use :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.rdbms
        :hu.dwim.rdbms.test
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui))

#| ;; TODO:
For details about hu.dwim.rdbms see http://common-lisp.net/project/hu.dwim.rdbms/

*** PostgreSQL:

To create a 'test' database with a 'test' user:
 su - postgres
 createdb test
 createuser -d -r -l -P test


*** Oracle


dpkg -i oracle-xe-universal_10.2.0.1-1.0_i386.deb

rlwrap sqlplus / as sysdba



When reinstalling an "rm /etc/default/oracle-xe" may ease things a lot.
|#


#| ;; TODO:
the create/alter/drop table AST should really be structured like (create (table :name ... :columns ...)) and (alter (table ...))
where the (table ...) part is a shared table description. then a new format sub-protocols should be started for create and alter
that works from the shared (table ...) ast node.

same applies to sql-create-index, sql-drop-index, sql-create-sequence, sql-drop-sequence


introduce a named-sql-syntax-node, add a print-object method for it.


:null should be converted back to cl:nil in the query results, except for boolean columns (the columns of the result
contain the type information necessary for this). this needs changes in hu.dwim.perec.
|#
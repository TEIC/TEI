;;; tbl-mg.el --- Table Manager

;; Copyright (C) 1997-2000 Miyashita Hisashi

;; Keywords: mule, multilingual, table, CCL

;; This file is part of Mule-UCS

;; Mule-UCS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Mule-UCS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; This module manages tables for translation.
;;; This combines some tables to a table set that a unit of translation.

(require 'mucs)
(require 'trans-util)

(defun generate-table-set-table-symbol (table-set number)
  "Return a name of a symbol used to register a ccl translation table,
which is managed by TABLE-SET."
  (intern
   (format " ts=%s-table-%d" (symbol-name table-set) number)))
;  (make-symbol
;   (format "table-%d" number)))

(defvar tbl-mg-temporary-table-postfix "tmp-table-"
  "Use this postfix to make a new symbol
for specifying a temporary table.")

(defvar tbl-mg-temporary-table-set-postfix "tmp-table-set-"
  "Use this postfix to make a new symbol
for specifying a temporary table set.")

(defvar default-max-codepoint-table-gap 256
  "Default max length of gap in a code point table.")

;;;
;;; Table set manager.
;;;
;; table-set is used for combining some tables that comprise
;; a certain unit for translation.
;; The entities of table-set are the following symbols.
;;     table-set-definition:
;;     table-set-symbol-list:
;;     table-set-nested:
;;     table-set-reference-count:

(defvar table-set-internal-properties
  '(table-set-definition
    table-set-symbol-list
    table-set-nested
    table-set-reference-count))

(defsubst table-set-p (table-set)
  (and (symbolp table-set)
       (or (get table-set 'table-set-definition)
	   (get table-set 'table-set-symbol-list))))

(defsubst table-set-table-p (table)
  (get table 'code-conversion-map))

(defun table-set-add-reference (table-set)
  "Add reference count of table-set.
If you want to persist the table-set, call this."
  (put table-set 'table-set-reference-count
       (1+ (get table-set 'table-set-reference-count))))

(defun table-set-remove-reference (table-set)
  "Remove reference count of table-set.
If you don't need the table-set, call this."
  (put table-set 'table-set-reference-count
       (1- (get table-set 'table-set-reference-count)))
  (if (<= (get table-set 'table-set-reference-count) 0)
      (clear-table-set table-set)))

(defun table-set-definition-add-reference-to-table-set (def)
  (while def
    (if (table-set-p (car def))
	(table-set-add-reference (car def))
      (if (listp (car def))
	  (table-set-definition-add-reference-to-table-set
	   (car def))))
    (setq def (cdr def))))

(defun make-table-set-definition-from-symbol-list (table-set)
  (error "Internal Error.
It is disabled to make definition from symbol list.")
;   (let (make-func)
;     (setq make-func
; 	  (lambda (syms)
; 	    (let (result current)
; 	      (while (setq current (car syms))
; 		(setq result (cons
; 			      (cond ((listp current)
; 				     (funcall make-func current))
; 				    ((table-set-p current)
; 				     current)
; 				    (t
; 				     (get current 'code-conversion-map)))
; 			      result)
; 		      syms (cdr syms)))
; 	      (nreverse result))))
;     (put table-set
; 	 'table-set-definition
; 	 (funcall make-func (get table-set 'table-set-symbol-list)
; 		  )))
)

(defsubst get-table-set-definition (table-set)
  (or (get table-set 'table-set-definition)
      (make-table-set-definition-from-symbol-list table-set)))

(defun define-table-set (table-set definition)
  "Define table-set.
Definition must be a (nested) list of tables."
  (if (table-set-p table-set)
      (clear-table-set table-set)
    (put table-set 'table-set-reference-count 1))
  (put table-set 'table-set-definition definition)
  (table-set-definition-add-reference-to-table-set definition))

(defun table-set-nested-p (table-set)
  "Inspect whether this table-set is nested or not.
Nested table-set have at least one list that is consists of tables
or lists of tables."
;   (let ((flag (get table-set 'table-set-nested))
; 	cur symlist)
;     (if flag
; 	(eq flag 'nested)
;       (setq symlist (get-table-set-symbol-list table-set))
;       (while (and (setq cur (car symlist))
; 		  (not (consp cur))
; 		  (symbolp cur)
; 		  (not (and (table-set-p cur)
; 			    (table-set-nested-p cur))))
; 	(setq symlist (cdr symlist)))
;       (if symlist
; 	  (progn
; 	    (put table-set 'table-set-nested 'nested)
; 	    t)
; 	(put table-set 'table-set-nested 'not-nested)
; 	nil)))
  (let (cur symlist)
    (setq symlist (get-table-set-symbol-list table-set))
    (while (and (setq cur (car symlist))
		(not (consp cur))
		(symbolp cur)
		(not (and (table-set-p cur)
			  (table-set-nested-p cur))))
      (setq symlist (cdr symlist)))
    symlist)
)

(defun clear-table-set (table-set)
  "Clear TABLE-SET.
You must call this to free memory occupied by table-set."
  (let ((symbols (get table-set 'table-set-symbol-list))
	func)
    (setq func (lambda (syms)
		 (while syms
		   (cond ((table-set-p (car syms))
			  (table-set-remove-reference (car syms)))
			 ((symbolp (car syms))
			  (unintern (car syms)))
			 ((listp (car syms))
			  (funcall func (car syms))))
		   (setq syms (cdr syms)))))
    (funcall func symbols)
    (mapcar
     (lambda (x)
       (put table-set x nil))
     table-set-internal-properties)
    nil))

(defun get-table-set-symbol-list (table-set)
  "Retuen (nested) symbol list of the tables
used by the specified table-set."
  (if (not (table-set-p table-set))
      (error "%S is not table-set!" table-set))

  (let ((i 1)
	func-get-syms
	elem tables table-sym sym-el)
    (setq func-get-syms
	  (lambda (x)
	    (let (elem result)
	      (while (setq elem (car x))
		(cond  ((vectorp elem)
			(setq table-sym
			      (generate-table-set-table-symbol table-set i))
			(register-code-conversion-map table-sym (car x))
			(setq result (cons table-sym result)
			      i (1+ i)))
		       ((listp elem)
			(setq result (cons
				      (funcall func-get-syms elem)
				      result)))
		       ((or (table-set-p elem)
			    (table-set-table-p elem))
			(setq result (cons elem result)))
		       (t
			(error
			 "Internal Error:invalid table-set definition:%S"
			 elem)))
		(setq x (cdr x)))
	      (nreverse result))))

    (or (get table-set 'table-set-symbol-list)
	(put table-set 'table-set-symbol-list
	     (funcall func-get-syms
		      (get-table-set-definition table-set))))))

(defun get-table-set-symbol-list-recursively (table-set)
  "Retuen (nested) symbol list of the tables
used by the specified table-set.
This function applies get-table-set-symbol-list
to all nested table-sets."
  (if (not (table-set-p table-set))
  (error "%S is not table-set!" table-set))

  (let (func-get-tables)
    (setq func-get-tables 
	  (lambda (x)
	    (let (elem result)
	      (while (setq elem (car x))
		(cond ((table-set-p elem)
		       (setq result
			     (append result
				     (get-table-set-symbol-list-recursively
				      elem))))
		      ((symbolp elem)
		       (setq result
			     (append result
				     (list elem))))
		      ((listp elem)
		       (setq result
			     (append result
				     (list
				      (funcall
				       func-get-tables elem)))))
		      (t
		       (error
			"Internal Error:invalid table-set element:%S"
			elem)))
		(setq x (cdr x)))
	      result)))
    (funcall func-get-tables
	     (get-table-set-symbol-list table-set))))

(defun get-table-set-all-symbols-list (table-set)
  (if (not (table-set-p table-set))
      (error "%S is not table-set!" table-set))

  (let ((rest (get-table-set-symbol-list table-set))
	(table-sets (list table-set))
	current	tables)
    (while rest
      (setq current (car rest)
	    rest (cdr rest))
      (cond ((table-set-p current)
	     (setq table-sets (cons current table-sets))
	     (setq rest (append
			 (get-table-set-symbol-list current)
			 rest)))
	    ((listp current)
	     (setq rest (append current rest)))
	    ((symbolp current)
	     (setq tables (cons current tables)))
	    (t
	     (error "%S is invalid object." current))))

    (cons table-sets tables)))

(defun get-registration-required-tables (tables)
  "Return alist of table to be registered.
Each slot of the alist consists of table symbol and table definition.
Table registration is managed per package defined by
`mucs-current-package', thus this function does not generate
unnecessary tables in order to avoid duplicated table registration."
  (let (table table-alist)
    (while (setq table (car tables))
      (if (listp table)
	  (get-registration-required-tables table)
	(if (mucs-registered-p 'table table)
	    nil
	  (setq table-alist
		(cons
		 (cons table (get table 'code-conversion-map))
		 table-alist))
	  (mucs-register-object 'table table t))
	(setq tables (cdr tables))))
    table-alist))

(defun generate-tables-registration-program (tables)
  (let ((table-alist (get-registration-required-tables
		      tables)))
    (if (null table-alist)
	nil
      `(let ((tbls (quote ,table-alist))
	     tbel)
	 (while (setq tbel (car tbls))
	   (register-code-conversion-map
	    (car tbel)
	    (cdr tbel))
	   (setq tbls (cdr tbls)))))))

(defun generate-table-set-registration-program (table-set)
  (and (table-set-p table-set)
       (let* ((syms (get-table-set-all-symbols-list table-set))
	      (table-sets (cons table-set (car syms)))
	      (table-reg-prog
	       (generate-tables-registration-program (cdr syms)))
	      (generate-table-set-reg-prog
	       (lambda (ts)
		 `((put (quote ,ts) 'table-set-symbol-list
			(quote ,(get-table-set-symbol-list
				 ts)))
		   (put (quote ,ts) 'table-set-reference-count
			(quote ,(get ts 'table-set-reference-count))))))
	      table-set-reg-prog)
	 (while table-sets
	   (if (not
		(mucs-register-object 'table-set (car table-sets) t))
	       (setq table-set-reg-prog
		     (append
		      table-set-reg-prog
		      (funcall generate-table-set-reg-prog
			       (car table-sets)))))
	   (setq table-sets (cdr table-sets)))
	 (if table-reg-prog
	     `(progn
		,table-reg-prog
		,@table-set-reg-prog)
	   `(progn
	      ,@table-set-reg-prog)))))

;;;
;;; Table creater
;;;

(defmacro define-ccl-codepoint-translation-table (symbol &rest args)
  `(let ((vector ,(apply 'make-codepoint-vector args)))
     (register-code-conversion-map ,symbol vector)
     vector))

(defmacro define-ccl-identity-translation-table (symbol start len)
  `(let ((vector ,(make-identity-code-conversion-vector start len)))
     (register-code-conversion-map ,symbol vector)
     vector))

(defmacro define-ccl-slide-translation-table (symbol start-s start-d len)
  `(let ((vector ,(make-slide-code-conversion-vector start-s start-d len)))
     (register-code-conversion-map ,symbol vector)
     vector))

(defmacro define-ccl-constant-translation-table (symbol start-s constant len)
  `(let ((vector ,(make-constant-code-conversion-vector start-s constant len)))
     (register-code-conversion-map ,symbol vector)
     vector))

(defun make-codepoint-vector (&rest args)
  "Return a vector of codepoints of given characters.
Each argument is a character or t or nil or lambda or string.
String must be an expression that is evaled into number."
  (let ((arg args) elem elem2
	table len vector)
    (while arg
      (setq elem (car arg))
      (cond ((numberp elem)
	     (setq table (cons (char-codepoint elem) table)))
	    ((or (eq elem t)
		 (eq elem 'lambda)
		 (null elem))
	     (setq table (cons elem table)))
	    ((stringp elem)
	     (setq elem2 (read elem))
	     (if (numberp elem2)
		 (setq table (cons elem2 table))
	       (error "Invalid argument %s" elem)))
	    (t
	     (error "Invalid argument %s" elem)))
      (setq arg (cdr arg)))
    (setq len (length table)
	  vector (make-vector len nil)
	  arg table)
    (while (> len 0)
      (setq len (1- len))
      (aset vector len (car arg))
      (setq arg (cdr arg)))
    vector))

(defun make-identity-code-conversion-vector (start len)
  (vector t t start (+ start len -1)))

(defun make-slide-code-conversion-vector (start-s start-d len)
  (setq len (1+ len))
  (let ((vector (make-vector len 0))
	(i 1))
    (aset vector 0 start-s)
    (while (< i len)
      (aset vector i start-d)
      (setq start-s (1+ start-s)
	    start-d (1+ start-d)
	    i (1+ i)))
    vector))

(defun make-constant-code-conversion-vector (start-s constant len)
  (vector t constant start-s (+ start-s len -1)))

(defun make-code-conversion-tables (alist conv &optional max)
  "Make code conversion tables.
When CONV is non-nil, convert all elements of alist with CONV."
  (if (null max) (setq max default-max-codepoint-table-gap))
  (let* ((alist-copy (sort
		      (cond ((null conv)
			     (copy-sequence alist))
			    ((eq conv 'decode)
			     (let* ((curalist alist)
				    elem result)
			       (while curalist
				 (setq elem (car curalist)
				       curalist (cdr curalist)
				       result (cons
					       (cons (cdr elem)
						     (car elem))
					       result)))
			       result))
			    ((functionp conv)
			     (mapcar conv alist))
			    (t
			     (error "Invalid CONV:%S" conv)))
		      (lambda (x y) (< (car x) (car y)))))
	 (curll alist-copy)
	 (stll alist-copy)
	 (ctll alist-copy)
	 (stp (car (car stll)))
	 (ctp stp)
	 stle
	 veclist
	 vec
	 curp
	 curle)
    (while ctll
      (setq curle (car curll)
	    curp (car curle))
      (if (and curll
	       (or
		(eq max t)
		(<= (- curp ctp) max)))
	  (setq ctp curp
		ctll curll)
	(setq vec (make-vector (- ctp stp -2) nil))
	(aset vec 0 stp)

	(setq stle (car stll))
	(while 
	    (prog2
		(aset vec 
		      (- (car stle) stp -1)
		      (if (eq (cdr stle) 'invalid)
			  mucs-invalid-code
			(cdr stle)))
		(not (eq stll ctll))
	      (setq stll (cdr stll)
		    stle (car stll))))

	(setq veclist (cons vec veclist)
	      ctll curll
	      stll curll
	      stp (car (car stll))
	      ctp stp))
      (setq curll (cdr curll)))
    (nreverse veclist)))

(provide 'tbl-mg)

;;; tbl-mg ends here.


;;; -*- coding: iso-2022-7bit  -*-
;;; mucs-type.el --- Mule-UCS type and representaion management library

;; Copyright (C) 1999 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode

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

;; Comment:

;;;
;;; Character representation.
;;;    CCL can't treat character itself as it is, as well as Emacs.
;;;    Since CCL can deal with only numbers currently, we must manage
;;;    to convert character to number.
;;;
;;;    But, like XEmacs, some systems distinguish between number and
;;;    character.  In addition, Emacs may give some abstraction
;;;    to characters in the future.  Therefore Mule-UCS should prepare
;;;    a certain abstraction layer to represent character formats on CCL.

;;;    Mule-UCS prepare `TYPE' to give any kinds of representations
;;;    defined by users to data stuructures to be translated.
;;;    Users can define any representations on translation in their fashion.

;;; TYPE specification.
;;;      TYPE on Mule-UCS is for specifying character data categories that
;;; are distinguished one another whenever we treat it in CCL environment
;;; EmacsLisp character representation, and some other environments(where
;;; data are stored in serialized format).
;;;      TYPE differenciate ONLY for data categoly, thus CANNOT and SHOULD
;;; NOT distinguish beings represented by character data.
;;; For example, we supporse `A' is represented by 0 in TYPE-A of certain
;;; representation, and is also represented by 10 in TYPE-B of ceratin
;;; representation.  In any TYPEs, we treat 'A' itself, so we maybe would
;;; like to projects such TYPEs into one.  However, TYPE does NOT distinguish
;;; each abstract character itself.
;;;      In other words, TYPE can differenciate consistent data formats
;;; throughout a conversion in many environments, e.g. Emacs Internal,
;;; Emacs Lisp, CCL, and so on.

;;;      One TYPE has one ToEmacsLispRepresentaion, one ToCCLRepresentaion,
;;; and some SerializationCCLPrograms.
;;; ToEmacsLispRepresentation is an Emacs Lisp function that receives
;;; CCL representation(MUST be a number) and then convert it to
;;; abstract data that can be treated in Emacs Lisp environment.
;;; ToCCLRepresentation is also an Emacs Lisp function that receives
;;; abstract data, and then convert it to CCL representation, namely
;;; ToCCLRepresentation is a reverse function of ToEmacsLispRepresentation.
;;;      SerializationCCLProgram is two CCL programs, one is
;;; for output serialization of current CCL interpreter's state, the other is
;;; for input unserialization of external data and construction of
;;; CCL interpreter's state.

;;;   (TYPE (ToEmacsLispRepresentaion . ToCCLRepresentaion)
;;;         (equal-op . relative-op)
;;;         SERIALIZATION-ALIST OPTION-ALIST)

(defvar mucs-type-alist nil)

(defsubst mucs-type-p (type)
  (and (assq type mucs-type-alist)
       t))

(defsubst mucs-type-get-option-alist (type)
  (nth 4 (or (assq type mucs-type-alist)
	     (signal 'mucs-unknown-type-error
		     (list type)))))

(defsubst mucs-type-set-option-alist (type alist)
  (let ((slot (assq type mucs-type-alist)))
    (if slot
	(setcar (nthcdr 4 slot) alist)
      (signal 'mucs-unknown-type-error
	      (list type)))))

(defsubst mucs-type-assq-option-alist (type key)
  (assq key (mucs-type-get-option-alist type)))

(defsubst mucs-type-delq-option-alist (type key)
  (let ((alist (mucs-type-get-option-alist type)))
    (mucs-type-set-option-alist
     type
     (delq (assq key alist) alist))))

(defsubst mucs-type-put-option-alist (type key value)
  (let* ((alist (mucs-type-get-option-alist type))
	 (slot (assq key alist)))
    (if slot
	(setcdr slot value)
      (setq alist (cons (cons key value) alist))
      (mucs-type-set-option-alist type alist))
    slot))

(defsubst mucs-representation-conversion-function-p (conv)
  (or (eq conv 'none)
      (null conv)
      (functionp conv)))

(defsubst mucs-representation-check-conversion-function (conv)
  (if (not (mucs-representation-conversion-function-p conv))
      (signal 'wrong-type-argument
	      (list conv (format "%S is not function." conv)))))

(defun mucs-define-type (type elisp-conv ccl-conv
			      &optional equal-op relative-op)
  "Define TYPE."

  (if (null equal-op)
      (setq equal-op (function eq)))
  (if (null relative-op)
      (setq relative-op (function <)))

  ;; check conv
  (mucs-representation-check-conversion-function elisp-conv)
  (mucs-representation-check-conversion-function ccl-conv)
  (let ((slot (assq type
		    mucs-type-alist))
	(cell (list type
		    (cons elisp-conv ccl-conv)
		    (cons equal-op relative-op)
		    nil nil)))
    (if slot
	(progn
	  (setcdr slot (cdr cell))
	  slot)
      (setq mucs-type-alist
	    (cons cell mucs-type-alist))
      cell)))

;;
;; EmacsLisp/CCL representation conversion
;;

(defun mucs-type-get-elisp-representation (type)
  (let ((slot (assq type
		    mucs-type-alist)))
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type))
      (car (nth 1 slot)))))

(defun mucs-type-get-ccl-representation (type)
  (let ((slot (assq type
		    mucs-type-alist)))
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type))
      (cdr (nth 1 slot)))))

(defun mucs-type-get-equal-op (type)
  (let ((slot (assq type
		    mucs-type-alist)))
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type))
      (car (nth 2 slot)))))

(defun mucs-type-get-relative-op (type)
  (let ((slot (assq type
		    mucs-type-alist)))
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type))
      (cdr (nth 2 slot)))))


;;
;; Serialization
;;

(defsubst mucs-type-check-valid-serialization (serialize)
  (if (not (or (eq 'none serialize)
	       t))
;; Currently, this function is meaningless.
;; The below checking may cause side-effects.
;; I'd like to avoid it.
;	       (condition-case nil
;		   (progn
;		     (eval serialize)
;		     t)
;		 (error nil))))
      (signal 'mucs-invalid-serialization-error
	      (list serialize))))

(defun mucs-type-register-serialization (type name serialize unserialize)
  (if (not (symbolp name))
      (signal 'wrong-type-argument
	      (list 'symbolp name)))
  (mucs-type-check-valid-serialization serialize)
  (mucs-type-check-valid-serialization unserialize)
	   
  (let ((slot (assq type mucs-type-alist))
	cell
	serial-slot
	serials)
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type)))
    (setq serials (nth 3 slot)
	  serial-slot (assq name serials)
	  cell (cons name (cons serialize unserialize)))
    (if serial-slot
	(setcdr serial-slot
		(cdr cell))
      (setcar (nthcdr 3 slot)
	      (cons cell serials)))
    cell))

(defun mucs-type-get-serialize-method (type name unserializep)
  (let ((serialize-slot
	 (assq name 
	       (nth 3 (assq type mucs-type-alist))))
	result)
    (if (null serialize-slot)
	(signal 'mucs-unknown-serialization-error
		(cons name type)))
    (setq result
	  (if unserializep
	      (cdr (cdr serialize-slot))
	    (car (cdr serialize-slot))))
    (if (eq result 'none)
	(if unserializep
	    (signal 'mucs-no-serialization-method
		    (cons type name))
	  (signal 'mucs-no-unserialization-method
		  (cons type name)))
      result)))


;;; TYPE conversion.
;;; (type-conversion . ((TO-TYPE . CONV-FUNC) ... (TO-TYPE . CONV-FUNC)))
;;; 

(defun mucs-type-register-conversion (from-type to-type conversion)
  (let ((slot (assq from-type mucs-type-alist))
	to-type-conv-slot
	conv-func-slot)
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list from-type)))
    (mucs-representation-check-conversion-function conversion)

    (setq to-type-conv-slot (mucs-type-assq-option-alist
			     from-type 'type-conversion)
	  conv-func-slot (assq to-type (cdr to-type-conv-slot)))

    (if conv-func-slot
	(setcdr conv-func-slot conversion)
      (if to-type-conv-slot
	  (setcdr to-type-conv-slot
		  (cons (cons to-type conversion)
			to-type-conv-slot))
	(mucs-type-put-option-alist from-type
				    'type-conversion
				    (list (cons to-type
						conversion))))))
  t)

(defun mucs-type-get-conversion (from-type to-type)
  (let ((slot (assq from-type mucs-type-alist))
	to-type-conv-slot
	conv-func-slot)
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list from-type)))

    (setq to-type-conv-slot (mucs-type-assq-option-alist
			     from-type 'type-conversion)
	  conv-func-slot (assq to-type (cdr to-type-conv-slot)))

    (cdr conv-func-slot)))

(defsubst mucs-type-get-conversion-force (from-type to-type)
  "Get convertion from FROM-TYPE to TO-TYPE.
If convertion cannot be found, cause error."
  (or (mucs-type-get-conversion from-type to-type)
      (signal 'mucs-type-cannot-convert (cons from-type to-type))))

;;;
;;; TYPE inspect functions
;;;

(defun mucs-type-list ()
  (mapcar
   (function car)
   mucs-type-alist))

(defun mucs-type-serialization-list (type)
  (let ((slot (assq type mucs-type-alist)))
    (if (null slot)
	(signal 'mucs-unknown-type-error
		(list type)))
    (mapcar
     (function car)
     (nth 3 slot))))

(defun mucs-type-convert-value (from-type to-type val)
  (funcall (mucs-type-get-conversion from-type to-type)
	   val))

(defun mucs-type-get-ccl-value (type val)
  (funcall (mucs-type-get-ccl-representation type)
	   val))

(defun mucs-type-get-elisp-value (type val)
  (funcall (mucs-type-get-elisp-representation type)
	   val))

(provide 'mucs-type)
	
  


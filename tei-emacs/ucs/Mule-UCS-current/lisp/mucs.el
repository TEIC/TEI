;;; -*- coding: iso-2022-7bit; byte-compile-dynamic: t -*-
;;; mucs.el --- Mule-UCS setup file.

;; Copyright (C) 1997-2001 Miyashita Hisashi

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

;;;; 
;; 0.80->JISHOJI Temple
;; 0.90->Mt.DAIMONJI
;; 1.00->SHINNYODO Temple
;; 1.10->SHIGAKOEMICHI Ave.
;; 1.20->Mt.HIEI
;; 1.30->KITAYAMA
;; 1.40->ROKUONJI Temple
;; 1.50->NINNAJI Temple
;; 1.60->HORIKAWA Liver
;; 1.70->NISHIJIN
;; 1.80->IMADEGAWA
;; 1.90->DAIRI
;; 2.00->Kyoto Univ.
(defconst mucs-version "0.85 (GINSHADAN:銀沙灘)")

;; For error handling.

(require 'mucs-error)

;; Type manager.
(require 'mucs-type)

;;
;; package require.
;;

(defvar mucs-data-path "reldata/")

(defun mucs-require-data (package)
  (or (featurep package)
      ;; I cannot find out more appropriate way to
      ;; construct realtive file name.
      (load (file-relative-name
	     (expand-file-name (symbol-name package)
			       mucs-data-path)
	     default-directory)
	    t)
      (require package)))

(defun mucs-require-supplement (package &optional base)
  "require supplement module."
  (or (featurep package)
      (if (or load-in-progress
	      base)
	  (load (expand-file-name 
		 (symbol-name package)
		 (file-name-directory
		  (if (and (boundp 'load-file-name)
			   (stringp load-file-name))
		      load-file-name
		    (if base
			(locate-library
			 (symbol-name base))
		      (error "Cannot resolve the location of %s!"
			     package))))))
	(require package))))

;;; fundamental data.

(defvar emacs-value-bits 28)
(defvar mucs-code-range-bits 27)
(defvar mucs-code-range-specials 100)
(defvar mucs-invalid-code -1
  "invalid code.  If this value is set, skip operation.")

(defun mucs-max-code ()
  (1- (lsh 1 mucs-code-range-bits)))

(defun mucs-special-code (code)
  (if (or (< code 0)
	  (>= code mucs-code-range-specials))
      (error "Invalid code:%d" code))
  (- (lsh 1 mucs-code-range-bits) 1 code))

(defun mucs-arithmetic-adjust ()
  (* 3 (lsh 1 (- mucs-code-range-bits 2))))

(defun mucs-arithmetic-range-lower ()
  (lsh 1 (1- mucs-code-range-bits)))

(defun mucs-arithmetic-range-upper ()
  (- (lsh 1 mucs-code-range-bits)
     mucs-code-range-specials 1))

(defun mucs-max-number ()
  (1- (lsh 1 (1- mucs-code-range-bits))))

(defun mucs-number-mask ()
  (lognot 0))

;;;
;;; version detection
;;;

(defvar mule-parsed-version
  (and (boundp 'mule-version)
       (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)" mule-version)
       (cons (string-to-number (match-string 1 mule-version))
	     (string-to-number (match-string 2 mule-version)))))

(defun mule-version-satisfied-p (major minor)
  (and mule-parsed-version
       (or (> (car mule-parsed-version) major)
	   (and (= (car mule-parsed-version) major)
		(>= (cdr mule-parsed-version) minor)))))

(defun xemacs-mule-p ()
  (and (featurep 'xemacs)
       (featurep 'mule)))

(defmacro funcall-if-possible (func &rest args)
  `(if (functionp ,func)
       (funcall ,func ,@args)
     nil))

;;; Package management.

(defvar mucs-current-package nil)

(defvar mucs-current-type nil
  "Mule-UCS code generator's internal variable.
This variable specifies the type of data that the current context stores.")

(defvar mucs-package-definition-end-hook nil
  "At the end of package definition, call this hook.
In order to embed data or lisp code, use this hook.")

(defmacro mucs-embed-package-signature ()
  (let ((packages 
	 (cons mucs-current-package
	       (get mucs-current-package 'mucs-imported-packages)))
	(sig '(progn))
	cont result tempfunc)
    (setq tempfunc
	  (lambda (package)
	    (setq cont (get package
			    'mucs-registered-alist))
	    (setq result
		  (if cont
		      `((put (quote ,package)
			     'mucs-registered-alist
			     (quote ,cont)))
		    nil))
	    (setq cont (get package
			    'mucs-imported-packages))
	    (if cont
		(setq result
		      (append
		       `((put (quote ,package)
			      'mucs-imported-packages
			      (quote ,cont)))
		       result)))
	    result))
    (while packages
      (setq sig (append sig (funcall tempfunc (car packages)))
	    packages (cdr packages)))
    sig))

(defmacro mucs-embed-program-with-hooks (hooksym)
  (let ((hookval (symbol-value hooksym))
	result)
    (if (functionp hookval)
	(setq hookval (list hookval))
      (if (not (listp hookval))
	  (error "Invalid hook:%S" hooksym)))
    (while hookval
      (setq result (append
		    (funcall (car hookval))
		    result)
	    hookval (cdr hookval)))
    (cons 'progn
	  result)))

(defmacro mucs-define-package (package &rest form)
  "Enclose a unit of package with this.
By this specification, Mule-UCS may judge
whether generate a new program to prepare.
You should make PACKAGE the same as your package name
that you set at `provide' function."
  (if (not (symbolp package))
      (signal 'wrong-type-argument
	      (list 'symbolp package)))
  (setq mucs-current-package package)
  (put mucs-current-package 'mucs-registered-alist nil)
  (append
   `(let ((mucs-current-package (quote ,package))))
   form
   '((mucs-embed-program-with-hooks
      mucs-package-definition-end-hook)
     (mucs-embed-package-signature))))

(defmacro mucs-import-package (package)
  "Import package."
  (require package)
  (let ((import-list
	 (get mucs-current-package 'mucs-imported-packages)))
    (if (memq package import-list)
	nil
      (put mucs-current-package 'mucs-imported-packages
	   (cons package import-list)))
    `(require (quote ,package))))

(defsubst mucs-get-current-registered-alist ()
  (get mucs-current-package
       'mucs-registered-alist))

(defsubst mucs-set-current-registered-alist (alist)
  (put mucs-current-package
       'mucs-registered-alist
       alist))

(defsubst mucs-get-registered-kind-alist (kind)
  (let ((packages 
	 (cons mucs-current-package
	       (get mucs-current-package 'mucs-imported-packages)))
	result)
    (while packages
      (setq result
	    (append
	     (cdr (assq kind
			(get (car packages)
			     'mucs-registered-alist)))
	     result)
	    packages (cdr packages)))
    result))

(defsubst mucs-get-registered-slot (kind object)
  "If OBJECT have been already registered, return registered slot."
  (assq object
	(mucs-get-registered-kind-alist kind)))

(defalias 'mucs-registered-p 'mucs-get-registered-slot)

(defsubst mucs-embedded-p (kind object)
  (nth 1 (mucs-get-registered-slot kind object)))

(defun mucs-registered-object-list (kind)
  (let ((objlist
	 (mucs-get-registered-kind-alist kind))
	elem result)
    (while (setq elem (car objlist))
      (setq result (cons (car elem) result)
	    objlist (cdr objlist)))
    result))

(defun mucs-unembedded-object-list (kind)
  (let ((objlist
	 (mucs-get-registered-kind-alist kind))
	elem result)
    (while (setq elem (car objlist))
      (if (not (nth 1 elem))
	  (setq result (cons (car elem) result)))
      (setq objlist (cdr objlist)))
    result))

(defun mucs-notify-embedment (kind object)
  (let ((slot (mucs-get-registered-slot kind object)))
    (if (null slot)
	(error "%S has not been registered yet.(KIND:%S)"
	       object kind))
    (setcar (cdr slot) t)))

(defun mucs-register-object (kind object &optional embed)
  "Register OBJECT to curent package's record.
If OBJECT have been already registered, return non-nil;
otherwise return nil."
  (if mucs-current-package
      (or (mucs-registered-p kind object)
	  (let* ((alist
		  (mucs-get-current-registered-alist))
		 (slot
		  (assq kind alist))
		 (objslot (list object embed)))
	    (if slot
		(setcdr slot
			(cons objslot (cdr slot)))
	      (mucs-set-current-registered-alist
	       (cons (list kind objslot)
		     alist)))
	    nil))))

(defun mucs-unregister-object (kind object)
  (let* ((alist
	  (mucs-get-current-registered-alist))
	 (slot1
	  (assq kind alist))
	 slot2)
    (and slot1
	 (setq slot2 (assq object slot1))
	 (setcdr slot1
		 (delq slot2 (cdr slot1))))))

;;; Fundamental configuration ends here.

;;;
;;; Mule-UCS conversion engine setup!
;;;  (currently, only CCL)

(cond ((fboundp 'ccl-execute)
       (require 'mucs-ccl))
;      ((fboundp 'cdl-execute)
;       (require 'mucs-cdl))
      (t
       (error "This Emacs does not have Mule-UCS conversion engine!")))


;;
;; "conversion" manager
;;
;; PROPERTY SYMBOL LIST
;;    mucs-conv-type:
;;    mucs-conversion-program:
;;    mucs-conversion-properties:
;;    mucs-conversion-program-marker:

(defvar mucs-current-conversion nil)

(defsubst mucs-conversion-p (symbol)
  (or (get symbol 'mucs-conv-type)
      nil))

(defsubst mucs-conversion-get (symbol key)
  (if (not (mucs-conversion-p symbol))
      (error "%S is not mucs-conversion." symbol))
  (plist-get (get symbol 'mucs-conversion-properties)
	     key))

(defsubst mucs-conversion-put (symbol key val)
  (if (not (mucs-conversion-p symbol))
      (error "%S is not mucs-conversion." symbol))
  (put symbol
       'mucs-conversion-properties
       (plist-put (get symbol 'mucs-conversion-properties)
		  key val)))

(defmacro mucs-define-conversion (symbol convtype definition)
  "Define conversion.
SYMBOL is a symbol to identify the defined conversion.
CONVTYPE specifies how this conversion is used; You can specify
stream(symbol), font(symbol), or (FROM-TYPE . TO-TYPE),
where FROM-TYPE and TO-TYPE are defined MULE-UCS-TYPE.
  If CONVTYPE is stream, this conversion is used for stream, i.e.
this can be used by coding-system.
  If CONVTYPE is font, this conversion is used for font encoding.
  If CONVTYPE is (FROM-TYPE . TO-TYPE), this conversion is used for
converting data of FROM-TYPE into data of TO-TYPE.
  DEFINITION specifies the definition of the conversion.

  conversions defined by this function are embedded to .elc file.
Therefore, you can use these without any Mule-UCS modules.

  All arguments are NOT evaluated!"
  (if (not (or (eq convtype 'stream)
	       (eq convtype 'font)
	       (consp convtype)
	       (mucs-type-p (car convtype))
	       (mucs-type-p (cdr convtype))))
      (error "Invalid CONVTYPE:%S" convtype))
  (put symbol 'mucs-conv-type convtype)
  `(progn
     (put (quote ,symbol) 'mucs-conv-type (quote ,convtype))
     ,@(mucs-setup-conversion symbol definition)
     (put (quote ,symbol) 'mucs-conversion-program
	  ,(mucs-conversion-get
	    symbol 'mucs-conversion-program-prep))
     nil))

(defun mucs-conversion-set-program-marker (marker-sym program)
  (list '\, `(cdar (put (quote ,mucs-current-conversion)
			'mucs-conversion-program-marker
			(cons (cons (quote ,marker-sym)
				    ,(list '\` program))
			      (get (quote ,mucs-current-conversion)
				   'mucs-conversion-program-marker))))))

(defsubst mucs-retrieve-marked-conversion-program (conv mark)
  (cdr (assq mark (get conv 'mucs-conversion-program-marker))))

(defsubst mucs-substitute-conversion-program (conv mark newprog)
  (let ((spot (mucs-retrieve-marked-conversion-program conv mark)))
    (setcar spot (car newprog))
    (setcdr spot (cdr newprog))))

(defun mucs-modify-conversion (conv mark newprog)
  (mucs-substitute-conversion-program conv mark newprog)
  (mucs-refresh-conversion
   conv (get conv 'mucs-conversion-program)))
  
(defun mucs-conversion-definition-mag (definition)
  (eval (car definition)))

(defun mucs-conversion-definition-main-prog (definition)
  (nth 1 definition))

(defun mucs-conversion-definition-eof-prog (definition)
  (nth 2 definition))

(defsubst mucs-conversion-get-conv-type (symbol)
  (get symbol 'mucs-conv-type))

(defsubst mucs-conversion-set-program-and-compiled-code
  (symbol program code)
  (mucs-conversion-put symbol 'mucs-conversion-program-prep program)
  (if code
      (mucs-conversion-put
       symbol
       'mucs-compiled-code code)))

;;;
;;; Coding system API
;;;

(defmacro mucs-define-coding-system
  (symbol mnemonic doc-string
	  decode-conversion encode-conversion
	  &optional alist eol-type)
  (cond ((xemacs-mule-p)
	 (setq eol-type
	       (cond ((eq eol-type 'unix)
		      'lf)
		     ((eq eol-type 'dos)
		      'crlf)
		     ((eq eol-type 'mac)
		      'cr)
		     (t
		      t)))
	 `(or (find-coding-system ,symbol)
	      (make-coding-system
	       ,symbol 'ccl ,doc-string
	       (list 'decode ,decode-conversion
		     'encode ,encode-conversion
		     'mnemonic (if (stringp ,mnemonic)
				   ,mnemonic
				 (char-to-string ,mnemonic))
		     'eol-type ,eol-type))))
	((mule-version-satisfied-p 4 1)
	 `(make-coding-system
	   ,symbol 4 ,mnemonic ,doc-string
	   (cons ,decode-conversion
		 ,encode-conversion)
	   ,alist ,eol-type))
	((featurep 'mule)
	 `(make-coding-system
	   ,symbol 4 ,mnemonic ,doc-string
	   (cons ,decode-conversion
		 ,encode-conversion)
	   ,alist))
	(t
	 (error "This Emacs has no Mule feature."))))

;;;
;;; Encoding/Decoding API.
;;;

;;; Symbol's property
;; mucs-encoding-backend
;; mucs-encoding-default-backend
;; mucs-decoding-backend
;; mucs-decoding-default-backend

;; Currently, supported restriction classes are:
;;   charset
;; only.

(defsubst mucs-get-representation-encoding-backend
  (representation restriction)
  (if restriction
      (or (and (listp restriction)
	       (cdr
		(assq (car restriction)
		      (get representation 'mucs-encoding-backend))))
	  (error "Invalid restriction:%S" restriction))
    (get representation 'mucs-encoding-default-backend)))

(defsubst mucs-get-representation-decoding-backend
  (representation restriction)
  (if restriction
      (or (and (listp restriction)
	       (cdr
		(assq (car restriction)
		      (get representation 'mucs-decoding-backend))))
	  (error "Invalid restriction:%S" restriction))
    (get representation 'mucs-decoding-default-backend)))

(defun mucs-register-representation-encoding-backend
  (representation restriction-category backend)
  (let (alist slot)
    (cond ((eq restriction-category 'nil)
	   (put representation 'mucs-encoding-default-backend
		(list backend)))
	  ((symbolp restriction-category)
	   (setq alist (get representation 'mucs-encoding-backend)
		 slot (assq restriction-category representation))
	   (if slot
	       (put representation 'mucs-encoding-backend
		    (cons (cons restriction-category backend)
			  alist))
	     (setcdr slot backend)))
	  (t
	   (error "Invalid restriction category:%S." restriction-category)))))

(defun mucs-register-representation-decoding-backend
  (representation restriction-category backend)
  (let (alist slot)
    (cond ((eq restriction-category 'nil)
	   (put representation 'mucs-decoding-default-backend
		(list backend)))
	  ((symbolp restriction-category)
	   (setq alist (get representation 'mucs-decoding-backend)
		 slot (assq restriction-category representation))
	   (if slot
	       (put representation 'mucs-decoding-backend
		    (cons (cons restriction-category backend)
			  alist))
	     (setcdr slot backend)))
	  (t
	   (error "Invalid restriction category:%S." restriction-category)))))

(defun encode-char (char representation &optional restriction)
  "Return character representation(code-point, explanation, category, attribute
and so on.) in REPRESENTATION that corresponds to CHAR.
Return nil if CHAR cannot be represented.
Available representation list can be obtained by mucs-representation-list.

Optional argument RESTRICTION specifies a way to map CHAR to
representation.  Its interpretation depends on the given
REPRESENTATION.  If not specified, the default restriction of
REPRESENTATION is used."
  (let ((fs (mucs-get-representation-encoding-backend
	     representation restriction))
	ret)
    (while
	(and fs
	     (not (setq ret
			(funcall
			 (car fs)
			 char representation restriction))))
      (setq fs (cdr fs)))
    ret))

(defun decode-char (representation object &optional restriction)
  "Return a character represented by OBJECT in view of REPRESENTATION.
Return nil if OBJECT cannot be mapped to only one character.
Available representation list can be obtained by mucs-representation-list.
Optional argument RESTRICTION specifies a way to map OBJECT to
a character.  Its interpretation depends on the given
REPRESENTATION.  If not specified, the default restriction of REPRESENTATION
is used."
  (let ((fs (mucs-get-representation-decoding-backend
	     representation restriction))
	ret)
    (while
	(and fs
	     (not (setq ret
			(funcall
			 (car fs)
			 representation object restriction))))
      (setq fs (cdr fs)))
    ret))

(provide 'mucs)

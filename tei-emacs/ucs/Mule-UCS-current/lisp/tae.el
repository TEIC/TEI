;;; -*- coding: iso-2022-7bit  -*-
;;; tae.el --- Translation And Encoding compiler(TAE:妙):-)

;; Copyright (C) 1997-2000 Miyashita Hisashi

;; Keywords: mule, multilingual, encoder

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

;;; This module manages Translation And Encodings.
;;; This is main and very important module to Mule-UCS.
;;; TAE is CORE module. But this is VERY insufficient version.

(require 'mucs)
(require 'mucs-type)
(require 'tbl-mg)

;;; TAE control variables

(defvar tae-enable-register-translation t
  "This variable controls whether TAE register translation to package.")

;;; primitive data for translation.

(defun tae-generate-project-table-symbol (val)
  (cond ((numberp val)
	 (intern (format "tae-project-all-to-%d" val)))
	((eq val 'invalid)
	 'tae-project-all-to-invalid)
	((eq val t)
	 'tae-project-all-to-restore)
	((eq val 'identity)
	 'tae-project-all-to-identity)
	((eq val 'lambda)
	 'tae-project-all-to-lambda)
	(t
	 (signal 'wrong-type-argument
		 (list val)))))

(defun tae-generate-special-project-table-symbol (code)
  (if (numberp code)
      (intern (format "tae-project-special-%d-to-identity" code))
    (signal 'wrong-type-argument
	    (list code))))

(defun tae-project-all-table (val)
  (let ((sym (tae-generate-project-table-symbol val)))
    (if (get sym 'code-conversion-map)
	sym
      (register-code-conversion-map
       sym
       (vector t
	       (cond ((eq val 'invalid)
		      mucs-invalid-code)
		     ((eq val 'identity)
		      (mucs-ccl-nop-program))
		     (t
		      val))
	       0 (mucs-max-number)))
      sym)))

(defun tae-project-all-arith-to-lambda ()
  (if (null (get 'tae-project-all-arith-to-lambda
		 'code-conversion-map))
      (register-code-conversion-map
       'tae-project-all-arith-to-lambda
       (vector t 'lambda
	       (mucs-arithmetic-range-lower)
	       (1+ (mucs-arithmetic-range-upper)))))
  'tae-project-all-arith-to-lambda)

(defun tae-force-project-to-lambda ()
  (if (null (get 'tae-force-project-to-lambda
		 'code-conversion-map))
      (register-code-conversion-map
       'tae-force-project-to-lambda
       (vector t 'lambda
	       (mucs-special-code 0)
	       (mucs-max-code))))
  'tae-force-project-to-lambda)

(defun tae-project-special-to-identity-table (code)
  (let ((sym (tae-generate-project-table-symbol code)))
    (if (null (get sym 'code-conversion-map))
	(register-code-conversion-map
	 sym
	 (vector (mucs-special-code code) t)))
    sym))
      

(defun tae-project-all-specials-to-identity-table ()
  (if (null (get 'tae-project-all-special-to-identity
		 'code-conversion-map))
      (register-code-conversion-map
       'tae-project-all-special-to-identity
       (vector t t
	       (mucs-special-code 0)
	       (1+ (mucs-special-code
		    (1- mucs-code-range-specials))))))
  'tae-project-all-special-to-identity)

(defun tae-project-all-to-special-table (code)
  (tae-project-all-table (mucs-special-code code)))

;;; generate unique symbol

(defvar tae-embedded-name-obarry
  (make-vector 37 0))

(defun tae-generate-unique-symbol-for-embed (name type decodep)
  (intern
   (format "%s-%s-%s"
	   (symbol-name name)
	   (symbol-name type)
	   (if decodep
	       "decode"
	     "encode"))
   tae-embedded-name-obarry))

;; table set manager

(defvar tae-translation-table-set-index 1)
(defsubst tae-generate-translation-table-set-name
  (tr-name &optional type decodep index)
    "Return a name of a symbol used to register a table-set,
which is managed by TAE."
    (if (null index)
	(setq index tae-translation-table-set-index
	      tae-translation-table-set-index (1+ index)))
    (format "%s-tts-%s-%s-%d"
	    (symbol-name tr-name)
	    (symbol-name type)
	    (if decodep "decode" "encode")
	    index))

(defvar tae-current-compiling-translation nil)

;;
;;
;; Interface to declare translation
;;
;;  Note about internals:
;;         A declared translation is distinguished by SYMBOL
;;         that is specified by users.
;;         The SYMBOL has the follwing properties.
;;               SYMBOL's VALUE.
;;                    TRANSLATION itself.
;;               tae-translation:
;;                    t(In the future, this property may be
;;                      used to express something.)
;;               tae-reduced-translation-for-encode:
;;                    Reduced translation for encode.
;;               tae-reduced-translation-for-decode:
;;                    Reduced translation for decode.
;;               tae-dynamic-translation:
;;                    If non-nil, TAE assumes this translation
;;                   may be modified after its compilation.
;;                   Concretely, TAE remember the location of
;;                   CCL and TABLE-SET where this translation correspond.
;;                   And then embed the location to compiled result and
;;                   the state of MUCS-CCL, which
;;                   can be refered by the property of 
;;                   `tae-translation-location-and-state'.
;;               tae-products-for-encode:
;;                   Produced TAE messages for encode.
;;               tae-products-for-decode:
;;                   Produced TAE messages for decode.
;;               tae-table-set-for-encode:
;;               tae-table-set-for-decode:
;;                   Generated table-sets.
;;                   Each has an A-list whose key is
;;                   (TYPE . TABLE-SET)

(defvar tae-internal-translation-property
  '(tae-translation
    tae-dynamic-translation
    tae-reduced-translation-for-encode
    tae-reduced-translation-for-decode
    tae-products-for-encode
    tae-products-for-decode
;;
    tae-table-set-for-encode
    tae-table-set-for-decode
;;
    tae-dependent-translations
    tae-dependent-conversions))

(defsubst tae-get-translation-definition (name)
  (symbol-value name))

(defsubst tae-declared-translation-p (name)
  (and (symbolp name)
       (get name 'tae-translation)))

(defsubst tae-translation-dynamic-p (name)
  (get name 'tae-dynamic-translation))

(defun tae-copy-declared-translation (dest src)
  (mapcar
   (lambda (x)
     (put dest x (get src x)))
   tae-internal-translation-property)
  ;;; declared table-set must be copied.
  (let ((table-sets (append (get src 'tae-table-set-for-encode)
			    (get src 'tae-table-set-for-decode)))
	new-table-set-name)
    (while table-sets
      (table-set-add-reference (car table-sets))
      (setq table-sets (cdr table-sets)))))

(defun tae-translation-add-table-set (name table-set type decodep)
  (let* ((sym (if decodep
		  'tae-table-set-for-decode
		'tae-table-set-for-encode))
	 (alist (get name sym))
	 (maybe (assq type alist)))
    (if (cdr maybe)
	(error "Translation %S has already have table-set:%S(TYPE:%S, DECODEP:%S)."
	       name maybe type decodep))
    (if maybe
	(setcdr maybe table-set)
      (put name sym
	   (cons (cons type table-set)
		 alist)))))

(defsubst tae-translation-get-table-set (name type decodep)
  (if decodep
      (cdr (assq type (get name 'tae-table-set-for-decode)))
    (cdr (assq type (get name 'tae-table-set-for-encode)))))

(defun tae-initialize-translation (name)
  (set name nil)

  ;;; Initialize internal properties.
  (mapcar
   (lambda (x)
     (put name x nil))
   tae-internal-translation-property)

  ;;; Remove references to all generated table-sets.
  (mapcar
   (lambda (x)
     (table-set-remove-reference (cdr x)))
   (get name 'tae-table-set-for-encode))
  (mapcar
   (lambda (x)
     (table-set-remove-reference (cdr x)))
   (get name 'tae-table-set-for-decode)))

(defsubst tae-get-translation-produced-products (translation type decodep)
  (cdr (assq type
	     (get translation
		  (if decodep
		      'tae-products-for-decode
		    'tae-products-for-encode)))))

(defun tae-set-translation-produced-products (translation type decodep value)
  (let* ((prop (if decodep
		  'tae-products-for-decode
		'tae-products-for-encode))
	 (slots (get translation prop))
	 (slot (assq type slots)))
    (if slot
	(setcdr slot value))
    (put translation prop 
	 (cons (cons type value)
	       slots))))

;;; registered translation manager

(defun tae-register-translation (name type decodep)
  (if tae-enable-register-translation
      (progn
	(mucs-register-object 'tae-translation name)
	(let* ((n (if decodep 3 2))
	       (slot (mucs-get-registered-slot
		      'tae-translation name))
	       (typelist (nth n slot))
	       (typeslot (assq type typelist)))
	  (if typeslot
	      nil
	    (if (< (length slot) 4)
		(setcdr (nthcdr 1 slot) (list nil nil)))
	    (setcar (nthcdr n slot) (cons (list type nil) typelist)))))))

(defsubst tae-registered-translation-rule-list ()
  (mucs-registered-object-list 'tae-translation))

(defun tae-notify-embedment-product-list (name product-list decodep)
  (let* ((slot (mucs-get-registered-slot 'tae-translation name))
	 (prlist (nth (if decodep 3 2) slot))
	 elem rec)
    (while (setq elem (car product-list))
      (setq rec (assq elem prlist)
	    product-list (cdr product-list))
      (if (null rec)
	  (error "TAE Internal error:%S is not product of %S"
		 elem name))
      (setcar (cdr rec) t)))
  nil)

(defun tae-embedment-required-product-list (name decodep)
  (let* ((slot (mucs-get-registered-slot 'tae-translation name))
	 (prlist (nth (if decodep 3 2) slot))
	 elem result)
;    (message "NAME:%S PL:%S" name prlist)
    (while (setq elem (car prlist))
      (if (not (nth 1 elem))
	  (setq result (cons (car elem) result)))
      (setq prlist (cdr prlist)))
    result))

(defun tae-register-dependent-translation (dep-name)
  (let ((trlist (get tae-current-compiling-translation
		     'tae-dependent-translations)))
    (if (not (memq dep-name trlist))
	(put tae-current-compiling-translation
	     'tae-dependent-translations
	     (cons dep-name trlist)))))

(defun tae-get-dependent-translations (name)
  (let ((dealing (list name))
	dealt)
    (while (setq name (car dealing))

      (or (memq name dealt)
	  (setq dealt (cons name dealt)))
      (setq dealing (append (get name 'tae-dependent-translations)
			    (cdr dealing))))
    dealt))

(defun tae-register-conversion (name type decodep conversion)
  (let ((trlist (tae-get-dependent-translations name))
	marksym
	slot
	convlist
	cur)
;    (message "MS:%S->%S" marksym trlist)
    (while (setq cur (car trlist))
      (setq convlist (get cur 'tae-dependent-conversions)
	    trlist (cdr trlist))
;      (message "TR:%S NM:%S CONV:%S" cur name conversion)
      (setq marksym (tae-generate-unique-symbol-for-embed
		     name type decodep))
      (if (not (and (setq slot (assq conversion convlist))
		    (eq marksym (nth 1 slot))))
	  (put cur 'tae-dependent-conversions
	       (cons (list conversion name marksym type decodep)
		     convlist))))))

;;; translation declaration.

(defun tae-declare-translation (name translation &optional holdp dynamicp)
  "Declare translation.
NAME must be a symbol to distinguish TRANSLATION.
If HOLDP is non-nil, the declared translation is not initialized on
already produced products and others.
If DYNAMICP is non-nil, this translation can be modified
after its compilation."
  (if (not (symbolp name))
      (error "NAME:%S must be a symbol!" name))

  (if (tae-declared-translation-p translation)
      (tae-copy-declared-translation name translation)
    (if (not holdp)
	(tae-initialize-translation name))
    (set name translation)
    (put name 'tae-translation t)
    (if dynamicp
	(put name 'tae-dynamic-translation t))))

;;; Translation rule syntax
;;;
;;; TR-ELEM := (`assoc' TYPE-SPEC A-LIST [OPTION-ALIST])
;;;            | (`range' TYPE-SPEC RANGE-A-LIST [OPTION-ALIST]) 
;;;            | (`ccl' TYPE-SPEC CCL_Program [OPTION-ALIST])
;;; Notice that `ccl' TR-ELEM does not work currently.
;;;
;;; If type of TR-ELEM is A-LIST or CCL program,
;;;  this TR-ELEM is called PRECOMPILED.
;;;
;;; RANGE-A-LIST := (RANGE-A-LIST-ELEMENT ...)
;;; RANGE-A-LIST-ELEMENT := (RANGE . RANGE) | (RANGE . OBJECT) | (OBJECT . RANGE)
;;; DEF-RANGE, VAL-RANGE := RANGE
;;; TYPE-SPEC := (CAR-TYPE . CDR-TYPE)
;;; RANGE := ((OBJECT-MIN . OBJECT-MAX) ...)
;;; A-LIST := ((OBJECT . OBJECT) ...)
;;; OBJECT_MIN, OBJECT_MAX := OBJECT
;;;
;;; TYPE := registered type (see mucs-types.el)
;;; OBJECT := Lisp Object
;;;  (that must be valid in terms of the specified TYPE) (see mucs-types.el)
;;;
;;;
;;; TRANSLATION := TRANSLATION-ELEMENT  |
;;;                DECLARED-TRANSLATION |
;;;                (OP TRANSLATION [...TRANSLATION] ) |
;;;                (FOP TRANSLATION TRANSLATION)
;;;
;;; DECLARED-TRANSLATION := SYMBOL (see `tae-declare-translation')
;;;
;;; OP := `&' | `|' | `c'
;;; FOP := `ct' | `f' | `ff'
;;; TAG := symbol

;;;;;;   A-LIST note.
;;;;;;
;;;;;;     A-LIST is an association list to describe connections between
;;;;;;   mainly OBJECT and OBJECT.
;;;;;;   For example, an A-LIST, ((A1 . A2) (B1 . B2) ... (Z1 . Z2)),
;;;;;;   means correspondences between A1 and A2, B1 and B2, and so on.
;;;;;;   The element of A-LIST is a cons cell.
;;;;;;     In encoding, TAE regard it as car element to cdr element
;;;;;;   translation, and in decoding, regard it as cdr to car translation.
;;;;;;   Threfore, in above example, TAE translates A1 to A2 in encoding,
;;;;;;   and translates A2 to A1 in decoding.
;;;
;;;;;;     In a special case, an element of A-LIST can have 'all(SYMBOL)
;;;;;;   that matches anything.  For example, an A-LIST, ((1 . 5) (all . 10))
;;;;;;   directs to translate 1 to 5, and any other number to 10 in encoding.
;;;;;;   In decoding, TAE ignores the (all . 10) element, because it is
;;;;;;   impossible to translate a number to everything.
;;;;;;     And, in another special case, it can also have 'invalid(SYMBOL)
;;;;;;   that matches nothing and if a translated result is invalid, it
;;;;;;   ceases any further translation, and make the CCL state invalid.

;;;;;;   RANGE-A-LIST note.
;;;;;;     RANGE-A-LIST is NOT an association list in the normal meanings
;;;;;;   of Lisp.  


(defmacro tae-get-tr-elem-type (tr-elem-zzz)
  (list 'car tr-elem-zzz))

(defmacro tae-tr-elem-assoc-p (tr-elem-zzz)
  (list 'eq (list 'car-safe tr-elem-zzz)
	''assoc))

(defmacro tae-tr-elem-ccl-p (tr-elem-zzz)
  (list 'eq (list 'car-safe tr-elem-zzz)
	''ccl))

(defmacro tae-tr-elem-range-p (tr-elem-zzz)
  (list 'eq (list 'car-safe tr-elem-zzz)
	''range))

(defmacro tae-tr-elem-p (tr-elem-zzz)
  (list 'memq (list 'car-safe tr-elem-zzz)
	''(assoc range ccl)))

(defsubst tae-get-tr-elem-type-spec (tr-elem)
  (nth 1 tr-elem))

(defsubst tae-get-tr-elem-assoc (tr-elem)
  (nth 2 tr-elem))

(defsubst tae-set-tr-elem-assoc (tr-elem assoc)
  (setcar (nthcdr 2 tr-elem) assoc))

(defsubst tae-get-tr-elem-ccl (tr-elem)
  (nth 2 tr-elem))

(defsubst tae-get-tr-elem-range-alist (tr-elem)
  (nth 2 tr-elem))

;(defsubst tae-get-tr-elem-range (tr-elem)
;  (if (tae-tr-elem-elisp-p tr-elem)
;      (nth 3 tr-elem)
;    (error "This translation element:%S have no RANGE."
;	   tr-elem)))

(defsubst tae-get-tr-elem-option-alist (tr-elem)
   (cond ((tae-tr-elem-assoc-p tr-elem)
	  (nth 3 tr-elem))
	 ((tae-tr-elem-range-p tr-elem)
	  (nth 3 tr-elem))
	 ((tae-tr-elem-ccl-p tr-elem)
	  (nth 3 tr-elem))
	 (t
	  nil)))

(defsubst tae-set-tr-elem-option-alist (tr-elem alist)
  (let (slot)
    (cond ((tae-tr-elem-assoc-p tr-elem)
	   (setq slot (nthcdr 3 tr-elem)))
	  ((tae-tr-elem-range-p tr-elem)
	   (setq slot (nthcdr 3 tr-elem)))
	  ((tae-tr-elem-ccl-p tr-elem)
	   (setq slot (nthcdr 3 tr-elem))))
    (if slot
	(setcar slot alist)
      (setq tr-elem (nconc tr-elem (list alist)))))
  tr-elem)

(defsubst tae-assq-tr-elem-option-alist (tr-elem key)
  (assq key (tae-get-tr-elem-option-alist tr-elem)))

(defsubst tae-delq-tr-elem-option-alist (tr-elem key)
  (let ((alist (tae-get-tr-elem-option-alist tr-elem)))
    (tae-set-tr-elem-option-alist
     tr-elem
     (delq (assq key alist) alist))))

(defsubst tae-put-tr-elem-option-alist (tr-elem key value)
  (let* ((alist (tae-get-tr-elem-option-alist tr-elem))
	 (slot (assq key alist)))
    (if slot
	(setcdr slot value)
      (setq alist (cons (cons key value) alist))
      (tae-set-tr-elem-option-alist tr-elem alist))
    slot))

(defsubst tae-tr-elem-set-normalized-flag (tr-elem decodep &optional resetp)
  (tae-put-tr-elem-option-alist
   tr-elem
   'normalized (and (not resetp)
		    (if decodep
			'decode
		      'encode))))

(defsubst tae-tr-elem-normalized-p (tr-elem decodep)
  (eq (cdr (tae-assq-tr-elem-option-alist tr-elem 'normalized))
      (if decodep
	  'decode
	'encode)))

(defsubst tae-get-tr-elem-all-key (tr-elem)
  (cdr (tae-assq-tr-elem-option-alist tr-elem 'all)))

(defsubst tae-set-tr-elem-all-key (tr-elem value)
  (tae-put-tr-elem-option-alist tr-elem 'all value))

(defsubst tae-tr-elem-not-inverse-p (tr-elem)
  (cdr (tae-assq-tr-elem-option-alist tr-elem 'not-inverse)))

(defun tae-tr-elem-inverse-p (tr-elem decodep)
  (and decodep
       (if (tae-tr-elem-not-inverse-p tr-elem)
	   (let ((slot (tae-get-tr-elem-type-spec tr-elem)))
	     (if (eq (car slot) (cdr slot))
		 nil
	       (error "not-inverse option flag is valid only if both of the specified types is identical.")))
	 t)))
  
(defun tae-check-and-normalize-range (range-list)
  (let (reduced-range
    	max-rl
	max-re
	min-rl
	min-re)
    (mapcar
     (lambda (x)
       (if (numberp x) (setq x (cons x x))
	 (if (not (and (numberp (car x))
		       (numberp (cdr x))
		       (>= (cdr x) (car x))))
	     (error "Invalid range!:%S" x)))

       (setq  min-re (car reduced-range)
	      min-rl (cons nil reduced-range))
       (while 
	   (if (null min-re)
	       (progn
		 (setq reduced-range
		       (nconc reduced-range
			      (list x)))
		 nil)
	     (if (<= (car x) (1+ (cdr min-re)))
		 (progn

		   ;; (message "%S: (%S-->)" x min-re)

		   (setq  max-re min-re
			  max-rl min-rl)
		   (while
		       (and (> (1+ (cdr x)) (cdr max-re))
			    (setq max-rl (cdr max-rl)
				  max-re (car (cdr max-rl)))))

		   ;;; fix up list
		   ;;; (message "%S: (%S<->%S)" x min-re max-re)

		   (if (< (car x) (car min-re))
		       (setcar min-re (car x)))
		   (if (> (cdr x) (cdr min-re))
		       (setcdr min-re (cdr x)))
		   (if (and max-re
			    (<= (car max-re)(cdr min-re)))
		       (progn
			 (setcdr min-re (cdr max-re))
			 (setq max-rl (cdr max-rl))))
		   (setcdr (cdr min-rl) (cdr max-rl))

		   nil)
	       (setq min-rl (cdr min-rl)
		     min-re (car (cdr min-rl)))
	       t))))
     range-list)
;;    (mapcar
;;     (lambda (x)
;;       (if (eq (car x) (cdr x))
;;	   (car x)
;;	 x))
;;     reduced-range)
    reduced-range
    ))

(defun tae-normalize-translation (tr-elem decodep)
  "Normalized TR.
TR-ELEM, the translation element, are reduced to
ASSOC or CCL program. 
But, we have not be able to reduce
Emacs Lisp Function to CCL program yet.
And then, if the translation-element is ASSOC, sort it.

When decodep is non-nil, normalize for decording."
  (setq tr-elem (copy-sequence tr-elem))
  (cond ((tae-tr-elem-assoc-p tr-elem)
	 (let ((curll (tae-get-tr-elem-assoc tr-elem))
	       key-type relative-op
	       curel alist-copy all-slot)
	   
	   ;;; copy alist and remove 'all and invalid slot.
	   (if (tae-tr-elem-inverse-p tr-elem decodep)
	       (progn
		 (setq key-type (cdr (tae-get-tr-elem-type-spec tr-elem))
		       relative-op (mucs-type-get-relative-op key-type))
		 (while 
		     (and (setq curel (car curll))
			  (cond ((eq (cdr curel) 'all)
				 (setq all-slot (cons (cdr curel)
						      (car curel)))
				 nil)
				;; skip the slot
				((or (eq (car curel) 'all)
				     (eq (cdr curel) 'invalid))
				 (setq curll (cdr curll)))
				(t
				 (setq alist-copy (cons (cons (cdr curel)
							      (car curel))
							alist-copy))
				 (setq curll (cdr curll)))))))
	     (setq key-type (cdr (tae-get-tr-elem-type-spec tr-elem))
		   relative-op (mucs-type-get-relative-op key-type))
	     (while 
		 (and (setq curel (car curll))
		      (cond ((eq (car curel) 'all)
			     (setq all-slot curel)
			     nil)
			    ;; skip the slot
			    ((or (eq (cdr curel) 'all)
				 (eq (car curel) 'invalid))
			     (setq curll (cdr curll)))
			    (t
			     (setq alist-copy (cons (cons (car curel)
							  (cdr curel))
						    alist-copy))
			     (setq curll (cdr curll)))))))

	   (tae-set-tr-elem-assoc
	    tr-elem
	    (nconc
	     (sort alist-copy
		   (lambda (x y) (funcall relative-op
					  (car x) (car y))))))
	   (if all-slot
	       (tae-set-tr-elem-all-key tr-elem
					(cdr all-slot)))))

	((tae-tr-elem-range-p tr-elem)
	 (let ((alist (tae-get-tr-elem-range-alist tr-elem))
	       range-elem i j
	       newalist)
	   ;;; We should implement normalization processes on range tr-elem.
	   ))

	((tae-tr-elem-ccl-p tr-elem)
	 ))
  (tae-tr-elem-set-normalized-flag tr-elem decodep)
  tr-elem)

(defun tae-reduce-OR-translations (tr-elems decodep)
  "Reduce `OR' translations.
TR-ELEMS must be a list that consists of TR-ELEMs
that must be normalized.
When DECODEP is t, reduce for decodeing translation."
  (let ((tr1 (car tr-elems))
	(tr2 (nth 1 tr-elems))
	(tr-curl (cdr tr-elems))
	result-list result tmp)
    (while (and tr2
		(not (tae-get-tr-elem-all-key tr1)))
      (setq result
	    (cond ((and (tae-tr-elem-assoc-p tr1)
			(tae-tr-elem-assoc-p tr2))
		   (let* ((alist1 (tae-get-tr-elem-assoc tr1))
			  (alist2 (tae-get-tr-elem-assoc tr2))
			  (cur-ll2 alist2)
			  (type-spec (tae-get-tr-elem-type-spec tr1))
			  (key-type (if decodep (cdr type-spec)
				      (car type-spec)))
			  (relative-op (mucs-type-get-relative-op key-type))
			  (equal-op (mucs-type-get-equal-op key-type))
			  cur-ll1 cur-el1 cur-el2 result)
		     (setq cur-el1 (car alist1)
			   cur-ll1 (cons nil alist1))
		     (while (setq cur-el2 (car cur-ll2))
		       (while 
			   (progn
			     (cond ((funcall relative-op
					     (car cur-el2)
					     (car cur-el1))
				    (if (null (car cur-ll1))
					(setq alist1
					      (cons cur-el2
						    alist1))
				      (setcdr cur-ll1
					      (cons cur-el2
						    (cdr cur-ll1))))
				    nil)
				   ((funcall equal-op
					     (car cur-el1)
					     (car cur-el2))
				    nil)
				   (t
				    (setq cur-ll1 (cdr cur-ll1))
				    (if (setq cur-el1 (car (cdr cur-ll1)))
					t
				      (setq alist1 (nconc alist1
							  cur-ll2)
					    cur-ll2 nil)
				      nil)))))
		       (setq cur-ll2 (cdr cur-ll2)))
		     ;; making assoc tr-elem
		     (setq result
			   (list 'assoc
				 ;; TYPE spec
				 (tae-get-tr-elem-type-spec tr1)
				 ;; Association
				 alist1))

		     ;; set all key
		     (tae-set-tr-elem-all-key
		      result
		      (tae-get-tr-elem-all-key tr2))

		     result))

		  ((or (and (tae-tr-elem-assoc-p tr1)
			    (tae-tr-elem-range-p tr2))
		       (and (tae-tr-elem-assoc-p tr2)
			    (tae-tr-elem-range-p tr1)
			    (setq tmp tr2
				  tr2 tr1
				  tr1 tmp)))
		   ;; tr1 ... assoc tr element
		   ;; tr2 ... range tr element
		   nil)

		  ((and (tae-tr-elem-range-p tr1)
			(tae-tr-elem-range-p tr2))
		   ;; tr1, tr2 ... range tr elements
		   nil)

		  ((or (or (tae-tr-elem-ccl-p tr1)
			   (tae-tr-elem-range-p tr1))
		       (or (tae-tr-elem-ccl-p tr2)
			   (tae-tr-elem-range-p tr2)))
		   nil)
		  ((or (tae-declared-translation-p tr1)
		       (tae-declared-translation-p tr2))
		   nil)
		  (t
		   (error "Unknown translations:%S, %S" tr1 tr2))
		  ))
      (if result
	  (progn
	    (tae-tr-elem-set-normalized-flag result decodep)
	    (setq tr1 result))
	(setq result-list (cons tr1 result-list)
	      tr1 tr2))
      (setq tr-curl (cdr tr-curl)
	    tr2 (car tr-curl)))
    (setq result-list (cons tr1 result-list))
    (nreverse result-list)))

(defun tae-reduce-AND-translations (tr-elems decodep)
  "Reduce `AND' translations.
TR-ELEMS must be a list that consists of TR-ELEMs
that must be normalized.
When DECODEP is t, reduce for decodeing translation."
  (let ((tr1 (car tr-elems))
	(tr2 (nth 1 tr-elems))
	(tr-curl (cdr tr-elems))
	result-list result)
    (while tr2
      (if (tae-get-tr-elem-all-key tr2)
	  (setq result tr1)
	(setq result
	      (cond ((and (tae-tr-elem-assoc-p tr1)
			  (tae-tr-elem-assoc-p tr2))
		     (tae-set-tr-elem-all-key tr1 nil)
		     (let* (result-alist
			    (alist1 (tae-get-tr-elem-assoc tr1))
			    (alist2 (tae-get-tr-elem-assoc tr2))
			    (cur-ll2 alist2)
			    (type-spec (tae-get-tr-elem-type-spec tr1))
			    (key-type (if decodep (cdr type-spec)
					(car type-spec)))
			    (relative-op (mucs-type-get-relative-op key-type))
			    (equal-op (mucs-type-get-equal-op key-type))
			    cur-ll1 cur-el1 cur-el2)
		       (while (setq cur-el2 (car cur-ll2))
			 (setq cur-el1 (car alist1)
			       cur-ll1 (cons nil alist1))
			 (while 
			     (progn
			       (cond ((funcall equal-op
					       (car cur-el1)
					       (car cur-el2))
				      (setq result-alist
					    (cons cur-el2 result-alist))
				      nil)
				     (t
				      (setq cur-ll1 (cdr cur-ll1)
					    cur-el1 (car (cdr cur-ll1)))
				      ))))
			 (setq cur-ll2 (cdr cur-ll2)))
		       (list 'assoc
			     (tae-get-tr-elem-type-spec tr1)
			     result-alist)))
		    ((or (or (tae-tr-elem-ccl-p tr1)
			     (tae-tr-elem-range-p tr1))
			 (or (tae-tr-elem-ccl-p tr2)
			     (tae-tr-elem-range-p tr2)))
		     nil)
		    ((or (tae-declared-translation-p tr1)
			 (tae-declared-translation-p tr2))
		     nil)
		    (t
		     (error "Unknown translations:%S, %S" tr1 tr2))
		    )))
      (if result
	  (progn
	    (tae-tr-elem-set-normalized-flag result decodep)
	    (setq tr1 result))
	(setq result-list (cons tr1 result-list)
	      tr1 tr2))
      (setq tr-curl (cdr tr-curl)
	    tr2 (car tr-curl)))
    (setq result-list (cons tr1 result-list))
    (nreverse result-list)))

(defun tae-reduce-composite-translations (tr-elems decodep)
  "Reduce `composite' translations.
TR-ELEMS must be a list that consists of TR-ELEMs
that must be normalized.
When DECODEP is t, reduce for decodeing translation."
  (let* ((trs (if decodep (reverse tr-elems) tr-elems))
	 (tr1 (car trs))
	 (tr2 (nth 1 trs))
	 (tr-curl (cdr trs))
	 tr1-all-key
	 tr2-all-key
	 result-list result)
    (while tr2
      (setq tr1-all-key (tae-get-tr-elem-all-key tr1)
	    tr2-all-key (tae-get-tr-elem-all-key tr2))
      (setq result
	    (cond ((and (tae-tr-elem-assoc-p tr1)
			(tae-tr-elem-assoc-p tr2))
		   (let* (result-alist
			  (alist1 (tae-get-tr-elem-assoc tr1))
			  (alist2 (tae-get-tr-elem-assoc tr2))
			  (cur-ll2 alist2)
			  (type-spec (tae-get-tr-elem-type-spec tr1))
			  (key-type (if decodep (cdr type-spec)
				      (car type-spec)))
			  (relative-op (mucs-type-get-relative-op key-type))
			  (equal-op (mucs-type-get-equal-op key-type))
			  cur-ll1 cur-el1 cur-el2
			  tr-new-all-key)
		     (while (setq cur-el2 (car cur-ll2))
		       (setq cur-el1 (car alist1)
			     cur-ll1 (cons nil alist1))
		       (while 
			   (progn
			     (cond ((funcall equal-op
					     (cdr cur-el1)
					     (car cur-el2))
				    (setq result-alist
					  (cons
					   (cons (car cur-el1)
						 (cdr cur-el2))
						result-alist))
				    ;; remove the slot that is never used.
				    (if (null (car cur-ll1))
					(setq alist1 (cdr alist1))
				      (setcdr cur-ll1 (cdr cur-ll1)))

				    nil)

				   (t
				    (setq cur-ll1 (cdr cur-ll1)
					  cur-el1 (car (cdr cur-ll1))))
				   )))

		       (if tr1-all-key
			   (if (and (null tr-new-all-key)
				    (funcall equal-op
					     tr1-all-key
					     (car cur-el2)))
			       (setq tr-new-all-key (cdr cur-el2))))
		       (setq cur-ll2 (cdr cur-ll2)))

		     ;; operate tr2-all-key
		     (if tr2-all-key
		     ;;; At encoding, append the rest of alist1.
			 (progn
			   (setq cur-ll1 alist1)
			   (while cur-ll1
			     (setq result-alist
				   (cons
				    (cons (car (car cur-ll1))
					  tr2-all-key)
				    result-alist)
				   cur-ll1 (cdr cur-ll1)))
			   (if (null tr-new-all-key)
			       (setq tr-new-all-key
				     tr2-all-key))))

		     (list 'assoc 
			   (tae-get-tr-elem-type-spec tr1)
			   result-alist
			   (list (cons 'all tr-new-all-key)))
		     ))
		  ((or (or (tae-tr-elem-ccl-p tr1)
			   (tae-tr-elem-range-p tr1))
		       (or (tae-tr-elem-ccl-p tr2)
			   (tae-tr-elem-range-p tr2)))
		   nil)
		  ((or (tae-declared-translation-p tr1)
		       (tae-declared-translation-p tr2))
		   nil)
		  (t
		   (error "Unknown translations:%S, %S" tr1 tr2))
		  ))
      (if result
	  (setq tr1 (tae-normalize-translation result decodep))
	;;; unreduced
	(setq result-list (cons tr1 result-list)
	      tr1 tr2))
      (setq tr-curl (cdr tr-curl)
	    tr2 (car tr-curl)))
    (setq result-list (cons tr1 result-list))
    (nreverse result-list)))

(defun tae-reduce-composite-transparent-translations (tr-elems decodep)
  "Reduce `composite' translations.
TR-ELEMS must be a list that consists of two TR-ELEMs
that must be normalized.
When DECODEP is t, reduce for decodeing translation."
  (if (/= (length tr-elems) 2)
      (error "Composite Transparent requires two arguments:%S"
	     tr-elems))
  (let ((copied (copy-sequence (car tr-elems))))
    (setq tr-elems
	  (tae-reduce-composite-translations tr-elems decodep))
    (setq tr-elems (mapcar
		    (lambda (x)
		      (if (tae-tr-elem-normalized-p x decodep)
			  x
			(tae-normalize-translation x decodep)))
		    tr-elems))
    (tae-reduce-OR-translations
     (nconc tr-elems (list copied)) decodep)))

;;
;;
;;

(defun tae-normalize-reduction-unit (unit &optional decodep)
  "Destructively, normalize reduction unit.
This function checks type-spec of UNIT, and
according to it, rearrange the elements of UNIT."
  (let ((cur-ll unit)
	cur-el)
  (while (setq cur-el (car cur-ll))
    (if (tae-tr-elem-p cur-el)
	(if (tae-tr-elem-normalized-p cur-el decodep)
	    nil
	  (setcar cur-ll (tae-normalize-translation cur-el decodep)))
      (setcar cur-ll (tae-reduce-translation-internal cur-el decodep)))
    (setq cur-ll (cdr cur-ll))))
  unit)

(defun tae-reduce-translation-internal (translation &optional decodep)
  (cond ((tae-declared-translation-p translation)
	 translation)
	((tae-tr-elem-p translation)
	 (or (tae-tr-elem-normalized-p translation decodep)
	     (setq translation
		   (tae-normalize-translation translation decodep)))
	 translation)
	(t
	 (let ((op (car translation))
	       (args (copy-sequence (cdr translation))))
	   (cond ((eq op '|)
		  (setq args (tae-normalize-reduction-unit args decodep))
		  (setq args (tae-reduce-OR-translations args decodep))
		  (if (= (length args) 1)
		      (car args)
		    (cons '| args)))
		 ((eq op '&)
		  (setq args (tae-normalize-reduction-unit args decodep))
		  (setq args (tae-reduce-AND-translations args decodep))
		  (if (= (length args) 1)
		      (car args)
		    (cons '& args)))
		 ((eq op 'c)
		  (setq args (tae-normalize-reduction-unit args decodep))
		  (setq args 
			(tae-normalize-reduction-unit
			 (tae-reduce-composite-translations args decodep)
			 decodep))
		  (if (= (length args) 1)
		      (car args)
		    (cons 'c args)))
; 		 ((eq op 'ct)
; 		  (setq args (tae-normalize-reduction-unit args decodep))
; 		  (setq args 
; 			(tae-normalize-reduction-unit
; 			 (tae-reduce-composite-transparent-translations
; 			  args decodep)
; 			 decodep))
; 		  (if (= (length args) 1)
; 		      (car args)
; 		    (cons 'ct args)))
		 (t
		  (error "TAE have not supported OP:`%S' yet!!" op)))))))

(defsubst tae-reduce-translation (name &optional decodep)
  "Reduce translation."
  (let ((sym (if decodep
		 'tae-reduced-translation-for-decode
	       'tae-reduced-translation-for-encode)))
    (or (get name sym)
	(put name sym
	     (tae-reduce-translation-internal
	      (symbol-value name)
	      decodep)))))

;; TAE message
;;   Not yet fixed :-P.
;;       ((tables . ...) ...???)

;; currently, nested table only --> table-set

;;
;; Produce MYO from translation rule.
;;

(defsubst tae-message-append (mes1 mes2)
  (let ((alist1 mes1)
	(alist2 mes2)
	elem1 elem2)
    (while (setq elem1 (car alist1))
      (setq elem2 (assq (car elem1) alist2)
	    alist1 (cdr alist1))
      (if elem2
	  (progn
	    (setq alist2 (delq elem2 alist2))
	    (if (memq (car elem1) '(new-type))     ;;;; non merged key list.
		(setcdr elem1 (cdr elem2))
	      (nconc elem1 (cdr elem2))
	      ))))
    (nconc mes1 alist2)))

(defsubst tae-message-add (message key data)
  (nconc (assq key message) (list data))
  message)

(defsubst tae-message-get (message key)
  (cdr (assq key message)))

(defsubst tae-message-put (message key value)
  (setcdr (assq key message) value))

(defsubst tae-get-message-from-translation (tr type decodep)
  (let ((result (copy-sequence
		 (tae-get-compiled-products-internal tr type decodep)))
	tables-new-slot)
    ;;; defined table-set.
    (if (tae-declared-translation-p tr)
	(progn
	  (if (assq 'tables result)
	      (error "Internal Error:message(%S)" result))
	  (cons result
		(list
		 (tae-translation-get-table-set tr type decodep))))
      ;;; generated 'tables
      (if (setq tables-new-slot (assq 'tables result))
	  (cons
	   (delq tables-new-slot result)
	   (cdr tables-new-slot))))))

(defsubst tae-message-set-tables (message tables)
  (let ((tables-slot (assq 'tables message)))
    (if tables-slot
	(setcdr tables-slot tables)
      (setq message (cons
		     (cons 'tables tables)
		     message)))
    message))

(defsubst tae-message-set-new-type (message type &optional orig-type)
  (let ((slot (assq 'new-type message)))
    (if slot
	(progn
	  (cond (type
		 (setcdr slot (list type orig-type)))
		(orig-type
		 (setcdr (cdr slot) (list orig-type))))
	  message)
      (cons (list 'new-type type orig-type) message))))

(defsubst tae-message-get-new-type (message &optional origp)
  (let ((slot (assq 'new-type message)))
    (and slot
	 (if origp (nth 2 slot) (nth 1 slot)))))

(defsubst tae-message-set-arithmetic (message flag)
  (let ((slot (assq 'arithmetic message)))
    (if slot
	(progn
	  (setcdr slot flag)
	  message)
      (cons (cons 'arithmetic flag) message))))

(defsubst tae-message-arithmetic-p (message)
  (cdr (assq 'arithmetic message)))

(defun tae-generate-new-table-set (tables name type decodep)
;  (message "NAME:%S TYPE:%S DECODEP:%S" name type decodep)
  (let (table-set)
    (while
	(progn
	  (setq table-set
		;;; It is useful for debugging
		;;; to intern generated symbols.
		(intern
		 (tae-generate-translation-table-set-name
		  name type decodep)))
	  (table-set-p table-set)))
    (tae-translation-add-table-set
     name table-set type decodep)
    (define-table-set table-set tables)
    table-set))

(defun tae-compile-translation-element (tr-elem type decodep)
  (let* ((type-spec (tae-get-tr-elem-type-spec tr-elem))
	 (from-type (if decodep
			(cdr type-spec)
		      (car type-spec)))
	 (to-type (if decodep
		      (car type-spec)
		    (cdr type-spec)))
	 (type-func (mucs-type-get-ccl-representation type))
	 (to-func (mucs-type-get-ccl-representation to-type))
	 (conv-func (if (eq type from-type)
			'identity
		      (mucs-type-get-conversion-force
		       from-type type)))
	 (tr-elem-all-key (funcall
			   to-func
			   (tae-get-tr-elem-all-key tr-elem)))
	 conv-slot
	 result)
    (setq result
	  (cond ((tae-tr-elem-assoc-p tr-elem)
		 (if (not (tae-tr-elem-normalized-p tr-elem decodep))
		     (error "TAE cannot compile unnormalized translation:%S"
			    tr-elem))
		   
		   ;; type-func, conv-func, and to-func are bound by the above `let'.
		   ;; Owing to Dynamic Binding of Emacs, this function can call
		   ;; a function bounded by them respectively.
		 (setq conv-slot (lambda (x)
				   (cons
				    (funcall type-func
					     (funcall conv-func (car x)))
				    (funcall to-func (cdr x)))))
		 (list 
		  (cons 'tables
			(nconc
			 (make-code-conversion-tables
			  (tae-get-tr-elem-assoc tr-elem)
			  conv-slot)
			 (if tr-elem-all-key
			     (list
			      (tae-project-all-table
			       tr-elem-all-key))
			   nil)))))

		((tae-tr-elem-ccl-p tr-elem)
		 (list (cons 'ccl (tae-get-tr-elem-ccl tr-elem))))

		((tae-tr-elem-range-p tr-elem)
		 (if (not (tae-tr-elem-normalized-p tr-elem decodep))
		     (error "TAE cannot compile unnormalized translation:%S"
			    tr-elem))
		 (let ((ralist (tae-get-tr-elem-range-alist tr-elem))
		       slot1 slot2 st1 end1 st2 end2
		       elem tables)

		   (while (setq elem (car ralist))
		     (setq ralist (cdr ralist))

		     ;; set st1, st2, end1, end2
		     (if (tae-tr-elem-inverse-p tr-elem decodep)
			 (setq slot1 (cdr elem)
			       slot2 (car elem))
		       (setq slot1 (car elem)
			     slot2 (cdr elem)))
		     (if (consp slot1)
			 (setq st1
			       (funcall type-func
					(funcall conv-func
						 (car slot1)))
			       end1
			       (funcall type-func
					(funcall conv-func
						 (cdr slot1))))
		       (setq st1
			     (funcall type-func
				      (funcall conv-func
					       slot1))
			     end1 nil))
		     (if (consp slot2)
			 (setq st2
			       (funcall to-func
					(car slot2))
			       end2
			       (funcall to-func
					(cdr slot2)))
		       (setq st2
			     (funcall to-func slot2)
			     end2 nil))

		     (cond ((and (null end1) (null end2))
			    (error "Invalid range slot:%S!" elem))
			   ((null end1)
			    (setq end1 (+ st1 (- end2 st2))))
			   ((null end2)
			    (setq end2 (+ st2 (- end1 st1))))
			   (t
			    (if (/= (- end1 st1) (- end2 st2))
				(error "Inconsistent range slot:%S(%d, %d)-(%d, %d)"
				       elem st1 end1 st2 end2))))
		     (if (mucs-ccl-inspect-facility 'valid-map-multiple)
			 (setq tables
			       (cons
				(vector t (mucs-ccl-add-program
					   'r0 (- st2 st1))
					st1 (1+ end1))
				tables))
		       (setq tables
			     (cons
			      (vector t (+ (- st2 st1)
					   (mucs-arithmetic-adjust))
				      st1 (1+ end1))
			      tables))))
		   (list 
		    (cons 'tables
			  (nreverse tables))
		    '(arithmetic . t))))))

    (tae-message-set-new-type result
			      to-type from-type)))

(defun tae-compile-OR-operation (translations type decodep)
  (let (tr mes-tbl mes tbls result-mes to-type new-type arith)
    (while (setq tr (car translations))
      (setq translations (cdr translations)
	    mes-tbl (tae-get-message-from-translation
		     tr type decodep)
	    mes (car mes-tbl)
	    tbls (nconc tbls (cdr mes-tbl))
	    new-type (tae-message-get-new-type mes)
	    arith (or arith (tae-message-arithmetic-p mes)))

      ;;; operate type.
      (if to-type
	  (if (not (eq to-type new-type))
	      (signal 'mucs-type-mismatch-error
		      (list to-type new-type)))
	(setq to-type new-type)
	(setq result-mes
	      (tae-message-set-new-type result-mes to-type)))

      ;; normal message keys are preserved.
      (setq result-mes (tae-message-append result-mes mes)))
    (tae-message-set-arithmetic
     (tae-message-set-tables result-mes
			     tbls)
     arith)))

(defun tae-compile-AND-operation (translations type decodep)
  (mucs-ccl-facility-error
   'valid-map-multiple
   "Sorry.  AND operation on compilation is disabled.")
  (let (tr mes-tbl mes tbls result-mes to-type new-type
	   arith first-tbl)
    (while (setq tr (car translations))
      (setq translations (cdr translations)
	    mes-tbl (tae-get-message-from-translation
		     tr type decodep)
	    mes (car mes-tbl)
	    new-type (tae-message-get-new-type mes))

      ;;; operate type.
      (if to-type
	  (if (not (eq to-type new-type))
	      (signal 'mucs-type-mismatch-error
		      (list to-type new-type)))
	(setq to-type new-type)
	(setq result-mes
	      (tae-message-set-new-type result-mes to-type)))
      (if first-tbl
	  (setq tbls (cons 
		      (cdr mes-tbl)
		      (cons
		       (tae-project-all-table t)
		       tbls)))
	(setq first-tbl (list (cdr mes-tbl))
	      tbls first-tbl
	      arith (tae-message-arithmetic-p mes)))

      ;; normal message keys are preserved.
      (setq result-mes (tae-message-append result-mes mes)))
    (tae-message-set-arithmetic
     (tae-message-set-tables result-mes
			     (list tbls))
     arith)))

(defun tae-compile-composite-operation (translations type decodep)
  (mucs-ccl-facility-error
   'valid-map-multiple
   "Sorry.  Composite operation on compilation is disabled.")
  (let (mes-tbl mes
	tbls result-mes
	new-type
	tr arith)
    (setq tr (car translations)
	  translations (cdr translations))
    (if tr
	(while 
	    (progn
	      (setq mes-tbl (tae-get-message-from-translation
			     tr type decodep)
		    mes (car mes-tbl)
		    new-type (tae-message-get-new-type mes)
		    arith (tae-message-arithmetic-p mes))

	      ;; operate type.
	      (setq type new-type)
	      (setq result-mes
		    (tae-message-set-new-type result-mes type))

	      ;; normal message keys are preserved.
	      (setq result-mes (tae-message-append result-mes mes))
	      translations)

	  ;; build up table structure.
	  (setq tbls (nconc tbls
			    (list (cdr mes-tbl))
;			    (if arith (list (tae-project-all-arith-to-lambda)
			    ))

	  (setq	tr (car translations)
		translations (cdr translations))))

    (setq tbls (list
		(nconc tbls
		       (list (cdr mes-tbl))
;		       (if arith (list (tae-project-all-arith-to-lambda)))
		       )
		(tae-project-all-table t)))
    (tae-message-set-arithmetic
     (tae-message-set-tables result-mes
			     tbls)
     arith)))

(defun tae-compile-composite-transparent-operation (translations type decodep)
  (error "Sorry!.  In this version, compilation of
composite-transparent operation is disabled intentionally.")
  (if (/= (length translations) 2)
      (error "Composite Transparent requires two arguments:%S"
	     translations))
  (let ((mes-tbl-a (tae-get-message-from-translation
		    (car translations) type decodep))
	mes-tbl-b mes tbls result-mes)
    (setq mes (car mes-tbl-a)
	  type (tae-message-get-new-type mes)
	  mes-tbl-b (tae-get-message-from-translation
		     (nth 1 translations) type decodep)
	  tbls (cons (cdr mes-tbl-a)
		     (nconc (cdr mes-tbl-b)
			    (list (tae-project-all-table t))
			    (cdr mes-tbl-a))))

    ;; normal message keys are preserved.
    (setq result-mes (tae-message-append mes (car mes-tbl-b)))
    (tae-message-set-tables
     (tae-message-set-new-type result-mes type)
     tbls)))

(defun tae-get-compiled-products-internal (translation type decodep)
  (let (op args)
    (if (consp translation)
	(setq op (car translation)
	      args (cdr translation)))
    (cond ((tae-declared-translation-p translation)
	   ;;; If this translation is dynamic,
	   ;;; all referred translations are used to
	   ;;; dynamic modification.
	   ;(if (tae-translation-dynamic-p
	   ;   tae-current-compiling-translation)
	   ;   (tae-register-dependent-translation
	   ;   translation nil))
	   ;;; If the referred transation is dynamic,
	   ;;; this translation must be also modified by
	   ;;; dynamic modification.
	   ;(if (tae-translation-dynamic-p translation)
	   ;    (tae-register-dependent-translation
	   ;    translation t))
	   (tae-register-dependent-translation
	    translation)
	   (tae-get-compiled-products translation type decodep))
	  ((tae-tr-elem-p translation)
	   (tae-compile-translation-element translation type decodep))
	  ((eq op '|)
	   (tae-compile-OR-operation args type decodep))
	  ((eq op '&)
	   (tae-compile-AND-operation args type decodep))
	  ((eq op 'c)
	   (tae-compile-composite-operation args type decodep))
;	  ((eq op 'ct)
;	   (tae-compile-composite-transparent-operation args type decodep))
	  (t
	   (error "TAE have not supported OP:`%S' yet!!" op)))))

(defun tae-get-compiled-products (name type decodep)
  (let ((products (tae-get-translation-produced-products
		   name type decodep))
	table-set tables-slot translation)
    ;; In order to make traslation data persistent.
    (tae-register-translation name type decodep)

    (if products
	products
      (setq translation (tae-reduce-translation
			 name decodep)
	    products (tae-get-compiled-products-internal
		      translation type decodep))

      ;; deal with 'tables message.
      ;; convert it to table-set.
      (if (setq tables-slot (assq 'tables products))
	  (progn
	    (if (setq table-set
		      (tae-translation-get-table-set
		       name type decodep))
		(progn
		  (define-table-set
		    table-set
		    (cdr tables-slot))
		  table-set)
	      (tae-generate-new-table-set
	       (cdr tables-slot) name type decodep))
	    (setq products
		  (delq tables-slot products))))
      ;; register products.
      (tae-set-translation-produced-products
       name type decodep products)
      ;; return products.
      products)))

;;;
;;; Persistent data for translation rule.
;;;   `Persistent' means data that is necessary
;;; to manipulate the tranlation rule for storing into
;;; a byte-compiled file.

(defun tae-persistent-program-template (name item objs replacep)
  ;;(message "NAME:%S ITEM:%S OBJS:%S" name item objs)
  (if replacep
      `(put (quote ,name)
	    (quote ,item)
	    (quote ,objs))
    `(put (quote ,name)
	  (quote ,item)
	  (nconc (quote ,objs)
		 (get (quote ,name)
		      (quote ,item))))))

(defun tae-generate-persistent-program (name persistent-items)
  "Generate programs to make translation NAME persistent."
  (if (not (tae-declared-translation-p name))
      (error "NAME:%S is not declared translation rule." name))
  (let (result current-item objs objlist replacep
	       product-list clist)
    (while (setq current-item (car persistent-items))
      (setq persistent-items (cdr persistent-items))
      (if (eq current-item 'tae-translation-definition)
	  (setq result
		(cons
		 `(setq ,name (quote ,(symbol-value name)))
		 result))
	(cond ((memq current-item
		     '(tae-translation
		       tae-dynamic-translation
		       tae-dependent-conversions))
	       (setq objs (get name current-item)
		     replacep t))

	      ((eq current-item
		   'tae-reduced-translation-for-encode)
	       (setq objs (tae-reduce-translation name nil)
		     replacep t))

	      ((eq current-item
		   'tae-reduced-translation-for-decode)
	       (setq objs (tae-reduce-translation name t)
		     replacep t))

	      ((eq current-item
		   'tae-products-for-encode)
	       (setq replacep nil
		     product-list (tae-embedment-required-product-list
				   name nil)
		     clist product-list
		     objs nil
		     objlist (get name 'tae-products-for-encode))
	       (while clist
		 (setq objs (cons (assq (car clist) objlist)
				  objs)
		       clist (cdr clist)))
	       (if objs 
		   (setq result
			 (cons (tae-persistent-program-template
				name 'tae-products-for-encode objs nil)
			       result)))
	       (setq objs nil
		     objlist (get name 'tae-table-set-for-encode)
		     clist product-list)
	       (while clist
		 (setq objs (cons (assq (car clist) objlist)
				  objs)
		       clist (cdr clist)))
	       (if objs
		   (setq result
			 (cons (tae-persistent-program-template
				name 'tae-table-set-for-encode objs nil)
			       result)))
	       (tae-notify-embedment-product-list
		name product-list nil))

	      ((eq current-item
		   'tae-products-for-decode)
	       (setq replacep nil
		     product-list (tae-embedment-required-product-list
				   name t)
		     clist product-list
		     objs nil
		     objlist (get name 'tae-products-for-decode))
	       (while clist
		 (setq objs (cons (assq (car clist) objlist)
				  objs)
		       clist (cdr clist)))
	       (if objs
		   (setq result
			 (cons (tae-persistent-program-template
				name 'tae-products-for-decode objs nil)
			       result)))
	       (setq objs nil
		     objlist (get name 'tae-table-set-for-decode)
		     clist product-list)
	       (while clist
		 (setq objs (cons (assq (car clist) objlist)
				  objs)
		       clist (cdr clist)))
	       (if objs
		   (setq result
			 (cons (tae-persistent-program-template
				name 'tae-table-set-for-decode objs nil)
			       result)))
	       (tae-notify-embedment-product-list name product-list t))

	      (t
	       (error "%S is not valid item that can be persistent."
		      current-item)))
	(if (and objs
		 replacep)
	    (setq result
		  (cons (tae-persistent-program-template
			 name current-item objs t)
			result)))))

;    (message "TR:%S,Result:%S" name result)
    result
;;; enclose with `progn' form?
;    (if (> (length result) 1)
;	(cons 'progn result)
;      (car result))
    ))

(defun tae-compile-internal (name &optional decodep type)
  (if (not (tae-declared-translation-p name))
      (error "Undeclared or invalid translation:%S" name))
  (let* ((tae-current-compiling-translation name)
	 (messages (tae-get-compiled-products name type decodep))
	 (mes-cur messages)
	 (result-myo (mucs-ccl-empty-myo))
	 (tae-translation-table-set-index
	  tae-translation-table-set-index)
	 mes-el
	 table-set table-syms table-syms-nested-p)
    ;; Mainly for dynamic modification,
    ;; register dependent conversions.
    (if mucs-current-conversion
	(tae-register-conversion
	 name type decodep
	 mucs-current-conversion))
    ;; We should not directly access message-slots...
    (while (setq mes-el (car mes-cur))
      (setq mes-cur (cdr mes-cur))
      (cond ((eq 'new-type (car mes-el))
	     (setq mucs-current-type (nth 1 mes-el)))

	    ((eq 'arithmetic (car mes-el))
	     ;; do nothing.
	     )
	    (t
	     (error "Internal error.  unknown TAE message:%S" mes-el)
	     )))
    (setq table-set
	  (tae-translation-get-table-set
	   name type decodep)
	  table-syms (get-table-set-symbol-list-recursively
		      table-set)
	  result-myo (mucs-ccl-myo-add-table-set
		      table-set result-myo))
    (if (table-set-nested-p table-set)
	(setq table-syms-nested-p t))
    (list
     result-myo
     table-syms
     table-syms-nested-p)))

(defun tae-compile-retrieve-ccl-prog (table-syms nestedp)
  (if (mucs-ccl-inspect-facility 'valid-map-multiple)
      (if nestedp
	  `(loop
	    (r1 = 0)
	    (r4 = r0)
	    (map-multiple r1 r0 ,table-syms))
	`(loop
	  (r1 = 0)
	  (r4 = r0)
	  (iterate-multiple-map r1 r0 ,@table-syms)))
    (if nestedp
	`(loop
	  (r1 = 0)
	  (r4 = r0)
	  (loop
	   (map-multiple r1 r0 ,table-syms)
	   (if (r0 > ,(mucs-max-number))
	       ((if (r0 == ,(mucs-special-code 1))
		    ((r0 = r4)
		     (r1 += 1)
		     (repeat))
		  ((r0 += (r4 - ,(mucs-arithmetic-adjust)))
		   (r1 += 1)
		   (repeat)))))))
      `(loop
	(r1 = 0)
	(r4 = r0)
	(iterate-multiple-map r1 r0 ,@table-syms)
	(if (r0 > ,(mucs-max-number))
	    ((r0 += (r4 - ,(mucs-arithmetic-adjust)))))))))

;; API for compilation.

(defun tae-compile (name &optional decodep applied-type)
  "Retrieve MYO from the declared translation.
NAME must be a declared translation."
  (if (not (tae-declared-translation-p name))
      (error "Undeclared or invalid translation:%S" name))
  (let* ((tae-enable-register-translation
	  (if mucs-current-package
	      tae-enable-register-translation
	    nil))
	 (type (or applied-type
		   mucs-current-type))
	 (intdata (tae-compile-internal name decodep type))
	 (result-myo (car intdata))
	 (table-syms (nth 1 intdata))
	 (table-syms-nested-p (nth 2 intdata)))
    (mucs-ccl-myo-add-ccl
     (list
      (mucs-conversion-set-program-marker
       (tae-generate-unique-symbol-for-embed
	name type decodep)
       (tae-compile-retrieve-ccl-prog
	table-syms
	table-syms-nested-p)))
     result-myo)
    result-myo))

;;; API for modification

(defun tae-modify-translation (name rule)
  "Modify declared tralslation.(Dynamic modification)"
  (if (not (tae-translation-dynamic-p name))
      (error "%S cannot be modified dynamically!"
	     name))
  (let ((tae-enable-register-translation nil)
	(conversion-list (get name 'tae-dependent-conversions))
	intdata	slot conv root-tr mark type decodep)
    (set name rule)
    (mapcar
     (lambda (x)
       (put name x nil))
     '(tae-reduced-translation-for-encode
       tae-reduced-translation-for-decode
       tae-products-for-encode
       tae-products-for-decode))
    (while conversion-list
      (setq slot (car conversion-list)
	    conv (car slot)
	    root-tr (nth 1 slot)
	    mark (nth 2 slot)
	    type (nth 3 slot)
	    decodep (nth 4 slot)
	    conversion-list (cdr conversion-list))

      ;; reconstruct translation products.
      (tae-compile-internal name decodep type)
      ; flush generated products.
;       (put root-tr
; 	   (if decodep
; 	       'tae-products-for-decode
; 	     'tae-products-for-encode)
; 	   nil)
      (setq intdata (tae-compile-internal root-tr decodep type))
      ;; modify conversion.
      (mucs-modify-conversion
       conv mark
       (tae-compile-retrieve-ccl-prog
	(nth 1 intdata)
	(nth 2 intdata))))))

;;; slightly ad-hoc.
;;; We should separate mucs-ccl and setup part to other modules.
(defmacro tae-embed-for-dynamic-modification (name rule)
  (setq name (eval name)
	rule (eval rule))
  (if (not (tae-translation-dynamic-p name))
      (error "%S cannot be modified dynamically!"
	     name))
  (let ((conversion-list (get name 'tae-dependent-conversions))
	cdata-myo rdata-myo slot conv root-tr mark type decodep
	result)
    (set name rule)
    (mapcar
     (lambda (x)
       (put name x nil))
     '(tae-reduced-translation-for-encode
       tae-reduced-translation-for-decode
       tae-products-for-encode
       tae-products-for-decode))
    (while conversion-list
      (setq slot (car conversion-list)
	    conv (car slot)
	    root-tr (nth 1 slot)
	    mark (nth 2 slot)
	    type (nth 3 slot)
	    decodep (nth 4 slot)
	    conversion-list (cdr conversion-list))

      ;; reconstruct translation products.
      (setq cdata-myo
	    (car (tae-compile-internal name decodep type)))
      ; flush generated products.
;       (put root-tr
; 	   (if decodep
; 	       'tae-products-for-decode
; 	     'tae-products-for-encode)
; 	   nil)
      (setq rdata-myo
	    (car (tae-compile-internal root-tr decodep type)))
      (setq result
	    (append result
		    (mucs-ccl-make-elisp-preparation-from-myo
		     cdata-myo)
		    (mucs-ccl-make-elisp-preparation-from-myo
		     rdata-myo))))
    (cons 'progn
	  result)))

;; MAPPING FUNCTION
;;
;;  mapping functions are 
;;

;; this is a temporal function.

(defun tae-generate-union-func-map (funcs decodep)
  "Make a union function from a list of functions."
  (let (elem func-alist tables ctables)
    (while funcs
	(setq elem (car funcs)
	      funcs (cdr funcs))
	(cond ((table-set-p elem)
	       (setq ctables
		     (get-table-set-symbol-list-recursively
		      elem)
		     tables (append tables ctables)))
	       (t
		(error "Not yet supported type! %S" elem))))
	tables))

(defun tae-generate-func-to-func-map (funcs decodep &optional type)
  "Recieve a-list of (TAG . function)s.
Return a-list<CAR> (TAG . TABLE-NO(index of <CDR> list from 0)) and
a list<CDR> (TABLE<SYMBOL> TABLE<SYMBOL> ...)."
  (let ((idx 0) tbl-alist tbl-c
	elem elemt elemf func-alist tables)
    (while (setq elem (car funcs))
      (setq elemt (car elem)
	    elemf (cdr elem)
	    funcs (cdr funcs))
      (cond ((table-set-p elemf)
	     (if (setq tbl-c (assq elemt tbl-alist))
		 (setcdr tbl-c
			 (nconc
			  (cdr tbl-c)
			  (get-table-set-symbol-list-recursively
			   elemf)))
	       (setq tbl-alist
		     (nconc
		      tbl-alist
		      (list
		       (cons elemt
			     (get-table-set-symbol-list-recursively
			      elemf)))))))
	    (t
	     (error "Not yet supported type! %S" elem))))
    (while (setq elem (car tbl-alist))
      (setq elemt (car elem)
	    elemf (cdr elem)
	    tbl-alist (cdr tbl-alist)
	    func-alist (cons
			(cons elemt
			      (cond ((eq type 'symbol)
				     (car elemf))
				    (t idx)))
			func-alist)
	    idx (+ idx (length elemf) 1)
	    tables (append tables
			   (if tables
			       '(tae-cease-translation-table)
			     nil)
			   elemf)))
    (cons func-alist tables)))

;; Generative Functions
;;

(defun tae-function-union-1 (func1 func2))
(defun tae-function-union-2 (func1 func2))

(defun tae-function-intersection-1 (func1 func2))
(defun tae-function-intersection-2 (func1 func2))

(defun tae-function-compose-1 (func1 func2))
(defun tae-function-compose-2 (func1 func2))

(defun tae-function-to-function (func1 reg))

;;; generate registration program

(defun tae-generate-registration-program ()
  (let ((translation-list (tae-registered-translation-rule-list))
	tr result)
    (while (setq tr (car translation-list))
      ;; (message "TR:%S EB:%S SP:%S" tr (mucs-embedded-p 'tae-translation tr)
      (setq result
	    (append
	     (tae-generate-persistent-program
	      tr
	      (if (mucs-embedded-p 'tae-translation tr)
		  '(tae-products-for-encode
		    tae-products-for-decode)
		(mucs-notify-embedment 'tae-translation tr)
		(if (tae-translation-dynamic-p tr)
		    '(tae-translation-definition
		      tae-translation
		      tae-dynamic-translation
		      tae-products-for-encode
		      tae-products-for-decode
		      tae-dependent-conversions)
		  '(tae-translation
		    tae-products-for-encode
		    tae-products-for-decode))))
	     result)
	    translation-list
	    (cdr translation-list)))
    result))

;;; Add hook for embedding translation informations to a package.
(add-hook 'mucs-package-definition-end-hook
	  (function tae-generate-registration-program))

(provide 'tae)

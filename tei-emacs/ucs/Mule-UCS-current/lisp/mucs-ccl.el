;;; -*- byte-compile-dynamic: t -*-
;;; mucs-ccl.el --- Mule-UCS CCL manager.(MUCS-CCL)

;; Copyright (C) 1997-2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, CCL

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

(require 'mucs-type)
(require 'trans-util)
(require 'ccl)

;;; To cease noisy compiler warnings.
(eval-when-compile
  (if (not (boundp 'mucs-version))
      (require 'mucs)))

;;; ------ Some notes on MUCS-CCL. ------
;;; MUCS-CCL stands for MUle-uCS CCL generation library.
;;; This module provides the following services.
;;;
;;; 1 ... more abstracted description. (that has upper compatibility.)
;;; 2 ... supply basic CCL libraries.
;;; 3 ... manages the state to produce CCL codes.
;;;           This state is called `mucs-ccl-production-state'.
;;; 4 ... register the mucs-ccl-production-state
;;;                at the location marked by users.
;;; 5 ... produce the table structures that are used by ExCCL.
;;;

;;;
;;; MUCS-CCL internal coding system.
;;;

(defvar mucs-ccl-temporal-coding-system-ccl-symbol nil)

(defsubst mucs-ccl-define-temporal-coding-system (conv)
  (if (eq mucs-ccl-temporal-coding-system-ccl-symbol
	  conv)
      nil
    (register-ccl-program
     'mucs-ccl-tmp-ccl-prog
     (mucs-conversion-get conv 'mucs-compiled-code))
    (if (not (coding-system-p
	      'mucs-ccl-temporary))
	(make-coding-system
	 'mucs-ccl-temporary
	 4 ?*
	 "Temporary coding-system privately used by MUCS-CCL"
	 '(mucs-ccl-tmp-ccl-prog . mucs-ccl-tmp-ccl-prog)))))

;;; CCL facility checker.

(defvar mucs-ccl-facility-alist
  '((valid-map-multiple
     mucs-ccl-check-map-multiple-function
     "CCL_MapMultiple facility is invalid."
     none)
    (eol-automatic-conversion
     mucs-ccl-check-eol-automatic-conversion
     "Automatic conversion is enabled."
     none)))

(defun mucs-ccl-check-map-multiple-function ()
  (let ((vec (make-vector 8 0)))
    (register-code-conversion-map
     'mucs-ccl-test-map-1
     [10 10])
    (register-code-conversion-map
     'mucs-ccl-test-map-2
     [0 1])
    (ccl-execute
     (ccl-compile
      '(0 ((map-multiple
	    r1 r0 (((mucs-ccl-test-map-1)) mucs-ccl-test-map-2)))))
     vec)
    (if (= (aref vec 0) 1)
	(progn
;	  (message "!!! This CCL interpreter have the valid MapMultiple instruction.
;!!! We enable all features on MUCS-CCL.")
	  t)
      (message "!!! Sorry.  This CCL interpreter have faults on MapMultiple instruction.
!!! Some features on MUCS-CCL are disabled.")
      nil)))

(defun mucs-ccl-check-eol-automatic-conversion ()
  (define-ccl-program
    mucs-ccl-check-eol-ccl-prog
    '(2 ((read r0) (loop (write-read-repeat r0)))))
  (mucs-define-coding-system
   'mucs-ccl-eol-test
   ?*
   "test coding-system for ccl EOL conversion facility"
   'mucs-ccl-check-eol-ccl-prog 'mucs-ccl-check-eol-ccl-prog)
  (if (and (coding-system-p 'mucs-ccl-eol-test-dos)
	   (= (aref (encode-coding-string
		     "a\n" 'mucs-ccl-eol-test-dos) 1) ?\r))
      (progn
	(message "! This CCL interpreter has automatic EOL conversion facility.")
	t)
    (message
     "! This CCL interpreter does not have automatic EOL conversion facility.
No problem.  MUCS-CCL use its own EOL conversion facility instead.")
    nil))

(defun mucs-ccl-inspect-facility (required-facility)
  (let ((slot (assq required-facility mucs-ccl-facility-alist))
	result)
    (if (null slot)
	(error "Unknown facility:%S" required-facility))
    (setq result (nth 3 slot))
    (if (eq result 'none)
	(progn
	  (setq result (funcall (nth 1 slot)))
	  (setcar (nthcdr 3 slot) result)))
    result))

(defun mucs-ccl-get-facility-error-message (facility)
  (nth 2 (assq facility mucs-ccl-facility-alist)))

(defsubst mucs-ccl-facility-error (required-facility
				   &optional additional-message)
  (if (not (mucs-ccl-inspect-facility required-facility))
      (error (concat
	      "  !!! "
	      (mucs-ccl-get-facility-error-message
	       required-facility)
	      "\n"
	      additional-message
	      "\n"
	      "If you would like to use this facility, please apply the patch for CCL to
your Emacs, and MUST recompile ALL PACKAGES of Mule-UCS!
For detail, please refer README file of Mule-UCS"))))

;;;
;;; some simple ccl codes.
;;;

(defvar mucs-ccl-read-ex-1-octet
  `((read r0)))

(defvar mucs-ccl-read-ex-be-2-octet
  `((read r0 r4)
    (r0 = (r0 <8 r4))))

(defvar mucs-ccl-read-ex-le-2-octet
  `((read r0 r4)
    (r0 = (r4 <8 r0))))

(defvar mucs-ccl-write-ex-1-octet
  `((write r0)))

(defvar mucs-ccl-write-ex-be-2-octet
  `((write (r0 >> 8))
    (write (r0 & 255)))) ; #xFF

(defvar mucs-ccl-write-ex-le-2-octet
  `((write (r0 & 255)) ; #xFF
    (write (r0 >> 8))))

;;;
;;; generic type definition
;;;

;;;
;;  char-1 is a generic type, which is naturally mapped to Emacs internal
;;  character.  It can be represented by only one number
;; (so we call it char-"1").  In CCL, r0 register holds it.  In Elisp, a
;; number lisp object holds it.
;;
;;  If it is less than (1<<24)(mucs-char-1-ucs-area-start)
;;  +-----------------+-+------------------+-------------------+
;;  |  RESERVED(6bit) |0| CHARSET-ID(8bit) |  CODEPOINT(16bit) |
;;  +-----------------+-+------------------+-------------------+
;;
;; Otherwise, it means
;;  +-----------------+-+--------------------------------------+
;;  |  RESERVED(6bit) |1|      UCS-codepoint (24bit)           |
;;  +-----------------+-+--------------------------------------+
;;

(mucs-define-type
 'char-1
 (function char-1-elisp-representation)
 (function char-1-ccl-representation))

(defun mucs-ccl-write-char-1 ()
  (let ((basic-ccl-write-char-1
	 '((r1 = (r0 >> 16))
	   (r0 = (r0 & 65535)) ; #xFFFF
	   (if (r0 > 255) ; #xFF
	       ((r0 -= 32) ; #x20
		(r4 = ((r0 / 96) << 7))
		(r0 = (((r0 % 96) + r4) + 32)))) ; 32 is #x20
	   (write-multibyte-character r1 r0))))
    (if (charsetp 'mule-ucs-unicode-multichar)
	`((if (r0 < ,mucs-char-1-ucs-area-start)
	      ,basic-ccl-write-char-1
	    ;; mule-ucs-unicode-multichar-form.
	    ((r1 = ,(charset-id 'mule-ucs-unicode-multichar))
	     (r0 -= ,mucs-char-1-ucs-area-start)
	     (r4 = (((r0 >> 18) & 63) + 32))
	     (write-multibyte-character r1 r4)
	     (r4 = (((r0 >> 12) & 63) + 32))
	     (write-multibyte-character r1 r4)
	     (r4 = (((r0 >> 6) & 63) + 32))
	     (write-multibyte-character r1 r4)
	     (r4 = ((r0 & 63) + 32))
	     (write-multibyte-character r1 r4))))
      basic-ccl-write-char-1)))

(defun mucs-ccl-read-char-1 ()
  (let ((basic-ccl-read-char-1
	 '((if (r0 > 255) ; #xFF
	       ((r4 = (r0 & 127)) ; #x7F
		(r0 = (((r0 >> 7) * 96) + r4))
		(r0 |= (r1 << 16)))
	     ((r0 |= (r1 << 16)))))))
    (if (charsetp 'mule-ucs-unicode-multichar)
	`((read-multibyte-character r1 r0)
	  (loop
	    (if (r1 != ,(charset-id 'mule-ucs-unicode-multichar))
		,basic-ccl-read-char-1
	      ;; UCS format
	      ((read-multibyte-character r1 r4)
	       (if (r1 != ,(charset-id 'mule-ucs-unicode-multichar))
		   ((repeat)))
	       (r0 -= 32)
	       (r4 -= 32)
	       (r0 = ((r0 << 6) | r4))
	       (read-multibyte-character r1 r4)
	       (if (r1 != ,(charset-id 'mule-ucs-unicode-multichar))
		   ((repeat)))
	       (r4 -= 32)
	       (r0 = ((r0 << 6) | r4))
	       (read-multibyte-character r1 r4)
	       (if (r1 != ,(charset-id 'mule-ucs-unicode-multichar))
		   ((repeat)))
	       (r4 -= 32)
	       (r0 = (((r0 << 6) | r4) + ,mucs-char-1-ucs-area-start))))))
      `((read-multibyte-character r1 r0)
	,@basic-ccl-read-char-1))))

(mucs-type-register-serialization
 'char-1
 'emacs-mule
 (quote `(,(mucs-ccl-write-char-1)))
 (quote `(,(mucs-ccl-read-char-1))))

;;
;; Internal state management functions
;;
(defvar mucs-ccl-internal-state-usage
  (make-vector mucs-code-range-bits nil))

(defun mucs-ccl-internal-state-reserve (name bit)
  (if (not (symbolp name))
      (signal 'wrong-type-argument
	      (list 'symbolp name)))
  (if (>= bit (length mucs-ccl-internal-state-usage))
      (error "Bit %d is too large.(%s)" bit name))
  (let ((slot (aref mucs-ccl-internal-state-usage bit)))
    (if (and name slot
	     (not (eq name slot)))
	(error "Bit %d have been already reserved.(%s)" bit name))
    (aset mucs-ccl-internal-state-usage bit name)
    (if name
	(put name 'mucs-ccl-internal-state-bit bit)
      (if slot
	  (put slot
	       'mucs-ccl-internal-state-bit nil)))))

(defun mucs-ccl-set-internal-state (name state)
  "Return a block that sets internal state to STATE for NAME."
  (let ((bit (get name 'mucs-ccl-internal-state-bit)))
    (if (not bit)
	(error "%s is not registered for internal state!"
	       name))
    (if state
	`((r5 |= ,(lsh 1 bit)))
      `((r5 &= ,(logxor (mucs-number-mask) (lsh 1 bit)))))))

(defun mucs-ccl-check-internal-state (name)
  "Return a statement that checks internal state for NAME."
  `(r5 & ,(lsh 1
	       (or (get name 'mucs-ccl-internal-state-bit)
		   (error "%s is not registered for internal state!"
			  name)))))

;;
;; Dealing with line separator problem.
;;

(mucs-ccl-internal-state-reserve 'previous-cr-p 0)

(defun mucs-ccl-write-char-1-dos ()
  (if (mucs-ccl-inspect-facility 'eol-automatic-conversion)
      (mucs-ccl-write-char-1)
    `((if (r0 == ,(char-1-ccl-representation ?\r))
	  ((if ,(mucs-ccl-check-internal-state 'previous-cr-p)
	       ((write ,(char-1-ccl-representation ?\r)))
	     ,(mucs-ccl-set-internal-state 'previous-cr-p t)))
	((r4 = (r0 != ,(char-1-ccl-representation ?\n)))
	 (if (,(mucs-ccl-check-internal-state 'previous-cr-p) & r4)
	     ((write ,(char-1-ccl-representation ?\r))))
	 ,@(mucs-ccl-set-internal-state 'previous-cr-p nil)
	 ,@(mucs-ccl-write-char-1))))))

(mucs-type-register-serialization
 'char-1
 'emacs-mule-dos
 (quote `(,(mucs-ccl-write-char-1-dos)))
 'none)

;;;
;;; CCL program generator
;;;

;;
;;  MYO fundamental section.
;;

;; NOTE:
;;    MUCS-CCL defines a simple message to pass generated program
;; and data structure, which is called MYO.
;; The followings show the specification of MYO.
;;
;;    (myo . A-LIST)
;;          Special data structure for mucs-ccl.
;;          A-LIST has data generated by CCL generator.
;;          The reserved keys of the A-LIST are as
;;          follows(All of them are SYMBOL),
;;          and all MYO MUST have these keys.
;;          (a) ... ccl
;;                   generated CCL programs
;;          (b) ... elisp
;;                   Elisps that are supporsed to evaled
;;                   before CCL programs are registered.
;;          (c) ... table-set
;;                   Generated table-set that are supporsed to be
;;                   registered before CCL programs are registered.
;;
;;

(defsubst mucs-ccl-myo-p (data)
  (eq 'myo (car-safe data)))

(defsubst mucs-ccl-check-myo (data)
  (if (not (and (mucs-ccl-myo-p data)
		(assq 'elisp (cdr data))
		(assq 'ccl (cdr data))
		(assq 'table-set (cdr data))))
      (error "Invalid MYO:%S" data)))


(defsubst mucs-ccl-empty-myo ()
  (cons 'myo (list
	      (cons 'ccl nil)
	      (cons 'elisp nil)
	      (cons 'table-set nil))))

(defsubst mucs-ccl-myo-from-ccl (ccl)
  (cons 'myo (list (list 'ccl ccl)
		   (cons 'elisp nil)
		   (cons 'table-set nil))))

(defsubst mucs-ccl-myo-append (myo1 myo2)
  "Returns concatinated MYO1 and MYO2.
MYO1 and MYO2 may be destructed."
  (let ((alist1 (cdr myo1))
	(alist2 (cdr myo2))
	elem1 elem2)
    (while (setq elem1 (car alist1))
      (setq elem2 (assq (car elem1) alist2)
	    alist1 (cdr alist1))
      (if elem2
	  (progn
	    (setcdr elem1 (append (cdr elem1)
				  (cdr elem2)))
	    (setq alist2 (delq elem2 alist2)))))
    (nconc myo1 alist2)))

(defsubst mucs-ccl-myo-append-safe (myo1 myo2)
  "Returns concatinated MYO1 and MYO2.
MYO1 and MYO2 is never destructed."
  (let ((alist1 (cdr myo1))
	(alist2 (copy-sequence (cdr myo2)))
	result
	elem1 elem2)
    (while (setq elem1 (car alist1))
      (setq elem2 (assq (car elem1) alist2)
	    alist1 (cdr alist1))
      (if elem2
	  (progn
	    (setq result (cons
			  (append elem1
				  (cdr elem2))
			  result))
	    (setq alist2 (delq elem2 alist2)))
	(setq result (cons elem1
			   result))))
    (cons 'myo
	  (nconc result alist2))))


(defsubst mucs-ccl-myo-add-elisp (elisp myo)
  (let ((slot (assq 'elisp (cdr myo))))
    (setcdr slot (append (cdr slot) elisp))
    myo))

(defsubst mucs-ccl-myo-get-elisp (myo)
  (cdr (assq 'elisp (cdr myo))))

(defsubst mucs-ccl-myo-set-elisp (elisp myo)
  (setcdr (assq 'elisp (cdr myo)) elisp))


(defsubst mucs-ccl-myo-add-ccl (ccl myo)
  (let ((slot (assq 'ccl (cdr myo))))
    (setcdr slot (append (cdr slot) ccl))
    myo))

(defsubst mucs-ccl-myo-get-ccl (myo)
  (cdr (assq 'ccl (cdr myo))))

(defsubst mucs-ccl-myo-set-ccl (ccl myo)
  (setcdr (assq 'ccl (cdr myo)) ccl)
  myo)


(defsubst mucs-ccl-myo-add-table-set (table-set myo)
  (let ((slot (assq 'table-set (cdr myo))))
    (setcdr slot (append (cdr slot)
			 (list table-set)))
    myo))

(defsubst mucs-ccl-myo-get-table-set (myo)
  (cdr (assq 'table-set (cdr myo))))

(defsubst mucs-ccl-myo-set-table-set (ccl myo)
  (setcdr (assq 'table-set (cdr myo)) ccl))


;;
;; conversion configuration handler
;;

(defun mucs-ccl-bind-program (progs)
  (let ((myo (mucs-ccl-empty-myo))
	ret)
    (while progs
      (setq ret (car progs)
	    progs (cdr progs))
      (if (mucs-ccl-myo-p ret)
	  (setq myo (mucs-ccl-myo-append myo ret))
	(if (consp ret)
	    (setq myo (mucs-ccl-myo-add-ccl ret myo))
	  (if ret
	      (error "Unknown MUCS-CCL data:%S" ret)))))
    myo))

(defun mucs-ccl-stream-form (&rest args)
  (let ((myo (mucs-ccl-bind-program args)))
    (mucs-ccl-myo-set-ccl
     (list
      (append '(loop)
	      (mucs-ccl-myo-get-ccl myo)
	      '((repeat))))
     myo)))

;;
;; misc.
;;

(defun mucs-ccl-make-elisp-preparation-from-myo (myo)
  (append
   (mucs-ccl-myo-get-elisp myo)
   (if mucs-current-package
       (mapcar
	(function generate-table-set-registration-program)
	(mucs-ccl-myo-get-table-set myo)))))

;;;
;;; MUCS Conversion Engine API.
;;;

(defun mucs-convert (conversion object)
  (let ((ccl-object
	 (funcall
	  (mucs-type-get-ccl-representation
	   (car (mucs-conversion-get-conv-type conversion)))
	  object))
	(execute-vector
	 (make-vector 8 0)))
    (ccl-execute conversion
		 (cond ((numberp ccl-object)
			(aset execute-vector 0 ccl-object)
			execute-vector)
		       ((vectorp ccl-object)
			ccl-object)
		       (t
			(signal 'mucs-ccl-invalid-error
				(list object)))))
    (if (= (aref execute-vector 0) mucs-invalid-code)
	nil
      (funcall
       (mucs-type-get-elisp-representation
	(cdr (mucs-conversion-get-conv-type conversion)))
       (if (vectorp ccl-object)
	   execute-vector
	 (aref execute-vector 0))))))

(defun mucs-convert-region (start end conversion)
  "Convert the current region by specified conversion,
which is defined by `mucs-define-conversion'.
It returns the length of the encoded text."
  (interactive 
   (list (region-beginning)
	 (region-end)
	 (intern
	  (completing-read
	   "MUCS Conversion:"
	   (let (candlist)
	     (mapatoms
	      (lambda (x)
		(if (and (mucs-conversion-p x)
			 (eq (mucs-conversion-get-conv-type x)
			     'stream))
		    (setq candlist
			  (cons
			   (cons (symbol-name x) 0)
			   candlist)))))
	     candlist)))))
  (if (not (and (mucs-conversion-p conversion)
		(eq (mucs-conversion-get-conv-type
		     conversion)
		    'stream)))
      (error "%S is not MUCS conversion" conversion))
  (mucs-ccl-define-temporal-coding-system conversion)
  (encode-coding-region start end 'mucs-ccl-temporary))

(defun mucs-convert-string (string conversion &optional unibyte-p)
  "Convert STRING by specified conversion,
which is defined by `mucs-define-conversion'.
It returns the encoded text.

If UNIBYTE-P is non-nil, return a unibyte string as a result."
  (if (not (stringp string))
      (signal 'wrong-type-argument
	      (list 'stringp string)))
  (if (not (and (mucs-conversion-p conversion)
		(eq (mucs-conversion-get-conv-type
		     conversion)
		    'stream)))
      (error "%S is not MUCS conversion" conversion))
  (ccl-execute-on-string conversion (make-vector 9 nil)
			 string nil unibyte-p))

(defun mucs-ccl-setup-myo (config)
  (let ((myo (mucs-ccl-empty-myo))
	ret)
    (while config
      (setq ret (eval (car config))
	    config (cdr config))
      (if (consp ret)
	  (if (mucs-ccl-myo-p ret)
	      (setq myo (mucs-ccl-myo-append myo ret))
	    (setq myo (mucs-ccl-myo-add-ccl ret myo)))
	(if ret
	    (error "Invalid MUCS-CCL:%S" ret))))
    myo))

(defun mucs-setup-conversion (conv def)
  (let ((convtype (mucs-conversion-get-conv-type conv))
	(mucs-current-conversion conv)
	(mucs-current-type nil)
	(mag (mucs-conversion-definition-mag def))
	(config-main (mucs-conversion-definition-main-prog def))
	(config-eof (mucs-conversion-definition-eof-prog def))
	main-ccl-myo eof-ccl-myo
	ccl-program
	compiled-ccl)

    (if (consp convtype)
	(setq mucs-current-type (car convtype)))
    (setq main-ccl-myo (mucs-ccl-setup-myo config-main))
    (if (and (consp convtype)
	     (not (eq (cdr convtype) mucs-current-type)))
	(signal 'mucs-type-mismatch-error
		(cons mucs-current-type (cdr convtype))))
    (if (consp convtype)
	(setq mucs-current-type (car convtype)))
    (setq eof-ccl-myo (mucs-ccl-setup-myo config-eof))
;;; EOF CCL program is not executed when non-stream conversion.
;    (if (and (consp convtype)
;	     (not (eq (cdr convtype) mucs-current-type)))
;	(signal 'mucs-type-mismatch-error
;		(cons mucs-current-type (cdr convtype))))
    (setq ccl-program (list '\` ;;; for backquoting.
			    (list
			     mag
			     (mucs-ccl-myo-get-ccl main-ccl-myo)
			     (mucs-ccl-myo-get-ccl eof-ccl-myo))))
    (setq compiled-ccl (ccl-compile (eval ccl-program)))
    (mucs-conversion-set-program-and-compiled-code
     conv ccl-program compiled-ccl)
    (append
     (mucs-ccl-make-elisp-preparation-from-myo main-ccl-myo)
     (mucs-ccl-make-elisp-preparation-from-myo eof-ccl-myo)
     `((declare-ccl-program ,conv ,compiled-ccl)))))

(defun mucs-refresh-conversion (conv program)
  (let ((compiled-ccl (ccl-compile program)))
;    (message "modify %S!" conv)
    (register-ccl-program conv
			  compiled-ccl)
    (mucs-conversion-set-program-and-compiled-code
     conv program compiled-ccl)))

;;;
;;; MUCS-CCL standard library.
;;;

(defun mucs-ccl-if (expression true-part &optional false-part)
  (let (true-part-myo
	false-part-myo
	result-myo)
    (setq true-part-myo (mucs-ccl-bind-program (eval true-part)))
    (if false-part
	(progn
	  (setq false-part-myo (mucs-ccl-bind-program (eval false-part))
		result-myo (mucs-ccl-myo-append-safe
			    true-part-myo
			    false-part-myo))
	  (mucs-ccl-myo-set-ccl
	   `((if ,expression
		 ,(mucs-ccl-myo-get-ccl true-part-myo)
	       ,(mucs-ccl-myo-get-ccl false-part-myo)))
	   result-myo))
      (mucs-ccl-myo-set-ccl
       `((if ,expression
	     ,(mucs-ccl-myo-get-ccl true-part-myo)))
       true-part-myo))))

(put 'mucs-ccl-if 'lisp-indent-function 2)

(defun mucs-ccl-if-invalid (invalid-part &optional valid-part)
  (let (invalid-part-myo valid-part-myo result-myo)
    (setq invalid-part-myo (mucs-ccl-bind-program (eval invalid-part)))
    (if valid-part
	(progn
	  (setq valid-part-myo (mucs-ccl-bind-program (eval valid-part))
		result-myo (mucs-ccl-myo-append-safe
			    invalid-part-myo
			    valid-part-myo))
	  (mucs-ccl-myo-set-ccl
	   `((if (r0 == ,mucs-invalid-code)
		 ,(mucs-ccl-myo-get-ccl invalid-part-myo)
	       ,(mucs-ccl-myo-get-ccl valid-part-myo)))
	   result-myo))
      (mucs-ccl-myo-set-ccl
       `((if (r0 == ,mucs-invalid-code)
	     ,(mucs-ccl-myo-get-ccl invalid-part-myo)))
       invalid-part-myo))))

(defun mucs-ccl-if-valid (valid-part)
  (let ((valid-part-myo (mucs-ccl-bind-program (eval valid-part))))
    (mucs-ccl-myo-set-ccl
     `((if (r0 != ,mucs-invalid-code)
	   ,(mucs-ccl-myo-get-ccl valid-part-myo)))
     valid-part-myo)))

(defun mucs-ccl-if-invalid-repeat ()
  (mucs-ccl-if-invalid '(quote (((repeat))))))

;;
;; CCL registration APIs.
;;

(defun mucs-ccl-register-ccl-program (name ccl-block)
  (mucs-register-object 'mucs-ccl-required name)
;  (message "MCCLREG(%S):%S" name ccl-block)
  (put name
       'mucs-ccl-required-ccl-program
       ccl-block))

(defun mucs-ccl-generate-registration-program ()
  (let ((ccl-pgm-list
	 (mucs-unembedded-object-list 'mucs-ccl-required))
	name myo result)
    (while (setq name (car ccl-pgm-list))
      (setq myo (mucs-ccl-setup-myo
		 (get name 'mucs-ccl-required-ccl-program)))
      (setq result
	    (append
	     result
	     (mucs-ccl-make-elisp-preparation-from-myo myo)
	     `((register-ccl-program
		',name
		,(ccl-compile 
		  (list 0
			(mucs-ccl-myo-get-ccl myo)))))))
      (mucs-notify-embedment 'mucs-ccl-required name)
      (setq ccl-pgm-list (cdr ccl-pgm-list)))
;   (message "MCCLREGFIN:%S" result)
    `((setq mucs-ccl-facility-alist
	    (quote ,mucs-ccl-facility-alist))
      ,@result)))

;;; Add hook for embedding translation informations to a package.
(add-hook 'mucs-package-definition-end-hook
	  (function mucs-ccl-generate-registration-program))

;;
;; Common I/O interface.
;;

(defun mucs-ccl-read (type serialize)
  (setq mucs-current-type type)
  (mucs-ccl-bind-program
   (eval (mucs-type-get-serialize-method type serialize t))))

(defun mucs-ccl-write (serialize)
  (if mucs-current-type
      (mucs-ccl-if-valid
       (mucs-type-get-serialize-method
	mucs-current-type serialize nil))
    (error "Current TYPE is not set.  Cannot write.")))

;;
;; Short CCL routine.
;;

(defun mucs-ccl-add-program (reg add)
  (if (not (symbolp reg))
      (signal 'wrong-type-argument
	      (list 'symbolp reg)))
  (if (not (numberp add))
      (signal 'wrong-type-argument
	      (list 'numberp add)))
  (let ((sym
	 (intern (format "mucs-ccl-add-%S-%d" reg add))))
    (if (not (mucs-registered-p 'mucs-ccl-required sym))
	(progn
	  (mucs-ccl-register-ccl-program
	   sym
	   `((quote ((,reg += ,add)))))
	  (register-ccl-program
	   sym
	   (ccl-compile `(0 ((,reg += ,add)))))))
    sym))

(defun mucs-ccl-nop-program ()
  (let ((sym
	 (intern "mucs-ccl-nop")))
    (if (not (mucs-registered-p 'mucs-ccl-required sym))
	(progn
	  (mucs-ccl-register-ccl-program
	   sym
	   '((quote ())))
	  (register-ccl-program
	   sym
	   (ccl-compile `(0 ())))))
    sym))

(provide 'mucs-ccl)

;;; mccl-font.el --- Library for Mule-UCS CCL Font encoder.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual,
;;           character set, coding-system, CCL, font

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
(require 'mucs)
(require 'mucs-ccl)
(require 'mucs-type)

(defvar mccl-font-current-charset nil
  "This variable indicates the charset of current MUCS-CCL environment,
which is bound by mccl-font-encoder.")

;;; conversion type check

(defsubst mccl-font-check-conversion-type ()
  (if (not (eq (mucs-conversion-get-conv-type
		mucs-current-conversion)
	       'font))
      (signal 'mucs-conversion-type-mismatch-error
	      (list (mucs-conversion-get-conv-type
		     mucs-current-conversion)
		    'font))))

;;; Type definitions

(mucs-define-type
 'font-codepoint
 'identity
 'identity)

;;; Type conversions.

(defun mccl-font-flat-code-to-font-encoding (bytes)
  (setq mucs-current-type 'font-codepoint)
  (cond ((= bytes 1)
	 '((r1 = r0)))
	((= bytes 2)
	 '((r1 = (r0 >8 r0))
	   (r2 = r7)))
	(t
	 (error "Not support such bytes in font encoding:%S."
		bytes))))

(defun mccl-font-convert-font-encoding-internal (dimension)
  (setq mucs-current-type 'char-1)
  (cond ((= dimension 1)
	 '((r0 = ((r0 << 16) | r1))))
	((= dimension 2)
	 `((r0 <<= 16)
	   (r0 |= ((r1 * 96) + r2))))
	(t
	 (error "Unknown dimension:%S." dimension))))

(defun mccl-font-convert-font-encoding ()
  (mccl-font-check-conversion-type)
  (if (not (charsetp mccl-font-current-charset))
      (error "mccl-font-current-charset is not valid.%S"
	     mccl-font-current-charset))
  (mccl-font-convert-font-encoding-internal
   (charset-dimension mccl-font-current-charset)))

;
; Charset
;

(defvar charset-id-charset-list
  (let* ((chidlist
	  (sort
	   (mapcar
	    (lambda (cs)
	      (cons cs (charset-id cs)))
	    (charset-list))
	   (lambda (x y) (< (cdr x) (cdr y)))))
	 (i 0)
	 slot charset id result)
    (while (setq slot (car chidlist))
      (setq chidlist (cdr chidlist)
	    charset (car slot)
	    id (cdr slot))
      (while (< i id)
	(setq result (cons nil result)
	      i (1+ i)))
      (setq result (cons charset result)))
    (nreverse result)))

(defun mccl-font-encoder (charset-spec)
  (mccl-font-check-conversion-type)
  (setq charset-spec
	(sort (copy-sequence charset-spec)
	      (lambda (x y) (< (charset-id (car x))
			       (charset-id (car y))))))
  (let ((result-myo (mucs-ccl-empty-myo))
	(i 0) id
	charset-clist slot myo)
    (while (setq slot (car charset-spec))
      (setq id (charset-id (car slot))
	    charset-spec (cdr charset-spec))
      (while (< i id)
	(setq charset-clist (cons (cons nil i)
				  charset-clist)
	      i (1+ i)))
      (setq charset-clist (cons slot charset-clist)
	    i (1+ i)))
    (setq charset-clist (nreverse charset-clist))
    (mucs-ccl-myo-set-ccl
     `((branch
	r0
	,@(mapcar
	   (lambda (cs)
	     (cond ((charsetp (car cs))
		    (let ((mccl-font-current-charset (car cs)))
		      (setq myo
			    (mucs-ccl-bind-program (eval (cdr cs))))
		      (mucs-ccl-myo-append-safe
		       result-myo
		       myo)
		      (mucs-conversion-set-program-marker
		       (car cs)
		       (mucs-ccl-myo-get-ccl myo))))
;		      (append
;		       (mucs-ccl-myo-get-ccl myo)
;		       '((end))))
		   (t
		    (mucs-conversion-set-program-marker
		     (cdr cs)
		     (list 'loop nil)))))
;		    '((end)))))
	   charset-clist)))
     result-myo)
    result-myo))

(defmacro mccl-font-modify-font-encoder (conv charset spec)
  (setq conv (eval conv)
	charset (eval charset)
	spec (eval spec))
  (let ((mucs-current-conversion conv)
	(mccl-font-current-charset charset)
	mark slot myo ccl-prog)
    (mccl-font-check-conversion-type)
    (setq mark charset)
    (setq slot (mucs-retrieve-marked-conversion-program conv mark))
    (if (and (null slot)
	     (setq mark (charset-id charset))
	     (null (setq slot (mucs-retrieve-marked-conversion-program
			       conv mark))))
	(error "Cannot modify conversion:%S for charset:%S"
	       conv charset))
    (setq myo (mucs-ccl-bind-program (eval spec))
	  ccl-prog (mucs-ccl-myo-get-ccl myo))
    (mucs-substitute-conversion-program
     conv mark ccl-prog)
    (setq ccl-prog (list '\` (get conv 'mucs-conversion-program)))

    `(progn
       ,@(mucs-ccl-make-elisp-preparation-from-myo myo)
       (put (quote ,conv) 'mucs-conversion-program
	    ,ccl-prog)
       (register-ccl-program
	(quote ,conv)
	,(ccl-compile (eval ccl-prog)))
       nil)))


(provide 'mccl-font)

;;; -*- byte-compile-dynamic: t;coding: iso-2022-7bit -*-
;;; un-supple.el --- Supplemental translation rules for
;;;                  other conversions than Unicode Consortium's definition.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, ISO/IEC 10646,
;;           Unicode, JIS X 0221, JDK, Japanese-EUC, Windows.

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

;;; Comment:

(require 'un-define)

(defvar un-supple-current-translation-rule nil)

(eval-when-compile
  (mucs-require-data 'usupple))

(eval-and-compile
  (require 'tae)

  (defvar un-supple-translation-rule-alist
    '((jisx0221 . unicode-translation-rule-for-jisx0221)
      (jdk . unicode-translation-rule-for-jdk)
      (windows . unicode-translation-rule-for-windows)))

  (defun un-supple-reconstruct-tr-def (tr-def sup-tr-rule)
    (let* ((tr1 (copy-sequence tr-def))
	   (tr2 tr1)
	   (alist (cons
		   (cons 'ascii
			 (unicode-get-translation-rule-from-charset
			  'ascii))
		   un-supple-translation-rule-alist))
	   ins slot)
      (if (memq sup-tr-rule
		'(unicode-translation-rule-for-fullwidth-or-halfwidth-normalization))
	  (if (mucs-ccl-inspect-facility 'valid-map-multiple)
	      (list 'c
		    tr1
		    sup-tr-rule)
	    tr1)
	(while tr2
	  (if (setq ins (rassq (car tr2) alist))
	      (setq slot tr2
		    tr2 nil)
	    (setq tr2 (cdr tr2))))
	(if (null slot)
	    (error "Cannot find out apropriate location in %S"
		   tr-def))
	(cond ((eq (car ins) 'ascii)
	       (if sup-tr-rule
		   (setcdr slot (cons sup-tr-rule
				      (cdr slot)))))
	      ((null sup-tr-rule)
	       (setq tr1 (delq (cdr ins) tr1)))
	      (t
	       (setcar slot sup-tr-rule)))
	tr1)))

  (defmacro un-supple-embed-translation-macro (tr)
    `(tae-embed-for-dynamic-modification
      'unicode-basic-translation-rule
      (un-supple-reconstruct-tr-def
       (tae-get-translation-definition
	'unicode-basic-translation-rule)
       (quote ,tr)))))

(mucs-define-package
 un-supple
 (mucs-import-package un-define)
 (un-supple-embed-translation-macro
  unicode-translation-rule-for-jisx0221)
 (un-supple-embed-translation-macro
  unicode-translation-rule-for-jdk)
 (un-supple-embed-translation-macro
  unicode-translation-rule-for-windows)
 (un-supple-embed-translation-macro
  unicode-translation-rule-for-fullwidth-or-halfwidth-normalization))

;; interface functions

(defun un-supple-modify-translation-rule (sup-tr base-tr)
  (tae-modify-translation
   base-tr
   (un-supple-reconstruct-tr-def
    (tae-get-translation-definition base-tr)
    sup-tr)))

(defun un-supple-enable (sup)
  (let (tr)
    (if sup
	(progn
	  (setq tr
		(cdr (assq sup
			   un-supple-translation-rule-alist)))
	  (if (null tr)
	      (error "Unknown supplemental translation for %S" sup))))
    (un-supple-modify-translation-rule
     tr 'unicode-basic-translation-rule)))

(provide 'un-supple)

;;; un-supple ends here.

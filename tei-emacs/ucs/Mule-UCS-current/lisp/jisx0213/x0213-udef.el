;;; x0213-udef.el --- JIS X 0213 translations
;;;                   for Unicode conversions on Mule-UCS.

;; Copyright (C) 1999-2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, ISO/IEC 10646,
;;           Unicode, UTF-8, UTF-16

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

(require 'x0213-cdef)
(require 'tae)
(require 'un-define)

(eval-and-compile
  (defun jisx0213-add-charsets-to-list (clist)
    (if (and (not (memq 'japanese-jisx0213-1
			clist))
	     (not (memq 'japanese-jisx0213-2
			clist)))
	(let* ((jisx0208-slot
		(memq 'japanese-jisx0208
		      clist)))
	  (if jisx0208-slot
	      (setcdr jisx0208-slot
		      (cons 'japanese-jisx0213-1
			    (cons 'japanese-jisx0213-2
				  (cdr jisx0208-slot))))
	    (setq clist
		  (nconc clist
			 (list 'japanese-jisx0213-1
			       'japanese-jisx0213-2))))))
    clist)

  ;; Put symbol of translation rule for Unicode.
  (put 'japanese-jisx0213-1
       'translation-rule-for-unicode
       'unicode-japanese-jisx0213-1-translation-rule)
  (put 'japanese-jisx0213-2
       'translation-rule-for-unicode
       'unicode-japanese-jisx0213-2-translation-rule)
)

(eval-when-compile
  (require 'un-trbase)
  (require 'ujisx0213)

  (mapcar
   (lambda (charset)
     (let ((sym
	    (unicode-get-translation-rule-from-charset
	     charset)))
       (tae-declare-translation
	sym
	(symbol-value (get charset 'unicode-assoc)))
       sym))
   '(japanese-jisx0213-1 japanese-jisx0213-2)))

(mucs-define-package
 x0213-udef
 (mucs-import-package un-define)

 (tae-embed-for-dynamic-modification
  'unicode-basic-translation-rule
  (un-define-generate-basic-translation-rule
   (jisx0213-add-charsets-to-list
    unicode-basic-translation-charset-order-list))))

(setq unicode-basic-translation-charset-order-list
      (jisx0213-add-charsets-to-list
       unicode-basic-translation-charset-order-list))

(un-define-change-charset-order)

(provide 'x0213-udef)



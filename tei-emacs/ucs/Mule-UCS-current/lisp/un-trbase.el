;;; un-trbase.el --- Basic Translation rule for Unicode.

;; Copyright (C) 1999-2001 MIYASHITA Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, ISO/IEC 10646,
;;           Unicode.

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
;;;   Because this module only define translation rules,
;;;   this should not be byte-compiled alone.
;;;   When using this module, you should require this
;;;   in eval-when-compile form.

(require 'tae)
;(require 'unicode)
(or (featurep 'mule-ucs-unicode)
    (load-library "unicode"))
(require 'mule-uni)

;
; Replacement configuration.
;

(defvar mucs-unicode-default-decode-replacement ??)
(defvar mucs-unicode-default-encode-replacement 
  unicode-character-replacement)

;;
;; UCS replacement or ignore translation rule.
;;

(defvar unicode-ignore-characters-assoc
  `(assoc (char-1 . ucs-generic)
	  ( ,@(mapcar
	       (lambda (x)
		 (cons 'invalid x))
	       unicode-ignore-characters))))

(defvar unicode-replacement-assoc
  `(assoc (char-1 . ucs-generic)
	  ((all . ,mucs-unicode-default-encode-replacement)
	   (,mucs-unicode-default-decode-replacement . all)))
  "Translate any values to replacement character.
If you want to deal with untranslated character, use this translation rule.")

(defvar unicode-not-found-to-invalid-assoc
  '(assoc (char-1 . ucs-generic)
	  ((all . invalid)
	   (invalid . all)))
  "Translate any values to invalid code.
If you want to deal with untranslated character, use this translation rule.")

;;
;; Dealing with line separator problem.
;;

(defvar lf-vs-cr-assoc
  `(assoc (char-1 . ucs-generic)
	  ((?\n . ,unicode-cr))))

(defvar lf-vs-unicode-line-separator-assoc
  `(assoc (char-1 . ucs-generic)
	  ((?\r . ,unicode-line-separator))))

;
; Loader for parts of translation rule.
;

(defvar unicode-charset-library-alist
  '((ascii . uascii)
    (latin-iso8859-1 . uiso8859-1)
    (latin-iso8859-2 . uiso8859-2)
    (latin-iso8859-3 . uiso8859-3)
    (latin-iso8859-4 . uiso8859-4)
    (latin-iso8859-14 . uiso8859-14)
    (latin-iso8859-15 . uiso8859-15)
    (cyrillic-iso8859-5 . uiso8859-5)
    (arabic-iso8859-6 . uiso8859-6)
    (greek-iso8859-7 . uiso8859-7)
    (hebrew-iso8859-8 . uiso8859-8)
    (latin-iso8859-9 . uiso8859-9)
    (latin-jisx0201 . ujisx0201)
    (katakana-jisx0201 . ujisx0201)
    (japanese-jisx0208 . ujisx0208)
    (japanese-jisx0212 . ujisx0212)
    (chinese-gb2312 . ugb2312)
    (chinese-big5-1 . ubig5)
    (chinese-big5-2 . ubig5)
    (chinese-cns11643-1 . u-cns-1)
    (chinese-cns11643-2 . u-cns-2)
    (chinese-cns11643-3 . u-cns-3)
    (chinese-cns11643-4 . u-cns-4)
    (chinese-cns11643-5 . u-cns-5)
    (chinese-cns11643-6 . u-cns-6)
    (chinese-cns11643-7 . u-cns-7)
    (korean-ksc5601 . uksc5601)
    (ipa . uipa)
    (mule-unicode-0100-24ff . nil)
    (mule-unicode-2500-33ff . nil)
    (mule-unicode-e000-ffff . nil)
    (thai-tis620 . utis620)
    (indian-is13194 . uiscii)
    (ethiopic . uethiopic)
    (chinese-sisheng . usisheng)
    (vietnamese-viscii-lower . uviscii)
    (vietnamese-viscii-upper . uviscii)
    (lao . ulao)
    (tibetan . utibetan)))

(defun require-unicode-charset-data (charset)
  (let ((package (cdr (assq charset unicode-charset-library-alist))))
    (if package
	(or (featurep package)
	    (load (concat (file-name-as-directory mucs-data-path)
			  (symbol-name package))
		  t)
	    (require package)))))

;; load-table & declare basic translation rule.
;; translation definition start
;;; basic translation rule, which can be modified dynamically.
(tae-declare-translation
 'unicode-basic-translation-rule
 `(| ,@(mapcar
	(lambda (charset)
	  (let ((sym
		 (unicode-get-translation-rule-from-charset
		  charset)))
	    (require-unicode-charset-data charset)
	    (tae-declare-translation
	     sym
	     (symbol-value (get charset 'unicode-assoc))
	     t)
	    sym))
	unicode-basic-translation-charset-order-list))
 t
 t ;; for dynamic modification!
 )

;; below translations are static.

(tae-declare-translation
 'unicode-basic-stream-translation-rule
 `(| ,unicode-ignore-characters-assoc
     unicode-basic-translation-rule
     ,unicode-replacement-assoc) t)

(tae-declare-translation
 'unicode-basic-non-stream-translation-rule
 `(| unicode-basic-translation-rule
     ,unicode-not-found-to-invalid-assoc) t)

(tae-declare-translation
 'unicode-line-separator-translation-rule
 `(| ,lf-vs-unicode-line-separator-assoc
     unicode-basic-stream-translation-rule) t)

(tae-declare-translation
 'unicode-mac-line-separator-translation-rule
 `(| ,lf-vs-cr-assoc
     unicode-basic-stream-translation-rule) t)

;; translation rule definition ends here

(provide 'un-trbase)

;;; un-trbase ends here.

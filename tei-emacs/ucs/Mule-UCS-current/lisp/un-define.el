;;; -*- byte-compile-dynamic: t -*-
;;; un-define.el --- fundamental template to comprise UTF
;;;                  and Unicode conversions on Mule-UCS.

;; Copyright (C) 1999-2001 MIYASHITA Hisashi

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

;;; Comment:
;;;   Mule-UCS can apply many kind of conversions to Unicode.
;;;   However, we should define fundamental configurations to
;;;   realize Unicode utilization.
;;;   This module provide such facilities, that is APIs for
;;;   other applications to use Unicode, UTF, and so on.

;;; private charset definition.

(require 'mule-uni)

;;; Autoload section
(autoload 'ucs-to-char "unicode")
(autoload 'char-to-ucs "unicode")
(autoload 'tae-modify-translation "tae")
(autoload 'insert-ucs-character "unicode"
  "Insert character which is converted from give UCS codepoint."
  t)

;;; un-define post-read-conversion and pre-write-conversion.

(defvar un-define-enable-buffer-conversion t
  "* non-nil means the UTF-* coding-systems converts buffer contents
by calling post-read-conversion and pre-write-conversion functions.")

(defvar un-define-post-read-conversion-charsets-alist
  (append
   '((thai-tis620 . thai-post-read-conversion))
   (if (fboundp (function tibetan-post-read-conversion))
       '((tibetan . tibetan-post-read-conversion)))
   (if (fboundp 'lao-post-read-conversion)
       '((lao . lao-post-read-conversion)))
    ;; in-is13194-devanagari-post-read-conversion does not work correctly.
    ;; I disabled the below line.
    ;; '((indian-is13194 . in-is13194-devanagari-post-read-conversion)))
   ;; This is from post-Emacs 21.1.
   (if (fboundp 'in-is13194-post-read-conversion)
       ;; Post-Emacs 21.1 Unicode-based Indian implementation.
       '((indian-is13194 . in-is13194-post-read-conversion)))
   ))

(defvar un-define-pre-write-conversion-charsets-alist
  (append
   ;; Disabled because read-multibyte-character
   ;; decompose composite characters
   ;; '((thai-tis620 . thai-pre-write-conversion))
   (if (fboundp 'in-is13194-pre-write-conversion)
       ;; Post-Emacs 21.1 Unicode-based Indian implementation.
       '((indian-is13194 . in-is13194-pre-write-conversion)
	  (indian-2-column . in-is13194-pre-write-conversion))
     '((indian-is13194 . in-is13194-devanagari-pre-write-conversion)
       (indian-1-column . in-is13194-devanagari-pre-write-conversion)
       (indian-2-column . in-is13194-devanagari-pre-write-conversion)))
   (if (fboundp (function tibetan-pre-write-canonicalize-for-unicode))
       '((tibetan . tibetan-pre-write-canonicalize-for-unicode)))
   ;; Post Emacs 21.1:
   (if (fboundp 'lao-pre-write-conversion)
       '((lao . lao-pre-write-conversion)))))

(defun un-define-post-read-conversion (len)
  (if un-define-enable-buffer-conversion
      (let* ((curpos (point))
	     (charsets (find-charset-region curpos (+ curpos len)))
	     slot func applied)
	(while charsets
	  (setq slot (assq (car charsets)
			   un-define-post-read-conversion-charsets-alist)
		func (cdr slot)
		charsets (cdr charsets))
	  (if (and slot
		   (not (memq func applied)))
	      (progn
		(setq len (funcall func len)
		      applied (cons func applied)))))))
  len)

(defun un-define-pre-write-conversion (from to)
  (if un-define-enable-buffer-conversion
      (let ((charsets (if (stringp from)
			  (find-charset-string from)
			(find-charset-region from to)))
	    slot func applied)
	(while charsets
	  (setq slot (assq (car charsets)
			   un-define-pre-write-conversion-charsets-alist)
		func (cdr slot)
		charsets (cdr charsets))
	  (if (and slot
		   (not (memq func applied)))
	      (progn
		(funcall func from to)
		(setq applied (cons func applied)))))))
  nil)

;;; Required for compile, and also persistent section.
(eval-and-compile

;
; unicode basic translation charset list.
;
  (defvar unicode-basic-translation-charset-order-list
    (let ((cand
	   (append
	    '(ascii
	      latin-iso8859-1
	      latin-iso8859-2
	      latin-iso8859-3
	      latin-iso8859-4
	      cyrillic-iso8859-5
					;  arabic-iso8859-6
	      greek-iso8859-7
	      hebrew-iso8859-8
	      latin-iso8859-9
	      latin-iso8859-14
	      latin-iso8859-15
	      ipa
	      japanese-jisx0208
	      japanese-jisx0212
	      chinese-gb2312
	      chinese-cns11643-1 
	      chinese-cns11643-2
	      chinese-cns11643-3
	      chinese-cns11643-4
	      chinese-cns11643-5
	      chinese-cns11643-6
	      chinese-cns11643-7
	      chinese-big5-1
	      chinese-big5-2
	      korean-ksc5601
	      latin-jisx0201
	      katakana-jisx0201
	      thai-tis620
	      ethiopic
	      indian-is13194
	      chinese-sisheng
	      lao
	      vietnamese-viscii-lower
	      vietnamese-viscii-upper)
	    (if (fboundp
		 (function tibetan-pre-write-canonicalize-for-unicode))
		'(tibetan))
	    '(mule-unicode-0100-24ff
	      mule-unicode-2500-33ff
	      mule-unicode-e000-ffff
	      mule-ucs-unicode-multichar)))
	  elem result)
      (while (setq elem (car cand))
	(if (or (funcall-if-possible (function find-charset) elem)
		(funcall-if-possible (function charsetp) elem))
	    (setq result (cons elem result)))
	(setq cand (cdr cand)))
      (nreverse result))
    "*Charset order used by unicode-basic-translation.")

  (defvar un-define-safe-charsets-for-coding-systems
    (append
     unicode-basic-translation-charset-order-list
     '(indian-2-column)))

;
; Default translation rule symbol per charset.
;

; Make symbol of translation rule for Unicode.
  (mapcar
   (lambda (cs)
     (put cs
	  'translation-rule-for-unicode
	  (intern (format "unicode-%s-translation-rule" cs))))
   unicode-basic-translation-charset-order-list)

  (defun unicode-get-translation-rule-from-charset (charset)
    (or (get charset 'translation-rule-for-unicode)
	(error "Charset:%S has no translation rule for Unicode"
	       charset)))

;
; font encoder charset spec alist
;

  (defvar unicode-font-encoder-charset-spec-alist
    (mapcar
     (lambda (cs)
       (cons cs
	     (or (get cs 'mccl-font-encoder)
		 `(list
		   (mccl-font-convert-font-encoding)
		   (tae-compile 
		    (quote
		     ,(unicode-get-translation-rule-from-charset cs)))
		   (mccl-font-flat-code-to-font-encoding 2)))))
     unicode-basic-translation-charset-order-list))

;;
;; Configuration functions for convenience.
;;

  (defun un-define-decode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'ucs-generic read)
     (tae-compile tr t)
     (mucs-ccl-write write)))

  (defun un-define-encode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-1 read)
     (tae-compile tr nil)
     (mucs-ccl-write write)))

  (defun un-define-get-coding-system-alist
    (coding-category &optional mime-charset charsets)
    (append
     '((pre-write-conversion . un-define-pre-write-conversion)
       (post-read-conversion . un-define-post-read-conversion))
     (if mime-charset
	 (list (cons 'mime-charset mime-charset)))
     (if (and coding-category
	      (get coding-category 'coding-category-index))
	 (list (cons 'coding-category coding-category)))
     (list (cons 'safe-charsets
		 (or charsets
		     un-define-safe-charsets-for-coding-systems)))))
  )

(eval-when-compile

  ;; predefined configuration
  ;; Any settings in here affect only
  ;; byte-compiled file.
  (require 'un-trbase)
  (require 'mccl-font)
  (require 'utf))

(mucs-define-package
 un-define

;;
;; conversion definition.
;;

;;; For stream conversion.

; UTF-8 conversions

 (mucs-define-conversion
  utf-8-unix-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule 'emacs-mule 'utf-8))))
 (mucs-define-conversion
  utf-8-unix-stream-decoder
  stream
  (utf-8-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule 'utf-8 'emacs-mule))))

 (mucs-define-conversion
  utf-8-dos-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule 'emacs-mule 'utf-8-dos))))
 (mucs-define-conversion
  utf-8-dos-stream-decoder
  stream
  (utf-8-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule 'utf-8 'emacs-mule-dos))))

 (mucs-define-conversion
  utf-8-mac-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-8))))
 (mucs-define-conversion
  utf-8-mac-stream-decoder
  stream
  (utf-8-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-mac-line-separator-translation-rule
     'utf-8 'emacs-mule))))

; UTF-8 with signature conversions

 (mucs-define-conversion
  utf-8-ws-unix-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((mucs-ccl-write-utf-8-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-8))))

 (mucs-define-conversion
  utf-8-ws-dos-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((mucs-ccl-write-utf-8-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-8-dos))))

 (mucs-define-conversion
  utf-8-ws-mac-stream-encoder
  stream
  (utf-8-encode-buffer-magnification
   ((mucs-ccl-write-utf-8-signature)
    (un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-8))))

; UTF-16-le conversions

 (mucs-define-conversion
  utf-16-le-unix-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-le-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-no-signature-unix-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-unix-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule
     'utf-16-le 'emacs-mule))))

 (mucs-define-conversion
  utf-16-le-dos-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-le-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-le-dos))))
 (mucs-define-conversion
  utf-16-le-no-signature-dos-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-le-dos))))
 (mucs-define-conversion
  utf-16-le-dos-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule
     'utf-16-le 'emacs-mule-dos))))

 (mucs-define-conversion
  utf-16-le-mac-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-le-signature)
    (un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-no-signature-mac-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-mac-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-mac-line-separator-translation-rule
     'utf-16-le 'emacs-mule))))

 ;; unicode line separator(the abberiviation is ul.)
 (mucs-define-conversion
  utf-16-le-ul-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-le-signature)
    (un-define-encode-template
     'unicode-line-separator-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-no-signature-ul-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-line-separator-translation-rule
     'emacs-mule 'utf-16-le))))
 (mucs-define-conversion
  utf-16-le-ul-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-line-separator-translation-rule
     'utf-16-le 'emacs-mule))))

; UTF-16-be conversions

 (mucs-define-conversion
  utf-16-be-unix-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-be-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-no-signature-unix-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-unix-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule
     'utf-16-be 'emacs-mule))))

 (mucs-define-conversion
  utf-16-be-dos-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-be-signature)
    (un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-be-dos))))
 (mucs-define-conversion
  utf-16-be-no-signature-dos-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-16-be-dos))))
 (mucs-define-conversion
  utf-16-be-dos-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule
     'utf-16-be 'emacs-mule-dos))))

 (mucs-define-conversion
  utf-16-be-mac-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-be-signature)
    (un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-no-signature-mac-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-mac-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-mac-line-separator-translation-rule
     'utf-16-be 'emacs-mule))))

 ;; unicode line separator(the abberiviation is ul.)
 (mucs-define-conversion
  utf-16-be-ul-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((mucs-ccl-write-utf-16-be-signature)
    (un-define-encode-template
     'unicode-line-separator-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-no-signature-ul-stream-encoder
  stream
  (utf-16-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-line-separator-translation-rule
     'emacs-mule 'utf-16-be))))
 (mucs-define-conversion
  utf-16-be-ul-stream-decoder
  stream
  (utf-16-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-line-separator-translation-rule
     'utf-16-be 'emacs-mule))))

; UTF-7 conversions

 (mucs-define-conversion
  utf-7-unix-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule 'emacs-mule 'utf-7))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-safe-unix-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-7-safe))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-unix-stream-decoder
  stream
  (utf-7-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule 'utf-7 'emacs-mule))))

 (mucs-define-conversion
  utf-7-dos-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule 'emacs-mule 'utf-7-dos))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-safe-dos-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-basic-stream-translation-rule
     'emacs-mule 'utf-7-safe-dos))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-dos-stream-decoder
  stream
  (utf-7-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-basic-stream-translation-rule 'utf-7 'emacs-mule-dos))))

 (mucs-define-conversion
  utf-7-mac-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-7))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-safe-mac-stream-encoder
  stream
  (utf-7-encode-buffer-magnification
   ((un-define-encode-template
     'unicode-mac-line-separator-translation-rule
     'emacs-mule 'utf-7-safe))
   ((mucs-ccl-utf-7-encode-eof))))
 (mucs-define-conversion
  utf-7-mac-stream-decoder
  stream
  (utf-7-decode-buffer-magnification
   ((un-define-decode-template
     'unicode-mac-line-separator-translation-rule
     'utf-7 'emacs-mule))))

;;; For non-stream conversion.

 (mucs-define-conversion
  emacs-char-to-ucs-codepoint-conversion
  (char-1 . ucs-generic)
  (0 ((tae-compile 'unicode-basic-non-stream-translation-rule nil))))

 (mucs-define-conversion
  ucs-codepoint-to-emacs-char-conversion
  (ucs-generic . char-1)
  (0 ((tae-compile 'unicode-basic-non-stream-translation-rule t))))

;;; For font encoder

 (mucs-define-conversion
  unicode-font-encoder
  font
  (0 ((mccl-font-encoder
       unicode-font-encoder-charset-spec-alist))))

;;
;; coding-system-definition.
;;

 (mapcar
  (lambda (x)
    (if (fboundp 'register-char-codings)
	;; Mule 5, where we don't need the eol-type specified and
	;; register-char-codings may be very slow for these coding
	;; system definitions.
	(let ((y (cadr x)))
	  (mucs-define-coding-system
	   (car x) (nth 1 y) (nth 2 y)
	   (nth 3 y) (nth 4 y) (nth 5 y)))
      (mapcar
       (lambda (y)
	 (mucs-define-coding-system
	  (nth 0 y) (nth 1 y) (nth 2 y)
	  (nth 3 y) (nth 4 y) (nth 5 y) (nth 6 y))
	 (coding-system-put (car y) 'alias-coding-systems (list (car x))))
       (cdr x))))
  `((utf-8
     (utf-8-unix
      ?u "UTF-8 coding system"
      utf-8-unix-stream-decoder utf-8-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-8 'utf-8)
      unix)
     (utf-8-dos
      ?u "UTF-8 coding system"
      utf-8-dos-stream-decoder utf-8-dos-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-8 'utf-8)
      dos)
     (utf-8-mac
      ?u "UTF-8 coding system"
      utf-8-mac-stream-decoder utf-8-mac-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-8 'utf-8)
      mac)
     (utf-8
      ?u "UTF-8 coding system"
      utf-8-unix-stream-decoder utf-8-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-8 'utf-8)
      [utf-8-unix utf-8-dos utf-8-mac]))

;    (utf-8-ws
;     (utf-8-ws-unix
;      ?u "UTF-8 with signature coding system"
;      utf-8-unix-stream-decoder utf-8-ws-unix-stream-encoder
;      ,(un-define-get-coding-system-alist
;	'coding-category-utf-8 'utf-8)
;      unix)
;     (utf-8-ws-dos
;      ?u "UTF-8 with signature coding system"
;      utf-8-dos-stream-decoder utf-8-ws-dos-stream-encoder
;      ,(un-define-get-coding-system-alist
;	'coding-category-utf-8 'utf-8)
;      dos)
;     (utf-8-ws-mac
;      ?u "UTF-8 with signature coding system"
;      utf-8-mac-stream-decoder utf-8-ws-mac-stream-encoder
;      ,(un-define-get-coding-system-alist
;	'coding-category-utf-8 'utf-8)
;      mac)
;     (utf-8-ws
;      ?u "UTF-8 with signature coding system"
;      utf-8-unix-stream-decoder utf-8-ws-unix-stream-encoder
;      ,(un-define-get-coding-system-alist
;	'coding-category-utf-8 'utf-8)
;      [utf-8-ws-unix utf-8-ws-dos utf-8-ws-mac]))

    (utf-16-le
     (utf-16-le-unix
      ?U "UTF-16 Little Endian coding system"
      utf-16-le-unix-stream-decoder utf-16-le-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      unix)
     (utf-16-le-dos
      ?U "UTF-16 Little Endian coding system"
      utf-16-le-dos-stream-decoder utf-16-le-dos-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      dos)
     (utf-16-le-mac
      ?U "UTF-16 Little Endian coding system"
      utf-16-le-mac-stream-decoder utf-16-le-mac-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      mac)
     (utf-16-le
      ?U "UTF-16 Little Endian coding system(Line serparator is U+2028)"
      utf-16-le-ul-stream-decoder utf-16-le-ul-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      [utf-16-le-unix utf-16-le-dos utf-16-le-mac]))

    (utf-16-le-no-signature
     (utf-16-le-no-signature-unix
      ?U "Same as utf-16-le but generate no Unicode signature."
      utf-16-le-unix-stream-decoder
      utf-16-le-no-signature-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      unix)
     (utf-16-le-no-signature-dos
      ?U "Same as utf-16-le but generate no Unicode signature."
      utf-16-le-dos-stream-decoder
      utf-16-le-no-signature-dos-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      dos)
     (utf-16-le-no-signature-mac
      ?U "Same as utf-16-le but generate no Unicode signature."
      utf-16-le-mac-stream-decoder
      utf-16-le-no-signature-mac-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      mac)
     (utf-16-le-no-signature
      ?U "Same as utf-16-le but generate no Unicode signature."
      utf-16-le-ul-stream-decoder
      utf-16-le-no-signature-ul-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-le)
      [utf-16-le-no-signature-unix
       utf-16-le-no-signature-dos
       utf-16-le-no-signature-mac]))

    (utf-16-be
     (utf-16-be-unix
      ?U "UTF-16 Big Endian coding system"
      utf-16-be-unix-stream-decoder utf-16-be-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-be)
      unix)
    (utf-16-be-dos
     ?U "UTF-16 Big Endian coding system"
     utf-16-be-dos-stream-decoder utf-16-be-dos-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     dos)
    (utf-16-be-mac
     ?U "UTF-16 Big Endian coding system"
     utf-16-be-mac-stream-decoder utf-16-be-mac-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     mac)
    (utf-16-be
     ?U "UTF-16 Big Endian coding system(Line serparator is U+2028)"
     utf-16-be-ul-stream-decoder utf-16-be-ul-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     [utf-16-be-unix utf-16-be-dos utf-16-be-mac]))

    (utf-16-be-no-signature
     (utf-16-be-no-signature-unix
      ?U "Same as utf-16-be but generate no Unicode signature."
      utf-16-be-unix-stream-decoder
      utf-16-be-no-signature-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-utf-16-be)
      unix)
    (utf-16-be-no-signature-dos
     ?U "Same as utf-16-be but generate no Unicode signature."
     utf-16-be-dos-stream-decoder
     utf-16-be-no-signature-dos-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     dos)
    (utf-16-be-no-signature-mac
     ?U "Same as utf-16-be but generate no Unicode signature."
     utf-16-be-mac-stream-decoder
     utf-16-be-no-signature-mac-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     mac)
    (utf-16-be-no-signature
     ?U "Same as utf-16-be but generate no Unicode signature."
     utf-16-be-ul-stream-decoder
     utf-16-be-no-signature-ul-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-utf-16-be)
     [utf-16-be-no-signature-unix
      utf-16-be-no-signature-dos
      utf-16-be-no-signature-mac]))

    (utf-7
     (utf-7-unix
      ?7 "UTF-7 coding system.  Output Set O characters directly."
      utf-7-unix-stream-decoder utf-7-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-ccl 'utf-7)
      unix)
     (utf-7-dos
      ?7 "UTF-7 coding system.  Output Set O characters directly."
      utf-7-dos-stream-decoder utf-7-dos-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-ccl 'utf-7)
      dos)
     (utf-7-mac
      ?7 "UTF-7 coding system.  Output Set O characters directly."
      utf-7-mac-stream-decoder utf-7-mac-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-ccl 'utf-7)
      mac)
     (utf-7
      ?7 "UTF-7 coding system.  Output Set O characters directly."
      utf-7-unix-stream-decoder utf-7-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-ccl 'utf-7)
      [utf-7-unix utf-7-dos utf-7-mac]))

    (utf-7-safe
     (utf-7-safe-unix
      ?7 "UTF-7 coding system.  Shift Set O characters."
      utf-7-unix-stream-decoder utf-7-safe-unix-stream-encoder
      ,(un-define-get-coding-system-alist
	'coding-category-ccl 'utf-7)
      unix)
    (utf-7-safe-dos
     ?7 "UTF-7 coding system.  Shift Set O characters."
     utf-7-dos-stream-decoder utf-7-safe-dos-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-ccl 'utf-7)
     dos)
    (utf-7-safe-mac
     ?7 "UTF-7 coding system.  Shift Set O characters."
     utf-7-mac-stream-decoder utf-7-safe-mac-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-ccl 'utf-7)
     mac)
    (utf-7-safe
     ?7 "UTF-7 coding system.  Shift Set O characters."
     utf-7-unix-stream-decoder utf-7-safe-unix-stream-encoder
     ,(un-define-get-coding-system-alist
       'coding-category-ccl 'utf-7)
     [utf-7-safe-unix utf-7-safe-dos utf-7-safe-mac]))
    ))

) ;; un-define package definition ends here


;;; font encoder setup
(add-to-list
 'font-ccl-encoder-alist
 '("iso10646" . unicode-font-encoder))

; font encoder setup (for Meadow)
(if (featurep 'meadow)
    (w32-regist-font-encoder
     'unicode-font-encoder 'unicode-font-encoder))

;;; coding-category setup
(let (category-list)
;; set up coding-category
  (if (boundp 'coding-category-utf-8)
      (setq coding-category-utf-8 'utf-8
	    category-list (cons 'coding-category-utf-8
				category-list)))
  (if (boundp 'coding-category-utf-16-be)
      (setq coding-category-utf-16-be 'utf-16-be
	    category-list (cons 'coding-category-utf-16-be
				category-list)))
  (if (boundp 'coding-category-utf-16-le)
      (setq coding-category-utf-16-le 'utf-16-le
	    category-list (cons 'coding-category-utf-16-le
				category-list)))
;; setup detect coding priority.
  (if category-list
      (progn
	(set-coding-priority
	 category-list)
	(add-hook 'set-language-environment-hook
		  `(lambda ()
		     (set-coding-priority (quote ,category-list)))))))

;
; Charset order dynamic modification. (Very simple version!)
;

(defun un-define-generate-basic-translation-rule (order-list)
  `(| ,@(mapcar
	 (function unicode-get-translation-rule-from-charset)
	 order-list)))

;;;###autoload
(defun un-define-change-charset-order (&optional order-list)
  "Change UCS to Mule charset conversion priority.

ORDER-LIST is a list of charsets.  When translating a UCS character to a
Mule character, the first charset in the list which contains the character
is used.  ORDER-LIST may be abbreviated to the charsets of interest.  In
this case, charsets in ORDER-LIST are given highest priority in that order,
followed by any charsets not mentioned, with the same relative order as in
the current priority list.

Use `list-character-sets' to get a list of character sets."
  (if (null order-list)
      (setq order-list
	    unicode-basic-translation-charset-order-list))
  (tae-modify-translation
   'unicode-basic-translation-rule
   (un-define-generate-basic-translation-rule
    order-list)))

(provide 'un-define)

;;; un-define ends here.

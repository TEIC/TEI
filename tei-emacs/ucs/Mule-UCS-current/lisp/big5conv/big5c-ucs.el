;;; -*- coding: iso-2022-7bit  -*-
;;; big5c-ucs.el --- Definition module for conversion of BIG5
;;;                  via UCS representation.

;; Copyright (C) 1999 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           MULE-UCS, Big5, UCS, Traditional Chinese

;; This file is part of MULE-UCS

;; MULE-UCS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; MULE-UCS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'un-define)

(defvar big5-conv-ucs-coding-system-alist
  '((safe-charsets .
		   (ascii
		    chinese-cns11643-1
		    chinese-cns11643-2
;;		    japanese-jisx0208
;;		    chinese-gb2312
		    ))
    (mime-charset . cn-big5))
  "An alist for big5-conv coding systems.")

(eval-when-compile
  (require 'mucs)
  (require 'un-data)
  (require 'un-trbase)
  (mucs-require-supplement 'big5conv 'big5c-ucs)

  (tae-declare-translation
   'big5-conv-ucs-external-translation
   `(| ,@(mapcar
	  (function
	   unicode-get-translation-rule-from-charset)
	  '(ascii chinese-big5-1 chinese-big5-2))))

  (tae-declare-translation
   'big5-conv-ucs-translation
   `(| ,@(mapcar
	  (function
	   unicode-get-translation-rule-from-charset)
	  (sort (copy-sequence
		 unicode-basic-translation-charset-order-list)
		(lambda (&rest cls)
		  (apply (function <)
			 (mapcar
			  (lambda (x)
			    (cond ((eq x 'ascii)
				   0)
				  ((memq x '(chinese-big5-1
					     chinese-big5-2))
				   100)
				  ((memq x '(chinese-cns11643-1
					     chinese-cns11643-2
					     chinese-cns11643-3
					     chinese-cns11643-4
					     chinese-cns11643-5
					     chinese-cns11643-6
					     chinese-cns11643-7))
				   1)
				  (t 10)))
			  cls))))))
   nil
   t) ;;; for dynamic modification.

  (tae-declare-translation
   'big5-conv-ucs-mac-external-translation
   `(| (assoc (char-1 . ucs-generic)
	      ((?\n . ,unicode-cr)))
       big5-conv-ucs-external-translation))

  (defun big5-conv-ucs-decode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-1 read)
     (tae-compile tr nil)
     (tae-compile 'big5-conv-ucs-translation t)
     (mucs-ccl-write write)))

  (defun big5-conv-ucs-encode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-1 read)
     (tae-compile 'big5-conv-ucs-translation nil)
     (tae-compile tr t)
     (mucs-ccl-write write)))
  )

;;
;; coding system definitions.
;;

(mucs-define-package
 big5c-ucs

 (mucs-import-package un-define)

;;
;; conversion definition.
;;

;;; For stream conversion.

;; for big5-conv-unix
 (mucs-define-conversion
  big5-conv-ucs-unix-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-ucs-encode-template
     'big5-conv-ucs-external-translation 'emacs-mule 'big5-char))))
 (mucs-define-conversion
  big5-conv-ucs-unix-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-ucs-decode-template
     'big5-conv-ucs-external-translation 'big5-char 'emacs-mule))))

;; for big5-conv-dos
 (mucs-define-conversion
  big5-conv-ucs-dos-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-ucs-encode-template
     'big5-conv-ucs-external-translation 'emacs-mule 'big5-char-dos))))
 (mucs-define-conversion
  big5-conv-ucs-dos-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-ucs-decode-template
     'big5-conv-ucs-external-translation 'big5-char 'emacs-mule-dos))))

;; for big5-conv-mac
 (mucs-define-conversion
  big5-conv-ucs-mac-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-ucs-encode-template
     'big5-conv-ucs-mac-external-translation 'emacs-mule 'big5-char))))
 (mucs-define-conversion
  big5-conv-ucs-mac-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-ucs-decode-template
     'big5-conv-ucs-mac-external-translation 'big5-char 'emacs-mule))))

;;
;; coding-system-definition.
;;
 (mucs-define-coding-system
  'big5-conv-unix ?B
  "Big5 coding system which convert Big5 characters via UCS representation."
  'big5-conv-ucs-unix-stream-decoder 'big5-conv-ucs-unix-stream-encoder
  big5-conv-ucs-coding-system-alist
  'unix)

 (mucs-define-coding-system
  'big5-conv-dos ?B
  "Big5 coding system which convert Big5 characters via UCS representation."
  'big5-conv-ucs-dos-stream-decoder 'big5-conv-ucs-dos-stream-encoder
  big5-conv-ucs-coding-system-alist
  'dos)

 (mucs-define-coding-system
  'big5-conv-mac ?B
  "Big5 coding system which convert Big5 characters via UCS representation."
  'big5-conv-ucs-mac-stream-decoder 'big5-conv-ucs-mac-stream-encoder
  big5-conv-ucs-coding-system-alist
  'mac)

 (mucs-define-coding-system
  'big5-conv ?B
  "Big5 coding system which convert Big5 characters via UCS representation."
  'big5-conv-ucs-unix-stream-decoder 'big5-conv-ucs-unix-stream-encoder
  big5-conv-ucs-coding-system-alist
  [big5-conv-unix big5-conv-dos big5-conv-mac])

 (mapcar
  (lambda (x)
    (coding-system-put x 'alias-coding-systems '(big5-conv)))
  '(big5-conv big5-conv-dos big5-conv-unix big5-conv-mac))

) ;; big5-cns package definition ends here

;; set up coding category
(if (boundp 'coding-category-big5)
    (setq coding-category-big5 'big5-conv))

(provide 'big5c-ucs)

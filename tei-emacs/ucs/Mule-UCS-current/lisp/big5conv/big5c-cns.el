;;; -*- coding: iso-2022-7bit  -*-
;;; big5c-cns.el --- Definition module for conversion of BIG5
;;;                  directly mapped to mainly CNS charsets.

;; Copyright (C) 1999 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           MULE-UCS, Big5, CNS, Traditional Chinese

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

(defvar big5-conv-cns-coding-system-alist
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
  (mucs-require-supplement 'big5conv 'big5c-cns)

  (tae-declare-translation
   'big5-conv-cns-translation
   `(| ,big5conv-ascii-assoc
       ,big5conv-emacs-char-1-vs-big5-assoc))

  (tae-declare-translation
   'big5-conv-cns-mac-translation
   `(| (assoc (char-1 . big5)
	      ((?\x0A . ?\x0D)))
       big5-conv-cns-translation))

  (defun big5-conv-cns-decode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'big5 read)
     (tae-compile tr t)
     (mucs-ccl-write write)))

  (defun big5-conv-cns-encode-template (tr read write)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-1 read)
     (tae-compile tr nil)
     (mucs-ccl-write write)))

  )

;;
;; coding system definitions.
;;

(mucs-define-package
 big5c-cns

;;
;; conversion definition.
;;

;;; For stream conversion.

;; for big5-conv-unix
 (mucs-define-conversion
  big5-conv-cns-unix-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-cns-encode-template
     'big5-conv-cns-translation 'emacs-mule 'big5-be-2-octet))))
 (mucs-define-conversion
  big5-conv-cns-unix-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-cns-decode-template
     'big5-conv-cns-translation 'big5-be-2-octet 'emacs-mule))))

;; for big5-conv-dos
 (mucs-define-conversion
  big5-conv-cns-dos-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-cns-encode-template
     'big5-conv-cns-translation 'emacs-mule 'big5-be-2-octet-dos))))
 (mucs-define-conversion
  big5-conv-cns-dos-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-cns-decode-template
     'big5-conv-cns-translation 'big5-be-2-octet 'emacs-mule-dos))))

;; for big5-conv-mac
 (mucs-define-conversion
  big5-conv-cns-mac-stream-encoder
  stream
  (big5conv-encode-buffer-magnification
   ((big5-conv-cns-encode-template
     'big5-conv-cns-mac-translation 'emacs-mule 'big5-be-2-octet))))
 (mucs-define-conversion
  big5-conv-cns-mac-stream-decoder
  stream
  (big5conv-decode-buffer-magnification
   ((big5-conv-cns-decode-template
     'big5-conv-cns-mac-translation 'big5-be-2-octet 'emacs-mule))))

;;
;; coding-system-definition.
;;
 (mucs-define-coding-system
  'big5-conv-unix ?B
  "Big5 coding system which convert Big5 characters to mainly CNS ones."
  'big5-conv-cns-unix-stream-decoder
  'big5-conv-cns-unix-stream-encoder
  big5-conv-cns-coding-system-alist
  'unix)

 (mucs-define-coding-system
  'big5-conv-dos ?B
  "Big5 coding system which convert Big5 characters to mainly CNS ones."
  'big5-conv-cns-dos-stream-decoder
  'big5-conv-cns-dos-stream-encoder
  big5-conv-cns-coding-system-alist
  'dos)

 (mucs-define-coding-system
  'big5-conv-mac ?B
  "Big5 coding system which convert Big5 characters to mainly CNS ones."
  'big5-conv-cns-mac-stream-decoder
  'big5-conv-cns-mac-stream-encoder
  big5-conv-cns-coding-system-alist
  'mac)

 (mucs-define-coding-system
  'big5-conv ?B
  "Big5 coding system which convert Big5 characters to mainly CNS ones."
  'big5-conv-cns-unix-stream-decoder
  'big5-conv-cns-unix-stream-encoder
  big5-conv-cns-coding-system-alist
  '[big5-conv-unix big5-conv-dos big5-conv-mac])

 (mapcar
  (lambda (x)
    (coding-system-put x 'alias-coding-systems '(big5-conv)))
  '(big5-conv big5-conv-dos big5-conv-unix big5-conv-mac))

) ;; big5c-cns package definition ends here

;; set up coding category
(if (boundp 'coding-category-big5)
    (setq coding-category-big5 'big5-conv))

(provide 'big5c-cns)

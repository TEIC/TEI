;;; unicode.el --- for UNICODE special features

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
;;  This module supports unicode translations.

(require 'mucs)
(require 'trans-util)

(require 'un-data)

;;; ucs-generic character elisp representation
;;   (1) ... a simple number.
;;   (2) ... a string that shows a number in c-format.
;;           E.g., "0x611A"
;; If any other representations are converted to elisp
;; representation, normalize it to (1) form.
;; Why introduce (2)?  Because there is no compatible format
;; on hexadecimal literal between Emacs20 and XEmacs...x_x;

(defun ucs-generic-elisp-representation (x)
  (if (numberp x)
      x
    (if (stringp x)
	(c-notated-string-to-number x)
      (error "Invalid ucs-generic represenation:%S" x))))

;;;
;;; UCS generic type definition.
;;;
(mucs-define-type
 'ucs-generic
 (function ucs-generic-elisp-representation)
 (function identity))

;;;
;;; translation generator
;;; (work in progress...)

;;;
;;; en/decode-char backends.
;;;

(defun ucs-representation-encoding-backend (char representation restriction)
  (logior (lsh (charset-id (char-charset char)) 16)
	  (char-codepoint char))
  (mucs-convert 'emacs-char-to-ucs-codepoint-conversion
		char))

(defun ucs-representation-decoding-backend (representation object restriction)
  (if (numberp object)
      (mucs-convert 'ucs-codepoint-to-emacs-char-conversion
		    object)
    (signal 'wrong-type-argument
	    (list 'numberp object))))

(mucs-register-representation-encoding-backend
 'ucs nil
 (function ucs-representation-encoding-backend))

(mucs-register-representation-decoding-backend
 'ucs nil
 (function ucs-representation-decoding-backend))

;;;
;;; Unicode or its transformation format support function.
;;; (obsoleted by encode-char/decode-char)
;;;

(defun ucs-to-char (codepoint)
  (ucs-representation-decoding-backend 'ucs codepoint nil))

(defun char-to-ucs (char)
  (ucs-representation-encoding-backend char 'ucs nil))

;;;
;;; Commands for editing.
;;;

(defun insert-ucs-character (codepoint)
  "Insert character which is converted from give UCS codepoint."
  (interactive "nUCS codepoint:")
  (insert (or (ucs-representation-decoding-backend 'ucs codepoint nil)
	      (error "Invalid or cannot translate:U+%X"
		     codepoint))))

(provide 'mule-ucs-unicode)
(provide 'unicode)
;;; unicode.el ends here.



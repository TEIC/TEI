;;; x0213-sjis.el --- Shift-JIS encoder and decoder for JIS X 0213.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, JIS X 0213
;;           Shift-JIS

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

(require 'mucs)
(require 'x0213-cdef)

;;; In JIS X 0213 plain 2, mapping between row and SJIS S1.
;; row 1 -> F0
;; row 3 -> F1
;; row 4 -> F1
;; row 5 -> F2
;; row 8 -> F0
;; row 12 -> F2
;; row 13 -> F3
;; row 14 -> F3
;; row 15 -> F4
;; row 78 -> F4
;; row 79 -> F5
;; row 80 -> F5
;; row 81 -> F6
;; row 82 -> F6
;; row 83 -> F7
;; row 84 -> F7
;; row 85 -> F8
;; row 86 -> F8
;; row 87 -> F9
;; row 88 -> F9
;; row 89 -> FA
;; row 90 -> FA
;; row 91 -> FB
;; row 92 -> FB
;; row 93 -> FC
;; row 94 -> FC

(mucs-define-type
 'char-2
 'identity
 'identity)

(mucs-type-register-serialization
 'char-2
 'emacs-mule
 '(quote (((write-multibyte-character r1 r0))))
 '(quote (((read-multibyte-character r1 r0)))))

(defun mucs-ccl-write-char-2-dos ()
  (if (mucs-ccl-inspect-facility 'eol-automatic-conversion)
      `((write-multibyte-character r1 r0))
    `((if (r0 == ?\x0d)
	((if ,(mucs-ccl-check-internal-state 'previous-cr-p)
	     ((write ?\x0d))
	   ,(mucs-ccl-set-internal-state 'previous-cr-p t)))
	((r4 = (r0 != ?\x0a))
	 (if (,(mucs-ccl-check-internal-state 'previous-cr-p) & r4)
	     ((write ?\x0d)))
	 ,@(mucs-ccl-set-internal-state 'previous-cr-p nil)
	 (write-multibyte-character r1 r0))))))

(mucs-type-register-serialization
 'char-2
 'emacs-mule-dos
 (quote `(,(mucs-ccl-write-char-2-dos)))
 'none)

(defvar jisx0213-shift-jis-plain-2-encode-table
  (let ((result (make-vector 128 0))
	(alist '((1 . ?\xF0) (3 . ?\xF1) (4 . ?\xF1) (5 . ?\xF2) (8 . ?\xF0)
		 (12 . ?\xF2) (13 . ?\xF3) (14 . ?\xF3) (15 . ?\xF4)
		 (78 . ?\xF4) (79 . ?\xF5) (80 . ?\xF5) (81 . ?\xF6)
		 (82 . ?\xF6) (83 . ?\xF7) (84 . ?\xF7) (85 . ?\xF8)
		 (86 . ?\xF8) (87 . ?\xF9) (88 . ?\xF9) (89 . ?\xFA)
		 (90 . ?\xFA) (91 . ?\xFB) (92 . ?\xFB) (93 . ?\xFC)
		 (94 . ?\xFC)))
	(i 0) elem)
    (while alist
      (setq elem (car alist)
	    alist (cdr alist)
	    i (+ (car elem) 32))
      (aset result i (cdr elem)))
    result))

(defun mucs-ccl-char-2-write-shift-jisx0213-char (dosp)
  `((if (r1 == ,(charset-id 'ascii))
       ,(if (and dosp
		 (not (mucs-ccl-inspect-facility 'eol-automatic-conversion)))
	    '((if (r0 == ?\x0A)
		  ((write "\x0d\x0a"))
		((write r0))))
	  '((write r0)))
     ((if (r1 == ,(charset-id 'japanese-jisx0213-1))
	  ((r1 = (r0 >> 7))
	   (r0 &= ?\x7F)
	   (r0 = (r1 en-sjis r0))
	   (write r0 r7))
	((if (r1 == ,(charset-id 'japanese-jisx0213-2))
	     ((r1 = (r0 >> 7))
	      (r0 &= ?\x7F)
	      (r0 = (r1 en-sjis r0))
	      (r0 = r1 ,jisx0213-shift-jis-plain-2-encode-table)
	      (write r0 r7))
	   ;;; katakana-jisx0201
	   ((write (r0 + ,(- ?\xA1 33)))))))))))

(defun mucs-ccl-char-2-read-shift-jisx0213-char ()
  `((r1 = 0)
    (read-if (r0 >= ?\x81)
      ((r1 = (r0 <= ?\x9F))
       (r1 |= (r0 >= ?\xE0))
       (if r1
	   ((read r1)
	    (if (r0 >= ?\xF0)
		;; JIS X 0213 plain 2
		((r4 = (r0 de-sjis r1))
		 (r4 = r7)
		 (if (r1 >= ?\x9F)
		     ((map-single r7 r0
				  jisx0213-shift-jis-plain-2-even-decode-map))
		   ((map-single r7 r0
				jisx0213-shift-jis-plain-2-odd-decode-map)))
		 (r0 <<= 7) (r0 += r4)
		 (r1 = ,(charset-id 'japanese-jisx0213-2)))
	      ;; JIS X 0213 plain 1.
	      ((r1 = (r0 de-sjis r1))
	       (r0 = (r1 << 7)) (r0 += r7)
	       (r1 = ,(charset-id 'japanese-jisx0213-1)))))
	 ((r1 = ,(charset-id 'katakana-jisx0201))
	  (r0 -= ,(- ?\xA1 33))))))))

(mucs-type-register-serialization
 'char-2
 'shift-jis
 (quote `(,(mucs-ccl-char-2-write-shift-jisx0213-char nil)))
 (quote `(,(mucs-ccl-char-2-read-shift-jisx0213-char))))

(mucs-type-register-serialization
 'char-2
 'shift-jis-dos
 (quote `(,(mucs-ccl-char-2-write-shift-jisx0213-char t)))
 'none)

(provide 'x0213-sjis)
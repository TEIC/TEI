;;; -*- coding: iso-2022-7bit  -*-
;;; big5type.el --- conversion between Big5 and Emacs representation(Mainly CNS)

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

;; Comment:
;;   This module provides type definition on big5conv.

(require 'mucs)

(mucs-define-type
 'big5
 'identity
 'identity)

(mucs-type-register-serialization
 'big5
 'big5-be-2-octet
 '(quote
   (((if (r0 > ?\xA0)
	 ((r0 = (r0 >8 0))
	  (write r0 r7))
       ((write r0))))))
 '(quote
   (((read-if (r0 > ?\xA0)
	      ((read r7)
	       (r0 = (r0 <8 r7))))))))

(mucs-type-register-serialization
 'big5
 'big5-be-2-octet-dos
 (if (mucs-ccl-inspect-facility 'eol-automatic-conversion)
     '(quote
       (((if (r0 > ?\xA0)
	     ((r0 = (r0 >8 0))
	      (write r0 r7))
	   ((write r0))))))
   '(quote
     (((if (r0 == ?\x0a)
	   ((write "\x0d\x0a"))
	 ((if (r0 > ?\xA0)
	      ((r0 = (r0 >8 0))
	       (write r0 r7))
	    ((write r0)))))))))
 'none)

(defvar char-1-big5-1-first-code
  (funcall (mucs-type-get-ccl-representation
	    'char-1)
	   (make-char 'chinese-big5-1 33 33)))
(defvar char-1-big5-2-first-code
  (funcall (mucs-type-get-ccl-representation
	    'char-1)
	   (make-char 'chinese-big5-2 33 33)))
(defvar big5-same-row (+ (- ?\x7F ?\x40)
			 (- ?\xFF ?\xA1)))

(defun mucs-ccl-char-1-write-big5-char (dosp)
  `((if (r0 <= ?\xFF)
	,(if (and dosp
		  (not (mucs-ccl-inspect-facility 'eol-automatic-conversion)))
	     '((if (r0 == ?\x0A)
		   ((write "\x0d\x0a"))
		 ((write r0))))
	   '((write r0)))
      ((if (r0 < ,char-1-big5-2-first-code)
	   ((r0 -= ,char-1-big5-1-first-code)
	    (r0 -= ((r0 / 96) * 2))
	    (write ((r0 / ,big5-same-row) + ?\xA1)))
	 ((r0 -= ,char-1-big5-2-first-code)
	  (r0 -= ((r0 / 96) * 2))
	  (write ((r0 / ,big5-same-row) + ?\xC9))))
       (r0 %= ,big5-same-row)
       (if (r0 < ?\x3F)
	   (write (r0 + ?\x40))
	 (write (r0 + ?\x62)))))))

(defun mucs-ccl-char-1-read-big5-char ()
  `((read-if (r0 >= ?\xA1)
      ((read-if (r1 < ?\x7F)
          ((r1 -= ?\x40))
         ((r1 -= ?\x62)))
       (if (r0 < ?\xC9)
	   ((r4 = (((r0 - ?\xA1) * ,big5-same-row) + r1))
	    (r4 += ((r4 / 94) * 2))
	    (r0 = (r4 + ,char-1-big5-1-first-code)))
	 ((r4 = (((r0 - ?\xC9) * ,big5-same-row) + r1))
	  (r4 += ((r4 / 94) * 2))
	  (r0 = (r4 + ,char-1-big5-2-first-code))))))))

(mucs-type-register-serialization
 'char-1
 'big5-char
 (quote
  `(,(mucs-ccl-char-1-write-big5-char nil)))
 (quote
  `(,(mucs-ccl-char-1-read-big5-char))))

(mucs-type-register-serialization
 'char-1
 'big5-char-dos
 (quote
  `(,(mucs-ccl-char-1-write-big5-char t)))
 'none)

(provide 'big5type)



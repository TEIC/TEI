;;; utf.el --- library for UCS Transformation Formats

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           coding-system, ISO/IEC 10646, UTF, Unicode

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
;;  This module supports major Transformation Formats for UCS.

(or (featurep 'mule-ucs-unicode)
    (load-library "unicode"))
;(require 'unicode)
(require 'tae)

;; Because of the incompatibility between XEmacs and Emacs on
;; number literal, we use cn macro that translates a string in
;; C-notation to number.  But if all hexadeciman numbers are
;; described by cn macro, the code seems very dirty, so I use
;; cn macro only on numbers more than 0xFF.  Strictly, it is
;; not reasonable, because we cannot apply arithmetic
;; transformation to characters. But we can use ?\x00-?\xFF, because
;; all of them are mapped into 0-255 in CCL environments of
;; both XEmacs and Emacs.

(require 'trans-util)

;;; Buffer magnification scales for UTFs.

(defconst utf-7-encode-buffer-magnification 3)
(defconst utf-7-decode-buffer-magnification 1)
(defconst utf-8-encode-buffer-magnification 2)
(defconst utf-8-decode-buffer-magnification 2)
(defconst utf-16-encode-buffer-magnification 2)
(defconst utf-16-decode-buffer-magnification 2)

;;
;; Dealing with line separator problem.
;;

(defun convert-unicode-lf-2-crlf (cr-output)
  (if (mucs-ccl-inspect-facility 'eol-automatic-conversion)
      nil
    `((if (r0 == ,unicode-lf)
	  ,(append cr-output)))))

;; The following CCL programs supprots
;; WRITE SIGNATURE, CHECK AND READ SIGNATURE
;; READ, and WRITE functions.

;UTF 8 ------------------------------------------------

(defvar utf-8-ccl-encode
  `((if (r0 < ?\x80)
	((write r0))
      (if (r0 < ,(cn "0x800"))
	  ((write ((r0 >> 6) | ?\xC0))
	   (write ((r0 & ?\x3F) | ?\x80)))
	(if (r0 < ,(cn "0x10000"))
	    ((write ((r0 >> 12) | ?\xE0))
	     (write (((r0 >> 6) & ?\x3F) | ?\x80))
	     (write ((r0 & ?\x3F) | ?\x80)))
	  (if (r0 < ,(cn "0x200000"))
	      ((write ((r0 >> 18) | ?\xF0))
	       (write (((r0 >> 12) & ?\x3F) | ?\x80))
	       (write (((r0 >> 6) & ?\x3F) | ?\x80))
	       (write ((r0 & ?\x3F) | ?\x80)))
	    (if (r0 < ,(cn "0x4000000"))
		((write ((r0 >> 24) | ?\xF8))
		 (write (((r0 >> 18) & ?\x3F) | ?\x80))
		 (write (((r0 >> 12) & ?\x3F) | ?\x80))
		 (write (((r0 >> 6) & ?\x3F) | ?\x80))
		 (write ((r0 & ?\x3f) | ?\x80)))
	      ((write ((r0 >> 30) | ?\xFC))
	       (write (((r0 >> 24) & ?\x3F) | ?\x80))
	       (write (((r0 >> 18) & ?\x3F) | ?\x80))
	       (write (((r0 >> 12) & ?\x3F) | ?\x80))
	       (write (((r0 >> 6) & ?\x3F) | ?\x80))
	       (write ((r0 & ?\x3f) | ?\x80))))))))))

(defvar utf-8-ccl-decode
  `((read-if (r0 >= ?\x80)
	((if (r0 < ?\xE0)
	     ((read r4)
	      (r4 &= ?\x3F)
	      (r0 = (((r0 & ?\x1F) << 6) | r4)))
	   (if (r0 < ?\xF0)
	       ((read r4 r6)
		(r4 = ((r4  & ?\x3F) << 6))
		(r6 &= ?\x3F)
		(r0 = ((((r0 & ?\x0F) << 12) | r4) | r6)))
	     (if (r0 < ?\xF8)
		 ((read r1 r4 r6)
		  (r1 = ((r1  & ?\x3F) << 12))
		  (r4 = ((r4  & ?\x3F) << 6))
		  (r6 &= ?\x3F)
		  (r0 = (((((r0 & ?\x07) << 18) | r1) | r4) | r6)))
	       (if (r0 < ?\xFC)
;;;; MUCS can't read any numbers lager than 24bit
		   ((read r0 r1 r4 r6)
		    (r1 = ((r1  & ?\x3F) << 12))
		    (r4 = ((r4  & ?\x3F) << 6))
		    (r6 &= ?\x3F)
		    (r0 = (((((r0 & ?\x3F) << 18) | r1) | r4) | r6)))
		 (r0 = 0)))))))))

(mucs-type-register-serialization
 'ucs-generic
 'utf-8
 (quote `(,utf-8-ccl-encode))
 (quote `(,utf-8-ccl-decode)))

(mucs-type-register-serialization
 'ucs-generic
 'utf-8-dos
 (quote
  `(,(append
      (convert-unicode-lf-2-crlf '((write ?\xd)))
      utf-8-ccl-encode)))
 'none)

(defun mucs-ccl-write-utf-8-signature ()
  '((write "\xEF\xBB\xBF")))

(defun mucs-ccl-utf-8-check-signature-read ()
  (append
   utf-8-ccl-decode
   `((if (r0 == ,unicode-signature)
	 ,utf-8-ccl-decode))))

;UTF 16 -----------------------------------------------

(defun utf-16-ccl-surrogate-pair-p (reg)
  `((,reg & ,(cn "0xF800")) == ,(cn "0xD800")))

(mucs-ccl-internal-state-reserve 'utf-16-little-endian-p 1)

(defvar utf-16-ccl-decode
  `((if ,(mucs-ccl-check-internal-state 'utf-16-little-endian-p)
	,mucs-ccl-read-ex-le-2-octet
      ,mucs-ccl-read-ex-be-2-octet)
    (if ,(utf-16-ccl-surrogate-pair-p 'r0)
	((if ,(mucs-ccl-check-internal-state 'utf-16-little-endian-p)
	     ((read r6 r4))
	   ((read r4 r6)))
	 (r0 = (((r0 & ?\x3ff) + ?\x40) << 10))
	 (r6 &= ?\x3f)
	 (r4 = ((r4 & ?\x3) << 6) | r6)
	 (r0 |=  r4)))))

(defun mucs-ccl-utf-16-check-signature-read ()
  (append mucs-ccl-read-ex-le-2-octet
	  `((if (r0 == ,unicode-signature)
		,(append (mucs-ccl-set-internal-state
			  'utf-16-little-endian-p t)
			 mucs-ccl-read-ex-le-2-octet)
	      (if (r0 == ,unicode-reverse-signature)
		  ,(append (mucs-ccl-set-internal-state
			    'utf-16-little-endian-p nil)
			   mucs-ccl-read-ex-be-2-octet)))
	    (if ,(utf-16-ccl-surrogate-pair-p 'r0)
		((if ,(mucs-ccl-check-internal-state 'utf-16-little-endian-p)
		     ((read r6 r4))
		   ((read r4 r6)))
		 (r0 = (((r0 & ,(cn "0x3FF")) + ?\x40) << 10))
		 (r6 &= ?\x3F)
		 (r4 = ((r4 & ?\x03) << 6) | r6)
		 (r0 |=  r4))))))

(defun mucs-ccl-read-utf-16 ()
  utf-16-ccl-decode)

;UTF 16 Little Endian----------------------------------

(defvar utf-16-le-ccl-encode
  `((if (r0 < ,(cn "0xFFFF"))
	,mucs-ccl-write-ex-le-2-octet
      ((r4 = (((r0 >> 10) & ,(cn "0x03FF")) - ?\x40)) ; - 0x10000
       (write (r4 & ?\xFF))
       (write ((r4 >> 8) | ?\xD8))
       (write (r0 & ?\xFF))
       (write (((r0 >> 8) & ?\x03) | ?\xDC))))))

(defvar utf-16-le-ccl-decode
  (append mucs-ccl-read-ex-le-2-octet
    `((if ,(utf-16-ccl-surrogate-pair-p 'r0)
	  ((read r6 r4)
	   (r4 = (((r4 & ?\x03) << 8) | r6))
	   (r0 = ((((r0 & ,(cn "0x3FF")) + ?\x40) << 10)
		  | r4)))))))

(mucs-type-register-serialization
 'ucs-generic
 'utf-16-le
 (quote `(,utf-16-le-ccl-encode))
 (quote `(,utf-16-le-ccl-decode)))

(mucs-type-register-serialization
 'ucs-generic
 'utf-16-le-dos
 (quote
  `(,(append
      (convert-unicode-lf-2-crlf '((write "\x0D\x00")))
      utf-16-le-ccl-encode)))
 'none)

(defun mucs-ccl-write-utf-16-le-signature ()
  '((write "\xFF\xFE")))

;UTF 16 Big Endian-------------------------------------

(defvar utf-16-be-ccl-decode
  (append mucs-ccl-read-ex-be-2-octet
    `((if ,(utf-16-ccl-surrogate-pair-p 'r0)
	  ((read r4 r6)
	   (r4 = (((r4 & ?\x03) << 8) | r6))
	   (r0 = ((((r0 & ,(cn "0x3FF")) + ?\x40) << 10)
		  | r4)))))))

(defvar utf-16-be-ccl-encode
  `((if (r0 < ,(cn "0xFFFF"))
	,mucs-ccl-write-ex-be-2-octet
      ((r4 = (((r0 >> 10) & ,(cn "0x03FF")) - ?\x40)) ; - 0x10000
       (write ((r4 >> 8) | ?\xD8))
       (write (r4 & ?\xFF))
       (write (((r0 >> 8) & ?\x03) | ?\xDC))
       (write (r0 & ?\xFF))))))

(mucs-type-register-serialization
 'ucs-generic
 'utf-16-be
 (quote `(,utf-16-be-ccl-encode))
 (quote `(,utf-16-be-ccl-decode)))

(mucs-type-register-serialization
 'ucs-generic
 'utf-16-be-dos
 (quote
  `(,(append
      (convert-unicode-lf-2-crlf
       '((write "\x00\x0D")))
      utf-16-be-ccl-encode)))
 'none)

(defun mucs-ccl-write-utf-16-be-signature ()
  '((write "\xFE\xFF")))

;UTF 7 ------------------------------------------------

(mucs-ccl-internal-state-reserve 'utf-7-shifted-p 2)

(defconst utf-7-direct-characters-assoc
  `(assoc
    (ucs-generic . ucs-generic)
    ,(mapcar
      (lambda (x)
	(cons x x))
      '(?+ ?  ?\t ?\r ?\n ;;;; Notice that `+' is included.
	   ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J
	   ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T
	   ?U ?V ?W ?X ?Y ?Z
	   ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
	   ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
	   ?u ?v ?w ?x ?y ?z
	   ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
	   ?' ?\( ?\)  ?, ?- ?. ?/ ?: ??))))

(defconst utf-7-optional-direct-characters-assoc
  `(assoc
    (ucs-generic . ucs-generic)
    ,(mapcar
      (lambda (x)
	(cons x x))
      '(?! ?\" ?# ?$ ?% ?& ?* ?; ?< ?=
	   ?> ?@ ?\[ ?\] ?^ ?_ ?` ?\{ ?| ?\}))))

(defconst utf-7-shifted-character-assoc
  '(assoc (ucs-generic . ucs-generic)
	  ((all . invalid))))

; (defconst utf-7-b64-assoc
;   `(assoc
;     (b64-char . number)
;     '((?A . 00) (?B . 01) (?C . 02) (?D . 03)
;       (?E . 04) (?F . 05) (?G . 06) (?H . 07)
;       (?I . 08) (?J . 09) (?K . 10) (?L . 11)
;       (?M . 12) (?N . 13) (?O . 14) (?P . 15)
;       (?Q . 16) (?R . 17) (?S . 18) (?T . 19)
;       (?U . 20) (?V . 21) (?W . 22) (?X . 23)
;       (?Y . 24) (?Z . 25) (?a . 26) (?b . 27)
;       (?c . 28) (?d . 29) (?e . 30) (?f . 31)
;       (?g . 32) (?h . 33) (?i . 34) (?j . 35) 
;       (?k . 36) (?l . 37) (?m . 38) (?n . 39) 
;       (?o . 40) (?p . 41) (?q . 42) (?r . 43) 
;       (?s . 44) (?t . 45) (?u . 46) (?v . 47)
;       (?w . 48) (?x . 49) (?y . 50) (?z . 51)
;       (?0 . 52) (?1 . 53) (?2 . 54) (?3 . 55)
;       (?4 . 56) (?5 . 57) (?6 . 58) (?7 . 59)
;       (?8 . 60) (?9 . 61) (?+ . 62) (?/ . 63)
;       (all . invalid) (invalid . all))))

(defconst ccl-b64-encode-table
  '[?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ;; 0-9
    ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ;; 10-19
    ?U ?V ?W ?X ?Y ?Z             ;; 20-25
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ;; 26-35
    ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ;; 36-45
    ?u ?v ?w ?x ?y ?z             ;; 46-51
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ;; 52-61
    ?+ ?/])                       ;; 62-63

(defconst ccl-b64-decode-table
  '[ -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  62  -1  -1  -1  63
     52  53  54  55  56  57  58  59  60  61  -1  -1  -1  -2  -1  -1
     -1   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     15  16  17  18  19  20  21  22  23  24  25  -1  -1  -1  -1  -1
     -1  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     41  42  43  44  45  46  47  48  49  50  51  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1
     -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1])

(tae-declare-translation
 'utf-7-direct-character-p-translation
 `(| ,utf-7-direct-characters-assoc
     ,utf-7-shifted-character-assoc))

(tae-declare-translation
 'utf-7-direct-or-optional-character-p-translation
 `(| ,utf-7-direct-characters-assoc
     ,utf-7-optional-direct-characters-assoc
     ,utf-7-shifted-character-assoc))

(defun ccl-utf-7-base64-encode (valc valp &optional init)
  (if (or (eq valc 'r0)
	  (eq valp 'r0))
      (error "r0 is reserved for caliculation."))
  (let (result)
    (if init
	(setq result
	      `(,@(mucs-ccl-set-internal-state 'utf-7-shifted-p t)
		  (,valp = 0))))
    (setq result
	  (append
	   result
	   `((if (,valc > ,(cn "0xFFFF"))
		 ((r0 = ((((,valc >> 10) - ?\x40)
			  & ,(cn "0x3FF")) | ,(cn "0xD800")))
		  (,valc = ((,valc & ,(cn "0x3FF"))
			    | ,(cn "0xDC00"))))
	       ((r0 = ,valc)
		(,valc = 0)))
	     (loop
	      (if (,valp == 0) ;; 0bit
		  ((,valp = (r0 >> 10))
		   (write ,valp ,ccl-b64-encode-table)
		   (,valp = ((r0 >> 4) & ?\x3F))
		   (write ,valp ,ccl-b64-encode-table)
		   (,valp = ((r0 & ?\x0F) | ?\x30)))
		((if (,valp >= ?\x30) ;; 4bit
		     ((,valp = ((,valp & ?\x0F) << 2))
		      (,valp |= (r0 >> 14))
		      (write ,valp ,ccl-b64-encode-table)
		      (,valp = ((r0 >> 8) & ?\x3F))
		      (write ,valp ,ccl-b64-encode-table)
		      (,valp = ((r0 >> 2) & ?\x3F))
		      (write ,valp ,ccl-b64-encode-table)
		      (,valp = ((r0 & ?\x03) | ?\x20)))
		   ((if (,valp >= ?\x20) ;; 2bit
			((,valp = ((,valp & ?\x03) << 4))
			 (,valp |= (r0 >> 12))
			 (write ,valp ,ccl-b64-encode-table)
			 (,valp = ((r0 >> 6) & ?\x3F))
			 (write ,valp ,ccl-b64-encode-table)
			 (,valp = (r0 & ?\x3F))
			 (write ,valp ,ccl-b64-encode-table)
			 (,valp = 0)))))))
	      (if (,valc != 0)
		  ((r0 = ,valc)
		   (,valc = 0)
		   (repeat)))))))))

(defun ccl-utf-7-base64-encode-flush (valp)
  `(,@(mucs-ccl-set-internal-state 'utf-7-shifted-p nil)
    (if (,valp >= ?\x30) ;; 4bit
	((,valp = ((,valp << 2) & ?\x3F))
	 (write ,valp ,ccl-b64-encode-table))
      ((if (,valp >= ?\x20) ;; 2bit
	   ((,valp = ((,valp << 4) & ?\x3F))
	    (write ,valp ,ccl-b64-encode-table)))))
    (write ?-)
    (,valp = 0)))

(defun mucs-ccl-utf-7-encode-eof ()
  `((if ,(mucs-ccl-check-internal-state 'utf-7-shifted-p)
	,(ccl-utf-7-base64-encode-flush 'r6))))

(defun mucs-ccl-utf-7-encode (direct-character-translation dos)
  `(((r4 = r0))
    ,(tae-compile direct-character-translation nil)
    ,(mucs-ccl-if-invalid
       ;;; shifted state
      (quote
       `(((if ,(mucs-ccl-check-internal-state 'utf-7-shifted-p)
	      ,(ccl-utf-7-base64-encode 'r4 'r6)
	    ((write ?+)
	     ,@(ccl-utf-7-base64-encode 'r4 'r6 t))))))
       ;;; Non-shifted state
      (quote
       `(((if ,(mucs-ccl-check-internal-state 'utf-7-shifted-p)
	      ,(ccl-utf-7-base64-encode-flush 'r6))
	  ,@(if (and dos
		     (not (mucs-ccl-inspect-facility 'eol-automatic-conversion)))
		'((if (r0 == ?\n)
		      ((write "\x0D\x0A"))
		    ((write r0))))
	      '((write r0)))
	  (if (r0 == ?+)
	      ((write ?-)))))))))

(defconst ccl-utf-7-decode
  `((r4 = 0)
    (loop
     (if ,(mucs-ccl-check-internal-state 'utf-7-shifted-p)
	 ((read r0)
	  (r1 = r0 ,ccl-b64-decode-table)
	  (if (r1 >= 0)
	      ((r6 = ((r6 << 6) | r1))
	       (read r0)
	       (r1 = r0 ,ccl-b64-decode-table)
	       (if (r1 >= 0)
		   ((r6 = ((r6 << 6) | r1))
		    (if (r6 >= ,(cn "0x30000"))
			((r0 = (r6 & ,(cn "0xFFFF")))
			 (r6 = 0))
		      ((read r0)
		       (r1 = r0 ,ccl-b64-decode-table)
		       (if (r1 >= 0)
			   ((r6 = ((r6 << 6) | r1))
			    (if (r6 >= ,(cn "0x800000"))
				;; r6 = 1000 XXXX XXXX XXXX XXXX XXXX
				((r0 = ((r6 >> 4) & ,(cn "0xFFFF")))
				 (r6 = ((r6 & ?\xF) | ?\x30)))
			      ;; r6 = XXXX XXXX XXXX XXXX XX
			      ((r0 = ((r6 >> 2) & ,(cn "0xFFFF")))
			       (r6 = ((r6 & ?\x3) | ?\x20)))))
			 ,(mucs-ccl-set-internal-state
			   'utf-7-shifted-p nil)))))
		 ,(mucs-ccl-set-internal-state
		   'utf-7-shifted-p nil)))
	    (,@(mucs-ccl-set-internal-state
		'utf-7-shifted-p nil)
	     (if (r0 == ?-)
		 ((repeat))))))
       ((read-if (r0 == ?+)
		 ((read-if (r0 == ?-)
			   ((r0 = ?+))
			((r6 = r0 ,ccl-b64-decode-table)
			 (if (r6 >= 0)
			     ((read r0)
			      (r1 = r0 ,ccl-b64-decode-table)
			      (if (r1 >= 0)
				  ((r6 = ((r6 << 6) | r1))
				   (read r0)
				   (r1 = r0 ,ccl-b64-decode-table)
				   (if (r1 >= 0)
				       ((r6 = ((r6 << 6) | r1))
					(r0 = (r6 >> 2))
					(r6 = ((r6 & ?\x03) | ?\x20))
					,@(mucs-ccl-set-internal-state
					   'utf-7-shifted-p t)))))))))))))
     (if (r4 != 0)
	 ((r0 &= ,(cn "0x3FF"))
	  (r0 = ((((r4 & ,(cn "0x3FF")) + ?\x40) << 10)
		 | r0)))
       ((if ,(utf-16-ccl-surrogate-pair-p 'r0)
	    ((r4 = r0)
	     (repeat))))))))

(mucs-type-register-serialization
 'ucs-generic
 'utf-7
 '(mucs-ccl-utf-7-encode
   'utf-7-direct-or-optional-character-p-translation
   nil)
 (quote `(,ccl-utf-7-decode)))

(mucs-type-register-serialization
 'ucs-generic
 'utf-7-safe
 '(mucs-ccl-utf-7-encode
   'utf-7-direct-character-p-translation
   nil)
 'none)

(mucs-type-register-serialization
 'ucs-generic
 'utf-7-dos
 '(mucs-ccl-utf-7-encode
   'utf-7-direct-or-optional-character-p-translation
   t)
 'none)

(mucs-type-register-serialization
 'ucs-generic
 'utf-7-safe-dos
 '(mucs-ccl-utf-7-encode
   'utf-7-direct-character-p-translation
   t)
 'none)

(provide 'utf)

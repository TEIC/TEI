;;; -*- coding: iso-2022-7bit  -*-
;;; big5conv.el --- conversion between Big5 and Emacs representation(Mainly CNS)

;; Copyright (C) 1997, 1998 Kawabata Taichi
;;               1999       Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           MULE-UCS, Big5, CNS, Traditional Chinese, RFC1922

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

; (defvar big5conv-ccl-big5-to-abs
;   '((r0 = (r0 >8 0))
;     (if (r7 >= ?\xa1)
; 	((r4 = (r7 - 98)))
;       ((r4 = (r7 - 64))))
;     (r0 = (((r0 - ?\xa1) * 157) - r4))))

(require 'mucs)
(require 'tae)
(mucs-require-supplement 'big5type 'big5conv)

(defconst big5conv-encode-buffer-magnification 2) ;; LF -> CR+LF
(defconst big5conv-decode-buffer-magnification 2)

(defun big5conv-big5-to-flat-code (num)
  (let ((hi (/ num 256))
        (lo (% num 256)))
    (+ (* 157 (- hi ?\xa1))
       (- lo (if (>= lo ?\xa1) 98 64)))))

(defun big5conv-char-to-flat-code (num)
  (let ((hi (/ num 256))
        (lo (% num 256)))
    (+ (* 94 (- hi ?\x21))
       (- lo ?\x21))))

(defun big5conv-flat-code-to-big5 (num)
  (let ((hi (/ num 157))
        (lo (% num 157)))
    (+ (* 256 (+ hi ?\xa1))
       (+ lo (if (< lo 63) 64 98)))))

(defun big5conv-flat-code-to-char (num)
  (let ((hi (/ num 94))
        (lo (% num 94)))
    (list (+ hi ?\x21) (+ lo ?\x21))))

(defun big5conv-expand-alist (alist)
  (let (elem result char big5
	     i end codepoint charset)
    (while alist
      (setq elem (car alist)
	    char (car elem)
	    big5 (cdr elem)
	    alist (cdr alist))

      (cond ((and (consp char)
		  (consp big5))
	     ;; convert strings described in C-style to numbers.
	     (if (stringp (car big5))
		 (setcar big5 (c-notated-string-to-number (car big5))))
	     (if (stringp (cdr big5))
		 (setcdr big5 (c-notated-string-to-number (cdr big5))))
	     (if (stringp (cdr char))
		 (setcdr char (c-notated-string-to-number (cdr char))))

	     (setq i (big5conv-big5-to-flat-code (car big5))
		   end (big5conv-big5-to-flat-code (cdr big5))
		   codepoint (big5conv-char-to-flat-code (cdr char))
		   charset (car char))
	     (while (>= end i)
	       (setq result (cons
			     (cons
			      (apply (function make-char)
				     charset
				     (big5conv-flat-code-to-char codepoint))
			      (big5conv-flat-code-to-big5 i))
			     result)
		     i (1+ i)
		     codepoint (1+ codepoint))
	     ))
	    ((and (trans-util-charp char)
		  (or (numberp big5)
		      (stringp big5)))
	     (if (stringp big5)
		 (setq big5 (c-notated-string-to-number big5)))
	     (setq result (cons (cons char big5)
				result)))
	    (t
	     (error "Unkown slot type:%S" elem))))
      (nreverse result)))

(defvar big5conv-ascii-assoc
  (list 'assoc
	'(char-1 . big5)
	(let ((i 0) result)
	  (while (< i 128)
	    (setq result
		  (nconc result
			 (list (cons (make-char 'ascii i) i)))
		  i (1+ i)))
	  result))
  "US-ASCII part of BIG5 translation rule")

(defvar big5conv-emacs-char-1-vs-big5-assoc
  `(assoc
    (char-1 . big5)
    ,(nconc
      (big5conv-expand-alist
       '(
	 ;; Symbols
      	 ((chinese-cns11643-1 . "0x2121") . ("0xA140" . "0xA1F5"))
  	 (?$(G"X(B . "0xA1F6")
  	 (?$(G"W(B . "0xA1F7")
      	 ((chinese-cns11643-1 . "0x2259") . ("0xA1F8" . "0xA2AE"))
      	 ((chinese-cns11643-1 . "0x2421") . ("0xA2AF" . "0xA3BF"))
	 ;; Control code (vender dependant)
      	 ((chinese-cns11643-1 . "0x4221") . ("0xA3C0" . "0xA3E0"))
	 ;; Level 1 Ideograhs
      	 ((chinese-cns11643-1 . "0x4421") . ("0xA440" . "0xACFD"))
  	 (?$(GWS(B . "0xACFE")
      	 ((chinese-cns11643-1 . "0x5323") . ("0xAD40" . "0xAFCF"))
      	 ((chinese-cns11643-1 . "0x5754") . ("0xAFD0" . "0xBBC7"))
      	 ((chinese-cns11643-1 . "0x6B51") . ("0xBBC8" . "0xBE51"))
  	 (?$(GkP(B . "0xBE52")
      	 ((chinese-cns11643-1 . "0x6F5C") . ("0xBE53" . "0xC1AA"))
      	 ((chinese-cns11643-1 . "0x7536") . ("0xC1AB" . "0xC2CA"))
  	 (?$(Gu5(B . "0xC2CB")
      	 ((chinese-cns11643-1 . "0x7737") . ("0xC2CC" . "0xC360"))
      	 ((chinese-cns11643-1 . "0x782E") . ("0xC361" . "0xC3B8"))
  	 (?$(Gxe(B . "0xC3B9")
  	 (?$(Gxd(B . "0xC3BA")
      	 ((chinese-cns11643-1 . "0x7866") . ("0xC3BB" . "0xC455"))
  	 (?$(Gx-(B . "0xC456")
      	 ((chinese-cns11643-1 . "0x7962") . ("0xC457" . "0xC67E"))
	 ;; Symbols
      	 ((chinese-cns11643-1 . "0x2621") . ("0xC6A1" . "0xC6BE"))
	 ;; Radicals
  	 (?$(G'#(B . "0xC6BF")
  	 (?$(G'$(B . "0xC6C0")
  	 (?$(G'&(B . "0xC6C1")
  	 (?$(G'((B . "0xC6C2")
  	 (?$(G'-(B . "0xC6C3")
  	 (?$(G'.(B . "0xC6C4")
  	 (?$(G'/(B . "0xC6C5")
  	 (?$(G'4(B . "0xC6C6")
  	 (?$(G'7(B . "0xC6C7")
  	 (?$(G':(B . "0xC6C8")
  	 (?$(G'<(B . "0xC6C9")
  	 (?$(G'B(B . "0xC6CA")
  	 (?$(G'G(B . "0xC6CB")
  	 (?$(G'N(B . "0xC6CC")
  	 (?$(G'S(B . "0xC6CD")
  	 (?$(G'T(B . "0xC6CE")
  	 (?$(G'U(B . "0xC6CF")
  	 (?$(G'Y(B . "0xC6D0")
  	 (?$(G'Z(B . "0xC6D1")
  	 (?$(G'a(B . "0xC6D2")
  	 (?$(G'f(B . "0xC6D3")
  	 (?$(G()(B . "0xC6D4")
  	 (?$(G(*(B . "0xC6D5")
  	 (?$(G(c(B . "0xC6D6")
  	 (?$(G(l(B . "0xC6D7")
	 ;; Diacritical Marks
      	 ((japanese-jisx0208 . "0x212F") . ("0xC6D8" . "0xC6D9"))
	 ;; Japanese Kana Supplement
      	 ((japanese-jisx0208 . "0x2133") . ("0xC6DA" . "0xC6E3"))
	 ;; Japanese Hiragana
      	 ((japanese-jisx0208 . "0x2421") . ("0xC6E7" . "0xC77A"))
	 ;; Japanese Katakana
      	 ((japanese-jisx0208 . "0x2521") . ("0xC77B" . "0xC7F2"))
	 ;; Cyrillic Characters
      	 ((japanese-jisx0208 . "0x2721") . ("0xC7F3" . "0xC854"))
      	 ((japanese-jisx0208 . "0x2751") . ("0xC855" . "0xC875"))
	 ;; Special Chinese Characters
  	 (?$(J!#(B . "0xC879")
  	 (?$(J!$(B . "0xC87B")
  	 (?$(J!*(B . "0xC87D")
  	 (?$(J!R(B . "0xC8A2")

	 ;; JIS X 0208 NOT SIGN (cf. U+00AC)
  	 (?$B"L(B . "0xC8CD")
	 ;; JIS X 0212 BROKEN BAR (cf. U+00A6)
  	 (?$(D"C(B . "0xC8CE")

	 ;; GB 2312 characters
  	 (?$A!d(B . "0xC8CF")
  	 (?$A!e(B . "0xC8D0")
        ;;;;; C8D1 - Japanese `($B3t(B)'
  	 (?$A!m(B . "0xC8D2")
        ;;;;; C8D2 - Tel.

	 ;; Level 2 Ideographs
      	 ((chinese-cns11643-2 . "0x2121") . ("0xC940" . "0xC949"))
    	 (?$(GDB(B . "0xC94A");; duplicates to "0xA461"
      	 ((chinese-cns11643-2 . "0x212B") . ("0xC94B" . "0xC96B"))
      	 ((chinese-cns11643-2 . "0x214D") . ("0xC96C" . "0xC9BD"))
  	 (?$(H!L(B . "0xC9BE")
      	 ((chinese-cns11643-2 . "0x217D") . ("0xC9BF" . "0xC9EC"))
      	 ((chinese-cns11643-2 . "0x224E") . ("0xC9ED" . "0xCAF6"))
  	 (?$(H"M(B . "0xCAF7")
      	 ((chinese-cns11643-2 . "0x2439") . ("0xCAF8" . "0xD6CB"))
         (?$(H>c(B . "0xD6CC")
         ((chinese-cns11643-2 . "0x3770") . ("0xD6CD" . "0xD779"))
  	 (?$(H?j(B . "0xD77A")
      	 ((chinese-cns11643-2 . "0x387E") . ("0xD77B" . "0xDADE"))
         (?$(H7o(B . "0xDADF")
         ((chinese-cns11643-2 . "0x3E64") . ("0xDAE0" . "0xDBA6"))
      	 ((chinese-cns11643-2 . "0x3F6B") . ("0xDBA7" . "0xDDFB"))
    	 (?$(HAv(B . "0xDDFC");; duplicates to "0xDCD1"
      	 ((chinese-cns11643-2 . "0x4424") . ("0xDDFD" . "0xE8A2"))
      	 ((chinese-cns11643-2 . "0x554C") . ("0xE8A3" . "0xE975"))
      	 ((chinese-cns11643-2 . "0x5723") . ("0xE976" . "0xEB5A"))
      	 ((chinese-cns11643-2 . "0x5A29") . ("0xEB5B" . "0xEBF0"))
  	 (?$(HUK(B . "0xEBF1")
      	 ((chinese-cns11643-2 . "0x5B3F") . ("0xEBF2" . "0xECDD"))
  	 (?$(HW"(B . "0xECDE")
      	 ((chinese-cns11643-2 . "0x5C6A") . ("0xECDF" . "0xEDA9"))
      	 ((chinese-cns11643-2 . "0x5D75") . ("0xEDAA" . "0xEEEA"))
  	 (?$(Hd/(B . "0xEEEB")
      	 ((chinese-cns11643-2 . "0x6039") . ("0xEEEC" . "0xF055"))
  	 (?$(H]t(B . "0xF056")
      	 ((chinese-cns11643-2 . "0x6243") . ("0xF057" . "0xF0CA"))
  	 (?$(HZ((B . "0xF0CB")
      	 ((chinese-cns11643-2 . "0x6337") . ("0xF0CC" . "0xF162"))
      	 ((chinese-cns11643-2 . "0x6430") . ("0xF163" . "0xF16A"))
  	 (?$(Hga(B . "0xF16B")
      	 ((chinese-cns11643-2 . "0x6438") . ("0xF16C" . "0xF267"))
  	 (?$(Hi4(B . "0xF268")
      	 ((chinese-cns11643-2 . "0x6573") . ("0xF269" . "0xF2C2"))
      	 ((chinese-cns11643-2 . "0x664E") . ("0xF2C3" . "0xF374"))
      	 ((chinese-cns11643-2 . "0x6762") . ("0xF375" . "0xF465"))
      	 ((chinese-cns11643-2 . "0x6935") . ("0xF466" . "0xF4B4"))
  	 (?$(HfM(B . "0xF4B5")
      	 ((chinese-cns11643-2 . "0x6962") . ("0xF4B6" . "0xF4FC"))
      	 ((chinese-cns11643-2 . "0x6A4C") . ("0xF4FD" . "0xF662"))
  	 (?$(HjK(B . "0xF663")
      	 ((chinese-cns11643-2 . "0x6C52") . ("0xF664" . "0xF976"))
      	 ((chinese-cns11643-2 . "0x7167") . ("0xF977" . "0xF9C3"))
  	 (?$(Hqf(B . "0xF9C4")
  	 (?$(Hr4(B . "0xF9C5")
  	 (?$(Hr@(B . "0xF9C6")
      	 ((chinese-cns11643-2 . "0x7235") . ("0xF9C7" . "0xF9D1"))
      	 ((chinese-cns11643-2 . "0x7241") . ("0xF9D2" . "0xF9D5"))

	 ;; Additional Ideographs
  	 (?$(IC7(B . "0xF9D6")
  	 (?$(IOP(B . "0xF9D7")
  	 (?$(IDN(B . "0xF9D8")
  	 (?$(IPJ(B . "0xF9D9")
  	 (?$(I,](B . "0xF9DA")
  	 (?$(I=~(B . "0xF9DB")
  	 (?$(IK\(B . "0xF9DC")
	 ))
     '((all . invalid))
     )))

(provide 'big5conv)


;;; trans-util.el --- useful functions for translating characters.

;; Copyright (C) 1997-2000 Miyashita Hisashi

;; Keywords: CCL, mule, multilingual, character set,
;;           coding-system, ISO10646, Unicode

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

;;; String to number translation functions

(defun string-to-number-with-radix (string radix)
  (let ((i 0)
	(j (length string))
	(result 0)
	c)
    (while (< i j)
      (setq c (aref string i))
      (if (and (>= c ?0)
	       (<= c ?9))
	  (setq c (- c ?0))
	(setq c (upcase c))
	(if (and (>= c ?A)
		 (<= c ?F))
	    (setq c (- c ?A -10))
	  (error "Invalid char:%c" c)))
      (setq result (+ (* result radix) c)
	    i (1+ i)))
    result))

(defun hex-string-to-number (string)
  (string-to-number-with-radix string 16))

(defun octal-string-to-number (string)
  (string-to-number-with-radix string 8))

(if (fboundp 'ccl-execute)
    (define-ccl-program
      ccl-c-notated-string-to-number
      `(1
	((r6 = 10)
	 (r2 = 0)
	 (read r0)
	 (if (r0 == ?0) ((read r1)
			 (if (r1 == ?x)
			     ((r6 = 16) (read r0))
			   ((r6 = 8) (r0 = r1)))))
	 (loop
	   (if (r0 < ?0) ((r2 = -1) (break)))
	   (r5 = (r0 > ?7)) (r5 &= (r6 == 8))
	   (if r5 ((r2 = -1) (break)))

	   (if (r0 > ?9)
	       ((r5 = (r6 != 16))
		(r5 |= (r0 < ?A))
		(if r5 ((r2 = -1) (break)))
		(if (r0 > ?F)
		    ((r5 = (r0 < ?a))
		     (r5 |= (r0 > ?f))
		     (if r5 ((r2 = -1) (break)))
		     (r0 -= ,(- ?a 10)))
		  ((r0 -= ,(- ?A 10)))))
	     ((r0 -= ?0)))

	   (r2 *= r6)
	   (r2 += r0)
	   (read r0)
	   (repeat))))))

(defun c-notated-string-to-number (string)
  (if (fboundp 'ccl-execute)
      (let ((vector [0 0 0 0 0 0 0 0 nil]))
	(ccl-execute-on-string
	 'ccl-c-notated-string-to-number
	 vector string)
	(aref vector 2))
    (cond ((string-match "0x\\(\\d+\\)" string)
	   (string-to-number-with-radix
	    (match-string 1 string) 16))
	  ((string-match "0\\(\\d+\\)" string)
	   (string-to-number-with-radix
	    (match-string 1 string) 8))
	  (t
	   (string-to-number string)))))

;; For embedding number literal.
(defmacro cn (string)
  (c-notated-string-to-number string))

;; For transformating list structure

;;; identity, cn...
(defmacro transformate-list-structure (spec lstr)
  (let (func1 func2 result)
    (setq func1
	  (lambda (le se)
	    (cond ((functionp se)
		   (funcall se le))
		  ((and (listp se)
			(listp le))
		   (funcall func2 le se))
		  (t
		   (error "Invalid spec or element" se le))))
	  func2
	  (lambda (l s)
	    (let (le se result)
	      (if (and (not (listp (cdr l)))
		       (not (listp (cdr s))))
		  (cons (funcall func1 (car l) (car s))
			(funcall func1 (cdr l) (cdr s)))
		(while (and (setq le (car l))
			    (setq se (car s)))
		  (setq result
			(cons
			 (funcall func1 le se)
			 result))
		  (setq l (cdr l)
			s (cdr s)))
		(nreverse result)))))
    (while lstr
      (setq result (cons (funcall func2 (car lstr) spec)
			 result)
	    lstr (cdr lstr)))
    (list 'quote (nreverse result))))

;;; character maker/translator.

(defconst make-char-internal-usable-p
  (and (fboundp (function make-char-internal))
       (eq (condition-case err
	       (make-char-internal
		(charset-id 'ascii)
		97)
	     (error
	      nil))
	   ?a)))

(defconst charset-id-table [])

(defun update-charset-id-table ()
  (let* ((len 256)
	 (vec (make-vector len nil))
	 (csl (charset-list))
	 id cs)
    (while (setq cs (car csl))
      (setq id (charset-id cs)
	    csl (cdr csl))
      (if (>= id len)
	  (setq vec (vconcat vec
			     (make-vector (- id len -1)
					  nil))
		len (length vec)))
      (aset vec (charset-id cs) cs))
    (setq charset-id-table vec)))

(defsubst make-char-from-charset-id (id c1 &optional c2)
  (if make-char-internal-usable-p
      (make-char-internal id c1 c2)
    (if (or (>= id (length charset-id-table))
	    (aref charset-id-table id))
	(update-charset-id-table))
    (make-char
     (or (aref charset-id-table id)
	 (error "ID:%d is not valid charset-id." id))
     c1 c2)))

(defsubst char-codepoint (char)
  "Return a codepoint of char."
  (let ((info (split-char char)))
    (if (= (length info) 3)
	(+ (* (nth 1 info) 96)
	   (nth 2 info))
      (nth 1 info))))

(defsubst make-char-from-charset-codepoint (charset codepoint)
  "Return a character of CODEPOINT in CHARSET"
  (if (> codepoint 255)
      (progn
	(setq codepoint (- codepoint 32))
	(make-char charset
		   (/ codepoint 96)
		   (+ (% codepoint 96) 32)))
    (make-char charset codepoint)))

(defsubst make-char-from-charset-id-codepoint (charset codepoint)
  "Return a character of CODEPOINT in CHARSET"
  (if (> codepoint 255)
      (progn
	(setq codepoint (- codepoint 32))
	(make-char-from-charset-id charset
				   (/ codepoint 96)
				   (+ (% codepoint 96) 32)))
    (make-char-from-charset-id charset codepoint)))

;;; character representations.

(defconst mucs-char-1-ucs-area-start (lsh 1 24))

(defalias 'trans-util-charp
  (cond ((fboundp (function char-valid-p))
	 (function char-valid-p))
	((fboundp (function characterp))
	 (function characterp))
	(t
	 (error "Cannot find out any equivalents to characterp."))))

(defun char-1-elisp-representation (x)
  (if (< x mucs-char-1-ucs-area-start)
      (make-char-from-charset-id-codepoint
       (lsh x -16)
       (logand x (cn "0xFFFF")))
    ;; currently return nil for char-1 UCS form.
    nil))

(defun char-1-ccl-representation (x)
  (if (trans-util-charp x)
      (logior (lsh (charset-id (char-charset x)) 16)
	      (char-codepoint x))
    x))

(provide 'trans-util)

;;; trans-util.el ends here.

;;; mule-uni.el --- Mule Private charset definitions for unicode.

;; Copyright (C) 2000-2001 MIYASHITA Hisashi

;; Keywords: mule, multilingual, 
;;           character set, ISO/IEC 10646, Unicode.

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

(defconst mule-unicode-xxxx-range-alist
  '((mule-unicode-0100-24ff (256 . 9471) (256 . 9471))
    (mule-unicode-2500-33ff (9472 . 13311) (9472 . 13311))
    (mule-unicode-e000-ffff (57344 . 65535) ; 0xE000 - 0xFFFF
			    (57344 . 65278) ; 0xE000 - 0xFEFF
			    (65280 . 65533) ; 0xFEFF - 0xFFFD
			    ))
  "ranges of mule-unicode-xxxx.")

;; charset and category definition.

(if (featurep 'xemacs)
    ;; For XEmacs.
    nil

  ;;
  (define-category ?u "Mule unicode characters")
  (if (not (charsetp 'mule-unicode-0100-24ff))
      (define-charset 244 'mule-unicode-0100-24ff
	[2 96 1 0 ?1 0 "Unicode subset" "Unicode subset (U+0100..U+24FF)"
	   "Unicode characters of the range U+0100..U+24FF."]))
  (modify-category-entry (make-char 'mule-unicode-0100-24ff) ?u)

  (if (not (charsetp 'mule-unicode-2500-33ff))
      (define-charset 242 'mule-unicode-2500-33ff
	[2 96 1 0 ?2 0 "Unicode subset 2" "Unicode subset (U+2500..U+33FF)"
	   "Unicode characters of the range U+2500..U+33FF."]))
  (modify-category-entry (make-char 'mule-unicode-2500-33ff) ?u)

  (if (not (charsetp 'mule-unicode-e000-ffff))
      (define-charset 243 'mule-unicode-e000-ffff
	[2 96 1 0 ?3 0 "Unicode subset 3" "Unicode subset (U+E000+FFFF)"
	   "Unicode characters of the range U+E000..U+FFFF."]))
  (modify-category-entry (make-char 'mule-unicode-e000-ffff) ?u)

  (if (not (charsetp 'mule-ucs-unicode-multichar))
      (define-charset 223 'mule-ucs-unicode-multichar
	[1 96 1 0 ?> 0 "Unicode multichar form"
	   "Unicode multichar"
	   "Unicode chars of Mule-UCS-Unicode multichar form"])))

(defun ucs-to-mule-unicode-xxxx (codepoint)
  (let ((rsl mule-unicode-xxxx-range-alist)
	rs r)
    (while (and (setq rs (car rsl))
		(setq r (nth 1 rs))
		(not (and (>= codepoint (car r))
			  (<= codepoint (cdr r)))))
      (setq rsl (cdr rsl)))
    (if (null rsl)
	(error "The given codepoint is out of the range.:%d"
	       codepoint))
    (setq codepoint (- codepoint (car r)))
    (make-char (car rs)
	       (+ (/ codepoint 96) 32)
	       (+ (% codepoint 96) 32))))

(defun mule-unicode-xxxx-to-ucs (char)
  (let ((rsl mule-unicode-xxxx-range-alist)
	rs cs row col)
    (if (and (char-valid-p char)
	     (setq cs (split-char char)
		   row (nth 1 cs)
		   col (nth 2 cs)
		   rs (assq (car cs) rsl)))
	(+ (* (- row 32) 96) (- col 32)
	   (car (nth 1 rs)))
      (error "The given char is invalid:%S" char))))

(let ((cls mule-unicode-xxxx-range-alist)
      spec range rl charset assoc-sym)
  (while (setq spec (car cls))
    (setq charset (car spec)
	  range (nth 1 spec)
	  rl (nthcdr 2 spec)
	  cls (cdr cls)
	  assoc-sym (intern
		     (format "%s-vs-unicode-assoc"
			     (symbol-name charset))))
    (if (charsetp charset)
	(progn
	  (set assoc-sym
	       (list 'range
		     '(char-1 . ucs-generic)
		     (mapcar
		      (lambda (re)
			(cons
			 (cons
			  (ucs-to-mule-unicode-xxxx
			   (car re))
			  (ucs-to-mule-unicode-xxxx
			   (cdr re)))
			 re))
		      rl)))
	  (put charset
	       'unicode-assoc
	       assoc-sym)
	  (put charset
	       'mccl-font-encoder
	       `(list
		 '((r2 += ((r1 * 96) - ,(+ (* 96 32) 32 (- (car range)))))
		   (r1 = (r2 >8 r2))
		   (r2 = r7))))))))

(if (charsetp 'mule-ucs-unicode-multichar)
    (progn
      (setq unicode-vs-mule-ucs-unicode-multichar-assoc
	    (eval-when-compile
	      `(range (char-1 . ucs-generic)
		      (,(cons (cons mucs-char-1-ucs-area-start
				    (* 2 mucs-char-1-ucs-area-start))
			      (cons 0 mucs-char-1-ucs-area-start))))))
      (put 'mule-ucs-unicode-multichar
	   'unicode-assoc
	   'unicode-vs-mule-ucs-unicode-multichar-assoc)))

(provide 'mule-uni)

;;; mule-uni.el ends here.

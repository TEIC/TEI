;;; txt-tbl.el --- useful functions for converting UNICODE CONSORTIUM table

;; Copyright (C) 1997-2000 Miyashita Hisashi

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
;;  

(require 'trans-util)

(defun iso-2022-based-codepoint-to-char (charset codepoint &optional errorp)
  (or
   (if (< codepoint 32)
       (if (eq charset 'ascii)
	   (make-char 'ascii codepoint))
     (if (< codepoint 128)
	 (if (= (charset-dimension charset) 1)
	     (make-char charset codepoint))
       (if (< codepoint 256)
	   (if (= (charset-dimension charset) 1)
	       (make-char charset (- codepoint 128)))
	 (if (< codepoint (cn "0x2020"))
	     (if (= (charset-dimension charset) 2)
		 (make-char charset
			    (lsh codepoint -8)
			    (logand codepoint (cn "0x80"))))))))
   (if errorp
       (error "Invalid code:%d(%S)" codepoint charset)
     nil)))

(defun make-table-alist-region
  (begin end
	 car-spec cdr-spec
	 description-regexp valid-line-regexp)
  (let ((cpoint begin)
	(i 1)
	(description-match 3)
	(car-regexp (car car-spec))
	(cdr-regexp (car cdr-spec))
	(car-func (cdr car-spec))
	(cdr-func (cdr cdr-spec))
	linestr
	description
	endpoint
	table
	slot1 slot2)

    (while (progn 
	     (goto-char cpoint)
	     (forward-line 1)
	     (setq endpoint (point))
	     (goto-char cpoint)
	     (and (<= endpoint end)
		  (< cpoint endpoint)))
      (if (re-search-forward valid-line-regexp
			     endpoint t)
	  (progn
	    (setq linestr (buffer-substring cpoint endpoint))
	    (if (and (string-match car-regexp linestr)
		     (setq slot1 (funcall car-func
					  (match-string 1 linestr)))
		     (string-match cdr-regexp linestr)
		     (setq slot2 (funcall cdr-func
					  (match-string 1 linestr)))
		     (or (null description-regexp)
			 (string-match description-regexp linestr)))
		  (setq description
			(if description-regexp
			    (match-string 1 linestr)
			  nil)
			table (cons (list slot1 slot2 description)
				    table))
	      (message "*Warning:%s is not match to specified regexp!"
		       linestr))
	    (setq i (1+ i))
	    (message "Count: %d" i)))
      (setq cpoint endpoint))

    (message "%d, %d" endpoint end)
    (nreverse table)))

(defun make-table-alist-region-by-col (begin end car-col cdr-col
					     &optional func)
  (if (null func)
      (setq func (function c-notated-string-to-number)))
  (let ((re-gen (lambda (c)
		  (let ((i 0) (res "^[ \t]*"))
		    (setq c (1- c))
		    (while (> c i)
		      (setq res
			    (concat res "[^ \t]+[ \t]+")
			    i (1+ i)))
		    (concat res "\\([0-9][0-9a-fA-Fx]*\\)")))))

    (make-table-alist-region begin end
			     (cons (funcall re-gen car-col)
				   func)
			     (cons (funcall re-gen cdr-col)
				   func)
			     "#[ \t]*\\(.*\\)$"
			     "^[^#]")))

(defun mucs-print-a-list (alist cols)
  (interactive "XA-list: \nnColumn: ")
  (let ((curlist alist) (i 1) curelem)
    (while (setq curelem (car curlist))
      (insert (format "(?\\x%X . ?\\x%04X) ;; %s"
		      (car curelem)
		      (nth 1 curelem)
		      (nth 2 curelem)
		      ))
      (cond ((= i cols)
	     (insert "\n")
	     (setq i 1))
	    (t
	     (setq i (1+ i))))
      (setq curlist (cdr curlist)))))

(defun mucs-print-character-a-list (alist cols code-to-char)
  (interactive "XA-list: \nnColumn: \nSCharset: ")
  (let ((curlist alist) (i 1) curelem char)
    (while (setq curelem (car curlist))
      (setq char (funcall code-to-char (car curelem)))
      (if char
	  (progn
	    (insert (format "(?%c . ?\\x%04X) ;; %s"
			    char
			    (nth 1 curelem)
			    (nth 2 curelem)))
	    (cond ((= i cols)
		   (insert "\n")
		   (setq i 1))
		  (t
		   (setq i (1+ i))))))
      (setq curlist (cdr curlist)))))


;;
;; Appendix.
;;

(defvar unicode-consortium-table-database
  '(("8859-1.TXT" 1 2
     (lambda (ch) (if (and (>= ch ?\xA0)
			   (<= ch ?\xFF))
		      (iso-2022-based-codepoint-to-char
		       'latin-iso-8859-1 ch))))
    ("8859-2.TXT" 1 2
     (lambda (ch) (if (and (>= ch ?\xA0)
			   (<= ch ?\xFF))
		      (iso-2022-based-codepoint-to-char
		       'latin-iso-8859-2 ch))))
    ("8859-3.TXT" 1 2
     (lambda (ch) (if (and (>= ch ?\xA0)
			   (<= ch ?\xFF))
		      (iso-2022-based-codepoint-to-char
		       'latin-iso-8859-3 ch))))
    ("8859-4.TXT" 1 2
     (lambda (ch) (if (and (>= ch ?\xA0)
			   (<= ch ?\xFF))
		      (iso-2022-based-codepoint-to-char
		       'latin-iso-8859-4 ch))))
    ("8859-5.TXT" 1 2
     (lambda (ch) (if (and (>= ch ?\xA0)
			   (<= ch ?\xFF))
		      (iso-2022-based-codepoint-to-char
		       'cyrillic-iso8859-5 ch))))))

(defun translate-table-file-for-unicode-consortium (filename)
  (interactive "fFilename:")
  (let* ((buf (generate-new-buffer
	       (format "*%s-tbl-translate*" filename)))
	 (basename (file-name-nondirectory filename))
	 (specslot (assoc basename unicode-consortium-table-database))
	 car-col cdr-col convfunc table)
    (if (null specslot)
	(error "I don't know about %s" filename))
    (setq car-col (nth 1 specslot)
	  cdr-col (nth 2 specslot)
	  convfunc (nth 3 specslot))
    (switch-to-buffer buf)
    (insert-file-contents filename)
    (setq table (make-table-alist-region-by-col
		 (point-min)
		 (point-max)
		 car-col
		 cdr-col))
    (erase-buffer)
    (mucs-print-character-a-list
     table 1 convfunc)))

(defun translate-table-file-generic
  (filename &optional charset car-col cdr-col)
  (interactive "fFilename:")
  (if (null charset)
      (setq charset
	    (intern
	     (completing-read
	      "Charset:"
	      (mapcar
	       (lambda (x) (cons (symbol-name x) 0))
	       (charset-list))
	      nil t))))
  (if (null car-col) (setq car-col 1))
  (if (null cdr-col) (setq cdr-col 2))
  (let ((buf (generate-new-buffer
	      (format "*%s-tbl-translate*" filename)))
	table)
    (switch-to-buffer buf)
    (insert-file-contents filename)
    (setq table (make-table-alist-region-by-col
		 (point-min)
		 (point-max)
		 car-col
		 cdr-col))
    (erase-buffer)
    (mucs-print-character-a-list
     table 1 (lambda (ch)
	       (iso-2022-based-codepoint-to-char charset ch)))))

(defun translate-unicode-cns-table-file (filename)
  (interactive "fFilename:")
  (let ((buf (generate-new-buffer
	      (format "*%s-tbl-translate*" filename)))
	(car-col 1)
	(cdr-col 2)
	table code
	table1 table2 table3
	elem plane)
    (switch-to-buffer buf)
    (insert-file-contents filename)
    (setq table (make-table-alist-region-by-col
		 (point-min)
		 (point-max)
		 car-col
		 cdr-col))
    (erase-buffer)
    (while (setq elem (car table))
      (setq plane (lsh (logand (car elem) (cn "0xF0000")) -16)
	    code (logand (car elem) (cn "0xFFFF")))
      (cond ((= plane 1)
	     (setq table1 (cons (cons code (nth 1 elem))
				table1)))
	    ((= plane 2)
	     (setq table2 (cons (cons code (nth 1 elem))
				table2)))
	    ((= plane ?\xe)
	     (setq table3 (cons (cons code (nth 1 elem))
				table3))))
      (setq table (cdr table)))
    (insert ";CNS 11643 Plane 1------------------------\n")
    (mucs-print-character-a-list table1 1 'chinese-cns11643-1)
    (insert ";CNS 11643 Plane 2------------------------\n")
    (mucs-print-character-a-list table2 1 'chinese-cns11643-2)
    (insert ";CNS 11643 Plane 3------------------------\n")
    (mucs-print-character-a-list table3 1 'chinese-cns11643-3)))

(provide 'txt-tbl)

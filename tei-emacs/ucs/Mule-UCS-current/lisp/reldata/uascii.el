;;; uascii.el --- tables between UCS and US-ASCII

;; Copyright (C) 1997-2000 Miyashita Hisashi

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, ASCII

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

(require 'tbl-mg)

(put 'ascii 'unicode-assoc
     'us-ascii-vs-unicode-assoc)

(defvar
  us-ascii-vs-unicode-assoc
  (list 'assoc
	'(char-1 . ucs-generic)
	(let ((i 0) result)
	  (while (< i 128)
	    (setq result
		  (nconc result
			 (list (cons (make-char 'ascii i) i)))
		  i (1+ i)))
	  result))
  "US-ASCII vs Unicode association list")

(provide 'uascii)



;;; un-tools.el --- useful functions for UNICODE special features.

;; Copyright (C) 1997-1999 Miyashita Hisashi

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
;;  This module supports useful functions for unicode.
;;  If you want to use auto detecting utf? .etc, 
;;  add (require 'un-tools) to your .emacs.

(require 'un-data)

(defvar unicode-set-auto-coding-enable-p t
  "* non-nil means unicode-set-auto-coding detect UTF signatures.")

(defvar unicode-original-auto-coding-function nil)

(defvar unicode-set-auto-coding-alist
  (list 
   (cons utf-16-le-dos-signature-regexp 'utf-16-le-dos)
   (cons utf-16-le-unix-signature-regexp 'utf-16-le-unix)
   (cons utf-16-le-mac-signature-regexp 'utf-16-le-mac)
   (cons utf-16-le-signature-regexp 'utf-16-le)
   (cons utf-16-be-dos-signature-regexp 'utf-16-be-dos)
   (cons utf-16-be-unix-signature-regexp 'utf-16-be-unix)
   (cons utf-16-be-mac-signature-regexp 'utf-16-be-mac)
   (cons utf-16-be-signature-regexp 'utf-16-be)
   (cons utf-8-ws-dos-signature-regexp 'utf-8-ws-dos)
   (cons utf-8-ws-unix-signature-regexp 'utf-8-ws-unix)
   (cons utf-8-ws-mac-signature-regexp 'utf-8-ws-mac)
   (cons utf-8-ws-signature-regexp 'utf-8-ws))
  "* Specify the correspond coding-system of each signature(regexp).")

;; Automatic detection for unicode signature.
;;  You should set `unicode-set-auto-coding-alist'
;;   to specify the correspond coding-system of each signature.

(defun unicode-set-auto-coding (filename size)
   (let ((curlist unicode-set-auto-coding-alist)
	 (string (buffer-substring (point) (+ (point) size)))
	 coding)
     (or
      (funcall unicode-original-auto-coding-function filename size)
      (while
	  (cond ((null curlist)
		 (setq coding nil))
		((and
		  (coding-system-p (setq coding (cdr (car curlist))))
		  (string-match (car (car curlist)) string))
		 nil)
	       (t
		(setq curlist (cdr curlist)
		      coding nil)
		t)))
     coding)))

	
(if (and unicode-set-auto-coding-enable-p
	 (not (eq set-auto-coding-function
		  'unicode-set-auto-coding)))
      (setq unicode-original-auto-coding-function 
	    set-auto-coding-function
	    set-auto-coding-function
	    'unicode-set-auto-coding))

(provide 'un-tools)

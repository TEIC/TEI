;;; x0213-mime.el --- MIME configurations of JIS X 0213:2000
;;;                   for FLIM/APEL/SEMI.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, character set,
;;           internet messsage, APEL, FLIM, SEMI, JIS X 0213

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar coding-system-for-mime-charset-iso-2022-jp-3
  ;; 'iso-2022-jp-3
  ;; 'iso-2022-jp-3-compatible
  'iso-2022-jp-3-strict
  "* coding system used for MIME charset `ISO-2022-JP-3'.
APEL will use this coding system for encoding or decoding
`ISO-2022-JP-3'.")

(defun setup-iso-2022-jp-3-mcharset-environment ()
  (if (and (boundp 'mime-charset-coding-system-alist)
	   (null (assq 'iso-2022-jp-3
		       mime-charset-coding-system-alist)))
      (setq mime-charset-coding-system-alist
	    (cons
	     (cons 'iso-2022-jp-3
		   (if (coding-system-p
			coding-system-for-mime-charset-iso-2022-jp-3)
		       coding-system-for-mime-charset-iso-2022-jp-3
		     (message 
		      "*Warning* Cannot find %S coding-system!"
		      coding-system-for-mime-charset-iso-2022-jp-3)
		     'iso-2022-jp-3))
	     mime-charset-coding-system-alist)))
  (let ((slot
	 '((ascii japanese-jisx0208
		  japanese-jisx0213-1
		  japanese-jisx0213-2) . iso-2022-jp-3)))
    (if (and (boundp 'charsets-mime-charset-alist)
	     (not (member slot charsets-mime-charset-alist)))
	(setq charsets-mime-charset-alist
	      (append charsets-mime-charset-alist (list slot))))))

(defun setup-iso-2022-jp-3-eword-environment ()
  (if (and (boundp 'eword-charset-encoding-alist)
	   (not (assq 'iso-2022-jp-3
		      eword-charset-encoding-alist)))
      (setq eword-charset-encoding-alist
	    (cons '(iso-2022-jp-3 . "B")
		  eword-charset-encoding-alist))))

(defun setup-iso-2022-jp-3-mime-edit-environment ()
  (if (and (boundp 'mime-charset-type-list)
	   (not (assq 'iso-2022-jp-3
		      mime-charset-type-list)))
      (setq mime-charset-type-list 
	    (cons '(iso-2022-jp-3 7 "base64")
		  mime-charset-type-list))))

(eval-after-load
    "mcharset" '(setup-iso-2022-jp-3-mcharset-environment))

(eval-after-load
    "eword-encode" '(setup-iso-2022-jp-3-eword-environment))

(add-hook
 'mime-edit-load-hook
 (function setup-iso-2022-jp-3-mime-edit-environment))

(provide 'x0213-mime)

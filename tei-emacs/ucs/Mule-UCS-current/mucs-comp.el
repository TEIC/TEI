;;; mucs-comp.el --- for byte-compile and other housekeeping jobs.

;; Copyright (C) 1999-2000 Miyashita Hisashi

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

(require 'bytecomp)

;;(message "%S\n" command-line-args-left)
(let ((load-path (append '("." "./lisp") load-path))
      (compile-file-list-1st '("./lisp/mucs.el"
			       "./lisp/mucs-type.el"
			       "./lisp/mucs-error.el"
			       "./lisp/mucs-ccl.el"
			       "./lisp/mccl-font.el"
			       "./lisp/tbl-mg.el"
			       "./lisp/trans-util.el"
			       "./lisp/txt-tbl.el"
			       "./lisp/tae.el"
			       "./lisp/mule-uni.el"
			       "./lisp/unicode.el"
			       "./lisp/utf.el"
			       "./lisp/un-data.el"
			       "./lisp/un-tools.el"
			       "./lisp/unidata.el"))
       (compile-file-list-2nd (if command-line-args-left
				  (prog1
				      command-line-args-left
				    (setq command-line-args-left nil))
				'(
				  "./lisp/un-define.el"
				  "./lisp/un-supple.el"
				  )))
       file)

  (message "Remove old byte-compiled files-----")
  (mapcar 
   (lambda (x)
     (setq file (concat
		 (file-name-sans-extension x)
		 ".elc"))
     (if (file-exists-p file)
	 (progn
	   (message "Remove %s" file)
	   (delete-file file)))
     nil)
   (append
    compile-file-list-1st
    compile-file-list-2nd))

  (message "Compiling 1st stage-----")
  (mapcar
   (lambda (file)
     (save-excursion
       (byte-compile-file file)))
   compile-file-list-1st)
  (message "Compiling 2nd stage!!---")
  (mapcar
   (lambda (file)
     (save-excursion
       (byte-compile-file file)))
   compile-file-list-2nd))


      

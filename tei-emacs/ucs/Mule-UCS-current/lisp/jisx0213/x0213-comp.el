;;; x0213-comp.el --- for byte-compile and other housekeeping jobs.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, Mule-UCS, JIS X 0213

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

;; Comment:

(require 'bytecomp)

;;(message "%S\n" command-line-args-left)
(let ((current-dir default-directory)
      (load-path (append '("." "..") load-path))
      (compile-file-list-1st '("./x0213-cdef.el"
			       "./x0213-font.el"
			       "./x0213-csys.el"
			       "./x0213-util.el"
			       "./x0213-char.el"
			       "./x0213-mime.el"))
      (compile-file-list-2nd '("./x0213-udef.el")))
  (cd "../../")
  (message "Mule-UCS set up-----")
  (load-file "./mucs-comp.el")
  (cd current-dir)

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


      

;;; x0213-cdef.el --- Character set definition for JIS X 0213:2000

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, character set, JIS X 0213

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

;;; This module defines character set for Japanese JIS X 0213.

(if (not (charsetp 'japanese-jisx0213-1))
    (define-charset 151 'japanese-jisx0213-1
      [2 94 2 0 ?O 0 "JISX0213-1" "JISX0213-1" "JISX0213-1 (Japanese)"]))

(if (eq window-system 'x)
    (put-charset-property 'japanese-jisx0213-1
			  'x-charset-registry "JISX0213-1"))

(if (not (charsetp 'japanese-jisx0213-2))
    (define-charset 254 'japanese-jisx0213-2
      [2 94 2 0 ?P 0 "JISX0213-2" "JISX0213-2" "JISX0213-2 (Japanese)"]))

(if (eq window-system 'x)
    (put-charset-property 'japanese-jisx0213-2
			  'x-charset-registry "JISX0213-2"))

(set-language-info "Japanese" 'charset
                   '(japanese-jisx0208 japanese-jisx0208-1978
                     japanese-jisx0213-1 japanese-jisx0213-2
                     japanese-jisx0212 latin-jisx0201 katakana-jisx0201))

(provide 'x0213-cdef)

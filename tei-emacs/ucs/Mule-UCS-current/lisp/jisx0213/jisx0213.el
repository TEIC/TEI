;;; jisx0213.el --- Setup file for JIS X 0213 configuration.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, font,
;;           coding-system, character-set,
;;           Unicode, JIS X 0213

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

;;; This module only load other configuration modules.

;; Character set definition.
(require 'x0213-cdef)

;; Unicode translation setup.
(require 'x0213-udef)

;; JIS X 0213 coding systems setup.
(require 'x0213-csys)

;; font and its encoder setup.
(require 'x0213-font)

;; MIME configuration for APEL/FLIM/SEMI.
(require 'x0213-mime)

(provide 'jisx0213)

;;; un-data.el --- basic data for UNICODE.

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
;;  This module defines basic data for unicode.

;; Because of the incompatibility between XEmacs and Emacs on
;; number literal, we use cn macro that translates a string in
;; C-notation to number.

(eval-when-compile
  (require 'trans-util))

(defconst unicode-byte-order-mark-lsb (cn "0xFF"))
(defconst unicode-byte-order-mark-msb (cn "0xFE"))
(defconst unicode-signature (cn "0xFEFF"))
(defconst unicode-reverse-signature (cn "0xFFFE"))
(defconst unicode-line-separator (cn "0x2028"))
(defconst unicode-paragraph-separator (cn "0x2029"))
(defconst unicode-character-replacement (cn "0xFFFD"))
(defconst unicode-ignore-characters
  (list
   (cn "0xFFFF")
   (cn "0xFFFE")
   (cn "0xFEFF")))
(defconst utf-8-signature
  (list
   (cn "0xEF")
   (cn "0xBB")
   (cn "0xBF")))
(defconst unicode-cr (cn "0x000D"))
(defconst unicode-lf (cn "0x000A"))

(defvar utf-8-ws-dos-signature-regexp
  "^\xef\xbb\xbf[^\n]*\xd\xa\\([^\xd\xa]\\|\\'\\)")
(defvar utf-8-ws-unix-signature-regexp
  "^\xef\xbb\xbf[^\xd]*\xa\\([^\xd\xa]\\|\\'\\)")
(defvar utf-8-ws-mac-signature-regexp
  "^\xef\xbb\xbf[^\n]*\xd\\([^\xd\xa]\\|\\'\\)")
(defvar utf-8-ws-signature-regexp "^\xef\xbb\xbf")

(defvar utf-16-le-dos-signature-regexp
  "^\xff\xfe\\(\xa[^\x0]\\|[^\xa].\\)*\xd\x0\xa\x0")
(defvar utf-16-le-unix-signature-regexp
  "^\xff\xfe\\(\xa[^\x0]\\|[^\xa].\\)*\xa\x0")
(defvar utf-16-le-mac-signature-regexp
  "^\xff\xfe\\(\xa[^\x0]\\|[^\xa].\\)*\xd\x0")
(defvar utf-16-le-signature-regexp "^\xff\xfe")

(defvar utf-16-be-dos-signature-regexp
  "^\xfe\xff\\(\x0[^\xa]\\|[^\x0].\\)*\x0\xd\x0\xa")
(defvar utf-16-be-unix-signature-regexp
  "^\xfe\xff\\(\x0[^\xa]\\|[^\x0].\\)*\x0\xa")
(defvar utf-16-be-mac-signature-regexp
  "^\xfe\xff\\(\x0[^\xa]\\|[^\x0].\\)*\x0\xd")
(defvar utf-16-be-signature-regexp "^\xfe\xff")

(defvar unicode-special-relation-alist
  (list (cons ?\n unicode-line-separator)))

(provide 'un-data)

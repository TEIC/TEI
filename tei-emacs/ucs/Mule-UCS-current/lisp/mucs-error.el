;;; -*- coding: iso-2022-7bit  -*-
;;; mucs-error.el --- Mule-UCS error handling library.

;; Copyright (C) 1999 Miyashita Hisashi

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

;;; Generic section
(put 'mucs-error 'error-conditions '(mucs-fatal mucs-generic mucs-error error))
(put 'mucs-error 'error-message "Mule-UCS: generic error")

;;; MUCS
(put 'mucs-conversion-type-mismatch-error 'error-conditions
     '(mucs-fatal mucs-error error))
(put 'mucs-conversion-type-mismatch-error 'error-message
     "Mule-UCS: conversion type mismatch")

;;; MUCS-TYPE
(put 'mucs-unknown-type-error 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-unknown-type-error 'error-message
     "Mule-UCS: unknown type")

(put 'mucs-unknown-serialization-error 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-unknown-serialization-error 'error-message
     "Mule-UCS: unknown serialization")

(put 'mucs-no-serialization-method 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-no-seriazlization-method 'error-message
     "Mule-UCS: NO serialization method")

(put 'mucs-no-unserialization-method 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-no-unseriazlization-method 'error-message
     "Mule-UCS: NO unserialization method")

(put 'mucs-invalid-serialization-error 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-invalid-serialization-error 'error-message
     "Mule-UCS: invalid serialization")

(put 'mucs-type-mismatch-error 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-type-mismatch-error 'error-message
     "Mule-UCS: type mismatch")

(put 'mucs-type-cannot-convert 'error-conditions
     '(mucs-fatal mucs-type mucs-error error))
(put 'mucs-type-cannot-convert 'error-message
     "Mule-UCS: cannot convert")

;;; MUCS-CCL
(put 'mucs-ccl-error 'error-conditions '(mucs-fatal mucs-ccl mucs-error error))
(put 'mucs-ccl-error 'error-message "Mule-UCS: MULE-UCS-CCL generic error")

(put 'mucs-ccl-invalid-error 'error-conditions
     '(mucs-fatal mucs-ccl mucs-error error))
(put 'mucs-ccl-invalid-error 'error-message "Mule-UCS: Invalid MULE-UCS-CCL")

(put 'mucs-ccl-convert-error 'error-conditions
     '(mucs-fatal mucs-ccl mucs-error error))
(put 'mucs-ccl-convert-error 'error-message
     "Mule-UCS: MULE-UCS-CCL(Convert)Invalid object")

;;; Generic functions.

(defun mucs-error (error-sym message &rest else)
  (signal error-sym (format message else)))

(provide 'mucs-error)

;;; -*- coding: iso-2022-7bit -*-
;;; jisx0213-font.el --- Font setup and define encoder for JIS X 0213.

;; Copyright (C) 2000 KAWABATA, Taichi

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, JIS X 0213

;;; This program defines character set for Japanese JIS X 0213.

;;; Comment:
;; Miyashita Hisashi extracted this part
;; from JIS X 0213 package made by KAWABATA, Taichi.

;; Add them to fontset.

(require 'x0213-cdef)

(eval-when-compile
  (require 'x0213-sjis))

(if window-system
    (let (fontset)
      (when (setq fontset (query-fontset "-*-*-medium-r-*-*-16-*-*-*-*-*-*-*"))
	(set-fontset-font fontset 'japanese-jisx0213-1 "*jisx0213.2000-1")
	(set-fontset-font fontset 'japanese-jisx0213-2 "*jisx0213.2000-2")
	;;(set-fontset-font fontset 'japanese-jisx0213-1 "*shiftjis-0")
	;;(set-fontset-font fontset 'japanese-jisx0213-2 "*shiftjis-0")
        )
      (when (setq fontset (query-fontset "-*-*-medium-r-*-*-24-*-*-*-*-*-*-*"))
	;;(set-fontset-font fontset 'japanese-jisx0213-1 "*jisx0213.2000-1")
	;;(set-fontset-font fontset 'japanese-jisx0213-2 "*jisx0213.2000-2")
	(set-fontset-font fontset 'japanese-jisx0213-1 "*shiftjis-0")
	(set-fontset-font fontset 'japanese-jisx0213-2 "*shiftjis-0")
        )))

;; Shift-JIS encoded Fontを使用する場合のfont encoder.

(define-ccl-program ccl-encode-shiftjis-font
  `(0
    ((r4 = r1)
     (r1 = (r4 en-sjis r2))
     (r2 = r7)
     (if (r0 == ,(charset-id 'japanese-jisx0213-2))
	 ((r1 = r4 ,jisx0213-shift-jis-plain-2-encode-table)))))
  "CCL program to encode a JIS X 0213 to code point of Shift-JIS font.")

(setq font-ccl-encoder-alist
      (cons (cons "shiftjis-0" 'ccl-encode-shiftjis-font)
            font-ccl-encoder-alist))

(if (featurep 'meadow)
    (w32-regist-font-encoder
     'shift-jisx0213-font-encoder
     'ccl-encode-shiftjis-font))

(provide 'x0213-font)

;; -*- coding: iso-2022-jp-3  -*-
;;; jisx0213-util.el
;;;     --- Some data and utility for JIS X 0213

;; Copyright (C) 2000 KAWABATA, Taichi <batta@beige.ocn.ne.jp>

;; Keywords: Tamago, egg, multilingual, JIS X 0213

(require 'x0213-csys)

;; 不満： translate-string関数があれば、こんなCCLは不要だった…

(define-ccl-program jisx0213-to-jisx0208
  `(1
    (loop
     (read-multibyte-character r0 r1)
     (translate-character jisx0213-to-jisx0208/0212 r0 r1)
     (write-multibyte-character r0 r1)
     (repeat))))

(define-ccl-program jisx0208-to-jisx0213
  `(1
    (loop
     (read-multibyte-character r0 r1)
     (translate-character jisx0208-to-jisx0213 r0 r1)
     (write-multibyte-character r0 r1)
     (repeat))))

(defun jisx0213-to-jisx0208-string (string)
  (ccl-execute-on-string 'jisx0213-to-jisx0208 
                         (make-vector 9 nil) string))

(defun jisx0208-to-jisx0213-string (string)
  (ccl-execute-on-string 'jisx0208-to-jisx0213
                         (make-vector 9 nil) string))

(defun make-jisx0213-char-list (from to)
  (setq from (string-to-char
              (jisx0208-to-jisx0213-string
               (char-to-string from))))
  (setq to   (string-to-char
              (jisx0208-to-jisx0213-string
               (char-to-string to))))
  (mapcar '(lambda (x)
             (let ((split (split-char x)))
               (setcar split 'japanese-jisx0213-1)
               (apply 'make-char split)))
          (make-chars-list from to)))

;; JIS X 0213 付属書 4 に基づく合成可能な文字群

(defvar jisx0213-combining-chars
  `(?͡ ?̆ 
    ,@(make-jisx0213-char-list ?̋?̂)
    ,@(make-jisx0213-char-list ?̥?̚)))

(provide 'x0213-util)

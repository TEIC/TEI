;;  -*- coding: iso-2022-7bit  -*-
;;;  x0213-csys.el --- Coding System Definition for JIS X 0213.

;; Copyright (C) 2000 KAWABATA, Taichi
;;                    Miyashita Hisashi

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, JIS X 0213

;; This program defines coding-system described in JIS X 0213 standard.

(eval-when-compile (require 'cl))
(require 'x0213-cdef)

(eval-when-compile
  (require 'x0213-sjis))

(eval-and-compile
  (defun make-list-of-range (from to)
    "Return the list of integers ranging from FROM to TO."
    (let (result)
      (while (<= from to)
	(setq result (cons to result)
	      to (1- to)))
      result))

;;; character list maker.
;;; only for 94x94 characters.
  (defun make-chars-list (from to)
    (let* ((from-split (split-char from))
	   (from-cs    (car from-split))
	   (from-row   (- (elt from-split 1) 33))
	   (from-col   (- (elt from-split 2) 33))
	   (from-num   (+ (* 94 from-row) from-col))
	   (to-split (split-char to))
	   (to-row   (- (elt to-split 1) 33))
	   (to-col   (- (elt to-split 2) 33))
	   (to-num   (+ (* 94 to-row) to-col))
	   table)
      (while (<= from-num to-num)
	(setq table
	      (cons (make-char from-cs
			       (+ (/ to-num 94) 33)
			       (+ (% to-num 94) 33))
		    table))
	(setq to-num (1- to-num)))
      table))

  (defun make-jisx0208-to-0213-translation-pair (char)
    (let* ((split (split-char char))
           (x (cadr split))
           (y (caddr split)))
      (list (cons (make-char 'japanese-jisx0208 x y)
                  (make-char 'japanese-jisx0213-1 x y)))))

  (defun make-jisx0208-to-0213-translation-pairs (from to)
    (let* ((table (make-chars-list from to)))
      (mapcar '(lambda (char)
                 (let* ((split (split-char char))
                        (x (cadr split))
                        (y (caddr split)))
                   (cons (make-char 'japanese-jisx0208 x y)
                         (make-char 'japanese-jisx0213-1 x y))))
              table)))

  (defun make-jisx0213-to-0208-translation-pairs (from to)
    (let* ((table (make-chars-list from to)))
      (mapcar '(lambda (char) 
		 (let* ((split (split-char char))
                        (x (cadr split))
                        (y (caddr split)))
		   (cons (make-char 'japanese-jisx0213-1 x y)
                         (make-char 'japanese-jisx0208 x y))))
	      table))))

(eval-when-compile
  (define-translation-table
    'jisx0208-to-jisx0213
    nil)
  (define-translation-table
    'jisx0208/0212-to-jisx0213
    nil)
  (define-translation-table
    'jisx0213-to-jisx0208/0212
    nil)
  (define-translation-table
    'jisx0208-to-jisx0213-restricted
    nil))

;; translation table

(define-translation-table 
  'jisx0208-to-jisx0213
  (list (cons (make-char 'japanese-jisx0208)
              (make-char 'japanese-jisx0213-1))))

(define-translation-table 
 'jisx0208/0212-to-jisx0213
  (list (cons (make-char 'japanese-jisx0208)
              (make-char 'japanese-jisx0213-1))
        (cons (make-char 'japanese-jisx0212)
              (make-char 'japanese-jisx0213-2))))

(define-translation-table
  'jisx0213-to-jisx0208/0212
  (eval-when-compile
    (make-translation-table
     (nconc 
      (mapcar '(lambda (x) 
                 (cons (make-char 'japanese-jisx0213-1 (+ 32 x))
                       (make-char 'japanese-jisx0208 (+ 32 x))))
              `(1 ,@(make-list-of-range 16 46)
                  ,@(make-list-of-range 48 83)))
      (make-jisx0213-to-0208-translation-pairs ?◆ ?〓)
      (make-jisx0213-to-0208-translation-pairs ?∈ ?∩)
      (make-jisx0213-to-0208-translation-pairs ?∧ ?∃)
      (make-jisx0213-to-0208-translation-pairs ?∠ ?∬)
      (make-jisx0213-to-0208-translation-pairs ?Å ?¶)
      (make-jisx0213-to-0208-translation-pairs ?◯ ?◯)
      (make-jisx0213-to-0208-translation-pairs ?０ ?９)
      (make-jisx0213-to-0208-translation-pairs ?Ａ ?Ｚ)
      (make-jisx0213-to-0208-translation-pairs ?ａ ?ｚ)
      (make-jisx0213-to-0208-translation-pairs ?ぁ ?ん)
      (make-jisx0213-to-0208-translation-pairs ?ァ ?ヶ)
      (make-jisx0213-to-0208-translation-pairs ?Α ?Ω)
      (make-jisx0213-to-0208-translation-pairs ?α ?ω)
      (make-jisx0213-to-0208-translation-pairs ?А ?Я)
      (make-jisx0213-to-0208-translation-pairs ?а ?я)
      (make-jisx0213-to-0208-translation-pairs ?─ ?╂)
      (make-jisx0213-to-0208-translation-pairs ?蓮 ?腕)
      (make-jisx0213-to-0208-translation-pairs ?堯 ?熙)
      (mapcar '(lambda (x) 
                 (cons (make-char 'japanese-jisx0213-2 (+ 32 x))
                       (make-char 'japanese-jisx0212 (+ 32 x))))
              `(2 6 7 9 10 11
                  ,@(make-list-of-range 16 77)))))))

;; The following translation table assures that JIS X 0208 characters
;; prohibited in ISO-2022-JP-3 encoding will all be translated to
;; equivalent JIS X 0213 characters.  
(define-translation-table
  'jisx0208-to-jisx0213-restricted
  (eval-when-compile
    (make-translation-table
     (nconc 
      (make-jisx0208-to-0213-translation-pairs ?＇ ?⤵)
      (make-jisx0208-to-0213-translation-pairs ?⦿ ?•)
      (make-jisx0208-to-0213-translation-pairs ?∓ ?℧)
      (make-jisx0208-to-0213-translation-pairs ?゠ ?⧻)
      (make-jisx0208-to-0213-translation-pairs ?ゔ ?こ゚)
      (make-jisx0208-to-0213-translation-pairs ?カ゚ ?ト゚)
      (make-jisx0208-to-0213-translation-pairs ?♤ ?♣)
      (make-jisx0208-to-0213-translation-pairs ?ς ?ㇿ)
      (make-jisx0208-to-0213-translation-pairs ?⎾ ?⏌)
      (make-jisx0208-to-0213-translation-pairs ?ヷ ?㊿)
      (make-jisx0208-to-0213-translation-pairs ?◐ ?ǜ)
      (make-jisx0208-to-0213-translation-pairs ?€ ?㋬)
      (make-jisx0208-to-0213-translation-pairs ?⁑ ?Ⅻ)
      (make-jisx0208-to-0213-translation-pairs ?㍻ ?㍼)
      (make-jisx0208-to-0213-translation-pair ?∮)
      (make-jisx0208-to-0213-translation-pair ?∟)
      (make-jisx0208-to-0213-translation-pair ?⊿)
      (make-jisx0208-to-0213-translation-pair ?❖)
      (make-jisx0208-to-0213-translation-pair ?☞)
      (make-jisx0208-to-0213-translation-pairs ?𠀋 ?嬥)
      (make-jisx0208-to-0213-translation-pair ?唖)
      (make-jisx0208-to-0213-translation-pair ?鯵)
      (make-jisx0208-to-0213-translation-pair ?逸)
      (make-jisx0208-to-0213-translation-pair ?謁)
      (make-jisx0208-to-0213-translation-pair ?焔)
      (make-jisx0208-to-0213-translation-pair ?縁)
      (make-jisx0208-to-0213-translation-pair ?横)
      (make-jisx0208-to-0213-translation-pair ?鴬)
      (make-jisx0208-to-0213-translation-pair ?鴎)
      (make-jisx0208-to-0213-translation-pair ?黄)
      (make-jisx0208-to-0213-translation-pair ?温)
      (make-jisx0208-to-0213-translation-pair ?禍)
      (make-jisx0208-to-0213-translation-pair ?悔)
      (make-jisx0208-to-0213-translation-pair ?海)
      (make-jisx0208-to-0213-translation-pair ?慨)
      (make-jisx0208-to-0213-translation-pair ?概)
      (make-jisx0208-to-0213-translation-pair ?蛎)
      (make-jisx0208-to-0213-translation-pair ?撹)
      (make-jisx0208-to-0213-translation-pair ?喝)
      (make-jisx0208-to-0213-translation-pair ?渇)
      (make-jisx0208-to-0213-translation-pair ?褐)
      (make-jisx0208-to-0213-translation-pair ?竃)
      (make-jisx0208-to-0213-translation-pair ?噛)
      (make-jisx0208-to-0213-translation-pair ?寛)
      (make-jisx0208-to-0213-translation-pair ?漢)
      (make-jisx0208-to-0213-translation-pair ?潅)
      (make-jisx0208-to-0213-translation-pair ?諌)
      (make-jisx0208-to-0213-translation-pair ?器)
      (make-jisx0208-to-0213-translation-pair ?既)
      (make-jisx0208-to-0213-translation-pair ?祈)
      (make-jisx0208-to-0213-translation-pair ?虚)
      (make-jisx0208-to-0213-translation-pair ?侠)
      (make-jisx0208-to-0213-translation-pair ?郷)
      (make-jisx0208-to-0213-translation-pair ?響)
      (make-jisx0208-to-0213-translation-pair ?尭)
      (make-jisx0208-to-0213-translation-pair ?勤)
      (make-jisx0208-to-0213-translation-pair ?謹)
      (make-jisx0208-to-0213-translation-pair ?躯)
      (make-jisx0208-to-0213-translation-pair ?薫)
      (make-jisx0208-to-0213-translation-pair ?掲)
      (make-jisx0208-to-0213-translation-pair ?頚)
      (make-jisx0208-to-0213-translation-pair ?撃)
      (make-jisx0208-to-0213-translation-pair ?研)
      (make-jisx0208-to-0213-translation-pair ?鹸)
      (make-jisx0208-to-0213-translation-pair ?砿)
      (make-jisx0208-to-0213-translation-pair ?麹)
      (make-jisx0208-to-0213-translation-pair ?穀)
      (make-jisx0208-to-0213-translation-pair ?黒)
      (make-jisx0208-to-0213-translation-pair ?殺)
      (make-jisx0208-to-0213-translation-pair ?祉)
      (make-jisx0208-to-0213-translation-pair ?視)
      (make-jisx0208-to-0213-translation-pair ?屡)
      (make-jisx0208-to-0213-translation-pair ?蕊)
      (make-jisx0208-to-0213-translation-pair ?煮)
      (make-jisx0208-to-0213-translation-pair ?社)
      (make-jisx0208-to-0213-translation-pair ?者)
      (make-jisx0208-to-0213-translation-pair ?繍)
      (make-jisx0208-to-0213-translation-pair ?臭)
      (make-jisx0208-to-0213-translation-pair ?祝)
      (make-jisx0208-to-0213-translation-pair ?暑)
      (make-jisx0208-to-0213-translation-pair ?渚)
      (make-jisx0208-to-0213-translation-pair ?緒)
      (make-jisx0208-to-0213-translation-pair ?署)
      (make-jisx0208-to-0213-translation-pair ?諸)
      (make-jisx0208-to-0213-translation-pair ?渉)
      (make-jisx0208-to-0213-translation-pair ?祥)
      (make-jisx0208-to-0213-translation-pair ?蒋)
      (make-jisx0208-to-0213-translation-pair ?醤)
      (make-jisx0208-to-0213-translation-pair ?状)
      (make-jisx0208-to-0213-translation-pair ?神)
      (make-jisx0208-to-0213-translation-pair ?靭)
      (make-jisx0208-to-0213-translation-pair ?瀬)
      (make-jisx0208-to-0213-translation-pair ?節)
      (make-jisx0208-to-0213-translation-pair ?蝉)
      (make-jisx0208-to-0213-translation-pair ?賎)
      (make-jisx0208-to-0213-translation-pair ?祖)
      (make-jisx0208-to-0213-translation-pair ?僧)
      (make-jisx0208-to-0213-translation-pair ?層)
      (make-jisx0208-to-0213-translation-pair ?掻)
      (make-jisx0208-to-0213-translation-pair ?巣)
      (make-jisx0208-to-0213-translation-pair ?増)
      (make-jisx0208-to-0213-translation-pair ?憎)
      (make-jisx0208-to-0213-translation-pair ?贈)
      (make-jisx0208-to-0213-translation-pair ?即)
      (make-jisx0208-to-0213-translation-pair ?騨)
      (make-jisx0208-to-0213-translation-pair ?琢)
      (make-jisx0208-to-0213-translation-pair ?嘆)
      (make-jisx0208-to-0213-translation-pair ?箪)
      (make-jisx0208-to-0213-translation-pair ?猪)
      (make-jisx0208-to-0213-translation-pair ?著)
      (make-jisx0208-to-0213-translation-pair ?徴)
      (make-jisx0208-to-0213-translation-pair ?懲)
      (make-jisx0208-to-0213-translation-pair ?塚)
      (make-jisx0208-to-0213-translation-pair ?掴)
      (make-jisx0208-to-0213-translation-pair ?壷)
      (make-jisx0208-to-0213-translation-pair ?禎)
      (make-jisx0208-to-0213-translation-pair ?填)
      (make-jisx0208-to-0213-translation-pair ?顛)
      (make-jisx0208-to-0213-translation-pair ?都)
      (make-jisx0208-to-0213-translation-pair ?砺)
      (make-jisx0208-to-0213-translation-pair ?梼)
      (make-jisx0208-to-0213-translation-pair ?涛)
      (make-jisx0208-to-0213-translation-pair ?祷)
      (make-jisx0208-to-0213-translation-pair ?徳)
      (make-jisx0208-to-0213-translation-pair ?涜)
      (make-jisx0208-to-0213-translation-pair ?突)
      (make-jisx0208-to-0213-translation-pair ?難)
      (make-jisx0208-to-0213-translation-pair ?迩)
      (make-jisx0208-to-0213-translation-pair ?嚢)
      (make-jisx0208-to-0213-translation-pair ?梅)
      (make-jisx0208-to-0213-translation-pair ?蝿)
      (make-jisx0208-to-0213-translation-pair ?溌)
      (make-jisx0208-to-0213-translation-pair ?醗)
      (make-jisx0208-to-0213-translation-pair ?繁)
      (make-jisx0208-to-0213-translation-pair ?晩)
      (make-jisx0208-to-0213-translation-pair ?卑)
      (make-jisx0208-to-0213-translation-pair ?碑)
      (make-jisx0208-to-0213-translation-pair ?桧)
      (make-jisx0208-to-0213-translation-pair ?賓)
      (make-jisx0208-to-0213-translation-pair ?頻)
      (make-jisx0208-to-0213-translation-pair ?敏)
      (make-jisx0208-to-0213-translation-pair ?瓶)
      (make-jisx0208-to-0213-translation-pair ?侮)
      (make-jisx0208-to-0213-translation-pair ?福)
      (make-jisx0208-to-0213-translation-pair ?併)
      (make-jisx0208-to-0213-translation-pair ?塀)
      (make-jisx0208-to-0213-translation-pair ?勉)
      (make-jisx0208-to-0213-translation-pair ?歩)
      (make-jisx0208-to-0213-translation-pair ?頬)
      (make-jisx0208-to-0213-translation-pair ?墨)
      (make-jisx0208-to-0213-translation-pair ?毎)
      (make-jisx0208-to-0213-translation-pair ?槙)
      (make-jisx0208-to-0213-translation-pair ?侭)
      (make-jisx0208-to-0213-translation-pair ?免)
      (make-jisx0208-to-0213-translation-pair ?麺)
      (make-jisx0208-to-0213-translation-pair ?戻)
      (make-jisx0208-to-0213-translation-pair ?薮)
      (make-jisx0208-to-0213-translation-pair ?祐)
      (make-jisx0208-to-0213-translation-pair ?遥)
      (make-jisx0208-to-0213-translation-pair ?莱)
      (make-jisx0208-to-0213-translation-pair ?頼)
      (make-jisx0208-to-0213-translation-pair ?欄)
      (make-jisx0208-to-0213-translation-pair ?隆)
      (make-jisx0208-to-0213-translation-pair ?虜)
      (make-jisx0208-to-0213-translation-pair ?緑)
      (make-jisx0208-to-0213-translation-pair ?涙)
      (make-jisx0208-to-0213-translation-pair ?類)
      (make-jisx0208-to-0213-translation-pair ?暦)
      (make-jisx0208-to-0213-translation-pair ?歴)
      (make-jisx0208-to-0213-translation-pair ?練)
      (make-jisx0208-to-0213-translation-pair ?錬)
      (make-jisx0208-to-0213-translation-pair ?廊)
      (make-jisx0208-to-0213-translation-pair ?朗)
      (make-jisx0208-to-0213-translation-pair ?篭)
      (make-jisx0208-to-0213-translation-pair ?蝋)
      (make-jisx0208-to-0213-translation-pair ?郎)
      (make-jisx0208-to-0213-translation-pair ?録)
      (make-jisx0208-to-0213-translation-pairs ?孁 ?巋)
      (make-jisx0208-to-0213-translation-pair ?儘)
      (make-jisx0208-to-0213-translation-pair ?壺)
      (make-jisx0208-to-0213-translation-pair ?攪)
      (make-jisx0208-to-0213-translation-pair ?攅)
      (make-jisx0208-to-0213-translation-pair ?檜)
      (make-jisx0208-to-0213-translation-pair ?檮)
      (make-jisx0208-to-0213-translation-pair ?濤)
      (make-jisx0208-to-0213-translation-pair ?灌)
      (make-jisx0208-to-0213-translation-pair ?煕)
      (make-jisx0208-to-0213-translation-pair ?瑶)
      (make-jisx0208-to-0213-translation-pair ?礦)
      (make-jisx0208-to-0213-translation-pair ?礪)
      (make-jisx0208-to-0213-translation-pair ?竈)
      (make-jisx0208-to-0213-translation-pair ?籠)
      (make-jisx0208-to-0213-translation-pair ?蘂)
      (make-jisx0208-to-0213-translation-pair ?藪)
      (make-jisx0208-to-0213-translation-pair ?蠣)
      (make-jisx0208-to-0213-translation-pair ?蠅)
      (make-jisx0208-to-0213-translation-pair ?諫)
      (make-jisx0208-to-0213-translation-pair ?賤)
      (make-jisx0208-to-0213-translation-pair ?邇)
      (make-jisx0208-to-0213-translation-pair ?靱)
      (make-jisx0208-to-0213-translation-pair ?頸)
      (make-jisx0208-to-0213-translation-pair ?鰺)
      (make-jisx0208-to-0213-translation-pair ?鶯)
      (make-jisx0208-to-0213-translation-pairs ?堯 ?熙)
      (make-jisx0208-to-0213-translation-pairs ?巢 ?龢)
      ))))

;;;
;;; JIS X 0213のISO-2022系統のcoding-systemの定義
;;;

(make-coding-system
 'iso-2022-jp-3-compatible 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3),
compatible to ISO-2022-JP."
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   ;; All JIS X 0213 characters compatible to JIS X 0208 will be
   ;; translated to JIS X 0208 equivalents before encoding.
   (translation-table-for-encode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'iso-2022-jp-3-strict 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3),
where JIS X 0208 characters would be encoded as ESC $ B as possible as
it can."
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   (translation-table-for-encode . ,(get 'jisx0208-to-jisx0213-restricted
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'iso-2022-jp-3 2 ?J
 "ISO 2022 based 7bit encoding for JIS X 0213 (MIME:ISO-2022-JP-3)"
 '((ascii japanese-jisx0213-1 japanese-jisx0213-2) nil nil nil
   short ascii-eol ascii-cntl seven)
 `((safe-charsets ascii japanese-jisx0208
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . iso-2022-jp-3)
   ;; All JIS X 0208 characters will be translated to JIS X 0213
   ;; equivalents before encoding.
   (translation-table-for-encode . ,(get 'jisx0208/0212-to-jisx0213 
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

(make-coding-system
 'euc-jisx0213 2 ?E
 "ISO 2022 based EUC encoding for JIS X 0213 (MIME:EUC-JISX0213)"
 '(ascii japanese-jisx0213-1 katakana-jisx0201 japanese-jisx0213-2
   short ascii-eol ascii-cntl nil nil single-shift)
 `((safe-charsets ascii katakana-jisx0201 japanese-jisx0208 
                  japanese-jisx0212
                  japanese-jisx0213-1 japanese-jisx0213-2)
   (mime-charset . euc-jisx0213)
   (translation-table-for-encode . ,(get 'jisx0208/0212-to-jisx0213 
                                         'translation-table))
   (translation-table-for-decode . ,(get 'jisx0213-to-jisx0208/0212
                                         'translation-table))))

;;;
;;; Shift-JIS
;;;
(eval-and-compile
  (register-code-conversion-map
   'jisx0213-shift-jis-plain-2-odd-decode-map
   (apply (function vector)
	  ?\xF0
	  (mapcar
	   (lambda (x) (+ x 32))
	   '(1 3 5 13 15 79 81 83 85 87 89 91 93))))
  (register-code-conversion-map
   'jisx0213-shift-jis-plain-2-even-decode-map
   (apply (function vector)
	  ?\xF0
	  (mapcar
	   (lambda (x) (+ x 32))
	   '(8 4 12 14 78 80 82 84 86 88 90 92 94)))))

(defvar shift-jisx0213-coding-system-alist
  '((safe-charsets .
		   (ascii
		    japanese-jisx0208
		    katakana-jisx0201
		    japanese-jisx0213-1
		    japanese-jisx0213-2))
    (mime-charset . shift_jisx0213))
  "An alist for japanese-shift-jisx0213 coding systems.")

(eval-when-compile
  (defun jisx0213-shift-jis-template (tr-table read write &optional macp)
    (mucs-ccl-stream-form
     (mucs-ccl-read 'char-2 read)
     (if macp
	 (if (eq read 'emacs-mule)
	     '((if (r0 == ?\x0A) ((r0 = ?\x0D))))
	   '((if (r0 == ?\x0D) ((r0 = ?\x0A))))))
     `((translate-character ,tr-table r1 r0))
     (mucs-ccl-write write))))

(mucs-define-package
 x0213-csys

 (mucs-define-conversion
  shift-jisx0213-unix-stream-encoder
  stream
  (1 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis))))

 (mucs-define-conversion
  shift-jisx0213-unix-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule))))

 (mucs-define-conversion
  shift-jisx0213-dos-stream-encoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis-dos))))

 (mucs-define-conversion
  shift-jisx0213-dos-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule-dos))))

 (mucs-define-conversion
  shift-jisx0213-mac-stream-encoder
  stream
  (1 ((jisx0213-shift-jis-template
       'jisx0208-to-jisx0213
       'emacs-mule 'shift-jis t))))

 (mucs-define-conversion
  shift-jisx0213-mac-stream-decoder
  stream
  (2 ((jisx0213-shift-jis-template
       'jisx0213-to-jisx0208/0212
       'shift-jis 'emacs-mule t))))

 ;;coding system definition

 (mucs-define-coding-system
  'japanese-shift-jisx0213-unix ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-unix-stream-decoder
  'shift-jisx0213-unix-stream-encoder
  shift-jisx0213-coding-system-alist
  'unix)

 (mucs-define-coding-system
  'japanese-shift-jisx0213-dos ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-dos-stream-decoder
  'shift-jisx0213-dos-stream-encoder
  shift-jisx0213-coding-system-alist
  'dos)

 (mucs-define-coding-system
  'japanese-shift-jisx0213-mac ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-mac-stream-decoder
  'shift-jisx0213-mac-stream-encoder
  shift-jisx0213-coding-system-alist
  'mac)

 (mucs-define-coding-system
  'japanese-shift-jisx0213 ?S
  "Shift_JISX0213 encoding for Japanese (MIME: Shift_JISX0213)."
  'shift-jisx0213-unix-stream-decoder
  'shift-jisx0213-unix-stream-encoder
  shift-jisx0213-coding-system-alist
  [japanese-shift-jisx0213-unix
   japanese-shift-jisx0213-dos
   japanese-shift-jisx0213-mac])

 (mapcar
  (lambda (x)
    (let ((master (car x))
	  (aliases (cdr x)))
      (coding-system-put master 'alias-coding-systems
			 '(japanese-shift-jisx0213))
      (while aliases
	(define-coding-system-alias
	  (car aliases) master)
	(setq aliases (cdr aliases)))))
  '((japanese-shift-jisx0213 shift_jisx0213)
    (japanese-shift-jisx0213-unix shift_jisx0213-unix)
    (japanese-shift-jisx0213-dos shift_jisx0213-dos)
    (japanese-shift-jisx0213-mac shift_jisx0213-mac)))
 )

;;
;; langauge-info-alist update.
;;

(set-language-info "Japanese" 'coding-priority
		   (let ((cand
			  '(iso-2022-jp-3-compatible
			    utf-8 utf-16-le utf-16-be
			    euc-jisx0213 japanese-shift-jisx0213
			    iso-2022-jp-2))
			 cs catlist result)
		     (while cand
		       (setq cs (car cand)
			     cand (cdr cand))
		       (if (and (coding-system-p cs)
				(not (memq (coding-system-category cs)
					   catlist)))
			   (setq result (cons cs result)
				 catlist (cons (coding-system-category cs)
					       catlist))))
		     (nreverse result)))

(coding-system-put 'japanese-shift-jisx0213 'coding-category
                   'coding-category-sjis)

(set-language-info "Japanese" 'coding-system 
                   '(iso-2022-jp euc-jisx0213 iso-2022-jp-3
		     japanese-shift-jisx0213
                     japanese-iso-8bit japanese-shift-jis 
                     japanese-iso-7bit-1978-irv iso-2022-jp-2))

(provide 'x0213-csys)

;; -*- coding: iso-2022-jp-3  -*-
;;; egg-sim-jisx0213.el 
;;;     --- Egg Simple Input for JIS X 0213.

;; Copyright (C) 2000 KAWABATA, Taichi <batta@beige.ocn.ne.jp>

;; Keywords: Tamago, egg, multilingual, JIS X 0213

;; JIS X 0213 付属書 4 に基づく記号類分類（若干変更）

(require 'egg-sim)
(require 'x0213-util)
(require 'x0213-char)

(defun make-0213char-list (from to)
  (mapcar '(lambda (x)
             (let ((split (split-char x)))
               (setcar split 'japanese-jisx0213-1)
               (jisx0213-to-jisx0208-string
                (char-to-string (apply 'make-char split)))))
          (make-chars-list from to)))

(defvar egg-sim-jisx0213-menu
  `("JISX0213" .
    (menu "JIS X 0213:" 
          (("JIS1面入力" . japanese-jisx0213-1)
           ("JIS2面入力" . japanese-jisx0213-1)
           ("かな" . 
            (menu "ひらがな" ,(make-0213char-list ?ぁ ?こ゚)))
           ("カナ" . 
            (menu "カタカナ" 
                  (,@(make-0213char-list ?ァ ?ト゚)
                   ,@(make-0213char-list ?ㇰ ?ㇿ)
                   ,@(make-0213char-list ?ヷ ?ヺ))))
           ("間隔" .
            (menu "間隔文字" (("　(和)" . "　") 
                              (" (欧NBSP)" . " ") "␣")))
           ("記述" .
            (menu "記述記号" 
                  (("横棒" .
                    (menu "ハイフン・ダッシュ"
                          (("‐(四分ハイフン)" . "‐")
                           ("–(二分ダーシ)" . "–")
                           ("－(ハイフンマイナス)" . "－")
                           ("­(ソフトハイフン)" . "­") ;; 互換文字
                           ("―(ダッシュ)" . "―")
                           ("゠(二重ハイフン)" . "゠"))))
                    ,@(make-0213char-list ?、 ?．)
                    ("・(和)" . "・") ("·(欧)" . "·")
                    ,@(make-0213char-list ?： ?！)
                    "¡" "¿"
                    ,@(make-0213char-list ?‼ ?⁉)
                    ,@(make-0213char-list ?／ ?‥))))
           ("diacr." .
            (menu "ダイアクリティカルマーク"
                  (("゛(濁音)" . "゛")
                   ("゜(半濁音)" . "゜")
                   ,@(make-0213char-list ?゛?＾)
                   ("￣(overline)" . "￣")
                   ("¯(macron)" . "¯")
                   "＿" "~" "¸" "˘" "˛" "ˇ" "˝" "˙" "‿"
                   ,@(make-0213char-list ?ˈ ?ˑ)
                   ,@(make-0213char-list ?˥ ?˥˩))))
           ("diacr.(合成可)" .
            (menu "ダイアクリティカル(合成可能)"
                  ,(mapcar 'char-to-string jisx0213-combining-chars)))
           ("かな漢字準" .
            (menu "かな・漢字に準じる文字"
                  (,@(make-0213char-list ?ヽ ?ー)
                   ,@(make-0213char-list ?〳 ?ゟ))))
           ("括弧" .
            (menu "括弧" 
                  (,@(make-0213char-list ?‘ ?】)
                   ,@(make-0213char-list ?⦅ ?〗)
                   "«" "»" "〝" "〟")))
           ("学術" . 
            (menu "学術記号"
                  (,@(make-0213char-list ?＋ ?♀)
                   ,@(make-0213char-list ?∈ ?∦)
                   ,@(make-0213char-list ?∠ ?↔)
                   ,@(make-0213char-list ?∓ ?ℏ)
                   "⧺" "⧻" "⋚" "⋛" "∮" "∟")))
           ("単位" .
            (menu "単位・通貨"
                  (("°(度)" . "°")
                   ("′(分)" . "′")
                   ("″(秒)" . "″")
                   ,@(make-0213char-list ?℃ ?％)
                   ,@(make-0213char-list ?㏋ ?℧)
                   "Å" "‰" "€" "¤"
                   ,@(make-0213char-list ?㍉ ?㎡)))) ;; 互換文字
           ;; 一般と図形の区別は、川幡のdiscretion
           ("一般" .
            (menu "一般"
                  (,@(make-0213char-list ?＃ ?§)
                   "※" "〒" "〓" "¶"
                   ("＇(中立アポストロフィ)" . "＇")
                   ("＂(中立引用符)" . "＂")
                   "†" 
                   ("‡(ダブルダガー)" . "‡")
                   ("⁑(ダブルアステ)" . "⁑")
                   "⁂" 
                   "¦" "©" "®" 
                   ("ª(女性序数)" . "ª")
                   ("º(男性序数)" . "º")
                   "﹆" "﹅" "◦" "•"
                   "✓" "␣")))
           ("図形" .
            (menu "図形・絵"
                  (,@(make-0213char-list ?☆ ?▼)
                   "◯"
                   ,@(make-0213char-list ?▷ ?◀)
                   "⦿" "◉" "〽"
                   ,@(make-0213char-list ?♤ ?♣)
                   ,@(make-0213char-list ?☖ ?▱)
                   ,@(make-0213char-list ?◐ ?◓)
                   "⊿" "❖" "☞" "⌘" "⏎")))
           ("矢印" .
            (menu "矢印"
                  (,@(make-0213char-list ?→ ?↓)
                   ,@(make-0213char-list ?↗ ?⤵)
                  "↔" "⇒" "⇔" "⏎"))) ;; 学術記号・図形の矢印
           ("音符" .
            (menu "音符"
                  (,@(make-0213char-list ?♯ ?♪)
                   ,@(make-0213char-list ?♮ ?♩))))
           ("数字" .
            (menu "数字・分数" 
                  (("数字" .
                    (menu "数字" ,(make-0213char-list ?０ ?９)))
                   ("ローマ数字"  . 
                    (menu "ローマ数字"
                          (,@(make-0213char-list ?ⅰ ?ⅻ)
                           ,@(make-0213char-list ?Ⅰ ?Ⅺ)
                           "Ⅻ")))
                   ("丸付き" . 
                    (menu "丸付き数字" 
                          (,@(make-0213char-list ?① ?⑳)
                           ,@(make-0213char-list ?㉑ ?㊿))))
                   ("黒丸" .
                    (menu "黒丸付き数字" ,(make-0213char-list ?❶ ?⓴)))
                   ("二重丸" .
                    (menu "二重丸付き数字" ,(make-0213char-list ?⓵ ?⓾)))
                   ("分数" .
                    (menu "分数"
                          (,@(make-0213char-list ?¼ ?¾)
                           ,@(make-0213char-list ?⅓ ?⅕))))
                   ("上付き" .
                    (menu "上付き" ("¹" "²" "³"))))))
           ("丸付き文字" . 
            (menu "丸付き文字"
                  (("ラテン小文字" .
                    (menu "丸付きラテン小文字"
                          ,(make-0213char-list ?ⓐ ?ⓩ)))
                   ("カタカナ" .
                    (menu "丸付きカタカナ"
                          ,(make-0213char-list ?㋐ ?㋬)))
                   "＠" "⊕" "⊖" "⊗" "©" "®" 
                   ,@(make-0213char-list ?㊤ ?㊨))))
           ("元号" .
            (menu "元号" ("㍻" "㍼" "㍽" "㍾")))
           ("略号" . 
            (menu "略号" 
                  (,@(make-0213char-list ?№ ?℡)
                   ,@(make-0213char-list ?㈱ ?㈹))))
           ("罫線・歯科" . 
            (menu "罫線・歯科記号" 
                  (,@(make-0213char-list ?─ ?╂)
                   ,@(make-0213char-list ?⎾ ?⏌))))
           ("ラテン文字" .
            (menu "ラテン文字" 
                  (("基本ラテン文字" .
                    (menu "基本ラテン文字"
                          (,@(make-0213char-list ?Ａ ?Ｚ)
                           ,@(make-0213char-list ?ａ ?ｚ))))
                   ("修飾付き基本ラテン大文字" .
                    (menu "修飾付き基本ラテン大文字"
                          ,(mapcar 
                            'char-to-string
                            (sort-char-by-charname
                             `(?Ḿ ?Ǹ
                               ,@(make-chars-list ?À ?Ý)
                               ,@(make-chars-list ?Ā ?Ō)
                               ?Ą
                               ,@(make-chars-list ?Ł ?Ż)
                               ,@(make-chars-list ?Ŕ ?Ţ)
                               ,@(make-chars-list ?Ĉ ?Ŭ))))))
                   ("修飾付き基本ラテン小文字" .
                    (menu "修飾付き基本ラテン小文字"
                          ,(mapcar
                            'char-to-string
                            (sort-char-by-charname
                             `(?ḿ ?ǹ ?ǒ ?ǔ ?ǖ ?ǘ ?ǚ ?ǜ
                               ,@(make-chars-list ?à ?ÿ)
                               ,@(make-chars-list ?ā ?ō)
                               ?ą
                               ,@(make-chars-list ?ł ?ś)
                               ,@(make-chars-list ?š ?ź)
                               ,@(make-chars-list ?ž ?ż)
                               ,@(make-chars-list ?ŕ ?ţ)
                               ,@(make-chars-list ?ĉ ?ŭ))))))
                   ("非基本ラテン文字・IPA記号" .
                    (menu "非基本ラテン文字・IPA記号"
                          ,(mapcar
                            'char-to-string
                            (sort-char-by-charname
                             `(?Þ ?ß
                               ,@(make-chars-list ?ʋ ?έ)))))))))
           ("ギリシャ文字" . 
            (menu "ギリシャ文字"
                  (,@(make-0213char-list ?Α ?Ω)
                   ,@(make-0213char-list ?α ?ς))))
           ("キリル文字" . 
            (menu "キリル文字"
                  (,@(make-0213char-list ?А ?Я)
                   ,@(make-0213char-list ?а ?я))))
           ("第三水準" .
            (menu "JIS第三水準"
                  (,@(make-0213char-list ?𠀋 ?嬥)
                   ,@(make-0213char-list ?孁 ?巋)
                   ,@(make-0213char-list ?巢 ?龢))))
           ("第四水準" .
            (menu "JIS第四水準"
                  (,@(make-char-list 'japanese-jisx0213-2 1 1)
                   ,@(make-char-list 'japanese-jisx0213-2 3 5)
                   ,@(make-char-list 'japanese-jisx0213-2 8 8)
                   ,@(make-char-list 'japanese-jisx0213-2 12 15)
                   ,@(make-char-list 'japanese-jisx0213-2 78 94))))))))

(setcdr (nthcdr 3 (caddr egg-sim-japanese-menu))
        (list egg-sim-jisx0213-menu))

(provide 'egg-sim-jisx0213)

;;; -*- coding: iso-2022-7bit  -*-
;;; usisheng.el --- tables between Unicode and Mule sisheng charset.

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, GB 2312

;; Miyashita Hisashi(himi@bird.scphys.kyoto-u.ac.jp)
;; converted mule-unicode.txt created by TAKAHASHI Naoto.

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

(put 'chinese-sisheng 'unicode-assoc
     'chinese-sisheng-vs-unicode-assoc)

(defvar
  chinese-sisheng-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?(0!(B . "0x0101") ;; LATIN SMALL LETTER A WITH MACRON
       (?(0"(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?(0#(B . "0x01CE") ;; LATIN SMALL LETTER A WITH CARON
       (?(0$(B . "0x00E0") ;; LATIN SMALL LETTER A WITH GRAVE
       (?(0%(B . "0x0113") ;; LATIN SMALL LETTER E WITH MACRON
       (?(0&(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?(0'(B . "0x011B") ;; LATIN SMALL LETTER E WITH CARON
       (?(0((B . "0x00E8") ;; LATIN SMALL LETTER E WITH GRAVE
       (?(0)(B . "0x012B") ;; LATIN SMALL LETTER I WITH MACRON
       (?(0*(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?(0+(B . "0x01D0") ;; LATIN SMALL LETTER I WITH CARON
       (?(0,(B . "0x00EC") ;; LATIN SMALL LETTER I WITH GRAVE
       (?(0-(B . "0x014D") ;; LATIN SMALL LETTER O WITH MACRON
       (?(0.(B . "0x00F3") ;; LATIN SMALL LETTER O WITH ACUTE
       (?(0/(B . "0x01D2") ;; LATIN SMALL LETTER O WITH CARON
       (?(00(B . "0x00F2") ;; LATIN SMALL LETTER O WITH GRAVE
       (?(01(B . "0x016B") ;; LATIN SMALL LETTER U WITH MACRON
       (?(02(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?(03(B . "0x01D4") ;; LATIN SMALL LETTER U WITH CARON
       (?(04(B . "0x00F9") ;; LATIN SMALL LETTER U WITH GRAVE
       (?(05(B . "0x01D6") ;; LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
       (?(06(B . "0x01D8") ;; LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
       (?(07(B . "0x01DA") ;; LATIN SMALL LETTER U WITH DIAERESIS AND CARON
       (?(08(B . "0x01DC") ;; LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
       (?(09(B . "0x00FC") ;; LATIN SMALL LETTER U WITH DIAERESIS
       (?(0:(B . "0x00EA") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?(0<(B . "0x1E3F") ;; LATIN SMALL LETTER M WITH ACUTE
       (?(0=(B . "0x0144") ;; LATIN SMALL LETTER N WITH ACUTE
       (?(0>(B . "0x0148") ;; LATIN SMALL LETTER N WITH CARON
       (?(0?(B . "0x01F9") ;; LATIN SMALL LETTER N WITH GRAVE
       (?(0A(B . "0x02C9") ;; MODIFIER LETTER MACRON
       (?(0B(B . "0x02CA") ;; MODIFIER LETTER ACUTE ACCENT
       (?(0C(B . "0x02C7") ;; CARON
       (?(0D(B . "0x02CB") ;; MODIFIER LETTER GRAVE ACCENT
       (?(0E(B . "0x3105") ;; BOPOMOFO LETTER B
       (?(0F(B . "0x3106") ;; BOPOMOFO LETTER P
       (?(0G(B . "0x3107") ;; BOPOMOFO LETTER M
       (?(0H(B . "0x3108") ;; BOPOMOFO LETTER F
       (?(0I(B . "0x3109") ;; BOPOMOFO LETTER D
       (?(0J(B . "0x310A") ;; BOPOMOFO LETTER T
       (?(0K(B . "0x310B") ;; BOPOMOFO LETTER N
       (?(0L(B . "0x310C") ;; BOPOMOFO LETTER L
       (?(0M(B . "0x310D") ;; BOPOMOFO LETTER G
       (?(0N(B . "0x310E") ;; BOPOMOFO LETTER K
       (?(0O(B . "0x310F") ;; BOPOMOFO LETTER H
       (?(0P(B . "0x3110") ;; BOPOMOFO LETTER J
       (?(0Q(B . "0x3111") ;; BOPOMOFO LETTER Q
       (?(0R(B . "0x3112") ;; BOPOMOFO LETTER X
       (?(0S(B . "0x3113") ;; BOPOMOFO LETTER ZH
       (?(0T(B . "0x3114") ;; BOPOMOFO LETTER CH
       (?(0U(B . "0x3115") ;; BOPOMOFO LETTER SH
       (?(0V(B . "0x3116") ;; BOPOMOFO LETTER R
       (?(0W(B . "0x3117") ;; BOPOMOFO LETTER Z
       (?(0X(B . "0x3118") ;; BOPOMOFO LETTER C
       (?(0Y(B . "0x3119") ;; BOPOMOFO LETTER S
       (?(0Z(B . "0x311A") ;; BOPOMOFO LETTER A
       (?(0[(B . "0x311B") ;; BOPOMOFO LETTER O
       (?(0\(B . "0x311C") ;; BOPOMOFO LETTER E
       (?(0](B . "0x311D") ;; BOPOMOFO LETTER EH
       (?(0^(B . "0x311E") ;; BOPOMOFO LETTER AI
       (?(0_(B . "0x311F") ;; BOPOMOFO LETTER EI
       (?(0`(B . "0x3120") ;; BOPOMOFO LETTER AU
       (?(0a(B . "0x3121") ;; BOPOMOFO LETTER OU
       (?(0b(B . "0x3122") ;; BOPOMOFO LETTER AN
       (?(0c(B . "0x3123") ;; BOPOMOFO LETTER EN
       (?(0d(B . "0x3124") ;; BOPOMOFO LETTER ANG
       (?(0e(B . "0x3125") ;; BOPOMOFO LETTER ENG
       (?(0f(B . "0x3126") ;; BOPOMOFO LETTER ER
       (?(0g(B . "0x3127") ;; BOPOMOFO LETTER I
       (?(0h(B . "0x3128") ;; BOPOMOFO LETTER U
       (?(0i(B . "0x3129") ;; BOPOMOFO LETTER IU
       ))))

(provide 'usisheng)

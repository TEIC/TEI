;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-9.el --- tables between UCS and ISO-8859-9

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, ISO8859

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

;;; This file is converted from 8859-9.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'latin-iso8859-9 'unicode-assoc
     'iso-8859-9-vs-unicode-assoc)

(defvar
  iso-8859-9-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,M (B . "0x00A0") ;; NO-BREAK SPACE
       (?,M!(B . "0x00A1") ;; INVERTED EXCLAMATION MARK
       (?,M"(B . "0x00A2") ;; CENT SIGN
       (?,M#(B . "0x00A3") ;; POUND SIGN
       (?,M$(B . "0x00A4") ;; CURRENCY SIGN
       (?,M%(B . "0x00A5") ;; YEN SIGN
       (?,M&(B . "0x00A6") ;; BROKEN BAR
       (?,M'(B . "0x00A7") ;; SECTION SIGN
       (?,M((B . "0x00A8") ;; DIAERESIS
       (?,M)(B . "0x00A9") ;; COPYRIGHT SIGN
       (?,M*(B . "0x00AA") ;; FEMININE ORDINAL INDICATOR
       (?,M+(B . "0x00AB") ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,M,(B . "0x00AC") ;; NOT SIGN
       (?,M-(B . "0x00AD") ;; SOFT HYPHEN
       (?,M.(B . "0x00AE") ;; REGISTERED SIGN
       (?,M/(B . "0x00AF") ;; MACRON
       (?,M0(B . "0x00B0") ;; DEGREE SIGN
       (?,M1(B . "0x00B1") ;; PLUS-MINUS SIGN
       (?,M2(B . "0x00B2") ;; SUPERSCRIPT TWO
       (?,M3(B . "0x00B3") ;; SUPERSCRIPT THREE
       (?,M4(B . "0x00B4") ;; ACUTE ACCENT
       (?,M5(B . "0x00B5") ;; MICRO SIGN
       (?,M6(B . "0x00B6") ;; PILCROW SIGN
       (?,M7(B . "0x00B7") ;; MIDDLE DOT
       (?,M8(B . "0x00B8") ;; CEDILLA
       (?,M9(B . "0x00B9") ;; SUPERSCRIPT ONE
       (?,M:(B . "0x00BA") ;; MASCULINE ORDINAL INDICATOR
       (?,M;(B . "0x00BB") ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,M<(B . "0x00BC") ;; VULGAR FRACTION ONE QUARTER
       (?,M=(B . "0x00BD") ;; VULGAR FRACTION ONE HALF
       (?,M>(B . "0x00BE") ;; VULGAR FRACTION THREE QUARTERS
       (?,M?(B . "0x00BF") ;; INVERTED QUESTION MARK
       (?,M@(B . "0x00C0") ;; LATIN CAPITAL LETTER A WITH GRAVE
       (?,MA(B . "0x00C1") ;; LATIN CAPITAL LETTER A WITH ACUTE
       (?,MB(B . "0x00C2") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,MC(B . "0x00C3") ;; LATIN CAPITAL LETTER A WITH TILDE
       (?,MD(B . "0x00C4") ;; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,ME(B . "0x00C5") ;; LATIN CAPITAL LETTER A WITH RING ABOVE
       (?,MF(B . "0x00C6") ;; LATIN CAPITAL LETTER AE
       (?,MG(B . "0x00C7") ;; LATIN CAPITAL LETTER C WITH CEDILLA
       (?,MH(B . "0x00C8") ;; LATIN CAPITAL LETTER E WITH GRAVE
       (?,MI(B . "0x00C9") ;; LATIN CAPITAL LETTER E WITH ACUTE
       (?,MJ(B . "0x00CA") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
       (?,MK(B . "0x00CB") ;; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,ML(B . "0x00CC") ;; LATIN CAPITAL LETTER I WITH GRAVE
       (?,MM(B . "0x00CD") ;; LATIN CAPITAL LETTER I WITH ACUTE
       (?,MN(B . "0x00CE") ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,MO(B . "0x00CF") ;; LATIN CAPITAL LETTER I WITH DIAERESIS
       (?,MP(B . "0x011E") ;; LATIN CAPITAL LETTER G WITH BREVE
       (?,MQ(B . "0x00D1") ;; LATIN CAPITAL LETTER N WITH TILDE
       (?,MR(B . "0x00D2") ;; LATIN CAPITAL LETTER O WITH GRAVE
       (?,MS(B . "0x00D3") ;; LATIN CAPITAL LETTER O WITH ACUTE
       (?,MT(B . "0x00D4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,MU(B . "0x00D5") ;; LATIN CAPITAL LETTER O WITH TILDE
       (?,MV(B . "0x00D6") ;; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,MW(B . "0x00D7") ;; MULTIPLICATION SIGN
       (?,MX(B . "0x00D8") ;; LATIN CAPITAL LETTER O WITH STROKE
       (?,MY(B . "0x00D9") ;; LATIN CAPITAL LETTER U WITH GRAVE
       (?,MZ(B . "0x00DA") ;; LATIN CAPITAL LETTER U WITH ACUTE
       (?,M[(B . "0x00DB") ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
       (?,M\(B . "0x00DC") ;; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,M](B . "0x0130") ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
       (?,M^(B . "0x015E") ;; LATIN CAPITAL LETTER S WITH CEDILLA
       (?,M_(B . "0x00DF") ;; LATIN SMALL LETTER SHARP S
       (?,M`(B . "0x00E0") ;; LATIN SMALL LETTER A WITH GRAVE
       (?,Ma(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?,Mb(B . "0x00E2") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,Mc(B . "0x00E3") ;; LATIN SMALL LETTER A WITH TILDE
       (?,Md(B . "0x00E4") ;; LATIN SMALL LETTER A WITH DIAERESIS
       (?,Me(B . "0x00E5") ;; LATIN SMALL LETTER A WITH RING ABOVE
       (?,Mf(B . "0x00E6") ;; LATIN SMALL LETTER AE
       (?,Mg(B . "0x00E7") ;; LATIN SMALL LETTER C WITH CEDILLA
       (?,Mh(B . "0x00E8") ;; LATIN SMALL LETTER E WITH GRAVE
       (?,Mi(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?,Mj(B . "0x00EA") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?,Mk(B . "0x00EB") ;; LATIN SMALL LETTER E WITH DIAERESIS
       (?,Ml(B . "0x00EC") ;; LATIN SMALL LETTER I WITH GRAVE
       (?,Mm(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?,Mn(B . "0x00EE") ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,Mo(B . "0x00EF") ;; LATIN SMALL LETTER I WITH DIAERESIS
       (?,Mp(B . "0x011F") ;; LATIN SMALL LETTER G WITH BREVE
       (?,Mq(B . "0x00F1") ;; LATIN SMALL LETTER N WITH TILDE
       (?,Mr(B . "0x00F2") ;; LATIN SMALL LETTER O WITH GRAVE
       (?,Ms(B . "0x00F3") ;; LATIN SMALL LETTER O WITH ACUTE
       (?,Mt(B . "0x00F4") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,Mu(B . "0x00F5") ;; LATIN SMALL LETTER O WITH TILDE
       (?,Mv(B . "0x00F6") ;; LATIN SMALL LETTER O WITH DIAERESIS
       (?,Mw(B . "0x00F7") ;; DIVISION SIGN
       (?,Mx(B . "0x00F8") ;; LATIN SMALL LETTER O WITH STROKE
       (?,My(B . "0x00F9") ;; LATIN SMALL LETTER U WITH GRAVE
       (?,Mz(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?,M{(B . "0x00FB") ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
       (?,M|(B . "0x00FC") ;; LATIN SMALL LETTER U WITH DIAERESIS
       (?,M}(B . "0x0131") ;; LATIN SMALL LETTER DOTLESS I
       (?,M~(B . "0x015F") ;; LATIN SMALL LETTER S WITH CEDILLA
       (?,M(B . "0x00FF") ;; LATIN SMALL LETTER Y WITH DIAERESIS
       ))))

(provide 'uiso8859-9)

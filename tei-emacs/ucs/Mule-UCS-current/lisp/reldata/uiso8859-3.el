;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-3.el --- tables between UCS and ISO-8859-3

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

;;; This file is converted from 8859-3.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'latin-iso8859-3 'unicode-assoc
     'iso-8859-3-vs-unicode-assoc)

(defvar
  iso-8859-3-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,C (B . "0x00A0") ;; NO-BREAK SPACE
       (?,C!(B . "0x0126") ;; LATIN CAPITAL LETTER H WITH STROKE
       (?,C"(B . "0x02D8") ;; BREVE
       (?,C#(B . "0x00A3") ;; POUND SIGN
       (?,C$(B . "0x00A4") ;; CURRENCY SIGN
       (?,C&(B . "0x0124") ;; LATIN CAPITAL LETTER H WITH CIRCUMFLEX
       (?,C'(B . "0x00A7") ;; SECTION SIGN
       (?,C((B . "0x00A8") ;; DIAERESIS
       (?,C)(B . "0x0130") ;; LATIN CAPITAL LETTER I WITH DOT ABOVE
       (?,C*(B . "0x015E") ;; LATIN CAPITAL LETTER S WITH CEDILLA
       (?,C+(B . "0x011E") ;; LATIN CAPITAL LETTER G WITH BREVE
       (?,C,(B . "0x0134") ;; LATIN CAPITAL LETTER J WITH CIRCUMFLEX
       (?,C-(B . "0x00AD") ;; SOFT HYPHEN
       (?,C/(B . "0x017B") ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
       (?,C0(B . "0x00B0") ;; DEGREE SIGN
       (?,C1(B . "0x0127") ;; LATIN SMALL LETTER H WITH STROKE
       (?,C2(B . "0x00B2") ;; SUPERSCRIPT TWO
       (?,C3(B . "0x00B3") ;; SUPERSCRIPT THREE
       (?,C4(B . "0x00B4") ;; ACUTE ACCENT
       (?,C5(B . "0x00B5") ;; MICRO SIGN
       (?,C6(B . "0x0125") ;; LATIN SMALL LETTER H WITH CIRCUMFLEX
       (?,C7(B . "0x00B7") ;; MIDDLE DOT
       (?,C8(B . "0x00B8") ;; CEDILLA
       (?,C9(B . "0x0131") ;; LATIN SMALL LETTER DOTLESS I
       (?,C:(B . "0x015F") ;; LATIN SMALL LETTER S WITH CEDILLA
       (?,C;(B . "0x011F") ;; LATIN SMALL LETTER G WITH BREVE
       (?,C<(B . "0x0135") ;; LATIN SMALL LETTER J WITH CIRCUMFLEX
       (?,C=(B . "0x00BD") ;; VULGAR FRACTION ONE HALF
       (?,C?(B . "0x017C") ;; LATIN SMALL LETTER Z WITH DOT ABOVE
       (?,C@(B . "0x00C0") ;; LATIN CAPITAL LETTER A WITH GRAVE
       (?,CA(B . "0x00C1") ;; LATIN CAPITAL LETTER A WITH ACUTE
       (?,CB(B . "0x00C2") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,CD(B . "0x00C4") ;; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,CE(B . "0x010A") ;; LATIN CAPITAL LETTER C WITH DOT ABOVE
       (?,CF(B . "0x0108") ;; LATIN CAPITAL LETTER C WITH CIRCUMFLEX
       (?,CG(B . "0x00C7") ;; LATIN CAPITAL LETTER C WITH CEDILLA
       (?,CH(B . "0x00C8") ;; LATIN CAPITAL LETTER E WITH GRAVE
       (?,CI(B . "0x00C9") ;; LATIN CAPITAL LETTER E WITH ACUTE
       (?,CJ(B . "0x00CA") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
       (?,CK(B . "0x00CB") ;; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,CL(B . "0x00CC") ;; LATIN CAPITAL LETTER I WITH GRAVE
       (?,CM(B . "0x00CD") ;; LATIN CAPITAL LETTER I WITH ACUTE
       (?,CN(B . "0x00CE") ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,CO(B . "0x00CF") ;; LATIN CAPITAL LETTER I WITH DIAERESIS
       (?,CQ(B . "0x00D1") ;; LATIN CAPITAL LETTER N WITH TILDE
       (?,CR(B . "0x00D2") ;; LATIN CAPITAL LETTER O WITH GRAVE
       (?,CS(B . "0x00D3") ;; LATIN CAPITAL LETTER O WITH ACUTE
       (?,CT(B . "0x00D4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,CU(B . "0x0120") ;; LATIN CAPITAL LETTER G WITH DOT ABOVE
       (?,CV(B . "0x00D6") ;; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,CW(B . "0x00D7") ;; MULTIPLICATION SIGN
       (?,CX(B . "0x011C") ;; LATIN CAPITAL LETTER G WITH CIRCUMFLEX
       (?,CY(B . "0x00D9") ;; LATIN CAPITAL LETTER U WITH GRAVE
       (?,CZ(B . "0x00DA") ;; LATIN CAPITAL LETTER U WITH ACUTE
       (?,C[(B . "0x00DB") ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
       (?,C\(B . "0x00DC") ;; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,C](B . "0x016C") ;; LATIN CAPITAL LETTER U WITH BREVE
       (?,C^(B . "0x015C") ;; LATIN CAPITAL LETTER S WITH CIRCUMFLEX
       (?,C_(B . "0x00DF") ;; LATIN SMALL LETTER SHARP S
       (?,C`(B . "0x00E0") ;; LATIN SMALL LETTER A WITH GRAVE
       (?,Ca(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?,Cb(B . "0x00E2") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,Cd(B . "0x00E4") ;; LATIN SMALL LETTER A WITH DIAERESIS
       (?,Ce(B . "0x010B") ;; LATIN SMALL LETTER C WITH DOT ABOVE
       (?,Cf(B . "0x0109") ;; LATIN SMALL LETTER C WITH CIRCUMFLEX
       (?,Cg(B . "0x00E7") ;; LATIN SMALL LETTER C WITH CEDILLA
       (?,Ch(B . "0x00E8") ;; LATIN SMALL LETTER E WITH GRAVE
       (?,Ci(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?,Cj(B . "0x00EA") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?,Ck(B . "0x00EB") ;; LATIN SMALL LETTER E WITH DIAERESIS
       (?,Cl(B . "0x00EC") ;; LATIN SMALL LETTER I WITH GRAVE
       (?,Cm(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?,Cn(B . "0x00EE") ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,Co(B . "0x00EF") ;; LATIN SMALL LETTER I WITH DIAERESIS
       (?,Cq(B . "0x00F1") ;; LATIN SMALL LETTER N WITH TILDE
       (?,Cr(B . "0x00F2") ;; LATIN SMALL LETTER O WITH GRAVE
       (?,Cs(B . "0x00F3") ;; LATIN SMALL LETTER O WITH ACUTE
       (?,Ct(B . "0x00F4") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,Cu(B . "0x0121") ;; LATIN SMALL LETTER G WITH DOT ABOVE
       (?,Cv(B . "0x00F6") ;; LATIN SMALL LETTER O WITH DIAERESIS
       (?,Cw(B . "0x00F7") ;; DIVISION SIGN
       (?,Cx(B . "0x011D") ;; LATIN SMALL LETTER G WITH CIRCUMFLEX
       (?,Cy(B . "0x00F9") ;; LATIN SMALL LETTER U WITH GRAVE
       (?,Cz(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?,C{(B . "0x00FB") ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
       (?,C|(B . "0x00FC") ;; LATIN SMALL LETTER U WITH DIAERESIS
       (?,C}(B . "0x016D") ;; LATIN SMALL LETTER U WITH BREVE
       (?,C~(B . "0x015D") ;; LATIN SMALL LETTER S WITH CIRCUMFLEX
       (?,C(B . "0x02D9") ;; DOT ABOVE
       ))))

(provide 'uiso8859-3)

;;; uiso8859-15.el --- tables between UCS and ISO-8859-15   -*- coding: iso-2022-7bit  -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

;; This file is a contribution to Mule-UCS.

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:

(put 'latin-iso8859-15 'unicode-assoc 'iso-8859-15-vs-unicode-assoc)

(defvar
  iso-8859-15-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?\,b (B . "0x00A0")			; NO-BREAK SPACE
       (?,b!(B . "0x00A1")			; INVERTED EXCLAMATION MARK
       (?,b"(B . "0x00A2")			; CENT SIGN
       (?,b#(B . "0x00A3")			; POUND SIGN
       (?,b$(B . "0x20AC")			; EURO SIGN
       (?,b%(B . "0x00A5")			; YEN SIGN
       (?,b&(B . "0x0160")			; LATIN CAPITAL LETTER S WITH CARON
       (?,b'(B . "0x00A7")			; SECTION SIGN
       (?,b((B . "0x0161")			; LATIN SMALL LETTER S WITH CARON
       (?,b)(B . "0x00A9")			; COPYRIGHT SIGN
       (?,b*(B . "0x00AA")			; FEMININE ORDINAL INDICATOR
       (?\,b+(B . "0x00AB")			; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,b,(B . "0x00AC")			; NOT SIGN
       (?,b-(B . "0x00AD")			; SOFT HYPHEN
       (?,b.(B . "0x00AE")			; REGISTERED SIGN
       (?,b/(B . "0x00AF")			; MACRON
       (?,b0(B . "0x00B0")			; DEGREE SIGN
       (?,b1(B . "0x00B1")			; PLUS-MINUS SIGN
       (?,b2(B . "0x00B2")			; SUPERSCRIPT TWO
       (?,b3(B . "0x00B3")			; SUPERSCRIPT THREE
       (?,b4(B . "0x017D")			; LATIN CAPITAL LETTER Z WITH CARON
       (?,b5(B . "0x00B5")			; MICRO SIGN
       (?,b6(B . "0x00B6")			; PILCROW SIGN
       (?,b7(B . "0x00B7")			; MIDDLE DOT
       (?,b8(B . "0x017E")			; LATIN SMALL LETTER Z WITH CARON
       (?,b9(B . "0x00B9")			; SUPERSCRIPT ONE
       (?,b:(B . "0x00BA")			; MASCULINE ORDINAL INDICATOR
       (?\,b;(B . "0x00BB")			; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,b<(B . "0x0152")			; LATIN CAPITAL LIGATURE OE
       (?,b=(B . "0x0153")			; LATIN SMALL LIGATURE OE
       (?,b>(B . "0x0178")			; LATIN CAPITAL LETTER Y WITH DIAERESIS
       (?,b?(B . "0x00BF")			; INVERTED QUESTION MARK
       (?,b@(B . "0x00C0")			; LATIN CAPITAL LETTER A WITH GRAVE
       (?,bA(B . "0x00C1")			; LATIN CAPITAL LETTER A WITH ACUTE
       (?,bB(B . "0x00C2")			; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,bC(B . "0x00C3")			; LATIN CAPITAL LETTER A WITH TILDE
       (?,bD(B . "0x00C4")			; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,bE(B . "0x00C5")			; LATIN CAPITAL LETTER A WITH RING ABOVE
       (?,bF(B . "0x00C6")			; LATIN CAPITAL LETTER AE
       (?,bG(B . "0x00C7")			; LATIN CAPITAL LETTER C WITH CEDILLA
       (?,bH(B . "0x00C8")			; LATIN CAPITAL LETTER E WITH GRAVE
       (?,bI(B . "0x00C9")			; LATIN CAPITAL LETTER E WITH ACUTE
       (?,bJ(B . "0x00CA")			; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
       (?,bK(B . "0x00CB")			; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,bL(B . "0x00CC")			; LATIN CAPITAL LETTER I WITH GRAVE
       (?,bM(B . "0x00CD")			; LATIN CAPITAL LETTER I WITH ACUTE
       (?,bN(B . "0x00CE")			; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,bO(B . "0x00CF")			; LATIN CAPITAL LETTER I WITH DIAERESIS
       (?,bP(B . "0x00D0")			; LATIN CAPITAL LETTER ETH
       (?,bQ(B . "0x00D1")			; LATIN CAPITAL LETTER N WITH TILDE
       (?,bR(B . "0x00D2")			; LATIN CAPITAL LETTER O WITH GRAVE
       (?,bS(B . "0x00D3")			; LATIN CAPITAL LETTER O WITH ACUTE
       (?,bT(B . "0x00D4")			; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,bU(B . "0x00D5")			; LATIN CAPITAL LETTER O WITH TILDE
       (?,bV(B . "0x00D6")			; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,bW(B . "0x00D7")			; MULTIPLICATION SIGN
       (?,bX(B . "0x00D8")			; LATIN CAPITAL LETTER O WITH STROKE
       (?,bY(B . "0x00D9")			; LATIN CAPITAL LETTER U WITH GRAVE
       (?,bZ(B . "0x00DA")			; LATIN CAPITAL LETTER U WITH ACUTE
       (?,b[(B . "0x00DB")			; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
       (?,b\(B . "0x00DC")			; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,b](B . "0x00DD")			; LATIN CAPITAL LETTER Y WITH ACUTE
       (?,b^(B . "0x00DE")			; LATIN CAPITAL LETTER THORN
       (?,b_(B . "0x00DF")			; LATIN SMALL LETTER SHARP S
       (?,b`(B . "0x00E0")			; LATIN SMALL LETTER A WITH GRAVE
       (?,ba(B . "0x00E1")			; LATIN SMALL LETTER A WITH ACUTE
       (?,bb(B . "0x00E2")			; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,bc(B . "0x00E3")			; LATIN SMALL LETTER A WITH TILDE
       (?,bd(B . "0x00E4")			; LATIN SMALL LETTER A WITH DIAERESIS
       (?,be(B . "0x00E5")			; LATIN SMALL LETTER A WITH RING ABOVE
       (?,bf(B . "0x00E6")			; LATIN SMALL LETTER AE
       (?,bg(B . "0x00E7")			; LATIN SMALL LETTER C WITH CEDILLA
       (?,bh(B . "0x00E8")			; LATIN SMALL LETTER E WITH GRAVE
       (?,bi(B . "0x00E9")			; LATIN SMALL LETTER E WITH ACUTE
       (?,bj(B . "0x00EA")			; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?,bk(B . "0x00EB")			; LATIN SMALL LETTER E WITH DIAERESIS
       (?,bl(B . "0x00EC")			; LATIN SMALL LETTER I WITH GRAVE
       (?,bm(B . "0x00ED")			; LATIN SMALL LETTER I WITH ACUTE
       (?,bn(B . "0x00EE")			; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,bo(B . "0x00EF")			; LATIN SMALL LETTER I WITH DIAERESIS
       (?,bp(B . "0x00F0")			; LATIN SMALL LETTER ETH
       (?,bq(B . "0x00F1")			; LATIN SMALL LETTER N WITH TILDE
       (?,br(B . "0x00F2")			; LATIN SMALL LETTER O WITH GRAVE
       (?,bs(B . "0x00F3")			; LATIN SMALL LETTER O WITH ACUTE
       (?,bt(B . "0x00F4")			; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,bu(B . "0x00F5")			; LATIN SMALL LETTER O WITH TILDE
       (?,bv(B . "0x00F6")			; LATIN SMALL LETTER O WITH DIAERESIS
       (?,bw(B . "0x00F7")			; DIVISION SIGN
       (?,bx(B . "0x00F8")			; LATIN SMALL LETTER O WITH STROKE
       (?,by(B . "0x00F9")			; LATIN SMALL LETTER U WITH GRAVE
       (?,bz(B . "0x00FA")			; LATIN SMALL LETTER U WITH ACUTE
       (?,b{(B . "0x00FB")			; LATIN SMALL LETTER U WITH CIRCUMFLEX
       (?,b|(B . "0x00FC")			; LATIN SMALL LETTER U WITH DIAERESIS
       (?,b}(B . "0x00FD")			; LATIN SMALL LETTER Y WITH ACUTE
       (?,b~(B . "0x00FE")			; LATIN SMALL LETTER THORN
       (?,b(B . "0x00FF")			; LATIN SMALL LETTER Y WITH DIAERESIS
       ))))

(provide 'uiso8859-15)

;;; uiso8859-15.el ends here

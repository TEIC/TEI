;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-2.el --- tables between UCS and ISO-8859-2

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

;;; This file is converted from 8859-2.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'latin-iso8859-2 'unicode-assoc
     'iso-8859-2-vs-unicode-assoc)

(defvar
  iso-8859-2-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,B (B . "0x00A0") ;; NO-BREAK SPACE
       (?,B!(B . "0x0104") ;; LATIN CAPITAL LETTER A WITH OGONEK
       (?,B"(B . "0x02D8") ;; BREVE
       (?,B#(B . "0x0141") ;; LATIN CAPITAL LETTER L WITH STROKE
       (?,B$(B . "0x00A4") ;; CURRENCY SIGN
       (?,B%(B . "0x013D") ;; LATIN CAPITAL LETTER L WITH CARON
       (?,B&(B . "0x015A") ;; LATIN CAPITAL LETTER S WITH ACUTE
       (?,B'(B . "0x00A7") ;; SECTION SIGN
       (?,B((B . "0x00A8") ;; DIAERESIS
       (?,B)(B . "0x0160") ;; LATIN CAPITAL LETTER S WITH CARON
       (?,B*(B . "0x015E") ;; LATIN CAPITAL LETTER S WITH CEDILLA
       (?,B+(B . "0x0164") ;; LATIN CAPITAL LETTER T WITH CARON
       (?,B,(B . "0x0179") ;; LATIN CAPITAL LETTER Z WITH ACUTE
       (?,B-(B . "0x00AD") ;; SOFT HYPHEN
       (?,B.(B . "0x017D") ;; LATIN CAPITAL LETTER Z WITH CARON
       (?,B/(B . "0x017B") ;; LATIN CAPITAL LETTER Z WITH DOT ABOVE
       (?,B0(B . "0x00B0") ;; DEGREE SIGN
       (?,B1(B . "0x0105") ;; LATIN SMALL LETTER A WITH OGONEK
       (?,B2(B . "0x02DB") ;; OGONEK
       (?,B3(B . "0x0142") ;; LATIN SMALL LETTER L WITH STROKE
       (?,B4(B . "0x00B4") ;; ACUTE ACCENT
       (?,B5(B . "0x013E") ;; LATIN SMALL LETTER L WITH CARON
       (?,B6(B . "0x015B") ;; LATIN SMALL LETTER S WITH ACUTE
       (?,B7(B . "0x02C7") ;; CARON
       (?,B8(B . "0x00B8") ;; CEDILLA
       (?,B9(B . "0x0161") ;; LATIN SMALL LETTER S WITH CARON
       (?,B:(B . "0x015F") ;; LATIN SMALL LETTER S WITH CEDILLA
       (?,B;(B . "0x0165") ;; LATIN SMALL LETTER T WITH CARON
       (?,B<(B . "0x017A") ;; LATIN SMALL LETTER Z WITH ACUTE
       (?,B=(B . "0x02DD") ;; DOUBLE ACUTE ACCENT
       (?,B>(B . "0x017E") ;; LATIN SMALL LETTER Z WITH CARON
       (?,B?(B . "0x017C") ;; LATIN SMALL LETTER Z WITH DOT ABOVE
       (?,B@(B . "0x0154") ;; LATIN CAPITAL LETTER R WITH ACUTE
       (?,BA(B . "0x00C1") ;; LATIN CAPITAL LETTER A WITH ACUTE
       (?,BB(B . "0x00C2") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,BC(B . "0x0102") ;; LATIN CAPITAL LETTER A WITH BREVE
       (?,BD(B . "0x00C4") ;; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,BE(B . "0x0139") ;; LATIN CAPITAL LETTER L WITH ACUTE
       (?,BF(B . "0x0106") ;; LATIN CAPITAL LETTER C WITH ACUTE
       (?,BG(B . "0x00C7") ;; LATIN CAPITAL LETTER C WITH CEDILLA
       (?,BH(B . "0x010C") ;; LATIN CAPITAL LETTER C WITH CARON
       (?,BI(B . "0x00C9") ;; LATIN CAPITAL LETTER E WITH ACUTE
       (?,BJ(B . "0x0118") ;; LATIN CAPITAL LETTER E WITH OGONEK
       (?,BK(B . "0x00CB") ;; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,BL(B . "0x011A") ;; LATIN CAPITAL LETTER E WITH CARON
       (?,BM(B . "0x00CD") ;; LATIN CAPITAL LETTER I WITH ACUTE
       (?,BN(B . "0x00CE") ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,BO(B . "0x010E") ;; LATIN CAPITAL LETTER D WITH CARON
       (?,BP(B . "0x0110") ;; LATIN CAPITAL LETTER D WITH STROKE
       (?,BQ(B . "0x0143") ;; LATIN CAPITAL LETTER N WITH ACUTE
       (?,BR(B . "0x0147") ;; LATIN CAPITAL LETTER N WITH CARON
       (?,BS(B . "0x00D3") ;; LATIN CAPITAL LETTER O WITH ACUTE
       (?,BT(B . "0x00D4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,BU(B . "0x0150") ;; LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
       (?,BV(B . "0x00D6") ;; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,BW(B . "0x00D7") ;; MULTIPLICATION SIGN
       (?,BX(B . "0x0158") ;; LATIN CAPITAL LETTER R WITH CARON
       (?,BY(B . "0x016E") ;; LATIN CAPITAL LETTER U WITH RING ABOVE
       (?,BZ(B . "0x00DA") ;; LATIN CAPITAL LETTER U WITH ACUTE
       (?,B[(B . "0x0170") ;; LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
       (?,B\(B . "0x00DC") ;; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,B](B . "0x00DD") ;; LATIN CAPITAL LETTER Y WITH ACUTE
       (?,B^(B . "0x0162") ;; LATIN CAPITAL LETTER T WITH CEDILLA
       (?,B_(B . "0x00DF") ;; LATIN SMALL LETTER SHARP S
       (?,B`(B . "0x0155") ;; LATIN SMALL LETTER R WITH ACUTE
       (?,Ba(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?,Bb(B . "0x00E2") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,Bc(B . "0x0103") ;; LATIN SMALL LETTER A WITH BREVE
       (?,Bd(B . "0x00E4") ;; LATIN SMALL LETTER A WITH DIAERESIS
       (?,Be(B . "0x013A") ;; LATIN SMALL LETTER L WITH ACUTE
       (?,Bf(B . "0x0107") ;; LATIN SMALL LETTER C WITH ACUTE
       (?,Bg(B . "0x00E7") ;; LATIN SMALL LETTER C WITH CEDILLA
       (?,Bh(B . "0x010D") ;; LATIN SMALL LETTER C WITH CARON
       (?,Bi(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?,Bj(B . "0x0119") ;; LATIN SMALL LETTER E WITH OGONEK
       (?,Bk(B . "0x00EB") ;; LATIN SMALL LETTER E WITH DIAERESIS
       (?,Bl(B . "0x011B") ;; LATIN SMALL LETTER E WITH CARON
       (?,Bm(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?,Bn(B . "0x00EE") ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,Bo(B . "0x010F") ;; LATIN SMALL LETTER D WITH CARON
       (?,Bp(B . "0x0111") ;; LATIN SMALL LETTER D WITH STROKE
       (?,Bq(B . "0x0144") ;; LATIN SMALL LETTER N WITH ACUTE
       (?,Br(B . "0x0148") ;; LATIN SMALL LETTER N WITH CARON
       (?,Bs(B . "0x00F3") ;; LATIN SMALL LETTER O WITH ACUTE
       (?,Bt(B . "0x00F4") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,Bu(B . "0x0151") ;; LATIN SMALL LETTER O WITH DOUBLE ACUTE
       (?,Bv(B . "0x00F6") ;; LATIN SMALL LETTER O WITH DIAERESIS
       (?,Bw(B . "0x00F7") ;; DIVISION SIGN
       (?,Bx(B . "0x0159") ;; LATIN SMALL LETTER R WITH CARON
       (?,By(B . "0x016F") ;; LATIN SMALL LETTER U WITH RING ABOVE
       (?,Bz(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?,B{(B . "0x0171") ;; LATIN SMALL LETTER U WITH DOUBLE ACUTE
       (?,B|(B . "0x00FC") ;; LATIN SMALL LETTER U WITH DIAERESIS
       (?,B}(B . "0x00FD") ;; LATIN SMALL LETTER Y WITH ACUTE
       (?,B~(B . "0x0163") ;; LATIN SMALL LETTER T WITH CEDILLA
       (?,B(B . "0x02D9") ;; DOT ABOVE
       ))))

(provide 'uiso8859-2)

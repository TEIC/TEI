;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-4.el --- tables between UCS and ISO-8859-4

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

;;; This file is converted from 8859-4.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'latin-iso8859-4 'unicode-assoc
     'iso-8859-4-vs-unicode-assoc)

(defvar
  iso-8859-4-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,D (B . "0x00A0") ;; NO-BREAK SPACE
       (?,D!(B . "0x0104") ;; LATIN CAPITAL LETTER A WITH OGONEK
       (?,D"(B . "0x0138") ;; LATIN SMALL LETTER KRA
       (?,D#(B . "0x0156") ;; LATIN CAPITAL LETTER R WITH CEDILLA
       (?,D$(B . "0x00A4") ;; CURRENCY SIGN
       (?,D%(B . "0x0128") ;; LATIN CAPITAL LETTER I WITH TILDE
       (?,D&(B . "0x013B") ;; LATIN CAPITAL LETTER L WITH CEDILLA
       (?,D'(B . "0x00A7") ;; SECTION SIGN
       (?,D((B . "0x00A8") ;; DIAERESIS
       (?,D)(B . "0x0160") ;; LATIN CAPITAL LETTER S WITH CARON
       (?,D*(B . "0x0112") ;; LATIN CAPITAL LETTER E WITH MACRON
       (?,D+(B . "0x0122") ;; LATIN CAPITAL LETTER G WITH CEDILLA
       (?,D,(B . "0x0166") ;; LATIN CAPITAL LETTER T WITH STROKE
       (?,D-(B . "0x00AD") ;; SOFT HYPHEN
       (?,D.(B . "0x017D") ;; LATIN CAPITAL LETTER Z WITH CARON
       (?,D/(B . "0x00AF") ;; MACRON
       (?,D0(B . "0x00B0") ;; DEGREE SIGN
       (?,D1(B . "0x0105") ;; LATIN SMALL LETTER A WITH OGONEK
       (?,D2(B . "0x02DB") ;; OGONEK
       (?,D3(B . "0x0157") ;; LATIN SMALL LETTER R WITH CEDILLA
       (?,D4(B . "0x00B4") ;; ACUTE ACCENT
       (?,D5(B . "0x0129") ;; LATIN SMALL LETTER I WITH TILDE
       (?,D6(B . "0x013C") ;; LATIN SMALL LETTER L WITH CEDILLA
       (?,D7(B . "0x02C7") ;; CARON
       (?,D8(B . "0x00B8") ;; CEDILLA
       (?,D9(B . "0x0161") ;; LATIN SMALL LETTER S WITH CARON
       (?,D:(B . "0x0113") ;; LATIN SMALL LETTER E WITH MACRON
       (?,D;(B . "0x0123") ;; LATIN SMALL LETTER G WITH CEDILLA
       (?,D<(B . "0x0167") ;; LATIN SMALL LETTER T WITH STROKE
       (?,D=(B . "0x014A") ;; LATIN CAPITAL LETTER ENG
       (?,D>(B . "0x017E") ;; LATIN SMALL LETTER Z WITH CARON
       (?,D?(B . "0x014B") ;; LATIN SMALL LETTER ENG
       (?,D@(B . "0x0100") ;; LATIN CAPITAL LETTER A WITH MACRON
       (?,DA(B . "0x00C1") ;; LATIN CAPITAL LETTER A WITH ACUTE
       (?,DB(B . "0x00C2") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,DC(B . "0x00C3") ;; LATIN CAPITAL LETTER A WITH TILDE
       (?,DD(B . "0x00C4") ;; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,DE(B . "0x00C5") ;; LATIN CAPITAL LETTER A WITH RING ABOVE
       (?,DF(B . "0x00C6") ;; LATIN CAPITAL LETTER AE
       (?,DG(B . "0x012E") ;; LATIN CAPITAL LETTER I WITH OGONEK
       (?,DH(B . "0x010C") ;; LATIN CAPITAL LETTER C WITH CARON
       (?,DI(B . "0x00C9") ;; LATIN CAPITAL LETTER E WITH ACUTE
       (?,DJ(B . "0x0118") ;; LATIN CAPITAL LETTER E WITH OGONEK
       (?,DK(B . "0x00CB") ;; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,DL(B . "0x0116") ;; LATIN CAPITAL LETTER E WITH DOT ABOVE
       (?,DM(B . "0x00CD") ;; LATIN CAPITAL LETTER I WITH ACUTE
       (?,DN(B . "0x00CE") ;; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,DO(B . "0x012A") ;; LATIN CAPITAL LETTER I WITH MACRON
       (?,DP(B . "0x0110") ;; LATIN CAPITAL LETTER D WITH STROKE
       (?,DQ(B . "0x0145") ;; LATIN CAPITAL LETTER N WITH CEDILLA
       (?,DR(B . "0x014C") ;; LATIN CAPITAL LETTER O WITH MACRON
       (?,DS(B . "0x0136") ;; LATIN CAPITAL LETTER K WITH CEDILLA
       (?,DT(B . "0x00D4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,DU(B . "0x00D5") ;; LATIN CAPITAL LETTER O WITH TILDE
       (?,DV(B . "0x00D6") ;; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,DW(B . "0x00D7") ;; MULTIPLICATION SIGN
       (?,DX(B . "0x00D8") ;; LATIN CAPITAL LETTER O WITH STROKE
       (?,DY(B . "0x0172") ;; LATIN CAPITAL LETTER U WITH OGONEK
       (?,DZ(B . "0x00DA") ;; LATIN CAPITAL LETTER U WITH ACUTE
       (?,D[(B . "0x00DB") ;; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
       (?,D\(B . "0x00DC") ;; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,D](B . "0x0168") ;; LATIN CAPITAL LETTER U WITH TILDE
       (?,D^(B . "0x016A") ;; LATIN CAPITAL LETTER U WITH MACRON
       (?,D_(B . "0x00DF") ;; LATIN SMALL LETTER SHARP S
       (?,D`(B . "0x0101") ;; LATIN SMALL LETTER A WITH MACRON
       (?,Da(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?,Db(B . "0x00E2") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,Dc(B . "0x00E3") ;; LATIN SMALL LETTER A WITH TILDE
       (?,Dd(B . "0x00E4") ;; LATIN SMALL LETTER A WITH DIAERESIS
       (?,De(B . "0x00E5") ;; LATIN SMALL LETTER A WITH RING ABOVE
       (?,Df(B . "0x00E6") ;; LATIN SMALL LETTER AE
       (?,Dg(B . "0x012F") ;; LATIN SMALL LETTER I WITH OGONEK
       (?,Dh(B . "0x010D") ;; LATIN SMALL LETTER C WITH CARON
       (?,Di(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?,Dj(B . "0x0119") ;; LATIN SMALL LETTER E WITH OGONEK
       (?,Dk(B . "0x00EB") ;; LATIN SMALL LETTER E WITH DIAERESIS
       (?,Dl(B . "0x0117") ;; LATIN SMALL LETTER E WITH DOT ABOVE
       (?,Dm(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?,Dn(B . "0x00EE") ;; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,Do(B . "0x012B") ;; LATIN SMALL LETTER I WITH MACRON
       (?,Dp(B . "0x0111") ;; LATIN SMALL LETTER D WITH STROKE
       (?,Dq(B . "0x0146") ;; LATIN SMALL LETTER N WITH CEDILLA
       (?,Dr(B . "0x014D") ;; LATIN SMALL LETTER O WITH MACRON
       (?,Ds(B . "0x0137") ;; LATIN SMALL LETTER K WITH CEDILLA
       (?,Dt(B . "0x00F4") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,Du(B . "0x00F5") ;; LATIN SMALL LETTER O WITH TILDE
       (?,Dv(B . "0x00F6") ;; LATIN SMALL LETTER O WITH DIAERESIS
       (?,Dw(B . "0x00F7") ;; DIVISION SIGN
       (?,Dx(B . "0x00F8") ;; LATIN SMALL LETTER O WITH STROKE
       (?,Dy(B . "0x0173") ;; LATIN SMALL LETTER U WITH OGONEK
       (?,Dz(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?,D{(B . "0x00FB") ;; LATIN SMALL LETTER U WITH CIRCUMFLEX
       (?,D|(B . "0x00FC") ;; LATIN SMALL LETTER U WITH DIAERESIS
       (?,D}(B . "0x0169") ;; LATIN SMALL LETTER U WITH TILDE
       (?,D~(B . "0x016B") ;; LATIN SMALL LETTER U WITH MACRON
       (?,D(B . "0x02D9") ;; DOT ABOVE
       ))))

(provide 'uiso8859-4)
 

;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-5.el --- tables between UCS and ISO-8859-5

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

;;; This file is converted from 8859-5.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'cyrillic-iso8859-5 'unicode-assoc
     'iso-8859-5-vs-unicode-assoc)

(defvar
  iso-8859-5-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,L (B . "0x00A0") ;; NO-BREAK SPACE
       (?,L!(B . "0x0401") ;; CYRILLIC CAPITAL LETTER IO
       (?,L"(B . "0x0402") ;; CYRILLIC CAPITAL LETTER DJE
       (?,L#(B . "0x0403") ;; CYRILLIC CAPITAL LETTER GJE
       (?,L$(B . "0x0404") ;; CYRILLIC CAPITAL LETTER UKRAINIAN IE
       (?,L%(B . "0x0405") ;; CYRILLIC CAPITAL LETTER DZE
       (?,L&(B . "0x0406") ;; CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
       (?,L'(B . "0x0407") ;; CYRILLIC CAPITAL LETTER YI
       (?,L((B . "0x0408") ;; CYRILLIC CAPITAL LETTER JE
       (?,L)(B . "0x0409") ;; CYRILLIC CAPITAL LETTER LJE
       (?,L*(B . "0x040A") ;; CYRILLIC CAPITAL LETTER NJE
       (?,L+(B . "0x040B") ;; CYRILLIC CAPITAL LETTER TSHE
       (?,L,(B . "0x040C") ;; CYRILLIC CAPITAL LETTER KJE
       (?,L-(B . "0x00AD") ;; SOFT HYPHEN
       (?,L.(B . "0x040E") ;; CYRILLIC CAPITAL LETTER SHORT U
       (?,L/(B . "0x040F") ;; CYRILLIC CAPITAL LETTER DZHE
       (?,L0(B . "0x0410") ;; CYRILLIC CAPITAL LETTER A
       (?,L1(B . "0x0411") ;; CYRILLIC CAPITAL LETTER BE
       (?,L2(B . "0x0412") ;; CYRILLIC CAPITAL LETTER VE
       (?,L3(B . "0x0413") ;; CYRILLIC CAPITAL LETTER GHE
       (?,L4(B . "0x0414") ;; CYRILLIC CAPITAL LETTER DE
       (?,L5(B . "0x0415") ;; CYRILLIC CAPITAL LETTER IE
       (?,L6(B . "0x0416") ;; CYRILLIC CAPITAL LETTER ZHE
       (?,L7(B . "0x0417") ;; CYRILLIC CAPITAL LETTER ZE
       (?,L8(B . "0x0418") ;; CYRILLIC CAPITAL LETTER I
       (?,L9(B . "0x0419") ;; CYRILLIC CAPITAL LETTER SHORT I
       (?,L:(B . "0x041A") ;; CYRILLIC CAPITAL LETTER KA
       (?,L;(B . "0x041B") ;; CYRILLIC CAPITAL LETTER EL
       (?,L<(B . "0x041C") ;; CYRILLIC CAPITAL LETTER EM
       (?,L=(B . "0x041D") ;; CYRILLIC CAPITAL LETTER EN
       (?,L>(B . "0x041E") ;; CYRILLIC CAPITAL LETTER O
       (?,L?(B . "0x041F") ;; CYRILLIC CAPITAL LETTER PE
       (?,L@(B . "0x0420") ;; CYRILLIC CAPITAL LETTER ER
       (?,LA(B . "0x0421") ;; CYRILLIC CAPITAL LETTER ES
       (?,LB(B . "0x0422") ;; CYRILLIC CAPITAL LETTER TE
       (?,LC(B . "0x0423") ;; CYRILLIC CAPITAL LETTER U
       (?,LD(B . "0x0424") ;; CYRILLIC CAPITAL LETTER EF
       (?,LE(B . "0x0425") ;; CYRILLIC CAPITAL LETTER HA
       (?,LF(B . "0x0426") ;; CYRILLIC CAPITAL LETTER TSE
       (?,LG(B . "0x0427") ;; CYRILLIC CAPITAL LETTER CHE
       (?,LH(B . "0x0428") ;; CYRILLIC CAPITAL LETTER SHA
       (?,LI(B . "0x0429") ;; CYRILLIC CAPITAL LETTER SHCHA
       (?,LJ(B . "0x042A") ;; CYRILLIC CAPITAL LETTER HARD SIGN
       (?,LK(B . "0x042B") ;; CYRILLIC CAPITAL LETTER YERU
       (?,LL(B . "0x042C") ;; CYRILLIC CAPITAL LETTER SOFT SIGN
       (?,LM(B . "0x042D") ;; CYRILLIC CAPITAL LETTER E
       (?,LN(B . "0x042E") ;; CYRILLIC CAPITAL LETTER YU
       (?,LO(B . "0x042F") ;; CYRILLIC CAPITAL LETTER YA
       (?,LP(B . "0x0430") ;; CYRILLIC SMALL LETTER A
       (?,LQ(B . "0x0431") ;; CYRILLIC SMALL LETTER BE
       (?,LR(B . "0x0432") ;; CYRILLIC SMALL LETTER VE
       (?,LS(B . "0x0433") ;; CYRILLIC SMALL LETTER GHE
       (?,LT(B . "0x0434") ;; CYRILLIC SMALL LETTER DE
       (?,LU(B . "0x0435") ;; CYRILLIC SMALL LETTER IE
       (?,LV(B . "0x0436") ;; CYRILLIC SMALL LETTER ZHE
       (?,LW(B . "0x0437") ;; CYRILLIC SMALL LETTER ZE
       (?,LX(B . "0x0438") ;; CYRILLIC SMALL LETTER I
       (?,LY(B . "0x0439") ;; CYRILLIC SMALL LETTER SHORT I
       (?,LZ(B . "0x043A") ;; CYRILLIC SMALL LETTER KA
       (?,L[(B . "0x043B") ;; CYRILLIC SMALL LETTER EL
       (?,L\(B . "0x043C") ;; CYRILLIC SMALL LETTER EM
       (?,L](B . "0x043D") ;; CYRILLIC SMALL LETTER EN
       (?,L^(B . "0x043E") ;; CYRILLIC SMALL LETTER O
       (?,L_(B . "0x043F") ;; CYRILLIC SMALL LETTER PE
       (?,L`(B . "0x0440") ;; CYRILLIC SMALL LETTER ER
       (?,La(B . "0x0441") ;; CYRILLIC SMALL LETTER ES
       (?,Lb(B . "0x0442") ;; CYRILLIC SMALL LETTER TE
       (?,Lc(B . "0x0443") ;; CYRILLIC SMALL LETTER U
       (?,Ld(B . "0x0444") ;; CYRILLIC SMALL LETTER EF
       (?,Le(B . "0x0445") ;; CYRILLIC SMALL LETTER HA
       (?,Lf(B . "0x0446") ;; CYRILLIC SMALL LETTER TSE
       (?,Lg(B . "0x0447") ;; CYRILLIC SMALL LETTER CHE
       (?,Lh(B . "0x0448") ;; CYRILLIC SMALL LETTER SHA
       (?,Li(B . "0x0449") ;; CYRILLIC SMALL LETTER SHCHA
       (?,Lj(B . "0x044A") ;; CYRILLIC SMALL LETTER HARD SIGN
       (?,Lk(B . "0x044B") ;; CYRILLIC SMALL LETTER YERU
       (?,Ll(B . "0x044C") ;; CYRILLIC SMALL LETTER SOFT SIGN
       (?,Lm(B . "0x044D") ;; CYRILLIC SMALL LETTER E
       (?,Ln(B . "0x044E") ;; CYRILLIC SMALL LETTER YU
       (?,Lo(B . "0x044F") ;; CYRILLIC SMALL LETTER YA
       (?,Lp(B . "0x2116") ;; NUMERO SIGN
       (?,Lq(B . "0x0451") ;; CYRILLIC SMALL LETTER IO
       (?,Lr(B . "0x0452") ;; CYRILLIC SMALL LETTER DJE
       (?,Ls(B . "0x0453") ;; CYRILLIC SMALL LETTER GJE
       (?,Lt(B . "0x0454") ;; CYRILLIC SMALL LETTER UKRAINIAN IE
       (?,Lu(B . "0x0455") ;; CYRILLIC SMALL LETTER DZE
       (?,Lv(B . "0x0456") ;; CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
       (?,Lw(B . "0x0457") ;; CYRILLIC SMALL LETTER YI
       (?,Lx(B . "0x0458") ;; CYRILLIC SMALL LETTER JE
       (?,Ly(B . "0x0459") ;; CYRILLIC SMALL LETTER LJE
       (?,Lz(B . "0x045A") ;; CYRILLIC SMALL LETTER NJE
       (?,L{(B . "0x045B") ;; CYRILLIC SMALL LETTER TSHE
       (?,L|(B . "0x045C") ;; CYRILLIC SMALL LETTER KJE
       (?,L}(B . "0x00A7") ;; SECTION SIGN
       (?,L~(B . "0x045E") ;; CYRILLIC SMALL LETTER SHORT U
       (?,L(B . "0x045F") ;; CYRILLIC SMALL LETTER DZHE
       ))))

(provide 'uiso8859-5)

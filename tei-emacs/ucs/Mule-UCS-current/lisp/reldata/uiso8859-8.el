;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-8.el --- tables between UCS and ISO-8859-8

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

;;; This file is converted from 8859-8.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'hebrew-iso8859-8 'unicode-assoc
     'iso-8859-8-vs-unicode-assoc)

(defvar
  iso-8859-8-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?[2],H [0](B . "0x00A0") ;; NO-BREAK SPACE
       (?[2],H"[0](B . "0x00A2") ;; CENT SIGN
       (?[2],H#[0](B . "0x00A3") ;; POUND SIGN
       (?[2],H$[0](B . "0x00A4") ;; CURRENCY SIGN
       (?[2],H%[0](B . "0x00A5") ;; YEN SIGN
       (?[2],H&[0](B . "0x00A6") ;; BROKEN BAR
       (?[2],H'[0](B . "0x00A7") ;; SECTION SIGN
       (?[2],H([0](B . "0x00A8") ;; DIAERESIS
       (?[2],H)[0](B . "0x00A9") ;; COPYRIGHT SIGN
       (?[2],H*[0](B . "0x00D7") ;; MULTIPLICATION SIGN
       (?[2],H+[0](B . "0x00AB") ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?[2],H,[0](B . "0x00AC") ;; NOT SIGN
       (?[2],H-[0](B . "0x00AD") ;; SOFT HYPHEN
       (?[2],H.[0](B . "0x00AE") ;; REGISTERED SIGN
       (?[2],H/[0](B . "0x203E") ;; OVERLINE
       (?[2],H0[0](B . "0x00B0") ;; DEGREE SIGN
       (?[2],H1[0](B . "0x00B1") ;; PLUS-MINUS SIGN
       (?[2],H2[0](B . "0x00B2") ;; SUPERSCRIPT TWO
       (?[2],H3[0](B . "0x00B3") ;; SUPERSCRIPT THREE
       (?[2],H4[0](B . "0x00B4") ;; ACUTE ACCENT
       (?[2],H5[0](B . "0x00B5") ;; MICRO SIGN
       (?[2],H6[0](B . "0x00B6") ;; PILCROW SIGN
       (?[2],H7[0](B . "0x00B7") ;; MIDDLE DOT
       (?[2],H8[0](B . "0x00B8") ;; CEDILLA
       (?[2],H9[0](B . "0x00B9") ;; SUPERSCRIPT ONE
       (?[2],H:[0](B . "0x00F7") ;; DIVISION SIGN
       (?[2],H;[0](B . "0x00BB") ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?[2],H<[0](B . "0x00BC") ;; VULGAR FRACTION ONE QUARTER
       (?[2],H=[0](B . "0x00BD") ;; VULGAR FRACTION ONE HALF
       (?[2],H>[0](B . "0x00BE") ;; VULGAR FRACTION THREE QUARTERS
       (?[2],H_[0](B . "0x2017") ;; DOUBLE LOW LINE
       (?[2],H`[0](B . "0x05D0") ;; HEBREW LETTER ALEF
       (?[2],Ha[0](B . "0x05D1") ;; HEBREW LETTER BET
       (?[2],Hb[0](B . "0x05D2") ;; HEBREW LETTER GIMEL
       (?[2],Hc[0](B . "0x05D3") ;; HEBREW LETTER DALET
       (?[2],Hd[0](B . "0x05D4") ;; HEBREW LETTER HE
       (?[2],He[0](B . "0x05D5") ;; HEBREW LETTER VAV
       (?[2],Hf[0](B . "0x05D6") ;; HEBREW LETTER ZAYIN
       (?[2],Hg[0](B . "0x05D7") ;; HEBREW LETTER HET
       (?[2],Hh[0](B . "0x05D8") ;; HEBREW LETTER TET
       (?[2],Hi[0](B . "0x05D9") ;; HEBREW LETTER YOD
       (?[2],Hj[0](B . "0x05DA") ;; HEBREW LETTER FINAL KAF
       (?[2],Hk[0](B . "0x05DB") ;; HEBREW LETTER KAF
       (?[2],Hl[0](B . "0x05DC") ;; HEBREW LETTER LAMED
       (?[2],Hm[0](B . "0x05DD") ;; HEBREW LETTER FINAL MEM
       (?[2],Hn[0](B . "0x05DE") ;; HEBREW LETTER MEM
       (?[2],Ho[0](B . "0x05DF") ;; HEBREW LETTER FINAL NUN
       (?[2],Hp[0](B . "0x05E0") ;; HEBREW LETTER NUN
       (?[2],Hq[0](B . "0x05E1") ;; HEBREW LETTER SAMEKH
       (?[2],Hr[0](B . "0x05E2") ;; HEBREW LETTER AYIN
       (?[2],Hs[0](B . "0x05E3") ;; HEBREW LETTER FINAL PE
       (?[2],Ht[0](B . "0x05E4") ;; HEBREW LETTER PE
       (?[2],Hu[0](B . "0x05E5") ;; HEBREW LETTER FINAL TSADI
       (?[2],Hv[0](B . "0x05E6") ;; HEBREW LETTER TSADI
       (?[2],Hw[0](B . "0x05E7") ;; HEBREW LETTER QOF
       (?[2],Hx[0](B . "0x05E8") ;; HEBREW LETTER RESH
       (?[2],Hy[0](B . "0x05E9") ;; HEBREW LETTER SHIN
       (?[2],Hz[0](B . "0x05EA") ;; HEBREW LETTER TAV
       ))))

(provide 'uiso8859-8)
 

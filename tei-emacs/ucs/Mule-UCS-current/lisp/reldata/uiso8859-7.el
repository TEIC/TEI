;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-7.el --- tables between UCS and ISO-8859-7

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

;;; This file is converted from 8859-7.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

;;; 99/10/22 reviced this mapping.  This corresponds to update of ISO 8859:1999.
;;; c.f. FCD 8859-7 and http://www.unicode.org/unicode/reports/tr8.html

(put 'greek-iso8859-7 'unicode-assoc
     'iso-8859-7-vs-unicode-assoc)

(defvar
  iso-8859-7-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,F (B . "0x00A0") ;; NO-BREAK SPACE
       (?,F!(B . "0x2018") ;; LEFT SINGLE QUOTATION MARK
       (?,F"(B . "0x2019") ;; RIGHT SINGLE QUOTATION MARK
       (?,F#(B . "0x00A3") ;; POUND SIGN
       (?,F&(B . "0x00A6") ;; BROKEN BAR
       (?,F'(B . "0x00A7") ;; SECTION SIGN
       (?,F((B . "0x00A8") ;; DIAERESIS
       (?,F)(B . "0x00A9") ;; COPYRIGHT SIGN
       (?,F+(B . "0x00AB") ;; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,F,(B . "0x00AC") ;; NOT SIGN
       (?,F-(B . "0x00AD") ;; SOFT HYPHEN
       (?,F/(B . "0x2015") ;; HORIZONTAL BAR
       (?,F0(B . "0x00B0") ;; DEGREE SIGN
       (?,F1(B . "0x00B1") ;; PLUS-MINUS SIGN
       (?,F2(B . "0x00B2") ;; SUPERSCRIPT TWO
       (?,F3(B . "0x00B3") ;; SUPERSCRIPT THREE
       (?,F4(B . "0x0384") ;; GREEK TONOS
       (?,F5(B . "0x0385") ;; GREEK DIALYTIKA TONOS
       (?,F6(B . "0x0386") ;; GREEK CAPITAL LETTER ALPHA WITH TONOS
       (?,F7(B . "0x00B7") ;; MIDDLE DOT
       (?,F8(B . "0x0388") ;; GREEK CAPITAL LETTER EPSILON WITH TONOS
       (?,F9(B . "0x0389") ;; GREEK CAPITAL LETTER ETA WITH TONOS
       (?,F:(B . "0x038A") ;; GREEK CAPITAL LETTER IOTA WITH TONOS
       (?,F;(B . "0x00BB") ;; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       (?,F<(B . "0x038C") ;; GREEK CAPITAL LETTER OMICRON WITH TONOS
       (?,F=(B . "0x00BD") ;; VULGAR FRACTION ONE HALF
       (?,F>(B . "0x038E") ;; GREEK CAPITAL LETTER UPSILON WITH TONOS
       (?,F?(B . "0x038F") ;; GREEK CAPITAL LETTER OMEGA WITH TONOS
       (?,F@(B . "0x0390") ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
       (?,FA(B . "0x0391") ;; GREEK CAPITAL LETTER ALPHA
       (?,FB(B . "0x0392") ;; GREEK CAPITAL LETTER BETA
       (?,FC(B . "0x0393") ;; GREEK CAPITAL LETTER GAMMA
       (?,FD(B . "0x0394") ;; GREEK CAPITAL LETTER DELTA
       (?,FE(B . "0x0395") ;; GREEK CAPITAL LETTER EPSILON
       (?,FF(B . "0x0396") ;; GREEK CAPITAL LETTER ZETA
       (?,FG(B . "0x0397") ;; GREEK CAPITAL LETTER ETA
       (?,FH(B . "0x0398") ;; GREEK CAPITAL LETTER THETA
       (?,FI(B . "0x0399") ;; GREEK CAPITAL LETTER IOTA
       (?,FJ(B . "0x039A") ;; GREEK CAPITAL LETTER KAPPA
       (?,FK(B . "0x039B") ;; GREEK CAPITAL LETTER LAMDA
       (?,FL(B . "0x039C") ;; GREEK CAPITAL LETTER MU
       (?,FM(B . "0x039D") ;; GREEK CAPITAL LETTER NU
       (?,FN(B . "0x039E") ;; GREEK CAPITAL LETTER XI
       (?,FO(B . "0x039F") ;; GREEK CAPITAL LETTER OMICRON
       (?,FP(B . "0x03A0") ;; GREEK CAPITAL LETTER PI
       (?,FQ(B . "0x03A1") ;; GREEK CAPITAL LETTER RHO
       (?,FS(B . "0x03A3") ;; GREEK CAPITAL LETTER SIGMA
       (?,FT(B . "0x03A4") ;; GREEK CAPITAL LETTER TAU
       (?,FU(B . "0x03A5") ;; GREEK CAPITAL LETTER UPSILON
       (?,FV(B . "0x03A6") ;; GREEK CAPITAL LETTER PHI
       (?,FW(B . "0x03A7") ;; GREEK CAPITAL LETTER CHI
       (?,FX(B . "0x03A8") ;; GREEK CAPITAL LETTER PSI
       (?,FY(B . "0x03A9") ;; GREEK CAPITAL LETTER OMEGA
       (?,FZ(B . "0x03AA") ;; GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
       (?,F[(B . "0x03AB") ;; GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
       (?,F\(B . "0x03AC") ;; GREEK SMALL LETTER ALPHA WITH TONOS
       (?,F](B . "0x03AD") ;; GREEK SMALL LETTER EPSILON WITH TONOS
       (?,F^(B . "0x03AE") ;; GREEK SMALL LETTER ETA WITH TONOS
       (?,F_(B . "0x03AF") ;; GREEK SMALL LETTER IOTA WITH TONOS
       (?,F`(B . "0x03B0") ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
       (?,Fa(B . "0x03B1") ;; GREEK SMALL LETTER ALPHA
       (?,Fb(B . "0x03B2") ;; GREEK SMALL LETTER BETA
       (?,Fc(B . "0x03B3") ;; GREEK SMALL LETTER GAMMA
       (?,Fd(B . "0x03B4") ;; GREEK SMALL LETTER DELTA
       (?,Fe(B . "0x03B5") ;; GREEK SMALL LETTER EPSILON
       (?,Ff(B . "0x03B6") ;; GREEK SMALL LETTER ZETA
       (?,Fg(B . "0x03B7") ;; GREEK SMALL LETTER ETA
       (?,Fh(B . "0x03B8") ;; GREEK SMALL LETTER THETA
       (?,Fi(B . "0x03B9") ;; GREEK SMALL LETTER IOTA
       (?,Fj(B . "0x03BA") ;; GREEK SMALL LETTER KAPPA
       (?,Fk(B . "0x03BB") ;; GREEK SMALL LETTER LAMDA
       (?,Fl(B . "0x03BC") ;; GREEK SMALL LETTER MU
       (?,Fm(B . "0x03BD") ;; GREEK SMALL LETTER NU
       (?,Fn(B . "0x03BE") ;; GREEK SMALL LETTER XI
       (?,Fo(B . "0x03BF") ;; GREEK SMALL LETTER OMICRON
       (?,Fp(B . "0x03C0") ;; GREEK SMALL LETTER PI
       (?,Fq(B . "0x03C1") ;; GREEK SMALL LETTER RHO
       (?,Fr(B . "0x03C2") ;; GREEK SMALL LETTER FINAL SIGMA
       (?,Fs(B . "0x03C3") ;; GREEK SMALL LETTER SIGMA
       (?,Ft(B . "0x03C4") ;; GREEK SMALL LETTER TAU
       (?,Fu(B . "0x03C5") ;; GREEK SMALL LETTER UPSILON
       (?,Fv(B . "0x03C6") ;; GREEK SMALL LETTER PHI
       (?,Fw(B . "0x03C7") ;; GREEK SMALL LETTER CHI
       (?,Fx(B . "0x03C8") ;; GREEK SMALL LETTER PSI
       (?,Fy(B . "0x03C9") ;; GREEK SMALL LETTER OMEGA
       (?,Fz(B . "0x03CA") ;; GREEK SMALL LETTER IOTA WITH DIALYTIKA
       (?,F{(B . "0x03CB") ;; GREEK SMALL LETTER UPSILON WITH DIALYTIKA
       (?,F|(B . "0x03CC") ;; GREEK SMALL LETTER OMICRON WITH TONOS
       (?,F}(B . "0x03CD") ;; GREEK SMALL LETTER UPSILON WITH TONOS
       (?,F~(B . "0x03CE") ;; GREEK SMALL LETTER OMEGA WITH TONOS
       ))))

(provide 'uiso8859-7)
 

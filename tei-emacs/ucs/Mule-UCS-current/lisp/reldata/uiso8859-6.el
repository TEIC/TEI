;;; -*- coding: iso-2022-7bit  -*-
;;; uiso8859-6.el --- tables between UCS and ISO-8859-6

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

;;; This file is converted from 8859-6.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'arabic-iso8859-6 'unicode-assoc
     'iso-8859-6-vs-unicode-assoc)

(defvar
  iso-8859-6-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,G (B . "0x00A0") ;; NO-BREAK SPACE
       (?,G$(B . "0x00A4") ;; CURRENCY SIGN
       (?,G,(B . "0x060C") ;; ARABIC COMMA
       (?,G-(B . "0x00AD") ;; SOFT HYPHEN
       (?,G;(B . "0x061B") ;; ARABIC SEMICOLON
       (?,G?(B . "0x061F") ;; ARABIC QUESTION MARK
       (?,GA(B . "0x0621") ;; ARABIC LETTER HAMZA
       (?,GB(B . "0x0622") ;; ARABIC LETTER ALEF WITH MADDA ABOVE
       (?,GC(B . "0x0623") ;; ARABIC LETTER ALEF WITH HAMZA ABOVE
       (?,GD(B . "0x0624") ;; ARABIC LETTER WAW WITH HAMZA ABOVE
       (?,GE(B . "0x0625") ;; ARABIC LETTER ALEF WITH HAMZA BELOW
       (?,GF(B . "0x0626") ;; ARABIC LETTER YEH WITH HAMZA ABOVE
       (?,GG(B . "0x0627") ;; ARABIC LETTER ALEF
       (?,GH(B . "0x0628") ;; ARABIC LETTER BEH
       (?,GI(B . "0x0629") ;; ARABIC LETTER TEH MARBUTA
       (?,GJ(B . "0x062A") ;; ARABIC LETTER TEH
       (?,GK(B . "0x062B") ;; ARABIC LETTER THEH
       (?,GL(B . "0x062C") ;; ARABIC LETTER JEEM
       (?,GM(B . "0x062D") ;; ARABIC LETTER HAH
       (?,GN(B . "0x062E") ;; ARABIC LETTER KHAH
       (?,GO(B . "0x062F") ;; ARABIC LETTER DAL
       (?,GP(B . "0x0630") ;; ARABIC LETTER THAL
       (?,GQ(B . "0x0631") ;; ARABIC LETTER REH
       (?,GR(B . "0x0632") ;; ARABIC LETTER ZAIN
       (?,GS(B . "0x0633") ;; ARABIC LETTER SEEN
       (?,GT(B . "0x0634") ;; ARABIC LETTER SHEEN
       (?,GU(B . "0x0635") ;; ARABIC LETTER SAD
       (?,GV(B . "0x0636") ;; ARABIC LETTER DAD
       (?,GW(B . "0x0637") ;; ARABIC LETTER TAH
       (?,GX(B . "0x0638") ;; ARABIC LETTER ZAH
       (?,GY(B . "0x0639") ;; ARABIC LETTER AIN
       (?,GZ(B . "0x063A") ;; ARABIC LETTER GHAIN
       (?,G`(B . "0x0640") ;; ARABIC TATWEEL
       (?,Ga(B . "0x0641") ;; ARABIC LETTER FEH
       (?,Gb(B . "0x0642") ;; ARABIC LETTER QAF
       (?,Gc(B . "0x0643") ;; ARABIC LETTER KAF
       (?,Gd(B . "0x0644") ;; ARABIC LETTER LAM
       (?,Ge(B . "0x0645") ;; ARABIC LETTER MEEM
       (?,Gf(B . "0x0646") ;; ARABIC LETTER NOON
       (?,Gg(B . "0x0647") ;; ARABIC LETTER HEH
       (?,Gh(B . "0x0648") ;; ARABIC LETTER WAW
       (?,Gi(B . "0x0649") ;; ARABIC LETTER ALEF MAKSURA
       (?,Gj(B . "0x064A") ;; ARABIC LETTER YEH
       (?,Gk(B . "0x064B") ;; ARABIC FATHATAN
       (?,Gl(B . "0x064C") ;; ARABIC DAMMATAN
       (?,Gm(B . "0x064D") ;; ARABIC KASRATAN
       (?,Gn(B . "0x064E") ;; ARABIC FATHA
       (?,Go(B . "0x064F") ;; ARABIC DAMMA
       (?,Gp(B . "0x0650") ;; ARABIC KASRA
       (?,Gq(B . "0x0651") ;; ARABIC SHADDA
       (?,Gr(B . "0x0652") ;; ARABIC SUKUN
       ))))

(provide 'uiso8859-6)

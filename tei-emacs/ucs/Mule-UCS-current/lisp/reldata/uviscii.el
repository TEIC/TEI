;;; -*- coding: iso-2022-7bit  -*-
;;; uviscii.el --- tables between Unicode and VISCII 1.1

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, VISCII

;; Miyashita Hisashi(himi@bird.scphys.kyoto-u.ac.jp)
;; converted VISCII.TXT created by Mark Leisher <mleisher@crl.nmsu.edu>

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

(put 'vietnamese-viscii-lower 'unicode-assoc
     'viscii-lower-vs-unicode-assoc)
(put 'vietnamese-viscii-upper 'unicode-assoc
     'viscii-upper-vs-unicode-assoc)

(defvar viscii-lower-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,1!(B . "0x1EAF") ;; LATIN SMALL LETTER A WITH BREVE AND ACUTE
       (?,1"(B . "0x1EB1") ;; LATIN SMALL LETTER A WITH BREVE AND GRAVE
       (?,1#(B . "0x1EB7") ;; LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
       (?,1$(B . "0x1EA5") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
       (?,1%(B . "0x1EA7") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
       (?,1&(B . "0x1EA9") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
       (?,1'(B . "0x1EAD") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
       (?,1((B . "0x1EBD") ;; LATIN SMALL LETTER E WITH TILDE
       (?,1)(B . "0x1EB9") ;; LATIN SMALL LETTER E WITH DOT BELOW
       (?,1*(B . "0x1EBF") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
       (?,1+(B . "0x1EC1") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
       (?,1,(B . "0x1EC3") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
       (?,1-(B . "0x1EC5") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
       (?,1.(B . "0x1EC7") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
       (?,1/(B . "0x1ED1") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
       (?,10(B . "0x1ED3") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
       (?,11(B . "0x1ED5") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
       (?,12(B . "0x1ED7") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
       (?,15(B . "0x1ED9") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
       (?,16(B . "0x1EDD") ;; LATIN SMALL LETTER O WITH HORN AND GRAVE
       (?,17(B . "0x1EDF") ;; LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
       (?,18(B . "0x1ECB") ;; LATIN SMALL LETTER I WITH DOT BELOW
       (?,1=(B . "0x01A1") ;; LATIN SMALL LETTER O WITH HORN
       (?,1>(B . "0x1EDB") ;; LATIN SMALL LETTER O WITH HORN AND ACUTE
       (?,1F(B . "0x1EB3") ;; LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
       (?,1G(B . "0x1EB5") ;; LATIN SMALL LETTER A WITH BREVE AND TILDE
       (?,1O(B . "0x1EF3") ;; LATIN SMALL LETTER Y WITH GRAVE
       (?,1Q(B . "0x1EE9") ;; LATIN SMALL LETTER U WITH HORN AND ACUTE
       (?,1U(B . "0x1EA1") ;; LATIN SMALL LETTER A WITH DOT BELOW
       (?,1V(B . "0x1EF7") ;; LATIN SMALL LETTER Y WITH HOOK ABOVE
       (?,1W(B . "0x1EEB") ;; LATIN SMALL LETTER U WITH HORN AND GRAVE
       (?,1X(B . "0x1EED") ;; LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
       (?,1[(B . "0x1EF9") ;; LATIN SMALL LETTER Y WITH TILDE
       (?,1\(B . "0x1EF5") ;; LATIN SMALL LETTER Y WITH DOT BELOW
       (?,1^(B . "0x1EE1") ;; LATIN SMALL LETTER O WITH HORN AND TILDE
       (?,1_(B . "0x01B0") ;; LATIN SMALL LETTER U WITH HORN
       (?,1`(B . "0x00E0") ;; LATIN SMALL LETTER A WITH GRAVE
       (?,1a(B . "0x00E1") ;; LATIN SMALL LETTER A WITH ACUTE
       (?,1b(B . "0x00E2") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,1c(B . "0x00E3") ;; LATIN SMALL LETTER A WITH TILDE
       (?,1d(B . "0x1EA3") ;; LATIN SMALL LETTER A WITH HOOK ABOVE
       (?,1e(B . "0x0103") ;; LATIN SMALL LETTER A WITH BREVE
       (?,1f(B . "0x1EEF") ;; LATIN SMALL LETTER U WITH HORN AND TILDE
       (?,1g(B . "0x1EAB") ;; LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
       (?,1h(B . "0x00E8") ;; LATIN SMALL LETTER E WITH GRAVE
       (?,1i(B . "0x00E9") ;; LATIN SMALL LETTER E WITH ACUTE
       (?,1j(B . "0x00EA") ;; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?,1k(B . "0x1EBB") ;; LATIN SMALL LETTER E WITH HOOK ABOVE
       (?,1l(B . "0x00EC") ;; LATIN SMALL LETTER I WITH GRAVE
       (?,1m(B . "0x00ED") ;; LATIN SMALL LETTER I WITH ACUTE
       (?,1n(B . "0x0129") ;; LATIN SMALL LETTER I WITH TILDE
       (?,1o(B . "0x1EC9") ;; LATIN SMALL LETTER I WITH HOOK ABOVE
       (?,1p(B . "0x0111") ;; LATIN SMALL LETTER D WITH STROKE
       (?,1q(B . "0x1EF1") ;; LATIN SMALL LETTER U WITH HORN AND DOT BELOW
       (?,1r(B . "0x00F2") ;; LATIN SMALL LETTER O WITH GRAVE
       (?,1s(B . "0x00F3") ;; LATIN SMALL LETTER O WITH ACUTE
       (?,1t(B . "0x00F4") ;; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,1u(B . "0x00F5") ;; LATIN SMALL LETTER O WITH TILDE
       (?,1v(B . "0x1ECF") ;; LATIN SMALL LETTER O WITH HOOK ABOVE
       (?,1w(B . "0x1ECD") ;; LATIN SMALL LETTER O WITH DOT BELOW
       (?,1x(B . "0x1EE5") ;; LATIN SMALL LETTER U WITH DOT BELOW
       (?,1y(B . "0x00F9") ;; LATIN SMALL LETTER U WITH GRAVE
       (?,1z(B . "0x00FA") ;; LATIN SMALL LETTER U WITH ACUTE
       (?,1{(B . "0x0169") ;; LATIN SMALL LETTER U WITH TILDE
       (?,1|(B . "0x1EE7") ;; LATIN SMALL LETTER U WITH HOOK ABOVE
       (?,1}(B . "0x00FD") ;; LATIN SMALL LETTER Y WITH ACUTE
       (?,1~(B . "0x1EE3") ;; LATIN SMALL LETTER O WITH HORN AND DOT BELOW
       ))))

(defvar viscii-upper-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,2!(B . "0x1EAE") ;; LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
       (?,2"(B . "0x1EB0") ;; LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
       (?,2#(B . "0x1EB6") ;; LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
       (?,2$(B . "0x1EA4") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
       (?,2%(B . "0x1EA6") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
       (?,2&(B . "0x1EA8") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
       (?,2'(B . "0x1EAC") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
       (?,2((B . "0x1EBC") ;; LATIN CAPITAL LETTER E WITH TILDE
       (?,2)(B . "0x1EB8") ;; LATIN CAPITAL LETTER E WITH DOT BELOW
       (?,2*(B . "0x1EBE") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
       (?,2+(B . "0x1EC0") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
       (?,2,(B . "0x1EC2") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
       (?,2-(B . "0x1EC4") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
       (?,2.(B . "0x1EC6") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
       (?,2/(B . "0x1ED0") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
       (?,20(B . "0x1ED2") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
       (?,21(B . "0x1ED4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
       (?,22(B . "0x1ED6") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
       (?,25(B . "0x1ED8") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
       (?,26(B . "0x1EDC") ;; LATIN CAPITAL LETTER O WITH HORN AND GRAVE
       (?,27(B . "0x1EDE") ;; LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
       (?,28(B . "0x1ECA") ;; LATIN CAPITAL LETTER I WITH DOT BELOW
       (?,2=(B . "0x01A0") ;; LATIN CAPITAL LETTER O WITH HORN
       (?,2>(B . "0x1EDA") ;; LATIN CAPITAL LETTER O WITH HORN AND ACUTE
       (?,2F(B . "0x1EB2") ;; LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
       (?,2G(B . "0x1EB4") ;; LATIN CAPITAL LETTER A WITH BREVE AND TILDE
       (?,2O(B . "0x1EF2") ;; LATIN CAPITAL LETTER Y WITH GRAVE
       (?,2Q(B . "0x1EE8") ;; LATIN CAPITAL LETTER U WITH HORN AND ACUTE
       (?,2U(B . "0x1EA0") ;; LATIN CAPITAL LETTER A WITH DOT BELOW
       (?,2V(B . "0x1EF6") ;; LATIN CAPITAL LETTER Y WITH HOOK ABOVE
       (?,2W(B . "0x1EEA") ;; LATIN CAPITAL LETTER U WITH HORN AND GRAVE
       (?,2X(B . "0x1EEC") ;; LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
       (?,2[(B . "0x1EF8") ;; LATIN CAPITAL LETTER Y WITH TILDE
       (?,2\(B . "0x1EF4") ;; LATIN CAPITAL LETTER Y WITH DOT BELOW
       (?,2^(B . "0x1EE0") ;; LATIN CAPITAL LETTER O WITH HORN AND TILDE
       (?,2_(B . "0x01AF") ;; LATIN CAPITAL LETTER U WITH HORN
       (?,2`(B . "0x00C0") ;; LATIN CAPITAL LETTER A WITH GRAVE
       (?,2a(B . "0x00C1") ;; LATIN CAPITAL LETTER A WITH ACUTE
       (?,2b(B . "0x00C2") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,2c(B . "0x00C3") ;; LATIN CAPITAL LETTER A WITH TILDE
       (?,2d(B . "0x1EA2") ;; LATIN CAPITAL LETTER A WITH HOOK ABOVE
       (?,2e(B . "0x0102") ;; LATIN CAPITAL LETTER A WITH BREVE
       (?,2f(B . "0x1EEE") ;; LATIN CAPITAL LETTER U WITH HORN AND TILDE
       (?,2g(B . "0x1EAA") ;; LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
       (?,2h(B . "0x00C8") ;; LATIN CAPITAL LETTER E WITH GRAVE
       (?,2i(B . "0x00C9") ;; LATIN CAPITAL LETTER E WITH ACUTE
       (?,2j(B . "0x00CA") ;; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
       (?,2k(B . "0x1EBA") ;; LATIN CAPITAL LETTER E WITH HOOK ABOVE
       (?,2l(B . "0x00CC") ;; LATIN CAPITAL LETTER I WITH GRAVE
       (?,2m(B . "0x00CD") ;; LATIN CAPITAL LETTER I WITH ACUTE
       (?,2n(B . "0x0128") ;; LATIN CAPITAL LETTER I WITH TILDE
       (?,2o(B . "0x1EC8") ;; LATIN CAPITAL LETTER I WITH HOOK ABOVE
       (?,2p(B . "0x0110") ;; LATIN CAPITAL LETTER D WITH STROKE
       (?,2q(B . "0x1EF0") ;; LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
       (?,2r(B . "0x00D2") ;; LATIN CAPITAL LETTER O WITH GRAVE
       (?,2s(B . "0x00D3") ;; LATIN CAPITAL LETTER O WITH ACUTE
       (?,2t(B . "0x00D4") ;; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,2u(B . "0x00D5") ;; LATIN CAPITAL LETTER O WITH TILDE
       (?,2v(B . "0x1ECE") ;; LATIN CAPITAL LETTER O WITH HOOK ABOVE
       (?,2w(B . "0x1ECC") ;; LATIN CAPITAL LETTER O WITH DOT BELOW
       (?,2x(B . "0x1EE4") ;; LATIN CAPITAL LETTER U WITH DOT BELOW
       (?,2y(B . "0x00D9") ;; LATIN CAPITAL LETTER U WITH GRAVE
       (?,2z(B . "0x00DA") ;; LATIN CAPITAL LETTER U WITH ACUTE
       (?,2{(B . "0x0168") ;; LATIN CAPITAL LETTER U WITH TILDE
       (?,2|(B . "0x1EE6") ;; LATIN CAPITAL LETTER U WITH HOOK ABOVE
       (?,2}(B . "0x00DD") ;; LATIN CAPITAL LETTER Y WITH ACUTE
       (?,2~(B . "0x1EE2") ;; LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
       ))))

(provide 'uviscii)


;;; uiso8859-14.el --- tables between UCS and ISO-8859-14   -*- coding: iso-2022-7bit  -*-

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

(put 'latin-iso8859-14 'unicode-assoc 'iso-8859-14-vs-unicode-assoc)

(defvar
  iso-8859-14-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?\,_ (B . "0x00A0")			; NO-BREAK SPACE
       (?,_!(B . "0x1E02")			; LATIN CAPITAL LETTER B WITH DOT ABOVE
       (?,_"(B . "0x1E03")			; LATIN SMALL LETTER B WITH DOT ABOVE
       (?,_#(B . "0x00A3")			; POUND SIGN
       (?,_$(B . "0x010A")			; LATIN CAPITAL LETTER C WITH DOT ABOVE
       (?,_%(B . "0x010B")			; LATIN SMALL LETTER C WITH DOT ABOVE
       (?,_&(B . "0x1E0A")			; LATIN CAPITAL LETTER D WITH DOT ABOVE
       (?,_'(B . "0x00A7")			; SECTION SIGN
       (?,_((B . "0x1E80")			; LATIN CAPITAL LETTER W WITH GRAVE
       (?,_)(B . "0x00A9")			; COPYRIGHT SIGN
       (?,_*(B . "0x1E82")			; LATIN CAPITAL LETTER W WITH ACUTE
       (?,_+(B . "0x1E0B")			; LATIN SMALL LETTER D WITH DOT ABOVE
       (?,_,(B . "0x1EF2")			; LATIN CAPITAL LETTER Y WITH GRAVE
       (?,_-(B . "0x00AD")			; SOFT HYPHEN
       (?,_.(B . "0x00AE")			; REGISTERED SIGN
       (?,_/(B . "0x0178")			; LATIN CAPITAL LETTER Y WITH DIAERESIS
       (?,_0(B . "0x1E1E")			; LATIN CAPITAL LETTER F WITH DOT ABOVE
       (?,_1(B . "0x1E1F")			; LATIN SMALL LETTER F WITH DOT ABOVE
       (?,_2(B . "0x0120")			; LATIN CAPITAL LETTER G WITH DOT ABOVE
       (?,_3(B . "0x0121")			; LATIN SMALL LETTER G WITH DOT ABOVE
       (?,_4(B . "0x1E40")			; LATIN CAPITAL LETTER M WITH DOT ABOVE
       (?,_5(B . "0x1E41")			; LATIN SMALL LETTER M WITH DOT ABOVE
       (?,_6(B . "0x00B6")			; PILCROW SIGN
       (?,_7(B . "0x1E56")			; LATIN CAPITAL LETTER P WITH DOT ABOVE
       (?,_8(B . "0x1E81")			; LATIN SMALL LETTER W WITH GRAVE
       (?,_9(B . "0x1E57")			; LATIN SMALL LETTER P WITH DOT ABOVE
       (?,_:(B . "0x1E83")			; LATIN SMALL LETTER W WITH ACUTE
       (?,_;(B . "0x1E60")			; LATIN CAPITAL LETTER S WITH DOT ABOVE
       (?,_<(B . "0x1EF3")			; LATIN SMALL LETTER Y WITH GRAVE
       (?,_=(B . "0x1E84")			; LATIN CAPITAL LETTER W WITH DIAERESIS
       (?,_>(B . "0x1E85")			; LATIN SMALL LETTER W WITH DIAERESIS
       (?,_?(B . "0x1E61")			; LATIN SMALL LETTER S WITH DOT ABOVE
       (?,_@(B . "0x00C0")			; LATIN CAPITAL LETTER A WITH GRAVE
       (?,_A(B . "0x00C1")			; LATIN CAPITAL LETTER A WITH ACUTE
       (?,_B(B . "0x00C2")			; LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       (?,_C(B . "0x00C3")			; LATIN CAPITAL LETTER A WITH TILDE
       (?,_D(B . "0x00C4")			; LATIN CAPITAL LETTER A WITH DIAERESIS
       (?,_E(B . "0x00C5")			; LATIN CAPITAL LETTER A WITH RING ABOVE
       (?,_F(B . "0x00C6")			; LATIN CAPITAL LETTER AE
       (?,_G(B . "0x00C7")			; LATIN CAPITAL LETTER C WITH CEDILLA
       (?,_H(B . "0x00C8")			; LATIN CAPITAL LETTER E WITH GRAVE
       (?,_I(B . "0x00C9")			; LATIN CAPITAL LETTER E WITH ACUTE
       (?,_J(B . "0x00CA")			; LATIN CAPITAL LETTER E WITH CIRCUMFLEX
       (?,_K(B . "0x00CB")			; LATIN CAPITAL LETTER E WITH DIAERESIS
       (?,_L(B . "0x00CC")			; LATIN CAPITAL LETTER I WITH GRAVE
       (?,_M(B . "0x00CD")			; LATIN CAPITAL LETTER I WITH ACUTE
       (?,_N(B . "0x00CE")			; LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       (?,_O(B . "0x00CF")			; LATIN CAPITAL LETTER I WITH DIAERESIS
       (?,_P(B . "0x0174")			; LATIN CAPITAL LETTER W WITH CIRCUMFLEX
       (?,_Q(B . "0x00D1")			; LATIN CAPITAL LETTER N WITH TILDE
       (?,_R(B . "0x00D2")			; LATIN CAPITAL LETTER O WITH GRAVE
       (?,_S(B . "0x00D3")			; LATIN CAPITAL LETTER O WITH ACUTE
       (?,_T(B . "0x00D4")			; LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       (?,_U(B . "0x00D5")			; LATIN CAPITAL LETTER O WITH TILDE
       (?,_V(B . "0x00D6")			; LATIN CAPITAL LETTER O WITH DIAERESIS
       (?,_W(B . "0x1E6A")			; LATIN CAPITAL LETTER T WITH DOT ABOVE
       (?,_X(B . "0x00D8")			; LATIN CAPITAL LETTER O WITH STROKE
       (?,_Y(B . "0x00D9")			; LATIN CAPITAL LETTER U WITH GRAVE
       (?,_Z(B . "0x00DA")			; LATIN CAPITAL LETTER U WITH ACUTE
       (?,_[(B . "0x00DB")			; LATIN CAPITAL LETTER U WITH CIRCUMFLEX
       (?,_\(B . "0x00DC")			; LATIN CAPITAL LETTER U WITH DIAERESIS
       (?,_](B . "0x00DD")			; LATIN CAPITAL LETTER Y WITH ACUTE
       (?,_^(B . "0x0176")			; LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
       (?,__(B . "0x00DF")			; LATIN SMALL LETTER SHARP S
       (?,_`(B . "0x00E0")			; LATIN SMALL LETTER A WITH GRAVE
       (?,_a(B . "0x00E1")			; LATIN SMALL LETTER A WITH ACUTE
       (?,_b(B . "0x00E2")			; LATIN SMALL LETTER A WITH CIRCUMFLEX
       (?,_c(B . "0x00E3")			; LATIN SMALL LETTER A WITH TILDE
       (?,_d(B . "0x00E4")			; LATIN SMALL LETTER A WITH DIAERESIS
       (?,_e(B . "0x00E5")			; LATIN SMALL LETTER A WITH RING ABOVE
       (?,_f(B . "0x00E6")			; LATIN SMALL LETTER AE
       (?,_g(B . "0x00E7")			; LATIN SMALL LETTER C WITH CEDILLA
       (?,_h(B . "0x00E8")			; LATIN SMALL LETTER E WITH GRAVE
       (?,_i(B . "0x00E9")			; LATIN SMALL LETTER E WITH ACUTE
       (?,_j(B . "0x00EA")			; LATIN SMALL LETTER E WITH CIRCUMFLEX
       (?,_k(B . "0x00EB")			; LATIN SMALL LETTER E WITH DIAERESIS
       (?,_l(B . "0x00EC")			; LATIN SMALL LETTER I WITH GRAVE
       (?,_m(B . "0x00ED")			; LATIN SMALL LETTER I WITH ACUTE
       (?,_n(B . "0x00EE")			; LATIN SMALL LETTER I WITH CIRCUMFLEX
       (?,_o(B . "0x00EF")			; LATIN SMALL LETTER I WITH DIAERESIS
       (?,_p(B . "0x0175")			; LATIN SMALL LETTER W WITH CIRCUMFLEX
       (?,_q(B . "0x00F1")			; LATIN SMALL LETTER N WITH TILDE
       (?,_r(B . "0x00F2")			; LATIN SMALL LETTER O WITH GRAVE
       (?,_s(B . "0x00F3")			; LATIN SMALL LETTER O WITH ACUTE
       (?,_t(B . "0x00F4")			; LATIN SMALL LETTER O WITH CIRCUMFLEX
       (?,_u(B . "0x00F5")			; LATIN SMALL LETTER O WITH TILDE
       (?,_v(B . "0x00F6")			; LATIN SMALL LETTER O WITH DIAERESIS
       (?,_w(B . "0x1E6B")			; LATIN SMALL LETTER T WITH DOT ABOVE
       (?,_x(B . "0x00F8")			; LATIN SMALL LETTER O WITH STROKE
       (?,_y(B . "0x00F9")			; LATIN SMALL LETTER U WITH GRAVE
       (?,_z(B . "0x00FA")			; LATIN SMALL LETTER U WITH ACUTE
       (?,_{(B . "0x00FB")			; LATIN SMALL LETTER U WITH CIRCUMFLEX
       (?,_|(B . "0x00FC")			; LATIN SMALL LETTER U WITH DIAERESIS
       (?,_}(B . "0x00FD")			; LATIN SMALL LETTER Y WITH ACUTE
       (?,_~(B . "0x0177")			; LATIN SMALL LETTER Y WITH CIRCUMFLEX
       (?,_(B . "0x00FF")			; LATIN SMALL LETTER Y WITH DIAERESIS
       ))))

(provide 'uiso8859-14)

;;; uiso8859-14.el ends here

;;; ulao.el --- tables between Unicode and Emacs Lao  -*- coding: iso-2022-7bit  -*-

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, Lao

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

(put 'lao 'unicode-assoc 'lao-vs-unicode-assoc)

(defvar
  lao-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?(1!(B . "0x0e81") ;; LAO LETTER KO
       (?(1"(B . "0x0e82") ;; LAO LETTER KHO SUNG
       (?(1$(B . "0x0e84") ;; LAO LETTER KHO TAM
       (?(1'(B . "0x0e87") ;; LAO LETTER NGO
       (?(1((B . "0x0e88") ;; LAO LETTER CO
       (?(1*(B . "0x0e8a") ;; LAO LETTER SO TAM
       (?(1-(B . "0x0e8d") ;; LAO LETTER NYO
       (?(14(B . "0x0e94") ;; LAO LETTER DO;
       (?(15(B . "0x0e95") ;; LAO LETTER TO;
       (?(16(B . "0x0e96") ;; LAO LETTER THO SUNG
       (?(17(B . "0x0e97") ;; LAO LETTER THO TAM
       (?(19(B . "0x0e99") ;; LAO LETTER NO
       (?(1:(B . "0x0e9a") ;; LAO LETTER BO
       (?(1;(B . "0x0e9b") ;; LAO LETTER PO
       (?(1<(B . "0x0e9c") ;; LAO LETTER PHO SUNG
       (?(1=(B . "0x0e9d") ;; LAO LETTER FO TAM
       (?(1>(B . "0x0e9e") ;; LAO LETTER PHO TAM
       (?(1?(B . "0x0e9f") ;; LAO LETTER FO SUNG
       (?(1A(B . "0x0ea1") ;; LAO LETTER MO
       (?(1B(B . "0x0ea2") ;; LAO LETTER YO
       (?(1C(B . "0x0ea3") ;; LAO LETTER LO LING
       (?(1E(B . "0x0ea5") ;; LAO LETTER LO LOOT
       (?(1G(B . "0x0ea7") ;; LAO LETTER WO
       (?(1J(B . "0x0eaa") ;; LAO LETTER SO SUNG
       (?(1K(B . "0x0eab") ;; LAO LETTER HO SUNG
       (?(1M(B . "0x0ead") ;; LAO LETTER O
       (?(1N(B . "0x0eae") ;; LAO LETTER HO TAM
       (?(1O(B . "0x0eaf") ;; LAO ELLIPSIS
       (?(1P(B . "0x0eb0") ;; LAO VOWEL SIGN A
       (?(1Q(B . "0x0eb1") ;; LAO VOWEL SIGN MAI KAN
       (?(1R(B . "0x0eb2") ;; LAO VOWEL SIGN AA
       (?(1S(B . "0x0eb3") ;; LAO VOWEL SIGN AM
       (?(1T(B . "0x0eb4") ;; LAO VOWEL SIGN I
       (?(1U(B . "0x0eb5") ;; LAO VOWEL SIGN II
       (?(1V(B . "0x0eb6") ;; LAO VOWEL SIGN Y
       (?(1W(B . "0x0eb7") ;; LAO VOWEL SIGN YY
       (?(1X(B . "0x0eb8") ;; LAO VOWEL SIGN U
       (?(1Y(B . "0x0eb9") ;; LAO VOWEL SIGN UU
       (?(1[(B . "0x0ebb") ;; LAO VOWEL SIGN MAI KON
       (?(1\(B . "0x0ebc") ;; LAO SEMIVOWEL SIGN LO
       (?(1](B . "0x0ebd") ;; LAO SEMIVOWEL SIGN NYO
       (?(1`(B . "0x0ec0") ;; LAO VOWEL SIGN E
       (?(1a(B . "0x0ec1") ;; LAO VOWEL SIGN EI
       (?(1b(B . "0x0ec2") ;; LAO VOWEL SIGN O
       (?(1c(B . "0x0ec3") ;; LAO VOWEL SIGN AY
       (?(1d(B . "0x0ec4") ;; LAO VOWEL SIGN AI
       (?(1f(B . "0x0ec6") ;; LAO KO LA
       (?(1h(B . "0x0ec8") ;; LAO TONE MAI EK
       (?(1i(B . "0x0ec9") ;; LAO TONE MAI THO
       (?(1j(B . "0x0eca") ;; LAO TONE MAI TI
       (?(1k(B . "0x0ecb") ;; LAO TONE MAI CATAWA
       (?(1l(B . "0x0ecc") ;; LAO CANCELLATION MARK
       (?(1m(B . "0x0ecd") ;; LAO NIGGAHITA
       (?(1p(B . "0x0ed0") ;; LAO DIGIT ZERO
       (?(1q(B . "0x0ed1") ;; LAO DIGIT ONE
       (?(1r(B . "0x0ed2") ;; LAO DIGIT TWO
       (?(1s(B . "0x0ed3") ;; LAO DIGIT THREE
       (?(1t(B . "0x0ed4") ;; LAO DIGIT FOUR
       (?(1u(B . "0x0ed5") ;; LAO DIGIT FIVE
       (?(1v(B . "0x0ed6") ;; LAO DIGIT SIX
       (?(1w(B . "0x0ed7") ;; LAO DIGIT SEVEN
       (?(1x(B . "0x0ed8") ;; LAO DIGIT EIGHT
       (?(1y(B . "0x0ed9") ;; LAO DIGIT NINE
       (?(1|(B . "0x0edc") ;; LAO HO NO
       (?(1}(B . "0x0edd") ;; LAO HO MO
       ))))

(provide 'ulao)

;;; ulao.el ends here

;;; -*- coding: iso-2022-7bit  -*-
;;; uiscii.el --- incomplete table between Unicode and IS13194:1991.

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, IS13194

;; Copyright (C) 2000 Miyashita Hisashi

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

(put 'indian-is13194 'unicode-assoc
     'indian-is13194-vs-unicode-assoc)

; IS13194 1991 code table.
;  0 1 2 3 4 5 6 7 8 9 A B C D E F
;
;   (5!"#$%&'()*+,-./(B
;
; (50123456789:;<=>?(B
;
; (5@ABCDEFGHIJKLMNO(B
;
; (5PQRSTUVWXYZ[\]^_(B
;
; (5`abcdefghijklmno(B
;
; (5pqrstuvwxyz(B

;;; This table is incomplete because Unicode has more
;;; characters precomposed with Nukta.

(defvar
  indian-is13194-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?(5!(B . "0x0901") ;; Vowel-modifier CHANDRABINDU
       (?(5"(B . "0x0902") ;; Vowel-modifier ANUSWAR
       (?(5#(B . "0x0903") ;; Vowel-modifier VISARG
       (?(5$(B . "0x0905") ;; Vowel A
       (?(5%(B . "0x0906") ;; Vowel AA
       (?(5&(B . "0x0907") ;; Vowel I
       (?(5'(B . "0x0908") ;; Vowel II
       (?(5((B . "0x0909") ;; Vowel U
       (?(5)(B . "0x090A") ;; Vowel UU
       (?(5*(B . "0x090B") ;; Vowel RI
       (?(5+(B . "0x090E") ;; Vowel E (Southern Scripts)
       (?(5,(B . "0x090F") ;; Vowel EY
       (?(5-(B . "0x0910") ;; Vowel AI
       (?(5.(B . "0x090D") ;; Vowel AYE (Devanagari Scripts)
       (?(5/(B . "0x0912") ;; Vowel O (Southern Scripts)
       (?(50(B . "0x0913") ;; Vowel OW
       (?(51(B . "0x0914") ;; Vowel AU
       (?(52(B . "0x090D") ;; Vowel AWE (Devanagari Scripts)
       (?(53(B . "0x0915") ;; Consonant KA
       (?(54(B . "0x0916") ;; Consonant KHA
       (?(55(B . "0x0917") ;; Consonant GA
       (?(56(B . "0x0918") ;; Consonant GHA
       (?(57(B . "0x0919") ;; Consonant NGA
       (?(58(B . "0x091A") ;; Consonant CHA
       (?(59(B . "0x091B") ;; Consonant CHHA
       (?(5:(B . "0x091C") ;; Consonant JA
       (?(5;(B . "0x091D") ;; Consonant JHA
       (?(5<(B . "0x091E") ;; Consonant JNA
       (?(5=(B . "0x091F") ;; Consonant Hard TA
       (?(5>(B . "0x0920") ;; Consonant Hard THA
       (?(5?(B . "0x0921") ;; Consonant Hard DA
       (?(5@(B . "0x0922") ;; Consonant Hard DHA
       (?(5A(B . "0x0923") ;; Consonant Hard NA
       (?(5B(B . "0x0924") ;; Consonant Soft TA
       (?(5C(B . "0x0925") ;; Consonant Soft THA
       (?(5D(B . "0x0926") ;; Consonant Soft DA
       (?(5E(B . "0x0927") ;; Consonant Soft DHA
       (?(5F(B . "0x0928") ;; Consonant Soft NA
       (?(5G(B . "0x0929") ;; Consonant NA (Tamil)
       (?(5H(B . "0x092A") ;; Consonant PA
       (?(5I(B . "0x092B") ;; Consonant PHA
       (?(5J(B . "0x092C") ;; Consonant BA
       (?(5K(B . "0x092D") ;; Consonant BHA
       (?(5L(B . "0x092E") ;; Consonant MA
       (?(5M(B . "0x092F") ;; Consonant YA
       (?(5N(B . "0x095F") ;; Consonant JYA (Bengali, Assamese, and Oriya)
       (?(5O(B . "0x0930") ;; Consonant RA
       (?(5P(B . "0x0931") ;; Consonant Hard RA (Southern Scripts)
       (?(5Q(B . "0x0932") ;; Consonant LA
       (?(5R(B . "0x0933") ;; Consonant Hard LA
       (?(5S(B . "0x0934") ;; Consonant ZHA
       (?(5T(B . "0x0935") ;; Consonant VA
       (?(5U(B . "0x0936") ;; Consonant SHA
       (?(5V(B . "0x0937") ;; Consonant Hard SHA
       (?(5W(B . "0x0938") ;; Consonant SA
       (?(5X(B . "0x0939") ;; Consonant HA
     ;;INV(0xD9) Consonant Invisible
       (?(5Z(B . "0x093E") ;; Vowel Sign AA
       (?(5[(B . "0x093F") ;; Vowel Sign I
       (?(5\(B . "0x0940") ;; Vowel Sign II
       (?(5](B . "0x0941") ;; Vowel Sign U
       (?(5^(B . "0x0942") ;; Vowel Sign UU
       (?(5_(B . "0x0943") ;; Vowel Sign RI
       (?(5`(B . "0x0946") ;; Vowel Sign E (Southern Scripts)
       (?(5a(B . "0x0947") ;; Vowel Sign EY
       (?(5b(B . "0x0948") ;; Vowel Sign AI
       (?(5c(B . "0x0945") ;; Vowel Sign AYE (Devanagari Scripts)
       (?(5d(B . "0x094A") ;; Vowel Sign O (Southern Scripts)
       (?(5e(B . "0x094B") ;; Vowel Sign OW
       (?(5f(B . "0x094C") ;; Vowel Sign AU
       (?(5g(B . "0x0949") ;; Vowel Sign AWE (Devanagari Scripts)
       (?(5h(B . "0x094D") ;; Vowel Omission Sign (Halant)
       (?(5i(B . "0x093C") ;; Diacritic Sign (Nukta)
       (?(5j(B . "0x0964") ;; Full Stop (Viram, Nothern Scripts)
       ;;  Reserved
       ;;  Reserved
       ;;  Reserved
       ;;  Reserved
       ;;  ATR(0xEF)
       ;;  EXT(0xF0)
       (?(5q(B . "0x0966") ;; DEVANAGARI DIGIT 0
       (?(5r(B . "0x0967") ;; DEVANAGARI DIGIT 1
       (?(5s(B . "0x0968") ;; DEVANAGARI DIGIT 2
       (?(5t(B . "0x0969") ;; DEVANAGARI DIGIT 3
       (?(5u(B . "0x096A") ;; DEVANAGARI DIGIT 4
       (?(5v(B . "0x096B") ;; DEVANAGARI DIGIT 5
       (?(5w(B . "0x096C") ;; DEVANAGARI DIGIT 6
       (?(5x(B . "0x096D") ;; DEVANAGARI DIGIT 7
       (?(5y(B . "0x096E") ;; DEVANAGARI DIGIT 8
       (?(5z(B . "0x096F") ;; DEVANAGARI DIGIT 9
       ))))

(provide 'uiscii)

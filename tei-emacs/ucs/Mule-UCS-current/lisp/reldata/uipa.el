;;; -*- coding: iso-2022-7bit  -*-
;;; uipa.el --- tables between Unicode and Mule IPA

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, IPA

;; Copyright (C) 1999-2000 Miyashita Hisashi

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

(put 'ipa 'unicode-assoc
     'ipa-vs-unicode-assoc)

(defvar
  ipa-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,0 (B . "0x0069") ;; LATIN SMALL LETTER I
                       ;; (unrounded front close)
       (?,0!(B . "0x026A") ;; LATIN SMALL CAPITAL I
                       ;; (unrounded front/central close/close-mid)
       (?,0"(B . "0x0065") ;; LATIN SMALL LETTER E
                       ;; (unrounded front close-mid)
       (?,0#(B . "0x025B") ;; LATIN SMALL LETTER OPEN E
                       ;; (lower-mid front unrounded vowel, unrounded front open-mid)
       (?,0$(B . "0x00E6") ;; LATIN SMALL LETTER AE (,Af(B)
                       ;; (unrounded front open-mid/open)
       (?,0%(B . "0x0061") ;; LATIN SMALL LETTER A
                       ;; (unrounded front open)
       (?,0&(B . "0x0268") ;; LATIN SMALL LETTER I WITH STROKE
                       ;; (high central unrounded vowel, unrounded central close)
       (?,0'(B . "0x0259") ;; LATIN SMALL LETTER SCHWA
                       ;; (mid central unrounded vowel, unrounded central close-mid/open-mid)
       (?,0((B . "0x0250") ;; LATIN SMALL LETTER TURNED A
                       ;; (low central unrounded vowel, unrounded central open-mid/open)
       (?,0)(B . "0x026F") ;; LATIN SMALL LETTER TURNED M
                       ;; (high back unrounded vowel, unrounded back close)
       (?,0*(B . "0x0264") ;; LATIN SMALL LETTER RAMS HORN
                       ;; == LATIN SMALL LETTER BABY GAMMA
                       ;; (upper-mid back unrounded vowel, unrouned back close-mid)
       (?,0+(B . "0x028C") ;; LATIN SMALL LETTER TURNED V
                       ;; (lower-mid back unrounded vowel, unrounded back open-mid)
       (?,0,(B . "0x0251") ;; LATIN SMALL LETTER ALPHA
                       ;; (low back unrounded vowel, unrounded back open)
       (?,0-(B . "0x0079") ;; LATIN SMALL LETTER Y
                       ;; (rounded front close)
       (?,0.(B . "0x028F") ;; LATIN LETTER SMALL CAPITAL Y
                       ;; (semi-high front rounded vowel, rounded front/central close/close-mid)
       (?,0/(B . "0x00F8") ;; LATIN SMALL LETTER O WITH STROKE (,Ax(B)
                       ;; (rounded front close-mid)
       (?,00(B . "0x0153") ;; LATIN SMALL LIGATURE OE
                       ;; (rounded front open-mid)
       (?,01(B . "0x0276") ;; LATIN LETTER SMALL CAPITAL OE
                       ;; (low fronted rounded vowel, rounded front open)
       (?,02(B . "0x0289") ;; LATIN SMALL LETTER U BAR
                       ;; (high central rounded vowel, rounded central close)
       (?,03(B . "0x0275") ;; LATIN SMALL LETTER BARRED O
                       ;; (rounded schwa, rounded central close-mid/open-mid)
       (?,04(B . "0x0075") ;; LATIN SMALL LETTER U
                       ;; (rounded back close)
       (?,05(B . "0x028A") ;; LATIN SMALL LETTER UPSILON
                       ;; (rounded central/back close/close-mid, semi-high back rounded vowel)
       (?,06(B . "0x006F")       ;; LATIN SMALL LETTER O
                       ;; (rounded back close-mid)
       (?,07(B . "0x0254") ;; LATIN SMALL LETTER OPEN O
                       ;; (rounded back open-mid)
       (?,08(B . "0x0252") ;; LATIN SMALL LETTER TURNED ALPHA
                       ;; (rounded back open)
       (?,0:(B . "0x025A") ;; LATIN SMALL LETTER SCHWA WITH HOOK
                       ;; (rhotacized schwa, rhotcity unrounded central close-mid/open-mid)

       ;;(?,0;(B . ?\x)    ;; LATIN SMALL LETTER OPEN E(U+025B) + COMBINING TILDA(U+0303)
                       ;; (nasalized front open-mid unrounded)
       ;;(?,0<(B . ?\x)    ;; LATIN SMALL LETTER ALPHA(U+0251) + COMBINING TILDA(U+0303)
                       ;; (nasalazed back open unrounded)
       ;;(?,0=(B . ?\x)    ;; LATIN SMALL LIGATURE OE(U+0153) + COMBINING TILDA(U+0303)
                       ;; (nasalazed front open-mid rounded)
       ;;(?,0>(B . ?\x)    ;; LATIN SMALL LETTER OPEN O(U+0254) + COMBINING TILDA(U+0303)
                       ;; (nasalazed back open-mid rounded)

       (?,0@(B . "0x0070") ;; LATIN SMALL LETTER P
                       ;; (voiceless bilabial plosive)
       (?,0A(B . "0x0062") ;; LATIN SMALL LETTER B
                       ;; (voiced bilabial plosive)
       (?,0B(B . "0x0074") ;; LATIN SMALL LETTER T
                       ;; (voiceless alveolar plosive)
       (?,0C(B . "0x0064") ;; LATIN SMALL LETTER D
                       ;; (voiced alveolar plosive)
       (?,0D(B . "0x006B") ;; LATIN SMALL LETTER K
                       ;; (voiceless velar plosive)
       (?,0E(B . "0x0067") ;; LATIN SMALL LETTER G
                       ;; (voiced velar plosive)
       (?,0F(B . "0x0066") ;; LATIN SMALL LETTER F
                       ;; (voiceless labiodental fricative)
       (?,0G(B . "0x0076") ;; LATIN SMALL LETTER V
                       ;; (voiced labiodental fricative)
       (?,0H(B . "0x03B8") ;; GREEK SMALL LETTER THETA (,Fh(B)
                       ;; (voiceless dental fricative)
       (?,0I(B . "0x00F0") ;; LATIN SMALL LETTER ETH (,Ap(B)
                       ;; (voiced dental fricative)
       (?,0J(B . "0x0073") ;; LATIN SMALL LETTER S
                       ;; (voiceless alveolar fricative)
       (?,0K(B . "0x007A") ;; LATIN SMALL LETTER Z
                       ;; (voiced alveolar fricative)
       (?,0L(B . "0x0283") ;; LATIN SMALL LETTER ESH
                       ;; (voiceless postalveolar fricative)
       (?,0M(B . "0x0292") ;; LATIN SMALL LETTER EZH
                       ;; (voiced postalveolar fricative)
       (?,0N(B . "0x00E7") ;; LATIN SMALL LETTER C WITH CEDILLA
                       ;; (voiceless palatal fricative)
       (?,0O(B . "0x0078") ;; LATIN SMALL LETTER X
                       ;; (voiceless velar fricative)
       (?,0P(B . "0x0281") ;; LATIN LETTER SMALL CAPITAL INVERTED R
                       ;; (voiced uvular fricative/approximant)
       (?,0Q(B . "0x0068") ;; LATIN SMALL LETTER H
                       ;; (voiceless pharyngeal fricative)
       (?,0R(B . "0x006D") ;; LATIN SMALL LETTER M
                       ;; (voiced bilabial nasal)
       (?,0S(B . "0x006E") ;; LATIN SMALL LETTER N
                       ;; (voiced alveolar nasal)
       (?,0T(B . "0x0272") ;; LATIN SMALL LETTER N WITH LEFT HOOK
                       ;; (voiced palatal nasal)
       (?,0U(B . "0x014B") ;; LATIN SMALL LETTER ENG (,D?(B)
                       ;; (voiced velar nasal)
       (?,0V(B . "0x0072") ;; LATIN SMALL LETTER R
                       ;; (voiced alveolar trill)
       (?,0W(B . "0x0280") ;; LATIN SMALL LETTER CAPITAL R
                       ;; (voiced uvular trill)
       (?,0X(B . "0x0279") ;; LATIN SMALL LETTER TURNED R
                       ;; (voiced alveolar approximant)
       (?,0Y(B . "0x006A") ;; LATIN SMALL LETTER J
                       ;; (voiced palatal approximant)
       (?,0Z(B . "0x006C") ;; LATIN SMALL LETTER L
                       ;; (voiced alveolar latelal approximant)
       (?,0[(B . "0x028E") ;; LATIN SMALL LETTER TURNED Y
                       ;; (voiced palatal lateral approximant)
       (?,0\(B . "0x029F") ;; LATIN SMALL CAPITAL L
                       ;; (voiced velar lateral approximant)
       (?,0](B . "0x0265") ;; LATIN SMALL LETTER TURNED H
                       ;; (voiced rounded palatal approximant, voiced labial-palatal approximant)
       (?,0^(B . "0x0077") ;; LATIN SMALL LETTER W
                       ;; (voiced labial-velar approximant)
       (?,0_(B . "0x028D") ;; LATIN SMALL LETTER TURNED W
                       ;; (voiceless rounded labiovelar approximant,
                       ;;  voiceless labial-velar fricative)
       (?,0p(B . "0x02C8") ;; MODIFIER LETTER VERTICAL LINE
                       ;; (primary stress)
       (?,0q(B . "0x02CC") ;; MODIFIER LETTER LOW VERTICAL LINE
                       ;; (secondary stress)
       (?,0r(B . "0x02D0") ;; MODIFIER LETTER TRIANGULAR COLON
                       ;; (length mark, long)
     )))
  "IPA(defined by Mule, which is sorted in phonetic order)
vs Unicode(mainly alphabetical order) translation rule.")

(provide 'uipa)



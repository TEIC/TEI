;;; -*- coding: iso-2022-7bit  -*-
;;; utibetan.el --- Table between Unicode and MuleTibetan.

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, Tibetan

;; Author: Toru Tomabechi <Toru.Tomabechi@orient.unil.ch>

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

(put 'tibetan 'unicode-assoc
     'tibetan-vs-unicode-assoc)

(defvar
  tibetan-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?$(7!0(B . "0x0F00") ;; TIBETAN SYLLABLE OM
       (?$(7!1(B . "0x0F01") ;; TIBETAN MARK GTER YIG MGO TRUNCATED A
       (?$(7!2(B . "0x0F02") ;; TIBETAN MARK GTER YIG MGO -UM RNAM BCAD MA
       (?$(7!3(B . "0x0F03") ;; TIBETAN MARK GTER YIG MGO -UM GTER TSHEG MA
       (?$(7!4(B . "0x0F04") ;; TIBETAN MARK INITIAL YIG MGO MDUN MA
       (?$(7!5(B . "0x0F05") ;; TIBETAN MARK CLOSING YIG MGO SGAB MA
       (?$(7!6(B . "0x0F06") ;; TIBETAN MARK CARET YIG MGO PHUR SHAD MA
       (?$(7!7(B . "0x0F07") ;; TIBETAN MARK YIG MGO TSHEG SHAD MA
       (?$(7!8(B . "0x0F08") ;; TIBETAN MARK SBRUL SHAD
       (?$(7!9(B . "0x0F09") ;; TIBETAN MARK BSKUR YIG MGO
       (?$(7!:(B . "0x0F0A") ;; TIBETAN MARK BKA- SHOG YIG MGO
       (?$(7!;(B . "0x0F0B") ;; TIBETAN MARK INTERSYLLABIC TSHEG
       (?$(7!<(B . "0x0F0C") ;; TIBETAN MARK DELIMITER TSHEG BSTAR
       (?$(7!=(B . "0x0F0D") ;; TIBETAN MARK SHAD
       (?$(7!>(B . "0x0F0E") ;; TIBETAN MARK NYIS SHAD
       (?$(7!?(B . "0x0F0F") ;; TIBETAN MARK TSHEG SHAD
       (?$(7!@(B . "0x0F10") ;; TIBETAN MARK NYIS TSHEG SHAD
       (?$(7!A(B . "0x0F11") ;; TIBETAN MARK RIN CHEN SPUNGS SHAD
       (?$(7!B(B . "0x0F12") ;; TIBETAN MARK RGYA GRAM SHAD
       (?$(7!C(B . "0x0F13") ;; TIBETAN MARK CARET -DZUD RTAGS ME LONG CAN
       (?$(7!D(B . "0x0F14") ;; TIBETAN MARK GTER TSHEG
       (?$(7!E(B . "0x0F15") ;; TIBETAN LOGOTYPE SIGN CHAD RTAGS
       (?$(7!F(B . "0x0F16") ;; TIBETAN LOGOTYPE SIGN LHAG RTAGS
       (?$(7!G(B . "0x0F17") ;; TIBETAN ASTROLOGICAL SIGN SGRA GCAN -CHAR RTAGS
       (?$(7!H(B . "0x0F18") ;; TIBETAN ASTROLOGICAL SIGN -KHYUD PA
       (?$(7!I(B . "0x0F19") ;; TIBETAN ASTROLOGICAL SIGN SDONG TSHUGS
       (?$(7!J(B . "0x0F1A") ;; TIBETAN SIGN RDEL DKAR GCIG
       (?$(7!K(B . "0x0F1B") ;; TIBETAN SIGN RDEL DKAR GNYIS
       (?$(7!L(B . "0x0F1C") ;; TIBETAN SIGN RDEL DKAR GSUM
       (?$(7!M(B . "0x0F1D") ;; TIBETAN SIGN RDEL NAG GCIG
       (?$(7!N(B . "0x0F1E") ;; TIBETAN SIGN RDEL NAG GNYIS
       (?$(7!O(B . "0x0F1F") ;; TIBETAN SIGN RDEL DKAR RDEL NAG
       (?$(7!P(B . "0x0F20") ;; TIBETAN DIGIT ZERO
       (?$(7!Q(B . "0x0F21") ;; TIBETAN DIGIT ONE
       (?$(7!R(B . "0x0F22") ;; TIBETAN DIGIT TWO
       (?$(7!S(B . "0x0F23") ;; TIBETAN DIGIT THREE
       (?$(7!T(B . "0x0F24") ;; TIBETAN DIGIT FOUR
       (?$(7!U(B . "0x0F25") ;; TIBETAN DIGIT FIVE
       (?$(7!V(B . "0x0F26") ;; TIBETAN DIGIT SIX
       (?$(7!W(B . "0x0F27") ;; TIBETAN DIGIT SEVEN
       (?$(7!X(B . "0x0F28") ;; TIBETAN DIGIT EIGHT
       (?$(7!Y(B . "0x0F29") ;; TIBETAN DIGIT NINE
       (?$(7!Z(B . "0x0F2A") ;; TIBETAN DIGIT HALF ONE
       (?$(7![(B . "0x0F2B") ;; TIBETAN DIGIT HALF TWO
       (?$(7!\(B . "0x0F2C") ;; TIBETAN DIGIT HALF THREE
       (?$(7!](B . "0x0F2D") ;; TIBETAN DIGIT HALF FOUR
       (?$(7!^(B . "0x0F2E") ;; TIBETAN DIGIT HALF FIVE
       (?$(7!_(B . "0x0F2F") ;; TIBETAN DIGIT HALF SIX
       (?$(7!`(B . "0x0F30") ;; TIBETAN DIGIT HALF SEVEN
       (?$(7!a(B . "0x0F31") ;; TIBETAN DIGIT HALF EIGHT
       (?$(7!b(B . "0x0F32") ;; TIBETAN DIGIT HALF NINE
       (?$(7!c(B . "0x0F33") ;; TIBETAN DIGIT HALF ZERO
       (?$(7!d(B . "0x0F34") ;; TIBETAN MARK BSDUS RTAGS
       (?$(7!e(B . "0x0F35") ;; TIBETAN MARK NGAS BZUNG NYI ZLA
       (?$(7!f(B . "0x0F36") ;; TIBETAN MARK CARET -DZUD RTAGS BZHI MIG CAN
       (?$(7!g(B . "0x0F37") ;; TIBETAN MARK NGAS BZUNG SGOR RTAGS
       (?$(7!h(B . "0x0F38") ;; TIBETAN MARK CHE MGO
       (?$(7!i(B . "0x0F39") ;; TIBETAN MARK TSA -PHRU
       (?$(7!j(B . "0x0F3A") ;; TIBETAN MARK GUG RTAGS GYON
       (?$(7!k(B . "0x0F3B") ;; TIBETAN MARK GUG RTAGS GYAS
       (?$(7!l(B . "0x0F3C") ;; TIBETAN MARK ANG KHANG GYON
       (?$(7!m(B . "0x0F3D") ;; TIBETAN MARK ANG KHANG GYAS
       (?$(7!n(B . "0x0F3E") ;; TIBETAN SIGN YAR TSHES
       (?$(7!o(B . "0x0F3F") ;; TIBETAN SIGN MAR TSHES
       (?4$(7"!0"!1(B . "0x0F40") ;; TIBETAN LETTER KA
       (?4$(7""0""1(B . "0x0F41") ;; TIBETAN LETTER KHA
       (?4$(7"#0"#1(B . "0x0F42") ;; TIBETAN LETTER GA
       (?4$(7"$0"$1(B . "0x0F43") ;; TIBETAN LETTER GHA
       (?4$(7"%0"%1(B . "0x0F44") ;; TIBETAN LETTER NGA
       (?4$(7"&0"&1(B . "0x0F45") ;; TIBETAN LETTER CA
       (?4$(7"'0"'1(B . "0x0F46") ;; TIBETAN LETTER CHA
       (?4$(7"(0"(1(B . "0x0F47") ;; TIBETAN LETTER JA
       (?4$(7"*0"*1(B . "0x0F49") ;; TIBETAN LETTER NYA
       (?4$(7"+0"+1(B . "0x0F4A") ;; TIBETAN LETTER TTA
       (?4$(7",0",1(B . "0x0F4B") ;; TIBETAN LETTER TTHA
       (?4$(7"-0"-1(B . "0x0F4C") ;; TIBETAN LETTER DDA
       (?4$(7".0".1(B . "0x0F4D") ;; TIBETAN LETTER DDHA
       (?4$(7"/0"/1(B . "0x0F4E") ;; TIBETAN LETTER NNA
       (?4$(7"00"01(B . "0x0F4F") ;; TIBETAN LETTER TA
       (?4$(7"10"11(B . "0x0F50") ;; TIBETAN LETTER THA
       (?4$(7"20"21(B . "0x0F51") ;; TIBETAN LETTER DA
       (?4$(7"30"31(B . "0x0F52") ;; TIBETAN LETTER DHA
       (?4$(7"40"41(B . "0x0F53") ;; TIBETAN LETTER NA
       (?4$(7"50"51(B . "0x0F54") ;; TIBETAN LETTER PA
       (?4$(7"60"61(B . "0x0F55") ;; TIBETAN LETTER PHA
       (?4$(7"70"71(B . "0x0F56") ;; TIBETAN LETTER BA
       (?4$(7"80"81(B . "0x0F57") ;; TIBETAN LETTER BHA
       (?4$(7"90"91(B . "0x0F58") ;; TIBETAN LETTER MA
       (?4$(7":0":1(B . "0x0F59") ;; TIBETAN LETTER TSA
       (?4$(7";0";1(B . "0x0F5A") ;; TIBETAN LETTER TSHA
       (?4$(7"<0"<1(B . "0x0F5B") ;; TIBETAN LETTER DZA
       (?4$(7"=0"=1(B . "0x0F5C") ;; TIBETAN LETTER DZHA
       (?4$(7">0">1(B . "0x0F5D") ;; TIBETAN LETTER WA
       (?4$(7"?0"?1(B . "0x0F5E") ;; TIBETAN LETTER ZHA
       (?4$(7"@0"@1(B . "0x0F5F") ;; TIBETAN LETTER ZA
       (?4$(7"A0"A1(B . "0x0F60") ;; TIBETAN LETTER -A
       (?4$(7"B0"B1(B . "0x0F61") ;; TIBETAN LETTER YA
       (?4$(7"C0"C1(B . "0x0F62") ;; TIBETAN LETTER RA
       (?4$(7"D0"D1(B . "0x0F63") ;; TIBETAN LETTER LA
       (?4$(7"E0"E1(B . "0x0F64") ;; TIBETAN LETTER SHA
       (?4$(7"F0"F1(B . "0x0F65") ;; TIBETAN LETTER SSA
       (?4$(7"G0"G1(B . "0x0F66") ;; TIBETAN LETTER SA
       (?4$(7"H0"H1(B . "0x0F67") ;; TIBETAN LETTER HA
       (?4$(7"I0"I1(B . "0x0F68") ;; TIBETAN LETTER A
       (?4$(7"J0"J1(B . "0x0F69") ;; TIBETAN LETTER KSSA
       (?4$(7"K0"K1(B . "0x0F6A") ;; TIBETAN LETTER FIXED-FORM RA
       ;;
       (?$(7"Q(B . "0x0F70") ;; TIBETAN VOWEL SIGN A (RESERVED in UNICODE)
       ;; Accepted when reading.
       ;; Removed before writing, if tibetan-strict-unicode is non-nil.
       ;;
       ;; Composite vowels, $(7"T(B $(7"V(B $(7"W(B $(7"X(B $(7"Y(B $(7"Z(B $(7"b(B,
       ;; are also canonicalized (i.e., decomposed) before writing,
       ;; if tibetan-strict-unicode is non-nil.
       (?$(7"R(B . "0x0F71") ;; TIBETAN VOWEL SIGN AA
       (?$(7"S(B . "0x0F72") ;; TIBETAN VOWEL SIGN I
       (?$(7"T(B . "0x0F73") ;; TIBETAN VOWEL SIGN II
       (?$(7"U(B . "0x0F74") ;; TIBETAN VOWEL SIGN U
       (?$(7"V(B . "0x0F75") ;; TIBETAN VOWEL SIGN UU
       (?$(7"W(B . "0x0F76") ;; TIBETAN VOWEL SIGN VOCALIC R
       (?$(7"X(B . "0x0F77") ;; TIBETAN VOWEL SIGN VOCALIC RR
       (?$(7"Y(B . "0x0F78") ;; TIBETAN VOWEL SIGN VOCALIC L
       (?$(7"Z(B . "0x0F79") ;; TIBETAN VOWEL SIGN VOCALIC LL
       (?$(7"[(B . "0x0F7A") ;; TIBETAN VOWEL SIGN E
       (?$(7"\(B . "0x0F7B") ;; TIBETAN VOWEL SIGN EE
       (?$(7"](B . "0x0F7C") ;; TIBETAN VOWEL SIGN O
       (?$(7"^(B . "0x0F7D") ;; TIBETAN VOWEL SIGN OO
       (?$(7"_(B . "0x0F7E") ;; TIBETAN SIGN RJES SU NGA RO
       (?$(7"`(B . "0x0F7F") ;; TIBETAN SIGN RNAM BCAD
       (?$(7"a(B . "0x0F80") ;; TIBETAN VOWEL SIGN REVERSED I
       (?$(7"b(B . "0x0F81") ;; TIBETAN VOWEL SIGN REVERSED II
       (?$(7"c(B . "0x0F82") ;; TIBETAN SIGN NYI ZLA NAA DA
       (?$(7"d(B . "0x0F83") ;; TIBETAN SIGN SNA LDAN
       (?$(7"e(B . "0x0F84") ;; TIBETAN MARK HALANTA
       (?$(7"f(B . "0x0F85") ;; TIBETAN MARK PALUTA
       (?$(7"g(B . "0x0F86") ;; TIBETAN SIGN LCI RTAGS
       (?$(7"h(B . "0x0F87") ;; TIBETAN SIGN YANG RTAGS
       (?$(7"i(B . "0x0F88") ;; TIBETAN SIGN LCE TSA CAN
       (?$(7"j(B . "0x0F89") ;; TIBETAN SIGN MCHU CAN
       (?$(7"k(B . "0x0F8A") ;; TIBETAN SIGN GRU CAN RGYINGS
       (?$(7"l(B . "0x0F8B") ;; TIBETAN SIGN GRU MED RGYINGS
       (?$(7#!(B . "0x0F90") ;; TIBETAN SUBJOINED LETTER KA
       (?$(7#"(B . "0x0F91") ;; TIBETAN SUBJOINED LETTER KHA
       (?$(7##(B . "0x0F92") ;; TIBETAN SUBJOINED LETTER GA
       (?$(7#$(B . "0x0F93") ;; TIBETAN SUBJOINED LETTER GHA
       (?$(7#%(B . "0x0F94") ;; TIBETAN SUBJOINED LETTER NGA
       (?$(7#&(B . "0x0F95") ;; TIBETAN SUBJOINED LETTER CA
       (?$(7#'(B . "0x0F96") ;; TIBETAN SUBJOINED LETTER CHA
       (?$(7#((B . "0x0F97") ;; TIBETAN SUBJOINED LETTER JA
       (?$(7#*(B . "0x0F99") ;; TIBETAN SUBJOINED LETTER NYA
       (?$(7#+(B . "0x0F9A") ;; TIBETAN SUBJOINED LETTER TTA
       (?$(7#,(B . "0x0F9B") ;; TIBETAN SUBJOINED LETTER TTHA
       (?$(7#-(B . "0x0F9C") ;; TIBETAN SUBJOINED LETTER DDA
       (?$(7#.(B . "0x0F9D") ;; TIBETAN SUBJOINED LETTER DDHA
       (?$(7#/(B . "0x0F9E") ;; TIBETAN SUBJOINED LETTER NNA
       (?$(7#0(B . "0x0F9F") ;; TIBETAN SUBJOINED LETTER TA
       (?$(7#1(B . "0x0FA0") ;; TIBETAN SUBJOINED LETTER THA
       (?$(7#2(B . "0x0FA1") ;; TIBETAN SUBJOINED LETTER DA
       (?$(7#3(B . "0x0FA2") ;; TIBETAN SUBJOINED LETTER DHA
       (?$(7#4(B . "0x0FA3") ;; TIBETAN SUBJOINED LETTER NA
       (?$(7#5(B . "0x0FA4") ;; TIBETAN SUBJOINED LETTER PA
       (?$(7#6(B . "0x0FA5") ;; TIBETAN SUBJOINED LETTER PHA
       (?$(7#7(B . "0x0FA6") ;; TIBETAN SUBJOINED LETTER BA
       (?$(7#8(B . "0x0FA7") ;; TIBETAN SUBJOINED LETTER BHA
       (?$(7#9(B . "0x0FA8") ;; TIBETAN SUBJOINED LETTER MA
       (?$(7#:(B . "0x0FA9") ;; TIBETAN SUBJOINED LETTER TSA
       (?$(7#;(B . "0x0FAA") ;; TIBETAN SUBJOINED LETTER TSHA
       (?$(7#<(B . "0x0FAB") ;; TIBETAN SUBJOINED LETTER DZA
       (?$(7#=(B . "0x0FAC") ;; TIBETAN SUBJOINED LETTER DZHA
       (?$(7#>(B . "0x0FAD") ;; TIBETAN SUBJOINED LETTER WA
       (?$(7#?(B . "0x0FAE") ;; TIBETAN SUBJOINED LETTER ZHA
       (?$(7#@(B . "0x0FAF") ;; TIBETAN SUBJOINED LETTER ZA
       (?$(7#A(B . "0x0FB0") ;; TIBETAN SUBJOINED LETTER -A
       (?$(7#B(B . "0x0FB1") ;; TIBETAN SUBJOINED LETTER YA
       (?$(7#C(B . "0x0FB2") ;; TIBETAN SUBJOINED LETTER RA
       (?$(7#D(B . "0x0FB3") ;; TIBETAN SUBJOINED LETTER LA
       (?$(7#E(B . "0x0FB4") ;; TIBETAN SUBJOINED LETTER SHA
       (?$(7#F(B . "0x0FB5") ;; TIBETAN SUBJOINED LETTER SSA
       (?$(7#G(B . "0x0FB6") ;; TIBETAN SUBJOINED LETTER SA
       (?$(7#H(B . "0x0FB7") ;; TIBETAN SUBJOINED LETTER HA
       (?$(7#I(B . "0x0FB8") ;; TIBETAN SUBJOINED LETTER A
       (?$(7#J(B . "0x0FB9") ;; TIBETAN SUBJOINED LETTER KSSA
       (?$(7#K(B . "0x0FBA") ;; TIBETAN SUBJOINED LETTER FIXED-FORM WA
       (?$(7#L(B . "0x0FBB") ;; TIBETAN SUBJOINED LETTER FIXED-FORM YA
       (?$(7#M(B . "0x0FBC") ;; TIBETAN SUBJOINED LETTER FIXED-FORM RA
       (?$(7#O(B . "0x0FBE") ;; TIBETAN KU RU KHA
       (?$(7#P(B . "0x0FBF") ;; TIBETAN KU RU KHA BZHI MIG CAN
       (?$(7#Q(B . "0x0FC0") ;; TIBETAN CANTILLATION SIGN HEAVY BEAT
       (?$(7#R(B . "0x0FC1") ;; TIBETAN CANTILLATION SIGN LIGHT BEAT
       (?$(7#S(B . "0x0FC2") ;; TIBETAN CANTILLATION SIGN CANG TE-U
       (?$(7#T(B . "0x0FC3") ;; TIBETAN CANTILLATION SIGN SBUB -CHAL
       (?$(7#U(B . "0x0FC4") ;; TIBETAN SYMBOL DRIL BU
       (?$(7#V(B . "0x0FC5") ;; TIBETAN SYMBOL RDO RJE
       (?$(7#W(B . "0x0FC6") ;; TIBETAN SYMBOL PADMA GDAN
       (?$(7#X(B . "0x0FC7") ;; TIBETAN SYMBOL RDO RJE RGYA GRAM
       (?$(7#Y(B . "0x0FC8") ;; TIBETAN SYMBOL PHUR PA
       (?$(7#Z(B . "0x0FC9") ;; TIBETAN SYMBOL NOR BU
       (?$(7#[(B . "0x0FCA") ;; TIBETAN SYMBOL NOR BU NYIS -KHYIL
       (?$(7#\(B . "0x0FCB") ;; TIBETAN SYMBOL NOR BU GSUM -KHYIL
       (?$(7#](B . "0x0FCC") ;; TIBETAN SYMBOL NOR BU BZHI -KHYIL
       (?$(7#`(B . "0x0FCF") ;; TIBETAN SIGN RDEL NAG GSUM
       ))))

(provide 'utibetan)

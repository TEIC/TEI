;;; -*- coding: iso-2022-7bit  -*-
;;; utis620.el --- tables between Unicode and Thai-TIS 620.

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, TIS 620

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

(put 'thai-tis620 'unicode-assoc
     'thai-tis620-vs-unicode-assoc)

(defvar
  thai-tis620-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?,T!(B . "0x0E01") ;; THAI CHARACTER KO KAI
       (?,T"(B . "0x0E02") ;; THAI CHARACTER KHO KHAI
       (?,T#(B . "0x0E03") ;; THAI CHARACTER KHO KHUAT
       (?,T$(B . "0x0E04") ;; THAI CHARACTER KHO KHWAI
       (?,T%(B . "0x0E05") ;; THAI CHARACTER KHO KHON
       (?,T&(B . "0x0E06") ;; THAI CHARACTER KHO RAKHANG
       (?,T'(B . "0x0E07") ;; THAI CHARACTER NGO NGU
       (?,T((B . "0x0E08") ;; THAI CHARACTER CHO CHAN
       (?,T)(B . "0x0E09") ;; THAI CHARACTER CHO CHING
       (?,T*(B . "0x0E0A") ;; THAI CHARACTER CHO CHANG
       (?,T+(B . "0x0E0B") ;; THAI CHARACTER SO SO
       (?,T,(B . "0x0E0C") ;; THAI CHARACTER CHO CHOE
       (?,T-(B . "0x0E0D") ;; THAI CHARACTER YO YING
       (?,T.(B . "0x0E0E") ;; THAI CHARACTER DO CHADA
       (?,T/(B . "0x0E0F") ;; THAI CHARACTER TO PATAK
       (?,T0(B . "0x0E10") ;; THAI CHARACTER THO THAN
       (?,T1(B . "0x0E11") ;; THAI CHARACTER THO NANGMONTHO
       (?,T2(B . "0x0E12") ;; THAI CHARACTER THO PHUTHAO
       (?,T3(B . "0x0E13") ;; THAI CHARACTER NO NEN
       (?,T4(B . "0x0E14") ;; THAI CHARACTER DO DEK
       (?,T5(B . "0x0E15") ;; THAI CHARACTER TO TAO
       (?,T6(B . "0x0E16") ;; THAI CHARACTER THO THUNG
       (?,T7(B . "0x0E17") ;; THAI CHARACTER THO THAHAN
       (?,T8(B . "0x0E18") ;; THAI CHARACTER THO THONG
       (?,T9(B . "0x0E19") ;; THAI CHARACTER NO NU
       (?,T:(B . "0x0E1A") ;; THAI CHARACTER BO BAIMAI
       (?,T;(B . "0x0E1B") ;; THAI CHARACTER PO PLA
       (?,T<(B . "0x0E1C") ;; THAI CHARACTER PHO PHUNG
       (?,T=(B . "0x0E1D") ;; THAI CHARACTER FO FA
       (?,T>(B . "0x0E1E") ;; THAI CHARACTER PHO PHAN
       (?,T?(B . "0x0E1F") ;; THAI CHARACTER FO FAN
       (?,T@(B . "0x0E20") ;; THAI CHARACTER PHO SAMPHAO
       (?,TA(B . "0x0E21") ;; THAI CHARACTER MO MA
       (?,TB(B . "0x0E22") ;; THAI CHARACTER YO YAK
       (?,TC(B . "0x0E23") ;; THAI CHARACTER RO RUA
       (?,TD(B . "0x0E24") ;; THAI CHARACTER RU
       (?,TE(B . "0x0E25") ;; THAI CHARACTER LO LING
       (?,TF(B . "0x0E26") ;; THAI CHARACTER LU
       (?,TG(B . "0x0E27") ;; THAI CHARACTER WO WAEN
       (?,TH(B . "0x0E28") ;; THAI CHARACTER SO SALA
       (?,TI(B . "0x0E29") ;; THAI CHARACTER SO RUSI
       (?,TJ(B . "0x0E2A") ;; THAI CHARACTER SO SUA
       (?,TK(B . "0x0E2B") ;; THAI CHARACTER HO HIP
       (?,TL(B . "0x0E2C") ;; THAI CHARACTER LO CHULA
       (?,TM(B . "0x0E2D") ;; THAI CHARACTER O ANG
       (?,TN(B . "0x0E2E") ;; THAI CHARACTER HO NOKHUK
       (?,TO(B . "0x0E2F") ;; THAI CHARACTER PAIYANNOI
       (?,TP(B . "0x0E30") ;; THAI CHARACTER SARA A
       (?,TQ(B . "0x0E31") ;; THAI CHARACTER MAI HAN-AKAT
       (?,TR(B . "0x0E32") ;; THAI CHARACTER SARA AA
       (?,TS(B . "0x0E33") ;; THAI CHARACTER SARA AM
       (?,TT(B . "0x0E34") ;; THAI CHARACTER SARA I
       (?,TU(B . "0x0E35") ;; THAI CHARACTER SARA II
       (?,TV(B . "0x0E36") ;; THAI CHARACTER SARA UE
       (?,TW(B . "0x0E37") ;; THAI CHARACTER SARA UEE
       (?,TX(B . "0x0E38") ;; THAI CHARACTER SARA U
       (?,TY(B . "0x0E39") ;; THAI CHARACTER SARA UU
       (?,TZ(B . "0x0E3A") ;; THAI CHARACTER PHINTHU
       (?,T_(B . "0x0E3F") ;; THAI CURRENCY SYMBOL BAHT
       (?,T`(B . "0x0E40") ;; THAI CHARACTER SARA E
       (?,Ta(B . "0x0E41") ;; THAI CHARACTER SARA AE
       (?,Tb(B . "0x0E42") ;; THAI CHARACTER SARA O
       (?,Tc(B . "0x0E43") ;; THAI CHARACTER SARA AI MAIMUAN
       (?,Td(B . "0x0E44") ;; THAI CHARACTER SARA AI MAIMALAI
       (?,Te(B . "0x0E45") ;; THAI CHARACTER LAKKHANGYAO
       (?,Tf(B . "0x0E46") ;; THAI CHARACTER MAIYAMOK
       (?,Tg(B . "0x0E47") ;; THAI CHARACTER MAITAIKHU
       (?,Th(B . "0x0E48") ;; THAI CHARACTER MAI EK
       (?,Ti(B . "0x0E49") ;; THAI CHARACTER MAI THO
       (?,Tj(B . "0x0E4A") ;; THAI CHARACTER MAI TRI
       (?,Tk(B . "0x0E4B") ;; THAI CHARACTER MAI CHATTAWA
       (?,Tl(B . "0x0E4C") ;; THAI CHARACTER THANTHAKHAT
       (?,Tm(B . "0x0E4D") ;; THAI CHARACTER NIKHAHIT
       (?,Tn(B . "0x0E4E") ;; THAI CHARACTER YAMAKKAN
       (?,To(B . "0x0E4F") ;; THAI CHARACTER FONGMAN
       (?,Tp(B . "0x0E50") ;; THAI DIGIT ZERO
       (?,Tq(B . "0x0E51") ;; THAI DIGIT ONE
       (?,Tr(B . "0x0E52") ;; THAI DIGIT TWO
       (?,Ts(B . "0x0E53") ;; THAI DIGIT THREE
       (?,Tt(B . "0x0E54") ;; THAI DIGIT FOUR
       (?,Tu(B . "0x0E55") ;; THAI DIGIT FIVE
       (?,Tv(B . "0x0E56") ;; THAI DIGIT SIX
       (?,Tw(B . "0x0E57") ;; THAI DIGIT SEVEN
       (?,Tx(B . "0x0E58") ;; THAI DIGIT EIGHT
       (?,Ty(B . "0x0E59") ;; THAI DIGIT NINE
       (?,Tz(B . "0x0E5A") ;; THAI CHARACTER ANGKHANKHU
       (?,T{(B . "0x0E5B") ;; THAI CHARACTER KHOMUT
       ))))

(provide 'utis620)

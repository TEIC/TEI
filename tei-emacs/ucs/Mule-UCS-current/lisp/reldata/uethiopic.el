;;; -*- coding: iso-2022-7bit  -*-
;;; uethiopic.el --- tables between Unicode and Mule ethiopic charset

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, ethiopic

;; Miyashita Hisashi(himi@bird.scphys.kyoto-u.ac.jp)
;; generated this table by converting UNIDATA of Unicode consortium.

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

;; Comment:
;; this file is converted from UNIDATA by the following dirty code.
; (defun foo ()
;   (let* ((filename "....")
; 	 (buf (generate-new-buffer
; 	       (format "*%s-tbl-translate*" filename)))
; 	 table)
;     (switch-to-buffer buf)
;     (insert-file-contents filename)
;     (setq table (make-table-alist-region
; 		 (point-min)
; 		 (point-max)
; 		 '("\\([0-9a-fA-F]+\\);" . hex-string-to-number)
; 		 '("\\([0-9a-fA-F]+\\);" . hex-string-to-number)
; 		 ".+;.+;\\(.*\\)$"
; 		 "^[0-9A-Fa-f]"))
;     (erase-buffer)
;     (mucs-print-character-a-list
;      table 1 (lambda (ch)
; 	       (setq ch (- ch #x1200))
; 	       (make-char 'ethiopic
; 			  (+ (/ ch 94) 33)
; 			  (+ (% ch 94) 33))))))

(put 'ethiopic 'unicode-assoc
     'ethiopic-vs-unicode-assoc)

(defvar
  ethiopic-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?$(3!!(B . "0x1200") ;; ETHIOPIC SYLLABLE HA
       (?$(3!"(B . "0x1201") ;; ETHIOPIC SYLLABLE HU
       (?$(3!#(B . "0x1202") ;; ETHIOPIC SYLLABLE HI
       (?$(3!$(B . "0x1203") ;; ETHIOPIC SYLLABLE HAA
       (?$(3!%(B . "0x1204") ;; ETHIOPIC SYLLABLE HEE
       (?$(3!&(B . "0x1205") ;; ETHIOPIC SYLLABLE HE
       (?$(3!'(B . "0x1206") ;; ETHIOPIC SYLLABLE HO
       (?$(3!)(B . "0x1208") ;; ETHIOPIC SYLLABLE LA
       (?$(3!*(B . "0x1209") ;; ETHIOPIC SYLLABLE LU
       (?$(3!+(B . "0x120A") ;; ETHIOPIC SYLLABLE LI
       (?$(3!,(B . "0x120B") ;; ETHIOPIC SYLLABLE LAA
       (?$(3!-(B . "0x120C") ;; ETHIOPIC SYLLABLE LEE
       (?$(3!.(B . "0x120D") ;; ETHIOPIC SYLLABLE LE
       (?$(3!/(B . "0x120E") ;; ETHIOPIC SYLLABLE LO
       (?$(3!0(B . "0x120F") ;; ETHIOPIC SYLLABLE LWA
       (?$(3!1(B . "0x1210") ;; ETHIOPIC SYLLABLE HHA
       (?$(3!2(B . "0x1211") ;; ETHIOPIC SYLLABLE HHU
       (?$(3!3(B . "0x1212") ;; ETHIOPIC SYLLABLE HHI
       (?$(3!4(B . "0x1213") ;; ETHIOPIC SYLLABLE HHAA
       (?$(3!5(B . "0x1214") ;; ETHIOPIC SYLLABLE HHEE
       (?$(3!6(B . "0x1215") ;; ETHIOPIC SYLLABLE HHE
       (?$(3!7(B . "0x1216") ;; ETHIOPIC SYLLABLE HHO
       (?$(3!8(B . "0x1217") ;; ETHIOPIC SYLLABLE HHWA
       (?$(3!9(B . "0x1218") ;; ETHIOPIC SYLLABLE MA
       (?$(3!:(B . "0x1219") ;; ETHIOPIC SYLLABLE MU
       (?$(3!;(B . "0x121A") ;; ETHIOPIC SYLLABLE MI
       (?$(3!<(B . "0x121B") ;; ETHIOPIC SYLLABLE MAA
       (?$(3!=(B . "0x121C") ;; ETHIOPIC SYLLABLE MEE
       (?$(3!>(B . "0x121D") ;; ETHIOPIC SYLLABLE ME
       (?$(3!?(B . "0x121E") ;; ETHIOPIC SYLLABLE MO
       (?$(3!@(B . "0x121F") ;; ETHIOPIC SYLLABLE MWA
       (?$(3!A(B . "0x1220") ;; ETHIOPIC SYLLABLE SZA
       (?$(3!B(B . "0x1221") ;; ETHIOPIC SYLLABLE SZU
       (?$(3!C(B . "0x1222") ;; ETHIOPIC SYLLABLE SZI
       (?$(3!D(B . "0x1223") ;; ETHIOPIC SYLLABLE SZAA
       (?$(3!E(B . "0x1224") ;; ETHIOPIC SYLLABLE SZEE
       (?$(3!F(B . "0x1225") ;; ETHIOPIC SYLLABLE SZE
       (?$(3!G(B . "0x1226") ;; ETHIOPIC SYLLABLE SZO
       (?$(3!H(B . "0x1227") ;; ETHIOPIC SYLLABLE SZWA
       (?$(3!I(B . "0x1228") ;; ETHIOPIC SYLLABLE RA
       (?$(3!J(B . "0x1229") ;; ETHIOPIC SYLLABLE RU
       (?$(3!K(B . "0x122A") ;; ETHIOPIC SYLLABLE RI
       (?$(3!L(B . "0x122B") ;; ETHIOPIC SYLLABLE RAA
       (?$(3!M(B . "0x122C") ;; ETHIOPIC SYLLABLE REE
       (?$(3!N(B . "0x122D") ;; ETHIOPIC SYLLABLE RE
       (?$(3!O(B . "0x122E") ;; ETHIOPIC SYLLABLE RO
       (?$(3!P(B . "0x122F") ;; ETHIOPIC SYLLABLE RWA
       (?$(3!Q(B . "0x1230") ;; ETHIOPIC SYLLABLE SA
       (?$(3!R(B . "0x1231") ;; ETHIOPIC SYLLABLE SU
       (?$(3!S(B . "0x1232") ;; ETHIOPIC SYLLABLE SI
       (?$(3!T(B . "0x1233") ;; ETHIOPIC SYLLABLE SAA
       (?$(3!U(B . "0x1234") ;; ETHIOPIC SYLLABLE SEE
       (?$(3!V(B . "0x1235") ;; ETHIOPIC SYLLABLE SE
       (?$(3!W(B . "0x1236") ;; ETHIOPIC SYLLABLE SO
       (?$(3!X(B . "0x1237") ;; ETHIOPIC SYLLABLE SWA
       (?$(3!Y(B . "0x1238") ;; ETHIOPIC SYLLABLE SHA
       (?$(3!Z(B . "0x1239") ;; ETHIOPIC SYLLABLE SHU
       (?$(3![(B . "0x123A") ;; ETHIOPIC SYLLABLE SHI
       (?$(3!\(B . "0x123B") ;; ETHIOPIC SYLLABLE SHAA
       (?$(3!](B . "0x123C") ;; ETHIOPIC SYLLABLE SHEE
       (?$(3!^(B . "0x123D") ;; ETHIOPIC SYLLABLE SHE
       (?$(3!_(B . "0x123E") ;; ETHIOPIC SYLLABLE SHO
       (?$(3!`(B . "0x123F") ;; ETHIOPIC SYLLABLE SHWA
       (?$(3!a(B . "0x1240") ;; ETHIOPIC SYLLABLE QA
       (?$(3!b(B . "0x1241") ;; ETHIOPIC SYLLABLE QU
       (?$(3!c(B . "0x1242") ;; ETHIOPIC SYLLABLE QI
       (?$(3!d(B . "0x1243") ;; ETHIOPIC SYLLABLE QAA
       (?$(3!e(B . "0x1244") ;; ETHIOPIC SYLLABLE QEE
       (?$(3!f(B . "0x1245") ;; ETHIOPIC SYLLABLE QE
       (?$(3!g(B . "0x1246") ;; ETHIOPIC SYLLABLE QO
       (?$(3!i(B . "0x1248") ;; ETHIOPIC SYLLABLE QWA
       (?$(3!k(B . "0x124A") ;; ETHIOPIC SYLLABLE QWI
       (?$(3!l(B . "0x124B") ;; ETHIOPIC SYLLABLE QWAA
       (?$(3!m(B . "0x124C") ;; ETHIOPIC SYLLABLE QWEE
       (?$(3!n(B . "0x124D") ;; ETHIOPIC SYLLABLE QWE
       (?$(3!q(B . "0x1250") ;; ETHIOPIC SYLLABLE QHA
       (?$(3!r(B . "0x1251") ;; ETHIOPIC SYLLABLE QHU
       (?$(3!s(B . "0x1252") ;; ETHIOPIC SYLLABLE QHI
       (?$(3!t(B . "0x1253") ;; ETHIOPIC SYLLABLE QHAA
       (?$(3!u(B . "0x1254") ;; ETHIOPIC SYLLABLE QHEE
       (?$(3!v(B . "0x1255") ;; ETHIOPIC SYLLABLE QHE
       (?$(3!w(B . "0x1256") ;; ETHIOPIC SYLLABLE QHO
       (?$(3!y(B . "0x1258") ;; ETHIOPIC SYLLABLE QHWA
       (?$(3!{(B . "0x125A") ;; ETHIOPIC SYLLABLE QHWI
       (?$(3!|(B . "0x125B") ;; ETHIOPIC SYLLABLE QHWAA
       (?$(3!}(B . "0x125C") ;; ETHIOPIC SYLLABLE QHWEE
       (?$(3!~(B . "0x125D") ;; ETHIOPIC SYLLABLE QHWE
       (?$(3"#(B . "0x1260") ;; ETHIOPIC SYLLABLE BA
       (?$(3"$(B . "0x1261") ;; ETHIOPIC SYLLABLE BU
       (?$(3"%(B . "0x1262") ;; ETHIOPIC SYLLABLE BI
       (?$(3"&(B . "0x1263") ;; ETHIOPIC SYLLABLE BAA
       (?$(3"'(B . "0x1264") ;; ETHIOPIC SYLLABLE BEE
       (?$(3"((B . "0x1265") ;; ETHIOPIC SYLLABLE BE
       (?$(3")(B . "0x1266") ;; ETHIOPIC SYLLABLE BO
       (?$(3"*(B . "0x1267") ;; ETHIOPIC SYLLABLE BWA
       (?$(3"+(B . "0x1268") ;; ETHIOPIC SYLLABLE VA
       (?$(3",(B . "0x1269") ;; ETHIOPIC SYLLABLE VU
       (?$(3"-(B . "0x126A") ;; ETHIOPIC SYLLABLE VI
       (?$(3".(B . "0x126B") ;; ETHIOPIC SYLLABLE VAA
       (?$(3"/(B . "0x126C") ;; ETHIOPIC SYLLABLE VEE
       (?$(3"0(B . "0x126D") ;; ETHIOPIC SYLLABLE VE
       (?$(3"1(B . "0x126E") ;; ETHIOPIC SYLLABLE VO
       (?$(3"2(B . "0x126F") ;; ETHIOPIC SYLLABLE VWA
       (?$(3"3(B . "0x1270") ;; ETHIOPIC SYLLABLE TA
       (?$(3"4(B . "0x1271") ;; ETHIOPIC SYLLABLE TU
       (?$(3"5(B . "0x1272") ;; ETHIOPIC SYLLABLE TI
       (?$(3"6(B . "0x1273") ;; ETHIOPIC SYLLABLE TAA
       (?$(3"7(B . "0x1274") ;; ETHIOPIC SYLLABLE TEE
       (?$(3"8(B . "0x1275") ;; ETHIOPIC SYLLABLE TE
       (?$(3"9(B . "0x1276") ;; ETHIOPIC SYLLABLE TO
       (?$(3":(B . "0x1277") ;; ETHIOPIC SYLLABLE TWA
       (?$(3";(B . "0x1278") ;; ETHIOPIC SYLLABLE CA
       (?$(3"<(B . "0x1279") ;; ETHIOPIC SYLLABLE CU
       (?$(3"=(B . "0x127A") ;; ETHIOPIC SYLLABLE CI
       (?$(3">(B . "0x127B") ;; ETHIOPIC SYLLABLE CAA
       (?$(3"?(B . "0x127C") ;; ETHIOPIC SYLLABLE CEE
       (?$(3"@(B . "0x127D") ;; ETHIOPIC SYLLABLE CE
       (?$(3"A(B . "0x127E") ;; ETHIOPIC SYLLABLE CO
       (?$(3"B(B . "0x127F") ;; ETHIOPIC SYLLABLE CWA
       (?$(3"C(B . "0x1280") ;; ETHIOPIC SYLLABLE XA
       (?$(3"D(B . "0x1281") ;; ETHIOPIC SYLLABLE XU
       (?$(3"E(B . "0x1282") ;; ETHIOPIC SYLLABLE XI
       (?$(3"F(B . "0x1283") ;; ETHIOPIC SYLLABLE XAA
       (?$(3"G(B . "0x1284") ;; ETHIOPIC SYLLABLE XEE
       (?$(3"H(B . "0x1285") ;; ETHIOPIC SYLLABLE XE
       (?$(3"I(B . "0x1286") ;; ETHIOPIC SYLLABLE XO
       (?$(3"K(B . "0x1288") ;; ETHIOPIC SYLLABLE XWA
       (?$(3"M(B . "0x128A") ;; ETHIOPIC SYLLABLE XWI
       (?$(3"N(B . "0x128B") ;; ETHIOPIC SYLLABLE XWAA
       (?$(3"O(B . "0x128C") ;; ETHIOPIC SYLLABLE XWEE
       (?$(3"P(B . "0x128D") ;; ETHIOPIC SYLLABLE XWE
       (?$(3"S(B . "0x1290") ;; ETHIOPIC SYLLABLE NA
       (?$(3"T(B . "0x1291") ;; ETHIOPIC SYLLABLE NU
       (?$(3"U(B . "0x1292") ;; ETHIOPIC SYLLABLE NI
       (?$(3"V(B . "0x1293") ;; ETHIOPIC SYLLABLE NAA
       (?$(3"W(B . "0x1294") ;; ETHIOPIC SYLLABLE NEE
       (?$(3"X(B . "0x1295") ;; ETHIOPIC SYLLABLE NE
       (?$(3"Y(B . "0x1296") ;; ETHIOPIC SYLLABLE NO
       (?$(3"Z(B . "0x1297") ;; ETHIOPIC SYLLABLE NWA
       (?$(3"[(B . "0x1298") ;; ETHIOPIC SYLLABLE NYA
       (?$(3"\(B . "0x1299") ;; ETHIOPIC SYLLABLE NYU
       (?$(3"](B . "0x129A") ;; ETHIOPIC SYLLABLE NYI
       (?$(3"^(B . "0x129B") ;; ETHIOPIC SYLLABLE NYAA
       (?$(3"_(B . "0x129C") ;; ETHIOPIC SYLLABLE NYEE
       (?$(3"`(B . "0x129D") ;; ETHIOPIC SYLLABLE NYE
       (?$(3"a(B . "0x129E") ;; ETHIOPIC SYLLABLE NYO
       (?$(3"b(B . "0x129F") ;; ETHIOPIC SYLLABLE NYWA
       (?$(3"c(B . "0x12A0") ;; ETHIOPIC SYLLABLE GLOTTAL A
       (?$(3"d(B . "0x12A1") ;; ETHIOPIC SYLLABLE GLOTTAL U
       (?$(3"e(B . "0x12A2") ;; ETHIOPIC SYLLABLE GLOTTAL I
       (?$(3"f(B . "0x12A3") ;; ETHIOPIC SYLLABLE GLOTTAL AA
       (?$(3"g(B . "0x12A4") ;; ETHIOPIC SYLLABLE GLOTTAL EE
       (?$(3"h(B . "0x12A5") ;; ETHIOPIC SYLLABLE GLOTTAL E
       (?$(3"i(B . "0x12A6") ;; ETHIOPIC SYLLABLE GLOTTAL O
       (?$(3"j(B . "0x12A7") ;; ETHIOPIC SYLLABLE GLOTTAL WA
       (?$(3"k(B . "0x12A8") ;; ETHIOPIC SYLLABLE KA
       (?$(3"l(B . "0x12A9") ;; ETHIOPIC SYLLABLE KU
       (?$(3"m(B . "0x12AA") ;; ETHIOPIC SYLLABLE KI
       (?$(3"n(B . "0x12AB") ;; ETHIOPIC SYLLABLE KAA
       (?$(3"o(B . "0x12AC") ;; ETHIOPIC SYLLABLE KEE
       (?$(3"p(B . "0x12AD") ;; ETHIOPIC SYLLABLE KE
       (?$(3"q(B . "0x12AE") ;; ETHIOPIC SYLLABLE KO
       (?$(3"s(B . "0x12B0") ;; ETHIOPIC SYLLABLE KWA
       (?$(3"u(B . "0x12B2") ;; ETHIOPIC SYLLABLE KWI
       (?$(3"v(B . "0x12B3") ;; ETHIOPIC SYLLABLE KWAA
       (?$(3"w(B . "0x12B4") ;; ETHIOPIC SYLLABLE KWEE
       (?$(3"x(B . "0x12B5") ;; ETHIOPIC SYLLABLE KWE
       (?$(3"{(B . "0x12B8") ;; ETHIOPIC SYLLABLE KXA
       (?$(3"|(B . "0x12B9") ;; ETHIOPIC SYLLABLE KXU
       (?$(3"}(B . "0x12BA") ;; ETHIOPIC SYLLABLE KXI
       (?$(3"~(B . "0x12BB") ;; ETHIOPIC SYLLABLE KXAA
       (?$(3#!(B . "0x12BC") ;; ETHIOPIC SYLLABLE KXEE
       (?$(3#"(B . "0x12BD") ;; ETHIOPIC SYLLABLE KXE
       (?$(3##(B . "0x12BE") ;; ETHIOPIC SYLLABLE KXO
       (?$(3#%(B . "0x12C0") ;; ETHIOPIC SYLLABLE KXWA
       (?$(3#'(B . "0x12C2") ;; ETHIOPIC SYLLABLE KXWI
       (?$(3#((B . "0x12C3") ;; ETHIOPIC SYLLABLE KXWAA
       (?$(3#)(B . "0x12C4") ;; ETHIOPIC SYLLABLE KXWEE
       (?$(3#*(B . "0x12C5") ;; ETHIOPIC SYLLABLE KXWE
       (?$(3#-(B . "0x12C8") ;; ETHIOPIC SYLLABLE WA
       (?$(3#.(B . "0x12C9") ;; ETHIOPIC SYLLABLE WU
       (?$(3#/(B . "0x12CA") ;; ETHIOPIC SYLLABLE WI
       (?$(3#0(B . "0x12CB") ;; ETHIOPIC SYLLABLE WAA
       (?$(3#1(B . "0x12CC") ;; ETHIOPIC SYLLABLE WEE
       (?$(3#2(B . "0x12CD") ;; ETHIOPIC SYLLABLE WE
       (?$(3#3(B . "0x12CE") ;; ETHIOPIC SYLLABLE WO
       (?$(3#5(B . "0x12D0") ;; ETHIOPIC SYLLABLE PHARYNGEAL A
       (?$(3#6(B . "0x12D1") ;; ETHIOPIC SYLLABLE PHARYNGEAL U
       (?$(3#7(B . "0x12D2") ;; ETHIOPIC SYLLABLE PHARYNGEAL I
       (?$(3#8(B . "0x12D3") ;; ETHIOPIC SYLLABLE PHARYNGEAL AA
       (?$(3#9(B . "0x12D4") ;; ETHIOPIC SYLLABLE PHARYNGEAL EE
       (?$(3#:(B . "0x12D5") ;; ETHIOPIC SYLLABLE PHARYNGEAL E
       (?$(3#;(B . "0x12D6") ;; ETHIOPIC SYLLABLE PHARYNGEAL O
       (?$(3#=(B . "0x12D8") ;; ETHIOPIC SYLLABLE ZA
       (?$(3#>(B . "0x12D9") ;; ETHIOPIC SYLLABLE ZU
       (?$(3#?(B . "0x12DA") ;; ETHIOPIC SYLLABLE ZI
       (?$(3#@(B . "0x12DB") ;; ETHIOPIC SYLLABLE ZAA
       (?$(3#A(B . "0x12DC") ;; ETHIOPIC SYLLABLE ZEE
       (?$(3#B(B . "0x12DD") ;; ETHIOPIC SYLLABLE ZE
       (?$(3#C(B . "0x12DE") ;; ETHIOPIC SYLLABLE ZO
       (?$(3#D(B . "0x12DF") ;; ETHIOPIC SYLLABLE ZWA
       (?$(3#E(B . "0x12E0") ;; ETHIOPIC SYLLABLE ZHA
       (?$(3#F(B . "0x12E1") ;; ETHIOPIC SYLLABLE ZHU
       (?$(3#G(B . "0x12E2") ;; ETHIOPIC SYLLABLE ZHI
       (?$(3#H(B . "0x12E3") ;; ETHIOPIC SYLLABLE ZHAA
       (?$(3#I(B . "0x12E4") ;; ETHIOPIC SYLLABLE ZHEE
       (?$(3#J(B . "0x12E5") ;; ETHIOPIC SYLLABLE ZHE
       (?$(3#K(B . "0x12E6") ;; ETHIOPIC SYLLABLE ZHO
       (?$(3#L(B . "0x12E7") ;; ETHIOPIC SYLLABLE ZHWA
       (?$(3#M(B . "0x12E8") ;; ETHIOPIC SYLLABLE YA
       (?$(3#N(B . "0x12E9") ;; ETHIOPIC SYLLABLE YU
       (?$(3#O(B . "0x12EA") ;; ETHIOPIC SYLLABLE YI
       (?$(3#P(B . "0x12EB") ;; ETHIOPIC SYLLABLE YAA
       (?$(3#Q(B . "0x12EC") ;; ETHIOPIC SYLLABLE YEE
       (?$(3#R(B . "0x12ED") ;; ETHIOPIC SYLLABLE YE
       (?$(3#S(B . "0x12EE") ;; ETHIOPIC SYLLABLE YO
       (?$(3#U(B . "0x12F0") ;; ETHIOPIC SYLLABLE DA
       (?$(3#V(B . "0x12F1") ;; ETHIOPIC SYLLABLE DU
       (?$(3#W(B . "0x12F2") ;; ETHIOPIC SYLLABLE DI
       (?$(3#X(B . "0x12F3") ;; ETHIOPIC SYLLABLE DAA
       (?$(3#Y(B . "0x12F4") ;; ETHIOPIC SYLLABLE DEE
       (?$(3#Z(B . "0x12F5") ;; ETHIOPIC SYLLABLE DE
       (?$(3#[(B . "0x12F6") ;; ETHIOPIC SYLLABLE DO
       (?$(3#\(B . "0x12F7") ;; ETHIOPIC SYLLABLE DWA
       (?$(3#](B . "0x12F8") ;; ETHIOPIC SYLLABLE DDA
       (?$(3#^(B . "0x12F9") ;; ETHIOPIC SYLLABLE DDU
       (?$(3#_(B . "0x12FA") ;; ETHIOPIC SYLLABLE DDI
       (?$(3#`(B . "0x12FB") ;; ETHIOPIC SYLLABLE DDAA
       (?$(3#a(B . "0x12FC") ;; ETHIOPIC SYLLABLE DDEE
       (?$(3#b(B . "0x12FD") ;; ETHIOPIC SYLLABLE DDE
       (?$(3#c(B . "0x12FE") ;; ETHIOPIC SYLLABLE DDO
       (?$(3#d(B . "0x12FF") ;; ETHIOPIC SYLLABLE DDWA
       (?$(3#e(B . "0x1300") ;; ETHIOPIC SYLLABLE JA
       (?$(3#f(B . "0x1301") ;; ETHIOPIC SYLLABLE JU
       (?$(3#g(B . "0x1302") ;; ETHIOPIC SYLLABLE JI
       (?$(3#h(B . "0x1303") ;; ETHIOPIC SYLLABLE JAA
       (?$(3#i(B . "0x1304") ;; ETHIOPIC SYLLABLE JEE
       (?$(3#j(B . "0x1305") ;; ETHIOPIC SYLLABLE JE
       (?$(3#k(B . "0x1306") ;; ETHIOPIC SYLLABLE JO
       (?$(3#l(B . "0x1307") ;; ETHIOPIC SYLLABLE JWA
       (?$(3#m(B . "0x1308") ;; ETHIOPIC SYLLABLE GA
       (?$(3#n(B . "0x1309") ;; ETHIOPIC SYLLABLE GU
       (?$(3#o(B . "0x130A") ;; ETHIOPIC SYLLABLE GI
       (?$(3#p(B . "0x130B") ;; ETHIOPIC SYLLABLE GAA
       (?$(3#q(B . "0x130C") ;; ETHIOPIC SYLLABLE GEE
       (?$(3#r(B . "0x130D") ;; ETHIOPIC SYLLABLE GE
       (?$(3#s(B . "0x130E") ;; ETHIOPIC SYLLABLE GO
       (?$(3#u(B . "0x1310") ;; ETHIOPIC SYLLABLE GWA
       (?$(3#w(B . "0x1312") ;; ETHIOPIC SYLLABLE GWI
       (?$(3#x(B . "0x1313") ;; ETHIOPIC SYLLABLE GWAA
       (?$(3#y(B . "0x1314") ;; ETHIOPIC SYLLABLE GWEE
       (?$(3#z(B . "0x1315") ;; ETHIOPIC SYLLABLE GWE
       (?$(3#}(B . "0x1318") ;; ETHIOPIC SYLLABLE GGA
       (?$(3#~(B . "0x1319") ;; ETHIOPIC SYLLABLE GGU
       (?$(3$!(B . "0x131A") ;; ETHIOPIC SYLLABLE GGI
       (?$(3$"(B . "0x131B") ;; ETHIOPIC SYLLABLE GGAA
       (?$(3$#(B . "0x131C") ;; ETHIOPIC SYLLABLE GGEE
       (?$(3$$(B . "0x131D") ;; ETHIOPIC SYLLABLE GGE
       (?$(3$%(B . "0x131E") ;; ETHIOPIC SYLLABLE GGO
       (?$(3$'(B . "0x1320") ;; ETHIOPIC SYLLABLE THA
       (?$(3$((B . "0x1321") ;; ETHIOPIC SYLLABLE THU
       (?$(3$)(B . "0x1322") ;; ETHIOPIC SYLLABLE THI
       (?$(3$*(B . "0x1323") ;; ETHIOPIC SYLLABLE THAA
       (?$(3$+(B . "0x1324") ;; ETHIOPIC SYLLABLE THEE
       (?$(3$,(B . "0x1325") ;; ETHIOPIC SYLLABLE THE
       (?$(3$-(B . "0x1326") ;; ETHIOPIC SYLLABLE THO
       (?$(3$.(B . "0x1327") ;; ETHIOPIC SYLLABLE THWA
       (?$(3$/(B . "0x1328") ;; ETHIOPIC SYLLABLE CHA
       (?$(3$0(B . "0x1329") ;; ETHIOPIC SYLLABLE CHU
       (?$(3$1(B . "0x132A") ;; ETHIOPIC SYLLABLE CHI
       (?$(3$2(B . "0x132B") ;; ETHIOPIC SYLLABLE CHAA
       (?$(3$3(B . "0x132C") ;; ETHIOPIC SYLLABLE CHEE
       (?$(3$4(B . "0x132D") ;; ETHIOPIC SYLLABLE CHE
       (?$(3$5(B . "0x132E") ;; ETHIOPIC SYLLABLE CHO
       (?$(3$6(B . "0x132F") ;; ETHIOPIC SYLLABLE CHWA
       (?$(3$7(B . "0x1330") ;; ETHIOPIC SYLLABLE PHA
       (?$(3$8(B . "0x1331") ;; ETHIOPIC SYLLABLE PHU
       (?$(3$9(B . "0x1332") ;; ETHIOPIC SYLLABLE PHI
       (?$(3$:(B . "0x1333") ;; ETHIOPIC SYLLABLE PHAA
       (?$(3$;(B . "0x1334") ;; ETHIOPIC SYLLABLE PHEE
       (?$(3$<(B . "0x1335") ;; ETHIOPIC SYLLABLE PHE
       (?$(3$=(B . "0x1336") ;; ETHIOPIC SYLLABLE PHO
       (?$(3$>(B . "0x1337") ;; ETHIOPIC SYLLABLE PHWA
       (?$(3$?(B . "0x1338") ;; ETHIOPIC SYLLABLE TSA
       (?$(3$@(B . "0x1339") ;; ETHIOPIC SYLLABLE TSU
       (?$(3$A(B . "0x133A") ;; ETHIOPIC SYLLABLE TSI
       (?$(3$B(B . "0x133B") ;; ETHIOPIC SYLLABLE TSAA
       (?$(3$C(B . "0x133C") ;; ETHIOPIC SYLLABLE TSEE
       (?$(3$D(B . "0x133D") ;; ETHIOPIC SYLLABLE TSE
       (?$(3$E(B . "0x133E") ;; ETHIOPIC SYLLABLE TSO
       (?$(3$F(B . "0x133F") ;; ETHIOPIC SYLLABLE TSWA
       (?$(3$G(B . "0x1340") ;; ETHIOPIC SYLLABLE TZA
       (?$(3$H(B . "0x1341") ;; ETHIOPIC SYLLABLE TZU
       (?$(3$I(B . "0x1342") ;; ETHIOPIC SYLLABLE TZI
       (?$(3$J(B . "0x1343") ;; ETHIOPIC SYLLABLE TZAA
       (?$(3$K(B . "0x1344") ;; ETHIOPIC SYLLABLE TZEE
       (?$(3$L(B . "0x1345") ;; ETHIOPIC SYLLABLE TZE
       (?$(3$M(B . "0x1346") ;; ETHIOPIC SYLLABLE TZO
       (?$(3$O(B . "0x1348") ;; ETHIOPIC SYLLABLE FA
       (?$(3$P(B . "0x1349") ;; ETHIOPIC SYLLABLE FU
       (?$(3$Q(B . "0x134A") ;; ETHIOPIC SYLLABLE FI
       (?$(3$R(B . "0x134B") ;; ETHIOPIC SYLLABLE FAA
       (?$(3$S(B . "0x134C") ;; ETHIOPIC SYLLABLE FEE
       (?$(3$T(B . "0x134D") ;; ETHIOPIC SYLLABLE FE
       (?$(3$U(B . "0x134E") ;; ETHIOPIC SYLLABLE FO
       (?$(3$V(B . "0x134F") ;; ETHIOPIC SYLLABLE FWA
       (?$(3$W(B . "0x1350") ;; ETHIOPIC SYLLABLE PA
       (?$(3$X(B . "0x1351") ;; ETHIOPIC SYLLABLE PU
       (?$(3$Y(B . "0x1352") ;; ETHIOPIC SYLLABLE PI
       (?$(3$Z(B . "0x1353") ;; ETHIOPIC SYLLABLE PAA
       (?$(3$[(B . "0x1354") ;; ETHIOPIC SYLLABLE PEE
       (?$(3$\(B . "0x1355") ;; ETHIOPIC SYLLABLE PE
       (?$(3$](B . "0x1356") ;; ETHIOPIC SYLLABLE PO
       (?$(3$^(B . "0x1357") ;; ETHIOPIC SYLLABLE PWA
       (?$(3$_(B . "0x1358") ;; ETHIOPIC SYLLABLE RYA
       (?$(3$`(B . "0x1359") ;; ETHIOPIC SYLLABLE MYA
       (?$(3$a(B . "0x135A") ;; ETHIOPIC SYLLABLE FYA
       (?$(3$h(B . "0x1361") ;; ETHIOPIC WORDSPACE
       (?$(3$i(B . "0x1362") ;; ETHIOPIC FULL STOP
       (?$(3$j(B . "0x1363") ;; ETHIOPIC COMMA
       (?$(3$k(B . "0x1364") ;; ETHIOPIC SEMICOLON
       (?$(3$l(B . "0x1365") ;; ETHIOPIC COLON
       (?$(3$m(B . "0x1366") ;; ETHIOPIC PREFACE COLON
       (?$(3$n(B . "0x1367") ;; ETHIOPIC QUESTION MARK
       (?$(3$o(B . "0x1368") ;; ETHIOPIC PARAGRAPH SEPARATOR
       (?$(3$p(B . "0x1369") ;; ETHIOPIC DIGIT ONE
       (?$(3$q(B . "0x136A") ;; ETHIOPIC DIGIT TWO
       (?$(3$r(B . "0x136B") ;; ETHIOPIC DIGIT THREE
       (?$(3$s(B . "0x136C") ;; ETHIOPIC DIGIT FOUR
       (?$(3$t(B . "0x136D") ;; ETHIOPIC DIGIT FIVE
       (?$(3$u(B . "0x136E") ;; ETHIOPIC DIGIT SIX
       (?$(3$v(B . "0x136F") ;; ETHIOPIC DIGIT SEVEN
       (?$(3$w(B . "0x1370") ;; ETHIOPIC DIGIT EIGHT
       (?$(3$x(B . "0x1371") ;; ETHIOPIC DIGIT NINE
       (?$(3$y(B . "0x1372") ;; ETHIOPIC NUMBER TEN
       (?$(3$z(B . "0x1373") ;; ETHIOPIC NUMBER TWENTY
       (?$(3${(B . "0x1374") ;; ETHIOPIC NUMBER THIRTY
       (?$(3$|(B . "0x1375") ;; ETHIOPIC NUMBER FORTY
       (?$(3$}(B . "0x1376") ;; ETHIOPIC NUMBER FIFTY
       (?$(3$~(B . "0x1377") ;; ETHIOPIC NUMBER SIXTY
       (?$(3%!(B . "0x1378") ;; ETHIOPIC NUMBER SEVENTY
       (?$(3%"(B . "0x1379") ;; ETHIOPIC NUMBER EIGHTY
       (?$(3%#(B . "0x137A") ;; ETHIOPIC NUMBER NINETY
       (?$(3%$(B . "0x137B") ;; ETHIOPIC NUMBER HUNDRED
       (?$(3%%(B . "0x137C") ;; ETHIOPIC NUMBER TEN THOUSAND
       ))))

(provide 'uethiopic)

; -*- coding: iso-2022-7bit  -*-
;;; u-cns-1.el --- tables between UCS and CNS 11643:1992 plain 1

;; Copyright (c) 1991-1994 Unicode, Inc.
;; Copyright (C) 1998 Koichi Yasuoka.
;; Copyright (C) 1998 MORIOKA Tomohiko.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>

;; Keywords: CCL, mule, multilingual, character set, coding-system,
;;                ISO/IEC 10646, Unicode 2.0, CNS 11643:1992

;; This file is part of Mule-UCS.

;;; Commentary:

;; This program was originally based on "Uni2CNS.Z" by Koichi Yasuoka
;; <yasuoka@kudpc.kyoto-u.ac.jp>, and checked, corrected and
;; translated to Emacs Lisp by MORIOKA Tomohiko.

;; "Uni2CNS.Z" was originally based on "CJKXRef.text" by John
;; H. Jenkins <John_Jenkins@taligent.com>.  Here the author expresses
;; his appreciation to Ken Lunde <lunde@mv.us.adobe.com>, Christian
;; Wittern <cwittern@conline.central.de>, and Jack Halpern
;; <jhalpern@super.win.or.jp>.

;;; Code:

(put 'chinese-cns11643-1 'unicode-assoc
     'cns11643-1-vs-unicode-assoc)

(defvar cns11643-1-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?$(G!!(B . "0x0020") ; SPACE
       (?$(G!"(B . "0x002C") ; COMMA
       (?$(G!#(B . "0x3001") ; IDEOGRAPHIC COMMA
       (?$(G!$(B . "0x3002") ; IDEOGRAPHIC FULL STOP
       (?$(G!%(B . "0x002E") ; FULL STOP
       (?$(G!&(B . "0x00B7") ; MIDDLE DOT
       (?$(G!'(B . "0x003B") ; SEMICOLON
       (?$(G!((B . "0x003A") ; COLON
       (?$(G!)(B . "0x003F") ; QUESTION MARK
       (?$(G!*(B . "0x0021") ; EXCLAMATION MARK
       (?$(G!+(B . "0xFE30") ; PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
       (?$(G!,(B . "0x2026") ; HORIZONTAL ELLIPSIS
       (?$(G!-(B . "0x2025") ; TWO DOT LEADER
       (?$(G!.(B . "0xFE50") ; SMALL COMMA
       (?$(G!/(B . "0xFE51") ; SMALL IDEOGRAPHIC COMMA
       (?$(G!0(B . "0xFE52") ; SMALL FULL STOP
       (?$(G!1(B . "0x2027") ; HYPHENATION POINT
       (?$(G!2(B . "0xFE54") ; SMALL SEMICOLON
       (?$(G!3(B . "0xFE55") ; SMALL COLON
       (?$(G!4(B . "0xFE56") ; SMALL QUESTION MARK
       (?$(G!5(B . "0xFE57") ; SMALL EXCLAMATION MARK
       (?$(G!6(B . "0xFE31") ; PRESENTATION FORM FOR VERTICAL EM DASH
       (?$(G!7(B . "0x2014") ; EM DASH
       (?$(G!8(B . "0xFE32") ; PRESENTATION FORM FOR VERTICAL EN DASH
       (?$(G!9(B . "0x2013") ; EN DASH
       (?$(G!=(B . "0xFE4B") ; WAVY OVERLINE
       (?$(G!>(B . "0x0028") ; LEFT PARENTHESIS
       (?$(G!?(B . "0x0029") ; RIGHT PARENTHESIS
       (?$(G!@(B . "0xFE35") ; PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
       (?$(G!A(B . "0xFE36") ; PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
       (?$(G!B(B . "0x007B") ; LEFT CURLY BRACKET
       (?$(G!C(B . "0x007D") ; RIGHT CURLY BRACKET
       (?$(G!D(B . "0xFE37") ; PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
       (?$(G!E(B . "0xFE38") ; PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
       (?$(G!F(B . "0x3014") ; LEFT TORTOISE SHELL BRACKET
       (?$(G!G(B . "0x3015") ; RIGHT TORTOISE SHELL BRACKET
       (?$(G!H(B . "0xFE39") ; PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
       (?$(G!I(B . "0xFE3A") ; PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
       (?$(G!J(B . "0x3010") ; LEFT BLACK LENTICULAR BRACKET
       (?$(G!K(B . "0x3011") ; RIGHT BLACK LENTICULAR BRACKET
       (?$(G!L(B . "0xFE3B") ; PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
       (?$(G!M(B . "0xFE3C") ; PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
       (?$(G!N(B . "0x300A") ; LEFT DOUBLE ANGLE BRACKET
       (?$(G!O(B . "0x300B") ; RIGHT DOUBLE ANGLE BRACKET
       (?$(G!P(B . "0xFE3D") ; PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
       (?$(G!Q(B . "0xFE3E") ; PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
       (?$(G!R(B . "0x3008") ; LEFT ANGLE BRACKET
       (?$(G!S(B . "0x3009") ; RIGHT ANGLE BRACKET
       (?$(G!T(B . "0xFE3F") ; PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
       (?$(G!U(B . "0xFE40") ; PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
       (?$(G!V(B . "0x300C") ; LEFT CORNER BRACKET
       (?$(G!W(B . "0x300D") ; RIGHT CORNER BRACKET
       (?$(G!X(B . "0xFE41") ; PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
       (?$(G!Y(B . "0xFE42") ; PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
       (?$(G!Z(B . "0x300E") ; LEFT WHITE CORNER BRACKET
       (?$(G![(B . "0x300F") ; RIGHT WHITE CORNER BRACKET
       (?$(G!\(B . "0xFE43") ; PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
       (?$(G!](B . "0xFE44") ; PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
       (?$(G!^(B . "0xFE59") ; SMALL LEFT PARENTHESIS
       (?$(G!_(B . "0xFE5A") ; SMALL RIGHT PARENTHESIS
       (?$(G!`(B . "0xFE5B") ; SMALL LEFT CURLY BRACKET
       (?$(G!a(B . "0xFE5C") ; SMALL RIGHT CURLY BRACKET
       (?$(G!b(B . "0xFE5D") ; SMALL LEFT TORTOISE SHELL BRACKET
       (?$(G!c(B . "0xFE5E") ; SMALL RIGHT TORTOISE SHELL BRACKET
       (?$(G!d(B . "0x2018") ; LEFT SINGLE QUOTATION MARK
       (?$(G!e(B . "0x2019") ; RIGHT SINGLE QUOTATION MARK
       (?$(G!f(B . "0x201C") ; LEFT DOUBLE QUOTATION MARK
       (?$(G!g(B . "0x201D") ; RIGHT DOUBLE QUOTATION MARK
       (?$(G!h(B . "0x301D") ; REVERSED DOUBLE PRIME QUOTATION MARK
       (?$(G!i(B . "0x301E") ; DOUBLE PRIME QUOTATION MARK
       (?$(G!j(B . "0x2035") ; REVERSED PRIME
       (?$(G!k(B . "0x2032") ; PRIME
       (?$(G!l(B . "0x0023") ; NUMBER SIGN
       (?$(G!m(B . "0x0026") ; AMPERSAND
       (?$(G!n(B . "0x2733") ; EIGHT SPOKED ASTERISK
       (?$(G!o(B . "0x203B") ; REFERENCE MARK
       (?$(G!p(B . "0x00A7") ; SECTION SIGN
       (?$(G!q(B . "0x3003") ; DITTO MARK
       (?$(G!r(B . "0x25CB") ; WHITE CIRCLE
       (?$(G!s(B . "0x25CF") ; BLACK CIRCLE
       (?$(G!t(B . "0x25B3") ; WHITE UP-POINTING TRIANGLE
       (?$(G!u(B . "0x25B2") ; BLACK UP-POINTING TRIANGLE
       (?$(G!v(B . "0x25CE") ; BULLSEYE
       (?$(G!w(B . "0x2606") ; WHITE STAR
       (?$(G!x(B . "0x2605") ; BLACK STAR
       (?$(G!y(B . "0x25C7") ; WHITE DIAMOND
       (?$(G!z(B . "0x25C6") ; BLACK DIAMOND
       (?$(G!{(B . "0x25A1") ; WHITE SQUARE
       (?$(G!|(B . "0x25A0") ; BLACK SQUARE
       (?$(G!}(B . "0x25BD") ; WHITE DOWN-POINTING TRIANGLE
       (?$(G!~(B . "0x25BC") ; BLACK DOWN-POINTING TRIANGLE
       (?$(G"!(B . "0x32A3") ; CIRCLED IDEOGRAPH CORRECT
       (?$(G""(B . "0x2105") ; CARE OF
       (?$(G"#(B . "0x203E") ; OVERLINE
       (?$(G"$(B . "0x203E") ; [OVERLINE] (heavy overline; spacing heavy overscore)
       (?$(G"%(B . "0x005F") ; LOW LINE
       (?$(G"&(B . "0x005F") ; [LOW LINE] (heavy low line; spacing heavy underscore)
       (?$(G"'(B . "0xFE49") ; DASHED OVERLINE
       (?$(G"((B . "0xFE4A") ; CENTRELINE OVERLINE
       (?$(G")(B . "0xFE4D") ; DASHED LOW LINE
       (?$(G"*(B . "0xFE4E") ; CENTRELINE LOW LINE
       (?$(G"+(B . "0xFE4B") ; WAVY OVERLINE
       (?$(G",(B . "0xFE4C") ; DOUBLE WAVY OVERLINE
       (?$(G"-(B . "0xFE5F") ; SMALL NUMBER SIGN
       (?$(G".(B . "0xFE60") ; SMALL AMPERSAND
       (?$(G"/(B . "0xFE61") ; SMALL ASTERISK
       (?$(G"0(B . "0x002B") ; PLUS SIGN
       (?$(G"1(B . "0x002D") ; HYPHEN-MINUS
       (?$(G"2(B . "0x00D7") ; MULTIPLICATION SIGN
       (?$(G"3(B . "0x00F7") ; DIVISION SIGN
       (?$(G"4(B . "0x00B1") ; PLUS-MINUS SIGN
       (?$(G"5(B . "0x221A") ; SQUARE ROOT
       (?$(G"6(B . "0x003C") ; LESS-THAN SIGN
       (?$(G"7(B . "0x003E") ; GREATER-THAN SIGN
       (?$(G"8(B . "0x003D") ; EQUALS SIGN
       (?$(G"9(B . "0x2266") ; LESS-THAN OVER EQUAL TO
       (?$(G":(B . "0x2267") ; GREATER-THAN OVER EQUAL TO
       (?$(G";(B . "0x2260") ; NOT EQUAL TO
       (?$(G"<(B . "0x221E") ; INFINITY
       (?$(G"=(B . "0x2252") ; APPROXIMATELY EQUAL TO OR THE IMAGE OF
       (?$(G">(B . "0x2261") ; IDENTICAL TO
       (?$(G"?(B . "0xFE62") ; SMALL PLUS SIGN
       (?$(G"@(B . "0xFE63") ; SMALL HYPHEN-MINUS
       (?$(G"A(B . "0xFE64") ; SMALL LESS-THAN SIGN
       (?$(G"B(B . "0xFE65") ; SMALL GREATER-THAN SIGN
       (?$(G"C(B . "0xFE66") ; SMALL EQUALS SIGN
       (?$(G"D(B . "0x301C") ; WAVE DASH
       (?$(G"E(B . "0x2229") ; INTERSECTION
       (?$(G"F(B . "0x222A") ; UNION
       (?$(G"G(B . "0x22A5") ; UP TACK
       (?$(G"H(B . "0x2220") ; ANGLE
       (?$(G"I(B . "0x221F") ; RIGHT ANGLE
       (?$(G"J(B . "0x22BF") ; RIGHT TRIANGLE
       (?$(G"K(B . "0x33D2") ; SQUARE LOG
       (?$(G"L(B . "0x33D1") ; SQUARE LN
       (?$(G"M(B . "0x222B") ; INTEGRAL
       (?$(G"N(B . "0x222E") ; CONTOUR INTEGRAL
       (?$(G"O(B . "0x2235") ; BECAUSE
       (?$(G"P(B . "0x2234") ; THEREFORE
       (?$(G"Q(B . "0x2640") ; FEMALE SIGN
       (?$(G"R(B . "0x2642") ; MALE SIGN
       (?$(G"S(B . "0x2295") ; CIRCLED PLUS
       (?$(G"T(B . "0x2299") ; CIRCLED DOT OPERATOR
       (?$(G"U(B . "0x2191") ; UPWARDS ARROW
       (?$(G"V(B . "0x2193") ; DOWNWARDS ARROW
       (?$(G"W(B . "0x2192") ; RIGHTWARDS ARROW
       (?$(G"X(B . "0x2190") ; LEFTWARDS ARROW
       (?$(G"Y(B . "0x2196") ; NORTH WEST ARROW
       (?$(G"Z(B . "0x2197") ; NORTH EAST ARROW
       (?$(G"[(B . "0x2199") ; SOUTH WEST ARROW
       (?$(G"\(B . "0x2198") ; SOUTH EAST ARROW
       (?$(G"](B . "0x2016") ; DOUBLE VERTICAL LINE
       (?$(G"^(B . "0x007C") ; VERTICAL LINE
       (?$(G"_(B . "0x2044") ; FRACTION SLASH
       (?$(G"`(B . "0x005C") ; REVERSE SOLIDUS
       (?$(G"a(B . "0x002F") ; SOLIDUS
       (?$(G"b(B . "0xFE68") ; SMALL REVERSE SOLIDUS
       (?$(G"c(B . "0x0024") ; DOLLAR SIGN
       (?$(G"d(B . "0x00A5") ; YEN SIGN
       (?$(G"e(B . "0x3012") ; POSTAL MARK
       (?$(G"f(B . "0x00A2") ; CENT SIGN
       (?$(G"g(B . "0x00A3") ; POUND SIGN
       (?$(G"h(B . "0x0025") ; PERCENT SIGN
       (?$(G"i(B . "0x0040") ; COMMERCIAL AT
       (?$(G"j(B . "0x2103") ; DEGREE CELSIUS
       (?$(G"k(B . "0x2109") ; DEGREE FAHRENHEIT
       (?$(G"l(B . "0xFE69") ; SMALL DOLLAR SIGN
       (?$(G"m(B . "0xFE6A") ; SMALL PERCENT SIGN
       (?$(G"n(B . "0xFE6B") ; SMALL COMMERCIAL AT
       (?$(G"o(B . "0x33D5") ; SQUARE MIL
       (?$(G"p(B . "0x339C") ; SQUARE MM
       (?$(G"q(B . "0x339D") ; SQUARE CM
       (?$(G"r(B . "0x339E") ; SQUARE KM
       (?$(G"s(B . "0x33CE") ; SQUARE KM CAPITAL
       (?$(G"t(B . "0x33A1") ; SQUARE M SQUARED
       (?$(G"u(B . "0x338E") ; SQUARE MG
       (?$(G"v(B . "0x338F") ; SQUARE KG
       (?$(G"w(B . "0x33C4") ; SQUARE CC
       (?$(G"x(B . "0x00B0") ; DEGREE SIGN
       (?$(G"y(B . "0x5159") ; <CJK>
       (?$(G"z(B . "0x515B") ; <CJK>
       (?$(G"{(B . "0x515E") ; <CJK>
       (?$(G"|(B . "0x515D") ; <CJK>
       (?$(G"}(B . "0x5161") ; <CJK>
       (?$(G"~(B . "0x5163") ; <CJK>
       (?$(G#!(B . "0x55E7") ; <CJK>
       (?$(G#"(B . "0x74E9") ; <CJK>
       (?$(G##(B . "0x7CCE") ; <CJK>
       (?$(G#$(B . "0x2581") ; LOWER ONE EIGHTH BLOCK
       (?$(G#%(B . "0x2582") ; LOWER ONE QUARTER BLOCK
       (?$(G#&(B . "0x2583") ; LOWER THREE EIGHTHS BLOCK
       (?$(G#'(B . "0x2584") ; LOWER HALF BLOCK
       (?$(G#((B . "0x2585") ; LOWER FIVE EIGHTHS BLOCK
       (?$(G#)(B . "0x2586") ; LOWER THREE QUARTERS BLOCK
       (?$(G#*(B . "0x2587") ; LOWER SEVEN EIGHTHS BLOCK
       (?$(G#+(B . "0x2588") ; FULL BLOCK
       (?$(G#,(B . "0x258F") ; LEFT ONE EIGHTH BLOCK
       (?$(G#-(B . "0x258E") ; LEFT ONE QUARTER BLOCK
       (?$(G#.(B . "0x258D") ; LEFT THREE EIGHTHS BLOCK
       (?$(G#/(B . "0x258C") ; LEFT HALF BLOCK
       (?$(G#0(B . "0x258B") ; LEFT FIVE EIGHTHS BLOCK
       (?$(G#1(B . "0x258A") ; LEFT THREE QUARTERS BLOCK
       (?$(G#2(B . "0x2589") ; LEFT SEVEN EIGHTHS BLOCK
       (?$(G#3(B . "0x253C") ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
       (?$(G#4(B . "0x2534") ; BOX DRAWINGS LIGHT UP AND HORIZONTAL
       (?$(G#5(B . "0x252C") ; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
       (?$(G#6(B . "0x2524") ; BOX DRAWINGS LIGHT VERTICAL AND LEFT
       (?$(G#7(B . "0x251C") ; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
       (?$(G#8(B . "0x2594") ; UPPER ONE EIGHTH BLOCK
       (?$(G#9(B . "0x2500") ; BOX DRAWINGS LIGHT HORIZONTAL
       (?$(G#:(B . "0x2502") ; BOX DRAWINGS LIGHT VERTICAL
       (?$(G#;(B . "0x2595") ; RIGHT ONE EIGHTH BLOCK
       (?$(G#<(B . "0x250C") ; BOX DRAWINGS LIGHT DOWN AND RIGHT
       (?$(G#=(B . "0x2510") ; BOX DRAWINGS LIGHT DOWN AND LEFT
       (?$(G#>(B . "0x2514") ; BOX DRAWINGS LIGHT UP AND RIGHT
       (?$(G#?(B . "0x2518") ; BOX DRAWINGS LIGHT UP AND LEFT
       (?$(G#@(B . "0x256D") ; BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
       (?$(G#A(B . "0x256E") ; BOX DRAWINGS LIGHT ARC DOWN AND LEFT
       (?$(G#B(B . "0x2570") ; BOX DRAWINGS LIGHT ARC UP AND RIGHT
       (?$(G#C(B . "0x256F") ; BOX DRAWINGS LIGHT ARC UP AND LEFT
       (?$(G#D(B . "0x2550") ; BOX DRAWINGS DOUBLE HORIZONTAL
       (?$(G#E(B . "0x255E") ; BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
       (?$(G#F(B . "0x256A") ; BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
       (?$(G#G(B . "0x2561") ; BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
       (?$(G#H(B . "0x25E2") ; BLACK LOWER RIGHT TRIANGLE
       (?$(G#I(B . "0x25E3") ; BLACK LOWER LEFT TRIANGLE
       (?$(G#J(B . "0x25E5") ; BLACK UPPER RIGHT TRIANGLE
       (?$(G#K(B . "0x25E4") ; BLACK UPPER LEFT TRIANGLE
       (?$(G#L(B . "0x2571") ; BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
       (?$(G#M(B . "0x2572") ; BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
       (?$(G#N(B . "0x2573") ; BOX DRAWINGS LIGHT DIAGONAL CROSS
       (?$(G$!(B . "0x0030") ; DIGIT ZERO
       (?$(G$"(B . "0x0031") ; DIGIT ONE
       (?$(G$#(B . "0x0032") ; DIGIT TWO
       (?$(G$$(B . "0x0033") ; DIGIT THREE
       (?$(G$%(B . "0x0034") ; DIGIT FOUR
       (?$(G$&(B . "0x0035") ; DIGIT FIVE
       (?$(G$'(B . "0x0036") ; DIGIT SIX
       (?$(G$((B . "0x0037") ; DIGIT SEVEN
       (?$(G$)(B . "0x0038") ; DIGIT EIGHT
       (?$(G$*(B . "0x0039") ; DIGIT NINE
       (?$(G$+(B . "0x2160") ; ROMAN NUMERAL ONE
       (?$(G$,(B . "0x2161") ; ROMAN NUMERAL TWO
       (?$(G$-(B . "0x2162") ; ROMAN NUMERAL THREE
       (?$(G$.(B . "0x2163") ; ROMAN NUMERAL FOUR
       (?$(G$/(B . "0x2164") ; ROMAN NUMERAL FIVE
       (?$(G$0(B . "0x2165") ; ROMAN NUMERAL SIX
       (?$(G$1(B . "0x2166") ; ROMAN NUMERAL SEVEN
       (?$(G$2(B . "0x2167") ; ROMAN NUMERAL EIGHT
       (?$(G$3(B . "0x2168") ; ROMAN NUMERAL NINE
       (?$(G$4(B . "0x2169") ; ROMAN NUMERAL TEN
       (?$(G$5(B . "0x3021") ; HANGZHOU NUMERAL ONE
       (?$(G$6(B . "0x3022") ; HANGZHOU NUMERAL TWO
       (?$(G$7(B . "0x3023") ; HANGZHOU NUMERAL THREE
       (?$(G$8(B . "0x3024") ; HANGZHOU NUMERAL FOUR
       (?$(G$9(B . "0x3025") ; HANGZHOU NUMERAL FIVE
       (?$(G$:(B . "0x3026") ; HANGZHOU NUMERAL SIX
       (?$(G$;(B . "0x3027") ; HANGZHOU NUMERAL SEVEN
       (?$(G$<(B . "0x3028") ; HANGZHOU NUMERAL EIGHT
       (?$(G$=(B . "0x3029") ; HANGZHOU NUMERAL NINE
       (?$(G$>(B . "0x5341") ; [<CJK>] (hangzhou numeral ten)
       (?$(G$?(B . "0x5344") ; [<CJK>] (hangzhou numeral twenty)
       (?$(G$@(B . "0x5345") ; [<CJK>] (hangzhou numeral thirty)
       (?$(G$A(B . "0x0041") ; LATIN CAPITAL LETTER A
       (?$(G$B(B . "0x0042") ; LATIN CAPITAL LETTER B
       (?$(G$C(B . "0x0043") ; LATIN CAPITAL LETTER C
       (?$(G$D(B . "0x0044") ; LATIN CAPITAL LETTER D
       (?$(G$E(B . "0x0045") ; LATIN CAPITAL LETTER E
       (?$(G$F(B . "0x0046") ; LATIN CAPITAL LETTER F
       (?$(G$G(B . "0x0047") ; LATIN CAPITAL LETTER G
       (?$(G$H(B . "0x0048") ; LATIN CAPITAL LETTER H
       (?$(G$I(B . "0x0049") ; LATIN CAPITAL LETTER I
       (?$(G$J(B . "0x004A") ; LATIN CAPITAL LETTER J
       (?$(G$K(B . "0x004B") ; LATIN CAPITAL LETTER K
       (?$(G$L(B . "0x004C") ; LATIN CAPITAL LETTER L
       (?$(G$M(B . "0x004D") ; LATIN CAPITAL LETTER M
       (?$(G$N(B . "0x004E") ; LATIN CAPITAL LETTER N
       (?$(G$O(B . "0x004F") ; LATIN CAPITAL LETTER O
       (?$(G$P(B . "0x0050") ; LATIN CAPITAL LETTER P
       (?$(G$Q(B . "0x0051") ; LATIN CAPITAL LETTER Q
       (?$(G$R(B . "0x0052") ; LATIN CAPITAL LETTER R
       (?$(G$S(B . "0x0053") ; LATIN CAPITAL LETTER S
       (?$(G$T(B . "0x0054") ; LATIN CAPITAL LETTER T
       (?$(G$U(B . "0x0055") ; LATIN CAPITAL LETTER U
       (?$(G$V(B . "0x0056") ; LATIN CAPITAL LETTER V
       (?$(G$W(B . "0x0057") ; LATIN CAPITAL LETTER W
       (?$(G$X(B . "0x0058") ; LATIN CAPITAL LETTER X
       (?$(G$Y(B . "0x0059") ; LATIN CAPITAL LETTER Y
       (?$(G$Z(B . "0x005A") ; LATIN CAPITAL LETTER Z
       (?$(G$[(B . "0x0061") ; LATIN SMALL LETTER A
       (?$(G$\(B . "0x0062") ; LATIN SMALL LETTER B
       (?$(G$](B . "0x0063") ; LATIN SMALL LETTER C
       (?$(G$^(B . "0x0064") ; LATIN SMALL LETTER D
       (?$(G$_(B . "0x0065") ; LATIN SMALL LETTER E
       (?$(G$`(B . "0x0066") ; LATIN SMALL LETTER F
       (?$(G$a(B . "0x0067") ; LATIN SMALL LETTER G
       (?$(G$b(B . "0x0068") ; LATIN SMALL LETTER H
       (?$(G$c(B . "0x0069") ; LATIN SMALL LETTER I
       (?$(G$d(B . "0x006A") ; LATIN SMALL LETTER J
       (?$(G$e(B . "0x006B") ; LATIN SMALL LETTER K
       (?$(G$f(B . "0x006C") ; LATIN SMALL LETTER L
       (?$(G$g(B . "0x006D") ; LATIN SMALL LETTER M
       (?$(G$h(B . "0x006E") ; LATIN SMALL LETTER N
       (?$(G$i(B . "0x006F") ; LATIN SMALL LETTER O
       (?$(G$j(B . "0x0070") ; LATIN SMALL LETTER P
       (?$(G$k(B . "0x0071") ; LATIN SMALL LETTER Q
       (?$(G$l(B . "0x0072") ; LATIN SMALL LETTER R
       (?$(G$m(B . "0x0073") ; LATIN SMALL LETTER S
       (?$(G$n(B . "0x0074") ; LATIN SMALL LETTER T
       (?$(G$o(B . "0x0075") ; LATIN SMALL LETTER U
       (?$(G$p(B . "0x0076") ; LATIN SMALL LETTER V
       (?$(G$q(B . "0x0077") ; LATIN SMALL LETTER W
       (?$(G$r(B . "0x0078") ; LATIN SMALL LETTER X
       (?$(G$s(B . "0x0079") ; LATIN SMALL LETTER Y
       (?$(G$t(B . "0x007A") ; LATIN SMALL LETTER Z
       (?$(G$u(B . "0x0391") ; GREEK CAPITAL LETTER ALPHA
       (?$(G$v(B . "0x0392") ; GREEK CAPITAL LETTER BETA
       (?$(G$w(B . "0x0393") ; GREEK CAPITAL LETTER GAMMA
       (?$(G$x(B . "0x0394") ; GREEK CAPITAL LETTER DELTA
       (?$(G$y(B . "0x0395") ; GREEK CAPITAL LETTER EPSILON
       (?$(G$z(B . "0x0396") ; GREEK CAPITAL LETTER ZETA
       (?$(G${(B . "0x0397") ; GREEK CAPITAL LETTER ETA
       (?$(G$|(B . "0x0398") ; GREEK CAPITAL LETTER THETA
       (?$(G$}(B . "0x0399") ; GREEK CAPITAL LETTER IOTA
       (?$(G$~(B . "0x039A") ; GREEK CAPITAL LETTER KAPPA
       (?$(G%!(B . "0x039B") ; GREEK CAPITAL LETTER LAMDA
       (?$(G%"(B . "0x039C") ; GREEK CAPITAL LETTER MU
       (?$(G%#(B . "0x039D") ; GREEK CAPITAL LETTER NU
       (?$(G%$(B . "0x039E") ; GREEK CAPITAL LETTER XI
       (?$(G%%(B . "0x039F") ; GREEK CAPITAL LETTER OMICRON
       (?$(G%&(B . "0x03A0") ; GREEK CAPITAL LETTER PI
       (?$(G%'(B . "0x03A1") ; GREEK CAPITAL LETTER RHO
       (?$(G%((B . "0x03A3") ; GREEK CAPITAL LETTER SIGMA
       (?$(G%)(B . "0x03A4") ; GREEK CAPITAL LETTER TAU
       (?$(G%*(B . "0x03A5") ; GREEK CAPITAL LETTER UPSILON
       (?$(G%+(B . "0x03A6") ; GREEK CAPITAL LETTER PHI
       (?$(G%,(B . "0x03A7") ; GREEK CAPITAL LETTER CHI
       (?$(G%-(B . "0x03A8") ; GREEK CAPITAL LETTER PSI
       (?$(G%.(B . "0x03A9") ; GREEK CAPITAL LETTER OMEGA
       (?$(G%/(B . "0x03B1") ; GREEK SMALL LETTER ALPHA
       (?$(G%0(B . "0x03B2") ; GREEK SMALL LETTER BETA
       (?$(G%1(B . "0x03B3") ; GREEK SMALL LETTER GAMMA
       (?$(G%2(B . "0x03B4") ; GREEK SMALL LETTER DELTA
       (?$(G%3(B . "0x03B5") ; GREEK SMALL LETTER EPSILON
       (?$(G%4(B . "0x03B6") ; GREEK SMALL LETTER ZETA
       (?$(G%5(B . "0x03B7") ; GREEK SMALL LETTER ETA
       (?$(G%6(B . "0x03B8") ; GREEK SMALL LETTER THETA
       (?$(G%7(B . "0x03B9") ; GREEK SMALL LETTER IOTA
       (?$(G%8(B . "0x03BA") ; GREEK SMALL LETTER KAPPA
       (?$(G%9(B . "0x03BB") ; GREEK SMALL LETTER LAMDA
       (?$(G%:(B . "0x03BC") ; GREEK SMALL LETTER MU
       (?$(G%;(B . "0x03BD") ; GREEK SMALL LETTER NU
       (?$(G%<(B . "0x03BE") ; GREEK SMALL LETTER XI
       (?$(G%=(B . "0x03BF") ; GREEK SMALL LETTER OMICRON
       (?$(G%>(B . "0x03C0") ; GREEK SMALL LETTER PI
       (?$(G%?(B . "0x03C1") ; GREEK SMALL LETTER RHO
       (?$(G%@(B . "0x03C3") ; GREEK SMALL LETTER SIGMA
       (?$(G%A(B . "0x03C4") ; GREEK SMALL LETTER TAU
       (?$(G%B(B . "0x03C5") ; GREEK SMALL LETTER UPSILON
       (?$(G%C(B . "0x03C6") ; GREEK SMALL LETTER PHI
       (?$(G%D(B . "0x03C7") ; GREEK SMALL LETTER CHI
       (?$(G%E(B . "0x03C8") ; GREEK SMALL LETTER PSI
       (?$(G%F(B . "0x03C9") ; GREEK SMALL LETTER OMEGA
       (?$(G%G(B . "0x3105") ; BOPOMOFO LETTER B
       (?$(G%H(B . "0x3106") ; BOPOMOFO LETTER P
       (?$(G%I(B . "0x3107") ; BOPOMOFO LETTER M
       (?$(G%J(B . "0x3108") ; BOPOMOFO LETTER F
       (?$(G%K(B . "0x3109") ; BOPOMOFO LETTER D
       (?$(G%L(B . "0x310A") ; BOPOMOFO LETTER T
       (?$(G%M(B . "0x310B") ; BOPOMOFO LETTER N
       (?$(G%N(B . "0x310C") ; BOPOMOFO LETTER L
       (?$(G%O(B . "0x310D") ; BOPOMOFO LETTER G
       (?$(G%P(B . "0x310E") ; BOPOMOFO LETTER K
       (?$(G%Q(B . "0x310F") ; BOPOMOFO LETTER H
       (?$(G%R(B . "0x3110") ; BOPOMOFO LETTER J
       (?$(G%S(B . "0x3111") ; BOPOMOFO LETTER Q
       (?$(G%T(B . "0x3112") ; BOPOMOFO LETTER X
       (?$(G%U(B . "0x3113") ; BOPOMOFO LETTER ZH
       (?$(G%V(B . "0x3114") ; BOPOMOFO LETTER CH
       (?$(G%W(B . "0x3115") ; BOPOMOFO LETTER SH
       (?$(G%X(B . "0x3116") ; BOPOMOFO LETTER R
       (?$(G%Y(B . "0x3117") ; BOPOMOFO LETTER Z
       (?$(G%Z(B . "0x3118") ; BOPOMOFO LETTER C
       (?$(G%[(B . "0x3119") ; BOPOMOFO LETTER S
       (?$(G%\(B . "0x311A") ; BOPOMOFO LETTER A
       (?$(G%](B . "0x311B") ; BOPOMOFO LETTER O
       (?$(G%^(B . "0x311C") ; BOPOMOFO LETTER E
       (?$(G%_(B . "0x311D") ; BOPOMOFO LETTER EH
       (?$(G%`(B . "0x311E") ; BOPOMOFO LETTER AI
       (?$(G%a(B . "0x311F") ; BOPOMOFO LETTER EI
       (?$(G%b(B . "0x3120") ; BOPOMOFO LETTER AU
       (?$(G%c(B . "0x3121") ; BOPOMOFO LETTER OU
       (?$(G%d(B . "0x3122") ; BOPOMOFO LETTER AN
       (?$(G%e(B . "0x3123") ; BOPOMOFO LETTER EN
       (?$(G%f(B . "0x3124") ; BOPOMOFO LETTER ANG
       (?$(G%g(B . "0x3125") ; BOPOMOFO LETTER ENG
       (?$(G%h(B . "0x3126") ; BOPOMOFO LETTER ER
       (?$(G%i(B . "0x3127") ; BOPOMOFO LETTER I
       (?$(G%j(B . "0x3128") ; BOPOMOFO LETTER U
       (?$(G%k(B . "0x3129") ; BOPOMOFO LETTER IU
       (?$(G%l(B . "0x02D9") ; DOT ABOVE (Mandarin Chinese light tone)
       (?$(G%m(B . "0x2003") ; EM SPACE
       (?$(G%n(B . "0x02CA") ; MODIFIER LETTER ACUTE ACCENT (Mandarin Chinese second tone)
       (?$(G%o(B . "0x02C7") ; CARON (Mandarin Chinese third tone)
       (?$(G%p(B . "0x02CB") ; MODIFIER LETTER GRAVE ACCENT (Mandarin Chinese fourth tone)
       (?$(G&!(B . "0x2460") ; CIRCLED DIGIT ONE
       (?$(G&"(B . "0x2461") ; CIRCLED DIGIT TWO
       (?$(G&#(B . "0x2462") ; CIRCLED DIGIT THREE
       (?$(G&$(B . "0x2463") ; CIRCLED DIGIT FOUR
       (?$(G&%(B . "0x2464") ; CIRCLED DIGIT FIVE
       (?$(G&&(B . "0x2465") ; CIRCLED DIGIT SIX
       (?$(G&'(B . "0x2466") ; CIRCLED DIGIT SEVEN
       (?$(G&((B . "0x2467") ; CIRCLED DIGIT EIGHT
       (?$(G&)(B . "0x2468") ; CIRCLED DIGIT NINE
       (?$(G&*(B . "0x2469") ; CIRCLED NUMBER TEN
       (?$(G&+(B . "0x2474") ; PARENTHESIZED DIGIT ONE
       (?$(G&,(B . "0x2475") ; PARENTHESIZED DIGIT TWO
       (?$(G&-(B . "0x2476") ; PARENTHESIZED DIGIT THREE
       (?$(G&.(B . "0x2477") ; PARENTHESIZED DIGIT FOUR
       (?$(G&/(B . "0x2478") ; PARENTHESIZED DIGIT FIVE
       (?$(G&0(B . "0x2479") ; PARENTHESIZED DIGIT SIX
       (?$(G&1(B . "0x247A") ; PARENTHESIZED DIGIT SEVEN
       (?$(G&2(B . "0x247B") ; PARENTHESIZED DIGIT EIGHT
       (?$(G&3(B . "0x247C") ; PARENTHESIZED DIGIT NINE
       (?$(G&4(B . "0x247D") ; PARENTHESIZED NUMBER TEN
       (?$(G&5(B . "0x2170") ; SMALL ROMAN NUMERAL ONE
       (?$(G&6(B . "0x2171") ; SMALL ROMAN NUMERAL TWO
       (?$(G&7(B . "0x2172") ; SMALL ROMAN NUMERAL THREE
       (?$(G&8(B . "0x2173") ; SMALL ROMAN NUMERAL FOUR
       (?$(G&9(B . "0x2174") ; SMALL ROMAN NUMERAL FIVE
       (?$(G&:(B . "0x2175") ; SMALL ROMAN NUMERAL SIX
       (?$(G&;(B . "0x2176") ; SMALL ROMAN NUMERAL SEVEN
       (?$(G&<(B . "0x2177") ; SMALL ROMAN NUMERAL EIGHT
       (?$(G&=(B . "0x2178") ; SMALL ROMAN NUMERAL NINE
       (?$(G&>(B . "0x2179") ; SMALL ROMAN NUMERAL TEN
       (?$(G'!(B . "0x2F00") ; KangXi radical ONE
       (?$(G'"(B . "0x2F01") ; KangXi radical LINE
       (?$(G'#(B . "0x2F02") ; KangXi radical DOT
       (?$(G'$(B . "0x2F03") ; KangXi radical SLASH
       (?$(G'%(B . "0x2F04") ; KangXi radical SECOND
       (?$(G'&(B . "0x2F05") ; KangXi radical HOOK
       (?$(G''(B . "0x2F06") ; KangXi radical TWO
       (?$(G'((B . "0x2F07") ; KangXi radical LID
       (?$(G'((B . "0x4EA0") ; <CJK>
       (?$(G')(B . "0x2F08") ; KangXi radical MAN
       (?$(G'*(B . "0x2F09") ; KangXi radical LEGS
       (?$(G'+(B . "0x2F0A") ; KangXi radical ENTER
       (?$(G',(B . "0x2F0B") ; KangXi radical EIGHT
       (?$(G'-(B . "0x2F0C") ; KangXi radical DOWN BOX
       (?$(G'.(B . "0x2F0D") ; KangXi radical COVER
       (?$(G'/(B . "0x2F0E") ; KangXi radical ICE
       (?$(G'/(B . "0x51AB") ; <CJK>
       (?$(G'0(B . "0x2F0F") ; KangXi radical TABLE
       (?$(G'1(B . "0x2F10") ; KangXi radical OPEN BOX
       (?$(G'2(B . "0x2F11") ; KangXi radical KNIFE
       (?$(G'3(B . "0x2F12") ; KangXi radical POWER
       (?$(G'4(B . "0x2F13") ; KangXi radical WRAP
       (?$(G'4(B . "0x52F9") ; <CJK>
       (?$(G'5(B . "0x2F14") ; KangXi radical SPOON
       (?$(G'6(B . "0x2F15") ; KangXi radical RIGHT OPEN BOX
       (?$(G'7(B . "0x2F16") ; KangXi radical HIDING ENCLOSURE
       (?$(G'8(B . "0x2F17") ; KangXi radical TEN
       (?$(G'9(B . "0x2F18") ; KangXi radical DIVINATION
       (?$(G':(B . "0x2F19") ; KangXi radical SEAL
       (?$(G';(B . "0x2F1A") ; KangXi radical CLIFF
       (?$(G'<(B . "0x2F1B") ; KangXi radical PRIVATE
       (?$(G'=(B . "0x2F1C") ; KangXi radical AGAIN
       (?$(G'>(B . "0x2F1D") ; KangXi radical MOUTH
       (?$(G'?(B . "0x2F1E") ; KangXi radical ENCLOSURE
       (?$(G'@(B . "0x2F1F") ; KangXi radical EARTH
       (?$(G'A(B . "0x2F20") ; KangXi radical SCHOLAR
       (?$(G'B(B . "0x2F22") ; KangXi radical GO SLOWLY
       (?$(G'C(B . "0x2F23") ; KangXi radical EVENING
       (?$(G'D(B . "0x2F24") ; KangXi radical BIG
       (?$(G'E(B . "0x2F25") ; KangXi radical WOMAN
       (?$(G'F(B . "0x2F26") ; KangXi radical CHILD
       (?$(G'G(B . "0x2F27") ; KangXi radical ROOF
       (?$(G'H(B . "0x2F28") ; KangXi radical INCH
       (?$(G'I(B . "0x2F29") ; KangXi radical SMALL
       (?$(G'J(B . "0x2F2A") ; KangXi radical LAME
       (?$(G'K(B . "0x2F2B") ; KangXi radical CORPSE
       (?$(G'L(B . "0x2F2C") ; KangXi radical SPROUT
       (?$(G'M(B . "0x2F2D") ; KangXi radical MOUNTAIN
       (?$(G'N(B . "0x2F2E") ; KangXi radical RIVER
       (?$(G'O(B . "0x2F2F") ; KangXi radical WORK
       (?$(G'P(B . "0x2F30") ; KangXi radical ONESELF
       (?$(G'Q(B . "0x2F31") ; KangXi radical TURBAN
       (?$(G'R(B . "0x2F32") ; KangXi radical DRY
       (?$(G'S(B . "0x2F33") ; KangXi radical SHORT THREAD
       (?$(G'T(B . "0x2F34") ; KangXi radical DOTTED CLIFF
       (?$(G'U(B . "0x2F35") ; KangXi radical LONG STRIDE
       (?$(G'V(B . "0x2F36") ; KangXi radical TWO HANDS
       (?$(G'W(B . "0x2F37") ; KangXi radical SHOOT
       (?$(G'X(B . "0x2F38") ; KangXi radical BOW
       (?$(G'Y(B . "0x2F39") ; KangXi radical SNOUT
       (?$(G'Z(B . "0x2F3A") ; KangXi radical BRISTLE
       (?$(G'[(B . "0x2F3B") ; KangXi radical STEP
       (?$(G'\(B . "0x2F3C") ; KangXi radical HEART
       (?$(G'](B . "0x2F3D") ; KangXi radical HALBERD
       (?$(G'^(B . "0x2F3E") ; KangXi radical DOOR
       (?$(G'_(B . "0x2F3F") ; KangXi radical HAND
       (?$(G'`(B . "0x2F40") ; KangXi radical BRANCH
       (?$(G'a(B . "0x2F41") ; KangXi radical RAP
       (?$(G'b(B . "0x2F42") ; KangXi radical SCRIPT
       (?$(G'c(B . "0x2F43") ; KangXi radical DIPPER
       (?$(G'd(B . "0x2F44") ; KangXi radical AXE
       (?$(G'e(B . "0x2F45") ; KangXi radical SQUARE
       (?$(G'f(B . "0x2F46") ; KangXi radical NOT
       (?$(G'g(B . "0x2F47") ; KangXi radical SUN
       (?$(G'h(B . "0x2F48") ; KangXi radical SAY
       (?$(G'i(B . "0x2F49") ; KangXi radical MOON
       (?$(G'j(B . "0x2F4A") ; KangXi radical TREE
       (?$(G'k(B . "0x2F4B") ; KangXi radical LACK
       (?$(G'l(B . "0x2F4C") ; KangXi radical STOP
       (?$(G'm(B . "0x2F4D") ; KangXi radical DEATH
       (?$(G'n(B . "0x2F4E") ; KangXi radical WEAPON
       (?$(G'o(B . "0x2F4F") ; KangXi radical DO NOT
       (?$(G'p(B . "0x2F50") ; KangXi radical COMPARE
       (?$(G'q(B . "0x2F51") ; KangXi radical FUR
       (?$(G'r(B . "0x2F52") ; KangXi radical CLAN
       (?$(G's(B . "0x2F53") ; KangXi radical STEAM
       (?$(G't(B . "0x2F54") ; KangXi radical WATER
       (?$(G'u(B . "0x2F55") ; KangXi radical FIRE
       (?$(G'v(B . "0x2F56") ; KangXi radical CLAW
       (?$(G'w(B . "0x2F57") ; KangXi radical FATHER
       (?$(G'x(B . "0x2F58") ; KangXi radical DOUBLE X
       (?$(G'y(B . "0x2F59") ; KangXi radical HALF TREE TRUNK
       (?$(G'z(B . "0x2F5A") ; KangXi radical SLICE
       (?$(G'{(B . "0x2F5B") ; KangXi radical FANG
       (?$(G'|(B . "0x2F5C") ; KangXi radical COW
       (?$(G'}(B . "0x2F5D") ; KangXi radical DOG
       (?$(G'~(B . "0x2F5E") ; KangXi radical PROFOUND
       (?$(G(!(B . "0x2F5F") ; KangXi radical JADE
       (?$(G("(B . "0x2F60") ; KangXi radical MELON
       (?$(G(#(B . "0x2F61") ; KangXi radical TILE
       (?$(G($(B . "0x2F62") ; KangXi radical SWEET
       (?$(G(%(B . "0x2F63") ; KangXi radical LIFE
       (?$(G(&(B . "0x2F64") ; KangXi radical USE
       (?$(G('(B . "0x2F65") ; KangXi radical FIELD
       (?$(G(((B . "0x2F66") ; KangXi radical BOLT OF CLOTH
       (?$(G()(B . "0x2F67") ; KangXi radical SICKNESS
       (?$(G(*(B . "0x2F68") ; KangXi radical DOTTED TENT
       (?$(G(+(B . "0x2F69") ; KangXi radical WHITE
       (?$(G(,(B . "0x2F6A") ; KangXi radical SKIN
       (?$(G(-(B . "0x2F6B") ; KangXi radical DISH
       (?$(G(.(B . "0x2F6C") ; KangXi radical EYE
       (?$(G(/(B . "0x2F6D") ; KangXi radical SPEAR
       (?$(G(0(B . "0x2F6E") ; KangXi radical ARROW
       (?$(G(1(B . "0x2F6F") ; KangXi radical STONE
       (?$(G(2(B . "0x2F70") ; KangXi radical SPIRIT
       (?$(G(3(B . "0x2F71") ; KangXi radical TRACK
       (?$(G(4(B . "0x2F72") ; KangXi radical GRAIN
       (?$(G(5(B . "0x2F73") ; KangXi radical CAVE
       (?$(G(6(B . "0x2F74") ; KangXi radical STAND
       (?$(G(7(B . "0x2F75") ; KangXi radical BAMBOO
       (?$(G(8(B . "0x2F76") ; KangXi radical RICE
       (?$(G(9(B . "0x2F77") ; KangXi radical SILK
       (?$(G(:(B . "0x2F78") ; KangXi radical JAR
       (?$(G(;(B . "0x2F79") ; KangXi radical NET
       (?$(G(<(B . "0x2F7A") ; KangXi radical SHEEP
       (?$(G(=(B . "0x2F7B") ; KangXi radical FEATHER
       (?$(G(>(B . "0x2F7C") ; KangXi radical OLD
       (?$(G(?(B . "0x2F7D") ; KangXi radical AND
       (?$(G(@(B . "0x2F7E") ; KangXi radical PLOW
       (?$(G(A(B . "0x2F7F") ; KangXi radical EAR
       (?$(G(B(B . "0x2F80") ; KangXi radical BRUSH
       (?$(G(C(B . "0x2F81") ; KangXi radical MEAT
       (?$(G(D(B . "0x2F82") ; KangXi radical MINISTER
       (?$(G(E(B . "0x2F83") ; KangXi radical SELF
       (?$(G(F(B . "0x2F84") ; KangXi radical ARRIVE
       (?$(G(G(B . "0x2F85") ; KangXi radical MORTAR
       (?$(G(H(B . "0x2F86") ; KangXi radical TONGUE
       (?$(G(I(B . "0x2F87") ; KangXi radical OPPOSE
       (?$(G(J(B . "0x2F88") ; KangXi radical BOAT
       (?$(G(K(B . "0x2F89") ; KangXi radical STOPPING
       (?$(G(L(B . "0x2F8A") ; KangXi radical COLOR
       (?$(G(M(B . "0x2F8B") ; KangXi radical GRASS
       (?$(G(N(B . "0x2F8C") ; KangXi radical TIGER
       (?$(G(O(B . "0x2F8D") ; KangXi radical INSECT
       (?$(G(P(B . "0x2F8E") ; KangXi radical BLOOD
       (?$(G(Q(B . "0x2F8F") ; KangXi radical WALK ENCLOSURE
       (?$(G(R(B . "0x2F90") ; KangXi radical CLOTHES
       (?$(G(S(B . "0x2F91") ; KangXi radical WEST
       (?$(G(T(B . "0x2F92") ; KangXi radical SEE
       (?$(G(U(B . "0x2F93") ; KangXi radical HORN
       (?$(G(V(B . "0x2F94") ; KangXi radical SPEECH
       (?$(G(W(B . "0x2F95") ; KangXi radical VALLEY
       (?$(G(X(B . "0x2F96") ; KangXi radical BEAN
       (?$(G(Y(B . "0x2F97") ; KangXi radical PIG
       (?$(G(Z(B . "0x2F98") ; KangXi radical BADGER
       (?$(G([(B . "0x2F99") ; KangXi radical SHELL
       (?$(G(\(B . "0x2F9A") ; KangXi radical RED
       (?$(G(](B . "0x2F9B") ; KangXi radical RUN
       (?$(G(^(B . "0x2F9C") ; KangXi radical FOOT
       (?$(G(_(B . "0x2F9D") ; KangXi radical BODY
       (?$(G(`(B . "0x2F9E") ; KangXi radical CART
       (?$(G(a(B . "0x2F9F") ; KangXi radical BITTER
       (?$(G(b(B . "0x2FA0") ; KangXi radical MORNING
       (?$(G(c(B . "0x2FA1") ; KangXi radical WALK
       (?$(G(d(B . "0x2FA2") ; KangXi radical CITY
       (?$(G(e(B . "0x2FA3") ; KangXi radical WINE
       (?$(G(f(B . "0x2FA4") ; KangXi radical DISTINGUISH
       (?$(G(g(B . "0x2FA5") ; KangXi radical VILLAGE
       (?$(G(h(B . "0x2FA6") ; KangXi radical GOLD
       (?$(G(i(B . "0x2FA7") ; KangXi radical LONG
       (?$(G(j(B . "0x2FA8") ; KangXi radical GATE
       (?$(G(k(B . "0x2FA9") ; KangXi radical MOUND
       (?$(G(l(B . "0x2FAA") ; KangXi radical SLAVE
       (?$(G(m(B . "0x2FAB") ; KangXi radical SHORT TAILED BIRD
       (?$(G(n(B . "0x2FAC") ; KangXi radical RAIN
       (?$(G(o(B . "0x2FAD") ; KangXi radical BLUE
       (?$(G(p(B . "0x2FAE") ; KangXi radical WRONG
       (?$(G(q(B . "0x2FAF") ; KangXi radical FACE
       (?$(G(r(B . "0x2FB0") ; KangXi radical LEATHER
       (?$(G(s(B . "0x2FB1") ; KangXi radical TANNED LEATHER
       (?$(G(t(B . "0x2FB2") ; KangXi radical LEEK
       (?$(G(u(B . "0x2FB3") ; KangXi radical SOUND
       (?$(G(v(B . "0x2FB4") ; KangXi radical LEAF
       (?$(G(w(B . "0x2FB5") ; KangXi radical WIND
       (?$(G(x(B . "0x2FB6") ; KangXi radical FLY
       (?$(G(y(B . "0x2FB7") ; KangXi radical EAT
       (?$(G(z(B . "0x2FB8") ; KangXi radical HEAD
       (?$(G({(B . "0x2FB9") ; KangXi radical FRAGRANT
       (?$(G(|(B . "0x2FBA") ; KangXi radical HORSE
       (?$(G(}(B . "0x2FBB") ; KangXi radical BONE
       (?$(G(~(B . "0x2FBC") ; KangXi radical TALL
       (?$(G)!(B . "0x2FBD") ; KangXi radical HAIR
       (?$(G)"(B . "0x2FBE") ; KangXi radical FIGHT
       (?$(G)#(B . "0x2FBF") ; KangXi radical SACRIFICIAL WINE
       (?$(G)$(B . "0x2FC0") ; KangXi radical CAULDRON
       (?$(G)%(B . "0x2FC1") ; KangXi radical GHOST
       (?$(G)&(B . "0x2FC2") ; KangXi radical FISH
       (?$(G)'(B . "0x2FC3") ; KangXi radical BIRD
       (?$(G)((B . "0x2FC4") ; KangXi radical SALT
       (?$(G))(B . "0x2FC5") ; KangXi radical DEER
       (?$(G)*(B . "0x2FC6") ; KangXi radical WHEAT
       (?$(G)+(B . "0x2FC7") ; KangXi radical HEMP
       (?$(G),(B . "0x2FC8") ; KangXi radical YELLOW
       (?$(G)-(B . "0x2FC9") ; KangXi radical MILLET
       (?$(G).(B . "0x2FCA") ; KangXi radical BLACK
       (?$(G)/(B . "0x2FCB") ; KangXi radical EMBROIDERY
       (?$(G)0(B . "0x2FCC") ; KangXi radical FROG
       (?$(G)1(B . "0x2FCD") ; KangXi radical TRIPOD
       (?$(G)2(B . "0x2FCE") ; KangXi radical DRUM
       (?$(G)3(B . "0x2FCF") ; KangXi radical RAT
       (?$(G)4(B . "0x2FD0") ; KangXi radical NOSE
       (?$(G)5(B . "0x2FD1") ; KangXi radical EVEN
       (?$(G)6(B . "0x2FD2") ; KangXi radical TOOTH
       (?$(G)7(B . "0x2FD3") ; KangXi radical DRAGON
       (?$(G)8(B . "0x2FD4") ; KangXi radical TURTLE
       (?$(G)9(B . "0x2FD5") ; KangXi radical FLUTE
       (?$(GB!(B . "0x2400") ; SYMBOL FOR NULL
       (?$(GB"(B . "0x2401") ; SYMBOL FOR START OF HEADING
       (?$(GB#(B . "0x2402") ; SYMBOL FOR START OF TEXT
       (?$(GB$(B . "0x2403") ; SYMBOL FOR END OF TEXT
       (?$(GB%(B . "0x2404") ; SYMBOL FOR END OF TRANSMISSION
       (?$(GB&(B . "0x2405") ; SYMBOL FOR ENQUIRY
       (?$(GB'(B . "0x2406") ; SYMBOL FOR ACKNOWLEDGE
       (?$(GB((B . "0x2407") ; SYMBOL FOR BELL
       (?$(GB)(B . "0x2408") ; SYMBOL FOR BACKSPACE
       (?$(GB*(B . "0x2409") ; SYMBOL FOR HORIZONTAL TABULATION
       (?$(GB+(B . "0x240A") ; SYMBOL FOR LINE FEED
       (?$(GB,(B . "0x240B") ; SYMBOL FOR VERTICAL TABULATION
       (?$(GB-(B . "0x240C") ; SYMBOL FOR FORM FEED
       (?$(GB.(B . "0x240D") ; SYMBOL FOR CARIIAGE RETURN
       (?$(GB/(B . "0x240E") ; SYMBOL FOR SHIFT OUT
       (?$(GB0(B . "0x240F") ; SYMBOL FOR SHIFT IN
       (?$(GB1(B . "0x2410") ; SYMBOL FOR DATA LINK ESCAPE
       (?$(GB2(B . "0x2411") ; SYMBOL FOR DEVICE CONTROL ONE
       (?$(GB3(B . "0x2412") ; SYMBOL FOR DEVICE CONTROL TWO
       (?$(GB4(B . "0x2413") ; SYMBOL FOR DEVICE CONTROL THREE
       (?$(GB5(B . "0x2414") ; SYMBOL FOR DEVICE CONTROL FOUR
       (?$(GB6(B . "0x2415") ; SYMBOL FOR NEGATIVE ACKNOWLEDGE
       (?$(GB7(B . "0x2416") ; SYMBOL FOR SYNCHRONOUS IDLE
       (?$(GB8(B . "0x2417") ; SYMBOL FOR END OF TRANSMISSION BLOCK
       (?$(GB9(B . "0x2418") ; SYMBOL FOR CANCEL
       (?$(GB:(B . "0x2419") ; SYMBOL FOR END OF MEDIUM
       (?$(GB;(B . "0x241A") ; SYMBOL FOR SUBSTITUTE
       (?$(GB<(B . "0x241B") ; SYMBOL FOR ESCAPE
       (?$(GB=(B . "0x241C") ; SYMBOL FOR FILE SEPARATOR
       (?$(GB>(B . "0x241D") ; SYMBOL FOR GROUP SEPARATOR
       (?$(GB?(B . "0x241E") ; SYMBOL FOR RECORD SEPARATOR
       (?$(GB@(B . "0x241F") ; SYMBOL FOR UNIT SEPARATOR
       (?$(GBA(B . "0x2421") ; SYMBOL FOR DELETE
       (?$(GD!(B . "0x4E00") ; <CJK>
       (?$(GD"(B . "0x4E59") ; <CJK>
       (?$(GD#(B . "0x4E01") ; <CJK>
       (?$(GD$(B . "0x4E03") ; <CJK>
       (?$(GD%(B . "0x4E43") ; <CJK>
       (?$(GD&(B . "0x4E5D") ; <CJK>
       (?$(GD'(B . "0x4E86") ; <CJK>
       (?$(GD((B . "0x4E8C") ; <CJK>
       (?$(GD)(B . "0x4EBA") ; <CJK>
       (?$(GD*(B . "0x513F") ; <CJK>
       (?$(GD+(B . "0x5165") ; <CJK>
       (?$(GD,(B . "0x516B") ; <CJK>
       (?$(GD-(B . "0x51E0") ; <CJK>
       (?$(GD.(B . "0x5200") ; <CJK>
       (?$(GD/(B . "0x5201") ; <CJK>
       (?$(GD0(B . "0x529B") ; <CJK>
       (?$(GD1(B . "0x5315") ; <CJK>
       (?$(GD2(B . "0x5341") ; <CJK>
       (?$(GD3(B . "0x535C") ; <CJK>
       (?$(GD4(B . "0x53C8") ; <CJK>
       (?$(GD5(B . "0x4E09") ; <CJK>
       (?$(GD6(B . "0x4E0B") ; <CJK>
       (?$(GD7(B . "0x4E08") ; <CJK>
       (?$(GD8(B . "0x4E0A") ; <CJK>
       (?$(GD9(B . "0x4E2B") ; <CJK>
       (?$(GD:(B . "0x4E38") ; <CJK>
       (?$(GD;(B . "0x51E1") ; <CJK>
       (?$(GD<(B . "0x4E45") ; <CJK>
       (?$(GD=(B . "0x4E48") ; <CJK>
       (?$(GD>(B . "0x4E5F") ; <CJK>
       (?$(GD?(B . "0x4E5E") ; <CJK>
       (?$(GD@(B . "0x4E8E") ; <CJK>
       (?$(GDA(B . "0x4EA1") ; <CJK>
       (?$(GDB(B . "0x5140") ; <CJK>
       (?$(GDC(B . "0x5203") ; <CJK>
       (?$(GDD(B . "0x52FA") ; <CJK>
       (?$(GDE(B . "0x5343") ; <CJK>
       (?$(GDF(B . "0x53C9") ; <CJK>
       (?$(GDG(B . "0x53E3") ; <CJK>
       (?$(GDH(B . "0x571F") ; <CJK>
       (?$(GDI(B . "0x58EB") ; <CJK>
       (?$(GDJ(B . "0x5915") ; <CJK>
       (?$(GDK(B . "0x5927") ; <CJK>
       (?$(GDL(B . "0x5973") ; <CJK>
       (?$(GDM(B . "0x5B50") ; <CJK>
       (?$(GDN(B . "0x5B51") ; <CJK>
       (?$(GDO(B . "0x5B53") ; <CJK>
       (?$(GDP(B . "0x5BF8") ; <CJK>
       (?$(GDQ(B . "0x5C0F") ; <CJK>
       (?$(GDR(B . "0x5C22") ; <CJK>
       (?$(GDS(B . "0x5C38") ; <CJK>
       (?$(GDT(B . "0x5C71") ; <CJK>
       (?$(GDU(B . "0x5DDD") ; <CJK>
       (?$(GDV(B . "0x5DE5") ; <CJK>
       (?$(GDW(B . "0x5DF1") ; <CJK>
       (?$(GDX(B . "0x5DF2") ; <CJK>
       (?$(GDY(B . "0x5DF3") ; <CJK>
       (?$(GDZ(B . "0x5DFE") ; <CJK>
       (?$(GD[(B . "0x5E72") ; <CJK>
       (?$(GD\(B . "0x5EFE") ; <CJK>
       (?$(GD](B . "0x5F0B") ; <CJK>
       (?$(GD^(B . "0x5F13") ; <CJK>
       (?$(GD_(B . "0x624D") ; <CJK>
       (?$(GD`(B . "0x4E11") ; <CJK>
       (?$(GDa(B . "0x4E10") ; <CJK>
       (?$(GDb(B . "0x4E0D") ; <CJK>
       (?$(GDc(B . "0x4E2D") ; <CJK>
       (?$(GDd(B . "0x4E30") ; <CJK>
       (?$(GDe(B . "0x4E39") ; <CJK>
       (?$(GDf(B . "0x4E4B") ; <CJK>
       (?$(GDg(B . "0x5C39") ; <CJK>
       (?$(GDh(B . "0x4E88") ; <CJK>
       (?$(GDi(B . "0x4E91") ; <CJK>
       (?$(GDj(B . "0x4E95") ; <CJK>
       (?$(GDk(B . "0x4E92") ; <CJK>
       (?$(GDl(B . "0x4E94") ; <CJK>
       (?$(GDm(B . "0x4EA2") ; <CJK>
       (?$(GDn(B . "0x4EC1") ; <CJK>
       (?$(GDo(B . "0x4EC0") ; <CJK>
       (?$(GDp(B . "0x4EC3") ; <CJK>
       (?$(GDq(B . "0x4EC6") ; <CJK>
       (?$(GDr(B . "0x4EC7") ; <CJK>
       (?$(GDs(B . "0x4ECD") ; <CJK>
       (?$(GDt(B . "0x4ECA") ; <CJK>
       (?$(GDu(B . "0x4ECB") ; <CJK>
       (?$(GDv(B . "0x4EC4") ; <CJK>
       (?$(GDw(B . "0x5143") ; <CJK>
       (?$(GDx(B . "0x5141") ; <CJK>
       (?$(GDy(B . "0x5167") ; <CJK>
       (?$(GDz(B . "0x516D") ; <CJK>
       (?$(GD{(B . "0x516E") ; <CJK>
       (?$(GD|(B . "0x516C") ; <CJK>
       (?$(GD}(B . "0x5197") ; <CJK>
       (?$(GD~(B . "0x51F6") ; <CJK>
       (?$(GE!(B . "0x5206") ; <CJK>
       (?$(GE"(B . "0x5207") ; <CJK>
       (?$(GE#(B . "0x5208") ; <CJK>
       (?$(GE$(B . "0x52FB") ; <CJK>
       (?$(GE%(B . "0x52FE") ; <CJK>
       (?$(GE&(B . "0x52FF") ; <CJK>
       (?$(GE'(B . "0x5316") ; <CJK>
       (?$(GE((B . "0x5339") ; <CJK>
       (?$(GE)(B . "0x5348") ; <CJK>
       (?$(GE*(B . "0x5347") ; <CJK>
       (?$(GE+(B . "0x5345") ; <CJK>
       (?$(GE,(B . "0x535E") ; <CJK>
       (?$(GE-(B . "0x5384") ; <CJK>
       (?$(GE.(B . "0x53CB") ; <CJK>
       (?$(GE/(B . "0x53CA") ; <CJK>
       (?$(GE0(B . "0x53CD") ; <CJK>
       (?$(GE1(B . "0x58EC") ; <CJK>
       (?$(GE2(B . "0x5929") ; <CJK>
       (?$(GE3(B . "0x592B") ; <CJK>
       (?$(GE4(B . "0x592A") ; <CJK>
       (?$(GE5(B . "0x592D") ; <CJK>
       (?$(GE6(B . "0x5B54") ; <CJK>
       (?$(GE7(B . "0x5C11") ; <CJK>
       (?$(GE8(B . "0x5C24") ; <CJK>
       (?$(GE9(B . "0x5C3A") ; <CJK>
       (?$(GE:(B . "0x5C6F") ; <CJK>
       (?$(GE;(B . "0x5DF4") ; <CJK>
       (?$(GE<(B . "0x5E7B") ; <CJK>
       (?$(GE=(B . "0x5EFF") ; <CJK>
       (?$(GE>(B . "0x5F14") ; <CJK>
       (?$(GE?(B . "0x5F15") ; <CJK>
       (?$(GE@(B . "0x5FC3") ; <CJK>
       (?$(GEA(B . "0x6208") ; <CJK>
       (?$(GEB(B . "0x6236") ; <CJK>
       (?$(GEC(B . "0x624B") ; <CJK>
       (?$(GED(B . "0x624E") ; <CJK>
       (?$(GEE(B . "0x652F") ; <CJK>
       (?$(GEF(B . "0x6587") ; <CJK>
       (?$(GEG(B . "0x6597") ; <CJK>
       (?$(GEH(B . "0x65A4") ; <CJK>
       (?$(GEI(B . "0x65B9") ; <CJK>
       (?$(GEJ(B . "0x65E5") ; <CJK>
       (?$(GEK(B . "0x66F0") ; <CJK>
       (?$(GEL(B . "0x6708") ; <CJK>
       (?$(GEM(B . "0x6728") ; <CJK>
       (?$(GEN(B . "0x6B20") ; <CJK>
       (?$(GEO(B . "0x6B62") ; <CJK>
       (?$(GEP(B . "0x6B79") ; <CJK>
       (?$(GEQ(B . "0x6BCB") ; <CJK>
       (?$(GER(B . "0x6BD4") ; <CJK>
       (?$(GES(B . "0x6BDB") ; <CJK>
       (?$(GET(B . "0x6C0F") ; <CJK>
       (?$(GEU(B . "0x6C34") ; <CJK>
       (?$(GEV(B . "0x706B") ; <CJK>
       (?$(GEW(B . "0x722A") ; <CJK>
       (?$(GEX(B . "0x7236") ; <CJK>
       (?$(GEY(B . "0x723B") ; <CJK>
       (?$(GEZ(B . "0x7247") ; <CJK>
       (?$(GE[(B . "0x7259") ; <CJK>
       (?$(GE\(B . "0x725B") ; <CJK>
       (?$(GE](B . "0x72AC") ; <CJK>
       (?$(GE^(B . "0x738B") ; <CJK>
       (?$(GE_(B . "0x4E19") ; <CJK>
       (?$(GE`(B . "0x4E16") ; <CJK>
       (?$(GEa(B . "0x4E15") ; <CJK>
       (?$(GEb(B . "0x4E14") ; <CJK>
       (?$(GEc(B . "0x4E18") ; <CJK>
       (?$(GEd(B . "0x4E3B") ; <CJK>
       (?$(GEe(B . "0x4E4D") ; <CJK>
       (?$(GEf(B . "0x4E4F") ; <CJK>
       (?$(GEg(B . "0x4E4E") ; <CJK>
       (?$(GEh(B . "0x4EE5") ; <CJK>
       (?$(GEi(B . "0x4ED8") ; <CJK>
       (?$(GEj(B . "0x4ED4") ; <CJK>
       (?$(GEk(B . "0x4ED5") ; <CJK>
       (?$(GEl(B . "0x4ED6") ; <CJK>
       (?$(GEm(B . "0x4ED7") ; <CJK>
       (?$(GEn(B . "0x4EE3") ; <CJK>
       (?$(GEo(B . "0x4EE4") ; <CJK>
       (?$(GEp(B . "0x4ED9") ; <CJK>
       (?$(GEq(B . "0x4EDE") ; <CJK>
       (?$(GEr(B . "0x5145") ; <CJK>
       (?$(GEs(B . "0x5144") ; <CJK>
       (?$(GEt(B . "0x5189") ; <CJK>
       (?$(GEu(B . "0x518A") ; <CJK>
       (?$(GEv(B . "0x51AC") ; <CJK>
       (?$(GEw(B . "0x51F9") ; <CJK>
       (?$(GEx(B . "0x51FA") ; <CJK>
       (?$(GEy(B . "0x51F8") ; <CJK>
       (?$(GEz(B . "0x520A") ; <CJK>
       (?$(GE{(B . "0x52A0") ; <CJK>
       (?$(GE|(B . "0x529F") ; <CJK>
       (?$(GE}(B . "0x5305") ; <CJK>
       (?$(GE~(B . "0x5306") ; <CJK>
       (?$(GF!(B . "0x5317") ; <CJK>
       (?$(GF"(B . "0x531D") ; <CJK>
       (?$(GF#(B . "0x4EDF") ; <CJK>
       (?$(GF$(B . "0x534A") ; <CJK>
       (?$(GF%(B . "0x5349") ; <CJK>
       (?$(GF&(B . "0x5361") ; <CJK>
       (?$(GF'(B . "0x5360") ; <CJK>
       (?$(GF((B . "0x536F") ; <CJK>
       (?$(GF)(B . "0x536E") ; <CJK>
       (?$(GF*(B . "0x53BB") ; <CJK>
       (?$(GF+(B . "0x53EF") ; <CJK>
       (?$(GF,(B . "0x53E4") ; <CJK>
       (?$(GF-(B . "0x53F3") ; <CJK>
       (?$(GF.(B . "0x53EC") ; <CJK>
       (?$(GF/(B . "0x53EE") ; <CJK>
       (?$(GF0(B . "0x53E9") ; <CJK>
       (?$(GF1(B . "0x53E8") ; <CJK>
       (?$(GF2(B . "0x53FC") ; <CJK>
       (?$(GF3(B . "0x53F8") ; <CJK>
       (?$(GF4(B . "0x53F5") ; <CJK>
       (?$(GF5(B . "0x53EB") ; <CJK>
       (?$(GF6(B . "0x53E6") ; <CJK>
       (?$(GF7(B . "0x53EA") ; <CJK>
       (?$(GF8(B . "0x53F2") ; <CJK>
       (?$(GF9(B . "0x53F1") ; <CJK>
       (?$(GF:(B . "0x53F0") ; <CJK>
       (?$(GF;(B . "0x53E5") ; <CJK>
       (?$(GF<(B . "0x53ED") ; <CJK>
       (?$(GF=(B . "0x53FB") ; <CJK>
       (?$(GF>(B . "0x56DB") ; <CJK>
       (?$(GF?(B . "0x56DA") ; <CJK>
       (?$(GF@(B . "0x5916") ; <CJK>
       (?$(GFA(B . "0x592E") ; <CJK>
       (?$(GFB(B . "0x5931") ; <CJK>
       (?$(GFC(B . "0x5974") ; <CJK>
       (?$(GFD(B . "0x5976") ; <CJK>
       (?$(GFE(B . "0x5B55") ; <CJK>
       (?$(GFF(B . "0x5B83") ; <CJK>
       (?$(GFG(B . "0x5C3C") ; <CJK>
       (?$(GFH(B . "0x5DE8") ; <CJK>
       (?$(GFI(B . "0x5DE7") ; <CJK>
       (?$(GFJ(B . "0x5DE6") ; <CJK>
       (?$(GFK(B . "0x5E02") ; <CJK>
       (?$(GFL(B . "0x5E03") ; <CJK>
       (?$(GFM(B . "0x5E73") ; <CJK>
       (?$(GFN(B . "0x5E7C") ; <CJK>
       (?$(GFO(B . "0x5F01") ; <CJK>
       (?$(GFP(B . "0x5F18") ; <CJK>
       (?$(GFQ(B . "0x5F17") ; <CJK>
       (?$(GFR(B . "0x5FC5") ; <CJK>
       (?$(GFS(B . "0x620A") ; <CJK>
       (?$(GFT(B . "0x6253") ; <CJK>
       (?$(GFU(B . "0x6254") ; <CJK>
       (?$(GFV(B . "0x6252") ; <CJK>
       (?$(GFW(B . "0x6251") ; <CJK>
       (?$(GFX(B . "0x65A5") ; <CJK>
       (?$(GFY(B . "0x65E6") ; <CJK>
       (?$(GFZ(B . "0x672E") ; <CJK>
       (?$(GF[(B . "0x672C") ; <CJK>
       (?$(GF\(B . "0x672A") ; <CJK>
       (?$(GF](B . "0x672B") ; <CJK>
       (?$(GF^(B . "0x672D") ; <CJK>
       (?$(GF_(B . "0x6B63") ; <CJK>
       (?$(GF`(B . "0x6BCD") ; <CJK>
       (?$(GFa(B . "0x6C11") ; <CJK>
       (?$(GFb(B . "0x6C10") ; <CJK>
       (?$(GFc(B . "0x6C38") ; <CJK>
       (?$(GFd(B . "0x6C41") ; <CJK>
       (?$(GFe(B . "0x6C40") ; <CJK>
       (?$(GFf(B . "0x6C3E") ; <CJK>
       (?$(GFg(B . "0x72AF") ; <CJK>
       (?$(GFh(B . "0x7384") ; <CJK>
       (?$(GFi(B . "0x7389") ; <CJK>
       (?$(GFj(B . "0x74DC") ; <CJK>
       (?$(GFk(B . "0x74E6") ; <CJK>
       (?$(GFl(B . "0x7518") ; <CJK>
       (?$(GFm(B . "0x751F") ; <CJK>
       (?$(GFn(B . "0x7528") ; <CJK>
       (?$(GFo(B . "0x7529") ; <CJK>
       (?$(GFp(B . "0x7530") ; <CJK>
       (?$(GFq(B . "0x7531") ; <CJK>
       (?$(GFr(B . "0x7532") ; <CJK>
       (?$(GFs(B . "0x7533") ; <CJK>
       (?$(GFt(B . "0x758B") ; <CJK>
       (?$(GFu(B . "0x767D") ; <CJK>
       (?$(GFv(B . "0x76AE") ; <CJK>
       (?$(GFw(B . "0x76BF") ; <CJK>
       (?$(GFx(B . "0x76EE") ; <CJK>
       (?$(GFy(B . "0x77DB") ; <CJK>
       (?$(GFz(B . "0x77E2") ; <CJK>
       (?$(GF{(B . "0x77F3") ; <CJK>
       (?$(GF|(B . "0x793A") ; <CJK>
       (?$(GF}(B . "0x79BE") ; <CJK>
       (?$(GF~(B . "0x7A74") ; <CJK>
       (?$(GG!(B . "0x7ACB") ; <CJK>
       (?$(GG"(B . "0x4E1E") ; <CJK>
       (?$(GG#(B . "0x4E1F") ; <CJK>
       (?$(GG$(B . "0x4E52") ; <CJK>
       (?$(GG%(B . "0x4E53") ; <CJK>
       (?$(GG&(B . "0x4E69") ; <CJK>
       (?$(GG'(B . "0x4E99") ; <CJK>
       (?$(GG((B . "0x4EA4") ; <CJK>
       (?$(GG)(B . "0x4EA6") ; <CJK>
       (?$(GG*(B . "0x4EA5") ; <CJK>
       (?$(GG+(B . "0x4EFF") ; <CJK>
       (?$(GG,(B . "0x4F09") ; <CJK>
       (?$(GG-(B . "0x4F19") ; <CJK>
       (?$(GG.(B . "0x4F0A") ; <CJK>
       (?$(GG/(B . "0x4F15") ; <CJK>
       (?$(GG0(B . "0x4F0D") ; <CJK>
       (?$(GG1(B . "0x4F10") ; <CJK>
       (?$(GG2(B . "0x4F11") ; <CJK>
       (?$(GG3(B . "0x4F0F") ; <CJK>
       (?$(GG4(B . "0x4EF2") ; <CJK>
       (?$(GG5(B . "0x4EF6") ; <CJK>
       (?$(GG6(B . "0x4EFB") ; <CJK>
       (?$(GG7(B . "0x4EF0") ; <CJK>
       (?$(GG8(B . "0x4EF3") ; <CJK>
       (?$(GG9(B . "0x4EFD") ; <CJK>
       (?$(GG:(B . "0x4F01") ; <CJK>
       (?$(GG;(B . "0x4F0B") ; <CJK>
       (?$(GG<(B . "0x5149") ; <CJK>
       (?$(GG=(B . "0x5147") ; <CJK>
       (?$(GG>(B . "0x5146") ; <CJK>
       (?$(GG?(B . "0x5148") ; <CJK>
       (?$(GG@(B . "0x5168") ; <CJK>
       (?$(GGA(B . "0x5171") ; <CJK>
       (?$(GGB(B . "0x518D") ; <CJK>
       (?$(GGC(B . "0x51B0") ; <CJK>
       (?$(GGD(B . "0x5217") ; <CJK>
       (?$(GGE(B . "0x5211") ; <CJK>
       (?$(GGF(B . "0x5212") ; <CJK>
       (?$(GGG(B . "0x520E") ; <CJK>
       (?$(GGH(B . "0x5216") ; <CJK>
       (?$(GGI(B . "0x52A3") ; <CJK>
       (?$(GGJ(B . "0x5308") ; <CJK>
       (?$(GGK(B . "0x5321") ; <CJK>
       (?$(GGL(B . "0x5320") ; <CJK>
       (?$(GGM(B . "0x5370") ; <CJK>
       (?$(GGN(B . "0x5371") ; <CJK>
       (?$(GGO(B . "0x5409") ; <CJK>
       (?$(GGP(B . "0x540F") ; <CJK>
       (?$(GGQ(B . "0x540C") ; <CJK>
       (?$(GGR(B . "0x540A") ; <CJK>
       (?$(GGS(B . "0x5410") ; <CJK>
       (?$(GGT(B . "0x5401") ; <CJK>
       (?$(GGU(B . "0x540B") ; <CJK>
       (?$(GGV(B . "0x5404") ; <CJK>
       (?$(GGW(B . "0x5411") ; <CJK>
       (?$(GGX(B . "0x540D") ; <CJK>
       (?$(GGY(B . "0x5408") ; <CJK>
       (?$(GGZ(B . "0x5403") ; <CJK>
       (?$(GG[(B . "0x540E") ; <CJK>
       (?$(GG\(B . "0x5406") ; <CJK>
       (?$(GG](B . "0x5412") ; <CJK>
       (?$(GG^(B . "0x56E0") ; <CJK>
       (?$(GG_(B . "0x56DE") ; <CJK>
       (?$(GG`(B . "0x56DD") ; <CJK>
       (?$(GGa(B . "0x5733") ; <CJK>
       (?$(GGb(B . "0x5730") ; <CJK>
       (?$(GGc(B . "0x5728") ; <CJK>
       (?$(GGd(B . "0x572D") ; <CJK>
       (?$(GGe(B . "0x572C") ; <CJK>
       (?$(GGf(B . "0x572F") ; <CJK>
       (?$(GGg(B . "0x5729") ; <CJK>
       (?$(GGh(B . "0x5919") ; <CJK>
       (?$(GGi(B . "0x591A") ; <CJK>
       (?$(GGj(B . "0x5937") ; <CJK>
       (?$(GGk(B . "0x5938") ; <CJK>
       (?$(GGl(B . "0x5984") ; <CJK>
       (?$(GGm(B . "0x5978") ; <CJK>
       (?$(GGn(B . "0x5983") ; <CJK>
       (?$(GGo(B . "0x597D") ; <CJK>
       (?$(GGp(B . "0x5979") ; <CJK>
       (?$(GGq(B . "0x5982") ; <CJK>
       (?$(GGr(B . "0x5981") ; <CJK>
       (?$(GGs(B . "0x5B57") ; <CJK>
       (?$(GGt(B . "0x5B58") ; <CJK>
       (?$(GGu(B . "0x5B87") ; <CJK>
       (?$(GGv(B . "0x5B88") ; <CJK>
       (?$(GGw(B . "0x5B85") ; <CJK>
       (?$(GGx(B . "0x5B89") ; <CJK>
       (?$(GGy(B . "0x5BFA") ; <CJK>
       (?$(GGz(B . "0x5C16") ; <CJK>
       (?$(GG{(B . "0x5C79") ; <CJK>
       (?$(GG|(B . "0x5DDE") ; <CJK>
       (?$(GG}(B . "0x5E06") ; <CJK>
       (?$(GG~(B . "0x5E76") ; <CJK>
       (?$(GH!(B . "0x5E74") ; <CJK>
       (?$(GH"(B . "0x5F0F") ; <CJK>
       (?$(GH#(B . "0x5F1B") ; <CJK>
       (?$(GH$(B . "0x5FD9") ; <CJK>
       (?$(GH%(B . "0x5FD6") ; <CJK>
       (?$(GH&(B . "0x620E") ; <CJK>
       (?$(GH'(B . "0x620C") ; <CJK>
       (?$(GH((B . "0x620D") ; <CJK>
       (?$(GH)(B . "0x6210") ; <CJK>
       (?$(GH*(B . "0x6263") ; <CJK>
       (?$(GH+(B . "0x625B") ; <CJK>
       (?$(GH,(B . "0x6258") ; <CJK>
       (?$(GH-(B . "0x6536") ; <CJK>
       (?$(GH.(B . "0x65E9") ; <CJK>
       (?$(GH/(B . "0x65E8") ; <CJK>
       (?$(GH0(B . "0x65EC") ; <CJK>
       (?$(GH1(B . "0x65ED") ; <CJK>
       (?$(GH2(B . "0x66F2") ; <CJK>
       (?$(GH3(B . "0x66F3") ; <CJK>
       (?$(GH4(B . "0x6709") ; <CJK>
       (?$(GH5(B . "0x673D") ; <CJK>
       (?$(GH6(B . "0x6734") ; <CJK>
       (?$(GH7(B . "0x6731") ; <CJK>
       (?$(GH8(B . "0x6735") ; <CJK>
       (?$(GH9(B . "0x6B21") ; <CJK>
       (?$(GH:(B . "0x6B64") ; <CJK>
       (?$(GH;(B . "0x6B7B") ; <CJK>
       (?$(GH<(B . "0x6C16") ; <CJK>
       (?$(GH=(B . "0x6C5D") ; <CJK>
       (?$(GH>(B . "0x6C57") ; <CJK>
       (?$(GH?(B . "0x6C59") ; <CJK>
       (?$(GH@(B . "0x6C5F") ; <CJK>
       (?$(GHA(B . "0x6C60") ; <CJK>
       (?$(GHB(B . "0x6C50") ; <CJK>
       (?$(GHC(B . "0x6C55") ; <CJK>
       (?$(GHD(B . "0x6C61") ; <CJK>
       (?$(GHE(B . "0x6C5B") ; <CJK>
       (?$(GHF(B . "0x6C4D") ; <CJK>
       (?$(GHG(B . "0x6C4E") ; <CJK>
       (?$(GHH(B . "0x7070") ; <CJK>
       (?$(GHI(B . "0x725F") ; <CJK>
       (?$(GHJ(B . "0x725D") ; <CJK>
       (?$(GHK(B . "0x767E") ; <CJK>
       (?$(GHL(B . "0x7AF9") ; <CJK>
       (?$(GHM(B . "0x7C73") ; <CJK>
       (?$(GHN(B . "0x7CF8") ; <CJK>
       (?$(GHO(B . "0x7F36") ; <CJK>
       (?$(GHP(B . "0x7F8A") ; <CJK>
       (?$(GHQ(B . "0x7FBD") ; <CJK>
       (?$(GHR(B . "0x8001") ; <CJK>
       (?$(GHS(B . "0x8003") ; <CJK>
       (?$(GHT(B . "0x800C") ; <CJK>
       (?$(GHU(B . "0x8012") ; <CJK>
       (?$(GHV(B . "0x8033") ; <CJK>
       (?$(GHW(B . "0x807F") ; <CJK>
       (?$(GHX(B . "0x8089") ; <CJK>
       (?$(GHY(B . "0x808B") ; <CJK>
       (?$(GHZ(B . "0x808C") ; <CJK>
       (?$(GH[(B . "0x81E3") ; <CJK>
       (?$(GH\(B . "0x81EA") ; <CJK>
       (?$(GH](B . "0x81F3") ; <CJK>
       (?$(GH^(B . "0x81FC") ; <CJK>
       (?$(GH_(B . "0x820C") ; <CJK>
       (?$(GH`(B . "0x821B") ; <CJK>
       (?$(GHa(B . "0x821F") ; <CJK>
       (?$(GHb(B . "0x826E") ; <CJK>
       (?$(GHc(B . "0x8272") ; <CJK>
       (?$(GHd(B . "0x827E") ; <CJK>
       (?$(GHe(B . "0x866B") ; <CJK>
       (?$(GHf(B . "0x8840") ; <CJK>
       (?$(GHg(B . "0x884C") ; <CJK>
       (?$(GHh(B . "0x8863") ; <CJK>
       (?$(GHi(B . "0x897F") ; <CJK>
       (?$(GHj(B . "0x9621") ; <CJK>
       (?$(GHk(B . "0x4E32") ; <CJK>
       (?$(GHl(B . "0x4EA8") ; <CJK>
       (?$(GHm(B . "0x4F4D") ; <CJK>
       (?$(GHn(B . "0x4F4F") ; <CJK>
       (?$(GHo(B . "0x4F47") ; <CJK>
       (?$(GHp(B . "0x4F57") ; <CJK>
       (?$(GHq(B . "0x4F5E") ; <CJK>
       (?$(GHr(B . "0x4F34") ; <CJK>
       (?$(GHs(B . "0x4F5B") ; <CJK>
       (?$(GHt(B . "0x4F55") ; <CJK>
       (?$(GHu(B . "0x4F30") ; <CJK>
       (?$(GHv(B . "0x4F50") ; <CJK>
       (?$(GHw(B . "0x4F51") ; <CJK>
       (?$(GHx(B . "0x4F3D") ; <CJK>
       (?$(GHy(B . "0x4F3A") ; <CJK>
       (?$(GHz(B . "0x4F38") ; <CJK>
       (?$(GH{(B . "0x4F43") ; <CJK>
       (?$(GH|(B . "0x4F54") ; <CJK>
       (?$(GH}(B . "0x4F3C") ; <CJK>
       (?$(GH~(B . "0x4F46") ; <CJK>
       (?$(GI!(B . "0x4F63") ; <CJK>
       (?$(GI"(B . "0x4F5C") ; <CJK>
       (?$(GI#(B . "0x4F60") ; <CJK>
       (?$(GI$(B . "0x4F2F") ; <CJK>
       (?$(GI%(B . "0x4F4E") ; <CJK>
       (?$(GI&(B . "0x4F36") ; <CJK>
       (?$(GI'(B . "0x4F59") ; <CJK>
       (?$(GI((B . "0x4F5D") ; <CJK>
       (?$(GI)(B . "0x4F48") ; <CJK>
       (?$(GI*(B . "0x4F5A") ; <CJK>
       (?$(GI+(B . "0x514C") ; <CJK>
       (?$(GI,(B . "0x514B") ; <CJK>
       (?$(GI-(B . "0x514D") ; <CJK>
       (?$(GI.(B . "0x5175") ; <CJK>
       (?$(GI/(B . "0x51B6") ; <CJK>
       (?$(GI0(B . "0x51B7") ; <CJK>
       (?$(GI1(B . "0x5225") ; <CJK>
       (?$(GI2(B . "0x5224") ; <CJK>
       (?$(GI3(B . "0x5229") ; <CJK>
       (?$(GI4(B . "0x522A") ; <CJK>
       (?$(GI5(B . "0x5228") ; <CJK>
       (?$(GI6(B . "0x52AB") ; <CJK>
       (?$(GI7(B . "0x52A9") ; <CJK>
       (?$(GI8(B . "0x52AA") ; <CJK>
       (?$(GI9(B . "0x52AC") ; <CJK>
       (?$(GI:(B . "0x5323") ; <CJK>
       (?$(GI;(B . "0x5373") ; <CJK>
       (?$(GI<(B . "0x5375") ; <CJK>
       (?$(GI=(B . "0x541D") ; <CJK>
       (?$(GI>(B . "0x542D") ; <CJK>
       (?$(GI?(B . "0x541E") ; <CJK>
       (?$(GI@(B . "0x543E") ; <CJK>
       (?$(GIA(B . "0x5426") ; <CJK>
       (?$(GIB(B . "0x544E") ; <CJK>
       (?$(GIC(B . "0x5427") ; <CJK>
       (?$(GID(B . "0x5446") ; <CJK>
       (?$(GIE(B . "0x5443") ; <CJK>
       (?$(GIF(B . "0x5433") ; <CJK>
       (?$(GIG(B . "0x5448") ; <CJK>
       (?$(GIH(B . "0x5442") ; <CJK>
       (?$(GII(B . "0x541B") ; <CJK>
       (?$(GIJ(B . "0x5429") ; <CJK>
       (?$(GIK(B . "0x544A") ; <CJK>
       (?$(GIL(B . "0x5439") ; <CJK>
       (?$(GIM(B . "0x543B") ; <CJK>
       (?$(GIN(B . "0x5438") ; <CJK>
       (?$(GIO(B . "0x542E") ; <CJK>
       (?$(GIP(B . "0x5435") ; <CJK>
       (?$(GIQ(B . "0x5436") ; <CJK>
       (?$(GIR(B . "0x5420") ; <CJK>
       (?$(GIS(B . "0x543C") ; <CJK>
       (?$(GIT(B . "0x5440") ; <CJK>
       (?$(GIU(B . "0x5431") ; <CJK>
       (?$(GIV(B . "0x542B") ; <CJK>
       (?$(GIW(B . "0x541F") ; <CJK>
       (?$(GIX(B . "0x542C") ; <CJK>
       (?$(GIY(B . "0x56EA") ; <CJK>
       (?$(GIZ(B . "0x56F0") ; <CJK>
       (?$(GI[(B . "0x56E4") ; <CJK>
       (?$(GI\(B . "0x56EB") ; <CJK>
       (?$(GI](B . "0x574A") ; <CJK>
       (?$(GI^(B . "0x5751") ; <CJK>
       (?$(GI_(B . "0x5740") ; <CJK>
       (?$(GI`(B . "0x574D") ; <CJK>
       (?$(GIa(B . "0x5747") ; <CJK>
       (?$(GIb(B . "0x574E") ; <CJK>
       (?$(GIc(B . "0x573E") ; <CJK>
       (?$(GId(B . "0x5750") ; <CJK>
       (?$(GIe(B . "0x574F") ; <CJK>
       (?$(GIf(B . "0x573B") ; <CJK>
       (?$(GIg(B . "0x58EF") ; <CJK>
       (?$(GIh(B . "0x593E") ; <CJK>
       (?$(GIi(B . "0x599D") ; <CJK>
       (?$(GIj(B . "0x5992") ; <CJK>
       (?$(GIk(B . "0x59A8") ; <CJK>
       (?$(GIl(B . "0x599E") ; <CJK>
       (?$(GIm(B . "0x59A3") ; <CJK>
       (?$(GIn(B . "0x5999") ; <CJK>
       (?$(GIo(B . "0x5996") ; <CJK>
       (?$(GIp(B . "0x598D") ; <CJK>
       (?$(GIq(B . "0x59A4") ; <CJK>
       (?$(GIr(B . "0x5993") ; <CJK>
       (?$(GIs(B . "0x598A") ; <CJK>
       (?$(GIt(B . "0x59A5") ; <CJK>
       (?$(GIu(B . "0x5B5D") ; <CJK>
       (?$(GIv(B . "0x5B5C") ; <CJK>
       (?$(GIw(B . "0x5B5A") ; <CJK>
       (?$(GIx(B . "0x5B5B") ; <CJK>
       (?$(GIy(B . "0x5B8C") ; <CJK>
       (?$(GIz(B . "0x5B8B") ; <CJK>
       (?$(GI{(B . "0x5B8F") ; <CJK>
       (?$(GI|(B . "0x5C2C") ; <CJK>
       (?$(GI}(B . "0x5C40") ; <CJK>
       (?$(GI~(B . "0x5C41") ; <CJK>
       (?$(GJ!(B . "0x5C3F") ; <CJK>
       (?$(GJ"(B . "0x5C3E") ; <CJK>
       (?$(GJ#(B . "0x5C90") ; <CJK>
       (?$(GJ$(B . "0x5C91") ; <CJK>
       (?$(GJ%(B . "0x5C94") ; <CJK>
       (?$(GJ&(B . "0x5C8C") ; <CJK>
       (?$(GJ'(B . "0x5DEB") ; <CJK>
       (?$(GJ((B . "0x5E0C") ; <CJK>
       (?$(GJ)(B . "0x5E8F") ; <CJK>
       (?$(GJ*(B . "0x5E87") ; <CJK>
       (?$(GJ+(B . "0x5E8A") ; <CJK>
       (?$(GJ,(B . "0x5EF7") ; <CJK>
       (?$(GJ-(B . "0x5F04") ; <CJK>
       (?$(GJ.(B . "0x5F1F") ; <CJK>
       (?$(GJ/(B . "0x5F64") ; <CJK>
       (?$(GJ0(B . "0x5F62") ; <CJK>
       (?$(GJ1(B . "0x5F77") ; <CJK>
       (?$(GJ2(B . "0x5F79") ; <CJK>
       (?$(GJ3(B . "0x5FD8") ; <CJK>
       (?$(GJ4(B . "0x5FCC") ; <CJK>
       (?$(GJ5(B . "0x5FD7") ; <CJK>
       (?$(GJ6(B . "0x5FCD") ; <CJK>
       (?$(GJ7(B . "0x5FF1") ; <CJK>
       (?$(GJ8(B . "0x5FEB") ; <CJK>
       (?$(GJ9(B . "0x5FF8") ; <CJK>
       (?$(GJ:(B . "0x5FEA") ; <CJK>
       (?$(GJ;(B . "0x6212") ; <CJK>
       (?$(GJ<(B . "0x6211") ; <CJK>
       (?$(GJ=(B . "0x6284") ; <CJK>
       (?$(GJ>(B . "0x6297") ; <CJK>
       (?$(GJ?(B . "0x6296") ; <CJK>
       (?$(GJ@(B . "0x6280") ; <CJK>
       (?$(GJA(B . "0x6276") ; <CJK>
       (?$(GJB(B . "0x6289") ; <CJK>
       (?$(GJC(B . "0x626D") ; <CJK>
       (?$(GJD(B . "0x628A") ; <CJK>
       (?$(GJE(B . "0x627C") ; <CJK>
       (?$(GJF(B . "0x627E") ; <CJK>
       (?$(GJG(B . "0x6279") ; <CJK>
       (?$(GJH(B . "0x6273") ; <CJK>
       (?$(GJI(B . "0x6292") ; <CJK>
       (?$(GJJ(B . "0x626F") ; <CJK>
       (?$(GJK(B . "0x6298") ; <CJK>
       (?$(GJL(B . "0x626E") ; <CJK>
       (?$(GJM(B . "0x6295") ; <CJK>
       (?$(GJN(B . "0x6293") ; <CJK>
       (?$(GJO(B . "0x6291") ; <CJK>
       (?$(GJP(B . "0x6286") ; <CJK>
       (?$(GJQ(B . "0x6539") ; <CJK>
       (?$(GJR(B . "0x653B") ; <CJK>
       (?$(GJS(B . "0x6538") ; <CJK>
       (?$(GJT(B . "0x65F1") ; <CJK>
       (?$(GJU(B . "0x66F4") ; <CJK>
       (?$(GJV(B . "0x675F") ; <CJK>
       (?$(GJW(B . "0x674E") ; <CJK>
       (?$(GJX(B . "0x674F") ; <CJK>
       (?$(GJY(B . "0x6750") ; <CJK>
       (?$(GJZ(B . "0x6751") ; <CJK>
       (?$(GJ[(B . "0x675C") ; <CJK>
       (?$(GJ\(B . "0x6756") ; <CJK>
       (?$(GJ](B . "0x675E") ; <CJK>
       (?$(GJ^(B . "0x6749") ; <CJK>
       (?$(GJ_(B . "0x6746") ; <CJK>
       (?$(GJ`(B . "0x6760") ; <CJK>
       (?$(GJa(B . "0x6753") ; <CJK>
       (?$(GJb(B . "0x6757") ; <CJK>
       (?$(GJc(B . "0x6B65") ; <CJK>
       (?$(GJd(B . "0x6BCF") ; <CJK>
       (?$(GJe(B . "0x6C42") ; <CJK>
       (?$(GJf(B . "0x6C5E") ; <CJK>
       (?$(GJg(B . "0x6C99") ; <CJK>
       (?$(GJh(B . "0x6C81") ; <CJK>
       (?$(GJi(B . "0x6C88") ; <CJK>
       (?$(GJj(B . "0x6C89") ; <CJK>
       (?$(GJk(B . "0x6C85") ; <CJK>
       (?$(GJl(B . "0x6C9B") ; <CJK>
       (?$(GJm(B . "0x6C6A") ; <CJK>
       (?$(GJn(B . "0x6C7A") ; <CJK>
       (?$(GJo(B . "0x6C90") ; <CJK>
       (?$(GJp(B . "0x6C70") ; <CJK>
       (?$(GJq(B . "0x6C8C") ; <CJK>
       (?$(GJr(B . "0x6C68") ; <CJK>
       (?$(GJs(B . "0x6C96") ; <CJK>
       (?$(GJt(B . "0x6C92") ; <CJK>
       (?$(GJu(B . "0x6C7D") ; <CJK>
       (?$(GJv(B . "0x6C83") ; <CJK>
       (?$(GJw(B . "0x6C72") ; <CJK>
       (?$(GJx(B . "0x6C7E") ; <CJK>
       (?$(GJy(B . "0x6C74") ; <CJK>
       (?$(GJz(B . "0x6C86") ; <CJK>
       (?$(GJ{(B . "0x6C76") ; <CJK>
       (?$(GJ|(B . "0x6C8D") ; <CJK>
       (?$(GJ}(B . "0x6C94") ; <CJK>
       (?$(GJ~(B . "0x6C98") ; <CJK>
       (?$(GK!(B . "0x6C82") ; <CJK>
       (?$(GK"(B . "0x7076") ; <CJK>
       (?$(GK#(B . "0x707C") ; <CJK>
       (?$(GK$(B . "0x707D") ; <CJK>
       (?$(GK%(B . "0x7078") ; <CJK>
       (?$(GK&(B . "0x7262") ; <CJK>
       (?$(GK'(B . "0x7261") ; <CJK>
       (?$(GK((B . "0x7260") ; <CJK>
       (?$(GK)(B . "0x72C4") ; <CJK>
       (?$(GK*(B . "0x72C2") ; <CJK>
       (?$(GK+(B . "0x7396") ; <CJK>
       (?$(GK,(B . "0x752C") ; <CJK>
       (?$(GK-(B . "0x752B") ; <CJK>
       (?$(GK.(B . "0x7537") ; <CJK>
       (?$(GK/(B . "0x7538") ; <CJK>
       (?$(GK0(B . "0x7682") ; <CJK>
       (?$(GK1(B . "0x76EF") ; <CJK>
       (?$(GK2(B . "0x77E3") ; <CJK>
       (?$(GK3(B . "0x79C1") ; <CJK>
       (?$(GK4(B . "0x79C0") ; <CJK>
       (?$(GK5(B . "0x79BF") ; <CJK>
       (?$(GK6(B . "0x7A76") ; <CJK>
       (?$(GK7(B . "0x7CFB") ; <CJK>
       (?$(GK8(B . "0x7F55") ; <CJK>
       (?$(GK9(B . "0x8096") ; <CJK>
       (?$(GK:(B . "0x8093") ; <CJK>
       (?$(GK;(B . "0x809D") ; <CJK>
       (?$(GK<(B . "0x8098") ; <CJK>
       (?$(GK=(B . "0x809B") ; <CJK>
       (?$(GK>(B . "0x809A") ; <CJK>
       (?$(GK?(B . "0x80B2") ; <CJK>
       (?$(GK@(B . "0x826F") ; <CJK>
       (?$(GKA(B . "0x8292") ; <CJK>
       (?$(GKB(B . "0x828B") ; <CJK>
       (?$(GKC(B . "0x828D") ; <CJK>
       (?$(GKD(B . "0x898B") ; <CJK>
       (?$(GKE(B . "0x89D2") ; <CJK>
       (?$(GKF(B . "0x8A00") ; <CJK>
       (?$(GKG(B . "0x8C37") ; <CJK>
       (?$(GKH(B . "0x8C46") ; <CJK>
       (?$(GKI(B . "0x8C55") ; <CJK>
       (?$(GKJ(B . "0x8C9D") ; <CJK>
       (?$(GKK(B . "0x8D64") ; <CJK>
       (?$(GKL(B . "0x8D70") ; <CJK>
       (?$(GKM(B . "0x8DB3") ; <CJK>
       (?$(GKN(B . "0x8EAB") ; <CJK>
       (?$(GKO(B . "0x8ECA") ; <CJK>
       (?$(GKP(B . "0x8F9B") ; <CJK>
       (?$(GKQ(B . "0x8FB0") ; <CJK>
       (?$(GKR(B . "0x8FC2") ; <CJK>
       (?$(GKS(B . "0x8FC6") ; <CJK>
       (?$(GKT(B . "0x8FC5") ; <CJK>
       (?$(GKU(B . "0x8FC4") ; <CJK>
       (?$(GKV(B . "0x5DE1") ; <CJK>
       (?$(GKW(B . "0x9091") ; <CJK>
       (?$(GKX(B . "0x90A2") ; <CJK>
       (?$(GKY(B . "0x90AA") ; <CJK>
       (?$(GKZ(B . "0x90A6") ; <CJK>
       (?$(GK[(B . "0x90A3") ; <CJK>
       (?$(GK\(B . "0x9149") ; <CJK>
       (?$(GK](B . "0x91C6") ; <CJK>
       (?$(GK^(B . "0x91CC") ; <CJK>
       (?$(GK_(B . "0x9632") ; <CJK>
       (?$(GK`(B . "0x962E") ; <CJK>
       (?$(GKa(B . "0x9631") ; <CJK>
       (?$(GKb(B . "0x962A") ; <CJK>
       (?$(GKc(B . "0x962C") ; <CJK>
       (?$(GKd(B . "0x4E26") ; <CJK>
       (?$(GKe(B . "0x4E56") ; <CJK>
       (?$(GKf(B . "0x4E73") ; <CJK>
       (?$(GKg(B . "0x4E8B") ; <CJK>
       (?$(GKh(B . "0x4E9B") ; <CJK>
       (?$(GKi(B . "0x4E9E") ; <CJK>
       (?$(GKj(B . "0x4EAB") ; <CJK>
       (?$(GKk(B . "0x4EAC") ; <CJK>
       (?$(GKl(B . "0x4F6F") ; <CJK>
       (?$(GKm(B . "0x4F9D") ; <CJK>
       (?$(GKn(B . "0x4F8D") ; <CJK>
       (?$(GKo(B . "0x4F73") ; <CJK>
       (?$(GKp(B . "0x4F7F") ; <CJK>
       (?$(GKq(B . "0x4F6C") ; <CJK>
       (?$(GKr(B . "0x4F9B") ; <CJK>
       (?$(GKs(B . "0x4F8B") ; <CJK>
       (?$(GKt(B . "0x4F86") ; <CJK>
       (?$(GKu(B . "0x4F83") ; <CJK>
       (?$(GKv(B . "0x4F70") ; <CJK>
       (?$(GKw(B . "0x4F75") ; <CJK>
       (?$(GKx(B . "0x4F88") ; <CJK>
       (?$(GKy(B . "0x4F69") ; <CJK>
       (?$(GKz(B . "0x4F7B") ; <CJK>
       (?$(GK{(B . "0x4F96") ; <CJK>
       (?$(GK|(B . "0x4F7E") ; <CJK>
       (?$(GK}(B . "0x4F8F") ; <CJK>
       (?$(GK~(B . "0x4F91") ; <CJK>
       (?$(GL!(B . "0x4F7A") ; <CJK>
       (?$(GL"(B . "0x5154") ; <CJK>
       (?$(GL#(B . "0x5152") ; <CJK>
       (?$(GL$(B . "0x5155") ; <CJK>
       (?$(GL%(B . "0x5169") ; <CJK>
       (?$(GL&(B . "0x5177") ; <CJK>
       (?$(GL'(B . "0x5176") ; <CJK>
       (?$(GL((B . "0x5178") ; <CJK>
       (?$(GL)(B . "0x51BD") ; <CJK>
       (?$(GL*(B . "0x51FD") ; <CJK>
       (?$(GL+(B . "0x523B") ; <CJK>
       (?$(GL,(B . "0x5238") ; <CJK>
       (?$(GL-(B . "0x5237") ; <CJK>
       (?$(GL.(B . "0x523A") ; <CJK>
       (?$(GL/(B . "0x5230") ; <CJK>
       (?$(GL0(B . "0x522E") ; <CJK>
       (?$(GL1(B . "0x5236") ; <CJK>
       (?$(GL2(B . "0x5241") ; <CJK>
       (?$(GL3(B . "0x52BE") ; <CJK>
       (?$(GL4(B . "0x52BB") ; <CJK>
       (?$(GL5(B . "0x5352") ; <CJK>
       (?$(GL6(B . "0x5354") ; <CJK>
       (?$(GL7(B . "0x5353") ; <CJK>
       (?$(GL8(B . "0x5351") ; <CJK>
       (?$(GL9(B . "0x5366") ; <CJK>
       (?$(GL:(B . "0x5377") ; <CJK>
       (?$(GL;(B . "0x5378") ; <CJK>
       (?$(GL<(B . "0x5379") ; <CJK>
       (?$(GL=(B . "0x53D6") ; <CJK>
       (?$(GL>(B . "0x53D4") ; <CJK>
       (?$(GL?(B . "0x53D7") ; <CJK>
       (?$(GL@(B . "0x5473") ; <CJK>
       (?$(GLA(B . "0x5475") ; <CJK>
       (?$(GLB(B . "0x5496") ; <CJK>
       (?$(GLC(B . "0x5478") ; <CJK>
       (?$(GLD(B . "0x5495") ; <CJK>
       (?$(GLE(B . "0x5480") ; <CJK>
       (?$(GLF(B . "0x547B") ; <CJK>
       (?$(GLG(B . "0x5477") ; <CJK>
       (?$(GLH(B . "0x5484") ; <CJK>
       (?$(GLI(B . "0x5492") ; <CJK>
       (?$(GLJ(B . "0x5486") ; <CJK>
       (?$(GLK(B . "0x547C") ; <CJK>
       (?$(GLL(B . "0x5490") ; <CJK>
       (?$(GLM(B . "0x5471") ; <CJK>
       (?$(GLN(B . "0x5476") ; <CJK>
       (?$(GLO(B . "0x548C") ; <CJK>
       (?$(GLP(B . "0x549A") ; <CJK>
       (?$(GLQ(B . "0x5462") ; <CJK>
       (?$(GLR(B . "0x5468") ; <CJK>
       (?$(GLS(B . "0x548B") ; <CJK>
       (?$(GLT(B . "0x547D") ; <CJK>
       (?$(GLU(B . "0x548E") ; <CJK>
       (?$(GLV(B . "0x56FA") ; <CJK>
       (?$(GLW(B . "0x5783") ; <CJK>
       (?$(GLX(B . "0x5777") ; <CJK>
       (?$(GLY(B . "0x576A") ; <CJK>
       (?$(GLZ(B . "0x5769") ; <CJK>
       (?$(GL[(B . "0x5761") ; <CJK>
       (?$(GL\(B . "0x5766") ; <CJK>
       (?$(GL](B . "0x5764") ; <CJK>
       (?$(GL^(B . "0x577C") ; <CJK>
       (?$(GL_(B . "0x591C") ; <CJK>
       (?$(GL`(B . "0x5949") ; <CJK>
       (?$(GLa(B . "0x5947") ; <CJK>
       (?$(GLb(B . "0x5948") ; <CJK>
       (?$(GLc(B . "0x5944") ; <CJK>
       (?$(GLd(B . "0x5954") ; <CJK>
       (?$(GLe(B . "0x59BE") ; <CJK>
       (?$(GLf(B . "0x59BB") ; <CJK>
       (?$(GLg(B . "0x59D4") ; <CJK>
       (?$(GLh(B . "0x59B9") ; <CJK>
       (?$(GLi(B . "0x59AE") ; <CJK>
       (?$(GLj(B . "0x59D1") ; <CJK>
       (?$(GLk(B . "0x59C6") ; <CJK>
       (?$(GLl(B . "0x59D0") ; <CJK>
       (?$(GLm(B . "0x59CD") ; <CJK>
       (?$(GLn(B . "0x59CB") ; <CJK>
       (?$(GLo(B . "0x59D3") ; <CJK>
       (?$(GLp(B . "0x59CA") ; <CJK>
       (?$(GLq(B . "0x59AF") ; <CJK>
       (?$(GLr(B . "0x59B3") ; <CJK>
       (?$(GLs(B . "0x59D2") ; <CJK>
       (?$(GLt(B . "0x59C5") ; <CJK>
       (?$(GLu(B . "0x5B5F") ; <CJK>
       (?$(GLv(B . "0x5B64") ; <CJK>
       (?$(GLw(B . "0x5B63") ; <CJK>
       (?$(GLx(B . "0x5B97") ; <CJK>
       (?$(GLy(B . "0x5B9A") ; <CJK>
       (?$(GLz(B . "0x5B98") ; <CJK>
       (?$(GL{(B . "0x5B9C") ; <CJK>
       (?$(GL|(B . "0x5B99") ; <CJK>
       (?$(GL}(B . "0x5B9B") ; <CJK>
       (?$(GL~(B . "0x5C1A") ; <CJK>
       (?$(GM!(B . "0x5C48") ; <CJK>
       (?$(GM"(B . "0x5C45") ; <CJK>
       (?$(GM#(B . "0x5C46") ; <CJK>
       (?$(GM$(B . "0x5CB7") ; <CJK>
       (?$(GM%(B . "0x5CA1") ; <CJK>
       (?$(GM&(B . "0x5CB8") ; <CJK>
       (?$(GM'(B . "0x5CA9") ; <CJK>
       (?$(GM((B . "0x5CAB") ; <CJK>
       (?$(GM)(B . "0x5CB1") ; <CJK>
       (?$(GM*(B . "0x5CB3") ; <CJK>
       (?$(GM+(B . "0x5E18") ; <CJK>
       (?$(GM,(B . "0x5E1A") ; <CJK>
       (?$(GM-(B . "0x5E16") ; <CJK>
       (?$(GM.(B . "0x5E15") ; <CJK>
       (?$(GM/(B . "0x5E1B") ; <CJK>
       (?$(GM0(B . "0x5E11") ; <CJK>
       (?$(GM1(B . "0x5E78") ; <CJK>
       (?$(GM2(B . "0x5E9A") ; <CJK>
       (?$(GM3(B . "0x5E97") ; <CJK>
       (?$(GM4(B . "0x5E9C") ; <CJK>
       (?$(GM5(B . "0x5E95") ; <CJK>
       (?$(GM6(B . "0x5E96") ; <CJK>
       (?$(GM7(B . "0x5EF6") ; <CJK>
       (?$(GM8(B . "0x5F26") ; <CJK>
       (?$(GM9(B . "0x5F27") ; <CJK>
       (?$(GM:(B . "0x5F29") ; <CJK>
       (?$(GM;(B . "0x5F80") ; <CJK>
       (?$(GM<(B . "0x5F81") ; <CJK>
       (?$(GM=(B . "0x5F7F") ; <CJK>
       (?$(GM>(B . "0x5F7C") ; <CJK>
       (?$(GM?(B . "0x5FDD") ; <CJK>
       (?$(GM@(B . "0x5FE0") ; <CJK>
       (?$(GMA(B . "0x5FFD") ; <CJK>
       (?$(GMB(B . "0x5FF5") ; <CJK>
       (?$(GMC(B . "0x5FFF") ; <CJK>
       (?$(GMD(B . "0x600F") ; <CJK>
       (?$(GME(B . "0x6014") ; <CJK>
       (?$(GMF(B . "0x602F") ; <CJK>
       (?$(GMG(B . "0x6035") ; <CJK>
       (?$(GMH(B . "0x6016") ; <CJK>
       (?$(GMI(B . "0x602A") ; <CJK>
       (?$(GMJ(B . "0x6015") ; <CJK>
       (?$(GMK(B . "0x6021") ; <CJK>
       (?$(GML(B . "0x6027") ; <CJK>
       (?$(GMM(B . "0x6029") ; <CJK>
       (?$(GMN(B . "0x602B") ; <CJK>
       (?$(GMO(B . "0x601B") ; <CJK>
       (?$(GMP(B . "0x6216") ; <CJK>
       (?$(GMQ(B . "0x6215") ; <CJK>
       (?$(GMR(B . "0x623F") ; <CJK>
       (?$(GMS(B . "0x623E") ; <CJK>
       (?$(GMT(B . "0x6240") ; <CJK>
       (?$(GMU(B . "0x627F") ; <CJK>
       (?$(GMV(B . "0x62C9") ; <CJK>
       (?$(GMW(B . "0x62CC") ; <CJK>
       (?$(GMX(B . "0x62C4") ; <CJK>
       (?$(GMY(B . "0x62BF") ; <CJK>
       (?$(GMZ(B . "0x62C2") ; <CJK>
       (?$(GM[(B . "0x62B9") ; <CJK>
       (?$(GM\(B . "0x62D2") ; <CJK>
       (?$(GM](B . "0x62DB") ; <CJK>
       (?$(GM^(B . "0x62AB") ; <CJK>
       (?$(GM_(B . "0x62D3") ; <CJK>
       (?$(GM`(B . "0x62D4") ; <CJK>
       (?$(GMa(B . "0x62CB") ; <CJK>
       (?$(GMb(B . "0x62C8") ; <CJK>
       (?$(GMc(B . "0x62A8") ; <CJK>
       (?$(GMd(B . "0x62BD") ; <CJK>
       (?$(GMe(B . "0x62BC") ; <CJK>
       (?$(GMf(B . "0x62D0") ; <CJK>
       (?$(GMg(B . "0x62D9") ; <CJK>
       (?$(GMh(B . "0x62C7") ; <CJK>
       (?$(GMi(B . "0x62CD") ; <CJK>
       (?$(GMj(B . "0x62B5") ; <CJK>
       (?$(GMk(B . "0x62DA") ; <CJK>
       (?$(GMl(B . "0x62B1") ; <CJK>
       (?$(GMm(B . "0x62D8") ; <CJK>
       (?$(GMn(B . "0x62D6") ; <CJK>
       (?$(GMo(B . "0x62D7") ; <CJK>
       (?$(GMp(B . "0x62C6") ; <CJK>
       (?$(GMq(B . "0x62AC") ; <CJK>
       (?$(GMr(B . "0x62CE") ; <CJK>
       (?$(GMs(B . "0x653E") ; <CJK>
       (?$(GMt(B . "0x65A7") ; <CJK>
       (?$(GMu(B . "0x65BC") ; <CJK>
       (?$(GMv(B . "0x65FA") ; <CJK>
       (?$(GMw(B . "0x6614") ; <CJK>
       (?$(GMx(B . "0x6613") ; <CJK>
       (?$(GMy(B . "0x660C") ; <CJK>
       (?$(GMz(B . "0x6606") ; <CJK>
       (?$(GM{(B . "0x6602") ; <CJK>
       (?$(GM|(B . "0x660E") ; <CJK>
       (?$(GM}(B . "0x6600") ; <CJK>
       (?$(GM~(B . "0x660F") ; <CJK>
       (?$(GN!(B . "0x6615") ; <CJK>
       (?$(GN"(B . "0x660A") ; <CJK>
       (?$(GN#(B . "0x6607") ; <CJK>
       (?$(GN$(B . "0x670D") ; <CJK>
       (?$(GN%(B . "0x670B") ; <CJK>
       (?$(GN&(B . "0x676D") ; <CJK>
       (?$(GN'(B . "0x678B") ; <CJK>
       (?$(GN((B . "0x6795") ; <CJK>
       (?$(GN)(B . "0x6771") ; <CJK>
       (?$(GN*(B . "0x679C") ; <CJK>
       (?$(GN+(B . "0x6773") ; <CJK>
       (?$(GN,(B . "0x6777") ; <CJK>
       (?$(GN-(B . "0x6787") ; <CJK>
       (?$(GN.(B . "0x679D") ; <CJK>
       (?$(GN/(B . "0x6797") ; <CJK>
       (?$(GN0(B . "0x676F") ; <CJK>
       (?$(GN1(B . "0x6770") ; <CJK>
       (?$(GN2(B . "0x677F") ; <CJK>
       (?$(GN3(B . "0x6789") ; <CJK>
       (?$(GN4(B . "0x677E") ; <CJK>
       (?$(GN5(B . "0x6790") ; <CJK>
       (?$(GN6(B . "0x6775") ; <CJK>
       (?$(GN7(B . "0x679A") ; <CJK>
       (?$(GN8(B . "0x6793") ; <CJK>
       (?$(GN9(B . "0x677C") ; <CJK>
       (?$(GN:(B . "0x676A") ; <CJK>
       (?$(GN;(B . "0x6772") ; <CJK>
       (?$(GN<(B . "0x6B23") ; <CJK>
       (?$(GN=(B . "0x6B66") ; <CJK>
       (?$(GN>(B . "0x6B67") ; <CJK>
       (?$(GN?(B . "0x6B7F") ; <CJK>
       (?$(GN@(B . "0x6C13") ; <CJK>
       (?$(GNA(B . "0x6C1B") ; <CJK>
       (?$(GNB(B . "0x6CE3") ; <CJK>
       (?$(GNC(B . "0x6CE8") ; <CJK>
       (?$(GND(B . "0x6CF3") ; <CJK>
       (?$(GNE(B . "0x6CB1") ; <CJK>
       (?$(GNF(B . "0x6CCC") ; <CJK>
       (?$(GNG(B . "0x6CE5") ; <CJK>
       (?$(GNH(B . "0x6CB3") ; <CJK>
       (?$(GNI(B . "0x6CBD") ; <CJK>
       (?$(GNJ(B . "0x6CBE") ; <CJK>
       (?$(GNK(B . "0x6CBC") ; <CJK>
       (?$(GNL(B . "0x6CE2") ; <CJK>
       (?$(GNM(B . "0x6CAB") ; <CJK>
       (?$(GNN(B . "0x6CD5") ; <CJK>
       (?$(GNO(B . "0x6CD3") ; <CJK>
       (?$(GNP(B . "0x6CB8") ; <CJK>
       (?$(GNQ(B . "0x6CC4") ; <CJK>
       (?$(GNR(B . "0x6CB9") ; <CJK>
       (?$(GNS(B . "0x6CC1") ; <CJK>
       (?$(GNT(B . "0x6CAE") ; <CJK>
       (?$(GNU(B . "0x6CD7") ; <CJK>
       (?$(GNV(B . "0x6CC5") ; <CJK>
       (?$(GNW(B . "0x6CF1") ; <CJK>
       (?$(GNX(B . "0x6CBF") ; <CJK>
       (?$(GNY(B . "0x6CBB") ; <CJK>
       (?$(GNZ(B . "0x6CE1") ; <CJK>
       (?$(GN[(B . "0x6CDB") ; <CJK>
       (?$(GN\(B . "0x6CCA") ; <CJK>
       (?$(GN](B . "0x6CAC") ; <CJK>
       (?$(GN^(B . "0x6CEF") ; <CJK>
       (?$(GN_(B . "0x6CDC") ; <CJK>
       (?$(GN`(B . "0x6CD6") ; <CJK>
       (?$(GNa(B . "0x6CE0") ; <CJK>
       (?$(GNb(B . "0x7095") ; <CJK>
       (?$(GNc(B . "0x708E") ; <CJK>
       (?$(GNd(B . "0x7092") ; <CJK>
       (?$(GNe(B . "0x708A") ; <CJK>
       (?$(GNf(B . "0x7099") ; <CJK>
       (?$(GNg(B . "0x722C") ; <CJK>
       (?$(GNh(B . "0x722D") ; <CJK>
       (?$(GNi(B . "0x7238") ; <CJK>
       (?$(GNj(B . "0x7248") ; <CJK>
       (?$(GNk(B . "0x7267") ; <CJK>
       (?$(GNl(B . "0x7269") ; <CJK>
       (?$(GNm(B . "0x72C0") ; <CJK>
       (?$(GNn(B . "0x72CE") ; <CJK>
       (?$(GNo(B . "0x72D9") ; <CJK>
       (?$(GNp(B . "0x72D7") ; <CJK>
       (?$(GNq(B . "0x72D0") ; <CJK>
       (?$(GNr(B . "0x73A9") ; <CJK>
       (?$(GNs(B . "0x73A8") ; <CJK>
       (?$(GNt(B . "0x739F") ; <CJK>
       (?$(GNu(B . "0x73AB") ; <CJK>
       (?$(GNv(B . "0x73A5") ; <CJK>
       (?$(GNw(B . "0x753D") ; <CJK>
       (?$(GNx(B . "0x759D") ; <CJK>
       (?$(GNy(B . "0x7599") ; <CJK>
       (?$(GNz(B . "0x759A") ; <CJK>
       (?$(GN{(B . "0x7684") ; <CJK>
       (?$(GN|(B . "0x76C2") ; <CJK>
       (?$(GN}(B . "0x76F2") ; <CJK>
       (?$(GN~(B . "0x76F4") ; <CJK>
       (?$(GO!(B . "0x77E5") ; <CJK>
       (?$(GO"(B . "0x77FD") ; <CJK>
       (?$(GO#(B . "0x793E") ; <CJK>
       (?$(GO$(B . "0x7940") ; <CJK>
       (?$(GO%(B . "0x7941") ; <CJK>
       (?$(GO&(B . "0x79C9") ; <CJK>
       (?$(GO'(B . "0x79C8") ; <CJK>
       (?$(GO((B . "0x7A7A") ; <CJK>
       (?$(GO)(B . "0x7A79") ; <CJK>
       (?$(GO*(B . "0x7AFA") ; <CJK>
       (?$(GO+(B . "0x7CFE") ; <CJK>
       (?$(GO,(B . "0x7F54") ; <CJK>
       (?$(GO-(B . "0x7F8C") ; <CJK>
       (?$(GO.(B . "0x7F8B") ; <CJK>
       (?$(GO/(B . "0x8005") ; <CJK>
       (?$(GO0(B . "0x80BA") ; <CJK>
       (?$(GO1(B . "0x80A5") ; <CJK>
       (?$(GO2(B . "0x80A2") ; <CJK>
       (?$(GO3(B . "0x80B1") ; <CJK>
       (?$(GO4(B . "0x80A1") ; <CJK>
       (?$(GO5(B . "0x80AB") ; <CJK>
       (?$(GO6(B . "0x80A9") ; <CJK>
       (?$(GO7(B . "0x80B4") ; <CJK>
       (?$(GO8(B . "0x80AA") ; <CJK>
       (?$(GO9(B . "0x80AF") ; <CJK>
       (?$(GO:(B . "0x81E5") ; <CJK>
       (?$(GO;(B . "0x81FE") ; <CJK>
       (?$(GO<(B . "0x820D") ; <CJK>
       (?$(GO=(B . "0x82B3") ; <CJK>
       (?$(GO>(B . "0x829D") ; <CJK>
       (?$(GO?(B . "0x8299") ; <CJK>
       (?$(GO@(B . "0x82AD") ; <CJK>
       (?$(GOA(B . "0x82BD") ; <CJK>
       (?$(GOB(B . "0x829F") ; <CJK>
       (?$(GOC(B . "0x82B9") ; <CJK>
       (?$(GOD(B . "0x82B1") ; <CJK>
       (?$(GOE(B . "0x82AC") ; <CJK>
       (?$(GOF(B . "0x82A5") ; <CJK>
       (?$(GOG(B . "0x82AF") ; <CJK>
       (?$(GOH(B . "0x82B8") ; <CJK>
       (?$(GOI(B . "0x82A3") ; <CJK>
       (?$(GOJ(B . "0x82B0") ; <CJK>
       (?$(GOK(B . "0x82BE") ; <CJK>
       (?$(GOL(B . "0x82B7") ; <CJK>
       (?$(GOM(B . "0x864E") ; <CJK>
       (?$(GON(B . "0x8671") ; <CJK>
       (?$(GOO(B . "0x521D") ; <CJK>
       (?$(GOP(B . "0x8868") ; <CJK>
       (?$(GOQ(B . "0x8ECB") ; <CJK>
       (?$(GOR(B . "0x8FCE") ; <CJK>
       (?$(GOS(B . "0x8FD4") ; <CJK>
       (?$(GOT(B . "0x8FD1") ; <CJK>
       (?$(GOU(B . "0x90B5") ; <CJK>
       (?$(GOV(B . "0x90B8") ; <CJK>
       (?$(GOW(B . "0x90B1") ; <CJK>
       (?$(GOX(B . "0x90B6") ; <CJK>
       (?$(GOY(B . "0x91C7") ; <CJK>
       (?$(GOZ(B . "0x91D1") ; <CJK>
       (?$(GO[(B . "0x9577") ; <CJK>
       (?$(GO\(B . "0x9580") ; <CJK>
       (?$(GO](B . "0x961C") ; <CJK>
       (?$(GO^(B . "0x9640") ; <CJK>
       (?$(GO_(B . "0x963F") ; <CJK>
       (?$(GO`(B . "0x963B") ; <CJK>
       (?$(GOa(B . "0x9644") ; <CJK>
       (?$(GOb(B . "0x9642") ; <CJK>
       (?$(GOc(B . "0x96B9") ; <CJK>
       (?$(GOd(B . "0x96E8") ; <CJK>
       (?$(GOe(B . "0x9752") ; <CJK>
       (?$(GOf(B . "0x975E") ; <CJK>
       (?$(GOg(B . "0x4E9F") ; <CJK>
       (?$(GOh(B . "0x4EAD") ; <CJK>
       (?$(GOi(B . "0x4EAE") ; <CJK>
       (?$(GOj(B . "0x4FE1") ; <CJK>
       (?$(GOk(B . "0x4FB5") ; <CJK>
       (?$(GOl(B . "0x4FAF") ; <CJK>
       (?$(GOm(B . "0x4FBF") ; <CJK>
       (?$(GOn(B . "0x4FE0") ; <CJK>
       (?$(GOo(B . "0x4FD1") ; <CJK>
       (?$(GOp(B . "0x4FCF") ; <CJK>
       (?$(GOq(B . "0x4FDD") ; <CJK>
       (?$(GOr(B . "0x4FC3") ; <CJK>
       (?$(GOs(B . "0x4FB6") ; <CJK>
       (?$(GOt(B . "0x4FD8") ; <CJK>
       (?$(GOu(B . "0x4FDF") ; <CJK>
       (?$(GOv(B . "0x4FCA") ; <CJK>
       (?$(GOw(B . "0x4FD7") ; <CJK>
       (?$(GOx(B . "0x4FAE") ; <CJK>
       (?$(GOy(B . "0x4FD0") ; <CJK>
       (?$(GOz(B . "0x4FC4") ; <CJK>
       (?$(GO{(B . "0x4FC2") ; <CJK>
       (?$(GO|(B . "0x4FDA") ; <CJK>
       (?$(GO}(B . "0x4FCE") ; <CJK>
       (?$(GO~(B . "0x4FDE") ; <CJK>
       (?$(GP!(B . "0x4FB7") ; <CJK>
       (?$(GP"(B . "0x5157") ; <CJK>
       (?$(GP#(B . "0x5192") ; <CJK>
       (?$(GP$(B . "0x5191") ; <CJK>
       (?$(GP%(B . "0x51A0") ; <CJK>
       (?$(GP&(B . "0x524E") ; <CJK>
       (?$(GP'(B . "0x5243") ; <CJK>
       (?$(GP((B . "0x524A") ; <CJK>
       (?$(GP)(B . "0x524D") ; <CJK>
       (?$(GP*(B . "0x524C") ; <CJK>
       (?$(GP+(B . "0x524B") ; <CJK>
       (?$(GP,(B . "0x5247") ; <CJK>
       (?$(GP-(B . "0x52C7") ; <CJK>
       (?$(GP.(B . "0x52C9") ; <CJK>
       (?$(GP/(B . "0x52C3") ; <CJK>
       (?$(GP0(B . "0x52C1") ; <CJK>
       (?$(GP1(B . "0x530D") ; <CJK>
       (?$(GP2(B . "0x5357") ; <CJK>
       (?$(GP3(B . "0x537B") ; <CJK>
       (?$(GP4(B . "0x539A") ; <CJK>
       (?$(GP5(B . "0x53DB") ; <CJK>
       (?$(GP6(B . "0x54AC") ; <CJK>
       (?$(GP7(B . "0x54C0") ; <CJK>
       (?$(GP8(B . "0x54A8") ; <CJK>
       (?$(GP9(B . "0x54CE") ; <CJK>
       (?$(GP:(B . "0x54C9") ; <CJK>
       (?$(GP;(B . "0x54B8") ; <CJK>
       (?$(GP<(B . "0x54A6") ; <CJK>
       (?$(GP=(B . "0x54B3") ; <CJK>
       (?$(GP>(B . "0x54C7") ; <CJK>
       (?$(GP?(B . "0x54C2") ; <CJK>
       (?$(GP@(B . "0x54BD") ; <CJK>
       (?$(GPA(B . "0x54AA") ; <CJK>
       (?$(GPB(B . "0x54C1") ; <CJK>
       (?$(GPC(B . "0x54C4") ; <CJK>
       (?$(GPD(B . "0x54C8") ; <CJK>
       (?$(GPE(B . "0x54AF") ; <CJK>
       (?$(GPF(B . "0x54AB") ; <CJK>
       (?$(GPG(B . "0x54B1") ; <CJK>
       (?$(GPH(B . "0x54BB") ; <CJK>
       (?$(GPI(B . "0x54A9") ; <CJK>
       (?$(GPJ(B . "0x54A7") ; <CJK>
       (?$(GPK(B . "0x54BF") ; <CJK>
       (?$(GPL(B . "0x56FF") ; <CJK>
       (?$(GPM(B . "0x5782") ; <CJK>
       (?$(GPN(B . "0x578B") ; <CJK>
       (?$(GPO(B . "0x57A0") ; <CJK>
       (?$(GPP(B . "0x57A3") ; <CJK>
       (?$(GPQ(B . "0x57A2") ; <CJK>
       (?$(GPR(B . "0x57CE") ; <CJK>
       (?$(GPS(B . "0x57AE") ; <CJK>
       (?$(GPT(B . "0x5793") ; <CJK>
       (?$(GPU(B . "0x5955") ; <CJK>
       (?$(GPV(B . "0x5951") ; <CJK>
       (?$(GPW(B . "0x594F") ; <CJK>
       (?$(GPX(B . "0x594E") ; <CJK>
       (?$(GPY(B . "0x5950") ; <CJK>
       (?$(GPZ(B . "0x59DC") ; <CJK>
       (?$(GP[(B . "0x59D8") ; <CJK>
       (?$(GP\(B . "0x59FF") ; <CJK>
       (?$(GP](B . "0x59E3") ; <CJK>
       (?$(GP^(B . "0x59E8") ; <CJK>
       (?$(GP_(B . "0x5A03") ; <CJK>
       (?$(GP`(B . "0x59E5") ; <CJK>
       (?$(GPa(B . "0x59EA") ; <CJK>
       (?$(GPb(B . "0x59DA") ; <CJK>
       (?$(GPc(B . "0x59E6") ; <CJK>
       (?$(GPd(B . "0x5A01") ; <CJK>
       (?$(GPe(B . "0x59FB") ; <CJK>
       (?$(GPf(B . "0x5B69") ; <CJK>
       (?$(GPg(B . "0x5BA3") ; <CJK>
       (?$(GPh(B . "0x5BA6") ; <CJK>
       (?$(GPi(B . "0x5BA4") ; <CJK>
       (?$(GPj(B . "0x5BA2") ; <CJK>
       (?$(GPk(B . "0x5BA5") ; <CJK>
       (?$(GPl(B . "0x5C01") ; <CJK>
       (?$(GPm(B . "0x5C4E") ; <CJK>
       (?$(GPn(B . "0x5C4F") ; <CJK>
       (?$(GPo(B . "0x5C4D") ; <CJK>
       (?$(GPp(B . "0x5C4B") ; <CJK>
       (?$(GPq(B . "0x5CD9") ; <CJK>
       (?$(GPr(B . "0x5CD2") ; <CJK>
       (?$(GPs(B . "0x5DF7") ; <CJK>
       (?$(GPt(B . "0x5E1D") ; <CJK>
       (?$(GPu(B . "0x5E25") ; <CJK>
       (?$(GPv(B . "0x5E1F") ; <CJK>
       (?$(GPw(B . "0x5E7D") ; <CJK>
       (?$(GPx(B . "0x5EA0") ; <CJK>
       (?$(GPy(B . "0x5EA6") ; <CJK>
       (?$(GPz(B . "0x5EFA") ; <CJK>
       (?$(GP{(B . "0x5F08") ; <CJK>
       (?$(GP|(B . "0x5F2D") ; <CJK>
       (?$(GP}(B . "0x5F65") ; <CJK>
       (?$(GP~(B . "0x5F88") ; <CJK>
       (?$(GQ!(B . "0x5F85") ; <CJK>
       (?$(GQ"(B . "0x5F8A") ; <CJK>
       (?$(GQ#(B . "0x5F8B") ; <CJK>
       (?$(GQ$(B . "0x5F87") ; <CJK>
       (?$(GQ%(B . "0x5F8C") ; <CJK>
       (?$(GQ&(B . "0x5F89") ; <CJK>
       (?$(GQ'(B . "0x6012") ; <CJK>
       (?$(GQ((B . "0x601D") ; <CJK>
       (?$(GQ)(B . "0x6020") ; <CJK>
       (?$(GQ*(B . "0x6025") ; <CJK>
       (?$(GQ+(B . "0x600E") ; <CJK>
       (?$(GQ,(B . "0x6028") ; <CJK>
       (?$(GQ-(B . "0x604D") ; <CJK>
       (?$(GQ.(B . "0x6070") ; <CJK>
       (?$(GQ/(B . "0x6068") ; <CJK>
       (?$(GQ0(B . "0x6062") ; <CJK>
       (?$(GQ1(B . "0x6046") ; <CJK>
       (?$(GQ2(B . "0x6043") ; <CJK>
       (?$(GQ3(B . "0x606C") ; <CJK>
       (?$(GQ4(B . "0x606B") ; <CJK>
       (?$(GQ5(B . "0x606A") ; <CJK>
       (?$(GQ6(B . "0x6064") ; <CJK>
       (?$(GQ7(B . "0x6241") ; <CJK>
       (?$(GQ8(B . "0x62DC") ; <CJK>
       (?$(GQ9(B . "0x6316") ; <CJK>
       (?$(GQ:(B . "0x6309") ; <CJK>
       (?$(GQ;(B . "0x62FC") ; <CJK>
       (?$(GQ<(B . "0x62ED") ; <CJK>
       (?$(GQ=(B . "0x6301") ; <CJK>
       (?$(GQ>(B . "0x62EE") ; <CJK>
       (?$(GQ?(B . "0x62FD") ; <CJK>
       (?$(GQ@(B . "0x6307") ; <CJK>
       (?$(GQA(B . "0x62F1") ; <CJK>
       (?$(GQB(B . "0x62F7") ; <CJK>
       (?$(GQC(B . "0x62EF") ; <CJK>
       (?$(GQD(B . "0x62EC") ; <CJK>
       (?$(GQE(B . "0x62FE") ; <CJK>
       (?$(GQF(B . "0x62F4") ; <CJK>
       (?$(GQG(B . "0x6311") ; <CJK>
       (?$(GQH(B . "0x6302") ; <CJK>
       (?$(GQI(B . "0x653F") ; <CJK>
       (?$(GQJ(B . "0x6545") ; <CJK>
       (?$(GQK(B . "0x65AB") ; <CJK>
       (?$(GQL(B . "0x65BD") ; <CJK>
       (?$(GQM(B . "0x65E2") ; <CJK>
       (?$(GQN(B . "0x6625") ; <CJK>
       (?$(GQO(B . "0x662D") ; <CJK>
       (?$(GQP(B . "0x6620") ; <CJK>
       (?$(GQQ(B . "0x6627") ; <CJK>
       (?$(GQR(B . "0x662F") ; <CJK>
       (?$(GQS(B . "0x661F") ; <CJK>
       (?$(GQT(B . "0x6628") ; <CJK>
       (?$(GQU(B . "0x6631") ; <CJK>
       (?$(GQV(B . "0x6624") ; <CJK>
       (?$(GQW(B . "0x66F7") ; <CJK>
       (?$(GQX(B . "0x67FF") ; <CJK>
       (?$(GQY(B . "0x67D3") ; <CJK>
       (?$(GQZ(B . "0x67F1") ; <CJK>
       (?$(GQ[(B . "0x67D4") ; <CJK>
       (?$(GQ\(B . "0x67D0") ; <CJK>
       (?$(GQ](B . "0x67EC") ; <CJK>
       (?$(GQ^(B . "0x67B6") ; <CJK>
       (?$(GQ_(B . "0x67AF") ; <CJK>
       (?$(GQ`(B . "0x67F5") ; <CJK>
       (?$(GQa(B . "0x67E9") ; <CJK>
       (?$(GQb(B . "0x67EF") ; <CJK>
       (?$(GQc(B . "0x67C4") ; <CJK>
       (?$(GQd(B . "0x67D1") ; <CJK>
       (?$(GQe(B . "0x67B4") ; <CJK>
       (?$(GQf(B . "0x67DA") ; <CJK>
       (?$(GQg(B . "0x67E5") ; <CJK>
       (?$(GQh(B . "0x67B8") ; <CJK>
       (?$(GQi(B . "0x67CF") ; <CJK>
       (?$(GQj(B . "0x67DE") ; <CJK>
       (?$(GQk(B . "0x67F3") ; <CJK>
       (?$(GQl(B . "0x67B0") ; <CJK>
       (?$(GQm(B . "0x67D9") ; <CJK>
       (?$(GQn(B . "0x67E2") ; <CJK>
       (?$(GQo(B . "0x67DD") ; <CJK>
       (?$(GQp(B . "0x67D2") ; <CJK>
       (?$(GQq(B . "0x6B6A") ; <CJK>
       (?$(GQr(B . "0x6B83") ; <CJK>
       (?$(GQs(B . "0x6B86") ; <CJK>
       (?$(GQt(B . "0x6BB5") ; <CJK>
       (?$(GQu(B . "0x6BD2") ; <CJK>
       (?$(GQv(B . "0x6BD7") ; <CJK>
       (?$(GQw(B . "0x6C1F") ; <CJK>
       (?$(GQx(B . "0x6CC9") ; <CJK>
       (?$(GQy(B . "0x6D0B") ; <CJK>
       (?$(GQz(B . "0x6D32") ; <CJK>
       (?$(GQ{(B . "0x6D2A") ; <CJK>
       (?$(GQ|(B . "0x6D41") ; <CJK>
       (?$(GQ}(B . "0x6D25") ; <CJK>
       (?$(GQ~(B . "0x6D0C") ; <CJK>
       (?$(GR!(B . "0x6D31") ; <CJK>
       (?$(GR"(B . "0x6D1E") ; <CJK>
       (?$(GR#(B . "0x6D17") ; <CJK>
       (?$(GR$(B . "0x6D3B") ; <CJK>
       (?$(GR%(B . "0x6D3D") ; <CJK>
       (?$(GR&(B . "0x6D3E") ; <CJK>
       (?$(GR'(B . "0x6D36") ; <CJK>
       (?$(GR((B . "0x6D1B") ; <CJK>
       (?$(GR)(B . "0x6CF5") ; <CJK>
       (?$(GR*(B . "0x6D39") ; <CJK>
       (?$(GR+(B . "0x6D27") ; <CJK>
       (?$(GR,(B . "0x6D38") ; <CJK>
       (?$(GR-(B . "0x6D29") ; <CJK>
       (?$(GR.(B . "0x6D2E") ; <CJK>
       (?$(GR/(B . "0x6D35") ; <CJK>
       (?$(GR0(B . "0x6D0E") ; <CJK>
       (?$(GR1(B . "0x6D2B") ; <CJK>
       (?$(GR2(B . "0x70AB") ; <CJK>
       (?$(GR3(B . "0x70BA") ; <CJK>
       (?$(GR4(B . "0x70B3") ; <CJK>
       (?$(GR5(B . "0x70AC") ; <CJK>
       (?$(GR6(B . "0x70AF") ; <CJK>
       (?$(GR7(B . "0x70AD") ; <CJK>
       (?$(GR8(B . "0x70B8") ; <CJK>
       (?$(GR9(B . "0x70AE") ; <CJK>
       (?$(GR:(B . "0x70A4") ; <CJK>
       (?$(GR;(B . "0x7230") ; <CJK>
       (?$(GR<(B . "0x7272") ; <CJK>
       (?$(GR=(B . "0x726F") ; <CJK>
       (?$(GR>(B . "0x7274") ; <CJK>
       (?$(GR?(B . "0x72E9") ; <CJK>
       (?$(GR@(B . "0x72E0") ; <CJK>
       (?$(GRA(B . "0x72E1") ; <CJK>
       (?$(GRB(B . "0x73B7") ; <CJK>
       (?$(GRC(B . "0x73CA") ; <CJK>
       (?$(GRD(B . "0x73BB") ; <CJK>
       (?$(GRE(B . "0x73B2") ; <CJK>
       (?$(GRF(B . "0x73CD") ; <CJK>
       (?$(GRG(B . "0x73C0") ; <CJK>
       (?$(GRH(B . "0x73B3") ; <CJK>
       (?$(GRI(B . "0x751A") ; <CJK>
       (?$(GRJ(B . "0x752D") ; <CJK>
       (?$(GRK(B . "0x754F") ; <CJK>
       (?$(GRL(B . "0x754C") ; <CJK>
       (?$(GRM(B . "0x754E") ; <CJK>
       (?$(GRN(B . "0x754B") ; <CJK>
       (?$(GRO(B . "0x75AB") ; <CJK>
       (?$(GRP(B . "0x75A4") ; <CJK>
       (?$(GRQ(B . "0x75A5") ; <CJK>
       (?$(GRR(B . "0x75A2") ; <CJK>
       (?$(GRS(B . "0x75A3") ; <CJK>
       (?$(GRT(B . "0x7678") ; <CJK>
       (?$(GRU(B . "0x7686") ; <CJK>
       (?$(GRV(B . "0x7687") ; <CJK>
       (?$(GRW(B . "0x7688") ; <CJK>
       (?$(GRX(B . "0x76C8") ; <CJK>
       (?$(GRY(B . "0x76C6") ; <CJK>
       (?$(GRZ(B . "0x76C3") ; <CJK>
       (?$(GR[(B . "0x76C5") ; <CJK>
       (?$(GR\(B . "0x7701") ; <CJK>
       (?$(GR](B . "0x76F9") ; <CJK>
       (?$(GR^(B . "0x76F8") ; <CJK>
       (?$(GR_(B . "0x7709") ; <CJK>
       (?$(GR`(B . "0x770B") ; <CJK>
       (?$(GRa(B . "0x76FE") ; <CJK>
       (?$(GRb(B . "0x76FC") ; <CJK>
       (?$(GRc(B . "0x7707") ; <CJK>
       (?$(GRd(B . "0x77DC") ; <CJK>
       (?$(GRe(B . "0x7802") ; <CJK>
       (?$(GRf(B . "0x7814") ; <CJK>
       (?$(GRg(B . "0x780C") ; <CJK>
       (?$(GRh(B . "0x780D") ; <CJK>
       (?$(GRi(B . "0x7946") ; <CJK>
       (?$(GRj(B . "0x7949") ; <CJK>
       (?$(GRk(B . "0x7948") ; <CJK>
       (?$(GRl(B . "0x7947") ; <CJK>
       (?$(GRm(B . "0x79B9") ; <CJK>
       (?$(GRn(B . "0x79BA") ; <CJK>
       (?$(GRo(B . "0x79D1") ; <CJK>
       (?$(GRp(B . "0x79D2") ; <CJK>
       (?$(GRq(B . "0x79CB") ; <CJK>
       (?$(GRr(B . "0x7A7F") ; <CJK>
       (?$(GRs(B . "0x7A81") ; <CJK>
       (?$(GRt(B . "0x7AFF") ; <CJK>
       (?$(GRu(B . "0x7AFD") ; <CJK>
       (?$(GRv(B . "0x7C7D") ; <CJK>
       (?$(GRw(B . "0x7D02") ; <CJK>
       (?$(GRx(B . "0x7D05") ; <CJK>
       (?$(GRy(B . "0x7D00") ; <CJK>
       (?$(GRz(B . "0x7D09") ; <CJK>
       (?$(GR{(B . "0x7D07") ; <CJK>
       (?$(GR|(B . "0x7D04") ; <CJK>
       (?$(GR}(B . "0x7D06") ; <CJK>
       (?$(GR~(B . "0x7F38") ; <CJK>
       (?$(GS!(B . "0x7F8E") ; <CJK>
       (?$(GS"(B . "0x7FBF") ; <CJK>
       (?$(GS#(B . "0x8010") ; <CJK>
       (?$(GS$(B . "0x800D") ; <CJK>
       (?$(GS%(B . "0x8011") ; <CJK>
       (?$(GS&(B . "0x8036") ; <CJK>
       (?$(GS'(B . "0x80D6") ; <CJK>
       (?$(GS((B . "0x80E5") ; <CJK>
       (?$(GS)(B . "0x80DA") ; <CJK>
       (?$(GS*(B . "0x80C3") ; <CJK>
       (?$(GS+(B . "0x80C4") ; <CJK>
       (?$(GS,(B . "0x80CC") ; <CJK>
       (?$(GS-(B . "0x80E1") ; <CJK>
       (?$(GS.(B . "0x80DB") ; <CJK>
       (?$(GS/(B . "0x80CE") ; <CJK>
       (?$(GS0(B . "0x80DE") ; <CJK>
       (?$(GS1(B . "0x80E4") ; <CJK>
       (?$(GS2(B . "0x80DD") ; <CJK>
       (?$(GS3(B . "0x81F4") ; <CJK>
       (?$(GS4(B . "0x8222") ; <CJK>
       (?$(GS5(B . "0x82E7") ; <CJK>
       (?$(GS6(B . "0x8303") ; <CJK>
       (?$(GS7(B . "0x8305") ; <CJK>
       (?$(GS8(B . "0x82E3") ; <CJK>
       (?$(GS9(B . "0x82DB") ; <CJK>
       (?$(GS:(B . "0x82E6") ; <CJK>
       (?$(GS;(B . "0x8304") ; <CJK>
       (?$(GS<(B . "0x82E5") ; <CJK>
       (?$(GS=(B . "0x8302") ; <CJK>
       (?$(GS>(B . "0x8309") ; <CJK>
       (?$(GS?(B . "0x82D2") ; <CJK>
       (?$(GS@(B . "0x82D7") ; <CJK>
       (?$(GSA(B . "0x82F1") ; <CJK>
       (?$(GSB(B . "0x8301") ; <CJK>
       (?$(GSC(B . "0x82DC") ; <CJK>
       (?$(GSD(B . "0x82D4") ; <CJK>
       (?$(GSE(B . "0x82D1") ; <CJK>
       (?$(GSF(B . "0x82DE") ; <CJK>
       (?$(GSG(B . "0x82D3") ; <CJK>
       (?$(GSH(B . "0x82DF") ; <CJK>
       (?$(GSI(B . "0x82EF") ; <CJK>
       (?$(GSJ(B . "0x8306") ; <CJK>
       (?$(GSK(B . "0x8650") ; <CJK>
       (?$(GSL(B . "0x8679") ; <CJK>
       (?$(GSM(B . "0x867B") ; <CJK>
       (?$(GSN(B . "0x867A") ; <CJK>
       (?$(GSO(B . "0x884D") ; <CJK>
       (?$(GSP(B . "0x886B") ; <CJK>
       (?$(GSQ(B . "0x8981") ; <CJK>
       (?$(GSR(B . "0x89D4") ; <CJK>
       (?$(GSS(B . "0x8A08") ; <CJK>
       (?$(GST(B . "0x8A02") ; <CJK>
       (?$(GSU(B . "0x8A03") ; <CJK>
       (?$(GSV(B . "0x8C9E") ; <CJK>
       (?$(GSW(B . "0x8CA0") ; <CJK>
       (?$(GSX(B . "0x8D74") ; <CJK>
       (?$(GSY(B . "0x8D73") ; <CJK>
       (?$(GSZ(B . "0x8DB4") ; <CJK>
       (?$(GS[(B . "0x8ECD") ; <CJK>
       (?$(GS\(B . "0x8ECC") ; <CJK>
       (?$(GS](B . "0x8FF0") ; <CJK>
       (?$(GS^(B . "0x8FE6") ; <CJK>
       (?$(GS_(B . "0x8FE2") ; <CJK>
       (?$(GS`(B . "0x8FEA") ; <CJK>
       (?$(GSa(B . "0x8FE5") ; <CJK>
       (?$(GSb(B . "0x8FED") ; <CJK>
       (?$(GSc(B . "0x8FEB") ; <CJK>
       (?$(GSd(B . "0x8FE4") ; <CJK>
       (?$(GSe(B . "0x8FE8") ; <CJK>
       (?$(GSf(B . "0x90CA") ; <CJK>
       (?$(GSg(B . "0x90CE") ; <CJK>
       (?$(GSh(B . "0x90C1") ; <CJK>
       (?$(GSi(B . "0x90C3") ; <CJK>
       (?$(GSj(B . "0x914B") ; <CJK>
       (?$(GSk(B . "0x914A") ; <CJK>
       (?$(GSl(B . "0x91CD") ; <CJK>
       (?$(GSm(B . "0x9582") ; <CJK>
       (?$(GSn(B . "0x9650") ; <CJK>
       (?$(GSo(B . "0x964B") ; <CJK>
       (?$(GSp(B . "0x964C") ; <CJK>
       (?$(GSq(B . "0x964D") ; <CJK>
       (?$(GSr(B . "0x9762") ; <CJK>
       (?$(GSs(B . "0x9769") ; <CJK>
       (?$(GSt(B . "0x97CB") ; <CJK>
       (?$(GSu(B . "0x97ED") ; <CJK>
       (?$(GSv(B . "0x97F3") ; <CJK>
       (?$(GSw(B . "0x9801") ; <CJK>
       (?$(GSx(B . "0x98A8") ; <CJK>
       (?$(GSy(B . "0x98DB") ; <CJK>
       (?$(GSz(B . "0x98DF") ; <CJK>
       (?$(GS{(B . "0x9996") ; <CJK>
       (?$(GS|(B . "0x9999") ; <CJK>
       (?$(GS}(B . "0x4E58") ; <CJK>
       (?$(GS~(B . "0x4EB3") ; <CJK>
       (?$(GT!(B . "0x500C") ; <CJK>
       (?$(GT"(B . "0x500D") ; <CJK>
       (?$(GT#(B . "0x5023") ; <CJK>
       (?$(GT$(B . "0x4FEF") ; <CJK>
       (?$(GT%(B . "0x5026") ; <CJK>
       (?$(GT&(B . "0x5025") ; <CJK>
       (?$(GT'(B . "0x4FF8") ; <CJK>
       (?$(GT((B . "0x5029") ; <CJK>
       (?$(GT)(B . "0x5016") ; <CJK>
       (?$(GT*(B . "0x5006") ; <CJK>
       (?$(GT+(B . "0x503C") ; <CJK>
       (?$(GT,(B . "0x501F") ; <CJK>
       (?$(GT-(B . "0x501A") ; <CJK>
       (?$(GT.(B . "0x5012") ; <CJK>
       (?$(GT/(B . "0x5011") ; <CJK>
       (?$(GT0(B . "0x4FFA") ; <CJK>
       (?$(GT1(B . "0x5000") ; <CJK>
       (?$(GT2(B . "0x5014") ; <CJK>
       (?$(GT3(B . "0x5028") ; <CJK>
       (?$(GT4(B . "0x4FF1") ; <CJK>
       (?$(GT5(B . "0x5021") ; <CJK>
       (?$(GT6(B . "0x500B") ; <CJK>
       (?$(GT7(B . "0x5019") ; <CJK>
       (?$(GT8(B . "0x5018") ; <CJK>
       (?$(GT9(B . "0x4FF3") ; <CJK>
       (?$(GT:(B . "0x4FEE") ; <CJK>
       (?$(GT;(B . "0x502D") ; <CJK>
       (?$(GT<(B . "0x502A") ; <CJK>
       (?$(GT=(B . "0x4FFE") ; <CJK>
       (?$(GT>(B . "0x502B") ; <CJK>
       (?$(GT?(B . "0x5009") ; <CJK>
       (?$(GT@(B . "0x517C") ; <CJK>
       (?$(GTA(B . "0x51A4") ; <CJK>
       (?$(GTB(B . "0x51A5") ; <CJK>
       (?$(GTC(B . "0x51A2") ; <CJK>
       (?$(GTD(B . "0x51CD") ; <CJK>
       (?$(GTE(B . "0x51CC") ; <CJK>
       (?$(GTF(B . "0x51C6") ; <CJK>
       (?$(GTG(B . "0x51CB") ; <CJK>
       (?$(GTH(B . "0x5256") ; <CJK>
       (?$(GTI(B . "0x525C") ; <CJK>
       (?$(GTJ(B . "0x5254") ; <CJK>
       (?$(GTK(B . "0x525B") ; <CJK>
       (?$(GTL(B . "0x525D") ; <CJK>
       (?$(GTM(B . "0x532A") ; <CJK>
       (?$(GTN(B . "0x537F") ; <CJK>
       (?$(GTO(B . "0x539F") ; <CJK>
       (?$(GTP(B . "0x539D") ; <CJK>
       (?$(GTQ(B . "0x53DF") ; <CJK>
       (?$(GTR(B . "0x54E8") ; <CJK>
       (?$(GTS(B . "0x5510") ; <CJK>
       (?$(GTT(B . "0x5501") ; <CJK>
       (?$(GTU(B . "0x5537") ; <CJK>
       (?$(GTV(B . "0x54FC") ; <CJK>
       (?$(GTW(B . "0x54E5") ; <CJK>
       (?$(GTX(B . "0x54F2") ; <CJK>
       (?$(GTY(B . "0x5506") ; <CJK>
       (?$(GTZ(B . "0x54FA") ; <CJK>
       (?$(GT[(B . "0x5514") ; <CJK>
       (?$(GT\(B . "0x54E9") ; <CJK>
       (?$(GT](B . "0x54ED") ; <CJK>
       (?$(GT^(B . "0x54E1") ; <CJK>
       (?$(GT_(B . "0x5509") ; <CJK>
       (?$(GT`(B . "0x54EE") ; <CJK>
       (?$(GTa(B . "0x54EA") ; <CJK>
       (?$(GTb(B . "0x54E6") ; <CJK>
       (?$(GTc(B . "0x5527") ; <CJK>
       (?$(GTd(B . "0x5507") ; <CJK>
       (?$(GTe(B . "0x54FD") ; <CJK>
       (?$(GTf(B . "0x550F") ; <CJK>
       (?$(GTg(B . "0x5703") ; <CJK>
       (?$(GTh(B . "0x5704") ; <CJK>
       (?$(GTi(B . "0x57C2") ; <CJK>
       (?$(GTj(B . "0x57D4") ; <CJK>
       (?$(GTk(B . "0x57CB") ; <CJK>
       (?$(GTl(B . "0x57C3") ; <CJK>
       (?$(GTm(B . "0x5809") ; <CJK>
       (?$(GTn(B . "0x590F") ; <CJK>
       (?$(GTo(B . "0x5957") ; <CJK>
       (?$(GTp(B . "0x5958") ; <CJK>
       (?$(GTq(B . "0x595A") ; <CJK>
       (?$(GTr(B . "0x5A11") ; <CJK>
       (?$(GTs(B . "0x5A18") ; <CJK>
       (?$(GTt(B . "0x5A1C") ; <CJK>
       (?$(GTu(B . "0x5A1F") ; <CJK>
       (?$(GTv(B . "0x5A1B") ; <CJK>
       (?$(GTw(B . "0x5A13") ; <CJK>
       (?$(GTx(B . "0x59EC") ; <CJK>
       (?$(GTy(B . "0x5A20") ; <CJK>
       (?$(GTz(B . "0x5A23") ; <CJK>
       (?$(GT{(B . "0x5A29") ; <CJK>
       (?$(GT|(B . "0x5A25") ; <CJK>
       (?$(GT}(B . "0x5A0C") ; <CJK>
       (?$(GT~(B . "0x5A09") ; <CJK>
       (?$(GU!(B . "0x5B6B") ; <CJK>
       (?$(GU"(B . "0x5C58") ; <CJK>
       (?$(GU#(B . "0x5BB0") ; <CJK>
       (?$(GU$(B . "0x5BB3") ; <CJK>
       (?$(GU%(B . "0x5BB6") ; <CJK>
       (?$(GU&(B . "0x5BB4") ; <CJK>
       (?$(GU'(B . "0x5BAE") ; <CJK>
       (?$(GU((B . "0x5BB5") ; <CJK>
       (?$(GU)(B . "0x5BB9") ; <CJK>
       (?$(GU*(B . "0x5BB8") ; <CJK>
       (?$(GU+(B . "0x5C04") ; <CJK>
       (?$(GU,(B . "0x5C51") ; <CJK>
       (?$(GU-(B . "0x5C55") ; <CJK>
       (?$(GU.(B . "0x5C50") ; <CJK>
       (?$(GU/(B . "0x5CED") ; <CJK>
       (?$(GU0(B . "0x5CFD") ; <CJK>
       (?$(GU1(B . "0x5CFB") ; <CJK>
       (?$(GU2(B . "0x5CEA") ; <CJK>
       (?$(GU3(B . "0x5CE8") ; <CJK>
       (?$(GU4(B . "0x5CF0") ; <CJK>
       (?$(GU5(B . "0x5CF6") ; <CJK>
       (?$(GU6(B . "0x5D01") ; <CJK>
       (?$(GU7(B . "0x5CF4") ; <CJK>
       (?$(GU8(B . "0x5DEE") ; <CJK>
       (?$(GU9(B . "0x5E2D") ; <CJK>
       (?$(GU:(B . "0x5E2B") ; <CJK>
       (?$(GU;(B . "0x5EAB") ; <CJK>
       (?$(GU<(B . "0x5EAD") ; <CJK>
       (?$(GU=(B . "0x5EA7") ; <CJK>
       (?$(GU>(B . "0x5F31") ; <CJK>
       (?$(GU?(B . "0x5F92") ; <CJK>
       (?$(GU@(B . "0x5F91") ; <CJK>
       (?$(GUA(B . "0x5F90") ; <CJK>
       (?$(GUB(B . "0x6059") ; <CJK>
       (?$(GUC(B . "0x6063") ; <CJK>
       (?$(GUD(B . "0x6065") ; <CJK>
       (?$(GUE(B . "0x6050") ; <CJK>
       (?$(GUF(B . "0x6055") ; <CJK>
       (?$(GUG(B . "0x606D") ; <CJK>
       (?$(GUH(B . "0x6069") ; <CJK>
       (?$(GUI(B . "0x606F") ; <CJK>
       (?$(GUJ(B . "0x6084") ; <CJK>
       (?$(GUK(B . "0x609F") ; <CJK>
       (?$(GUL(B . "0x609A") ; <CJK>
       (?$(GUM(B . "0x608D") ; <CJK>
       (?$(GUN(B . "0x6094") ; <CJK>
       (?$(GUO(B . "0x608C") ; <CJK>
       (?$(GUP(B . "0x6085") ; <CJK>
       (?$(GUQ(B . "0x6096") ; <CJK>
       (?$(GUR(B . "0x6247") ; <CJK>
       (?$(GUS(B . "0x62F3") ; <CJK>
       (?$(GUT(B . "0x6308") ; <CJK>
       (?$(GUU(B . "0x62FF") ; <CJK>
       (?$(GUV(B . "0x634E") ; <CJK>
       (?$(GUW(B . "0x633E") ; <CJK>
       (?$(GUX(B . "0x632F") ; <CJK>
       (?$(GUY(B . "0x6355") ; <CJK>
       (?$(GUZ(B . "0x6342") ; <CJK>
       (?$(GU[(B . "0x6346") ; <CJK>
       (?$(GU\(B . "0x634F") ; <CJK>
       (?$(GU](B . "0x6349") ; <CJK>
       (?$(GU^(B . "0x633A") ; <CJK>
       (?$(GU_(B . "0x6350") ; <CJK>
       (?$(GU`(B . "0x633D") ; <CJK>
       (?$(GUa(B . "0x632A") ; <CJK>
       (?$(GUb(B . "0x632B") ; <CJK>
       (?$(GUc(B . "0x6328") ; <CJK>
       (?$(GUd(B . "0x634D") ; <CJK>
       (?$(GUe(B . "0x634C") ; <CJK>
       (?$(GUf(B . "0x6548") ; <CJK>
       (?$(GUg(B . "0x6549") ; <CJK>
       (?$(GUh(B . "0x6599") ; <CJK>
       (?$(GUi(B . "0x65C1") ; <CJK>
       (?$(GUj(B . "0x65C5") ; <CJK>
       (?$(GUk(B . "0x6642") ; <CJK>
       (?$(GUl(B . "0x6649") ; <CJK>
       (?$(GUm(B . "0x664F") ; <CJK>
       (?$(GUn(B . "0x6643") ; <CJK>
       (?$(GUo(B . "0x6652") ; <CJK>
       (?$(GUp(B . "0x664C") ; <CJK>
       (?$(GUq(B . "0x6645") ; <CJK>
       (?$(GUr(B . "0x6641") ; <CJK>
       (?$(GUs(B . "0x66F8") ; <CJK>
       (?$(GUt(B . "0x6714") ; <CJK>
       (?$(GUu(B . "0x6715") ; <CJK>
       (?$(GUv(B . "0x6717") ; <CJK>
       (?$(GUw(B . "0x6821") ; <CJK>
       (?$(GUx(B . "0x6838") ; <CJK>
       (?$(GUy(B . "0x6848") ; <CJK>
       (?$(GUz(B . "0x6846") ; <CJK>
       (?$(GU{(B . "0x6853") ; <CJK>
       (?$(GU|(B . "0x6839") ; <CJK>
       (?$(GU}(B . "0x6842") ; <CJK>
       (?$(GU~(B . "0x6854") ; <CJK>
       (?$(GV!(B . "0x6829") ; <CJK>
       (?$(GV"(B . "0x68B3") ; <CJK>
       (?$(GV#(B . "0x6817") ; <CJK>
       (?$(GV$(B . "0x684C") ; <CJK>
       (?$(GV%(B . "0x6851") ; <CJK>
       (?$(GV&(B . "0x683D") ; <CJK>
       (?$(GV'(B . "0x67F4") ; <CJK>
       (?$(GV((B . "0x6850") ; <CJK>
       (?$(GV)(B . "0x6840") ; <CJK>
       (?$(GV*(B . "0x683C") ; <CJK>
       (?$(GV+(B . "0x6843") ; <CJK>
       (?$(GV,(B . "0x682A") ; <CJK>
       (?$(GV-(B . "0x6845") ; <CJK>
       (?$(GV.(B . "0x6813") ; <CJK>
       (?$(GV/(B . "0x6818") ; <CJK>
       (?$(GV0(B . "0x6841") ; <CJK>
       (?$(GV1(B . "0x6B8A") ; <CJK>
       (?$(GV2(B . "0x6B89") ; <CJK>
       (?$(GV3(B . "0x6BB7") ; <CJK>
       (?$(GV4(B . "0x6C23") ; <CJK>
       (?$(GV5(B . "0x6C27") ; <CJK>
       (?$(GV6(B . "0x6C28") ; <CJK>
       (?$(GV7(B . "0x6C26") ; <CJK>
       (?$(GV8(B . "0x6C24") ; <CJK>
       (?$(GV9(B . "0x6CF0") ; <CJK>
       (?$(GV:(B . "0x6D6A") ; <CJK>
       (?$(GV;(B . "0x6D95") ; <CJK>
       (?$(GV<(B . "0x6D88") ; <CJK>
       (?$(GV=(B . "0x6D87") ; <CJK>
       (?$(GV>(B . "0x6D66") ; <CJK>
       (?$(GV?(B . "0x6D78") ; <CJK>
       (?$(GV@(B . "0x6D77") ; <CJK>
       (?$(GVA(B . "0x6D59") ; <CJK>
       (?$(GVB(B . "0x6D93") ; <CJK>
       (?$(GVC(B . "0x6D6C") ; <CJK>
       (?$(GVD(B . "0x6D89") ; <CJK>
       (?$(GVE(B . "0x6D6E") ; <CJK>
       (?$(GVF(B . "0x6D5A") ; <CJK>
       (?$(GVG(B . "0x6D74") ; <CJK>
       (?$(GVH(B . "0x6D69") ; <CJK>
       (?$(GVI(B . "0x6D8C") ; <CJK>
       (?$(GVJ(B . "0x6D8A") ; <CJK>
       (?$(GVK(B . "0x6D79") ; <CJK>
       (?$(GVL(B . "0x6D85") ; <CJK>
       (?$(GVM(B . "0x6D65") ; <CJK>
       (?$(GVN(B . "0x6D94") ; <CJK>
       (?$(GVO(B . "0x70CA") ; <CJK>
       (?$(GVP(B . "0x70D8") ; <CJK>
       (?$(GVQ(B . "0x70E4") ; <CJK>
       (?$(GVR(B . "0x70D9") ; <CJK>
       (?$(GVS(B . "0x70C8") ; <CJK>
       (?$(GVT(B . "0x70CF") ; <CJK>
       (?$(GVU(B . "0x7239") ; <CJK>
       (?$(GVV(B . "0x7279") ; <CJK>
       (?$(GVW(B . "0x72FC") ; <CJK>
       (?$(GVX(B . "0x72F9") ; <CJK>
       (?$(GVY(B . "0x72FD") ; <CJK>
       (?$(GVZ(B . "0x72F8") ; <CJK>
       (?$(GV[(B . "0x72F7") ; <CJK>
       (?$(GV\(B . "0x7386") ; <CJK>
       (?$(GV](B . "0x73ED") ; <CJK>
       (?$(GV^(B . "0x7409") ; <CJK>
       (?$(GV_(B . "0x73EE") ; <CJK>
       (?$(GV`(B . "0x73E0") ; <CJK>
       (?$(GVa(B . "0x73EA") ; <CJK>
       (?$(GVb(B . "0x73DE") ; <CJK>
       (?$(GVc(B . "0x7554") ; <CJK>
       (?$(GVd(B . "0x755D") ; <CJK>
       (?$(GVe(B . "0x755C") ; <CJK>
       (?$(GVf(B . "0x755A") ; <CJK>
       (?$(GVg(B . "0x7559") ; <CJK>
       (?$(GVh(B . "0x75BE") ; <CJK>
       (?$(GVi(B . "0x75C5") ; <CJK>
       (?$(GVj(B . "0x75C7") ; <CJK>
       (?$(GVk(B . "0x75B2") ; <CJK>
       (?$(GVl(B . "0x75B3") ; <CJK>
       (?$(GVm(B . "0x75BD") ; <CJK>
       (?$(GVn(B . "0x75BC") ; <CJK>
       (?$(GVo(B . "0x75B9") ; <CJK>
       (?$(GVp(B . "0x75C2") ; <CJK>
       (?$(GVq(B . "0x75B8") ; <CJK>
       (?$(GVr(B . "0x768B") ; <CJK>
       (?$(GVs(B . "0x76B0") ; <CJK>
       (?$(GVt(B . "0x76CA") ; <CJK>
       (?$(GVu(B . "0x76CD") ; <CJK>
       (?$(GVv(B . "0x76CE") ; <CJK>
       (?$(GVw(B . "0x7729") ; <CJK>
       (?$(GVx(B . "0x771F") ; <CJK>
       (?$(GVy(B . "0x7720") ; <CJK>
       (?$(GVz(B . "0x7728") ; <CJK>
       (?$(GV{(B . "0x77E9") ; <CJK>
       (?$(GV|(B . "0x7830") ; <CJK>
       (?$(GV}(B . "0x7827") ; <CJK>
       (?$(GV~(B . "0x7838") ; <CJK>
       (?$(GW!(B . "0x781D") ; <CJK>
       (?$(GW"(B . "0x7834") ; <CJK>
       (?$(GW#(B . "0x7837") ; <CJK>
       (?$(GW$(B . "0x7825") ; <CJK>
       (?$(GW%(B . "0x782D") ; <CJK>
       (?$(GW&(B . "0x7820") ; <CJK>
       (?$(GW'(B . "0x781F") ; <CJK>
       (?$(GW((B . "0x7832") ; <CJK>
       (?$(GW)(B . "0x7955") ; <CJK>
       (?$(GW*(B . "0x7950") ; <CJK>
       (?$(GW+(B . "0x7960") ; <CJK>
       (?$(GW,(B . "0x795F") ; <CJK>
       (?$(GW-(B . "0x7956") ; <CJK>
       (?$(GW.(B . "0x795E") ; <CJK>
       (?$(GW/(B . "0x795D") ; <CJK>
       (?$(GW0(B . "0x7957") ; <CJK>
       (?$(GW1(B . "0x795A") ; <CJK>
       (?$(GW2(B . "0x79E4") ; <CJK>
       (?$(GW3(B . "0x79E3") ; <CJK>
       (?$(GW4(B . "0x79E7") ; <CJK>
       (?$(GW5(B . "0x79DF") ; <CJK>
       (?$(GW6(B . "0x79E6") ; <CJK>
       (?$(GW7(B . "0x79E9") ; <CJK>
       (?$(GW8(B . "0x79D8") ; <CJK>
       (?$(GW9(B . "0x7A84") ; <CJK>
       (?$(GW:(B . "0x7A88") ; <CJK>
       (?$(GW;(B . "0x7AD9") ; <CJK>
       (?$(GW<(B . "0x7B06") ; <CJK>
       (?$(GW=(B . "0x7B11") ; <CJK>
       (?$(GW>(B . "0x7C89") ; <CJK>
       (?$(GW?(B . "0x7D21") ; <CJK>
       (?$(GW@(B . "0x7D17") ; <CJK>
       (?$(GWA(B . "0x7D0B") ; <CJK>
       (?$(GWB(B . "0x7D0A") ; <CJK>
       (?$(GWC(B . "0x7D20") ; <CJK>
       (?$(GWD(B . "0x7D22") ; <CJK>
       (?$(GWE(B . "0x7D14") ; <CJK>
       (?$(GWF(B . "0x7D10") ; <CJK>
       (?$(GWG(B . "0x7D15") ; <CJK>
       (?$(GWH(B . "0x7D1A") ; <CJK>
       (?$(GWI(B . "0x7D1C") ; <CJK>
       (?$(GWJ(B . "0x7D0D") ; <CJK>
       (?$(GWK(B . "0x7D19") ; <CJK>
       (?$(GWL(B . "0x7D1B") ; <CJK>
       (?$(GWM(B . "0x7F3A") ; <CJK>
       (?$(GWN(B . "0x7F5F") ; <CJK>
       (?$(GWO(B . "0x7F94") ; <CJK>
       (?$(GWP(B . "0x7FC5") ; <CJK>
       (?$(GWQ(B . "0x7FC1") ; <CJK>
       (?$(GWR(B . "0x8006") ; <CJK>
       (?$(GWS(B . "0x8004") ; <CJK>
       (?$(GWT(B . "0x8018") ; <CJK>
       (?$(GWU(B . "0x8015") ; <CJK>
       (?$(GWV(B . "0x8019") ; <CJK>
       (?$(GWW(B . "0x8017") ; <CJK>
       (?$(GWX(B . "0x803D") ; <CJK>
       (?$(GWY(B . "0x803F") ; <CJK>
       (?$(GWZ(B . "0x80F1") ; <CJK>
       (?$(GW[(B . "0x8102") ; <CJK>
       (?$(GW\(B . "0x80F0") ; <CJK>
       (?$(GW](B . "0x8105") ; <CJK>
       (?$(GW^(B . "0x80ED") ; <CJK>
       (?$(GW_(B . "0x80F4") ; <CJK>
       (?$(GW`(B . "0x8106") ; <CJK>
       (?$(GWa(B . "0x80F8") ; <CJK>
       (?$(GWb(B . "0x80F3") ; <CJK>
       (?$(GWc(B . "0x8108") ; <CJK>
       (?$(GWd(B . "0x80FD") ; <CJK>
       (?$(GWe(B . "0x810A") ; <CJK>
       (?$(GWf(B . "0x80FC") ; <CJK>
       (?$(GWg(B . "0x80EF") ; <CJK>
       (?$(GWh(B . "0x81ED") ; <CJK>
       (?$(GWi(B . "0x81EC") ; <CJK>
       (?$(GWj(B . "0x8200") ; <CJK>
       (?$(GWk(B . "0x8210") ; <CJK>
       (?$(GWl(B . "0x822A") ; <CJK>
       (?$(GWm(B . "0x822B") ; <CJK>
       (?$(GWn(B . "0x8228") ; <CJK>
       (?$(GWo(B . "0x822C") ; <CJK>
       (?$(GWp(B . "0x82BB") ; <CJK>
       (?$(GWq(B . "0x832B") ; <CJK>
       (?$(GWr(B . "0x8352") ; <CJK>
       (?$(GWs(B . "0x8354") ; <CJK>
       (?$(GWt(B . "0x834A") ; <CJK>
       (?$(GWu(B . "0x8338") ; <CJK>
       (?$(GWv(B . "0x8350") ; <CJK>
       (?$(GWw(B . "0x8349") ; <CJK>
       (?$(GWx(B . "0x8335") ; <CJK>
       (?$(GWy(B . "0x8334") ; <CJK>
       (?$(GWz(B . "0x834F") ; <CJK>
       (?$(GW{(B . "0x8332") ; <CJK>
       (?$(GW|(B . "0x8339") ; <CJK>
       (?$(GW}(B . "0x8336") ; <CJK>
       (?$(GW~(B . "0x8317") ; <CJK>
       (?$(GX!(B . "0x8340") ; <CJK>
       (?$(GX"(B . "0x8331") ; <CJK>
       (?$(GX#(B . "0x8328") ; <CJK>
       (?$(GX$(B . "0x8343") ; <CJK>
       (?$(GX%(B . "0x8654") ; <CJK>
       (?$(GX&(B . "0x868A") ; <CJK>
       (?$(GX'(B . "0x86AA") ; <CJK>
       (?$(GX((B . "0x8693") ; <CJK>
       (?$(GX)(B . "0x86A4") ; <CJK>
       (?$(GX*(B . "0x86A9") ; <CJK>
       (?$(GX+(B . "0x868C") ; <CJK>
       (?$(GX,(B . "0x86A3") ; <CJK>
       (?$(GX-(B . "0x869C") ; <CJK>
       (?$(GX.(B . "0x8870") ; <CJK>
       (?$(GX/(B . "0x8877") ; <CJK>
       (?$(GX0(B . "0x8881") ; <CJK>
       (?$(GX1(B . "0x8882") ; <CJK>
       (?$(GX2(B . "0x887D") ; <CJK>
       (?$(GX3(B . "0x8879") ; <CJK>
       (?$(GX4(B . "0x8A18") ; <CJK>
       (?$(GX5(B . "0x8A10") ; <CJK>
       (?$(GX6(B . "0x8A0E") ; <CJK>
       (?$(GX7(B . "0x8A0C") ; <CJK>
       (?$(GX8(B . "0x8A15") ; <CJK>
       (?$(GX9(B . "0x8A0A") ; <CJK>
       (?$(GX:(B . "0x8A17") ; <CJK>
       (?$(GX;(B . "0x8A13") ; <CJK>
       (?$(GX<(B . "0x8A16") ; <CJK>
       (?$(GX=(B . "0x8A0F") ; <CJK>
       (?$(GX>(B . "0x8A11") ; <CJK>
       (?$(GX?(B . "0x8C48") ; <CJK>
       (?$(GX@(B . "0x8C7A") ; <CJK>
       (?$(GXA(B . "0x8C79") ; <CJK>
       (?$(GXB(B . "0x8CA1") ; <CJK>
       (?$(GXC(B . "0x8CA2") ; <CJK>
       (?$(GXD(B . "0x8D77") ; <CJK>
       (?$(GXE(B . "0x8EAC") ; <CJK>
       (?$(GXF(B . "0x8ED2") ; <CJK>
       (?$(GXG(B . "0x8ED4") ; <CJK>
       (?$(GXH(B . "0x8ECF") ; <CJK>
       (?$(GXI(B . "0x8FB1") ; <CJK>
       (?$(GXJ(B . "0x9001") ; <CJK>
       (?$(GXK(B . "0x9006") ; <CJK>
       (?$(GXL(B . "0x8FF7") ; <CJK>
       (?$(GXM(B . "0x9000") ; <CJK>
       (?$(GXN(B . "0x8FFA") ; <CJK>
       (?$(GXO(B . "0x8FF4") ; <CJK>
       (?$(GXP(B . "0x9003") ; <CJK>
       (?$(GXQ(B . "0x8FFD") ; <CJK>
       (?$(GXR(B . "0x9005") ; <CJK>
       (?$(GXS(B . "0x8FF8") ; <CJK>
       (?$(GXT(B . "0x9095") ; <CJK>
       (?$(GXU(B . "0x90E1") ; <CJK>
       (?$(GXV(B . "0x90DD") ; <CJK>
       (?$(GXW(B . "0x90E2") ; <CJK>
       (?$(GXX(B . "0x9152") ; <CJK>
       (?$(GXY(B . "0x914D") ; <CJK>
       (?$(GXZ(B . "0x914C") ; <CJK>
       (?$(GX[(B . "0x91D8") ; <CJK>
       (?$(GX\(B . "0x91DD") ; <CJK>
       (?$(GX](B . "0x91D7") ; <CJK>
       (?$(GX^(B . "0x91DC") ; <CJK>
       (?$(GX_(B . "0x91D9") ; <CJK>
       (?$(GX`(B . "0x9583") ; <CJK>
       (?$(GXa(B . "0x9662") ; <CJK>
       (?$(GXb(B . "0x9663") ; <CJK>
       (?$(GXc(B . "0x9661") ; <CJK>
       (?$(GXd(B . "0x965B") ; <CJK>
       (?$(GXe(B . "0x965D") ; <CJK>
       (?$(GXf(B . "0x9664") ; <CJK>
       (?$(GXg(B . "0x9658") ; <CJK>
       (?$(GXh(B . "0x965E") ; <CJK>
       (?$(GXi(B . "0x96BB") ; <CJK>
       (?$(GXj(B . "0x98E2") ; <CJK>
       (?$(GXk(B . "0x99AC") ; <CJK>
       (?$(GXl(B . "0x9AA8") ; <CJK>
       (?$(GXm(B . "0x9AD8") ; <CJK>
       (?$(GXn(B . "0x9B25") ; <CJK>
       (?$(GXo(B . "0x9B32") ; <CJK>
       (?$(GXp(B . "0x9B3C") ; <CJK>
       (?$(GXq(B . "0x4E7E") ; <CJK>
       (?$(GXr(B . "0x507A") ; <CJK>
       (?$(GXs(B . "0x507D") ; <CJK>
       (?$(GXt(B . "0x505C") ; <CJK>
       (?$(GXu(B . "0x5047") ; <CJK>
       (?$(GXv(B . "0x5043") ; <CJK>
       (?$(GXw(B . "0x504C") ; <CJK>
       (?$(GXx(B . "0x505A") ; <CJK>
       (?$(GXy(B . "0x5049") ; <CJK>
       (?$(GXz(B . "0x5065") ; <CJK>
       (?$(GX{(B . "0x5076") ; <CJK>
       (?$(GX|(B . "0x504E") ; <CJK>
       (?$(GX}(B . "0x5055") ; <CJK>
       (?$(GX~(B . "0x5075") ; <CJK>
       (?$(GY!(B . "0x5074") ; <CJK>
       (?$(GY"(B . "0x5077") ; <CJK>
       (?$(GY#(B . "0x504F") ; <CJK>
       (?$(GY$(B . "0x500F") ; <CJK>
       (?$(GY%(B . "0x506F") ; <CJK>
       (?$(GY&(B . "0x506D") ; <CJK>
       (?$(GY'(B . "0x515C") ; <CJK>
       (?$(GY((B . "0x5195") ; <CJK>
       (?$(GY)(B . "0x51F0") ; <CJK>
       (?$(GY*(B . "0x526A") ; <CJK>
       (?$(GY+(B . "0x526F") ; <CJK>
       (?$(GY,(B . "0x52D2") ; <CJK>
       (?$(GY-(B . "0x52D9") ; <CJK>
       (?$(GY.(B . "0x52D8") ; <CJK>
       (?$(GY/(B . "0x52D5") ; <CJK>
       (?$(GY0(B . "0x5310") ; <CJK>
       (?$(GY1(B . "0x530F") ; <CJK>
       (?$(GY2(B . "0x5319") ; <CJK>
       (?$(GY3(B . "0x533F") ; <CJK>
       (?$(GY4(B . "0x5340") ; <CJK>
       (?$(GY5(B . "0x533E") ; <CJK>
       (?$(GY6(B . "0x53C3") ; <CJK>
       (?$(GY7(B . "0x66FC") ; <CJK>
       (?$(GY8(B . "0x5546") ; <CJK>
       (?$(GY9(B . "0x556A") ; <CJK>
       (?$(GY:(B . "0x5566") ; <CJK>
       (?$(GY;(B . "0x5544") ; <CJK>
       (?$(GY<(B . "0x555E") ; <CJK>
       (?$(GY=(B . "0x5561") ; <CJK>
       (?$(GY>(B . "0x5543") ; <CJK>
       (?$(GY?(B . "0x554A") ; <CJK>
       (?$(GY@(B . "0x5531") ; <CJK>
       (?$(GYA(B . "0x5556") ; <CJK>
       (?$(GYB(B . "0x554F") ; <CJK>
       (?$(GYC(B . "0x5555") ; <CJK>
       (?$(GYD(B . "0x552F") ; <CJK>
       (?$(GYE(B . "0x5564") ; <CJK>
       (?$(GYF(B . "0x5538") ; <CJK>
       (?$(GYG(B . "0x552E") ; <CJK>
       (?$(GYH(B . "0x555C") ; <CJK>
       (?$(GYI(B . "0x552C") ; <CJK>
       (?$(GYJ(B . "0x5563") ; <CJK>
       (?$(GYK(B . "0x5533") ; <CJK>
       (?$(GYL(B . "0x5541") ; <CJK>
       (?$(GYM(B . "0x5557") ; <CJK>
       (?$(GYN(B . "0x5708") ; <CJK>
       (?$(GYO(B . "0x570B") ; <CJK>
       (?$(GYP(B . "0x5709") ; <CJK>
       (?$(GYQ(B . "0x57DF") ; <CJK>
       (?$(GYR(B . "0x5805") ; <CJK>
       (?$(GYS(B . "0x580A") ; <CJK>
       (?$(GYT(B . "0x5806") ; <CJK>
       (?$(GYU(B . "0x57E0") ; <CJK>
       (?$(GYV(B . "0x57E4") ; <CJK>
       (?$(GYW(B . "0x57FA") ; <CJK>
       (?$(GYX(B . "0x5802") ; <CJK>
       (?$(GYY(B . "0x5835") ; <CJK>
       (?$(GYZ(B . "0x57F7") ; <CJK>
       (?$(GY[(B . "0x57F9") ; <CJK>
       (?$(GY\(B . "0x5920") ; <CJK>
       (?$(GY](B . "0x5962") ; <CJK>
       (?$(GY^(B . "0x5A36") ; <CJK>
       (?$(GY_(B . "0x5A41") ; <CJK>
       (?$(GY`(B . "0x5A49") ; <CJK>
       (?$(GYa(B . "0x5A66") ; <CJK>
       (?$(GYb(B . "0x5A6A") ; <CJK>
       (?$(GYc(B . "0x5A40") ; <CJK>
       (?$(GYd(B . "0x5A3C") ; <CJK>
       (?$(GYe(B . "0x5A62") ; <CJK>
       (?$(GYf(B . "0x5A5A") ; <CJK>
       (?$(GYg(B . "0x5A46") ; <CJK>
       (?$(GYh(B . "0x5A4A") ; <CJK>
       (?$(GYi(B . "0x5B70") ; <CJK>
       (?$(GYj(B . "0x5BC7") ; <CJK>
       (?$(GYk(B . "0x5BC5") ; <CJK>
       (?$(GYl(B . "0x5BC4") ; <CJK>
       (?$(GYm(B . "0x5BC2") ; <CJK>
       (?$(GYn(B . "0x5BBF") ; <CJK>
       (?$(GYo(B . "0x5BC6") ; <CJK>
       (?$(GYp(B . "0x5C09") ; <CJK>
       (?$(GYq(B . "0x5C08") ; <CJK>
       (?$(GYr(B . "0x5C07") ; <CJK>
       (?$(GYs(B . "0x5C60") ; <CJK>
       (?$(GYt(B . "0x5C5C") ; <CJK>
       (?$(GYu(B . "0x5C5D") ; <CJK>
       (?$(GYv(B . "0x5D07") ; <CJK>
       (?$(GYw(B . "0x5D06") ; <CJK>
       (?$(GYx(B . "0x5D0E") ; <CJK>
       (?$(GYy(B . "0x5D1B") ; <CJK>
       (?$(GYz(B . "0x5D16") ; <CJK>
       (?$(GY{(B . "0x5D22") ; <CJK>
       (?$(GY|(B . "0x5D11") ; <CJK>
       (?$(GY}(B . "0x5D29") ; <CJK>
       (?$(GY~(B . "0x5D14") ; <CJK>
       (?$(GZ!(B . "0x5D19") ; <CJK>
       (?$(GZ"(B . "0x5D24") ; <CJK>
       (?$(GZ#(B . "0x5D27") ; <CJK>
       (?$(GZ$(B . "0x5D17") ; <CJK>
       (?$(GZ%(B . "0x5DE2") ; <CJK>
       (?$(GZ&(B . "0x5E38") ; <CJK>
       (?$(GZ'(B . "0x5E36") ; <CJK>
       (?$(GZ((B . "0x5E33") ; <CJK>
       (?$(GZ)(B . "0x5E37") ; <CJK>
       (?$(GZ*(B . "0x5EB7") ; <CJK>
       (?$(GZ+(B . "0x5EB8") ; <CJK>
       (?$(GZ,(B . "0x5EB6") ; <CJK>
       (?$(GZ-(B . "0x5EB5") ; <CJK>
       (?$(GZ.(B . "0x5EBE") ; <CJK>
       (?$(GZ/(B . "0x5F35") ; <CJK>
       (?$(GZ0(B . "0x5F37") ; <CJK>
       (?$(GZ1(B . "0x5F57") ; <CJK>
       (?$(GZ2(B . "0x5F6C") ; <CJK>
       (?$(GZ3(B . "0x5F69") ; <CJK>
       (?$(GZ4(B . "0x5F6B") ; <CJK>
       (?$(GZ5(B . "0x5F97") ; <CJK>
       (?$(GZ6(B . "0x5F99") ; <CJK>
       (?$(GZ7(B . "0x5F9E") ; <CJK>
       (?$(GZ8(B . "0x5F98") ; <CJK>
       (?$(GZ9(B . "0x5FA1") ; <CJK>
       (?$(GZ:(B . "0x5FA0") ; <CJK>
       (?$(GZ;(B . "0x5F9C") ; <CJK>
       (?$(GZ<(B . "0x607F") ; <CJK>
       (?$(GZ=(B . "0x60A3") ; <CJK>
       (?$(GZ>(B . "0x6089") ; <CJK>
       (?$(GZ?(B . "0x60A0") ; <CJK>
       (?$(GZ@(B . "0x60A8") ; <CJK>
       (?$(GZA(B . "0x60CB") ; <CJK>
       (?$(GZB(B . "0x60B4") ; <CJK>
       (?$(GZC(B . "0x60E6") ; <CJK>
       (?$(GZD(B . "0x60BD") ; <CJK>
       (?$(GZE(B . "0x60C5") ; <CJK>
       (?$(GZF(B . "0x60BB") ; <CJK>
       (?$(GZG(B . "0x60B5") ; <CJK>
       (?$(GZH(B . "0x60DC") ; <CJK>
       (?$(GZI(B . "0x60BC") ; <CJK>
       (?$(GZJ(B . "0x60D8") ; <CJK>
       (?$(GZK(B . "0x60D5") ; <CJK>
       (?$(GZL(B . "0x60C6") ; <CJK>
       (?$(GZM(B . "0x60DF") ; <CJK>
       (?$(GZN(B . "0x60B8") ; <CJK>
       (?$(GZO(B . "0x60DA") ; <CJK>
       (?$(GZP(B . "0x60C7") ; <CJK>
       (?$(GZQ(B . "0x621A") ; <CJK>
       (?$(GZR(B . "0x621B") ; <CJK>
       (?$(GZS(B . "0x6248") ; <CJK>
       (?$(GZT(B . "0x63A0") ; <CJK>
       (?$(GZU(B . "0x63A7") ; <CJK>
       (?$(GZV(B . "0x6372") ; <CJK>
       (?$(GZW(B . "0x6396") ; <CJK>
       (?$(GZX(B . "0x63A2") ; <CJK>
       (?$(GZY(B . "0x63A5") ; <CJK>
       (?$(GZZ(B . "0x6377") ; <CJK>
       (?$(GZ[(B . "0x6367") ; <CJK>
       (?$(GZ\(B . "0x6398") ; <CJK>
       (?$(GZ](B . "0x63AA") ; <CJK>
       (?$(GZ^(B . "0x6371") ; <CJK>
       (?$(GZ_(B . "0x63A9") ; <CJK>
       (?$(GZ`(B . "0x6389") ; <CJK>
       (?$(GZa(B . "0x6383") ; <CJK>
       (?$(GZb(B . "0x639B") ; <CJK>
       (?$(GZc(B . "0x636B") ; <CJK>
       (?$(GZd(B . "0x63A8") ; <CJK>
       (?$(GZe(B . "0x6384") ; <CJK>
       (?$(GZf(B . "0x6388") ; <CJK>
       (?$(GZg(B . "0x6399") ; <CJK>
       (?$(GZh(B . "0x63A1") ; <CJK>
       (?$(GZi(B . "0x63AC") ; <CJK>
       (?$(GZj(B . "0x6392") ; <CJK>
       (?$(GZk(B . "0x638F") ; <CJK>
       (?$(GZl(B . "0x6380") ; <CJK>
       (?$(GZm(B . "0x637B") ; <CJK>
       (?$(GZn(B . "0x6369") ; <CJK>
       (?$(GZo(B . "0x6368") ; <CJK>
       (?$(GZp(B . "0x637A") ; <CJK>
       (?$(GZq(B . "0x655D") ; <CJK>
       (?$(GZr(B . "0x6556") ; <CJK>
       (?$(GZs(B . "0x6551") ; <CJK>
       (?$(GZt(B . "0x6559") ; <CJK>
       (?$(GZu(B . "0x6557") ; <CJK>
       (?$(GZv(B . "0x555F") ; <CJK>
       (?$(GZw(B . "0x654F") ; <CJK>
       (?$(GZx(B . "0x6558") ; <CJK>
       (?$(GZy(B . "0x6555") ; <CJK>
       (?$(GZz(B . "0x6554") ; <CJK>
       (?$(GZ{(B . "0x659C") ; <CJK>
       (?$(GZ|(B . "0x659B") ; <CJK>
       (?$(GZ}(B . "0x65AC") ; <CJK>
       (?$(GZ~(B . "0x65CF") ; <CJK>
       (?$(G[!(B . "0x65CB") ; <CJK>
       (?$(G["(B . "0x65CC") ; <CJK>
       (?$(G[#(B . "0x65CE") ; <CJK>
       (?$(G[$(B . "0x665D") ; <CJK>
       (?$(G[%(B . "0x665A") ; <CJK>
       (?$(G[&(B . "0x6664") ; <CJK>
       (?$(G['(B . "0x6668") ; <CJK>
       (?$(G[((B . "0x6666") ; <CJK>
       (?$(G[)(B . "0x665E") ; <CJK>
       (?$(G[*(B . "0x66F9") ; <CJK>
       (?$(G[+(B . "0x52D7") ; <CJK>
       (?$(G[,(B . "0x671B") ; <CJK>
       (?$(G[-(B . "0x6881") ; <CJK>
       (?$(G[.(B . "0x68AF") ; <CJK>
       (?$(G[/(B . "0x68A2") ; <CJK>
       (?$(G[0(B . "0x6893") ; <CJK>
       (?$(G[1(B . "0x68B5") ; <CJK>
       (?$(G[2(B . "0x687F") ; <CJK>
       (?$(G[3(B . "0x6876") ; <CJK>
       (?$(G[4(B . "0x68B1") ; <CJK>
       (?$(G[5(B . "0x68A7") ; <CJK>
       (?$(G[6(B . "0x6897") ; <CJK>
       (?$(G[7(B . "0x68B0") ; <CJK>
       (?$(G[8(B . "0x6883") ; <CJK>
       (?$(G[9(B . "0x68C4") ; <CJK>
       (?$(G[:(B . "0x68AD") ; <CJK>
       (?$(G[;(B . "0x6886") ; <CJK>
       (?$(G[<(B . "0x6885") ; <CJK>
       (?$(G[=(B . "0x6894") ; <CJK>
       (?$(G[>(B . "0x689D") ; <CJK>
       (?$(G[?(B . "0x68A8") ; <CJK>
       (?$(G[@(B . "0x689F") ; <CJK>
       (?$(G[A(B . "0x68A1") ; <CJK>
       (?$(G[B(B . "0x6882") ; <CJK>
       (?$(G[C(B . "0x6B32") ; <CJK>
       (?$(G[D(B . "0x6BBA") ; <CJK>
       (?$(G[E(B . "0x6BEB") ; <CJK>
       (?$(G[F(B . "0x6BEC") ; <CJK>
       (?$(G[G(B . "0x6C2B") ; <CJK>
       (?$(G[H(B . "0x6D8E") ; <CJK>
       (?$(G[I(B . "0x6DBC") ; <CJK>
       (?$(G[J(B . "0x6DF3") ; <CJK>
       (?$(G[K(B . "0x6DD9") ; <CJK>
       (?$(G[L(B . "0x6DB2") ; <CJK>
       (?$(G[M(B . "0x6DE1") ; <CJK>
       (?$(G[N(B . "0x6DCC") ; <CJK>
       (?$(G[O(B . "0x6DE4") ; <CJK>
       (?$(G[P(B . "0x6DFB") ; <CJK>
       (?$(G[Q(B . "0x6DFA") ; <CJK>
       (?$(G[R(B . "0x6E05") ; <CJK>
       (?$(G[S(B . "0x6DC7") ; <CJK>
       (?$(G[T(B . "0x6DCB") ; <CJK>
       (?$(G[U(B . "0x6DAF") ; <CJK>
       (?$(G[V(B . "0x6DD1") ; <CJK>
       (?$(G[W(B . "0x6DAE") ; <CJK>
       (?$(G[X(B . "0x6DDE") ; <CJK>
       (?$(G[Y(B . "0x6DF9") ; <CJK>
       (?$(G[Z(B . "0x6DB8") ; <CJK>
       (?$(G[[(B . "0x6DF7") ; <CJK>
       (?$(G[\(B . "0x6DF5") ; <CJK>
       (?$(G[](B . "0x6DC5") ; <CJK>
       (?$(G[^(B . "0x6DD2") ; <CJK>
       (?$(G[_(B . "0x6E1A") ; <CJK>
       (?$(G[`(B . "0x6DB5") ; <CJK>
       (?$(G[a(B . "0x6DDA") ; <CJK>
       (?$(G[b(B . "0x6DEB") ; <CJK>
       (?$(G[c(B . "0x6DD8") ; <CJK>
       (?$(G[d(B . "0x6DEA") ; <CJK>
       (?$(G[e(B . "0x6DF1") ; <CJK>
       (?$(G[f(B . "0x6DEE") ; <CJK>
       (?$(G[g(B . "0x6DE8") ; <CJK>
       (?$(G[h(B . "0x6DC6") ; <CJK>
       (?$(G[i(B . "0x6DC4") ; <CJK>
       (?$(G[j(B . "0x6DAA") ; <CJK>
       (?$(G[k(B . "0x6DEC") ; <CJK>
       (?$(G[l(B . "0x6DBF") ; <CJK>
       (?$(G[m(B . "0x6DE6") ; <CJK>
       (?$(G[n(B . "0x70F9") ; <CJK>
       (?$(G[o(B . "0x7109") ; <CJK>
       (?$(G[p(B . "0x710A") ; <CJK>
       (?$(G[q(B . "0x70FD") ; <CJK>
       (?$(G[r(B . "0x70EF") ; <CJK>
       (?$(G[s(B . "0x723D") ; <CJK>
       (?$(G[t(B . "0x727D") ; <CJK>
       (?$(G[u(B . "0x7281") ; <CJK>
       (?$(G[v(B . "0x731C") ; <CJK>
       (?$(G[w(B . "0x731B") ; <CJK>
       (?$(G[x(B . "0x7316") ; <CJK>
       (?$(G[y(B . "0x7313") ; <CJK>
       (?$(G[z(B . "0x7319") ; <CJK>
       (?$(G[{(B . "0x7387") ; <CJK>
       (?$(G[|(B . "0x7405") ; <CJK>
       (?$(G[}(B . "0x740A") ; <CJK>
       (?$(G[~(B . "0x7403") ; <CJK>
       (?$(G\!(B . "0x7406") ; <CJK>
       (?$(G\"(B . "0x73FE") ; <CJK>
       (?$(G\#(B . "0x740D") ; <CJK>
       (?$(G\$(B . "0x74E0") ; <CJK>
       (?$(G\%(B . "0x74F6") ; <CJK>
       (?$(G\&(B . "0x74F7") ; <CJK>
       (?$(G\'(B . "0x751C") ; <CJK>
       (?$(G\((B . "0x7522") ; <CJK>
       (?$(G\)(B . "0x7565") ; <CJK>
       (?$(G\*(B . "0x7566") ; <CJK>
       (?$(G\+(B . "0x7562") ; <CJK>
       (?$(G\,(B . "0x7570") ; <CJK>
       (?$(G\-(B . "0x758F") ; <CJK>
       (?$(G\.(B . "0x75D4") ; <CJK>
       (?$(G\/(B . "0x75D5") ; <CJK>
       (?$(G\0(B . "0x75B5") ; <CJK>
       (?$(G\1(B . "0x75CA") ; <CJK>
       (?$(G\2(B . "0x75CD") ; <CJK>
       (?$(G\3(B . "0x768E") ; <CJK>
       (?$(G\4(B . "0x76D4") ; <CJK>
       (?$(G\5(B . "0x76D2") ; <CJK>
       (?$(G\6(B . "0x76DB") ; <CJK>
       (?$(G\7(B . "0x7737") ; <CJK>
       (?$(G\8(B . "0x773E") ; <CJK>
       (?$(G\9(B . "0x773C") ; <CJK>
       (?$(G\:(B . "0x7736") ; <CJK>
       (?$(G\;(B . "0x7738") ; <CJK>
       (?$(G\<(B . "0x773A") ; <CJK>
       (?$(G\=(B . "0x786B") ; <CJK>
       (?$(G\>(B . "0x7843") ; <CJK>
       (?$(G\?(B . "0x784E") ; <CJK>
       (?$(G\@(B . "0x7965") ; <CJK>
       (?$(G\A(B . "0x7968") ; <CJK>
       (?$(G\B(B . "0x796D") ; <CJK>
       (?$(G\C(B . "0x79FB") ; <CJK>
       (?$(G\D(B . "0x7A92") ; <CJK>
       (?$(G\E(B . "0x7A95") ; <CJK>
       (?$(G\F(B . "0x7B20") ; <CJK>
       (?$(G\G(B . "0x7B28") ; <CJK>
       (?$(G\H(B . "0x7B1B") ; <CJK>
       (?$(G\I(B . "0x7B2C") ; <CJK>
       (?$(G\J(B . "0x7B26") ; <CJK>
       (?$(G\K(B . "0x7B19") ; <CJK>
       (?$(G\L(B . "0x7B1E") ; <CJK>
       (?$(G\M(B . "0x7B2E") ; <CJK>
       (?$(G\N(B . "0x7C92") ; <CJK>
       (?$(G\O(B . "0x7C97") ; <CJK>
       (?$(G\P(B . "0x7C95") ; <CJK>
       (?$(G\Q(B . "0x7D46") ; <CJK>
       (?$(G\R(B . "0x7D43") ; <CJK>
       (?$(G\S(B . "0x7D71") ; <CJK>
       (?$(G\T(B . "0x7D2E") ; <CJK>
       (?$(G\U(B . "0x7D39") ; <CJK>
       (?$(G\V(B . "0x7D3C") ; <CJK>
       (?$(G\W(B . "0x7D40") ; <CJK>
       (?$(G\X(B . "0x7D30") ; <CJK>
       (?$(G\Y(B . "0x7D33") ; <CJK>
       (?$(G\Z(B . "0x7D44") ; <CJK>
       (?$(G\[(B . "0x7D2F") ; <CJK>
       (?$(G\\(B . "0x7D42") ; <CJK>
       (?$(G\](B . "0x7D32") ; <CJK>
       (?$(G\^(B . "0x7D31") ; <CJK>
       (?$(G\_(B . "0x7F3D") ; <CJK>
       (?$(G\`(B . "0x7F9E") ; <CJK>
       (?$(G\a(B . "0x7F9A") ; <CJK>
       (?$(G\b(B . "0x7FCC") ; <CJK>
       (?$(G\c(B . "0x7FCE") ; <CJK>
       (?$(G\d(B . "0x7FD2") ; <CJK>
       (?$(G\e(B . "0x801C") ; <CJK>
       (?$(G\f(B . "0x804A") ; <CJK>
       (?$(G\g(B . "0x8046") ; <CJK>
       (?$(G\h(B . "0x812F") ; <CJK>
       (?$(G\i(B . "0x8116") ; <CJK>
       (?$(G\j(B . "0x8123") ; <CJK>
       (?$(G\k(B . "0x812B") ; <CJK>
       (?$(G\l(B . "0x8129") ; <CJK>
       (?$(G\m(B . "0x8130") ; <CJK>
       (?$(G\n(B . "0x8124") ; <CJK>
       (?$(G\o(B . "0x8202") ; <CJK>
       (?$(G\p(B . "0x8235") ; <CJK>
       (?$(G\q(B . "0x8237") ; <CJK>
       (?$(G\r(B . "0x8236") ; <CJK>
       (?$(G\s(B . "0x8239") ; <CJK>
       (?$(G\t(B . "0x838E") ; <CJK>
       (?$(G\u(B . "0x839E") ; <CJK>
       (?$(G\v(B . "0x8398") ; <CJK>
       (?$(G\w(B . "0x8378") ; <CJK>
       (?$(G\x(B . "0x83A2") ; <CJK>
       (?$(G\y(B . "0x8396") ; <CJK>
       (?$(G\z(B . "0x83BD") ; <CJK>
       (?$(G\{(B . "0x83AB") ; <CJK>
       (?$(G\|(B . "0x8392") ; <CJK>
       (?$(G\}(B . "0x838A") ; <CJK>
       (?$(G\~(B . "0x8393") ; <CJK>
       (?$(G]!(B . "0x8389") ; <CJK>
       (?$(G]"(B . "0x83A0") ; <CJK>
       (?$(G]#(B . "0x8377") ; <CJK>
       (?$(G]$(B . "0x837B") ; <CJK>
       (?$(G]%(B . "0x837C") ; <CJK>
       (?$(G]&(B . "0x8386") ; <CJK>
       (?$(G]'(B . "0x83A7") ; <CJK>
       (?$(G]((B . "0x8655") ; <CJK>
       (?$(G])(B . "0x5F6A") ; <CJK>
       (?$(G]*(B . "0x86C7") ; <CJK>
       (?$(G]+(B . "0x86C0") ; <CJK>
       (?$(G],(B . "0x86B6") ; <CJK>
       (?$(G]-(B . "0x86C4") ; <CJK>
       (?$(G].(B . "0x86B5") ; <CJK>
       (?$(G]/(B . "0x86C6") ; <CJK>
       (?$(G]0(B . "0x86CB") ; <CJK>
       (?$(G]1(B . "0x86B1") ; <CJK>
       (?$(G]2(B . "0x86AF") ; <CJK>
       (?$(G]3(B . "0x86C9") ; <CJK>
       (?$(G]4(B . "0x8853") ; <CJK>
       (?$(G]5(B . "0x889E") ; <CJK>
       (?$(G]6(B . "0x8888") ; <CJK>
       (?$(G]7(B . "0x88AB") ; <CJK>
       (?$(G]8(B . "0x8892") ; <CJK>
       (?$(G]9(B . "0x8896") ; <CJK>
       (?$(G]:(B . "0x888D") ; <CJK>
       (?$(G];(B . "0x888B") ; <CJK>
       (?$(G]<(B . "0x8993") ; <CJK>
       (?$(G]=(B . "0x898F") ; <CJK>
       (?$(G]>(B . "0x8A2A") ; <CJK>
       (?$(G]?(B . "0x8A1D") ; <CJK>
       (?$(G]@(B . "0x8A23") ; <CJK>
       (?$(G]A(B . "0x8A25") ; <CJK>
       (?$(G]B(B . "0x8A31") ; <CJK>
       (?$(G]C(B . "0x8A2D") ; <CJK>
       (?$(G]D(B . "0x8A1F") ; <CJK>
       (?$(G]E(B . "0x8A1B") ; <CJK>
       (?$(G]F(B . "0x8A22") ; <CJK>
       (?$(G]G(B . "0x8C49") ; <CJK>
       (?$(G]H(B . "0x8C5A") ; <CJK>
       (?$(G]I(B . "0x8CA9") ; <CJK>
       (?$(G]J(B . "0x8CAC") ; <CJK>
       (?$(G]K(B . "0x8CAB") ; <CJK>
       (?$(G]L(B . "0x8CA8") ; <CJK>
       (?$(G]M(B . "0x8CAA") ; <CJK>
       (?$(G]N(B . "0x8CA7") ; <CJK>
       (?$(G]O(B . "0x8D67") ; <CJK>
       (?$(G]P(B . "0x8D66") ; <CJK>
       (?$(G]Q(B . "0x8DBE") ; <CJK>
       (?$(G]R(B . "0x8DBA") ; <CJK>
       (?$(G]S(B . "0x8EDB") ; <CJK>
       (?$(G]T(B . "0x8EDF") ; <CJK>
       (?$(G]U(B . "0x9019") ; <CJK>
       (?$(G]V(B . "0x900D") ; <CJK>
       (?$(G]W(B . "0x901A") ; <CJK>
       (?$(G]X(B . "0x9017") ; <CJK>
       (?$(G]Y(B . "0x9023") ; <CJK>
       (?$(G]Z(B . "0x901F") ; <CJK>
       (?$(G][(B . "0x901D") ; <CJK>
       (?$(G]\(B . "0x9010") ; <CJK>
       (?$(G]](B . "0x9015") ; <CJK>
       (?$(G]^(B . "0x901E") ; <CJK>
       (?$(G]_(B . "0x9020") ; <CJK>
       (?$(G]`(B . "0x900F") ; <CJK>
       (?$(G]a(B . "0x9022") ; <CJK>
       (?$(G]b(B . "0x9016") ; <CJK>
       (?$(G]c(B . "0x901B") ; <CJK>
       (?$(G]d(B . "0x9014") ; <CJK>
       (?$(G]e(B . "0x90E8") ; <CJK>
       (?$(G]f(B . "0x90ED") ; <CJK>
       (?$(G]g(B . "0x90FD") ; <CJK>
       (?$(G]h(B . "0x9157") ; <CJK>
       (?$(G]i(B . "0x91CE") ; <CJK>
       (?$(G]j(B . "0x91F5") ; <CJK>
       (?$(G]k(B . "0x91E6") ; <CJK>
       (?$(G]l(B . "0x91E3") ; <CJK>
       (?$(G]m(B . "0x91E7") ; <CJK>
       (?$(G]n(B . "0x91ED") ; <CJK>
       (?$(G]o(B . "0x91E9") ; <CJK>
       (?$(G]p(B . "0x9589") ; <CJK>
       (?$(G]q(B . "0x966A") ; <CJK>
       (?$(G]r(B . "0x9675") ; <CJK>
       (?$(G]s(B . "0x9673") ; <CJK>
       (?$(G]t(B . "0x9678") ; <CJK>
       (?$(G]u(B . "0x9670") ; <CJK>
       (?$(G]v(B . "0x9674") ; <CJK>
       (?$(G]w(B . "0x9676") ; <CJK>
       (?$(G]x(B . "0x9677") ; <CJK>
       (?$(G]y(B . "0x966C") ; <CJK>
       (?$(G]z(B . "0x96C0") ; <CJK>
       (?$(G]{(B . "0x96EA") ; <CJK>
       (?$(G]|(B . "0x96E9") ; <CJK>
       (?$(G]}(B . "0x7AE0") ; <CJK>
       (?$(G]~(B . "0x7ADF") ; <CJK>
       (?$(G^!(B . "0x9802") ; <CJK>
       (?$(G^"(B . "0x9803") ; <CJK>
       (?$(G^#(B . "0x9B5A") ; <CJK>
       (?$(G^$(B . "0x9CE5") ; <CJK>
       (?$(G^%(B . "0x9E75") ; <CJK>
       (?$(G^&(B . "0x9E7F") ; <CJK>
       (?$(G^'(B . "0x9EA5") ; <CJK>
       (?$(G^((B . "0x9EBB") ; <CJK>
       (?$(G^)(B . "0x50A2") ; <CJK>
       (?$(G^*(B . "0x508D") ; <CJK>
       (?$(G^+(B . "0x5085") ; <CJK>
       (?$(G^,(B . "0x5099") ; <CJK>
       (?$(G^-(B . "0x5091") ; <CJK>
       (?$(G^.(B . "0x5080") ; <CJK>
       (?$(G^/(B . "0x5096") ; <CJK>
       (?$(G^0(B . "0x5098") ; <CJK>
       (?$(G^1(B . "0x509A") ; <CJK>
       (?$(G^2(B . "0x6700") ; <CJK>
       (?$(G^3(B . "0x51F1") ; <CJK>
       (?$(G^4(B . "0x5272") ; <CJK>
       (?$(G^5(B . "0x5274") ; <CJK>
       (?$(G^6(B . "0x5275") ; <CJK>
       (?$(G^7(B . "0x5269") ; <CJK>
       (?$(G^8(B . "0x52DE") ; <CJK>
       (?$(G^9(B . "0x52DD") ; <CJK>
       (?$(G^:(B . "0x52DB") ; <CJK>
       (?$(G^;(B . "0x535A") ; <CJK>
       (?$(G^<(B . "0x53A5") ; <CJK>
       (?$(G^=(B . "0x557B") ; <CJK>
       (?$(G^>(B . "0x5580") ; <CJK>
       (?$(G^?(B . "0x55A7") ; <CJK>
       (?$(G^@(B . "0x557C") ; <CJK>
       (?$(G^A(B . "0x558A") ; <CJK>
       (?$(G^B(B . "0x559D") ; <CJK>
       (?$(G^C(B . "0x5598") ; <CJK>
       (?$(G^D(B . "0x5582") ; <CJK>
       (?$(G^E(B . "0x559C") ; <CJK>
       (?$(G^F(B . "0x55AA") ; <CJK>
       (?$(G^G(B . "0x5594") ; <CJK>
       (?$(G^H(B . "0x5587") ; <CJK>
       (?$(G^I(B . "0x558B") ; <CJK>
       (?$(G^J(B . "0x5583") ; <CJK>
       (?$(G^K(B . "0x55B3") ; <CJK>
       (?$(G^L(B . "0x55AE") ; <CJK>
       (?$(G^M(B . "0x559F") ; <CJK>
       (?$(G^N(B . "0x553E") ; <CJK>
       (?$(G^O(B . "0x55B2") ; <CJK>
       (?$(G^P(B . "0x559A") ; <CJK>
       (?$(G^Q(B . "0x55BB") ; <CJK>
       (?$(G^R(B . "0x55AC") ; <CJK>
       (?$(G^S(B . "0x55B1") ; <CJK>
       (?$(G^T(B . "0x557E") ; <CJK>
       (?$(G^U(B . "0x5589") ; <CJK>
       (?$(G^V(B . "0x55AB") ; <CJK>
       (?$(G^W(B . "0x5599") ; <CJK>
       (?$(G^X(B . "0x570D") ; <CJK>
       (?$(G^Y(B . "0x582F") ; <CJK>
       (?$(G^Z(B . "0x582A") ; <CJK>
       (?$(G^[(B . "0x5834") ; <CJK>
       (?$(G^\(B . "0x5824") ; <CJK>
       (?$(G^](B . "0x5830") ; <CJK>
       (?$(G^^(B . "0x5831") ; <CJK>
       (?$(G^_(B . "0x5821") ; <CJK>
       (?$(G^`(B . "0x581D") ; <CJK>
       (?$(G^a(B . "0x5820") ; <CJK>
       (?$(G^b(B . "0x58F9") ; <CJK>
       (?$(G^c(B . "0x58FA") ; <CJK>
       (?$(G^d(B . "0x5960") ; <CJK>
       (?$(G^e(B . "0x5A77") ; <CJK>
       (?$(G^f(B . "0x5A9A") ; <CJK>
       (?$(G^g(B . "0x5A7F") ; <CJK>
       (?$(G^h(B . "0x5A92") ; <CJK>
       (?$(G^i(B . "0x5A9B") ; <CJK>
       (?$(G^j(B . "0x5AA7") ; <CJK>
       (?$(G^k(B . "0x5B73") ; <CJK>
       (?$(G^l(B . "0x5B71") ; <CJK>
       (?$(G^m(B . "0x5BD2") ; <CJK>
       (?$(G^n(B . "0x5BCC") ; <CJK>
       (?$(G^o(B . "0x5BD3") ; <CJK>
       (?$(G^p(B . "0x5BD0") ; <CJK>
       (?$(G^q(B . "0x5C0A") ; <CJK>
       (?$(G^r(B . "0x5C0B") ; <CJK>
       (?$(G^s(B . "0x5C31") ; <CJK>
       (?$(G^t(B . "0x5D4C") ; <CJK>
       (?$(G^u(B . "0x5D50") ; <CJK>
       (?$(G^v(B . "0x5D34") ; <CJK>
       (?$(G^w(B . "0x5D47") ; <CJK>
       (?$(G^x(B . "0x5DFD") ; <CJK>
       (?$(G^y(B . "0x5E45") ; <CJK>
       (?$(G^z(B . "0x5E3D") ; <CJK>
       (?$(G^{(B . "0x5E40") ; <CJK>
       (?$(G^|(B . "0x5E43") ; <CJK>
       (?$(G^}(B . "0x5E7E") ; <CJK>
       (?$(G^~(B . "0x5ECA") ; <CJK>
       (?$(G_!(B . "0x5EC1") ; <CJK>
       (?$(G_"(B . "0x5EC2") ; <CJK>
       (?$(G_#(B . "0x5EC4") ; <CJK>
       (?$(G_$(B . "0x5F3C") ; <CJK>
       (?$(G_%(B . "0x5F6D") ; <CJK>
       (?$(G_&(B . "0x5FA9") ; <CJK>
       (?$(G_'(B . "0x5FAA") ; <CJK>
       (?$(G_((B . "0x5FA8") ; <CJK>
       (?$(G_)(B . "0x60D1") ; <CJK>
       (?$(G_*(B . "0x60E1") ; <CJK>
       (?$(G_+(B . "0x60B2") ; <CJK>
       (?$(G_,(B . "0x60B6") ; <CJK>
       (?$(G_-(B . "0x60E0") ; <CJK>
       (?$(G_.(B . "0x611C") ; <CJK>
       (?$(G_/(B . "0x6123") ; <CJK>
       (?$(G_0(B . "0x60FA") ; <CJK>
       (?$(G_1(B . "0x6115") ; <CJK>
       (?$(G_2(B . "0x60F0") ; <CJK>
       (?$(G_3(B . "0x60FB") ; <CJK>
       (?$(G_4(B . "0x60F4") ; <CJK>
       (?$(G_5(B . "0x6168") ; <CJK>
       (?$(G_6(B . "0x60F1") ; <CJK>
       (?$(G_7(B . "0x610E") ; <CJK>
       (?$(G_8(B . "0x60F6") ; <CJK>
       (?$(G_9(B . "0x6109") ; <CJK>
       (?$(G_:(B . "0x6100") ; <CJK>
       (?$(G_;(B . "0x6112") ; <CJK>
       (?$(G_<(B . "0x621F") ; <CJK>
       (?$(G_=(B . "0x6249") ; <CJK>
       (?$(G_>(B . "0x63A3") ; <CJK>
       (?$(G_?(B . "0x638C") ; <CJK>
       (?$(G_@(B . "0x63CF") ; <CJK>
       (?$(G_A(B . "0x63C0") ; <CJK>
       (?$(G_B(B . "0x63E9") ; <CJK>
       (?$(G_C(B . "0x63C9") ; <CJK>
       (?$(G_D(B . "0x63C6") ; <CJK>
       (?$(G_E(B . "0x63CD") ; <CJK>
       (?$(G_F(B . "0x63D2") ; <CJK>
       (?$(G_G(B . "0x63E3") ; <CJK>
       (?$(G_H(B . "0x63D0") ; <CJK>
       (?$(G_I(B . "0x63E1") ; <CJK>
       (?$(G_J(B . "0x63D6") ; <CJK>
       (?$(G_K(B . "0x63ED") ; <CJK>
       (?$(G_L(B . "0x63EE") ; <CJK>
       (?$(G_M(B . "0x6376") ; <CJK>
       (?$(G_N(B . "0x63F4") ; <CJK>
       (?$(G_O(B . "0x63EA") ; <CJK>
       (?$(G_P(B . "0x63DB") ; <CJK>
       (?$(G_Q(B . "0x6452") ; <CJK>
       (?$(G_R(B . "0x63DA") ; <CJK>
       (?$(G_S(B . "0x63F9") ; <CJK>
       (?$(G_T(B . "0x655E") ; <CJK>
       (?$(G_U(B . "0x6566") ; <CJK>
       (?$(G_V(B . "0x6562") ; <CJK>
       (?$(G_W(B . "0x6563") ; <CJK>
       (?$(G_X(B . "0x6591") ; <CJK>
       (?$(G_Y(B . "0x6590") ; <CJK>
       (?$(G_Z(B . "0x65AF") ; <CJK>
       (?$(G_[(B . "0x666E") ; <CJK>
       (?$(G_\(B . "0x6670") ; <CJK>
       (?$(G_](B . "0x6674") ; <CJK>
       (?$(G_^(B . "0x6676") ; <CJK>
       (?$(G__(B . "0x666F") ; <CJK>
       (?$(G_`(B . "0x6691") ; <CJK>
       (?$(G_a(B . "0x667A") ; <CJK>
       (?$(G_b(B . "0x667E") ; <CJK>
       (?$(G_c(B . "0x6677") ; <CJK>
       (?$(G_d(B . "0x66FE") ; <CJK>
       (?$(G_e(B . "0x66FF") ; <CJK>
       (?$(G_f(B . "0x671F") ; <CJK>
       (?$(G_g(B . "0x671D") ; <CJK>
       (?$(G_h(B . "0x68FA") ; <CJK>
       (?$(G_i(B . "0x68D5") ; <CJK>
       (?$(G_j(B . "0x68E0") ; <CJK>
       (?$(G_k(B . "0x68D8") ; <CJK>
       (?$(G_l(B . "0x68D7") ; <CJK>
       (?$(G_m(B . "0x6905") ; <CJK>
       (?$(G_n(B . "0x68DF") ; <CJK>
       (?$(G_o(B . "0x68F5") ; <CJK>
       (?$(G_p(B . "0x68EE") ; <CJK>
       (?$(G_q(B . "0x68E7") ; <CJK>
       (?$(G_r(B . "0x68F9") ; <CJK>
       (?$(G_s(B . "0x68D2") ; <CJK>
       (?$(G_t(B . "0x68F2") ; <CJK>
       (?$(G_u(B . "0x68E3") ; <CJK>
       (?$(G_v(B . "0x68CB") ; <CJK>
       (?$(G_w(B . "0x68CD") ; <CJK>
       (?$(G_x(B . "0x690D") ; <CJK>
       (?$(G_y(B . "0x6912") ; <CJK>
       (?$(G_z(B . "0x690E") ; <CJK>
       (?$(G_{(B . "0x68C9") ; <CJK>
       (?$(G_|(B . "0x68DA") ; <CJK>
       (?$(G_}(B . "0x696E") ; <CJK>
       (?$(G_~(B . "0x68FB") ; <CJK>
       (?$(G`!(B . "0x6B3E") ; <CJK>
       (?$(G`"(B . "0x6B3A") ; <CJK>
       (?$(G`#(B . "0x6B3D") ; <CJK>
       (?$(G`$(B . "0x6B98") ; <CJK>
       (?$(G`%(B . "0x6B96") ; <CJK>
       (?$(G`&(B . "0x6BBC") ; <CJK>
       (?$(G`'(B . "0x6BEF") ; <CJK>
       (?$(G`((B . "0x6C2E") ; <CJK>
       (?$(G`)(B . "0x6C2F") ; <CJK>
       (?$(G`*(B . "0x6C2C") ; <CJK>
       (?$(G`+(B . "0x6E2F") ; <CJK>
       (?$(G`,(B . "0x6E38") ; <CJK>
       (?$(G`-(B . "0x6E54") ; <CJK>
       (?$(G`.(B . "0x6E21") ; <CJK>
       (?$(G`/(B . "0x6E32") ; <CJK>
       (?$(G`0(B . "0x6E67") ; <CJK>
       (?$(G`1(B . "0x6E4A") ; <CJK>
       (?$(G`2(B . "0x6E20") ; <CJK>
       (?$(G`3(B . "0x6E25") ; <CJK>
       (?$(G`4(B . "0x6E23") ; <CJK>
       (?$(G`5(B . "0x6E1B") ; <CJK>
       (?$(G`6(B . "0x6E5B") ; <CJK>
       (?$(G`7(B . "0x6E58") ; <CJK>
       (?$(G`8(B . "0x6E24") ; <CJK>
       (?$(G`9(B . "0x6E56") ; <CJK>
       (?$(G`:(B . "0x6E6E") ; <CJK>
       (?$(G`;(B . "0x6E2D") ; <CJK>
       (?$(G`<(B . "0x6E26") ; <CJK>
       (?$(G`=(B . "0x6E6F") ; <CJK>
       (?$(G`>(B . "0x6E34") ; <CJK>
       (?$(G`?(B . "0x6E4D") ; <CJK>
       (?$(G`@(B . "0x6E3A") ; <CJK>
       (?$(G`A(B . "0x6E2C") ; <CJK>
       (?$(G`B(B . "0x6E43") ; <CJK>
       (?$(G`C(B . "0x6E1D") ; <CJK>
       (?$(G`D(B . "0x6E3E") ; <CJK>
       (?$(G`E(B . "0x6ECB") ; <CJK>
       (?$(G`F(B . "0x6E89") ; <CJK>
       (?$(G`G(B . "0x6E19") ; <CJK>
       (?$(G`H(B . "0x6E4E") ; <CJK>
       (?$(G`I(B . "0x6E63") ; <CJK>
       (?$(G`J(B . "0x6E44") ; <CJK>
       (?$(G`K(B . "0x6E72") ; <CJK>
       (?$(G`L(B . "0x6E69") ; <CJK>
       (?$(G`M(B . "0x6E5F") ; <CJK>
       (?$(G`N(B . "0x7119") ; <CJK>
       (?$(G`O(B . "0x711A") ; <CJK>
       (?$(G`P(B . "0x7126") ; <CJK>
       (?$(G`Q(B . "0x7130") ; <CJK>
       (?$(G`R(B . "0x7121") ; <CJK>
       (?$(G`S(B . "0x7136") ; <CJK>
       (?$(G`T(B . "0x716E") ; <CJK>
       (?$(G`U(B . "0x711C") ; <CJK>
       (?$(G`V(B . "0x724C") ; <CJK>
       (?$(G`W(B . "0x7284") ; <CJK>
       (?$(G`X(B . "0x7280") ; <CJK>
       (?$(G`Y(B . "0x7336") ; <CJK>
       (?$(G`Z(B . "0x7325") ; <CJK>
       (?$(G`[(B . "0x7334") ; <CJK>
       (?$(G`\(B . "0x7329") ; <CJK>
       (?$(G`](B . "0x743A") ; <CJK>
       (?$(G`^(B . "0x742A") ; <CJK>
       (?$(G`_(B . "0x7433") ; <CJK>
       (?$(G``(B . "0x7422") ; <CJK>
       (?$(G`a(B . "0x7425") ; <CJK>
       (?$(G`b(B . "0x7435") ; <CJK>
       (?$(G`c(B . "0x7436") ; <CJK>
       (?$(G`d(B . "0x7434") ; <CJK>
       (?$(G`e(B . "0x742F") ; <CJK>
       (?$(G`f(B . "0x741B") ; <CJK>
       (?$(G`g(B . "0x7426") ; <CJK>
       (?$(G`h(B . "0x7428") ; <CJK>
       (?$(G`i(B . "0x7525") ; <CJK>
       (?$(G`j(B . "0x7526") ; <CJK>
       (?$(G`k(B . "0x756B") ; <CJK>
       (?$(G`l(B . "0x756A") ; <CJK>
       (?$(G`m(B . "0x75E2") ; <CJK>
       (?$(G`n(B . "0x75DB") ; <CJK>
       (?$(G`o(B . "0x75E3") ; <CJK>
       (?$(G`p(B . "0x75D9") ; <CJK>
       (?$(G`q(B . "0x75D8") ; <CJK>
       (?$(G`r(B . "0x75DE") ; <CJK>
       (?$(G`s(B . "0x75E0") ; <CJK>
       (?$(G`t(B . "0x767B") ; <CJK>
       (?$(G`u(B . "0x767C") ; <CJK>
       (?$(G`v(B . "0x7696") ; <CJK>
       (?$(G`w(B . "0x7693") ; <CJK>
       (?$(G`x(B . "0x76B4") ; <CJK>
       (?$(G`y(B . "0x76DC") ; <CJK>
       (?$(G`z(B . "0x774F") ; <CJK>
       (?$(G`{(B . "0x77ED") ; <CJK>
       (?$(G`|(B . "0x785D") ; <CJK>
       (?$(G`}(B . "0x786C") ; <CJK>
       (?$(G`~(B . "0x786F") ; <CJK>
       (?$(Ga!(B . "0x7A0D") ; <CJK>
       (?$(Ga"(B . "0x7A08") ; <CJK>
       (?$(Ga#(B . "0x7A0B") ; <CJK>
       (?$(Ga$(B . "0x7A05") ; <CJK>
       (?$(Ga%(B . "0x7A00") ; <CJK>
       (?$(Ga&(B . "0x7A98") ; <CJK>
       (?$(Ga'(B . "0x7A97") ; <CJK>
       (?$(Ga((B . "0x7A96") ; <CJK>
       (?$(Ga)(B . "0x7AE5") ; <CJK>
       (?$(Ga*(B . "0x7AE3") ; <CJK>
       (?$(Ga+(B . "0x7B49") ; <CJK>
       (?$(Ga,(B . "0x7B56") ; <CJK>
       (?$(Ga-(B . "0x7B46") ; <CJK>
       (?$(Ga.(B . "0x7B50") ; <CJK>
       (?$(Ga/(B . "0x7B52") ; <CJK>
       (?$(Ga0(B . "0x7B54") ; <CJK>
       (?$(Ga1(B . "0x7B4D") ; <CJK>
       (?$(Ga2(B . "0x7B4B") ; <CJK>
       (?$(Ga3(B . "0x7B4F") ; <CJK>
       (?$(Ga4(B . "0x7B51") ; <CJK>
       (?$(Ga5(B . "0x7C9F") ; <CJK>
       (?$(Ga6(B . "0x7CA5") ; <CJK>
       (?$(Ga7(B . "0x7D5E") ; <CJK>
       (?$(Ga8(B . "0x7D50") ; <CJK>
       (?$(Ga9(B . "0x7D68") ; <CJK>
       (?$(Ga:(B . "0x7D55") ; <CJK>
       (?$(Ga;(B . "0x7D2B") ; <CJK>
       (?$(Ga<(B . "0x7D6E") ; <CJK>
       (?$(Ga=(B . "0x7D72") ; <CJK>
       (?$(Ga>(B . "0x7D61") ; <CJK>
       (?$(Ga?(B . "0x7D66") ; <CJK>
       (?$(Ga@(B . "0x7D62") ; <CJK>
       (?$(GaA(B . "0x7D70") ; <CJK>
       (?$(GaB(B . "0x7D73") ; <CJK>
       (?$(GaC(B . "0x5584") ; <CJK>
       (?$(GaD(B . "0x7FD4") ; <CJK>
       (?$(GaE(B . "0x7FD5") ; <CJK>
       (?$(GaF(B . "0x800B") ; <CJK>
       (?$(GaG(B . "0x8052") ; <CJK>
       (?$(GaH(B . "0x8085") ; <CJK>
       (?$(GaI(B . "0x8155") ; <CJK>
       (?$(GaJ(B . "0x8154") ; <CJK>
       (?$(GaK(B . "0x814B") ; <CJK>
       (?$(GaL(B . "0x8151") ; <CJK>
       (?$(GaM(B . "0x814E") ; <CJK>
       (?$(GaN(B . "0x8139") ; <CJK>
       (?$(GaO(B . "0x8146") ; <CJK>
       (?$(GaP(B . "0x813E") ; <CJK>
       (?$(GaQ(B . "0x814C") ; <CJK>
       (?$(GaR(B . "0x8153") ; <CJK>
       (?$(GaS(B . "0x8174") ; <CJK>
       (?$(GaT(B . "0x8212") ; <CJK>
       (?$(GaU(B . "0x821C") ; <CJK>
       (?$(GaV(B . "0x83E9") ; <CJK>
       (?$(GaW(B . "0x8403") ; <CJK>
       (?$(GaX(B . "0x83F8") ; <CJK>
       (?$(GaY(B . "0x840D") ; <CJK>
       (?$(GaZ(B . "0x83E0") ; <CJK>
       (?$(Ga[(B . "0x83C5") ; <CJK>
       (?$(Ga\(B . "0x840B") ; <CJK>
       (?$(Ga](B . "0x83C1") ; <CJK>
       (?$(Ga^(B . "0x83EF") ; <CJK>
       (?$(Ga_(B . "0x83F1") ; <CJK>
       (?$(Ga`(B . "0x83F4") ; <CJK>
       (?$(Gaa(B . "0x8457") ; <CJK>
       (?$(Gab(B . "0x840A") ; <CJK>
       (?$(Gac(B . "0x83F0") ; <CJK>
       (?$(Gad(B . "0x840C") ; <CJK>
       (?$(Gae(B . "0x83CC") ; <CJK>
       (?$(Gaf(B . "0x83FD") ; <CJK>
       (?$(Gag(B . "0x83F2") ; <CJK>
       (?$(Gah(B . "0x83CA") ; <CJK>
       (?$(Gai(B . "0x8438") ; <CJK>
       (?$(Gaj(B . "0x840E") ; <CJK>
       (?$(Gak(B . "0x8404") ; <CJK>
       (?$(Gal(B . "0x83DC") ; <CJK>
       (?$(Gam(B . "0x8407") ; <CJK>
       (?$(Gan(B . "0x83D4") ; <CJK>
       (?$(Gao(B . "0x83DF") ; <CJK>
       (?$(Gap(B . "0x865B") ; <CJK>
       (?$(Gaq(B . "0x86DF") ; <CJK>
       (?$(Gar(B . "0x86D9") ; <CJK>
       (?$(Gas(B . "0x86ED") ; <CJK>
       (?$(Gat(B . "0x86D4") ; <CJK>
       (?$(Gau(B . "0x86DB") ; <CJK>
       (?$(Gav(B . "0x86E4") ; <CJK>
       (?$(Gaw(B . "0x86D0") ; <CJK>
       (?$(Gax(B . "0x86DE") ; <CJK>
       (?$(Gay(B . "0x8857") ; <CJK>
       (?$(Gaz(B . "0x88C1") ; <CJK>
       (?$(Ga{(B . "0x88C2") ; <CJK>
       (?$(Ga|(B . "0x88B1") ; <CJK>
       (?$(Ga}(B . "0x8983") ; <CJK>
       (?$(Ga~(B . "0x8996") ; <CJK>
       (?$(Gb!(B . "0x8A3B") ; <CJK>
       (?$(Gb"(B . "0x8A60") ; <CJK>
       (?$(Gb#(B . "0x8A55") ; <CJK>
       (?$(Gb$(B . "0x8A5E") ; <CJK>
       (?$(Gb%(B . "0x8A3C") ; <CJK>
       (?$(Gb&(B . "0x8A41") ; <CJK>
       (?$(Gb'(B . "0x8A54") ; <CJK>
       (?$(Gb((B . "0x8A5B") ; <CJK>
       (?$(Gb)(B . "0x8A50") ; <CJK>
       (?$(Gb*(B . "0x8A46") ; <CJK>
       (?$(Gb+(B . "0x8A34") ; <CJK>
       (?$(Gb,(B . "0x8A3A") ; <CJK>
       (?$(Gb-(B . "0x8A36") ; <CJK>
       (?$(Gb.(B . "0x8A56") ; <CJK>
       (?$(Gb/(B . "0x8C61") ; <CJK>
       (?$(Gb0(B . "0x8C82") ; <CJK>
       (?$(Gb1(B . "0x8CAF") ; <CJK>
       (?$(Gb2(B . "0x8CBC") ; <CJK>
       (?$(Gb3(B . "0x8CB3") ; <CJK>
       (?$(Gb4(B . "0x8CBD") ; <CJK>
       (?$(Gb5(B . "0x8CC1") ; <CJK>
       (?$(Gb6(B . "0x8CBB") ; <CJK>
       (?$(Gb7(B . "0x8CC0") ; <CJK>
       (?$(Gb8(B . "0x8CB4") ; <CJK>
       (?$(Gb9(B . "0x8CB7") ; <CJK>
       (?$(Gb:(B . "0x8CB6") ; <CJK>
       (?$(Gb;(B . "0x8CBF") ; <CJK>
       (?$(Gb<(B . "0x8CB8") ; <CJK>
       (?$(Gb=(B . "0x8D8A") ; <CJK>
       (?$(Gb>(B . "0x8D85") ; <CJK>
       (?$(Gb?(B . "0x8D81") ; <CJK>
       (?$(Gb@(B . "0x8DCE") ; <CJK>
       (?$(GbA(B . "0x8DDD") ; <CJK>
       (?$(GbB(B . "0x8DCB") ; <CJK>
       (?$(GbC(B . "0x8DDA") ; <CJK>
       (?$(GbD(B . "0x8DD1") ; <CJK>
       (?$(GbE(B . "0x8DCC") ; <CJK>
       (?$(GbF(B . "0x8DDB") ; <CJK>
       (?$(GbG(B . "0x8DC6") ; <CJK>
       (?$(GbH(B . "0x8EFB") ; <CJK>
       (?$(GbI(B . "0x8EF8") ; <CJK>
       (?$(GbJ(B . "0x8EFC") ; <CJK>
       (?$(GbK(B . "0x8F9C") ; <CJK>
       (?$(GbL(B . "0x902E") ; <CJK>
       (?$(GbM(B . "0x9035") ; <CJK>
       (?$(GbN(B . "0x9031") ; <CJK>
       (?$(GbO(B . "0x9038") ; <CJK>
       (?$(GbP(B . "0x9032") ; <CJK>
       (?$(GbQ(B . "0x9036") ; <CJK>
       (?$(GbR(B . "0x9102") ; <CJK>
       (?$(GbS(B . "0x90F5") ; <CJK>
       (?$(GbT(B . "0x9109") ; <CJK>
       (?$(GbU(B . "0x90FE") ; <CJK>
       (?$(GbV(B . "0x9163") ; <CJK>
       (?$(GbW(B . "0x9165") ; <CJK>
       (?$(GbX(B . "0x91CF") ; <CJK>
       (?$(GbY(B . "0x9214") ; <CJK>
       (?$(GbZ(B . "0x9215") ; <CJK>
       (?$(Gb[(B . "0x9223") ; <CJK>
       (?$(Gb\(B . "0x9209") ; <CJK>
       (?$(Gb](B . "0x921E") ; <CJK>
       (?$(Gb^(B . "0x920D") ; <CJK>
       (?$(Gb_(B . "0x9210") ; <CJK>
       (?$(Gb`(B . "0x9207") ; <CJK>
       (?$(Gba(B . "0x9211") ; <CJK>
       (?$(Gbb(B . "0x9594") ; <CJK>
       (?$(Gbc(B . "0x958F") ; <CJK>
       (?$(Gbd(B . "0x958B") ; <CJK>
       (?$(Gbe(B . "0x9591") ; <CJK>
       (?$(Gbf(B . "0x9593") ; <CJK>
       (?$(Gbg(B . "0x9592") ; <CJK>
       (?$(Gbh(B . "0x958E") ; <CJK>
       (?$(Gbi(B . "0x968A") ; <CJK>
       (?$(Gbj(B . "0x968E") ; <CJK>
       (?$(Gbk(B . "0x968B") ; <CJK>
       (?$(Gbl(B . "0x967D") ; <CJK>
       (?$(Gbm(B . "0x9685") ; <CJK>
       (?$(Gbn(B . "0x9686") ; <CJK>
       (?$(Gbo(B . "0x968D") ; <CJK>
       (?$(Gbp(B . "0x9672") ; <CJK>
       (?$(Gbq(B . "0x9684") ; <CJK>
       (?$(Gbr(B . "0x96C1") ; <CJK>
       (?$(Gbs(B . "0x96C5") ; <CJK>
       (?$(Gbt(B . "0x96C4") ; <CJK>
       (?$(Gbu(B . "0x96C6") ; <CJK>
       (?$(Gbv(B . "0x96C7") ; <CJK>
       (?$(Gbw(B . "0x96EF") ; <CJK>
       (?$(Gbx(B . "0x96F2") ; <CJK>
       (?$(Gby(B . "0x97CC") ; <CJK>
       (?$(Gbz(B . "0x9805") ; <CJK>
       (?$(Gb{(B . "0x9806") ; <CJK>
       (?$(Gb|(B . "0x9808") ; <CJK>
       (?$(Gb}(B . "0x98E7") ; <CJK>
       (?$(Gb~(B . "0x98EA") ; <CJK>
       (?$(Gc!(B . "0x98EF") ; <CJK>
       (?$(Gc"(B . "0x98E9") ; <CJK>
       (?$(Gc#(B . "0x98F2") ; <CJK>
       (?$(Gc$(B . "0x98ED") ; <CJK>
       (?$(Gc%(B . "0x99AE") ; <CJK>
       (?$(Gc&(B . "0x99AD") ; <CJK>
       (?$(Gc'(B . "0x9EC3") ; <CJK>
       (?$(Gc((B . "0x9ECD") ; <CJK>
       (?$(Gc)(B . "0x9ED1") ; <CJK>
       (?$(Gc*(B . "0x4E82") ; <CJK>
       (?$(Gc+(B . "0x50AD") ; <CJK>
       (?$(Gc,(B . "0x50B5") ; <CJK>
       (?$(Gc-(B . "0x50B2") ; <CJK>
       (?$(Gc.(B . "0x50B3") ; <CJK>
       (?$(Gc/(B . "0x50C5") ; <CJK>
       (?$(Gc0(B . "0x50BE") ; <CJK>
       (?$(Gc1(B . "0x50AC") ; <CJK>
       (?$(Gc2(B . "0x50B7") ; <CJK>
       (?$(Gc3(B . "0x50BB") ; <CJK>
       (?$(Gc4(B . "0x50AF") ; <CJK>
       (?$(Gc5(B . "0x50C7") ; <CJK>
       (?$(Gc6(B . "0x527F") ; <CJK>
       (?$(Gc7(B . "0x5277") ; <CJK>
       (?$(Gc8(B . "0x527D") ; <CJK>
       (?$(Gc9(B . "0x52DF") ; <CJK>
       (?$(Gc:(B . "0x52E6") ; <CJK>
       (?$(Gc;(B . "0x52E4") ; <CJK>
       (?$(Gc<(B . "0x52E2") ; <CJK>
       (?$(Gc=(B . "0x52E3") ; <CJK>
       (?$(Gc>(B . "0x532F") ; <CJK>
       (?$(Gc?(B . "0x55DF") ; <CJK>
       (?$(Gc@(B . "0x55E8") ; <CJK>
       (?$(GcA(B . "0x55D3") ; <CJK>
       (?$(GcB(B . "0x55E6") ; <CJK>
       (?$(GcC(B . "0x55CE") ; <CJK>
       (?$(GcD(B . "0x55DC") ; <CJK>
       (?$(GcE(B . "0x55C7") ; <CJK>
       (?$(GcF(B . "0x55D1") ; <CJK>
       (?$(GcG(B . "0x55E3") ; <CJK>
       (?$(GcH(B . "0x55E4") ; <CJK>
       (?$(GcI(B . "0x55EF") ; <CJK>
       (?$(GcJ(B . "0x55DA") ; <CJK>
       (?$(GcK(B . "0x55E1") ; <CJK>
       (?$(GcL(B . "0x55C5") ; <CJK>
       (?$(GcM(B . "0x55C6") ; <CJK>
       (?$(GcN(B . "0x55E5") ; <CJK>
       (?$(GcO(B . "0x55C9") ; <CJK>
       (?$(GcP(B . "0x5712") ; <CJK>
       (?$(GcQ(B . "0x5713") ; <CJK>
       (?$(GcR(B . "0x585E") ; <CJK>
       (?$(GcS(B . "0x5851") ; <CJK>
       (?$(GcT(B . "0x5858") ; <CJK>
       (?$(GcU(B . "0x5857") ; <CJK>
       (?$(GcV(B . "0x585A") ; <CJK>
       (?$(GcW(B . "0x5854") ; <CJK>
       (?$(GcX(B . "0x586B") ; <CJK>
       (?$(GcY(B . "0x584C") ; <CJK>
       (?$(GcZ(B . "0x586D") ; <CJK>
       (?$(Gc[(B . "0x584A") ; <CJK>
       (?$(Gc\(B . "0x5862") ; <CJK>
       (?$(Gc](B . "0x5852") ; <CJK>
       (?$(Gc^(B . "0x584B") ; <CJK>
       (?$(Gc_(B . "0x5967") ; <CJK>
       (?$(Gc`(B . "0x5AC1") ; <CJK>
       (?$(Gca(B . "0x5AC9") ; <CJK>
       (?$(Gcb(B . "0x5ACC") ; <CJK>
       (?$(Gcc(B . "0x5ABE") ; <CJK>
       (?$(Gcd(B . "0x5ABD") ; <CJK>
       (?$(Gce(B . "0x5ABC") ; <CJK>
       (?$(Gcf(B . "0x5AB3") ; <CJK>
       (?$(Gcg(B . "0x5AC2") ; <CJK>
       (?$(Gch(B . "0x5AB2") ; <CJK>
       (?$(Gci(B . "0x5D69") ; <CJK>
       (?$(Gcj(B . "0x5D6F") ; <CJK>
       (?$(Gck(B . "0x5E4C") ; <CJK>
       (?$(Gcl(B . "0x5E79") ; <CJK>
       (?$(Gcm(B . "0x5EC9") ; <CJK>
       (?$(Gcn(B . "0x5EC8") ; <CJK>
       (?$(Gco(B . "0x5F12") ; <CJK>
       (?$(Gcp(B . "0x5F59") ; <CJK>
       (?$(Gcq(B . "0x5FAC") ; <CJK>
       (?$(Gcr(B . "0x5FAE") ; <CJK>
       (?$(Gcs(B . "0x611A") ; <CJK>
       (?$(Gct(B . "0x610F") ; <CJK>
       (?$(Gcu(B . "0x6148") ; <CJK>
       (?$(Gcv(B . "0x611F") ; <CJK>
       (?$(Gcw(B . "0x60F3") ; <CJK>
       (?$(Gcx(B . "0x611B") ; <CJK>
       (?$(Gcy(B . "0x60F9") ; <CJK>
       (?$(Gcz(B . "0x6101") ; <CJK>
       (?$(Gc{(B . "0x6108") ; <CJK>
       (?$(Gc|(B . "0x614E") ; <CJK>
       (?$(Gc}(B . "0x614C") ; <CJK>
       (?$(Gc~(B . "0x6144") ; <CJK>
       (?$(Gd!(B . "0x614D") ; <CJK>
       (?$(Gd"(B . "0x613E") ; <CJK>
       (?$(Gd#(B . "0x6134") ; <CJK>
       (?$(Gd$(B . "0x6127") ; <CJK>
       (?$(Gd%(B . "0x610D") ; <CJK>
       (?$(Gd&(B . "0x6106") ; <CJK>
       (?$(Gd'(B . "0x6137") ; <CJK>
       (?$(Gd((B . "0x6221") ; <CJK>
       (?$(Gd)(B . "0x6222") ; <CJK>
       (?$(Gd*(B . "0x6413") ; <CJK>
       (?$(Gd+(B . "0x643E") ; <CJK>
       (?$(Gd,(B . "0x641E") ; <CJK>
       (?$(Gd-(B . "0x642A") ; <CJK>
       (?$(Gd.(B . "0x642D") ; <CJK>
       (?$(Gd/(B . "0x643D") ; <CJK>
       (?$(Gd0(B . "0x642C") ; <CJK>
       (?$(Gd1(B . "0x640F") ; <CJK>
       (?$(Gd2(B . "0x641C") ; <CJK>
       (?$(Gd3(B . "0x6414") ; <CJK>
       (?$(Gd4(B . "0x640D") ; <CJK>
       (?$(Gd5(B . "0x6436") ; <CJK>
       (?$(Gd6(B . "0x6416") ; <CJK>
       (?$(Gd7(B . "0x6417") ; <CJK>
       (?$(Gd8(B . "0x6406") ; <CJK>
       (?$(Gd9(B . "0x656C") ; <CJK>
       (?$(Gd:(B . "0x659F") ; <CJK>
       (?$(Gd;(B . "0x65B0") ; <CJK>
       (?$(Gd<(B . "0x6697") ; <CJK>
       (?$(Gd=(B . "0x6689") ; <CJK>
       (?$(Gd>(B . "0x6687") ; <CJK>
       (?$(Gd?(B . "0x6688") ; <CJK>
       (?$(Gd@(B . "0x6696") ; <CJK>
       (?$(GdA(B . "0x6684") ; <CJK>
       (?$(GdB(B . "0x6698") ; <CJK>
       (?$(GdC(B . "0x668D") ; <CJK>
       (?$(GdD(B . "0x6703") ; <CJK>
       (?$(GdE(B . "0x6994") ; <CJK>
       (?$(GdF(B . "0x696D") ; <CJK>
       (?$(GdG(B . "0x695A") ; <CJK>
       (?$(GdH(B . "0x6977") ; <CJK>
       (?$(GdI(B . "0x6960") ; <CJK>
       (?$(GdJ(B . "0x6954") ; <CJK>
       (?$(GdK(B . "0x6975") ; <CJK>
       (?$(GdL(B . "0x6930") ; <CJK>
       (?$(GdM(B . "0x6982") ; <CJK>
       (?$(GdN(B . "0x694A") ; <CJK>
       (?$(GdO(B . "0x6968") ; <CJK>
       (?$(GdP(B . "0x696B") ; <CJK>
       (?$(GdQ(B . "0x695E") ; <CJK>
       (?$(GdR(B . "0x6953") ; <CJK>
       (?$(GdS(B . "0x6979") ; <CJK>
       (?$(GdT(B . "0x6986") ; <CJK>
       (?$(GdU(B . "0x695D") ; <CJK>
       (?$(GdV(B . "0x6963") ; <CJK>
       (?$(GdW(B . "0x695B") ; <CJK>
       (?$(GdX(B . "0x6B47") ; <CJK>
       (?$(GdY(B . "0x6B72") ; <CJK>
       (?$(GdZ(B . "0x6BC0") ; <CJK>
       (?$(Gd[(B . "0x6BBF") ; <CJK>
       (?$(Gd\(B . "0x6BD3") ; <CJK>
       (?$(Gd](B . "0x6BFD") ; <CJK>
       (?$(Gd^(B . "0x6EA2") ; <CJK>
       (?$(Gd_(B . "0x6EAF") ; <CJK>
       (?$(Gd`(B . "0x6ED3") ; <CJK>
       (?$(Gda(B . "0x6EB6") ; <CJK>
       (?$(Gdb(B . "0x6EC2") ; <CJK>
       (?$(Gdc(B . "0x6E90") ; <CJK>
       (?$(Gdd(B . "0x6E9D") ; <CJK>
       (?$(Gde(B . "0x6EC7") ; <CJK>
       (?$(Gdf(B . "0x6EC5") ; <CJK>
       (?$(Gdg(B . "0x6EA5") ; <CJK>
       (?$(Gdh(B . "0x6E98") ; <CJK>
       (?$(Gdi(B . "0x6EBC") ; <CJK>
       (?$(Gdj(B . "0x6EBA") ; <CJK>
       (?$(Gdk(B . "0x6EAB") ; <CJK>
       (?$(Gdl(B . "0x6ED1") ; <CJK>
       (?$(Gdm(B . "0x6E96") ; <CJK>
       (?$(Gdn(B . "0x6E9C") ; <CJK>
       (?$(Gdo(B . "0x6EC4") ; <CJK>
       (?$(Gdp(B . "0x6ED4") ; <CJK>
       (?$(Gdq(B . "0x6EAA") ; <CJK>
       (?$(Gdr(B . "0x6EA7") ; <CJK>
       (?$(Gds(B . "0x6EB4") ; <CJK>
       (?$(Gdt(B . "0x714E") ; <CJK>
       (?$(Gdu(B . "0x7159") ; <CJK>
       (?$(Gdv(B . "0x7169") ; <CJK>
       (?$(Gdw(B . "0x7164") ; <CJK>
       (?$(Gdx(B . "0x7149") ; <CJK>
       (?$(Gdy(B . "0x7167") ; <CJK>
       (?$(Gdz(B . "0x715C") ; <CJK>
       (?$(Gd{(B . "0x716C") ; <CJK>
       (?$(Gd|(B . "0x7166") ; <CJK>
       (?$(Gd}(B . "0x714C") ; <CJK>
       (?$(Gd~(B . "0x7165") ; <CJK>
       (?$(Ge!(B . "0x715E") ; <CJK>
       (?$(Ge"(B . "0x7146") ; <CJK>
       (?$(Ge#(B . "0x7168") ; <CJK>
       (?$(Ge$(B . "0x7156") ; <CJK>
       (?$(Ge%(B . "0x723A") ; <CJK>
       (?$(Ge&(B . "0x7252") ; <CJK>
       (?$(Ge'(B . "0x7337") ; <CJK>
       (?$(Ge((B . "0x7345") ; <CJK>
       (?$(Ge)(B . "0x733F") ; <CJK>
       (?$(Ge*(B . "0x733E") ; <CJK>
       (?$(Ge+(B . "0x746F") ; <CJK>
       (?$(Ge,(B . "0x745A") ; <CJK>
       (?$(Ge-(B . "0x7455") ; <CJK>
       (?$(Ge.(B . "0x745F") ; <CJK>
       (?$(Ge/(B . "0x745E") ; <CJK>
       (?$(Ge0(B . "0x7441") ; <CJK>
       (?$(Ge1(B . "0x743F") ; <CJK>
       (?$(Ge2(B . "0x7459") ; <CJK>
       (?$(Ge3(B . "0x745B") ; <CJK>
       (?$(Ge4(B . "0x745C") ; <CJK>
       (?$(Ge5(B . "0x7576") ; <CJK>
       (?$(Ge6(B . "0x7578") ; <CJK>
       (?$(Ge7(B . "0x7600") ; <CJK>
       (?$(Ge8(B . "0x75F0") ; <CJK>
       (?$(Ge9(B . "0x7601") ; <CJK>
       (?$(Ge:(B . "0x75F2") ; <CJK>
       (?$(Ge;(B . "0x75F1") ; <CJK>
       (?$(Ge<(B . "0x75FA") ; <CJK>
       (?$(Ge=(B . "0x75FF") ; <CJK>
       (?$(Ge>(B . "0x75F4") ; <CJK>
       (?$(Ge?(B . "0x75F3") ; <CJK>
       (?$(Ge@(B . "0x76DE") ; <CJK>
       (?$(GeA(B . "0x76DF") ; <CJK>
       (?$(GeB(B . "0x775B") ; <CJK>
       (?$(GeC(B . "0x776B") ; <CJK>
       (?$(GeD(B . "0x7766") ; <CJK>
       (?$(GeE(B . "0x775E") ; <CJK>
       (?$(GeF(B . "0x7763") ; <CJK>
       (?$(GeG(B . "0x7779") ; <CJK>
       (?$(GeH(B . "0x776A") ; <CJK>
       (?$(GeI(B . "0x776C") ; <CJK>
       (?$(GeJ(B . "0x775C") ; <CJK>
       (?$(GeK(B . "0x7765") ; <CJK>
       (?$(GeL(B . "0x7768") ; <CJK>
       (?$(GeM(B . "0x7762") ; <CJK>
       (?$(GeN(B . "0x77EE") ; <CJK>
       (?$(GeO(B . "0x788E") ; <CJK>
       (?$(GeP(B . "0x78B0") ; <CJK>
       (?$(GeQ(B . "0x7897") ; <CJK>
       (?$(GeR(B . "0x7898") ; <CJK>
       (?$(GeS(B . "0x788C") ; <CJK>
       (?$(GeT(B . "0x7889") ; <CJK>
       (?$(GeU(B . "0x787C") ; <CJK>
       (?$(GeV(B . "0x7891") ; <CJK>
       (?$(GeW(B . "0x7893") ; <CJK>
       (?$(GeX(B . "0x787F") ; <CJK>
       (?$(GeY(B . "0x797A") ; <CJK>
       (?$(GeZ(B . "0x797F") ; <CJK>
       (?$(Ge[(B . "0x7981") ; <CJK>
       (?$(Ge\(B . "0x842C") ; <CJK>
       (?$(Ge](B . "0x79BD") ; <CJK>
       (?$(Ge^(B . "0x7A1C") ; <CJK>
       (?$(Ge_(B . "0x7A1A") ; <CJK>
       (?$(Ge`(B . "0x7A20") ; <CJK>
       (?$(Gea(B . "0x7A14") ; <CJK>
       (?$(Geb(B . "0x7A1F") ; <CJK>
       (?$(Gec(B . "0x7A1E") ; <CJK>
       (?$(Ged(B . "0x7A9F") ; <CJK>
       (?$(Gee(B . "0x7AA0") ; <CJK>
       (?$(Gef(B . "0x7B77") ; <CJK>
       (?$(Geg(B . "0x7BC0") ; <CJK>
       (?$(Geh(B . "0x7B60") ; <CJK>
       (?$(Gei(B . "0x7B6E") ; <CJK>
       (?$(Gej(B . "0x7B67") ; <CJK>
       (?$(Gek(B . "0x7CB1") ; <CJK>
       (?$(Gel(B . "0x7CB3") ; <CJK>
       (?$(Gem(B . "0x7CB5") ; <CJK>
       (?$(Gen(B . "0x7D93") ; <CJK>
       (?$(Geo(B . "0x7D79") ; <CJK>
       (?$(Gep(B . "0x7D91") ; <CJK>
       (?$(Geq(B . "0x7D81") ; <CJK>
       (?$(Ger(B . "0x7D8F") ; <CJK>
       (?$(Ges(B . "0x7D5B") ; <CJK>
       (?$(Get(B . "0x7F6E") ; <CJK>
       (?$(Geu(B . "0x7F69") ; <CJK>
       (?$(Gev(B . "0x7F6A") ; <CJK>
       (?$(Gew(B . "0x7F72") ; <CJK>
       (?$(Gex(B . "0x7FA9") ; <CJK>
       (?$(Gey(B . "0x7FA8") ; <CJK>
       (?$(Gez(B . "0x7FA4") ; <CJK>
       (?$(Ge{(B . "0x8056") ; <CJK>
       (?$(Ge|(B . "0x8058") ; <CJK>
       (?$(Ge}(B . "0x8086") ; <CJK>
       (?$(Ge~(B . "0x8084") ; <CJK>
       (?$(Gf!(B . "0x8171") ; <CJK>
       (?$(Gf"(B . "0x8170") ; <CJK>
       (?$(Gf#(B . "0x8178") ; <CJK>
       (?$(Gf$(B . "0x8165") ; <CJK>
       (?$(Gf%(B . "0x816E") ; <CJK>
       (?$(Gf&(B . "0x8173") ; <CJK>
       (?$(Gf'(B . "0x816B") ; <CJK>
       (?$(Gf((B . "0x8179") ; <CJK>
       (?$(Gf)(B . "0x817A") ; <CJK>
       (?$(Gf*(B . "0x8166") ; <CJK>
       (?$(Gf+(B . "0x8205") ; <CJK>
       (?$(Gf,(B . "0x8247") ; <CJK>
       (?$(Gf-(B . "0x8482") ; <CJK>
       (?$(Gf.(B . "0x8477") ; <CJK>
       (?$(Gf/(B . "0x843D") ; <CJK>
       (?$(Gf0(B . "0x8431") ; <CJK>
       (?$(Gf1(B . "0x8475") ; <CJK>
       (?$(Gf2(B . "0x8466") ; <CJK>
       (?$(Gf3(B . "0x846B") ; <CJK>
       (?$(Gf4(B . "0x8449") ; <CJK>
       (?$(Gf5(B . "0x846C") ; <CJK>
       (?$(Gf6(B . "0x845B") ; <CJK>
       (?$(Gf7(B . "0x843C") ; <CJK>
       (?$(Gf8(B . "0x8435") ; <CJK>
       (?$(Gf9(B . "0x8461") ; <CJK>
       (?$(Gf:(B . "0x8463") ; <CJK>
       (?$(Gf;(B . "0x8469") ; <CJK>
       (?$(Gf<(B . "0x846D") ; <CJK>
       (?$(Gf=(B . "0x8446") ; <CJK>
       (?$(Gf>(B . "0x865E") ; <CJK>
       (?$(Gf?(B . "0x865C") ; <CJK>
       (?$(Gf@(B . "0x865F") ; <CJK>
       (?$(GfA(B . "0x86F9") ; <CJK>
       (?$(GfB(B . "0x8713") ; <CJK>
       (?$(GfC(B . "0x8708") ; <CJK>
       (?$(GfD(B . "0x8707") ; <CJK>
       (?$(GfE(B . "0x8700") ; <CJK>
       (?$(GfF(B . "0x86FE") ; <CJK>
       (?$(GfG(B . "0x86FB") ; <CJK>
       (?$(GfH(B . "0x8702") ; <CJK>
       (?$(GfI(B . "0x8703") ; <CJK>
       (?$(GfJ(B . "0x8706") ; <CJK>
       (?$(GfK(B . "0x870A") ; <CJK>
       (?$(GfL(B . "0x8859") ; <CJK>
       (?$(GfM(B . "0x88DF") ; <CJK>
       (?$(GfN(B . "0x88D4") ; <CJK>
       (?$(GfO(B . "0x88D9") ; <CJK>
       (?$(GfP(B . "0x88DC") ; <CJK>
       (?$(GfQ(B . "0x88D8") ; <CJK>
       (?$(GfR(B . "0x88DD") ; <CJK>
       (?$(GfS(B . "0x88E1") ; <CJK>
       (?$(GfT(B . "0x88CA") ; <CJK>
       (?$(GfU(B . "0x88D5") ; <CJK>
       (?$(GfV(B . "0x88D2") ; <CJK>
       (?$(GfW(B . "0x899C") ; <CJK>
       (?$(GfX(B . "0x89E3") ; <CJK>
       (?$(GfY(B . "0x8A6B") ; <CJK>
       (?$(GfZ(B . "0x8A72") ; <CJK>
       (?$(Gf[(B . "0x8A73") ; <CJK>
       (?$(Gf\(B . "0x8A66") ; <CJK>
       (?$(Gf](B . "0x8A69") ; <CJK>
       (?$(Gf^(B . "0x8A70") ; <CJK>
       (?$(Gf_(B . "0x8A87") ; <CJK>
       (?$(Gf`(B . "0x8A7C") ; <CJK>
       (?$(Gfa(B . "0x8A63") ; <CJK>
       (?$(Gfb(B . "0x8AA0") ; <CJK>
       (?$(Gfc(B . "0x8A71") ; <CJK>
       (?$(Gfd(B . "0x8A85") ; <CJK>
       (?$(Gfe(B . "0x8A6D") ; <CJK>
       (?$(Gff(B . "0x8A62") ; <CJK>
       (?$(Gfg(B . "0x8A6E") ; <CJK>
       (?$(Gfh(B . "0x8A6C") ; <CJK>
       (?$(Gfi(B . "0x8A79") ; <CJK>
       (?$(Gfj(B . "0x8A7B") ; <CJK>
       (?$(Gfk(B . "0x8A3E") ; <CJK>
       (?$(Gfl(B . "0x8A68") ; <CJK>
       (?$(Gfm(B . "0x8C62") ; <CJK>
       (?$(Gfn(B . "0x8C8A") ; <CJK>
       (?$(Gfo(B . "0x8C89") ; <CJK>
       (?$(Gfp(B . "0x8CCA") ; <CJK>
       (?$(Gfq(B . "0x8CC7") ; <CJK>
       (?$(Gfr(B . "0x8CC8") ; <CJK>
       (?$(Gfs(B . "0x8CC4") ; <CJK>
       (?$(Gft(B . "0x8CB2") ; <CJK>
       (?$(Gfu(B . "0x8CC3") ; <CJK>
       (?$(Gfv(B . "0x8CC2") ; <CJK>
       (?$(Gfw(B . "0x8CC5") ; <CJK>
       (?$(Gfx(B . "0x8DE1") ; <CJK>
       (?$(Gfy(B . "0x8DDF") ; <CJK>
       (?$(Gfz(B . "0x8DE8") ; <CJK>
       (?$(Gf{(B . "0x8DEF") ; <CJK>
       (?$(Gf|(B . "0x8DF3") ; <CJK>
       (?$(Gf}(B . "0x8DFA") ; <CJK>
       (?$(Gf~(B . "0x8DEA") ; <CJK>
       (?$(Gg!(B . "0x8DE4") ; <CJK>
       (?$(Gg"(B . "0x8DE6") ; <CJK>
       (?$(Gg#(B . "0x8EB2") ; <CJK>
       (?$(Gg$(B . "0x8F03") ; <CJK>
       (?$(Gg%(B . "0x8F09") ; <CJK>
       (?$(Gg&(B . "0x8EFE") ; <CJK>
       (?$(Gg'(B . "0x8F0A") ; <CJK>
       (?$(Gg((B . "0x8F9F") ; <CJK>
       (?$(Gg)(B . "0x8FB2") ; <CJK>
       (?$(Gg*(B . "0x904B") ; <CJK>
       (?$(Gg+(B . "0x904A") ; <CJK>
       (?$(Gg,(B . "0x9053") ; <CJK>
       (?$(Gg-(B . "0x9042") ; <CJK>
       (?$(Gg.(B . "0x9054") ; <CJK>
       (?$(Gg/(B . "0x903C") ; <CJK>
       (?$(Gg0(B . "0x9055") ; <CJK>
       (?$(Gg1(B . "0x9050") ; <CJK>
       (?$(Gg2(B . "0x9047") ; <CJK>
       (?$(Gg3(B . "0x904F") ; <CJK>
       (?$(Gg4(B . "0x904E") ; <CJK>
       (?$(Gg5(B . "0x904D") ; <CJK>
       (?$(Gg6(B . "0x9051") ; <CJK>
       (?$(Gg7(B . "0x903E") ; <CJK>
       (?$(Gg8(B . "0x9041") ; <CJK>
       (?$(Gg9(B . "0x9112") ; <CJK>
       (?$(Gg:(B . "0x9117") ; <CJK>
       (?$(Gg;(B . "0x916C") ; <CJK>
       (?$(Gg<(B . "0x916A") ; <CJK>
       (?$(Gg=(B . "0x9169") ; <CJK>
       (?$(Gg>(B . "0x91C9") ; <CJK>
       (?$(Gg?(B . "0x9237") ; <CJK>
       (?$(Gg@(B . "0x9257") ; <CJK>
       (?$(GgA(B . "0x9238") ; <CJK>
       (?$(GgB(B . "0x923D") ; <CJK>
       (?$(GgC(B . "0x9240") ; <CJK>
       (?$(GgD(B . "0x923E") ; <CJK>
       (?$(GgE(B . "0x925B") ; <CJK>
       (?$(GgF(B . "0x924B") ; <CJK>
       (?$(GgG(B . "0x9264") ; <CJK>
       (?$(GgH(B . "0x9251") ; <CJK>
       (?$(GgI(B . "0x9234") ; <CJK>
       (?$(GgJ(B . "0x9249") ; <CJK>
       (?$(GgK(B . "0x924D") ; <CJK>
       (?$(GgL(B . "0x9245") ; <CJK>
       (?$(GgM(B . "0x9239") ; <CJK>
       (?$(GgN(B . "0x923F") ; <CJK>
       (?$(GgO(B . "0x925A") ; <CJK>
       (?$(GgP(B . "0x9598") ; <CJK>
       (?$(GgQ(B . "0x9698") ; <CJK>
       (?$(GgR(B . "0x9694") ; <CJK>
       (?$(GgS(B . "0x9695") ; <CJK>
       (?$(GgT(B . "0x96CD") ; <CJK>
       (?$(GgU(B . "0x96CB") ; <CJK>
       (?$(GgV(B . "0x96C9") ; <CJK>
       (?$(GgW(B . "0x96CA") ; <CJK>
       (?$(GgX(B . "0x96F7") ; <CJK>
       (?$(GgY(B . "0x96FB") ; <CJK>
       (?$(GgZ(B . "0x96F9") ; <CJK>
       (?$(Gg[(B . "0x96F6") ; <CJK>
       (?$(Gg\(B . "0x9756") ; <CJK>
       (?$(Gg](B . "0x9774") ; <CJK>
       (?$(Gg^(B . "0x9776") ; <CJK>
       (?$(Gg_(B . "0x9810") ; <CJK>
       (?$(Gg`(B . "0x9811") ; <CJK>
       (?$(Gga(B . "0x9813") ; <CJK>
       (?$(Ggb(B . "0x980A") ; <CJK>
       (?$(Ggc(B . "0x9812") ; <CJK>
       (?$(Ggd(B . "0x980C") ; <CJK>
       (?$(Gge(B . "0x98FC") ; <CJK>
       (?$(Ggf(B . "0x98F4") ; <CJK>
       (?$(Ggg(B . "0x98FD") ; <CJK>
       (?$(Ggh(B . "0x98FE") ; <CJK>
       (?$(Ggi(B . "0x99B3") ; <CJK>
       (?$(Ggj(B . "0x99B1") ; <CJK>
       (?$(Ggk(B . "0x99B4") ; <CJK>
       (?$(Ggl(B . "0x9AE1") ; <CJK>
       (?$(Ggm(B . "0x9CE9") ; <CJK>
       (?$(Ggn(B . "0x9E82") ; <CJK>
       (?$(Ggo(B . "0x9F0E") ; <CJK>
       (?$(Ggp(B . "0x9F13") ; <CJK>
       (?$(Ggq(B . "0x9F20") ; <CJK>
       (?$(Ggr(B . "0x50E7") ; <CJK>
       (?$(Ggs(B . "0x50EE") ; <CJK>
       (?$(Ggt(B . "0x50E5") ; <CJK>
       (?$(Ggu(B . "0x50D6") ; <CJK>
       (?$(Ggv(B . "0x50ED") ; <CJK>
       (?$(Ggw(B . "0x50DA") ; <CJK>
       (?$(Ggx(B . "0x50D5") ; <CJK>
       (?$(Ggy(B . "0x50CF") ; <CJK>
       (?$(Ggz(B . "0x50D1") ; <CJK>
       (?$(Gg{(B . "0x50F1") ; <CJK>
       (?$(Gg|(B . "0x50CE") ; <CJK>
       (?$(Gg}(B . "0x50E9") ; <CJK>
       (?$(Gg~(B . "0x5162") ; <CJK>
       (?$(Gh!(B . "0x51F3") ; <CJK>
       (?$(Gh"(B . "0x5283") ; <CJK>
       (?$(Gh#(B . "0x5282") ; <CJK>
       (?$(Gh$(B . "0x5331") ; <CJK>
       (?$(Gh%(B . "0x53AD") ; <CJK>
       (?$(Gh&(B . "0x55FE") ; <CJK>
       (?$(Gh'(B . "0x5600") ; <CJK>
       (?$(Gh((B . "0x561B") ; <CJK>
       (?$(Gh)(B . "0x5617") ; <CJK>
       (?$(Gh*(B . "0x55FD") ; <CJK>
       (?$(Gh+(B . "0x5614") ; <CJK>
       (?$(Gh,(B . "0x5606") ; <CJK>
       (?$(Gh-(B . "0x5609") ; <CJK>
       (?$(Gh.(B . "0x560D") ; <CJK>
       (?$(Gh/(B . "0x560E") ; <CJK>
       (?$(Gh0(B . "0x55F7") ; <CJK>
       (?$(Gh1(B . "0x5616") ; <CJK>
       (?$(Gh2(B . "0x561F") ; <CJK>
       (?$(Gh3(B . "0x5608") ; <CJK>
       (?$(Gh4(B . "0x5610") ; <CJK>
       (?$(Gh5(B . "0x55F6") ; <CJK>
       (?$(Gh6(B . "0x5718") ; <CJK>
       (?$(Gh7(B . "0x5716") ; <CJK>
       (?$(Gh8(B . "0x5875") ; <CJK>
       (?$(Gh9(B . "0x587E") ; <CJK>
       (?$(Gh:(B . "0x5883") ; <CJK>
       (?$(Gh;(B . "0x5893") ; <CJK>
       (?$(Gh<(B . "0x588A") ; <CJK>
       (?$(Gh=(B . "0x5879") ; <CJK>
       (?$(Gh>(B . "0x5885") ; <CJK>
       (?$(Gh?(B . "0x587D") ; <CJK>
       (?$(Gh@(B . "0x58FD") ; <CJK>
       (?$(GhA(B . "0x5925") ; <CJK>
       (?$(GhB(B . "0x5922") ; <CJK>
       (?$(GhC(B . "0x5924") ; <CJK>
       (?$(GhD(B . "0x596A") ; <CJK>
       (?$(GhE(B . "0x5969") ; <CJK>
       (?$(GhF(B . "0x5AE1") ; <CJK>
       (?$(GhG(B . "0x5AE6") ; <CJK>
       (?$(GhH(B . "0x5AE9") ; <CJK>
       (?$(GhI(B . "0x5AD7") ; <CJK>
       (?$(GhJ(B . "0x5AD6") ; <CJK>
       (?$(GhK(B . "0x5AD8") ; <CJK>
       (?$(GhL(B . "0x5AE3") ; <CJK>
       (?$(GhM(B . "0x5B75") ; <CJK>
       (?$(GhN(B . "0x5BDE") ; <CJK>
       (?$(GhO(B . "0x5BE7") ; <CJK>
       (?$(GhP(B . "0x5BE1") ; <CJK>
       (?$(GhQ(B . "0x5BE5") ; <CJK>
       (?$(GhR(B . "0x5BE6") ; <CJK>
       (?$(GhS(B . "0x5BE8") ; <CJK>
       (?$(GhT(B . "0x5BE2") ; <CJK>
       (?$(GhU(B . "0x5BE4") ; <CJK>
       (?$(GhV(B . "0x5BDF") ; <CJK>
       (?$(GhW(B . "0x5C0D") ; <CJK>
       (?$(GhX(B . "0x5C62") ; <CJK>
       (?$(GhY(B . "0x5D84") ; <CJK>
       (?$(GhZ(B . "0x5D87") ; <CJK>
       (?$(Gh[(B . "0x5E5B") ; <CJK>
       (?$(Gh\(B . "0x5E63") ; <CJK>
       (?$(Gh](B . "0x5E55") ; <CJK>
       (?$(Gh^(B . "0x5E57") ; <CJK>
       (?$(Gh_(B . "0x5E54") ; <CJK>
       (?$(Gh`(B . "0x5ED3") ; <CJK>
       (?$(Gha(B . "0x5ED6") ; <CJK>
       (?$(Ghb(B . "0x5F0A") ; <CJK>
       (?$(Ghc(B . "0x5F46") ; <CJK>
       (?$(Ghd(B . "0x5F70") ; <CJK>
       (?$(Ghe(B . "0x5FB9") ; <CJK>
       (?$(Ghf(B . "0x6147") ; <CJK>
       (?$(Ghg(B . "0x613F") ; <CJK>
       (?$(Ghh(B . "0x614B") ; <CJK>
       (?$(Ghi(B . "0x6177") ; <CJK>
       (?$(Ghj(B . "0x6162") ; <CJK>
       (?$(Ghk(B . "0x6163") ; <CJK>
       (?$(Ghl(B . "0x615F") ; <CJK>
       (?$(Ghm(B . "0x615A") ; <CJK>
       (?$(Ghn(B . "0x6158") ; <CJK>
       (?$(Gho(B . "0x6175") ; <CJK>
       (?$(Ghp(B . "0x622A") ; <CJK>
       (?$(Ghq(B . "0x6487") ; <CJK>
       (?$(Ghr(B . "0x6458") ; <CJK>
       (?$(Ghs(B . "0x6454") ; <CJK>
       (?$(Ght(B . "0x64A4") ; <CJK>
       (?$(Ghu(B . "0x6478") ; <CJK>
       (?$(Ghv(B . "0x645F") ; <CJK>
       (?$(Ghw(B . "0x647A") ; <CJK>
       (?$(Ghx(B . "0x6451") ; <CJK>
       (?$(Ghy(B . "0x6467") ; <CJK>
       (?$(Ghz(B . "0x6434") ; <CJK>
       (?$(Gh{(B . "0x646D") ; <CJK>
       (?$(Gh|(B . "0x647B") ; <CJK>
       (?$(Gh}(B . "0x6572") ; <CJK>
       (?$(Gh~(B . "0x65A1") ; <CJK>
       (?$(Gi!(B . "0x65D7") ; <CJK>
       (?$(Gi"(B . "0x65D6") ; <CJK>
       (?$(Gi#(B . "0x66A2") ; <CJK>
       (?$(Gi$(B . "0x66A8") ; <CJK>
       (?$(Gi%(B . "0x669D") ; <CJK>
       (?$(Gi&(B . "0x699C") ; <CJK>
       (?$(Gi'(B . "0x69A8") ; <CJK>
       (?$(Gi((B . "0x6995") ; <CJK>
       (?$(Gi)(B . "0x69C1") ; <CJK>
       (?$(Gi*(B . "0x69AE") ; <CJK>
       (?$(Gi+(B . "0x69D3") ; <CJK>
       (?$(Gi,(B . "0x69CB") ; <CJK>
       (?$(Gi-(B . "0x699B") ; <CJK>
       (?$(Gi.(B . "0x69B7") ; <CJK>
       (?$(Gi/(B . "0x69BB") ; <CJK>
       (?$(Gi0(B . "0x69AB") ; <CJK>
       (?$(Gi1(B . "0x69B4") ; <CJK>
       (?$(Gi2(B . "0x69D0") ; <CJK>
       (?$(Gi3(B . "0x69CD") ; <CJK>
       (?$(Gi4(B . "0x69AD") ; <CJK>
       (?$(Gi5(B . "0x69CC") ; <CJK>
       (?$(Gi6(B . "0x69A6") ; <CJK>
       (?$(Gi7(B . "0x69C3") ; <CJK>
       (?$(Gi8(B . "0x69A3") ; <CJK>
       (?$(Gi9(B . "0x6B49") ; <CJK>
       (?$(Gi:(B . "0x6B4C") ; <CJK>
       (?$(Gi;(B . "0x6C33") ; <CJK>
       (?$(Gi<(B . "0x6F33") ; <CJK>
       (?$(Gi=(B . "0x6F14") ; <CJK>
       (?$(Gi>(B . "0x6EFE") ; <CJK>
       (?$(Gi?(B . "0x6F13") ; <CJK>
       (?$(Gi@(B . "0x6EF4") ; <CJK>
       (?$(GiA(B . "0x6F29") ; <CJK>
       (?$(GiB(B . "0x6F3E") ; <CJK>
       (?$(GiC(B . "0x6F20") ; <CJK>
       (?$(GiD(B . "0x6F2C") ; <CJK>
       (?$(GiE(B . "0x6F0F") ; <CJK>
       (?$(GiF(B . "0x6F02") ; <CJK>
       (?$(GiG(B . "0x6F22") ; <CJK>
       (?$(GiH(B . "0x6EFF") ; <CJK>
       (?$(GiI(B . "0x6EEF") ; <CJK>
       (?$(GiJ(B . "0x6F06") ; <CJK>
       (?$(GiK(B . "0x6F31") ; <CJK>
       (?$(GiL(B . "0x6F38") ; <CJK>
       (?$(GiM(B . "0x6F32") ; <CJK>
       (?$(GiN(B . "0x6F23") ; <CJK>
       (?$(GiO(B . "0x6F15") ; <CJK>
       (?$(GiP(B . "0x6F2B") ; <CJK>
       (?$(GiQ(B . "0x6F2F") ; <CJK>
       (?$(GiR(B . "0x6F88") ; <CJK>
       (?$(GiS(B . "0x6F2A") ; <CJK>
       (?$(GiT(B . "0x6EEC") ; <CJK>
       (?$(GiU(B . "0x6F01") ; <CJK>
       (?$(GiV(B . "0x6EF2") ; <CJK>
       (?$(GiW(B . "0x6ECC") ; <CJK>
       (?$(GiX(B . "0x6EF7") ; <CJK>
       (?$(GiY(B . "0x7194") ; <CJK>
       (?$(GiZ(B . "0x7199") ; <CJK>
       (?$(Gi[(B . "0x717D") ; <CJK>
       (?$(Gi\(B . "0x718A") ; <CJK>
       (?$(Gi](B . "0x7184") ; <CJK>
       (?$(Gi^(B . "0x7192") ; <CJK>
       (?$(Gi_(B . "0x723E") ; <CJK>
       (?$(Gi`(B . "0x7292") ; <CJK>
       (?$(Gia(B . "0x7296") ; <CJK>
       (?$(Gib(B . "0x7344") ; <CJK>
       (?$(Gic(B . "0x7350") ; <CJK>
       (?$(Gid(B . "0x7464") ; <CJK>
       (?$(Gie(B . "0x7463") ; <CJK>
       (?$(Gif(B . "0x746A") ; <CJK>
       (?$(Gig(B . "0x7470") ; <CJK>
       (?$(Gih(B . "0x746D") ; <CJK>
       (?$(Gii(B . "0x7504") ; <CJK>
       (?$(Gij(B . "0x7591") ; <CJK>
       (?$(Gik(B . "0x7627") ; <CJK>
       (?$(Gil(B . "0x760D") ; <CJK>
       (?$(Gim(B . "0x760B") ; <CJK>
       (?$(Gin(B . "0x7609") ; <CJK>
       (?$(Gio(B . "0x7613") ; <CJK>
       (?$(Gip(B . "0x76E1") ; <CJK>
       (?$(Giq(B . "0x76E3") ; <CJK>
       (?$(Gir(B . "0x7784") ; <CJK>
       (?$(Gis(B . "0x777D") ; <CJK>
       (?$(Git(B . "0x777F") ; <CJK>
       (?$(Giu(B . "0x7761") ; <CJK>
       (?$(Giv(B . "0x78C1") ; <CJK>
       (?$(Giw(B . "0x789F") ; <CJK>
       (?$(Gix(B . "0x78A7") ; <CJK>
       (?$(Giy(B . "0x78B3") ; <CJK>
       (?$(Giz(B . "0x78A9") ; <CJK>
       (?$(Gi{(B . "0x78A3") ; <CJK>
       (?$(Gi|(B . "0x798E") ; <CJK>
       (?$(Gi}(B . "0x798F") ; <CJK>
       (?$(Gi~(B . "0x798D") ; <CJK>
       (?$(Gj!(B . "0x7A2E") ; <CJK>
       (?$(Gj"(B . "0x7A31") ; <CJK>
       (?$(Gj#(B . "0x7AAA") ; <CJK>
       (?$(Gj$(B . "0x7AA9") ; <CJK>
       (?$(Gj%(B . "0x7AED") ; <CJK>
       (?$(Gj&(B . "0x7AEF") ; <CJK>
       (?$(Gj'(B . "0x7BA1") ; <CJK>
       (?$(Gj((B . "0x7B95") ; <CJK>
       (?$(Gj)(B . "0x7B8B") ; <CJK>
       (?$(Gj*(B . "0x7B75") ; <CJK>
       (?$(Gj+(B . "0x7B97") ; <CJK>
       (?$(Gj,(B . "0x7B9D") ; <CJK>
       (?$(Gj-(B . "0x7B94") ; <CJK>
       (?$(Gj.(B . "0x7B8F") ; <CJK>
       (?$(Gj/(B . "0x7BB8") ; <CJK>
       (?$(Gj0(B . "0x7B87") ; <CJK>
       (?$(Gj1(B . "0x7B84") ; <CJK>
       (?$(Gj2(B . "0x7CB9") ; <CJK>
       (?$(Gj3(B . "0x7CBD") ; <CJK>
       (?$(Gj4(B . "0x7CBE") ; <CJK>
       (?$(Gj5(B . "0x7DBB") ; <CJK>
       (?$(Gj6(B . "0x7DB0") ; <CJK>
       (?$(Gj7(B . "0x7D9C") ; <CJK>
       (?$(Gj8(B . "0x7DBD") ; <CJK>
       (?$(Gj9(B . "0x7DBE") ; <CJK>
       (?$(Gj:(B . "0x7DA0") ; <CJK>
       (?$(Gj;(B . "0x7DCA") ; <CJK>
       (?$(Gj<(B . "0x7DB4") ; <CJK>
       (?$(Gj=(B . "0x7DB2") ; <CJK>
       (?$(Gj>(B . "0x7DB1") ; <CJK>
       (?$(Gj?(B . "0x7DBA") ; <CJK>
       (?$(Gj@(B . "0x7DA2") ; <CJK>
       (?$(GjA(B . "0x7DBF") ; <CJK>
       (?$(GjB(B . "0x7DB5") ; <CJK>
       (?$(GjC(B . "0x7DB8") ; <CJK>
       (?$(GjD(B . "0x7DAD") ; <CJK>
       (?$(GjE(B . "0x7DD2") ; <CJK>
       (?$(GjF(B . "0x7DC7") ; <CJK>
       (?$(GjG(B . "0x7DAC") ; <CJK>
       (?$(GjH(B . "0x7F70") ; <CJK>
       (?$(GjI(B . "0x7FE0") ; <CJK>
       (?$(GjJ(B . "0x7FE1") ; <CJK>
       (?$(GjK(B . "0x7FDF") ; <CJK>
       (?$(GjL(B . "0x805E") ; <CJK>
       (?$(GjM(B . "0x805A") ; <CJK>
       (?$(GjN(B . "0x8087") ; <CJK>
       (?$(GjO(B . "0x8150") ; <CJK>
       (?$(GjP(B . "0x8180") ; <CJK>
       (?$(GjQ(B . "0x818F") ; <CJK>
       (?$(GjR(B . "0x8188") ; <CJK>
       (?$(GjS(B . "0x818A") ; <CJK>
       (?$(GjT(B . "0x817F") ; <CJK>
       (?$(GjU(B . "0x8182") ; <CJK>
       (?$(GjV(B . "0x81E7") ; <CJK>
       (?$(GjW(B . "0x81FA") ; <CJK>
       (?$(GjX(B . "0x8207") ; <CJK>
       (?$(GjY(B . "0x8214") ; <CJK>
       (?$(GjZ(B . "0x821E") ; <CJK>
       (?$(Gj[(B . "0x824B") ; <CJK>
       (?$(Gj\(B . "0x84C9") ; <CJK>
       (?$(Gj](B . "0x84BF") ; <CJK>
       (?$(Gj^(B . "0x84C6") ; <CJK>
       (?$(Gj_(B . "0x84C4") ; <CJK>
       (?$(Gj`(B . "0x8499") ; <CJK>
       (?$(Gja(B . "0x849E") ; <CJK>
       (?$(Gjb(B . "0x84B2") ; <CJK>
       (?$(Gjc(B . "0x849C") ; <CJK>
       (?$(Gjd(B . "0x84CB") ; <CJK>
       (?$(Gje(B . "0x84B8") ; <CJK>
       (?$(Gjf(B . "0x84C0") ; <CJK>
       (?$(Gjg(B . "0x84D3") ; <CJK>
       (?$(Gjh(B . "0x8490") ; <CJK>
       (?$(Gji(B . "0x84BC") ; <CJK>
       (?$(Gjj(B . "0x84D1") ; <CJK>
       (?$(Gjk(B . "0x84CA") ; <CJK>
       (?$(Gjl(B . "0x873F") ; <CJK>
       (?$(Gjm(B . "0x871C") ; <CJK>
       (?$(Gjn(B . "0x873B") ; <CJK>
       (?$(Gjo(B . "0x8722") ; <CJK>
       (?$(Gjp(B . "0x8725") ; <CJK>
       (?$(Gjq(B . "0x8734") ; <CJK>
       (?$(Gjr(B . "0x8718") ; <CJK>
       (?$(Gjs(B . "0x8755") ; <CJK>
       (?$(Gjt(B . "0x8737") ; <CJK>
       (?$(Gju(B . "0x8729") ; <CJK>
       (?$(Gjv(B . "0x88F3") ; <CJK>
       (?$(Gjw(B . "0x8902") ; <CJK>
       (?$(Gjx(B . "0x88F4") ; <CJK>
       (?$(Gjy(B . "0x88F9") ; <CJK>
       (?$(Gjz(B . "0x88F8") ; <CJK>
       (?$(Gj{(B . "0x88FD") ; <CJK>
       (?$(Gj|(B . "0x88E8") ; <CJK>
       (?$(Gj}(B . "0x891A") ; <CJK>
       (?$(Gj~(B . "0x88EF") ; <CJK>
       (?$(Gk!(B . "0x8AA6") ; <CJK>
       (?$(Gk"(B . "0x8A8C") ; <CJK>
       (?$(Gk#(B . "0x8A9E") ; <CJK>
       (?$(Gk$(B . "0x8AA3") ; <CJK>
       (?$(Gk%(B . "0x8A8D") ; <CJK>
       (?$(Gk&(B . "0x8AA1") ; <CJK>
       (?$(Gk'(B . "0x8A93") ; <CJK>
       (?$(Gk((B . "0x8AA4") ; <CJK>
       (?$(Gk)(B . "0x8AAA") ; <CJK>
       (?$(Gk*(B . "0x8AA5") ; <CJK>
       (?$(Gk+(B . "0x8AA8") ; <CJK>
       (?$(Gk,(B . "0x8A98") ; <CJK>
       (?$(Gk-(B . "0x8A91") ; <CJK>
       (?$(Gk.(B . "0x8A9A") ; <CJK>
       (?$(Gk/(B . "0x8AA7") ; <CJK>
       (?$(Gk0(B . "0x8C6A") ; <CJK>
       (?$(Gk1(B . "0x8C8D") ; <CJK>
       (?$(Gk2(B . "0x8C8C") ; <CJK>
       (?$(Gk3(B . "0x8CD3") ; <CJK>
       (?$(Gk4(B . "0x8CD1") ; <CJK>
       (?$(Gk5(B . "0x8CD2") ; <CJK>
       (?$(Gk6(B . "0x8D6B") ; <CJK>
       (?$(Gk7(B . "0x8D99") ; <CJK>
       (?$(Gk8(B . "0x8D95") ; <CJK>
       (?$(Gk9(B . "0x8DFC") ; <CJK>
       (?$(Gk:(B . "0x8F14") ; <CJK>
       (?$(Gk;(B . "0x8F12") ; <CJK>
       (?$(Gk<(B . "0x8F15") ; <CJK>
       (?$(Gk=(B . "0x8F13") ; <CJK>
       (?$(Gk>(B . "0x8FA3") ; <CJK>
       (?$(Gk?(B . "0x9060") ; <CJK>
       (?$(Gk@(B . "0x9058") ; <CJK>
       (?$(GkA(B . "0x905C") ; <CJK>
       (?$(GkB(B . "0x9063") ; <CJK>
       (?$(GkC(B . "0x9059") ; <CJK>
       (?$(GkD(B . "0x905E") ; <CJK>
       (?$(GkE(B . "0x9062") ; <CJK>
       (?$(GkF(B . "0x905D") ; <CJK>
       (?$(GkG(B . "0x905B") ; <CJK>
       (?$(GkH(B . "0x9119") ; <CJK>
       (?$(GkI(B . "0x9118") ; <CJK>
       (?$(GkJ(B . "0x911E") ; <CJK>
       (?$(GkK(B . "0x9175") ; <CJK>
       (?$(GkL(B . "0x9178") ; <CJK>
       (?$(GkM(B . "0x9177") ; <CJK>
       (?$(GkN(B . "0x9174") ; <CJK>
       (?$(GkO(B . "0x9278") ; <CJK>
       (?$(GkP(B . "0x92AC") ; <CJK>
       (?$(GkQ(B . "0x9280") ; <CJK>
       (?$(GkR(B . "0x9285") ; <CJK>
       (?$(GkS(B . "0x9298") ; <CJK>
       (?$(GkT(B . "0x9296") ; <CJK>
       (?$(GkU(B . "0x927B") ; <CJK>
       (?$(GkV(B . "0x9293") ; <CJK>
       (?$(GkW(B . "0x929C") ; <CJK>
       (?$(GkX(B . "0x92A8") ; <CJK>
       (?$(GkY(B . "0x927C") ; <CJK>
       (?$(GkZ(B . "0x9291") ; <CJK>
       (?$(Gk[(B . "0x95A1") ; <CJK>
       (?$(Gk\(B . "0x95A8") ; <CJK>
       (?$(Gk](B . "0x95A9") ; <CJK>
       (?$(Gk^(B . "0x95A3") ; <CJK>
       (?$(Gk_(B . "0x95A5") ; <CJK>
       (?$(Gk`(B . "0x95A4") ; <CJK>
       (?$(Gka(B . "0x9699") ; <CJK>
       (?$(Gkb(B . "0x969C") ; <CJK>
       (?$(Gkc(B . "0x969B") ; <CJK>
       (?$(Gkd(B . "0x96CC") ; <CJK>
       (?$(Gke(B . "0x96D2") ; <CJK>
       (?$(Gkf(B . "0x9700") ; <CJK>
       (?$(Gkg(B . "0x977C") ; <CJK>
       (?$(Gkh(B . "0x9785") ; <CJK>
       (?$(Gki(B . "0x97F6") ; <CJK>
       (?$(Gkj(B . "0x9817") ; <CJK>
       (?$(Gkk(B . "0x9818") ; <CJK>
       (?$(Gkl(B . "0x98AF") ; <CJK>
       (?$(Gkm(B . "0x98B1") ; <CJK>
       (?$(Gkn(B . "0x9903") ; <CJK>
       (?$(Gko(B . "0x9905") ; <CJK>
       (?$(Gkp(B . "0x990C") ; <CJK>
       (?$(Gkq(B . "0x9909") ; <CJK>
       (?$(Gkr(B . "0x99C1") ; <CJK>
       (?$(Gks(B . "0x9AAF") ; <CJK>
       (?$(Gkt(B . "0x9AB0") ; <CJK>
       (?$(Gku(B . "0x9AE6") ; <CJK>
       (?$(Gkv(B . "0x9B41") ; <CJK>
       (?$(Gkw(B . "0x9B42") ; <CJK>
       (?$(Gkx(B . "0x9CF4") ; <CJK>
       (?$(Gky(B . "0x9CF6") ; <CJK>
       (?$(Gkz(B . "0x9CF3") ; <CJK>
       (?$(Gk{(B . "0x9EBC") ; <CJK>
       (?$(Gk|(B . "0x9F3B") ; <CJK>
       (?$(Gk}(B . "0x9F4A") ; <CJK>
       (?$(Gk~(B . "0x5104") ; <CJK>
       (?$(Gl!(B . "0x5100") ; <CJK>
       (?$(Gl"(B . "0x50FB") ; <CJK>
       (?$(Gl#(B . "0x50F5") ; <CJK>
       (?$(Gl$(B . "0x50F9") ; <CJK>
       (?$(Gl%(B . "0x5102") ; <CJK>
       (?$(Gl&(B . "0x5108") ; <CJK>
       (?$(Gl'(B . "0x5109") ; <CJK>
       (?$(Gl((B . "0x5105") ; <CJK>
       (?$(Gl)(B . "0x51DC") ; <CJK>
       (?$(Gl*(B . "0x5287") ; <CJK>
       (?$(Gl+(B . "0x5288") ; <CJK>
       (?$(Gl,(B . "0x5289") ; <CJK>
       (?$(Gl-(B . "0x528D") ; <CJK>
       (?$(Gl.(B . "0x528A") ; <CJK>
       (?$(Gl/(B . "0x52F0") ; <CJK>
       (?$(Gl0(B . "0x53B2") ; <CJK>
       (?$(Gl1(B . "0x562E") ; <CJK>
       (?$(Gl2(B . "0x563B") ; <CJK>
       (?$(Gl3(B . "0x5639") ; <CJK>
       (?$(Gl4(B . "0x5632") ; <CJK>
       (?$(Gl5(B . "0x563F") ; <CJK>
       (?$(Gl6(B . "0x5634") ; <CJK>
       (?$(Gl7(B . "0x5629") ; <CJK>
       (?$(Gl8(B . "0x5653") ; <CJK>
       (?$(Gl9(B . "0x564E") ; <CJK>
       (?$(Gl:(B . "0x5657") ; <CJK>
       (?$(Gl;(B . "0x5674") ; <CJK>
       (?$(Gl<(B . "0x5636") ; <CJK>
       (?$(Gl=(B . "0x562F") ; <CJK>
       (?$(Gl>(B . "0x5630") ; <CJK>
       (?$(Gl?(B . "0x5880") ; <CJK>
       (?$(Gl@(B . "0x589F") ; <CJK>
       (?$(GlA(B . "0x589E") ; <CJK>
       (?$(GlB(B . "0x58B3") ; <CJK>
       (?$(GlC(B . "0x589C") ; <CJK>
       (?$(GlD(B . "0x58AE") ; <CJK>
       (?$(GlE(B . "0x58A9") ; <CJK>
       (?$(GlF(B . "0x58A6") ; <CJK>
       (?$(GlG(B . "0x596D") ; <CJK>
       (?$(GlH(B . "0x5B09") ; <CJK>
       (?$(GlI(B . "0x5AFB") ; <CJK>
       (?$(GlJ(B . "0x5B0B") ; <CJK>
       (?$(GlK(B . "0x5AF5") ; <CJK>
       (?$(GlL(B . "0x5B0C") ; <CJK>
       (?$(GlM(B . "0x5B08") ; <CJK>
       (?$(GlN(B . "0x5BEE") ; <CJK>
       (?$(GlO(B . "0x5BEC") ; <CJK>
       (?$(GlP(B . "0x5BE9") ; <CJK>
       (?$(GlQ(B . "0x5BEB") ; <CJK>
       (?$(GlR(B . "0x5C64") ; <CJK>
       (?$(GlS(B . "0x5C65") ; <CJK>
       (?$(GlT(B . "0x5D9D") ; <CJK>
       (?$(GlU(B . "0x5D94") ; <CJK>
       (?$(GlV(B . "0x5E62") ; <CJK>
       (?$(GlW(B . "0x5E5F") ; <CJK>
       (?$(GlX(B . "0x5E61") ; <CJK>
       (?$(GlY(B . "0x5EE2") ; <CJK>
       (?$(GlZ(B . "0x5EDA") ; <CJK>
       (?$(Gl[(B . "0x5EDF") ; <CJK>
       (?$(Gl\(B . "0x5EDD") ; <CJK>
       (?$(Gl](B . "0x5EE3") ; <CJK>
       (?$(Gl^(B . "0x5EE0") ; <CJK>
       (?$(Gl_(B . "0x5F48") ; <CJK>
       (?$(Gl`(B . "0x5F71") ; <CJK>
       (?$(Gla(B . "0x5FB7") ; <CJK>
       (?$(Glb(B . "0x5FB5") ; <CJK>
       (?$(Glc(B . "0x6176") ; <CJK>
       (?$(Gld(B . "0x6167") ; <CJK>
       (?$(Gle(B . "0x616E") ; <CJK>
       (?$(Glf(B . "0x615D") ; <CJK>
       (?$(Glg(B . "0x6155") ; <CJK>
       (?$(Glh(B . "0x6182") ; <CJK>
       (?$(Gli(B . "0x617C") ; <CJK>
       (?$(Glj(B . "0x6170") ; <CJK>
       (?$(Glk(B . "0x616B") ; <CJK>
       (?$(Gll(B . "0x617E") ; <CJK>
       (?$(Glm(B . "0x61A7") ; <CJK>
       (?$(Gln(B . "0x6190") ; <CJK>
       (?$(Glo(B . "0x61AB") ; <CJK>
       (?$(Glp(B . "0x618E") ; <CJK>
       (?$(Glq(B . "0x61AC") ; <CJK>
       (?$(Glr(B . "0x619A") ; <CJK>
       (?$(Gls(B . "0x61A4") ; <CJK>
       (?$(Glt(B . "0x6194") ; <CJK>
       (?$(Glu(B . "0x61AE") ; <CJK>
       (?$(Glv(B . "0x622E") ; <CJK>
       (?$(Glw(B . "0x6469") ; <CJK>
       (?$(Glx(B . "0x646F") ; <CJK>
       (?$(Gly(B . "0x6479") ; <CJK>
       (?$(Glz(B . "0x649E") ; <CJK>
       (?$(Gl{(B . "0x64B2") ; <CJK>
       (?$(Gl|(B . "0x6488") ; <CJK>
       (?$(Gl}(B . "0x6490") ; <CJK>
       (?$(Gl~(B . "0x64B0") ; <CJK>
       (?$(Gm!(B . "0x64A5") ; <CJK>
       (?$(Gm"(B . "0x6493") ; <CJK>
       (?$(Gm#(B . "0x6495") ; <CJK>
       (?$(Gm$(B . "0x64A9") ; <CJK>
       (?$(Gm%(B . "0x6492") ; <CJK>
       (?$(Gm&(B . "0x64AE") ; <CJK>
       (?$(Gm'(B . "0x64AD") ; <CJK>
       (?$(Gm((B . "0x64AB") ; <CJK>
       (?$(Gm)(B . "0x649A") ; <CJK>
       (?$(Gm*(B . "0x64AC") ; <CJK>
       (?$(Gm+(B . "0x6499") ; <CJK>
       (?$(Gm,(B . "0x64A2") ; <CJK>
       (?$(Gm-(B . "0x64B3") ; <CJK>
       (?$(Gm.(B . "0x6575") ; <CJK>
       (?$(Gm/(B . "0x6577") ; <CJK>
       (?$(Gm0(B . "0x6578") ; <CJK>
       (?$(Gm1(B . "0x66AE") ; <CJK>
       (?$(Gm2(B . "0x66AB") ; <CJK>
       (?$(Gm3(B . "0x66B4") ; <CJK>
       (?$(Gm4(B . "0x66B1") ; <CJK>
       (?$(Gm5(B . "0x6A23") ; <CJK>
       (?$(Gm6(B . "0x6A1F") ; <CJK>
       (?$(Gm7(B . "0x69E8") ; <CJK>
       (?$(Gm8(B . "0x6A01") ; <CJK>
       (?$(Gm9(B . "0x6A1E") ; <CJK>
       (?$(Gm:(B . "0x6A19") ; <CJK>
       (?$(Gm;(B . "0x69FD") ; <CJK>
       (?$(Gm<(B . "0x6A21") ; <CJK>
       (?$(Gm=(B . "0x6A13") ; <CJK>
       (?$(Gm>(B . "0x6A0A") ; <CJK>
       (?$(Gm?(B . "0x69F3") ; <CJK>
       (?$(Gm@(B . "0x6A02") ; <CJK>
       (?$(GmA(B . "0x6A05") ; <CJK>
       (?$(GmB(B . "0x69ED") ; <CJK>
       (?$(GmC(B . "0x6A11") ; <CJK>
       (?$(GmD(B . "0x6B50") ; <CJK>
       (?$(GmE(B . "0x6B4E") ; <CJK>
       (?$(GmF(B . "0x6BA4") ; <CJK>
       (?$(GmG(B . "0x6BC5") ; <CJK>
       (?$(GmH(B . "0x6BC6") ; <CJK>
       (?$(GmI(B . "0x6F3F") ; <CJK>
       (?$(GmJ(B . "0x6F7C") ; <CJK>
       (?$(GmK(B . "0x6F84") ; <CJK>
       (?$(GmL(B . "0x6F51") ; <CJK>
       (?$(GmM(B . "0x6F66") ; <CJK>
       (?$(GmN(B . "0x6F54") ; <CJK>
       (?$(GmO(B . "0x6F86") ; <CJK>
       (?$(GmP(B . "0x6F6D") ; <CJK>
       (?$(GmQ(B . "0x6F5B") ; <CJK>
       (?$(GmR(B . "0x6F78") ; <CJK>
       (?$(GmS(B . "0x6F6E") ; <CJK>
       (?$(GmT(B . "0x6F8E") ; <CJK>
       (?$(GmU(B . "0x6F7A") ; <CJK>
       (?$(GmV(B . "0x6F70") ; <CJK>
       (?$(GmW(B . "0x6F64") ; <CJK>
       (?$(GmX(B . "0x6F97") ; <CJK>
       (?$(GmY(B . "0x6F58") ; <CJK>
       (?$(GmZ(B . "0x6ED5") ; <CJK>
       (?$(Gm[(B . "0x6F6F") ; <CJK>
       (?$(Gm\(B . "0x6F60") ; <CJK>
       (?$(Gm](B . "0x6F5F") ; <CJK>
       (?$(Gm^(B . "0x719F") ; <CJK>
       (?$(Gm_(B . "0x71AC") ; <CJK>
       (?$(Gm`(B . "0x71B1") ; <CJK>
       (?$(Gma(B . "0x71A8") ; <CJK>
       (?$(Gmb(B . "0x7256") ; <CJK>
       (?$(Gmc(B . "0x729B") ; <CJK>
       (?$(Gmd(B . "0x734E") ; <CJK>
       (?$(Gme(B . "0x7357") ; <CJK>
       (?$(Gmf(B . "0x7469") ; <CJK>
       (?$(Gmg(B . "0x748B") ; <CJK>
       (?$(Gmh(B . "0x7483") ; <CJK>
       (?$(Gmi(B . "0x747E") ; <CJK>
       (?$(Gmj(B . "0x7480") ; <CJK>
       (?$(Gmk(B . "0x757F") ; <CJK>
       (?$(Gml(B . "0x7620") ; <CJK>
       (?$(Gmm(B . "0x7629") ; <CJK>
       (?$(Gmn(B . "0x761F") ; <CJK>
       (?$(Gmo(B . "0x7624") ; <CJK>
       (?$(Gmp(B . "0x7626") ; <CJK>
       (?$(Gmq(B . "0x7621") ; <CJK>
       (?$(Gmr(B . "0x7622") ; <CJK>
       (?$(Gms(B . "0x769A") ; <CJK>
       (?$(Gmt(B . "0x76BA") ; <CJK>
       (?$(Gmu(B . "0x76E4") ; <CJK>
       (?$(Gmv(B . "0x778E") ; <CJK>
       (?$(Gmw(B . "0x7787") ; <CJK>
       (?$(Gmx(B . "0x778C") ; <CJK>
       (?$(Gmy(B . "0x7791") ; <CJK>
       (?$(Gmz(B . "0x778B") ; <CJK>
       (?$(Gm{(B . "0x78CB") ; <CJK>
       (?$(Gm|(B . "0x78C5") ; <CJK>
       (?$(Gm}(B . "0x78BA") ; <CJK>
       (?$(Gm~(B . "0x78CA") ; <CJK>
       (?$(Gn!(B . "0x78BE") ; <CJK>
       (?$(Gn"(B . "0x78D5") ; <CJK>
       (?$(Gn#(B . "0x78BC") ; <CJK>
       (?$(Gn$(B . "0x78D0") ; <CJK>
       (?$(Gn%(B . "0x7A3F") ; <CJK>
       (?$(Gn&(B . "0x7A3C") ; <CJK>
       (?$(Gn'(B . "0x7A40") ; <CJK>
       (?$(Gn((B . "0x7A3D") ; <CJK>
       (?$(Gn)(B . "0x7A37") ; <CJK>
       (?$(Gn*(B . "0x7A3B") ; <CJK>
       (?$(Gn+(B . "0x7AAF") ; <CJK>
       (?$(Gn,(B . "0x7AAE") ; <CJK>
       (?$(Gn-(B . "0x7BAD") ; <CJK>
       (?$(Gn.(B . "0x7BB1") ; <CJK>
       (?$(Gn/(B . "0x7BC4") ; <CJK>
       (?$(Gn0(B . "0x7BB4") ; <CJK>
       (?$(Gn1(B . "0x7BC6") ; <CJK>
       (?$(Gn2(B . "0x7BC7") ; <CJK>
       (?$(Gn3(B . "0x7BC1") ; <CJK>
       (?$(Gn4(B . "0x7BA0") ; <CJK>
       (?$(Gn5(B . "0x7BCC") ; <CJK>
       (?$(Gn6(B . "0x7CCA") ; <CJK>
       (?$(Gn7(B . "0x7DE0") ; <CJK>
       (?$(Gn8(B . "0x7DF4") ; <CJK>
       (?$(Gn9(B . "0x7DEF") ; <CJK>
       (?$(Gn:(B . "0x7DFB") ; <CJK>
       (?$(Gn;(B . "0x7DD8") ; <CJK>
       (?$(Gn<(B . "0x7DEC") ; <CJK>
       (?$(Gn=(B . "0x7DDD") ; <CJK>
       (?$(Gn>(B . "0x7DE8") ; <CJK>
       (?$(Gn?(B . "0x7DE3") ; <CJK>
       (?$(Gn@(B . "0x7DDA") ; <CJK>
       (?$(GnA(B . "0x7DDE") ; <CJK>
       (?$(GnB(B . "0x7DE9") ; <CJK>
       (?$(GnC(B . "0x7D9E") ; <CJK>
       (?$(GnD(B . "0x7DD9") ; <CJK>
       (?$(GnE(B . "0x7DF2") ; <CJK>
       (?$(GnF(B . "0x7DF9") ; <CJK>
       (?$(GnG(B . "0x7F75") ; <CJK>
       (?$(GnH(B . "0x7F77") ; <CJK>
       (?$(GnI(B . "0x7FAF") ; <CJK>
       (?$(GnJ(B . "0x7FE9") ; <CJK>
       (?$(GnK(B . "0x8026") ; <CJK>
       (?$(GnL(B . "0x819B") ; <CJK>
       (?$(GnM(B . "0x819C") ; <CJK>
       (?$(GnN(B . "0x819D") ; <CJK>
       (?$(GnO(B . "0x81A0") ; <CJK>
       (?$(GnP(B . "0x819A") ; <CJK>
       (?$(GnQ(B . "0x8198") ; <CJK>
       (?$(GnR(B . "0x8517") ; <CJK>
       (?$(GnS(B . "0x853D") ; <CJK>
       (?$(GnT(B . "0x851A") ; <CJK>
       (?$(GnU(B . "0x84EE") ; <CJK>
       (?$(GnV(B . "0x852C") ; <CJK>
       (?$(GnW(B . "0x852D") ; <CJK>
       (?$(GnX(B . "0x8513") ; <CJK>
       (?$(GnY(B . "0x8511") ; <CJK>
       (?$(GnZ(B . "0x8523") ; <CJK>
       (?$(Gn[(B . "0x8521") ; <CJK>
       (?$(Gn\(B . "0x8514") ; <CJK>
       (?$(Gn](B . "0x84EC") ; <CJK>
       (?$(Gn^(B . "0x8525") ; <CJK>
       (?$(Gn_(B . "0x84FF") ; <CJK>
       (?$(Gn`(B . "0x8506") ; <CJK>
       (?$(Gna(B . "0x8782") ; <CJK>
       (?$(Gnb(B . "0x8774") ; <CJK>
       (?$(Gnc(B . "0x8776") ; <CJK>
       (?$(Gnd(B . "0x8760") ; <CJK>
       (?$(Gne(B . "0x8766") ; <CJK>
       (?$(Gnf(B . "0x8778") ; <CJK>
       (?$(Gng(B . "0x8768") ; <CJK>
       (?$(Gnh(B . "0x8759") ; <CJK>
       (?$(Gni(B . "0x8757") ; <CJK>
       (?$(Gnj(B . "0x874C") ; <CJK>
       (?$(Gnk(B . "0x8753") ; <CJK>
       (?$(Gnl(B . "0x885B") ; <CJK>
       (?$(Gnm(B . "0x885D") ; <CJK>
       (?$(Gnn(B . "0x8910") ; <CJK>
       (?$(Gno(B . "0x8907") ; <CJK>
       (?$(Gnp(B . "0x8912") ; <CJK>
       (?$(Gnq(B . "0x8913") ; <CJK>
       (?$(Gnr(B . "0x8915") ; <CJK>
       (?$(Gns(B . "0x890A") ; <CJK>
       (?$(Gnt(B . "0x8ABC") ; <CJK>
       (?$(Gnu(B . "0x8AD2") ; <CJK>
       (?$(Gnv(B . "0x8AC7") ; <CJK>
       (?$(Gnw(B . "0x8AC4") ; <CJK>
       (?$(Gnx(B . "0x8A95") ; <CJK>
       (?$(Gny(B . "0x8ACB") ; <CJK>
       (?$(Gnz(B . "0x8AF8") ; <CJK>
       (?$(Gn{(B . "0x8AB2") ; <CJK>
       (?$(Gn|(B . "0x8AC9") ; <CJK>
       (?$(Gn}(B . "0x8AC2") ; <CJK>
       (?$(Gn~(B . "0x8ABF") ; <CJK>
       (?$(Go!(B . "0x8AB0") ; <CJK>
       (?$(Go"(B . "0x8AD6") ; <CJK>
       (?$(Go#(B . "0x8ACD") ; <CJK>
       (?$(Go$(B . "0x8AB6") ; <CJK>
       (?$(Go%(B . "0x8AB9") ; <CJK>
       (?$(Go&(B . "0x8ADB") ; <CJK>
       (?$(Go'(B . "0x8C4C") ; <CJK>
       (?$(Go((B . "0x8C4E") ; <CJK>
       (?$(Go)(B . "0x8C6C") ; <CJK>
       (?$(Go*(B . "0x8CE0") ; <CJK>
       (?$(Go+(B . "0x8CDE") ; <CJK>
       (?$(Go,(B . "0x8CE6") ; <CJK>
       (?$(Go-(B . "0x8CE4") ; <CJK>
       (?$(Go.(B . "0x8CEC") ; <CJK>
       (?$(Go/(B . "0x8CED") ; <CJK>
       (?$(Go0(B . "0x8CE2") ; <CJK>
       (?$(Go1(B . "0x8CE3") ; <CJK>
       (?$(Go2(B . "0x8CDC") ; <CJK>
       (?$(Go3(B . "0x8CEA") ; <CJK>
       (?$(Go4(B . "0x8CE1") ; <CJK>
       (?$(Go5(B . "0x8D6D") ; <CJK>
       (?$(Go6(B . "0x8D9F") ; <CJK>
       (?$(Go7(B . "0x8DA3") ; <CJK>
       (?$(Go8(B . "0x8E2B") ; <CJK>
       (?$(Go9(B . "0x8E10") ; <CJK>
       (?$(Go:(B . "0x8E1D") ; <CJK>
       (?$(Go;(B . "0x8E22") ; <CJK>
       (?$(Go<(B . "0x8E0F") ; <CJK>
       (?$(Go=(B . "0x8E29") ; <CJK>
       (?$(Go>(B . "0x8E1F") ; <CJK>
       (?$(Go?(B . "0x8E21") ; <CJK>
       (?$(Go@(B . "0x8E1E") ; <CJK>
       (?$(GoA(B . "0x8EBA") ; <CJK>
       (?$(GoB(B . "0x8F1D") ; <CJK>
       (?$(GoC(B . "0x8F1B") ; <CJK>
       (?$(GoD(B . "0x8F1F") ; <CJK>
       (?$(GoE(B . "0x8F29") ; <CJK>
       (?$(GoF(B . "0x8F26") ; <CJK>
       (?$(GoG(B . "0x8F2A") ; <CJK>
       (?$(GoH(B . "0x8F1C") ; <CJK>
       (?$(GoI(B . "0x8F1E") ; <CJK>
       (?$(GoJ(B . "0x8F25") ; <CJK>
       (?$(GoK(B . "0x9069") ; <CJK>
       (?$(GoL(B . "0x906E") ; <CJK>
       (?$(GoM(B . "0x9068") ; <CJK>
       (?$(GoN(B . "0x906D") ; <CJK>
       (?$(GoO(B . "0x9077") ; <CJK>
       (?$(GoP(B . "0x9130") ; <CJK>
       (?$(GoQ(B . "0x912D") ; <CJK>
       (?$(GoR(B . "0x9127") ; <CJK>
       (?$(GoS(B . "0x9131") ; <CJK>
       (?$(GoT(B . "0x9187") ; <CJK>
       (?$(GoU(B . "0x9189") ; <CJK>
       (?$(GoV(B . "0x918B") ; <CJK>
       (?$(GoW(B . "0x9183") ; <CJK>
       (?$(GoX(B . "0x92C5") ; <CJK>
       (?$(GoY(B . "0x92BB") ; <CJK>
       (?$(GoZ(B . "0x92B7") ; <CJK>
       (?$(Go[(B . "0x92EA") ; <CJK>
       (?$(Go\(B . "0x92E4") ; <CJK>
       (?$(Go](B . "0x92C1") ; <CJK>
       (?$(Go^(B . "0x92B3") ; <CJK>
       (?$(Go_(B . "0x92BC") ; <CJK>
       (?$(Go`(B . "0x92D2") ; <CJK>
       (?$(Goa(B . "0x92C7") ; <CJK>
       (?$(Gob(B . "0x92F0") ; <CJK>
       (?$(Goc(B . "0x92B2") ; <CJK>
       (?$(God(B . "0x95AD") ; <CJK>
       (?$(Goe(B . "0x95B1") ; <CJK>
       (?$(Gof(B . "0x9704") ; <CJK>
       (?$(Gog(B . "0x9706") ; <CJK>
       (?$(Goh(B . "0x9707") ; <CJK>
       (?$(Goi(B . "0x9709") ; <CJK>
       (?$(Goj(B . "0x9760") ; <CJK>
       (?$(Gok(B . "0x978D") ; <CJK>
       (?$(Gol(B . "0x978B") ; <CJK>
       (?$(Gom(B . "0x978F") ; <CJK>
       (?$(Gon(B . "0x9821") ; <CJK>
       (?$(Goo(B . "0x982B") ; <CJK>
       (?$(Gop(B . "0x981C") ; <CJK>
       (?$(Goq(B . "0x98B3") ; <CJK>
       (?$(Gor(B . "0x990A") ; <CJK>
       (?$(Gos(B . "0x9913") ; <CJK>
       (?$(Got(B . "0x9912") ; <CJK>
       (?$(Gou(B . "0x9918") ; <CJK>
       (?$(Gov(B . "0x99DD") ; <CJK>
       (?$(Gow(B . "0x99D0") ; <CJK>
       (?$(Gox(B . "0x99DF") ; <CJK>
       (?$(Goy(B . "0x99DB") ; <CJK>
       (?$(Goz(B . "0x99D1") ; <CJK>
       (?$(Go{(B . "0x99D5") ; <CJK>
       (?$(Go|(B . "0x99D2") ; <CJK>
       (?$(Go}(B . "0x99D9") ; <CJK>
       (?$(Go~(B . "0x9AB7") ; <CJK>
       (?$(Gp!(B . "0x9AEE") ; <CJK>
       (?$(Gp"(B . "0x9AEF") ; <CJK>
       (?$(Gp#(B . "0x9B27") ; <CJK>
       (?$(Gp$(B . "0x9B45") ; <CJK>
       (?$(Gp%(B . "0x9B44") ; <CJK>
       (?$(Gp&(B . "0x9B77") ; <CJK>
       (?$(Gp'(B . "0x9B6F") ; <CJK>
       (?$(Gp((B . "0x9D06") ; <CJK>
       (?$(Gp)(B . "0x9D09") ; <CJK>
       (?$(Gp*(B . "0x9D03") ; <CJK>
       (?$(Gp+(B . "0x9EA9") ; <CJK>
       (?$(Gp,(B . "0x9EBE") ; <CJK>
       (?$(Gp-(B . "0x9ECE") ; <CJK>
       (?$(Gp.(B . "0x58A8") ; <CJK>
       (?$(Gp/(B . "0x9F52") ; <CJK>
       (?$(Gp0(B . "0x5112") ; <CJK>
       (?$(Gp1(B . "0x5118") ; <CJK>
       (?$(Gp2(B . "0x5114") ; <CJK>
       (?$(Gp3(B . "0x5110") ; <CJK>
       (?$(Gp4(B . "0x5115") ; <CJK>
       (?$(Gp5(B . "0x5180") ; <CJK>
       (?$(Gp6(B . "0x51AA") ; <CJK>
       (?$(Gp7(B . "0x51DD") ; <CJK>
       (?$(Gp8(B . "0x5291") ; <CJK>
       (?$(Gp9(B . "0x5293") ; <CJK>
       (?$(Gp:(B . "0x52F3") ; <CJK>
       (?$(Gp;(B . "0x5659") ; <CJK>
       (?$(Gp<(B . "0x566B") ; <CJK>
       (?$(Gp=(B . "0x5679") ; <CJK>
       (?$(Gp>(B . "0x5669") ; <CJK>
       (?$(Gp?(B . "0x5664") ; <CJK>
       (?$(Gp@(B . "0x5678") ; <CJK>
       (?$(GpA(B . "0x566A") ; <CJK>
       (?$(GpB(B . "0x5668") ; <CJK>
       (?$(GpC(B . "0x5665") ; <CJK>
       (?$(GpD(B . "0x5671") ; <CJK>
       (?$(GpE(B . "0x566F") ; <CJK>
       (?$(GpF(B . "0x566C") ; <CJK>
       (?$(GpG(B . "0x5662") ; <CJK>
       (?$(GpH(B . "0x5676") ; <CJK>
       (?$(GpI(B . "0x58C1") ; <CJK>
       (?$(GpJ(B . "0x58BE") ; <CJK>
       (?$(GpK(B . "0x58C7") ; <CJK>
       (?$(GpL(B . "0x58C5") ; <CJK>
       (?$(GpM(B . "0x596E") ; <CJK>
       (?$(GpN(B . "0x5B1D") ; <CJK>
       (?$(GpO(B . "0x5B34") ; <CJK>
       (?$(GpP(B . "0x5B78") ; <CJK>
       (?$(GpQ(B . "0x5BF0") ; <CJK>
       (?$(GpR(B . "0x5C0E") ; <CJK>
       (?$(GpS(B . "0x5F4A") ; <CJK>
       (?$(GpT(B . "0x61B2") ; <CJK>
       (?$(GpU(B . "0x6191") ; <CJK>
       (?$(GpV(B . "0x61A9") ; <CJK>
       (?$(GpW(B . "0x618A") ; <CJK>
       (?$(GpX(B . "0x61CD") ; <CJK>
       (?$(GpY(B . "0x61B6") ; <CJK>
       (?$(GpZ(B . "0x61BE") ; <CJK>
       (?$(Gp[(B . "0x61CA") ; <CJK>
       (?$(Gp\(B . "0x61C8") ; <CJK>
       (?$(Gp](B . "0x6230") ; <CJK>
       (?$(Gp^(B . "0x64C5") ; <CJK>
       (?$(Gp_(B . "0x64C1") ; <CJK>
       (?$(Gp`(B . "0x64CB") ; <CJK>
       (?$(Gpa(B . "0x64BB") ; <CJK>
       (?$(Gpb(B . "0x64BC") ; <CJK>
       (?$(Gpc(B . "0x64DA") ; <CJK>
       (?$(Gpd(B . "0x64C4") ; <CJK>
       (?$(Gpe(B . "0x64C7") ; <CJK>
       (?$(Gpf(B . "0x64C2") ; <CJK>
       (?$(Gpg(B . "0x64CD") ; <CJK>
       (?$(Gph(B . "0x64BF") ; <CJK>
       (?$(Gpi(B . "0x64D2") ; <CJK>
       (?$(Gpj(B . "0x64D4") ; <CJK>
       (?$(Gpk(B . "0x64BE") ; <CJK>
       (?$(Gpl(B . "0x6574") ; <CJK>
       (?$(Gpm(B . "0x66C6") ; <CJK>
       (?$(Gpn(B . "0x66C9") ; <CJK>
       (?$(Gpo(B . "0x66B9") ; <CJK>
       (?$(Gpp(B . "0x66C4") ; <CJK>
       (?$(Gpq(B . "0x66C7") ; <CJK>
       (?$(Gpr(B . "0x66B8") ; <CJK>
       (?$(Gps(B . "0x6A3D") ; <CJK>
       (?$(Gpt(B . "0x6A38") ; <CJK>
       (?$(Gpu(B . "0x6A3A") ; <CJK>
       (?$(Gpv(B . "0x6A59") ; <CJK>
       (?$(Gpw(B . "0x6A6B") ; <CJK>
       (?$(Gpx(B . "0x6A58") ; <CJK>
       (?$(Gpy(B . "0x6A39") ; <CJK>
       (?$(Gpz(B . "0x6A44") ; <CJK>
       (?$(Gp{(B . "0x6A62") ; <CJK>
       (?$(Gp|(B . "0x6A61") ; <CJK>
       (?$(Gp}(B . "0x6A4B") ; <CJK>
       (?$(Gp~(B . "0x6A47") ; <CJK>
       (?$(Gq!(B . "0x6A35") ; <CJK>
       (?$(Gq"(B . "0x6A5F") ; <CJK>
       (?$(Gq#(B . "0x6A48") ; <CJK>
       (?$(Gq$(B . "0x6B59") ; <CJK>
       (?$(Gq%(B . "0x6B77") ; <CJK>
       (?$(Gq&(B . "0x6C05") ; <CJK>
       (?$(Gq'(B . "0x6FC2") ; <CJK>
       (?$(Gq((B . "0x6FB1") ; <CJK>
       (?$(Gq)(B . "0x6FA1") ; <CJK>
       (?$(Gq*(B . "0x6FC3") ; <CJK>
       (?$(Gq+(B . "0x6FA4") ; <CJK>
       (?$(Gq,(B . "0x6FC1") ; <CJK>
       (?$(Gq-(B . "0x6FA7") ; <CJK>
       (?$(Gq.(B . "0x6FB3") ; <CJK>
       (?$(Gq/(B . "0x6FC0") ; <CJK>
       (?$(Gq0(B . "0x6FB9") ; <CJK>
       (?$(Gq1(B . "0x6FB6") ; <CJK>
       (?$(Gq2(B . "0x6FA6") ; <CJK>
       (?$(Gq3(B . "0x6FA0") ; <CJK>
       (?$(Gq4(B . "0x6FB4") ; <CJK>
       (?$(Gq5(B . "0x71BE") ; <CJK>
       (?$(Gq6(B . "0x71C9") ; <CJK>
       (?$(Gq7(B . "0x71D0") ; <CJK>
       (?$(Gq8(B . "0x71D2") ; <CJK>
       (?$(Gq9(B . "0x71C8") ; <CJK>
       (?$(Gq:(B . "0x71D5") ; <CJK>
       (?$(Gq;(B . "0x71B9") ; <CJK>
       (?$(Gq<(B . "0x71CE") ; <CJK>
       (?$(Gq=(B . "0x71D9") ; <CJK>
       (?$(Gq>(B . "0x71DC") ; <CJK>
       (?$(Gq?(B . "0x71C3") ; <CJK>
       (?$(Gq@(B . "0x71C4") ; <CJK>
       (?$(GqA(B . "0x7368") ; <CJK>
       (?$(GqB(B . "0x749C") ; <CJK>
       (?$(GqC(B . "0x74A3") ; <CJK>
       (?$(GqD(B . "0x7498") ; <CJK>
       (?$(GqE(B . "0x749F") ; <CJK>
       (?$(GqF(B . "0x749E") ; <CJK>
       (?$(GqG(B . "0x74E2") ; <CJK>
       (?$(GqH(B . "0x750C") ; <CJK>
       (?$(GqI(B . "0x750D") ; <CJK>
       (?$(GqJ(B . "0x7634") ; <CJK>
       (?$(GqK(B . "0x7638") ; <CJK>
       (?$(GqL(B . "0x763A") ; <CJK>
       (?$(GqM(B . "0x76E7") ; <CJK>
       (?$(GqN(B . "0x76E5") ; <CJK>
       (?$(GqO(B . "0x77A0") ; <CJK>
       (?$(GqP(B . "0x779E") ; <CJK>
       (?$(GqQ(B . "0x779F") ; <CJK>
       (?$(GqR(B . "0x77A5") ; <CJK>
       (?$(GqS(B . "0x78E8") ; <CJK>
       (?$(GqT(B . "0x78DA") ; <CJK>
       (?$(GqU(B . "0x78EC") ; <CJK>
       (?$(GqV(B . "0x78E7") ; <CJK>
       (?$(GqW(B . "0x79A6") ; <CJK>
       (?$(GqX(B . "0x7A4D") ; <CJK>
       (?$(GqY(B . "0x7A4E") ; <CJK>
       (?$(GqZ(B . "0x7A46") ; <CJK>
       (?$(Gq[(B . "0x7A4C") ; <CJK>
       (?$(Gq\(B . "0x7A4B") ; <CJK>
       (?$(Gq](B . "0x7ABA") ; <CJK>
       (?$(Gq^(B . "0x7BD9") ; <CJK>
       (?$(Gq_(B . "0x7C11") ; <CJK>
       (?$(Gq`(B . "0x7BC9") ; <CJK>
       (?$(Gqa(B . "0x7BE4") ; <CJK>
       (?$(Gqb(B . "0x7BDB") ; <CJK>
       (?$(Gqc(B . "0x7BE1") ; <CJK>
       (?$(Gqd(B . "0x7BE9") ; <CJK>
       (?$(Gqe(B . "0x7BE6") ; <CJK>
       (?$(Gqf(B . "0x7CD5") ; <CJK>
       (?$(Gqg(B . "0x7CD6") ; <CJK>
       (?$(Gqh(B . "0x7E0A") ; <CJK>
       (?$(Gqi(B . "0x7E11") ; <CJK>
       (?$(Gqj(B . "0x7E08") ; <CJK>
       (?$(Gqk(B . "0x7E1B") ; <CJK>
       (?$(Gql(B . "0x7E23") ; <CJK>
       (?$(Gqm(B . "0x7E1E") ; <CJK>
       (?$(Gqn(B . "0x7E1D") ; <CJK>
       (?$(Gqo(B . "0x7E09") ; <CJK>
       (?$(Gqp(B . "0x7E10") ; <CJK>
       (?$(Gqq(B . "0x7F79") ; <CJK>
       (?$(Gqr(B . "0x7FB2") ; <CJK>
       (?$(Gqs(B . "0x7FF0") ; <CJK>
       (?$(Gqt(B . "0x7FF1") ; <CJK>
       (?$(Gqu(B . "0x7FEE") ; <CJK>
       (?$(Gqv(B . "0x8028") ; <CJK>
       (?$(Gqw(B . "0x81B3") ; <CJK>
       (?$(Gqx(B . "0x81A9") ; <CJK>
       (?$(Gqy(B . "0x81A8") ; <CJK>
       (?$(Gqz(B . "0x81FB") ; <CJK>
       (?$(Gq{(B . "0x8208") ; <CJK>
       (?$(Gq|(B . "0x8258") ; <CJK>
       (?$(Gq}(B . "0x8259") ; <CJK>
       (?$(Gq~(B . "0x854A") ; <CJK>
       (?$(Gr!(B . "0x8559") ; <CJK>
       (?$(Gr"(B . "0x8548") ; <CJK>
       (?$(Gr#(B . "0x8568") ; <CJK>
       (?$(Gr$(B . "0x8569") ; <CJK>
       (?$(Gr%(B . "0x8543") ; <CJK>
       (?$(Gr&(B . "0x8549") ; <CJK>
       (?$(Gr'(B . "0x856D") ; <CJK>
       (?$(Gr((B . "0x856A") ; <CJK>
       (?$(Gr)(B . "0x855E") ; <CJK>
       (?$(Gr*(B . "0x8783") ; <CJK>
       (?$(Gr+(B . "0x879F") ; <CJK>
       (?$(Gr,(B . "0x879E") ; <CJK>
       (?$(Gr-(B . "0x87A2") ; <CJK>
       (?$(Gr.(B . "0x878D") ; <CJK>
       (?$(Gr/(B . "0x8861") ; <CJK>
       (?$(Gr0(B . "0x892A") ; <CJK>
       (?$(Gr1(B . "0x8932") ; <CJK>
       (?$(Gr2(B . "0x8925") ; <CJK>
       (?$(Gr3(B . "0x892B") ; <CJK>
       (?$(Gr4(B . "0x8921") ; <CJK>
       (?$(Gr5(B . "0x89AA") ; <CJK>
       (?$(Gr6(B . "0x89A6") ; <CJK>
       (?$(Gr7(B . "0x8AE6") ; <CJK>
       (?$(Gr8(B . "0x8AFA") ; <CJK>
       (?$(Gr9(B . "0x8AEB") ; <CJK>
       (?$(Gr:(B . "0x8AF1") ; <CJK>
       (?$(Gr;(B . "0x8B00") ; <CJK>
       (?$(Gr<(B . "0x8ADC") ; <CJK>
       (?$(Gr=(B . "0x8AE7") ; <CJK>
       (?$(Gr>(B . "0x8AEE") ; <CJK>
       (?$(Gr?(B . "0x8AFE") ; <CJK>
       (?$(Gr@(B . "0x8B01") ; <CJK>
       (?$(GrA(B . "0x8B02") ; <CJK>
       (?$(GrB(B . "0x8AF7") ; <CJK>
       (?$(GrC(B . "0x8AED") ; <CJK>
       (?$(GrD(B . "0x8AF3") ; <CJK>
       (?$(GrE(B . "0x8AF6") ; <CJK>
       (?$(GrF(B . "0x8AFC") ; <CJK>
       (?$(GrG(B . "0x8C6B") ; <CJK>
       (?$(GrH(B . "0x8C6D") ; <CJK>
       (?$(GrI(B . "0x8C93") ; <CJK>
       (?$(GrJ(B . "0x8CF4") ; <CJK>
       (?$(GrK(B . "0x8E44") ; <CJK>
       (?$(GrL(B . "0x8E31") ; <CJK>
       (?$(GrM(B . "0x8E34") ; <CJK>
       (?$(GrN(B . "0x8E42") ; <CJK>
       (?$(GrO(B . "0x8E39") ; <CJK>
       (?$(GrP(B . "0x8E35") ; <CJK>
       (?$(GrQ(B . "0x8F3B") ; <CJK>
       (?$(GrR(B . "0x8F2F") ; <CJK>
       (?$(GrS(B . "0x8F38") ; <CJK>
       (?$(GrT(B . "0x8F33") ; <CJK>
       (?$(GrU(B . "0x8FA8") ; <CJK>
       (?$(GrV(B . "0x8FA6") ; <CJK>
       (?$(GrW(B . "0x9075") ; <CJK>
       (?$(GrX(B . "0x9074") ; <CJK>
       (?$(GrY(B . "0x9078") ; <CJK>
       (?$(GrZ(B . "0x9072") ; <CJK>
       (?$(Gr[(B . "0x907C") ; <CJK>
       (?$(Gr\(B . "0x907A") ; <CJK>
       (?$(Gr](B . "0x9134") ; <CJK>
       (?$(Gr^(B . "0x9192") ; <CJK>
       (?$(Gr_(B . "0x9320") ; <CJK>
       (?$(Gr`(B . "0x9336") ; <CJK>
       (?$(Gra(B . "0x92F8") ; <CJK>
       (?$(Grb(B . "0x9333") ; <CJK>
       (?$(Grc(B . "0x932F") ; <CJK>
       (?$(Grd(B . "0x9322") ; <CJK>
       (?$(Gre(B . "0x92FC") ; <CJK>
       (?$(Grf(B . "0x932B") ; <CJK>
       (?$(Grg(B . "0x9304") ; <CJK>
       (?$(Grh(B . "0x931A") ; <CJK>
       (?$(Gri(B . "0x9310") ; <CJK>
       (?$(Grj(B . "0x9326") ; <CJK>
       (?$(Grk(B . "0x9321") ; <CJK>
       (?$(Grl(B . "0x9315") ; <CJK>
       (?$(Grm(B . "0x932E") ; <CJK>
       (?$(Grn(B . "0x9319") ; <CJK>
       (?$(Gro(B . "0x95BB") ; <CJK>
       (?$(Grp(B . "0x96A7") ; <CJK>
       (?$(Grq(B . "0x96A8") ; <CJK>
       (?$(Grr(B . "0x96AA") ; <CJK>
       (?$(Grs(B . "0x96D5") ; <CJK>
       (?$(Grt(B . "0x970E") ; <CJK>
       (?$(Gru(B . "0x9711") ; <CJK>
       (?$(Grv(B . "0x9716") ; <CJK>
       (?$(Grw(B . "0x970D") ; <CJK>
       (?$(Grx(B . "0x9713") ; <CJK>
       (?$(Gry(B . "0x970F") ; <CJK>
       (?$(Grz(B . "0x975B") ; <CJK>
       (?$(Gr{(B . "0x975C") ; <CJK>
       (?$(Gr|(B . "0x9766") ; <CJK>
       (?$(Gr}(B . "0x9798") ; <CJK>
       (?$(Gr~(B . "0x9830") ; <CJK>
       (?$(Gs!(B . "0x9838") ; <CJK>
       (?$(Gs"(B . "0x983B") ; <CJK>
       (?$(Gs#(B . "0x9837") ; <CJK>
       (?$(Gs$(B . "0x982D") ; <CJK>
       (?$(Gs%(B . "0x9839") ; <CJK>
       (?$(Gs&(B . "0x9824") ; <CJK>
       (?$(Gs'(B . "0x9910") ; <CJK>
       (?$(Gs((B . "0x9928") ; <CJK>
       (?$(Gs)(B . "0x991E") ; <CJK>
       (?$(Gs*(B . "0x991B") ; <CJK>
       (?$(Gs+(B . "0x9921") ; <CJK>
       (?$(Gs,(B . "0x991A") ; <CJK>
       (?$(Gs-(B . "0x99ED") ; <CJK>
       (?$(Gs.(B . "0x99E2") ; <CJK>
       (?$(Gs/(B . "0x99F1") ; <CJK>
       (?$(Gs0(B . "0x9AB8") ; <CJK>
       (?$(Gs1(B . "0x9ABC") ; <CJK>
       (?$(Gs2(B . "0x9AFB") ; <CJK>
       (?$(Gs3(B . "0x9AED") ; <CJK>
       (?$(Gs4(B . "0x9B28") ; <CJK>
       (?$(Gs5(B . "0x9B91") ; <CJK>
       (?$(Gs6(B . "0x9D15") ; <CJK>
       (?$(Gs7(B . "0x9D23") ; <CJK>
       (?$(Gs8(B . "0x9D26") ; <CJK>
       (?$(Gs9(B . "0x9D28") ; <CJK>
       (?$(Gs:(B . "0x9D12") ; <CJK>
       (?$(Gs;(B . "0x9D1B") ; <CJK>
       (?$(Gs<(B . "0x9ED8") ; <CJK>
       (?$(Gs=(B . "0x9ED4") ; <CJK>
       (?$(Gs>(B . "0x9F8D") ; <CJK>
       (?$(Gs?(B . "0x9F9C") ; <CJK>
       (?$(Gs@(B . "0x512A") ; <CJK>
       (?$(GsA(B . "0x511F") ; <CJK>
       (?$(GsB(B . "0x5121") ; <CJK>
       (?$(GsC(B . "0x5132") ; <CJK>
       (?$(GsD(B . "0x52F5") ; <CJK>
       (?$(GsE(B . "0x568E") ; <CJK>
       (?$(GsF(B . "0x5680") ; <CJK>
       (?$(GsG(B . "0x5690") ; <CJK>
       (?$(GsH(B . "0x5685") ; <CJK>
       (?$(GsI(B . "0x5687") ; <CJK>
       (?$(GsJ(B . "0x568F") ; <CJK>
       (?$(GsK(B . "0x58D5") ; <CJK>
       (?$(GsL(B . "0x58D3") ; <CJK>
       (?$(GsM(B . "0x58D1") ; <CJK>
       (?$(GsN(B . "0x58CE") ; <CJK>
       (?$(GsO(B . "0x5B30") ; <CJK>
       (?$(GsP(B . "0x5B2A") ; <CJK>
       (?$(GsQ(B . "0x5B24") ; <CJK>
       (?$(GsR(B . "0x5B7A") ; <CJK>
       (?$(GsS(B . "0x5C37") ; <CJK>
       (?$(GsT(B . "0x5C68") ; <CJK>
       (?$(GsU(B . "0x5DBC") ; <CJK>
       (?$(GsV(B . "0x5DBA") ; <CJK>
       (?$(GsW(B . "0x5DBD") ; <CJK>
       (?$(GsX(B . "0x5DB8") ; <CJK>
       (?$(GsY(B . "0x5E6B") ; <CJK>
       (?$(GsZ(B . "0x5F4C") ; <CJK>
       (?$(Gs[(B . "0x5FBD") ; <CJK>
       (?$(Gs\(B . "0x61C9") ; <CJK>
       (?$(Gs](B . "0x61C2") ; <CJK>
       (?$(Gs^(B . "0x61C7") ; <CJK>
       (?$(Gs_(B . "0x61E6") ; <CJK>
       (?$(Gs`(B . "0x61CB") ; <CJK>
       (?$(Gsa(B . "0x6232") ; <CJK>
       (?$(Gsb(B . "0x6234") ; <CJK>
       (?$(Gsc(B . "0x64CE") ; <CJK>
       (?$(Gsd(B . "0x64CA") ; <CJK>
       (?$(Gse(B . "0x64D8") ; <CJK>
       (?$(Gsf(B . "0x64E0") ; <CJK>
       (?$(Gsg(B . "0x64F0") ; <CJK>
       (?$(Gsh(B . "0x64E6") ; <CJK>
       (?$(Gsi(B . "0x64EC") ; <CJK>
       (?$(Gsj(B . "0x64F1") ; <CJK>
       (?$(Gsk(B . "0x64E2") ; <CJK>
       (?$(Gsl(B . "0x64ED") ; <CJK>
       (?$(Gsm(B . "0x6582") ; <CJK>
       (?$(Gsn(B . "0x6583") ; <CJK>
       (?$(Gso(B . "0x66D9") ; <CJK>
       (?$(Gsp(B . "0x66D6") ; <CJK>
       (?$(Gsq(B . "0x6A80") ; <CJK>
       (?$(Gsr(B . "0x6A94") ; <CJK>
       (?$(Gss(B . "0x6A84") ; <CJK>
       (?$(Gst(B . "0x6AA2") ; <CJK>
       (?$(Gsu(B . "0x6A9C") ; <CJK>
       (?$(Gsv(B . "0x6ADB") ; <CJK>
       (?$(Gsw(B . "0x6AA3") ; <CJK>
       (?$(Gsx(B . "0x6A7E") ; <CJK>
       (?$(Gsy(B . "0x6A97") ; <CJK>
       (?$(Gsz(B . "0x6A90") ; <CJK>
       (?$(Gs{(B . "0x6AA0") ; <CJK>
       (?$(Gs|(B . "0x6B5C") ; <CJK>
       (?$(Gs}(B . "0x6BAE") ; <CJK>
       (?$(Gs~(B . "0x6BDA") ; <CJK>
       (?$(Gt!(B . "0x6C08") ; <CJK>
       (?$(Gt"(B . "0x6FD8") ; <CJK>
       (?$(Gt#(B . "0x6FF1") ; <CJK>
       (?$(Gt$(B . "0x6FDF") ; <CJK>
       (?$(Gt%(B . "0x6FE0") ; <CJK>
       (?$(Gt&(B . "0x6FDB") ; <CJK>
       (?$(Gt'(B . "0x6FE4") ; <CJK>
       (?$(Gt((B . "0x6FEB") ; <CJK>
       (?$(Gt)(B . "0x6FEF") ; <CJK>
       (?$(Gt*(B . "0x6F80") ; <CJK>
       (?$(Gt+(B . "0x6FEC") ; <CJK>
       (?$(Gt,(B . "0x6FE1") ; <CJK>
       (?$(Gt-(B . "0x6FE9") ; <CJK>
       (?$(Gt.(B . "0x6FD5") ; <CJK>
       (?$(Gt/(B . "0x6FEE") ; <CJK>
       (?$(Gt0(B . "0x6FF0") ; <CJK>
       (?$(Gt1(B . "0x71E7") ; <CJK>
       (?$(Gt2(B . "0x71DF") ; <CJK>
       (?$(Gt3(B . "0x71EE") ; <CJK>
       (?$(Gt4(B . "0x71E6") ; <CJK>
       (?$(Gt5(B . "0x71E5") ; <CJK>
       (?$(Gt6(B . "0x71ED") ; <CJK>
       (?$(Gt7(B . "0x71EC") ; <CJK>
       (?$(Gt8(B . "0x71F4") ; <CJK>
       (?$(Gt9(B . "0x71E0") ; <CJK>
       (?$(Gt:(B . "0x7235") ; <CJK>
       (?$(Gt;(B . "0x7246") ; <CJK>
       (?$(Gt<(B . "0x7370") ; <CJK>
       (?$(Gt=(B . "0x7372") ; <CJK>
       (?$(Gt>(B . "0x74A9") ; <CJK>
       (?$(Gt?(B . "0x74B0") ; <CJK>
       (?$(Gt@(B . "0x74A6") ; <CJK>
       (?$(GtA(B . "0x74A8") ; <CJK>
       (?$(GtB(B . "0x7646") ; <CJK>
       (?$(GtC(B . "0x7642") ; <CJK>
       (?$(GtD(B . "0x764C") ; <CJK>
       (?$(GtE(B . "0x76EA") ; <CJK>
       (?$(GtF(B . "0x77B3") ; <CJK>
       (?$(GtG(B . "0x77AA") ; <CJK>
       (?$(GtH(B . "0x77B0") ; <CJK>
       (?$(GtI(B . "0x77AC") ; <CJK>
       (?$(GtJ(B . "0x77A7") ; <CJK>
       (?$(GtK(B . "0x77AD") ; <CJK>
       (?$(GtL(B . "0x77EF") ; <CJK>
       (?$(GtM(B . "0x78F7") ; <CJK>
       (?$(GtN(B . "0x78FA") ; <CJK>
       (?$(GtO(B . "0x78F4") ; <CJK>
       (?$(GtP(B . "0x78EF") ; <CJK>
       (?$(GtQ(B . "0x7901") ; <CJK>
       (?$(GtR(B . "0x79A7") ; <CJK>
       (?$(GtS(B . "0x79AA") ; <CJK>
       (?$(GtT(B . "0x7A57") ; <CJK>
       (?$(GtU(B . "0x7ABF") ; <CJK>
       (?$(GtV(B . "0x7C07") ; <CJK>
       (?$(GtW(B . "0x7C0D") ; <CJK>
       (?$(GtX(B . "0x7BFE") ; <CJK>
       (?$(GtY(B . "0x7BF7") ; <CJK>
       (?$(GtZ(B . "0x7C0C") ; <CJK>
       (?$(Gt[(B . "0x7BE0") ; <CJK>
       (?$(Gt\(B . "0x7CE0") ; <CJK>
       (?$(Gt](B . "0x7CDC") ; <CJK>
       (?$(Gt^(B . "0x7CDE") ; <CJK>
       (?$(Gt_(B . "0x7CE2") ; <CJK>
       (?$(Gt`(B . "0x7CDF") ; <CJK>
       (?$(Gta(B . "0x7CD9") ; <CJK>
       (?$(Gtb(B . "0x7CDD") ; <CJK>
       (?$(Gtc(B . "0x7E2E") ; <CJK>
       (?$(Gtd(B . "0x7E3E") ; <CJK>
       (?$(Gte(B . "0x7E46") ; <CJK>
       (?$(Gtf(B . "0x7E37") ; <CJK>
       (?$(Gtg(B . "0x7E32") ; <CJK>
       (?$(Gth(B . "0x7E43") ; <CJK>
       (?$(Gti(B . "0x7E2B") ; <CJK>
       (?$(Gtj(B . "0x7E3D") ; <CJK>
       (?$(Gtk(B . "0x7E31") ; <CJK>
       (?$(Gtl(B . "0x7E45") ; <CJK>
       (?$(Gtm(B . "0x7E41") ; <CJK>
       (?$(Gtn(B . "0x7E34") ; <CJK>
       (?$(Gto(B . "0x7E39") ; <CJK>
       (?$(Gtp(B . "0x7E48") ; <CJK>
       (?$(Gtq(B . "0x7E35") ; <CJK>
       (?$(Gtr(B . "0x7E3F") ; <CJK>
       (?$(Gts(B . "0x7E2F") ; <CJK>
       (?$(Gtt(B . "0x7F44") ; <CJK>
       (?$(Gtu(B . "0x7FF3") ; <CJK>
       (?$(Gtv(B . "0x7FFC") ; <CJK>
       (?$(Gtw(B . "0x8071") ; <CJK>
       (?$(Gtx(B . "0x8072") ; <CJK>
       (?$(Gty(B . "0x8070") ; <CJK>
       (?$(Gtz(B . "0x806F") ; <CJK>
       (?$(Gt{(B . "0x8073") ; <CJK>
       (?$(Gt|(B . "0x81C6") ; <CJK>
       (?$(Gt}(B . "0x81C3") ; <CJK>
       (?$(Gt~(B . "0x81BA") ; <CJK>
       (?$(Gu!(B . "0x81C2") ; <CJK>
       (?$(Gu"(B . "0x81C0") ; <CJK>
       (?$(Gu#(B . "0x81BF") ; <CJK>
       (?$(Gu$(B . "0x81BD") ; <CJK>
       (?$(Gu%(B . "0x81C9") ; <CJK>
       (?$(Gu&(B . "0x81BE") ; <CJK>
       (?$(Gu'(B . "0x81E8") ; <CJK>
       (?$(Gu((B . "0x8209") ; <CJK>
       (?$(Gu)(B . "0x8271") ; <CJK>
       (?$(Gu*(B . "0x85AA") ; <CJK>
       (?$(Gu+(B . "0x8584") ; <CJK>
       (?$(Gu,(B . "0x857E") ; <CJK>
       (?$(Gu-(B . "0x859C") ; <CJK>
       (?$(Gu.(B . "0x8591") ; <CJK>
       (?$(Gu/(B . "0x8594") ; <CJK>
       (?$(Gu0(B . "0x85AF") ; <CJK>
       (?$(Gu1(B . "0x859B") ; <CJK>
       (?$(Gu2(B . "0x8587") ; <CJK>
       (?$(Gu3(B . "0x85A8") ; <CJK>
       (?$(Gu4(B . "0x858A") ; <CJK>
       (?$(Gu5(B . "0x85A6") ; <CJK>
       (?$(Gu6(B . "0x8667") ; <CJK>
       (?$(Gu7(B . "0x87C0") ; <CJK>
       (?$(Gu8(B . "0x87D1") ; <CJK>
       (?$(Gu9(B . "0x87B3") ; <CJK>
       (?$(Gu:(B . "0x87D2") ; <CJK>
       (?$(Gu;(B . "0x87C6") ; <CJK>
       (?$(Gu<(B . "0x87AB") ; <CJK>
       (?$(Gu=(B . "0x87BB") ; <CJK>
       (?$(Gu>(B . "0x87BA") ; <CJK>
       (?$(Gu?(B . "0x87C8") ; <CJK>
       (?$(Gu@(B . "0x87CB") ; <CJK>
       (?$(GuA(B . "0x893B") ; <CJK>
       (?$(GuB(B . "0x8936") ; <CJK>
       (?$(GuC(B . "0x8944") ; <CJK>
       (?$(GuD(B . "0x8938") ; <CJK>
       (?$(GuE(B . "0x893D") ; <CJK>
       (?$(GuF(B . "0x89AC") ; <CJK>
       (?$(GuG(B . "0x8B0E") ; <CJK>
       (?$(GuH(B . "0x8B17") ; <CJK>
       (?$(GuI(B . "0x8B19") ; <CJK>
       (?$(GuJ(B . "0x8B1B") ; <CJK>
       (?$(GuK(B . "0x8B0A") ; <CJK>
       (?$(GuL(B . "0x8B20") ; <CJK>
       (?$(GuM(B . "0x8B1D") ; <CJK>
       (?$(GuN(B . "0x8B04") ; <CJK>
       (?$(GuO(B . "0x8B10") ; <CJK>
       (?$(GuP(B . "0x8C41") ; <CJK>
       (?$(GuQ(B . "0x8C3F") ; <CJK>
       (?$(GuR(B . "0x8C73") ; <CJK>
       (?$(GuS(B . "0x8CFA") ; <CJK>
       (?$(GuT(B . "0x8CFD") ; <CJK>
       (?$(GuU(B . "0x8CFC") ; <CJK>
       (?$(GuV(B . "0x8CF8") ; <CJK>
       (?$(GuW(B . "0x8CFB") ; <CJK>
       (?$(GuX(B . "0x8DA8") ; <CJK>
       (?$(GuY(B . "0x8E49") ; <CJK>
       (?$(GuZ(B . "0x8E4B") ; <CJK>
       (?$(Gu[(B . "0x8E48") ; <CJK>
       (?$(Gu\(B . "0x8E4A") ; <CJK>
       (?$(Gu](B . "0x8F44") ; <CJK>
       (?$(Gu^(B . "0x8F3E") ; <CJK>
       (?$(Gu_(B . "0x8F42") ; <CJK>
       (?$(Gu`(B . "0x8F45") ; <CJK>
       (?$(Gua(B . "0x8F3F") ; <CJK>
       (?$(Gub(B . "0x907F") ; <CJK>
       (?$(Guc(B . "0x907D") ; <CJK>
       (?$(Gud(B . "0x9084") ; <CJK>
       (?$(Gue(B . "0x9081") ; <CJK>
       (?$(Guf(B . "0x9082") ; <CJK>
       (?$(Gug(B . "0x9080") ; <CJK>
       (?$(Guh(B . "0x9139") ; <CJK>
       (?$(Gui(B . "0x91A3") ; <CJK>
       (?$(Guj(B . "0x919E") ; <CJK>
       (?$(Guk(B . "0x919C") ; <CJK>
       (?$(Gul(B . "0x934D") ; <CJK>
       (?$(Gum(B . "0x9382") ; <CJK>
       (?$(Gun(B . "0x9328") ; <CJK>
       (?$(Guo(B . "0x9375") ; <CJK>
       (?$(Gup(B . "0x934A") ; <CJK>
       (?$(Guq(B . "0x9365") ; <CJK>
       (?$(Gur(B . "0x934B") ; <CJK>
       (?$(Gus(B . "0x9318") ; <CJK>
       (?$(Gut(B . "0x937E") ; <CJK>
       (?$(Guu(B . "0x936C") ; <CJK>
       (?$(Guv(B . "0x935B") ; <CJK>
       (?$(Guw(B . "0x9370") ; <CJK>
       (?$(Gux(B . "0x935A") ; <CJK>
       (?$(Guy(B . "0x9354") ; <CJK>
       (?$(Guz(B . "0x95CA") ; <CJK>
       (?$(Gu{(B . "0x95CB") ; <CJK>
       (?$(Gu|(B . "0x95CC") ; <CJK>
       (?$(Gu}(B . "0x95C8") ; <CJK>
       (?$(Gu~(B . "0x95C6") ; <CJK>
       (?$(Gv!(B . "0x96B1") ; <CJK>
       (?$(Gv"(B . "0x96B8") ; <CJK>
       (?$(Gv#(B . "0x96D6") ; <CJK>
       (?$(Gv$(B . "0x971C") ; <CJK>
       (?$(Gv%(B . "0x971E") ; <CJK>
       (?$(Gv&(B . "0x97A0") ; <CJK>
       (?$(Gv'(B . "0x97D3") ; <CJK>
       (?$(Gv((B . "0x9846") ; <CJK>
       (?$(Gv)(B . "0x98B6") ; <CJK>
       (?$(Gv*(B . "0x9935") ; <CJK>
       (?$(Gv+(B . "0x9A01") ; <CJK>
       (?$(Gv,(B . "0x99FF") ; <CJK>
       (?$(Gv-(B . "0x9BAE") ; <CJK>
       (?$(Gv.(B . "0x9BAB") ; <CJK>
       (?$(Gv/(B . "0x9BAA") ; <CJK>
       (?$(Gv0(B . "0x9BAD") ; <CJK>
       (?$(Gv1(B . "0x9D3B") ; <CJK>
       (?$(Gv2(B . "0x9D3F") ; <CJK>
       (?$(Gv3(B . "0x9E8B") ; <CJK>
       (?$(Gv4(B . "0x9ECF") ; <CJK>
       (?$(Gv5(B . "0x9EDE") ; <CJK>
       (?$(Gv6(B . "0x9EDC") ; <CJK>
       (?$(Gv7(B . "0x9EDD") ; <CJK>
       (?$(Gv8(B . "0x9EDB") ; <CJK>
       (?$(Gv9(B . "0x9F3E") ; <CJK>
       (?$(Gv:(B . "0x9F4B") ; <CJK>
       (?$(Gv;(B . "0x53E2") ; <CJK>
       (?$(Gv<(B . "0x5695") ; <CJK>
       (?$(Gv=(B . "0x56AE") ; <CJK>
       (?$(Gv>(B . "0x58D9") ; <CJK>
       (?$(Gv?(B . "0x58D8") ; <CJK>
       (?$(Gv@(B . "0x5B38") ; <CJK>
       (?$(GvA(B . "0x5F5E") ; <CJK>
       (?$(GvB(B . "0x61E3") ; <CJK>
       (?$(GvC(B . "0x6233") ; <CJK>
       (?$(GvD(B . "0x64F4") ; <CJK>
       (?$(GvE(B . "0x64F2") ; <CJK>
       (?$(GvF(B . "0x64FE") ; <CJK>
       (?$(GvG(B . "0x6506") ; <CJK>
       (?$(GvH(B . "0x64FA") ; <CJK>
       (?$(GvI(B . "0x64FB") ; <CJK>
       (?$(GvJ(B . "0x64F7") ; <CJK>
       (?$(GvK(B . "0x65B7") ; <CJK>
       (?$(GvL(B . "0x66DC") ; <CJK>
       (?$(GvM(B . "0x6726") ; <CJK>
       (?$(GvN(B . "0x6AB3") ; <CJK>
       (?$(GvO(B . "0x6AAC") ; <CJK>
       (?$(GvP(B . "0x6AC3") ; <CJK>
       (?$(GvQ(B . "0x6ABB") ; <CJK>
       (?$(GvR(B . "0x6AB8") ; <CJK>
       (?$(GvS(B . "0x6AC2") ; <CJK>
       (?$(GvT(B . "0x6AAE") ; <CJK>
       (?$(GvU(B . "0x6AAF") ; <CJK>
       (?$(GvV(B . "0x6B5F") ; <CJK>
       (?$(GvW(B . "0x6B78") ; <CJK>
       (?$(GvX(B . "0x6BAF") ; <CJK>
       (?$(GvY(B . "0x7009") ; <CJK>
       (?$(GvZ(B . "0x700B") ; <CJK>
       (?$(Gv[(B . "0x6FFE") ; <CJK>
       (?$(Gv\(B . "0x7006") ; <CJK>
       (?$(Gv](B . "0x6FFA") ; <CJK>
       (?$(Gv^(B . "0x7011") ; <CJK>
       (?$(Gv_(B . "0x700F") ; <CJK>
       (?$(Gv`(B . "0x71FB") ; <CJK>
       (?$(Gva(B . "0x71FC") ; <CJK>
       (?$(Gvb(B . "0x71FE") ; <CJK>
       (?$(Gvc(B . "0x71F8") ; <CJK>
       (?$(Gvd(B . "0x7377") ; <CJK>
       (?$(Gve(B . "0x7375") ; <CJK>
       (?$(Gvf(B . "0x74A7") ; <CJK>
       (?$(Gvg(B . "0x74BF") ; <CJK>
       (?$(Gvh(B . "0x7515") ; <CJK>
       (?$(Gvi(B . "0x7656") ; <CJK>
       (?$(Gvj(B . "0x7658") ; <CJK>
       (?$(Gvk(B . "0x7652") ; <CJK>
       (?$(Gvl(B . "0x77BD") ; <CJK>
       (?$(Gvm(B . "0x77BF") ; <CJK>
       (?$(Gvn(B . "0x77BB") ; <CJK>
       (?$(Gvo(B . "0x77BC") ; <CJK>
       (?$(Gvp(B . "0x790E") ; <CJK>
       (?$(Gvq(B . "0x79AE") ; <CJK>
       (?$(Gvr(B . "0x7A61") ; <CJK>
       (?$(Gvs(B . "0x7A62") ; <CJK>
       (?$(Gvt(B . "0x7A60") ; <CJK>
       (?$(Gvu(B . "0x7AC4") ; <CJK>
       (?$(Gvv(B . "0x7AC5") ; <CJK>
       (?$(Gvw(B . "0x7C2B") ; <CJK>
       (?$(Gvx(B . "0x7C27") ; <CJK>
       (?$(Gvy(B . "0x7C2A") ; <CJK>
       (?$(Gvz(B . "0x7C1E") ; <CJK>
       (?$(Gv{(B . "0x7C23") ; <CJK>
       (?$(Gv|(B . "0x7C21") ; <CJK>
       (?$(Gv}(B . "0x7CE7") ; <CJK>
       (?$(Gv~(B . "0x7E54") ; <CJK>
       (?$(Gw!(B . "0x7E55") ; <CJK>
       (?$(Gw"(B . "0x7E5E") ; <CJK>
       (?$(Gw#(B . "0x7E5A") ; <CJK>
       (?$(Gw$(B . "0x7E61") ; <CJK>
       (?$(Gw%(B . "0x7E52") ; <CJK>
       (?$(Gw&(B . "0x7E59") ; <CJK>
       (?$(Gw'(B . "0x7F48") ; <CJK>
       (?$(Gw((B . "0x7FF9") ; <CJK>
       (?$(Gw)(B . "0x7FFB") ; <CJK>
       (?$(Gw*(B . "0x8077") ; <CJK>
       (?$(Gw+(B . "0x8076") ; <CJK>
       (?$(Gw,(B . "0x81CD") ; <CJK>
       (?$(Gw-(B . "0x81CF") ; <CJK>
       (?$(Gw.(B . "0x820A") ; <CJK>
       (?$(Gw/(B . "0x85CF") ; <CJK>
       (?$(Gw0(B . "0x85A9") ; <CJK>
       (?$(Gw1(B . "0x85CD") ; <CJK>
       (?$(Gw2(B . "0x85D0") ; <CJK>
       (?$(Gw3(B . "0x85C9") ; <CJK>
       (?$(Gw4(B . "0x85B0") ; <CJK>
       (?$(Gw5(B . "0x85BA") ; <CJK>
       (?$(Gw6(B . "0x85B9") ; <CJK>
       (?$(Gw7(B . "0x87EF") ; <CJK>
       (?$(Gw8(B . "0x87EC") ; <CJK>
       (?$(Gw9(B . "0x87F2") ; <CJK>
       (?$(Gw:(B . "0x87E0") ; <CJK>
       (?$(Gw;(B . "0x8986") ; <CJK>
       (?$(Gw<(B . "0x89B2") ; <CJK>
       (?$(Gw=(B . "0x89F4") ; <CJK>
       (?$(Gw>(B . "0x8B28") ; <CJK>
       (?$(Gw?(B . "0x8B39") ; <CJK>
       (?$(Gw@(B . "0x8B2C") ; <CJK>
       (?$(GwA(B . "0x8B2B") ; <CJK>
       (?$(GwB(B . "0x8C50") ; <CJK>
       (?$(GwC(B . "0x8D05") ; <CJK>
       (?$(GwD(B . "0x8E59") ; <CJK>
       (?$(GwE(B . "0x8E63") ; <CJK>
       (?$(GwF(B . "0x8E66") ; <CJK>
       (?$(GwG(B . "0x8E64") ; <CJK>
       (?$(GwH(B . "0x8E5F") ; <CJK>
       (?$(GwI(B . "0x8E55") ; <CJK>
       (?$(GwJ(B . "0x8EC0") ; <CJK>
       (?$(GwK(B . "0x8F49") ; <CJK>
       (?$(GwL(B . "0x8F4D") ; <CJK>
       (?$(GwM(B . "0x9087") ; <CJK>
       (?$(GwN(B . "0x9083") ; <CJK>
       (?$(GwO(B . "0x9088") ; <CJK>
       (?$(GwP(B . "0x91AB") ; <CJK>
       (?$(GwQ(B . "0x91AC") ; <CJK>
       (?$(GwR(B . "0x91D0") ; <CJK>
       (?$(GwS(B . "0x9394") ; <CJK>
       (?$(GwT(B . "0x938A") ; <CJK>
       (?$(GwU(B . "0x9396") ; <CJK>
       (?$(GwV(B . "0x93A2") ; <CJK>
       (?$(GwW(B . "0x93B3") ; <CJK>
       (?$(GwX(B . "0x93AE") ; <CJK>
       (?$(GwY(B . "0x93AC") ; <CJK>
       (?$(GwZ(B . "0x93B0") ; <CJK>
       (?$(Gw[(B . "0x9398") ; <CJK>
       (?$(Gw\(B . "0x939A") ; <CJK>
       (?$(Gw](B . "0x9397") ; <CJK>
       (?$(Gw^(B . "0x95D4") ; <CJK>
       (?$(Gw_(B . "0x95D6") ; <CJK>
       (?$(Gw`(B . "0x95D0") ; <CJK>
       (?$(Gwa(B . "0x95D5") ; <CJK>
       (?$(Gwb(B . "0x96E2") ; <CJK>
       (?$(Gwc(B . "0x96DC") ; <CJK>
       (?$(Gwd(B . "0x96D9") ; <CJK>
       (?$(Gwe(B . "0x96DB") ; <CJK>
       (?$(Gwf(B . "0x96DE") ; <CJK>
       (?$(Gwg(B . "0x9724") ; <CJK>
       (?$(Gwh(B . "0x97A3") ; <CJK>
       (?$(Gwi(B . "0x97A6") ; <CJK>
       (?$(Gwj(B . "0x97AD") ; <CJK>
       (?$(Gwk(B . "0x97F9") ; <CJK>
       (?$(Gwl(B . "0x984D") ; <CJK>
       (?$(Gwm(B . "0x984F") ; <CJK>
       (?$(Gwn(B . "0x984C") ; <CJK>
       (?$(Gwo(B . "0x984E") ; <CJK>
       (?$(Gwp(B . "0x9853") ; <CJK>
       (?$(Gwq(B . "0x98BA") ; <CJK>
       (?$(Gwr(B . "0x993E") ; <CJK>
       (?$(Gws(B . "0x993F") ; <CJK>
       (?$(Gwt(B . "0x993D") ; <CJK>
       (?$(Gwu(B . "0x992E") ; <CJK>
       (?$(Gwv(B . "0x99A5") ; <CJK>
       (?$(Gww(B . "0x9A0E") ; <CJK>
       (?$(Gwx(B . "0x9AC1") ; <CJK>
       (?$(Gwy(B . "0x9B03") ; <CJK>
       (?$(Gwz(B . "0x9B06") ; <CJK>
       (?$(Gw{(B . "0x9B4F") ; <CJK>
       (?$(Gw|(B . "0x9B4E") ; <CJK>
       (?$(Gw}(B . "0x9B4D") ; <CJK>
       (?$(Gw~(B . "0x9BCA") ; <CJK>
       (?$(Gx!(B . "0x9BC9") ; <CJK>
       (?$(Gx"(B . "0x9BFD") ; <CJK>
       (?$(Gx#(B . "0x9BC8") ; <CJK>
       (?$(Gx$(B . "0x9BC0") ; <CJK>
       (?$(Gx%(B . "0x9D51") ; <CJK>
       (?$(Gx&(B . "0x9D5D") ; <CJK>
       (?$(Gx'(B . "0x9D60") ; <CJK>
       (?$(Gx((B . "0x9EE0") ; <CJK>
       (?$(Gx)(B . "0x9F15") ; <CJK>
       (?$(Gx*(B . "0x9F2C") ; <CJK>
       (?$(Gx+(B . "0x5133") ; <CJK>
       (?$(Gx,(B . "0x56A5") ; <CJK>
       (?$(Gx-(B . "0x56A8") ; <CJK>
       (?$(Gx.(B . "0x58DE") ; <CJK>
       (?$(Gx/(B . "0x58DF") ; <CJK>
       (?$(Gx0(B . "0x58E2") ; <CJK>
       (?$(Gx1(B . "0x5BF5") ; <CJK>
       (?$(Gx2(B . "0x9F90") ; <CJK>
       (?$(Gx3(B . "0x5EEC") ; <CJK>
       (?$(Gx4(B . "0x61F2") ; <CJK>
       (?$(Gx5(B . "0x61F7") ; <CJK>
       (?$(Gx6(B . "0x61F6") ; <CJK>
       (?$(Gx7(B . "0x61F5") ; <CJK>
       (?$(Gx8(B . "0x6500") ; <CJK>
       (?$(Gx9(B . "0x650F") ; <CJK>
       (?$(Gx:(B . "0x66E0") ; <CJK>
       (?$(Gx;(B . "0x66DD") ; <CJK>
       (?$(Gx<(B . "0x6AE5") ; <CJK>
       (?$(Gx=(B . "0x6ADD") ; <CJK>
       (?$(Gx>(B . "0x6ADA") ; <CJK>
       (?$(Gx?(B . "0x6AD3") ; <CJK>
       (?$(Gx@(B . "0x701B") ; <CJK>
       (?$(GxA(B . "0x701F") ; <CJK>
       (?$(GxB(B . "0x7028") ; <CJK>
       (?$(GxC(B . "0x701A") ; <CJK>
       (?$(GxD(B . "0x701D") ; <CJK>
       (?$(GxE(B . "0x7015") ; <CJK>
       (?$(GxF(B . "0x7018") ; <CJK>
       (?$(GxG(B . "0x7206") ; <CJK>
       (?$(GxH(B . "0x720D") ; <CJK>
       (?$(GxI(B . "0x7258") ; <CJK>
       (?$(GxJ(B . "0x72A2") ; <CJK>
       (?$(GxK(B . "0x7378") ; <CJK>
       (?$(GxL(B . "0x737A") ; <CJK>
       (?$(GxM(B . "0x74BD") ; <CJK>
       (?$(GxN(B . "0x74CA") ; <CJK>
       (?$(GxO(B . "0x74E3") ; <CJK>
       (?$(GxP(B . "0x7587") ; <CJK>
       (?$(GxQ(B . "0x7586") ; <CJK>
       (?$(GxR(B . "0x765F") ; <CJK>
       (?$(GxS(B . "0x7661") ; <CJK>
       (?$(GxT(B . "0x77C7") ; <CJK>
       (?$(GxU(B . "0x7919") ; <CJK>
       (?$(GxV(B . "0x79B1") ; <CJK>
       (?$(GxW(B . "0x7A6B") ; <CJK>
       (?$(GxX(B . "0x7A69") ; <CJK>
       (?$(GxY(B . "0x7C3E") ; <CJK>
       (?$(GxZ(B . "0x7C3F") ; <CJK>
       (?$(Gx[(B . "0x7C38") ; <CJK>
       (?$(Gx\(B . "0x7C3D") ; <CJK>
       (?$(Gx](B . "0x7C37") ; <CJK>
       (?$(Gx^(B . "0x7C40") ; <CJK>
       (?$(Gx_(B . "0x7E6B") ; <CJK>
       (?$(Gx`(B . "0x7E6D") ; <CJK>
       (?$(Gxa(B . "0x7E79") ; <CJK>
       (?$(Gxb(B . "0x7E69") ; <CJK>
       (?$(Gxc(B . "0x7E6A") ; <CJK>
       (?$(Gxd(B . "0x7E73") ; <CJK>
       (?$(Gxe(B . "0x7F85") ; <CJK>
       (?$(Gxf(B . "0x7FB6") ; <CJK>
       (?$(Gxg(B . "0x7FB9") ; <CJK>
       (?$(Gxh(B . "0x7FB8") ; <CJK>
       (?$(Gxi(B . "0x81D8") ; <CJK>
       (?$(Gxj(B . "0x85E9") ; <CJK>
       (?$(Gxk(B . "0x85DD") ; <CJK>
       (?$(Gxl(B . "0x85EA") ; <CJK>
       (?$(Gxm(B . "0x85D5") ; <CJK>
       (?$(Gxn(B . "0x85E4") ; <CJK>
       (?$(Gxo(B . "0x85E5") ; <CJK>
       (?$(Gxp(B . "0x85F7") ; <CJK>
       (?$(Gxq(B . "0x87FB") ; <CJK>
       (?$(Gxr(B . "0x8805") ; <CJK>
       (?$(Gxs(B . "0x880D") ; <CJK>
       (?$(Gxt(B . "0x87F9") ; <CJK>
       (?$(Gxu(B . "0x87FE") ; <CJK>
       (?$(Gxv(B . "0x8960") ; <CJK>
       (?$(Gxw(B . "0x895F") ; <CJK>
       (?$(Gxx(B . "0x8956") ; <CJK>
       (?$(Gxy(B . "0x895E") ; <CJK>
       (?$(Gxz(B . "0x8B41") ; <CJK>
       (?$(Gx{(B . "0x8B5C") ; <CJK>
       (?$(Gx|(B . "0x8B58") ; <CJK>
       (?$(Gx}(B . "0x8B49") ; <CJK>
       (?$(Gx~(B . "0x8B5A") ; <CJK>
       (?$(Gy!(B . "0x8B4E") ; <CJK>
       (?$(Gy"(B . "0x8B4F") ; <CJK>
       (?$(Gy#(B . "0x8B46") ; <CJK>
       (?$(Gy$(B . "0x8B59") ; <CJK>
       (?$(Gy%(B . "0x8D08") ; <CJK>
       (?$(Gy&(B . "0x8D0A") ; <CJK>
       (?$(Gy'(B . "0x8E7C") ; <CJK>
       (?$(Gy((B . "0x8E72") ; <CJK>
       (?$(Gy)(B . "0x8E87") ; <CJK>
       (?$(Gy*(B . "0x8E76") ; <CJK>
       (?$(Gy+(B . "0x8E6C") ; <CJK>
       (?$(Gy,(B . "0x8E7A") ; <CJK>
       (?$(Gy-(B . "0x8E74") ; <CJK>
       (?$(Gy.(B . "0x8F54") ; <CJK>
       (?$(Gy/(B . "0x8F4E") ; <CJK>
       (?$(Gy0(B . "0x8FAD") ; <CJK>
       (?$(Gy1(B . "0x908A") ; <CJK>
       (?$(Gy2(B . "0x908B") ; <CJK>
       (?$(Gy3(B . "0x91B1") ; <CJK>
       (?$(Gy4(B . "0x91AE") ; <CJK>
       (?$(Gy5(B . "0x93E1") ; <CJK>
       (?$(Gy6(B . "0x93D1") ; <CJK>
       (?$(Gy7(B . "0x93DF") ; <CJK>
       (?$(Gy8(B . "0x93C3") ; <CJK>
       (?$(Gy9(B . "0x93C8") ; <CJK>
       (?$(Gy:(B . "0x93DC") ; <CJK>
       (?$(Gy;(B . "0x93DD") ; <CJK>
       (?$(Gy<(B . "0x93D6") ; <CJK>
       (?$(Gy=(B . "0x93E2") ; <CJK>
       (?$(Gy>(B . "0x93CD") ; <CJK>
       (?$(Gy?(B . "0x93D8") ; <CJK>
       (?$(Gy@(B . "0x93E4") ; <CJK>
       (?$(GyA(B . "0x93D7") ; <CJK>
       (?$(GyB(B . "0x93E8") ; <CJK>
       (?$(GyC(B . "0x95DC") ; <CJK>
       (?$(GyD(B . "0x96B4") ; <CJK>
       (?$(GyE(B . "0x96E3") ; <CJK>
       (?$(GyF(B . "0x972A") ; <CJK>
       (?$(GyG(B . "0x9727") ; <CJK>
       (?$(GyH(B . "0x9761") ; <CJK>
       (?$(GyI(B . "0x97DC") ; <CJK>
       (?$(GyJ(B . "0x97FB") ; <CJK>
       (?$(GyK(B . "0x985E") ; <CJK>
       (?$(GyL(B . "0x9858") ; <CJK>
       (?$(GyM(B . "0x985B") ; <CJK>
       (?$(GyN(B . "0x98BC") ; <CJK>
       (?$(GyO(B . "0x9945") ; <CJK>
       (?$(GyP(B . "0x9949") ; <CJK>
       (?$(GyQ(B . "0x9A16") ; <CJK>
       (?$(GyR(B . "0x9A19") ; <CJK>
       (?$(GyS(B . "0x9B0D") ; <CJK>
       (?$(GyT(B . "0x9BE8") ; <CJK>
       (?$(GyU(B . "0x9BE7") ; <CJK>
       (?$(GyV(B . "0x9BD6") ; <CJK>
       (?$(GyW(B . "0x9BDB") ; <CJK>
       (?$(GyX(B . "0x9D89") ; <CJK>
       (?$(GyY(B . "0x9D61") ; <CJK>
       (?$(GyZ(B . "0x9D72") ; <CJK>
       (?$(Gy[(B . "0x9D6A") ; <CJK>
       (?$(Gy\(B . "0x9D6C") ; <CJK>
       (?$(Gy](B . "0x9E92") ; <CJK>
       (?$(Gy^(B . "0x9E97") ; <CJK>
       (?$(Gy_(B . "0x9E93") ; <CJK>
       (?$(Gy`(B . "0x9EB4") ; <CJK>
       (?$(Gya(B . "0x52F8") ; <CJK>
       (?$(Gyb(B . "0x56B7") ; <CJK>
       (?$(Gyc(B . "0x56B6") ; <CJK>
       (?$(Gyd(B . "0x56B4") ; <CJK>
       (?$(Gye(B . "0x56BC") ; <CJK>
       (?$(Gyf(B . "0x58E4") ; <CJK>
       (?$(Gyg(B . "0x5B40") ; <CJK>
       (?$(Gyh(B . "0x5B43") ; <CJK>
       (?$(Gyi(B . "0x5B7D") ; <CJK>
       (?$(Gyj(B . "0x5BF6") ; <CJK>
       (?$(Gyk(B . "0x5DC9") ; <CJK>
       (?$(Gyl(B . "0x61F8") ; <CJK>
       (?$(Gym(B . "0x61FA") ; <CJK>
       (?$(Gyn(B . "0x6518") ; <CJK>
       (?$(Gyo(B . "0x6514") ; <CJK>
       (?$(Gyp(B . "0x6519") ; <CJK>
       (?$(Gyq(B . "0x66E6") ; <CJK>
       (?$(Gyr(B . "0x6727") ; <CJK>
       (?$(Gys(B . "0x6AEC") ; <CJK>
       (?$(Gyt(B . "0x703E") ; <CJK>
       (?$(Gyu(B . "0x7030") ; <CJK>
       (?$(Gyv(B . "0x7032") ; <CJK>
       (?$(Gyw(B . "0x7210") ; <CJK>
       (?$(Gyx(B . "0x737B") ; <CJK>
       (?$(Gyy(B . "0x74CF") ; <CJK>
       (?$(Gyz(B . "0x7662") ; <CJK>
       (?$(Gy{(B . "0x7665") ; <CJK>
       (?$(Gy|(B . "0x7926") ; <CJK>
       (?$(Gy}(B . "0x792A") ; <CJK>
       (?$(Gy~(B . "0x792C") ; <CJK>
       (?$(Gz!(B . "0x792B") ; <CJK>
       (?$(Gz"(B . "0x7AC7") ; <CJK>
       (?$(Gz#(B . "0x7AF6") ; <CJK>
       (?$(Gz$(B . "0x7C4C") ; <CJK>
       (?$(Gz%(B . "0x7C43") ; <CJK>
       (?$(Gz&(B . "0x7C4D") ; <CJK>
       (?$(Gz'(B . "0x7CEF") ; <CJK>
       (?$(Gz((B . "0x7CF0") ; <CJK>
       (?$(Gz)(B . "0x8FAE") ; <CJK>
       (?$(Gz*(B . "0x7E7D") ; <CJK>
       (?$(Gz+(B . "0x7E7C") ; <CJK>
       (?$(Gz,(B . "0x7E82") ; <CJK>
       (?$(Gz-(B . "0x7F4C") ; <CJK>
       (?$(Gz.(B . "0x8000") ; <CJK>
       (?$(Gz/(B . "0x81DA") ; <CJK>
       (?$(Gz0(B . "0x8266") ; <CJK>
       (?$(Gz1(B . "0x85FB") ; <CJK>
       (?$(Gz2(B . "0x85F9") ; <CJK>
       (?$(Gz3(B . "0x8611") ; <CJK>
       (?$(Gz4(B . "0x85FA") ; <CJK>
       (?$(Gz5(B . "0x8606") ; <CJK>
       (?$(Gz6(B . "0x860B") ; <CJK>
       (?$(Gz7(B . "0x8607") ; <CJK>
       (?$(Gz8(B . "0x860A") ; <CJK>
       (?$(Gz9(B . "0x8814") ; <CJK>
       (?$(Gz:(B . "0x8815") ; <CJK>
       (?$(Gz;(B . "0x8964") ; <CJK>
       (?$(Gz<(B . "0x89BA") ; <CJK>
       (?$(Gz=(B . "0x89F8") ; <CJK>
       (?$(Gz>(B . "0x8B70") ; <CJK>
       (?$(Gz?(B . "0x8B6C") ; <CJK>
       (?$(Gz@(B . "0x8B66") ; <CJK>
       (?$(GzA(B . "0x8B6F") ; <CJK>
       (?$(GzB(B . "0x8B5F") ; <CJK>
       (?$(GzC(B . "0x8B6B") ; <CJK>
       (?$(GzD(B . "0x8D0F") ; <CJK>
       (?$(GzE(B . "0x8D0D") ; <CJK>
       (?$(GzF(B . "0x8E89") ; <CJK>
       (?$(GzG(B . "0x8E81") ; <CJK>
       (?$(GzH(B . "0x8E85") ; <CJK>
       (?$(GzI(B . "0x8E82") ; <CJK>
       (?$(GzJ(B . "0x91B4") ; <CJK>
       (?$(GzK(B . "0x91CB") ; <CJK>
       (?$(GzL(B . "0x9418") ; <CJK>
       (?$(GzM(B . "0x9403") ; <CJK>
       (?$(GzN(B . "0x93FD") ; <CJK>
       (?$(GzO(B . "0x95E1") ; <CJK>
       (?$(GzP(B . "0x9730") ; <CJK>
       (?$(GzQ(B . "0x98C4") ; <CJK>
       (?$(GzR(B . "0x9952") ; <CJK>
       (?$(GzS(B . "0x9951") ; <CJK>
       (?$(GzT(B . "0x99A8") ; <CJK>
       (?$(GzU(B . "0x9A2B") ; <CJK>
       (?$(GzV(B . "0x9A30") ; <CJK>
       (?$(GzW(B . "0x9A37") ; <CJK>
       (?$(GzX(B . "0x9A35") ; <CJK>
       (?$(GzY(B . "0x9C13") ; <CJK>
       (?$(GzZ(B . "0x9C0D") ; <CJK>
       (?$(Gz[(B . "0x9E79") ; <CJK>
       (?$(Gz\(B . "0x9EB5") ; <CJK>
       (?$(Gz](B . "0x9EE8") ; <CJK>
       (?$(Gz^(B . "0x9F2F") ; <CJK>
       (?$(Gz_(B . "0x9F5F") ; <CJK>
       (?$(Gz`(B . "0x9F63") ; <CJK>
       (?$(Gza(B . "0x9F61") ; <CJK>
       (?$(Gzb(B . "0x5137") ; <CJK>
       (?$(Gzc(B . "0x5138") ; <CJK>
       (?$(Gzd(B . "0x56C1") ; <CJK>
       (?$(Gze(B . "0x56C0") ; <CJK>
       (?$(Gzf(B . "0x56C2") ; <CJK>
       (?$(Gzg(B . "0x5914") ; <CJK>
       (?$(Gzh(B . "0x5C6C") ; <CJK>
       (?$(Gzi(B . "0x5DCD") ; <CJK>
       (?$(Gzj(B . "0x61FC") ; <CJK>
       (?$(Gzk(B . "0x61FE") ; <CJK>
       (?$(Gzl(B . "0x651D") ; <CJK>
       (?$(Gzm(B . "0x651C") ; <CJK>
       (?$(Gzn(B . "0x6595") ; <CJK>
       (?$(Gzo(B . "0x66E9") ; <CJK>
       (?$(Gzp(B . "0x6AFB") ; <CJK>
       (?$(Gzq(B . "0x6B04") ; <CJK>
       (?$(Gzr(B . "0x6AFA") ; <CJK>
       (?$(Gzs(B . "0x6BB2") ; <CJK>
       (?$(Gzt(B . "0x704C") ; <CJK>
       (?$(Gzu(B . "0x721B") ; <CJK>
       (?$(Gzv(B . "0x72A7") ; <CJK>
       (?$(Gzw(B . "0x74D6") ; <CJK>
       (?$(Gzx(B . "0x74D4") ; <CJK>
       (?$(Gzy(B . "0x7669") ; <CJK>
       (?$(Gzz(B . "0x77D3") ; <CJK>
       (?$(Gz{(B . "0x7C50") ; <CJK>
       (?$(Gz|(B . "0x7E8F") ; <CJK>
       (?$(Gz}(B . "0x7E8C") ; <CJK>
       (?$(Gz~(B . "0x7FBC") ; <CJK>
       (?$(G{!(B . "0x8617") ; <CJK>
       (?$(G{"(B . "0x862D") ; <CJK>
       (?$(G{#(B . "0x861A") ; <CJK>
       (?$(G{$(B . "0x8823") ; <CJK>
       (?$(G{%(B . "0x8822") ; <CJK>
       (?$(G{&(B . "0x8821") ; <CJK>
       (?$(G{'(B . "0x881F") ; <CJK>
       (?$(G{((B . "0x896A") ; <CJK>
       (?$(G{)(B . "0x896C") ; <CJK>
       (?$(G{*(B . "0x89BD") ; <CJK>
       (?$(G{+(B . "0x8B74") ; <CJK>
       (?$(G{,(B . "0x8B77") ; <CJK>
       (?$(G{-(B . "0x8B7D") ; <CJK>
       (?$(G{.(B . "0x8D13") ; <CJK>
       (?$(G{/(B . "0x8E8A") ; <CJK>
       (?$(G{0(B . "0x8E8D") ; <CJK>
       (?$(G{1(B . "0x8E8B") ; <CJK>
       (?$(G{2(B . "0x8F5F") ; <CJK>
       (?$(G{3(B . "0x8FAF") ; <CJK>
       (?$(G{4(B . "0x91BA") ; <CJK>
       (?$(G{5(B . "0x942E") ; <CJK>
       (?$(G{6(B . "0x9433") ; <CJK>
       (?$(G{7(B . "0x9435") ; <CJK>
       (?$(G{8(B . "0x943A") ; <CJK>
       (?$(G{9(B . "0x9438") ; <CJK>
       (?$(G{:(B . "0x9432") ; <CJK>
       (?$(G{;(B . "0x942B") ; <CJK>
       (?$(G{<(B . "0x95E2") ; <CJK>
       (?$(G{=(B . "0x9738") ; <CJK>
       (?$(G{>(B . "0x9739") ; <CJK>
       (?$(G{?(B . "0x9732") ; <CJK>
       (?$(G{@(B . "0x97FF") ; <CJK>
       (?$(G{A(B . "0x9867") ; <CJK>
       (?$(G{B(B . "0x9865") ; <CJK>
       (?$(G{C(B . "0x9957") ; <CJK>
       (?$(G{D(B . "0x9A45") ; <CJK>
       (?$(G{E(B . "0x9A43") ; <CJK>
       (?$(G{F(B . "0x9A40") ; <CJK>
       (?$(G{G(B . "0x9A3E") ; <CJK>
       (?$(G{H(B . "0x9ACF") ; <CJK>
       (?$(G{I(B . "0x9B54") ; <CJK>
       (?$(G{J(B . "0x9B51") ; <CJK>
       (?$(G{K(B . "0x9C2D") ; <CJK>
       (?$(G{L(B . "0x9C25") ; <CJK>
       (?$(G{M(B . "0x9DAF") ; <CJK>
       (?$(G{N(B . "0x9DB4") ; <CJK>
       (?$(G{O(B . "0x9DC2") ; <CJK>
       (?$(G{P(B . "0x9DB8") ; <CJK>
       (?$(G{Q(B . "0x9E9D") ; <CJK>
       (?$(G{R(B . "0x9EEF") ; <CJK>
       (?$(G{S(B . "0x9F19") ; <CJK>
       (?$(G{T(B . "0x9F5C") ; <CJK>
       (?$(G{U(B . "0x9F66") ; <CJK>
       (?$(G{V(B . "0x9F67") ; <CJK>
       (?$(G{W(B . "0x513C") ; <CJK>
       (?$(G{X(B . "0x513B") ; <CJK>
       (?$(G{Y(B . "0x56C8") ; <CJK>
       (?$(G{Z(B . "0x56CA") ; <CJK>
       (?$(G{[(B . "0x56C9") ; <CJK>
       (?$(G{\(B . "0x5B7F") ; <CJK>
       (?$(G{](B . "0x5DD4") ; <CJK>
       (?$(G{^(B . "0x5DD2") ; <CJK>
       (?$(G{_(B . "0x5F4E") ; <CJK>
       (?$(G{`(B . "0x61FF") ; <CJK>
       (?$(G{a(B . "0x6524") ; <CJK>
       (?$(G{b(B . "0x6B0A") ; <CJK>
       (?$(G{c(B . "0x6B61") ; <CJK>
       (?$(G{d(B . "0x7051") ; <CJK>
       (?$(G{e(B . "0x7058") ; <CJK>
       (?$(G{f(B . "0x7380") ; <CJK>
       (?$(G{g(B . "0x74E4") ; <CJK>
       (?$(G{h(B . "0x758A") ; <CJK>
       (?$(G{i(B . "0x766E") ; <CJK>
       (?$(G{j(B . "0x766C") ; <CJK>
       (?$(G{k(B . "0x79B3") ; <CJK>
       (?$(G{l(B . "0x7C60") ; <CJK>
       (?$(G{m(B . "0x7C5F") ; <CJK>
       (?$(G{n(B . "0x807E") ; <CJK>
       (?$(G{o(B . "0x807D") ; <CJK>
       (?$(G{p(B . "0x81DF") ; <CJK>
       (?$(G{q(B . "0x8972") ; <CJK>
       (?$(G{r(B . "0x896F") ; <CJK>
       (?$(G{s(B . "0x89FC") ; <CJK>
       (?$(G{t(B . "0x8B80") ; <CJK>
       (?$(G{u(B . "0x8D16") ; <CJK>
       (?$(G{v(B . "0x8D17") ; <CJK>
       (?$(G{w(B . "0x8E91") ; <CJK>
       (?$(G{x(B . "0x8E93") ; <CJK>
       (?$(G{y(B . "0x8F61") ; <CJK>
       (?$(G{z(B . "0x9148") ; <CJK>
       (?$(G{{(B . "0x9444") ; <CJK>
       (?$(G{|(B . "0x9451") ; <CJK>
       (?$(G{}(B . "0x9452") ; <CJK>
       (?$(G{~(B . "0x973D") ; <CJK>
       (?$(G|!(B . "0x973E") ; <CJK>
       (?$(G|"(B . "0x97C3") ; <CJK>
       (?$(G|#(B . "0x97C1") ; <CJK>
       (?$(G|$(B . "0x986B") ; <CJK>
       (?$(G|%(B . "0x9955") ; <CJK>
       (?$(G|&(B . "0x9A55") ; <CJK>
       (?$(G|'(B . "0x9A4D") ; <CJK>
       (?$(G|((B . "0x9AD2") ; <CJK>
       (?$(G|)(B . "0x9B1A") ; <CJK>
       (?$(G|*(B . "0x9C49") ; <CJK>
       (?$(G|+(B . "0x9C31") ; <CJK>
       (?$(G|,(B . "0x9C3E") ; <CJK>
       (?$(G|-(B . "0x9C3B") ; <CJK>
       (?$(G|.(B . "0x9DD3") ; <CJK>
       (?$(G|/(B . "0x9DD7") ; <CJK>
       (?$(G|0(B . "0x9F34") ; <CJK>
       (?$(G|1(B . "0x9F6C") ; <CJK>
       (?$(G|2(B . "0x9F6A") ; <CJK>
       (?$(G|3(B . "0x9F94") ; <CJK>
       (?$(G|4(B . "0x56CC") ; <CJK>
       (?$(G|5(B . "0x5DD6") ; <CJK>
       (?$(G|6(B . "0x6200") ; <CJK>
       (?$(G|7(B . "0x6523") ; <CJK>
       (?$(G|8(B . "0x652B") ; <CJK>
       (?$(G|9(B . "0x652A") ; <CJK>
       (?$(G|:(B . "0x66EC") ; <CJK>
       (?$(G|;(B . "0x6B10") ; <CJK>
       (?$(G|<(B . "0x74DA") ; <CJK>
       (?$(G|=(B . "0x7ACA") ; <CJK>
       (?$(G|>(B . "0x7C64") ; <CJK>
       (?$(G|?(B . "0x7C63") ; <CJK>
       (?$(G|@(B . "0x7C65") ; <CJK>
       (?$(G|A(B . "0x7E93") ; <CJK>
       (?$(G|B(B . "0x7E96") ; <CJK>
       (?$(G|C(B . "0x7E94") ; <CJK>
       (?$(G|D(B . "0x81E2") ; <CJK>
       (?$(G|E(B . "0x8638") ; <CJK>
       (?$(G|F(B . "0x863F") ; <CJK>
       (?$(G|G(B . "0x8831") ; <CJK>
       (?$(G|H(B . "0x8B8A") ; <CJK>
       (?$(G|I(B . "0x9090") ; <CJK>
       (?$(G|J(B . "0x908F") ; <CJK>
       (?$(G|K(B . "0x9463") ; <CJK>
       (?$(G|L(B . "0x9460") ; <CJK>
       (?$(G|M(B . "0x9464") ; <CJK>
       (?$(G|N(B . "0x9768") ; <CJK>
       (?$(G|O(B . "0x986F") ; <CJK>
       (?$(G|P(B . "0x995C") ; <CJK>
       (?$(G|Q(B . "0x9A5A") ; <CJK>
       (?$(G|R(B . "0x9A5B") ; <CJK>
       (?$(G|S(B . "0x9A57") ; <CJK>
       (?$(G|T(B . "0x9AD3") ; <CJK>
       (?$(G|U(B . "0x9AD4") ; <CJK>
       (?$(G|V(B . "0x9AD1") ; <CJK>
       (?$(G|W(B . "0x9C54") ; <CJK>
       (?$(G|X(B . "0x9C57") ; <CJK>
       (?$(G|Y(B . "0x9C56") ; <CJK>
       (?$(G|Z(B . "0x9DE5") ; <CJK>
       (?$(G|[(B . "0x9E9F") ; <CJK>
       (?$(G|\(B . "0x9EF4") ; <CJK>
       (?$(G|](B . "0x56D1") ; <CJK>
       (?$(G|^(B . "0x58E9") ; <CJK>
       (?$(G|_(B . "0x652C") ; <CJK>
       (?$(G|`(B . "0x705E") ; <CJK>
       (?$(G|a(B . "0x7671") ; <CJK>
       (?$(G|b(B . "0x7672") ; <CJK>
       (?$(G|c(B . "0x77D7") ; <CJK>
       (?$(G|d(B . "0x7F50") ; <CJK>
       (?$(G|e(B . "0x7F88") ; <CJK>
       (?$(G|f(B . "0x8836") ; <CJK>
       (?$(G|g(B . "0x8839") ; <CJK>
       (?$(G|h(B . "0x8862") ; <CJK>
       (?$(G|i(B . "0x8B93") ; <CJK>
       (?$(G|j(B . "0x8B92") ; <CJK>
       (?$(G|k(B . "0x8B96") ; <CJK>
       (?$(G|l(B . "0x8277") ; <CJK>
       (?$(G|m(B . "0x8D1B") ; <CJK>
       (?$(G|n(B . "0x91C0") ; <CJK>
       (?$(G|o(B . "0x946A") ; <CJK>
       (?$(G|p(B . "0x9742") ; <CJK>
       (?$(G|q(B . "0x9748") ; <CJK>
       (?$(G|r(B . "0x9744") ; <CJK>
       (?$(G|s(B . "0x97C6") ; <CJK>
       (?$(G|t(B . "0x9870") ; <CJK>
       (?$(G|u(B . "0x9A5F") ; <CJK>
       (?$(G|v(B . "0x9B22") ; <CJK>
       (?$(G|w(B . "0x9B58") ; <CJK>
       (?$(G|x(B . "0x9C5F") ; <CJK>
       (?$(G|y(B . "0x9DF9") ; <CJK>
       (?$(G|z(B . "0x9DFA") ; <CJK>
       (?$(G|{(B . "0x9E7C") ; <CJK>
       (?$(G||(B . "0x9E7D") ; <CJK>
       (?$(G|}(B . "0x9F07") ; <CJK>
       (?$(G|~(B . "0x9F77") ; <CJK>
       (?$(G}!(B . "0x9F72") ; <CJK>
       (?$(G}"(B . "0x5EF3") ; <CJK>
       (?$(G}#(B . "0x6B16") ; <CJK>
       (?$(G}$(B . "0x7063") ; <CJK>
       (?$(G}%(B . "0x7C6C") ; <CJK>
       (?$(G}&(B . "0x7C6E") ; <CJK>
       (?$(G}'(B . "0x883B") ; <CJK>
       (?$(G}((B . "0x89C0") ; <CJK>
       (?$(G})(B . "0x8EA1") ; <CJK>
       (?$(G}*(B . "0x91C1") ; <CJK>
       (?$(G}+(B . "0x9472") ; <CJK>
       (?$(G},(B . "0x9470") ; <CJK>
       (?$(G}-(B . "0x9871") ; <CJK>
       (?$(G}.(B . "0x995E") ; <CJK>
       (?$(G}/(B . "0x9AD6") ; <CJK>
       (?$(G}0(B . "0x9B23") ; <CJK>
       (?$(G}1(B . "0x9ECC") ; <CJK>
       (?$(G}2(B . "0x7064") ; <CJK>
       (?$(G}3(B . "0x77DA") ; <CJK>
       (?$(G}4(B . "0x8B9A") ; <CJK>
       (?$(G}5(B . "0x9477") ; <CJK>
       (?$(G}6(B . "0x97C9") ; <CJK>
       (?$(G}7(B . "0x9A62") ; <CJK>
       (?$(G}8(B . "0x9A65") ; <CJK>
       (?$(G}9(B . "0x7E9C") ; <CJK>
       (?$(G}:(B . "0x8B9C") ; <CJK>
       (?$(G};(B . "0x8EAA") ; <CJK>
       (?$(G}<(B . "0x91C5") ; <CJK>
       (?$(G}=(B . "0x947D") ; <CJK>
       (?$(G}>(B . "0x947E") ; <CJK>
       (?$(G}?(B . "0x947C") ; <CJK>
       (?$(G}@(B . "0x9C77") ; <CJK>
       (?$(G}A(B . "0x9C78") ; <CJK>
       (?$(G}B(B . "0x9EF7") ; <CJK>
       (?$(G}C(B . "0x8C54") ; <CJK>
       (?$(G}D(B . "0x947F") ; <CJK>
       (?$(G}E(B . "0x9E1A") ; <CJK>
       (?$(G}F(B . "0x7228") ; <CJK>
       (?$(G}G(B . "0x9A6A") ; <CJK>
       (?$(G}H(B . "0x9B31") ; <CJK>
       (?$(G}I(B . "0x9E1B") ; <CJK>
       (?$(G}J(B . "0x9E1E") ; <CJK>
       (?$(G}K(B . "0x7C72") ; <CJK>
       ))))

(provide 'u-cns-1)

;;; u-cns-1.el ends here

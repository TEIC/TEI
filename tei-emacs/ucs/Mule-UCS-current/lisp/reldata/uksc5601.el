; -*- coding: iso-2022-7bit  -*-
;;; uksc5601.el --- tables between UCS and KS C 5601-1987

;; Author: Lori Hoerth <lorih@microsoft.com>
;;         K.D.Chang   <a-kchang@microsoft.com>

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, KS C 5601

;; This file is part of Mule-UCS.

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

;;; Commentary:

;; This file is converted from

;;	ftp://ftp.unicode.org/Public/MAPPINGS/EASTASIA/KSC/KSC5601.TXT

;; by MORIOKA Tomohiko <morioka@jaist.ac.jp>.

;;; Code:

(put 'korean-ksc5601 'unicode-assoc
     'ks-c-5601-1987-vs-unicode-assoc)

(defvar
  ks-c-5601-1987-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?　 . "0x3000") ; IDEOGRAPHIC SPACE
       (?、 . "0x3001") ; IDEOGRAPHIC COMMA
       (?。 . "0x3002") ; IDEOGRAPHIC FULL STOP
       (?· . "0x00B7") ; MIDDLE DOT
       (?‥ . "0x2025") ; TWO DOT LEADER
       (?… . "0x2026") ; HORIZONTAL ELLIPSIS
       (?¨ . "0x00A8") ; DIAERESIS
       (?〃 . "0x3003") ; DITTO MARK
       (?­ . "0x00AD") ; SOFT HYPHEN
       (?― . "0x2015") ; HORIZONTAL BAR
       (?∥ . "0x2225") ; PARALLEL TO
       (?＼ . "0xFF3C") ; FULLWIDTH REVERSE SOLIDUS
       (?∼ . "0x223C") ; TILDE OPERATOR
       (?‘ . "0x2018") ; LEFT SINGLE QUOTATION MARK
       (?’ . "0x2019") ; RIGHT SINGLE QUOTATION MARK
       (?“ . "0x201C") ; LEFT DOUBLE QUOTATION MARK
       (?” . "0x201D") ; RIGHT DOUBLE QUOTATION MARK
       (?〔 . "0x3014") ; LEFT TORTOISE SHELL BRACKET
       (?〕 . "0x3015") ; RIGHT TORTOISE SHELL BRACKET
       (?〈 . "0x3008") ; LEFT ANGLE BRACKET
       (?〉 . "0x3009") ; RIGHT ANGLE BRACKET
       (?《 . "0x300A") ; LEFT DOUBLE ANGLE BRACKET
       (?》 . "0x300B") ; RIGHT DOUBLE ANGLE BRACKET
       (?「 . "0x300C") ; LEFT CORNER BRACKET
       (?」 . "0x300D") ; RIGHT CORNER BRACKET
       (?『 . "0x300E") ; LEFT WHITE CORNER BRACKET
       (?』 . "0x300F") ; RIGHT WHITE CORNER BRACKET
       (?【 . "0x3010") ; LEFT BLACK LENTICULAR BRACKET
       (?】 . "0x3011") ; RIGHT BLACK LENTICULAR BRACKET
       (?± . "0x00B1") ; PLUS-MINUS SIGN
       (?× . "0x00D7") ; MULTIPLICATION SIGN
       (?÷ . "0x00F7") ; DIVISION SIGN
       (?≠ . "0x2260") ; NOT EQUAL TO
       (?≤ . "0x2264") ; LESS-THAN OR EQUAL TO
       (?≥ . "0x2265") ; GREATER-THAN OR EQUAL TO
       (?∞ . "0x221E") ; INFINITY
       (?∴ . "0x2234") ; THEREFORE
       (?° . "0x00B0") ; DEGREE SIGN
       (?′ . "0x2032") ; PRIME
       (?″ . "0x2033") ; DOUBLE PRIME
       (?℃ . "0x2103") ; DEGREE CELSIUS
       (?Å . "0x212B") ; ANGSTROM SIGN
       (?￠ . "0xFFE0") ; FULLWIDTH CENT SIGN
       (?￡ . "0xFFE1") ; FULLWIDTH POUND SIGN
       (?￥ . "0xFFE5") ; FULLWIDTH YEN SIGN
       (?♂ . "0x2642") ; MALE SIGN
       (?♀ . "0x2640") ; FEMALE SIGN
       (?∠ . "0x2220") ; ANGLE
       (?⊥ . "0x22A5") ; UP TACK
       (?⌒ . "0x2312") ; ARC
       (?∂ . "0x2202") ; PARTIAL DIFFERENTIAL
       (?∇ . "0x2207") ; NABLA
       (?≡ . "0x2261") ; IDENTICAL TO
       (?≒ . "0x2252") ; APPROXIMATELY EQUAL TO OR THE IMAGE OF
       (?§ . "0x00A7") ; SECTION SIGN
       (?※ . "0x203B") ; REFERENCE MARK
       (?☆ . "0x2606") ; WHITE STAR
       (?★ . "0x2605") ; BLACK STAR
       (?○ . "0x25CB") ; WHITE CIRCLE
       (?● . "0x25CF") ; BLACK CIRCLE
       (?◎ . "0x25CE") ; BULLSEYE
       (?◇ . "0x25C7") ; WHITE DIAMOND
       (?◆ . "0x25C6") ; BLACK DIAMOND
       (?□ . "0x25A1") ; WHITE SQUARE
       (?■ . "0x25A0") ; BLACK SQUARE
       (?△ . "0x25B3") ; WHITE UP-POINTING TRIANGLE
       (?▲ . "0x25B2") ; BLACK UP-POINTING TRIANGLE
       (?▽ . "0x25BD") ; WHITE DOWN-POINTING TRIANGLE
       (?▼ . "0x25BC") ; BLACK DOWN-POINTING TRIANGLE
       (?→ . "0x2192") ; RIGHTWARDS ARROW
       (?← . "0x2190") ; LEFTWARDS ARROW
       (?↑ . "0x2191") ; UPWARDS ARROW
       (?↓ . "0x2193") ; DOWNWARDS ARROW
       (?↔ . "0x2194") ; LEFT RIGHT ARROW
       (?〓 . "0x3013") ; GETA MARK
       (?≪ . "0x226A") ; MUCH LESS-THAN
       (?≫ . "0x226B") ; MUCH GREATER-THAN
       (?√ . "0x221A") ; SQUARE ROOT
       (?∽ . "0x223D") ; REVERSED TILDE
       (?∝ . "0x221D") ; PROPORTIONAL TO
       (?∵ . "0x2235") ; BECAUSE
       (?∫ . "0x222B") ; INTEGRAL
       (?∬ . "0x222C") ; DOUBLE INTEGRAL
       (?∈ . "0x2208") ; ELEMENT OF
       (?∋ . "0x220B") ; CONTAINS AS MEMBER
       (?⊆ . "0x2286") ; SUBSET OF OR EQUAL TO
       (?⊇ . "0x2287") ; SUPERSET OF OR EQUAL TO
       (?⊂ . "0x2282") ; SUBSET OF
       (?⊃ . "0x2283") ; SUPERSET OF
       (?∪ . "0x222A") ; UNION
       (?∩ . "0x2229") ; INTERSECTION
       (?∧ . "0x2227") ; LOGICAL AND
       (?∨ . "0x2228") ; LOGICAL OR
       (?￢ . "0xFFE2") ; FULLWIDTH NOT SIGN
       (?⇒ . "0x21D2") ; RIGHTWARDS DOUBLE ARROW
       (?⇔ . "0x21D4") ; LEFT RIGHT DOUBLE ARROW
       (?∀ . "0x2200") ; FOR ALL
       (?∃ . "0x2203") ; THERE EXISTS
       (?´ . "0x00B4") ; ACUTE ACCENT
       (?～ . "0xFF5E") ; FULLWIDTH TILDE
       (?ˇ . "0x02C7") ; CARON
       (?˘ . "0x02D8") ; BREVE
       (?˝ . "0x02DD") ; DOUBLE ACUTE ACCENT
       (?˚ . "0x02DA") ; RING ABOVE
       (?˙ . "0x02D9") ; DOT ABOVE
       (?¸ . "0x00B8") ; CEDILLA
       (?˛ . "0x02DB") ; OGONEK
       (?¡ . "0x00A1") ; INVERTED EXCLAMATION MARK
       (?¿ . "0x00BF") ; INVERTED QUESTION MARK
       (?ː . "0x02D0") ; MODIFIER LETTER TRIANGULAR COLON
       (?∮ . "0x222E") ; CONTOUR INTEGRAL
       (?∑ . "0x2211") ; N-ARY SUMMATION
       (?∏ . "0x220F") ; N-ARY PRODUCT
       (?¤ . "0x00A4") ; CURRENCY SIGN
       (?℉ . "0x2109") ; DEGREE FAHRENHEIT
       (?‰ . "0x2030") ; PER MILLE SIGN
       (?◁ . "0x25C1") ; WHITE LEFT-POINTING TRIANGLE
       (?◀ . "0x25C0") ; BLACK LEFT-POINTING TRIANGLE
       (?▷ . "0x25B7") ; WHITE RIGHT-POINTING TRIANGLE
       (?▶ . "0x25B6") ; BLACK RIGHT-POINTING TRIANGLE
       (?♤ . "0x2664") ; WHITE SPADE SUIT
       (?♠ . "0x2660") ; BLACK SPADE SUIT
       (?♡ . "0x2661") ; WHITE HEART SUIT
       (?♥ . "0x2665") ; BLACK HEART SUIT
       (?♧ . "0x2667") ; WHITE CLUB SUIT
       (?♣ . "0x2663") ; BLACK CLUB SUIT
       (?⊙ . "0x2299") ; CIRCLED DOT OPERATOR
       (?◈ . "0x25C8") ; WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
       (?▣ . "0x25A3") ; WHITE SQUARE CONTAINING BLACK SMALL SQUARE
       (?◐ . "0x25D0") ; CIRCLE WITH LEFT HALF BLACK
       (?◑ . "0x25D1") ; CIRCLE WITH RIGHT HALF BLACK
       (?▒ . "0x2592") ; MEDIUM SHADE
       (?▤ . "0x25A4") ; SQUARE WITH HORIZONTAL FILL
       (?▥ . "0x25A5") ; SQUARE WITH VERTICAL FILL
       (?▨ . "0x25A8") ; SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
       (?▧ . "0x25A7") ; SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
       (?▦ . "0x25A6") ; SQUARE WITH ORTHOGONAL CROSSHATCH FILL
       (?▩ . "0x25A9") ; SQUARE WITH DIAGONAL CROSSHATCH FILL
       (?♨ . "0x2668") ; HOT SPRINGS
       (?☏ . "0x260F") ; WHITE TELEPHONE
       (?☎ . "0x260E") ; BLACK TELEPHONE
       (?☜ . "0x261C") ; WHITE LEFT POINTING INDEX
       (?☞ . "0x261E") ; WHITE RIGHT POINTING INDEX
       (?¶ . "0x00B6") ; PILCROW SIGN
       (?† . "0x2020") ; DAGGER
       (?‡ . "0x2021") ; DOUBLE DAGGER
       (?↕ . "0x2195") ; UP DOWN ARROW
       (?↗ . "0x2197") ; NORTH EAST ARROW
       (?↙ . "0x2199") ; SOUTH WEST ARROW
       (?↖ . "0x2196") ; NORTH WEST ARROW
       (?↘ . "0x2198") ; SOUTH EAST ARROW
       (?♭ . "0x266D") ; MUSIC FLAT SIGN
       (?♩ . "0x2669") ; QUARTER NOTE
       (?♪ . "0x266A") ; EIGHTH NOTE
       (?♬ . "0x266C") ; BEAMED SIXTEENTH NOTES
       (?㉿ . "0x327F") ; KOREAN STANDARD SYMBOL
       (?㈜ . "0x321C") ; PARENTHESIZED HANGUL CIEUC U
       (?№ . "0x2116") ; NUMERO SIGN
       (?㏇ . "0x33C7") ; SQUARE CO
       (?™ . "0x2122") ; TRADE MARK SIGN
       (?㏂ . "0x33C2") ; SQUARE AM
       (?㏘ . "0x33D8") ; SQUARE PM
       (?℡ . "0x2121") ; TELEPHONE SIGN
       (?！ . "0xFF01") ; FULLWIDTH EXCLAMATION MARK
       (?＂ . "0xFF02") ; FULLWIDTH QUOTATION MARK
       (?＃ . "0xFF03") ; FULLWIDTH NUMBER SIGN
       (?＄ . "0xFF04") ; FULLWIDTH DOLLAR SIGN
       (?％ . "0xFF05") ; FULLWIDTH PERCENT SIGN
       (?＆ . "0xFF06") ; FULLWIDTH AMPERSAND
       (?＇ . "0xFF07") ; FULLWIDTH APOSTROPHE
       (?（ . "0xFF08") ; FULLWIDTH LEFT PARENTHESIS
       (?） . "0xFF09") ; FULLWIDTH RIGHT PARENTHESIS
       (?＊ . "0xFF0A") ; FULLWIDTH ASTERISK
       (?＋ . "0xFF0B") ; FULLWIDTH PLUS SIGN
       (?， . "0xFF0C") ; FULLWIDTH COMMA
       (?－ . "0xFF0D") ; FULLWIDTH HYPHEN-MINUS
       (?． . "0xFF0E") ; FULLWIDTH FULL STOP
       (?／ . "0xFF0F") ; FULLWIDTH SOLIDUS
       (?０ . "0xFF10") ; FULLWIDTH DIGIT ZERO
       (?１ . "0xFF11") ; FULLWIDTH DIGIT ONE
       (?２ . "0xFF12") ; FULLWIDTH DIGIT TWO
       (?３ . "0xFF13") ; FULLWIDTH DIGIT THREE
       (?４ . "0xFF14") ; FULLWIDTH DIGIT FOUR
       (?５ . "0xFF15") ; FULLWIDTH DIGIT FIVE
       (?６ . "0xFF16") ; FULLWIDTH DIGIT SIX
       (?７ . "0xFF17") ; FULLWIDTH DIGIT SEVEN
       (?８ . "0xFF18") ; FULLWIDTH DIGIT EIGHT
       (?９ . "0xFF19") ; FULLWIDTH DIGIT NINE
       (?： . "0xFF1A") ; FULLWIDTH COLON
       (?； . "0xFF1B") ; FULLWIDTH SEMICOLON
       (?＜ . "0xFF1C") ; FULLWIDTH LESS-THAN SIGN
       (?＝ . "0xFF1D") ; FULLWIDTH EQUALS SIGN
       (?＞ . "0xFF1E") ; FULLWIDTH GREATER-THAN SIGN
       (?？ . "0xFF1F") ; FULLWIDTH QUESTION MARK
       (?＠ . "0xFF20") ; FULLWIDTH COMMERCIAL AT
       (?Ａ . "0xFF21") ; FULLWIDTH LATIN CAPITAL LETTER A
       (?Ｂ . "0xFF22") ; FULLWIDTH LATIN CAPITAL LETTER B
       (?Ｃ . "0xFF23") ; FULLWIDTH LATIN CAPITAL LETTER C
       (?Ｄ . "0xFF24") ; FULLWIDTH LATIN CAPITAL LETTER D
       (?Ｅ . "0xFF25") ; FULLWIDTH LATIN CAPITAL LETTER E
       (?Ｆ . "0xFF26") ; FULLWIDTH LATIN CAPITAL LETTER F
       (?Ｇ . "0xFF27") ; FULLWIDTH LATIN CAPITAL LETTER G
       (?Ｈ . "0xFF28") ; FULLWIDTH LATIN CAPITAL LETTER H
       (?Ｉ . "0xFF29") ; FULLWIDTH LATIN CAPITAL LETTER I
       (?Ｊ . "0xFF2A") ; FULLWIDTH LATIN CAPITAL LETTER J
       (?Ｋ . "0xFF2B") ; FULLWIDTH LATIN CAPITAL LETTER K
       (?Ｌ . "0xFF2C") ; FULLWIDTH LATIN CAPITAL LETTER L
       (?Ｍ . "0xFF2D") ; FULLWIDTH LATIN CAPITAL LETTER M
       (?Ｎ . "0xFF2E") ; FULLWIDTH LATIN CAPITAL LETTER N
       (?Ｏ . "0xFF2F") ; FULLWIDTH LATIN CAPITAL LETTER O
       (?Ｐ . "0xFF30") ; FULLWIDTH LATIN CAPITAL LETTER P
       (?Ｑ . "0xFF31") ; FULLWIDTH LATIN CAPITAL LETTER Q
       (?Ｒ . "0xFF32") ; FULLWIDTH LATIN CAPITAL LETTER R
       (?Ｓ . "0xFF33") ; FULLWIDTH LATIN CAPITAL LETTER S
       (?Ｔ . "0xFF34") ; FULLWIDTH LATIN CAPITAL LETTER T
       (?Ｕ . "0xFF35") ; FULLWIDTH LATIN CAPITAL LETTER U
       (?Ｖ . "0xFF36") ; FULLWIDTH LATIN CAPITAL LETTER V
       (?Ｗ . "0xFF37") ; FULLWIDTH LATIN CAPITAL LETTER W
       (?Ｘ . "0xFF38") ; FULLWIDTH LATIN CAPITAL LETTER X
       (?Ｙ . "0xFF39") ; FULLWIDTH LATIN CAPITAL LETTER Y
       (?Ｚ . "0xFF3A") ; FULLWIDTH LATIN CAPITAL LETTER Z
       (?［ . "0xFF3B") ; FULLWIDTH LEFT SQUARE BRACKET
       (?￦ . "0xFFE6") ; FULLWIDTH WON SIGN
       (?］ . "0xFF3D") ; FULLWIDTH RIGHT SQUARE BRACKET
       (?＾ . "0xFF3E") ; FULLWIDTH CIRCUMFLEX ACCENT
       (?＿ . "0xFF3F") ; FULLWIDTH LOW LINE
       (?｀ . "0xFF40") ; FULLWIDTH GRAVE ACCENT
       (?ａ . "0xFF41") ; FULLWIDTH LATIN SMALL LETTER A
       (?ｂ . "0xFF42") ; FULLWIDTH LATIN SMALL LETTER B
       (?ｃ . "0xFF43") ; FULLWIDTH LATIN SMALL LETTER C
       (?ｄ . "0xFF44") ; FULLWIDTH LATIN SMALL LETTER D
       (?ｅ . "0xFF45") ; FULLWIDTH LATIN SMALL LETTER E
       (?ｆ . "0xFF46") ; FULLWIDTH LATIN SMALL LETTER F
       (?ｇ . "0xFF47") ; FULLWIDTH LATIN SMALL LETTER G
       (?ｈ . "0xFF48") ; FULLWIDTH LATIN SMALL LETTER H
       (?ｉ . "0xFF49") ; FULLWIDTH LATIN SMALL LETTER I
       (?ｊ . "0xFF4A") ; FULLWIDTH LATIN SMALL LETTER J
       (?ｋ . "0xFF4B") ; FULLWIDTH LATIN SMALL LETTER K
       (?ｌ . "0xFF4C") ; FULLWIDTH LATIN SMALL LETTER L
       (?ｍ . "0xFF4D") ; FULLWIDTH LATIN SMALL LETTER M
       (?ｎ . "0xFF4E") ; FULLWIDTH LATIN SMALL LETTER N
       (?ｏ . "0xFF4F") ; FULLWIDTH LATIN SMALL LETTER O
       (?ｐ . "0xFF50") ; FULLWIDTH LATIN SMALL LETTER P
       (?ｑ . "0xFF51") ; FULLWIDTH LATIN SMALL LETTER Q
       (?ｒ . "0xFF52") ; FULLWIDTH LATIN SMALL LETTER R
       (?ｓ . "0xFF53") ; FULLWIDTH LATIN SMALL LETTER S
       (?ｔ . "0xFF54") ; FULLWIDTH LATIN SMALL LETTER T
       (?ｕ . "0xFF55") ; FULLWIDTH LATIN SMALL LETTER U
       (?ｖ . "0xFF56") ; FULLWIDTH LATIN SMALL LETTER V
       (?ｗ . "0xFF57") ; FULLWIDTH LATIN SMALL LETTER W
       (?ｘ . "0xFF58") ; FULLWIDTH LATIN SMALL LETTER X
       (?ｙ . "0xFF59") ; FULLWIDTH LATIN SMALL LETTER Y
       (?ｚ . "0xFF5A") ; FULLWIDTH LATIN SMALL LETTER Z
       (?｛ . "0xFF5B") ; FULLWIDTH LEFT CURLY BRACKET
       (?｜ . "0xFF5C") ; FULLWIDTH VERTICAL LINE
       (?｝ . "0xFF5D") ; FULLWIDTH RIGHT CURLY BRACKET
       (?￣ . "0xFFE3") ; FULLWIDTH MACRON
       (?ㄱ . "0x3131") ; HANGUL LETTER KIYEOK
       (?ㄲ . "0x3132") ; HANGUL LETTER SSANGKIYEOK
       (?ㄳ . "0x3133") ; HANGUL LETTER KIYEOK-SIOS
       (?ㄴ . "0x3134") ; HANGUL LETTER NIEUN
       (?ㄵ . "0x3135") ; HANGUL LETTER NIEUN-CIEUC
       (?ㄶ . "0x3136") ; HANGUL LETTER NIEUN-HIEUH
       (?ㄷ . "0x3137") ; HANGUL LETTER TIKEUT
       (?ㄸ . "0x3138") ; HANGUL LETTER SSANGTIKEUT
       (?ㄹ . "0x3139") ; HANGUL LETTER RIEUL
       (?ㄺ . "0x313A") ; HANGUL LETTER RIEUL-KIYEOK
       (?ㄻ . "0x313B") ; HANGUL LETTER RIEUL-MIEUM
       (?ㄼ . "0x313C") ; HANGUL LETTER RIEUL-PIEUP
       (?ㄽ . "0x313D") ; HANGUL LETTER RIEUL-SIOS
       (?ㄾ . "0x313E") ; HANGUL LETTER RIEUL-THIEUTH
       (?ㄿ . "0x313F") ; HANGUL LETTER RIEUL-PHIEUPH
       (?ㅀ . "0x3140") ; HANGUL LETTER RIEUL-HIEUH
       (?ㅁ . "0x3141") ; HANGUL LETTER MIEUM
       (?ㅂ . "0x3142") ; HANGUL LETTER PIEUP
       (?ㅃ . "0x3143") ; HANGUL LETTER SSANGPIEUP
       (?ㅄ . "0x3144") ; HANGUL LETTER PIEUP-SIOS
       (?ㅅ . "0x3145") ; HANGUL LETTER SIOS
       (?ㅆ . "0x3146") ; HANGUL LETTER SSANGSIOS
       (?ㅇ . "0x3147") ; HANGUL LETTER IEUNG
       (?ㅈ . "0x3148") ; HANGUL LETTER CIEUC
       (?ㅉ . "0x3149") ; HANGUL LETTER SSANGCIEUC
       (?ㅊ . "0x314A") ; HANGUL LETTER CHIEUCH
       (?ㅋ . "0x314B") ; HANGUL LETTER KHIEUKH
       (?ㅌ . "0x314C") ; HANGUL LETTER THIEUTH
       (?ㅍ . "0x314D") ; HANGUL LETTER PHIEUPH
       (?ㅎ . "0x314E") ; HANGUL LETTER HIEUH
       (?ㅏ . "0x314F") ; HANGUL LETTER A
       (?ㅐ . "0x3150") ; HANGUL LETTER AE
       (?ㅑ . "0x3151") ; HANGUL LETTER YA
       (?ㅒ . "0x3152") ; HANGUL LETTER YAE
       (?ㅓ . "0x3153") ; HANGUL LETTER EO
       (?ㅔ . "0x3154") ; HANGUL LETTER E
       (?ㅕ . "0x3155") ; HANGUL LETTER YEO
       (?ㅖ . "0x3156") ; HANGUL LETTER YE
       (?ㅗ . "0x3157") ; HANGUL LETTER O
       (?ㅘ . "0x3158") ; HANGUL LETTER WA
       (?ㅙ . "0x3159") ; HANGUL LETTER WAE
       (?ㅚ . "0x315A") ; HANGUL LETTER OE
       (?ㅛ . "0x315B") ; HANGUL LETTER YO
       (?ㅜ . "0x315C") ; HANGUL LETTER U
       (?ㅝ . "0x315D") ; HANGUL LETTER WEO
       (?ㅞ . "0x315E") ; HANGUL LETTER WE
       (?ㅟ . "0x315F") ; HANGUL LETTER WI
       (?ㅠ . "0x3160") ; HANGUL LETTER YU
       (?ㅡ . "0x3161") ; HANGUL LETTER EU
       (?ㅢ . "0x3162") ; HANGUL LETTER YI
       (?ㅣ . "0x3163") ; HANGUL LETTER I
       (?ㅤ . "0x3164") ; HANGUL FILLER
       (?ㅥ . "0x3165") ; HANGUL LETTER SSANGNIEUN
       (?ㅦ . "0x3166") ; HANGUL LETTER NIEUN-TIKEUT
       (?ㅧ . "0x3167") ; HANGUL LETTER NIEUN-SIOS
       (?ㅨ . "0x3168") ; HANGUL LETTER NIEUN-PANSIOS
       (?ㅩ . "0x3169") ; HANGUL LETTER RIEUL-KIYEOK-SIOS
       (?ㅪ . "0x316A") ; HANGUL LETTER RIEUL-TIKEUT
       (?ㅫ . "0x316B") ; HANGUL LETTER RIEUL-PIEUP-SIOS
       (?ㅬ . "0x316C") ; HANGUL LETTER RIEUL-PANSIOS
       (?ㅭ . "0x316D") ; HANGUL LETTER RIEUL-YEORINHIEUH
       (?ㅮ . "0x316E") ; HANGUL LETTER MIEUM-PIEUP
       (?ㅯ . "0x316F") ; HANGUL LETTER MIEUM-SIOS
       (?ㅰ . "0x3170") ; HANGUL LETTER MIEUM-PANSIOS
       (?ㅱ . "0x3171") ; HANGUL LETTER KAPYEOUNMIEUM
       (?ㅲ . "0x3172") ; HANGUL LETTER PIEUP-KIYEOK
       (?ㅳ . "0x3173") ; HANGUL LETTER PIEUP-TIKEUT
       (?ㅴ . "0x3174") ; HANGUL LETTER PIEUP-SIOS-KIYEOK
       (?ㅵ . "0x3175") ; HANGUL LETTER PIEUP-SIOS-TIKEUT
       (?ㅶ . "0x3176") ; HANGUL LETTER PIEUP-CIEUC
       (?ㅷ . "0x3177") ; HANGUL LETTER PIEUP-THIEUTH
       (?ㅸ . "0x3178") ; HANGUL LETTER KAPYEOUNPIEUP
       (?ㅹ . "0x3179") ; HANGUL LETTER KAPYEOUNSSANGPIEUP
       (?ㅺ . "0x317A") ; HANGUL LETTER SIOS-KIYEOK
       (?ㅻ . "0x317B") ; HANGUL LETTER SIOS-NIEUN
       (?ㅼ . "0x317C") ; HANGUL LETTER SIOS-TIKEUT
       (?ㅽ . "0x317D") ; HANGUL LETTER SIOS-PIEUP
       (?ㅾ . "0x317E") ; HANGUL LETTER SIOS-CIEUC
       (?ㅿ . "0x317F") ; HANGUL LETTER PANSIOS
       (?ㆀ . "0x3180") ; HANGUL LETTER SSANGIEUNG
       (?ㆁ . "0x3181") ; HANGUL LETTER YESIEUNG
       (?ㆂ . "0x3182") ; HANGUL LETTER YESIEUNG-SIOS
       (?ㆃ . "0x3183") ; HANGUL LETTER YESIEUNG-PANSIOS
       (?ㆄ . "0x3184") ; HANGUL LETTER KAPYEOUNPHIEUPH
       (?ㆅ . "0x3185") ; HANGUL LETTER SSANGHIEUH
       (?ㆆ . "0x3186") ; HANGUL LETTER YEORINHIEUH
       (?ㆇ . "0x3187") ; HANGUL LETTER YO-YA
       (?ㆈ . "0x3188") ; HANGUL LETTER YO-YAE
       (?ㆉ . "0x3189") ; HANGUL LETTER YO-I
       (?ㆊ . "0x318A") ; HANGUL LETTER YU-YEO
       (?ㆋ . "0x318B") ; HANGUL LETTER YU-YE
       (?ㆌ . "0x318C") ; HANGUL LETTER YU-I
       (?ㆍ . "0x318D") ; HANGUL LETTER ARAEA
       (?ㆎ . "0x318E") ; HANGUL LETTER ARAEAE
       (?ⅰ . "0x2170") ; SMALL ROMAN NUMERAL ONE
       (?ⅱ . "0x2171") ; SMALL ROMAN NUMERAL TWO
       (?ⅲ . "0x2172") ; SMALL ROMAN NUMERAL THREE
       (?ⅳ . "0x2173") ; SMALL ROMAN NUMERAL FOUR
       (?ⅴ . "0x2174") ; SMALL ROMAN NUMERAL FIVE
       (?ⅵ . "0x2175") ; SMALL ROMAN NUMERAL SIX
       (?ⅶ . "0x2176") ; SMALL ROMAN NUMERAL SEVEN
       (?ⅷ . "0x2177") ; SMALL ROMAN NUMERAL EIGHT
       (?ⅸ . "0x2178") ; SMALL ROMAN NUMERAL NINE
       (?ⅹ . "0x2179") ; SMALL ROMAN NUMERAL TEN
       (?Ⅰ . "0x2160") ; ROMAN NUMERAL ONE
       (?Ⅱ . "0x2161") ; ROMAN NUMERAL TWO
       (?Ⅲ . "0x2162") ; ROMAN NUMERAL THREE
       (?Ⅳ . "0x2163") ; ROMAN NUMERAL FOUR
       (?Ⅴ . "0x2164") ; ROMAN NUMERAL FIVE
       (?Ⅵ . "0x2165") ; ROMAN NUMERAL SIX
       (?Ⅶ . "0x2166") ; ROMAN NUMERAL SEVEN
       (?Ⅷ . "0x2167") ; ROMAN NUMERAL EIGHT
       (?Ⅸ . "0x2168") ; ROMAN NUMERAL NINE
       (?Ⅹ . "0x2169") ; ROMAN NUMERAL TEN
       (?Α . "0x0391") ; GREEK CAPITAL LETTER ALPHA
       (?Β . "0x0392") ; GREEK CAPITAL LETTER BETA
       (?Γ . "0x0393") ; GREEK CAPITAL LETTER GAMMA
       (?Δ . "0x0394") ; GREEK CAPITAL LETTER DELTA
       (?Ε . "0x0395") ; GREEK CAPITAL LETTER EPSILON
       (?Ζ . "0x0396") ; GREEK CAPITAL LETTER ZETA
       (?Η . "0x0397") ; GREEK CAPITAL LETTER ETA
       (?Θ . "0x0398") ; GREEK CAPITAL LETTER THETA
       (?Ι . "0x0399") ; GREEK CAPITAL LETTER IOTA
       (?Κ . "0x039A") ; GREEK CAPITAL LETTER KAPPA
       (?Λ . "0x039B") ; GREEK CAPITAL LETTER LAMDA
       (?Μ . "0x039C") ; GREEK CAPITAL LETTER MU
       (?Ν . "0x039D") ; GREEK CAPITAL LETTER NU
       (?Ξ . "0x039E") ; GREEK CAPITAL LETTER XI
       (?Ο . "0x039F") ; GREEK CAPITAL LETTER OMICRON
       (?Π . "0x03A0") ; GREEK CAPITAL LETTER PI
       (?Ρ . "0x03A1") ; GREEK CAPITAL LETTER RHO
       (?Σ . "0x03A3") ; GREEK CAPITAL LETTER SIGMA
       (?Τ . "0x03A4") ; GREEK CAPITAL LETTER TAU
       (?Υ . "0x03A5") ; GREEK CAPITAL LETTER UPSILON
       (?Φ . "0x03A6") ; GREEK CAPITAL LETTER PHI
       (?Χ . "0x03A7") ; GREEK CAPITAL LETTER CHI
       (?Ψ . "0x03A8") ; GREEK CAPITAL LETTER PSI
       (?Ω . "0x03A9") ; GREEK CAPITAL LETTER OMEGA
       (?α . "0x03B1") ; GREEK SMALL LETTER ALPHA
       (?β . "0x03B2") ; GREEK SMALL LETTER BETA
       (?γ . "0x03B3") ; GREEK SMALL LETTER GAMMA
       (?δ . "0x03B4") ; GREEK SMALL LETTER DELTA
       (?ε . "0x03B5") ; GREEK SMALL LETTER EPSILON
       (?ζ . "0x03B6") ; GREEK SMALL LETTER ZETA
       (?η . "0x03B7") ; GREEK SMALL LETTER ETA
       (?θ . "0x03B8") ; GREEK SMALL LETTER THETA
       (?ι . "0x03B9") ; GREEK SMALL LETTER IOTA
       (?κ . "0x03BA") ; GREEK SMALL LETTER KAPPA
       (?λ . "0x03BB") ; GREEK SMALL LETTER LAMDA
       (?μ . "0x03BC") ; GREEK SMALL LETTER MU
       (?ν . "0x03BD") ; GREEK SMALL LETTER NU
       (?ξ . "0x03BE") ; GREEK SMALL LETTER XI
       (?ο . "0x03BF") ; GREEK SMALL LETTER OMICRON
       (?π . "0x03C0") ; GREEK SMALL LETTER PI
       (?ρ . "0x03C1") ; GREEK SMALL LETTER RHO
       (?σ . "0x03C3") ; GREEK SMALL LETTER SIGMA
       (?τ . "0x03C4") ; GREEK SMALL LETTER TAU
       (?υ . "0x03C5") ; GREEK SMALL LETTER UPSILON
       (?φ . "0x03C6") ; GREEK SMALL LETTER PHI
       (?χ . "0x03C7") ; GREEK SMALL LETTER CHI
       (?ψ . "0x03C8") ; GREEK SMALL LETTER PSI
       (?ω . "0x03C9") ; GREEK SMALL LETTER OMEGA
       (?─ . "0x2500") ; BOX DRAWINGS LIGHT HORIZONTAL
       (?│ . "0x2502") ; BOX DRAWINGS LIGHT VERTICAL
       (?┌ . "0x250C") ; BOX DRAWINGS LIGHT DOWN AND RIGHT
       (?┐ . "0x2510") ; BOX DRAWINGS LIGHT DOWN AND LEFT
       (?┘ . "0x2518") ; BOX DRAWINGS LIGHT UP AND LEFT
       (?└ . "0x2514") ; BOX DRAWINGS LIGHT UP AND RIGHT
       (?├ . "0x251C") ; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
       (?┬ . "0x252C") ; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
       (?┤ . "0x2524") ; BOX DRAWINGS LIGHT VERTICAL AND LEFT
       (?┴ . "0x2534") ; BOX DRAWINGS LIGHT UP AND HORIZONTAL
       (?┼ . "0x253C") ; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
       (?━ . "0x2501") ; BOX DRAWINGS HEAVY HORIZONTAL
       (?┃ . "0x2503") ; BOX DRAWINGS HEAVY VERTICAL
       (?┏ . "0x250F") ; BOX DRAWINGS HEAVY DOWN AND RIGHT
       (?┓ . "0x2513") ; BOX DRAWINGS HEAVY DOWN AND LEFT
       (?┛ . "0x251B") ; BOX DRAWINGS HEAVY UP AND LEFT
       (?┗ . "0x2517") ; BOX DRAWINGS HEAVY UP AND RIGHT
       (?┣ . "0x2523") ; BOX DRAWINGS HEAVY VERTICAL AND RIGHT
       (?┳ . "0x2533") ; BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
       (?┫ . "0x252B") ; BOX DRAWINGS HEAVY VERTICAL AND LEFT
       (?┻ . "0x253B") ; BOX DRAWINGS HEAVY UP AND HORIZONTAL
       (?╋ . "0x254B") ; BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
       (?┠ . "0x2520") ; BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
       (?┯ . "0x252F") ; BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
       (?┨ . "0x2528") ; BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
       (?┷ . "0x2537") ; BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
       (?┿ . "0x253F") ; BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
       (?┝ . "0x251D") ; BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
       (?┰ . "0x2530") ; BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
       (?┥ . "0x2525") ; BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
       (?┸ . "0x2538") ; BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
       (?╂ . "0x2542") ; BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
       (?┒ . "0x2512") ; BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
       (?┑ . "0x2511") ; BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
       (?┚ . "0x251A") ; BOX DRAWINGS UP HEAVY AND LEFT LIGHT
       (?┙ . "0x2519") ; BOX DRAWINGS UP LIGHT AND LEFT HEAVY
       (?┖ . "0x2516") ; BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
       (?┕ . "0x2515") ; BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
       (?┎ . "0x250E") ; BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
       (?┍ . "0x250D") ; BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
       (?┞ . "0x251E") ; BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
       (?┟ . "0x251F") ; BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
       (?┡ . "0x2521") ; BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
       (?┢ . "0x2522") ; BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
       (?┦ . "0x2526") ; BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
       (?┧ . "0x2527") ; BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
       (?┩ . "0x2529") ; BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
       (?┪ . "0x252A") ; BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
       (?┭ . "0x252D") ; BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
       (?┮ . "0x252E") ; BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
       (?┱ . "0x2531") ; BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
       (?┲ . "0x2532") ; BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
       (?┵ . "0x2535") ; BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
       (?┶ . "0x2536") ; BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
       (?┹ . "0x2539") ; BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
       (?┺ . "0x253A") ; BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
       (?┽ . "0x253D") ; BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
       (?┾ . "0x253E") ; BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
       (?╀ . "0x2540") ; BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
       (?╁ . "0x2541") ; BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
       (?╃ . "0x2543") ; BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
       (?╄ . "0x2544") ; BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
       (?╅ . "0x2545") ; BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
       (?╆ . "0x2546") ; BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
       (?╇ . "0x2547") ; BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
       (?╈ . "0x2548") ; BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
       (?╉ . "0x2549") ; BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
       (?╊ . "0x254A") ; BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
       (?㎕ . "0x3395") ; SQUARE MU L
       (?㎖ . "0x3396") ; SQUARE ML
       (?㎗ . "0x3397") ; SQUARE DL
       (?ℓ . "0x2113") ; SCRIPT SMALL L
       (?㎘ . "0x3398") ; SQUARE KL
       (?㏄ . "0x33C4") ; SQUARE CC
       (?㎣ . "0x33A3") ; SQUARE MM CUBED
       (?㎤ . "0x33A4") ; SQUARE CM CUBED
       (?㎥ . "0x33A5") ; SQUARE M CUBED
       (?㎦ . "0x33A6") ; SQUARE KM CUBED
       (?㎙ . "0x3399") ; SQUARE FM
       (?㎚ . "0x339A") ; SQUARE NM
       (?㎛ . "0x339B") ; SQUARE MU M
       (?㎜ . "0x339C") ; SQUARE MM
       (?㎝ . "0x339D") ; SQUARE CM
       (?㎞ . "0x339E") ; SQUARE KM
       (?㎟ . "0x339F") ; SQUARE MM SQUARED
       (?㎠ . "0x33A0") ; SQUARE CM SQUARED
       (?㎡ . "0x33A1") ; SQUARE M SQUARED
       (?㎢ . "0x33A2") ; SQUARE KM SQUARED
       (?㏊ . "0x33CA") ; SQUARE HA
       (?㎍ . "0x338D") ; SQUARE MU G
       (?㎎ . "0x338E") ; SQUARE MG
       (?㎏ . "0x338F") ; SQUARE KG
       (?㏏ . "0x33CF") ; SQUARE KT
       (?㎈ . "0x3388") ; SQUARE CAL
       (?㎉ . "0x3389") ; SQUARE KCAL
       (?㏈ . "0x33C8") ; SQUARE DB
       (?㎧ . "0x33A7") ; SQUARE M OVER S
       (?㎨ . "0x33A8") ; SQUARE M OVER S SQUARED
       (?㎰ . "0x33B0") ; SQUARE PS
       (?㎱ . "0x33B1") ; SQUARE NS
       (?㎲ . "0x33B2") ; SQUARE MU S
       (?㎳ . "0x33B3") ; SQUARE MS
       (?㎴ . "0x33B4") ; SQUARE PV
       (?㎵ . "0x33B5") ; SQUARE NV
       (?㎶ . "0x33B6") ; SQUARE MU V
       (?㎷ . "0x33B7") ; SQUARE MV
       (?㎸ . "0x33B8") ; SQUARE KV
       (?㎹ . "0x33B9") ; SQUARE MV MEGA
       (?㎀ . "0x3380") ; SQUARE PA AMPS
       (?㎁ . "0x3381") ; SQUARE NA
       (?㎂ . "0x3382") ; SQUARE MU A
       (?㎃ . "0x3383") ; SQUARE MA
       (?㎄ . "0x3384") ; SQUARE KA
       (?㎺ . "0x33BA") ; SQUARE PW
       (?㎻ . "0x33BB") ; SQUARE NW
       (?㎼ . "0x33BC") ; SQUARE MU W
       (?㎽ . "0x33BD") ; SQUARE MW
       (?㎾ . "0x33BE") ; SQUARE KW
       (?㎿ . "0x33BF") ; SQUARE MW MEGA
       (?㎐ . "0x3390") ; SQUARE HZ
       (?㎑ . "0x3391") ; SQUARE KHZ
       (?㎒ . "0x3392") ; SQUARE MHZ
       (?㎓ . "0x3393") ; SQUARE GHZ
       (?㎔ . "0x3394") ; SQUARE THZ
       (?Ω . "0x2126") ; OHM SIGN
       (?㏀ . "0x33C0") ; SQUARE K OHM
       (?㏁ . "0x33C1") ; SQUARE M OHM
       (?㎊ . "0x338A") ; SQUARE PF
       (?㎋ . "0x338B") ; SQUARE NF
       (?㎌ . "0x338C") ; SQUARE MU F
       (?㏖ . "0x33D6") ; SQUARE MOL
       (?㏅ . "0x33C5") ; SQUARE CD
       (?㎭ . "0x33AD") ; SQUARE RAD
       (?㎮ . "0x33AE") ; SQUARE RAD OVER S
       (?㎯ . "0x33AF") ; SQUARE RAD OVER S SQUARED
       (?㏛ . "0x33DB") ; SQUARE SR
       (?㎩ . "0x33A9") ; SQUARE PA
       (?㎪ . "0x33AA") ; SQUARE KPA
       (?㎫ . "0x33AB") ; SQUARE MPA
       (?㎬ . "0x33AC") ; SQUARE GPA
       (?㏝ . "0x33DD") ; SQUARE WB
       (?㏐ . "0x33D0") ; SQUARE LM
       (?㏓ . "0x33D3") ; SQUARE LX
       (?㏃ . "0x33C3") ; SQUARE BQ
       (?㏉ . "0x33C9") ; SQUARE GY
       (?㏜ . "0x33DC") ; SQUARE SV
       (?㏆ . "0x33C6") ; SQUARE C OVER KG
       (?Æ . "0x00C6") ; LATIN CAPITAL LIGATURE AE
       (?Ð . "0x00D0") ; LATIN CAPITAL LETTER ETH
       (?ª . "0x00AA") ; FEMININE ORDINAL INDICATOR
       (?Ħ . "0x0126") ; LATIN CAPITAL LETTER H WITH STROKE
       (?Ĳ . "0x0132") ; LATIN CAPITAL LIGATURE IJ
       (?Ŀ . "0x013F") ; LATIN CAPITAL LETTER L WITH MIDDLE DOT
       (?Ł . "0x0141") ; LATIN CAPITAL LETTER L WITH STROKE
       (?Ø . "0x00D8") ; LATIN CAPITAL LETTER O WITH STROKE
       (?Œ . "0x0152") ; LATIN CAPITAL LIGATURE OE
       (?º . "0x00BA") ; MASCULINE ORDINAL INDICATOR
       (?Þ . "0x00DE") ; LATIN CAPITAL LETTER THORN
       (?Ŧ . "0x0166") ; LATIN CAPITAL LETTER T WITH STROKE
       (?Ŋ . "0x014A") ; LATIN CAPITAL LETTER ENG
       (?㉠ . "0x3260") ; CIRCLED HANGUL KIYEOK
       (?㉡ . "0x3261") ; CIRCLED HANGUL NIEUN
       (?㉢ . "0x3262") ; CIRCLED HANGUL TIKEUT
       (?㉣ . "0x3263") ; CIRCLED HANGUL RIEUL
       (?㉤ . "0x3264") ; CIRCLED HANGUL MIEUM
       (?㉥ . "0x3265") ; CIRCLED HANGUL PIEUP
       (?㉦ . "0x3266") ; CIRCLED HANGUL SIOS
       (?㉧ . "0x3267") ; CIRCLED HANGUL IEUNG
       (?㉨ . "0x3268") ; CIRCLED HANGUL CIEUC
       (?㉩ . "0x3269") ; CIRCLED HANGUL CHIEUCH
       (?㉪ . "0x326A") ; CIRCLED HANGUL KHIEUKH
       (?㉫ . "0x326B") ; CIRCLED HANGUL THIEUTH
       (?㉬ . "0x326C") ; CIRCLED HANGUL PHIEUPH
       (?㉭ . "0x326D") ; CIRCLED HANGUL HIEUH
       (?㉮ . "0x326E") ; CIRCLED HANGUL KIYEOK A
       (?㉯ . "0x326F") ; CIRCLED HANGUL NIEUN A
       (?㉰ . "0x3270") ; CIRCLED HANGUL TIKEUT A
       (?㉱ . "0x3271") ; CIRCLED HANGUL RIEUL A
       (?㉲ . "0x3272") ; CIRCLED HANGUL MIEUM A
       (?㉳ . "0x3273") ; CIRCLED HANGUL PIEUP A
       (?㉴ . "0x3274") ; CIRCLED HANGUL SIOS A
       (?㉵ . "0x3275") ; CIRCLED HANGUL IEUNG A
       (?㉶ . "0x3276") ; CIRCLED HANGUL CIEUC A
       (?㉷ . "0x3277") ; CIRCLED HANGUL CHIEUCH A
       (?㉸ . "0x3278") ; CIRCLED HANGUL KHIEUKH A
       (?㉹ . "0x3279") ; CIRCLED HANGUL THIEUTH A
       (?㉺ . "0x327A") ; CIRCLED HANGUL PHIEUPH A
       (?㉻ . "0x327B") ; CIRCLED HANGUL HIEUH A
       (?ⓐ . "0x24D0") ; CIRCLED LATIN SMALL LETTER A
       (?ⓑ . "0x24D1") ; CIRCLED LATIN SMALL LETTER B
       (?ⓒ . "0x24D2") ; CIRCLED LATIN SMALL LETTER C
       (?ⓓ . "0x24D3") ; CIRCLED LATIN SMALL LETTER D
       (?ⓔ . "0x24D4") ; CIRCLED LATIN SMALL LETTER E
       (?ⓕ . "0x24D5") ; CIRCLED LATIN SMALL LETTER F
       (?ⓖ . "0x24D6") ; CIRCLED LATIN SMALL LETTER G
       (?ⓗ . "0x24D7") ; CIRCLED LATIN SMALL LETTER H
       (?ⓘ . "0x24D8") ; CIRCLED LATIN SMALL LETTER I
       (?ⓙ . "0x24D9") ; CIRCLED LATIN SMALL LETTER J
       (?ⓚ . "0x24DA") ; CIRCLED LATIN SMALL LETTER K
       (?ⓛ . "0x24DB") ; CIRCLED LATIN SMALL LETTER L
       (?ⓜ . "0x24DC") ; CIRCLED LATIN SMALL LETTER M
       (?ⓝ . "0x24DD") ; CIRCLED LATIN SMALL LETTER N
       (?ⓞ . "0x24DE") ; CIRCLED LATIN SMALL LETTER O
       (?ⓟ . "0x24DF") ; CIRCLED LATIN SMALL LETTER P
       (?ⓠ . "0x24E0") ; CIRCLED LATIN SMALL LETTER Q
       (?ⓡ . "0x24E1") ; CIRCLED LATIN SMALL LETTER R
       (?ⓢ . "0x24E2") ; CIRCLED LATIN SMALL LETTER S
       (?ⓣ . "0x24E3") ; CIRCLED LATIN SMALL LETTER T
       (?ⓤ . "0x24E4") ; CIRCLED LATIN SMALL LETTER U
       (?ⓥ . "0x24E5") ; CIRCLED LATIN SMALL LETTER V
       (?ⓦ . "0x24E6") ; CIRCLED LATIN SMALL LETTER W
       (?ⓧ . "0x24E7") ; CIRCLED LATIN SMALL LETTER X
       (?ⓨ . "0x24E8") ; CIRCLED LATIN SMALL LETTER Y
       (?ⓩ . "0x24E9") ; CIRCLED LATIN SMALL LETTER Z
       (?① . "0x2460") ; CIRCLED DIGIT ONE
       (?② . "0x2461") ; CIRCLED DIGIT TWO
       (?③ . "0x2462") ; CIRCLED DIGIT THREE
       (?④ . "0x2463") ; CIRCLED DIGIT FOUR
       (?⑤ . "0x2464") ; CIRCLED DIGIT FIVE
       (?⑥ . "0x2465") ; CIRCLED DIGIT SIX
       (?⑦ . "0x2466") ; CIRCLED DIGIT SEVEN
       (?⑧ . "0x2467") ; CIRCLED DIGIT EIGHT
       (?⑨ . "0x2468") ; CIRCLED DIGIT NINE
       (?⑩ . "0x2469") ; CIRCLED NUMBER TEN
       (?⑪ . "0x246A") ; CIRCLED NUMBER ELEVEN
       (?⑫ . "0x246B") ; CIRCLED NUMBER TWELVE
       (?⑬ . "0x246C") ; CIRCLED NUMBER THIRTEEN
       (?⑭ . "0x246D") ; CIRCLED NUMBER FOURTEEN
       (?⑮ . "0x246E") ; CIRCLED NUMBER FIFTEEN
       (?½ . "0x00BD") ; VULGAR FRACTION ONE HALF
       (?⅓ . "0x2153") ; VULGAR FRACTION ONE THIRD
       (?⅔ . "0x2154") ; VULGAR FRACTION TWO THIRDS
       (?¼ . "0x00BC") ; VULGAR FRACTION ONE QUARTER
       (?¾ . "0x00BE") ; VULGAR FRACTION THREE QUARTERS
       (?⅛ . "0x215B") ; VULGAR FRACTION ONE EIGHTH
       (?⅜ . "0x215C") ; VULGAR FRACTION THREE EIGHTHS
       (?⅝ . "0x215D") ; VULGAR FRACTION FIVE EIGHTHS
       (?⅞ . "0x215E") ; VULGAR FRACTION SEVEN EIGHTHS
       (?æ . "0x00E6") ; LATIN SMALL LIGATURE AE
       (?đ . "0x0111") ; LATIN SMALL LETTER D WITH STROKE
       (?ð . "0x00F0") ; LATIN SMALL LETTER ETH
       (?ħ . "0x0127") ; LATIN SMALL LETTER H WITH STROKE
       (?ı . "0x0131") ; LATIN SMALL LETTER DOTLESS I
       (?ĳ . "0x0133") ; LATIN SMALL LIGATURE IJ
       (?ĸ . "0x0138") ; LATIN SMALL LETTER KRA
       (?ŀ . "0x0140") ; LATIN SMALL LETTER L WITH MIDDLE DOT
       (?ł . "0x0142") ; LATIN SMALL LETTER L WITH STROKE
       (?ø . "0x00F8") ; LATIN SMALL LETTER O WITH STROKE
       (?œ . "0x0153") ; LATIN SMALL LIGATURE OE
       (?ß . "0x00DF") ; LATIN SMALL LETTER SHARP S
       (?þ . "0x00FE") ; LATIN SMALL LETTER THORN
       (?ŧ . "0x0167") ; LATIN SMALL LETTER T WITH STROKE
       (?ŋ . "0x014B") ; LATIN SMALL LETTER ENG
       (?ŉ . "0x0149") ; LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
       (?㈀ . "0x3200") ; PARENTHESIZED HANGUL KIYEOK
       (?㈁ . "0x3201") ; PARENTHESIZED HANGUL NIEUN
       (?㈂ . "0x3202") ; PARENTHESIZED HANGUL TIKEUT
       (?㈃ . "0x3203") ; PARENTHESIZED HANGUL RIEUL
       (?㈄ . "0x3204") ; PARENTHESIZED HANGUL MIEUM
       (?㈅ . "0x3205") ; PARENTHESIZED HANGUL PIEUP
       (?㈆ . "0x3206") ; PARENTHESIZED HANGUL SIOS
       (?㈇ . "0x3207") ; PARENTHESIZED HANGUL IEUNG
       (?㈈ . "0x3208") ; PARENTHESIZED HANGUL CIEUC
       (?㈉ . "0x3209") ; PARENTHESIZED HANGUL CHIEUCH
       (?㈊ . "0x320A") ; PARENTHESIZED HANGUL KHIEUKH
       (?㈋ . "0x320B") ; PARENTHESIZED HANGUL THIEUTH
       (?㈌ . "0x320C") ; PARENTHESIZED HANGUL PHIEUPH
       (?㈍ . "0x320D") ; PARENTHESIZED HANGUL HIEUH
       (?㈎ . "0x320E") ; PARENTHESIZED HANGUL KIYEOK A
       (?㈏ . "0x320F") ; PARENTHESIZED HANGUL NIEUN A
       (?㈐ . "0x3210") ; PARENTHESIZED HANGUL TIKEUT A
       (?㈑ . "0x3211") ; PARENTHESIZED HANGUL RIEUL A
       (?㈒ . "0x3212") ; PARENTHESIZED HANGUL MIEUM A
       (?㈓ . "0x3213") ; PARENTHESIZED HANGUL PIEUP A
       (?㈔ . "0x3214") ; PARENTHESIZED HANGUL SIOS A
       (?㈕ . "0x3215") ; PARENTHESIZED HANGUL IEUNG A
       (?㈖ . "0x3216") ; PARENTHESIZED HANGUL CIEUC A
       (?㈗ . "0x3217") ; PARENTHESIZED HANGUL CHIEUCH A
       (?㈘ . "0x3218") ; PARENTHESIZED HANGUL KHIEUKH A
       (?㈙ . "0x3219") ; PARENTHESIZED HANGUL THIEUTH A
       (?㈚ . "0x321A") ; PARENTHESIZED HANGUL PHIEUPH A
       (?㈛ . "0x321B") ; PARENTHESIZED HANGUL HIEUH A
       (?⒜ . "0x249C") ; PARENTHESIZED LATIN SMALL LETTER A
       (?⒝ . "0x249D") ; PARENTHESIZED LATIN SMALL LETTER B
       (?⒞ . "0x249E") ; PARENTHESIZED LATIN SMALL LETTER C
       (?⒟ . "0x249F") ; PARENTHESIZED LATIN SMALL LETTER D
       (?⒠ . "0x24A0") ; PARENTHESIZED LATIN SMALL LETTER E
       (?⒡ . "0x24A1") ; PARENTHESIZED LATIN SMALL LETTER F
       (?⒢ . "0x24A2") ; PARENTHESIZED LATIN SMALL LETTER G
       (?⒣ . "0x24A3") ; PARENTHESIZED LATIN SMALL LETTER H
       (?⒤ . "0x24A4") ; PARENTHESIZED LATIN SMALL LETTER I
       (?⒥ . "0x24A5") ; PARENTHESIZED LATIN SMALL LETTER J
       (?⒦ . "0x24A6") ; PARENTHESIZED LATIN SMALL LETTER K
       (?⒧ . "0x24A7") ; PARENTHESIZED LATIN SMALL LETTER L
       (?⒨ . "0x24A8") ; PARENTHESIZED LATIN SMALL LETTER M
       (?⒩ . "0x24A9") ; PARENTHESIZED LATIN SMALL LETTER N
       (?⒪ . "0x24AA") ; PARENTHESIZED LATIN SMALL LETTER O
       (?⒫ . "0x24AB") ; PARENTHESIZED LATIN SMALL LETTER P
       (?⒬ . "0x24AC") ; PARENTHESIZED LATIN SMALL LETTER Q
       (?⒭ . "0x24AD") ; PARENTHESIZED LATIN SMALL LETTER R
       (?⒮ . "0x24AE") ; PARENTHESIZED LATIN SMALL LETTER S
       (?⒯ . "0x24AF") ; PARENTHESIZED LATIN SMALL LETTER T
       (?⒰ . "0x24B0") ; PARENTHESIZED LATIN SMALL LETTER U
       (?⒱ . "0x24B1") ; PARENTHESIZED LATIN SMALL LETTER V
       (?⒲ . "0x24B2") ; PARENTHESIZED LATIN SMALL LETTER W
       (?⒳ . "0x24B3") ; PARENTHESIZED LATIN SMALL LETTER X
       (?⒴ . "0x24B4") ; PARENTHESIZED LATIN SMALL LETTER Y
       (?⒵ . "0x24B5") ; PARENTHESIZED LATIN SMALL LETTER Z
       (?⑴ . "0x2474") ; PARENTHESIZED DIGIT ONE
       (?⑵ . "0x2475") ; PARENTHESIZED DIGIT TWO
       (?⑶ . "0x2476") ; PARENTHESIZED DIGIT THREE
       (?⑷ . "0x2477") ; PARENTHESIZED DIGIT FOUR
       (?⑸ . "0x2478") ; PARENTHESIZED DIGIT FIVE
       (?⑹ . "0x2479") ; PARENTHESIZED DIGIT SIX
       (?⑺ . "0x247A") ; PARENTHESIZED DIGIT SEVEN
       (?⑻ . "0x247B") ; PARENTHESIZED DIGIT EIGHT
       (?⑼ . "0x247C") ; PARENTHESIZED DIGIT NINE
       (?⑽ . "0x247D") ; PARENTHESIZED NUMBER TEN
       (?⑾ . "0x247E") ; PARENTHESIZED NUMBER ELEVEN
       (?⑿ . "0x247F") ; PARENTHESIZED NUMBER TWELVE
       (?⒀ . "0x2480") ; PARENTHESIZED NUMBER THIRTEEN
       (?⒁ . "0x2481") ; PARENTHESIZED NUMBER FOURTEEN
       (?⒂ . "0x2482") ; PARENTHESIZED NUMBER FIFTEEN
       (?¹ . "0x00B9") ; SUPERSCRIPT ONE
       (?² . "0x00B2") ; SUPERSCRIPT TWO
       (?³ . "0x00B3") ; SUPERSCRIPT THREE
       (?⁴ . "0x2074") ; SUPERSCRIPT FOUR
       (?ⁿ . "0x207F") ; SUPERSCRIPT LATIN SMALL LETTER N
       (?₁ . "0x2081") ; SUBSCRIPT ONE
       (?₂ . "0x2082") ; SUBSCRIPT TWO
       (?₃ . "0x2083") ; SUBSCRIPT THREE
       (?₄ . "0x2084") ; SUBSCRIPT FOUR
       (?ぁ . "0x3041") ; HIRAGANA LETTER SMALL A
       (?あ . "0x3042") ; HIRAGANA LETTER A
       (?ぃ . "0x3043") ; HIRAGANA LETTER SMALL I
       (?い . "0x3044") ; HIRAGANA LETTER I
       (?ぅ . "0x3045") ; HIRAGANA LETTER SMALL U
       (?う . "0x3046") ; HIRAGANA LETTER U
       (?ぇ . "0x3047") ; HIRAGANA LETTER SMALL E
       (?え . "0x3048") ; HIRAGANA LETTER E
       (?ぉ . "0x3049") ; HIRAGANA LETTER SMALL O
       (?お . "0x304A") ; HIRAGANA LETTER O
       (?か . "0x304B") ; HIRAGANA LETTER KA
       (?が . "0x304C") ; HIRAGANA LETTER GA
       (?き . "0x304D") ; HIRAGANA LETTER KI
       (?ぎ . "0x304E") ; HIRAGANA LETTER GI
       (?く . "0x304F") ; HIRAGANA LETTER KU
       (?ぐ . "0x3050") ; HIRAGANA LETTER GU
       (?け . "0x3051") ; HIRAGANA LETTER KE
       (?げ . "0x3052") ; HIRAGANA LETTER GE
       (?こ . "0x3053") ; HIRAGANA LETTER KO
       (?ご . "0x3054") ; HIRAGANA LETTER GO
       (?さ . "0x3055") ; HIRAGANA LETTER SA
       (?ざ . "0x3056") ; HIRAGANA LETTER ZA
       (?し . "0x3057") ; HIRAGANA LETTER SI
       (?じ . "0x3058") ; HIRAGANA LETTER ZI
       (?す . "0x3059") ; HIRAGANA LETTER SU
       (?ず . "0x305A") ; HIRAGANA LETTER ZU
       (?せ . "0x305B") ; HIRAGANA LETTER SE
       (?ぜ . "0x305C") ; HIRAGANA LETTER ZE
       (?そ . "0x305D") ; HIRAGANA LETTER SO
       (?ぞ . "0x305E") ; HIRAGANA LETTER ZO
       (?た . "0x305F") ; HIRAGANA LETTER TA
       (?だ . "0x3060") ; HIRAGANA LETTER DA
       (?ち . "0x3061") ; HIRAGANA LETTER TI
       (?ぢ . "0x3062") ; HIRAGANA LETTER DI
       (?っ . "0x3063") ; HIRAGANA LETTER SMALL TU
       (?つ . "0x3064") ; HIRAGANA LETTER TU
       (?づ . "0x3065") ; HIRAGANA LETTER DU
       (?て . "0x3066") ; HIRAGANA LETTER TE
       (?で . "0x3067") ; HIRAGANA LETTER DE
       (?と . "0x3068") ; HIRAGANA LETTER TO
       (?ど . "0x3069") ; HIRAGANA LETTER DO
       (?な . "0x306A") ; HIRAGANA LETTER NA
       (?に . "0x306B") ; HIRAGANA LETTER NI
       (?ぬ . "0x306C") ; HIRAGANA LETTER NU
       (?ね . "0x306D") ; HIRAGANA LETTER NE
       (?の . "0x306E") ; HIRAGANA LETTER NO
       (?は . "0x306F") ; HIRAGANA LETTER HA
       (?ば . "0x3070") ; HIRAGANA LETTER BA
       (?ぱ . "0x3071") ; HIRAGANA LETTER PA
       (?ひ . "0x3072") ; HIRAGANA LETTER HI
       (?び . "0x3073") ; HIRAGANA LETTER BI
       (?ぴ . "0x3074") ; HIRAGANA LETTER PI
       (?ふ . "0x3075") ; HIRAGANA LETTER HU
       (?ぶ . "0x3076") ; HIRAGANA LETTER BU
       (?ぷ . "0x3077") ; HIRAGANA LETTER PU
       (?へ . "0x3078") ; HIRAGANA LETTER HE
       (?べ . "0x3079") ; HIRAGANA LETTER BE
       (?ぺ . "0x307A") ; HIRAGANA LETTER PE
       (?ほ . "0x307B") ; HIRAGANA LETTER HO
       (?ぼ . "0x307C") ; HIRAGANA LETTER BO
       (?ぽ . "0x307D") ; HIRAGANA LETTER PO
       (?ま . "0x307E") ; HIRAGANA LETTER MA
       (?み . "0x307F") ; HIRAGANA LETTER MI
       (?む . "0x3080") ; HIRAGANA LETTER MU
       (?め . "0x3081") ; HIRAGANA LETTER ME
       (?も . "0x3082") ; HIRAGANA LETTER MO
       (?ゃ . "0x3083") ; HIRAGANA LETTER SMALL YA
       (?や . "0x3084") ; HIRAGANA LETTER YA
       (?ゅ . "0x3085") ; HIRAGANA LETTER SMALL YU
       (?ゆ . "0x3086") ; HIRAGANA LETTER YU
       (?ょ . "0x3087") ; HIRAGANA LETTER SMALL YO
       (?よ . "0x3088") ; HIRAGANA LETTER YO
       (?ら . "0x3089") ; HIRAGANA LETTER RA
       (?り . "0x308A") ; HIRAGANA LETTER RI
       (?る . "0x308B") ; HIRAGANA LETTER RU
       (?れ . "0x308C") ; HIRAGANA LETTER RE
       (?ろ . "0x308D") ; HIRAGANA LETTER RO
       (?ゎ . "0x308E") ; HIRAGANA LETTER SMALL WA
       (?わ . "0x308F") ; HIRAGANA LETTER WA
       (?ゐ . "0x3090") ; HIRAGANA LETTER WI
       (?ゑ . "0x3091") ; HIRAGANA LETTER WE
       (?を . "0x3092") ; HIRAGANA LETTER WO
       (?ん . "0x3093") ; HIRAGANA LETTER N
       (?ァ . "0x30A1") ; KATAKANA LETTER SMALL A
       (?ア . "0x30A2") ; KATAKANA LETTER A
       (?ィ . "0x30A3") ; KATAKANA LETTER SMALL I
       (?イ . "0x30A4") ; KATAKANA LETTER I
       (?ゥ . "0x30A5") ; KATAKANA LETTER SMALL U
       (?ウ . "0x30A6") ; KATAKANA LETTER U
       (?ェ . "0x30A7") ; KATAKANA LETTER SMALL E
       (?エ . "0x30A8") ; KATAKANA LETTER E
       (?ォ . "0x30A9") ; KATAKANA LETTER SMALL O
       (?オ . "0x30AA") ; KATAKANA LETTER O
       (?カ . "0x30AB") ; KATAKANA LETTER KA
       (?ガ . "0x30AC") ; KATAKANA LETTER GA
       (?キ . "0x30AD") ; KATAKANA LETTER KI
       (?ギ . "0x30AE") ; KATAKANA LETTER GI
       (?ク . "0x30AF") ; KATAKANA LETTER KU
       (?グ . "0x30B0") ; KATAKANA LETTER GU
       (?ケ . "0x30B1") ; KATAKANA LETTER KE
       (?ゲ . "0x30B2") ; KATAKANA LETTER GE
       (?コ . "0x30B3") ; KATAKANA LETTER KO
       (?ゴ . "0x30B4") ; KATAKANA LETTER GO
       (?サ . "0x30B5") ; KATAKANA LETTER SA
       (?ザ . "0x30B6") ; KATAKANA LETTER ZA
       (?シ . "0x30B7") ; KATAKANA LETTER SI
       (?ジ . "0x30B8") ; KATAKANA LETTER ZI
       (?ス . "0x30B9") ; KATAKANA LETTER SU
       (?ズ . "0x30BA") ; KATAKANA LETTER ZU
       (?セ . "0x30BB") ; KATAKANA LETTER SE
       (?ゼ . "0x30BC") ; KATAKANA LETTER ZE
       (?ソ . "0x30BD") ; KATAKANA LETTER SO
       (?ゾ . "0x30BE") ; KATAKANA LETTER ZO
       (?タ . "0x30BF") ; KATAKANA LETTER TA
       (?ダ . "0x30C0") ; KATAKANA LETTER DA
       (?チ . "0x30C1") ; KATAKANA LETTER TI
       (?ヂ . "0x30C2") ; KATAKANA LETTER DI
       (?ッ . "0x30C3") ; KATAKANA LETTER SMALL TU
       (?ツ . "0x30C4") ; KATAKANA LETTER TU
       (?ヅ . "0x30C5") ; KATAKANA LETTER DU
       (?テ . "0x30C6") ; KATAKANA LETTER TE
       (?デ . "0x30C7") ; KATAKANA LETTER DE
       (?ト . "0x30C8") ; KATAKANA LETTER TO
       (?ド . "0x30C9") ; KATAKANA LETTER DO
       (?ナ . "0x30CA") ; KATAKANA LETTER NA
       (?ニ . "0x30CB") ; KATAKANA LETTER NI
       (?ヌ . "0x30CC") ; KATAKANA LETTER NU
       (?ネ . "0x30CD") ; KATAKANA LETTER NE
       (?ノ . "0x30CE") ; KATAKANA LETTER NO
       (?ハ . "0x30CF") ; KATAKANA LETTER HA
       (?バ . "0x30D0") ; KATAKANA LETTER BA
       (?パ . "0x30D1") ; KATAKANA LETTER PA
       (?ヒ . "0x30D2") ; KATAKANA LETTER HI
       (?ビ . "0x30D3") ; KATAKANA LETTER BI
       (?ピ . "0x30D4") ; KATAKANA LETTER PI
       (?フ . "0x30D5") ; KATAKANA LETTER HU
       (?ブ . "0x30D6") ; KATAKANA LETTER BU
       (?プ . "0x30D7") ; KATAKANA LETTER PU
       (?ヘ . "0x30D8") ; KATAKANA LETTER HE
       (?ベ . "0x30D9") ; KATAKANA LETTER BE
       (?ペ . "0x30DA") ; KATAKANA LETTER PE
       (?ホ . "0x30DB") ; KATAKANA LETTER HO
       (?ボ . "0x30DC") ; KATAKANA LETTER BO
       (?ポ . "0x30DD") ; KATAKANA LETTER PO
       (?マ . "0x30DE") ; KATAKANA LETTER MA
       (?ミ . "0x30DF") ; KATAKANA LETTER MI
       (?ム . "0x30E0") ; KATAKANA LETTER MU
       (?メ . "0x30E1") ; KATAKANA LETTER ME
       (?モ . "0x30E2") ; KATAKANA LETTER MO
       (?ャ . "0x30E3") ; KATAKANA LETTER SMALL YA
       (?ヤ . "0x30E4") ; KATAKANA LETTER YA
       (?ュ . "0x30E5") ; KATAKANA LETTER SMALL YU
       (?ユ . "0x30E6") ; KATAKANA LETTER YU
       (?ョ . "0x30E7") ; KATAKANA LETTER SMALL YO
       (?ヨ . "0x30E8") ; KATAKANA LETTER YO
       (?ラ . "0x30E9") ; KATAKANA LETTER RA
       (?リ . "0x30EA") ; KATAKANA LETTER RI
       (?ル . "0x30EB") ; KATAKANA LETTER RU
       (?レ . "0x30EC") ; KATAKANA LETTER RE
       (?ロ . "0x30ED") ; KATAKANA LETTER RO
       (?ヮ . "0x30EE") ; KATAKANA LETTER SMALL WA
       (?ワ . "0x30EF") ; KATAKANA LETTER WA
       (?ヰ . "0x30F0") ; KATAKANA LETTER WI
       (?ヱ . "0x30F1") ; KATAKANA LETTER WE
       (?ヲ . "0x30F2") ; KATAKANA LETTER WO
       (?ン . "0x30F3") ; KATAKANA LETTER N
       (?ヴ . "0x30F4") ; KATAKANA LETTER VU
       (?ヵ . "0x30F5") ; KATAKANA LETTER SMALL KA
       (?ヶ . "0x30F6") ; KATAKANA LETTER SMALL KE
       (?А . "0x0410") ; CYRILLIC CAPITAL LETTER A
       (?Б . "0x0411") ; CYRILLIC CAPITAL LETTER BE
       (?В . "0x0412") ; CYRILLIC CAPITAL LETTER VE
       (?Г . "0x0413") ; CYRILLIC CAPITAL LETTER GHE
       (?Д . "0x0414") ; CYRILLIC CAPITAL LETTER DE
       (?Е . "0x0415") ; CYRILLIC CAPITAL LETTER IE
       (?Ё . "0x0401") ; CYRILLIC CAPITAL LETTER IO
       (?Ж . "0x0416") ; CYRILLIC CAPITAL LETTER ZHE
       (?З . "0x0417") ; CYRILLIC CAPITAL LETTER ZE
       (?И . "0x0418") ; CYRILLIC CAPITAL LETTER I
       (?Й . "0x0419") ; CYRILLIC CAPITAL LETTER SHORT I
       (?К . "0x041A") ; CYRILLIC CAPITAL LETTER KA
       (?Л . "0x041B") ; CYRILLIC CAPITAL LETTER EL
       (?М . "0x041C") ; CYRILLIC CAPITAL LETTER EM
       (?Н . "0x041D") ; CYRILLIC CAPITAL LETTER EN
       (?О . "0x041E") ; CYRILLIC CAPITAL LETTER O
       (?П . "0x041F") ; CYRILLIC CAPITAL LETTER PE
       (?Р . "0x0420") ; CYRILLIC CAPITAL LETTER ER
       (?С . "0x0421") ; CYRILLIC CAPITAL LETTER ES
       (?Т . "0x0422") ; CYRILLIC CAPITAL LETTER TE
       (?У . "0x0423") ; CYRILLIC CAPITAL LETTER U
       (?Ф . "0x0424") ; CYRILLIC CAPITAL LETTER EF
       (?Х . "0x0425") ; CYRILLIC CAPITAL LETTER HA
       (?Ц . "0x0426") ; CYRILLIC CAPITAL LETTER TSE
       (?Ч . "0x0427") ; CYRILLIC CAPITAL LETTER CHE
       (?Ш . "0x0428") ; CYRILLIC CAPITAL LETTER SHA
       (?Щ . "0x0429") ; CYRILLIC CAPITAL LETTER SHCHA
       (?Ъ . "0x042A") ; CYRILLIC CAPITAL LETTER HARD SIGN
       (?Ы . "0x042B") ; CYRILLIC CAPITAL LETTER YERU
       (?Ь . "0x042C") ; CYRILLIC CAPITAL LETTER SOFT SIGN
       (?Э . "0x042D") ; CYRILLIC CAPITAL LETTER E
       (?Ю . "0x042E") ; CYRILLIC CAPITAL LETTER YU
       (?Я . "0x042F") ; CYRILLIC CAPITAL LETTER YA
       (?а . "0x0430") ; CYRILLIC SMALL LETTER A
       (?б . "0x0431") ; CYRILLIC SMALL LETTER BE
       (?в . "0x0432") ; CYRILLIC SMALL LETTER VE
       (?г . "0x0433") ; CYRILLIC SMALL LETTER GHE
       (?д . "0x0434") ; CYRILLIC SMALL LETTER DE
       (?е . "0x0435") ; CYRILLIC SMALL LETTER IE
       (?ё . "0x0451") ; CYRILLIC SMALL LETTER IO
       (?ж . "0x0436") ; CYRILLIC SMALL LETTER ZHE
       (?з . "0x0437") ; CYRILLIC SMALL LETTER ZE
       (?и . "0x0438") ; CYRILLIC SMALL LETTER I
       (?й . "0x0439") ; CYRILLIC SMALL LETTER SHORT I
       (?к . "0x043A") ; CYRILLIC SMALL LETTER KA
       (?л . "0x043B") ; CYRILLIC SMALL LETTER EL
       (?м . "0x043C") ; CYRILLIC SMALL LETTER EM
       (?н . "0x043D") ; CYRILLIC SMALL LETTER EN
       (?о . "0x043E") ; CYRILLIC SMALL LETTER O
       (?п . "0x043F") ; CYRILLIC SMALL LETTER PE
       (?р . "0x0440") ; CYRILLIC SMALL LETTER ER
       (?с . "0x0441") ; CYRILLIC SMALL LETTER ES
       (?т . "0x0442") ; CYRILLIC SMALL LETTER TE
       (?у . "0x0443") ; CYRILLIC SMALL LETTER U
       (?ф . "0x0444") ; CYRILLIC SMALL LETTER EF
       (?х . "0x0445") ; CYRILLIC SMALL LETTER HA
       (?ц . "0x0446") ; CYRILLIC SMALL LETTER TSE
       (?ч . "0x0447") ; CYRILLIC SMALL LETTER CHE
       (?ш . "0x0448") ; CYRILLIC SMALL LETTER SHA
       (?щ . "0x0449") ; CYRILLIC SMALL LETTER SHCHA
       (?ъ . "0x044A") ; CYRILLIC SMALL LETTER HARD SIGN
       (?ы . "0x044B") ; CYRILLIC SMALL LETTER YERU
       (?ь . "0x044C") ; CYRILLIC SMALL LETTER SOFT SIGN
       (?э . "0x044D") ; CYRILLIC SMALL LETTER E
       (?ю . "0x044E") ; CYRILLIC SMALL LETTER YU
       (?я . "0x044F") ; CYRILLIC SMALL LETTER YA
       (?가 . "0xAC00") ; HANGUL SYLLABLE KIYEOK-A
       (?각 . "0xAC01") ; HANGUL SYLLABLE KIYEOK-A-KIYEOK
       (?간 . "0xAC04") ; HANGUL SYLLABLE KIYEOK-A-NIEUN
       (?갇 . "0xAC07") ; HANGUL SYLLABLE KIYEOK-A-TIKEUT
       (?갈 . "0xAC08") ; HANGUL SYLLABLE KIYEOK-A-RIEUL
       (?갉 . "0xAC09") ; HANGUL SYLLABLE KIYEOK-A-RIEULKIYEOK
       (?갊 . "0xAC0A") ; HANGUL SYLLABLE KIYEOK-A-RIEULMIEUM
       (?감 . "0xAC10") ; HANGUL SYLLABLE KIYEOK-A-MIEUM
       (?갑 . "0xAC11") ; HANGUL SYLLABLE KIYEOK-A-PIEUP
       (?값 . "0xAC12") ; HANGUL SYLLABLE KIYEOK-A-PIEUPSIOS
       (?갓 . "0xAC13") ; HANGUL SYLLABLE KIYEOK-A-SIOS
       (?갔 . "0xAC14") ; HANGUL SYLLABLE KIYEOK-A-SSANGSIOS
       (?강 . "0xAC15") ; HANGUL SYLLABLE KIYEOK-A-IEUNG
       (?갖 . "0xAC16") ; HANGUL SYLLABLE KIYEOK-A-CIEUC
       (?갗 . "0xAC17") ; HANGUL SYLLABLE KIYEOK-A-CHIEUCH
       (?같 . "0xAC19") ; HANGUL SYLLABLE KIYEOK-A-THIEUTH
       (?갚 . "0xAC1A") ; HANGUL SYLLABLE KIYEOK-A-PHIEUPH
       (?갛 . "0xAC1B") ; HANGUL SYLLABLE KIYEOK-A-HIEUH
       (?개 . "0xAC1C") ; HANGUL SYLLABLE KIYEOK-AE
       (?객 . "0xAC1D") ; HANGUL SYLLABLE KIYEOK-AE-KIYEOK
       (?갠 . "0xAC20") ; HANGUL SYLLABLE KIYEOK-AE-NIEUN
       (?갤 . "0xAC24") ; HANGUL SYLLABLE KIYEOK-AE-RIEUL
       (?갬 . "0xAC2C") ; HANGUL SYLLABLE KIYEOK-AE-MIEUM
       (?갭 . "0xAC2D") ; HANGUL SYLLABLE KIYEOK-AE-PIEUP
       (?갯 . "0xAC2F") ; HANGUL SYLLABLE KIYEOK-AE-SIOS
       (?갰 . "0xAC30") ; HANGUL SYLLABLE KIYEOK-AE-SSANGSIOS
       (?갱 . "0xAC31") ; HANGUL SYLLABLE KIYEOK-AE-IEUNG
       (?갸 . "0xAC38") ; HANGUL SYLLABLE KIYEOK-YA
       (?갹 . "0xAC39") ; HANGUL SYLLABLE KIYEOK-YA-KIYEOK
       (?갼 . "0xAC3C") ; HANGUL SYLLABLE KIYEOK-YA-NIEUN
       (?걀 . "0xAC40") ; HANGUL SYLLABLE KIYEOK-YA-RIEUL
       (?걋 . "0xAC4B") ; HANGUL SYLLABLE KIYEOK-YA-SIOS
       (?걍 . "0xAC4D") ; HANGUL SYLLABLE KIYEOK-YA-IEUNG
       (?걔 . "0xAC54") ; HANGUL SYLLABLE KIYEOK-YAE
       (?걘 . "0xAC58") ; HANGUL SYLLABLE KIYEOK-YAE-NIEUN
       (?걜 . "0xAC5C") ; HANGUL SYLLABLE KIYEOK-YAE-RIEUL
       (?거 . "0xAC70") ; HANGUL SYLLABLE KIYEOK-EO
       (?걱 . "0xAC71") ; HANGUL SYLLABLE KIYEOK-EO-KIYEOK
       (?건 . "0xAC74") ; HANGUL SYLLABLE KIYEOK-EO-NIEUN
       (?걷 . "0xAC77") ; HANGUL SYLLABLE KIYEOK-EO-TIKEUT
       (?걸 . "0xAC78") ; HANGUL SYLLABLE KIYEOK-EO-RIEUL
       (?걺 . "0xAC7A") ; HANGUL SYLLABLE KIYEOK-EO-RIEULMIEUM
       (?검 . "0xAC80") ; HANGUL SYLLABLE KIYEOK-EO-MIEUM
       (?겁 . "0xAC81") ; HANGUL SYLLABLE KIYEOK-EO-PIEUP
       (?것 . "0xAC83") ; HANGUL SYLLABLE KIYEOK-EO-SIOS
       (?겄 . "0xAC84") ; HANGUL SYLLABLE KIYEOK-EO-SSANGSIOS
       (?겅 . "0xAC85") ; HANGUL SYLLABLE KIYEOK-EO-IEUNG
       (?겆 . "0xAC86") ; HANGUL SYLLABLE KIYEOK-EO-CIEUC
       (?겉 . "0xAC89") ; HANGUL SYLLABLE KIYEOK-EO-THIEUTH
       (?겊 . "0xAC8A") ; HANGUL SYLLABLE KIYEOK-EO-PHIEUPH
       (?겋 . "0xAC8B") ; HANGUL SYLLABLE KIYEOK-EO-HIEUH
       (?게 . "0xAC8C") ; HANGUL SYLLABLE KIYEOK-E
       (?겐 . "0xAC90") ; HANGUL SYLLABLE KIYEOK-E-NIEUN
       (?겔 . "0xAC94") ; HANGUL SYLLABLE KIYEOK-E-RIEUL
       (?겜 . "0xAC9C") ; HANGUL SYLLABLE KIYEOK-E-MIEUM
       (?겝 . "0xAC9D") ; HANGUL SYLLABLE KIYEOK-E-PIEUP
       (?겟 . "0xAC9F") ; HANGUL SYLLABLE KIYEOK-E-SIOS
       (?겠 . "0xACA0") ; HANGUL SYLLABLE KIYEOK-E-SSANGSIOS
       (?겡 . "0xACA1") ; HANGUL SYLLABLE KIYEOK-E-IEUNG
       (?겨 . "0xACA8") ; HANGUL SYLLABLE KIYEOK-YEO
       (?격 . "0xACA9") ; HANGUL SYLLABLE KIYEOK-YEO-KIYEOK
       (?겪 . "0xACAA") ; HANGUL SYLLABLE KIYEOK-YEO-SSANGKIYEOK
       (?견 . "0xACAC") ; HANGUL SYLLABLE KIYEOK-YEO-NIEUN
       (?겯 . "0xACAF") ; HANGUL SYLLABLE KIYEOK-YEO-TIKEUT
       (?결 . "0xACB0") ; HANGUL SYLLABLE KIYEOK-YEO-RIEUL
       (?겸 . "0xACB8") ; HANGUL SYLLABLE KIYEOK-YEO-MIEUM
       (?겹 . "0xACB9") ; HANGUL SYLLABLE KIYEOK-YEO-PIEUP
       (?겻 . "0xACBB") ; HANGUL SYLLABLE KIYEOK-YEO-SIOS
       (?겼 . "0xACBC") ; HANGUL SYLLABLE KIYEOK-YEO-SSANGSIOS
       (?경 . "0xACBD") ; HANGUL SYLLABLE KIYEOK-YEO-IEUNG
       (?곁 . "0xACC1") ; HANGUL SYLLABLE KIYEOK-YEO-THIEUTH
       (?계 . "0xACC4") ; HANGUL SYLLABLE KIYEOK-YE
       (?곈 . "0xACC8") ; HANGUL SYLLABLE KIYEOK-YE-NIEUN
       (?곌 . "0xACCC") ; HANGUL SYLLABLE KIYEOK-YE-RIEUL
       (?곕 . "0xACD5") ; HANGUL SYLLABLE KIYEOK-YE-PIEUP
       (?곗 . "0xACD7") ; HANGUL SYLLABLE KIYEOK-YE-SIOS
       (?고 . "0xACE0") ; HANGUL SYLLABLE KIYEOK-O
       (?곡 . "0xACE1") ; HANGUL SYLLABLE KIYEOK-O-KIYEOK
       (?곤 . "0xACE4") ; HANGUL SYLLABLE KIYEOK-O-NIEUN
       (?곧 . "0xACE7") ; HANGUL SYLLABLE KIYEOK-O-TIKEUT
       (?골 . "0xACE8") ; HANGUL SYLLABLE KIYEOK-O-RIEUL
       (?곪 . "0xACEA") ; HANGUL SYLLABLE KIYEOK-O-RIEULMIEUM
       (?곬 . "0xACEC") ; HANGUL SYLLABLE KIYEOK-O-RIEULSIOS
       (?곯 . "0xACEF") ; HANGUL SYLLABLE KIYEOK-O-RIEULHIEUH
       (?곰 . "0xACF0") ; HANGUL SYLLABLE KIYEOK-O-MIEUM
       (?곱 . "0xACF1") ; HANGUL SYLLABLE KIYEOK-O-PIEUP
       (?곳 . "0xACF3") ; HANGUL SYLLABLE KIYEOK-O-SIOS
       (?공 . "0xACF5") ; HANGUL SYLLABLE KIYEOK-O-IEUNG
       (?곶 . "0xACF6") ; HANGUL SYLLABLE KIYEOK-O-CIEUC
       (?과 . "0xACFC") ; HANGUL SYLLABLE KIYEOK-WA
       (?곽 . "0xACFD") ; HANGUL SYLLABLE KIYEOK-WA-KIYEOK
       (?관 . "0xAD00") ; HANGUL SYLLABLE KIYEOK-WA-NIEUN
       (?괄 . "0xAD04") ; HANGUL SYLLABLE KIYEOK-WA-RIEUL
       (?괆 . "0xAD06") ; HANGUL SYLLABLE KIYEOK-WA-RIEULMIEUM
       (?괌 . "0xAD0C") ; HANGUL SYLLABLE KIYEOK-WA-MIEUM
       (?괍 . "0xAD0D") ; HANGUL SYLLABLE KIYEOK-WA-PIEUP
       (?괏 . "0xAD0F") ; HANGUL SYLLABLE KIYEOK-WA-SIOS
       (?광 . "0xAD11") ; HANGUL SYLLABLE KIYEOK-WA-IEUNG
       (?괘 . "0xAD18") ; HANGUL SYLLABLE KIYEOK-WAE
       (?괜 . "0xAD1C") ; HANGUL SYLLABLE KIYEOK-WAE-NIEUN
       (?괠 . "0xAD20") ; HANGUL SYLLABLE KIYEOK-WAE-RIEUL
       (?괩 . "0xAD29") ; HANGUL SYLLABLE KIYEOK-WAE-PIEUP
       (?괬 . "0xAD2C") ; HANGUL SYLLABLE KIYEOK-WAE-SSANGSIOS
       (?괭 . "0xAD2D") ; HANGUL SYLLABLE KIYEOK-WAE-IEUNG
       (?괴 . "0xAD34") ; HANGUL SYLLABLE KIYEOK-OE
       (?괵 . "0xAD35") ; HANGUL SYLLABLE KIYEOK-OE-KIYEOK
       (?괸 . "0xAD38") ; HANGUL SYLLABLE KIYEOK-OE-NIEUN
       (?괼 . "0xAD3C") ; HANGUL SYLLABLE KIYEOK-OE-RIEUL
       (?굄 . "0xAD44") ; HANGUL SYLLABLE KIYEOK-OE-MIEUM
       (?굅 . "0xAD45") ; HANGUL SYLLABLE KIYEOK-OE-PIEUP
       (?굇 . "0xAD47") ; HANGUL SYLLABLE KIYEOK-OE-SIOS
       (?굉 . "0xAD49") ; HANGUL SYLLABLE KIYEOK-OE-IEUNG
       (?교 . "0xAD50") ; HANGUL SYLLABLE KIYEOK-YO
       (?굔 . "0xAD54") ; HANGUL SYLLABLE KIYEOK-YO-NIEUN
       (?굘 . "0xAD58") ; HANGUL SYLLABLE KIYEOK-YO-RIEUL
       (?굡 . "0xAD61") ; HANGUL SYLLABLE KIYEOK-YO-PIEUP
       (?굣 . "0xAD63") ; HANGUL SYLLABLE KIYEOK-YO-SIOS
       (?구 . "0xAD6C") ; HANGUL SYLLABLE KIYEOK-U
       (?국 . "0xAD6D") ; HANGUL SYLLABLE KIYEOK-U-KIYEOK
       (?군 . "0xAD70") ; HANGUL SYLLABLE KIYEOK-U-NIEUN
       (?굳 . "0xAD73") ; HANGUL SYLLABLE KIYEOK-U-TIKEUT
       (?굴 . "0xAD74") ; HANGUL SYLLABLE KIYEOK-U-RIEUL
       (?굵 . "0xAD75") ; HANGUL SYLLABLE KIYEOK-U-RIEULKIYEOK
       (?굶 . "0xAD76") ; HANGUL SYLLABLE KIYEOK-U-RIEULMIEUM
       (?굻 . "0xAD7B") ; HANGUL SYLLABLE KIYEOK-U-RIEULHIEUH
       (?굼 . "0xAD7C") ; HANGUL SYLLABLE KIYEOK-U-MIEUM
       (?굽 . "0xAD7D") ; HANGUL SYLLABLE KIYEOK-U-PIEUP
       (?굿 . "0xAD7F") ; HANGUL SYLLABLE KIYEOK-U-SIOS
       (?궁 . "0xAD81") ; HANGUL SYLLABLE KIYEOK-U-IEUNG
       (?궂 . "0xAD82") ; HANGUL SYLLABLE KIYEOK-U-CIEUC
       (?궈 . "0xAD88") ; HANGUL SYLLABLE KIYEOK-WEO
       (?궉 . "0xAD89") ; HANGUL SYLLABLE KIYEOK-WEO-KIYEOK
       (?권 . "0xAD8C") ; HANGUL SYLLABLE KIYEOK-WEO-NIEUN
       (?궐 . "0xAD90") ; HANGUL SYLLABLE KIYEOK-WEO-RIEUL
       (?궜 . "0xAD9C") ; HANGUL SYLLABLE KIYEOK-WEO-SSANGSIOS
       (?궝 . "0xAD9D") ; HANGUL SYLLABLE KIYEOK-WEO-IEUNG
       (?궤 . "0xADA4") ; HANGUL SYLLABLE KIYEOK-WE
       (?궷 . "0xADB7") ; HANGUL SYLLABLE KIYEOK-WE-SIOS
       (?귀 . "0xADC0") ; HANGUL SYLLABLE KIYEOK-WI
       (?귁 . "0xADC1") ; HANGUL SYLLABLE KIYEOK-WI-KIYEOK
       (?귄 . "0xADC4") ; HANGUL SYLLABLE KIYEOK-WI-NIEUN
       (?귈 . "0xADC8") ; HANGUL SYLLABLE KIYEOK-WI-RIEUL
       (?귐 . "0xADD0") ; HANGUL SYLLABLE KIYEOK-WI-MIEUM
       (?귑 . "0xADD1") ; HANGUL SYLLABLE KIYEOK-WI-PIEUP
       (?귓 . "0xADD3") ; HANGUL SYLLABLE KIYEOK-WI-SIOS
       (?규 . "0xADDC") ; HANGUL SYLLABLE KIYEOK-YU
       (?균 . "0xADE0") ; HANGUL SYLLABLE KIYEOK-YU-NIEUN
       (?귤 . "0xADE4") ; HANGUL SYLLABLE KIYEOK-YU-RIEUL
       (?그 . "0xADF8") ; HANGUL SYLLABLE KIYEOK-EU
       (?극 . "0xADF9") ; HANGUL SYLLABLE KIYEOK-EU-KIYEOK
       (?근 . "0xADFC") ; HANGUL SYLLABLE KIYEOK-EU-NIEUN
       (?귿 . "0xADFF") ; HANGUL SYLLABLE KIYEOK-EU-TIKEUT
       (?글 . "0xAE00") ; HANGUL SYLLABLE KIYEOK-EU-RIEUL
       (?긁 . "0xAE01") ; HANGUL SYLLABLE KIYEOK-EU-RIEULKIYEOK
       (?금 . "0xAE08") ; HANGUL SYLLABLE KIYEOK-EU-MIEUM
       (?급 . "0xAE09") ; HANGUL SYLLABLE KIYEOK-EU-PIEUP
       (?긋 . "0xAE0B") ; HANGUL SYLLABLE KIYEOK-EU-SIOS
       (?긍 . "0xAE0D") ; HANGUL SYLLABLE KIYEOK-EU-IEUNG
       (?긔 . "0xAE14") ; HANGUL SYLLABLE KIYEOK-YI
       (?기 . "0xAE30") ; HANGUL SYLLABLE KIYEOK-I
       (?긱 . "0xAE31") ; HANGUL SYLLABLE KIYEOK-I-KIYEOK
       (?긴 . "0xAE34") ; HANGUL SYLLABLE KIYEOK-I-NIEUN
       (?긷 . "0xAE37") ; HANGUL SYLLABLE KIYEOK-I-TIKEUT
       (?길 . "0xAE38") ; HANGUL SYLLABLE KIYEOK-I-RIEUL
       (?긺 . "0xAE3A") ; HANGUL SYLLABLE KIYEOK-I-RIEULMIEUM
       (?김 . "0xAE40") ; HANGUL SYLLABLE KIYEOK-I-MIEUM
       (?깁 . "0xAE41") ; HANGUL SYLLABLE KIYEOK-I-PIEUP
       (?깃 . "0xAE43") ; HANGUL SYLLABLE KIYEOK-I-SIOS
       (?깅 . "0xAE45") ; HANGUL SYLLABLE KIYEOK-I-IEUNG
       (?깆 . "0xAE46") ; HANGUL SYLLABLE KIYEOK-I-CIEUC
       (?깊 . "0xAE4A") ; HANGUL SYLLABLE KIYEOK-I-PHIEUPH
       (?까 . "0xAE4C") ; HANGUL SYLLABLE SSANGKIYEOK-A
       (?깍 . "0xAE4D") ; HANGUL SYLLABLE SSANGKIYEOK-A-KIYEOK
       (?깎 . "0xAE4E") ; HANGUL SYLLABLE SSANGKIYEOK-A-SSANGKIYEOK
       (?깐 . "0xAE50") ; HANGUL SYLLABLE SSANGKIYEOK-A-NIEUN
       (?깔 . "0xAE54") ; HANGUL SYLLABLE SSANGKIYEOK-A-RIEUL
       (?깖 . "0xAE56") ; HANGUL SYLLABLE SSANGKIYEOK-A-RIEULMIEUM
       (?깜 . "0xAE5C") ; HANGUL SYLLABLE SSANGKIYEOK-A-MIEUM
       (?깝 . "0xAE5D") ; HANGUL SYLLABLE SSANGKIYEOK-A-PIEUP
       (?깟 . "0xAE5F") ; HANGUL SYLLABLE SSANGKIYEOK-A-SIOS
       (?깠 . "0xAE60") ; HANGUL SYLLABLE SSANGKIYEOK-A-SSANGSIOS
       (?깡 . "0xAE61") ; HANGUL SYLLABLE SSANGKIYEOK-A-IEUNG
       (?깥 . "0xAE65") ; HANGUL SYLLABLE SSANGKIYEOK-A-THIEUTH
       (?깨 . "0xAE68") ; HANGUL SYLLABLE SSANGKIYEOK-AE
       (?깩 . "0xAE69") ; HANGUL SYLLABLE SSANGKIYEOK-AE-KIYEOK
       (?깬 . "0xAE6C") ; HANGUL SYLLABLE SSANGKIYEOK-AE-NIEUN
       (?깰 . "0xAE70") ; HANGUL SYLLABLE SSANGKIYEOK-AE-RIEUL
       (?깸 . "0xAE78") ; HANGUL SYLLABLE SSANGKIYEOK-AE-MIEUM
       (?깹 . "0xAE79") ; HANGUL SYLLABLE SSANGKIYEOK-AE-PIEUP
       (?깻 . "0xAE7B") ; HANGUL SYLLABLE SSANGKIYEOK-AE-SIOS
       (?깼 . "0xAE7C") ; HANGUL SYLLABLE SSANGKIYEOK-AE-SSANGSIOS
       (?깽 . "0xAE7D") ; HANGUL SYLLABLE SSANGKIYEOK-AE-IEUNG
       (?꺄 . "0xAE84") ; HANGUL SYLLABLE SSANGKIYEOK-YA
       (?꺅 . "0xAE85") ; HANGUL SYLLABLE SSANGKIYEOK-YA-KIYEOK
       (?꺌 . "0xAE8C") ; HANGUL SYLLABLE SSANGKIYEOK-YA-RIEUL
       (?꺼 . "0xAEBC") ; HANGUL SYLLABLE SSANGKIYEOK-EO
       (?꺽 . "0xAEBD") ; HANGUL SYLLABLE SSANGKIYEOK-EO-KIYEOK
       (?꺾 . "0xAEBE") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SSANGKIYEOK
       (?껀 . "0xAEC0") ; HANGUL SYLLABLE SSANGKIYEOK-EO-NIEUN
       (?껄 . "0xAEC4") ; HANGUL SYLLABLE SSANGKIYEOK-EO-RIEUL
       (?껌 . "0xAECC") ; HANGUL SYLLABLE SSANGKIYEOK-EO-MIEUM
       (?껍 . "0xAECD") ; HANGUL SYLLABLE SSANGKIYEOK-EO-PIEUP
       (?껏 . "0xAECF") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SIOS
       (?껐 . "0xAED0") ; HANGUL SYLLABLE SSANGKIYEOK-EO-SSANGSIOS
       (?껑 . "0xAED1") ; HANGUL SYLLABLE SSANGKIYEOK-EO-IEUNG
       (?께 . "0xAED8") ; HANGUL SYLLABLE SSANGKIYEOK-E
       (?껙 . "0xAED9") ; HANGUL SYLLABLE SSANGKIYEOK-E-KIYEOK
       (?껜 . "0xAEDC") ; HANGUL SYLLABLE SSANGKIYEOK-E-NIEUN
       (?껨 . "0xAEE8") ; HANGUL SYLLABLE SSANGKIYEOK-E-MIEUM
       (?껫 . "0xAEEB") ; HANGUL SYLLABLE SSANGKIYEOK-E-SIOS
       (?껭 . "0xAEED") ; HANGUL SYLLABLE SSANGKIYEOK-E-IEUNG
       (?껴 . "0xAEF4") ; HANGUL SYLLABLE SSANGKIYEOK-YEO
       (?껸 . "0xAEF8") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-NIEUN
       (?껼 . "0xAEFC") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-RIEUL
       (?꼇 . "0xAF07") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-SIOS
       (?꼈 . "0xAF08") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-SSANGSIOS
       (?꼍 . "0xAF0D") ; HANGUL SYLLABLE SSANGKIYEOK-YEO-THIEUTH
       (?꼐 . "0xAF10") ; HANGUL SYLLABLE SSANGKIYEOK-YE
       (?꼬 . "0xAF2C") ; HANGUL SYLLABLE SSANGKIYEOK-O
       (?꼭 . "0xAF2D") ; HANGUL SYLLABLE SSANGKIYEOK-O-KIYEOK
       (?꼰 . "0xAF30") ; HANGUL SYLLABLE SSANGKIYEOK-O-NIEUN
       (?꼲 . "0xAF32") ; HANGUL SYLLABLE SSANGKIYEOK-O-NIEUNHIEUH
       (?꼴 . "0xAF34") ; HANGUL SYLLABLE SSANGKIYEOK-O-RIEUL
       (?꼼 . "0xAF3C") ; HANGUL SYLLABLE SSANGKIYEOK-O-MIEUM
       (?꼽 . "0xAF3D") ; HANGUL SYLLABLE SSANGKIYEOK-O-PIEUP
       (?꼿 . "0xAF3F") ; HANGUL SYLLABLE SSANGKIYEOK-O-SIOS
       (?꽁 . "0xAF41") ; HANGUL SYLLABLE SSANGKIYEOK-O-IEUNG
       (?꽂 . "0xAF42") ; HANGUL SYLLABLE SSANGKIYEOK-O-CIEUC
       (?꽃 . "0xAF43") ; HANGUL SYLLABLE SSANGKIYEOK-O-CHIEUCH
       (?꽈 . "0xAF48") ; HANGUL SYLLABLE SSANGKIYEOK-WA
       (?꽉 . "0xAF49") ; HANGUL SYLLABLE SSANGKIYEOK-WA-KIYEOK
       (?꽐 . "0xAF50") ; HANGUL SYLLABLE SSANGKIYEOK-WA-RIEUL
       (?꽜 . "0xAF5C") ; HANGUL SYLLABLE SSANGKIYEOK-WA-SSANGSIOS
       (?꽝 . "0xAF5D") ; HANGUL SYLLABLE SSANGKIYEOK-WA-IEUNG
       (?꽤 . "0xAF64") ; HANGUL SYLLABLE SSANGKIYEOK-WAE
       (?꽥 . "0xAF65") ; HANGUL SYLLABLE SSANGKIYEOK-WAE-KIYEOK
       (?꽹 . "0xAF79") ; HANGUL SYLLABLE SSANGKIYEOK-WAE-IEUNG
       (?꾀 . "0xAF80") ; HANGUL SYLLABLE SSANGKIYEOK-OE
       (?꾄 . "0xAF84") ; HANGUL SYLLABLE SSANGKIYEOK-OE-NIEUN
       (?꾈 . "0xAF88") ; HANGUL SYLLABLE SSANGKIYEOK-OE-RIEUL
       (?꾐 . "0xAF90") ; HANGUL SYLLABLE SSANGKIYEOK-OE-MIEUM
       (?꾑 . "0xAF91") ; HANGUL SYLLABLE SSANGKIYEOK-OE-PIEUP
       (?꾕 . "0xAF95") ; HANGUL SYLLABLE SSANGKIYEOK-OE-IEUNG
       (?꾜 . "0xAF9C") ; HANGUL SYLLABLE SSANGKIYEOK-YO
       (?꾸 . "0xAFB8") ; HANGUL SYLLABLE SSANGKIYEOK-U
       (?꾹 . "0xAFB9") ; HANGUL SYLLABLE SSANGKIYEOK-U-KIYEOK
       (?꾼 . "0xAFBC") ; HANGUL SYLLABLE SSANGKIYEOK-U-NIEUN
       (?꿀 . "0xAFC0") ; HANGUL SYLLABLE SSANGKIYEOK-U-RIEUL
       (?꿇 . "0xAFC7") ; HANGUL SYLLABLE SSANGKIYEOK-U-RIEULHIEUH
       (?꿈 . "0xAFC8") ; HANGUL SYLLABLE SSANGKIYEOK-U-MIEUM
       (?꿉 . "0xAFC9") ; HANGUL SYLLABLE SSANGKIYEOK-U-PIEUP
       (?꿋 . "0xAFCB") ; HANGUL SYLLABLE SSANGKIYEOK-U-SIOS
       (?꿍 . "0xAFCD") ; HANGUL SYLLABLE SSANGKIYEOK-U-IEUNG
       (?꿎 . "0xAFCE") ; HANGUL SYLLABLE SSANGKIYEOK-U-CIEUC
       (?꿔 . "0xAFD4") ; HANGUL SYLLABLE SSANGKIYEOK-WEO
       (?꿜 . "0xAFDC") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-RIEUL
       (?꿨 . "0xAFE8") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-SSANGSIOS
       (?꿩 . "0xAFE9") ; HANGUL SYLLABLE SSANGKIYEOK-WEO-IEUNG
       (?꿰 . "0xAFF0") ; HANGUL SYLLABLE SSANGKIYEOK-WE
       (?꿱 . "0xAFF1") ; HANGUL SYLLABLE SSANGKIYEOK-WE-KIYEOK
       (?꿴 . "0xAFF4") ; HANGUL SYLLABLE SSANGKIYEOK-WE-NIEUN
       (?꿸 . "0xAFF8") ; HANGUL SYLLABLE SSANGKIYEOK-WE-RIEUL
       (?뀀 . "0xB000") ; HANGUL SYLLABLE SSANGKIYEOK-WE-MIEUM
       (?뀁 . "0xB001") ; HANGUL SYLLABLE SSANGKIYEOK-WE-PIEUP
       (?뀄 . "0xB004") ; HANGUL SYLLABLE SSANGKIYEOK-WE-SSANGSIOS
       (?뀌 . "0xB00C") ; HANGUL SYLLABLE SSANGKIYEOK-WI
       (?뀐 . "0xB010") ; HANGUL SYLLABLE SSANGKIYEOK-WI-NIEUN
       (?뀔 . "0xB014") ; HANGUL SYLLABLE SSANGKIYEOK-WI-RIEUL
       (?뀜 . "0xB01C") ; HANGUL SYLLABLE SSANGKIYEOK-WI-MIEUM
       (?뀝 . "0xB01D") ; HANGUL SYLLABLE SSANGKIYEOK-WI-PIEUP
       (?뀨 . "0xB028") ; HANGUL SYLLABLE SSANGKIYEOK-YU
       (?끄 . "0xB044") ; HANGUL SYLLABLE SSANGKIYEOK-EU
       (?끅 . "0xB045") ; HANGUL SYLLABLE SSANGKIYEOK-EU-KIYEOK
       (?끈 . "0xB048") ; HANGUL SYLLABLE SSANGKIYEOK-EU-NIEUN
       (?끊 . "0xB04A") ; HANGUL SYLLABLE SSANGKIYEOK-EU-NIEUNHIEUH
       (?끌 . "0xB04C") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEUL
       (?끎 . "0xB04E") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEULMIEUM
       (?끓 . "0xB053") ; HANGUL SYLLABLE SSANGKIYEOK-EU-RIEULHIEUH
       (?끔 . "0xB054") ; HANGUL SYLLABLE SSANGKIYEOK-EU-MIEUM
       (?끕 . "0xB055") ; HANGUL SYLLABLE SSANGKIYEOK-EU-PIEUP
       (?끗 . "0xB057") ; HANGUL SYLLABLE SSANGKIYEOK-EU-SIOS
       (?끙 . "0xB059") ; HANGUL SYLLABLE SSANGKIYEOK-EU-IEUNG
       (?끝 . "0xB05D") ; HANGUL SYLLABLE SSANGKIYEOK-EU-THIEUTH
       (?끼 . "0xB07C") ; HANGUL SYLLABLE SSANGKIYEOK-I
       (?끽 . "0xB07D") ; HANGUL SYLLABLE SSANGKIYEOK-I-KIYEOK
       (?낀 . "0xB080") ; HANGUL SYLLABLE SSANGKIYEOK-I-NIEUN
       (?낄 . "0xB084") ; HANGUL SYLLABLE SSANGKIYEOK-I-RIEUL
       (?낌 . "0xB08C") ; HANGUL SYLLABLE SSANGKIYEOK-I-MIEUM
       (?낍 . "0xB08D") ; HANGUL SYLLABLE SSANGKIYEOK-I-PIEUP
       (?낏 . "0xB08F") ; HANGUL SYLLABLE SSANGKIYEOK-I-SIOS
       (?낑 . "0xB091") ; HANGUL SYLLABLE SSANGKIYEOK-I-IEUNG
       (?나 . "0xB098") ; HANGUL SYLLABLE NIEUN-A
       (?낙 . "0xB099") ; HANGUL SYLLABLE NIEUN-A-KIYEOK
       (?낚 . "0xB09A") ; HANGUL SYLLABLE NIEUN-A-SSANGKIYEOK
       (?난 . "0xB09C") ; HANGUL SYLLABLE NIEUN-A-NIEUN
       (?낟 . "0xB09F") ; HANGUL SYLLABLE NIEUN-A-TIKEUT
       (?날 . "0xB0A0") ; HANGUL SYLLABLE NIEUN-A-RIEUL
       (?낡 . "0xB0A1") ; HANGUL SYLLABLE NIEUN-A-RIEULKIYEOK
       (?낢 . "0xB0A2") ; HANGUL SYLLABLE NIEUN-A-RIEULMIEUM
       (?남 . "0xB0A8") ; HANGUL SYLLABLE NIEUN-A-MIEUM
       (?납 . "0xB0A9") ; HANGUL SYLLABLE NIEUN-A-PIEUP
       (?낫 . "0xB0AB") ; HANGUL SYLLABLE NIEUN-A-SIOS
       (?났 . "0xB0AC") ; HANGUL SYLLABLE NIEUN-A-SSANGSIOS
       (?낭 . "0xB0AD") ; HANGUL SYLLABLE NIEUN-A-IEUNG
       (?낮 . "0xB0AE") ; HANGUL SYLLABLE NIEUN-A-CIEUC
       (?낯 . "0xB0AF") ; HANGUL SYLLABLE NIEUN-A-CHIEUCH
       (?낱 . "0xB0B1") ; HANGUL SYLLABLE NIEUN-A-THIEUTH
       (?낳 . "0xB0B3") ; HANGUL SYLLABLE NIEUN-A-HIEUH
       (?내 . "0xB0B4") ; HANGUL SYLLABLE NIEUN-AE
       (?낵 . "0xB0B5") ; HANGUL SYLLABLE NIEUN-AE-KIYEOK
       (?낸 . "0xB0B8") ; HANGUL SYLLABLE NIEUN-AE-NIEUN
       (?낼 . "0xB0BC") ; HANGUL SYLLABLE NIEUN-AE-RIEUL
       (?냄 . "0xB0C4") ; HANGUL SYLLABLE NIEUN-AE-MIEUM
       (?냅 . "0xB0C5") ; HANGUL SYLLABLE NIEUN-AE-PIEUP
       (?냇 . "0xB0C7") ; HANGUL SYLLABLE NIEUN-AE-SIOS
       (?냈 . "0xB0C8") ; HANGUL SYLLABLE NIEUN-AE-SSANGSIOS
       (?냉 . "0xB0C9") ; HANGUL SYLLABLE NIEUN-AE-IEUNG
       (?냐 . "0xB0D0") ; HANGUL SYLLABLE NIEUN-YA
       (?냑 . "0xB0D1") ; HANGUL SYLLABLE NIEUN-YA-KIYEOK
       (?냔 . "0xB0D4") ; HANGUL SYLLABLE NIEUN-YA-NIEUN
       (?냘 . "0xB0D8") ; HANGUL SYLLABLE NIEUN-YA-RIEUL
       (?냠 . "0xB0E0") ; HANGUL SYLLABLE NIEUN-YA-MIEUM
       (?냥 . "0xB0E5") ; HANGUL SYLLABLE NIEUN-YA-IEUNG
       (?너 . "0xB108") ; HANGUL SYLLABLE NIEUN-EO
       (?넉 . "0xB109") ; HANGUL SYLLABLE NIEUN-EO-KIYEOK
       (?넋 . "0xB10B") ; HANGUL SYLLABLE NIEUN-EO-KIYEOKSIOS
       (?넌 . "0xB10C") ; HANGUL SYLLABLE NIEUN-EO-NIEUN
       (?널 . "0xB110") ; HANGUL SYLLABLE NIEUN-EO-RIEUL
       (?넒 . "0xB112") ; HANGUL SYLLABLE NIEUN-EO-RIEULMIEUM
       (?넓 . "0xB113") ; HANGUL SYLLABLE NIEUN-EO-RIEULPIEUP
       (?넘 . "0xB118") ; HANGUL SYLLABLE NIEUN-EO-MIEUM
       (?넙 . "0xB119") ; HANGUL SYLLABLE NIEUN-EO-PIEUP
       (?넛 . "0xB11B") ; HANGUL SYLLABLE NIEUN-EO-SIOS
       (?넜 . "0xB11C") ; HANGUL SYLLABLE NIEUN-EO-SSANGSIOS
       (?넝 . "0xB11D") ; HANGUL SYLLABLE NIEUN-EO-IEUNG
       (?넣 . "0xB123") ; HANGUL SYLLABLE NIEUN-EO-HIEUH
       (?네 . "0xB124") ; HANGUL SYLLABLE NIEUN-E
       (?넥 . "0xB125") ; HANGUL SYLLABLE NIEUN-E-KIYEOK
       (?넨 . "0xB128") ; HANGUL SYLLABLE NIEUN-E-NIEUN
       (?넬 . "0xB12C") ; HANGUL SYLLABLE NIEUN-E-RIEUL
       (?넴 . "0xB134") ; HANGUL SYLLABLE NIEUN-E-MIEUM
       (?넵 . "0xB135") ; HANGUL SYLLABLE NIEUN-E-PIEUP
       (?넷 . "0xB137") ; HANGUL SYLLABLE NIEUN-E-SIOS
       (?넸 . "0xB138") ; HANGUL SYLLABLE NIEUN-E-SSANGSIOS
       (?넹 . "0xB139") ; HANGUL SYLLABLE NIEUN-E-IEUNG
       (?녀 . "0xB140") ; HANGUL SYLLABLE NIEUN-YEO
       (?녁 . "0xB141") ; HANGUL SYLLABLE NIEUN-YEO-KIYEOK
       (?년 . "0xB144") ; HANGUL SYLLABLE NIEUN-YEO-NIEUN
       (?녈 . "0xB148") ; HANGUL SYLLABLE NIEUN-YEO-RIEUL
       (?념 . "0xB150") ; HANGUL SYLLABLE NIEUN-YEO-MIEUM
       (?녑 . "0xB151") ; HANGUL SYLLABLE NIEUN-YEO-PIEUP
       (?녔 . "0xB154") ; HANGUL SYLLABLE NIEUN-YEO-SSANGSIOS
       (?녕 . "0xB155") ; HANGUL SYLLABLE NIEUN-YEO-IEUNG
       (?녘 . "0xB158") ; HANGUL SYLLABLE NIEUN-YEO-KHIEUKH
       (?녜 . "0xB15C") ; HANGUL SYLLABLE NIEUN-YE
       (?녠 . "0xB160") ; HANGUL SYLLABLE NIEUN-YE-NIEUN
       (?노 . "0xB178") ; HANGUL SYLLABLE NIEUN-O
       (?녹 . "0xB179") ; HANGUL SYLLABLE NIEUN-O-KIYEOK
       (?논 . "0xB17C") ; HANGUL SYLLABLE NIEUN-O-NIEUN
       (?놀 . "0xB180") ; HANGUL SYLLABLE NIEUN-O-RIEUL
       (?놂 . "0xB182") ; HANGUL SYLLABLE NIEUN-O-RIEULMIEUM
       (?놈 . "0xB188") ; HANGUL SYLLABLE NIEUN-O-MIEUM
       (?놉 . "0xB189") ; HANGUL SYLLABLE NIEUN-O-PIEUP
       (?놋 . "0xB18B") ; HANGUL SYLLABLE NIEUN-O-SIOS
       (?농 . "0xB18D") ; HANGUL SYLLABLE NIEUN-O-IEUNG
       (?높 . "0xB192") ; HANGUL SYLLABLE NIEUN-O-PHIEUPH
       (?놓 . "0xB193") ; HANGUL SYLLABLE NIEUN-O-HIEUH
       (?놔 . "0xB194") ; HANGUL SYLLABLE NIEUN-WA
       (?놘 . "0xB198") ; HANGUL SYLLABLE NIEUN-WA-NIEUN
       (?놜 . "0xB19C") ; HANGUL SYLLABLE NIEUN-WA-RIEUL
       (?놨 . "0xB1A8") ; HANGUL SYLLABLE NIEUN-WA-SSANGSIOS
       (?뇌 . "0xB1CC") ; HANGUL SYLLABLE NIEUN-OE
       (?뇐 . "0xB1D0") ; HANGUL SYLLABLE NIEUN-OE-NIEUN
       (?뇔 . "0xB1D4") ; HANGUL SYLLABLE NIEUN-OE-RIEUL
       (?뇜 . "0xB1DC") ; HANGUL SYLLABLE NIEUN-OE-MIEUM
       (?뇝 . "0xB1DD") ; HANGUL SYLLABLE NIEUN-OE-PIEUP
       (?뇟 . "0xB1DF") ; HANGUL SYLLABLE NIEUN-OE-SIOS
       (?뇨 . "0xB1E8") ; HANGUL SYLLABLE NIEUN-YO
       (?뇩 . "0xB1E9") ; HANGUL SYLLABLE NIEUN-YO-KIYEOK
       (?뇬 . "0xB1EC") ; HANGUL SYLLABLE NIEUN-YO-NIEUN
       (?뇰 . "0xB1F0") ; HANGUL SYLLABLE NIEUN-YO-RIEUL
       (?뇹 . "0xB1F9") ; HANGUL SYLLABLE NIEUN-YO-PIEUP
       (?뇻 . "0xB1FB") ; HANGUL SYLLABLE NIEUN-YO-SIOS
       (?뇽 . "0xB1FD") ; HANGUL SYLLABLE NIEUN-YO-IEUNG
       (?누 . "0xB204") ; HANGUL SYLLABLE NIEUN-U
       (?눅 . "0xB205") ; HANGUL SYLLABLE NIEUN-U-KIYEOK
       (?눈 . "0xB208") ; HANGUL SYLLABLE NIEUN-U-NIEUN
       (?눋 . "0xB20B") ; HANGUL SYLLABLE NIEUN-U-TIKEUT
       (?눌 . "0xB20C") ; HANGUL SYLLABLE NIEUN-U-RIEUL
       (?눔 . "0xB214") ; HANGUL SYLLABLE NIEUN-U-MIEUM
       (?눕 . "0xB215") ; HANGUL SYLLABLE NIEUN-U-PIEUP
       (?눗 . "0xB217") ; HANGUL SYLLABLE NIEUN-U-SIOS
       (?눙 . "0xB219") ; HANGUL SYLLABLE NIEUN-U-IEUNG
       (?눠 . "0xB220") ; HANGUL SYLLABLE NIEUN-WEO
       (?눴 . "0xB234") ; HANGUL SYLLABLE NIEUN-WEO-SSANGSIOS
       (?눼 . "0xB23C") ; HANGUL SYLLABLE NIEUN-WE
       (?뉘 . "0xB258") ; HANGUL SYLLABLE NIEUN-WI
       (?뉜 . "0xB25C") ; HANGUL SYLLABLE NIEUN-WI-NIEUN
       (?뉠 . "0xB260") ; HANGUL SYLLABLE NIEUN-WI-RIEUL
       (?뉨 . "0xB268") ; HANGUL SYLLABLE NIEUN-WI-MIEUM
       (?뉩 . "0xB269") ; HANGUL SYLLABLE NIEUN-WI-PIEUP
       (?뉴 . "0xB274") ; HANGUL SYLLABLE NIEUN-YU
       (?뉵 . "0xB275") ; HANGUL SYLLABLE NIEUN-YU-KIYEOK
       (?뉼 . "0xB27C") ; HANGUL SYLLABLE NIEUN-YU-RIEUL
       (?늄 . "0xB284") ; HANGUL SYLLABLE NIEUN-YU-MIEUM
       (?늅 . "0xB285") ; HANGUL SYLLABLE NIEUN-YU-PIEUP
       (?늉 . "0xB289") ; HANGUL SYLLABLE NIEUN-YU-IEUNG
       (?느 . "0xB290") ; HANGUL SYLLABLE NIEUN-EU
       (?늑 . "0xB291") ; HANGUL SYLLABLE NIEUN-EU-KIYEOK
       (?는 . "0xB294") ; HANGUL SYLLABLE NIEUN-EU-NIEUN
       (?늘 . "0xB298") ; HANGUL SYLLABLE NIEUN-EU-RIEUL
       (?늙 . "0xB299") ; HANGUL SYLLABLE NIEUN-EU-RIEULKIYEOK
       (?늚 . "0xB29A") ; HANGUL SYLLABLE NIEUN-EU-RIEULMIEUM
       (?늠 . "0xB2A0") ; HANGUL SYLLABLE NIEUN-EU-MIEUM
       (?늡 . "0xB2A1") ; HANGUL SYLLABLE NIEUN-EU-PIEUP
       (?늣 . "0xB2A3") ; HANGUL SYLLABLE NIEUN-EU-SIOS
       (?능 . "0xB2A5") ; HANGUL SYLLABLE NIEUN-EU-IEUNG
       (?늦 . "0xB2A6") ; HANGUL SYLLABLE NIEUN-EU-CIEUC
       (?늪 . "0xB2AA") ; HANGUL SYLLABLE NIEUN-EU-PHIEUPH
       (?늬 . "0xB2AC") ; HANGUL SYLLABLE NIEUN-YI
       (?늰 . "0xB2B0") ; HANGUL SYLLABLE NIEUN-YI-NIEUN
       (?늴 . "0xB2B4") ; HANGUL SYLLABLE NIEUN-YI-RIEUL
       (?니 . "0xB2C8") ; HANGUL SYLLABLE NIEUN-I
       (?닉 . "0xB2C9") ; HANGUL SYLLABLE NIEUN-I-KIYEOK
       (?닌 . "0xB2CC") ; HANGUL SYLLABLE NIEUN-I-NIEUN
       (?닐 . "0xB2D0") ; HANGUL SYLLABLE NIEUN-I-RIEUL
       (?닒 . "0xB2D2") ; HANGUL SYLLABLE NIEUN-I-RIEULMIEUM-<3/22/95>
       (?님 . "0xB2D8") ; HANGUL SYLLABLE NIEUN-I-MIEUM
       (?닙 . "0xB2D9") ; HANGUL SYLLABLE NIEUN-I-PIEUP
       (?닛 . "0xB2DB") ; HANGUL SYLLABLE NIEUN-I-SIOS
       (?닝 . "0xB2DD") ; HANGUL SYLLABLE NIEUN-I-IEUNG
       (?닢 . "0xB2E2") ; HANGUL SYLLABLE NIEUN-I-PHIEUPH
       (?다 . "0xB2E4") ; HANGUL SYLLABLE TIKEUT-A
       (?닥 . "0xB2E5") ; HANGUL SYLLABLE TIKEUT-A-KIYEOK
       (?닦 . "0xB2E6") ; HANGUL SYLLABLE TIKEUT-A-SSANGKIYEOK
       (?단 . "0xB2E8") ; HANGUL SYLLABLE TIKEUT-A-NIEUN
       (?닫 . "0xB2EB") ; HANGUL SYLLABLE TIKEUT-A-TIKEUT
       (?달 . "0xB2EC") ; HANGUL SYLLABLE TIKEUT-A-RIEUL
       (?닭 . "0xB2ED") ; HANGUL SYLLABLE TIKEUT-A-RIEULKIYEOK
       (?닮 . "0xB2EE") ; HANGUL SYLLABLE TIKEUT-A-RIEULMIEUM
       (?닯 . "0xB2EF") ; HANGUL SYLLABLE TIKEUT-A-RIEULPIEUP
       (?닳 . "0xB2F3") ; HANGUL SYLLABLE TIKEUT-A-RIEULHIEUH
       (?담 . "0xB2F4") ; HANGUL SYLLABLE TIKEUT-A-MIEUM
       (?답 . "0xB2F5") ; HANGUL SYLLABLE TIKEUT-A-PIEUP
       (?닷 . "0xB2F7") ; HANGUL SYLLABLE TIKEUT-A-SIOS
       (?닸 . "0xB2F8") ; HANGUL SYLLABLE TIKEUT-A-SSANGSIOS
       (?당 . "0xB2F9") ; HANGUL SYLLABLE TIKEUT-A-IEUNG
       (?닺 . "0xB2FA") ; HANGUL SYLLABLE TIKEUT-A-CIEUC
       (?닻 . "0xB2FB") ; HANGUL SYLLABLE TIKEUT-A-CHIEUCH
       (?닿 . "0xB2FF") ; HANGUL SYLLABLE TIKEUT-A-HIEUH
       (?대 . "0xB300") ; HANGUL SYLLABLE TIKEUT-AE
       (?댁 . "0xB301") ; HANGUL SYLLABLE TIKEUT-AE-KIYEOK
       (?댄 . "0xB304") ; HANGUL SYLLABLE TIKEUT-AE-NIEUN
       (?댈 . "0xB308") ; HANGUL SYLLABLE TIKEUT-AE-RIEUL
       (?댐 . "0xB310") ; HANGUL SYLLABLE TIKEUT-AE-MIEUM
       (?댑 . "0xB311") ; HANGUL SYLLABLE TIKEUT-AE-PIEUP
       (?댓 . "0xB313") ; HANGUL SYLLABLE TIKEUT-AE-SIOS
       (?댔 . "0xB314") ; HANGUL SYLLABLE TIKEUT-AE-SSANGSIOS
       (?댕 . "0xB315") ; HANGUL SYLLABLE TIKEUT-AE-IEUNG
       (?댜 . "0xB31C") ; HANGUL SYLLABLE TIKEUT-YA
       (?더 . "0xB354") ; HANGUL SYLLABLE TIKEUT-EO
       (?덕 . "0xB355") ; HANGUL SYLLABLE TIKEUT-EO-KIYEOK
       (?덖 . "0xB356") ; HANGUL SYLLABLE TIKEUT-EO-SSANGKIYEOK
       (?던 . "0xB358") ; HANGUL SYLLABLE TIKEUT-EO-NIEUN
       (?덛 . "0xB35B") ; HANGUL SYLLABLE TIKEUT-EO-TIKEUT
       (?덜 . "0xB35C") ; HANGUL SYLLABLE TIKEUT-EO-RIEUL
       (?덞 . "0xB35E") ; HANGUL SYLLABLE TIKEUT-EO-RIEULMIEUM
       (?덟 . "0xB35F") ; HANGUL SYLLABLE TIKEUT-EO-RIEULPIEUP
       (?덤 . "0xB364") ; HANGUL SYLLABLE TIKEUT-EO-MIEUM
       (?덥 . "0xB365") ; HANGUL SYLLABLE TIKEUT-EO-PIEUP
       (?덧 . "0xB367") ; HANGUL SYLLABLE TIKEUT-EO-SIOS
       (?덩 . "0xB369") ; HANGUL SYLLABLE TIKEUT-EO-IEUNG
       (?덫 . "0xB36B") ; HANGUL SYLLABLE TIKEUT-EO-CHIEUCH
       (?덮 . "0xB36E") ; HANGUL SYLLABLE TIKEUT-EO-PHIEUPH
       (?데 . "0xB370") ; HANGUL SYLLABLE TIKEUT-E
       (?덱 . "0xB371") ; HANGUL SYLLABLE TIKEUT-E-KIYEOK
       (?덴 . "0xB374") ; HANGUL SYLLABLE TIKEUT-E-NIEUN
       (?델 . "0xB378") ; HANGUL SYLLABLE TIKEUT-E-RIEUL
       (?뎀 . "0xB380") ; HANGUL SYLLABLE TIKEUT-E-MIEUM
       (?뎁 . "0xB381") ; HANGUL SYLLABLE TIKEUT-E-PIEUP
       (?뎃 . "0xB383") ; HANGUL SYLLABLE TIKEUT-E-SIOS
       (?뎄 . "0xB384") ; HANGUL SYLLABLE TIKEUT-E-SSANGSIOS
       (?뎅 . "0xB385") ; HANGUL SYLLABLE TIKEUT-E-IEUNG
       (?뎌 . "0xB38C") ; HANGUL SYLLABLE TIKEUT-YEO
       (?뎐 . "0xB390") ; HANGUL SYLLABLE TIKEUT-YEO-NIEUN
       (?뎔 . "0xB394") ; HANGUL SYLLABLE TIKEUT-YEO-RIEUL
       (?뎠 . "0xB3A0") ; HANGUL SYLLABLE TIKEUT-YEO-SSANGSIOS
       (?뎡 . "0xB3A1") ; HANGUL SYLLABLE TIKEUT-YEO-IEUNG
       (?뎨 . "0xB3A8") ; HANGUL SYLLABLE TIKEUT-YE
       (?뎬 . "0xB3AC") ; HANGUL SYLLABLE TIKEUT-YE-NIEUN
       (?도 . "0xB3C4") ; HANGUL SYLLABLE TIKEUT-O
       (?독 . "0xB3C5") ; HANGUL SYLLABLE TIKEUT-O-KIYEOK
       (?돈 . "0xB3C8") ; HANGUL SYLLABLE TIKEUT-O-NIEUN
       (?돋 . "0xB3CB") ; HANGUL SYLLABLE TIKEUT-O-TIKEUT
       (?돌 . "0xB3CC") ; HANGUL SYLLABLE TIKEUT-O-RIEUL
       (?돎 . "0xB3CE") ; HANGUL SYLLABLE TIKEUT-O-RIEULMIEUM
       (?돐 . "0xB3D0") ; HANGUL SYLLABLE TIKEUT-O-RIEULSIOS
       (?돔 . "0xB3D4") ; HANGUL SYLLABLE TIKEUT-O-MIEUM
       (?돕 . "0xB3D5") ; HANGUL SYLLABLE TIKEUT-O-PIEUP
       (?돗 . "0xB3D7") ; HANGUL SYLLABLE TIKEUT-O-SIOS
       (?동 . "0xB3D9") ; HANGUL SYLLABLE TIKEUT-O-IEUNG
       (?돛 . "0xB3DB") ; HANGUL SYLLABLE TIKEUT-O-CHIEUCH
       (?돝 . "0xB3DD") ; HANGUL SYLLABLE TIKEUT-O-THIEUTH
       (?돠 . "0xB3E0") ; HANGUL SYLLABLE TIKEUT-WA
       (?돤 . "0xB3E4") ; HANGUL SYLLABLE TIKEUT-WA-NIEUN
       (?돨 . "0xB3E8") ; HANGUL SYLLABLE TIKEUT-WA-RIEUL
       (?돼 . "0xB3FC") ; HANGUL SYLLABLE TIKEUT-WAE
       (?됐 . "0xB410") ; HANGUL SYLLABLE TIKEUT-WAE-SSANGSIOS
       (?되 . "0xB418") ; HANGUL SYLLABLE TIKEUT-OE
       (?된 . "0xB41C") ; HANGUL SYLLABLE TIKEUT-OE-NIEUN
       (?될 . "0xB420") ; HANGUL SYLLABLE TIKEUT-OE-RIEUL
       (?됨 . "0xB428") ; HANGUL SYLLABLE TIKEUT-OE-MIEUM
       (?됩 . "0xB429") ; HANGUL SYLLABLE TIKEUT-OE-PIEUP
       (?됫 . "0xB42B") ; HANGUL SYLLABLE TIKEUT-OE-SIOS
       (?됴 . "0xB434") ; HANGUL SYLLABLE TIKEUT-YO
       (?두 . "0xB450") ; HANGUL SYLLABLE TIKEUT-U
       (?둑 . "0xB451") ; HANGUL SYLLABLE TIKEUT-U-KIYEOK
       (?둔 . "0xB454") ; HANGUL SYLLABLE TIKEUT-U-NIEUN
       (?둘 . "0xB458") ; HANGUL SYLLABLE TIKEUT-U-RIEUL
       (?둠 . "0xB460") ; HANGUL SYLLABLE TIKEUT-U-MIEUM
       (?둡 . "0xB461") ; HANGUL SYLLABLE TIKEUT-U-PIEUP
       (?둣 . "0xB463") ; HANGUL SYLLABLE TIKEUT-U-SIOS
       (?둥 . "0xB465") ; HANGUL SYLLABLE TIKEUT-U-IEUNG
       (?둬 . "0xB46C") ; HANGUL SYLLABLE TIKEUT-WEO
       (?뒀 . "0xB480") ; HANGUL SYLLABLE TIKEUT-WEO-SSANGSIOS
       (?뒈 . "0xB488") ; HANGUL SYLLABLE TIKEUT-WE
       (?뒝 . "0xB49D") ; HANGUL SYLLABLE TIKEUT-WE-IEUNG
       (?뒤 . "0xB4A4") ; HANGUL SYLLABLE TIKEUT-WI
       (?뒨 . "0xB4A8") ; HANGUL SYLLABLE TIKEUT-WI-NIEUN
       (?뒬 . "0xB4AC") ; HANGUL SYLLABLE TIKEUT-WI-RIEUL
       (?뒵 . "0xB4B5") ; HANGUL SYLLABLE TIKEUT-WI-PIEUP
       (?뒷 . "0xB4B7") ; HANGUL SYLLABLE TIKEUT-WI-SIOS
       (?뒹 . "0xB4B9") ; HANGUL SYLLABLE TIKEUT-WI-IEUNG
       (?듀 . "0xB4C0") ; HANGUL SYLLABLE TIKEUT-YU
       (?듄 . "0xB4C4") ; HANGUL SYLLABLE TIKEUT-YU-NIEUN
       (?듈 . "0xB4C8") ; HANGUL SYLLABLE TIKEUT-YU-RIEUL
       (?듐 . "0xB4D0") ; HANGUL SYLLABLE TIKEUT-YU-MIEUM
       (?듕 . "0xB4D5") ; HANGUL SYLLABLE TIKEUT-YU-IEUNG
       (?드 . "0xB4DC") ; HANGUL SYLLABLE TIKEUT-EU
       (?득 . "0xB4DD") ; HANGUL SYLLABLE TIKEUT-EU-KIYEOK
       (?든 . "0xB4E0") ; HANGUL SYLLABLE TIKEUT-EU-NIEUN
       (?듣 . "0xB4E3") ; HANGUL SYLLABLE TIKEUT-EU-TIKEUT
       (?들 . "0xB4E4") ; HANGUL SYLLABLE TIKEUT-EU-RIEUL
       (?듦 . "0xB4E6") ; HANGUL SYLLABLE TIKEUT-EU-RIEULMIEUM
       (?듬 . "0xB4EC") ; HANGUL SYLLABLE TIKEUT-EU-MIEUM
       (?듭 . "0xB4ED") ; HANGUL SYLLABLE TIKEUT-EU-PIEUP
       (?듯 . "0xB4EF") ; HANGUL SYLLABLE TIKEUT-EU-SIOS
       (?등 . "0xB4F1") ; HANGUL SYLLABLE TIKEUT-EU-IEUNG
       (?듸 . "0xB4F8") ; HANGUL SYLLABLE TIKEUT-YI
       (?디 . "0xB514") ; HANGUL SYLLABLE TIKEUT-I
       (?딕 . "0xB515") ; HANGUL SYLLABLE TIKEUT-I-KIYEOK
       (?딘 . "0xB518") ; HANGUL SYLLABLE TIKEUT-I-NIEUN
       (?딛 . "0xB51B") ; HANGUL SYLLABLE TIKEUT-I-TIKEUT
       (?딜 . "0xB51C") ; HANGUL SYLLABLE TIKEUT-I-RIEUL
       (?딤 . "0xB524") ; HANGUL SYLLABLE TIKEUT-I-MIEUM
       (?딥 . "0xB525") ; HANGUL SYLLABLE TIKEUT-I-PIEUP
       (?딧 . "0xB527") ; HANGUL SYLLABLE TIKEUT-I-SIOS
       (?딨 . "0xB528") ; HANGUL SYLLABLE TIKEUT-I-SSANGSIOS
       (?딩 . "0xB529") ; HANGUL SYLLABLE TIKEUT-I-IEUNG
       (?딪 . "0xB52A") ; HANGUL SYLLABLE TIKEUT-I-CIEUC
       (?따 . "0xB530") ; HANGUL SYLLABLE SSANGTIKEUT-A
       (?딱 . "0xB531") ; HANGUL SYLLABLE SSANGTIKEUT-A-KIYEOK
       (?딴 . "0xB534") ; HANGUL SYLLABLE SSANGTIKEUT-A-NIEUN
       (?딸 . "0xB538") ; HANGUL SYLLABLE SSANGTIKEUT-A-RIEUL
       (?땀 . "0xB540") ; HANGUL SYLLABLE SSANGTIKEUT-A-MIEUM
       (?땁 . "0xB541") ; HANGUL SYLLABLE SSANGTIKEUT-A-PIEUP
       (?땃 . "0xB543") ; HANGUL SYLLABLE SSANGTIKEUT-A-SIOS
       (?땄 . "0xB544") ; HANGUL SYLLABLE SSANGTIKEUT-A-SSANGSIOS
       (?땅 . "0xB545") ; HANGUL SYLLABLE SSANGTIKEUT-A-IEUNG
       (?땋 . "0xB54B") ; HANGUL SYLLABLE SSANGTIKEUT-A-HIEUH
       (?때 . "0xB54C") ; HANGUL SYLLABLE SSANGTIKEUT-AE
       (?땍 . "0xB54D") ; HANGUL SYLLABLE SSANGTIKEUT-AE-KIYEOK
       (?땐 . "0xB550") ; HANGUL SYLLABLE SSANGTIKEUT-AE-NIEUN
       (?땔 . "0xB554") ; HANGUL SYLLABLE SSANGTIKEUT-AE-RIEUL
       (?땜 . "0xB55C") ; HANGUL SYLLABLE SSANGTIKEUT-AE-MIEUM
       (?땝 . "0xB55D") ; HANGUL SYLLABLE SSANGTIKEUT-AE-PIEUP
       (?땟 . "0xB55F") ; HANGUL SYLLABLE SSANGTIKEUT-AE-SIOS
       (?땠 . "0xB560") ; HANGUL SYLLABLE SSANGTIKEUT-AE-SSANGSIOS
       (?땡 . "0xB561") ; HANGUL SYLLABLE SSANGTIKEUT-AE-IEUNG
       (?떠 . "0xB5A0") ; HANGUL SYLLABLE SSANGTIKEUT-EO
       (?떡 . "0xB5A1") ; HANGUL SYLLABLE SSANGTIKEUT-EO-KIYEOK
       (?떤 . "0xB5A4") ; HANGUL SYLLABLE SSANGTIKEUT-EO-NIEUN
       (?떨 . "0xB5A8") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEUL
       (?떪 . "0xB5AA") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEULMIEUM
       (?떫 . "0xB5AB") ; HANGUL SYLLABLE SSANGTIKEUT-EO-RIEULPIEUP
       (?떰 . "0xB5B0") ; HANGUL SYLLABLE SSANGTIKEUT-EO-MIEUM
       (?떱 . "0xB5B1") ; HANGUL SYLLABLE SSANGTIKEUT-EO-PIEUP
       (?떳 . "0xB5B3") ; HANGUL SYLLABLE SSANGTIKEUT-EO-SIOS
       (?떴 . "0xB5B4") ; HANGUL SYLLABLE SSANGTIKEUT-EO-SSANGSIOS
       (?떵 . "0xB5B5") ; HANGUL SYLLABLE SSANGTIKEUT-EO-IEUNG
       (?떻 . "0xB5BB") ; HANGUL SYLLABLE SSANGTIKEUT-EO-HIEUH
       (?떼 . "0xB5BC") ; HANGUL SYLLABLE SSANGTIKEUT-E
       (?떽 . "0xB5BD") ; HANGUL SYLLABLE SSANGTIKEUT-E-KIYEOK
       (?뗀 . "0xB5C0") ; HANGUL SYLLABLE SSANGTIKEUT-E-NIEUN
       (?뗄 . "0xB5C4") ; HANGUL SYLLABLE SSANGTIKEUT-E-RIEUL
       (?뗌 . "0xB5CC") ; HANGUL SYLLABLE SSANGTIKEUT-E-MIEUM
       (?뗍 . "0xB5CD") ; HANGUL SYLLABLE SSANGTIKEUT-E-PIEUP
       (?뗏 . "0xB5CF") ; HANGUL SYLLABLE SSANGTIKEUT-E-SIOS
       (?뗐 . "0xB5D0") ; HANGUL SYLLABLE SSANGTIKEUT-E-SSANGSIOS
       (?뗑 . "0xB5D1") ; HANGUL SYLLABLE SSANGTIKEUT-E-IEUNG
       (?뗘 . "0xB5D8") ; HANGUL SYLLABLE SSANGTIKEUT-YEO
       (?뗬 . "0xB5EC") ; HANGUL SYLLABLE SSANGTIKEUT-YEO-SSANGSIOS
       (?또 . "0xB610") ; HANGUL SYLLABLE SSANGTIKEUT-O
       (?똑 . "0xB611") ; HANGUL SYLLABLE SSANGTIKEUT-O-KIYEOK
       (?똔 . "0xB614") ; HANGUL SYLLABLE SSANGTIKEUT-O-NIEUN
       (?똘 . "0xB618") ; HANGUL SYLLABLE SSANGTIKEUT-O-RIEUL
       (?똥 . "0xB625") ; HANGUL SYLLABLE SSANGTIKEUT-O-IEUNG
       (?똬 . "0xB62C") ; HANGUL SYLLABLE SSANGTIKEUT-WA
       (?똴 . "0xB634") ; HANGUL SYLLABLE SSANGTIKEUT-WA-RIEUL
       (?뙈 . "0xB648") ; HANGUL SYLLABLE SSANGTIKEUT-WAE
       (?뙤 . "0xB664") ; HANGUL SYLLABLE SSANGTIKEUT-OE
       (?뙨 . "0xB668") ; HANGUL SYLLABLE SSANGTIKEUT-OE-NIEUN
       (?뚜 . "0xB69C") ; HANGUL SYLLABLE SSANGTIKEUT-U
       (?뚝 . "0xB69D") ; HANGUL SYLLABLE SSANGTIKEUT-U-KIYEOK
       (?뚠 . "0xB6A0") ; HANGUL SYLLABLE SSANGTIKEUT-U-NIEUN
       (?뚤 . "0xB6A4") ; HANGUL SYLLABLE SSANGTIKEUT-U-RIEUL
       (?뚫 . "0xB6AB") ; HANGUL SYLLABLE SSANGTIKEUT-U-RIEULHIEUH
       (?뚬 . "0xB6AC") ; HANGUL SYLLABLE SSANGTIKEUT-U-MIEUM
       (?뚱 . "0xB6B1") ; HANGUL SYLLABLE SSANGTIKEUT-U-IEUNG
       (?뛔 . "0xB6D4") ; HANGUL SYLLABLE SSANGTIKEUT-WE
       (?뛰 . "0xB6F0") ; HANGUL SYLLABLE SSANGTIKEUT-WI
       (?뛴 . "0xB6F4") ; HANGUL SYLLABLE SSANGTIKEUT-WI-NIEUN
       (?뛸 . "0xB6F8") ; HANGUL SYLLABLE SSANGTIKEUT-WI-RIEUL
       (?뜀 . "0xB700") ; HANGUL SYLLABLE SSANGTIKEUT-WI-MIEUM
       (?뜁 . "0xB701") ; HANGUL SYLLABLE SSANGTIKEUT-WI-PIEUP
       (?뜅 . "0xB705") ; HANGUL SYLLABLE SSANGTIKEUT-WI-IEUNG
       (?뜨 . "0xB728") ; HANGUL SYLLABLE SSANGTIKEUT-EU
       (?뜩 . "0xB729") ; HANGUL SYLLABLE SSANGTIKEUT-EU-KIYEOK
       (?뜬 . "0xB72C") ; HANGUL SYLLABLE SSANGTIKEUT-EU-NIEUN
       (?뜯 . "0xB72F") ; HANGUL SYLLABLE SSANGTIKEUT-EU-TIKEUT
       (?뜰 . "0xB730") ; HANGUL SYLLABLE SSANGTIKEUT-EU-RIEUL
       (?뜸 . "0xB738") ; HANGUL SYLLABLE SSANGTIKEUT-EU-MIEUM
       (?뜹 . "0xB739") ; HANGUL SYLLABLE SSANGTIKEUT-EU-PIEUP
       (?뜻 . "0xB73B") ; HANGUL SYLLABLE SSANGTIKEUT-EU-SIOS
       (?띄 . "0xB744") ; HANGUL SYLLABLE SSANGTIKEUT-YI
       (?띈 . "0xB748") ; HANGUL SYLLABLE SSANGTIKEUT-YI-NIEUN
       (?띌 . "0xB74C") ; HANGUL SYLLABLE SSANGTIKEUT-YI-RIEUL
       (?띔 . "0xB754") ; HANGUL SYLLABLE SSANGTIKEUT-YI-MIEUM
       (?띕 . "0xB755") ; HANGUL SYLLABLE SSANGTIKEUT-YI-PIEUP
       (?띠 . "0xB760") ; HANGUL SYLLABLE SSANGTIKEUT-I
       (?띤 . "0xB764") ; HANGUL SYLLABLE SSANGTIKEUT-I-NIEUN
       (?띨 . "0xB768") ; HANGUL SYLLABLE SSANGTIKEUT-I-RIEUL
       (?띰 . "0xB770") ; HANGUL SYLLABLE SSANGTIKEUT-I-MIEUM
       (?띱 . "0xB771") ; HANGUL SYLLABLE SSANGTIKEUT-I-PIEUP
       (?띳 . "0xB773") ; HANGUL SYLLABLE SSANGTIKEUT-I-SIOS
       (?띵 . "0xB775") ; HANGUL SYLLABLE SSANGTIKEUT-I-IEUNG
       (?라 . "0xB77C") ; HANGUL SYLLABLE RIEUL-A
       (?락 . "0xB77D") ; HANGUL SYLLABLE RIEUL-A-KIYEOK
       (?란 . "0xB780") ; HANGUL SYLLABLE RIEUL-A-NIEUN
       (?랄 . "0xB784") ; HANGUL SYLLABLE RIEUL-A-RIEUL
       (?람 . "0xB78C") ; HANGUL SYLLABLE RIEUL-A-MIEUM
       (?랍 . "0xB78D") ; HANGUL SYLLABLE RIEUL-A-PIEUP
       (?랏 . "0xB78F") ; HANGUL SYLLABLE RIEUL-A-SIOS
       (?랐 . "0xB790") ; HANGUL SYLLABLE RIEUL-A-SSANGSIOS
       (?랑 . "0xB791") ; HANGUL SYLLABLE RIEUL-A-IEUNG
       (?랒 . "0xB792") ; HANGUL SYLLABLE RIEUL-A-CIEUC
       (?랖 . "0xB796") ; HANGUL SYLLABLE RIEUL-A-PHIEUPH
       (?랗 . "0xB797") ; HANGUL SYLLABLE RIEUL-A-HIEUH
       (?래 . "0xB798") ; HANGUL SYLLABLE RIEUL-AE
       (?랙 . "0xB799") ; HANGUL SYLLABLE RIEUL-AE-KIYEOK
       (?랜 . "0xB79C") ; HANGUL SYLLABLE RIEUL-AE-NIEUN
       (?랠 . "0xB7A0") ; HANGUL SYLLABLE RIEUL-AE-RIEUL
       (?램 . "0xB7A8") ; HANGUL SYLLABLE RIEUL-AE-MIEUM
       (?랩 . "0xB7A9") ; HANGUL SYLLABLE RIEUL-AE-PIEUP
       (?랫 . "0xB7AB") ; HANGUL SYLLABLE RIEUL-AE-SIOS
       (?랬 . "0xB7AC") ; HANGUL SYLLABLE RIEUL-AE-SSANGSIOS
       (?랭 . "0xB7AD") ; HANGUL SYLLABLE RIEUL-AE-IEUNG
       (?랴 . "0xB7B4") ; HANGUL SYLLABLE RIEUL-YA
       (?략 . "0xB7B5") ; HANGUL SYLLABLE RIEUL-YA-KIYEOK
       (?랸 . "0xB7B8") ; HANGUL SYLLABLE RIEUL-YA-NIEUN
       (?럇 . "0xB7C7") ; HANGUL SYLLABLE RIEUL-YA-SIOS
       (?량 . "0xB7C9") ; HANGUL SYLLABLE RIEUL-YA-IEUNG
       (?러 . "0xB7EC") ; HANGUL SYLLABLE RIEUL-EO
       (?럭 . "0xB7ED") ; HANGUL SYLLABLE RIEUL-EO-KIYEOK
       (?런 . "0xB7F0") ; HANGUL SYLLABLE RIEUL-EO-NIEUN
       (?럴 . "0xB7F4") ; HANGUL SYLLABLE RIEUL-EO-RIEUL
       (?럼 . "0xB7FC") ; HANGUL SYLLABLE RIEUL-EO-MIEUM
       (?럽 . "0xB7FD") ; HANGUL SYLLABLE RIEUL-EO-PIEUP
       (?럿 . "0xB7FF") ; HANGUL SYLLABLE RIEUL-EO-SIOS
       (?렀 . "0xB800") ; HANGUL SYLLABLE RIEUL-EO-SSANGSIOS
       (?렁 . "0xB801") ; HANGUL SYLLABLE RIEUL-EO-IEUNG
       (?렇 . "0xB807") ; HANGUL SYLLABLE RIEUL-EO-HIEUH
       (?레 . "0xB808") ; HANGUL SYLLABLE RIEUL-E
       (?렉 . "0xB809") ; HANGUL SYLLABLE RIEUL-E-KIYEOK
       (?렌 . "0xB80C") ; HANGUL SYLLABLE RIEUL-E-NIEUN
       (?렐 . "0xB810") ; HANGUL SYLLABLE RIEUL-E-RIEUL
       (?렘 . "0xB818") ; HANGUL SYLLABLE RIEUL-E-MIEUM
       (?렙 . "0xB819") ; HANGUL SYLLABLE RIEUL-E-PIEUP
       (?렛 . "0xB81B") ; HANGUL SYLLABLE RIEUL-E-SIOS
       (?렝 . "0xB81D") ; HANGUL SYLLABLE RIEUL-E-IEUNG
       (?려 . "0xB824") ; HANGUL SYLLABLE RIEUL-YEO
       (?력 . "0xB825") ; HANGUL SYLLABLE RIEUL-YEO-KIYEOK
       (?련 . "0xB828") ; HANGUL SYLLABLE RIEUL-YEO-NIEUN
       (?렬 . "0xB82C") ; HANGUL SYLLABLE RIEUL-YEO-RIEUL
       (?렴 . "0xB834") ; HANGUL SYLLABLE RIEUL-YEO-MIEUM
       (?렵 . "0xB835") ; HANGUL SYLLABLE RIEUL-YEO-PIEUP
       (?렷 . "0xB837") ; HANGUL SYLLABLE RIEUL-YEO-SIOS
       (?렸 . "0xB838") ; HANGUL SYLLABLE RIEUL-YEO-SSANGSIOS
       (?령 . "0xB839") ; HANGUL SYLLABLE RIEUL-YEO-IEUNG
       (?례 . "0xB840") ; HANGUL SYLLABLE RIEUL-YE
       (?롄 . "0xB844") ; HANGUL SYLLABLE RIEUL-YE-NIEUN
       (?롑 . "0xB851") ; HANGUL SYLLABLE RIEUL-YE-PIEUP
       (?롓 . "0xB853") ; HANGUL SYLLABLE RIEUL-YE-SIOS
       (?로 . "0xB85C") ; HANGUL SYLLABLE RIEUL-O
       (?록 . "0xB85D") ; HANGUL SYLLABLE RIEUL-O-KIYEOK
       (?론 . "0xB860") ; HANGUL SYLLABLE RIEUL-O-NIEUN
       (?롤 . "0xB864") ; HANGUL SYLLABLE RIEUL-O-RIEUL
       (?롬 . "0xB86C") ; HANGUL SYLLABLE RIEUL-O-MIEUM
       (?롭 . "0xB86D") ; HANGUL SYLLABLE RIEUL-O-PIEUP
       (?롯 . "0xB86F") ; HANGUL SYLLABLE RIEUL-O-SIOS
       (?롱 . "0xB871") ; HANGUL SYLLABLE RIEUL-O-IEUNG
       (?롸 . "0xB878") ; HANGUL SYLLABLE RIEUL-WA
       (?롼 . "0xB87C") ; HANGUL SYLLABLE RIEUL-WA-NIEUN
       (?뢍 . "0xB88D") ; HANGUL SYLLABLE RIEUL-WA-IEUNG
       (?뢨 . "0xB8A8") ; HANGUL SYLLABLE RIEUL-WAE-SSANGSIOS
       (?뢰 . "0xB8B0") ; HANGUL SYLLABLE RIEUL-OE
       (?뢴 . "0xB8B4") ; HANGUL SYLLABLE RIEUL-OE-NIEUN
       (?뢸 . "0xB8B8") ; HANGUL SYLLABLE RIEUL-OE-RIEUL
       (?룀 . "0xB8C0") ; HANGUL SYLLABLE RIEUL-OE-MIEUM
       (?룁 . "0xB8C1") ; HANGUL SYLLABLE RIEUL-OE-PIEUP
       (?룃 . "0xB8C3") ; HANGUL SYLLABLE RIEUL-OE-SIOS
       (?룅 . "0xB8C5") ; HANGUL SYLLABLE RIEUL-OE-IEUNG
       (?료 . "0xB8CC") ; HANGUL SYLLABLE RIEUL-YO
       (?룐 . "0xB8D0") ; HANGUL SYLLABLE RIEUL-YO-NIEUN
       (?룔 . "0xB8D4") ; HANGUL SYLLABLE RIEUL-YO-RIEUL
       (?룝 . "0xB8DD") ; HANGUL SYLLABLE RIEUL-YO-PIEUP
       (?룟 . "0xB8DF") ; HANGUL SYLLABLE RIEUL-YO-SIOS
       (?룡 . "0xB8E1") ; HANGUL SYLLABLE RIEUL-YO-IEUNG
       (?루 . "0xB8E8") ; HANGUL SYLLABLE RIEUL-U
       (?룩 . "0xB8E9") ; HANGUL SYLLABLE RIEUL-U-KIYEOK
       (?룬 . "0xB8EC") ; HANGUL SYLLABLE RIEUL-U-NIEUN
       (?룰 . "0xB8F0") ; HANGUL SYLLABLE RIEUL-U-RIEUL
       (?룸 . "0xB8F8") ; HANGUL SYLLABLE RIEUL-U-MIEUM
       (?룹 . "0xB8F9") ; HANGUL SYLLABLE RIEUL-U-PIEUP
       (?룻 . "0xB8FB") ; HANGUL SYLLABLE RIEUL-U-SIOS
       (?룽 . "0xB8FD") ; HANGUL SYLLABLE RIEUL-U-IEUNG
       (?뤄 . "0xB904") ; HANGUL SYLLABLE RIEUL-WEO
       (?뤘 . "0xB918") ; HANGUL SYLLABLE RIEUL-WEO-SSANGSIOS
       (?뤠 . "0xB920") ; HANGUL SYLLABLE RIEUL-WE
       (?뤼 . "0xB93C") ; HANGUL SYLLABLE RIEUL-WI
       (?뤽 . "0xB93D") ; HANGUL SYLLABLE RIEUL-WI-KIYEOK
       (?륀 . "0xB940") ; HANGUL SYLLABLE RIEUL-WI-NIEUN
       (?륄 . "0xB944") ; HANGUL SYLLABLE RIEUL-WI-RIEUL
       (?륌 . "0xB94C") ; HANGUL SYLLABLE RIEUL-WI-MIEUM
       (?륏 . "0xB94F") ; HANGUL SYLLABLE RIEUL-WI-SIOS
       (?륑 . "0xB951") ; HANGUL SYLLABLE RIEUL-WI-IEUNG
       (?류 . "0xB958") ; HANGUL SYLLABLE RIEUL-YU
       (?륙 . "0xB959") ; HANGUL SYLLABLE RIEUL-YU-KIYEOK
       (?륜 . "0xB95C") ; HANGUL SYLLABLE RIEUL-YU-NIEUN
       (?률 . "0xB960") ; HANGUL SYLLABLE RIEUL-YU-RIEUL
       (?륨 . "0xB968") ; HANGUL SYLLABLE RIEUL-YU-MIEUM
       (?륩 . "0xB969") ; HANGUL SYLLABLE RIEUL-YU-PIEUP
       (?륫 . "0xB96B") ; HANGUL SYLLABLE RIEUL-YU-SIOS
       (?륭 . "0xB96D") ; HANGUL SYLLABLE RIEUL-YU-IEUNG
       (?르 . "0xB974") ; HANGUL SYLLABLE RIEUL-EU
       (?륵 . "0xB975") ; HANGUL SYLLABLE RIEUL-EU-KIYEOK
       (?른 . "0xB978") ; HANGUL SYLLABLE RIEUL-EU-NIEUN
       (?를 . "0xB97C") ; HANGUL SYLLABLE RIEUL-EU-RIEUL
       (?름 . "0xB984") ; HANGUL SYLLABLE RIEUL-EU-MIEUM
       (?릅 . "0xB985") ; HANGUL SYLLABLE RIEUL-EU-PIEUP
       (?릇 . "0xB987") ; HANGUL SYLLABLE RIEUL-EU-SIOS
       (?릉 . "0xB989") ; HANGUL SYLLABLE RIEUL-EU-IEUNG
       (?릊 . "0xB98A") ; HANGUL SYLLABLE RIEUL-EU-CIEUC
       (?릍 . "0xB98D") ; HANGUL SYLLABLE RIEUL-EU-THIEUTH
       (?릎 . "0xB98E") ; HANGUL SYLLABLE RIEUL-EU-PHIEUPH
       (?리 . "0xB9AC") ; HANGUL SYLLABLE RIEUL-I
       (?릭 . "0xB9AD") ; HANGUL SYLLABLE RIEUL-I-KIYEOK
       (?린 . "0xB9B0") ; HANGUL SYLLABLE RIEUL-I-NIEUN
       (?릴 . "0xB9B4") ; HANGUL SYLLABLE RIEUL-I-RIEUL
       (?림 . "0xB9BC") ; HANGUL SYLLABLE RIEUL-I-MIEUM
       (?립 . "0xB9BD") ; HANGUL SYLLABLE RIEUL-I-PIEUP
       (?릿 . "0xB9BF") ; HANGUL SYLLABLE RIEUL-I-SIOS
       (?링 . "0xB9C1") ; HANGUL SYLLABLE RIEUL-I-IEUNG
       (?마 . "0xB9C8") ; HANGUL SYLLABLE MIEUM-A
       (?막 . "0xB9C9") ; HANGUL SYLLABLE MIEUM-A-KIYEOK
       (?만 . "0xB9CC") ; HANGUL SYLLABLE MIEUM-A-NIEUN
       (?많 . "0xB9CE") ; HANGUL SYLLABLE MIEUM-A-NIEUNHIEUH
       (?맏 . "0xB9CF") ; HANGUL SYLLABLE MIEUM-A-TIKEUT
       (?말 . "0xB9D0") ; HANGUL SYLLABLE MIEUM-A-RIEUL
       (?맑 . "0xB9D1") ; HANGUL SYLLABLE MIEUM-A-RIEULKIYEOK
       (?맒 . "0xB9D2") ; HANGUL SYLLABLE MIEUM-A-RIEULMIEUM
       (?맘 . "0xB9D8") ; HANGUL SYLLABLE MIEUM-A-MIEUM
       (?맙 . "0xB9D9") ; HANGUL SYLLABLE MIEUM-A-PIEUP
       (?맛 . "0xB9DB") ; HANGUL SYLLABLE MIEUM-A-SIOS
       (?망 . "0xB9DD") ; HANGUL SYLLABLE MIEUM-A-IEUNG
       (?맞 . "0xB9DE") ; HANGUL SYLLABLE MIEUM-A-CIEUC
       (?맡 . "0xB9E1") ; HANGUL SYLLABLE MIEUM-A-THIEUTH
       (?맣 . "0xB9E3") ; HANGUL SYLLABLE MIEUM-A-HIEUH
       (?매 . "0xB9E4") ; HANGUL SYLLABLE MIEUM-AE
       (?맥 . "0xB9E5") ; HANGUL SYLLABLE MIEUM-AE-KIYEOK
       (?맨 . "0xB9E8") ; HANGUL SYLLABLE MIEUM-AE-NIEUN
       (?맬 . "0xB9EC") ; HANGUL SYLLABLE MIEUM-AE-RIEUL
       (?맴 . "0xB9F4") ; HANGUL SYLLABLE MIEUM-AE-MIEUM
       (?맵 . "0xB9F5") ; HANGUL SYLLABLE MIEUM-AE-PIEUP
       (?맷 . "0xB9F7") ; HANGUL SYLLABLE MIEUM-AE-SIOS
       (?맸 . "0xB9F8") ; HANGUL SYLLABLE MIEUM-AE-SSANGSIOS
       (?맹 . "0xB9F9") ; HANGUL SYLLABLE MIEUM-AE-IEUNG
       (?맺 . "0xB9FA") ; HANGUL SYLLABLE MIEUM-AE-CIEUC
       (?먀 . "0xBA00") ; HANGUL SYLLABLE MIEUM-YA
       (?먁 . "0xBA01") ; HANGUL SYLLABLE MIEUM-YA-KIYEOK
       (?먈 . "0xBA08") ; HANGUL SYLLABLE MIEUM-YA-RIEUL
       (?먕 . "0xBA15") ; HANGUL SYLLABLE MIEUM-YA-IEUNG
       (?머 . "0xBA38") ; HANGUL SYLLABLE MIEUM-EO
       (?먹 . "0xBA39") ; HANGUL SYLLABLE MIEUM-EO-KIYEOK
       (?먼 . "0xBA3C") ; HANGUL SYLLABLE MIEUM-EO-NIEUN
       (?멀 . "0xBA40") ; HANGUL SYLLABLE MIEUM-EO-RIEUL
       (?멂 . "0xBA42") ; HANGUL SYLLABLE MIEUM-EO-RIEULMIEUM
       (?멈 . "0xBA48") ; HANGUL SYLLABLE MIEUM-EO-MIEUM
       (?멉 . "0xBA49") ; HANGUL SYLLABLE MIEUM-EO-PIEUP
       (?멋 . "0xBA4B") ; HANGUL SYLLABLE MIEUM-EO-SIOS
       (?멍 . "0xBA4D") ; HANGUL SYLLABLE MIEUM-EO-IEUNG
       (?멎 . "0xBA4E") ; HANGUL SYLLABLE MIEUM-EO-CIEUC
       (?멓 . "0xBA53") ; HANGUL SYLLABLE MIEUM-EO-HIEUH
       (?메 . "0xBA54") ; HANGUL SYLLABLE MIEUM-E
       (?멕 . "0xBA55") ; HANGUL SYLLABLE MIEUM-E-KIYEOK
       (?멘 . "0xBA58") ; HANGUL SYLLABLE MIEUM-E-NIEUN
       (?멜 . "0xBA5C") ; HANGUL SYLLABLE MIEUM-E-RIEUL
       (?멤 . "0xBA64") ; HANGUL SYLLABLE MIEUM-E-MIEUM
       (?멥 . "0xBA65") ; HANGUL SYLLABLE MIEUM-E-PIEUP
       (?멧 . "0xBA67") ; HANGUL SYLLABLE MIEUM-E-SIOS
       (?멨 . "0xBA68") ; HANGUL SYLLABLE MIEUM-E-SSANGSIOS
       (?멩 . "0xBA69") ; HANGUL SYLLABLE MIEUM-E-IEUNG
       (?며 . "0xBA70") ; HANGUL SYLLABLE MIEUM-YEO
       (?멱 . "0xBA71") ; HANGUL SYLLABLE MIEUM-YEO-KIYEOK
       (?면 . "0xBA74") ; HANGUL SYLLABLE MIEUM-YEO-NIEUN
       (?멸 . "0xBA78") ; HANGUL SYLLABLE MIEUM-YEO-RIEUL
       (?몃 . "0xBA83") ; HANGUL SYLLABLE MIEUM-YEO-SIOS
       (?몄 . "0xBA84") ; HANGUL SYLLABLE MIEUM-YEO-SSANGSIOS
       (?명 . "0xBA85") ; HANGUL SYLLABLE MIEUM-YEO-IEUNG
       (?몇 . "0xBA87") ; HANGUL SYLLABLE MIEUM-YEO-CHIEUCH
       (?몌 . "0xBA8C") ; HANGUL SYLLABLE MIEUM-YE
       (?모 . "0xBAA8") ; HANGUL SYLLABLE MIEUM-O
       (?목 . "0xBAA9") ; HANGUL SYLLABLE MIEUM-O-KIYEOK
       (?몫 . "0xBAAB") ; HANGUL SYLLABLE MIEUM-O-KIYEOKSIOS
       (?몬 . "0xBAAC") ; HANGUL SYLLABLE MIEUM-O-NIEUN
       (?몰 . "0xBAB0") ; HANGUL SYLLABLE MIEUM-O-RIEUL
       (?몲 . "0xBAB2") ; HANGUL SYLLABLE MIEUM-O-RIEULMIEUM
       (?몸 . "0xBAB8") ; HANGUL SYLLABLE MIEUM-O-MIEUM
       (?몹 . "0xBAB9") ; HANGUL SYLLABLE MIEUM-O-PIEUP
       (?못 . "0xBABB") ; HANGUL SYLLABLE MIEUM-O-SIOS
       (?몽 . "0xBABD") ; HANGUL SYLLABLE MIEUM-O-IEUNG
       (?뫄 . "0xBAC4") ; HANGUL SYLLABLE MIEUM-WA
       (?뫈 . "0xBAC8") ; HANGUL SYLLABLE MIEUM-WA-NIEUN
       (?뫘 . "0xBAD8") ; HANGUL SYLLABLE MIEUM-WA-SSANGSIOS
       (?뫙 . "0xBAD9") ; HANGUL SYLLABLE MIEUM-WA-IEUNG
       (?뫼 . "0xBAFC") ; HANGUL SYLLABLE MIEUM-OE
       (?묀 . "0xBB00") ; HANGUL SYLLABLE MIEUM-OE-NIEUN
       (?묄 . "0xBB04") ; HANGUL SYLLABLE MIEUM-OE-RIEUL
       (?묍 . "0xBB0D") ; HANGUL SYLLABLE MIEUM-OE-PIEUP
       (?묏 . "0xBB0F") ; HANGUL SYLLABLE MIEUM-OE-SIOS
       (?묑 . "0xBB11") ; HANGUL SYLLABLE MIEUM-OE-IEUNG
       (?묘 . "0xBB18") ; HANGUL SYLLABLE MIEUM-YO
       (?묜 . "0xBB1C") ; HANGUL SYLLABLE MIEUM-YO-NIEUN
       (?묠 . "0xBB20") ; HANGUL SYLLABLE MIEUM-YO-RIEUL
       (?묩 . "0xBB29") ; HANGUL SYLLABLE MIEUM-YO-PIEUP
       (?묫 . "0xBB2B") ; HANGUL SYLLABLE MIEUM-YO-SIOS
       (?무 . "0xBB34") ; HANGUL SYLLABLE MIEUM-U
       (?묵 . "0xBB35") ; HANGUL SYLLABLE MIEUM-U-KIYEOK
       (?묶 . "0xBB36") ; HANGUL SYLLABLE MIEUM-U-SSANGKIYEOK
       (?문 . "0xBB38") ; HANGUL SYLLABLE MIEUM-U-NIEUN
       (?묻 . "0xBB3B") ; HANGUL SYLLABLE MIEUM-U-TIKEUT
       (?물 . "0xBB3C") ; HANGUL SYLLABLE MIEUM-U-RIEUL
       (?묽 . "0xBB3D") ; HANGUL SYLLABLE MIEUM-U-RIEULKIYEOK
       (?묾 . "0xBB3E") ; HANGUL SYLLABLE MIEUM-U-RIEULMIEUM
       (?뭄 . "0xBB44") ; HANGUL SYLLABLE MIEUM-U-MIEUM
       (?뭅 . "0xBB45") ; HANGUL SYLLABLE MIEUM-U-PIEUP
       (?뭇 . "0xBB47") ; HANGUL SYLLABLE MIEUM-U-SIOS
       (?뭉 . "0xBB49") ; HANGUL SYLLABLE MIEUM-U-IEUNG
       (?뭍 . "0xBB4D") ; HANGUL SYLLABLE MIEUM-U-THIEUTH
       (?뭏 . "0xBB4F") ; HANGUL SYLLABLE MIEUM-U-HIEUH
       (?뭐 . "0xBB50") ; HANGUL SYLLABLE MIEUM-WEO
       (?뭔 . "0xBB54") ; HANGUL SYLLABLE MIEUM-WEO-NIEUN
       (?뭘 . "0xBB58") ; HANGUL SYLLABLE MIEUM-WEO-RIEUL
       (?뭡 . "0xBB61") ; HANGUL SYLLABLE MIEUM-WEO-PIEUP
       (?뭣 . "0xBB63") ; HANGUL SYLLABLE MIEUM-WEO-SIOS
       (?뭬 . "0xBB6C") ; HANGUL SYLLABLE MIEUM-WE
       (?뮈 . "0xBB88") ; HANGUL SYLLABLE MIEUM-WI
       (?뮌 . "0xBB8C") ; HANGUL SYLLABLE MIEUM-WI-NIEUN
       (?뮐 . "0xBB90") ; HANGUL SYLLABLE MIEUM-WI-RIEUL
       (?뮤 . "0xBBA4") ; HANGUL SYLLABLE MIEUM-YU
       (?뮨 . "0xBBA8") ; HANGUL SYLLABLE MIEUM-YU-NIEUN
       (?뮬 . "0xBBAC") ; HANGUL SYLLABLE MIEUM-YU-RIEUL
       (?뮴 . "0xBBB4") ; HANGUL SYLLABLE MIEUM-YU-MIEUM
       (?뮷 . "0xBBB7") ; HANGUL SYLLABLE MIEUM-YU-SIOS
       (?므 . "0xBBC0") ; HANGUL SYLLABLE MIEUM-EU
       (?믄 . "0xBBC4") ; HANGUL SYLLABLE MIEUM-EU-NIEUN
       (?믈 . "0xBBC8") ; HANGUL SYLLABLE MIEUM-EU-RIEUL
       (?믐 . "0xBBD0") ; HANGUL SYLLABLE MIEUM-EU-MIEUM
       (?믓 . "0xBBD3") ; HANGUL SYLLABLE MIEUM-EU-SIOS
       (?미 . "0xBBF8") ; HANGUL SYLLABLE MIEUM-I
       (?믹 . "0xBBF9") ; HANGUL SYLLABLE MIEUM-I-KIYEOK
       (?민 . "0xBBFC") ; HANGUL SYLLABLE MIEUM-I-NIEUN
       (?믿 . "0xBBFF") ; HANGUL SYLLABLE MIEUM-I-TIKEUT
       (?밀 . "0xBC00") ; HANGUL SYLLABLE MIEUM-I-RIEUL
       (?밂 . "0xBC02") ; HANGUL SYLLABLE MIEUM-I-RIEULMIEUM
       (?밈 . "0xBC08") ; HANGUL SYLLABLE MIEUM-I-MIEUM
       (?밉 . "0xBC09") ; HANGUL SYLLABLE MIEUM-I-PIEUP
       (?밋 . "0xBC0B") ; HANGUL SYLLABLE MIEUM-I-SIOS
       (?밌 . "0xBC0C") ; HANGUL SYLLABLE MIEUM-I-SSANGSIOS
       (?밍 . "0xBC0D") ; HANGUL SYLLABLE MIEUM-I-IEUNG
       (?및 . "0xBC0F") ; HANGUL SYLLABLE MIEUM-I-CHIEUCH
       (?밑 . "0xBC11") ; HANGUL SYLLABLE MIEUM-I-THIEUTH
       (?바 . "0xBC14") ; HANGUL SYLLABLE PIEUP-A
       (?박 . "0xBC15") ; HANGUL SYLLABLE PIEUP-A-KIYEOK
       (?밖 . "0xBC16") ; HANGUL SYLLABLE PIEUP-A-SSANGKIYEOK
       (?밗 . "0xBC17") ; HANGUL SYLLABLE PIEUP-A-KIYEOKSIOS
       (?반 . "0xBC18") ; HANGUL SYLLABLE PIEUP-A-NIEUN
       (?받 . "0xBC1B") ; HANGUL SYLLABLE PIEUP-A-TIKEUT
       (?발 . "0xBC1C") ; HANGUL SYLLABLE PIEUP-A-RIEUL
       (?밝 . "0xBC1D") ; HANGUL SYLLABLE PIEUP-A-RIEULKIYEOK
       (?밞 . "0xBC1E") ; HANGUL SYLLABLE PIEUP-A-RIEULMIEUM
       (?밟 . "0xBC1F") ; HANGUL SYLLABLE PIEUP-A-RIEULPIEUP
       (?밤 . "0xBC24") ; HANGUL SYLLABLE PIEUP-A-MIEUM
       (?밥 . "0xBC25") ; HANGUL SYLLABLE PIEUP-A-PIEUP
       (?밧 . "0xBC27") ; HANGUL SYLLABLE PIEUP-A-SIOS
       (?방 . "0xBC29") ; HANGUL SYLLABLE PIEUP-A-IEUNG
       (?밭 . "0xBC2D") ; HANGUL SYLLABLE PIEUP-A-THIEUTH
       (?배 . "0xBC30") ; HANGUL SYLLABLE PIEUP-AE
       (?백 . "0xBC31") ; HANGUL SYLLABLE PIEUP-AE-KIYEOK
       (?밴 . "0xBC34") ; HANGUL SYLLABLE PIEUP-AE-NIEUN
       (?밸 . "0xBC38") ; HANGUL SYLLABLE PIEUP-AE-RIEUL
       (?뱀 . "0xBC40") ; HANGUL SYLLABLE PIEUP-AE-MIEUM
       (?뱁 . "0xBC41") ; HANGUL SYLLABLE PIEUP-AE-PIEUP
       (?뱃 . "0xBC43") ; HANGUL SYLLABLE PIEUP-AE-SIOS
       (?뱄 . "0xBC44") ; HANGUL SYLLABLE PIEUP-AE-SSANGSIOS
       (?뱅 . "0xBC45") ; HANGUL SYLLABLE PIEUP-AE-IEUNG
       (?뱉 . "0xBC49") ; HANGUL SYLLABLE PIEUP-AE-THIEUTH
       (?뱌 . "0xBC4C") ; HANGUL SYLLABLE PIEUP-YA
       (?뱍 . "0xBC4D") ; HANGUL SYLLABLE PIEUP-YA-KIYEOK
       (?뱐 . "0xBC50") ; HANGUL SYLLABLE PIEUP-YA-NIEUN
       (?뱝 . "0xBC5D") ; HANGUL SYLLABLE PIEUP-YA-PIEUP
       (?버 . "0xBC84") ; HANGUL SYLLABLE PIEUP-EO
       (?벅 . "0xBC85") ; HANGUL SYLLABLE PIEUP-EO-KIYEOK
       (?번 . "0xBC88") ; HANGUL SYLLABLE PIEUP-EO-NIEUN
       (?벋 . "0xBC8B") ; HANGUL SYLLABLE PIEUP-EO-TIKEUT
       (?벌 . "0xBC8C") ; HANGUL SYLLABLE PIEUP-EO-RIEUL
       (?벎 . "0xBC8E") ; HANGUL SYLLABLE PIEUP-EO-RIEULMIEUM
       (?범 . "0xBC94") ; HANGUL SYLLABLE PIEUP-EO-MIEUM
       (?법 . "0xBC95") ; HANGUL SYLLABLE PIEUP-EO-PIEUP
       (?벗 . "0xBC97") ; HANGUL SYLLABLE PIEUP-EO-SIOS
       (?벙 . "0xBC99") ; HANGUL SYLLABLE PIEUP-EO-IEUNG
       (?벚 . "0xBC9A") ; HANGUL SYLLABLE PIEUP-EO-CIEUC
       (?베 . "0xBCA0") ; HANGUL SYLLABLE PIEUP-E
       (?벡 . "0xBCA1") ; HANGUL SYLLABLE PIEUP-E-KIYEOK
       (?벤 . "0xBCA4") ; HANGUL SYLLABLE PIEUP-E-NIEUN
       (?벧 . "0xBCA7") ; HANGUL SYLLABLE PIEUP-E-TIKEUT
       (?벨 . "0xBCA8") ; HANGUL SYLLABLE PIEUP-E-RIEUL
       (?벰 . "0xBCB0") ; HANGUL SYLLABLE PIEUP-E-MIEUM
       (?벱 . "0xBCB1") ; HANGUL SYLLABLE PIEUP-E-PIEUP
       (?벳 . "0xBCB3") ; HANGUL SYLLABLE PIEUP-E-SIOS
       (?벴 . "0xBCB4") ; HANGUL SYLLABLE PIEUP-E-SSANGSIOS
       (?벵 . "0xBCB5") ; HANGUL SYLLABLE PIEUP-E-IEUNG
       (?벼 . "0xBCBC") ; HANGUL SYLLABLE PIEUP-YEO
       (?벽 . "0xBCBD") ; HANGUL SYLLABLE PIEUP-YEO-KIYEOK
       (?변 . "0xBCC0") ; HANGUL SYLLABLE PIEUP-YEO-NIEUN
       (?별 . "0xBCC4") ; HANGUL SYLLABLE PIEUP-YEO-RIEUL
       (?볍 . "0xBCCD") ; HANGUL SYLLABLE PIEUP-YEO-PIEUP
       (?볏 . "0xBCCF") ; HANGUL SYLLABLE PIEUP-YEO-SIOS
       (?볐 . "0xBCD0") ; HANGUL SYLLABLE PIEUP-YEO-SSANGSIOS
       (?병 . "0xBCD1") ; HANGUL SYLLABLE PIEUP-YEO-IEUNG
       (?볕 . "0xBCD5") ; HANGUL SYLLABLE PIEUP-YEO-THIEUTH
       (?볘 . "0xBCD8") ; HANGUL SYLLABLE PIEUP-YE
       (?볜 . "0xBCDC") ; HANGUL SYLLABLE PIEUP-YE-NIEUN
       (?보 . "0xBCF4") ; HANGUL SYLLABLE PIEUP-O
       (?복 . "0xBCF5") ; HANGUL SYLLABLE PIEUP-O-KIYEOK
       (?볶 . "0xBCF6") ; HANGUL SYLLABLE PIEUP-O-SSANGKIYEOK
       (?본 . "0xBCF8") ; HANGUL SYLLABLE PIEUP-O-NIEUN
       (?볼 . "0xBCFC") ; HANGUL SYLLABLE PIEUP-O-RIEUL
       (?봄 . "0xBD04") ; HANGUL SYLLABLE PIEUP-O-MIEUM
       (?봅 . "0xBD05") ; HANGUL SYLLABLE PIEUP-O-PIEUP
       (?봇 . "0xBD07") ; HANGUL SYLLABLE PIEUP-O-SIOS
       (?봉 . "0xBD09") ; HANGUL SYLLABLE PIEUP-O-IEUNG
       (?봐 . "0xBD10") ; HANGUL SYLLABLE PIEUP-WA
       (?봔 . "0xBD14") ; HANGUL SYLLABLE PIEUP-WA-NIEUN
       (?봤 . "0xBD24") ; HANGUL SYLLABLE PIEUP-WA-SSANGSIOS
       (?봬 . "0xBD2C") ; HANGUL SYLLABLE PIEUP-WAE
       (?뵀 . "0xBD40") ; HANGUL SYLLABLE PIEUP-WAE-SSANGSIOS
       (?뵈 . "0xBD48") ; HANGUL SYLLABLE PIEUP-OE
       (?뵉 . "0xBD49") ; HANGUL SYLLABLE PIEUP-OE-KIYEOK
       (?뵌 . "0xBD4C") ; HANGUL SYLLABLE PIEUP-OE-NIEUN
       (?뵐 . "0xBD50") ; HANGUL SYLLABLE PIEUP-OE-RIEUL
       (?뵘 . "0xBD58") ; HANGUL SYLLABLE PIEUP-OE-MIEUM
       (?뵙 . "0xBD59") ; HANGUL SYLLABLE PIEUP-OE-PIEUP
       (?뵤 . "0xBD64") ; HANGUL SYLLABLE PIEUP-YO
       (?뵨 . "0xBD68") ; HANGUL SYLLABLE PIEUP-YO-NIEUN
       (?부 . "0xBD80") ; HANGUL SYLLABLE PIEUP-U
       (?북 . "0xBD81") ; HANGUL SYLLABLE PIEUP-U-KIYEOK
       (?분 . "0xBD84") ; HANGUL SYLLABLE PIEUP-U-NIEUN
       (?붇 . "0xBD87") ; HANGUL SYLLABLE PIEUP-U-TIKEUT
       (?불 . "0xBD88") ; HANGUL SYLLABLE PIEUP-U-RIEUL
       (?붉 . "0xBD89") ; HANGUL SYLLABLE PIEUP-U-RIEULKIYEOK
       (?붊 . "0xBD8A") ; HANGUL SYLLABLE PIEUP-U-RIEULMIEUM
       (?붐 . "0xBD90") ; HANGUL SYLLABLE PIEUP-U-MIEUM
       (?붑 . "0xBD91") ; HANGUL SYLLABLE PIEUP-U-PIEUP
       (?붓 . "0xBD93") ; HANGUL SYLLABLE PIEUP-U-SIOS
       (?붕 . "0xBD95") ; HANGUL SYLLABLE PIEUP-U-IEUNG
       (?붙 . "0xBD99") ; HANGUL SYLLABLE PIEUP-U-THIEUTH
       (?붚 . "0xBD9A") ; HANGUL SYLLABLE PIEUP-U-PHIEUPH
       (?붜 . "0xBD9C") ; HANGUL SYLLABLE PIEUP-WEO
       (?붤 . "0xBDA4") ; HANGUL SYLLABLE PIEUP-WEO-RIEUL
       (?붰 . "0xBDB0") ; HANGUL SYLLABLE PIEUP-WEO-SSANGSIOS
       (?붸 . "0xBDB8") ; HANGUL SYLLABLE PIEUP-WE
       (?뷔 . "0xBDD4") ; HANGUL SYLLABLE PIEUP-WI
       (?뷕 . "0xBDD5") ; HANGUL SYLLABLE PIEUP-WI-KIYEOK
       (?뷘 . "0xBDD8") ; HANGUL SYLLABLE PIEUP-WI-NIEUN
       (?뷜 . "0xBDDC") ; HANGUL SYLLABLE PIEUP-WI-RIEUL
       (?뷩 . "0xBDE9") ; HANGUL SYLLABLE PIEUP-WI-IEUNG
       (?뷰 . "0xBDF0") ; HANGUL SYLLABLE PIEUP-YU
       (?뷴 . "0xBDF4") ; HANGUL SYLLABLE PIEUP-YU-NIEUN
       (?뷸 . "0xBDF8") ; HANGUL SYLLABLE PIEUP-YU-RIEUL
       (?븀 . "0xBE00") ; HANGUL SYLLABLE PIEUP-YU-MIEUM
       (?븃 . "0xBE03") ; HANGUL SYLLABLE PIEUP-YU-SIOS
       (?븅 . "0xBE05") ; HANGUL SYLLABLE PIEUP-YU-IEUNG
       (?브 . "0xBE0C") ; HANGUL SYLLABLE PIEUP-EU
       (?븍 . "0xBE0D") ; HANGUL SYLLABLE PIEUP-EU-KIYEOK
       (?븐 . "0xBE10") ; HANGUL SYLLABLE PIEUP-EU-NIEUN
       (?블 . "0xBE14") ; HANGUL SYLLABLE PIEUP-EU-RIEUL
       (?븜 . "0xBE1C") ; HANGUL SYLLABLE PIEUP-EU-MIEUM
       (?븝 . "0xBE1D") ; HANGUL SYLLABLE PIEUP-EU-PIEUP
       (?븟 . "0xBE1F") ; HANGUL SYLLABLE PIEUP-EU-SIOS
       (?비 . "0xBE44") ; HANGUL SYLLABLE PIEUP-I
       (?빅 . "0xBE45") ; HANGUL SYLLABLE PIEUP-I-KIYEOK
       (?빈 . "0xBE48") ; HANGUL SYLLABLE PIEUP-I-NIEUN
       (?빌 . "0xBE4C") ; HANGUL SYLLABLE PIEUP-I-RIEUL
       (?빎 . "0xBE4E") ; HANGUL SYLLABLE PIEUP-I-RIEULMIEUM
       (?빔 . "0xBE54") ; HANGUL SYLLABLE PIEUP-I-MIEUM
       (?빕 . "0xBE55") ; HANGUL SYLLABLE PIEUP-I-PIEUP
       (?빗 . "0xBE57") ; HANGUL SYLLABLE PIEUP-I-SIOS
       (?빙 . "0xBE59") ; HANGUL SYLLABLE PIEUP-I-IEUNG
       (?빚 . "0xBE5A") ; HANGUL SYLLABLE PIEUP-I-CIEUC
       (?빛 . "0xBE5B") ; HANGUL SYLLABLE PIEUP-I-CHIEUCH
       (?빠 . "0xBE60") ; HANGUL SYLLABLE SSANGPIEUP-A
       (?빡 . "0xBE61") ; HANGUL SYLLABLE SSANGPIEUP-A-KIYEOK
       (?빤 . "0xBE64") ; HANGUL SYLLABLE SSANGPIEUP-A-NIEUN
       (?빨 . "0xBE68") ; HANGUL SYLLABLE SSANGPIEUP-A-RIEUL
       (?빪 . "0xBE6A") ; HANGUL SYLLABLE SSANGPIEUP-A-RIEULMIEUM
       (?빰 . "0xBE70") ; HANGUL SYLLABLE SSANGPIEUP-A-MIEUM
       (?빱 . "0xBE71") ; HANGUL SYLLABLE SSANGPIEUP-A-PIEUP
       (?빳 . "0xBE73") ; HANGUL SYLLABLE SSANGPIEUP-A-SIOS
       (?빴 . "0xBE74") ; HANGUL SYLLABLE SSANGPIEUP-A-SSANGSIOS
       (?빵 . "0xBE75") ; HANGUL SYLLABLE SSANGPIEUP-A-IEUNG
       (?빻 . "0xBE7B") ; HANGUL SYLLABLE SSANGPIEUP-A-HIEUH
       (?빼 . "0xBE7C") ; HANGUL SYLLABLE SSANGPIEUP-AE
       (?빽 . "0xBE7D") ; HANGUL SYLLABLE SSANGPIEUP-AE-KIYEOK
       (?뺀 . "0xBE80") ; HANGUL SYLLABLE SSANGPIEUP-AE-NIEUN
       (?뺄 . "0xBE84") ; HANGUL SYLLABLE SSANGPIEUP-AE-RIEUL
       (?뺌 . "0xBE8C") ; HANGUL SYLLABLE SSANGPIEUP-AE-MIEUM
       (?뺍 . "0xBE8D") ; HANGUL SYLLABLE SSANGPIEUP-AE-PIEUP
       (?뺏 . "0xBE8F") ; HANGUL SYLLABLE SSANGPIEUP-AE-SIOS
       (?뺐 . "0xBE90") ; HANGUL SYLLABLE SSANGPIEUP-AE-SSANGSIOS
       (?뺑 . "0xBE91") ; HANGUL SYLLABLE SSANGPIEUP-AE-IEUNG
       (?뺘 . "0xBE98") ; HANGUL SYLLABLE SSANGPIEUP-YA
       (?뺙 . "0xBE99") ; HANGUL SYLLABLE SSANGPIEUP-YA-KIYEOK
       (?뺨 . "0xBEA8") ; HANGUL SYLLABLE SSANGPIEUP-YA-MIEUM
       (?뻐 . "0xBED0") ; HANGUL SYLLABLE SSANGPIEUP-EO
       (?뻑 . "0xBED1") ; HANGUL SYLLABLE SSANGPIEUP-EO-KIYEOK
       (?뻔 . "0xBED4") ; HANGUL SYLLABLE SSANGPIEUP-EO-NIEUN
       (?뻗 . "0xBED7") ; HANGUL SYLLABLE SSANGPIEUP-EO-TIKEUT
       (?뻘 . "0xBED8") ; HANGUL SYLLABLE SSANGPIEUP-EO-RIEUL
       (?뻠 . "0xBEE0") ; HANGUL SYLLABLE SSANGPIEUP-EO-MIEUM
       (?뻣 . "0xBEE3") ; HANGUL SYLLABLE SSANGPIEUP-EO-SIOS
       (?뻤 . "0xBEE4") ; HANGUL SYLLABLE SSANGPIEUP-EO-SSANGSIOS
       (?뻥 . "0xBEE5") ; HANGUL SYLLABLE SSANGPIEUP-EO-IEUNG
       (?뻬 . "0xBEEC") ; HANGUL SYLLABLE SSANGPIEUP-E
       (?뼁 . "0xBF01") ; HANGUL SYLLABLE SSANGPIEUP-E-IEUNG
       (?뼈 . "0xBF08") ; HANGUL SYLLABLE SSANGPIEUP-YEO
       (?뼉 . "0xBF09") ; HANGUL SYLLABLE SSANGPIEUP-YEO-KIYEOK
       (?뼘 . "0xBF18") ; HANGUL SYLLABLE SSANGPIEUP-YEO-MIEUM
       (?뼙 . "0xBF19") ; HANGUL SYLLABLE SSANGPIEUP-YEO-PIEUP
       (?뼛 . "0xBF1B") ; HANGUL SYLLABLE SSANGPIEUP-YEO-SIOS
       (?뼜 . "0xBF1C") ; HANGUL SYLLABLE SSANGPIEUP-YEO-SSANGSIOS
       (?뼝 . "0xBF1D") ; HANGUL SYLLABLE SSANGPIEUP-YEO-IEUNG
       (?뽀 . "0xBF40") ; HANGUL SYLLABLE SSANGPIEUP-O
       (?뽁 . "0xBF41") ; HANGUL SYLLABLE SSANGPIEUP-O-KIYEOK
       (?뽄 . "0xBF44") ; HANGUL SYLLABLE SSANGPIEUP-O-NIEUN
       (?뽈 . "0xBF48") ; HANGUL SYLLABLE SSANGPIEUP-O-RIEUL
       (?뽐 . "0xBF50") ; HANGUL SYLLABLE SSANGPIEUP-O-MIEUM
       (?뽑 . "0xBF51") ; HANGUL SYLLABLE SSANGPIEUP-O-PIEUP
       (?뽕 . "0xBF55") ; HANGUL SYLLABLE SSANGPIEUP-O-IEUNG
       (?뾔 . "0xBF94") ; HANGUL SYLLABLE SSANGPIEUP-OE
       (?뾰 . "0xBFB0") ; HANGUL SYLLABLE SSANGPIEUP-YO
       (?뿅 . "0xBFC5") ; HANGUL SYLLABLE SSANGPIEUP-YO-IEUNG
       (?뿌 . "0xBFCC") ; HANGUL SYLLABLE SSANGPIEUP-U
       (?뿍 . "0xBFCD") ; HANGUL SYLLABLE SSANGPIEUP-U-KIYEOK
       (?뿐 . "0xBFD0") ; HANGUL SYLLABLE SSANGPIEUP-U-NIEUN
       (?뿔 . "0xBFD4") ; HANGUL SYLLABLE SSANGPIEUP-U-RIEUL
       (?뿜 . "0xBFDC") ; HANGUL SYLLABLE SSANGPIEUP-U-MIEUM
       (?뿟 . "0xBFDF") ; HANGUL SYLLABLE SSANGPIEUP-U-SIOS
       (?뿡 . "0xBFE1") ; HANGUL SYLLABLE SSANGPIEUP-U-IEUNG
       (?쀼 . "0xC03C") ; HANGUL SYLLABLE SSANGPIEUP-YU
       (?쁑 . "0xC051") ; HANGUL SYLLABLE SSANGPIEUP-YU-IEUNG
       (?쁘 . "0xC058") ; HANGUL SYLLABLE SSANGPIEUP-EU
       (?쁜 . "0xC05C") ; HANGUL SYLLABLE SSANGPIEUP-EU-NIEUN
       (?쁠 . "0xC060") ; HANGUL SYLLABLE SSANGPIEUP-EU-RIEUL
       (?쁨 . "0xC068") ; HANGUL SYLLABLE SSANGPIEUP-EU-MIEUM
       (?쁩 . "0xC069") ; HANGUL SYLLABLE SSANGPIEUP-EU-PIEUP
       (?삐 . "0xC090") ; HANGUL SYLLABLE SSANGPIEUP-I
       (?삑 . "0xC091") ; HANGUL SYLLABLE SSANGPIEUP-I-KIYEOK
       (?삔 . "0xC094") ; HANGUL SYLLABLE SSANGPIEUP-I-NIEUN
       (?삘 . "0xC098") ; HANGUL SYLLABLE SSANGPIEUP-I-RIEUL
       (?삠 . "0xC0A0") ; HANGUL SYLLABLE SSANGPIEUP-I-MIEUM
       (?삡 . "0xC0A1") ; HANGUL SYLLABLE SSANGPIEUP-I-PIEUP
       (?삣 . "0xC0A3") ; HANGUL SYLLABLE SSANGPIEUP-I-SIOS
       (?삥 . "0xC0A5") ; HANGUL SYLLABLE SSANGPIEUP-I-IEUNG
       (?사 . "0xC0AC") ; HANGUL SYLLABLE SIOS-A
       (?삭 . "0xC0AD") ; HANGUL SYLLABLE SIOS-A-KIYEOK
       (?삯 . "0xC0AF") ; HANGUL SYLLABLE SIOS-A-KIYEOKSIOS
       (?산 . "0xC0B0") ; HANGUL SYLLABLE SIOS-A-NIEUN
       (?삳 . "0xC0B3") ; HANGUL SYLLABLE SIOS-A-TIKEUT
       (?살 . "0xC0B4") ; HANGUL SYLLABLE SIOS-A-RIEUL
       (?삵 . "0xC0B5") ; HANGUL SYLLABLE SIOS-A-RIEULKIYEOK
       (?삶 . "0xC0B6") ; HANGUL SYLLABLE SIOS-A-RIEULMIEUM
       (?삼 . "0xC0BC") ; HANGUL SYLLABLE SIOS-A-MIEUM
       (?삽 . "0xC0BD") ; HANGUL SYLLABLE SIOS-A-PIEUP
       (?삿 . "0xC0BF") ; HANGUL SYLLABLE SIOS-A-SIOS
       (?샀 . "0xC0C0") ; HANGUL SYLLABLE SIOS-A-SSANGSIOS
       (?상 . "0xC0C1") ; HANGUL SYLLABLE SIOS-A-IEUNG
       (?샅 . "0xC0C5") ; HANGUL SYLLABLE SIOS-A-THIEUTH
       (?새 . "0xC0C8") ; HANGUL SYLLABLE SIOS-AE
       (?색 . "0xC0C9") ; HANGUL SYLLABLE SIOS-AE-KIYEOK
       (?샌 . "0xC0CC") ; HANGUL SYLLABLE SIOS-AE-NIEUN
       (?샐 . "0xC0D0") ; HANGUL SYLLABLE SIOS-AE-RIEUL
       (?샘 . "0xC0D8") ; HANGUL SYLLABLE SIOS-AE-MIEUM
       (?샙 . "0xC0D9") ; HANGUL SYLLABLE SIOS-AE-PIEUP
       (?샛 . "0xC0DB") ; HANGUL SYLLABLE SIOS-AE-SIOS
       (?샜 . "0xC0DC") ; HANGUL SYLLABLE SIOS-AE-SSANGSIOS
       (?생 . "0xC0DD") ; HANGUL SYLLABLE SIOS-AE-IEUNG
       (?샤 . "0xC0E4") ; HANGUL SYLLABLE SIOS-YA
       (?샥 . "0xC0E5") ; HANGUL SYLLABLE SIOS-YA-KIYEOK
       (?샨 . "0xC0E8") ; HANGUL SYLLABLE SIOS-YA-NIEUN
       (?샬 . "0xC0EC") ; HANGUL SYLLABLE SIOS-YA-RIEUL
       (?샴 . "0xC0F4") ; HANGUL SYLLABLE SIOS-YA-MIEUM
       (?샵 . "0xC0F5") ; HANGUL SYLLABLE SIOS-YA-PIEUP
       (?샷 . "0xC0F7") ; HANGUL SYLLABLE SIOS-YA-SIOS
       (?샹 . "0xC0F9") ; HANGUL SYLLABLE SIOS-YA-IEUNG
       (?섀 . "0xC100") ; HANGUL SYLLABLE SIOS-YAE
       (?섄 . "0xC104") ; HANGUL SYLLABLE SIOS-YAE-NIEUN
       (?섈 . "0xC108") ; HANGUL SYLLABLE SIOS-YAE-RIEUL
       (?섐 . "0xC110") ; HANGUL SYLLABLE SIOS-YAE-MIEUM
       (?섕 . "0xC115") ; HANGUL SYLLABLE SIOS-YAE-IEUNG
       (?서 . "0xC11C") ; HANGUL SYLLABLE SIOS-EO
       (?석 . "0xC11D") ; HANGUL SYLLABLE SIOS-EO-KIYEOK
       (?섞 . "0xC11E") ; HANGUL SYLLABLE SIOS-EO-SSANGKIYEOK
       (?섟 . "0xC11F") ; HANGUL SYLLABLE SIOS-EO-KIYEOKSIOS
       (?선 . "0xC120") ; HANGUL SYLLABLE SIOS-EO-NIEUN
       (?섣 . "0xC123") ; HANGUL SYLLABLE SIOS-EO-TIKEUT
       (?설 . "0xC124") ; HANGUL SYLLABLE SIOS-EO-RIEUL
       (?섦 . "0xC126") ; HANGUL SYLLABLE SIOS-EO-RIEULMIEUM
       (?섧 . "0xC127") ; HANGUL SYLLABLE SIOS-EO-RIEULPIEUP
       (?섬 . "0xC12C") ; HANGUL SYLLABLE SIOS-EO-MIEUM
       (?섭 . "0xC12D") ; HANGUL SYLLABLE SIOS-EO-PIEUP
       (?섯 . "0xC12F") ; HANGUL SYLLABLE SIOS-EO-SIOS
       (?섰 . "0xC130") ; HANGUL SYLLABLE SIOS-EO-SSANGSIOS
       (?성 . "0xC131") ; HANGUL SYLLABLE SIOS-EO-IEUNG
       (?섶 . "0xC136") ; HANGUL SYLLABLE SIOS-EO-PHIEUPH
       (?세 . "0xC138") ; HANGUL SYLLABLE SIOS-E
       (?섹 . "0xC139") ; HANGUL SYLLABLE SIOS-E-KIYEOK
       (?센 . "0xC13C") ; HANGUL SYLLABLE SIOS-E-NIEUN
       (?셀 . "0xC140") ; HANGUL SYLLABLE SIOS-E-RIEUL
       (?셈 . "0xC148") ; HANGUL SYLLABLE SIOS-E-MIEUM
       (?셉 . "0xC149") ; HANGUL SYLLABLE SIOS-E-PIEUP
       (?셋 . "0xC14B") ; HANGUL SYLLABLE SIOS-E-SIOS
       (?셌 . "0xC14C") ; HANGUL SYLLABLE SIOS-E-SSANGSIOS
       (?셍 . "0xC14D") ; HANGUL SYLLABLE SIOS-E-IEUNG
       (?셔 . "0xC154") ; HANGUL SYLLABLE SIOS-YEO
       (?셕 . "0xC155") ; HANGUL SYLLABLE SIOS-YEO-KIYEOK
       (?션 . "0xC158") ; HANGUL SYLLABLE SIOS-YEO-NIEUN
       (?셜 . "0xC15C") ; HANGUL SYLLABLE SIOS-YEO-RIEUL
       (?셤 . "0xC164") ; HANGUL SYLLABLE SIOS-YEO-MIEUM
       (?셥 . "0xC165") ; HANGUL SYLLABLE SIOS-YEO-PIEUP
       (?셧 . "0xC167") ; HANGUL SYLLABLE SIOS-YEO-SIOS
       (?셨 . "0xC168") ; HANGUL SYLLABLE SIOS-YEO-SSANGSIOS
       (?셩 . "0xC169") ; HANGUL SYLLABLE SIOS-YEO-IEUNG
       (?셰 . "0xC170") ; HANGUL SYLLABLE SIOS-YE
       (?셴 . "0xC174") ; HANGUL SYLLABLE SIOS-YE-NIEUN
       (?셸 . "0xC178") ; HANGUL SYLLABLE SIOS-YE-RIEUL
       (?솅 . "0xC185") ; HANGUL SYLLABLE SIOS-YE-IEUNG
       (?소 . "0xC18C") ; HANGUL SYLLABLE SIOS-O
       (?속 . "0xC18D") ; HANGUL SYLLABLE SIOS-O-KIYEOK
       (?솎 . "0xC18E") ; HANGUL SYLLABLE SIOS-O-SSANGKIYEOK
       (?손 . "0xC190") ; HANGUL SYLLABLE SIOS-O-NIEUN
       (?솔 . "0xC194") ; HANGUL SYLLABLE SIOS-O-RIEUL
       (?솖 . "0xC196") ; HANGUL SYLLABLE SIOS-O-RIEULMIEUM
       (?솜 . "0xC19C") ; HANGUL SYLLABLE SIOS-O-MIEUM
       (?솝 . "0xC19D") ; HANGUL SYLLABLE SIOS-O-PIEUP
       (?솟 . "0xC19F") ; HANGUL SYLLABLE SIOS-O-SIOS
       (?송 . "0xC1A1") ; HANGUL SYLLABLE SIOS-O-IEUNG
       (?솥 . "0xC1A5") ; HANGUL SYLLABLE SIOS-O-THIEUTH
       (?솨 . "0xC1A8") ; HANGUL SYLLABLE SIOS-WA
       (?솩 . "0xC1A9") ; HANGUL SYLLABLE SIOS-WA-KIYEOK
       (?솬 . "0xC1AC") ; HANGUL SYLLABLE SIOS-WA-NIEUN
       (?솰 . "0xC1B0") ; HANGUL SYLLABLE SIOS-WA-RIEUL
       (?솽 . "0xC1BD") ; HANGUL SYLLABLE SIOS-WA-IEUNG
       (?쇄 . "0xC1C4") ; HANGUL SYLLABLE SIOS-WAE
       (?쇈 . "0xC1C8") ; HANGUL SYLLABLE SIOS-WAE-NIEUN
       (?쇌 . "0xC1CC") ; HANGUL SYLLABLE SIOS-WAE-RIEUL
       (?쇔 . "0xC1D4") ; HANGUL SYLLABLE SIOS-WAE-MIEUM
       (?쇗 . "0xC1D7") ; HANGUL SYLLABLE SIOS-WAE-SIOS
       (?쇘 . "0xC1D8") ; HANGUL SYLLABLE SIOS-WAE-SSANGSIOS
       (?쇠 . "0xC1E0") ; HANGUL SYLLABLE SIOS-OE
       (?쇤 . "0xC1E4") ; HANGUL SYLLABLE SIOS-OE-NIEUN
       (?쇨 . "0xC1E8") ; HANGUL SYLLABLE SIOS-OE-RIEUL
       (?쇰 . "0xC1F0") ; HANGUL SYLLABLE SIOS-OE-MIEUM
       (?쇱 . "0xC1F1") ; HANGUL SYLLABLE SIOS-OE-PIEUP
       (?쇳 . "0xC1F3") ; HANGUL SYLLABLE SIOS-OE-SIOS
       (?쇼 . "0xC1FC") ; HANGUL SYLLABLE SIOS-YO
       (?쇽 . "0xC1FD") ; HANGUL SYLLABLE SIOS-YO-KIYEOK
       (?숀 . "0xC200") ; HANGUL SYLLABLE SIOS-YO-NIEUN
       (?숄 . "0xC204") ; HANGUL SYLLABLE SIOS-YO-RIEUL
       (?숌 . "0xC20C") ; HANGUL SYLLABLE SIOS-YO-MIEUM
       (?숍 . "0xC20D") ; HANGUL SYLLABLE SIOS-YO-PIEUP
       (?숏 . "0xC20F") ; HANGUL SYLLABLE SIOS-YO-SIOS
       (?숑 . "0xC211") ; HANGUL SYLLABLE SIOS-YO-IEUNG
       (?수 . "0xC218") ; HANGUL SYLLABLE SIOS-U
       (?숙 . "0xC219") ; HANGUL SYLLABLE SIOS-U-KIYEOK
       (?순 . "0xC21C") ; HANGUL SYLLABLE SIOS-U-NIEUN
       (?숟 . "0xC21F") ; HANGUL SYLLABLE SIOS-U-TIKEUT
       (?술 . "0xC220") ; HANGUL SYLLABLE SIOS-U-RIEUL
       (?숨 . "0xC228") ; HANGUL SYLLABLE SIOS-U-MIEUM
       (?숩 . "0xC229") ; HANGUL SYLLABLE SIOS-U-PIEUP
       (?숫 . "0xC22B") ; HANGUL SYLLABLE SIOS-U-SIOS
       (?숭 . "0xC22D") ; HANGUL SYLLABLE SIOS-U-IEUNG
       (?숯 . "0xC22F") ; HANGUL SYLLABLE SIOS-U-CHIEUCH
       (?숱 . "0xC231") ; HANGUL SYLLABLE SIOS-U-THIEUTH
       (?숲 . "0xC232") ; HANGUL SYLLABLE SIOS-U-PHIEUPH
       (?숴 . "0xC234") ; HANGUL SYLLABLE SIOS-WEO
       (?쉈 . "0xC248") ; HANGUL SYLLABLE SIOS-WEO-SSANGSIOS
       (?쉐 . "0xC250") ; HANGUL SYLLABLE SIOS-WE
       (?쉑 . "0xC251") ; HANGUL SYLLABLE SIOS-WE-KIYEOK
       (?쉔 . "0xC254") ; HANGUL SYLLABLE SIOS-WE-NIEUN
       (?쉘 . "0xC258") ; HANGUL SYLLABLE SIOS-WE-RIEUL
       (?쉠 . "0xC260") ; HANGUL SYLLABLE SIOS-WE-MIEUM
       (?쉥 . "0xC265") ; HANGUL SYLLABLE SIOS-WE-IEUNG
       (?쉬 . "0xC26C") ; HANGUL SYLLABLE SIOS-WI
       (?쉭 . "0xC26D") ; HANGUL SYLLABLE SIOS-WI-KIYEOK
       (?쉰 . "0xC270") ; HANGUL SYLLABLE SIOS-WI-NIEUN
       (?쉴 . "0xC274") ; HANGUL SYLLABLE SIOS-WI-RIEUL
       (?쉼 . "0xC27C") ; HANGUL SYLLABLE SIOS-WI-MIEUM
       (?쉽 . "0xC27D") ; HANGUL SYLLABLE SIOS-WI-PIEUP
       (?쉿 . "0xC27F") ; HANGUL SYLLABLE SIOS-WI-SIOS
       (?슁 . "0xC281") ; HANGUL SYLLABLE SIOS-WI-IEUNG
       (?슈 . "0xC288") ; HANGUL SYLLABLE SIOS-YU
       (?슉 . "0xC289") ; HANGUL SYLLABLE SIOS-YU-KIYEOK
       (?슐 . "0xC290") ; HANGUL SYLLABLE SIOS-YU-RIEUL
       (?슘 . "0xC298") ; HANGUL SYLLABLE SIOS-YU-MIEUM
       (?슛 . "0xC29B") ; HANGUL SYLLABLE SIOS-YU-SIOS
       (?슝 . "0xC29D") ; HANGUL SYLLABLE SIOS-YU-IEUNG
       (?스 . "0xC2A4") ; HANGUL SYLLABLE SIOS-EU
       (?슥 . "0xC2A5") ; HANGUL SYLLABLE SIOS-EU-KIYEOK
       (?슨 . "0xC2A8") ; HANGUL SYLLABLE SIOS-EU-NIEUN
       (?슬 . "0xC2AC") ; HANGUL SYLLABLE SIOS-EU-RIEUL
       (?슭 . "0xC2AD") ; HANGUL SYLLABLE SIOS-EU-RIEULKIYEOK
       (?슴 . "0xC2B4") ; HANGUL SYLLABLE SIOS-EU-MIEUM
       (?습 . "0xC2B5") ; HANGUL SYLLABLE SIOS-EU-PIEUP
       (?슷 . "0xC2B7") ; HANGUL SYLLABLE SIOS-EU-SIOS
       (?승 . "0xC2B9") ; HANGUL SYLLABLE SIOS-EU-IEUNG
       (?시 . "0xC2DC") ; HANGUL SYLLABLE SIOS-I
       (?식 . "0xC2DD") ; HANGUL SYLLABLE SIOS-I-KIYEOK
       (?신 . "0xC2E0") ; HANGUL SYLLABLE SIOS-I-NIEUN
       (?싣 . "0xC2E3") ; HANGUL SYLLABLE SIOS-I-TIKEUT
       (?실 . "0xC2E4") ; HANGUL SYLLABLE SIOS-I-RIEUL
       (?싫 . "0xC2EB") ; HANGUL SYLLABLE SIOS-I-RIEULHIEUH
       (?심 . "0xC2EC") ; HANGUL SYLLABLE SIOS-I-MIEUM
       (?십 . "0xC2ED") ; HANGUL SYLLABLE SIOS-I-PIEUP
       (?싯 . "0xC2EF") ; HANGUL SYLLABLE SIOS-I-SIOS
       (?싱 . "0xC2F1") ; HANGUL SYLLABLE SIOS-I-IEUNG
       (?싶 . "0xC2F6") ; HANGUL SYLLABLE SIOS-I-PHIEUPH
       (?싸 . "0xC2F8") ; HANGUL SYLLABLE SSANGSIOS-A
       (?싹 . "0xC2F9") ; HANGUL SYLLABLE SSANGSIOS-A-KIYEOK
       (?싻 . "0xC2FB") ; HANGUL SYLLABLE SSANGSIOS-A-KIYEOKSIOS
       (?싼 . "0xC2FC") ; HANGUL SYLLABLE SSANGSIOS-A-NIEUN
       (?쌀 . "0xC300") ; HANGUL SYLLABLE SSANGSIOS-A-RIEUL
       (?쌈 . "0xC308") ; HANGUL SYLLABLE SSANGSIOS-A-MIEUM
       (?쌉 . "0xC309") ; HANGUL SYLLABLE SSANGSIOS-A-PIEUP
       (?쌌 . "0xC30C") ; HANGUL SYLLABLE SSANGSIOS-A-SSANGSIOS
       (?쌍 . "0xC30D") ; HANGUL SYLLABLE SSANGSIOS-A-IEUNG
       (?쌓 . "0xC313") ; HANGUL SYLLABLE SSANGSIOS-A-HIEUH
       (?쌔 . "0xC314") ; HANGUL SYLLABLE SSANGSIOS-AE
       (?쌕 . "0xC315") ; HANGUL SYLLABLE SSANGSIOS-AE-KIYEOK
       (?쌘 . "0xC318") ; HANGUL SYLLABLE SSANGSIOS-AE-NIEUN
       (?쌜 . "0xC31C") ; HANGUL SYLLABLE SSANGSIOS-AE-RIEUL
       (?쌤 . "0xC324") ; HANGUL SYLLABLE SSANGSIOS-AE-MIEUM
       (?쌥 . "0xC325") ; HANGUL SYLLABLE SSANGSIOS-AE-PIEUP
       (?쌨 . "0xC328") ; HANGUL SYLLABLE SSANGSIOS-AE-SSANGSIOS
       (?쌩 . "0xC329") ; HANGUL SYLLABLE SSANGSIOS-AE-IEUNG
       (?썅 . "0xC345") ; HANGUL SYLLABLE SSANGSIOS-YA-IEUNG
       (?써 . "0xC368") ; HANGUL SYLLABLE SSANGSIOS-EO
       (?썩 . "0xC369") ; HANGUL SYLLABLE SSANGSIOS-EO-KIYEOK
       (?썬 . "0xC36C") ; HANGUL SYLLABLE SSANGSIOS-EO-NIEUN
       (?썰 . "0xC370") ; HANGUL SYLLABLE SSANGSIOS-EO-RIEUL
       (?썲 . "0xC372") ; HANGUL SYLLABLE SSANGSIOS-EO-RIEULMIEUM
       (?썸 . "0xC378") ; HANGUL SYLLABLE SSANGSIOS-EO-MIEUM
       (?썹 . "0xC379") ; HANGUL SYLLABLE SSANGSIOS-EO-PIEUP
       (?썼 . "0xC37C") ; HANGUL SYLLABLE SSANGSIOS-EO-SSANGSIOS
       (?썽 . "0xC37D") ; HANGUL SYLLABLE SSANGSIOS-EO-IEUNG
       (?쎄 . "0xC384") ; HANGUL SYLLABLE SSANGSIOS-E
       (?쎈 . "0xC388") ; HANGUL SYLLABLE SSANGSIOS-E-NIEUN
       (?쎌 . "0xC38C") ; HANGUL SYLLABLE SSANGSIOS-E-RIEUL
       (?쏀 . "0xC3C0") ; HANGUL SYLLABLE SSANGSIOS-YE-NIEUN
       (?쏘 . "0xC3D8") ; HANGUL SYLLABLE SSANGSIOS-O
       (?쏙 . "0xC3D9") ; HANGUL SYLLABLE SSANGSIOS-O-KIYEOK
       (?쏜 . "0xC3DC") ; HANGUL SYLLABLE SSANGSIOS-O-NIEUN
       (?쏟 . "0xC3DF") ; HANGUL SYLLABLE SSANGSIOS-O-TIKEUT
       (?쏠 . "0xC3E0") ; HANGUL SYLLABLE SSANGSIOS-O-RIEUL
       (?쏢 . "0xC3E2") ; HANGUL SYLLABLE SSANGSIOS-O-RIEULMIEUM
       (?쏨 . "0xC3E8") ; HANGUL SYLLABLE SSANGSIOS-O-MIEUM
       (?쏩 . "0xC3E9") ; HANGUL SYLLABLE SSANGSIOS-O-PIEUP
       (?쏭 . "0xC3ED") ; HANGUL SYLLABLE SSANGSIOS-O-IEUNG
       (?쏴 . "0xC3F4") ; HANGUL SYLLABLE SSANGSIOS-WA
       (?쏵 . "0xC3F5") ; HANGUL SYLLABLE SSANGSIOS-WA-KIYEOK
       (?쏸 . "0xC3F8") ; HANGUL SYLLABLE SSANGSIOS-WA-NIEUN
       (?쐈 . "0xC408") ; HANGUL SYLLABLE SSANGSIOS-WA-SSANGSIOS
       (?쐐 . "0xC410") ; HANGUL SYLLABLE SSANGSIOS-WAE
       (?쐤 . "0xC424") ; HANGUL SYLLABLE SSANGSIOS-WAE-SSANGSIOS
       (?쐬 . "0xC42C") ; HANGUL SYLLABLE SSANGSIOS-OE
       (?쐰 . "0xC430") ; HANGUL SYLLABLE SSANGSIOS-OE-NIEUN
       (?쐴 . "0xC434") ; HANGUL SYLLABLE SSANGSIOS-OE-RIEUL
       (?쐼 . "0xC43C") ; HANGUL SYLLABLE SSANGSIOS-OE-MIEUM
       (?쐽 . "0xC43D") ; HANGUL SYLLABLE SSANGSIOS-OE-PIEUP
       (?쑈 . "0xC448") ; HANGUL SYLLABLE SSANGSIOS-YO
       (?쑤 . "0xC464") ; HANGUL SYLLABLE SSANGSIOS-U
       (?쑥 . "0xC465") ; HANGUL SYLLABLE SSANGSIOS-U-KIYEOK
       (?쑨 . "0xC468") ; HANGUL SYLLABLE SSANGSIOS-U-NIEUN
       (?쑬 . "0xC46C") ; HANGUL SYLLABLE SSANGSIOS-U-RIEUL
       (?쑴 . "0xC474") ; HANGUL SYLLABLE SSANGSIOS-U-MIEUM
       (?쑵 . "0xC475") ; HANGUL SYLLABLE SSANGSIOS-U-PIEUP
       (?쑹 . "0xC479") ; HANGUL SYLLABLE SSANGSIOS-U-IEUNG
       (?쒀 . "0xC480") ; HANGUL SYLLABLE SSANGSIOS-WEO
       (?쒔 . "0xC494") ; HANGUL SYLLABLE SSANGSIOS-WEO-SSANGSIOS
       (?쒜 . "0xC49C") ; HANGUL SYLLABLE SSANGSIOS-WE
       (?쒸 . "0xC4B8") ; HANGUL SYLLABLE SSANGSIOS-WI
       (?쒼 . "0xC4BC") ; HANGUL SYLLABLE SSANGSIOS-WI-NIEUN
       (?쓩 . "0xC4E9") ; HANGUL SYLLABLE SSANGSIOS-YU-IEUNG
       (?쓰 . "0xC4F0") ; HANGUL SYLLABLE SSANGSIOS-EU
       (?쓱 . "0xC4F1") ; HANGUL SYLLABLE SSANGSIOS-EU-KIYEOK
       (?쓴 . "0xC4F4") ; HANGUL SYLLABLE SSANGSIOS-EU-NIEUN
       (?쓸 . "0xC4F8") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEUL
       (?쓺 . "0xC4FA") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEULMIEUM
       (?쓿 . "0xC4FF") ; HANGUL SYLLABLE SSANGSIOS-EU-RIEULHIEUH
       (?씀 . "0xC500") ; HANGUL SYLLABLE SSANGSIOS-EU-MIEUM
       (?씁 . "0xC501") ; HANGUL SYLLABLE SSANGSIOS-EU-PIEUP
       (?씌 . "0xC50C") ; HANGUL SYLLABLE SSANGSIOS-YI
       (?씐 . "0xC510") ; HANGUL SYLLABLE SSANGSIOS-YI-NIEUN
       (?씔 . "0xC514") ; HANGUL SYLLABLE SSANGSIOS-YI-RIEUL
       (?씜 . "0xC51C") ; HANGUL SYLLABLE SSANGSIOS-YI-MIEUM
       (?씨 . "0xC528") ; HANGUL SYLLABLE SSANGSIOS-I
       (?씩 . "0xC529") ; HANGUL SYLLABLE SSANGSIOS-I-KIYEOK
       (?씬 . "0xC52C") ; HANGUL SYLLABLE SSANGSIOS-I-NIEUN
       (?씰 . "0xC530") ; HANGUL SYLLABLE SSANGSIOS-I-RIEUL
       (?씸 . "0xC538") ; HANGUL SYLLABLE SSANGSIOS-I-MIEUM
       (?씹 . "0xC539") ; HANGUL SYLLABLE SSANGSIOS-I-PIEUP
       (?씻 . "0xC53B") ; HANGUL SYLLABLE SSANGSIOS-I-SIOS
       (?씽 . "0xC53D") ; HANGUL SYLLABLE SSANGSIOS-I-IEUNG
       (?아 . "0xC544") ; HANGUL SYLLABLE IEUNG-A
       (?악 . "0xC545") ; HANGUL SYLLABLE IEUNG-A-KIYEOK
       (?안 . "0xC548") ; HANGUL SYLLABLE IEUNG-A-NIEUN
       (?앉 . "0xC549") ; HANGUL SYLLABLE IEUNG-A-NIEUNCIEUC
       (?않 . "0xC54A") ; HANGUL SYLLABLE IEUNG-A-NIEUNHIEUH
       (?알 . "0xC54C") ; HANGUL SYLLABLE IEUNG-A-RIEUL
       (?앍 . "0xC54D") ; HANGUL SYLLABLE IEUNG-A-RIEULKIYEOK
       (?앎 . "0xC54E") ; HANGUL SYLLABLE IEUNG-A-RIEULMIEUM
       (?앓 . "0xC553") ; HANGUL SYLLABLE IEUNG-A-RIEULHIEUH
       (?암 . "0xC554") ; HANGUL SYLLABLE IEUNG-A-MIEUM
       (?압 . "0xC555") ; HANGUL SYLLABLE IEUNG-A-PIEUP
       (?앗 . "0xC557") ; HANGUL SYLLABLE IEUNG-A-SIOS
       (?았 . "0xC558") ; HANGUL SYLLABLE IEUNG-A-SSANGSIOS
       (?앙 . "0xC559") ; HANGUL SYLLABLE IEUNG-A-IEUNG
       (?앝 . "0xC55D") ; HANGUL SYLLABLE IEUNG-A-THIEUTH
       (?앞 . "0xC55E") ; HANGUL SYLLABLE IEUNG-A-PHIEUPH
       (?애 . "0xC560") ; HANGUL SYLLABLE IEUNG-AE
       (?액 . "0xC561") ; HANGUL SYLLABLE IEUNG-AE-KIYEOK
       (?앤 . "0xC564") ; HANGUL SYLLABLE IEUNG-AE-NIEUN
       (?앨 . "0xC568") ; HANGUL SYLLABLE IEUNG-AE-RIEUL
       (?앰 . "0xC570") ; HANGUL SYLLABLE IEUNG-AE-MIEUM
       (?앱 . "0xC571") ; HANGUL SYLLABLE IEUNG-AE-PIEUP
       (?앳 . "0xC573") ; HANGUL SYLLABLE IEUNG-AE-SIOS
       (?앴 . "0xC574") ; HANGUL SYLLABLE IEUNG-AE-SSANGSIOS
       (?앵 . "0xC575") ; HANGUL SYLLABLE IEUNG-AE-IEUNG
       (?야 . "0xC57C") ; HANGUL SYLLABLE IEUNG-YA
       (?약 . "0xC57D") ; HANGUL SYLLABLE IEUNG-YA-KIYEOK
       (?얀 . "0xC580") ; HANGUL SYLLABLE IEUNG-YA-NIEUN
       (?얄 . "0xC584") ; HANGUL SYLLABLE IEUNG-YA-RIEUL
       (?얇 . "0xC587") ; HANGUL SYLLABLE IEUNG-YA-RIEULPIEUP
       (?얌 . "0xC58C") ; HANGUL SYLLABLE IEUNG-YA-MIEUM
       (?얍 . "0xC58D") ; HANGUL SYLLABLE IEUNG-YA-PIEUP
       (?얏 . "0xC58F") ; HANGUL SYLLABLE IEUNG-YA-SIOS
       (?양 . "0xC591") ; HANGUL SYLLABLE IEUNG-YA-IEUNG
       (?얕 . "0xC595") ; HANGUL SYLLABLE IEUNG-YA-THIEUTH
       (?얗 . "0xC597") ; HANGUL SYLLABLE IEUNG-YA-HIEUH
       (?얘 . "0xC598") ; HANGUL SYLLABLE IEUNG-YAE
       (?얜 . "0xC59C") ; HANGUL SYLLABLE IEUNG-YAE-NIEUN
       (?얠 . "0xC5A0") ; HANGUL SYLLABLE IEUNG-YAE-RIEUL
       (?얩 . "0xC5A9") ; HANGUL SYLLABLE IEUNG-YAE-PIEUP
       (?어 . "0xC5B4") ; HANGUL SYLLABLE IEUNG-EO
       (?억 . "0xC5B5") ; HANGUL SYLLABLE IEUNG-EO-KIYEOK
       (?언 . "0xC5B8") ; HANGUL SYLLABLE IEUNG-EO-NIEUN
       (?얹 . "0xC5B9") ; HANGUL SYLLABLE IEUNG-EO-NIEUNCIEUC
       (?얻 . "0xC5BB") ; HANGUL SYLLABLE IEUNG-EO-TIKEUT
       (?얼 . "0xC5BC") ; HANGUL SYLLABLE IEUNG-EO-RIEUL
       (?얽 . "0xC5BD") ; HANGUL SYLLABLE IEUNG-EO-RIEULKIYEOK
       (?얾 . "0xC5BE") ; HANGUL SYLLABLE IEUNG-EO-RIEULMIEUM
       (?엄 . "0xC5C4") ; HANGUL SYLLABLE IEUNG-EO-MIEUM
       (?업 . "0xC5C5") ; HANGUL SYLLABLE IEUNG-EO-PIEUP
       (?없 . "0xC5C6") ; HANGUL SYLLABLE IEUNG-EO-PIEUPSIOS
       (?엇 . "0xC5C7") ; HANGUL SYLLABLE IEUNG-EO-SIOS
       (?었 . "0xC5C8") ; HANGUL SYLLABLE IEUNG-EO-SSANGSIOS
       (?엉 . "0xC5C9") ; HANGUL SYLLABLE IEUNG-EO-IEUNG
       (?엊 . "0xC5CA") ; HANGUL SYLLABLE IEUNG-EO-CIEUC
       (?엌 . "0xC5CC") ; HANGUL SYLLABLE IEUNG-EO-KHIEUKH
       (?엎 . "0xC5CE") ; HANGUL SYLLABLE IEUNG-EO-PHIEUPH
       (?에 . "0xC5D0") ; HANGUL SYLLABLE IEUNG-E
       (?엑 . "0xC5D1") ; HANGUL SYLLABLE IEUNG-E-KIYEOK
       (?엔 . "0xC5D4") ; HANGUL SYLLABLE IEUNG-E-NIEUN
       (?엘 . "0xC5D8") ; HANGUL SYLLABLE IEUNG-E-RIEUL
       (?엠 . "0xC5E0") ; HANGUL SYLLABLE IEUNG-E-MIEUM
       (?엡 . "0xC5E1") ; HANGUL SYLLABLE IEUNG-E-PIEUP
       (?엣 . "0xC5E3") ; HANGUL SYLLABLE IEUNG-E-SIOS
       (?엥 . "0xC5E5") ; HANGUL SYLLABLE IEUNG-E-IEUNG
       (?여 . "0xC5EC") ; HANGUL SYLLABLE IEUNG-YEO
       (?역 . "0xC5ED") ; HANGUL SYLLABLE IEUNG-YEO-KIYEOK
       (?엮 . "0xC5EE") ; HANGUL SYLLABLE IEUNG-YEO-SSANGKIYEOK
       (?연 . "0xC5F0") ; HANGUL SYLLABLE IEUNG-YEO-NIEUN
       (?열 . "0xC5F4") ; HANGUL SYLLABLE IEUNG-YEO-RIEUL
       (?엶 . "0xC5F6") ; HANGUL SYLLABLE IEUNG-YEO-RIEULMIEUM
       (?엷 . "0xC5F7") ; HANGUL SYLLABLE IEUNG-YEO-RIEULPIEUP
       (?염 . "0xC5FC") ; HANGUL SYLLABLE IEUNG-YEO-MIEUM
       (?엽 . "0xC5FD") ; HANGUL SYLLABLE IEUNG-YEO-PIEUP
       (?엾 . "0xC5FE") ; HANGUL SYLLABLE IEUNG-YEO-PIEUPSIOS
       (?엿 . "0xC5FF") ; HANGUL SYLLABLE IEUNG-YEO-SIOS
       (?였 . "0xC600") ; HANGUL SYLLABLE IEUNG-YEO-SSANGSIOS
       (?영 . "0xC601") ; HANGUL SYLLABLE IEUNG-YEO-IEUNG
       (?옅 . "0xC605") ; HANGUL SYLLABLE IEUNG-YEO-THIEUTH
       (?옆 . "0xC606") ; HANGUL SYLLABLE IEUNG-YEO-PHIEUPH
       (?옇 . "0xC607") ; HANGUL SYLLABLE IEUNG-YEO-HIEUH
       (?예 . "0xC608") ; HANGUL SYLLABLE IEUNG-YE
       (?옌 . "0xC60C") ; HANGUL SYLLABLE IEUNG-YE-NIEUN
       (?옐 . "0xC610") ; HANGUL SYLLABLE IEUNG-YE-RIEUL
       (?옘 . "0xC618") ; HANGUL SYLLABLE IEUNG-YE-MIEUM
       (?옙 . "0xC619") ; HANGUL SYLLABLE IEUNG-YE-PIEUP
       (?옛 . "0xC61B") ; HANGUL SYLLABLE IEUNG-YE-SIOS
       (?옜 . "0xC61C") ; HANGUL SYLLABLE IEUNG-YE-SSANGSIOS
       (?오 . "0xC624") ; HANGUL SYLLABLE IEUNG-O
       (?옥 . "0xC625") ; HANGUL SYLLABLE IEUNG-O-KIYEOK
       (?온 . "0xC628") ; HANGUL SYLLABLE IEUNG-O-NIEUN
       (?올 . "0xC62C") ; HANGUL SYLLABLE IEUNG-O-RIEUL
       (?옭 . "0xC62D") ; HANGUL SYLLABLE IEUNG-O-RIEULKIYEOK
       (?옮 . "0xC62E") ; HANGUL SYLLABLE IEUNG-O-RIEULMIEUM
       (?옰 . "0xC630") ; HANGUL SYLLABLE IEUNG-O-RIEULSIOS
       (?옳 . "0xC633") ; HANGUL SYLLABLE IEUNG-O-RIEULHIEUH
       (?옴 . "0xC634") ; HANGUL SYLLABLE IEUNG-O-MIEUM
       (?옵 . "0xC635") ; HANGUL SYLLABLE IEUNG-O-PIEUP
       (?옷 . "0xC637") ; HANGUL SYLLABLE IEUNG-O-SIOS
       (?옹 . "0xC639") ; HANGUL SYLLABLE IEUNG-O-IEUNG
       (?옻 . "0xC63B") ; HANGUL SYLLABLE IEUNG-O-CHIEUCH
       (?와 . "0xC640") ; HANGUL SYLLABLE IEUNG-WA
       (?왁 . "0xC641") ; HANGUL SYLLABLE IEUNG-WA-KIYEOK
       (?완 . "0xC644") ; HANGUL SYLLABLE IEUNG-WA-NIEUN
       (?왈 . "0xC648") ; HANGUL SYLLABLE IEUNG-WA-RIEUL
       (?왐 . "0xC650") ; HANGUL SYLLABLE IEUNG-WA-MIEUM
       (?왑 . "0xC651") ; HANGUL SYLLABLE IEUNG-WA-PIEUP
       (?왓 . "0xC653") ; HANGUL SYLLABLE IEUNG-WA-SIOS
       (?왔 . "0xC654") ; HANGUL SYLLABLE IEUNG-WA-SSANGSIOS
       (?왕 . "0xC655") ; HANGUL SYLLABLE IEUNG-WA-IEUNG
       (?왜 . "0xC65C") ; HANGUL SYLLABLE IEUNG-WAE
       (?왝 . "0xC65D") ; HANGUL SYLLABLE IEUNG-WAE-KIYEOK
       (?왠 . "0xC660") ; HANGUL SYLLABLE IEUNG-WAE-NIEUN
       (?왬 . "0xC66C") ; HANGUL SYLLABLE IEUNG-WAE-MIEUM
       (?왯 . "0xC66F") ; HANGUL SYLLABLE IEUNG-WAE-SIOS
       (?왱 . "0xC671") ; HANGUL SYLLABLE IEUNG-WAE-IEUNG
       (?외 . "0xC678") ; HANGUL SYLLABLE IEUNG-OE
       (?왹 . "0xC679") ; HANGUL SYLLABLE IEUNG-OE-KIYEOK
       (?왼 . "0xC67C") ; HANGUL SYLLABLE IEUNG-OE-NIEUN
       (?욀 . "0xC680") ; HANGUL SYLLABLE IEUNG-OE-RIEUL
       (?욈 . "0xC688") ; HANGUL SYLLABLE IEUNG-OE-MIEUM
       (?욉 . "0xC689") ; HANGUL SYLLABLE IEUNG-OE-PIEUP
       (?욋 . "0xC68B") ; HANGUL SYLLABLE IEUNG-OE-SIOS
       (?욍 . "0xC68D") ; HANGUL SYLLABLE IEUNG-OE-IEUNG
       (?요 . "0xC694") ; HANGUL SYLLABLE IEUNG-YO
       (?욕 . "0xC695") ; HANGUL SYLLABLE IEUNG-YO-KIYEOK
       (?욘 . "0xC698") ; HANGUL SYLLABLE IEUNG-YO-NIEUN
       (?욜 . "0xC69C") ; HANGUL SYLLABLE IEUNG-YO-RIEUL
       (?욤 . "0xC6A4") ; HANGUL SYLLABLE IEUNG-YO-MIEUM
       (?욥 . "0xC6A5") ; HANGUL SYLLABLE IEUNG-YO-PIEUP
       (?욧 . "0xC6A7") ; HANGUL SYLLABLE IEUNG-YO-SIOS
       (?용 . "0xC6A9") ; HANGUL SYLLABLE IEUNG-YO-IEUNG
       (?우 . "0xC6B0") ; HANGUL SYLLABLE IEUNG-U
       (?욱 . "0xC6B1") ; HANGUL SYLLABLE IEUNG-U-KIYEOK
       (?운 . "0xC6B4") ; HANGUL SYLLABLE IEUNG-U-NIEUN
       (?울 . "0xC6B8") ; HANGUL SYLLABLE IEUNG-U-RIEUL
       (?욹 . "0xC6B9") ; HANGUL SYLLABLE IEUNG-U-RIEULKIYEOK
       (?욺 . "0xC6BA") ; HANGUL SYLLABLE IEUNG-U-RIEULMIEUM
       (?움 . "0xC6C0") ; HANGUL SYLLABLE IEUNG-U-MIEUM
       (?웁 . "0xC6C1") ; HANGUL SYLLABLE IEUNG-U-PIEUP
       (?웃 . "0xC6C3") ; HANGUL SYLLABLE IEUNG-U-SIOS
       (?웅 . "0xC6C5") ; HANGUL SYLLABLE IEUNG-U-IEUNG
       (?워 . "0xC6CC") ; HANGUL SYLLABLE IEUNG-WEO
       (?웍 . "0xC6CD") ; HANGUL SYLLABLE IEUNG-WEO-KIYEOK
       (?원 . "0xC6D0") ; HANGUL SYLLABLE IEUNG-WEO-NIEUN
       (?월 . "0xC6D4") ; HANGUL SYLLABLE IEUNG-WEO-RIEUL
       (?웜 . "0xC6DC") ; HANGUL SYLLABLE IEUNG-WEO-MIEUM
       (?웝 . "0xC6DD") ; HANGUL SYLLABLE IEUNG-WEO-PIEUP
       (?웠 . "0xC6E0") ; HANGUL SYLLABLE IEUNG-WEO-SSANGSIOS
       (?웡 . "0xC6E1") ; HANGUL SYLLABLE IEUNG-WEO-IEUNG
       (?웨 . "0xC6E8") ; HANGUL SYLLABLE IEUNG-WE
       (?웩 . "0xC6E9") ; HANGUL SYLLABLE IEUNG-WE-KIYEOK
       (?웬 . "0xC6EC") ; HANGUL SYLLABLE IEUNG-WE-NIEUN
       (?웰 . "0xC6F0") ; HANGUL SYLLABLE IEUNG-WE-RIEUL
       (?웸 . "0xC6F8") ; HANGUL SYLLABLE IEUNG-WE-MIEUM
       (?웹 . "0xC6F9") ; HANGUL SYLLABLE IEUNG-WE-PIEUP
       (?웽 . "0xC6FD") ; HANGUL SYLLABLE IEUNG-WE-IEUNG
       (?위 . "0xC704") ; HANGUL SYLLABLE IEUNG-WI
       (?윅 . "0xC705") ; HANGUL SYLLABLE IEUNG-WI-KIYEOK
       (?윈 . "0xC708") ; HANGUL SYLLABLE IEUNG-WI-NIEUN
       (?윌 . "0xC70C") ; HANGUL SYLLABLE IEUNG-WI-RIEUL
       (?윔 . "0xC714") ; HANGUL SYLLABLE IEUNG-WI-MIEUM
       (?윕 . "0xC715") ; HANGUL SYLLABLE IEUNG-WI-PIEUP
       (?윗 . "0xC717") ; HANGUL SYLLABLE IEUNG-WI-SIOS
       (?윙 . "0xC719") ; HANGUL SYLLABLE IEUNG-WI-IEUNG
       (?유 . "0xC720") ; HANGUL SYLLABLE IEUNG-YU
       (?육 . "0xC721") ; HANGUL SYLLABLE IEUNG-YU-KIYEOK
       (?윤 . "0xC724") ; HANGUL SYLLABLE IEUNG-YU-NIEUN
       (?율 . "0xC728") ; HANGUL SYLLABLE IEUNG-YU-RIEUL
       (?윰 . "0xC730") ; HANGUL SYLLABLE IEUNG-YU-MIEUM
       (?윱 . "0xC731") ; HANGUL SYLLABLE IEUNG-YU-PIEUP
       (?윳 . "0xC733") ; HANGUL SYLLABLE IEUNG-YU-SIOS
       (?융 . "0xC735") ; HANGUL SYLLABLE IEUNG-YU-IEUNG
       (?윷 . "0xC737") ; HANGUL SYLLABLE IEUNG-YU-CHIEUCH
       (?으 . "0xC73C") ; HANGUL SYLLABLE IEUNG-EU
       (?윽 . "0xC73D") ; HANGUL SYLLABLE IEUNG-EU-KIYEOK
       (?은 . "0xC740") ; HANGUL SYLLABLE IEUNG-EU-NIEUN
       (?을 . "0xC744") ; HANGUL SYLLABLE IEUNG-EU-RIEUL
       (?읊 . "0xC74A") ; HANGUL SYLLABLE IEUNG-EU-RIEULPHIEUPH
       (?음 . "0xC74C") ; HANGUL SYLLABLE IEUNG-EU-MIEUM
       (?읍 . "0xC74D") ; HANGUL SYLLABLE IEUNG-EU-PIEUP
       (?읏 . "0xC74F") ; HANGUL SYLLABLE IEUNG-EU-SIOS
       (?응 . "0xC751") ; HANGUL SYLLABLE IEUNG-EU-IEUNG
       (?읒 . "0xC752") ; HANGUL SYLLABLE IEUNG-EU-CIEUC
       (?읓 . "0xC753") ; HANGUL SYLLABLE IEUNG-EU-CHIEUCH
       (?읔 . "0xC754") ; HANGUL SYLLABLE IEUNG-EU-KHIEUKH
       (?읕 . "0xC755") ; HANGUL SYLLABLE IEUNG-EU-THIEUTH
       (?읖 . "0xC756") ; HANGUL SYLLABLE IEUNG-EU-PHIEUPH
       (?읗 . "0xC757") ; HANGUL SYLLABLE IEUNG-EU-HIEUH
       (?의 . "0xC758") ; HANGUL SYLLABLE IEUNG-YI
       (?읜 . "0xC75C") ; HANGUL SYLLABLE IEUNG-YI-NIEUN
       (?읠 . "0xC760") ; HANGUL SYLLABLE IEUNG-YI-RIEUL
       (?읨 . "0xC768") ; HANGUL SYLLABLE IEUNG-YI-MIEUM
       (?읫 . "0xC76B") ; HANGUL SYLLABLE IEUNG-YI-SIOS
       (?이 . "0xC774") ; HANGUL SYLLABLE IEUNG-I
       (?익 . "0xC775") ; HANGUL SYLLABLE IEUNG-I-KIYEOK
       (?인 . "0xC778") ; HANGUL SYLLABLE IEUNG-I-NIEUN
       (?일 . "0xC77C") ; HANGUL SYLLABLE IEUNG-I-RIEUL
       (?읽 . "0xC77D") ; HANGUL SYLLABLE IEUNG-I-RIEULKIYEOK
       (?읾 . "0xC77E") ; HANGUL SYLLABLE IEUNG-I-RIEULMIEUM
       (?잃 . "0xC783") ; HANGUL SYLLABLE IEUNG-I-RIEULHIEUH
       (?임 . "0xC784") ; HANGUL SYLLABLE IEUNG-I-MIEUM
       (?입 . "0xC785") ; HANGUL SYLLABLE IEUNG-I-PIEUP
       (?잇 . "0xC787") ; HANGUL SYLLABLE IEUNG-I-SIOS
       (?있 . "0xC788") ; HANGUL SYLLABLE IEUNG-I-SSANGSIOS
       (?잉 . "0xC789") ; HANGUL SYLLABLE IEUNG-I-IEUNG
       (?잊 . "0xC78A") ; HANGUL SYLLABLE IEUNG-I-CIEUC
       (?잎 . "0xC78E") ; HANGUL SYLLABLE IEUNG-I-PHIEUPH
       (?자 . "0xC790") ; HANGUL SYLLABLE CIEUC-A
       (?작 . "0xC791") ; HANGUL SYLLABLE CIEUC-A-KIYEOK
       (?잔 . "0xC794") ; HANGUL SYLLABLE CIEUC-A-NIEUN
       (?잖 . "0xC796") ; HANGUL SYLLABLE CIEUC-A-NIEUNHIEUH
       (?잗 . "0xC797") ; HANGUL SYLLABLE CIEUC-A-TIKEUT
       (?잘 . "0xC798") ; HANGUL SYLLABLE CIEUC-A-RIEUL
       (?잚 . "0xC79A") ; HANGUL SYLLABLE CIEUC-A-RIEULMIEUM
       (?잠 . "0xC7A0") ; HANGUL SYLLABLE CIEUC-A-MIEUM
       (?잡 . "0xC7A1") ; HANGUL SYLLABLE CIEUC-A-PIEUP
       (?잣 . "0xC7A3") ; HANGUL SYLLABLE CIEUC-A-SIOS
       (?잤 . "0xC7A4") ; HANGUL SYLLABLE CIEUC-A-SSANGSIOS
       (?장 . "0xC7A5") ; HANGUL SYLLABLE CIEUC-A-IEUNG
       (?잦 . "0xC7A6") ; HANGUL SYLLABLE CIEUC-A-CIEUC
       (?재 . "0xC7AC") ; HANGUL SYLLABLE CIEUC-AE
       (?잭 . "0xC7AD") ; HANGUL SYLLABLE CIEUC-AE-KIYEOK
       (?잰 . "0xC7B0") ; HANGUL SYLLABLE CIEUC-AE-NIEUN
       (?잴 . "0xC7B4") ; HANGUL SYLLABLE CIEUC-AE-RIEUL
       (?잼 . "0xC7BC") ; HANGUL SYLLABLE CIEUC-AE-MIEUM
       (?잽 . "0xC7BD") ; HANGUL SYLLABLE CIEUC-AE-PIEUP
       (?잿 . "0xC7BF") ; HANGUL SYLLABLE CIEUC-AE-SIOS
       (?쟀 . "0xC7C0") ; HANGUL SYLLABLE CIEUC-AE-SSANGSIOS
       (?쟁 . "0xC7C1") ; HANGUL SYLLABLE CIEUC-AE-IEUNG
       (?쟈 . "0xC7C8") ; HANGUL SYLLABLE CIEUC-YA
       (?쟉 . "0xC7C9") ; HANGUL SYLLABLE CIEUC-YA-KIYEOK
       (?쟌 . "0xC7CC") ; HANGUL SYLLABLE CIEUC-YA-NIEUN
       (?쟎 . "0xC7CE") ; HANGUL SYLLABLE CIEUC-YA-NIEUNHIEUH
       (?쟐 . "0xC7D0") ; HANGUL SYLLABLE CIEUC-YA-RIEUL
       (?쟘 . "0xC7D8") ; HANGUL SYLLABLE CIEUC-YA-MIEUM
       (?쟝 . "0xC7DD") ; HANGUL SYLLABLE CIEUC-YA-IEUNG
       (?쟤 . "0xC7E4") ; HANGUL SYLLABLE CIEUC-YAE
       (?쟨 . "0xC7E8") ; HANGUL SYLLABLE CIEUC-YAE-NIEUN
       (?쟬 . "0xC7EC") ; HANGUL SYLLABLE CIEUC-YAE-RIEUL
       (?저 . "0xC800") ; HANGUL SYLLABLE CIEUC-EO
       (?적 . "0xC801") ; HANGUL SYLLABLE CIEUC-EO-KIYEOK
       (?전 . "0xC804") ; HANGUL SYLLABLE CIEUC-EO-NIEUN
       (?절 . "0xC808") ; HANGUL SYLLABLE CIEUC-EO-RIEUL
       (?젊 . "0xC80A") ; HANGUL SYLLABLE CIEUC-EO-RIEULMIEUM
       (?점 . "0xC810") ; HANGUL SYLLABLE CIEUC-EO-MIEUM
       (?접 . "0xC811") ; HANGUL SYLLABLE CIEUC-EO-PIEUP
       (?젓 . "0xC813") ; HANGUL SYLLABLE CIEUC-EO-SIOS
       (?정 . "0xC815") ; HANGUL SYLLABLE CIEUC-EO-IEUNG
       (?젖 . "0xC816") ; HANGUL SYLLABLE CIEUC-EO-CIEUC
       (?제 . "0xC81C") ; HANGUL SYLLABLE CIEUC-E
       (?젝 . "0xC81D") ; HANGUL SYLLABLE CIEUC-E-KIYEOK
       (?젠 . "0xC820") ; HANGUL SYLLABLE CIEUC-E-NIEUN
       (?젤 . "0xC824") ; HANGUL SYLLABLE CIEUC-E-RIEUL
       (?젬 . "0xC82C") ; HANGUL SYLLABLE CIEUC-E-MIEUM
       (?젭 . "0xC82D") ; HANGUL SYLLABLE CIEUC-E-PIEUP
       (?젯 . "0xC82F") ; HANGUL SYLLABLE CIEUC-E-SIOS
       (?젱 . "0xC831") ; HANGUL SYLLABLE CIEUC-E-IEUNG
       (?져 . "0xC838") ; HANGUL SYLLABLE CIEUC-YEO
       (?젼 . "0xC83C") ; HANGUL SYLLABLE CIEUC-YEO-NIEUN
       (?졀 . "0xC840") ; HANGUL SYLLABLE CIEUC-YEO-RIEUL
       (?졈 . "0xC848") ; HANGUL SYLLABLE CIEUC-YEO-MIEUM
       (?졉 . "0xC849") ; HANGUL SYLLABLE CIEUC-YEO-PIEUP
       (?졌 . "0xC84C") ; HANGUL SYLLABLE CIEUC-YEO-SSANGSIOS
       (?졍 . "0xC84D") ; HANGUL SYLLABLE CIEUC-YEO-IEUNG
       (?졔 . "0xC854") ; HANGUL SYLLABLE CIEUC-YE
       (?조 . "0xC870") ; HANGUL SYLLABLE CIEUC-O
       (?족 . "0xC871") ; HANGUL SYLLABLE CIEUC-O-KIYEOK
       (?존 . "0xC874") ; HANGUL SYLLABLE CIEUC-O-NIEUN
       (?졸 . "0xC878") ; HANGUL SYLLABLE CIEUC-O-RIEUL
       (?졺 . "0xC87A") ; HANGUL SYLLABLE CIEUC-O-RIEULMIEUM
       (?좀 . "0xC880") ; HANGUL SYLLABLE CIEUC-O-MIEUM
       (?좁 . "0xC881") ; HANGUL SYLLABLE CIEUC-O-PIEUP
       (?좃 . "0xC883") ; HANGUL SYLLABLE CIEUC-O-SIOS
       (?종 . "0xC885") ; HANGUL SYLLABLE CIEUC-O-IEUNG
       (?좆 . "0xC886") ; HANGUL SYLLABLE CIEUC-O-CIEUC
       (?좇 . "0xC887") ; HANGUL SYLLABLE CIEUC-O-CHIEUCH
       (?좋 . "0xC88B") ; HANGUL SYLLABLE CIEUC-O-HIEUH
       (?좌 . "0xC88C") ; HANGUL SYLLABLE CIEUC-WA
       (?좍 . "0xC88D") ; HANGUL SYLLABLE CIEUC-WA-KIYEOK
       (?좔 . "0xC894") ; HANGUL SYLLABLE CIEUC-WA-RIEUL
       (?좝 . "0xC89D") ; HANGUL SYLLABLE CIEUC-WA-PIEUP
       (?좟 . "0xC89F") ; HANGUL SYLLABLE CIEUC-WA-SIOS
       (?좡 . "0xC8A1") ; HANGUL SYLLABLE CIEUC-WA-IEUNG
       (?좨 . "0xC8A8") ; HANGUL SYLLABLE CIEUC-WAE
       (?좼 . "0xC8BC") ; HANGUL SYLLABLE CIEUC-WAE-SSANGSIOS
       (?좽 . "0xC8BD") ; HANGUL SYLLABLE CIEUC-WAE-IEUNG
       (?죄 . "0xC8C4") ; HANGUL SYLLABLE CIEUC-OE
       (?죈 . "0xC8C8") ; HANGUL SYLLABLE CIEUC-OE-NIEUN
       (?죌 . "0xC8CC") ; HANGUL SYLLABLE CIEUC-OE-RIEUL
       (?죔 . "0xC8D4") ; HANGUL SYLLABLE CIEUC-OE-MIEUM
       (?죕 . "0xC8D5") ; HANGUL SYLLABLE CIEUC-OE-PIEUP
       (?죗 . "0xC8D7") ; HANGUL SYLLABLE CIEUC-OE-SIOS
       (?죙 . "0xC8D9") ; HANGUL SYLLABLE CIEUC-OE-IEUNG
       (?죠 . "0xC8E0") ; HANGUL SYLLABLE CIEUC-YO
       (?죡 . "0xC8E1") ; HANGUL SYLLABLE CIEUC-YO-KIYEOK
       (?죤 . "0xC8E4") ; HANGUL SYLLABLE CIEUC-YO-NIEUN
       (?죵 . "0xC8F5") ; HANGUL SYLLABLE CIEUC-YO-IEUNG
       (?주 . "0xC8FC") ; HANGUL SYLLABLE CIEUC-U
       (?죽 . "0xC8FD") ; HANGUL SYLLABLE CIEUC-U-KIYEOK
       (?준 . "0xC900") ; HANGUL SYLLABLE CIEUC-U-NIEUN
       (?줄 . "0xC904") ; HANGUL SYLLABLE CIEUC-U-RIEUL
       (?줅 . "0xC905") ; HANGUL SYLLABLE CIEUC-U-RIEULKIYEOK
       (?줆 . "0xC906") ; HANGUL SYLLABLE CIEUC-U-RIEULMIEUM
       (?줌 . "0xC90C") ; HANGUL SYLLABLE CIEUC-U-MIEUM
       (?줍 . "0xC90D") ; HANGUL SYLLABLE CIEUC-U-PIEUP
       (?줏 . "0xC90F") ; HANGUL SYLLABLE CIEUC-U-SIOS
       (?중 . "0xC911") ; HANGUL SYLLABLE CIEUC-U-IEUNG
       (?줘 . "0xC918") ; HANGUL SYLLABLE CIEUC-WEO
       (?줬 . "0xC92C") ; HANGUL SYLLABLE CIEUC-WEO-SSANGSIOS
       (?줴 . "0xC934") ; HANGUL SYLLABLE CIEUC-WE
       (?쥐 . "0xC950") ; HANGUL SYLLABLE CIEUC-WI
       (?쥑 . "0xC951") ; HANGUL SYLLABLE CIEUC-WI-KIYEOK
       (?쥔 . "0xC954") ; HANGUL SYLLABLE CIEUC-WI-NIEUN
       (?쥘 . "0xC958") ; HANGUL SYLLABLE CIEUC-WI-RIEUL
       (?쥠 . "0xC960") ; HANGUL SYLLABLE CIEUC-WI-MIEUM
       (?쥡 . "0xC961") ; HANGUL SYLLABLE CIEUC-WI-PIEUP
       (?쥣 . "0xC963") ; HANGUL SYLLABLE CIEUC-WI-SIOS
       (?쥬 . "0xC96C") ; HANGUL SYLLABLE CIEUC-YU
       (?쥰 . "0xC970") ; HANGUL SYLLABLE CIEUC-YU-NIEUN
       (?쥴 . "0xC974") ; HANGUL SYLLABLE CIEUC-YU-RIEUL
       (?쥼 . "0xC97C") ; HANGUL SYLLABLE CIEUC-YU-MIEUM
       (?즈 . "0xC988") ; HANGUL SYLLABLE CIEUC-EU
       (?즉 . "0xC989") ; HANGUL SYLLABLE CIEUC-EU-KIYEOK
       (?즌 . "0xC98C") ; HANGUL SYLLABLE CIEUC-EU-NIEUN
       (?즐 . "0xC990") ; HANGUL SYLLABLE CIEUC-EU-RIEUL
       (?즘 . "0xC998") ; HANGUL SYLLABLE CIEUC-EU-MIEUM
       (?즙 . "0xC999") ; HANGUL SYLLABLE CIEUC-EU-PIEUP
       (?즛 . "0xC99B") ; HANGUL SYLLABLE CIEUC-EU-SIOS
       (?증 . "0xC99D") ; HANGUL SYLLABLE CIEUC-EU-IEUNG
       (?지 . "0xC9C0") ; HANGUL SYLLABLE CIEUC-I
       (?직 . "0xC9C1") ; HANGUL SYLLABLE CIEUC-I-KIYEOK
       (?진 . "0xC9C4") ; HANGUL SYLLABLE CIEUC-I-NIEUN
       (?짇 . "0xC9C7") ; HANGUL SYLLABLE CIEUC-I-TIKEUT
       (?질 . "0xC9C8") ; HANGUL SYLLABLE CIEUC-I-RIEUL
       (?짊 . "0xC9CA") ; HANGUL SYLLABLE CIEUC-I-RIEULMIEUM
       (?짐 . "0xC9D0") ; HANGUL SYLLABLE CIEUC-I-MIEUM
       (?집 . "0xC9D1") ; HANGUL SYLLABLE CIEUC-I-PIEUP
       (?짓 . "0xC9D3") ; HANGUL SYLLABLE CIEUC-I-SIOS
       (?징 . "0xC9D5") ; HANGUL SYLLABLE CIEUC-I-IEUNG
       (?짖 . "0xC9D6") ; HANGUL SYLLABLE CIEUC-I-CIEUC
       (?짙 . "0xC9D9") ; HANGUL SYLLABLE CIEUC-I-THIEUTH
       (?짚 . "0xC9DA") ; HANGUL SYLLABLE CIEUC-I-PHIEUPH
       (?짜 . "0xC9DC") ; HANGUL SYLLABLE SSANGCIEUC-A
       (?짝 . "0xC9DD") ; HANGUL SYLLABLE SSANGCIEUC-A-KIYEOK
       (?짠 . "0xC9E0") ; HANGUL SYLLABLE SSANGCIEUC-A-NIEUN
       (?짢 . "0xC9E2") ; HANGUL SYLLABLE SSANGCIEUC-A-NIEUNHIEUH
       (?짤 . "0xC9E4") ; HANGUL SYLLABLE SSANGCIEUC-A-RIEUL
       (?짧 . "0xC9E7") ; HANGUL SYLLABLE SSANGCIEUC-A-RIEULPIEUP
       (?짬 . "0xC9EC") ; HANGUL SYLLABLE SSANGCIEUC-A-MIEUM
       (?짭 . "0xC9ED") ; HANGUL SYLLABLE SSANGCIEUC-A-PIEUP
       (?짯 . "0xC9EF") ; HANGUL SYLLABLE SSANGCIEUC-A-SIOS
       (?짰 . "0xC9F0") ; HANGUL SYLLABLE SSANGCIEUC-A-SSANGSIOS
       (?짱 . "0xC9F1") ; HANGUL SYLLABLE SSANGCIEUC-A-IEUNG
       (?째 . "0xC9F8") ; HANGUL SYLLABLE SSANGCIEUC-AE
       (?짹 . "0xC9F9") ; HANGUL SYLLABLE SSANGCIEUC-AE-KIYEOK
       (?짼 . "0xC9FC") ; HANGUL SYLLABLE SSANGCIEUC-AE-NIEUN
       (?쨀 . "0xCA00") ; HANGUL SYLLABLE SSANGCIEUC-AE-RIEUL
       (?쨈 . "0xCA08") ; HANGUL SYLLABLE SSANGCIEUC-AE-MIEUM
       (?쨉 . "0xCA09") ; HANGUL SYLLABLE SSANGCIEUC-AE-PIEUP
       (?쨋 . "0xCA0B") ; HANGUL SYLLABLE SSANGCIEUC-AE-SIOS
       (?쨌 . "0xCA0C") ; HANGUL SYLLABLE SSANGCIEUC-AE-SSANGSIOS
       (?쨍 . "0xCA0D") ; HANGUL SYLLABLE SSANGCIEUC-AE-IEUNG
       (?쨔 . "0xCA14") ; HANGUL SYLLABLE SSANGCIEUC-YA
       (?쨘 . "0xCA18") ; HANGUL SYLLABLE SSANGCIEUC-YA-NIEUN
       (?쨩 . "0xCA29") ; HANGUL SYLLABLE SSANGCIEUC-YA-IEUNG
       (?쩌 . "0xCA4C") ; HANGUL SYLLABLE SSANGCIEUC-EO
       (?쩍 . "0xCA4D") ; HANGUL SYLLABLE SSANGCIEUC-EO-KIYEOK
       (?쩐 . "0xCA50") ; HANGUL SYLLABLE SSANGCIEUC-EO-NIEUN
       (?쩔 . "0xCA54") ; HANGUL SYLLABLE SSANGCIEUC-EO-RIEUL
       (?쩜 . "0xCA5C") ; HANGUL SYLLABLE SSANGCIEUC-EO-MIEUM
       (?쩝 . "0xCA5D") ; HANGUL SYLLABLE SSANGCIEUC-EO-PIEUP
       (?쩟 . "0xCA5F") ; HANGUL SYLLABLE SSANGCIEUC-EO-SIOS
       (?쩠 . "0xCA60") ; HANGUL SYLLABLE SSANGCIEUC-EO-SSANGSIOS
       (?쩡 . "0xCA61") ; HANGUL SYLLABLE SSANGCIEUC-EO-IEUNG
       (?쩨 . "0xCA68") ; HANGUL SYLLABLE SSANGCIEUC-E
       (?쩽 . "0xCA7D") ; HANGUL SYLLABLE SSANGCIEUC-E-IEUNG
       (?쪄 . "0xCA84") ; HANGUL SYLLABLE SSANGCIEUC-YEO
       (?쪘 . "0xCA98") ; HANGUL SYLLABLE SSANGCIEUC-YEO-SSANGSIOS
       (?쪼 . "0xCABC") ; HANGUL SYLLABLE SSANGCIEUC-O
       (?쪽 . "0xCABD") ; HANGUL SYLLABLE SSANGCIEUC-O-KIYEOK
       (?쫀 . "0xCAC0") ; HANGUL SYLLABLE SSANGCIEUC-O-NIEUN
       (?쫄 . "0xCAC4") ; HANGUL SYLLABLE SSANGCIEUC-O-RIEUL
       (?쫌 . "0xCACC") ; HANGUL SYLLABLE SSANGCIEUC-O-MIEUM
       (?쫍 . "0xCACD") ; HANGUL SYLLABLE SSANGCIEUC-O-PIEUP
       (?쫏 . "0xCACF") ; HANGUL SYLLABLE SSANGCIEUC-O-SIOS
       (?쫑 . "0xCAD1") ; HANGUL SYLLABLE SSANGCIEUC-O-IEUNG
       (?쫓 . "0xCAD3") ; HANGUL SYLLABLE SSANGCIEUC-O-CHIEUCH
       (?쫘 . "0xCAD8") ; HANGUL SYLLABLE SSANGCIEUC-WA
       (?쫙 . "0xCAD9") ; HANGUL SYLLABLE SSANGCIEUC-WA-KIYEOK
       (?쫠 . "0xCAE0") ; HANGUL SYLLABLE SSANGCIEUC-WA-RIEUL
       (?쫬 . "0xCAEC") ; HANGUL SYLLABLE SSANGCIEUC-WA-SSANGSIOS
       (?쫴 . "0xCAF4") ; HANGUL SYLLABLE SSANGCIEUC-WAE
       (?쬈 . "0xCB08") ; HANGUL SYLLABLE SSANGCIEUC-WAE-SSANGSIOS
       (?쬐 . "0xCB10") ; HANGUL SYLLABLE SSANGCIEUC-OE
       (?쬔 . "0xCB14") ; HANGUL SYLLABLE SSANGCIEUC-OE-NIEUN
       (?쬘 . "0xCB18") ; HANGUL SYLLABLE SSANGCIEUC-OE-RIEUL
       (?쬠 . "0xCB20") ; HANGUL SYLLABLE SSANGCIEUC-OE-MIEUM
       (?쬡 . "0xCB21") ; HANGUL SYLLABLE SSANGCIEUC-OE-PIEUP
       (?쭁 . "0xCB41") ; HANGUL SYLLABLE SSANGCIEUC-YO-IEUNG
       (?쭈 . "0xCB48") ; HANGUL SYLLABLE SSANGCIEUC-U
       (?쭉 . "0xCB49") ; HANGUL SYLLABLE SSANGCIEUC-U-KIYEOK
       (?쭌 . "0xCB4C") ; HANGUL SYLLABLE SSANGCIEUC-U-NIEUN
       (?쭐 . "0xCB50") ; HANGUL SYLLABLE SSANGCIEUC-U-RIEUL
       (?쭘 . "0xCB58") ; HANGUL SYLLABLE SSANGCIEUC-U-MIEUM
       (?쭙 . "0xCB59") ; HANGUL SYLLABLE SSANGCIEUC-U-PIEUP
       (?쭝 . "0xCB5D") ; HANGUL SYLLABLE SSANGCIEUC-U-IEUNG
       (?쭤 . "0xCB64") ; HANGUL SYLLABLE SSANGCIEUC-WEO
       (?쭸 . "0xCB78") ; HANGUL SYLLABLE SSANGCIEUC-WEO-SSANGSIOS
       (?쭹 . "0xCB79") ; HANGUL SYLLABLE SSANGCIEUC-WEO-IEUNG
       (?쮜 . "0xCB9C") ; HANGUL SYLLABLE SSANGCIEUC-WI
       (?쮸 . "0xCBB8") ; HANGUL SYLLABLE SSANGCIEUC-YU
       (?쯔 . "0xCBD4") ; HANGUL SYLLABLE SSANGCIEUC-EU
       (?쯤 . "0xCBE4") ; HANGUL SYLLABLE SSANGCIEUC-EU-MIEUM
       (?쯧 . "0xCBE7") ; HANGUL SYLLABLE SSANGCIEUC-EU-SIOS
       (?쯩 . "0xCBE9") ; HANGUL SYLLABLE SSANGCIEUC-EU-IEUNG
       (?찌 . "0xCC0C") ; HANGUL SYLLABLE SSANGCIEUC-I
       (?찍 . "0xCC0D") ; HANGUL SYLLABLE SSANGCIEUC-I-KIYEOK
       (?찐 . "0xCC10") ; HANGUL SYLLABLE SSANGCIEUC-I-NIEUN
       (?찔 . "0xCC14") ; HANGUL SYLLABLE SSANGCIEUC-I-RIEUL
       (?찜 . "0xCC1C") ; HANGUL SYLLABLE SSANGCIEUC-I-MIEUM
       (?찝 . "0xCC1D") ; HANGUL SYLLABLE SSANGCIEUC-I-PIEUP
       (?찡 . "0xCC21") ; HANGUL SYLLABLE SSANGCIEUC-I-IEUNG
       (?찢 . "0xCC22") ; HANGUL SYLLABLE SSANGCIEUC-I-CIEUC
       (?찧 . "0xCC27") ; HANGUL SYLLABLE SSANGCIEUC-I-HIEUH
       (?차 . "0xCC28") ; HANGUL SYLLABLE CHIEUCH-A
       (?착 . "0xCC29") ; HANGUL SYLLABLE CHIEUCH-A-KIYEOK
       (?찬 . "0xCC2C") ; HANGUL SYLLABLE CHIEUCH-A-NIEUN
       (?찮 . "0xCC2E") ; HANGUL SYLLABLE CHIEUCH-A-NIEUNHIEUH
       (?찰 . "0xCC30") ; HANGUL SYLLABLE CHIEUCH-A-RIEUL
       (?참 . "0xCC38") ; HANGUL SYLLABLE CHIEUCH-A-MIEUM
       (?찹 . "0xCC39") ; HANGUL SYLLABLE CHIEUCH-A-PIEUP
       (?찻 . "0xCC3B") ; HANGUL SYLLABLE CHIEUCH-A-SIOS
       (?찼 . "0xCC3C") ; HANGUL SYLLABLE CHIEUCH-A-SSANGSIOS
       (?창 . "0xCC3D") ; HANGUL SYLLABLE CHIEUCH-A-IEUNG
       (?찾 . "0xCC3E") ; HANGUL SYLLABLE CHIEUCH-A-CIEUC
       (?채 . "0xCC44") ; HANGUL SYLLABLE CHIEUCH-AE
       (?책 . "0xCC45") ; HANGUL SYLLABLE CHIEUCH-AE-KIYEOK
       (?챈 . "0xCC48") ; HANGUL SYLLABLE CHIEUCH-AE-NIEUN
       (?챌 . "0xCC4C") ; HANGUL SYLLABLE CHIEUCH-AE-RIEUL
       (?챔 . "0xCC54") ; HANGUL SYLLABLE CHIEUCH-AE-MIEUM
       (?챕 . "0xCC55") ; HANGUL SYLLABLE CHIEUCH-AE-PIEUP
       (?챗 . "0xCC57") ; HANGUL SYLLABLE CHIEUCH-AE-SIOS
       (?챘 . "0xCC58") ; HANGUL SYLLABLE CHIEUCH-AE-SSANGSIOS
       (?챙 . "0xCC59") ; HANGUL SYLLABLE CHIEUCH-AE-IEUNG
       (?챠 . "0xCC60") ; HANGUL SYLLABLE CHIEUCH-YA
       (?챤 . "0xCC64") ; HANGUL SYLLABLE CHIEUCH-YA-NIEUN
       (?챦 . "0xCC66") ; HANGUL SYLLABLE CHIEUCH-YA-NIEUNHIEUH
       (?챨 . "0xCC68") ; HANGUL SYLLABLE CHIEUCH-YA-RIEUL
       (?챰 . "0xCC70") ; HANGUL SYLLABLE CHIEUCH-YA-MIEUM
       (?챵 . "0xCC75") ; HANGUL SYLLABLE CHIEUCH-YA-IEUNG
       (?처 . "0xCC98") ; HANGUL SYLLABLE CHIEUCH-EO
       (?척 . "0xCC99") ; HANGUL SYLLABLE CHIEUCH-EO-KIYEOK
       (?천 . "0xCC9C") ; HANGUL SYLLABLE CHIEUCH-EO-NIEUN
       (?철 . "0xCCA0") ; HANGUL SYLLABLE CHIEUCH-EO-RIEUL
       (?첨 . "0xCCA8") ; HANGUL SYLLABLE CHIEUCH-EO-MIEUM
       (?첩 . "0xCCA9") ; HANGUL SYLLABLE CHIEUCH-EO-PIEUP
       (?첫 . "0xCCAB") ; HANGUL SYLLABLE CHIEUCH-EO-SIOS
       (?첬 . "0xCCAC") ; HANGUL SYLLABLE CHIEUCH-EO-SSANGSIOS
       (?청 . "0xCCAD") ; HANGUL SYLLABLE CHIEUCH-EO-IEUNG
       (?체 . "0xCCB4") ; HANGUL SYLLABLE CHIEUCH-E
       (?첵 . "0xCCB5") ; HANGUL SYLLABLE CHIEUCH-E-KIYEOK
       (?첸 . "0xCCB8") ; HANGUL SYLLABLE CHIEUCH-E-NIEUN
       (?첼 . "0xCCBC") ; HANGUL SYLLABLE CHIEUCH-E-RIEUL
       (?쳄 . "0xCCC4") ; HANGUL SYLLABLE CHIEUCH-E-MIEUM
       (?쳅 . "0xCCC5") ; HANGUL SYLLABLE CHIEUCH-E-PIEUP
       (?쳇 . "0xCCC7") ; HANGUL SYLLABLE CHIEUCH-E-SIOS
       (?쳉 . "0xCCC9") ; HANGUL SYLLABLE CHIEUCH-E-IEUNG
       (?쳐 . "0xCCD0") ; HANGUL SYLLABLE CHIEUCH-YEO
       (?쳔 . "0xCCD4") ; HANGUL SYLLABLE CHIEUCH-YEO-NIEUN
       (?쳤 . "0xCCE4") ; HANGUL SYLLABLE CHIEUCH-YEO-SSANGSIOS
       (?쳬 . "0xCCEC") ; HANGUL SYLLABLE CHIEUCH-YE
       (?쳰 . "0xCCF0") ; HANGUL SYLLABLE CHIEUCH-YE-NIEUN
       (?촁 . "0xCD01") ; HANGUL SYLLABLE CHIEUCH-YE-IEUNG
       (?초 . "0xCD08") ; HANGUL SYLLABLE CHIEUCH-O
       (?촉 . "0xCD09") ; HANGUL SYLLABLE CHIEUCH-O-KIYEOK
       (?촌 . "0xCD0C") ; HANGUL SYLLABLE CHIEUCH-O-NIEUN
       (?촐 . "0xCD10") ; HANGUL SYLLABLE CHIEUCH-O-RIEUL
       (?촘 . "0xCD18") ; HANGUL SYLLABLE CHIEUCH-O-MIEUM
       (?촙 . "0xCD19") ; HANGUL SYLLABLE CHIEUCH-O-PIEUP
       (?촛 . "0xCD1B") ; HANGUL SYLLABLE CHIEUCH-O-SIOS
       (?총 . "0xCD1D") ; HANGUL SYLLABLE CHIEUCH-O-IEUNG
       (?촤 . "0xCD24") ; HANGUL SYLLABLE CHIEUCH-WA
       (?촨 . "0xCD28") ; HANGUL SYLLABLE CHIEUCH-WA-NIEUN
       (?촬 . "0xCD2C") ; HANGUL SYLLABLE CHIEUCH-WA-RIEUL
       (?촹 . "0xCD39") ; HANGUL SYLLABLE CHIEUCH-WA-IEUNG
       (?최 . "0xCD5C") ; HANGUL SYLLABLE CHIEUCH-OE
       (?쵠 . "0xCD60") ; HANGUL SYLLABLE CHIEUCH-OE-NIEUN
       (?쵤 . "0xCD64") ; HANGUL SYLLABLE CHIEUCH-OE-RIEUL
       (?쵬 . "0xCD6C") ; HANGUL SYLLABLE CHIEUCH-OE-MIEUM
       (?쵭 . "0xCD6D") ; HANGUL SYLLABLE CHIEUCH-OE-PIEUP
       (?쵯 . "0xCD6F") ; HANGUL SYLLABLE CHIEUCH-OE-SIOS
       (?쵱 . "0xCD71") ; HANGUL SYLLABLE CHIEUCH-OE-IEUNG
       (?쵸 . "0xCD78") ; HANGUL SYLLABLE CHIEUCH-YO
       (?춈 . "0xCD88") ; HANGUL SYLLABLE CHIEUCH-YO-MIEUM
       (?추 . "0xCD94") ; HANGUL SYLLABLE CHIEUCH-U
       (?축 . "0xCD95") ; HANGUL SYLLABLE CHIEUCH-U-KIYEOK
       (?춘 . "0xCD98") ; HANGUL SYLLABLE CHIEUCH-U-NIEUN
       (?출 . "0xCD9C") ; HANGUL SYLLABLE CHIEUCH-U-RIEUL
       (?춤 . "0xCDA4") ; HANGUL SYLLABLE CHIEUCH-U-MIEUM
       (?춥 . "0xCDA5") ; HANGUL SYLLABLE CHIEUCH-U-PIEUP
       (?춧 . "0xCDA7") ; HANGUL SYLLABLE CHIEUCH-U-SIOS
       (?충 . "0xCDA9") ; HANGUL SYLLABLE CHIEUCH-U-IEUNG
       (?춰 . "0xCDB0") ; HANGUL SYLLABLE CHIEUCH-WEO
       (?췄 . "0xCDC4") ; HANGUL SYLLABLE CHIEUCH-WEO-SSANGSIOS
       (?췌 . "0xCDCC") ; HANGUL SYLLABLE CHIEUCH-WE
       (?췐 . "0xCDD0") ; HANGUL SYLLABLE CHIEUCH-WE-NIEUN
       (?취 . "0xCDE8") ; HANGUL SYLLABLE CHIEUCH-WI
       (?췬 . "0xCDEC") ; HANGUL SYLLABLE CHIEUCH-WI-NIEUN
       (?췰 . "0xCDF0") ; HANGUL SYLLABLE CHIEUCH-WI-RIEUL
       (?췸 . "0xCDF8") ; HANGUL SYLLABLE CHIEUCH-WI-MIEUM
       (?췹 . "0xCDF9") ; HANGUL SYLLABLE CHIEUCH-WI-PIEUP
       (?췻 . "0xCDFB") ; HANGUL SYLLABLE CHIEUCH-WI-SIOS
       (?췽 . "0xCDFD") ; HANGUL SYLLABLE CHIEUCH-WI-IEUNG
       (?츄 . "0xCE04") ; HANGUL SYLLABLE CHIEUCH-YU
       (?츈 . "0xCE08") ; HANGUL SYLLABLE CHIEUCH-YU-NIEUN
       (?츌 . "0xCE0C") ; HANGUL SYLLABLE CHIEUCH-YU-RIEUL
       (?츔 . "0xCE14") ; HANGUL SYLLABLE CHIEUCH-YU-MIEUM
       (?츙 . "0xCE19") ; HANGUL SYLLABLE CHIEUCH-YU-IEUNG
       (?츠 . "0xCE20") ; HANGUL SYLLABLE CHIEUCH-EU
       (?측 . "0xCE21") ; HANGUL SYLLABLE CHIEUCH-EU-KIYEOK
       (?츤 . "0xCE24") ; HANGUL SYLLABLE CHIEUCH-EU-NIEUN
       (?츨 . "0xCE28") ; HANGUL SYLLABLE CHIEUCH-EU-RIEUL
       (?츰 . "0xCE30") ; HANGUL SYLLABLE CHIEUCH-EU-MIEUM
       (?츱 . "0xCE31") ; HANGUL SYLLABLE CHIEUCH-EU-PIEUP
       (?츳 . "0xCE33") ; HANGUL SYLLABLE CHIEUCH-EU-SIOS
       (?층 . "0xCE35") ; HANGUL SYLLABLE CHIEUCH-EU-IEUNG
       (?치 . "0xCE58") ; HANGUL SYLLABLE CHIEUCH-I
       (?칙 . "0xCE59") ; HANGUL SYLLABLE CHIEUCH-I-KIYEOK
       (?친 . "0xCE5C") ; HANGUL SYLLABLE CHIEUCH-I-NIEUN
       (?칟 . "0xCE5F") ; HANGUL SYLLABLE CHIEUCH-I-TIKEUT
       (?칠 . "0xCE60") ; HANGUL SYLLABLE CHIEUCH-I-RIEUL
       (?칡 . "0xCE61") ; HANGUL SYLLABLE CHIEUCH-I-RIEULKIYEOK
       (?침 . "0xCE68") ; HANGUL SYLLABLE CHIEUCH-I-MIEUM
       (?칩 . "0xCE69") ; HANGUL SYLLABLE CHIEUCH-I-PIEUP
       (?칫 . "0xCE6B") ; HANGUL SYLLABLE CHIEUCH-I-SIOS
       (?칭 . "0xCE6D") ; HANGUL SYLLABLE CHIEUCH-I-IEUNG
       (?카 . "0xCE74") ; HANGUL SYLLABLE KHIEUKH-A
       (?칵 . "0xCE75") ; HANGUL SYLLABLE KHIEUKH-A-KIYEOK
       (?칸 . "0xCE78") ; HANGUL SYLLABLE KHIEUKH-A-NIEUN
       (?칼 . "0xCE7C") ; HANGUL SYLLABLE KHIEUKH-A-RIEUL
       (?캄 . "0xCE84") ; HANGUL SYLLABLE KHIEUKH-A-MIEUM
       (?캅 . "0xCE85") ; HANGUL SYLLABLE KHIEUKH-A-PIEUP
       (?캇 . "0xCE87") ; HANGUL SYLLABLE KHIEUKH-A-SIOS
       (?캉 . "0xCE89") ; HANGUL SYLLABLE KHIEUKH-A-IEUNG
       (?캐 . "0xCE90") ; HANGUL SYLLABLE KHIEUKH-AE
       (?캑 . "0xCE91") ; HANGUL SYLLABLE KHIEUKH-AE-KIYEOK
       (?캔 . "0xCE94") ; HANGUL SYLLABLE KHIEUKH-AE-NIEUN
       (?캘 . "0xCE98") ; HANGUL SYLLABLE KHIEUKH-AE-RIEUL
       (?캠 . "0xCEA0") ; HANGUL SYLLABLE KHIEUKH-AE-MIEUM
       (?캡 . "0xCEA1") ; HANGUL SYLLABLE KHIEUKH-AE-PIEUP
       (?캣 . "0xCEA3") ; HANGUL SYLLABLE KHIEUKH-AE-SIOS
       (?캤 . "0xCEA4") ; HANGUL SYLLABLE KHIEUKH-AE-SSANGSIOS
       (?캥 . "0xCEA5") ; HANGUL SYLLABLE KHIEUKH-AE-IEUNG
       (?캬 . "0xCEAC") ; HANGUL SYLLABLE KHIEUKH-YA
       (?캭 . "0xCEAD") ; HANGUL SYLLABLE KHIEUKH-YA-KIYEOK
       (?컁 . "0xCEC1") ; HANGUL SYLLABLE KHIEUKH-YA-IEUNG
       (?커 . "0xCEE4") ; HANGUL SYLLABLE KHIEUKH-EO
       (?컥 . "0xCEE5") ; HANGUL SYLLABLE KHIEUKH-EO-KIYEOK
       (?컨 . "0xCEE8") ; HANGUL SYLLABLE KHIEUKH-EO-NIEUN
       (?컫 . "0xCEEB") ; HANGUL SYLLABLE KHIEUKH-EO-TIKEUT
       (?컬 . "0xCEEC") ; HANGUL SYLLABLE KHIEUKH-EO-RIEUL
       (?컴 . "0xCEF4") ; HANGUL SYLLABLE KHIEUKH-EO-MIEUM
       (?컵 . "0xCEF5") ; HANGUL SYLLABLE KHIEUKH-EO-PIEUP
       (?컷 . "0xCEF7") ; HANGUL SYLLABLE KHIEUKH-EO-SIOS
       (?컸 . "0xCEF8") ; HANGUL SYLLABLE KHIEUKH-EO-SSANGSIOS
       (?컹 . "0xCEF9") ; HANGUL SYLLABLE KHIEUKH-EO-IEUNG
       (?케 . "0xCF00") ; HANGUL SYLLABLE KHIEUKH-E
       (?켁 . "0xCF01") ; HANGUL SYLLABLE KHIEUKH-E-KIYEOK
       (?켄 . "0xCF04") ; HANGUL SYLLABLE KHIEUKH-E-NIEUN
       (?켈 . "0xCF08") ; HANGUL SYLLABLE KHIEUKH-E-RIEUL
       (?켐 . "0xCF10") ; HANGUL SYLLABLE KHIEUKH-E-MIEUM
       (?켑 . "0xCF11") ; HANGUL SYLLABLE KHIEUKH-E-PIEUP
       (?켓 . "0xCF13") ; HANGUL SYLLABLE KHIEUKH-E-SIOS
       (?켕 . "0xCF15") ; HANGUL SYLLABLE KHIEUKH-E-IEUNG
       (?켜 . "0xCF1C") ; HANGUL SYLLABLE KHIEUKH-YEO
       (?켠 . "0xCF20") ; HANGUL SYLLABLE KHIEUKH-YEO-NIEUN
       (?켤 . "0xCF24") ; HANGUL SYLLABLE KHIEUKH-YEO-RIEUL
       (?켬 . "0xCF2C") ; HANGUL SYLLABLE KHIEUKH-YEO-MIEUM
       (?켭 . "0xCF2D") ; HANGUL SYLLABLE KHIEUKH-YEO-PIEUP
       (?켯 . "0xCF2F") ; HANGUL SYLLABLE KHIEUKH-YEO-SIOS
       (?켰 . "0xCF30") ; HANGUL SYLLABLE KHIEUKH-YEO-SSANGSIOS
       (?켱 . "0xCF31") ; HANGUL SYLLABLE KHIEUKH-YEO-IEUNG
       (?켸 . "0xCF38") ; HANGUL SYLLABLE KHIEUKH-YE
       (?코 . "0xCF54") ; HANGUL SYLLABLE KHIEUKH-O
       (?콕 . "0xCF55") ; HANGUL SYLLABLE KHIEUKH-O-KIYEOK
       (?콘 . "0xCF58") ; HANGUL SYLLABLE KHIEUKH-O-NIEUN
       (?콜 . "0xCF5C") ; HANGUL SYLLABLE KHIEUKH-O-RIEUL
       (?콤 . "0xCF64") ; HANGUL SYLLABLE KHIEUKH-O-MIEUM
       (?콥 . "0xCF65") ; HANGUL SYLLABLE KHIEUKH-O-PIEUP
       (?콧 . "0xCF67") ; HANGUL SYLLABLE KHIEUKH-O-SIOS
       (?콩 . "0xCF69") ; HANGUL SYLLABLE KHIEUKH-O-IEUNG
       (?콰 . "0xCF70") ; HANGUL SYLLABLE KHIEUKH-WA
       (?콱 . "0xCF71") ; HANGUL SYLLABLE KHIEUKH-WA-KIYEOK
       (?콴 . "0xCF74") ; HANGUL SYLLABLE KHIEUKH-WA-NIEUN
       (?콸 . "0xCF78") ; HANGUL SYLLABLE KHIEUKH-WA-RIEUL
       (?쾀 . "0xCF80") ; HANGUL SYLLABLE KHIEUKH-WA-MIEUM
       (?쾅 . "0xCF85") ; HANGUL SYLLABLE KHIEUKH-WA-IEUNG
       (?쾌 . "0xCF8C") ; HANGUL SYLLABLE KHIEUKH-WAE
       (?쾡 . "0xCFA1") ; HANGUL SYLLABLE KHIEUKH-WAE-IEUNG
       (?쾨 . "0xCFA8") ; HANGUL SYLLABLE KHIEUKH-OE
       (?쾰 . "0xCFB0") ; HANGUL SYLLABLE KHIEUKH-OE-RIEUL
       (?쿄 . "0xCFC4") ; HANGUL SYLLABLE KHIEUKH-YO
       (?쿠 . "0xCFE0") ; HANGUL SYLLABLE KHIEUKH-U
       (?쿡 . "0xCFE1") ; HANGUL SYLLABLE KHIEUKH-U-KIYEOK
       (?쿤 . "0xCFE4") ; HANGUL SYLLABLE KHIEUKH-U-NIEUN
       (?쿨 . "0xCFE8") ; HANGUL SYLLABLE KHIEUKH-U-RIEUL
       (?쿰 . "0xCFF0") ; HANGUL SYLLABLE KHIEUKH-U-MIEUM
       (?쿱 . "0xCFF1") ; HANGUL SYLLABLE KHIEUKH-U-PIEUP
       (?쿳 . "0xCFF3") ; HANGUL SYLLABLE KHIEUKH-U-SIOS
       (?쿵 . "0xCFF5") ; HANGUL SYLLABLE KHIEUKH-U-IEUNG
       (?쿼 . "0xCFFC") ; HANGUL SYLLABLE KHIEUKH-WEO
       (?퀀 . "0xD000") ; HANGUL SYLLABLE KHIEUKH-WEO-NIEUN
       (?퀄 . "0xD004") ; HANGUL SYLLABLE KHIEUKH-WEO-RIEUL
       (?퀑 . "0xD011") ; HANGUL SYLLABLE KHIEUKH-WEO-IEUNG
       (?퀘 . "0xD018") ; HANGUL SYLLABLE KHIEUKH-WE
       (?퀭 . "0xD02D") ; HANGUL SYLLABLE KHIEUKH-WE-IEUNG
       (?퀴 . "0xD034") ; HANGUL SYLLABLE KHIEUKH-WI
       (?퀵 . "0xD035") ; HANGUL SYLLABLE KHIEUKH-WI-KIYEOK
       (?퀸 . "0xD038") ; HANGUL SYLLABLE KHIEUKH-WI-NIEUN
       (?퀼 . "0xD03C") ; HANGUL SYLLABLE KHIEUKH-WI-RIEUL
       (?큄 . "0xD044") ; HANGUL SYLLABLE KHIEUKH-WI-MIEUM
       (?큅 . "0xD045") ; HANGUL SYLLABLE KHIEUKH-WI-PIEUP
       (?큇 . "0xD047") ; HANGUL SYLLABLE KHIEUKH-WI-SIOS
       (?큉 . "0xD049") ; HANGUL SYLLABLE KHIEUKH-WI-IEUNG
       (?큐 . "0xD050") ; HANGUL SYLLABLE KHIEUKH-YU
       (?큔 . "0xD054") ; HANGUL SYLLABLE KHIEUKH-YU-NIEUN
       (?큘 . "0xD058") ; HANGUL SYLLABLE KHIEUKH-YU-RIEUL
       (?큠 . "0xD060") ; HANGUL SYLLABLE KHIEUKH-YU-MIEUM
       (?크 . "0xD06C") ; HANGUL SYLLABLE KHIEUKH-EU
       (?큭 . "0xD06D") ; HANGUL SYLLABLE KHIEUKH-EU-KIYEOK
       (?큰 . "0xD070") ; HANGUL SYLLABLE KHIEUKH-EU-NIEUN
       (?클 . "0xD074") ; HANGUL SYLLABLE KHIEUKH-EU-RIEUL
       (?큼 . "0xD07C") ; HANGUL SYLLABLE KHIEUKH-EU-MIEUM
       (?큽 . "0xD07D") ; HANGUL SYLLABLE KHIEUKH-EU-PIEUP
       (?킁 . "0xD081") ; HANGUL SYLLABLE KHIEUKH-EU-IEUNG
       (?키 . "0xD0A4") ; HANGUL SYLLABLE KHIEUKH-I
       (?킥 . "0xD0A5") ; HANGUL SYLLABLE KHIEUKH-I-KIYEOK
       (?킨 . "0xD0A8") ; HANGUL SYLLABLE KHIEUKH-I-NIEUN
       (?킬 . "0xD0AC") ; HANGUL SYLLABLE KHIEUKH-I-RIEUL
       (?킴 . "0xD0B4") ; HANGUL SYLLABLE KHIEUKH-I-MIEUM
       (?킵 . "0xD0B5") ; HANGUL SYLLABLE KHIEUKH-I-PIEUP
       (?킷 . "0xD0B7") ; HANGUL SYLLABLE KHIEUKH-I-SIOS
       (?킹 . "0xD0B9") ; HANGUL SYLLABLE KHIEUKH-I-IEUNG
       (?타 . "0xD0C0") ; HANGUL SYLLABLE THIEUTH-A
       (?탁 . "0xD0C1") ; HANGUL SYLLABLE THIEUTH-A-KIYEOK
       (?탄 . "0xD0C4") ; HANGUL SYLLABLE THIEUTH-A-NIEUN
       (?탈 . "0xD0C8") ; HANGUL SYLLABLE THIEUTH-A-RIEUL
       (?탉 . "0xD0C9") ; HANGUL SYLLABLE THIEUTH-A-RIEULKIYEOK
       (?탐 . "0xD0D0") ; HANGUL SYLLABLE THIEUTH-A-MIEUM
       (?탑 . "0xD0D1") ; HANGUL SYLLABLE THIEUTH-A-PIEUP
       (?탓 . "0xD0D3") ; HANGUL SYLLABLE THIEUTH-A-SIOS
       (?탔 . "0xD0D4") ; HANGUL SYLLABLE THIEUTH-A-SSANGSIOS
       (?탕 . "0xD0D5") ; HANGUL SYLLABLE THIEUTH-A-IEUNG
       (?태 . "0xD0DC") ; HANGUL SYLLABLE THIEUTH-AE
       (?택 . "0xD0DD") ; HANGUL SYLLABLE THIEUTH-AE-KIYEOK
       (?탠 . "0xD0E0") ; HANGUL SYLLABLE THIEUTH-AE-NIEUN
       (?탤 . "0xD0E4") ; HANGUL SYLLABLE THIEUTH-AE-RIEUL
       (?탬 . "0xD0EC") ; HANGUL SYLLABLE THIEUTH-AE-MIEUM
       (?탭 . "0xD0ED") ; HANGUL SYLLABLE THIEUTH-AE-PIEUP
       (?탯 . "0xD0EF") ; HANGUL SYLLABLE THIEUTH-AE-SIOS
       (?탰 . "0xD0F0") ; HANGUL SYLLABLE THIEUTH-AE-SSANGSIOS
       (?탱 . "0xD0F1") ; HANGUL SYLLABLE THIEUTH-AE-IEUNG
       (?탸 . "0xD0F8") ; HANGUL SYLLABLE THIEUTH-YA
       (?턍 . "0xD10D") ; HANGUL SYLLABLE THIEUTH-YA-IEUNG
       (?터 . "0xD130") ; HANGUL SYLLABLE THIEUTH-EO
       (?턱 . "0xD131") ; HANGUL SYLLABLE THIEUTH-EO-KIYEOK
       (?턴 . "0xD134") ; HANGUL SYLLABLE THIEUTH-EO-NIEUN
       (?털 . "0xD138") ; HANGUL SYLLABLE THIEUTH-EO-RIEUL
       (?턺 . "0xD13A") ; HANGUL SYLLABLE THIEUTH-EO-RIEULMIEUM
       (?텀 . "0xD140") ; HANGUL SYLLABLE THIEUTH-EO-MIEUM
       (?텁 . "0xD141") ; HANGUL SYLLABLE THIEUTH-EO-PIEUP
       (?텃 . "0xD143") ; HANGUL SYLLABLE THIEUTH-EO-SIOS
       (?텄 . "0xD144") ; HANGUL SYLLABLE THIEUTH-EO-SSANGSIOS
       (?텅 . "0xD145") ; HANGUL SYLLABLE THIEUTH-EO-IEUNG
       (?테 . "0xD14C") ; HANGUL SYLLABLE THIEUTH-E
       (?텍 . "0xD14D") ; HANGUL SYLLABLE THIEUTH-E-KIYEOK
       (?텐 . "0xD150") ; HANGUL SYLLABLE THIEUTH-E-NIEUN
       (?텔 . "0xD154") ; HANGUL SYLLABLE THIEUTH-E-RIEUL
       (?템 . "0xD15C") ; HANGUL SYLLABLE THIEUTH-E-MIEUM
       (?텝 . "0xD15D") ; HANGUL SYLLABLE THIEUTH-E-PIEUP
       (?텟 . "0xD15F") ; HANGUL SYLLABLE THIEUTH-E-SIOS
       (?텡 . "0xD161") ; HANGUL SYLLABLE THIEUTH-E-IEUNG
       (?텨 . "0xD168") ; HANGUL SYLLABLE THIEUTH-YEO
       (?텬 . "0xD16C") ; HANGUL SYLLABLE THIEUTH-YEO-NIEUN
       (?텼 . "0xD17C") ; HANGUL SYLLABLE THIEUTH-YEO-SSANGSIOS
       (?톄 . "0xD184") ; HANGUL SYLLABLE THIEUTH-YE
       (?톈 . "0xD188") ; HANGUL SYLLABLE THIEUTH-YE-NIEUN
       (?토 . "0xD1A0") ; HANGUL SYLLABLE THIEUTH-O
       (?톡 . "0xD1A1") ; HANGUL SYLLABLE THIEUTH-O-KIYEOK
       (?톤 . "0xD1A4") ; HANGUL SYLLABLE THIEUTH-O-NIEUN
       (?톨 . "0xD1A8") ; HANGUL SYLLABLE THIEUTH-O-RIEUL
       (?톰 . "0xD1B0") ; HANGUL SYLLABLE THIEUTH-O-MIEUM
       (?톱 . "0xD1B1") ; HANGUL SYLLABLE THIEUTH-O-PIEUP
       (?톳 . "0xD1B3") ; HANGUL SYLLABLE THIEUTH-O-SIOS
       (?통 . "0xD1B5") ; HANGUL SYLLABLE THIEUTH-O-IEUNG
       (?톺 . "0xD1BA") ; HANGUL SYLLABLE THIEUTH-O-PHIEUPH
       (?톼 . "0xD1BC") ; HANGUL SYLLABLE THIEUTH-WA
       (?퇀 . "0xD1C0") ; HANGUL SYLLABLE THIEUTH-WA-NIEUN
       (?퇘 . "0xD1D8") ; HANGUL SYLLABLE THIEUTH-WAE
       (?퇴 . "0xD1F4") ; HANGUL SYLLABLE THIEUTH-OE
       (?퇸 . "0xD1F8") ; HANGUL SYLLABLE THIEUTH-OE-NIEUN
       (?툇 . "0xD207") ; HANGUL SYLLABLE THIEUTH-OE-SIOS
       (?툉 . "0xD209") ; HANGUL SYLLABLE THIEUTH-OE-IEUNG
       (?툐 . "0xD210") ; HANGUL SYLLABLE THIEUTH-YO
       (?투 . "0xD22C") ; HANGUL SYLLABLE THIEUTH-U
       (?툭 . "0xD22D") ; HANGUL SYLLABLE THIEUTH-U-KIYEOK
       (?툰 . "0xD230") ; HANGUL SYLLABLE THIEUTH-U-NIEUN
       (?툴 . "0xD234") ; HANGUL SYLLABLE THIEUTH-U-RIEUL
       (?툼 . "0xD23C") ; HANGUL SYLLABLE THIEUTH-U-MIEUM
       (?툽 . "0xD23D") ; HANGUL SYLLABLE THIEUTH-U-PIEUP
       (?툿 . "0xD23F") ; HANGUL SYLLABLE THIEUTH-U-SIOS
       (?퉁 . "0xD241") ; HANGUL SYLLABLE THIEUTH-U-IEUNG
       (?퉈 . "0xD248") ; HANGUL SYLLABLE THIEUTH-WEO
       (?퉜 . "0xD25C") ; HANGUL SYLLABLE THIEUTH-WEO-SSANGSIOS
       (?퉤 . "0xD264") ; HANGUL SYLLABLE THIEUTH-WE
       (?튀 . "0xD280") ; HANGUL SYLLABLE THIEUTH-WI
       (?튁 . "0xD281") ; HANGUL SYLLABLE THIEUTH-WI-KIYEOK
       (?튄 . "0xD284") ; HANGUL SYLLABLE THIEUTH-WI-NIEUN
       (?튈 . "0xD288") ; HANGUL SYLLABLE THIEUTH-WI-RIEUL
       (?튐 . "0xD290") ; HANGUL SYLLABLE THIEUTH-WI-MIEUM
       (?튑 . "0xD291") ; HANGUL SYLLABLE THIEUTH-WI-PIEUP
       (?튕 . "0xD295") ; HANGUL SYLLABLE THIEUTH-WI-IEUNG
       (?튜 . "0xD29C") ; HANGUL SYLLABLE THIEUTH-YU
       (?튠 . "0xD2A0") ; HANGUL SYLLABLE THIEUTH-YU-NIEUN
       (?튤 . "0xD2A4") ; HANGUL SYLLABLE THIEUTH-YU-RIEUL
       (?튬 . "0xD2AC") ; HANGUL SYLLABLE THIEUTH-YU-MIEUM
       (?튱 . "0xD2B1") ; HANGUL SYLLABLE THIEUTH-YU-IEUNG
       (?트 . "0xD2B8") ; HANGUL SYLLABLE THIEUTH-EU
       (?특 . "0xD2B9") ; HANGUL SYLLABLE THIEUTH-EU-KIYEOK
       (?튼 . "0xD2BC") ; HANGUL SYLLABLE THIEUTH-EU-NIEUN
       (?튿 . "0xD2BF") ; HANGUL SYLLABLE THIEUTH-EU-TIKEUT
       (?틀 . "0xD2C0") ; HANGUL SYLLABLE THIEUTH-EU-RIEUL
       (?틂 . "0xD2C2") ; HANGUL SYLLABLE THIEUTH-EU-RIEULMIEUM
       (?틈 . "0xD2C8") ; HANGUL SYLLABLE THIEUTH-EU-MIEUM
       (?틉 . "0xD2C9") ; HANGUL SYLLABLE THIEUTH-EU-PIEUP
       (?틋 . "0xD2CB") ; HANGUL SYLLABLE THIEUTH-EU-SIOS
       (?틔 . "0xD2D4") ; HANGUL SYLLABLE THIEUTH-YI
       (?틘 . "0xD2D8") ; HANGUL SYLLABLE THIEUTH-YI-NIEUN
       (?틜 . "0xD2DC") ; HANGUL SYLLABLE THIEUTH-YI-RIEUL
       (?틤 . "0xD2E4") ; HANGUL SYLLABLE THIEUTH-YI-MIEUM
       (?틥 . "0xD2E5") ; HANGUL SYLLABLE THIEUTH-YI-PIEUP
       (?티 . "0xD2F0") ; HANGUL SYLLABLE THIEUTH-I
       (?틱 . "0xD2F1") ; HANGUL SYLLABLE THIEUTH-I-KIYEOK
       (?틴 . "0xD2F4") ; HANGUL SYLLABLE THIEUTH-I-NIEUN
       (?틸 . "0xD2F8") ; HANGUL SYLLABLE THIEUTH-I-RIEUL
       (?팀 . "0xD300") ; HANGUL SYLLABLE THIEUTH-I-MIEUM
       (?팁 . "0xD301") ; HANGUL SYLLABLE THIEUTH-I-PIEUP
       (?팃 . "0xD303") ; HANGUL SYLLABLE THIEUTH-I-SIOS
       (?팅 . "0xD305") ; HANGUL SYLLABLE THIEUTH-I-IEUNG
       (?파 . "0xD30C") ; HANGUL SYLLABLE PHIEUPH-A
       (?팍 . "0xD30D") ; HANGUL SYLLABLE PHIEUPH-A-KIYEOK
       (?팎 . "0xD30E") ; HANGUL SYLLABLE PHIEUPH-A-SSANGKIYEOK
       (?판 . "0xD310") ; HANGUL SYLLABLE PHIEUPH-A-NIEUN
       (?팔 . "0xD314") ; HANGUL SYLLABLE PHIEUPH-A-RIEUL
       (?팖 . "0xD316") ; HANGUL SYLLABLE PHIEUPH-A-RIEULMIEUM
       (?팜 . "0xD31C") ; HANGUL SYLLABLE PHIEUPH-A-MIEUM
       (?팝 . "0xD31D") ; HANGUL SYLLABLE PHIEUPH-A-PIEUP
       (?팟 . "0xD31F") ; HANGUL SYLLABLE PHIEUPH-A-SIOS
       (?팠 . "0xD320") ; HANGUL SYLLABLE PHIEUPH-A-SSANGSIOS
       (?팡 . "0xD321") ; HANGUL SYLLABLE PHIEUPH-A-IEUNG
       (?팥 . "0xD325") ; HANGUL SYLLABLE PHIEUPH-A-THIEUTH
       (?패 . "0xD328") ; HANGUL SYLLABLE PHIEUPH-AE
       (?팩 . "0xD329") ; HANGUL SYLLABLE PHIEUPH-AE-KIYEOK
       (?팬 . "0xD32C") ; HANGUL SYLLABLE PHIEUPH-AE-NIEUN
       (?팰 . "0xD330") ; HANGUL SYLLABLE PHIEUPH-AE-RIEUL
       (?팸 . "0xD338") ; HANGUL SYLLABLE PHIEUPH-AE-MIEUM
       (?팹 . "0xD339") ; HANGUL SYLLABLE PHIEUPH-AE-PIEUP
       (?팻 . "0xD33B") ; HANGUL SYLLABLE PHIEUPH-AE-SIOS
       (?팼 . "0xD33C") ; HANGUL SYLLABLE PHIEUPH-AE-SSANGSIOS
       (?팽 . "0xD33D") ; HANGUL SYLLABLE PHIEUPH-AE-IEUNG
       (?퍄 . "0xD344") ; HANGUL SYLLABLE PHIEUPH-YA
       (?퍅 . "0xD345") ; HANGUL SYLLABLE PHIEUPH-YA-KIYEOK
       (?퍼 . "0xD37C") ; HANGUL SYLLABLE PHIEUPH-EO
       (?퍽 . "0xD37D") ; HANGUL SYLLABLE PHIEUPH-EO-KIYEOK
       (?펀 . "0xD380") ; HANGUL SYLLABLE PHIEUPH-EO-NIEUN
       (?펄 . "0xD384") ; HANGUL SYLLABLE PHIEUPH-EO-RIEUL
       (?펌 . "0xD38C") ; HANGUL SYLLABLE PHIEUPH-EO-MIEUM
       (?펍 . "0xD38D") ; HANGUL SYLLABLE PHIEUPH-EO-PIEUP
       (?펏 . "0xD38F") ; HANGUL SYLLABLE PHIEUPH-EO-SIOS
       (?펐 . "0xD390") ; HANGUL SYLLABLE PHIEUPH-EO-SSANGSIOS
       (?펑 . "0xD391") ; HANGUL SYLLABLE PHIEUPH-EO-IEUNG
       (?페 . "0xD398") ; HANGUL SYLLABLE PHIEUPH-E
       (?펙 . "0xD399") ; HANGUL SYLLABLE PHIEUPH-E-KIYEOK
       (?펜 . "0xD39C") ; HANGUL SYLLABLE PHIEUPH-E-NIEUN
       (?펠 . "0xD3A0") ; HANGUL SYLLABLE PHIEUPH-E-RIEUL
       (?펨 . "0xD3A8") ; HANGUL SYLLABLE PHIEUPH-E-MIEUM
       (?펩 . "0xD3A9") ; HANGUL SYLLABLE PHIEUPH-E-PIEUP
       (?펫 . "0xD3AB") ; HANGUL SYLLABLE PHIEUPH-E-SIOS
       (?펭 . "0xD3AD") ; HANGUL SYLLABLE PHIEUPH-E-IEUNG
       (?펴 . "0xD3B4") ; HANGUL SYLLABLE PHIEUPH-YEO
       (?편 . "0xD3B8") ; HANGUL SYLLABLE PHIEUPH-YEO-NIEUN
       (?펼 . "0xD3BC") ; HANGUL SYLLABLE PHIEUPH-YEO-RIEUL
       (?폄 . "0xD3C4") ; HANGUL SYLLABLE PHIEUPH-YEO-MIEUM
       (?폅 . "0xD3C5") ; HANGUL SYLLABLE PHIEUPH-YEO-PIEUP
       (?폈 . "0xD3C8") ; HANGUL SYLLABLE PHIEUPH-YEO-SSANGSIOS
       (?평 . "0xD3C9") ; HANGUL SYLLABLE PHIEUPH-YEO-IEUNG
       (?폐 . "0xD3D0") ; HANGUL SYLLABLE PHIEUPH-YE
       (?폘 . "0xD3D8") ; HANGUL SYLLABLE PHIEUPH-YE-RIEUL
       (?폡 . "0xD3E1") ; HANGUL SYLLABLE PHIEUPH-YE-PIEUP
       (?폣 . "0xD3E3") ; HANGUL SYLLABLE PHIEUPH-YE-SIOS
       (?포 . "0xD3EC") ; HANGUL SYLLABLE PHIEUPH-O
       (?폭 . "0xD3ED") ; HANGUL SYLLABLE PHIEUPH-O-KIYEOK
       (?폰 . "0xD3F0") ; HANGUL SYLLABLE PHIEUPH-O-NIEUN
       (?폴 . "0xD3F4") ; HANGUL SYLLABLE PHIEUPH-O-RIEUL
       (?폼 . "0xD3FC") ; HANGUL SYLLABLE PHIEUPH-O-MIEUM
       (?폽 . "0xD3FD") ; HANGUL SYLLABLE PHIEUPH-O-PIEUP
       (?폿 . "0xD3FF") ; HANGUL SYLLABLE PHIEUPH-O-SIOS
       (?퐁 . "0xD401") ; HANGUL SYLLABLE PHIEUPH-O-IEUNG
       (?퐈 . "0xD408") ; HANGUL SYLLABLE PHIEUPH-WA
       (?퐝 . "0xD41D") ; HANGUL SYLLABLE PHIEUPH-WA-IEUNG
       (?푀 . "0xD440") ; HANGUL SYLLABLE PHIEUPH-OE
       (?푄 . "0xD444") ; HANGUL SYLLABLE PHIEUPH-OE-NIEUN
       (?표 . "0xD45C") ; HANGUL SYLLABLE PHIEUPH-YO
       (?푠 . "0xD460") ; HANGUL SYLLABLE PHIEUPH-YO-NIEUN
       (?푤 . "0xD464") ; HANGUL SYLLABLE PHIEUPH-YO-RIEUL
       (?푭 . "0xD46D") ; HANGUL SYLLABLE PHIEUPH-YO-PIEUP
       (?푯 . "0xD46F") ; HANGUL SYLLABLE PHIEUPH-YO-SIOS
       (?푸 . "0xD478") ; HANGUL SYLLABLE PHIEUPH-U
       (?푹 . "0xD479") ; HANGUL SYLLABLE PHIEUPH-U-KIYEOK
       (?푼 . "0xD47C") ; HANGUL SYLLABLE PHIEUPH-U-NIEUN
       (?푿 . "0xD47F") ; HANGUL SYLLABLE PHIEUPH-U-TIKEUT
       (?풀 . "0xD480") ; HANGUL SYLLABLE PHIEUPH-U-RIEUL
       (?풂 . "0xD482") ; HANGUL SYLLABLE PHIEUPH-U-RIEULMIEUM
       (?품 . "0xD488") ; HANGUL SYLLABLE PHIEUPH-U-MIEUM
       (?풉 . "0xD489") ; HANGUL SYLLABLE PHIEUPH-U-PIEUP
       (?풋 . "0xD48B") ; HANGUL SYLLABLE PHIEUPH-U-SIOS
       (?풍 . "0xD48D") ; HANGUL SYLLABLE PHIEUPH-U-IEUNG
       (?풔 . "0xD494") ; HANGUL SYLLABLE PHIEUPH-WEO
       (?풩 . "0xD4A9") ; HANGUL SYLLABLE PHIEUPH-WEO-IEUNG
       (?퓌 . "0xD4CC") ; HANGUL SYLLABLE PHIEUPH-WI
       (?퓐 . "0xD4D0") ; HANGUL SYLLABLE PHIEUPH-WI-NIEUN
       (?퓔 . "0xD4D4") ; HANGUL SYLLABLE PHIEUPH-WI-RIEUL
       (?퓜 . "0xD4DC") ; HANGUL SYLLABLE PHIEUPH-WI-MIEUM
       (?퓟 . "0xD4DF") ; HANGUL SYLLABLE PHIEUPH-WI-SIOS
       (?퓨 . "0xD4E8") ; HANGUL SYLLABLE PHIEUPH-YU
       (?퓬 . "0xD4EC") ; HANGUL SYLLABLE PHIEUPH-YU-NIEUN
       (?퓰 . "0xD4F0") ; HANGUL SYLLABLE PHIEUPH-YU-RIEUL
       (?퓸 . "0xD4F8") ; HANGUL SYLLABLE PHIEUPH-YU-MIEUM
       (?퓻 . "0xD4FB") ; HANGUL SYLLABLE PHIEUPH-YU-SIOS
       (?퓽 . "0xD4FD") ; HANGUL SYLLABLE PHIEUPH-YU-IEUNG
       (?프 . "0xD504") ; HANGUL SYLLABLE PHIEUPH-EU
       (?픈 . "0xD508") ; HANGUL SYLLABLE PHIEUPH-EU-NIEUN
       (?플 . "0xD50C") ; HANGUL SYLLABLE PHIEUPH-EU-RIEUL
       (?픔 . "0xD514") ; HANGUL SYLLABLE PHIEUPH-EU-MIEUM
       (?픕 . "0xD515") ; HANGUL SYLLABLE PHIEUPH-EU-PIEUP
       (?픗 . "0xD517") ; HANGUL SYLLABLE PHIEUPH-EU-SIOS
       (?피 . "0xD53C") ; HANGUL SYLLABLE PHIEUPH-I
       (?픽 . "0xD53D") ; HANGUL SYLLABLE PHIEUPH-I-KIYEOK
       (?핀 . "0xD540") ; HANGUL SYLLABLE PHIEUPH-I-NIEUN
       (?필 . "0xD544") ; HANGUL SYLLABLE PHIEUPH-I-RIEUL
       (?핌 . "0xD54C") ; HANGUL SYLLABLE PHIEUPH-I-MIEUM
       (?핍 . "0xD54D") ; HANGUL SYLLABLE PHIEUPH-I-PIEUP
       (?핏 . "0xD54F") ; HANGUL SYLLABLE PHIEUPH-I-SIOS
       (?핑 . "0xD551") ; HANGUL SYLLABLE PHIEUPH-I-IEUNG
       (?하 . "0xD558") ; HANGUL SYLLABLE HIEUH-A
       (?학 . "0xD559") ; HANGUL SYLLABLE HIEUH-A-KIYEOK
       (?한 . "0xD55C") ; HANGUL SYLLABLE HIEUH-A-NIEUN
       (?할 . "0xD560") ; HANGUL SYLLABLE HIEUH-A-RIEUL
       (?핥 . "0xD565") ; HANGUL SYLLABLE HIEUH-A-RIEULTHIEUTH
       (?함 . "0xD568") ; HANGUL SYLLABLE HIEUH-A-MIEUM
       (?합 . "0xD569") ; HANGUL SYLLABLE HIEUH-A-PIEUP
       (?핫 . "0xD56B") ; HANGUL SYLLABLE HIEUH-A-SIOS
       (?항 . "0xD56D") ; HANGUL SYLLABLE HIEUH-A-IEUNG
       (?해 . "0xD574") ; HANGUL SYLLABLE HIEUH-AE
       (?핵 . "0xD575") ; HANGUL SYLLABLE HIEUH-AE-KIYEOK
       (?핸 . "0xD578") ; HANGUL SYLLABLE HIEUH-AE-NIEUN
       (?핼 . "0xD57C") ; HANGUL SYLLABLE HIEUH-AE-RIEUL
       (?햄 . "0xD584") ; HANGUL SYLLABLE HIEUH-AE-MIEUM
       (?햅 . "0xD585") ; HANGUL SYLLABLE HIEUH-AE-PIEUP
       (?햇 . "0xD587") ; HANGUL SYLLABLE HIEUH-AE-SIOS
       (?했 . "0xD588") ; HANGUL SYLLABLE HIEUH-AE-SSANGSIOS
       (?행 . "0xD589") ; HANGUL SYLLABLE HIEUH-AE-IEUNG
       (?햐 . "0xD590") ; HANGUL SYLLABLE HIEUH-YA
       (?향 . "0xD5A5") ; HANGUL SYLLABLE HIEUH-YA-IEUNG
       (?허 . "0xD5C8") ; HANGUL SYLLABLE HIEUH-EO
       (?헉 . "0xD5C9") ; HANGUL SYLLABLE HIEUH-EO-KIYEOK
       (?헌 . "0xD5CC") ; HANGUL SYLLABLE HIEUH-EO-NIEUN
       (?헐 . "0xD5D0") ; HANGUL SYLLABLE HIEUH-EO-RIEUL
       (?헒 . "0xD5D2") ; HANGUL SYLLABLE HIEUH-EO-RIEULMIEUM
       (?험 . "0xD5D8") ; HANGUL SYLLABLE HIEUH-EO-MIEUM
       (?헙 . "0xD5D9") ; HANGUL SYLLABLE HIEUH-EO-PIEUP
       (?헛 . "0xD5DB") ; HANGUL SYLLABLE HIEUH-EO-SIOS
       (?헝 . "0xD5DD") ; HANGUL SYLLABLE HIEUH-EO-IEUNG
       (?헤 . "0xD5E4") ; HANGUL SYLLABLE HIEUH-E
       (?헥 . "0xD5E5") ; HANGUL SYLLABLE HIEUH-E-KIYEOK
       (?헨 . "0xD5E8") ; HANGUL SYLLABLE HIEUH-E-NIEUN
       (?헬 . "0xD5EC") ; HANGUL SYLLABLE HIEUH-E-RIEUL
       (?헴 . "0xD5F4") ; HANGUL SYLLABLE HIEUH-E-MIEUM
       (?헵 . "0xD5F5") ; HANGUL SYLLABLE HIEUH-E-PIEUP
       (?헷 . "0xD5F7") ; HANGUL SYLLABLE HIEUH-E-SIOS
       (?헹 . "0xD5F9") ; HANGUL SYLLABLE HIEUH-E-IEUNG
       (?혀 . "0xD600") ; HANGUL SYLLABLE HIEUH-YEO
       (?혁 . "0xD601") ; HANGUL SYLLABLE HIEUH-YEO-KIYEOK
       (?현 . "0xD604") ; HANGUL SYLLABLE HIEUH-YEO-NIEUN
       (?혈 . "0xD608") ; HANGUL SYLLABLE HIEUH-YEO-RIEUL
       (?혐 . "0xD610") ; HANGUL SYLLABLE HIEUH-YEO-MIEUM
       (?협 . "0xD611") ; HANGUL SYLLABLE HIEUH-YEO-PIEUP
       (?혓 . "0xD613") ; HANGUL SYLLABLE HIEUH-YEO-SIOS
       (?혔 . "0xD614") ; HANGUL SYLLABLE HIEUH-YEO-SSANGSIOS
       (?형 . "0xD615") ; HANGUL SYLLABLE HIEUH-YEO-IEUNG
       (?혜 . "0xD61C") ; HANGUL SYLLABLE HIEUH-YE
       (?혠 . "0xD620") ; HANGUL SYLLABLE HIEUH-YE-NIEUN
       (?혤 . "0xD624") ; HANGUL SYLLABLE HIEUH-YE-RIEUL
       (?혭 . "0xD62D") ; HANGUL SYLLABLE HIEUH-YE-PIEUP
       (?호 . "0xD638") ; HANGUL SYLLABLE HIEUH-O
       (?혹 . "0xD639") ; HANGUL SYLLABLE HIEUH-O-KIYEOK
       (?혼 . "0xD63C") ; HANGUL SYLLABLE HIEUH-O-NIEUN
       (?홀 . "0xD640") ; HANGUL SYLLABLE HIEUH-O-RIEUL
       (?홅 . "0xD645") ; HANGUL SYLLABLE HIEUH-O-RIEULTHIEUTH
       (?홈 . "0xD648") ; HANGUL SYLLABLE HIEUH-O-MIEUM
       (?홉 . "0xD649") ; HANGUL SYLLABLE HIEUH-O-PIEUP
       (?홋 . "0xD64B") ; HANGUL SYLLABLE HIEUH-O-SIOS
       (?홍 . "0xD64D") ; HANGUL SYLLABLE HIEUH-O-IEUNG
       (?홑 . "0xD651") ; HANGUL SYLLABLE HIEUH-O-THIEUTH
       (?화 . "0xD654") ; HANGUL SYLLABLE HIEUH-WA
       (?확 . "0xD655") ; HANGUL SYLLABLE HIEUH-WA-KIYEOK
       (?환 . "0xD658") ; HANGUL SYLLABLE HIEUH-WA-NIEUN
       (?활 . "0xD65C") ; HANGUL SYLLABLE HIEUH-WA-RIEUL
       (?홧 . "0xD667") ; HANGUL SYLLABLE HIEUH-WA-SIOS
       (?황 . "0xD669") ; HANGUL SYLLABLE HIEUH-WA-IEUNG
       (?홰 . "0xD670") ; HANGUL SYLLABLE HIEUH-WAE
       (?홱 . "0xD671") ; HANGUL SYLLABLE HIEUH-WAE-KIYEOK
       (?홴 . "0xD674") ; HANGUL SYLLABLE HIEUH-WAE-NIEUN
       (?횃 . "0xD683") ; HANGUL SYLLABLE HIEUH-WAE-SIOS
       (?횅 . "0xD685") ; HANGUL SYLLABLE HIEUH-WAE-IEUNG
       (?회 . "0xD68C") ; HANGUL SYLLABLE HIEUH-OE
       (?획 . "0xD68D") ; HANGUL SYLLABLE HIEUH-OE-KIYEOK
       (?횐 . "0xD690") ; HANGUL SYLLABLE HIEUH-OE-NIEUN
       (?횔 . "0xD694") ; HANGUL SYLLABLE HIEUH-OE-RIEUL
       (?횝 . "0xD69D") ; HANGUL SYLLABLE HIEUH-OE-PIEUP
       (?횟 . "0xD69F") ; HANGUL SYLLABLE HIEUH-OE-SIOS
       (?횡 . "0xD6A1") ; HANGUL SYLLABLE HIEUH-OE-IEUNG
       (?효 . "0xD6A8") ; HANGUL SYLLABLE HIEUH-YO
       (?횬 . "0xD6AC") ; HANGUL SYLLABLE HIEUH-YO-NIEUN
       (?횰 . "0xD6B0") ; HANGUL SYLLABLE HIEUH-YO-RIEUL
       (?횹 . "0xD6B9") ; HANGUL SYLLABLE HIEUH-YO-PIEUP
       (?횻 . "0xD6BB") ; HANGUL SYLLABLE HIEUH-YO-SIOS
       (?후 . "0xD6C4") ; HANGUL SYLLABLE HIEUH-U
       (?훅 . "0xD6C5") ; HANGUL SYLLABLE HIEUH-U-KIYEOK
       (?훈 . "0xD6C8") ; HANGUL SYLLABLE HIEUH-U-NIEUN
       (?훌 . "0xD6CC") ; HANGUL SYLLABLE HIEUH-U-RIEUL
       (?훑 . "0xD6D1") ; HANGUL SYLLABLE HIEUH-U-RIEULTHIEUTH
       (?훔 . "0xD6D4") ; HANGUL SYLLABLE HIEUH-U-MIEUM
       (?훗 . "0xD6D7") ; HANGUL SYLLABLE HIEUH-U-SIOS
       (?훙 . "0xD6D9") ; HANGUL SYLLABLE HIEUH-U-IEUNG
       (?훠 . "0xD6E0") ; HANGUL SYLLABLE HIEUH-WEO
       (?훤 . "0xD6E4") ; HANGUL SYLLABLE HIEUH-WEO-NIEUN
       (?훨 . "0xD6E8") ; HANGUL SYLLABLE HIEUH-WEO-RIEUL
       (?훰 . "0xD6F0") ; HANGUL SYLLABLE HIEUH-WEO-MIEUM
       (?훵 . "0xD6F5") ; HANGUL SYLLABLE HIEUH-WEO-IEUNG
       (?훼 . "0xD6FC") ; HANGUL SYLLABLE HIEUH-WE
       (?훽 . "0xD6FD") ; HANGUL SYLLABLE HIEUH-WE-KIYEOK
       (?휀 . "0xD700") ; HANGUL SYLLABLE HIEUH-WE-NIEUN
       (?휄 . "0xD704") ; HANGUL SYLLABLE HIEUH-WE-RIEUL
       (?휑 . "0xD711") ; HANGUL SYLLABLE HIEUH-WE-IEUNG
       (?휘 . "0xD718") ; HANGUL SYLLABLE HIEUH-WI
       (?휙 . "0xD719") ; HANGUL SYLLABLE HIEUH-WI-KIYEOK
       (?휜 . "0xD71C") ; HANGUL SYLLABLE HIEUH-WI-NIEUN
       (?휠 . "0xD720") ; HANGUL SYLLABLE HIEUH-WI-RIEUL
       (?휨 . "0xD728") ; HANGUL SYLLABLE HIEUH-WI-MIEUM
       (?휩 . "0xD729") ; HANGUL SYLLABLE HIEUH-WI-PIEUP
       (?휫 . "0xD72B") ; HANGUL SYLLABLE HIEUH-WI-SIOS
       (?휭 . "0xD72D") ; HANGUL SYLLABLE HIEUH-WI-IEUNG
       (?휴 . "0xD734") ; HANGUL SYLLABLE HIEUH-YU
       (?휵 . "0xD735") ; HANGUL SYLLABLE HIEUH-YU-KIYEOK
       (?휸 . "0xD738") ; HANGUL SYLLABLE HIEUH-YU-NIEUN
       (?휼 . "0xD73C") ; HANGUL SYLLABLE HIEUH-YU-RIEUL
       (?흄 . "0xD744") ; HANGUL SYLLABLE HIEUH-YU-MIEUM
       (?흇 . "0xD747") ; HANGUL SYLLABLE HIEUH-YU-SIOS
       (?흉 . "0xD749") ; HANGUL SYLLABLE HIEUH-YU-IEUNG
       (?흐 . "0xD750") ; HANGUL SYLLABLE HIEUH-EU
       (?흑 . "0xD751") ; HANGUL SYLLABLE HIEUH-EU-KIYEOK
       (?흔 . "0xD754") ; HANGUL SYLLABLE HIEUH-EU-NIEUN
       (?흖 . "0xD756") ; HANGUL SYLLABLE HIEUH-EU-NIEUNHIEUH
       (?흗 . "0xD757") ; HANGUL SYLLABLE HIEUH-EU-TIKEUT
       (?흘 . "0xD758") ; HANGUL SYLLABLE HIEUH-EU-RIEUL
       (?흙 . "0xD759") ; HANGUL SYLLABLE HIEUH-EU-RIEULKIYEOK
       (?흠 . "0xD760") ; HANGUL SYLLABLE HIEUH-EU-MIEUM
       (?흡 . "0xD761") ; HANGUL SYLLABLE HIEUH-EU-PIEUP
       (?흣 . "0xD763") ; HANGUL SYLLABLE HIEUH-EU-SIOS
       (?흥 . "0xD765") ; HANGUL SYLLABLE HIEUH-EU-IEUNG
       (?흩 . "0xD769") ; HANGUL SYLLABLE HIEUH-EU-THIEUTH
       (?희 . "0xD76C") ; HANGUL SYLLABLE HIEUH-YI
       (?흰 . "0xD770") ; HANGUL SYLLABLE HIEUH-YI-NIEUN
       (?흴 . "0xD774") ; HANGUL SYLLABLE HIEUH-YI-RIEUL
       (?흼 . "0xD77C") ; HANGUL SYLLABLE HIEUH-YI-MIEUM
       (?흽 . "0xD77D") ; HANGUL SYLLABLE HIEUH-YI-PIEUP
       (?힁 . "0xD781") ; HANGUL SYLLABLE HIEUH-YI-IEUNG
       (?히 . "0xD788") ; HANGUL SYLLABLE HIEUH-I
       (?힉 . "0xD789") ; HANGUL SYLLABLE HIEUH-I-KIYEOK
       (?힌 . "0xD78C") ; HANGUL SYLLABLE HIEUH-I-NIEUN
       (?힐 . "0xD790") ; HANGUL SYLLABLE HIEUH-I-RIEUL
       (?힘 . "0xD798") ; HANGUL SYLLABLE HIEUH-I-MIEUM
       (?힙 . "0xD799") ; HANGUL SYLLABLE HIEUH-I-PIEUP
       (?힛 . "0xD79B") ; HANGUL SYLLABLE HIEUH-I-SIOS
       (?힝 . "0xD79D") ; HANGUL SYLLABLE HIEUH-I-IEUNG
       (?伽 . "0x4F3D") ; <CJK>
       (?佳 . "0x4F73") ; <CJK>
       (?假 . "0x5047") ; <CJK>
       (?價 . "0x50F9") ; <CJK>
       (?加 . "0x52A0") ; <CJK>
       (?可 . "0x53EF") ; <CJK>
       (?呵 . "0x5475") ; <CJK>
       (?哥 . "0x54E5") ; <CJK>
       (?嘉 . "0x5609") ; <CJK>
       (?嫁 . "0x5AC1") ; <CJK>
       (?家 . "0x5BB6") ; <CJK>
       (?暇 . "0x6687") ; <CJK>
       (?架 . "0x67B6") ; <CJK>
       (?枷 . "0x67B7") ; <CJK>
       (?柯 . "0x67EF") ; <CJK>
       (?歌 . "0x6B4C") ; <CJK>
       (?珂 . "0x73C2") ; <CJK>
       (?痂 . "0x75C2") ; <CJK>
       (?稼 . "0x7A3C") ; <CJK>
       (?苛 . "0x82DB") ; <CJK>
       (?茄 . "0x8304") ; <CJK>
       (?街 . "0x8857") ; <CJK>
       (?袈 . "0x8888") ; <CJK>
       (?訶 . "0x8A36") ; <CJK>
       (?賈 . "0x8CC8") ; <CJK>
       (?跏 . "0x8DCF") ; <CJK>
       (?軻 . "0x8EFB") ; <CJK>
       (?迦 . "0x8FE6") ; <CJK>
       (?駕 . "0x99D5") ; <CJK>
       (?刻 . "0x523B") ; <CJK>
       (?却 . "0x5374") ; <CJK>
       (?各 . "0x5404") ; <CJK>
       (?恪 . "0x606A") ; <CJK>
       (?慤 . "0x6164") ; <CJK>
       (?殼 . "0x6BBC") ; <CJK>
       (?珏 . "0x73CF") ; <CJK>
       (?脚 . "0x811A") ; <CJK>
       (?覺 . "0x89BA") ; <CJK>
       (?角 . "0x89D2") ; <CJK>
       (?閣 . "0x95A3") ; <CJK>
       (?侃 . "0x4F83") ; <CJK>
       (?刊 . "0x520A") ; <CJK>
       (?墾 . "0x58BE") ; <CJK>
       (?奸 . "0x5978") ; <CJK>
       (?姦 . "0x59E6") ; <CJK>
       (?干 . "0x5E72") ; <CJK>
       (?幹 . "0x5E79") ; <CJK>
       (?懇 . "0x61C7") ; <CJK>
       (?揀 . "0x63C0") ; <CJK>
       (?杆 . "0x6746") ; <CJK>
       (?柬 . "0x67EC") ; <CJK>
       (?桿 . "0x687F") ; <CJK>
       (?澗 . "0x6F97") ; <CJK>
       (?癎 . "0x764E") ; <CJK>
       (?看 . "0x770B") ; <CJK>
       (?磵 . "0x78F5") ; <CJK>
       (?稈 . "0x7A08") ; <CJK>
       (?竿 . "0x7AFF") ; <CJK>
       (?簡 . "0x7C21") ; <CJK>
       (?肝 . "0x809D") ; <CJK>
       (?艮 . "0x826E") ; <CJK>
       (?艱 . "0x8271") ; <CJK>
       (?諫 . "0x8AEB") ; <CJK>
       (?間 . "0x9593") ; <CJK>
       (?乫 . "0x4E6B") ; <CJK>
       (?喝 . "0x559D") ; <CJK>
       (?曷 . "0x66F7") ; <CJK>
       (?渴 . "0x6E34") ; <CJK>
       (?碣 . "0x78A3") ; <CJK>
       (?竭 . "0x7AED") ; <CJK>
       (?葛 . "0x845B") ; <CJK>
       (?褐 . "0x8910") ; <CJK>
       (?蝎 . "0x874E") ; <CJK>
       (?鞨 . "0x97A8") ; <CJK>
       (?勘 . "0x52D8") ; <CJK>
       (?坎 . "0x574E") ; <CJK>
       (?堪 . "0x582A") ; <CJK>
       (?嵌 . "0x5D4C") ; <CJK>
       (?感 . "0x611F") ; <CJK>
       (?憾 . "0x61BE") ; <CJK>
       (?戡 . "0x6221") ; <CJK>
       (?敢 . "0x6562") ; <CJK>
       (?柑 . "0x67D1") ; <CJK>
       (?橄 . "0x6A44") ; <CJK>
       (?減 . "0x6E1B") ; <CJK>
       (?甘 . "0x7518") ; <CJK>
       (?疳 . "0x75B3") ; <CJK>
       (?監 . "0x76E3") ; <CJK>
       (?瞰 . "0x77B0") ; <CJK>
       (?紺 . "0x7D3A") ; <CJK>
       (?邯 . "0x90AF") ; <CJK>
       (?鑑 . "0x9451") ; <CJK>
       (?鑒 . "0x9452") ; <CJK>
       (?龕 . "0x9F95") ; <CJK>
       (?匣 . "0x5323") ; <CJK>
       (?岬 . "0x5CAC") ; <CJK>
       (?甲 . "0x7532") ; <CJK>
       (?胛 . "0x80DB") ; <CJK>
       (?鉀 . "0x9240") ; <CJK>
       (?閘 . "0x9598") ; <CJK>
       (?剛 . "0x525B") ; <CJK>
       (?堈 . "0x5808") ; <CJK>
       (?姜 . "0x59DC") ; <CJK>
       (?岡 . "0x5CA1") ; <CJK>
       (?崗 . "0x5D17") ; <CJK>
       (?康 . "0x5EB7") ; <CJK>
       (?强 . "0x5F3A") ; <CJK>
       (?彊 . "0x5F4A") ; <CJK>
       (?慷 . "0x6177") ; <CJK>
       (?江 . "0x6C5F") ; <CJK>
       (?畺 . "0x757A") ; <CJK>
       (?疆 . "0x7586") ; <CJK>
       (?糠 . "0x7CE0") ; <CJK>
       (?絳 . "0x7D73") ; <CJK>
       (?綱 . "0x7DB1") ; <CJK>
       (?羌 . "0x7F8C") ; <CJK>
       (?腔 . "0x8154") ; <CJK>
       (?舡 . "0x8221") ; <CJK>
       (?薑 . "0x8591") ; <CJK>
       (?襁 . "0x8941") ; <CJK>
       (?講 . "0x8B1B") ; <CJK>
       (?鋼 . "0x92FC") ; <CJK>
       (?降 . "0x964D") ; <CJK>
       (?鱇 . "0x9C47") ; <CJK>
       (?介 . "0x4ECB") ; <CJK>
       (?价 . "0x4EF7") ; <CJK>
       (?個 . "0x500B") ; <CJK>
       (?凱 . "0x51F1") ; <CJK>
       (?塏 . "0x584F") ; <CJK>
       (?愷 . "0x6137") ; <CJK>
       (?愾 . "0x613E") ; <CJK>
       (?慨 . "0x6168") ; <CJK>
       (?改 . "0x6539") ; <CJK>
       (?槪 . "0x69EA") ; <CJK>
       (?漑 . "0x6F11") ; <CJK>
       (?疥 . "0x75A5") ; <CJK>
       (?皆 . "0x7686") ; <CJK>
       (?盖 . "0x76D6") ; <CJK>
       (?箇 . "0x7B87") ; <CJK>
       (?芥 . "0x82A5") ; <CJK>
       (?蓋 . "0x84CB") ; <CJK>
       (?豈 . "0xF900") ; <CJK>
       (?鎧 . "0x93A7") ; <CJK>
       (?開 . "0x958B") ; <CJK>
       (?喀 . "0x5580") ; <CJK>
       (?客 . "0x5BA2") ; <CJK>
       (?坑 . "0x5751") ; <CJK>
       (?更 . "0xF901") ; <CJK>
       (?粳 . "0x7CB3") ; <CJK>
       (?羹 . "0x7FB9") ; <CJK>
       (?醵 . "0x91B5") ; <CJK>
       (?倨 . "0x5028") ; <CJK>
       (?去 . "0x53BB") ; <CJK>
       (?居 . "0x5C45") ; <CJK>
       (?巨 . "0x5DE8") ; <CJK>
       (?拒 . "0x62D2") ; <CJK>
       (?据 . "0x636E") ; <CJK>
       (?據 . "0x64DA") ; <CJK>
       (?擧 . "0x64E7") ; <CJK>
       (?渠 . "0x6E20") ; <CJK>
       (?炬 . "0x70AC") ; <CJK>
       (?祛 . "0x795B") ; <CJK>
       (?距 . "0x8DDD") ; <CJK>
       (?踞 . "0x8E1E") ; <CJK>
       (?車 . "0xF902") ; <CJK>
       (?遽 . "0x907D") ; <CJK>
       (?鉅 . "0x9245") ; <CJK>
       (?鋸 . "0x92F8") ; <CJK>
       (?乾 . "0x4E7E") ; <CJK>
       (?件 . "0x4EF6") ; <CJK>
       (?健 . "0x5065") ; <CJK>
       (?巾 . "0x5DFE") ; <CJK>
       (?建 . "0x5EFA") ; <CJK>
       (?愆 . "0x6106") ; <CJK>
       (?楗 . "0x6957") ; <CJK>
       (?腱 . "0x8171") ; <CJK>
       (?虔 . "0x8654") ; <CJK>
       (?蹇 . "0x8E47") ; <CJK>
       (?鍵 . "0x9375") ; <CJK>
       (?騫 . "0x9A2B") ; <CJK>
       (?乞 . "0x4E5E") ; <CJK>
       (?傑 . "0x5091") ; <CJK>
       (?杰 . "0x6770") ; <CJK>
       (?桀 . "0x6840") ; <CJK>
       (?儉 . "0x5109") ; <CJK>
       (?劍 . "0x528D") ; <CJK>
       (?劒 . "0x5292") ; <CJK>
       (?檢 . "0x6AA2") ; <CJK>
       (?瞼 . "0x77BC") ; <CJK>
       (?鈐 . "0x9210") ; <CJK>
       (?黔 . "0x9ED4") ; <CJK>
       (?劫 . "0x52AB") ; <CJK>
       (?怯 . "0x602F") ; <CJK>
       (?迲 . "0x8FF2") ; <CJK>
       (?偈 . "0x5048") ; <CJK>
       (?憩 . "0x61A9") ; <CJK>
       (?揭 . "0x63ED") ; <CJK>
       (?擊 . "0x64CA") ; <CJK>
       (?格 . "0x683C") ; <CJK>
       (?檄 . "0x6A84") ; <CJK>
       (?激 . "0x6FC0") ; <CJK>
       (?膈 . "0x8188") ; <CJK>
       (?覡 . "0x89A1") ; <CJK>
       (?隔 . "0x9694") ; <CJK>
       (?堅 . "0x5805") ; <CJK>
       (?牽 . "0x727D") ; <CJK>
       (?犬 . "0x72AC") ; <CJK>
       (?甄 . "0x7504") ; <CJK>
       (?絹 . "0x7D79") ; <CJK>
       (?繭 . "0x7E6D") ; <CJK>
       (?肩 . "0x80A9") ; <CJK>
       (?見 . "0x898B") ; <CJK>
       (?譴 . "0x8B74") ; <CJK>
       (?遣 . "0x9063") ; <CJK>
       (?鵑 . "0x9D51") ; <CJK>
       (?抉 . "0x6289") ; <CJK>
       (?決 . "0x6C7A") ; <CJK>
       (?潔 . "0x6F54") ; <CJK>
       (?結 . "0x7D50") ; <CJK>
       (?缺 . "0x7F3A") ; <CJK>
       (?訣 . "0x8A23") ; <CJK>
       (?兼 . "0x517C") ; <CJK>
       (?慊 . "0x614A") ; <CJK>
       (?箝 . "0x7B9D") ; <CJK>
       (?謙 . "0x8B19") ; <CJK>
       (?鉗 . "0x9257") ; <CJK>
       (?鎌 . "0x938C") ; <CJK>
       (?京 . "0x4EAC") ; <CJK>
       (?俓 . "0x4FD3") ; <CJK>
       (?倞 . "0x501E") ; <CJK>
       (?傾 . "0x50BE") ; <CJK>
       (?儆 . "0x5106") ; <CJK>
       (?勁 . "0x52C1") ; <CJK>
       (?勍 . "0x52CD") ; <CJK>
       (?卿 . "0x537F") ; <CJK>
       (?坰 . "0x5770") ; <CJK>
       (?境 . "0x5883") ; <CJK>
       (?庚 . "0x5E9A") ; <CJK>
       (?徑 . "0x5F91") ; <CJK>
       (?慶 . "0x6176") ; <CJK>
       (?憬 . "0x61AC") ; <CJK>
       (?擎 . "0x64CE") ; <CJK>
       (?敬 . "0x656C") ; <CJK>
       (?景 . "0x666F") ; <CJK>
       (?暻 . "0x66BB") ; <CJK>
       (?更 . "0x66F4") ; <CJK>
       (?梗 . "0x6897") ; <CJK>
       (?涇 . "0x6D87") ; <CJK>
       (?炅 . "0x7085") ; <CJK>
       (?烱 . "0x70F1") ; <CJK>
       (?璟 . "0x749F") ; <CJK>
       (?璥 . "0x74A5") ; <CJK>
       (?瓊 . "0x74CA") ; <CJK>
       (?痙 . "0x75D9") ; <CJK>
       (?硬 . "0x786C") ; <CJK>
       (?磬 . "0x78EC") ; <CJK>
       (?竟 . "0x7ADF") ; <CJK>
       (?競 . "0x7AF6") ; <CJK>
       (?絅 . "0x7D45") ; <CJK>
       (?經 . "0x7D93") ; <CJK>
       (?耕 . "0x8015") ; <CJK>
       (?耿 . "0x803F") ; <CJK>
       (?脛 . "0x811B") ; <CJK>
       (?莖 . "0x8396") ; <CJK>
       (?警 . "0x8B66") ; <CJK>
       (?輕 . "0x8F15") ; <CJK>
       (?逕 . "0x9015") ; <CJK>
       (?鏡 . "0x93E1") ; <CJK>
       (?頃 . "0x9803") ; <CJK>
       (?頸 . "0x9838") ; <CJK>
       (?驚 . "0x9A5A") ; <CJK>
       (?鯨 . "0x9BE8") ; <CJK>
       (?係 . "0x4FC2") ; <CJK>
       (?啓 . "0x5553") ; <CJK>
       (?堺 . "0x583A") ; <CJK>
       (?契 . "0x5951") ; <CJK>
       (?季 . "0x5B63") ; <CJK>
       (?屆 . "0x5C46") ; <CJK>
       (?悸 . "0x60B8") ; <CJK>
       (?戒 . "0x6212") ; <CJK>
       (?桂 . "0x6842") ; <CJK>
       (?械 . "0x68B0") ; <CJK>
       (?棨 . "0x68E8") ; <CJK>
       (?溪 . "0x6EAA") ; <CJK>
       (?界 . "0x754C") ; <CJK>
       (?癸 . "0x7678") ; <CJK>
       (?磎 . "0x78CE") ; <CJK>
       (?稽 . "0x7A3D") ; <CJK>
       (?系 . "0x7CFB") ; <CJK>
       (?繫 . "0x7E6B") ; <CJK>
       (?繼 . "0x7E7C") ; <CJK>
       (?計 . "0x8A08") ; <CJK>
       (?誡 . "0x8AA1") ; <CJK>
       (?谿 . "0x8C3F") ; <CJK>
       (?階 . "0x968E") ; <CJK>
       (?鷄 . "0x9DC4") ; <CJK>
       (?古 . "0x53E4") ; <CJK>
       (?叩 . "0x53E9") ; <CJK>
       (?告 . "0x544A") ; <CJK>
       (?呱 . "0x5471") ; <CJK>
       (?固 . "0x56FA") ; <CJK>
       (?姑 . "0x59D1") ; <CJK>
       (?孤 . "0x5B64") ; <CJK>
       (?尻 . "0x5C3B") ; <CJK>
       (?庫 . "0x5EAB") ; <CJK>
       (?拷 . "0x62F7") ; <CJK>
       (?攷 . "0x6537") ; <CJK>
       (?故 . "0x6545") ; <CJK>
       (?敲 . "0x6572") ; <CJK>
       (?暠 . "0x66A0") ; <CJK>
       (?枯 . "0x67AF") ; <CJK>
       (?槁 . "0x69C1") ; <CJK>
       (?沽 . "0x6CBD") ; <CJK>
       (?痼 . "0x75FC") ; <CJK>
       (?皐 . "0x7690") ; <CJK>
       (?睾 . "0x777E") ; <CJK>
       (?稿 . "0x7A3F") ; <CJK>
       (?羔 . "0x7F94") ; <CJK>
       (?考 . "0x8003") ; <CJK>
       (?股 . "0x80A1") ; <CJK>
       (?膏 . "0x818F") ; <CJK>
       (?苦 . "0x82E6") ; <CJK>
       (?苽 . "0x82FD") ; <CJK>
       (?菰 . "0x83F0") ; <CJK>
       (?藁 . "0x85C1") ; <CJK>
       (?蠱 . "0x8831") ; <CJK>
       (?袴 . "0x88B4") ; <CJK>
       (?誥 . "0x8AA5") ; <CJK>
       (?賈 . "0xF903") ; <CJK>
       (?辜 . "0x8F9C") ; <CJK>
       (?錮 . "0x932E") ; <CJK>
       (?雇 . "0x96C7") ; <CJK>
       (?顧 . "0x9867") ; <CJK>
       (?高 . "0x9AD8") ; <CJK>
       (?鼓 . "0x9F13") ; <CJK>
       (?哭 . "0x54ED") ; <CJK>
       (?斛 . "0x659B") ; <CJK>
       (?曲 . "0x66F2") ; <CJK>
       (?梏 . "0x688F") ; <CJK>
       (?穀 . "0x7A40") ; <CJK>
       (?谷 . "0x8C37") ; <CJK>
       (?鵠 . "0x9D60") ; <CJK>
       (?困 . "0x56F0") ; <CJK>
       (?坤 . "0x5764") ; <CJK>
       (?崑 . "0x5D11") ; <CJK>
       (?昆 . "0x6606") ; <CJK>
       (?梱 . "0x68B1") ; <CJK>
       (?棍 . "0x68CD") ; <CJK>
       (?滾 . "0x6EFE") ; <CJK>
       (?琨 . "0x7428") ; <CJK>
       (?袞 . "0x889E") ; <CJK>
       (?鯤 . "0x9BE4") ; <CJK>
       (?汨 . "0x6C68") ; <CJK>
       (?滑 . "0xF904") ; <CJK>
       (?骨 . "0x9AA8") ; <CJK>
       (?供 . "0x4F9B") ; <CJK>
       (?公 . "0x516C") ; <CJK>
       (?共 . "0x5171") ; <CJK>
       (?功 . "0x529F") ; <CJK>
       (?孔 . "0x5B54") ; <CJK>
       (?工 . "0x5DE5") ; <CJK>
       (?恐 . "0x6050") ; <CJK>
       (?恭 . "0x606D") ; <CJK>
       (?拱 . "0x62F1") ; <CJK>
       (?控 . "0x63A7") ; <CJK>
       (?攻 . "0x653B") ; <CJK>
       (?珙 . "0x73D9") ; <CJK>
       (?空 . "0x7A7A") ; <CJK>
       (?蚣 . "0x86A3") ; <CJK>
       (?貢 . "0x8CA2") ; <CJK>
       (?鞏 . "0x978F") ; <CJK>
       (?串 . "0x4E32") ; <CJK>
       (?寡 . "0x5BE1") ; <CJK>
       (?戈 . "0x6208") ; <CJK>
       (?果 . "0x679C") ; <CJK>
       (?瓜 . "0x74DC") ; <CJK>
       (?科 . "0x79D1") ; <CJK>
       (?菓 . "0x83D3") ; <CJK>
       (?誇 . "0x8A87") ; <CJK>
       (?課 . "0x8AB2") ; <CJK>
       (?跨 . "0x8DE8") ; <CJK>
       (?過 . "0x904E") ; <CJK>
       (?鍋 . "0x934B") ; <CJK>
       (?顆 . "0x9846") ; <CJK>
       (?廓 . "0x5ED3") ; <CJK>
       (?槨 . "0x69E8") ; <CJK>
       (?藿 . "0x85FF") ; <CJK>
       (?郭 . "0x90ED") ; <CJK>
       (?串 . "0xF905") ; <CJK>
       (?冠 . "0x51A0") ; <CJK>
       (?官 . "0x5B98") ; <CJK>
       (?寬 . "0x5BEC") ; <CJK>
       (?慣 . "0x6163") ; <CJK>
       (?棺 . "0x68FA") ; <CJK>
       (?款 . "0x6B3E") ; <CJK>
       (?灌 . "0x704C") ; <CJK>
       (?琯 . "0x742F") ; <CJK>
       (?瓘 . "0x74D8") ; <CJK>
       (?管 . "0x7BA1") ; <CJK>
       (?罐 . "0x7F50") ; <CJK>
       (?菅 . "0x83C5") ; <CJK>
       (?觀 . "0x89C0") ; <CJK>
       (?貫 . "0x8CAB") ; <CJK>
       (?關 . "0x95DC") ; <CJK>
       (?館 . "0x9928") ; <CJK>
       (?刮 . "0x522E") ; <CJK>
       (?恝 . "0x605D") ; <CJK>
       (?括 . "0x62EC") ; <CJK>
       (?适 . "0x9002") ; <CJK>
       (?侊 . "0x4F8A") ; <CJK>
       (?光 . "0x5149") ; <CJK>
       (?匡 . "0x5321") ; <CJK>
       (?壙 . "0x58D9") ; <CJK>
       (?廣 . "0x5EE3") ; <CJK>
       (?曠 . "0x66E0") ; <CJK>
       (?洸 . "0x6D38") ; <CJK>
       (?炚 . "0x709A") ; <CJK>
       (?狂 . "0x72C2") ; <CJK>
       (?珖 . "0x73D6") ; <CJK>
       (?筐 . "0x7B50") ; <CJK>
       (?胱 . "0x80F1") ; <CJK>
       (?鑛 . "0x945B") ; <CJK>
       (?卦 . "0x5366") ; <CJK>
       (?掛 . "0x639B") ; <CJK>
       (?罫 . "0x7F6B") ; <CJK>
       (?乖 . "0x4E56") ; <CJK>
       (?傀 . "0x5080") ; <CJK>
       (?塊 . "0x584A") ; <CJK>
       (?壞 . "0x58DE") ; <CJK>
       (?怪 . "0x602A") ; <CJK>
       (?愧 . "0x6127") ; <CJK>
       (?拐 . "0x62D0") ; <CJK>
       (?槐 . "0x69D0") ; <CJK>
       (?魁 . "0x9B41") ; <CJK>
       (?宏 . "0x5B8F") ; <CJK>
       (?紘 . "0x7D18") ; <CJK>
       (?肱 . "0x80B1") ; <CJK>
       (?轟 . "0x8F5F") ; <CJK>
       (?交 . "0x4EA4") ; <CJK>
       (?僑 . "0x50D1") ; <CJK>
       (?咬 . "0x54AC") ; <CJK>
       (?喬 . "0x55AC") ; <CJK>
       (?嬌 . "0x5B0C") ; <CJK>
       (?嶠 . "0x5DA0") ; <CJK>
       (?巧 . "0x5DE7") ; <CJK>
       (?攪 . "0x652A") ; <CJK>
       (?敎 . "0x654E") ; <CJK>
       (?校 . "0x6821") ; <CJK>
       (?橋 . "0x6A4B") ; <CJK>
       (?狡 . "0x72E1") ; <CJK>
       (?皎 . "0x768E") ; <CJK>
       (?矯 . "0x77EF") ; <CJK>
       (?絞 . "0x7D5E") ; <CJK>
       (?翹 . "0x7FF9") ; <CJK>
       (?膠 . "0x81A0") ; <CJK>
       (?蕎 . "0x854E") ; <CJK>
       (?蛟 . "0x86DF") ; <CJK>
       (?較 . "0x8F03") ; <CJK>
       (?轎 . "0x8F4E") ; <CJK>
       (?郊 . "0x90CA") ; <CJK>
       (?餃 . "0x9903") ; <CJK>
       (?驕 . "0x9A55") ; <CJK>
       (?鮫 . "0x9BAB") ; <CJK>
       (?丘 . "0x4E18") ; <CJK>
       (?久 . "0x4E45") ; <CJK>
       (?九 . "0x4E5D") ; <CJK>
       (?仇 . "0x4EC7") ; <CJK>
       (?俱 . "0x4FF1") ; <CJK>
       (?具 . "0x5177") ; <CJK>
       (?勾 . "0x52FE") ; <CJK>
       (?區 . "0x5340") ; <CJK>
       (?口 . "0x53E3") ; <CJK>
       (?句 . "0x53E5") ; <CJK>
       (?咎 . "0x548E") ; <CJK>
       (?嘔 . "0x5614") ; <CJK>
       (?坵 . "0x5775") ; <CJK>
       (?垢 . "0x57A2") ; <CJK>
       (?寇 . "0x5BC7") ; <CJK>
       (?嶇 . "0x5D87") ; <CJK>
       (?廐 . "0x5ED0") ; <CJK>
       (?懼 . "0x61FC") ; <CJK>
       (?拘 . "0x62D8") ; <CJK>
       (?救 . "0x6551") ; <CJK>
       (?枸 . "0x67B8") ; <CJK>
       (?柩 . "0x67E9") ; <CJK>
       (?構 . "0x69CB") ; <CJK>
       (?歐 . "0x6B50") ; <CJK>
       (?毆 . "0x6BC6") ; <CJK>
       (?毬 . "0x6BEC") ; <CJK>
       (?求 . "0x6C42") ; <CJK>
       (?溝 . "0x6E9D") ; <CJK>
       (?灸 . "0x7078") ; <CJK>
       (?狗 . "0x72D7") ; <CJK>
       (?玖 . "0x7396") ; <CJK>
       (?球 . "0x7403") ; <CJK>
       (?瞿 . "0x77BF") ; <CJK>
       (?矩 . "0x77E9") ; <CJK>
       (?究 . "0x7A76") ; <CJK>
       (?絿 . "0x7D7F") ; <CJK>
       (?耉 . "0x8009") ; <CJK>
       (?臼 . "0x81FC") ; <CJK>
       (?舅 . "0x8205") ; <CJK>
       (?舊 . "0x820A") ; <CJK>
       (?苟 . "0x82DF") ; <CJK>
       (?衢 . "0x8862") ; <CJK>
       (?謳 . "0x8B33") ; <CJK>
       (?購 . "0x8CFC") ; <CJK>
       (?軀 . "0x8EC0") ; <CJK>
       (?逑 . "0x9011") ; <CJK>
       (?邱 . "0x90B1") ; <CJK>
       (?鉤 . "0x9264") ; <CJK>
       (?銶 . "0x92B6") ; <CJK>
       (?駒 . "0x99D2") ; <CJK>
       (?驅 . "0x9A45") ; <CJK>
       (?鳩 . "0x9CE9") ; <CJK>
       (?鷗 . "0x9DD7") ; <CJK>
       (?龜 . "0x9F9C") ; <CJK>
       (?國 . "0x570B") ; <CJK>
       (?局 . "0x5C40") ; <CJK>
       (?菊 . "0x83CA") ; <CJK>
       (?鞠 . "0x97A0") ; <CJK>
       (?鞫 . "0x97AB") ; <CJK>
       (?麴 . "0x9EB4") ; <CJK>
       (?君 . "0x541B") ; <CJK>
       (?窘 . "0x7A98") ; <CJK>
       (?群 . "0x7FA4") ; <CJK>
       (?裙 . "0x88D9") ; <CJK>
       (?軍 . "0x8ECD") ; <CJK>
       (?郡 . "0x90E1") ; <CJK>
       (?堀 . "0x5800") ; <CJK>
       (?屈 . "0x5C48") ; <CJK>
       (?掘 . "0x6398") ; <CJK>
       (?窟 . "0x7A9F") ; <CJK>
       (?宮 . "0x5BAE") ; <CJK>
       (?弓 . "0x5F13") ; <CJK>
       (?穹 . "0x7A79") ; <CJK>
       (?窮 . "0x7AAE") ; <CJK>
       (?芎 . "0x828E") ; <CJK>
       (?躬 . "0x8EAC") ; <CJK>
       (?倦 . "0x5026") ; <CJK>
       (?券 . "0x5238") ; <CJK>
       (?勸 . "0x52F8") ; <CJK>
       (?卷 . "0x5377") ; <CJK>
       (?圈 . "0x5708") ; <CJK>
       (?拳 . "0x62F3") ; <CJK>
       (?捲 . "0x6372") ; <CJK>
       (?權 . "0x6B0A") ; <CJK>
       (?淃 . "0x6DC3") ; <CJK>
       (?眷 . "0x7737") ; <CJK>
       (?厥 . "0x53A5") ; <CJK>
       (?獗 . "0x7357") ; <CJK>
       (?蕨 . "0x8568") ; <CJK>
       (?蹶 . "0x8E76") ; <CJK>
       (?闕 . "0x95D5") ; <CJK>
       (?机 . "0x673A") ; <CJK>
       (?櫃 . "0x6AC3") ; <CJK>
       (?潰 . "0x6F70") ; <CJK>
       (?詭 . "0x8A6D") ; <CJK>
       (?軌 . "0x8ECC") ; <CJK>
       (?饋 . "0x994B") ; <CJK>
       (?句 . "0xF906") ; <CJK>
       (?晷 . "0x6677") ; <CJK>
       (?歸 . "0x6B78") ; <CJK>
       (?貴 . "0x8CB4") ; <CJK>
       (?鬼 . "0x9B3C") ; <CJK>
       (?龜 . "0xF907") ; <CJK>
       (?叫 . "0x53EB") ; <CJK>
       (?圭 . "0x572D") ; <CJK>
       (?奎 . "0x594E") ; <CJK>
       (?揆 . "0x63C6") ; <CJK>
       (?槻 . "0x69FB") ; <CJK>
       (?珪 . "0x73EA") ; <CJK>
       (?硅 . "0x7845") ; <CJK>
       (?窺 . "0x7ABA") ; <CJK>
       (?竅 . "0x7AC5") ; <CJK>
       (?糾 . "0x7CFE") ; <CJK>
       (?葵 . "0x8475") ; <CJK>
       (?規 . "0x898F") ; <CJK>
       (?赳 . "0x8D73") ; <CJK>
       (?逵 . "0x9035") ; <CJK>
       (?閨 . "0x95A8") ; <CJK>
       (?勻 . "0x52FB") ; <CJK>
       (?均 . "0x5747") ; <CJK>
       (?畇 . "0x7547") ; <CJK>
       (?筠 . "0x7B60") ; <CJK>
       (?菌 . "0x83CC") ; <CJK>
       (?鈞 . "0x921E") ; <CJK>
       (?龜 . "0xF908") ; <CJK>
       (?橘 . "0x6A58") ; <CJK>
       (?克 . "0x514B") ; <CJK>
       (?剋 . "0x524B") ; <CJK>
       (?劇 . "0x5287") ; <CJK>
       (?戟 . "0x621F") ; <CJK>
       (?棘 . "0x68D8") ; <CJK>
       (?極 . "0x6975") ; <CJK>
       (?隙 . "0x9699") ; <CJK>
       (?僅 . "0x50C5") ; <CJK>
       (?劤 . "0x52A4") ; <CJK>
       (?勤 . "0x52E4") ; <CJK>
       (?懃 . "0x61C3") ; <CJK>
       (?斤 . "0x65A4") ; <CJK>
       (?根 . "0x6839") ; <CJK>
       (?槿 . "0x69FF") ; <CJK>
       (?瑾 . "0x747E") ; <CJK>
       (?筋 . "0x7B4B") ; <CJK>
       (?芹 . "0x82B9") ; <CJK>
       (?菫 . "0x83EB") ; <CJK>
       (?覲 . "0x89B2") ; <CJK>
       (?謹 . "0x8B39") ; <CJK>
       (?近 . "0x8FD1") ; <CJK>
       (?饉 . "0x9949") ; <CJK>
       (?契 . "0xF909") ; <CJK>
       (?今 . "0x4ECA") ; <CJK>
       (?妗 . "0x5997") ; <CJK>
       (?擒 . "0x64D2") ; <CJK>
       (?昑 . "0x6611") ; <CJK>
       (?檎 . "0x6A8E") ; <CJK>
       (?琴 . "0x7434") ; <CJK>
       (?禁 . "0x7981") ; <CJK>
       (?禽 . "0x79BD") ; <CJK>
       (?芩 . "0x82A9") ; <CJK>
       (?衾 . "0x887E") ; <CJK>
       (?衿 . "0x887F") ; <CJK>
       (?襟 . "0x895F") ; <CJK>
       (?金 . "0xF90A") ; <CJK>
       (?錦 . "0x9326") ; <CJK>
       (?伋 . "0x4F0B") ; <CJK>
       (?及 . "0x53CA") ; <CJK>
       (?急 . "0x6025") ; <CJK>
       (?扱 . "0x6271") ; <CJK>
       (?汲 . "0x6C72") ; <CJK>
       (?級 . "0x7D1A") ; <CJK>
       (?給 . "0x7D66") ; <CJK>
       (?亘 . "0x4E98") ; <CJK>
       (?兢 . "0x5162") ; <CJK>
       (?矜 . "0x77DC") ; <CJK>
       (?肯 . "0x80AF") ; <CJK>
       (?企 . "0x4F01") ; <CJK>
       (?伎 . "0x4F0E") ; <CJK>
       (?其 . "0x5176") ; <CJK>
       (?冀 . "0x5180") ; <CJK>
       (?嗜 . "0x55DC") ; <CJK>
       (?器 . "0x5668") ; <CJK>
       (?圻 . "0x573B") ; <CJK>
       (?基 . "0x57FA") ; <CJK>
       (?埼 . "0x57FC") ; <CJK>
       (?夔 . "0x5914") ; <CJK>
       (?奇 . "0x5947") ; <CJK>
       (?妓 . "0x5993") ; <CJK>
       (?寄 . "0x5BC4") ; <CJK>
       (?岐 . "0x5C90") ; <CJK>
       (?崎 . "0x5D0E") ; <CJK>
       (?己 . "0x5DF1") ; <CJK>
       (?幾 . "0x5E7E") ; <CJK>
       (?忌 . "0x5FCC") ; <CJK>
       (?技 . "0x6280") ; <CJK>
       (?旗 . "0x65D7") ; <CJK>
       (?旣 . "0x65E3") ; <CJK>
       (?朞 . "0x671E") ; <CJK>
       (?期 . "0x671F") ; <CJK>
       (?杞 . "0x675E") ; <CJK>
       (?棋 . "0x68CB") ; <CJK>
       (?棄 . "0x68C4") ; <CJK>
       (?機 . "0x6A5F") ; <CJK>
       (?欺 . "0x6B3A") ; <CJK>
       (?氣 . "0x6C23") ; <CJK>
       (?汽 . "0x6C7D") ; <CJK>
       (?沂 . "0x6C82") ; <CJK>
       (?淇 . "0x6DC7") ; <CJK>
       (?玘 . "0x7398") ; <CJK>
       (?琦 . "0x7426") ; <CJK>
       (?琪 . "0x742A") ; <CJK>
       (?璂 . "0x7482") ; <CJK>
       (?璣 . "0x74A3") ; <CJK>
       (?畸 . "0x7578") ; <CJK>
       (?畿 . "0x757F") ; <CJK>
       (?碁 . "0x7881") ; <CJK>
       (?磯 . "0x78EF") ; <CJK>
       (?祁 . "0x7941") ; <CJK>
       (?祇 . "0x7947") ; <CJK>
       (?祈 . "0x7948") ; <CJK>
       (?祺 . "0x797A") ; <CJK>
       (?箕 . "0x7B95") ; <CJK>
       (?紀 . "0x7D00") ; <CJK>
       (?綺 . "0x7DBA") ; <CJK>
       (?羈 . "0x7F88") ; <CJK>
       (?耆 . "0x8006") ; <CJK>
       (?耭 . "0x802D") ; <CJK>
       (?肌 . "0x808C") ; <CJK>
       (?記 . "0x8A18") ; <CJK>
       (?譏 . "0x8B4F") ; <CJK>
       (?豈 . "0x8C48") ; <CJK>
       (?起 . "0x8D77") ; <CJK>
       (?錡 . "0x9321") ; <CJK>
       (?錤 . "0x9324") ; <CJK>
       (?飢 . "0x98E2") ; <CJK>
       (?饑 . "0x9951") ; <CJK>
       (?騎 . "0x9A0E") ; <CJK>
       (?騏 . "0x9A0F") ; <CJK>
       (?驥 . "0x9A65") ; <CJK>
       (?麒 . "0x9E92") ; <CJK>
       (?緊 . "0x7DCA") ; <CJK>
       (?佶 . "0x4F76") ; <CJK>
       (?吉 . "0x5409") ; <CJK>
       (?拮 . "0x62EE") ; <CJK>
       (?桔 . "0x6854") ; <CJK>
       (?金 . "0x91D1") ; <CJK>
       (?喫 . "0x55AB") ; <CJK>
       (?儺 . "0x513A") ; <CJK>
       (?喇 . "0xF90B") ; <CJK>
       (?奈 . "0xF90C") ; <CJK>
       (?娜 . "0x5A1C") ; <CJK>
       (?懦 . "0x61E6") ; <CJK>
       (?懶 . "0xF90D") ; <CJK>
       (?拏 . "0x62CF") ; <CJK>
       (?拿 . "0x62FF") ; <CJK>
       (?癩 . "0xF90E") ; <CJK>
       (?羅 . "0xF90F") ; <CJK>
       (?蘿 . "0xF910") ; <CJK>
       (?螺 . "0xF911") ; <CJK>
       (?裸 . "0xF912") ; <CJK>
       (?邏 . "0xF913") ; <CJK>
       (?那 . "0x90A3") ; <CJK>
       (?樂 . "0xF914") ; <CJK>
       (?洛 . "0xF915") ; <CJK>
       (?烙 . "0xF916") ; <CJK>
       (?珞 . "0xF917") ; <CJK>
       (?落 . "0xF918") ; <CJK>
       (?諾 . "0x8AFE") ; <CJK>
       (?酪 . "0xF919") ; <CJK>
       (?駱 . "0xF91A") ; <CJK>
       (?亂 . "0xF91B") ; <CJK>
       (?卵 . "0xF91C") ; <CJK>
       (?暖 . "0x6696") ; <CJK>
       (?欄 . "0xF91D") ; <CJK>
       (?煖 . "0x7156") ; <CJK>
       (?爛 . "0xF91E") ; <CJK>
       (?蘭 . "0xF91F") ; <CJK>
       (?難 . "0x96E3") ; <CJK>
       (?鸞 . "0xF920") ; <CJK>
       (?捏 . "0x634F") ; <CJK>
       (?捺 . "0x637A") ; <CJK>
       (?南 . "0x5357") ; <CJK>
       (?嵐 . "0xF921") ; <CJK>
       (?枏 . "0x678F") ; <CJK>
       (?楠 . "0x6960") ; <CJK>
       (?湳 . "0x6E73") ; <CJK>
       (?濫 . "0xF922") ; <CJK>
       (?男 . "0x7537") ; <CJK>
       (?藍 . "0xF923") ; <CJK>
       (?襤 . "0xF924") ; <CJK>
       (?拉 . "0xF925") ; <CJK>
       (?納 . "0x7D0D") ; <CJK>
       (?臘 . "0xF926") ; <CJK>
       (?蠟 . "0xF927") ; <CJK>
       (?衲 . "0x8872") ; <CJK>
       (?囊 . "0x56CA") ; <CJK>
       (?娘 . "0x5A18") ; <CJK>
       (?廊 . "0xF928") ; <CJK>
       (?朗 . "0xF929") ; <CJK>
       (?浪 . "0xF92A") ; <CJK>
       (?狼 . "0xF92B") ; <CJK>
       (?郎 . "0xF92C") ; <CJK>
       (?乃 . "0x4E43") ; <CJK>
       (?來 . "0xF92D") ; <CJK>
       (?內 . "0x5167") ; <CJK>
       (?奈 . "0x5948") ; <CJK>
       (?柰 . "0x67F0") ; <CJK>
       (?耐 . "0x8010") ; <CJK>
       (?冷 . "0xF92E") ; <CJK>
       (?女 . "0x5973") ; <CJK>
       (?年 . "0x5E74") ; <CJK>
       (?撚 . "0x649A") ; <CJK>
       (?秊 . "0x79CA") ; <CJK>
       (?念 . "0x5FF5") ; <CJK>
       (?恬 . "0x606C") ; <CJK>
       (?拈 . "0x62C8") ; <CJK>
       (?捻 . "0x637B") ; <CJK>
       (?寧 . "0x5BE7") ; <CJK>
       (?寗 . "0x5BD7") ; <CJK>
       (?努 . "0x52AA") ; <CJK>
       (?勞 . "0xF92F") ; <CJK>
       (?奴 . "0x5974") ; <CJK>
       (?弩 . "0x5F29") ; <CJK>
       (?怒 . "0x6012") ; <CJK>
       (?擄 . "0xF930") ; <CJK>
       (?櫓 . "0xF931") ; <CJK>
       (?爐 . "0xF932") ; <CJK>
       (?瑙 . "0x7459") ; <CJK>
       (?盧 . "0xF933") ; <CJK>
       (?老 . "0xF934") ; <CJK>
       (?蘆 . "0xF935") ; <CJK>
       (?虜 . "0xF936") ; <CJK>
       (?路 . "0xF937") ; <CJK>
       (?露 . "0xF938") ; <CJK>
       (?駑 . "0x99D1") ; <CJK>
       (?魯 . "0xF939") ; <CJK>
       (?鷺 . "0xF93A") ; <CJK>
       (?碌 . "0xF93B") ; <CJK>
       (?祿 . "0xF93C") ; <CJK>
       (?綠 . "0xF93D") ; <CJK>
       (?菉 . "0xF93E") ; <CJK>
       (?錄 . "0xF93F") ; <CJK>
       (?鹿 . "0xF940") ; <CJK>
       (?論 . "0xF941") ; <CJK>
       (?壟 . "0xF942") ; <CJK>
       (?弄 . "0xF943") ; <CJK>
       (?濃 . "0x6FC3") ; <CJK>
       (?籠 . "0xF944") ; <CJK>
       (?聾 . "0xF945") ; <CJK>
       (?膿 . "0x81BF") ; <CJK>
       (?農 . "0x8FB2") ; <CJK>
       (?惱 . "0x60F1") ; <CJK>
       (?牢 . "0xF946") ; <CJK>
       (?磊 . "0xF947") ; <CJK>
       (?腦 . "0x8166") ; <CJK>
       (?賂 . "0xF948") ; <CJK>
       (?雷 . "0xF949") ; <CJK>
       (?尿 . "0x5C3F") ; <CJK>
       (?壘 . "0xF94A") ; <CJK>
       (?屢 . "0xF94B") ; <CJK>
       (?樓 . "0xF94C") ; <CJK>
       (?淚 . "0xF94D") ; <CJK>
       (?漏 . "0xF94E") ; <CJK>
       (?累 . "0xF94F") ; <CJK>
       (?縷 . "0xF950") ; <CJK>
       (?陋 . "0xF951") ; <CJK>
       (?嫩 . "0x5AE9") ; <CJK>
       (?訥 . "0x8A25") ; <CJK>
       (?杻 . "0x677B") ; <CJK>
       (?紐 . "0x7D10") ; <CJK>
       (?勒 . "0xF952") ; <CJK>
       (?肋 . "0xF953") ; <CJK>
       (?凜 . "0xF954") ; <CJK>
       (?凌 . "0xF955") ; <CJK>
       (?稜 . "0xF956") ; <CJK>
       (?綾 . "0xF957") ; <CJK>
       (?能 . "0x80FD") ; <CJK>
       (?菱 . "0xF958") ; <CJK>
       (?陵 . "0xF959") ; <CJK>
       (?尼 . "0x5C3C") ; <CJK>
       (?泥 . "0x6CE5") ; <CJK>
       (?匿 . "0x533F") ; <CJK>
       (?溺 . "0x6EBA") ; <CJK>
       (?多 . "0x591A") ; <CJK>
       (?茶 . "0x8336") ; <CJK>
       (?丹 . "0x4E39") ; <CJK>
       (?亶 . "0x4EB6") ; <CJK>
       (?但 . "0x4F46") ; <CJK>
       (?單 . "0x55AE") ; <CJK>
       (?團 . "0x5718") ; <CJK>
       (?壇 . "0x58C7") ; <CJK>
       (?彖 . "0x5F56") ; <CJK>
       (?斷 . "0x65B7") ; <CJK>
       (?旦 . "0x65E6") ; <CJK>
       (?檀 . "0x6A80") ; <CJK>
       (?段 . "0x6BB5") ; <CJK>
       (?湍 . "0x6E4D") ; <CJK>
       (?短 . "0x77ED") ; <CJK>
       (?端 . "0x7AEF") ; <CJK>
       (?簞 . "0x7C1E") ; <CJK>
       (?緞 . "0x7DDE") ; <CJK>
       (?蛋 . "0x86CB") ; <CJK>
       (?袒 . "0x8892") ; <CJK>
       (?鄲 . "0x9132") ; <CJK>
       (?鍛 . "0x935B") ; <CJK>
       (?撻 . "0x64BB") ; <CJK>
       (?澾 . "0x6FBE") ; <CJK>
       (?獺 . "0x737A") ; <CJK>
       (?疸 . "0x75B8") ; <CJK>
       (?達 . "0x9054") ; <CJK>
       (?啖 . "0x5556") ; <CJK>
       (?坍 . "0x574D") ; <CJK>
       (?憺 . "0x61BA") ; <CJK>
       (?擔 . "0x64D4") ; <CJK>
       (?曇 . "0x66C7") ; <CJK>
       (?淡 . "0x6DE1") ; <CJK>
       (?湛 . "0x6E5B") ; <CJK>
       (?潭 . "0x6F6D") ; <CJK>
       (?澹 . "0x6FB9") ; <CJK>
       (?痰 . "0x75F0") ; <CJK>
       (?聃 . "0x8043") ; <CJK>
       (?膽 . "0x81BD") ; <CJK>
       (?蕁 . "0x8541") ; <CJK>
       (?覃 . "0x8983") ; <CJK>
       (?談 . "0x8AC7") ; <CJK>
       (?譚 . "0x8B5A") ; <CJK>
       (?錟 . "0x931F") ; <CJK>
       (?沓 . "0x6C93") ; <CJK>
       (?畓 . "0x7553") ; <CJK>
       (?答 . "0x7B54") ; <CJK>
       (?踏 . "0x8E0F") ; <CJK>
       (?遝 . "0x905D") ; <CJK>
       (?唐 . "0x5510") ; <CJK>
       (?堂 . "0x5802") ; <CJK>
       (?塘 . "0x5858") ; <CJK>
       (?幢 . "0x5E62") ; <CJK>
       (?戇 . "0x6207") ; <CJK>
       (?撞 . "0x649E") ; <CJK>
       (?棠 . "0x68E0") ; <CJK>
       (?當 . "0x7576") ; <CJK>
       (?糖 . "0x7CD6") ; <CJK>
       (?螳 . "0x87B3") ; <CJK>
       (?黨 . "0x9EE8") ; <CJK>
       (?代 . "0x4EE3") ; <CJK>
       (?垈 . "0x5788") ; <CJK>
       (?坮 . "0x576E") ; <CJK>
       (?大 . "0x5927") ; <CJK>
       (?對 . "0x5C0D") ; <CJK>
       (?岱 . "0x5CB1") ; <CJK>
       (?帶 . "0x5E36") ; <CJK>
       (?待 . "0x5F85") ; <CJK>
       (?戴 . "0x6234") ; <CJK>
       (?擡 . "0x64E1") ; <CJK>
       (?玳 . "0x73B3") ; <CJK>
       (?臺 . "0x81FA") ; <CJK>
       (?袋 . "0x888B") ; <CJK>
       (?貸 . "0x8CB8") ; <CJK>
       (?隊 . "0x968A") ; <CJK>
       (?黛 . "0x9EDB") ; <CJK>
       (?宅 . "0x5B85") ; <CJK>
       (?德 . "0x5FB7") ; <CJK>
       (?悳 . "0x60B3") ; <CJK>
       (?倒 . "0x5012") ; <CJK>
       (?刀 . "0x5200") ; <CJK>
       (?到 . "0x5230") ; <CJK>
       (?圖 . "0x5716") ; <CJK>
       (?堵 . "0x5835") ; <CJK>
       (?塗 . "0x5857") ; <CJK>
       (?導 . "0x5C0E") ; <CJK>
       (?屠 . "0x5C60") ; <CJK>
       (?島 . "0x5CF6") ; <CJK>
       (?嶋 . "0x5D8B") ; <CJK>
       (?度 . "0x5EA6") ; <CJK>
       (?徒 . "0x5F92") ; <CJK>
       (?悼 . "0x60BC") ; <CJK>
       (?挑 . "0x6311") ; <CJK>
       (?掉 . "0x6389") ; <CJK>
       (?搗 . "0x6417") ; <CJK>
       (?桃 . "0x6843") ; <CJK>
       (?棹 . "0x68F9") ; <CJK>
       (?櫂 . "0x6AC2") ; <CJK>
       (?淘 . "0x6DD8") ; <CJK>
       (?渡 . "0x6E21") ; <CJK>
       (?滔 . "0x6ED4") ; <CJK>
       (?濤 . "0x6FE4") ; <CJK>
       (?燾 . "0x71FE") ; <CJK>
       (?盜 . "0x76DC") ; <CJK>
       (?睹 . "0x7779") ; <CJK>
       (?禱 . "0x79B1") ; <CJK>
       (?稻 . "0x7A3B") ; <CJK>
       (?萄 . "0x8404") ; <CJK>
       (?覩 . "0x89A9") ; <CJK>
       (?賭 . "0x8CED") ; <CJK>
       (?跳 . "0x8DF3") ; <CJK>
       (?蹈 . "0x8E48") ; <CJK>
       (?逃 . "0x9003") ; <CJK>
       (?途 . "0x9014") ; <CJK>
       (?道 . "0x9053") ; <CJK>
       (?都 . "0x90FD") ; <CJK>
       (?鍍 . "0x934D") ; <CJK>
       (?陶 . "0x9676") ; <CJK>
       (?韜 . "0x97DC") ; <CJK>
       (?毒 . "0x6BD2") ; <CJK>
       (?瀆 . "0x7006") ; <CJK>
       (?牘 . "0x7258") ; <CJK>
       (?犢 . "0x72A2") ; <CJK>
       (?獨 . "0x7368") ; <CJK>
       (?督 . "0x7763") ; <CJK>
       (?禿 . "0x79BF") ; <CJK>
       (?篤 . "0x7BE4") ; <CJK>
       (?纛 . "0x7E9B") ; <CJK>
       (?讀 . "0x8B80") ; <CJK>
       (?墩 . "0x58A9") ; <CJK>
       (?惇 . "0x60C7") ; <CJK>
       (?敦 . "0x6566") ; <CJK>
       (?旽 . "0x65FD") ; <CJK>
       (?暾 . "0x66BE") ; <CJK>
       (?沌 . "0x6C8C") ; <CJK>
       (?焞 . "0x711E") ; <CJK>
       (?燉 . "0x71C9") ; <CJK>
       (?豚 . "0x8C5A") ; <CJK>
       (?頓 . "0x9813") ; <CJK>
       (?乭 . "0x4E6D") ; <CJK>
       (?突 . "0x7A81") ; <CJK>
       (?仝 . "0x4EDD") ; <CJK>
       (?冬 . "0x51AC") ; <CJK>
       (?凍 . "0x51CD") ; <CJK>
       (?動 . "0x52D5") ; <CJK>
       (?同 . "0x540C") ; <CJK>
       (?憧 . "0x61A7") ; <CJK>
       (?東 . "0x6771") ; <CJK>
       (?桐 . "0x6850") ; <CJK>
       (?棟 . "0x68DF") ; <CJK>
       (?洞 . "0x6D1E") ; <CJK>
       (?潼 . "0x6F7C") ; <CJK>
       (?疼 . "0x75BC") ; <CJK>
       (?瞳 . "0x77B3") ; <CJK>
       (?童 . "0x7AE5") ; <CJK>
       (?胴 . "0x80F4") ; <CJK>
       (?董 . "0x8463") ; <CJK>
       (?銅 . "0x9285") ; <CJK>
       (?兜 . "0x515C") ; <CJK>
       (?斗 . "0x6597") ; <CJK>
       (?杜 . "0x675C") ; <CJK>
       (?枓 . "0x6793") ; <CJK>
       (?痘 . "0x75D8") ; <CJK>
       (?竇 . "0x7AC7") ; <CJK>
       (?荳 . "0x8373") ; <CJK>
       (?讀 . "0xF95A") ; <CJK>
       (?豆 . "0x8C46") ; <CJK>
       (?逗 . "0x9017") ; <CJK>
       (?頭 . "0x982D") ; <CJK>
       (?屯 . "0x5C6F") ; <CJK>
       (?臀 . "0x81C0") ; <CJK>
       (?芚 . "0x829A") ; <CJK>
       (?遁 . "0x9041") ; <CJK>
       (?遯 . "0x906F") ; <CJK>
       (?鈍 . "0x920D") ; <CJK>
       (?得 . "0x5F97") ; <CJK>
       (?嶝 . "0x5D9D") ; <CJK>
       (?橙 . "0x6A59") ; <CJK>
       (?燈 . "0x71C8") ; <CJK>
       (?登 . "0x767B") ; <CJK>
       (?等 . "0x7B49") ; <CJK>
       (?藤 . "0x85E4") ; <CJK>
       (?謄 . "0x8B04") ; <CJK>
       (?鄧 . "0x9127") ; <CJK>
       (?騰 . "0x9A30") ; <CJK>
       (?喇 . "0x5587") ; <CJK>
       (?懶 . "0x61F6") ; <CJK>
       (?拏 . "0xF95B") ; <CJK>
       (?癩 . "0x7669") ; <CJK>
       (?羅 . "0x7F85") ; <CJK>
       (?蘿 . "0x863F") ; <CJK>
       (?螺 . "0x87BA") ; <CJK>
       (?裸 . "0x88F8") ; <CJK>
       (?邏 . "0x908F") ; <CJK>
       (?樂 . "0xF95C") ; <CJK>
       (?洛 . "0x6D1B") ; <CJK>
       (?烙 . "0x70D9") ; <CJK>
       (?珞 . "0x73DE") ; <CJK>
       (?絡 . "0x7D61") ; <CJK>
       (?落 . "0x843D") ; <CJK>
       (?諾 . "0xF95D") ; <CJK>
       (?酪 . "0x916A") ; <CJK>
       (?駱 . "0x99F1") ; <CJK>
       (?丹 . "0xF95E") ; <CJK>
       (?亂 . "0x4E82") ; <CJK>
       (?卵 . "0x5375") ; <CJK>
       (?欄 . "0x6B04") ; <CJK>
       (?欒 . "0x6B12") ; <CJK>
       (?瀾 . "0x703E") ; <CJK>
       (?爛 . "0x721B") ; <CJK>
       (?蘭 . "0x862D") ; <CJK>
       (?鸞 . "0x9E1E") ; <CJK>
       (?剌 . "0x524C") ; <CJK>
       (?辣 . "0x8FA3") ; <CJK>
       (?嵐 . "0x5D50") ; <CJK>
       (?擥 . "0x64E5") ; <CJK>
       (?攬 . "0x652C") ; <CJK>
       (?欖 . "0x6B16") ; <CJK>
       (?濫 . "0x6FEB") ; <CJK>
       (?籃 . "0x7C43") ; <CJK>
       (?纜 . "0x7E9C") ; <CJK>
       (?藍 . "0x85CD") ; <CJK>
       (?襤 . "0x8964") ; <CJK>
       (?覽 . "0x89BD") ; <CJK>
       (?拉 . "0x62C9") ; <CJK>
       (?臘 . "0x81D8") ; <CJK>
       (?蠟 . "0x881F") ; <CJK>
       (?廊 . "0x5ECA") ; <CJK>
       (?朗 . "0x6717") ; <CJK>
       (?浪 . "0x6D6A") ; <CJK>
       (?狼 . "0x72FC") ; <CJK>
       (?琅 . "0x7405") ; <CJK>
       (?瑯 . "0x746F") ; <CJK>
       (?螂 . "0x8782") ; <CJK>
       (?郞 . "0x90DE") ; <CJK>
       (?來 . "0x4F86") ; <CJK>
       (?崍 . "0x5D0D") ; <CJK>
       (?徠 . "0x5FA0") ; <CJK>
       (?萊 . "0x840A") ; <CJK>
       (?冷 . "0x51B7") ; <CJK>
       (?掠 . "0x63A0") ; <CJK>
       (?略 . "0x7565") ; <CJK>
       (?亮 . "0x4EAE") ; <CJK>
       (?倆 . "0x5006") ; <CJK>
       (?兩 . "0x5169") ; <CJK>
       (?凉 . "0x51C9") ; <CJK>
       (?梁 . "0x6881") ; <CJK>
       (?樑 . "0x6A11") ; <CJK>
       (?粮 . "0x7CAE") ; <CJK>
       (?粱 . "0x7CB1") ; <CJK>
       (?糧 . "0x7CE7") ; <CJK>
       (?良 . "0x826F") ; <CJK>
       (?諒 . "0x8AD2") ; <CJK>
       (?輛 . "0x8F1B") ; <CJK>
       (?量 . "0x91CF") ; <CJK>
       (?侶 . "0x4FB6") ; <CJK>
       (?儷 . "0x5137") ; <CJK>
       (?勵 . "0x52F5") ; <CJK>
       (?呂 . "0x5442") ; <CJK>
       (?廬 . "0x5EEC") ; <CJK>
       (?慮 . "0x616E") ; <CJK>
       (?戾 . "0x623E") ; <CJK>
       (?旅 . "0x65C5") ; <CJK>
       (?櫚 . "0x6ADA") ; <CJK>
       (?濾 . "0x6FFE") ; <CJK>
       (?礪 . "0x792A") ; <CJK>
       (?藜 . "0x85DC") ; <CJK>
       (?蠣 . "0x8823") ; <CJK>
       (?閭 . "0x95AD") ; <CJK>
       (?驢 . "0x9A62") ; <CJK>
       (?驪 . "0x9A6A") ; <CJK>
       (?麗 . "0x9E97") ; <CJK>
       (?黎 . "0x9ECE") ; <CJK>
       (?力 . "0x529B") ; <CJK>
       (?曆 . "0x66C6") ; <CJK>
       (?歷 . "0x6B77") ; <CJK>
       (?瀝 . "0x701D") ; <CJK>
       (?礫 . "0x792B") ; <CJK>
       (?轢 . "0x8F62") ; <CJK>
       (?靂 . "0x9742") ; <CJK>
       (?憐 . "0x6190") ; <CJK>
       (?戀 . "0x6200") ; <CJK>
       (?攣 . "0x6523") ; <CJK>
       (?漣 . "0x6F23") ; <CJK>
       (?煉 . "0x7149") ; <CJK>
       (?璉 . "0x7489") ; <CJK>
       (?練 . "0x7DF4") ; <CJK>
       (?聯 . "0x806F") ; <CJK>
       (?蓮 . "0x84EE") ; <CJK>
       (?輦 . "0x8F26") ; <CJK>
       (?連 . "0x9023") ; <CJK>
       (?鍊 . "0x934A") ; <CJK>
       (?冽 . "0x51BD") ; <CJK>
       (?列 . "0x5217") ; <CJK>
       (?劣 . "0x52A3") ; <CJK>
       (?洌 . "0x6D0C") ; <CJK>
       (?烈 . "0x70C8") ; <CJK>
       (?裂 . "0x88C2") ; <CJK>
       (?廉 . "0x5EC9") ; <CJK>
       (?斂 . "0x6582") ; <CJK>
       (?殮 . "0x6BAE") ; <CJK>
       (?濂 . "0x6FC2") ; <CJK>
       (?簾 . "0x7C3E") ; <CJK>
       (?獵 . "0x7375") ; <CJK>
       (?令 . "0x4EE4") ; <CJK>
       (?伶 . "0x4F36") ; <CJK>
       (?囹 . "0x56F9") ; <CJK>
       (?寧 . "0xF95F") ; <CJK>
       (?岺 . "0x5CBA") ; <CJK>
       (?嶺 . "0x5DBA") ; <CJK>
       (?怜 . "0x601C") ; <CJK>
       (?玲 . "0x73B2") ; <CJK>
       (?笭 . "0x7B2D") ; <CJK>
       (?羚 . "0x7F9A") ; <CJK>
       (?翎 . "0x7FCE") ; <CJK>
       (?聆 . "0x8046") ; <CJK>
       (?逞 . "0x901E") ; <CJK>
       (?鈴 . "0x9234") ; <CJK>
       (?零 . "0x96F6") ; <CJK>
       (?靈 . "0x9748") ; <CJK>
       (?領 . "0x9818") ; <CJK>
       (?齡 . "0x9F61") ; <CJK>
       (?例 . "0x4F8B") ; <CJK>
       (?澧 . "0x6FA7") ; <CJK>
       (?禮 . "0x79AE") ; <CJK>
       (?醴 . "0x91B4") ; <CJK>
       (?隷 . "0x96B7") ; <CJK>
       (?勞 . "0x52DE") ; <CJK>
       (?怒 . "0xF960") ; <CJK>
       (?撈 . "0x6488") ; <CJK>
       (?擄 . "0x64C4") ; <CJK>
       (?櫓 . "0x6AD3") ; <CJK>
       (?潞 . "0x6F5E") ; <CJK>
       (?瀘 . "0x7018") ; <CJK>
       (?爐 . "0x7210") ; <CJK>
       (?盧 . "0x76E7") ; <CJK>
       (?老 . "0x8001") ; <CJK>
       (?蘆 . "0x8606") ; <CJK>
       (?虜 . "0x865C") ; <CJK>
       (?路 . "0x8DEF") ; <CJK>
       (?輅 . "0x8F05") ; <CJK>
       (?露 . "0x9732") ; <CJK>
       (?魯 . "0x9B6F") ; <CJK>
       (?鷺 . "0x9DFA") ; <CJK>
       (?鹵 . "0x9E75") ; <CJK>
       (?碌 . "0x788C") ; <CJK>
       (?祿 . "0x797F") ; <CJK>
       (?綠 . "0x7DA0") ; <CJK>
       (?菉 . "0x83C9") ; <CJK>
       (?錄 . "0x9304") ; <CJK>
       (?鹿 . "0x9E7F") ; <CJK>
       (?麓 . "0x9E93") ; <CJK>
       (?論 . "0x8AD6") ; <CJK>
       (?壟 . "0x58DF") ; <CJK>
       (?弄 . "0x5F04") ; <CJK>
       (?朧 . "0x6727") ; <CJK>
       (?瀧 . "0x7027") ; <CJK>
       (?瓏 . "0x74CF") ; <CJK>
       (?籠 . "0x7C60") ; <CJK>
       (?聾 . "0x807E") ; <CJK>
       (?儡 . "0x5121") ; <CJK>
       (?瀨 . "0x7028") ; <CJK>
       (?牢 . "0x7262") ; <CJK>
       (?磊 . "0x78CA") ; <CJK>
       (?賂 . "0x8CC2") ; <CJK>
       (?賚 . "0x8CDA") ; <CJK>
       (?賴 . "0x8CF4") ; <CJK>
       (?雷 . "0x96F7") ; <CJK>
       (?了 . "0x4E86") ; <CJK>
       (?僚 . "0x50DA") ; <CJK>
       (?寮 . "0x5BEE") ; <CJK>
       (?廖 . "0x5ED6") ; <CJK>
       (?料 . "0x6599") ; <CJK>
       (?燎 . "0x71CE") ; <CJK>
       (?療 . "0x7642") ; <CJK>
       (?瞭 . "0x77AD") ; <CJK>
       (?聊 . "0x804A") ; <CJK>
       (?蓼 . "0x84FC") ; <CJK>
       (?遼 . "0x907C") ; <CJK>
       (?鬧 . "0x9B27") ; <CJK>
       (?龍 . "0x9F8D") ; <CJK>
       (?壘 . "0x58D8") ; <CJK>
       (?婁 . "0x5A41") ; <CJK>
       (?屢 . "0x5C62") ; <CJK>
       (?樓 . "0x6A13") ; <CJK>
       (?淚 . "0x6DDA") ; <CJK>
       (?漏 . "0x6F0F") ; <CJK>
       (?瘻 . "0x763B") ; <CJK>
       (?累 . "0x7D2F") ; <CJK>
       (?縷 . "0x7E37") ; <CJK>
       (?蔞 . "0x851E") ; <CJK>
       (?褸 . "0x8938") ; <CJK>
       (?鏤 . "0x93E4") ; <CJK>
       (?陋 . "0x964B") ; <CJK>
       (?劉 . "0x5289") ; <CJK>
       (?旒 . "0x65D2") ; <CJK>
       (?柳 . "0x67F3") ; <CJK>
       (?榴 . "0x69B4") ; <CJK>
       (?流 . "0x6D41") ; <CJK>
       (?溜 . "0x6E9C") ; <CJK>
       (?瀏 . "0x700F") ; <CJK>
       (?琉 . "0x7409") ; <CJK>
       (?瑠 . "0x7460") ; <CJK>
       (?留 . "0x7559") ; <CJK>
       (?瘤 . "0x7624") ; <CJK>
       (?硫 . "0x786B") ; <CJK>
       (?謬 . "0x8B2C") ; <CJK>
       (?類 . "0x985E") ; <CJK>
       (?六 . "0x516D") ; <CJK>
       (?戮 . "0x622E") ; <CJK>
       (?陸 . "0x9678") ; <CJK>
       (?侖 . "0x4F96") ; <CJK>
       (?倫 . "0x502B") ; <CJK>
       (?崙 . "0x5D19") ; <CJK>
       (?淪 . "0x6DEA") ; <CJK>
       (?綸 . "0x7DB8") ; <CJK>
       (?輪 . "0x8F2A") ; <CJK>
       (?律 . "0x5F8B") ; <CJK>
       (?慄 . "0x6144") ; <CJK>
       (?栗 . "0x6817") ; <CJK>
       (?率 . "0xF961") ; <CJK>
       (?隆 . "0x9686") ; <CJK>
       (?勒 . "0x52D2") ; <CJK>
       (?肋 . "0x808B") ; <CJK>
       (?凜 . "0x51DC") ; <CJK>
       (?凌 . "0x51CC") ; <CJK>
       (?楞 . "0x695E") ; <CJK>
       (?稜 . "0x7A1C") ; <CJK>
       (?綾 . "0x7DBE") ; <CJK>
       (?菱 . "0x83F1") ; <CJK>
       (?陵 . "0x9675") ; <CJK>
       (?俚 . "0x4FDA") ; <CJK>
       (?利 . "0x5229") ; <CJK>
       (?厘 . "0x5398") ; <CJK>
       (?吏 . "0x540F") ; <CJK>
       (?唎 . "0x550E") ; <CJK>
       (?履 . "0x5C65") ; <CJK>
       (?悧 . "0x60A7") ; <CJK>
       (?李 . "0x674E") ; <CJK>
       (?梨 . "0x68A8") ; <CJK>
       (?浬 . "0x6D6C") ; <CJK>
       (?犁 . "0x7281") ; <CJK>
       (?狸 . "0x72F8") ; <CJK>
       (?理 . "0x7406") ; <CJK>
       (?璃 . "0x7483") ; <CJK>
       (?異 . "0xF962") ; <CJK>
       (?痢 . "0x75E2") ; <CJK>
       (?籬 . "0x7C6C") ; <CJK>
       (?罹 . "0x7F79") ; <CJK>
       (?羸 . "0x7FB8") ; <CJK>
       (?莉 . "0x8389") ; <CJK>
       (?裏 . "0x88CF") ; <CJK>
       (?裡 . "0x88E1") ; <CJK>
       (?里 . "0x91CC") ; <CJK>
       (?釐 . "0x91D0") ; <CJK>
       (?離 . "0x96E2") ; <CJK>
       (?鯉 . "0x9BC9") ; <CJK>
       (?吝 . "0x541D") ; <CJK>
       (?潾 . "0x6F7E") ; <CJK>
       (?燐 . "0x71D0") ; <CJK>
       (?璘 . "0x7498") ; <CJK>
       (?藺 . "0x85FA") ; <CJK>
       (?躪 . "0x8EAA") ; <CJK>
       (?隣 . "0x96A3") ; <CJK>
       (?鱗 . "0x9C57") ; <CJK>
       (?麟 . "0x9E9F") ; <CJK>
       (?林 . "0x6797") ; <CJK>
       (?淋 . "0x6DCB") ; <CJK>
       (?琳 . "0x7433") ; <CJK>
       (?臨 . "0x81E8") ; <CJK>
       (?霖 . "0x9716") ; <CJK>
       (?砬 . "0x782C") ; <CJK>
       (?立 . "0x7ACB") ; <CJK>
       (?笠 . "0x7B20") ; <CJK>
       (?粒 . "0x7C92") ; <CJK>
       (?摩 . "0x6469") ; <CJK>
       (?瑪 . "0x746A") ; <CJK>
       (?痲 . "0x75F2") ; <CJK>
       (?碼 . "0x78BC") ; <CJK>
       (?磨 . "0x78E8") ; <CJK>
       (?馬 . "0x99AC") ; <CJK>
       (?魔 . "0x9B54") ; <CJK>
       (?麻 . "0x9EBB") ; <CJK>
       (?寞 . "0x5BDE") ; <CJK>
       (?幕 . "0x5E55") ; <CJK>
       (?漠 . "0x6F20") ; <CJK>
       (?膜 . "0x819C") ; <CJK>
       (?莫 . "0x83AB") ; <CJK>
       (?邈 . "0x9088") ; <CJK>
       (?万 . "0x4E07") ; <CJK>
       (?卍 . "0x534D") ; <CJK>
       (?娩 . "0x5A29") ; <CJK>
       (?巒 . "0x5DD2") ; <CJK>
       (?彎 . "0x5F4E") ; <CJK>
       (?慢 . "0x6162") ; <CJK>
       (?挽 . "0x633D") ; <CJK>
       (?晩 . "0x6669") ; <CJK>
       (?曼 . "0x66FC") ; <CJK>
       (?滿 . "0x6EFF") ; <CJK>
       (?漫 . "0x6F2B") ; <CJK>
       (?灣 . "0x7063") ; <CJK>
       (?瞞 . "0x779E") ; <CJK>
       (?萬 . "0x842C") ; <CJK>
       (?蔓 . "0x8513") ; <CJK>
       (?蠻 . "0x883B") ; <CJK>
       (?輓 . "0x8F13") ; <CJK>
       (?饅 . "0x9945") ; <CJK>
       (?鰻 . "0x9C3B") ; <CJK>
       (?唜 . "0x551C") ; <CJK>
       (?抹 . "0x62B9") ; <CJK>
       (?末 . "0x672B") ; <CJK>
       (?沫 . "0x6CAB") ; <CJK>
       (?茉 . "0x8309") ; <CJK>
       (?襪 . "0x896A") ; <CJK>
       (?靺 . "0x977A") ; <CJK>
       (?亡 . "0x4EA1") ; <CJK>
       (?妄 . "0x5984") ; <CJK>
       (?忘 . "0x5FD8") ; <CJK>
       (?忙 . "0x5FD9") ; <CJK>
       (?望 . "0x671B") ; <CJK>
       (?網 . "0x7DB2") ; <CJK>
       (?罔 . "0x7F54") ; <CJK>
       (?芒 . "0x8292") ; <CJK>
       (?茫 . "0x832B") ; <CJK>
       (?莽 . "0x83BD") ; <CJK>
       (?輞 . "0x8F1E") ; <CJK>
       (?邙 . "0x9099") ; <CJK>
       (?埋 . "0x57CB") ; <CJK>
       (?妹 . "0x59B9") ; <CJK>
       (?媒 . "0x5A92") ; <CJK>
       (?寐 . "0x5BD0") ; <CJK>
       (?昧 . "0x6627") ; <CJK>
       (?枚 . "0x679A") ; <CJK>
       (?梅 . "0x6885") ; <CJK>
       (?每 . "0x6BCF") ; <CJK>
       (?煤 . "0x7164") ; <CJK>
       (?罵 . "0x7F75") ; <CJK>
       (?買 . "0x8CB7") ; <CJK>
       (?賣 . "0x8CE3") ; <CJK>
       (?邁 . "0x9081") ; <CJK>
       (?魅 . "0x9B45") ; <CJK>
       (?脈 . "0x8108") ; <CJK>
       (?貊 . "0x8C8A") ; <CJK>
       (?陌 . "0x964C") ; <CJK>
       (?驀 . "0x9A40") ; <CJK>
       (?麥 . "0x9EA5") ; <CJK>
       (?孟 . "0x5B5F") ; <CJK>
       (?氓 . "0x6C13") ; <CJK>
       (?猛 . "0x731B") ; <CJK>
       (?盲 . "0x76F2") ; <CJK>
       (?盟 . "0x76DF") ; <CJK>
       (?萌 . "0x840C") ; <CJK>
       (?冪 . "0x51AA") ; <CJK>
       (?覓 . "0x8993") ; <CJK>
       (?免 . "0x514D") ; <CJK>
       (?冕 . "0x5195") ; <CJK>
       (?勉 . "0x52C9") ; <CJK>
       (?棉 . "0x68C9") ; <CJK>
       (?沔 . "0x6C94") ; <CJK>
       (?眄 . "0x7704") ; <CJK>
       (?眠 . "0x7720") ; <CJK>
       (?綿 . "0x7DBF") ; <CJK>
       (?緬 . "0x7DEC") ; <CJK>
       (?面 . "0x9762") ; <CJK>
       (?麵 . "0x9EB5") ; <CJK>
       (?滅 . "0x6EC5") ; <CJK>
       (?蔑 . "0x8511") ; <CJK>
       (?冥 . "0x51A5") ; <CJK>
       (?名 . "0x540D") ; <CJK>
       (?命 . "0x547D") ; <CJK>
       (?明 . "0x660E") ; <CJK>
       (?暝 . "0x669D") ; <CJK>
       (?椧 . "0x6927") ; <CJK>
       (?溟 . "0x6E9F") ; <CJK>
       (?皿 . "0x76BF") ; <CJK>
       (?瞑 . "0x7791") ; <CJK>
       (?茗 . "0x8317") ; <CJK>
       (?蓂 . "0x84C2") ; <CJK>
       (?螟 . "0x879F") ; <CJK>
       (?酩 . "0x9169") ; <CJK>
       (?銘 . "0x9298") ; <CJK>
       (?鳴 . "0x9CF4") ; <CJK>
       (?袂 . "0x8882") ; <CJK>
       (?侮 . "0x4FAE") ; <CJK>
       (?冒 . "0x5192") ; <CJK>
       (?募 . "0x52DF") ; <CJK>
       (?姆 . "0x59C6") ; <CJK>
       (?帽 . "0x5E3D") ; <CJK>
       (?慕 . "0x6155") ; <CJK>
       (?摸 . "0x6478") ; <CJK>
       (?摹 . "0x6479") ; <CJK>
       (?暮 . "0x66AE") ; <CJK>
       (?某 . "0x67D0") ; <CJK>
       (?模 . "0x6A21") ; <CJK>
       (?母 . "0x6BCD") ; <CJK>
       (?毛 . "0x6BDB") ; <CJK>
       (?牟 . "0x725F") ; <CJK>
       (?牡 . "0x7261") ; <CJK>
       (?瑁 . "0x7441") ; <CJK>
       (?眸 . "0x7738") ; <CJK>
       (?矛 . "0x77DB") ; <CJK>
       (?耗 . "0x8017") ; <CJK>
       (?芼 . "0x82BC") ; <CJK>
       (?茅 . "0x8305") ; <CJK>
       (?謀 . "0x8B00") ; <CJK>
       (?謨 . "0x8B28") ; <CJK>
       (?貌 . "0x8C8C") ; <CJK>
       (?木 . "0x6728") ; <CJK>
       (?沐 . "0x6C90") ; <CJK>
       (?牧 . "0x7267") ; <CJK>
       (?目 . "0x76EE") ; <CJK>
       (?睦 . "0x7766") ; <CJK>
       (?穆 . "0x7A46") ; <CJK>
       (?鶩 . "0x9DA9") ; <CJK>
       (?歿 . "0x6B7F") ; <CJK>
       (?沒 . "0x6C92") ; <CJK>
       (?夢 . "0x5922") ; <CJK>
       (?朦 . "0x6726") ; <CJK>
       (?蒙 . "0x8499") ; <CJK>
       (?卯 . "0x536F") ; <CJK>
       (?墓 . "0x5893") ; <CJK>
       (?妙 . "0x5999") ; <CJK>
       (?廟 . "0x5EDF") ; <CJK>
       (?描 . "0x63CF") ; <CJK>
       (?昴 . "0x6634") ; <CJK>
       (?杳 . "0x6773") ; <CJK>
       (?渺 . "0x6E3A") ; <CJK>
       (?猫 . "0x732B") ; <CJK>
       (?竗 . "0x7AD7") ; <CJK>
       (?苗 . "0x82D7") ; <CJK>
       (?錨 . "0x9328") ; <CJK>
       (?務 . "0x52D9") ; <CJK>
       (?巫 . "0x5DEB") ; <CJK>
       (?憮 . "0x61AE") ; <CJK>
       (?懋 . "0x61CB") ; <CJK>
       (?戊 . "0x620A") ; <CJK>
       (?拇 . "0x62C7") ; <CJK>
       (?撫 . "0x64AB") ; <CJK>
       (?无 . "0x65E0") ; <CJK>
       (?楙 . "0x6959") ; <CJK>
       (?武 . "0x6B66") ; <CJK>
       (?毋 . "0x6BCB") ; <CJK>
       (?無 . "0x7121") ; <CJK>
       (?珷 . "0x73F7") ; <CJK>
       (?畝 . "0x755D") ; <CJK>
       (?繆 . "0x7E46") ; <CJK>
       (?舞 . "0x821E") ; <CJK>
       (?茂 . "0x8302") ; <CJK>
       (?蕪 . "0x856A") ; <CJK>
       (?誣 . "0x8AA3") ; <CJK>
       (?貿 . "0x8CBF") ; <CJK>
       (?霧 . "0x9727") ; <CJK>
       (?鵡 . "0x9D61") ; <CJK>
       (?墨 . "0x58A8") ; <CJK>
       (?默 . "0x9ED8") ; <CJK>
       (?們 . "0x5011") ; <CJK>
       (?刎 . "0x520E") ; <CJK>
       (?吻 . "0x543B") ; <CJK>
       (?問 . "0x554F") ; <CJK>
       (?文 . "0x6587") ; <CJK>
       (?汶 . "0x6C76") ; <CJK>
       (?紊 . "0x7D0A") ; <CJK>
       (?紋 . "0x7D0B") ; <CJK>
       (?聞 . "0x805E") ; <CJK>
       (?蚊 . "0x868A") ; <CJK>
       (?門 . "0x9580") ; <CJK>
       (?雯 . "0x96EF") ; <CJK>
       (?勿 . "0x52FF") ; <CJK>
       (?沕 . "0x6C95") ; <CJK>
       (?物 . "0x7269") ; <CJK>
       (?味 . "0x5473") ; <CJK>
       (?媚 . "0x5A9A") ; <CJK>
       (?尾 . "0x5C3E") ; <CJK>
       (?嵋 . "0x5D4B") ; <CJK>
       (?彌 . "0x5F4C") ; <CJK>
       (?微 . "0x5FAE") ; <CJK>
       (?未 . "0x672A") ; <CJK>
       (?梶 . "0x68B6") ; <CJK>
       (?楣 . "0x6963") ; <CJK>
       (?渼 . "0x6E3C") ; <CJK>
       (?湄 . "0x6E44") ; <CJK>
       (?眉 . "0x7709") ; <CJK>
       (?米 . "0x7C73") ; <CJK>
       (?美 . "0x7F8E") ; <CJK>
       (?薇 . "0x8587") ; <CJK>
       (?謎 . "0x8B0E") ; <CJK>
       (?迷 . "0x8FF7") ; <CJK>
       (?靡 . "0x9761") ; <CJK>
       (?黴 . "0x9EF4") ; <CJK>
       (?岷 . "0x5CB7") ; <CJK>
       (?悶 . "0x60B6") ; <CJK>
       (?愍 . "0x610D") ; <CJK>
       (?憫 . "0x61AB") ; <CJK>
       (?敏 . "0x654F") ; <CJK>
       (?旻 . "0x65FB") ; <CJK>
       (?旼 . "0x65FC") ; <CJK>
       (?民 . "0x6C11") ; <CJK>
       (?泯 . "0x6CEF") ; <CJK>
       (?玟 . "0x739F") ; <CJK>
       (?珉 . "0x73C9") ; <CJK>
       (?緡 . "0x7DE1") ; <CJK>
       (?閔 . "0x9594") ; <CJK>
       (?密 . "0x5BC6") ; <CJK>
       (?蜜 . "0x871C") ; <CJK>
       (?謐 . "0x8B10") ; <CJK>
       (?剝 . "0x525D") ; <CJK>
       (?博 . "0x535A") ; <CJK>
       (?拍 . "0x62CD") ; <CJK>
       (?搏 . "0x640F") ; <CJK>
       (?撲 . "0x64B2") ; <CJK>
       (?朴 . "0x6734") ; <CJK>
       (?樸 . "0x6A38") ; <CJK>
       (?泊 . "0x6CCA") ; <CJK>
       (?珀 . "0x73C0") ; <CJK>
       (?璞 . "0x749E") ; <CJK>
       (?箔 . "0x7B94") ; <CJK>
       (?粕 . "0x7C95") ; <CJK>
       (?縛 . "0x7E1B") ; <CJK>
       (?膊 . "0x818A") ; <CJK>
       (?舶 . "0x8236") ; <CJK>
       (?薄 . "0x8584") ; <CJK>
       (?迫 . "0x8FEB") ; <CJK>
       (?雹 . "0x96F9") ; <CJK>
       (?駁 . "0x99C1") ; <CJK>
       (?伴 . "0x4F34") ; <CJK>
       (?半 . "0x534A") ; <CJK>
       (?反 . "0x53CD") ; <CJK>
       (?叛 . "0x53DB") ; <CJK>
       (?拌 . "0x62CC") ; <CJK>
       (?搬 . "0x642C") ; <CJK>
       (?攀 . "0x6500") ; <CJK>
       (?斑 . "0x6591") ; <CJK>
       (?槃 . "0x69C3") ; <CJK>
       (?泮 . "0x6CEE") ; <CJK>
       (?潘 . "0x6F58") ; <CJK>
       (?班 . "0x73ED") ; <CJK>
       (?畔 . "0x7554") ; <CJK>
       (?瘢 . "0x7622") ; <CJK>
       (?盤 . "0x76E4") ; <CJK>
       (?盼 . "0x76FC") ; <CJK>
       (?磐 . "0x78D0") ; <CJK>
       (?磻 . "0x78FB") ; <CJK>
       (?礬 . "0x792C") ; <CJK>
       (?絆 . "0x7D46") ; <CJK>
       (?般 . "0x822C") ; <CJK>
       (?蟠 . "0x87E0") ; <CJK>
       (?返 . "0x8FD4") ; <CJK>
       (?頒 . "0x9812") ; <CJK>
       (?飯 . "0x98EF") ; <CJK>
       (?勃 . "0x52C3") ; <CJK>
       (?拔 . "0x62D4") ; <CJK>
       (?撥 . "0x64A5") ; <CJK>
       (?渤 . "0x6E24") ; <CJK>
       (?潑 . "0x6F51") ; <CJK>
       (?發 . "0x767C") ; <CJK>
       (?跋 . "0x8DCB") ; <CJK>
       (?醱 . "0x91B1") ; <CJK>
       (?鉢 . "0x9262") ; <CJK>
       (?髮 . "0x9AEE") ; <CJK>
       (?魃 . "0x9B43") ; <CJK>
       (?倣 . "0x5023") ; <CJK>
       (?傍 . "0x508D") ; <CJK>
       (?坊 . "0x574A") ; <CJK>
       (?妨 . "0x59A8") ; <CJK>
       (?尨 . "0x5C28") ; <CJK>
       (?幇 . "0x5E47") ; <CJK>
       (?彷 . "0x5F77") ; <CJK>
       (?房 . "0x623F") ; <CJK>
       (?放 . "0x653E") ; <CJK>
       (?方 . "0x65B9") ; <CJK>
       (?旁 . "0x65C1") ; <CJK>
       (?昉 . "0x6609") ; <CJK>
       (?枋 . "0x678B") ; <CJK>
       (?榜 . "0x699C") ; <CJK>
       (?滂 . "0x6EC2") ; <CJK>
       (?磅 . "0x78C5") ; <CJK>
       (?紡 . "0x7D21") ; <CJK>
       (?肪 . "0x80AA") ; <CJK>
       (?膀 . "0x8180") ; <CJK>
       (?舫 . "0x822B") ; <CJK>
       (?芳 . "0x82B3") ; <CJK>
       (?蒡 . "0x84A1") ; <CJK>
       (?蚌 . "0x868C") ; <CJK>
       (?訪 . "0x8A2A") ; <CJK>
       (?謗 . "0x8B17") ; <CJK>
       (?邦 . "0x90A6") ; <CJK>
       (?防 . "0x9632") ; <CJK>
       (?龐 . "0x9F90") ; <CJK>
       (?倍 . "0x500D") ; <CJK>
       (?俳 . "0x4FF3") ; <CJK>
       (?北 . "0xF963") ; <CJK>
       (?培 . "0x57F9") ; <CJK>
       (?徘 . "0x5F98") ; <CJK>
       (?拜 . "0x62DC") ; <CJK>
       (?排 . "0x6392") ; <CJK>
       (?杯 . "0x676F") ; <CJK>
       (?湃 . "0x6E43") ; <CJK>
       (?焙 . "0x7119") ; <CJK>
       (?盃 . "0x76C3") ; <CJK>
       (?背 . "0x80CC") ; <CJK>
       (?胚 . "0x80DA") ; <CJK>
       (?裴 . "0x88F4") ; <CJK>
       (?裵 . "0x88F5") ; <CJK>
       (?褙 . "0x8919") ; <CJK>
       (?賠 . "0x8CE0") ; <CJK>
       (?輩 . "0x8F29") ; <CJK>
       (?配 . "0x914D") ; <CJK>
       (?陪 . "0x966A") ; <CJK>
       (?伯 . "0x4F2F") ; <CJK>
       (?佰 . "0x4F70") ; <CJK>
       (?帛 . "0x5E1B") ; <CJK>
       (?柏 . "0x67CF") ; <CJK>
       (?栢 . "0x6822") ; <CJK>
       (?白 . "0x767D") ; <CJK>
       (?百 . "0x767E") ; <CJK>
       (?魄 . "0x9B44") ; <CJK>
       (?幡 . "0x5E61") ; <CJK>
       (?樊 . "0x6A0A") ; <CJK>
       (?煩 . "0x7169") ; <CJK>
       (?燔 . "0x71D4") ; <CJK>
       (?番 . "0x756A") ; <CJK>
       (?磻 . "0xF964") ; <CJK>
       (?繁 . "0x7E41") ; <CJK>
       (?蕃 . "0x8543") ; <CJK>
       (?藩 . "0x85E9") ; <CJK>
       (?飜 . "0x98DC") ; <CJK>
       (?伐 . "0x4F10") ; <CJK>
       (?筏 . "0x7B4F") ; <CJK>
       (?罰 . "0x7F70") ; <CJK>
       (?閥 . "0x95A5") ; <CJK>
       (?凡 . "0x51E1") ; <CJK>
       (?帆 . "0x5E06") ; <CJK>
       (?梵 . "0x68B5") ; <CJK>
       (?氾 . "0x6C3E") ; <CJK>
       (?汎 . "0x6C4E") ; <CJK>
       (?泛 . "0x6CDB") ; <CJK>
       (?犯 . "0x72AF") ; <CJK>
       (?範 . "0x7BC4") ; <CJK>
       (?范 . "0x8303") ; <CJK>
       (?法 . "0x6CD5") ; <CJK>
       (?琺 . "0x743A") ; <CJK>
       (?僻 . "0x50FB") ; <CJK>
       (?劈 . "0x5288") ; <CJK>
       (?壁 . "0x58C1") ; <CJK>
       (?擘 . "0x64D8") ; <CJK>
       (?檗 . "0x6A97") ; <CJK>
       (?璧 . "0x74A7") ; <CJK>
       (?癖 . "0x7656") ; <CJK>
       (?碧 . "0x78A7") ; <CJK>
       (?蘗 . "0x8617") ; <CJK>
       (?闢 . "0x95E2") ; <CJK>
       (?霹 . "0x9739") ; <CJK>
       (?便 . "0xF965") ; <CJK>
       (?卞 . "0x535E") ; <CJK>
       (?弁 . "0x5F01") ; <CJK>
       (?變 . "0x8B8A") ; <CJK>
       (?辨 . "0x8FA8") ; <CJK>
       (?辯 . "0x8FAF") ; <CJK>
       (?邊 . "0x908A") ; <CJK>
       (?別 . "0x5225") ; <CJK>
       (?瞥 . "0x77A5") ; <CJK>
       (?鱉 . "0x9C49") ; <CJK>
       (?鼈 . "0x9F08") ; <CJK>
       (?丙 . "0x4E19") ; <CJK>
       (?倂 . "0x5002") ; <CJK>
       (?兵 . "0x5175") ; <CJK>
       (?屛 . "0x5C5B") ; <CJK>
       (?幷 . "0x5E77") ; <CJK>
       (?昞 . "0x661E") ; <CJK>
       (?昺 . "0x663A") ; <CJK>
       (?柄 . "0x67C4") ; <CJK>
       (?棅 . "0x68C5") ; <CJK>
       (?炳 . "0x70B3") ; <CJK>
       (?甁 . "0x7501") ; <CJK>
       (?病 . "0x75C5") ; <CJK>
       (?秉 . "0x79C9") ; <CJK>
       (?竝 . "0x7ADD") ; <CJK>
       (?輧 . "0x8F27") ; <CJK>
       (?餠 . "0x9920") ; <CJK>
       (?騈 . "0x9A08") ; <CJK>
       (?保 . "0x4FDD") ; <CJK>
       (?堡 . "0x5821") ; <CJK>
       (?報 . "0x5831") ; <CJK>
       (?寶 . "0x5BF6") ; <CJK>
       (?普 . "0x666E") ; <CJK>
       (?步 . "0x6B65") ; <CJK>
       (?洑 . "0x6D11") ; <CJK>
       (?湺 . "0x6E7A") ; <CJK>
       (?潽 . "0x6F7D") ; <CJK>
       (?珤 . "0x73E4") ; <CJK>
       (?甫 . "0x752B") ; <CJK>
       (?菩 . "0x83E9") ; <CJK>
       (?補 . "0x88DC") ; <CJK>
       (?褓 . "0x8913") ; <CJK>
       (?譜 . "0x8B5C") ; <CJK>
       (?輔 . "0x8F14") ; <CJK>
       (?伏 . "0x4F0F") ; <CJK>
       (?僕 . "0x50D5") ; <CJK>
       (?匐 . "0x5310") ; <CJK>
       (?卜 . "0x535C") ; <CJK>
       (?宓 . "0x5B93") ; <CJK>
       (?復 . "0x5FA9") ; <CJK>
       (?服 . "0x670D") ; <CJK>
       (?福 . "0x798F") ; <CJK>
       (?腹 . "0x8179") ; <CJK>
       (?茯 . "0x832F") ; <CJK>
       (?蔔 . "0x8514") ; <CJK>
       (?複 . "0x8907") ; <CJK>
       (?覆 . "0x8986") ; <CJK>
       (?輹 . "0x8F39") ; <CJK>
       (?輻 . "0x8F3B") ; <CJK>
       (?馥 . "0x99A5") ; <CJK>
       (?鰒 . "0x9C12") ; <CJK>
       (?本 . "0x672C") ; <CJK>
       (?乶 . "0x4E76") ; <CJK>
       (?俸 . "0x4FF8") ; <CJK>
       (?奉 . "0x5949") ; <CJK>
       (?封 . "0x5C01") ; <CJK>
       (?峯 . "0x5CEF") ; <CJK>
       (?峰 . "0x5CF0") ; <CJK>
       (?捧 . "0x6367") ; <CJK>
       (?棒 . "0x68D2") ; <CJK>
       (?烽 . "0x70FD") ; <CJK>
       (?熢 . "0x71A2") ; <CJK>
       (?琫 . "0x742B") ; <CJK>
       (?縫 . "0x7E2B") ; <CJK>
       (?蓬 . "0x84EC") ; <CJK>
       (?蜂 . "0x8702") ; <CJK>
       (?逢 . "0x9022") ; <CJK>
       (?鋒 . "0x92D2") ; <CJK>
       (?鳳 . "0x9CF3") ; <CJK>
       (?不 . "0x4E0D") ; <CJK>
       (?付 . "0x4ED8") ; <CJK>
       (?俯 . "0x4FEF") ; <CJK>
       (?傅 . "0x5085") ; <CJK>
       (?剖 . "0x5256") ; <CJK>
       (?副 . "0x526F") ; <CJK>
       (?否 . "0x5426") ; <CJK>
       (?咐 . "0x5490") ; <CJK>
       (?埠 . "0x57E0") ; <CJK>
       (?夫 . "0x592B") ; <CJK>
       (?婦 . "0x5A66") ; <CJK>
       (?孚 . "0x5B5A") ; <CJK>
       (?孵 . "0x5B75") ; <CJK>
       (?富 . "0x5BCC") ; <CJK>
       (?府 . "0x5E9C") ; <CJK>
       (?復 . "0xF966") ; <CJK>
       (?扶 . "0x6276") ; <CJK>
       (?敷 . "0x6577") ; <CJK>
       (?斧 . "0x65A7") ; <CJK>
       (?浮 . "0x6D6E") ; <CJK>
       (?溥 . "0x6EA5") ; <CJK>
       (?父 . "0x7236") ; <CJK>
       (?符 . "0x7B26") ; <CJK>
       (?簿 . "0x7C3F") ; <CJK>
       (?缶 . "0x7F36") ; <CJK>
       (?腐 . "0x8150") ; <CJK>
       (?腑 . "0x8151") ; <CJK>
       (?膚 . "0x819A") ; <CJK>
       (?艀 . "0x8240") ; <CJK>
       (?芙 . "0x8299") ; <CJK>
       (?莩 . "0x83A9") ; <CJK>
       (?訃 . "0x8A03") ; <CJK>
       (?負 . "0x8CA0") ; <CJK>
       (?賦 . "0x8CE6") ; <CJK>
       (?賻 . "0x8CFB") ; <CJK>
       (?赴 . "0x8D74") ; <CJK>
       (?趺 . "0x8DBA") ; <CJK>
       (?部 . "0x90E8") ; <CJK>
       (?釜 . "0x91DC") ; <CJK>
       (?阜 . "0x961C") ; <CJK>
       (?附 . "0x9644") ; <CJK>
       (?駙 . "0x99D9") ; <CJK>
       (?鳧 . "0x9CE7") ; <CJK>
       (?北 . "0x5317") ; <CJK>
       (?分 . "0x5206") ; <CJK>
       (?吩 . "0x5429") ; <CJK>
       (?噴 . "0x5674") ; <CJK>
       (?墳 . "0x58B3") ; <CJK>
       (?奔 . "0x5954") ; <CJK>
       (?奮 . "0x596E") ; <CJK>
       (?忿 . "0x5FFF") ; <CJK>
       (?憤 . "0x61A4") ; <CJK>
       (?扮 . "0x626E") ; <CJK>
       (?昐 . "0x6610") ; <CJK>
       (?汾 . "0x6C7E") ; <CJK>
       (?焚 . "0x711A") ; <CJK>
       (?盆 . "0x76C6") ; <CJK>
       (?粉 . "0x7C89") ; <CJK>
       (?糞 . "0x7CDE") ; <CJK>
       (?紛 . "0x7D1B") ; <CJK>
       (?芬 . "0x82AC") ; <CJK>
       (?賁 . "0x8CC1") ; <CJK>
       (?雰 . "0x96F0") ; <CJK>
       (?不 . "0xF967") ; <CJK>
       (?佛 . "0x4F5B") ; <CJK>
       (?弗 . "0x5F17") ; <CJK>
       (?彿 . "0x5F7F") ; <CJK>
       (?拂 . "0x62C2") ; <CJK>
       (?崩 . "0x5D29") ; <CJK>
       (?朋 . "0x670B") ; <CJK>
       (?棚 . "0x68DA") ; <CJK>
       (?硼 . "0x787C") ; <CJK>
       (?繃 . "0x7E43") ; <CJK>
       (?鵬 . "0x9D6C") ; <CJK>
       (?丕 . "0x4E15") ; <CJK>
       (?備 . "0x5099") ; <CJK>
       (?匕 . "0x5315") ; <CJK>
       (?匪 . "0x532A") ; <CJK>
       (?卑 . "0x5351") ; <CJK>
       (?妃 . "0x5983") ; <CJK>
       (?婢 . "0x5A62") ; <CJK>
       (?庇 . "0x5E87") ; <CJK>
       (?悲 . "0x60B2") ; <CJK>
       (?憊 . "0x618A") ; <CJK>
       (?扉 . "0x6249") ; <CJK>
       (?批 . "0x6279") ; <CJK>
       (?斐 . "0x6590") ; <CJK>
       (?枇 . "0x6787") ; <CJK>
       (?榧 . "0x69A7") ; <CJK>
       (?比 . "0x6BD4") ; <CJK>
       (?毖 . "0x6BD6") ; <CJK>
       (?毗 . "0x6BD7") ; <CJK>
       (?毘 . "0x6BD8") ; <CJK>
       (?沸 . "0x6CB8") ; <CJK>
       (?泌 . "0xF968") ; <CJK>
       (?琵 . "0x7435") ; <CJK>
       (?痺 . "0x75FA") ; <CJK>
       (?砒 . "0x7812") ; <CJK>
       (?碑 . "0x7891") ; <CJK>
       (?秕 . "0x79D5") ; <CJK>
       (?秘 . "0x79D8") ; <CJK>
       (?粃 . "0x7C83") ; <CJK>
       (?緋 . "0x7DCB") ; <CJK>
       (?翡 . "0x7FE1") ; <CJK>
       (?肥 . "0x80A5") ; <CJK>
       (?脾 . "0x813E") ; <CJK>
       (?臂 . "0x81C2") ; <CJK>
       (?菲 . "0x83F2") ; <CJK>
       (?蜚 . "0x871A") ; <CJK>
       (?裨 . "0x88E8") ; <CJK>
       (?誹 . "0x8AB9") ; <CJK>
       (?譬 . "0x8B6C") ; <CJK>
       (?費 . "0x8CBB") ; <CJK>
       (?鄙 . "0x9119") ; <CJK>
       (?非 . "0x975E") ; <CJK>
       (?飛 . "0x98DB") ; <CJK>
       (?鼻 . "0x9F3B") ; <CJK>
       (?嚬 . "0x56AC") ; <CJK>
       (?嬪 . "0x5B2A") ; <CJK>
       (?彬 . "0x5F6C") ; <CJK>
       (?斌 . "0x658C") ; <CJK>
       (?檳 . "0x6AB3") ; <CJK>
       (?殯 . "0x6BAF") ; <CJK>
       (?浜 . "0x6D5C") ; <CJK>
       (?濱 . "0x6FF1") ; <CJK>
       (?瀕 . "0x7015") ; <CJK>
       (?牝 . "0x725D") ; <CJK>
       (?玭 . "0x73AD") ; <CJK>
       (?貧 . "0x8CA7") ; <CJK>
       (?賓 . "0x8CD3") ; <CJK>
       (?頻 . "0x983B") ; <CJK>
       (?憑 . "0x6191") ; <CJK>
       (?氷 . "0x6C37") ; <CJK>
       (?聘 . "0x8058") ; <CJK>
       (?騁 . "0x9A01") ; <CJK>
       (?乍 . "0x4E4D") ; <CJK>
       (?事 . "0x4E8B") ; <CJK>
       (?些 . "0x4E9B") ; <CJK>
       (?仕 . "0x4ED5") ; <CJK>
       (?伺 . "0x4F3A") ; <CJK>
       (?似 . "0x4F3C") ; <CJK>
       (?使 . "0x4F7F") ; <CJK>
       (?俟 . "0x4FDF") ; <CJK>
       (?僿 . "0x50FF") ; <CJK>
       (?史 . "0x53F2") ; <CJK>
       (?司 . "0x53F8") ; <CJK>
       (?唆 . "0x5506") ; <CJK>
       (?嗣 . "0x55E3") ; <CJK>
       (?四 . "0x56DB") ; <CJK>
       (?士 . "0x58EB") ; <CJK>
       (?奢 . "0x5962") ; <CJK>
       (?娑 . "0x5A11") ; <CJK>
       (?寫 . "0x5BEB") ; <CJK>
       (?寺 . "0x5BFA") ; <CJK>
       (?射 . "0x5C04") ; <CJK>
       (?巳 . "0x5DF3") ; <CJK>
       (?師 . "0x5E2B") ; <CJK>
       (?徙 . "0x5F99") ; <CJK>
       (?思 . "0x601D") ; <CJK>
       (?捨 . "0x6368") ; <CJK>
       (?斜 . "0x659C") ; <CJK>
       (?斯 . "0x65AF") ; <CJK>
       (?柶 . "0x67F6") ; <CJK>
       (?査 . "0x67FB") ; <CJK>
       (?梭 . "0x68AD") ; <CJK>
       (?死 . "0x6B7B") ; <CJK>
       (?沙 . "0x6C99") ; <CJK>
       (?泗 . "0x6CD7") ; <CJK>
       (?渣 . "0x6E23") ; <CJK>
       (?瀉 . "0x7009") ; <CJK>
       (?獅 . "0x7345") ; <CJK>
       (?砂 . "0x7802") ; <CJK>
       (?社 . "0x793E") ; <CJK>
       (?祀 . "0x7940") ; <CJK>
       (?祠 . "0x7960") ; <CJK>
       (?私 . "0x79C1") ; <CJK>
       (?篩 . "0x7BE9") ; <CJK>
       (?紗 . "0x7D17") ; <CJK>
       (?絲 . "0x7D72") ; <CJK>
       (?肆 . "0x8086") ; <CJK>
       (?舍 . "0x820D") ; <CJK>
       (?莎 . "0x838E") ; <CJK>
       (?蓑 . "0x84D1") ; <CJK>
       (?蛇 . "0x86C7") ; <CJK>
       (?裟 . "0x88DF") ; <CJK>
       (?詐 . "0x8A50") ; <CJK>
       (?詞 . "0x8A5E") ; <CJK>
       (?謝 . "0x8B1D") ; <CJK>
       (?賜 . "0x8CDC") ; <CJK>
       (?赦 . "0x8D66") ; <CJK>
       (?辭 . "0x8FAD") ; <CJK>
       (?邪 . "0x90AA") ; <CJK>
       (?飼 . "0x98FC") ; <CJK>
       (?駟 . "0x99DF") ; <CJK>
       (?麝 . "0x9E9D") ; <CJK>
       (?削 . "0x524A") ; <CJK>
       (?數 . "0xF969") ; <CJK>
       (?朔 . "0x6714") ; <CJK>
       (?索 . "0xF96A") ; <CJK>
       (?傘 . "0x5098") ; <CJK>
       (?刪 . "0x522A") ; <CJK>
       (?山 . "0x5C71") ; <CJK>
       (?散 . "0x6563") ; <CJK>
       (?汕 . "0x6C55") ; <CJK>
       (?珊 . "0x73CA") ; <CJK>
       (?産 . "0x7523") ; <CJK>
       (?疝 . "0x759D") ; <CJK>
       (?算 . "0x7B97") ; <CJK>
       (?蒜 . "0x849C") ; <CJK>
       (?酸 . "0x9178") ; <CJK>
       (?霰 . "0x9730") ; <CJK>
       (?乷 . "0x4E77") ; <CJK>
       (?撒 . "0x6492") ; <CJK>
       (?殺 . "0x6BBA") ; <CJK>
       (?煞 . "0x715E") ; <CJK>
       (?薩 . "0x85A9") ; <CJK>
       (?三 . "0x4E09") ; <CJK>
       (?參 . "0xF96B") ; <CJK>
       (?杉 . "0x6749") ; <CJK>
       (?森 . "0x68EE") ; <CJK>
       (?渗 . "0x6E17") ; <CJK>
       (?芟 . "0x829F") ; <CJK>
       (?蔘 . "0x8518") ; <CJK>
       (?衫 . "0x886B") ; <CJK>
       (?揷 . "0x63F7") ; <CJK>
       (?澁 . "0x6F81") ; <CJK>
       (?鈒 . "0x9212") ; <CJK>
       (?颯 . "0x98AF") ; <CJK>
       (?上 . "0x4E0A") ; <CJK>
       (?傷 . "0x50B7") ; <CJK>
       (?像 . "0x50CF") ; <CJK>
       (?償 . "0x511F") ; <CJK>
       (?商 . "0x5546") ; <CJK>
       (?喪 . "0x55AA") ; <CJK>
       (?嘗 . "0x5617") ; <CJK>
       (?孀 . "0x5B40") ; <CJK>
       (?尙 . "0x5C19") ; <CJK>
       (?峠 . "0x5CE0") ; <CJK>
       (?常 . "0x5E38") ; <CJK>
       (?床 . "0x5E8A") ; <CJK>
       (?庠 . "0x5EA0") ; <CJK>
       (?廂 . "0x5EC2") ; <CJK>
       (?想 . "0x60F3") ; <CJK>
       (?桑 . "0x6851") ; <CJK>
       (?橡 . "0x6A61") ; <CJK>
       (?湘 . "0x6E58") ; <CJK>
       (?爽 . "0x723D") ; <CJK>
       (?牀 . "0x7240") ; <CJK>
       (?狀 . "0x72C0") ; <CJK>
       (?相 . "0x76F8") ; <CJK>
       (?祥 . "0x7965") ; <CJK>
       (?箱 . "0x7BB1") ; <CJK>
       (?翔 . "0x7FD4") ; <CJK>
       (?裳 . "0x88F3") ; <CJK>
       (?觴 . "0x89F4") ; <CJK>
       (?詳 . "0x8A73") ; <CJK>
       (?象 . "0x8C61") ; <CJK>
       (?賞 . "0x8CDE") ; <CJK>
       (?霜 . "0x971C") ; <CJK>
       (?塞 . "0x585E") ; <CJK>
       (?璽 . "0x74BD") ; <CJK>
       (?賽 . "0x8CFD") ; <CJK>
       (?嗇 . "0x55C7") ; <CJK>
       (?塞 . "0xF96C") ; <CJK>
       (?穡 . "0x7A61") ; <CJK>
       (?索 . "0x7D22") ; <CJK>
       (?色 . "0x8272") ; <CJK>
       (?牲 . "0x7272") ; <CJK>
       (?生 . "0x751F") ; <CJK>
       (?甥 . "0x7525") ; <CJK>
       (?省 . "0xF96D") ; <CJK>
       (?笙 . "0x7B19") ; <CJK>
       (?墅 . "0x5885") ; <CJK>
       (?壻 . "0x58FB") ; <CJK>
       (?嶼 . "0x5DBC") ; <CJK>
       (?序 . "0x5E8F") ; <CJK>
       (?庶 . "0x5EB6") ; <CJK>
       (?徐 . "0x5F90") ; <CJK>
       (?恕 . "0x6055") ; <CJK>
       (?抒 . "0x6292") ; <CJK>
       (?捿 . "0x637F") ; <CJK>
       (?敍 . "0x654D") ; <CJK>
       (?暑 . "0x6691") ; <CJK>
       (?曙 . "0x66D9") ; <CJK>
       (?書 . "0x66F8") ; <CJK>
       (?栖 . "0x6816") ; <CJK>
       (?棲 . "0x68F2") ; <CJK>
       (?犀 . "0x7280") ; <CJK>
       (?瑞 . "0x745E") ; <CJK>
       (?筮 . "0x7B6E") ; <CJK>
       (?絮 . "0x7D6E") ; <CJK>
       (?緖 . "0x7DD6") ; <CJK>
       (?署 . "0x7F72") ; <CJK>
       (?胥 . "0x80E5") ; <CJK>
       (?舒 . "0x8212") ; <CJK>
       (?薯 . "0x85AF") ; <CJK>
       (?西 . "0x897F") ; <CJK>
       (?誓 . "0x8A93") ; <CJK>
       (?逝 . "0x901D") ; <CJK>
       (?鋤 . "0x92E4") ; <CJK>
       (?黍 . "0x9ECD") ; <CJK>
       (?鼠 . "0x9F20") ; <CJK>
       (?夕 . "0x5915") ; <CJK>
       (?奭 . "0x596D") ; <CJK>
       (?席 . "0x5E2D") ; <CJK>
       (?惜 . "0x60DC") ; <CJK>
       (?昔 . "0x6614") ; <CJK>
       (?晳 . "0x6673") ; <CJK>
       (?析 . "0x6790") ; <CJK>
       (?汐 . "0x6C50") ; <CJK>
       (?淅 . "0x6DC5") ; <CJK>
       (?潟 . "0x6F5F") ; <CJK>
       (?石 . "0x77F3") ; <CJK>
       (?碩 . "0x78A9") ; <CJK>
       (?蓆 . "0x84C6") ; <CJK>
       (?釋 . "0x91CB") ; <CJK>
       (?錫 . "0x932B") ; <CJK>
       (?仙 . "0x4ED9") ; <CJK>
       (?僊 . "0x50CA") ; <CJK>
       (?先 . "0x5148") ; <CJK>
       (?善 . "0x5584") ; <CJK>
       (?嬋 . "0x5B0B") ; <CJK>
       (?宣 . "0x5BA3") ; <CJK>
       (?扇 . "0x6247") ; <CJK>
       (?敾 . "0x657E") ; <CJK>
       (?旋 . "0x65CB") ; <CJK>
       (?渲 . "0x6E32") ; <CJK>
       (?煽 . "0x717D") ; <CJK>
       (?琁 . "0x7401") ; <CJK>
       (?瑄 . "0x7444") ; <CJK>
       (?璇 . "0x7487") ; <CJK>
       (?璿 . "0x74BF") ; <CJK>
       (?癬 . "0x766C") ; <CJK>
       (?禪 . "0x79AA") ; <CJK>
       (?線 . "0x7DDA") ; <CJK>
       (?繕 . "0x7E55") ; <CJK>
       (?羨 . "0x7FA8") ; <CJK>
       (?腺 . "0x817A") ; <CJK>
       (?膳 . "0x81B3") ; <CJK>
       (?船 . "0x8239") ; <CJK>
       (?蘚 . "0x861A") ; <CJK>
       (?蟬 . "0x87EC") ; <CJK>
       (?詵 . "0x8A75") ; <CJK>
       (?跣 . "0x8DE3") ; <CJK>
       (?選 . "0x9078") ; <CJK>
       (?銑 . "0x9291") ; <CJK>
       (?鐥 . "0x9425") ; <CJK>
       (?饍 . "0x994D") ; <CJK>
       (?鮮 . "0x9BAE") ; <CJK>
       (?卨 . "0x5368") ; <CJK>
       (?屑 . "0x5C51") ; <CJK>
       (?楔 . "0x6954") ; <CJK>
       (?泄 . "0x6CC4") ; <CJK>
       (?洩 . "0x6D29") ; <CJK>
       (?渫 . "0x6E2B") ; <CJK>
       (?舌 . "0x820C") ; <CJK>
       (?薛 . "0x859B") ; <CJK>
       (?褻 . "0x893B") ; <CJK>
       (?設 . "0x8A2D") ; <CJK>
       (?說 . "0x8AAA") ; <CJK>
       (?雪 . "0x96EA") ; <CJK>
       (?齧 . "0x9F67") ; <CJK>
       (?剡 . "0x5261") ; <CJK>
       (?暹 . "0x66B9") ; <CJK>
       (?殲 . "0x6BB2") ; <CJK>
       (?纖 . "0x7E96") ; <CJK>
       (?蟾 . "0x87FE") ; <CJK>
       (?贍 . "0x8D0D") ; <CJK>
       (?閃 . "0x9583") ; <CJK>
       (?陝 . "0x965D") ; <CJK>
       (?攝 . "0x651D") ; <CJK>
       (?涉 . "0x6D89") ; <CJK>
       (?燮 . "0x71EE") ; <CJK>
       (?葉 . "0xF96E") ; <CJK>
       (?城 . "0x57CE") ; <CJK>
       (?姓 . "0x59D3") ; <CJK>
       (?宬 . "0x5BAC") ; <CJK>
       (?性 . "0x6027") ; <CJK>
       (?惺 . "0x60FA") ; <CJK>
       (?成 . "0x6210") ; <CJK>
       (?星 . "0x661F") ; <CJK>
       (?晟 . "0x665F") ; <CJK>
       (?猩 . "0x7329") ; <CJK>
       (?珹 . "0x73F9") ; <CJK>
       (?盛 . "0x76DB") ; <CJK>
       (?省 . "0x7701") ; <CJK>
       (?筬 . "0x7B6C") ; <CJK>
       (?聖 . "0x8056") ; <CJK>
       (?聲 . "0x8072") ; <CJK>
       (?腥 . "0x8165") ; <CJK>
       (?誠 . "0x8AA0") ; <CJK>
       (?醒 . "0x9192") ; <CJK>
       (?世 . "0x4E16") ; <CJK>
       (?勢 . "0x52E2") ; <CJK>
       (?歲 . "0x6B72") ; <CJK>
       (?洗 . "0x6D17") ; <CJK>
       (?稅 . "0x7A05") ; <CJK>
       (?笹 . "0x7B39") ; <CJK>
       (?細 . "0x7D30") ; <CJK>
       (?說 . "0xF96F") ; <CJK>
       (?貰 . "0x8CB0") ; <CJK>
       (?召 . "0x53EC") ; <CJK>
       (?嘯 . "0x562F") ; <CJK>
       (?塑 . "0x5851") ; <CJK>
       (?宵 . "0x5BB5") ; <CJK>
       (?小 . "0x5C0F") ; <CJK>
       (?少 . "0x5C11") ; <CJK>
       (?巢 . "0x5DE2") ; <CJK>
       (?所 . "0x6240") ; <CJK>
       (?掃 . "0x6383") ; <CJK>
       (?搔 . "0x6414") ; <CJK>
       (?昭 . "0x662D") ; <CJK>
       (?梳 . "0x68B3") ; <CJK>
       (?沼 . "0x6CBC") ; <CJK>
       (?消 . "0x6D88") ; <CJK>
       (?溯 . "0x6EAF") ; <CJK>
       (?瀟 . "0x701F") ; <CJK>
       (?炤 . "0x70A4") ; <CJK>
       (?燒 . "0x71D2") ; <CJK>
       (?甦 . "0x7526") ; <CJK>
       (?疏 . "0x758F") ; <CJK>
       (?疎 . "0x758E") ; <CJK>
       (?瘙 . "0x7619") ; <CJK>
       (?笑 . "0x7B11") ; <CJK>
       (?篠 . "0x7BE0") ; <CJK>
       (?簫 . "0x7C2B") ; <CJK>
       (?素 . "0x7D20") ; <CJK>
       (?紹 . "0x7D39") ; <CJK>
       (?蔬 . "0x852C") ; <CJK>
       (?蕭 . "0x856D") ; <CJK>
       (?蘇 . "0x8607") ; <CJK>
       (?訴 . "0x8A34") ; <CJK>
       (?逍 . "0x900D") ; <CJK>
       (?遡 . "0x9061") ; <CJK>
       (?邵 . "0x90B5") ; <CJK>
       (?銷 . "0x92B7") ; <CJK>
       (?韶 . "0x97F6") ; <CJK>
       (?騷 . "0x9A37") ; <CJK>
       (?俗 . "0x4FD7") ; <CJK>
       (?屬 . "0x5C6C") ; <CJK>
       (?束 . "0x675F") ; <CJK>
       (?涑 . "0x6D91") ; <CJK>
       (?粟 . "0x7C9F") ; <CJK>
       (?續 . "0x7E8C") ; <CJK>
       (?謖 . "0x8B16") ; <CJK>
       (?贖 . "0x8D16") ; <CJK>
       (?速 . "0x901F") ; <CJK>
       (?孫 . "0x5B6B") ; <CJK>
       (?巽 . "0x5DFD") ; <CJK>
       (?損 . "0x640D") ; <CJK>
       (?蓀 . "0x84C0") ; <CJK>
       (?遜 . "0x905C") ; <CJK>
       (?飡 . "0x98E1") ; <CJK>
       (?率 . "0x7387") ; <CJK>
       (?宋 . "0x5B8B") ; <CJK>
       (?悚 . "0x609A") ; <CJK>
       (?松 . "0x677E") ; <CJK>
       (?淞 . "0x6DDE") ; <CJK>
       (?訟 . "0x8A1F") ; <CJK>
       (?誦 . "0x8AA6") ; <CJK>
       (?送 . "0x9001") ; <CJK>
       (?頌 . "0x980C") ; <CJK>
       (?刷 . "0x5237") ; <CJK>
       (?殺 . "0xF970") ; <CJK>
       (?灑 . "0x7051") ; <CJK>
       (?碎 . "0x788E") ; <CJK>
       (?鎖 . "0x9396") ; <CJK>
       (?衰 . "0x8870") ; <CJK>
       (?釗 . "0x91D7") ; <CJK>
       (?修 . "0x4FEE") ; <CJK>
       (?受 . "0x53D7") ; <CJK>
       (?嗽 . "0x55FD") ; <CJK>
       (?囚 . "0x56DA") ; <CJK>
       (?垂 . "0x5782") ; <CJK>
       (?壽 . "0x58FD") ; <CJK>
       (?嫂 . "0x5AC2") ; <CJK>
       (?守 . "0x5B88") ; <CJK>
       (?岫 . "0x5CAB") ; <CJK>
       (?峀 . "0x5CC0") ; <CJK>
       (?帥 . "0x5E25") ; <CJK>
       (?愁 . "0x6101") ; <CJK>
       (?戍 . "0x620D") ; <CJK>
       (?手 . "0x624B") ; <CJK>
       (?授 . "0x6388") ; <CJK>
       (?搜 . "0x641C") ; <CJK>
       (?收 . "0x6536") ; <CJK>
       (?數 . "0x6578") ; <CJK>
       (?樹 . "0x6A39") ; <CJK>
       (?殊 . "0x6B8A") ; <CJK>
       (?水 . "0x6C34") ; <CJK>
       (?洙 . "0x6D19") ; <CJK>
       (?漱 . "0x6F31") ; <CJK>
       (?燧 . "0x71E7") ; <CJK>
       (?狩 . "0x72E9") ; <CJK>
       (?獸 . "0x7378") ; <CJK>
       (?琇 . "0x7407") ; <CJK>
       (?璲 . "0x74B2") ; <CJK>
       (?瘦 . "0x7626") ; <CJK>
       (?睡 . "0x7761") ; <CJK>
       (?秀 . "0x79C0") ; <CJK>
       (?穗 . "0x7A57") ; <CJK>
       (?竪 . "0x7AEA") ; <CJK>
       (?粹 . "0x7CB9") ; <CJK>
       (?綏 . "0x7D8F") ; <CJK>
       (?綬 . "0x7DAC") ; <CJK>
       (?繡 . "0x7E61") ; <CJK>
       (?羞 . "0x7F9E") ; <CJK>
       (?脩 . "0x8129") ; <CJK>
       (?茱 . "0x8331") ; <CJK>
       (?蒐 . "0x8490") ; <CJK>
       (?蓚 . "0x84DA") ; <CJK>
       (?藪 . "0x85EA") ; <CJK>
       (?袖 . "0x8896") ; <CJK>
       (?誰 . "0x8AB0") ; <CJK>
       (?讐 . "0x8B90") ; <CJK>
       (?輸 . "0x8F38") ; <CJK>
       (?遂 . "0x9042") ; <CJK>
       (?邃 . "0x9083") ; <CJK>
       (?酬 . "0x916C") ; <CJK>
       (?銖 . "0x9296") ; <CJK>
       (?銹 . "0x92B9") ; <CJK>
       (?隋 . "0x968B") ; <CJK>
       (?隧 . "0x96A7") ; <CJK>
       (?隨 . "0x96A8") ; <CJK>
       (?雖 . "0x96D6") ; <CJK>
       (?需 . "0x9700") ; <CJK>
       (?須 . "0x9808") ; <CJK>
       (?首 . "0x9996") ; <CJK>
       (?髓 . "0x9AD3") ; <CJK>
       (?鬚 . "0x9B1A") ; <CJK>
       (?叔 . "0x53D4") ; <CJK>
       (?塾 . "0x587E") ; <CJK>
       (?夙 . "0x5919") ; <CJK>
       (?孰 . "0x5B70") ; <CJK>
       (?宿 . "0x5BBF") ; <CJK>
       (?淑 . "0x6DD1") ; <CJK>
       (?潚 . "0x6F5A") ; <CJK>
       (?熟 . "0x719F") ; <CJK>
       (?琡 . "0x7421") ; <CJK>
       (?璹 . "0x74B9") ; <CJK>
       (?肅 . "0x8085") ; <CJK>
       (?菽 . "0x83FD") ; <CJK>
       (?巡 . "0x5DE1") ; <CJK>
       (?徇 . "0x5F87") ; <CJK>
       (?循 . "0x5FAA") ; <CJK>
       (?恂 . "0x6042") ; <CJK>
       (?旬 . "0x65EC") ; <CJK>
       (?栒 . "0x6812") ; <CJK>
       (?楯 . "0x696F") ; <CJK>
       (?橓 . "0x6A53") ; <CJK>
       (?殉 . "0x6B89") ; <CJK>
       (?洵 . "0x6D35") ; <CJK>
       (?淳 . "0x6DF3") ; <CJK>
       (?珣 . "0x73E3") ; <CJK>
       (?盾 . "0x76FE") ; <CJK>
       (?瞬 . "0x77AC") ; <CJK>
       (?筍 . "0x7B4D") ; <CJK>
       (?純 . "0x7D14") ; <CJK>
       (?脣 . "0x8123") ; <CJK>
       (?舜 . "0x821C") ; <CJK>
       (?荀 . "0x8340") ; <CJK>
       (?蓴 . "0x84F4") ; <CJK>
       (?蕣 . "0x8563") ; <CJK>
       (?詢 . "0x8A62") ; <CJK>
       (?諄 . "0x8AC4") ; <CJK>
       (?醇 . "0x9187") ; <CJK>
       (?錞 . "0x931E") ; <CJK>
       (?順 . "0x9806") ; <CJK>
       (?馴 . "0x99B4") ; <CJK>
       (?戌 . "0x620C") ; <CJK>
       (?術 . "0x8853") ; <CJK>
       (?述 . "0x8FF0") ; <CJK>
       (?鉥 . "0x9265") ; <CJK>
       (?崇 . "0x5D07") ; <CJK>
       (?崧 . "0x5D27") ; <CJK>
       (?嵩 . "0x5D69") ; <CJK>
       (?瑟 . "0x745F") ; <CJK>
       (?膝 . "0x819D") ; <CJK>
       (?蝨 . "0x8768") ; <CJK>
       (?濕 . "0x6FD5") ; <CJK>
       (?拾 . "0x62FE") ; <CJK>
       (?習 . "0x7FD2") ; <CJK>
       (?褶 . "0x8936") ; <CJK>
       (?襲 . "0x8972") ; <CJK>
       (?丞 . "0x4E1E") ; <CJK>
       (?乘 . "0x4E58") ; <CJK>
       (?僧 . "0x50E7") ; <CJK>
       (?勝 . "0x52DD") ; <CJK>
       (?升 . "0x5347") ; <CJK>
       (?承 . "0x627F") ; <CJK>
       (?昇 . "0x6607") ; <CJK>
       (?繩 . "0x7E69") ; <CJK>
       (?蠅 . "0x8805") ; <CJK>
       (?陞 . "0x965E") ; <CJK>
       (?侍 . "0x4F8D") ; <CJK>
       (?匙 . "0x5319") ; <CJK>
       (?嘶 . "0x5636") ; <CJK>
       (?始 . "0x59CB") ; <CJK>
       (?媤 . "0x5AA4") ; <CJK>
       (?尸 . "0x5C38") ; <CJK>
       (?屎 . "0x5C4E") ; <CJK>
       (?屍 . "0x5C4D") ; <CJK>
       (?市 . "0x5E02") ; <CJK>
       (?弑 . "0x5F11") ; <CJK>
       (?恃 . "0x6043") ; <CJK>
       (?施 . "0x65BD") ; <CJK>
       (?是 . "0x662F") ; <CJK>
       (?時 . "0x6642") ; <CJK>
       (?枾 . "0x67BE") ; <CJK>
       (?柴 . "0x67F4") ; <CJK>
       (?猜 . "0x731C") ; <CJK>
       (?矢 . "0x77E2") ; <CJK>
       (?示 . "0x793A") ; <CJK>
       (?翅 . "0x7FC5") ; <CJK>
       (?蒔 . "0x8494") ; <CJK>
       (?蓍 . "0x84CD") ; <CJK>
       (?視 . "0x8996") ; <CJK>
       (?試 . "0x8A66") ; <CJK>
       (?詩 . "0x8A69") ; <CJK>
       (?諡 . "0x8AE1") ; <CJK>
       (?豕 . "0x8C55") ; <CJK>
       (?豺 . "0x8C7A") ; <CJK>
       (?埴 . "0x57F4") ; <CJK>
       (?寔 . "0x5BD4") ; <CJK>
       (?式 . "0x5F0F") ; <CJK>
       (?息 . "0x606F") ; <CJK>
       (?拭 . "0x62ED") ; <CJK>
       (?植 . "0x690D") ; <CJK>
       (?殖 . "0x6B96") ; <CJK>
       (?湜 . "0x6E5C") ; <CJK>
       (?熄 . "0x7184") ; <CJK>
       (?篒 . "0x7BD2") ; <CJK>
       (?蝕 . "0x8755") ; <CJK>
       (?識 . "0x8B58") ; <CJK>
       (?軾 . "0x8EFE") ; <CJK>
       (?食 . "0x98DF") ; <CJK>
       (?飾 . "0x98FE") ; <CJK>
       (?伸 . "0x4F38") ; <CJK>
       (?侁 . "0x4F81") ; <CJK>
       (?信 . "0x4FE1") ; <CJK>
       (?呻 . "0x547B") ; <CJK>
       (?娠 . "0x5A20") ; <CJK>
       (?宸 . "0x5BB8") ; <CJK>
       (?愼 . "0x613C") ; <CJK>
       (?新 . "0x65B0") ; <CJK>
       (?晨 . "0x6668") ; <CJK>
       (?燼 . "0x71FC") ; <CJK>
       (?申 . "0x7533") ; <CJK>
       (?神 . "0x795E") ; <CJK>
       (?紳 . "0x7D33") ; <CJK>
       (?腎 . "0x814E") ; <CJK>
       (?臣 . "0x81E3") ; <CJK>
       (?莘 . "0x8398") ; <CJK>
       (?薪 . "0x85AA") ; <CJK>
       (?藎 . "0x85CE") ; <CJK>
       (?蜃 . "0x8703") ; <CJK>
       (?訊 . "0x8A0A") ; <CJK>
       (?身 . "0x8EAB") ; <CJK>
       (?辛 . "0x8F9B") ; <CJK>
       (?辰 . "0xF971") ; <CJK>
       (?迅 . "0x8FC5") ; <CJK>
       (?失 . "0x5931") ; <CJK>
       (?室 . "0x5BA4") ; <CJK>
       (?實 . "0x5BE6") ; <CJK>
       (?悉 . "0x6089") ; <CJK>
       (?審 . "0x5BE9") ; <CJK>
       (?尋 . "0x5C0B") ; <CJK>
       (?心 . "0x5FC3") ; <CJK>
       (?沁 . "0x6C81") ; <CJK>
       (?沈 . "0xF972") ; <CJK>
       (?深 . "0x6DF1") ; <CJK>
       (?瀋 . "0x700B") ; <CJK>
       (?甚 . "0x751A") ; <CJK>
       (?芯 . "0x82AF") ; <CJK>
       (?諶 . "0x8AF6") ; <CJK>
       (?什 . "0x4EC0") ; <CJK>
       (?十 . "0x5341") ; <CJK>
       (?拾 . "0xF973") ; <CJK>
       (?雙 . "0x96D9") ; <CJK>
       (?氏 . "0x6C0F") ; <CJK>
       (?亞 . "0x4E9E") ; <CJK>
       (?俄 . "0x4FC4") ; <CJK>
       (?兒 . "0x5152") ; <CJK>
       (?啞 . "0x555E") ; <CJK>
       (?娥 . "0x5A25") ; <CJK>
       (?峨 . "0x5CE8") ; <CJK>
       (?我 . "0x6211") ; <CJK>
       (?牙 . "0x7259") ; <CJK>
       (?芽 . "0x82BD") ; <CJK>
       (?莪 . "0x83AA") ; <CJK>
       (?蛾 . "0x86FE") ; <CJK>
       (?衙 . "0x8859") ; <CJK>
       (?訝 . "0x8A1D") ; <CJK>
       (?阿 . "0x963F") ; <CJK>
       (?雅 . "0x96C5") ; <CJK>
       (?餓 . "0x9913") ; <CJK>
       (?鴉 . "0x9D09") ; <CJK>
       (?鵝 . "0x9D5D") ; <CJK>
       (?堊 . "0x580A") ; <CJK>
       (?岳 . "0x5CB3") ; <CJK>
       (?嶽 . "0x5DBD") ; <CJK>
       (?幄 . "0x5E44") ; <CJK>
       (?惡 . "0x60E1") ; <CJK>
       (?愕 . "0x6115") ; <CJK>
       (?握 . "0x63E1") ; <CJK>
       (?樂 . "0x6A02") ; <CJK>
       (?渥 . "0x6E25") ; <CJK>
       (?鄂 . "0x9102") ; <CJK>
       (?鍔 . "0x9354") ; <CJK>
       (?顎 . "0x984E") ; <CJK>
       (?鰐 . "0x9C10") ; <CJK>
       (?齷 . "0x9F77") ; <CJK>
       (?安 . "0x5B89") ; <CJK>
       (?岸 . "0x5CB8") ; <CJK>
       (?按 . "0x6309") ; <CJK>
       (?晏 . "0x664F") ; <CJK>
       (?案 . "0x6848") ; <CJK>
       (?眼 . "0x773C") ; <CJK>
       (?雁 . "0x96C1") ; <CJK>
       (?鞍 . "0x978D") ; <CJK>
       (?顔 . "0x9854") ; <CJK>
       (?鮟 . "0x9B9F") ; <CJK>
       (?斡 . "0x65A1") ; <CJK>
       (?謁 . "0x8B01") ; <CJK>
       (?軋 . "0x8ECB") ; <CJK>
       (?閼 . "0x95BC") ; <CJK>
       (?唵 . "0x5535") ; <CJK>
       (?岩 . "0x5CA9") ; <CJK>
       (?巖 . "0x5DD6") ; <CJK>
       (?庵 . "0x5EB5") ; <CJK>
       (?暗 . "0x6697") ; <CJK>
       (?癌 . "0x764C") ; <CJK>
       (?菴 . "0x83F4") ; <CJK>
       (?闇 . "0x95C7") ; <CJK>
       (?壓 . "0x58D3") ; <CJK>
       (?押 . "0x62BC") ; <CJK>
       (?狎 . "0x72CE") ; <CJK>
       (?鴨 . "0x9D28") ; <CJK>
       (?仰 . "0x4EF0") ; <CJK>
       (?央 . "0x592E") ; <CJK>
       (?怏 . "0x600F") ; <CJK>
       (?昻 . "0x663B") ; <CJK>
       (?殃 . "0x6B83") ; <CJK>
       (?秧 . "0x79E7") ; <CJK>
       (?鴦 . "0x9D26") ; <CJK>
       (?厓 . "0x5393") ; <CJK>
       (?哀 . "0x54C0") ; <CJK>
       (?埃 . "0x57C3") ; <CJK>
       (?崖 . "0x5D16") ; <CJK>
       (?愛 . "0x611B") ; <CJK>
       (?曖 . "0x66D6") ; <CJK>
       (?涯 . "0x6DAF") ; <CJK>
       (?碍 . "0x788D") ; <CJK>
       (?艾 . "0x827E") ; <CJK>
       (?隘 . "0x9698") ; <CJK>
       (?靄 . "0x9744") ; <CJK>
       (?厄 . "0x5384") ; <CJK>
       (?扼 . "0x627C") ; <CJK>
       (?掖 . "0x6396") ; <CJK>
       (?液 . "0x6DB2") ; <CJK>
       (?縊 . "0x7E0A") ; <CJK>
       (?腋 . "0x814B") ; <CJK>
       (?額 . "0x984D") ; <CJK>
       (?櫻 . "0x6AFB") ; <CJK>
       (?罌 . "0x7F4C") ; <CJK>
       (?鶯 . "0x9DAF") ; <CJK>
       (?鸚 . "0x9E1A") ; <CJK>
       (?也 . "0x4E5F") ; <CJK>
       (?倻 . "0x503B") ; <CJK>
       (?冶 . "0x51B6") ; <CJK>
       (?夜 . "0x591C") ; <CJK>
       (?惹 . "0x60F9") ; <CJK>
       (?揶 . "0x63F6") ; <CJK>
       (?椰 . "0x6930") ; <CJK>
       (?爺 . "0x723A") ; <CJK>
       (?耶 . "0x8036") ; <CJK>
       (?若 . "0xF974") ; <CJK>
       (?野 . "0x91CE") ; <CJK>
       (?弱 . "0x5F31") ; <CJK>
       (?掠 . "0xF975") ; <CJK>
       (?略 . "0xF976") ; <CJK>
       (?約 . "0x7D04") ; <CJK>
       (?若 . "0x82E5") ; <CJK>
       (?葯 . "0x846F") ; <CJK>
       (?蒻 . "0x84BB") ; <CJK>
       (?藥 . "0x85E5") ; <CJK>
       (?躍 . "0x8E8D") ; <CJK>
       (?亮 . "0xF977") ; <CJK>
       (?佯 . "0x4F6F") ; <CJK>
       (?兩 . "0xF978") ; <CJK>
       (?凉 . "0xF979") ; <CJK>
       (?壤 . "0x58E4") ; <CJK>
       (?孃 . "0x5B43") ; <CJK>
       (?恙 . "0x6059") ; <CJK>
       (?揚 . "0x63DA") ; <CJK>
       (?攘 . "0x6518") ; <CJK>
       (?敭 . "0x656D") ; <CJK>
       (?暘 . "0x6698") ; <CJK>
       (?梁 . "0xF97A") ; <CJK>
       (?楊 . "0x694A") ; <CJK>
       (?樣 . "0x6A23") ; <CJK>
       (?洋 . "0x6D0B") ; <CJK>
       (?瀁 . "0x7001") ; <CJK>
       (?煬 . "0x716C") ; <CJK>
       (?痒 . "0x75D2") ; <CJK>
       (?瘍 . "0x760D") ; <CJK>
       (?禳 . "0x79B3") ; <CJK>
       (?穰 . "0x7A70") ; <CJK>
       (?糧 . "0xF97B") ; <CJK>
       (?羊 . "0x7F8A") ; <CJK>
       (?良 . "0xF97C") ; <CJK>
       (?襄 . "0x8944") ; <CJK>
       (?諒 . "0xF97D") ; <CJK>
       (?讓 . "0x8B93") ; <CJK>
       (?釀 . "0x91C0") ; <CJK>
       (?陽 . "0x967D") ; <CJK>
       (?量 . "0xF97E") ; <CJK>
       (?養 . "0x990A") ; <CJK>
       (?圄 . "0x5704") ; <CJK>
       (?御 . "0x5FA1") ; <CJK>
       (?於 . "0x65BC") ; <CJK>
       (?漁 . "0x6F01") ; <CJK>
       (?瘀 . "0x7600") ; <CJK>
       (?禦 . "0x79A6") ; <CJK>
       (?語 . "0x8A9E") ; <CJK>
       (?馭 . "0x99AD") ; <CJK>
       (?魚 . "0x9B5A") ; <CJK>
       (?齬 . "0x9F6C") ; <CJK>
       (?億 . "0x5104") ; <CJK>
       (?憶 . "0x61B6") ; <CJK>
       (?抑 . "0x6291") ; <CJK>
       (?檍 . "0x6A8D") ; <CJK>
       (?臆 . "0x81C6") ; <CJK>
       (?偃 . "0x5043") ; <CJK>
       (?堰 . "0x5830") ; <CJK>
       (?彦 . "0x5F66") ; <CJK>
       (?焉 . "0x7109") ; <CJK>
       (?言 . "0x8A00") ; <CJK>
       (?諺 . "0x8AFA") ; <CJK>
       (?孼 . "0x5B7C") ; <CJK>
       (?蘖 . "0x8616") ; <CJK>
       (?俺 . "0x4FFA") ; <CJK>
       (?儼 . "0x513C") ; <CJK>
       (?嚴 . "0x56B4") ; <CJK>
       (?奄 . "0x5944") ; <CJK>
       (?掩 . "0x63A9") ; <CJK>
       (?淹 . "0x6DF9") ; <CJK>
       (?嶪 . "0x5DAA") ; <CJK>
       (?業 . "0x696D") ; <CJK>
       (?円 . "0x5186") ; <CJK>
       (?予 . "0x4E88") ; <CJK>
       (?余 . "0x4F59") ; <CJK>
       (?勵 . "0xF97F") ; <CJK>
       (?呂 . "0xF980") ; <CJK>
       (?女 . "0xF981") ; <CJK>
       (?如 . "0x5982") ; <CJK>
       (?廬 . "0xF982") ; <CJK>
       (?旅 . "0xF983") ; <CJK>
       (?歟 . "0x6B5F") ; <CJK>
       (?汝 . "0x6C5D") ; <CJK>
       (?濾 . "0xF984") ; <CJK>
       (?璵 . "0x74B5") ; <CJK>
       (?礖 . "0x7916") ; <CJK>
       (?礪 . "0xF985") ; <CJK>
       (?與 . "0x8207") ; <CJK>
       (?艅 . "0x8245") ; <CJK>
       (?茹 . "0x8339") ; <CJK>
       (?輿 . "0x8F3F") ; <CJK>
       (?轝 . "0x8F5D") ; <CJK>
       (?閭 . "0xF986") ; <CJK>
       (?餘 . "0x9918") ; <CJK>
       (?驪 . "0xF987") ; <CJK>
       (?麗 . "0xF988") ; <CJK>
       (?黎 . "0xF989") ; <CJK>
       (?亦 . "0x4EA6") ; <CJK>
       (?力 . "0xF98A") ; <CJK>
       (?域 . "0x57DF") ; <CJK>
       (?役 . "0x5F79") ; <CJK>
       (?易 . "0x6613") ; <CJK>
       (?曆 . "0xF98B") ; <CJK>
       (?歷 . "0xF98C") ; <CJK>
       (?疫 . "0x75AB") ; <CJK>
       (?繹 . "0x7E79") ; <CJK>
       (?譯 . "0x8B6F") ; <CJK>
       (?轢 . "0xF98D") ; <CJK>
       (?逆 . "0x9006") ; <CJK>
       (?驛 . "0x9A5B") ; <CJK>
       (?嚥 . "0x56A5") ; <CJK>
       (?堧 . "0x5827") ; <CJK>
       (?姸 . "0x59F8") ; <CJK>
       (?娟 . "0x5A1F") ; <CJK>
       (?宴 . "0x5BB4") ; <CJK>
       (?年 . "0xF98E") ; <CJK>
       (?延 . "0x5EF6") ; <CJK>
       (?憐 . "0xF98F") ; <CJK>
       (?戀 . "0xF990") ; <CJK>
       (?捐 . "0x6350") ; <CJK>
       (?挻 . "0x633B") ; <CJK>
       (?撚 . "0xF991") ; <CJK>
       (?椽 . "0x693D") ; <CJK>
       (?沇 . "0x6C87") ; <CJK>
       (?沿 . "0x6CBF") ; <CJK>
       (?涎 . "0x6D8E") ; <CJK>
       (?涓 . "0x6D93") ; <CJK>
       (?淵 . "0x6DF5") ; <CJK>
       (?演 . "0x6F14") ; <CJK>
       (?漣 . "0xF992") ; <CJK>
       (?烟 . "0x70DF") ; <CJK>
       (?然 . "0x7136") ; <CJK>
       (?煙 . "0x7159") ; <CJK>
       (?煉 . "0xF993") ; <CJK>
       (?燃 . "0x71C3") ; <CJK>
       (?燕 . "0x71D5") ; <CJK>
       (?璉 . "0xF994") ; <CJK>
       (?硏 . "0x784F") ; <CJK>
       (?硯 . "0x786F") ; <CJK>
       (?秊 . "0xF995") ; <CJK>
       (?筵 . "0x7B75") ; <CJK>
       (?緣 . "0x7DE3") ; <CJK>
       (?練 . "0xF996") ; <CJK>
       (?縯 . "0x7E2F") ; <CJK>
       (?聯 . "0xF997") ; <CJK>
       (?衍 . "0x884D") ; <CJK>
       (?軟 . "0x8EDF") ; <CJK>
       (?輦 . "0xF998") ; <CJK>
       (?蓮 . "0xF999") ; <CJK>
       (?連 . "0xF99A") ; <CJK>
       (?鉛 . "0x925B") ; <CJK>
       (?鍊 . "0xF99B") ; <CJK>
       (?鳶 . "0x9CF6") ; <CJK>
       (?列 . "0xF99C") ; <CJK>
       (?劣 . "0xF99D") ; <CJK>
       (?咽 . "0xF99E") ; <CJK>
       (?悅 . "0x6085") ; <CJK>
       (?涅 . "0x6D85") ; <CJK>
       (?烈 . "0xF99F") ; <CJK>
       (?熱 . "0x71B1") ; <CJK>
       (?裂 . "0xF9A0") ; <CJK>
       (?說 . "0xF9A1") ; <CJK>
       (?閱 . "0x95B1") ; <CJK>
       (?厭 . "0x53AD") ; <CJK>
       (?廉 . "0xF9A2") ; <CJK>
       (?念 . "0xF9A3") ; <CJK>
       (?捻 . "0xF9A4") ; <CJK>
       (?染 . "0x67D3") ; <CJK>
       (?殮 . "0xF9A5") ; <CJK>
       (?炎 . "0x708E") ; <CJK>
       (?焰 . "0x7130") ; <CJK>
       (?琰 . "0x7430") ; <CJK>
       (?艶 . "0x8276") ; <CJK>
       (?苒 . "0x82D2") ; <CJK>
       (?簾 . "0xF9A6") ; <CJK>
       (?閻 . "0x95BB") ; <CJK>
       (?髥 . "0x9AE5") ; <CJK>
       (?鹽 . "0x9E7D") ; <CJK>
       (?曄 . "0x66C4") ; <CJK>
       (?獵 . "0xF9A7") ; <CJK>
       (?燁 . "0x71C1") ; <CJK>
       (?葉 . "0x8449") ; <CJK>
       (?令 . "0xF9A8") ; <CJK>
       (?囹 . "0xF9A9") ; <CJK>
       (?塋 . "0x584B") ; <CJK>
       (?寧 . "0xF9AA") ; <CJK>
       (?嶺 . "0xF9AB") ; <CJK>
       (?嶸 . "0x5DB8") ; <CJK>
       (?影 . "0x5F71") ; <CJK>
       (?怜 . "0xF9AC") ; <CJK>
       (?映 . "0x6620") ; <CJK>
       (?暎 . "0x668E") ; <CJK>
       (?楹 . "0x6979") ; <CJK>
       (?榮 . "0x69AE") ; <CJK>
       (?永 . "0x6C38") ; <CJK>
       (?泳 . "0x6CF3") ; <CJK>
       (?渶 . "0x6E36") ; <CJK>
       (?潁 . "0x6F41") ; <CJK>
       (?濚 . "0x6FDA") ; <CJK>
       (?瀛 . "0x701B") ; <CJK>
       (?瀯 . "0x702F") ; <CJK>
       (?煐 . "0x7150") ; <CJK>
       (?營 . "0x71DF") ; <CJK>
       (?獰 . "0x7370") ; <CJK>
       (?玲 . "0xF9AD") ; <CJK>
       (?瑛 . "0x745B") ; <CJK>
       (?瑩 . "0xF9AE") ; <CJK>
       (?瓔 . "0x74D4") ; <CJK>
       (?盈 . "0x76C8") ; <CJK>
       (?穎 . "0x7A4E") ; <CJK>
       (?纓 . "0x7E93") ; <CJK>
       (?羚 . "0xF9AF") ; <CJK>
       (?聆 . "0xF9B0") ; <CJK>
       (?英 . "0x82F1") ; <CJK>
       (?詠 . "0x8A60") ; <CJK>
       (?迎 . "0x8FCE") ; <CJK>
       (?鈴 . "0xF9B1") ; <CJK>
       (?鍈 . "0x9348") ; <CJK>
       (?零 . "0xF9B2") ; <CJK>
       (?霙 . "0x9719") ; <CJK>
       (?靈 . "0xF9B3") ; <CJK>
       (?領 . "0xF9B4") ; <CJK>
       (?乂 . "0x4E42") ; <CJK>
       (?倪 . "0x502A") ; <CJK>
       (?例 . "0xF9B5") ; <CJK>
       (?刈 . "0x5208") ; <CJK>
       (?叡 . "0x53E1") ; <CJK>
       (?曳 . "0x66F3") ; <CJK>
       (?汭 . "0x6C6D") ; <CJK>
       (?濊 . "0x6FCA") ; <CJK>
       (?猊 . "0x730A") ; <CJK>
       (?睿 . "0x777F") ; <CJK>
       (?穢 . "0x7A62") ; <CJK>
       (?芮 . "0x82AE") ; <CJK>
       (?藝 . "0x85DD") ; <CJK>
       (?蘂 . "0x8602") ; <CJK>
       (?禮 . "0xF9B6") ; <CJK>
       (?裔 . "0x88D4") ; <CJK>
       (?詣 . "0x8A63") ; <CJK>
       (?譽 . "0x8B7D") ; <CJK>
       (?豫 . "0x8C6B") ; <CJK>
       (?醴 . "0xF9B7") ; <CJK>
       (?銳 . "0x92B3") ; <CJK>
       (?隸 . "0xF9B8") ; <CJK>
       (?霓 . "0x9713") ; <CJK>
       (?預 . "0x9810") ; <CJK>
       (?五 . "0x4E94") ; <CJK>
       (?伍 . "0x4F0D") ; <CJK>
       (?俉 . "0x4FC9") ; <CJK>
       (?傲 . "0x50B2") ; <CJK>
       (?午 . "0x5348") ; <CJK>
       (?吾 . "0x543E") ; <CJK>
       (?吳 . "0x5433") ; <CJK>
       (?嗚 . "0x55DA") ; <CJK>
       (?塢 . "0x5862") ; <CJK>
       (?墺 . "0x58BA") ; <CJK>
       (?奧 . "0x5967") ; <CJK>
       (?娛 . "0x5A1B") ; <CJK>
       (?寤 . "0x5BE4") ; <CJK>
       (?悟 . "0x609F") ; <CJK>
       (?惡 . "0xF9B9") ; <CJK>
       (?懊 . "0x61CA") ; <CJK>
       (?敖 . "0x6556") ; <CJK>
       (?旿 . "0x65FF") ; <CJK>
       (?晤 . "0x6664") ; <CJK>
       (?梧 . "0x68A7") ; <CJK>
       (?汚 . "0x6C5A") ; <CJK>
       (?澳 . "0x6FB3") ; <CJK>
       (?烏 . "0x70CF") ; <CJK>
       (?熬 . "0x71AC") ; <CJK>
       (?獒 . "0x7352") ; <CJK>
       (?筽 . "0x7B7D") ; <CJK>
       (?蜈 . "0x8708") ; <CJK>
       (?誤 . "0x8AA4") ; <CJK>
       (?鰲 . "0x9C32") ; <CJK>
       (?鼇 . "0x9F07") ; <CJK>
       (?屋 . "0x5C4B") ; <CJK>
       (?沃 . "0x6C83") ; <CJK>
       (?獄 . "0x7344") ; <CJK>
       (?玉 . "0x7389") ; <CJK>
       (?鈺 . "0x923A") ; <CJK>
       (?溫 . "0x6EAB") ; <CJK>
       (?瑥 . "0x7465") ; <CJK>
       (?瘟 . "0x761F") ; <CJK>
       (?穩 . "0x7A69") ; <CJK>
       (?縕 . "0x7E15") ; <CJK>
       (?蘊 . "0x860A") ; <CJK>
       (?兀 . "0x5140") ; <CJK>
       (?壅 . "0x58C5") ; <CJK>
       (?擁 . "0x64C1") ; <CJK>
       (?瓮 . "0x74EE") ; <CJK>
       (?甕 . "0x7515") ; <CJK>
       (?癰 . "0x7670") ; <CJK>
       (?翁 . "0x7FC1") ; <CJK>
       (?邕 . "0x9095") ; <CJK>
       (?雍 . "0x96CD") ; <CJK>
       (?饔 . "0x9954") ; <CJK>
       (?渦 . "0x6E26") ; <CJK>
       (?瓦 . "0x74E6") ; <CJK>
       (?窩 . "0x7AA9") ; <CJK>
       (?窪 . "0x7AAA") ; <CJK>
       (?臥 . "0x81E5") ; <CJK>
       (?蛙 . "0x86D9") ; <CJK>
       (?蝸 . "0x8778") ; <CJK>
       (?訛 . "0x8A1B") ; <CJK>
       (?婉 . "0x5A49") ; <CJK>
       (?完 . "0x5B8C") ; <CJK>
       (?宛 . "0x5B9B") ; <CJK>
       (?梡 . "0x68A1") ; <CJK>
       (?椀 . "0x6900") ; <CJK>
       (?浣 . "0x6D63") ; <CJK>
       (?玩 . "0x73A9") ; <CJK>
       (?琓 . "0x7413") ; <CJK>
       (?琬 . "0x742C") ; <CJK>
       (?碗 . "0x7897") ; <CJK>
       (?緩 . "0x7DE9") ; <CJK>
       (?翫 . "0x7FEB") ; <CJK>
       (?脘 . "0x8118") ; <CJK>
       (?腕 . "0x8155") ; <CJK>
       (?莞 . "0x839E") ; <CJK>
       (?豌 . "0x8C4C") ; <CJK>
       (?阮 . "0x962E") ; <CJK>
       (?頑 . "0x9811") ; <CJK>
       (?曰 . "0x66F0") ; <CJK>
       (?往 . "0x5F80") ; <CJK>
       (?旺 . "0x65FA") ; <CJK>
       (?枉 . "0x6789") ; <CJK>
       (?汪 . "0x6C6A") ; <CJK>
       (?王 . "0x738B") ; <CJK>
       (?倭 . "0x502D") ; <CJK>
       (?娃 . "0x5A03") ; <CJK>
       (?歪 . "0x6B6A") ; <CJK>
       (?矮 . "0x77EE") ; <CJK>
       (?外 . "0x5916") ; <CJK>
       (?嵬 . "0x5D6C") ; <CJK>
       (?巍 . "0x5DCD") ; <CJK>
       (?猥 . "0x7325") ; <CJK>
       (?畏 . "0x754F") ; <CJK>
       (?了 . "0xF9BA") ; <CJK>
       (?僚 . "0xF9BB") ; <CJK>
       (?僥 . "0x50E5") ; <CJK>
       (?凹 . "0x51F9") ; <CJK>
       (?堯 . "0x582F") ; <CJK>
       (?夭 . "0x592D") ; <CJK>
       (?妖 . "0x5996") ; <CJK>
       (?姚 . "0x59DA") ; <CJK>
       (?寥 . "0x5BE5") ; <CJK>
       (?寮 . "0xF9BC") ; <CJK>
       (?尿 . "0xF9BD") ; <CJK>
       (?嶢 . "0x5DA2") ; <CJK>
       (?拗 . "0x62D7") ; <CJK>
       (?搖 . "0x6416") ; <CJK>
       (?撓 . "0x6493") ; <CJK>
       (?擾 . "0x64FE") ; <CJK>
       (?料 . "0xF9BE") ; <CJK>
       (?曜 . "0x66DC") ; <CJK>
       (?樂 . "0xF9BF") ; <CJK>
       (?橈 . "0x6A48") ; <CJK>
       (?燎 . "0xF9C0") ; <CJK>
       (?燿 . "0x71FF") ; <CJK>
       (?瑤 . "0x7464") ; <CJK>
       (?療 . "0xF9C1") ; <CJK>
       (?窈 . "0x7A88") ; <CJK>
       (?窯 . "0x7AAF") ; <CJK>
       (?繇 . "0x7E47") ; <CJK>
       (?繞 . "0x7E5E") ; <CJK>
       (?耀 . "0x8000") ; <CJK>
       (?腰 . "0x8170") ; <CJK>
       (?蓼 . "0xF9C2") ; <CJK>
       (?蟯 . "0x87EF") ; <CJK>
       (?要 . "0x8981") ; <CJK>
       (?謠 . "0x8B20") ; <CJK>
       (?遙 . "0x9059") ; <CJK>
       (?遼 . "0xF9C3") ; <CJK>
       (?邀 . "0x9080") ; <CJK>
       (?饒 . "0x9952") ; <CJK>
       (?慾 . "0x617E") ; <CJK>
       (?欲 . "0x6B32") ; <CJK>
       (?浴 . "0x6D74") ; <CJK>
       (?縟 . "0x7E1F") ; <CJK>
       (?褥 . "0x8925") ; <CJK>
       (?辱 . "0x8FB1") ; <CJK>
       (?俑 . "0x4FD1") ; <CJK>
       (?傭 . "0x50AD") ; <CJK>
       (?冗 . "0x5197") ; <CJK>
       (?勇 . "0x52C7") ; <CJK>
       (?埇 . "0x57C7") ; <CJK>
       (?墉 . "0x5889") ; <CJK>
       (?容 . "0x5BB9") ; <CJK>
       (?庸 . "0x5EB8") ; <CJK>
       (?慂 . "0x6142") ; <CJK>
       (?榕 . "0x6995") ; <CJK>
       (?涌 . "0x6D8C") ; <CJK>
       (?湧 . "0x6E67") ; <CJK>
       (?溶 . "0x6EB6") ; <CJK>
       (?熔 . "0x7194") ; <CJK>
       (?瑢 . "0x7462") ; <CJK>
       (?用 . "0x7528") ; <CJK>
       (?甬 . "0x752C") ; <CJK>
       (?聳 . "0x8073") ; <CJK>
       (?茸 . "0x8338") ; <CJK>
       (?蓉 . "0x84C9") ; <CJK>
       (?踊 . "0x8E0A") ; <CJK>
       (?鎔 . "0x9394") ; <CJK>
       (?鏞 . "0x93DE") ; <CJK>
       (?龍 . "0xF9C4") ; <CJK>
       (?于 . "0x4E8E") ; <CJK>
       (?佑 . "0x4F51") ; <CJK>
       (?偶 . "0x5076") ; <CJK>
       (?優 . "0x512A") ; <CJK>
       (?又 . "0x53C8") ; <CJK>
       (?友 . "0x53CB") ; <CJK>
       (?右 . "0x53F3") ; <CJK>
       (?宇 . "0x5B87") ; <CJK>
       (?寓 . "0x5BD3") ; <CJK>
       (?尤 . "0x5C24") ; <CJK>
       (?愚 . "0x611A") ; <CJK>
       (?憂 . "0x6182") ; <CJK>
       (?旴 . "0x65F4") ; <CJK>
       (?牛 . "0x725B") ; <CJK>
       (?玗 . "0x7397") ; <CJK>
       (?瑀 . "0x7440") ; <CJK>
       (?盂 . "0x76C2") ; <CJK>
       (?祐 . "0x7950") ; <CJK>
       (?禑 . "0x7991") ; <CJK>
       (?禹 . "0x79B9") ; <CJK>
       (?紆 . "0x7D06") ; <CJK>
       (?羽 . "0x7FBD") ; <CJK>
       (?芋 . "0x828B") ; <CJK>
       (?藕 . "0x85D5") ; <CJK>
       (?虞 . "0x865E") ; <CJK>
       (?迂 . "0x8FC2") ; <CJK>
       (?遇 . "0x9047") ; <CJK>
       (?郵 . "0x90F5") ; <CJK>
       (?釪 . "0x91EA") ; <CJK>
       (?隅 . "0x9685") ; <CJK>
       (?雨 . "0x96E8") ; <CJK>
       (?雩 . "0x96E9") ; <CJK>
       (?勖 . "0x52D6") ; <CJK>
       (?彧 . "0x5F67") ; <CJK>
       (?旭 . "0x65ED") ; <CJK>
       (?昱 . "0x6631") ; <CJK>
       (?栯 . "0x682F") ; <CJK>
       (?煜 . "0x715C") ; <CJK>
       (?稶 . "0x7A36") ; <CJK>
       (?郁 . "0x90C1") ; <CJK>
       (?頊 . "0x980A") ; <CJK>
       (?云 . "0x4E91") ; <CJK>
       (?暈 . "0xF9C5") ; <CJK>
       (?橒 . "0x6A52") ; <CJK>
       (?殞 . "0x6B9E") ; <CJK>
       (?澐 . "0x6F90") ; <CJK>
       (?熉 . "0x7189") ; <CJK>
       (?耘 . "0x8018") ; <CJK>
       (?芸 . "0x82B8") ; <CJK>
       (?蕓 . "0x8553") ; <CJK>
       (?運 . "0x904B") ; <CJK>
       (?隕 . "0x9695") ; <CJK>
       (?雲 . "0x96F2") ; <CJK>
       (?韻 . "0x97FB") ; <CJK>
       (?蔚 . "0x851A") ; <CJK>
       (?鬱 . "0x9B31") ; <CJK>
       (?亐 . "0x4E90") ; <CJK>
       (?熊 . "0x718A") ; <CJK>
       (?雄 . "0x96C4") ; <CJK>
       (?元 . "0x5143") ; <CJK>
       (?原 . "0x539F") ; <CJK>
       (?員 . "0x54E1") ; <CJK>
       (?圓 . "0x5713") ; <CJK>
       (?園 . "0x5712") ; <CJK>
       (?垣 . "0x57A3") ; <CJK>
       (?媛 . "0x5A9B") ; <CJK>
       (?嫄 . "0x5AC4") ; <CJK>
       (?寃 . "0x5BC3") ; <CJK>
       (?怨 . "0x6028") ; <CJK>
       (?愿 . "0x613F") ; <CJK>
       (?援 . "0x63F4") ; <CJK>
       (?沅 . "0x6C85") ; <CJK>
       (?洹 . "0x6D39") ; <CJK>
       (?湲 . "0x6E72") ; <CJK>
       (?源 . "0x6E90") ; <CJK>
       (?爰 . "0x7230") ; <CJK>
       (?猿 . "0x733F") ; <CJK>
       (?瑗 . "0x7457") ; <CJK>
       (?苑 . "0x82D1") ; <CJK>
       (?袁 . "0x8881") ; <CJK>
       (?轅 . "0x8F45") ; <CJK>
       (?遠 . "0x9060") ; <CJK>
       (?阮 . "0xF9C6") ; <CJK>
       (?院 . "0x9662") ; <CJK>
       (?願 . "0x9858") ; <CJK>
       (?鴛 . "0x9D1B") ; <CJK>
       (?月 . "0x6708") ; <CJK>
       (?越 . "0x8D8A") ; <CJK>
       (?鉞 . "0x925E") ; <CJK>
       (?位 . "0x4F4D") ; <CJK>
       (?偉 . "0x5049") ; <CJK>
       (?僞 . "0x50DE") ; <CJK>
       (?危 . "0x5371") ; <CJK>
       (?圍 . "0x570D") ; <CJK>
       (?委 . "0x59D4") ; <CJK>
       (?威 . "0x5A01") ; <CJK>
       (?尉 . "0x5C09") ; <CJK>
       (?慰 . "0x6170") ; <CJK>
       (?暐 . "0x6690") ; <CJK>
       (?渭 . "0x6E2D") ; <CJK>
       (?爲 . "0x7232") ; <CJK>
       (?瑋 . "0x744B") ; <CJK>
       (?緯 . "0x7DEF") ; <CJK>
       (?胃 . "0x80C3") ; <CJK>
       (?萎 . "0x840E") ; <CJK>
       (?葦 . "0x8466") ; <CJK>
       (?蔿 . "0x853F") ; <CJK>
       (?蝟 . "0x875F") ; <CJK>
       (?衛 . "0x885B") ; <CJK>
       (?褘 . "0x8918") ; <CJK>
       (?謂 . "0x8B02") ; <CJK>
       (?違 . "0x9055") ; <CJK>
       (?韋 . "0x97CB") ; <CJK>
       (?魏 . "0x9B4F") ; <CJK>
       (?乳 . "0x4E73") ; <CJK>
       (?侑 . "0x4F91") ; <CJK>
       (?儒 . "0x5112") ; <CJK>
       (?兪 . "0x516A") ; <CJK>
       (?劉 . "0xF9C7") ; <CJK>
       (?唯 . "0x552F") ; <CJK>
       (?喩 . "0x55A9") ; <CJK>
       (?孺 . "0x5B7A") ; <CJK>
       (?宥 . "0x5BA5") ; <CJK>
       (?幼 . "0x5E7C") ; <CJK>
       (?幽 . "0x5E7D") ; <CJK>
       (?庾 . "0x5EBE") ; <CJK>
       (?悠 . "0x60A0") ; <CJK>
       (?惟 . "0x60DF") ; <CJK>
       (?愈 . "0x6108") ; <CJK>
       (?愉 . "0x6109") ; <CJK>
       (?揄 . "0x63C4") ; <CJK>
       (?攸 . "0x6538") ; <CJK>
       (?有 . "0x6709") ; <CJK>
       (?杻 . "0xF9C8") ; <CJK>
       (?柔 . "0x67D4") ; <CJK>
       (?柚 . "0x67DA") ; <CJK>
       (?柳 . "0xF9C9") ; <CJK>
       (?楡 . "0x6961") ; <CJK>
       (?楢 . "0x6962") ; <CJK>
       (?油 . "0x6CB9") ; <CJK>
       (?洧 . "0x6D27") ; <CJK>
       (?流 . "0xF9CA") ; <CJK>
       (?游 . "0x6E38") ; <CJK>
       (?溜 . "0xF9CB") ; <CJK>
       (?濡 . "0x6FE1") ; <CJK>
       (?猶 . "0x7336") ; <CJK>
       (?猷 . "0x7337") ; <CJK>
       (?琉 . "0xF9CC") ; <CJK>
       (?瑜 . "0x745C") ; <CJK>
       (?由 . "0x7531") ; <CJK>
       (?留 . "0xF9CD") ; <CJK>
       (?癒 . "0x7652") ; <CJK>
       (?硫 . "0xF9CE") ; <CJK>
       (?紐 . "0xF9CF") ; <CJK>
       (?維 . "0x7DAD") ; <CJK>
       (?臾 . "0x81FE") ; <CJK>
       (?萸 . "0x8438") ; <CJK>
       (?裕 . "0x88D5") ; <CJK>
       (?誘 . "0x8A98") ; <CJK>
       (?諛 . "0x8ADB") ; <CJK>
       (?諭 . "0x8AED") ; <CJK>
       (?踰 . "0x8E30") ; <CJK>
       (?蹂 . "0x8E42") ; <CJK>
       (?遊 . "0x904A") ; <CJK>
       (?逾 . "0x903E") ; <CJK>
       (?遺 . "0x907A") ; <CJK>
       (?酉 . "0x9149") ; <CJK>
       (?釉 . "0x91C9") ; <CJK>
       (?鍮 . "0x936E") ; <CJK>
       (?類 . "0xF9D0") ; <CJK>
       (?六 . "0xF9D1") ; <CJK>
       (?堉 . "0x5809") ; <CJK>
       (?戮 . "0xF9D2") ; <CJK>
       (?毓 . "0x6BD3") ; <CJK>
       (?肉 . "0x8089") ; <CJK>
       (?育 . "0x80B2") ; <CJK>
       (?陸 . "0xF9D3") ; <CJK>
       (?倫 . "0xF9D4") ; <CJK>
       (?允 . "0x5141") ; <CJK>
       (?奫 . "0x596B") ; <CJK>
       (?尹 . "0x5C39") ; <CJK>
       (?崙 . "0xF9D5") ; <CJK>
       (?淪 . "0xF9D6") ; <CJK>
       (?潤 . "0x6F64") ; <CJK>
       (?玧 . "0x73A7") ; <CJK>
       (?胤 . "0x80E4") ; <CJK>
       (?贇 . "0x8D07") ; <CJK>
       (?輪 . "0xF9D7") ; <CJK>
       (?鈗 . "0x9217") ; <CJK>
       (?閏 . "0x958F") ; <CJK>
       (?律 . "0xF9D8") ; <CJK>
       (?慄 . "0xF9D9") ; <CJK>
       (?栗 . "0xF9DA") ; <CJK>
       (?率 . "0xF9DB") ; <CJK>
       (?聿 . "0x807F") ; <CJK>
       (?戎 . "0x620E") ; <CJK>
       (?瀜 . "0x701C") ; <CJK>
       (?絨 . "0x7D68") ; <CJK>
       (?融 . "0x878D") ; <CJK>
       (?隆 . "0xF9DC") ; <CJK>
       (?垠 . "0x57A0") ; <CJK>
       (?恩 . "0x6069") ; <CJK>
       (?慇 . "0x6147") ; <CJK>
       (?殷 . "0x6BB7") ; <CJK>
       (?誾 . "0x8ABE") ; <CJK>
       (?銀 . "0x9280") ; <CJK>
       (?隱 . "0x96B1") ; <CJK>
       (?乙 . "0x4E59") ; <CJK>
       (?吟 . "0x541F") ; <CJK>
       (?淫 . "0x6DEB") ; <CJK>
       (?蔭 . "0x852D") ; <CJK>
       (?陰 . "0x9670") ; <CJK>
       (?音 . "0x97F3") ; <CJK>
       (?飮 . "0x98EE") ; <CJK>
       (?揖 . "0x63D6") ; <CJK>
       (?泣 . "0x6CE3") ; <CJK>
       (?邑 . "0x9091") ; <CJK>
       (?凝 . "0x51DD") ; <CJK>
       (?應 . "0x61C9") ; <CJK>
       (?膺 . "0x81BA") ; <CJK>
       (?鷹 . "0x9DF9") ; <CJK>
       (?依 . "0x4F9D") ; <CJK>
       (?倚 . "0x501A") ; <CJK>
       (?儀 . "0x5100") ; <CJK>
       (?宜 . "0x5B9C") ; <CJK>
       (?意 . "0x610F") ; <CJK>
       (?懿 . "0x61FF") ; <CJK>
       (?擬 . "0x64EC") ; <CJK>
       (?椅 . "0x6905") ; <CJK>
       (?毅 . "0x6BC5") ; <CJK>
       (?疑 . "0x7591") ; <CJK>
       (?矣 . "0x77E3") ; <CJK>
       (?義 . "0x7FA9") ; <CJK>
       (?艤 . "0x8264") ; <CJK>
       (?薏 . "0x858F") ; <CJK>
       (?蟻 . "0x87FB") ; <CJK>
       (?衣 . "0x8863") ; <CJK>
       (?誼 . "0x8ABC") ; <CJK>
       (?議 . "0x8B70") ; <CJK>
       (?醫 . "0x91AB") ; <CJK>
       (?二 . "0x4E8C") ; <CJK>
       (?以 . "0x4EE5") ; <CJK>
       (?伊 . "0x4F0A") ; <CJK>
       (?利 . "0xF9DD") ; <CJK>
       (?吏 . "0xF9DE") ; <CJK>
       (?夷 . "0x5937") ; <CJK>
       (?姨 . "0x59E8") ; <CJK>
       (?履 . "0xF9DF") ; <CJK>
       (?已 . "0x5DF2") ; <CJK>
       (?弛 . "0x5F1B") ; <CJK>
       (?彛 . "0x5F5B") ; <CJK>
       (?怡 . "0x6021") ; <CJK>
       (?易 . "0xF9E0") ; <CJK>
       (?李 . "0xF9E1") ; <CJK>
       (?梨 . "0xF9E2") ; <CJK>
       (?泥 . "0xF9E3") ; <CJK>
       (?爾 . "0x723E") ; <CJK>
       (?珥 . "0x73E5") ; <CJK>
       (?理 . "0xF9E4") ; <CJK>
       (?異 . "0x7570") ; <CJK>
       (?痍 . "0x75CD") ; <CJK>
       (?痢 . "0xF9E5") ; <CJK>
       (?移 . "0x79FB") ; <CJK>
       (?罹 . "0xF9E6") ; <CJK>
       (?而 . "0x800C") ; <CJK>
       (?耳 . "0x8033") ; <CJK>
       (?肄 . "0x8084") ; <CJK>
       (?苡 . "0x82E1") ; <CJK>
       (?荑 . "0x8351") ; <CJK>
       (?裏 . "0xF9E7") ; <CJK>
       (?裡 . "0xF9E8") ; <CJK>
       (?貽 . "0x8CBD") ; <CJK>
       (?貳 . "0x8CB3") ; <CJK>
       (?邇 . "0x9087") ; <CJK>
       (?里 . "0xF9E9") ; <CJK>
       (?離 . "0xF9EA") ; <CJK>
       (?飴 . "0x98F4") ; <CJK>
       (?餌 . "0x990C") ; <CJK>
       (?匿 . "0xF9EB") ; <CJK>
       (?溺 . "0xF9EC") ; <CJK>
       (?瀷 . "0x7037") ; <CJK>
       (?益 . "0x76CA") ; <CJK>
       (?翊 . "0x7FCA") ; <CJK>
       (?翌 . "0x7FCC") ; <CJK>
       (?翼 . "0x7FFC") ; <CJK>
       (?謚 . "0x8B1A") ; <CJK>
       (?人 . "0x4EBA") ; <CJK>
       (?仁 . "0x4EC1") ; <CJK>
       (?刃 . "0x5203") ; <CJK>
       (?印 . "0x5370") ; <CJK>
       (?吝 . "0xF9ED") ; <CJK>
       (?咽 . "0x54BD") ; <CJK>
       (?因 . "0x56E0") ; <CJK>
       (?姻 . "0x59FB") ; <CJK>
       (?寅 . "0x5BC5") ; <CJK>
       (?引 . "0x5F15") ; <CJK>
       (?忍 . "0x5FCD") ; <CJK>
       (?湮 . "0x6E6E") ; <CJK>
       (?燐 . "0xF9EE") ; <CJK>
       (?璘 . "0xF9EF") ; <CJK>
       (?絪 . "0x7D6A") ; <CJK>
       (?茵 . "0x8335") ; <CJK>
       (?藺 . "0xF9F0") ; <CJK>
       (?蚓 . "0x8693") ; <CJK>
       (?認 . "0x8A8D") ; <CJK>
       (?隣 . "0xF9F1") ; <CJK>
       (?靭 . "0x976D") ; <CJK>
       (?靷 . "0x9777") ; <CJK>
       (?鱗 . "0xF9F2") ; <CJK>
       (?麟 . "0xF9F3") ; <CJK>
       (?一 . "0x4E00") ; <CJK>
       (?佚 . "0x4F5A") ; <CJK>
       (?佾 . "0x4F7E") ; <CJK>
       (?壹 . "0x58F9") ; <CJK>
       (?日 . "0x65E5") ; <CJK>
       (?溢 . "0x6EA2") ; <CJK>
       (?逸 . "0x9038") ; <CJK>
       (?鎰 . "0x93B0") ; <CJK>
       (?馹 . "0x99B9") ; <CJK>
       (?任 . "0x4EFB") ; <CJK>
       (?壬 . "0x58EC") ; <CJK>
       (?妊 . "0x598A") ; <CJK>
       (?姙 . "0x59D9") ; <CJK>
       (?恁 . "0x6041") ; <CJK>
       (?林 . "0xF9F4") ; <CJK>
       (?淋 . "0xF9F5") ; <CJK>
       (?稔 . "0x7A14") ; <CJK>
       (?臨 . "0xF9F6") ; <CJK>
       (?荏 . "0x834F") ; <CJK>
       (?賃 . "0x8CC3") ; <CJK>
       (?入 . "0x5165") ; <CJK>
       (?卄 . "0x5344") ; <CJK>
       (?立 . "0xF9F7") ; <CJK>
       (?笠 . "0xF9F8") ; <CJK>
       (?粒 . "0xF9F9") ; <CJK>
       (?仍 . "0x4ECD") ; <CJK>
       (?剩 . "0x5269") ; <CJK>
       (?孕 . "0x5B55") ; <CJK>
       (?芿 . "0x82BF") ; <CJK>
       (?仔 . "0x4ED4") ; <CJK>
       (?刺 . "0x523A") ; <CJK>
       (?咨 . "0x54A8") ; <CJK>
       (?姉 . "0x59C9") ; <CJK>
       (?姿 . "0x59FF") ; <CJK>
       (?子 . "0x5B50") ; <CJK>
       (?字 . "0x5B57") ; <CJK>
       (?孜 . "0x5B5C") ; <CJK>
       (?恣 . "0x6063") ; <CJK>
       (?慈 . "0x6148") ; <CJK>
       (?滋 . "0x6ECB") ; <CJK>
       (?炙 . "0x7099") ; <CJK>
       (?煮 . "0x716E") ; <CJK>
       (?玆 . "0x7386") ; <CJK>
       (?瓷 . "0x74F7") ; <CJK>
       (?疵 . "0x75B5") ; <CJK>
       (?磁 . "0x78C1") ; <CJK>
       (?紫 . "0x7D2B") ; <CJK>
       (?者 . "0x8005") ; <CJK>
       (?自 . "0x81EA") ; <CJK>
       (?茨 . "0x8328") ; <CJK>
       (?蔗 . "0x8517") ; <CJK>
       (?藉 . "0x85C9") ; <CJK>
       (?諮 . "0x8AEE") ; <CJK>
       (?資 . "0x8CC7") ; <CJK>
       (?雌 . "0x96CC") ; <CJK>
       (?作 . "0x4F5C") ; <CJK>
       (?勺 . "0x52FA") ; <CJK>
       (?嚼 . "0x56BC") ; <CJK>
       (?斫 . "0x65AB") ; <CJK>
       (?昨 . "0x6628") ; <CJK>
       (?灼 . "0x707C") ; <CJK>
       (?炸 . "0x70B8") ; <CJK>
       (?爵 . "0x7235") ; <CJK>
       (?綽 . "0x7DBD") ; <CJK>
       (?芍 . "0x828D") ; <CJK>
       (?酌 . "0x914C") ; <CJK>
       (?雀 . "0x96C0") ; <CJK>
       (?鵲 . "0x9D72") ; <CJK>
       (?孱 . "0x5B71") ; <CJK>
       (?棧 . "0x68E7") ; <CJK>
       (?殘 . "0x6B98") ; <CJK>
       (?潺 . "0x6F7A") ; <CJK>
       (?盞 . "0x76DE") ; <CJK>
       (?岑 . "0x5C91") ; <CJK>
       (?暫 . "0x66AB") ; <CJK>
       (?潛 . "0x6F5B") ; <CJK>
       (?箴 . "0x7BB4") ; <CJK>
       (?簪 . "0x7C2A") ; <CJK>
       (?蠶 . "0x8836") ; <CJK>
       (?雜 . "0x96DC") ; <CJK>
       (?丈 . "0x4E08") ; <CJK>
       (?仗 . "0x4ED7") ; <CJK>
       (?匠 . "0x5320") ; <CJK>
       (?場 . "0x5834") ; <CJK>
       (?墻 . "0x58BB") ; <CJK>
       (?壯 . "0x58EF") ; <CJK>
       (?奬 . "0x596C") ; <CJK>
       (?將 . "0x5C07") ; <CJK>
       (?帳 . "0x5E33") ; <CJK>
       (?庄 . "0x5E84") ; <CJK>
       (?張 . "0x5F35") ; <CJK>
       (?掌 . "0x638C") ; <CJK>
       (?暲 . "0x66B2") ; <CJK>
       (?杖 . "0x6756") ; <CJK>
       (?樟 . "0x6A1F") ; <CJK>
       (?檣 . "0x6AA3") ; <CJK>
       (?欌 . "0x6B0C") ; <CJK>
       (?漿 . "0x6F3F") ; <CJK>
       (?牆 . "0x7246") ; <CJK>
       (?狀 . "0xF9FA") ; <CJK>
       (?獐 . "0x7350") ; <CJK>
       (?璋 . "0x748B") ; <CJK>
       (?章 . "0x7AE0") ; <CJK>
       (?粧 . "0x7CA7") ; <CJK>
       (?腸 . "0x8178") ; <CJK>
       (?臟 . "0x81DF") ; <CJK>
       (?臧 . "0x81E7") ; <CJK>
       (?莊 . "0x838A") ; <CJK>
       (?葬 . "0x846C") ; <CJK>
       (?蔣 . "0x8523") ; <CJK>
       (?薔 . "0x8594") ; <CJK>
       (?藏 . "0x85CF") ; <CJK>
       (?裝 . "0x88DD") ; <CJK>
       (?贓 . "0x8D13") ; <CJK>
       (?醬 . "0x91AC") ; <CJK>
       (?長 . "0x9577") ; <CJK>
       (?障 . "0x969C") ; <CJK>
       (?再 . "0x518D") ; <CJK>
       (?哉 . "0x54C9") ; <CJK>
       (?在 . "0x5728") ; <CJK>
       (?宰 . "0x5BB0") ; <CJK>
       (?才 . "0x624D") ; <CJK>
       (?材 . "0x6750") ; <CJK>
       (?栽 . "0x683D") ; <CJK>
       (?梓 . "0x6893") ; <CJK>
       (?渽 . "0x6E3D") ; <CJK>
       (?滓 . "0x6ED3") ; <CJK>
       (?災 . "0x707D") ; <CJK>
       (?縡 . "0x7E21") ; <CJK>
       (?裁 . "0x88C1") ; <CJK>
       (?財 . "0x8CA1") ; <CJK>
       (?載 . "0x8F09") ; <CJK>
       (?齋 . "0x9F4B") ; <CJK>
       (?齎 . "0x9F4E") ; <CJK>
       (?爭 . "0x722D") ; <CJK>
       (?箏 . "0x7B8F") ; <CJK>
       (?諍 . "0x8ACD") ; <CJK>
       (?錚 . "0x931A") ; <CJK>
       (?佇 . "0x4F47") ; <CJK>
       (?低 . "0x4F4E") ; <CJK>
       (?儲 . "0x5132") ; <CJK>
       (?咀 . "0x5480") ; <CJK>
       (?姐 . "0x59D0") ; <CJK>
       (?底 . "0x5E95") ; <CJK>
       (?抵 . "0x62B5") ; <CJK>
       (?杵 . "0x6775") ; <CJK>
       (?楮 . "0x696E") ; <CJK>
       (?樗 . "0x6A17") ; <CJK>
       (?沮 . "0x6CAE") ; <CJK>
       (?渚 . "0x6E1A") ; <CJK>
       (?狙 . "0x72D9") ; <CJK>
       (?猪 . "0x732A") ; <CJK>
       (?疽 . "0x75BD") ; <CJK>
       (?箸 . "0x7BB8") ; <CJK>
       (?紵 . "0x7D35") ; <CJK>
       (?苧 . "0x82E7") ; <CJK>
       (?菹 . "0x83F9") ; <CJK>
       (?著 . "0x8457") ; <CJK>
       (?藷 . "0x85F7") ; <CJK>
       (?詛 . "0x8A5B") ; <CJK>
       (?貯 . "0x8CAF") ; <CJK>
       (?躇 . "0x8E87") ; <CJK>
       (?這 . "0x9019") ; <CJK>
       (?邸 . "0x90B8") ; <CJK>
       (?雎 . "0x96CE") ; <CJK>
       (?齟 . "0x9F5F") ; <CJK>
       (?勣 . "0x52E3") ; <CJK>
       (?吊 . "0x540A") ; <CJK>
       (?嫡 . "0x5AE1") ; <CJK>
       (?寂 . "0x5BC2") ; <CJK>
       (?摘 . "0x6458") ; <CJK>
       (?敵 . "0x6575") ; <CJK>
       (?滴 . "0x6EF4") ; <CJK>
       (?狄 . "0x72C4") ; <CJK>
       (?炙 . "0xF9FB") ; <CJK>
       (?的 . "0x7684") ; <CJK>
       (?積 . "0x7A4D") ; <CJK>
       (?笛 . "0x7B1B") ; <CJK>
       (?籍 . "0x7C4D") ; <CJK>
       (?績 . "0x7E3E") ; <CJK>
       (?翟 . "0x7FDF") ; <CJK>
       (?荻 . "0x837B") ; <CJK>
       (?謫 . "0x8B2B") ; <CJK>
       (?賊 . "0x8CCA") ; <CJK>
       (?赤 . "0x8D64") ; <CJK>
       (?跡 . "0x8DE1") ; <CJK>
       (?蹟 . "0x8E5F") ; <CJK>
       (?迪 . "0x8FEA") ; <CJK>
       (?迹 . "0x8FF9") ; <CJK>
       (?適 . "0x9069") ; <CJK>
       (?鏑 . "0x93D1") ; <CJK>
       (?佃 . "0x4F43") ; <CJK>
       (?佺 . "0x4F7A") ; <CJK>
       (?傳 . "0x50B3") ; <CJK>
       (?全 . "0x5168") ; <CJK>
       (?典 . "0x5178") ; <CJK>
       (?前 . "0x524D") ; <CJK>
       (?剪 . "0x526A") ; <CJK>
       (?塡 . "0x5861") ; <CJK>
       (?塼 . "0x587C") ; <CJK>
       (?奠 . "0x5960") ; <CJK>
       (?專 . "0x5C08") ; <CJK>
       (?展 . "0x5C55") ; <CJK>
       (?廛 . "0x5EDB") ; <CJK>
       (?悛 . "0x609B") ; <CJK>
       (?戰 . "0x6230") ; <CJK>
       (?栓 . "0x6813") ; <CJK>
       (?殿 . "0x6BBF") ; <CJK>
       (?氈 . "0x6C08") ; <CJK>
       (?澱 . "0x6FB1") ; <CJK>
       (?煎 . "0x714E") ; <CJK>
       (?琠 . "0x7420") ; <CJK>
       (?田 . "0x7530") ; <CJK>
       (?甸 . "0x7538") ; <CJK>
       (?畑 . "0x7551") ; <CJK>
       (?癲 . "0x7672") ; <CJK>
       (?筌 . "0x7B4C") ; <CJK>
       (?箋 . "0x7B8B") ; <CJK>
       (?箭 . "0x7BAD") ; <CJK>
       (?篆 . "0x7BC6") ; <CJK>
       (?纏 . "0x7E8F") ; <CJK>
       (?詮 . "0x8A6E") ; <CJK>
       (?輾 . "0x8F3E") ; <CJK>
       (?轉 . "0x8F49") ; <CJK>
       (?鈿 . "0x923F") ; <CJK>
       (?銓 . "0x9293") ; <CJK>
       (?錢 . "0x9322") ; <CJK>
       (?鐫 . "0x942B") ; <CJK>
       (?電 . "0x96FB") ; <CJK>
       (?顚 . "0x985A") ; <CJK>
       (?顫 . "0x986B") ; <CJK>
       (?餞 . "0x991E") ; <CJK>
       (?切 . "0x5207") ; <CJK>
       (?截 . "0x622A") ; <CJK>
       (?折 . "0x6298") ; <CJK>
       (?浙 . "0x6D59") ; <CJK>
       (?癤 . "0x7664") ; <CJK>
       (?竊 . "0x7ACA") ; <CJK>
       (?節 . "0x7BC0") ; <CJK>
       (?絶 . "0x7D76") ; <CJK>
       (?占 . "0x5360") ; <CJK>
       (?岾 . "0x5CBE") ; <CJK>
       (?店 . "0x5E97") ; <CJK>
       (?漸 . "0x6F38") ; <CJK>
       (?点 . "0x70B9") ; <CJK>
       (?粘 . "0x7C98") ; <CJK>
       (?霑 . "0x9711") ; <CJK>
       (?鮎 . "0x9B8E") ; <CJK>
       (?點 . "0x9EDE") ; <CJK>
       (?接 . "0x63A5") ; <CJK>
       (?摺 . "0x647A") ; <CJK>
       (?蝶 . "0x8776") ; <CJK>
       (?丁 . "0x4E01") ; <CJK>
       (?井 . "0x4E95") ; <CJK>
       (?亭 . "0x4EAD") ; <CJK>
       (?停 . "0x505C") ; <CJK>
       (?偵 . "0x5075") ; <CJK>
       (?呈 . "0x5448") ; <CJK>
       (?姃 . "0x59C3") ; <CJK>
       (?定 . "0x5B9A") ; <CJK>
       (?幀 . "0x5E40") ; <CJK>
       (?庭 . "0x5EAD") ; <CJK>
       (?廷 . "0x5EF7") ; <CJK>
       (?征 . "0x5F81") ; <CJK>
       (?情 . "0x60C5") ; <CJK>
       (?挺 . "0x633A") ; <CJK>
       (?政 . "0x653F") ; <CJK>
       (?整 . "0x6574") ; <CJK>
       (?旌 . "0x65CC") ; <CJK>
       (?晶 . "0x6676") ; <CJK>
       (?晸 . "0x6678") ; <CJK>
       (?柾 . "0x67FE") ; <CJK>
       (?楨 . "0x6968") ; <CJK>
       (?檉 . "0x6A89") ; <CJK>
       (?正 . "0x6B63") ; <CJK>
       (?汀 . "0x6C40") ; <CJK>
       (?淀 . "0x6DC0") ; <CJK>
       (?淨 . "0x6DE8") ; <CJK>
       (?渟 . "0x6E1F") ; <CJK>
       (?湞 . "0x6E5E") ; <CJK>
       (?瀞 . "0x701E") ; <CJK>
       (?炡 . "0x70A1") ; <CJK>
       (?玎 . "0x738E") ; <CJK>
       (?珽 . "0x73FD") ; <CJK>
       (?町 . "0x753A") ; <CJK>
       (?睛 . "0x775B") ; <CJK>
       (?碇 . "0x7887") ; <CJK>
       (?禎 . "0x798E") ; <CJK>
       (?程 . "0x7A0B") ; <CJK>
       (?穽 . "0x7A7D") ; <CJK>
       (?精 . "0x7CBE") ; <CJK>
       (?綎 . "0x7D8E") ; <CJK>
       (?艇 . "0x8247") ; <CJK>
       (?訂 . "0x8A02") ; <CJK>
       (?諪 . "0x8AEA") ; <CJK>
       (?貞 . "0x8C9E") ; <CJK>
       (?鄭 . "0x912D") ; <CJK>
       (?酊 . "0x914A") ; <CJK>
       (?釘 . "0x91D8") ; <CJK>
       (?鉦 . "0x9266") ; <CJK>
       (?鋌 . "0x92CC") ; <CJK>
       (?錠 . "0x9320") ; <CJK>
       (?霆 . "0x9706") ; <CJK>
       (?靖 . "0x9756") ; <CJK>
       (?靜 . "0x975C") ; <CJK>
       (?頂 . "0x9802") ; <CJK>
       (?鼎 . "0x9F0E") ; <CJK>
       (?制 . "0x5236") ; <CJK>
       (?劑 . "0x5291") ; <CJK>
       (?啼 . "0x557C") ; <CJK>
       (?堤 . "0x5824") ; <CJK>
       (?帝 . "0x5E1D") ; <CJK>
       (?弟 . "0x5F1F") ; <CJK>
       (?悌 . "0x608C") ; <CJK>
       (?提 . "0x63D0") ; <CJK>
       (?梯 . "0x68AF") ; <CJK>
       (?濟 . "0x6FDF") ; <CJK>
       (?祭 . "0x796D") ; <CJK>
       (?第 . "0x7B2C") ; <CJK>
       (?臍 . "0x81CD") ; <CJK>
       (?薺 . "0x85BA") ; <CJK>
       (?製 . "0x88FD") ; <CJK>
       (?諸 . "0x8AF8") ; <CJK>
       (?蹄 . "0x8E44") ; <CJK>
       (?醍 . "0x918D") ; <CJK>
       (?除 . "0x9664") ; <CJK>
       (?際 . "0x969B") ; <CJK>
       (?霽 . "0x973D") ; <CJK>
       (?題 . "0x984C") ; <CJK>
       (?齊 . "0x9F4A") ; <CJK>
       (?俎 . "0x4FCE") ; <CJK>
       (?兆 . "0x5146") ; <CJK>
       (?凋 . "0x51CB") ; <CJK>
       (?助 . "0x52A9") ; <CJK>
       (?嘲 . "0x5632") ; <CJK>
       (?弔 . "0x5F14") ; <CJK>
       (?彫 . "0x5F6B") ; <CJK>
       (?措 . "0x63AA") ; <CJK>
       (?操 . "0x64CD") ; <CJK>
       (?早 . "0x65E9") ; <CJK>
       (?晁 . "0x6641") ; <CJK>
       (?曺 . "0x66FA") ; <CJK>
       (?曹 . "0x66F9") ; <CJK>
       (?朝 . "0x671D") ; <CJK>
       (?條 . "0x689D") ; <CJK>
       (?棗 . "0x68D7") ; <CJK>
       (?槽 . "0x69FD") ; <CJK>
       (?漕 . "0x6F15") ; <CJK>
       (?潮 . "0x6F6E") ; <CJK>
       (?照 . "0x7167") ; <CJK>
       (?燥 . "0x71E5") ; <CJK>
       (?爪 . "0x722A") ; <CJK>
       (?璪 . "0x74AA") ; <CJK>
       (?眺 . "0x773A") ; <CJK>
       (?祖 . "0x7956") ; <CJK>
       (?祚 . "0x795A") ; <CJK>
       (?租 . "0x79DF") ; <CJK>
       (?稠 . "0x7A20") ; <CJK>
       (?窕 . "0x7A95") ; <CJK>
       (?粗 . "0x7C97") ; <CJK>
       (?糟 . "0x7CDF") ; <CJK>
       (?組 . "0x7D44") ; <CJK>
       (?繰 . "0x7E70") ; <CJK>
       (?肇 . "0x8087") ; <CJK>
       (?藻 . "0x85FB") ; <CJK>
       (?蚤 . "0x86A4") ; <CJK>
       (?詔 . "0x8A54") ; <CJK>
       (?調 . "0x8ABF") ; <CJK>
       (?趙 . "0x8D99") ; <CJK>
       (?躁 . "0x8E81") ; <CJK>
       (?造 . "0x9020") ; <CJK>
       (?遭 . "0x906D") ; <CJK>
       (?釣 . "0x91E3") ; <CJK>
       (?阻 . "0x963B") ; <CJK>
       (?雕 . "0x96D5") ; <CJK>
       (?鳥 . "0x9CE5") ; <CJK>
       (?族 . "0x65CF") ; <CJK>
       (?簇 . "0x7C07") ; <CJK>
       (?足 . "0x8DB3") ; <CJK>
       (?鏃 . "0x93C3") ; <CJK>
       (?存 . "0x5B58") ; <CJK>
       (?尊 . "0x5C0A") ; <CJK>
       (?卒 . "0x5352") ; <CJK>
       (?拙 . "0x62D9") ; <CJK>
       (?猝 . "0x731D") ; <CJK>
       (?倧 . "0x5027") ; <CJK>
       (?宗 . "0x5B97") ; <CJK>
       (?從 . "0x5F9E") ; <CJK>
       (?悰 . "0x60B0") ; <CJK>
       (?慫 . "0x616B") ; <CJK>
       (?棕 . "0x68D5") ; <CJK>
       (?淙 . "0x6DD9") ; <CJK>
       (?琮 . "0x742E") ; <CJK>
       (?種 . "0x7A2E") ; <CJK>
       (?終 . "0x7D42") ; <CJK>
       (?綜 . "0x7D9C") ; <CJK>
       (?縱 . "0x7E31") ; <CJK>
       (?腫 . "0x816B") ; <CJK>
       (?踪 . "0x8E2A") ; <CJK>
       (?踵 . "0x8E35") ; <CJK>
       (?鍾 . "0x937E") ; <CJK>
       (?鐘 . "0x9418") ; <CJK>
       (?佐 . "0x4F50") ; <CJK>
       (?坐 . "0x5750") ; <CJK>
       (?左 . "0x5DE6") ; <CJK>
       (?座 . "0x5EA7") ; <CJK>
       (?挫 . "0x632B") ; <CJK>
       (?罪 . "0x7F6A") ; <CJK>
       (?主 . "0x4E3B") ; <CJK>
       (?住 . "0x4F4F") ; <CJK>
       (?侏 . "0x4F8F") ; <CJK>
       (?做 . "0x505A") ; <CJK>
       (?姝 . "0x59DD") ; <CJK>
       (?胄 . "0x80C4") ; <CJK>
       (?呪 . "0x546A") ; <CJK>
       (?周 . "0x5468") ; <CJK>
       (?嗾 . "0x55FE") ; <CJK>
       (?奏 . "0x594F") ; <CJK>
       (?宙 . "0x5B99") ; <CJK>
       (?州 . "0x5DDE") ; <CJK>
       (?廚 . "0x5EDA") ; <CJK>
       (?晝 . "0x665D") ; <CJK>
       (?朱 . "0x6731") ; <CJK>
       (?柱 . "0x67F1") ; <CJK>
       (?株 . "0x682A") ; <CJK>
       (?注 . "0x6CE8") ; <CJK>
       (?洲 . "0x6D32") ; <CJK>
       (?湊 . "0x6E4A") ; <CJK>
       (?澍 . "0x6F8D") ; <CJK>
       (?炷 . "0x70B7") ; <CJK>
       (?珠 . "0x73E0") ; <CJK>
       (?疇 . "0x7587") ; <CJK>
       (?籌 . "0x7C4C") ; <CJK>
       (?紂 . "0x7D02") ; <CJK>
       (?紬 . "0x7D2C") ; <CJK>
       (?綢 . "0x7DA2") ; <CJK>
       (?舟 . "0x821F") ; <CJK>
       (?蛛 . "0x86DB") ; <CJK>
       (?註 . "0x8A3B") ; <CJK>
       (?誅 . "0x8A85") ; <CJK>
       (?走 . "0x8D70") ; <CJK>
       (?躊 . "0x8E8A") ; <CJK>
       (?輳 . "0x8F33") ; <CJK>
       (?週 . "0x9031") ; <CJK>
       (?酎 . "0x914E") ; <CJK>
       (?酒 . "0x9152") ; <CJK>
       (?鑄 . "0x9444") ; <CJK>
       (?駐 . "0x99D0") ; <CJK>
       (?竹 . "0x7AF9") ; <CJK>
       (?粥 . "0x7CA5") ; <CJK>
       (?俊 . "0x4FCA") ; <CJK>
       (?儁 . "0x5101") ; <CJK>
       (?准 . "0x51C6") ; <CJK>
       (?埈 . "0x57C8") ; <CJK>
       (?寯 . "0x5BEF") ; <CJK>
       (?峻 . "0x5CFB") ; <CJK>
       (?晙 . "0x6659") ; <CJK>
       (?樽 . "0x6A3D") ; <CJK>
       (?浚 . "0x6D5A") ; <CJK>
       (?準 . "0x6E96") ; <CJK>
       (?濬 . "0x6FEC") ; <CJK>
       (?焌 . "0x710C") ; <CJK>
       (?畯 . "0x756F") ; <CJK>
       (?竣 . "0x7AE3") ; <CJK>
       (?蠢 . "0x8822") ; <CJK>
       (?逡 . "0x9021") ; <CJK>
       (?遵 . "0x9075") ; <CJK>
       (?雋 . "0x96CB") ; <CJK>
       (?駿 . "0x99FF") ; <CJK>
       (?茁 . "0x8301") ; <CJK>
       (?中 . "0x4E2D") ; <CJK>
       (?仲 . "0x4EF2") ; <CJK>
       (?衆 . "0x8846") ; <CJK>
       (?重 . "0x91CD") ; <CJK>
       (?卽 . "0x537D") ; <CJK>
       (?櫛 . "0x6ADB") ; <CJK>
       (?楫 . "0x696B") ; <CJK>
       (?汁 . "0x6C41") ; <CJK>
       (?葺 . "0x847A") ; <CJK>
       (?增 . "0x589E") ; <CJK>
       (?憎 . "0x618E") ; <CJK>
       (?曾 . "0x66FE") ; <CJK>
       (?拯 . "0x62EF") ; <CJK>
       (?烝 . "0x70DD") ; <CJK>
       (?甑 . "0x7511") ; <CJK>
       (?症 . "0x75C7") ; <CJK>
       (?繒 . "0x7E52") ; <CJK>
       (?蒸 . "0x84B8") ; <CJK>
       (?證 . "0x8B49") ; <CJK>
       (?贈 . "0x8D08") ; <CJK>
       (?之 . "0x4E4B") ; <CJK>
       (?只 . "0x53EA") ; <CJK>
       (?咫 . "0x54AB") ; <CJK>
       (?地 . "0x5730") ; <CJK>
       (?址 . "0x5740") ; <CJK>
       (?志 . "0x5FD7") ; <CJK>
       (?持 . "0x6301") ; <CJK>
       (?指 . "0x6307") ; <CJK>
       (?摯 . "0x646F") ; <CJK>
       (?支 . "0x652F") ; <CJK>
       (?旨 . "0x65E8") ; <CJK>
       (?智 . "0x667A") ; <CJK>
       (?枝 . "0x679D") ; <CJK>
       (?枳 . "0x67B3") ; <CJK>
       (?止 . "0x6B62") ; <CJK>
       (?池 . "0x6C60") ; <CJK>
       (?沚 . "0x6C9A") ; <CJK>
       (?漬 . "0x6F2C") ; <CJK>
       (?知 . "0x77E5") ; <CJK>
       (?砥 . "0x7825") ; <CJK>
       (?祉 . "0x7949") ; <CJK>
       (?祗 . "0x7957") ; <CJK>
       (?紙 . "0x7D19") ; <CJK>
       (?肢 . "0x80A2") ; <CJK>
       (?脂 . "0x8102") ; <CJK>
       (?至 . "0x81F3") ; <CJK>
       (?芝 . "0x829D") ; <CJK>
       (?芷 . "0x82B7") ; <CJK>
       (?蜘 . "0x8718") ; <CJK>
       (?誌 . "0x8A8C") ; <CJK>
       (?識 . "0xF9FC") ; <CJK>
       (?贄 . "0x8D04") ; <CJK>
       (?趾 . "0x8DBE") ; <CJK>
       (?遲 . "0x9072") ; <CJK>
       (?直 . "0x76F4") ; <CJK>
       (?稙 . "0x7A19") ; <CJK>
       (?稷 . "0x7A37") ; <CJK>
       (?織 . "0x7E54") ; <CJK>
       (?職 . "0x8077") ; <CJK>
       (?唇 . "0x5507") ; <CJK>
       (?嗔 . "0x55D4") ; <CJK>
       (?塵 . "0x5875") ; <CJK>
       (?振 . "0x632F") ; <CJK>
       (?搢 . "0x6422") ; <CJK>
       (?晉 . "0x6649") ; <CJK>
       (?晋 . "0x664B") ; <CJK>
       (?桭 . "0x686D") ; <CJK>
       (?榛 . "0x699B") ; <CJK>
       (?殄 . "0x6B84") ; <CJK>
       (?津 . "0x6D25") ; <CJK>
       (?溱 . "0x6EB1") ; <CJK>
       (?珍 . "0x73CD") ; <CJK>
       (?瑨 . "0x7468") ; <CJK>
       (?璡 . "0x74A1") ; <CJK>
       (?畛 . "0x755B") ; <CJK>
       (?疹 . "0x75B9") ; <CJK>
       (?盡 . "0x76E1") ; <CJK>
       (?眞 . "0x771E") ; <CJK>
       (?瞋 . "0x778B") ; <CJK>
       (?秦 . "0x79E6") ; <CJK>
       (?縉 . "0x7E09") ; <CJK>
       (?縝 . "0x7E1D") ; <CJK>
       (?臻 . "0x81FB") ; <CJK>
       (?蔯 . "0x852F") ; <CJK>
       (?袗 . "0x8897") ; <CJK>
       (?診 . "0x8A3A") ; <CJK>
       (?賑 . "0x8CD1") ; <CJK>
       (?軫 . "0x8EEB") ; <CJK>
       (?辰 . "0x8FB0") ; <CJK>
       (?進 . "0x9032") ; <CJK>
       (?鎭 . "0x93AD") ; <CJK>
       (?陣 . "0x9663") ; <CJK>
       (?陳 . "0x9673") ; <CJK>
       (?震 . "0x9707") ; <CJK>
       (?侄 . "0x4F84") ; <CJK>
       (?叱 . "0x53F1") ; <CJK>
       (?姪 . "0x59EA") ; <CJK>
       (?嫉 . "0x5AC9") ; <CJK>
       (?帙 . "0x5E19") ; <CJK>
       (?桎 . "0x684E") ; <CJK>
       (?瓆 . "0x74C6") ; <CJK>
       (?疾 . "0x75BE") ; <CJK>
       (?秩 . "0x79E9") ; <CJK>
       (?窒 . "0x7A92") ; <CJK>
       (?膣 . "0x81A3") ; <CJK>
       (?蛭 . "0x86ED") ; <CJK>
       (?質 . "0x8CEA") ; <CJK>
       (?跌 . "0x8DCC") ; <CJK>
       (?迭 . "0x8FED") ; <CJK>
       (?斟 . "0x659F") ; <CJK>
       (?朕 . "0x6715") ; <CJK>
       (?什 . "0xF9FD") ; <CJK>
       (?執 . "0x57F7") ; <CJK>
       (?潗 . "0x6F57") ; <CJK>
       (?緝 . "0x7DDD") ; <CJK>
       (?輯 . "0x8F2F") ; <CJK>
       (?鏶 . "0x93F6") ; <CJK>
       (?集 . "0x96C6") ; <CJK>
       (?徵 . "0x5FB5") ; <CJK>
       (?懲 . "0x61F2") ; <CJK>
       (?澄 . "0x6F84") ; <CJK>
       (?且 . "0x4E14") ; <CJK>
       (?侘 . "0x4F98") ; <CJK>
       (?借 . "0x501F") ; <CJK>
       (?叉 . "0x53C9") ; <CJK>
       (?嗟 . "0x55DF") ; <CJK>
       (?嵯 . "0x5D6F") ; <CJK>
       (?差 . "0x5DEE") ; <CJK>
       (?次 . "0x6B21") ; <CJK>
       (?此 . "0x6B64") ; <CJK>
       (?磋 . "0x78CB") ; <CJK>
       (?箚 . "0x7B9A") ; <CJK>
       (?茶 . "0xF9FE") ; <CJK>
       (?蹉 . "0x8E49") ; <CJK>
       (?車 . "0x8ECA") ; <CJK>
       (?遮 . "0x906E") ; <CJK>
       (?捉 . "0x6349") ; <CJK>
       (?搾 . "0x643E") ; <CJK>
       (?着 . "0x7740") ; <CJK>
       (?窄 . "0x7A84") ; <CJK>
       (?錯 . "0x932F") ; <CJK>
       (?鑿 . "0x947F") ; <CJK>
       (?齪 . "0x9F6A") ; <CJK>
       (?撰 . "0x64B0") ; <CJK>
       (?澯 . "0x6FAF") ; <CJK>
       (?燦 . "0x71E6") ; <CJK>
       (?璨 . "0x74A8") ; <CJK>
       (?瓚 . "0x74DA") ; <CJK>
       (?竄 . "0x7AC4") ; <CJK>
       (?簒 . "0x7C12") ; <CJK>
       (?纂 . "0x7E82") ; <CJK>
       (?粲 . "0x7CB2") ; <CJK>
       (?纘 . "0x7E98") ; <CJK>
       (?讚 . "0x8B9A") ; <CJK>
       (?贊 . "0x8D0A") ; <CJK>
       (?鑽 . "0x947D") ; <CJK>
       (?餐 . "0x9910") ; <CJK>
       (?饌 . "0x994C") ; <CJK>
       (?刹 . "0x5239") ; <CJK>
       (?察 . "0x5BDF") ; <CJK>
       (?擦 . "0x64E6") ; <CJK>
       (?札 . "0x672D") ; <CJK>
       (?紮 . "0x7D2E") ; <CJK>
       (?僭 . "0x50ED") ; <CJK>
       (?參 . "0x53C3") ; <CJK>
       (?塹 . "0x5879") ; <CJK>
       (?慘 . "0x6158") ; <CJK>
       (?慙 . "0x6159") ; <CJK>
       (?懺 . "0x61FA") ; <CJK>
       (?斬 . "0x65AC") ; <CJK>
       (?站 . "0x7AD9") ; <CJK>
       (?讒 . "0x8B92") ; <CJK>
       (?讖 . "0x8B96") ; <CJK>
       (?倉 . "0x5009") ; <CJK>
       (?倡 . "0x5021") ; <CJK>
       (?創 . "0x5275") ; <CJK>
       (?唱 . "0x5531") ; <CJK>
       (?娼 . "0x5A3C") ; <CJK>
       (?廠 . "0x5EE0") ; <CJK>
       (?彰 . "0x5F70") ; <CJK>
       (?愴 . "0x6134") ; <CJK>
       (?敞 . "0x655E") ; <CJK>
       (?昌 . "0x660C") ; <CJK>
       (?昶 . "0x6636") ; <CJK>
       (?暢 . "0x66A2") ; <CJK>
       (?槍 . "0x69CD") ; <CJK>
       (?滄 . "0x6EC4") ; <CJK>
       (?漲 . "0x6F32") ; <CJK>
       (?猖 . "0x7316") ; <CJK>
       (?瘡 . "0x7621") ; <CJK>
       (?窓 . "0x7A93") ; <CJK>
       (?脹 . "0x8139") ; <CJK>
       (?艙 . "0x8259") ; <CJK>
       (?菖 . "0x83D6") ; <CJK>
       (?蒼 . "0x84BC") ; <CJK>
       (?債 . "0x50B5") ; <CJK>
       (?埰 . "0x57F0") ; <CJK>
       (?寀 . "0x5BC0") ; <CJK>
       (?寨 . "0x5BE8") ; <CJK>
       (?彩 . "0x5F69") ; <CJK>
       (?採 . "0x63A1") ; <CJK>
       (?砦 . "0x7826") ; <CJK>
       (?綵 . "0x7DB5") ; <CJK>
       (?菜 . "0x83DC") ; <CJK>
       (?蔡 . "0x8521") ; <CJK>
       (?采 . "0x91C7") ; <CJK>
       (?釵 . "0x91F5") ; <CJK>
       (?冊 . "0x518A") ; <CJK>
       (?柵 . "0x67F5") ; <CJK>
       (?策 . "0x7B56") ; <CJK>
       (?責 . "0x8CAC") ; <CJK>
       (?凄 . "0x51C4") ; <CJK>
       (?妻 . "0x59BB") ; <CJK>
       (?悽 . "0x60BD") ; <CJK>
       (?處 . "0x8655") ; <CJK>
       (?倜 . "0x501C") ; <CJK>
       (?刺 . "0xF9FF") ; <CJK>
       (?剔 . "0x5254") ; <CJK>
       (?尺 . "0x5C3A") ; <CJK>
       (?慽 . "0x617D") ; <CJK>
       (?戚 . "0x621A") ; <CJK>
       (?拓 . "0x62D3") ; <CJK>
       (?擲 . "0x64F2") ; <CJK>
       (?斥 . "0x65A5") ; <CJK>
       (?滌 . "0x6ECC") ; <CJK>
       (?瘠 . "0x7620") ; <CJK>
       (?脊 . "0x810A") ; <CJK>
       (?蹠 . "0x8E60") ; <CJK>
       (?陟 . "0x965F") ; <CJK>
       (?隻 . "0x96BB") ; <CJK>
       (?仟 . "0x4EDF") ; <CJK>
       (?千 . "0x5343") ; <CJK>
       (?喘 . "0x5598") ; <CJK>
       (?天 . "0x5929") ; <CJK>
       (?川 . "0x5DDD") ; <CJK>
       (?擅 . "0x64C5") ; <CJK>
       (?泉 . "0x6CC9") ; <CJK>
       (?淺 . "0x6DFA") ; <CJK>
       (?玔 . "0x7394") ; <CJK>
       (?穿 . "0x7A7F") ; <CJK>
       (?舛 . "0x821B") ; <CJK>
       (?薦 . "0x85A6") ; <CJK>
       (?賤 . "0x8CE4") ; <CJK>
       (?踐 . "0x8E10") ; <CJK>
       (?遷 . "0x9077") ; <CJK>
       (?釧 . "0x91E7") ; <CJK>
       (?闡 . "0x95E1") ; <CJK>
       (?阡 . "0x9621") ; <CJK>
       (?韆 . "0x97C6") ; <CJK>
       (?凸 . "0x51F8") ; <CJK>
       (?哲 . "0x54F2") ; <CJK>
       (?喆 . "0x5586") ; <CJK>
       (?徹 . "0x5FB9") ; <CJK>
       (?撤 . "0x64A4") ; <CJK>
       (?澈 . "0x6F88") ; <CJK>
       (?綴 . "0x7DB4") ; <CJK>
       (?輟 . "0x8F1F") ; <CJK>
       (?轍 . "0x8F4D") ; <CJK>
       (?鐵 . "0x9435") ; <CJK>
       (?僉 . "0x50C9") ; <CJK>
       (?尖 . "0x5C16") ; <CJK>
       (?沾 . "0x6CBE") ; <CJK>
       (?添 . "0x6DFB") ; <CJK>
       (?甛 . "0x751B") ; <CJK>
       (?瞻 . "0x77BB") ; <CJK>
       (?簽 . "0x7C3D") ; <CJK>
       (?籤 . "0x7C64") ; <CJK>
       (?詹 . "0x8A79") ; <CJK>
       (?諂 . "0x8AC2") ; <CJK>
       (?堞 . "0x581E") ; <CJK>
       (?妾 . "0x59BE") ; <CJK>
       (?帖 . "0x5E16") ; <CJK>
       (?捷 . "0x6377") ; <CJK>
       (?牒 . "0x7252") ; <CJK>
       (?疊 . "0x758A") ; <CJK>
       (?睫 . "0x776B") ; <CJK>
       (?諜 . "0x8ADC") ; <CJK>
       (?貼 . "0x8CBC") ; <CJK>
       (?輒 . "0x8F12") ; <CJK>
       (?廳 . "0x5EF3") ; <CJK>
       (?晴 . "0x6674") ; <CJK>
       (?淸 . "0x6DF8") ; <CJK>
       (?聽 . "0x807D") ; <CJK>
       (?菁 . "0x83C1") ; <CJK>
       (?請 . "0x8ACB") ; <CJK>
       (?靑 . "0x9751") ; <CJK>
       (?鯖 . "0x9BD6") ; <CJK>
       (?切 . "0xFA00") ; <CJK>
       (?剃 . "0x5243") ; <CJK>
       (?替 . "0x66FF") ; <CJK>
       (?涕 . "0x6D95") ; <CJK>
       (?滯 . "0x6EEF") ; <CJK>
       (?締 . "0x7DE0") ; <CJK>
       (?諦 . "0x8AE6") ; <CJK>
       (?逮 . "0x902E") ; <CJK>
       (?遞 . "0x905E") ; <CJK>
       (?體 . "0x9AD4") ; <CJK>
       (?初 . "0x521D") ; <CJK>
       (?剿 . "0x527F") ; <CJK>
       (?哨 . "0x54E8") ; <CJK>
       (?憔 . "0x6194") ; <CJK>
       (?抄 . "0x6284") ; <CJK>
       (?招 . "0x62DB") ; <CJK>
       (?梢 . "0x68A2") ; <CJK>
       (?椒 . "0x6912") ; <CJK>
       (?楚 . "0x695A") ; <CJK>
       (?樵 . "0x6A35") ; <CJK>
       (?炒 . "0x7092") ; <CJK>
       (?焦 . "0x7126") ; <CJK>
       (?硝 . "0x785D") ; <CJK>
       (?礁 . "0x7901") ; <CJK>
       (?礎 . "0x790E") ; <CJK>
       (?秒 . "0x79D2") ; <CJK>
       (?稍 . "0x7A0D") ; <CJK>
       (?肖 . "0x8096") ; <CJK>
       (?艸 . "0x8278") ; <CJK>
       (?苕 . "0x82D5") ; <CJK>
       (?草 . "0x8349") ; <CJK>
       (?蕉 . "0x8549") ; <CJK>
       (?貂 . "0x8C82") ; <CJK>
       (?超 . "0x8D85") ; <CJK>
       (?酢 . "0x9162") ; <CJK>
       (?醋 . "0x918B") ; <CJK>
       (?醮 . "0x91AE") ; <CJK>
       (?促 . "0x4FC3") ; <CJK>
       (?囑 . "0x56D1") ; <CJK>
       (?燭 . "0x71ED") ; <CJK>
       (?矗 . "0x77D7") ; <CJK>
       (?蜀 . "0x8700") ; <CJK>
       (?觸 . "0x89F8") ; <CJK>
       (?寸 . "0x5BF8") ; <CJK>
       (?忖 . "0x5FD6") ; <CJK>
       (?村 . "0x6751") ; <CJK>
       (?邨 . "0x90A8") ; <CJK>
       (?叢 . "0x53E2") ; <CJK>
       (?塚 . "0x585A") ; <CJK>
       (?寵 . "0x5BF5") ; <CJK>
       (?悤 . "0x60A4") ; <CJK>
       (?憁 . "0x6181") ; <CJK>
       (?摠 . "0x6460") ; <CJK>
       (?總 . "0x7E3D") ; <CJK>
       (?聰 . "0x8070") ; <CJK>
       (?蔥 . "0x8525") ; <CJK>
       (?銃 . "0x9283") ; <CJK>
       (?撮 . "0x64AE") ; <CJK>
       (?催 . "0x50AC") ; <CJK>
       (?崔 . "0x5D14") ; <CJK>
       (?最 . "0x6700") ; <CJK>
       (?墜 . "0x589C") ; <CJK>
       (?抽 . "0x62BD") ; <CJK>
       (?推 . "0x63A8") ; <CJK>
       (?椎 . "0x690E") ; <CJK>
       (?楸 . "0x6978") ; <CJK>
       (?樞 . "0x6A1E") ; <CJK>
       (?湫 . "0x6E6B") ; <CJK>
       (?皺 . "0x76BA") ; <CJK>
       (?秋 . "0x79CB") ; <CJK>
       (?芻 . "0x82BB") ; <CJK>
       (?萩 . "0x8429") ; <CJK>
       (?諏 . "0x8ACF") ; <CJK>
       (?趨 . "0x8DA8") ; <CJK>
       (?追 . "0x8FFD") ; <CJK>
       (?鄒 . "0x9112") ; <CJK>
       (?酋 . "0x914B") ; <CJK>
       (?醜 . "0x919C") ; <CJK>
       (?錐 . "0x9310") ; <CJK>
       (?錘 . "0x9318") ; <CJK>
       (?鎚 . "0x939A") ; <CJK>
       (?雛 . "0x96DB") ; <CJK>
       (?騶 . "0x9A36") ; <CJK>
       (?鰍 . "0x9C0D") ; <CJK>
       (?丑 . "0x4E11") ; <CJK>
       (?畜 . "0x755C") ; <CJK>
       (?祝 . "0x795D") ; <CJK>
       (?竺 . "0x7AFA") ; <CJK>
       (?筑 . "0x7B51") ; <CJK>
       (?築 . "0x7BC9") ; <CJK>
       (?縮 . "0x7E2E") ; <CJK>
       (?蓄 . "0x84C4") ; <CJK>
       (?蹙 . "0x8E59") ; <CJK>
       (?蹴 . "0x8E74") ; <CJK>
       (?軸 . "0x8EF8") ; <CJK>
       (?逐 . "0x9010") ; <CJK>
       (?春 . "0x6625") ; <CJK>
       (?椿 . "0x693F") ; <CJK>
       (?瑃 . "0x7443") ; <CJK>
       (?出 . "0x51FA") ; <CJK>
       (?朮 . "0x672E") ; <CJK>
       (?黜 . "0x9EDC") ; <CJK>
       (?充 . "0x5145") ; <CJK>
       (?忠 . "0x5FE0") ; <CJK>
       (?沖 . "0x6C96") ; <CJK>
       (?蟲 . "0x87F2") ; <CJK>
       (?衝 . "0x885D") ; <CJK>
       (?衷 . "0x8877") ; <CJK>
       (?悴 . "0x60B4") ; <CJK>
       (?膵 . "0x81B5") ; <CJK>
       (?萃 . "0x8403") ; <CJK>
       (?贅 . "0x8D05") ; <CJK>
       (?取 . "0x53D6") ; <CJK>
       (?吹 . "0x5439") ; <CJK>
       (?嘴 . "0x5634") ; <CJK>
       (?娶 . "0x5A36") ; <CJK>
       (?就 . "0x5C31") ; <CJK>
       (?炊 . "0x708A") ; <CJK>
       (?翠 . "0x7FE0") ; <CJK>
       (?聚 . "0x805A") ; <CJK>
       (?脆 . "0x8106") ; <CJK>
       (?臭 . "0x81ED") ; <CJK>
       (?趣 . "0x8DA3") ; <CJK>
       (?醉 . "0x9189") ; <CJK>
       (?驟 . "0x9A5F") ; <CJK>
       (?鷲 . "0x9DF2") ; <CJK>
       (?側 . "0x5074") ; <CJK>
       (?仄 . "0x4EC4") ; <CJK>
       (?厠 . "0x53A0") ; <CJK>
       (?惻 . "0x60FB") ; <CJK>
       (?測 . "0x6E2C") ; <CJK>
       (?層 . "0x5C64") ; <CJK>
       (?侈 . "0x4F88") ; <CJK>
       (?値 . "0x5024") ; <CJK>
       (?嗤 . "0x55E4") ; <CJK>
       (?峙 . "0x5CD9") ; <CJK>
       (?幟 . "0x5E5F") ; <CJK>
       (?恥 . "0x6065") ; <CJK>
       (?梔 . "0x6894") ; <CJK>
       (?治 . "0x6CBB") ; <CJK>
       (?淄 . "0x6DC4") ; <CJK>
       (?熾 . "0x71BE") ; <CJK>
       (?痔 . "0x75D4") ; <CJK>
       (?痴 . "0x75F4") ; <CJK>
       (?癡 . "0x7661") ; <CJK>
       (?稚 . "0x7A1A") ; <CJK>
       (?穉 . "0x7A49") ; <CJK>
       (?緇 . "0x7DC7") ; <CJK>
       (?緻 . "0x7DFB") ; <CJK>
       (?置 . "0x7F6E") ; <CJK>
       (?致 . "0x81F4") ; <CJK>
       (?蚩 . "0x86A9") ; <CJK>
       (?輜 . "0x8F1C") ; <CJK>
       (?雉 . "0x96C9") ; <CJK>
       (?馳 . "0x99B3") ; <CJK>
       (?齒 . "0x9F52") ; <CJK>
       (?則 . "0x5247") ; <CJK>
       (?勅 . "0x52C5") ; <CJK>
       (?飭 . "0x98ED") ; <CJK>
       (?親 . "0x89AA") ; <CJK>
       (?七 . "0x4E03") ; <CJK>
       (?柒 . "0x67D2") ; <CJK>
       (?漆 . "0x6F06") ; <CJK>
       (?侵 . "0x4FB5") ; <CJK>
       (?寢 . "0x5BE2") ; <CJK>
       (?枕 . "0x6795") ; <CJK>
       (?沈 . "0x6C88") ; <CJK>
       (?浸 . "0x6D78") ; <CJK>
       (?琛 . "0x741B") ; <CJK>
       (?砧 . "0x7827") ; <CJK>
       (?針 . "0x91DD") ; <CJK>
       (?鍼 . "0x937C") ; <CJK>
       (?蟄 . "0x87C4") ; <CJK>
       (?秤 . "0x79E4") ; <CJK>
       (?稱 . "0x7A31") ; <CJK>
       (?快 . "0x5FEB") ; <CJK>
       (?他 . "0x4ED6") ; <CJK>
       (?咤 . "0x54A4") ; <CJK>
       (?唾 . "0x553E") ; <CJK>
       (?墮 . "0x58AE") ; <CJK>
       (?妥 . "0x59A5") ; <CJK>
       (?惰 . "0x60F0") ; <CJK>
       (?打 . "0x6253") ; <CJK>
       (?拖 . "0x62D6") ; <CJK>
       (?朶 . "0x6736") ; <CJK>
       (?楕 . "0x6955") ; <CJK>
       (?舵 . "0x8235") ; <CJK>
       (?陀 . "0x9640") ; <CJK>
       (?馱 . "0x99B1") ; <CJK>
       (?駝 . "0x99DD") ; <CJK>
       (?倬 . "0x502C") ; <CJK>
       (?卓 . "0x5353") ; <CJK>
       (?啄 . "0x5544") ; <CJK>
       (?坼 . "0x577C") ; <CJK>
       (?度 . "0xFA01") ; <CJK>
       (?托 . "0x6258") ; <CJK>
       (?拓 . "0xFA02") ; <CJK>
       (?擢 . "0x64E2") ; <CJK>
       (?晫 . "0x666B") ; <CJK>
       (?柝 . "0x67DD") ; <CJK>
       (?濁 . "0x6FC1") ; <CJK>
       (?濯 . "0x6FEF") ; <CJK>
       (?琢 . "0x7422") ; <CJK>
       (?琸 . "0x7438") ; <CJK>
       (?託 . "0x8A17") ; <CJK>
       (?鐸 . "0x9438") ; <CJK>
       (?呑 . "0x5451") ; <CJK>
       (?嘆 . "0x5606") ; <CJK>
       (?坦 . "0x5766") ; <CJK>
       (?彈 . "0x5F48") ; <CJK>
       (?憚 . "0x619A") ; <CJK>
       (?歎 . "0x6B4E") ; <CJK>
       (?灘 . "0x7058") ; <CJK>
       (?炭 . "0x70AD") ; <CJK>
       (?綻 . "0x7DBB") ; <CJK>
       (?誕 . "0x8A95") ; <CJK>
       (?奪 . "0x596A") ; <CJK>
       (?脫 . "0x812B") ; <CJK>
       (?探 . "0x63A2") ; <CJK>
       (?眈 . "0x7708") ; <CJK>
       (?耽 . "0x803D") ; <CJK>
       (?貪 . "0x8CAA") ; <CJK>
       (?塔 . "0x5854") ; <CJK>
       (?搭 . "0x642D") ; <CJK>
       (?榻 . "0x69BB") ; <CJK>
       (?宕 . "0x5B95") ; <CJK>
       (?帑 . "0x5E11") ; <CJK>
       (?湯 . "0x6E6F") ; <CJK>
       (?糖 . "0xFA03") ; <CJK>
       (?蕩 . "0x8569") ; <CJK>
       (?兌 . "0x514C") ; <CJK>
       (?台 . "0x53F0") ; <CJK>
       (?太 . "0x592A") ; <CJK>
       (?怠 . "0x6020") ; <CJK>
       (?態 . "0x614B") ; <CJK>
       (?殆 . "0x6B86") ; <CJK>
       (?汰 . "0x6C70") ; <CJK>
       (?泰 . "0x6CF0") ; <CJK>
       (?笞 . "0x7B1E") ; <CJK>
       (?胎 . "0x80CE") ; <CJK>
       (?苔 . "0x82D4") ; <CJK>
       (?跆 . "0x8DC6") ; <CJK>
       (?邰 . "0x90B0") ; <CJK>
       (?颱 . "0x98B1") ; <CJK>
       (?宅 . "0xFA04") ; <CJK>
       (?擇 . "0x64C7") ; <CJK>
       (?澤 . "0x6FA4") ; <CJK>
       (?撑 . "0x6491") ; <CJK>
       (?攄 . "0x6504") ; <CJK>
       (?兎 . "0x514E") ; <CJK>
       (?吐 . "0x5410") ; <CJK>
       (?土 . "0x571F") ; <CJK>
       (?討 . "0x8A0E") ; <CJK>
       (?慟 . "0x615F") ; <CJK>
       (?桶 . "0x6876") ; <CJK>
       (?洞 . "0xFA05") ; <CJK>
       (?痛 . "0x75DB") ; <CJK>
       (?筒 . "0x7B52") ; <CJK>
       (?統 . "0x7D71") ; <CJK>
       (?通 . "0x901A") ; <CJK>
       (?堆 . "0x5806") ; <CJK>
       (?槌 . "0x69CC") ; <CJK>
       (?腿 . "0x817F") ; <CJK>
       (?褪 . "0x892A") ; <CJK>
       (?退 . "0x9000") ; <CJK>
       (?頹 . "0x9839") ; <CJK>
       (?偸 . "0x5078") ; <CJK>
       (?套 . "0x5957") ; <CJK>
       (?妬 . "0x59AC") ; <CJK>
       (?投 . "0x6295") ; <CJK>
       (?透 . "0x900F") ; <CJK>
       (?鬪 . "0x9B2A") ; <CJK>
       (?慝 . "0x615D") ; <CJK>
       (?特 . "0x7279") ; <CJK>
       (?闖 . "0x95D6") ; <CJK>
       (?坡 . "0x5761") ; <CJK>
       (?婆 . "0x5A46") ; <CJK>
       (?巴 . "0x5DF4") ; <CJK>
       (?把 . "0x628A") ; <CJK>
       (?播 . "0x64AD") ; <CJK>
       (?擺 . "0x64FA") ; <CJK>
       (?杷 . "0x6777") ; <CJK>
       (?波 . "0x6CE2") ; <CJK>
       (?派 . "0x6D3E") ; <CJK>
       (?爬 . "0x722C") ; <CJK>
       (?琶 . "0x7436") ; <CJK>
       (?破 . "0x7834") ; <CJK>
       (?罷 . "0x7F77") ; <CJK>
       (?芭 . "0x82AD") ; <CJK>
       (?跛 . "0x8DDB") ; <CJK>
       (?頗 . "0x9817") ; <CJK>
       (?判 . "0x5224") ; <CJK>
       (?坂 . "0x5742") ; <CJK>
       (?板 . "0x677F") ; <CJK>
       (?版 . "0x7248") ; <CJK>
       (?瓣 . "0x74E3") ; <CJK>
       (?販 . "0x8CA9") ; <CJK>
       (?辦 . "0x8FA6") ; <CJK>
       (?鈑 . "0x9211") ; <CJK>
       (?阪 . "0x962A") ; <CJK>
       (?八 . "0x516B") ; <CJK>
       (?叭 . "0x53ED") ; <CJK>
       (?捌 . "0x634C") ; <CJK>
       (?佩 . "0x4F69") ; <CJK>
       (?唄 . "0x5504") ; <CJK>
       (?悖 . "0x6096") ; <CJK>
       (?敗 . "0x6557") ; <CJK>
       (?沛 . "0x6C9B") ; <CJK>
       (?浿 . "0x6D7F") ; <CJK>
       (?牌 . "0x724C") ; <CJK>
       (?狽 . "0x72FD") ; <CJK>
       (?稗 . "0x7A17") ; <CJK>
       (?覇 . "0x8987") ; <CJK>
       (?貝 . "0x8C9D") ; <CJK>
       (?彭 . "0x5F6D") ; <CJK>
       (?澎 . "0x6F8E") ; <CJK>
       (?烹 . "0x70F9") ; <CJK>
       (?膨 . "0x81A8") ; <CJK>
       (?愎 . "0x610E") ; <CJK>
       (?便 . "0x4FBF") ; <CJK>
       (?偏 . "0x504F") ; <CJK>
       (?扁 . "0x6241") ; <CJK>
       (?片 . "0x7247") ; <CJK>
       (?篇 . "0x7BC7") ; <CJK>
       (?編 . "0x7DE8") ; <CJK>
       (?翩 . "0x7FE9") ; <CJK>
       (?遍 . "0x904D") ; <CJK>
       (?鞭 . "0x97AD") ; <CJK>
       (?騙 . "0x9A19") ; <CJK>
       (?貶 . "0x8CB6") ; <CJK>
       (?坪 . "0x576A") ; <CJK>
       (?平 . "0x5E73") ; <CJK>
       (?枰 . "0x67B0") ; <CJK>
       (?萍 . "0x840D") ; <CJK>
       (?評 . "0x8A55") ; <CJK>
       (?吠 . "0x5420") ; <CJK>
       (?嬖 . "0x5B16") ; <CJK>
       (?幣 . "0x5E63") ; <CJK>
       (?廢 . "0x5EE2") ; <CJK>
       (?弊 . "0x5F0A") ; <CJK>
       (?斃 . "0x6583") ; <CJK>
       (?肺 . "0x80BA") ; <CJK>
       (?蔽 . "0x853D") ; <CJK>
       (?閉 . "0x9589") ; <CJK>
       (?陛 . "0x965B") ; <CJK>
       (?佈 . "0x4F48") ; <CJK>
       (?包 . "0x5305") ; <CJK>
       (?匍 . "0x530D") ; <CJK>
       (?匏 . "0x530F") ; <CJK>
       (?咆 . "0x5486") ; <CJK>
       (?哺 . "0x54FA") ; <CJK>
       (?圃 . "0x5703") ; <CJK>
       (?布 . "0x5E03") ; <CJK>
       (?怖 . "0x6016") ; <CJK>
       (?抛 . "0x629B") ; <CJK>
       (?抱 . "0x62B1") ; <CJK>
       (?捕 . "0x6355") ; <CJK>
       (?暴 . "0xFA06") ; <CJK>
       (?泡 . "0x6CE1") ; <CJK>
       (?浦 . "0x6D66") ; <CJK>
       (?疱 . "0x75B1") ; <CJK>
       (?砲 . "0x7832") ; <CJK>
       (?胞 . "0x80DE") ; <CJK>
       (?脯 . "0x812F") ; <CJK>
       (?苞 . "0x82DE") ; <CJK>
       (?葡 . "0x8461") ; <CJK>
       (?蒲 . "0x84B2") ; <CJK>
       (?袍 . "0x888D") ; <CJK>
       (?褒 . "0x8912") ; <CJK>
       (?逋 . "0x900B") ; <CJK>
       (?鋪 . "0x92EA") ; <CJK>
       (?飽 . "0x98FD") ; <CJK>
       (?鮑 . "0x9B91") ; <CJK>
       (?幅 . "0x5E45") ; <CJK>
       (?暴 . "0x66B4") ; <CJK>
       (?曝 . "0x66DD") ; <CJK>
       (?瀑 . "0x7011") ; <CJK>
       (?爆 . "0x7206") ; <CJK>
       (?輻 . "0xFA07") ; <CJK>
       (?俵 . "0x4FF5") ; <CJK>
       (?剽 . "0x527D") ; <CJK>
       (?彪 . "0x5F6A") ; <CJK>
       (?慓 . "0x6153") ; <CJK>
       (?杓 . "0x6753") ; <CJK>
       (?標 . "0x6A19") ; <CJK>
       (?漂 . "0x6F02") ; <CJK>
       (?瓢 . "0x74E2") ; <CJK>
       (?票 . "0x7968") ; <CJK>
       (?表 . "0x8868") ; <CJK>
       (?豹 . "0x8C79") ; <CJK>
       (?飇 . "0x98C7") ; <CJK>
       (?飄 . "0x98C4") ; <CJK>
       (?驃 . "0x9A43") ; <CJK>
       (?品 . "0x54C1") ; <CJK>
       (?稟 . "0x7A1F") ; <CJK>
       (?楓 . "0x6953") ; <CJK>
       (?諷 . "0x8AF7") ; <CJK>
       (?豊 . "0x8C4A") ; <CJK>
       (?風 . "0x98A8") ; <CJK>
       (?馮 . "0x99AE") ; <CJK>
       (?彼 . "0x5F7C") ; <CJK>
       (?披 . "0x62AB") ; <CJK>
       (?疲 . "0x75B2") ; <CJK>
       (?皮 . "0x76AE") ; <CJK>
       (?被 . "0x88AB") ; <CJK>
       (?避 . "0x907F") ; <CJK>
       (?陂 . "0x9642") ; <CJK>
       (?匹 . "0x5339") ; <CJK>
       (?弼 . "0x5F3C") ; <CJK>
       (?必 . "0x5FC5") ; <CJK>
       (?泌 . "0x6CCC") ; <CJK>
       (?珌 . "0x73CC") ; <CJK>
       (?畢 . "0x7562") ; <CJK>
       (?疋 . "0x758B") ; <CJK>
       (?筆 . "0x7B46") ; <CJK>
       (?苾 . "0x82FE") ; <CJK>
       (?馝 . "0x999D") ; <CJK>
       (?乏 . "0x4E4F") ; <CJK>
       (?逼 . "0x903C") ; <CJK>
       (?下 . "0x4E0B") ; <CJK>
       (?何 . "0x4F55") ; <CJK>
       (?厦 . "0x53A6") ; <CJK>
       (?夏 . "0x590F") ; <CJK>
       (?廈 . "0x5EC8") ; <CJK>
       (?昰 . "0x6630") ; <CJK>
       (?河 . "0x6CB3") ; <CJK>
       (?瑕 . "0x7455") ; <CJK>
       (?荷 . "0x8377") ; <CJK>
       (?蝦 . "0x8766") ; <CJK>
       (?賀 . "0x8CC0") ; <CJK>
       (?遐 . "0x9050") ; <CJK>
       (?霞 . "0x971E") ; <CJK>
       (?鰕 . "0x9C15") ; <CJK>
       (?壑 . "0x58D1") ; <CJK>
       (?學 . "0x5B78") ; <CJK>
       (?虐 . "0x8650") ; <CJK>
       (?謔 . "0x8B14") ; <CJK>
       (?鶴 . "0x9DB4") ; <CJK>
       (?寒 . "0x5BD2") ; <CJK>
       (?恨 . "0x6068") ; <CJK>
       (?悍 . "0x608D") ; <CJK>
       (?旱 . "0x65F1") ; <CJK>
       (?汗 . "0x6C57") ; <CJK>
       (?漢 . "0x6F22") ; <CJK>
       (?澣 . "0x6FA3") ; <CJK>
       (?瀚 . "0x701A") ; <CJK>
       (?罕 . "0x7F55") ; <CJK>
       (?翰 . "0x7FF0") ; <CJK>
       (?閑 . "0x9591") ; <CJK>
       (?閒 . "0x9592") ; <CJK>
       (?限 . "0x9650") ; <CJK>
       (?韓 . "0x97D3") ; <CJK>
       (?割 . "0x5272") ; <CJK>
       (?轄 . "0x8F44") ; <CJK>
       (?函 . "0x51FD") ; <CJK>
       (?含 . "0x542B") ; <CJK>
       (?咸 . "0x54B8") ; <CJK>
       (?啣 . "0x5563") ; <CJK>
       (?喊 . "0x558A") ; <CJK>
       (?檻 . "0x6ABB") ; <CJK>
       (?涵 . "0x6DB5") ; <CJK>
       (?緘 . "0x7DD8") ; <CJK>
       (?艦 . "0x8266") ; <CJK>
       (?銜 . "0x929C") ; <CJK>
       (?陷 . "0x9677") ; <CJK>
       (?鹹 . "0x9E79") ; <CJK>
       (?合 . "0x5408") ; <CJK>
       (?哈 . "0x54C8") ; <CJK>
       (?盒 . "0x76D2") ; <CJK>
       (?蛤 . "0x86E4") ; <CJK>
       (?閤 . "0x95A4") ; <CJK>
       (?闔 . "0x95D4") ; <CJK>
       (?陜 . "0x965C") ; <CJK>
       (?亢 . "0x4EA2") ; <CJK>
       (?伉 . "0x4F09") ; <CJK>
       (?姮 . "0x59EE") ; <CJK>
       (?嫦 . "0x5AE6") ; <CJK>
       (?巷 . "0x5DF7") ; <CJK>
       (?恒 . "0x6052") ; <CJK>
       (?抗 . "0x6297") ; <CJK>
       (?杭 . "0x676D") ; <CJK>
       (?桁 . "0x6841") ; <CJK>
       (?沆 . "0x6C86") ; <CJK>
       (?港 . "0x6E2F") ; <CJK>
       (?缸 . "0x7F38") ; <CJK>
       (?肛 . "0x809B") ; <CJK>
       (?航 . "0x822A") ; <CJK>
       (?行 . "0xFA08") ; <CJK>
       (?降 . "0xFA09") ; <CJK>
       (?項 . "0x9805") ; <CJK>
       (?亥 . "0x4EA5") ; <CJK>
       (?偕 . "0x5055") ; <CJK>
       (?咳 . "0x54B3") ; <CJK>
       (?垓 . "0x5793") ; <CJK>
       (?奚 . "0x595A") ; <CJK>
       (?孩 . "0x5B69") ; <CJK>
       (?害 . "0x5BB3") ; <CJK>
       (?懈 . "0x61C8") ; <CJK>
       (?楷 . "0x6977") ; <CJK>
       (?海 . "0x6D77") ; <CJK>
       (?瀣 . "0x7023") ; <CJK>
       (?蟹 . "0x87F9") ; <CJK>
       (?解 . "0x89E3") ; <CJK>
       (?該 . "0x8A72") ; <CJK>
       (?諧 . "0x8AE7") ; <CJK>
       (?邂 . "0x9082") ; <CJK>
       (?駭 . "0x99ED") ; <CJK>
       (?骸 . "0x9AB8") ; <CJK>
       (?劾 . "0x52BE") ; <CJK>
       (?核 . "0x6838") ; <CJK>
       (?倖 . "0x5016") ; <CJK>
       (?幸 . "0x5E78") ; <CJK>
       (?杏 . "0x674F") ; <CJK>
       (?荇 . "0x8347") ; <CJK>
       (?行 . "0x884C") ; <CJK>
       (?享 . "0x4EAB") ; <CJK>
       (?向 . "0x5411") ; <CJK>
       (?嚮 . "0x56AE") ; <CJK>
       (?珦 . "0x73E6") ; <CJK>
       (?鄕 . "0x9115") ; <CJK>
       (?響 . "0x97FF") ; <CJK>
       (?餉 . "0x9909") ; <CJK>
       (?饗 . "0x9957") ; <CJK>
       (?香 . "0x9999") ; <CJK>
       (?噓 . "0x5653") ; <CJK>
       (?墟 . "0x589F") ; <CJK>
       (?虛 . "0x865B") ; <CJK>
       (?許 . "0x8A31") ; <CJK>
       (?憲 . "0x61B2") ; <CJK>
       (?櫶 . "0x6AF6") ; <CJK>
       (?獻 . "0x737B") ; <CJK>
       (?軒 . "0x8ED2") ; <CJK>
       (?歇 . "0x6B47") ; <CJK>
       (?險 . "0x96AA") ; <CJK>
       (?驗 . "0x9A57") ; <CJK>
       (?奕 . "0x5955") ; <CJK>
       (?爀 . "0x7200") ; <CJK>
       (?赫 . "0x8D6B") ; <CJK>
       (?革 . "0x9769") ; <CJK>
       (?俔 . "0x4FD4") ; <CJK>
       (?峴 . "0x5CF4") ; <CJK>
       (?弦 . "0x5F26") ; <CJK>
       (?懸 . "0x61F8") ; <CJK>
       (?晛 . "0x665B") ; <CJK>
       (?泫 . "0x6CEB") ; <CJK>
       (?炫 . "0x70AB") ; <CJK>
       (?玄 . "0x7384") ; <CJK>
       (?玹 . "0x73B9") ; <CJK>
       (?現 . "0x73FE") ; <CJK>
       (?眩 . "0x7729") ; <CJK>
       (?睍 . "0x774D") ; <CJK>
       (?絃 . "0x7D43") ; <CJK>
       (?絢 . "0x7D62") ; <CJK>
       (?縣 . "0x7E23") ; <CJK>
       (?舷 . "0x8237") ; <CJK>
       (?衒 . "0x8852") ; <CJK>
       (?見 . "0xFA0A") ; <CJK>
       (?賢 . "0x8CE2") ; <CJK>
       (?鉉 . "0x9249") ; <CJK>
       (?顯 . "0x986F") ; <CJK>
       (?孑 . "0x5B51") ; <CJK>
       (?穴 . "0x7A74") ; <CJK>
       (?血 . "0x8840") ; <CJK>
       (?頁 . "0x9801") ; <CJK>
       (?嫌 . "0x5ACC") ; <CJK>
       (?俠 . "0x4FE0") ; <CJK>
       (?協 . "0x5354") ; <CJK>
       (?夾 . "0x593E") ; <CJK>
       (?峽 . "0x5CFD") ; <CJK>
       (?挾 . "0x633E") ; <CJK>
       (?浹 . "0x6D79") ; <CJK>
       (?狹 . "0x72F9") ; <CJK>
       (?脅 . "0x8105") ; <CJK>
       (?脇 . "0x8107") ; <CJK>
       (?莢 . "0x83A2") ; <CJK>
       (?鋏 . "0x92CF") ; <CJK>
       (?頰 . "0x9830") ; <CJK>
       (?亨 . "0x4EA8") ; <CJK>
       (?兄 . "0x5144") ; <CJK>
       (?刑 . "0x5211") ; <CJK>
       (?型 . "0x578B") ; <CJK>
       (?形 . "0x5F62") ; <CJK>
       (?泂 . "0x6CC2") ; <CJK>
       (?滎 . "0x6ECE") ; <CJK>
       (?瀅 . "0x7005") ; <CJK>
       (?灐 . "0x7050") ; <CJK>
       (?炯 . "0x70AF") ; <CJK>
       (?熒 . "0x7192") ; <CJK>
       (?珩 . "0x73E9") ; <CJK>
       (?瑩 . "0x7469") ; <CJK>
       (?荊 . "0x834A") ; <CJK>
       (?螢 . "0x87A2") ; <CJK>
       (?衡 . "0x8861") ; <CJK>
       (?逈 . "0x9008") ; <CJK>
       (?邢 . "0x90A2") ; <CJK>
       (?鎣 . "0x93A3") ; <CJK>
       (?馨 . "0x99A8") ; <CJK>
       (?兮 . "0x516E") ; <CJK>
       (?彗 . "0x5F57") ; <CJK>
       (?惠 . "0x60E0") ; <CJK>
       (?慧 . "0x6167") ; <CJK>
       (?暳 . "0x66B3") ; <CJK>
       (?蕙 . "0x8559") ; <CJK>
       (?蹊 . "0x8E4A") ; <CJK>
       (?醯 . "0x91AF") ; <CJK>
       (?鞋 . "0x978B") ; <CJK>
       (?乎 . "0x4E4E") ; <CJK>
       (?互 . "0x4E92") ; <CJK>
       (?呼 . "0x547C") ; <CJK>
       (?壕 . "0x58D5") ; <CJK>
       (?壺 . "0x58FA") ; <CJK>
       (?好 . "0x597D") ; <CJK>
       (?岵 . "0x5CB5") ; <CJK>
       (?弧 . "0x5F27") ; <CJK>
       (?戶 . "0x6236") ; <CJK>
       (?扈 . "0x6248") ; <CJK>
       (?昊 . "0x660A") ; <CJK>
       (?晧 . "0x6667") ; <CJK>
       (?毫 . "0x6BEB") ; <CJK>
       (?浩 . "0x6D69") ; <CJK>
       (?淏 . "0x6DCF") ; <CJK>
       (?湖 . "0x6E56") ; <CJK>
       (?滸 . "0x6EF8") ; <CJK>
       (?澔 . "0x6F94") ; <CJK>
       (?濠 . "0x6FE0") ; <CJK>
       (?濩 . "0x6FE9") ; <CJK>
       (?灝 . "0x705D") ; <CJK>
       (?狐 . "0x72D0") ; <CJK>
       (?琥 . "0x7425") ; <CJK>
       (?瑚 . "0x745A") ; <CJK>
       (?瓠 . "0x74E0") ; <CJK>
       (?皓 . "0x7693") ; <CJK>
       (?祜 . "0x795C") ; <CJK>
       (?糊 . "0x7CCA") ; <CJK>
       (?縞 . "0x7E1E") ; <CJK>
       (?胡 . "0x80E1") ; <CJK>
       (?芦 . "0x82A6") ; <CJK>
       (?葫 . "0x846B") ; <CJK>
       (?蒿 . "0x84BF") ; <CJK>
       (?虎 . "0x864E") ; <CJK>
       (?號 . "0x865F") ; <CJK>
       (?蝴 . "0x8774") ; <CJK>
       (?護 . "0x8B77") ; <CJK>
       (?豪 . "0x8C6A") ; <CJK>
       (?鎬 . "0x93AC") ; <CJK>
       (?頀 . "0x9800") ; <CJK>
       (?顥 . "0x9865") ; <CJK>
       (?惑 . "0x60D1") ; <CJK>
       (?或 . "0x6216") ; <CJK>
       (?酷 . "0x9177") ; <CJK>
       (?婚 . "0x5A5A") ; <CJK>
       (?昏 . "0x660F") ; <CJK>
       (?混 . "0x6DF7") ; <CJK>
       (?渾 . "0x6E3E") ; <CJK>
       (?琿 . "0x743F") ; <CJK>
       (?魂 . "0x9B42") ; <CJK>
       (?忽 . "0x5FFD") ; <CJK>
       (?惚 . "0x60DA") ; <CJK>
       (?笏 . "0x7B0F") ; <CJK>
       (?哄 . "0x54C4") ; <CJK>
       (?弘 . "0x5F18") ; <CJK>
       (?汞 . "0x6C5E") ; <CJK>
       (?泓 . "0x6CD3") ; <CJK>
       (?洪 . "0x6D2A") ; <CJK>
       (?烘 . "0x70D8") ; <CJK>
       (?紅 . "0x7D05") ; <CJK>
       (?虹 . "0x8679") ; <CJK>
       (?訌 . "0x8A0C") ; <CJK>
       (?鴻 . "0x9D3B") ; <CJK>
       (?化 . "0x5316") ; <CJK>
       (?和 . "0x548C") ; <CJK>
       (?嬅 . "0x5B05") ; <CJK>
       (?樺 . "0x6A3A") ; <CJK>
       (?火 . "0x706B") ; <CJK>
       (?畵 . "0x7575") ; <CJK>
       (?禍 . "0x798D") ; <CJK>
       (?禾 . "0x79BE") ; <CJK>
       (?花 . "0x82B1") ; <CJK>
       (?華 . "0x83EF") ; <CJK>
       (?話 . "0x8A71") ; <CJK>
       (?譁 . "0x8B41") ; <CJK>
       (?貨 . "0x8CA8") ; <CJK>
       (?靴 . "0x9774") ; <CJK>
       (?廓 . "0xFA0B") ; <CJK>
       (?擴 . "0x64F4") ; <CJK>
       (?攫 . "0x652B") ; <CJK>
       (?確 . "0x78BA") ; <CJK>
       (?碻 . "0x78BB") ; <CJK>
       (?穫 . "0x7A6B") ; <CJK>
       (?丸 . "0x4E38") ; <CJK>
       (?喚 . "0x559A") ; <CJK>
       (?奐 . "0x5950") ; <CJK>
       (?宦 . "0x5BA6") ; <CJK>
       (?幻 . "0x5E7B") ; <CJK>
       (?患 . "0x60A3") ; <CJK>
       (?換 . "0x63DB") ; <CJK>
       (?歡 . "0x6B61") ; <CJK>
       (?晥 . "0x6665") ; <CJK>
       (?桓 . "0x6853") ; <CJK>
       (?渙 . "0x6E19") ; <CJK>
       (?煥 . "0x7165") ; <CJK>
       (?環 . "0x74B0") ; <CJK>
       (?紈 . "0x7D08") ; <CJK>
       (?還 . "0x9084") ; <CJK>
       (?驩 . "0x9A69") ; <CJK>
       (?鰥 . "0x9C25") ; <CJK>
       (?活 . "0x6D3B") ; <CJK>
       (?滑 . "0x6ED1") ; <CJK>
       (?猾 . "0x733E") ; <CJK>
       (?豁 . "0x8C41") ; <CJK>
       (?闊 . "0x95CA") ; <CJK>
       (?凰 . "0x51F0") ; <CJK>
       (?幌 . "0x5E4C") ; <CJK>
       (?徨 . "0x5FA8") ; <CJK>
       (?恍 . "0x604D") ; <CJK>
       (?惶 . "0x60F6") ; <CJK>
       (?愰 . "0x6130") ; <CJK>
       (?慌 . "0x614C") ; <CJK>
       (?晃 . "0x6643") ; <CJK>
       (?晄 . "0x6644") ; <CJK>
       (?榥 . "0x69A5") ; <CJK>
       (?況 . "0x6CC1") ; <CJK>
       (?湟 . "0x6E5F") ; <CJK>
       (?滉 . "0x6EC9") ; <CJK>
       (?潢 . "0x6F62") ; <CJK>
       (?煌 . "0x714C") ; <CJK>
       (?璜 . "0x749C") ; <CJK>
       (?皇 . "0x7687") ; <CJK>
       (?篁 . "0x7BC1") ; <CJK>
       (?簧 . "0x7C27") ; <CJK>
       (?荒 . "0x8352") ; <CJK>
       (?蝗 . "0x8757") ; <CJK>
       (?遑 . "0x9051") ; <CJK>
       (?隍 . "0x968D") ; <CJK>
       (?黃 . "0x9EC3") ; <CJK>
       (?匯 . "0x532F") ; <CJK>
       (?回 . "0x56DE") ; <CJK>
       (?廻 . "0x5EFB") ; <CJK>
       (?徊 . "0x5F8A") ; <CJK>
       (?恢 . "0x6062") ; <CJK>
       (?悔 . "0x6094") ; <CJK>
       (?懷 . "0x61F7") ; <CJK>
       (?晦 . "0x6666") ; <CJK>
       (?會 . "0x6703") ; <CJK>
       (?檜 . "0x6A9C") ; <CJK>
       (?淮 . "0x6DEE") ; <CJK>
       (?澮 . "0x6FAE") ; <CJK>
       (?灰 . "0x7070") ; <CJK>
       (?獪 . "0x736A") ; <CJK>
       (?繪 . "0x7E6A") ; <CJK>
       (?膾 . "0x81BE") ; <CJK>
       (?茴 . "0x8334") ; <CJK>
       (?蛔 . "0x86D4") ; <CJK>
       (?誨 . "0x8AA8") ; <CJK>
       (?賄 . "0x8CC4") ; <CJK>
       (?劃 . "0x5283") ; <CJK>
       (?獲 . "0x7372") ; <CJK>
       (?宖 . "0x5B96") ; <CJK>
       (?橫 . "0x6A6B") ; <CJK>
       (?鐄 . "0x9404") ; <CJK>
       (?哮 . "0x54EE") ; <CJK>
       (?嚆 . "0x5686") ; <CJK>
       (?孝 . "0x5B5D") ; <CJK>
       (?效 . "0x6548") ; <CJK>
       (?斅 . "0x6585") ; <CJK>
       (?曉 . "0x66C9") ; <CJK>
       (?梟 . "0x689F") ; <CJK>
       (?涍 . "0x6D8D") ; <CJK>
       (?淆 . "0x6DC6") ; <CJK>
       (?爻 . "0x723B") ; <CJK>
       (?肴 . "0x80B4") ; <CJK>
       (?酵 . "0x9175") ; <CJK>
       (?驍 . "0x9A4D") ; <CJK>
       (?侯 . "0x4FAF") ; <CJK>
       (?候 . "0x5019") ; <CJK>
       (?厚 . "0x539A") ; <CJK>
       (?后 . "0x540E") ; <CJK>
       (?吼 . "0x543C") ; <CJK>
       (?喉 . "0x5589") ; <CJK>
       (?嗅 . "0x55C5") ; <CJK>
       (?帿 . "0x5E3F") ; <CJK>
       (?後 . "0x5F8C") ; <CJK>
       (?朽 . "0x673D") ; <CJK>
       (?煦 . "0x7166") ; <CJK>
       (?珝 . "0x73DD") ; <CJK>
       (?逅 . "0x9005") ; <CJK>
       (?勛 . "0x52DB") ; <CJK>
       (?勳 . "0x52F3") ; <CJK>
       (?塤 . "0x5864") ; <CJK>
       (?壎 . "0x58CE") ; <CJK>
       (?焄 . "0x7104") ; <CJK>
       (?熏 . "0x718F") ; <CJK>
       (?燻 . "0x71FB") ; <CJK>
       (?薰 . "0x85B0") ; <CJK>
       (?訓 . "0x8A13") ; <CJK>
       (?暈 . "0x6688") ; <CJK>
       (?薨 . "0x85A8") ; <CJK>
       (?喧 . "0x55A7") ; <CJK>
       (?暄 . "0x6684") ; <CJK>
       (?煊 . "0x714A") ; <CJK>
       (?萱 . "0x8431") ; <CJK>
       (?卉 . "0x5349") ; <CJK>
       (?喙 . "0x5599") ; <CJK>
       (?毁 . "0x6BC1") ; <CJK>
       (?彙 . "0x5F59") ; <CJK>
       (?徽 . "0x5FBD") ; <CJK>
       (?揮 . "0x63EE") ; <CJK>
       (?暉 . "0x6689") ; <CJK>
       (?煇 . "0x7147") ; <CJK>
       (?諱 . "0x8AF1") ; <CJK>
       (?輝 . "0x8F1D") ; <CJK>
       (?麾 . "0x9EBE") ; <CJK>
       (?休 . "0x4F11") ; <CJK>
       (?携 . "0x643A") ; <CJK>
       (?烋 . "0x70CB") ; <CJK>
       (?畦 . "0x7566") ; <CJK>
       (?虧 . "0x8667") ; <CJK>
       (?恤 . "0x6064") ; <CJK>
       (?譎 . "0x8B4E") ; <CJK>
       (?鷸 . "0x9DF8") ; <CJK>
       (?兇 . "0x5147") ; <CJK>
       (?凶 . "0x51F6") ; <CJK>
       (?匈 . "0x5308") ; <CJK>
       (?洶 . "0x6D36") ; <CJK>
       (?胸 . "0x80F8") ; <CJK>
       (?黑 . "0x9ED1") ; <CJK>
       (?昕 . "0x6615") ; <CJK>
       (?欣 . "0x6B23") ; <CJK>
       (?炘 . "0x7098") ; <CJK>
       (?痕 . "0x75D5") ; <CJK>
       (?吃 . "0x5403") ; <CJK>
       (?屹 . "0x5C79") ; <CJK>
       (?紇 . "0x7D07") ; <CJK>
       (?訖 . "0x8A16") ; <CJK>
       (?欠 . "0x6B20") ; <CJK>
       (?欽 . "0x6B3D") ; <CJK>
       (?歆 . "0x6B46") ; <CJK>
       (?吸 . "0x5438") ; <CJK>
       (?恰 . "0x6070") ; <CJK>
       (?洽 . "0x6D3D") ; <CJK>
       (?翕 . "0x7FD5") ; <CJK>
       (?興 . "0x8208") ; <CJK>
       (?僖 . "0x50D6") ; <CJK>
       (?凞 . "0x51DE") ; <CJK>
       (?喜 . "0x559C") ; <CJK>
       (?噫 . "0x566B") ; <CJK>
       (?囍 . "0x56CD") ; <CJK>
       (?姬 . "0x59EC") ; <CJK>
       (?嬉 . "0x5B09") ; <CJK>
       (?希 . "0x5E0C") ; <CJK>
       (?憙 . "0x6199") ; <CJK>
       (?憘 . "0x6198") ; <CJK>
       (?戱 . "0x6231") ; <CJK>
       (?晞 . "0x665E") ; <CJK>
       (?曦 . "0x66E6") ; <CJK>
       (?熙 . "0x7199") ; <CJK>
       (?熹 . "0x71B9") ; <CJK>
       (?熺 . "0x71BA") ; <CJK>
       (?犧 . "0x72A7") ; <CJK>
       (?禧 . "0x79A7") ; <CJK>
       (?稀 . "0x7A00") ; <CJK>
       (?羲 . "0x7FB2") ; <CJK>
       (?詰 . "0x8A70") ; <CJK>
       ))))

(provide 'uksc5601)

;;; uksc5601.el ends here
 

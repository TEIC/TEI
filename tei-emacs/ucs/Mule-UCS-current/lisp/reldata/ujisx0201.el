; -*- coding: iso-2022-7bit  -*-
;;; ujisx0201.el --- tables between UCS and JIS X 0201

;; Copyright (C) 1997 Miyashita Hisashi

;; Keywords: CCL, mule, multilingual, 
;;           character set, coding-system, ISO10646, Unicode, ISO8859

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

;;; This file is converted from JIX0201.TXT of Unicode consortium
;;; by Miyashita Hisashi <himi@bird.scphys.kyoto-u.ac.jp>.

(put 'latin-jisx0201 'unicode-assoc
     'jis-x-0201-latin-vs-unicode-assoc)
(put 'katakana-jisx0201 'unicode-assoc
     'jis-x-0201-katakana-vs-unicode-assoc)

(defvar
  jis-x-0201-latin-vs-unicode-assoc
  (list
   'assoc
   '(char-1 . ucs-generic)
   (transformate-list-structure
    ((lambda (x) (make-char 'latin-jisx0201 x)) .
     c-notated-string-to-number)
    ((33 . "0x0021") ;; EXCLAMATION MARK
     (34 . "0x0022") ;; QUOTATION MARK
     (35 . "0x0023") ;; NUMBER SIGN
     (36 . "0x0024") ;; DOLLAR SIGN
     (37 . "0x0025") ;; PERCENT SIGN
     (38 . "0x0026") ;; AMPERSAND
     (39 . "0x0027") ;; APOSTROPHE
     (40 . "0x0028") ;; LEFT PARENTHESIS
     (41 . "0x0029") ;; RIGHT PARENTHESIS
     (42 . "0x002A") ;; ASTERISK
     (43 . "0x002B") ;; PLUS SIGN
     (44 . "0x002C") ;; COMMA
     (45 . "0x002D") ;; HYPHEN-MINUS
     (46 . "0x002E") ;; FULL STOP
     (47 . "0x002F") ;; SOLIDUS
     (48 . "0x0030") ;; DIGIT ZERO
     (49 . "0x0031") ;; DIGIT ONE
     (50 . "0x0032") ;; DIGIT TWO
     (51 . "0x0033") ;; DIGIT THREE
     (52 . "0x0034") ;; DIGIT FOUR
     (53 . "0x0035") ;; DIGIT FIVE
     (54 . "0x0036") ;; DIGIT SIX
     (55 . "0x0037") ;; DIGIT SEVEN
     (56 . "0x0038") ;; DIGIT EIGHT
     (57 . "0x0039") ;; DIGIT NINE
     (58 . "0x003A") ;; COLON
     (59 . "0x003B") ;; SEMICOLON
     (60 . "0x003C") ;; LESS-THAN SIGN
     (61 . "0x003D") ;; EQUALS SIGN
     (62 . "0x003E") ;; GREATER-THAN SIGN
     (63 . "0x003F") ;; QUESTION MARK
     (64 . "0x0040") ;; COMMERCIAL AT
     (65 . "0x0041") ;; LATIN CAPITAL LETTER A
     (66 . "0x0042") ;; LATIN CAPITAL LETTER B
     (67 . "0x0043") ;; LATIN CAPITAL LETTER C
     (68 . "0x0044") ;; LATIN CAPITAL LETTER D
     (69 . "0x0045") ;; LATIN CAPITAL LETTER E
     (70 . "0x0046") ;; LATIN CAPITAL LETTER F
     (71 . "0x0047") ;; LATIN CAPITAL LETTER G
     (72 . "0x0048") ;; LATIN CAPITAL LETTER H
     (73 . "0x0049") ;; LATIN CAPITAL LETTER I
     (74 . "0x004A") ;; LATIN CAPITAL LETTER J
     (75 . "0x004B") ;; LATIN CAPITAL LETTER K
     (76 . "0x004C") ;; LATIN CAPITAL LETTER L
     (77 . "0x004D") ;; LATIN CAPITAL LETTER M
     (78 . "0x004E") ;; LATIN CAPITAL LETTER N
     (79 . "0x004F") ;; LATIN CAPITAL LETTER O
     (80 . "0x0050") ;; LATIN CAPITAL LETTER P
     (81 . "0x0051") ;; LATIN CAPITAL LETTER Q
     (82 . "0x0052") ;; LATIN CAPITAL LETTER R
     (83 . "0x0053") ;; LATIN CAPITAL LETTER S
     (84 . "0x0054") ;; LATIN CAPITAL LETTER T
     (85 . "0x0055") ;; LATIN CAPITAL LETTER U
     (86 . "0x0056") ;; LATIN CAPITAL LETTER V
     (87 . "0x0057") ;; LATIN CAPITAL LETTER W
     (88 . "0x0058") ;; LATIN CAPITAL LETTER X
     (89 . "0x0059") ;; LATIN CAPITAL LETTER Y
     (90 . "0x005A") ;; LATIN CAPITAL LETTER Z
     (91 . "0x005B") ;; LEFT SQUARE BRACKET
     (92 . "0x00A5") ;; YEN SIGN
     (93 . "0x005D") ;; RIGHT SQUARE BRACKET
     (94 . "0x005E") ;; CIRCUMFLEX ACCENT
     (95 . "0x005F") ;; LOW LINE
     (96 . "0x0060") ;; GRAVE ACCENT
     (97 . "0x0061") ;; LATIN SMALL LETTER A
     (98 . "0x0062") ;; LATIN SMALL LETTER B
     (99 . "0x0063") ;; LATIN SMALL LETTER C
     (100 . "0x0064") ;; LATIN SMALL LETTER D
     (101 . "0x0065") ;; LATIN SMALL LETTER E
     (102 . "0x0066") ;; LATIN SMALL LETTER F
     (103 . "0x0067") ;; LATIN SMALL LETTER G
     (104 . "0x0068") ;; LATIN SMALL LETTER H
     (105 . "0x0069") ;; LATIN SMALL LETTER I
     (106 . "0x006A") ;; LATIN SMALL LETTER J
     (107 . "0x006B") ;; LATIN SMALL LETTER K
     (108 . "0x006C") ;; LATIN SMALL LETTER L
     (109 . "0x006D") ;; LATIN SMALL LETTER M
     (110 . "0x006E") ;; LATIN SMALL LETTER N
     (111 . "0x006F") ;; LATIN SMALL LETTER O
     (112 . "0x0070") ;; LATIN SMALL LETTER P
     (113 . "0x0071") ;; LATIN SMALL LETTER Q
     (114 . "0x0072") ;; LATIN SMALL LETTER R
     (115 . "0x0073") ;; LATIN SMALL LETTER S
     (116 . "0x0074") ;; LATIN SMALL LETTER T
     (117 . "0x0075") ;; LATIN SMALL LETTER U
     (118 . "0x0076") ;; LATIN SMALL LETTER V
     (119 . "0x0077") ;; LATIN SMALL LETTER W
     (120 . "0x0078") ;; LATIN SMALL LETTER X
     (121 . "0x0079") ;; LATIN SMALL LETTER Y
     (122 . "0x007A") ;; LATIN SMALL LETTER Z
     (123 . "0x007B") ;; LEFT CURLY BRACKET
     (124 . "0x007C") ;; VERTICAL LINE
     (125 . "0x007D") ;; RIGHT CURLY BRACKET
     (126 . "0x203E") ;; OVERLINE
     ))))

(defvar
  jis-x-0201-katakana-vs-unicode-assoc
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?｡ . "0xFF61") ;; HALFWIDTH IDEOGRAPHIC FULL STOP
       (?｢ . "0xFF62") ;; HALFWIDTH LEFT CORNER BRACKET
       (?｣ . "0xFF63") ;; HALFWIDTH RIGHT CORNER BRACKET
       (?､ . "0xFF64") ;; HALFWIDTH IDEOGRAPHIC COMMA
       (?･ . "0xFF65") ;; HALFWIDTH KATAKANA MIDDLE DOT
       (?ｦ . "0xFF66") ;; HALFWIDTH KATAKANA LETTER WO
       (?ｧ . "0xFF67") ;; HALFWIDTH KATAKANA LETTER SMALL A
       (?ｨ . "0xFF68") ;; HALFWIDTH KATAKANA LETTER SMALL I
       (?ｩ . "0xFF69") ;; HALFWIDTH KATAKANA LETTER SMALL U
       (?ｪ . "0xFF6A") ;; HALFWIDTH KATAKANA LETTER SMALL E
       (?ｫ . "0xFF6B") ;; HALFWIDTH KATAKANA LETTER SMALL O
       (?ｬ . "0xFF6C") ;; HALFWIDTH KATAKANA LETTER SMALL YA
       (?ｭ . "0xFF6D") ;; HALFWIDTH KATAKANA LETTER SMALL YU
       (?ｮ . "0xFF6E") ;; HALFWIDTH KATAKANA LETTER SMALL YO
       (?ｯ . "0xFF6F") ;; HALFWIDTH KATAKANA LETTER SMALL TU
       (?ｰ . "0xFF70") ;; HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
       (?ｱ . "0xFF71") ;; HALFWIDTH KATAKANA LETTER A
       (?ｲ . "0xFF72") ;; HALFWIDTH KATAKANA LETTER I
       (?ｳ . "0xFF73") ;; HALFWIDTH KATAKANA LETTER U
       (?ｴ . "0xFF74") ;; HALFWIDTH KATAKANA LETTER E
       (?ｵ . "0xFF75") ;; HALFWIDTH KATAKANA LETTER O
       (?ｶ . "0xFF76") ;; HALFWIDTH KATAKANA LETTER KA
       (?ｷ . "0xFF77") ;; HALFWIDTH KATAKANA LETTER KI
       (?ｸ . "0xFF78") ;; HALFWIDTH KATAKANA LETTER KU
       (?ｹ . "0xFF79") ;; HALFWIDTH KATAKANA LETTER KE
       (?ｺ . "0xFF7A") ;; HALFWIDTH KATAKANA LETTER KO
       (?ｻ . "0xFF7B") ;; HALFWIDTH KATAKANA LETTER SA
       (?ｼ . "0xFF7C") ;; HALFWIDTH KATAKANA LETTER SI
       (?ｽ . "0xFF7D") ;; HALFWIDTH KATAKANA LETTER SU
       (?ｾ . "0xFF7E") ;; HALFWIDTH KATAKANA LETTER SE
       (?ｿ . "0xFF7F") ;; HALFWIDTH KATAKANA LETTER SO
       (?ﾀ . "0xFF80") ;; HALFWIDTH KATAKANA LETTER TA
       (?ﾁ . "0xFF81") ;; HALFWIDTH KATAKANA LETTER TI
       (?ﾂ . "0xFF82") ;; HALFWIDTH KATAKANA LETTER TU
       (?ﾃ . "0xFF83") ;; HALFWIDTH KATAKANA LETTER TE
       (?ﾄ . "0xFF84") ;; HALFWIDTH KATAKANA LETTER TO
       (?ﾅ . "0xFF85") ;; HALFWIDTH KATAKANA LETTER NA
       (?ﾆ . "0xFF86") ;; HALFWIDTH KATAKANA LETTER NI
       (?ﾇ . "0xFF87") ;; HALFWIDTH KATAKANA LETTER NU
       (?ﾈ . "0xFF88") ;; HALFWIDTH KATAKANA LETTER NE
       (?ﾉ . "0xFF89") ;; HALFWIDTH KATAKANA LETTER NO
       (?ﾊ . "0xFF8A") ;; HALFWIDTH KATAKANA LETTER HA
       (?ﾋ . "0xFF8B") ;; HALFWIDTH KATAKANA LETTER HI
       (?ﾌ . "0xFF8C") ;; HALFWIDTH KATAKANA LETTER HU
       (?ﾍ . "0xFF8D") ;; HALFWIDTH KATAKANA LETTER HE
       (?ﾎ . "0xFF8E") ;; HALFWIDTH KATAKANA LETTER HO
       (?ﾏ . "0xFF8F") ;; HALFWIDTH KATAKANA LETTER MA
       (?ﾐ . "0xFF90") ;; HALFWIDTH KATAKANA LETTER MI
       (?ﾑ . "0xFF91") ;; HALFWIDTH KATAKANA LETTER MU
       (?ﾒ . "0xFF92") ;; HALFWIDTH KATAKANA LETTER ME
       (?ﾓ . "0xFF93") ;; HALFWIDTH KATAKANA LETTER MO
       (?ﾔ . "0xFF94") ;; HALFWIDTH KATAKANA LETTER YA
       (?ﾕ . "0xFF95") ;; HALFWIDTH KATAKANA LETTER YU
       (?ﾖ . "0xFF96") ;; HALFWIDTH KATAKANA LETTER YO
       (?ﾗ . "0xFF97") ;; HALFWIDTH KATAKANA LETTER RA
       (?ﾘ . "0xFF98") ;; HALFWIDTH KATAKANA LETTER RI
       (?ﾙ . "0xFF99") ;; HALFWIDTH KATAKANA LETTER RU
       (?ﾚ . "0xFF9A") ;; HALFWIDTH KATAKANA LETTER RE
       (?ﾛ . "0xFF9B") ;; HALFWIDTH KATAKANA LETTER RO
       (?ﾜ . "0xFF9C") ;; HALFWIDTH KATAKANA LETTER WA
       (?ﾝ . "0xFF9D") ;; HALFWIDTH KATAKANA LETTER N
       (?ﾞ . "0xFF9E") ;; HALFWIDTH KATAKANA VOICED SOUND MARK
       (?ﾟ . "0xFF9F") ;; HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
       ))))

(provide 'ujisx0201)

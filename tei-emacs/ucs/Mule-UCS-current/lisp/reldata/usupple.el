;;; -*- coding: iso-2022-7bit -*-
;;; usupple.el --- tables for various definitions that are
;;;                different from Unicode Consortium's.

;; Copyright (C) 2000 Miyashita Hisashi

;; Keywords: mule, multilingual, 
;;           character set, coding-system, ISO/IEC 10646,
;;           Unicode, JIS X 0221, JDK, Japanese-EUC, Windows,
;;           halfwidth or fullwidth character.

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

;; (defvar exclude-compatibility-area-unicode-translation-rule nil)

(defvar unicode-assoc-for-jisx0221
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      (identity . c-notated-string-to-number)
      ((?― . "0x0214") ;; HORIZONTAL BAR
       (?￣ . "0x203E") ;; OVERLINE
       (?― . "0x2014") ;; EM DASH
       (?＼ . "0xFF3C") ;; FULLWIDTH REVERSE SOLIDUS
       (?￥ . "0x00A5") ;; YEN SIGN
       (?~ . "0xFFE5") ;; FULLWIDTH TILDE
       ))))

(defvar unicode-assoc-for-jdk
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      ((lambda (x)
	 (cond ((stringp x)
		(make-char
		 'ascii
		 (c-notated-string-to-number x)))
	       (t x))) .
       c-notated-string-to-number)
      (("0x5C" . "0x005C") ;; REVERSE SOLIDUS
       ("0x5C" . "0x00A5") ;; YEN SIGN(ignored at encoding time)
       ("0x7E" . "0x007E") ;; TILDE
       ("0x7E" . "0x203E") ;; OVERLINE(ignored at encoding time)
       (?＼ . "0xFF3C")    ;; FULLWIDTH REVERSE SOLIDUS
       ))))

(defvar unicode-assoc-for-windows
  `(assoc
    (char-1 . ucs-generic)
    ,(transformate-list-structure
      ((lambda (x)
	 (cond ((stringp x)
		(make-char
		 'latin-jisx0201
		 (c-notated-string-to-number x)))
	       (t x))) .
       c-notated-string-to-number)
      (;; JIS X 0208 correspondings
       (?＼ . "0xFF3C") ;; FULLWIDTH REVERSE SOLIDUS
       (?〜 . "0xFF5E") ;; FULLWIDTH TILDE
       (?‖ . "0x2225") ;; PARALLEL TO
       (?− . "0xFF0D") ;; FULLWIDTH HYPHENMINUS
       (?¢ . "0xFFE0") ;; FULLWIDTH CENT SIGN
       (?£ . "0xFFE1") ;; FULLWIDTH POUND SIGN
       (?¬ . "0xFFE2") ;; FULLWIDTH NOT SIGN
       ;; JIS X 0212 correspondings
       (?~ . "0xFF5E") ;; FULLWIDTH TILDE (usually not used for decoding)
       (?¦ . "0xFF4E") ;; FULLWIDTH BROKEN BAR
       ;; JIS X 0201 correspondings
       ;; ("0x5C" . "0x005C") ;; REVERSE SOLIDUS
       ;; ("0x7E" . "0x007E") ;; TILDE
       ))))

(defvar unicode-assoc-for-fullwidth-or-halfwidth-normalization
  `(assoc
    (ucs-generic . ucs-generic)
    ,(nconc
      (transformate-list-structure
       (c-notated-string-to-number . c-notated-string-to-number)
       (("0xFF01" . "0x0021") ;; FULLWIDTH EXCLAMATION MARK
	("0xFF02" . "0x0022") ;; FULLWIDTH QUOTATION MARK
	("0xFF03" . "0x0023") ;; FULLWIDTH NUMBER SIGN
        ("0xFF04" . "0x0024") ;; FULLWIDTH DOLLAR SIGN
        ("0xFF05" . "0x0025") ;; FULLWIDTH PERCENT SIGN
        ("0xFF06" . "0x0026") ;; FULLWIDTH AMPERSAND
        ("0xFF07" . "0x0027") ;; FULLWIDTH APOSTROPHE
        ("0xFF08" . "0x0028") ;; FULLWIDTH LEFT PARENTHESIS
        ("0xFF09" . "0x0029") ;; FULLWIDTH RIGHT PARENTHESIS
        ("0xFF0A" . "0x002A") ;; FULLWIDTH ASTERISK
        ("0xFF0B" . "0x002B") ;; FULLWIDTH PLUS SIGN
        ("0xFF0C" . "0x002C") ;; FULLWIDTH COMMA
        ("0xFF0D" . "0x002D") ;; FULLWIDTH HYPHEN-MINUS
        ("0xFF0E" . "0x002E") ;; FULLWIDTH FULL STOP
        ("0xFF0F" . "0x002F") ;; FULLWIDTH SOLIDUS
        ("0xFF10" . "0x0030") ;; FULLWIDTH DIGIT ZERO
        ("0xFF11" . "0x0031") ;; FULLWIDTH DIGIT ONE
        ("0xFF12" . "0x0032") ;; FULLWIDTH DIGIT TWO
        ("0xFF13" . "0x0033") ;; FULLWIDTH DIGIT THREE
        ("0xFF14" . "0x0034") ;; FULLWIDTH DIGIT FOUR
        ("0xFF15" . "0x0035") ;; FULLWIDTH DIGIT FIVE
        ("0xFF16" . "0x0036") ;; FULLWIDTH DIGIT SIX
        ("0xFF17" . "0x0037") ;; FULLWIDTH DIGIT SEVEN
        ("0xFF18" . "0x0038") ;; FULLWIDTH DIGIT EIGHT
        ("0xFF19" . "0x0039") ;; FULLWIDTH DIGIT NINE
        ("0xFF1A" . "0x003A") ;; FULLWIDTH COLON
        ("0xFF1B" . "0x003B") ;; FULLWIDTH SEMICOLON
        ("0xFF1C" . "0x003C") ;; FULLWIDTH LESS-THAN SIGN
        ("0xFF1D" . "0x003D") ;; FULLWIDTH EQUALS SIGN
        ("0xFF1E" . "0x003E") ;; FULLWIDTH GREATER-THAN SIGN
        ("0xFF1F" . "0x003F") ;; FULLWIDTH QUESTION MARK
        ("0xFF20" . "0x0040") ;; FULLWIDTH COMMERCIAL AT
        ("0xFF21" . "0x0041") ;; FULLWIDTH LATIN CAPITAL LETTER A
        ("0xFF22" . "0x0042") ;; FULLWIDTH LATIN CAPITAL LETTER B
        ("0xFF23" . "0x0043") ;; FULLWIDTH LATIN CAPITAL LETTER C
        ("0xFF24" . "0x0044") ;; FULLWIDTH LATIN CAPITAL LETTER D
        ("0xFF25" . "0x0045") ;; FULLWIDTH LATIN CAPITAL LETTER E
        ("0xFF26" . "0x0046") ;; FULLWIDTH LATIN CAPITAL LETTER F
        ("0xFF27" . "0x0047") ;; FULLWIDTH LATIN CAPITAL LETTER G
        ("0xFF28" . "0x0048") ;; FULLWIDTH LATIN CAPITAL LETTER H
        ("0xFF29" . "0x0049") ;; FULLWIDTH LATIN CAPITAL LETTER I
        ("0xFF2A" . "0x004A") ;; FULLWIDTH LATIN CAPITAL LETTER J
        ("0xFF2B" . "0x004B") ;; FULLWIDTH LATIN CAPITAL LETTER K
        ("0xFF2C" . "0x004C") ;; FULLWIDTH LATIN CAPITAL LETTER L
        ("0xFF2D" . "0x004D") ;; FULLWIDTH LATIN CAPITAL LETTER M
        ("0xFF2E" . "0x004E") ;; FULLWIDTH LATIN CAPITAL LETTER N
        ("0xFF2F" . "0x004F") ;; FULLWIDTH LATIN CAPITAL LETTER O
        ("0xFF30" . "0x0050") ;; FULLWIDTH LATIN CAPITAL LETTER P
        ("0xFF31" . "0x0051") ;; FULLWIDTH LATIN CAPITAL LETTER Q
        ("0xFF32" . "0x0052") ;; FULLWIDTH LATIN CAPITAL LETTER R
        ("0xFF33" . "0x0053") ;; FULLWIDTH LATIN CAPITAL LETTER S
        ("0xFF34" . "0x0054") ;; FULLWIDTH LATIN CAPITAL LETTER T
        ("0xFF35" . "0x0055") ;; FULLWIDTH LATIN CAPITAL LETTER U
        ("0xFF36" . "0x0056") ;; FULLWIDTH LATIN CAPITAL LETTER V
        ("0xFF37" . "0x0057") ;; FULLWIDTH LATIN CAPITAL LETTER W
        ("0xFF38" . "0x0058") ;; FULLWIDTH LATIN CAPITAL LETTER X
        ("0xFF39" . "0x0059") ;; FULLWIDTH LATIN CAPITAL LETTER Y
        ("0xFF3A" . "0x005A") ;; FULLWIDTH LATIN CAPITAL LETTER Z
        ("0xFF3B" . "0x005B") ;; FULLWIDTH LEFT SQUARE BRACKET
        ("0xFF3C" . "0x005C") ;; FULLWIDTH REVERSE SOLIDUS
        ("0xFF3D" . "0x005D") ;; FULLWIDTH RIGHT SQUARE BRACKET
        ("0xFF3E" . "0x005E") ;; FULLWIDTH CIRCUMFLEX ACCENT
        ("0xFF3F" . "0x005F") ;; FULLWIDTH LOW LINE
        ("0xFF40" . "0x0060") ;; FULLWIDTH GRAVE ACCENT
        ("0xFF41" . "0x0061") ;; FULLWIDTH LATIN SMALL LETTER A
        ("0xFF42" . "0x0062") ;; FULLWIDTH LATIN SMALL LETTER B
        ("0xFF43" . "0x0063") ;; FULLWIDTH LATIN SMALL LETTER C
        ("0xFF44" . "0x0064") ;; FULLWIDTH LATIN SMALL LETTER D
        ("0xFF45" . "0x0065") ;; FULLWIDTH LATIN SMALL LETTER E
        ("0xFF46" . "0x0066") ;; FULLWIDTH LATIN SMALL LETTER F
        ("0xFF47" . "0x0067") ;; FULLWIDTH LATIN SMALL LETTER G
        ("0xFF48" . "0x0068") ;; FULLWIDTH LATIN SMALL LETTER H
        ("0xFF49" . "0x0069") ;; FULLWIDTH LATIN SMALL LETTER I
        ("0xFF4A" . "0x006A") ;; FULLWIDTH LATIN SMALL LETTER J
        ("0xFF4B" . "0x006B") ;; FULLWIDTH LATIN SMALL LETTER K
        ("0xFF4C" . "0x006C") ;; FULLWIDTH LATIN SMALL LETTER L
        ("0xFF4D" . "0x006D") ;; FULLWIDTH LATIN SMALL LETTER M
        ("0xFF4E" . "0x006E") ;; FULLWIDTH LATIN SMALL LETTER N
        ("0xFF4F" . "0x006F") ;; FULLWIDTH LATIN SMALL LETTER O
        ("0xFF50" . "0x0070") ;; FULLWIDTH LATIN SMALL LETTER P
        ("0xFF51" . "0x0071") ;; FULLWIDTH LATIN SMALL LETTER Q
        ("0xFF52" . "0x0072") ;; FULLWIDTH LATIN SMALL LETTER R
        ("0xFF53" . "0x0073") ;; FULLWIDTH LATIN SMALL LETTER S
        ("0xFF54" . "0x0074") ;; FULLWIDTH LATIN SMALL LETTER T
        ("0xFF55" . "0x0075") ;; FULLWIDTH LATIN SMALL LETTER U
        ("0xFF56" . "0x0076") ;; FULLWIDTH LATIN SMALL LETTER V
        ("0xFF57" . "0x0077") ;; FULLWIDTH LATIN SMALL LETTER W
        ("0xFF58" . "0x0078") ;; FULLWIDTH LATIN SMALL LETTER X
        ("0xFF59" . "0x0079") ;; FULLWIDTH LATIN SMALL LETTER Y
        ("0xFF5A" . "0x007A") ;; FULLWIDTH LATIN SMALL LETTER Z
        ("0xFF5B" . "0x007B") ;; FULLWIDTH LEFT CURLY BRACKET
        ("0xFF5C" . "0x007C") ;; FULLWIDTH VERTICAL LINE
        ("0xFF5D" . "0x007D") ;; FULLWIDTH RIGHT CURLY BRACKET
        ("0xFF5E" . "0x007E") ;; FULLWIDTH TILDE
        ("0xFF61" . "0x3002") ;; HALFWIDTH IDEOGRAPHIC FULL STOP
        ("0xFF62" . "0x300C") ;; HALFWIDTH LEFT CORNER BRACKET
        ("0xFF63" . "0x300D") ;; HALFWIDTH RIGHT CORNER BRACKET
        ("0xFF64" . "0x3001") ;; HALFWIDTH IDEOGRAPHIC COMMA
        ("0xFF65" . "0x30FB") ;; HALFWIDTH KATAKANA MIDDLE DOT
        ("0xFF66" . "0x30F2") ;; HALFWIDTH KATAKANA LETTER WO
        ("0xFF67" . "0x30A1") ;; HALFWIDTH KATAKANA LETTER SMALL A
        ("0xFF68" . "0x30A3") ;; HALFWIDTH KATAKANA LETTER SMALL I
        ("0xFF69" . "0x30A5") ;; HALFWIDTH KATAKANA LETTER SMALL U
        ("0xFF6A" . "0x30A7") ;; HALFWIDTH KATAKANA LETTER SMALL E
        ("0xFF6B" . "0x30A9") ;; HALFWIDTH KATAKANA LETTER SMALL O
        ("0xFF6C" . "0x30E3") ;; HALFWIDTH KATAKANA LETTER SMALL YA
        ("0xFF6D" . "0x30E5") ;; HALFWIDTH KATAKANA LETTER SMALL YU
        ("0xFF6E" . "0x30E7") ;; HALFWIDTH KATAKANA LETTER SMALL YO
        ("0xFF6F" . "0x30C3") ;; HALFWIDTH KATAKANA LETTER SMALL TU
        ("0xFF70" . "0x30FC") ;; HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
        ("0xFF71" . "0x30A2") ;; HALFWIDTH KATAKANA LETTER A
        ("0xFF72" . "0x30A4") ;; HALFWIDTH KATAKANA LETTER I
        ("0xFF73" . "0x30A6") ;; HALFWIDTH KATAKANA LETTER U
        ("0xFF74" . "0x30A8") ;; HALFWIDTH KATAKANA LETTER E
        ("0xFF75" . "0x30AA") ;; HALFWIDTH KATAKANA LETTER O
        ("0xFF76" . "0x30AB") ;; HALFWIDTH KATAKANA LETTER KA
        ("0xFF77" . "0x30AD") ;; HALFWIDTH KATAKANA LETTER KI
        ("0xFF78" . "0x30AF") ;; HALFWIDTH KATAKANA LETTER KU
        ("0xFF79" . "0x30B1") ;; HALFWIDTH KATAKANA LETTER KE
        ("0xFF7A" . "0x30B3") ;; HALFWIDTH KATAKANA LETTER KO
        ("0xFF7B" . "0x30B5") ;; HALFWIDTH KATAKANA LETTER SA
        ("0xFF7C" . "0x30B7") ;; HALFWIDTH KATAKANA LETTER SI
        ("0xFF7D" . "0x30B9") ;; HALFWIDTH KATAKANA LETTER SU
        ("0xFF7E" . "0x30BB") ;; HALFWIDTH KATAKANA LETTER SE
        ("0xFF7F" . "0x30BD") ;; HALFWIDTH KATAKANA LETTER SO
        ("0xFF80" . "0x30BF") ;; HALFWIDTH KATAKANA LETTER TA
        ("0xFF81" . "0x30C1") ;; HALFWIDTH KATAKANA LETTER TI
        ("0xFF82" . "0x30C4") ;; HALFWIDTH KATAKANA LETTER TU
        ("0xFF83" . "0x30C6") ;; HALFWIDTH KATAKANA LETTER TE
        ("0xFF84" . "0x30C8") ;; HALFWIDTH KATAKANA LETTER TO
        ("0xFF85" . "0x30CA") ;; HALFWIDTH KATAKANA LETTER NA
        ("0xFF86" . "0x30CB") ;; HALFWIDTH KATAKANA LETTER NI
        ("0xFF87" . "0x30CC") ;; HALFWIDTH KATAKANA LETTER NU
        ("0xFF88" . "0x30CD") ;; HALFWIDTH KATAKANA LETTER NE
        ("0xFF89" . "0x30CE") ;; HALFWIDTH KATAKANA LETTER NO
        ("0xFF8A" . "0x30CF") ;; HALFWIDTH KATAKANA LETTER HA
        ("0xFF8B" . "0x30D2") ;; HALFWIDTH KATAKANA LETTER HI
        ("0xFF8C" . "0x30D5") ;; HALFWIDTH KATAKANA LETTER HU
        ("0xFF8D" . "0x30D8") ;; HALFWIDTH KATAKANA LETTER HE
        ("0xFF8E" . "0x30DB") ;; HALFWIDTH KATAKANA LETTER HO
        ("0xFF8F" . "0x30DE") ;; HALFWIDTH KATAKANA LETTER MA
        ("0xFF90" . "0x30DF") ;; HALFWIDTH KATAKANA LETTER MI
        ("0xFF91" . "0x30E0") ;; HALFWIDTH KATAKANA LETTER MU
        ("0xFF92" . "0x30E1") ;; HALFWIDTH KATAKANA LETTER ME
        ("0xFF93" . "0x30E2") ;; HALFWIDTH KATAKANA LETTER MO
        ("0xFF94" . "0x30E4") ;; HALFWIDTH KATAKANA LETTER YA
        ("0xFF95" . "0x30E6") ;; HALFWIDTH KATAKANA LETTER YU
        ("0xFF96" . "0x30E8") ;; HALFWIDTH KATAKANA LETTER YO
        ("0xFF97" . "0x30E9") ;; HALFWIDTH KATAKANA LETTER RA
        ("0xFF98" . "0x30EA") ;; HALFWIDTH KATAKANA LETTER RI
        ("0xFF99" . "0x30EB") ;; HALFWIDTH KATAKANA LETTER RU
        ("0xFF9A" . "0x30EC") ;; HALFWIDTH KATAKANA LETTER RE
        ("0xFF9B" . "0x30ED") ;; HALFWIDTH KATAKANA LETTER RO
        ("0xFF9C" . "0x30EF") ;; HALFWIDTH KATAKANA LETTER WA
        ("0xFF9D" . "0x30F3") ;; HALFWIDTH KATAKANA LETTER N
        ("0xFF9E" . "0x3099") ;; HALFWIDTH KATAKANA VOICED SOUND MARK
        ("0xFF9F" . "0x309A") ;; HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
        ("0xFFA0" . "0x3164") ;; HALFWIDTH HANGUL FILLER
        ("0xFFA1" . "0x3131") ;; HALFWIDTH HANGUL LETTER KIYEOK
        ("0xFFA2" . "0x3132") ;; HALFWIDTH HANGUL LETTER SSANGKIYEOK
        ("0xFFA3" . "0x3133") ;; HALFWIDTH HANGUL LETTER KIYEOK-SIOS
        ("0xFFA4" . "0x3134") ;; HALFWIDTH HANGUL LETTER NIEUN
        ("0xFFA5" . "0x3135") ;; HALFWIDTH HANGUL LETTER NIEUN-CIEUC
        ("0xFFA6" . "0x3136") ;; HALFWIDTH HANGUL LETTER NIEUN-HIEUH
        ("0xFFA7" . "0x3137") ;; HALFWIDTH HANGUL LETTER TIKEUT
        ("0xFFA8" . "0x3138") ;; HALFWIDTH HANGUL LETTER SSANGTIKEUT
        ("0xFFA9" . "0x3139") ;; HALFWIDTH HANGUL LETTER RIEUL
        ("0xFFAA" . "0x313A") ;; HALFWIDTH HANGUL LETTER RIEUL-KIYEOK
        ("0xFFAB" . "0x313B") ;; HALFWIDTH HANGUL LETTER RIEUL-MIEUM
        ("0xFFAC" . "0x313C") ;; HALFWIDTH HANGUL LETTER RIEUL-PIEUP
        ("0xFFAD" . "0x313D") ;; HALFWIDTH HANGUL LETTER RIEUL-SIOS
        ("0xFFAE" . "0x313E") ;; HALFWIDTH HANGUL LETTER RIEUL-THIEUTH
        ("0xFFAF" . "0x313F") ;; HALFWIDTH HANGUL LETTER RIEUL-PHIEUPH
        ("0xFFB0" . "0x3140") ;; HALFWIDTH HANGUL LETTER RIEUL-HIEUH
        ("0xFFB1" . "0x3141") ;; HALFWIDTH HANGUL LETTER MIEUM
        ("0xFFB2" . "0x3142") ;; HALFWIDTH HANGUL LETTER PIEUP
        ("0xFFB3" . "0x3143") ;; HALFWIDTH HANGUL LETTER SSANGPIEUP
        ("0xFFB4" . "0x3144") ;; HALFWIDTH HANGUL LETTER PIEUP-SIOS
        ("0xFFB5" . "0x3145") ;; HALFWIDTH HANGUL LETTER SIOS
        ("0xFFB6" . "0x3146") ;; HALFWIDTH HANGUL LETTER SSANGSIOS
        ("0xFFB7" . "0x3147") ;; HALFWIDTH HANGUL LETTER IEUNG
        ("0xFFB8" . "0x3148") ;; HALFWIDTH HANGUL LETTER CIEUC
        ("0xFFB9" . "0x3149") ;; HALFWIDTH HANGUL LETTER SSANGCIEUC
        ("0xFFBA" . "0x314A") ;; HALFWIDTH HANGUL LETTER CHIEUCH
        ("0xFFBB" . "0x314B") ;; HALFWIDTH HANGUL LETTER KHIEUKH
        ("0xFFBC" . "0x314C") ;; HALFWIDTH HANGUL LETTER THIEUTH
        ("0xFFBD" . "0x314D") ;; HALFWIDTH HANGUL LETTER PHIEUPH
        ("0xFFBE" . "0x314E") ;; HALFWIDTH HANGUL LETTER HIEUH
        ("0xFFC2" . "0x314F") ;; HALFWIDTH HANGUL LETTER A
        ("0xFFC3" . "0x3150") ;; HALFWIDTH HANGUL LETTER AE
        ("0xFFC4" . "0x3151") ;; HALFWIDTH HANGUL LETTER YA
        ("0xFFC5" . "0x3152") ;; HALFWIDTH HANGUL LETTER YAE
        ("0xFFC6" . "0x3153") ;; HALFWIDTH HANGUL LETTER EO
        ("0xFFC7" . "0x3154") ;; HALFWIDTH HANGUL LETTER E
        ("0xFFCA" . "0x3155") ;; HALFWIDTH HANGUL LETTER YEO
        ("0xFFCB" . "0x3156") ;; HALFWIDTH HANGUL LETTER YE
        ("0xFFCC" . "0x3157") ;; HALFWIDTH HANGUL LETTER O
        ("0xFFCD" . "0x3158") ;; HALFWIDTH HANGUL LETTER WA
        ("0xFFCE" . "0x3159") ;; HALFWIDTH HANGUL LETTER WAE
        ("0xFFCF" . "0x315A") ;; HALFWIDTH HANGUL LETTER OE
        ("0xFFD2" . "0x315B") ;; HALFWIDTH HANGUL LETTER YO
        ("0xFFD3" . "0x315C") ;; HALFWIDTH HANGUL LETTER U
        ("0xFFD4" . "0x315D") ;; HALFWIDTH HANGUL LETTER WEO
        ("0xFFD5" . "0x315E") ;; HALFWIDTH HANGUL LETTER WE
        ("0xFFD6" . "0x315F") ;; HALFWIDTH HANGUL LETTER WI
        ("0xFFD7" . "0x3160") ;; HALFWIDTH HANGUL LETTER YU
        ("0xFFDA" . "0x3161") ;; HALFWIDTH HANGUL LETTER EU
        ("0xFFDB" . "0x3162") ;; HALFWIDTH HANGUL LETTER YI
        ("0xFFDC" . "0x3163") ;; HALFWIDTH HANGUL LETTER I
        ("0xFFE0" . "0x00A2") ;; FULLWIDTH CENT SIGN
        ("0xFFE1" . "0x00A3") ;; FULLWIDTH POUND SIGN
        ("0xFFE2" . "0x00AC") ;; FULLWIDTH NOT SIGN
        ("0xFFE3" . "0x00AF") ;; FULLWIDTH MACRON
        ("0xFFE4" . "0x00A6") ;; FULLWIDTH BROKEN BAR
        ("0xFFE5" . "0x00A5") ;; FULLWIDTH YEN SIGN
        ("0xFFE6" . "0x20A9") ;; FULLWIDTH WON SIGN
        ("0xFFE8" . "0x2502") ;; HALFWIDTH FORMS LIGHT VERTICAL
        ("0xFFE9" . "0x2190") ;; HALFWIDTH LEFTWARDS ARROW
        ("0xFFEA" . "0x2191") ;; HALFWIDTH UPWARDS ARROW
        ("0xFFEB" . "0x2192") ;; HALFWIDTH RIGHTWARDS ARROW
        ("0xFFEC" . "0x2193") ;; HALFWIDTH DOWNWARDS ARROW
        ("0xFFED" . "0x25A0") ;; HALFWIDTH BLACK SQUARE
        ("0xFFEE" . "0x25CB") ;; HALFWIDTH WHITE CIRCLE
        ))
      '((all . identity)))
    ((not-inverse . t))))

;; translation definitions

(tae-declare-translation
 'unicode-translation-rule-for-jisx0221
 unicode-assoc-for-jisx0221)

(tae-declare-translation
 'unicode-translation-rule-for-jdk
 unicode-assoc-for-jdk)

(tae-declare-translation
 'unicode-translation-rule-for-windows
 unicode-assoc-for-windows)

(tae-declare-translation
 'unicode-translation-rule-for-fullwidth-or-halfwidth-normalization
 unicode-assoc-for-fullwidth-or-halfwidth-normalization)

(provide 'usupple)

;;; usupple ends here.

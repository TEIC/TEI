JIS X 0213 Emacsキット、使い方メモ。

 Emacs20上で、JIS X 0213を使うためのパッケージがこのディレクトリに
入っています。このパッケージは、基本的に川幡 太一氏によって作成され
ました。

このマニュアルは、使用法と、このパッケージにおける基本的な
設計を簡便に記してあります。(一部、himiによって補足されています。)

o... Install

このdirectoryをcurrentにして、

emacs -batch -q --no-site-file -l x0213-comp.el

もしくは、Meadowであれば、

MeadowNT(95).exe -batch -q --no-site-file -l x0213-comp.el

で、byte compileできます。あとは、このdirectoryの
中のすべてのファイルをload-pathの通ったところに置いてください。
その際、Mule-UCSもきちんとインストールしておいてください。

o... 基本設定

.emacsに(若しくは、site-start.elなどの適切なstart up fileに)
以下の行を加えるだけで、通常は問題がないはずです。
---
(require 'jisx0213)
---
言語環境(language-environment)の設定を行いたい場合は、上記の設定の
後に(重要、もし、上記の設定の前に行った場合は、JIS X 0213 環境が
現在の言語環境に導入されません。)以下の設定を加えてください。
---
(set-language-environment "Japanese")
---

o... Module 構成

jisx0213.el   ... setup module
    各moduleをrequireするだけです。

x0213-cdef.el ... charset definition
    define-charsetで、２つのcharsetを定義しています。

x0213-udef.el ... Mule-UCS definition

   Mule-UCSのパッケージ定義ファイルです。
  un-define.elcに依存し、un-defineを読み込んだあと、JIS X 0213と
  Unicode間の変換規則をunicode-basic-translation-ruleに導入します。

x0213-csys.el ... coding system definition

    coding-systemとして、iso-2022-jp-3(-strict, -compatible)と、
  euc-jisx0213, shift_jisx0213 を定義しています。euc-jisx0213、
  shift_jisx0213については、JIS X 0213の規格書を参照してください。なお、
  euc-jisx0213では、JIS X 0212文字もG3に対応しますので、euc-japanの上
  位互換となっています。

    iso-2022-jp-3(-strict, -compatible)のコーディング系の違いに付いて
  は、下の方を参照してください。

x0213-mime.el ... MIME encoding setup for APEL/FLIM/SEMI

    APEL, FLIM, SEMIのための設定ファイルです。
  JIS X 0213文字を含む文書をISO-2022-JP-3としてMIME符号化する際、
  変数 coding-system-for-mime-charset-iso-2022-jp-3 によって、
  iso-2022-jp-3-compatible, iso-2022-jp-3-strict, iso-2022-jp-3のうち
  のどれを使用するかを選択することができます。
  例えば、
---
(setq coding-system-for-mime-charset-iso-2022-jp-3 'iso-2022-jp-3)
---
  と、.emacsなどの設定ファイルに記述すると、`ISO-2022-JP-3' MIME charset
  を、符号化もしくは複合化するときに iso-2022-jp-3 coding systemを用います。

x0213-font.el ... font setup / its encoder definition.

  フォントの設定とフォント用のShift JISX0213 encoderを設定するモジュールです。

x0213-util.el ... other supplement JIS X 0213 utilities.

  JIS X 0213を扱う上で、有用な関数などの定義がなされているモジュールです。

x0213-sjis.el ... Shift-JIS encoder and decoder for JIS X 0213
                  This module is required only at byte-compile time.

  Shift JIS encoderおよびdecoderの設定を行うモジュールです。内部的に
Mule-UCSによって利用され、x0213-csys.elによって必要な部分が用いられます。
なお、このモジュールはバイトコンパイル時にのみ要求され、実行時には
通常、必要とされません。


o... フォントの設定

  (A) Shift-JIS フォントを使う。

      charset-registryが、"-shiftjis-0"であるフォントを使用できます。

      そのようなフォントの例としては、フリーウェアの"W1基本漢字フォン
      ト"があります。これはTrueType フォントですので、例えば以下のよう
      にBDFを作成してシステムに組み込みます。（ttf2bdfは
      FreeType(http://www.freetype.org)の標準添付ソフトです。）

    ttf2bdf -f waka -t w1 -eid 2 -r 72 -p 24 W1_01.TTF > w1-sjis-24.bdf

      なお、w1基本漢字フォントは、以下からダウンロード可能です。

         http://www.vector.co.jp/soft/data/writing/se136466.html

  (B) JIS X 0213のBDFフォントを使う。

      charset-registryが、"-jisx0213.2000-1"および"-jisx0213.2000-2"の
      フォントが使用できます。

      今村さんが作られたフリーな16ドットのJIS X 0213 BDFフォントは、
          http://www.mars.sphere.ne.jp/imamura/jisx0213.html
          ftp://ftp.m17n.org/pub/character/fonts
      から入手可能（になる予定）です。

  どちらのフォントを使うかによって、set-fontset-fontで設定する値が変り
  ますので、x0213-font.elの先頭部分を参考に、各自、必要ならば.emacs.el
  で、set-fontset-fontを実行しなおして下さい。

o... Meadow上でのフォント設定について。

  Meadowにおいても、もちろん、JIS X 0213 fontを用いることが出来ます。

　(1) BDF font の設定

     (a) ... すでに、BDF fontをMeadow上で利用しており、
             bdffont.elなどのモジュールを使用している場合、

       bdf-font-file-alistに
         (japanese-jisx0213-1 "<jiskan16-2000-1.bdfの(相対)パス>" 0)
         (japanese-jisx0213-2 "<jiskan16-2000-1.bdfの(相対)パス>" 0)
       の2つのエントリを新たに追加してください。

     (b) ... 既存のfontsetに、新たにjapanese-jisx0213-1 および
             japanese-jisx0213-2のcharsetにBDF fontを設定する場合。

       (w32-auto-regist-bdf-font
        "bdf-font-for-jiskan16-2000-1"
	"<jiskan16-2000-1.bdfの絶対パス>"
	 0)
       (w32-auto-regist-bdf-font
        "bdf-font-for-jiskan16-2000-2"
	"<jiskan16-2000-2.bdfの絶対パス>"
	 0)

       のようにして、まず、BDF fontを登録します。
       そのあと、使用したいfontsetに対して、

       (set-fontset-font
        "<登録したいFONTSETの名前>"
	'japanese-jisx0213-1
        "bdf-font-for-jiskan16-2000-1")
       (set-fontset-font
        "<登録したいFONTSETの名前>"
	'japanese-jisx0213-2
        "bdf-font-for-jiskan16-2000-2")

       のようにして、登録を行ってください。

　(2) TrueType font の利用

      残念ながら、現状のところ、Windows上で問題なく利用できるJIS X
      0213文字集合を持つTrueType fontの存在は確認できておりません。前
      述の"W1基本漢字フォント"が、現状で存在するおそらく唯一のJIS X
      0213文字集合をもつTrueType fontですが、筆者によって確かめた限り
      では、Windows NT/2000上では、Shift_JISX0213 encodingのフォントを
      扱うことが出来ない、言い換えると、JIS X 0213 repertorieを利用す
      ることができません。推測に過ぎませんが、おそらく、内部処理時に
      Unicodeに直すためだと思われます。このため、CP932で定められていな
      いcode rangeを持つ文字の表示が出来ないようです。Windows 95/98では、
      利用できるようですが(GDIでの内部処理がそもそもCP932だからであろう。)、
      将来的にはその限りではありません。
      ("W1基本漢字フォント"が、Shift_JISX0213 encodingではなく、独自の
      encoding にしてあれば、Meadow側でどうにかなったのかもしれませんが。)

      そのような問題があっても、Shift_JISX0213 encodingのTrueTypeフォ
      ントを利用したいという方の為に、このセクションでは、簡単に設定を
      紹介します。なお、既に述べたとおり、そのようなフォントには、現状
      では、"W1基本漢字フォント" しか存在しないと思われますが、このフォ
      ントは、propotionalフォントであり、しかも、サイズを小さくした時
      の品質が著しく劣るので、現状では、使用をお勧めしかねます。なお、
      その他のTrueType fontでも、JIS X 0213 レパートリーを包含する場合
      は、使用できるかもしませんが、このセクションでの内容は有効ではな
      い可能性があります。ご了承ください。現状では、propotional フォン
      トを無理にMeadowで使用したときには、問題が多いので、改善する可能
      性はありますが、早期にEmacs 21 baseに移行する方が根本的な対処に
      なると思われます。(のでEmacs20 baseのMeadowでは対策は行わないか
      もしれません。)
      
     (a) ... 手動設定の例

     low level font selection APIを用いて、手動設定をしてください。
     その際、encoderに、shift-jisx0213-font-encoderを指定してください。

     例: 
     注:(logfontや、<YOUR FONTSET NAME>の部分は、お好みの形に修正してください。)
---
(let* ((fontsetname "<YOUR FONTSET NAME>")
       (fontname "W1-ShiftJISX0213-font")
       (logfont '(w32-logfont "W1-01xxxx" 8 16 400 0 nil nil nil 128 1 3 2))
       (preference
	(append
	 '((encoding-type . 4)
	   (encoder . shift-jisx0213-font-encoder))
	 (w32-get-logfont-info logfont))))
  (w32-add-font
   fontname
   preference)
  (w32-change-font-logfont
   fontname 0 logfont)
  (set-fontset-font
   fontsetname 'japanese-jisx0213-1 fontname)
  (set-fontset-font
   fontsetname 'japanese-jisx0213-2 fontname))
---

     (b) ... その他

     将来のMeadowでは、JIS X 0213 fontの自動設定機能を組み込む可能性が
     あります。もし、そのような自動設定機能が有効である場合には、上記の
     設定はhigh-level font setlection APIで置き換えることができるように
     なるでしょう。

     これらの仕様は将来予告なく変更されることがありますので、ご注意ください。


o... コード系、`iso-2022-jp-3-compatible'の注意事項と、
     `iso-2022-jp-strict', `iso-2022-jp-3'との相違点

    JIS X 0213:2000の附属書２「ISO-2022-JP-3符号化表現」の4.1 (e)では、
  ISO-2022-JP-3の符号化に際し、1B 24 42の切り替え符号以降で使用しては
  いけないJIS X 0213-1文字集合を附属書２表１で定めています。

    この表には、JIS X 0208で定義されていない文字はもちろんのこと、JIS
  X 0208とJIS X 0213の包摂基準が変更された漢字等も含まれています。例え
  ば、「噛」、「黄」、「鴎」など漢字は、ISO-2022-JP-3では 1B 24 42 呼
  び出しでの使用が禁止されています。

    しかしJIS X 0208漢字の中から、これら1B 24 42での使用禁止漢字をいち
  いち、1B 24 28 4Fに切り替え直してしまうと、ISO-2022-JPで保存されたファ
  イルの内容を読み込み、再び保存する際に内容が破壊される可能性がありま
  す。

    これを防ぐため、コード系`iso-2022-jp-3-compatible'では、これら「禁
  止漢字」も、1B 24 42で符号化します。そのため、
  iso-2022-jp-3-compatibleは、iso-2022-jpの完全上位互換となります。
  （JIS X 0208において未定義の文字が1B 24 42で呼ばれることはありませ
  ん。）

    一方、iso-202-jp-3-strictは、あくまでISO-2022-JP-3に従いつつも、可
  能な限りのJIS X 0208文字を、1B 24 42で符号化します。そのため、
  iso-2022-jp-3-strictでで符号化した文書は、ISO-2022-JPにしか対応して
  いないエディタでも「ある程度は」読めるようになっています。

    これらのコード系における、1B 24 42と、1B 24 28 4Fの混在が気になる
  場合は、JIS X 0213 文書の保存を、コード系`iso-2022-jp-3' で行なって
  ください。このコード系では、全てのJIS X 0213-1の文字を1B 24 28 4Fで
  呼び出します。ただし、このコード系で保存した文書が、JIS X 0213に未対
  応なエディタ等で読める可能性は低くなります。

    以下に、「森鴎外」を各コード系でエンコードした場合の出力例を示しま
  す。

  iso-2022-jp-3-compatible：
    1B 24 42 3F 39 32 2A 33 30 1B 28 42
    ESC $ B  ?  9  2  *  3  0  ESC ( B

  iso-2022-jp-3-strict：
    1B 24 42 3F 39 1B 24 28 4F 32 2A 1B 24 42 33 30 1B 28 42
    ESC $ B  ?  9  ESC $ (  O  2  * ESC $  B  3  0  ESC ( B

  iso-2022-jp-3：
    1B 24 28 4F 3F 39 32 2A 33 30 1B 28 42
    ESC $ (  O  ?  9  2  *  3  0  ESC ( B

o ... JIS X 0213で利用できる辞書

  http://www.m17n.org/kawabata/x0213dic/ に、現在フリーで出回っている
  辞書をJIS X 0213用に変換したものを用意したので、ご利用下さい。

o... 今後の予定

  辞書の整備
  入力ツールの整備
  変換用データの整備

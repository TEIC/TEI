<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:s="http://www.ascc.net/xml/schematron"
                
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="s a rng tei teix"
                version="2.0">

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet customization module for LaTeX output.</p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:key name="ENDNOTES" match="tei:note[tei:isEndNote(.)]" use="1"/>
  <xsl:key name="FOOTNOTES" match="tei:note[tei:isFootNote(.) ]" use="1"/>
  <xsl:key name="TREES" match="tei:eTree[not(ancestor::tei:eTree)]" use="1"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[latex] Hook where LaTeX commands can be inserted after 
the beginning of the document</desc>
   </doc>
   <xsl:template name="beginDocumentHook"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[latex] Hook where LaTeX commands can be at start of setup</desc>
   </doc>
   <xsl:template name="latexSetupHook"/>
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="hook">
      <desc>[latex] Hook where LaTeX commands can be inserted in the
    preamble before the beginning of the document</desc>
   </doc>
   <xsl:template name="latexPreambleHook"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>Optional parameters for documentclass</desc>
   </doc>
   <xsl:param name="classParameters">11pt,twoside</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="string">
      <desc>location of original XML file, for looking up relative pointers</desc>
   </doc>
   <xsl:param name="ORIGDIR"/>
    
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>Logo graphics file</desc>
   </doc>
   <xsl:param name="latexLogo"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="string">
      <desc>URL root where referenced documents are located</desc>
   </doc>
   <xsl:param name="baseURL"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="userpackage" type="string">
      <desc>The name of a LaTeX style package which should be loaded</desc>
   </doc>
   <xsl:param name="userpackage"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="output" type="boolean">
      <desc>Use real name of graphics files rather than pointers</desc>
   </doc>
   <xsl:param name="realFigures">true</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX package setup</p>
         <p>Declaration of the LaTeX packages needed to implement
    this markup</p>
      </desc>
   </doc>
   <xsl:template name="latexPackages">
      <xsl:text>
\usepackage[</xsl:text>
      <xsl:value-of select="$latexPaperSize"/>
      <xsl:text>,</xsl:text>
      <xsl:value-of select="$latexGeometryOptions"/>
      <xsl:text>]{geometry}
\usepackage{framed}
</xsl:text>
<xsl:text>
\definecolor{shadecolor}{gray}{0.95}
\usepackage{longtable}
\usepackage[normalem]{ulem}
\usepackage{fancyvrb}
\usepackage{fancyhdr}
\usepackage{graphicx}
\usepackage{marginnote}

</xsl:text>
<xsl:if test="not($marginFont='')">
\renewcommand*{\marginfont}{<xsl:value-of select="$marginFont"/>}
</xsl:if>
<xsl:if test="key('TREES',1)">
  \usepackage{pstricks,pst-node,pst-tree}
</xsl:if>
<xsl:if test="key('ENDNOTES',1)">
  \usepackage{endnotes}
  <xsl:choose>
    <xsl:when test="key('FOOTNOTES',1)">
      \def\theendnote{\@alph\c@endnote}
    </xsl:when>
    <xsl:otherwise>
      \def\theendnote{\@arabic\c@endnote}
    </xsl:otherwise>
  </xsl:choose>
</xsl:if>
<xsl:text>
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
</xsl:text>
      <xsl:if test="not($userpackage='')">
  \usepackage{<xsl:value-of select="$userpackage"/>}
</xsl:if>
      <xsl:text>
  \pagestyle{</xsl:text><xsl:value-of select="$pageStyle"/><xsl:text>}
</xsl:text>
\usepackage[pdftitle={<xsl:sequence select="tei:generateSimpleTitle(.)"/>},
 pdfauthor={<xsl:sequence
 select="replace(string-join(tei:generateAuthor(.),''),'\\[A-z]+','')"/>}]{hyperref}
\hyperbaseurl{<xsl:value-of select="$baseURL"/>}
<xsl:if test="count(key('APP',1))&gt;0">
\usepackage[noreledmac]{eledmac}
<xsl:call-template name="ledmacOptions"/>
</xsl:if>

   </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="float">
      <desc>When processing a "pb" element, decide what to generate: "active"
generates a page break; "visible" generates a bracketed number (with
scissors), and "bracketsonly" generates a bracketed number (without
scissors).</desc>
   </doc>
   <xsl:param name="pagebreakStyle"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="float">
      <desc>When making a table, what width must be constrained to fit,
as a proportion of the page width.</desc>
   </doc>
   <xsl:param name="tableMaxWidth">0.85</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>Which environment to use for quotes (quote, quotation, quoting, ...)</desc>
   </doc>
   <xsl:param name="quoteEnv">quote</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="boolean">
      <desc>Whether to number lines of poetry</desc>
   </doc>
   <xsl:param name="verseNumbering">false</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="integer">
      <desc>When numbering poetry, how often to put in a line number</desc>
   </doc>
   <xsl:param name="everyHowManyLines">5</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout" type="string">
      <desc>When numbering poetry, when to restart the sequence; this must be the name of a TEI element</desc>
   </doc>
   <xsl:param name="resetVerseLineNumbering">div1</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="userpackage" type="string">
      <desc>Options to pass to the geometry package to set margins etc</desc>
   </doc>
   <xsl:param name="latexGeometryOptions">twoside,lmargin=1in,rmargin=1in,tmargin=1in,bmargin=1in,marginparwidth=0.75in</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="userpackage" type="string">
      <desc>The page style to use with the \pagestyle command (empty, plain, fancy, ...).</desc>
   </doc>
   <xsl:param name="pageStyle">fancy</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="userpackage" type="string">
      <desc>Configuration to pass to hypersetup.</desc>
   </doc>
   <xsl:param name="hyperSetup">linkbordercolor=0.75 0.75 0.75,urlbordercolor=0.75 0.75 0.75,bookmarksnumbered=true</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="userpackage" type="string">
      <desc>Depth of nesting of reference documentation when processing ODD</desc>
   </doc>
   <xsl:param name="specLinkDepth">2</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX setup</p>
         <p>The basic LaTeX setup which you should not 
really tinker with unless you really understand  why and how. Note
that we need to set up a mapping here for Unicode 8421, 10100 and
10100 to glyphs for backslash and the two curly brackets, to provide literal
characters. The normal characters remain active for LaTeX commands.
</p>
      </desc>
   </doc>
   <xsl:template name="latexSetup">
   <xsl:call-template name="latexSetupHook"/>
\IfFileExists{xcolor.sty}%
  {\RequirePackage{xcolor}}%
  {\RequirePackage{color}}
\usepackage{colortbl}
\usepackage{wrapfig}
\usepackage{ifxetex}
\ifxetex
  \usepackage{fontspec}
  \usepackage{xunicode}
  \catcode`⃥=\active \def⃥{\textbackslash}
  \catcode`❴=\active \def❴{\{}
  \catcode`❵=\active \def❵{\}}
  \def\textJapanese{\fontspec{IPAMincho}}
  \def\textChinese{\fontspec{HAN NOM A}\XeTeXlinebreaklocale "zh"\XeTeXlinebreakskip = 0pt plus 1pt }
  \def\textKorean{\fontspec{Baekmuk Gulim} }
  \setmonofont{<xsl:value-of select="$typewriterFont"/>}
  <xsl:if test="not($sansFont='')">
    \setsansfont{<xsl:value-of select="$sansFont"/>}
  </xsl:if>
  <xsl:if test="not($romanFont='')">
    \setromanfont{<xsl:value-of select="$romanFont"/>}
  </xsl:if>
\else
  \IfFileExists{utf8x.def}%
   {\usepackage[utf8x]{inputenc}
      \PrerenderUnicode{–}
    }%
   {\usepackage[utf8]{inputenc}}
  <xsl:call-template name="latexBabel"/>
  \usepackage[T1]{fontenc}
  \usepackage{float}
  \usepackage[]{ucs}
  \uc@dclc{8421}{default}{\textbackslash }
  \uc@dclc{10100}{default}{\{}
  \uc@dclc{10101}{default}{\}}
  \uc@dclc{8491}{default}{\AA{}}
  \uc@dclc{8239}{default}{\,}
  \uc@dclc{20154}{default}{ }
  \uc@dclc{10148}{default}{>}
  \def\textschwa{\rotatebox{-90}{e}}
  \def\textJapanese{}
  \def\textChinese{}
  \IfFileExists{tipa.sty}{\usepackage{tipa}}{}
  \usepackage{times}
\fi
\def\exampleFont{\ttfamily\small}
\DeclareTextSymbol{\textpi}{OML}{25}
\usepackage{relsize}
\RequirePackage{array}
\def\@testpach{\@chclass
 \ifnum \@lastchclass=6 \@ne \@chnum \@ne \else
  \ifnum \@lastchclass=7 5 \else
   \ifnum \@lastchclass=8 \tw@ \else
    \ifnum \@lastchclass=9 \thr@@
   \else \z@
   \ifnum \@lastchclass = 10 \else
   \edef\@nextchar{\expandafter\string\@nextchar}%
   \@chnum
   \if \@nextchar c\z@ \else
    \if \@nextchar l\@ne \else
     \if \@nextchar r\tw@ \else
   \z@ \@chclass
   \if\@nextchar |\@ne \else
    \if \@nextchar !6 \else
     \if \@nextchar @7 \else
      \if \@nextchar (8 \else
       \if \@nextchar )9 \else
  10
  \@chnum
  \if \@nextchar m\thr@@\else
   \if \@nextchar p4 \else
    \if \@nextchar b5 \else
   \z@ \@chclass \z@ \@preamerr \z@ \fi \fi \fi \fi
   \fi \fi  \fi  \fi  \fi  \fi  \fi \fi \fi \fi \fi \fi}
\gdef\arraybackslash{\let\\=\@arraycr}
\def\@textsubscript#1{{\m@th\ensuremath{_{\mbox{\fontsize\sf@size\z@#1}}}}}
\def\Panel#1#2#3#4{\multicolumn{#3}{){\columncolor{#2}}#4}{#1}}
\def\abbr{}
\def\corr{}
\def\expan{}
\def\gap{}
\def\orig{}
\def\reg{}
\def\ref{}
\def\sic{}
\def\persName{}\def\name{}
\def\placeName{}
\def\orgName{}
\def\textcal#1{{\fontspec{<xsl:value-of select="$calligraphicFont"/>}#1}}
\def\textgothic#1{{\fontspec{<xsl:value-of select="$gothicFont"/>}#1}}
\def\textlarge#1{{\large #1}}
\def\textoverbar#1{\ensuremath{\overline{#1}}}
\def\textquoted#1{‘#1’}
\def\textsmall#1{{\small #1}}
\def\textsubscript#1{\@textsubscript{\selectfont#1}}
\def\textxi{\ensuremath{\xi}}
\def\titlem{\itshape}
\newenvironment{biblfree}{}{\ifvmode\par\fi }
\newenvironment{bibl}{}{}
\newenvironment{byline}{\vskip6pt\itshape\fontsize{16pt}{18pt}\selectfont}{\par }
\newenvironment{citbibl}{}{\ifvmode\par\fi }
\newenvironment{docAuthor}{\ifvmode\vskip4pt\fontsize{16pt}{18pt}\selectfont\fi\itshape}{\ifvmode\par\fi }
\newenvironment{docDate}{}{\ifvmode\par\fi }
\newenvironment{docImprint}{\vskip 6pt}{\ifvmode\par\fi }
\newenvironment{docTitle}{\vskip6pt\bfseries\fontsize{18pt}{22pt}\selectfont}{\par }
\newenvironment{msHead}{\vskip 6pt}{\par}
\newenvironment{msItem}{\vskip 6pt}{\par}
\newenvironment{rubric}{}{}
\newenvironment{titlePart}{}{\par }
<xsl:text disable-output-escaping="yes">
\newcolumntype{L}[1]{){\raggedright\arraybackslash}p{#1}}
\newcolumntype{C}[1]{){\centering\arraybackslash}p{#1}}
\newcolumntype{R}[1]{){\raggedleft\arraybackslash}p{#1}}
\newcolumntype{P}[1]{){\arraybackslash}p{#1}}
\newcolumntype{B}[1]{){\arraybackslash}b{#1}}
\newcolumntype{M}[1]{){\arraybackslash}m{#1}}
\definecolor{label}{gray}{0.75}
\def\unusedattribute#1{\sout{\textcolor{label}{#1}}}
\DeclareRobustCommand*{\xref}{\hyper@normalise\xref@}
\def\xref@#1#2{\hyper@linkurl{#2}{#1}}
\begingroup
\catcode`\_=\active
\gdef_#1{\ensuremath{\sb{\mathrm{#1}}}}
\endgroup
\mathcode`\_=\string"8000
\catcode`\_=12\relax
</xsl:text>
   </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX loading of babel with options</p>
      </desc>
   </doc>
   <xsl:template name="latexBabel">
<xsl:text>\usepackage[english]{babel}</xsl:text>
</xsl:template>



<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
  <desc>
    <p>LaTeX paper size</p>
  </desc>
</doc>
<xsl:param name="latexPaperSize">a4paper</xsl:param>
     
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for examples</desc>   </doc>
<xsl:param name="exampleFont">Courier New</xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for literal code</desc>   </doc>
<xsl:param name="typewriterFont">DejaVu Sans Mono</xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for sans-serif</desc>   </doc>
<xsl:param name="sansFont"></xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for serif</desc>   </doc>
<xsl:param name="romanFont"></xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for gothic</desc>   </doc>
<xsl:param name="gothicFont">Lucida Blackletter</xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Font for calligraphic</desc>   </doc>
<xsl:param name="calligraphicFont">Lucida Calligraphy</xsl:param>
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="style" type="string"><desc>Command to set margin font</desc>   </doc>
<xsl:param name="marginFont">\itshape\footnotesize</xsl:param>
  <xsl:param name="longtables">true</xsl:param>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX layout preamble</p>
         <p>All the LaTeX setup which affects page layout</p>
      </desc>
   </doc>
   <xsl:template name="latexLayout">
     <xsl:choose>
       <xsl:when test="$latexPaperSize='a3paper'">
	 \paperwidth297mm
	 \paperheight420mm
       </xsl:when>
       <xsl:when test="$latexPaperSize='a5paper'">	
	 \paperwidth148mm
	 \paperheight210mm
       </xsl:when>
       <xsl:when test="$latexPaperSize='a4paper'">
	 \paperwidth210mm
	 \paperheight297mm
       </xsl:when>
       <xsl:when test="$latexPaperSize='letterpaper'">
	 \paperwidth216mm
	 \paperheight279mm
       </xsl:when>
	 <xsl:otherwise>
	 </xsl:otherwise>
       </xsl:choose>       
\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{3}
\clubpenalty=8000
\emergencystretch 3em
\hbadness=4000
\hyphenpenalty=400
\pretolerance=750
\tolerance=2000
\vbadness=4000
\widowpenalty=10000
<xsl:if test="not($documentclass='letter')">
\renewcommand\section{\@startsection {section}{1}{\z@}%
     {-1.75ex \@plus -0.5ex \@minus -.2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\Large\bfseries\sffamily}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
     {-1.75ex\@plus -0.5ex \@minus- .2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\Large\sffamily}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
     {-1.5ex\@plus -0.35ex \@minus -.2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\large\sffamily}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
     {-1ex \@plus-0.35ex \@minus -0.2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\normalsize\sffamily}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
     {1.5ex \@plus1ex \@minus .2ex}%
     {-1em}%
     {\reset@font\normalsize\bfseries}}

</xsl:if>
\def\l@section#1#2{\addpenalty{\@secpenalty} \addvspace{1.0em plus 1pt}
 \@tempdima 1.5em \begingroup
 \parindent \z@ \rightskip \@pnumwidth 
 \parfillskip -\@pnumwidth 
 \bfseries \leavevmode #1\hfil \hbox to\@pnumwidth{\hss #2}\par
 \endgroup}
\def\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\def\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\def\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\def\l@subparagraph{\@dottedtocline{5}{10em}{5em}}
\@ifundefined{c@section}{\newcounter{section}}{}
\@ifundefined{c@chapter}{\newcounter{chapter}}{}
\newif\if@mainmatter 
\@mainmattertrue
\def\chaptername{Chapter}
\def\frontmatter{%
  \pagenumbering{roman}
  \def\thechapter{\@roman\c@chapter}
  \def\theHchapter{\roman{chapter}}
  \def\thesection{\@roman\c@section}
  \def\theHsection{\roman{section}}
  \def\@chapapp{}%
}
\def\mainmatter{%
  \cleardoublepage
  \def\thechapter{\@arabic\c@chapter}
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \pagenumbering{arabic}
  \setcounter{secnumdepth}{6}
  \def\@chapapp{\chaptername}%
  \def\theHchapter{\arabic{chapter}}
  \def\thesection{\@arabic\c@section}
  \def\theHsection{\arabic{section}}
}
\def\backmatter{%
  \cleardoublepage
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \setcounter{secnumdepth}{2}
  \def\@chapapp{\appendixname}%
  \def\thechapter{\@Alph\c@chapter}
  \def\theHchapter{\Alph{chapter}}
  \appendix
}
\newenvironment{bibitemlist}[1]{%
   \list{\@biblabel{\@arabic\c@enumiv}}%
       {\settowidth\labelwidth{\@biblabel{#1}}%
        \leftmargin\labelwidth
        \advance\leftmargin\labelsep
        \@openbib@code
        \usecounter{enumiv}%
        \let\p@enumiv\@empty
        \renewcommand\theenumiv{\@arabic\c@enumiv}%
	}%
  \sloppy
  \clubpenalty4000
  \@clubpenalty \clubpenalty
  \widowpenalty4000%
  \sfcode`\.\@m}%
  {\def\@noitemerr
    {\@latex@warning{Empty `bibitemlist' environment}}%
    \endlist}

\def\tableofcontents{\section*{\contentsname}\@starttoc{toc}}
\parskip<xsl:value-of select="$parSkip"/>
\parindent<xsl:value-of select="$parIndent"/>
\def\Panel#1#2#3#4{\multicolumn{#3}{){\columncolor{#2}}#4}{#1}}
\newenvironment{reflist}{%
  \begin{raggedright}\begin{list}{}
  {%
   \setlength{\topsep}{0pt}%
   \setlength{\rightmargin}{0.25in}%
   \setlength{\itemsep}{0pt}%
   \setlength{\itemindent}{0pt}%
   \setlength{\parskip}{0pt}%
   \setlength{\parsep}{2pt}%
   \def\makelabel##1{\itshape ##1}}%
  }
  {\end{list}\end{raggedright}}
\newenvironment{sansreflist}{%
  \begin{raggedright}\begin{list}{}
  {%
   \setlength{\topsep}{0pt}%
   \setlength{\rightmargin}{0.25in}%
   \setlength{\itemindent}{0pt}%
   \setlength{\parskip}{0pt}%
   \setlength{\itemsep}{0pt}%
   \setlength{\parsep}{2pt}%
   \def\makelabel##1{\upshape\sffamily ##1}}%
  }
  {\end{list}\end{raggedright}}
\newenvironment{specHead}[2]%
 {\vspace{20pt}\hrule\vspace{10pt}%
  \label{#1}\markright{#2}%
<xsl:text>
  \pdfbookmark[</xsl:text>
      <xsl:value-of select="$specLinkDepth"/>
      <xsl:text>]{#2}{#1}%
  \hspace{-0.75in}{\bfseries\fontsize{16pt}{18pt}\selectfont#2}%
  }{}
      </xsl:text>   
      <xsl:call-template name="latexPreambleHook"/>
   </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX setup commands for ledmac package</p>
      </desc>
   </doc>
<xsl:template name="ledmacOptions">
<xsl:text>
\renewcommand{\notenumfont}{\bfseries}
\lineation{page}
\linenummargin{inner}
\footthreecol{A}
\foottwocol{B}
</xsl:text>
</xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX setup before start of document</p>
         <p>All the LaTeX setup which are executed before the start of
    the document</p>
      </desc>
   </doc>
   <xsl:template name="latexBegin">
      <xsl:text>
\makeatletter
\newcommand*{\cleartoleftpage}{%
  \clearpage
    \if@twoside
    \ifodd\c@page
      \hbox{}\newpage
      \if@twocolumn
        \hbox{}\newpage
      \fi
    \fi
  \fi
}
\makeatother
\makeatletter
\thispagestyle{empty}
\markright{\@title}\markboth{\@title}{\@author}
\renewcommand\small{\@setfontsize\small{9pt}{11pt}\abovedisplayskip 8.5\p@ plus3\p@ minus4\p@
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip \z@ plus2\p@
\belowdisplayshortskip 4\p@ plus2\p@ minus2\p@
\def\@listi{\leftmargin\leftmargini
               \topsep 2\p@ plus1\p@ minus1\p@
               \parsep 2\p@ plus\p@ minus\p@
               \itemsep 1pt}
}
\makeatother
\fvset{frame=single,numberblanklines=false,xleftmargin=5mm,xrightmargin=5mm}
\fancyhf{} 
\setlength{\headheight}{14pt}
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{\TheID}
\fancyfoot[LE]{}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{\TheID}
\hypersetup{</xsl:text><xsl:value-of select="$hyperSetup"/><xsl:text>}
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}</xsl:text>
   </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" class="layout">
      <desc>
         <p>LaTeX setup at end of document</p>
         <p>All the LaTeX setup which are executed at the end of
    the document</p>
      </desc>
   </doc>
   <xsl:template name="latexEnd">
</xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] Title banner </desc>
   </doc>
   <xsl:template name="printTitleAndLogo">
\makeatletter
\noindent\parbox[b]{.75\textwidth}{\fontsize{14pt}{16pt}\bfseries\raggedright\sffamily\selectfont \@title}
\vskip20pt
\par\noindent{\fontsize{11pt}{13pt}\sffamily\itshape\raggedright\selectfont\@author\hfill\TheDate}
\vspace{18pt}
\makeatother
</xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML element in a verbatim context</desc>
  </doc>

  <xsl:template name="Element">
    <xsl:param name="content"/>
    <xsl:text>{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML element name in a verbatim context</desc>
  </doc>
  <xsl:template name="ElementName">
    <xsl:param name="content"/>
    <xsl:text>\textbf{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML element name highlighted in a verbatim context</desc>
  </doc>
 <xsl:template name="HighlightElementName">
    <xsl:param name="content"/>
    <xsl:text>\textcolor{red}{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute value in a verbatim context</desc>
  </doc>

  <xsl:template name="AttributeValue">
    <xsl:param name="content"/>
    <xsl:text>{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute in a verbatim context</desc>
  </doc>

  <xsl:template name="Attribute">
    <xsl:param name="content"/>
    <xsl:text>{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML namespace in a verbatim context</desc>
  </doc>
  <xsl:template name="Namespace">
    <xsl:param name="content"/>
    <xsl:text>\color{red}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text></xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML comment in a verbatim context</desc>
  </doc>
  <xsl:template name="Comment">
    <xsl:param name="content"/>
    <xsl:text>\textit{</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] the content of a list item</desc>
  </doc>
  <xsl:template name="makeItem">
      <xsl:text>&#10;\item</xsl:text>
      <xsl:if test="@n">[<xsl:value-of select="@n"/>]</xsl:if>
      <xsl:text> </xsl:text>
      <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:call-template name="rendering"/>
  </xsl:template>  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] the content of a list item in a gloss list</desc>
  </doc>
  <xsl:template name="makeLabelItem">
      <xsl:text>&#10;\item</xsl:text>
      <xsl:if test="@n">[<xsl:value-of select="@n"/>]</xsl:if>
      <xsl:text> </xsl:text>
      <xsl:call-template name="rendering"/>
  </xsl:template>  

</xsl:stylesheet>

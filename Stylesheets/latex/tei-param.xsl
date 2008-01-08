<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="xd exsl estr edate a rng tei teix xd" version="1.0">

<xd:doc type="stylesheet">
    <xd:short>TEI stylesheet customization module for LaTeX output.</xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2007, TEI Consortium</xd:copyright>
</xd:doc>

<xd:doc class="hook">
    <xd:short>[latex] Hook where LaTeX commands can be inserted after 
the beginning of the document</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="beginDocumentHook"/>

<xd:doc class="hook">
    <xd:short>[latex] Hook where LaTeX commands can be inserted in the
    preamble before the beginning of the document</xd:short>
    <xd:detail> </xd:detail>
</xd:doc>
<xsl:template name="latexPreambleHook"/>

<xd:doc type="string" class="layout">
Optional parameters for documentclass
</xd:doc>
<xsl:param name="classParameters">11pt</xsl:param>

<xd:doc type="string" class="layout">
Logo graphics file
</xd:doc>
<xsl:param name="latexLogo"/>

<xd:doc type="string" class="output">
URL root where referenced documents are located
</xd:doc>
<xsl:param name="baseURL"></xsl:param>

<xd:doc type="boolean" class="output">
Whether or not to load LaTeX packages which attempt to
process the UTF-8 characters. Set to "false" if you are
using XeTeX or similar.
</xd:doc>
<xsl:param name="reencode">true</xsl:param>

<xd:doc type="string" class="userpackage">
The name of a LaTeX style package which should be loaded
</xd:doc>
<xsl:param name="userpackage"></xsl:param>

<xd:doc type="boolean" class="output">
Use real name of graphics files rather than pointers
</xd:doc>
<xsl:param name="realFigures">true</xsl:param>

<xd:doc class="layout">
    <xd:short>LaTeX package setup</xd:short>
    <xd:detail>Declaration of the LaTeX packages needed to implement
    this markup</xd:detail>
</xd:doc>
<xsl:template name="latexPackages">
<xsl:text>&#10;\usepackage[</xsl:text>
<xsl:value-of select="$latexGeometryOptions"/>
<xsl:text>]{geometry}
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.95}
\usepackage{longtable}
\usepackage{colortbl}
\usepackage[normalem]{ulem}
\usepackage{fancyvrb}
\usepackage{fancyhdr}
\usepackage{graphicx}
\usepackage{endnotes}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
</xsl:text>
<xsl:if test="$reencode='true'">
<xsl:text>
\IfFileExists{tipa.sty}{\usepackage{tipa}}{}
\usepackage{times}
</xsl:text>
</xsl:if>
<xsl:if test="not($userpackage='')">
  \usepackage{<xsl:value-of select="$userpackage"/>}
</xsl:if>
<xsl:text>
  \pagestyle{fancy} 
</xsl:text>
</xsl:template>

<xd:doc type="float" class="layout">
When processing a "pb" element, decide what to generate: "active" generates a page break; "visible" genrates a bracketed number (with scissors), and "bracketsonly" generates a bracketed number (without scissors).
</xd:doc>
<xsl:param name="pagebreakStyle"></xsl:param>

<xd:doc type="float" class="layout">
When making a table, what width must be constrained to fit,
as a proportion of the page width.
</xd:doc>
<xsl:param name="tableMaxWidth">0.85</xsl:param>

<xd:doc type="boolean" class="layout">
Whether to number lines of poetry
</xd:doc>
<xsl:param name="verseNumbering">false</xsl:param>

<xd:doc type="integer" class="layout">
When numbering poetry, how often to put in a line number
</xd:doc>
<xsl:param name="everyHowManyLines">5</xsl:param>

<xd:doc type="string" class="layout">
When numbering poetry, when to restart the sequence;
this must be the name of a TEI element
</xd:doc>
<xsl:param name="resetVerseLineNumbering">div1</xsl:param>

<xd:doc type="string" class="userpackage">
Options to pass to the geometry package to set margins etc
</xd:doc>
<xsl:param name="latexGeometryOptions">twoside,a4paper,lmargin=1in,rmargin=1in,tmargin=1in,bmargin=1in</xsl:param>

<xd:doc class="layout">
    <xd:short>LaTeX setup</xd:short>
    <xd:detail>The basic LaTeX setup which you should not 
really tinker with unless you really understand  why and how. Note
that we need to set up a mapping here for Unicode 8421, 10100 and
10100 to glyphs for backslash and the two curly brackets, to provide literal
characters. The normal characters remain active for LaTeX commands.
Note that if $reencode is set to false, no input or output encoding
packages are loaded, since it is assumed you are using a TeX variant
capable of dealing with UTF-8 directly.
</xd:detail>
</xd:doc>
<xsl:template name="latexSetup">
<xsl:choose>
<xsl:when test="$reencode='true'">
\IfFileExists{utf8x.def}%
 {\usepackage[utf8x]{inputenc}}%
 {\usepackage[utf8]{inputenc}}
<xsl:call-template name="latexBabel"/>
\usepackage[T1]{fontenc}
\usepackage[]{ucs}
\uc@dclc{8421}{default}{\textbackslash }
\uc@dclc{10100}{default}{\{}
\uc@dclc{10101}{default}{\}}
</xsl:when>
<xsl:otherwise>
\usepackage{fontspec}
\usepackage{xunicode}
\catcode`⃥=\active \def⃥{\textbackslash}
\catcode`❴=\active \def❴{\{}
\catcode`❵=\active \def❵{\}}
</xsl:otherwise>
</xsl:choose>
\DeclareTextSymbol{\textpi}{OML}{25}
\usepackage{relsize}
\def\textsubscript#1{%
  \@textsubscript{\selectfont#1}}
\def\@textsubscript#1{%
  {\m@th\ensuremath{_{\mbox{\fontsize\sf@size\z@#1}}}}}
\def\textquoted#1{‘#1’}
\def\textcal#1{\ensuremath{\mathcal{#1}}}
\def\textsmall#1{{\small #1}}
\def\textlarge#1{{\large #1}}
\def\textoverbar#1{\ensuremath{\overline{#1}}}
\def\textgothic#1{\ensuremath{\mathscr{#1}}}
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
\def\Panel#1#2#3#4{\multicolumn{#3}{){\columncolor{#2}}#4}{#1}}
\gdef\arraybackslash{\let\\=\@arraycr}
<xsl:text disable-output-escaping="yes">
\newcolumntype{L}[1]{){\raggedright\arraybackslash}p{#1}}
\newcolumntype{C}[1]{){\centering\arraybackslash}p{#1}}
\newcolumntype{R}[1]{){\raggedleft\arraybackslash}p{#1}}
\newcolumntype{P}[1]{){\arraybackslash}p{#1}}
\IfFileExists{xcolor.sty}%
  {\RequirePackage{xcolor}}%
  {\RequirePackage{color}}
\definecolor{label}{gray}{0.75}
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
  \hypertarget{#1}{}%
  \markright{#1}%
  \pdfbookmark[1]{#2}{#1}%
  \hspace{-0.75in}{\bfseries\fontsize{16pt}{18pt}\selectfont#2}%
  }{}
\DeclareRobustCommand*{\xref}{\hyper@normalise\xref@}
\def\xref@#1#2{\hyper@linkurl{#2}{#1}}
\def\Div[#1]#2{\section*{#2}}
\catcode`\_=12\relax
\def\exampleFontSet{\ttfamily\small\selectfont}
</xsl:text>

</xsl:template>

<xd:doc class="layout">
    <xd:short>LaTeX babel setup</xd:short>
    <xd:detail>LaTeX loading of babel with options</xd:detail>
</xd:doc>
<xsl:template name="latexBabel">
\usepackage[russianb,greek,english]{babel}
</xsl:template>

<xd:doc class="layout">
    <xd:short>LaTeX layout preamble</xd:short>
    <xd:detail>All the LaTeX setup which affects page layout</xd:detail>
</xd:doc>
<xsl:template name="latexLayout">
\paperwidth211mm
\paperheight297mm
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
<xsl:if test="not($docClass='letter')">
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
\def\frontmatter{%
  \pagenumbering{roman}
  \def\thechapter{\@roman\c@chapter}
  \def\theHchapter{\alph{chapter}}
  \def\@chapapp{}%
}
\def\mainmatter{%
  \cleardoublepage
  \def\thechapter{\@arabic\c@chapter}
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \pagenumbering{arabic}
  \setcounter{secnumdepth}{6}
  \def\@chapapp{Chapter}%
  \def\theHchapter{\arabic{chapter}}
}
\def\backmatter{%
  \cleardoublepage
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \setcounter{secnumdepth}{0}
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
\usepackage[pdftitle={<xsl:call-template name="generateSimpleTitle"/>},
 pdfauthor={<xsl:call-template name="generateAuthor"/>}]{hyperref}
\hyperbaseurl{<xsl:value-of select="$baseURL"/>}
<xsl:call-template name="latexPreambleHook"/>
</xsl:template>

<xd:doc class="layout">
    <xd:short>LaTeX setup at start of document</xd:short>
    <xd:detail>All the LaTeX setup which are executed at the start of
    the document</xd:detail>
</xd:doc>
<xsl:template name="latexBegin">
<xsl:text>\makeatletter
\thispagestyle{plain}</xsl:text>
<xsl:if test="not(tei:text/tei:front/tei:titlePage)">
  <xsl:call-template name="printTitleAndLogo"/>
</xsl:if>
<xsl:text>\markright{\@title}%
\markboth{\@title}{\@author}%
\makeatother
\fvset{frame=single,numberblanklines=false,xleftmargin=5mm,xrightmargin=5mm}
\fancyhf{} 
\setlength{\headheight}{14pt}
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{\TheFullDate}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{\TheID}
\fancyfoot[LE]{\TheFullDate}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{\TheID}
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}</xsl:text>
<xsl:call-template name="beginDocumentHook"/>
</xsl:template>

<xd:doc class="layout">
    <xd:short>LaTeX setup at end of document</xd:short>
    <xd:detail>All the LaTeX setup which are executed at the end of
    the document</xd:detail>
</xd:doc>
<xsl:template name="latexEnd">
</xsl:template>

<xd:doc>
    <xd:short>[latex] Title banner </xd:short>
    <xd:detail>&#160;</xd:detail>
</xd:doc>
<xsl:template name="printTitleAndLogo">
\noindent\parbox[b]{.75\textwidth}{\fontsize{14pt}{16pt}\bfseries\raggedright\sffamily\selectfont \@title}
\vskip20pt
\par\noindent{\fontsize{11pt}{13pt}\sffamily\itshape\raggedright\selectfont\@author\hfill\TheDate}
\vspace{18pt}
</xsl:template>

  

</xsl:stylesheet>

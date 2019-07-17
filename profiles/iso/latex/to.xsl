<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:dbk="http://docbook.org/ns/docbook"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:html="http://www.w3.org/1999/xhtml"

                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="xlink dbk rng tei teix xhtml a html  xs xsl"
                version="2.0">
   <xsl:import href="../../../latex/latex.xsl"/>
   <xsl:import href="../isoutils.xsl"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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

  

   <xsl:param name="numberBackHeadings">A.1</xsl:param>
   <xsl:param name="numberFrontHeadings">i</xsl:param>
   <xsl:param name="spaceCharacter">\hspace*{1em}</xsl:param>
   <xsl:param name="classParameters">11pt,twoside</xsl:param>
   <xsl:param name="tocNumberSuffix">.\ </xsl:param>
   <xsl:param name="numberSpacer">\ </xsl:param>
   <xsl:template name="latexPreambleHook">
\usepackage{makeidx}
\makeindex
\defaultfontfeatures{Scale=MatchLowercase}
%\setromanfont{DejaVu Serif}
%\setsansfont{DejaVu Sans}
\setmonofont{DejaVu Sans Mono}
%\setmonofont[Scale=0.9]{Lucida Sans Typewriter}
%\setsansfont[Scale=0.85]{Lucida Sans}
%\setromanfont{Times New Roman}
\setromanfont{Minion Pro}
%\setmonofont{CourierStd}
\setsansfont{Myriad Pro}
\setlength{\headheight}{14pt}
</xsl:template>


   <xsl:template name="latexBegin">
      <xsl:text>\makeatletter
\thispagestyle{plain}</xsl:text>
      <xsl:if test="not(tei:text/tei:front/tei:titlePage)">
         <xsl:call-template name="printTitleAndLogo"/>
      </xsl:if>
      <xsl:text>\markright{\@title}%
\markboth{\@title}{\@author}%
\fvset{frame=single,numberblanklines=false,xleftmargin=5mm,xrightmargin=5mm}
\fancyhf{} 
\setlength{\headheight}{14pt}
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{}
\fancyfoot[LE]{}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{}
\hypersetup{linkbordercolor=0.75 0.75 0.75,urlbordercolor=0.75 0.75 0.75,bookmarksnumbered=true}
\def\l@section{\@dottedtocline{1}{3em}{2.3em}}
\def\l@subsection{\@dottedtocline{2}{4em}{3.2em}}
\def\l@subsubsection{\@dottedtocline{3}{5em}{4.1em}}
\def\l@paragraph{\@dottedtocline{4}{6em}{6em}}
\def\l@subparagraph{\@dottedtocline{5}{7em}{6em}}
\def\@pnumwidth{3em}
\setcounter{tocdepth}{2}
\def\tableofcontents{
\clearpage
\pdfbookmark[0]{Table of Contents}{TOC}
\hypertarget{TOC}{}
\section*{\contentsname}\@starttoc{toc}}
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}
\def\chaptermark#1{\markboth {\thechapter. \ #1}{}}
\def\sectionmark#1{\markright { \ifnum \c@secnumdepth &gt;\z@
          \thesection. \ %
        \fi
	#1}}
\def\egxmlcite#1{\raisebox{12pt}[0pt][0pt]{\parbox{.95\textwidth}{\raggedleft #1}}}
\def\oddindex#1{{\bfseries\hyperpage{#1}}}
\def\exampleindex#1{{\itshape\hyperpage{#1}}}
\def\mainexampleindex#1{{\bfseries\itshape\hyperpage{#1}}}
\setlength{\leftmargini}{2\parindent}%
\renewcommand{\@listI}{%
   \setlength{\leftmargin}{\leftmargini}%
   \setlength{\topsep}{\medskipamount}%
   \setlength{\itemsep}{0pt}%
   \setlength{\listparindent}{1em}%
   \setlength{\rightskip}{1em}%
}
\renewcommand\normalsize{\@setfontsize\normalsize{10}{12}%
  \abovedisplayskip 10\p@ plus2\p@ minus5\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip  \z@ plus3\p@
  \belowdisplayshortskip  6\p@ plus3\p@ minus3\p@
  \let\@listi\@listI
}
\renewcommand\small{\@setfontsize\small{9pt}{11pt}%
   \abovedisplayskip 8.5\p@ plus3\p@ minus4\p@
   \belowdisplayskip \abovedisplayskip
   \abovedisplayshortskip \z@ plus2\p@
   \belowdisplayshortskip 4\p@ plus2\p@ minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 2\p@ plus1\p@ minus1\p@
               \parsep 2\p@ plus\p@ minus\p@
               \itemsep 1pt}
}
\renewcommand\footnotesize{\@setfontsize\footnotesize{8}{9.5}%
  \abovedisplayskip 6\p@ plus2\p@ minus4\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip \z@ plus\p@
  \belowdisplayshortskip 3\p@ plus\p@ minus2\p@
  \def\@listi{\leftmargin\leftmargini
              \topsep 2\p@ plus\p@ minus\p@
              \parsep 2\p@ plus\p@ minus\p@
              \itemsep \parsep}
}
\renewcommand\scriptsize{\@setfontsize\scriptsize{7}{8}}
\renewcommand\tiny{\@setfontsize\tiny{5}{6}}
\renewcommand\large{\@setfontsize\large{12}{14.4}}
\renewcommand\Large{\@setfontsize\Large{14.4}{18}}
\renewcommand\LARGE{\@setfontsize\LARGE{17.28}{22}}
\renewcommand\huge{\@setfontsize\huge{20.74}{25}}
\renewcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}
%\parskip3pt
%\parindent0em
% for refdocs
\renewenvironment{itemize}{%
  \advance\@itemdepth \@ne
  \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
  \begin{list}{\csname\@itemitem\endcsname}
  {%
   \setlength{\leftmargin}{\parindent}%
   \setlength{\labelwidth}{.7\parindent}%
   \setlength{\topsep}{2pt}%
   \setlength{\itemsep}{2pt}%
   \setlength{\itemindent}{2pt}%
   \setlength{\parskip}{0pt}%
   \setlength{\parsep}{2pt}%
   \def\makelabel##1{\hfil##1\hfil}}%
  }
  {\end{list}}
\catcode`說=\active \def說{{\fontspec{AR PL ZenKai Uni}\char35498}}
\catcode`説=\active \def説{{\fontspec{IPAMincho}\char35500}}
\catcode`人=\active \def人{{\fontspec{IPAMincho}\char20154}}
\catcode`⁊=\active \def⁊{{\fontspec{Junicode}\char8266}} 
\catcode`Å=\active \defÅ{{\fontspec{DejaVu Serif}\char8491}} 
\catcode`⁻=\active \def⁻{\textsuperscript{-}}
\catcode` =\active \def {\,}
\fancyhfoffset[LO,LE]{2em}
\renewcommand\section{\@startsection {section}{1}{-2em}%
     {-1.75ex \@plus -0.5ex \@minus -.2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\Large\bfseries\sffamily}}
\renewcommand\subsection{\@startsection{subsection}{2}{-2em}%
     {-1.75ex\@plus -0.5ex \@minus- .2ex}%
     {0.5ex \@plus .2ex}%
     {\reset@font\Large\sffamily}}
\makeatother </xsl:text>
      <xsl:call-template name="beginDocumentHook"/>
   </xsl:template>

   <xsl:param name="latexGeometryOptions">twoside,lmargin=1in,rmargin=1in,tmargin=1in,bmargin=1in</xsl:param>

   <xsl:template match="tei:byline"/>
   <xsl:template match="tei:titlePage/tei:note"/>

   <xsl:template match="tei:list">
      <xsl:if test="parent::tei:item">\mbox{}\\[-10pt] </xsl:if>
      <xsl:apply-imports/>
   </xsl:template>

   <xsl:template name="lineBreak">
      <xsl:param name="id"/>
      <xsl:text>\mbox{}\newline 
</xsl:text>
   </xsl:template>

   <xsl:template name="tableHline"/>

   <xsl:template name="makeTable">
      <xsl:variable name="r">
         <xsl:value-of select="@rend"/>
      </xsl:variable>
      <xsl:text>{</xsl:text>
      <xsl:if test="$r='rules'">|</xsl:if>
      <xsl:choose>
         <xsl:when test="@xml:id='tab-conformance'">
            <xsl:text>P{.35\textwidth}llllllll</xsl:text>
         </xsl:when>
         <xsl:when test="@xml:id='tab-content-models'">
            <xsl:text>P{.25\textwidth}P{.15\textwidth}P{.5\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="@xml:id='tab-mods'">
            <xsl:text>L{.15\textwidth}P{.4\textwidth}L{.35\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'wovenodd')">
            <xsl:text>L{.15\textwidth}P{.85\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'attList')">
            <xsl:text>L{.15\textwidth}P{.65\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'attDef')">
            <xsl:text>L{.1\textwidth}P{.5\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="tei:match(@rend,'valList')">
            <xsl:text>L{.1\textwidth}P{.4\textwidth}</xsl:text>
         </xsl:when>
         <xsl:when test="@preamble">
            <xsl:value-of select="@preamble"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="makePreamble-complex">
      </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>}
</xsl:text>
      <xsl:call-template name="tableHline"/>
      <xsl:choose>
         <xsl:when test="tei:head and not(tei:match(@rend,'display'))">
            <xsl:if test="not(ancestor::tei:table)">
	              <xsl:text>\endfirsthead </xsl:text>
	              <xsl:text>\multicolumn{</xsl:text>
	              <xsl:value-of select="count(tei:row[1]/tei:cell)"/>
	              <xsl:text>}{c}{</xsl:text>
	              <xsl:apply-templates mode="ok" select="tei:head"/>
	              <xsl:text>(cont.)}\\\hline \endhead </xsl:text>
            </xsl:if>
            <xsl:text>\caption{</xsl:text>
            <xsl:apply-templates mode="ok" select="tei:head"/>
            <xsl:text>}\\ </xsl:text>
         </xsl:when>
         <xsl:otherwise> </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="$r='rules'">\hline </xsl:if>
      <xsl:apply-templates/>
      <xsl:if test="$r='rules'">
         <xsl:text>\\ \hline </xsl:text>
      </xsl:if>
   </xsl:template>

   <xsl:template match="tei:ident">
      <xsl:apply-imports/>
      <xsl:if test="@type">
         <xsl:processing-instruction name="xmltex">
            <xsl:text>\index{</xsl:text>
            <xsl:value-of select="normalize-space(.)"/>
            <xsl:text> (</xsl:text>
            <xsl:value-of select="@type"/>
            <xsl:text>)}</xsl:text>
         </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>


   <xsl:template name="egXMLEndHook">
      <xsl:if test="@corresp and id(substring(corresp,2))">
         <xsl:text>\egxmlcite{</xsl:text>
         <xsl:for-each select="id(substring(corresp,2))">
            <xsl:text>Source: \cite{</xsl:text>
            <xsl:value-of select="@xml:id"/>
            <xsl:text>}</xsl:text>
         </xsl:for-each>
         <xsl:text>}</xsl:text>
      </xsl:if>
   </xsl:template>

   <xsl:template name="egXMLStartHook">
      <xsl:for-each select=".//teix:*">
         <xsl:variable name="Me">
            <xsl:value-of select="local-name(.)"/>
         </xsl:variable>
         <xsl:text>\index{</xsl:text>
         <xsl:value-of select="$Me"/>
         <xsl:text>=</xsl:text>
         <xsl:text>&lt;</xsl:text>
         <xsl:value-of select="$Me"/>
         <xsl:text>&gt;</xsl:text>
         <xsl:text>|</xsl:text>
         <xsl:choose>
            <xsl:when test="ancestor::tei:div[@xml:id=$Me]">
               <xsl:text>mainexampleindex</xsl:text>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>exampleindex</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
         <xsl:text>}</xsl:text>
         <xsl:for-each select="@*">
            <xsl:choose>
               <xsl:when test="starts-with(name(),'xml:')"/>
               <xsl:otherwise>
                  <xsl:text>\index{</xsl:text>
                  <xsl:value-of select="name()"/>
                  <xsl:text>=@</xsl:text>
                  <xsl:value-of select="name()"/>
                  <xsl:text>!&lt;</xsl:text>
                  <xsl:value-of select="$Me"/>
                  <xsl:text>&gt;</xsl:text>
                  <xsl:text>|exampleindex}</xsl:text>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:for-each>
      </xsl:for-each>
   </xsl:template>


   <xsl:template name="latexEnd">
\cleardoublepage
\pdfbookmark[0]{Index}{INDEX}
\hypertarget{INDEX}{}
\printindex
</xsl:template>

   <xsl:template name="numberFrontDiv">
      <xsl:param name="minimal"/>
   </xsl:template>

   <xsl:template name="generateTitle">
      <xsl:call-template name="getiso_documentNumber"/>
      <xsl:text>-</xsl:text>
      <xsl:call-template name="getiso_partNumber"/>
      <xsl:text>:</xsl:text>
      <xsl:call-template name="getiso_year"/>
   </xsl:template>

   <xsl:template name="printTitleAndLogo">
\begin{raggedleft}
\begin{LARGE}
\hfill\begin{tabular}{lr}
INTERNATIONAL  &amp; \bfseries <xsl:call-template name="getiso_authority"/>\\
STANDARD &amp;\bfseries <xsl:call-template name="getiso_documentNumber"/>-<xsl:call-template name="getiso_partNumber"/>\\\\
\end{tabular}
\end{LARGE}
\end{raggedleft}

\hrule
\vskip4pt

\begin{Large}\upshape\noindent
<xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='en']"/>
\end{Large}

\vskip 12pt

\begin{large}\itshape\noindent
<xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@xml:lang='fr']"/>
\end{large}

\vskip4pt
\hrule

\tableofcontents
</xsl:template>

   <xsl:template match="tei:titleStmt/tei:title[@type='main']">
      <xsl:value-of select="normalize-space(.)"/>
      <xsl:text> </xsl:text>
   </xsl:template>

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
\def\frontmatter{%
  \pagenumbering{roman}
  \setcounter{secnumdepth}{0}
  \def\@chapapp{}%
}
\def\mainmatter{%
  \cleardoublepage
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \pagenumbering{arabic}
  \setcounter{secnumdepth}{6}
}
\def\backmatter{%
  \setcounter{chapter}{0}
  \setcounter{section}{0}
  \setcounter{secnumdepth}{6}
  \def\thesection{A.\arabic{section}}
  \def\theHsection{A.\arabic{section}}
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
\usepackage[pdftitle={<xsl:sequence select="tei:generateSimpleTitle(.)"/>},
 pdfauthor={<xsl:sequence select="replace(string-join(tei:generateAuthor(.),''),'\\newline','')"/>}]{hyperref}
\hyperbaseurl{<xsl:value-of select="$baseURL"/>}
<xsl:call-template name="latexPreambleHook"/>
   </xsl:template>

  <xsl:template name="simpleRun">
    <xsl:param name="text"/>
    <xsl:param name="prefix"/>
    <xsl:param name="italic"/>
    <xsl:value-of select="$prefix"/>
    <xsl:choose>
      <xsl:when test="$italic='true'">
	<xsl:text>\textit{</xsl:text>
	  <xsl:value-of select="$text"/>
	  <xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="termNum">
    <xsl:value-of select="substring-after(../@id,'_')"/>
    <xsl:text> </xsl:text>
  </xsl:template>

   <xsl:template name="block-element">
     <xsl:param name="pPr" as="node()*"/>
     <xsl:param name="style"/>
     <xsl:param name="select" select="."/>
     <xsl:for-each select="$select">
       <xsl:text>\par &#10;</xsl:text>
       <xsl:apply-templates/>
     </xsl:for-each>
   </xsl:template>

</xsl:stylesheet>

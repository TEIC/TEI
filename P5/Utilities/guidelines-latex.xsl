<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
<xsl:import href="/usr/share/xml/tei/stylesheet/latex/tei.xsl"/>
<xsl:param name="reencode">false</xsl:param>
<xsl:param name="numberBackHeadings">true</xsl:param>
<xsl:param name="numberFrontHeadings">true</xsl:param>
<xsl:param name="spaceCharacter">\hspace*{1em}</xsl:param>
<xsl:param name="classParameters">11pt,twoside</xsl:param>
<xsl:param name="startNamespace"></xsl:param>
<xsl:param name="tocNumberSuffix">.\ </xsl:param>
<xsl:param name="numberSpacer">\ </xsl:param>

  <xsl:variable name="docClass">book</xsl:variable>
<xsl:template name="latexPreambleHook">
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.95}
\defaultfontfeatures{Scale=MatchLowercase}
\setromanfont{DejaVu Serif}
\setsansfont{DejaVu Sans}
\setmonofont{DejaVu Sans Mono}
%\setmonofont[Scale=0.9]{Lucida Sans Typewriter}
\setlength{\headheight}{14pt}
</xsl:template>


<xsl:template name="latexBegin">
\makeatletter
\thispagestyle{plain}
<xsl:if test="not(tei:text/tei:front/tei:titlePage)">
  <xsl:call-template name="printTitleAndLogo"/>
</xsl:if>
\markright{\@title}%
\markboth{\@title}{\@author}%
\makeatother
\fvset{frame=single,numberblanklines=false,xleftmargin=5mm,xrightmargin=5mm}
\fancyhf{} 
\setlength{\headheight}{14pt}
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{\TheDate}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{}
\fancyfoot[LE]{\TheDate}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{}
\hyperset{bookmarksnumbered=true}
\makeatletter
\def\@pnumwidth{3.5em}
\def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\leftmark}%
      \def\@oddhead{{\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markboth {%
        \ifnum \c@secnumdepth >\m@ne
          \if@mainmatter
            \@chapapp\ \thechapter. \ %
          \fi
        \fi
        ##1}{}}%
    \def\sectionmark##1{%
      \markright {%
        \ifnum \c@secnumdepth >\z@
          \thesection. \ %
        \fi
        ##1}}}

\def\tableofcontents{\clearpage\section*{\contentsname}\@starttoc{toc}}
\makeatother
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}
<xsl:call-template name="beginDocumentHook"/>
</xsl:template>

<xsl:param name="latexGeometryOptions">twoside,letterpaper,lmargin=.8in,rmargin=.8in,tmargin=.8in,bmargin=.8in</xsl:param>

<xsl:template match="tei:byline"/>
<xsl:template match="tei:titlePage/tei:note"/>

<xsl:template match="tei:list">
  <xsl:if test="parent::tei:item">\mbox{}\\[-10pt] </xsl:if>
  <xsl:apply-imports/>
</xsl:template>

<xsl:template name="lineBreak">
  <xsl:param name="id"/>
  <xsl:text>\mbox{}\newline &#10;</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-elementSpec']">
  <xsl:text>\textbf{&lt;</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-macroSpec']">
 <xsl:text>\textbf{</xsl:text>
  <xsl:value-of select="."/>
 <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-classSpec']">
 <xsl:text>\textbf{</xsl:text>
 <xsl:value-of select="."/>
 <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template name="tableHline"/>

<xsl:template name="makeTable">
  <xsl:variable name="r">
    <xsl:value-of select="@rend"/>
  </xsl:variable>
  <xsl:text>{</xsl:text>
  <xsl:if test="$r='rules'">|</xsl:if>
  <xsl:choose>
    <xsl:when test="@rend='wovenodd'">
      <xsl:text>L{.15\textwidth}P{.85\textwidth}</xsl:text>
    </xsl:when>
    <xsl:when test="@rend='attList'">
      <xsl:text>L{.15\textwidth}P{.65\textwidth}</xsl:text>
    </xsl:when>
    <xsl:when test="@rend='attDef'">
      <xsl:text>L{.1\textwidth}P{.5\textwidth}</xsl:text>
    </xsl:when>
    <xsl:when test="@rend='valList'">
      <xsl:text>L{.1\textwidth}P{.4\textwidth}</xsl:text>
    </xsl:when>
    <xsl:when test="@preamble">
      <xsl:value-of select="@preamble"/>
    </xsl:when>
    <xsl:when test="function-available('exsl:node-set')">
      <xsl:call-template name="makePreamble-complex">
	<xsl:with-param name="r" select="$r"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="makePreamble-simple">
	<xsl:with-param name="r" select="$r"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>}&#10;</xsl:text>
  <xsl:call-template name="tableHline"/>
  <xsl:choose>
    <xsl:when test="tei:head and not(@rend='display')">
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

  <xsl:template match="tei:table">
    <xsl:if test="@xml:id">
      <xsl:text>\label{</xsl:text>
      <xsl:value-of   select="@xml:id"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:text> \par</xsl:text>
    <xsl:choose>
      <xsl:when test="@rend='wovenodd' or @rend='attList' or
		      @rend='valList' or @rend='attDef'"> 
	<xsl:text>\begin{small}\begin{tabular}</xsl:text>
	<xsl:call-template name="makeTable"/>
	<xsl:text>\end{tabular}\end{small}\par</xsl:text>
      </xsl:when>
      <xsl:when test="ancestor::tei:table"> 
	<xsl:text>\begin{tabular}</xsl:text>
	<xsl:call-template  name="makeTable"/> 
	<xsl:text>\end{tabular}</xsl:text>
      </xsl:when>
      <xsl:otherwise> 
	<xsl:text>\begin{longtable}</xsl:text>
	<xsl:call-template name="makeTable"/>
	<xsl:text>\end{longtable} \par</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:gi">
    <xsl:choose>
      <xsl:when test="parent::tei:ref">
	<xsl:text>\texttt{&lt;</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>&gt;}</xsl:text>
      </xsl:when>
      <xsl:when test="key('IDS',.)">
	<xsl:text>\hyperlink{</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>}{</xsl:text>
	<xsl:text>\texttt{&lt;</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>&gt;}</xsl:text>
	<xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>\texttt{&lt;</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>&gt;}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>



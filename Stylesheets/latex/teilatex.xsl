<?xml version="1.0" encoding="utf-8"?>
<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to LaTeX

##LICENSE
--> 
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"   
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:exsl="http://exslt.org/common"
  exclude-result-prefixes="exsl" 
  extension-element-prefixes="exsl"
  version="1.0">

<xsl:import href="../common/teicommon.xsl"/>
<xsl:param name="realFigures">false</xsl:param>
<xsl:param name="latexLogoFile"/>
<xsl:param name="latexLogoFile1"/>
<xsl:param name="latexLogoFile2"/>
<xsl:param name="latexLogoFile3"/>
<xsl:param name="useHeaderFrontMatter"/>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="authorWord">Author:</xsl:param>
<xsl:param name="revisedWord">revised</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="REQUEST"></xsl:param>
<xsl:param name="standardScale">1</xsl:param>
<xsl:output method="text" encoding="utf8"/>
<xsl:param name="pagesetup">a4paper,lmargin=1in,rmargin=1in,tmargin=0.25in,bmargin=0.75in</xsl:param>

<xsl:key name="IDS" match="tei:*[@id|@xml:id]" use="@id|@xml:id"/>

<xsl:strip-space elements="*"/>

<xsl:template match="tei:TEI">
  <xsl:variable name="docstyle">
  <xsl:choose>
   <xsl:when test="@rend">
   	<xsl:value-of select="@rend"/>
   </xsl:when>
   <xsl:otherwise>
    	<xsl:text>article</xsl:text>
   </xsl:otherwise>
  </xsl:choose>
  </xsl:variable>
<xsl:if test="not($realFigures='true')">
  <xsl:text>%BEGINFIGMAP
</xsl:text>
<xsl:if test="not($latexLogoFile='')">
  <xsl:text>%FIGMAP </xsl:text>
  <xsl:value-of select="$latexLogoFile"/><xsl:text> FIG0
</xsl:text>
</xsl:if>
<xsl:if test="not($latexLogoFile1='')">
  <xsl:text>%FIGMAP </xsl:text>
  <xsl:value-of select="$latexLogoFile1"/><xsl:text> FIG1
</xsl:text>
</xsl:if>
<xsl:if test="not($latexLogoFile2='')">
  <xsl:text>%FIGMAP </xsl:text>
  <xsl:value-of select="$latexLogoFile2"/><xsl:text> FIG2
</xsl:text>
</xsl:if>
<xsl:if test="not($latexLogoFile3='')">
  <xsl:text>%FIGMAP </xsl:text>
  <xsl:value-of select="$latexLogoFile3"/><xsl:text> FIG3
</xsl:text>
</xsl:if>
  <xsl:for-each select="//tei:figure">
    <xsl:variable name="c"><xsl:number level="any"/></xsl:variable>
    <xsl:text>%FIGMAP </xsl:text>
    <xsl:call-template name="findFileName"/>
    <xsl:text> FIG</xsl:text>
    <xsl:value-of select="$c + 1000"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:for-each>
  <xsl:text>%ENDFIGMAP
</xsl:text>
</xsl:if>
\documentclass{<xsl:value-of select="$docstyle"/>}
\usepackage[twoside,<xsl:value-of select="$pagesetup"/>]{geometry}
\usepackage{times}
\usepackage{longtable}
\usepackage{colortbl}
\usepackage{ulem}
\usepackage{fancyvrb}
\usepackage{fancyhdr}
\usepackage{graphicx}
\pagestyle{fancy} 
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[]{ucs}
\RequirePackage{array}
\makeatletter
\gdef\arraybackslash{\let\\=\@arraycr}
<xsl:text disable-output-escaping="yes">
\newcolumntype{L}[1]{&gt;{\raggedright\arraybackslash}p{#1}}
\newcolumntype{C}[1]{&gt;{\centering\arraybackslash}p{#1}}
\newcolumntype{R}[1]{&gt;{\raggedleft\arraybackslash}p{#1}}
\newcolumntype{P}[1]{&gt;{\arraybackslash}p{#1}}
\definecolor{label}{gray}{0.75}
\def\Panel#1#2#3#4{\multicolumn{#3}{>{\columncolor{#2}}#4}{#1}}
</xsl:text>
\usepackage[pdftitle={<xsl:call-template name="generateSimpleTitle"/>},
pdfauthor={<xsl:call-template name="generateAuthor"/>},
pdfcreator={Oxford University Computing Services}
]{hyperref}
\DeclareRobustCommand*{\xref}{\hyper@normalise\xref@}
\def\xref@#1#2{\hyper@linkurl{#2}{#1}}
\makeatother
\def\TheFullDate{<xsl:call-template name="generateDate"/>}
\def\TheDate{<xsl:call-template name="generateDate">
<xsl:with-param name="showRev"/></xsl:call-template>}
\catcode`\_=12\relax
<xsl:text disable-output-escaping="yes">\let\tabcellsep&amp;
\catcode`\&amp;=12\relax
</xsl:text>
\title{<xsl:call-template name="generateTitle"/>}
\author{<xsl:call-template name="generateAuthor"/>}
\paperwidth211mm
\paperheight297mm
\hyperbaseurl{<xsl:value-of select="$baseURL"/>}
\makeatletter
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
<xsl:if test="not($docstyle='letter')">
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-1.75ex \@plus -0.5ex \@minus -.2ex}%
                                   {0.5ex \@plus .2ex}%
                                   {\reset@font\large\bfseries\sffamily}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-1.75ex\@plus -0.5ex \@minus- .2ex}%
                                     {0.5ex \@plus .2ex}%
                                     {\reset@font\large\sffamily}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-1.5ex\@plus -0.35ex \@minus -.2ex}%
                                     {0.5ex \@plus .2ex}%
                                     {\reset@font\normalsize\sffamily}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {1.5ex \@plus0.5ex \@minus.2ex}%
                                    {-1em}%
                                    {\reset@font\normalsize\bfseries}}
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
\makeatother
<xsl:call-template name="preambleHook"/>
\begin{document}
\thispagestyle{plain}
\parindent0em
\parskip3pt
\makeatletter
\def\tableofcontents{\section*{\contentsname}\@starttoc{toc}}
   <xsl:if test="not(tei:text/tei:front/tei:titlePage)">
     <xsl:call-template name="printTitleAndLogo"/>
   </xsl:if>
\markright{\@title}%
\markboth{\@title}{\@author}%
\makeatother
<xsl:if test="not($docstyle='letter')">
\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}}
</xsl:if>
\fancyhf{} 
\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[RO]{\bfseries\rightmark} 
\fancyfoot[RO]{\TheFullDate}
\fancyfoot[CO]{\thepage}
\fancyfoot[LO]{<xsl:value-of select="$REQUEST"/>}
\fancyfoot[LE]{\TheFullDate}
\fancyfoot[CE]{\thepage}
\fancyfoot[RE]{<xsl:value-of select="$REQUEST"/>}
\fancypagestyle{plain}{\fancyhead{}\renewcommand{\headrulewidth}{0pt}}
<xsl:call-template name="begindocumentHook"/>
<xsl:apply-templates select="tei:text"/>
\end{document}
</xsl:template>

<xsl:template match="/">
   <xsl:apply-templates select="tei:TEI"/>
</xsl:template>

<xsl:template match="tei:anchor">
 <xsl:text>\hypertarget{</xsl:text>
 <xsl:value-of select="@id|@xml:id"/>
  <xsl:text>}{}</xsl:text>
</xsl:template>

<xsl:template match="tei:back">
\appendix
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:back/tei:div/tei:div/tei:head">
\subsection{<xsl:apply-templates />}
</xsl:template>

<xsl:template match="tei:back/tei:div/tei:head">
\section{<xsl:apply-templates />}
</xsl:template>

<xsl:template match="tei:bibl" mode="cite">
  <xsl:apply-templates select="text()[1]"/>
</xsl:template>

<xsl:template match="tei:listBibl/tei:bibl">
\bibitem {<xsl:value-of select="@id|@xml:id"/>}
<xsl:apply-templates/>
<xsl:text>&#10;</xsl:text>
</xsl:template>


<xsl:template match="tei:body">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:body/tei:div/tei:div/tei:div/tei:div/tei:div/tei:head">
\paragraph<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:body/tei:div/tei:div/tei:div/tei:div/tei:head">
\subsubsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:body/tei:div/tei:div/tei:div/tei:head">
\subsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:body/tei:div/tei:div/tei:head">
  \section<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:body/tei:div/tei:head">
\chapter<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:cell">
  <xsl:if test="preceding-sibling::tei:cell">\tabcellsep </xsl:if>
  <xsl:choose>
    <xsl:when test="@role='label'">
      <xsl:text>\Panel{</xsl:text>
        <xsl:if test="starts-with(normalize-space(.),'[')"><xsl:text>{}</xsl:text></xsl:if><xsl:apply-templates/>
      <xsl:text>}{label}{</xsl:text>
      <xsl:choose>
	<xsl:when test="@cols"><xsl:value-of select="@cols"/>
	</xsl:when>
	<xsl:otherwise>1</xsl:otherwise>
      </xsl:choose>
      <xsl:text>}{</xsl:text>
      <xsl:choose>
	<xsl:when test="@align='right'">r</xsl:when>
	<xsl:when test="@align='centre'">c</xsl:when>
	<xsl:when test="@align='center'">c</xsl:when>
	<xsl:when test="@align='left'">l</xsl:when>
	<xsl:otherwise>l</xsl:otherwise>
      </xsl:choose>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:when test="@cols &gt; 1">
      <xsl:text>\multicolumn{</xsl:text>
      <xsl:value-of select="@cols"/>
      <xsl:text>}{c}{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
        <xsl:if test="starts-with(normalize-space(.),'[')"><xsl:text>{}</xsl:text></xsl:if>
	<xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:code">\texttt{<xsl:apply-templates/>}</xsl:template>

<xsl:template match="tei:div0/tei:head">
\chapter<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:div1/tei:head">
\section<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:div2/tei:head">
  \subsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:div3/tei:head">
\subsubsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="tei:divGen[@type='toc']">
\tableofcontents
</xsl:template>

<xsl:template match="tei:div[@type='bibliography']">
\begin{thebibliography}{1}
  <xsl:call-template name="bibliography"/>
\end{thebibliography}  
</xsl:template>

<xsl:template match="tei:docAuthor">
<xsl:if test="preceding-sibling::tei:docAuthor">, </xsl:if>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:docAuthor">
\author{<xsl:apply-templates/>}
</xsl:template>

<xsl:template match="tei:docDate">
\date{<xsl:apply-templates/>}
</xsl:template>


<xsl:template match="tei:eg">
\begin{quote}\ttfamily\color{black}\obeylines <xsl:apply-templates/> \end{quote}
</xsl:template>

<xsl:template match="tei:emph">\textit{<xsl:apply-templates/>}</xsl:template>

<xsl:template match="tei:figure">
  <xsl:choose>
    <xsl:when test="@rend='centre'">
      <xsl:text>\par\centerline{</xsl:text>
    </xsl:when>
    <xsl:when test="@rend='display'">
      <xsl:text>\begin{figure}[htbp]
      </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      \noindent
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>\includegraphics[</xsl:text>
  <xsl:if test="@width">
    <xsl:variable name="w">
      <xsl:choose>
	<xsl:when test="contains(@width,'pt')"><xsl:value-of select="@width"/></xsl:when>
	<xsl:when test="contains(@width,'in')"><xsl:value-of select="@width"/></xsl:when>
	<xsl:when test="contains(@width,'cm')"><xsl:value-of select="@width"/></xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@width"/><xsl:text>pt</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>width=</xsl:text><xsl:value-of select="$w"/><xsl:text>,</xsl:text>
  </xsl:if>
  <xsl:if test="@height">
    <xsl:variable name="h">
      <xsl:choose>
	<xsl:when test="contains(@height,'pt')"><xsl:value-of select="@height"/></xsl:when>
	<xsl:when test="contains(@height,'in')"><xsl:value-of select="@height"/></xsl:when>
	<xsl:when test="contains(@height,'cm')"><xsl:value-of select="@height"/></xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@height"/><xsl:text>pt</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>height=</xsl:text><xsl:value-of select="$h"/><xsl:text>,</xsl:text>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="@scale and contains(@scale,'%')">
      <xsl:text>scale=</xsl:text>
      <xsl:value-of select="substring-before(@scale,'%') div 100"/>
      <xsl:text>,</xsl:text>
    </xsl:when>
    
    <xsl:when test="@scale">
      <xsl:text>scale=</xsl:text>
      <xsl:value-of select="@scale"/>
      <xsl:text>,</xsl:text>
    </xsl:when>
    <xsl:when test="not(@width) and not(@height) and not($standardScale=1)">
      <xsl:text>scale=</xsl:text>
      <xsl:value-of select="$standardScale"/>
      <xsl:text>,</xsl:text>
    </xsl:when>
  </xsl:choose>
  <xsl:text>]{</xsl:text>
  <xsl:choose>
    <xsl:when test="$realFigures='true'">
      <xsl:choose>
	<xsl:when test="@url">
	  <xsl:value-of select="@url"/>
	</xsl:when>
	<xsl:when test="@entity">
	  <xsl:value-of select="unparsed-entity-uri(@entity)"/>
	</xsl:when>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="c"><xsl:number level="any"/></xsl:variable>
      <xsl:text>FIG</xsl:text>
      <xsl:value-of select="$c+1000"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>}</xsl:text>
  <xsl:choose>
    <xsl:when test="@rend='display'">
      <xsl:text>&#10;\caption{</xsl:text><xsl:value-of select="tei:head"/>
      <xsl:text>}</xsl:text>
      <xsl:if test="@id|@xml:id">
	\label{<xsl:value-of select="@id|@xml:id"/>}
      </xsl:if>
      <xsl:text>\end{figure}
      </xsl:text>
    </xsl:when>
    <xsl:when test="@rend='centre'">
      <xsl:text>}\par</xsl:text>
    </xsl:when>
    <xsl:otherwise>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:foreign">
  <xsl:text>\textit{</xsl:text><xsl:apply-templates/><xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:front/tei:div/tei:head">
\section*{<xsl:apply-templates />}
</xsl:template>

<xsl:template match="tei:gi">\texttt{&lt;<xsl:apply-templates/>&gt;}</xsl:template>

<xsl:template match="tei:hi">
<xsl:text>\textbf{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='sub']">
<xsl:text>\textsubscript{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='sup']">
<xsl:text>\textsuperscript{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hr">
 \hline
</xsl:template>

<xsl:template match="tei:ident">
  <xsl:text>\textsf{</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:item">
 \item <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:label"/>

<xsl:template match="tei:lb">
<xsl:text>\newline </xsl:text>
</xsl:template>

<xsl:template match="tei:list">
  <xsl:if test="head">
    \centerline{<xsl:value-of select="tei:head"/>}
  </xsl:if>
<xsl:choose>
 <xsl:when test="@type='gloss'">
   \begin{description}<xsl:apply-templates mode="gloss" select="tei:item"/>
   \end{description}
 </xsl:when>
 <xsl:when test="@type='unordered'">
   \begin{itemize}<xsl:apply-templates/>
   \end{itemize}
 </xsl:when>
 <xsl:when test="@type='ordered'">
   \begin{enumerate}<xsl:apply-templates/>
    \end{enumerate}
 </xsl:when>
 <xsl:otherwise>
   \begin{itemize}<xsl:apply-templates/>
   \end{itemize}
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:list/tei:head"/>

<xsl:template match="tei:mentioned">
  <xsl:text>\emph{</xsl:text><xsl:apply-templates/><xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:note[@place='foot']">
  <xsl:text>\footnote{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:p">\par
<xsl:apply-templates/></xsl:template>

<xsl:template match="tei:q">
  <xsl:text>`</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>'</xsl:text>
</xsl:template>

<xsl:template match="tei:q[@rend='eg']">
\begin{quote}\ttfamily\color{black}\obeylines <xsl:apply-templates/> \end{quote}
</xsl:template>

<xsl:template match="tei:row">
  <xsl:if test="@role='label'">\rowcolor{label}</xsl:if>
  <xsl:apply-templates/>
  <xsl:if test="following-sibling::tei:row">
    <xsl:text>\\
</xsl:text>
</xsl:if>
</xsl:template>

<xsl:template match="tei:soCalled">
  <xsl:text>`</xsl:text><xsl:apply-templates/><xsl:text>'</xsl:text>
</xsl:template>

<xsl:template match="tei:table" mode="xref">
<xsl:text>the table on p. \pageref{</xsl:text>
<xsl:value-of select="@id|@xml:id"/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:table">
\par  
<xsl:if test="@id|@xml:id">\label{<xsl:value-of select="@id|@xml:id"/>}</xsl:if>
<xsl:choose>
<xsl:when test="ancestor::tei:table">
\begin{tabular}
<xsl:call-template name="makeTable"/>
\end{tabular}
</xsl:when>
<xsl:otherwise>
\begin{longtable}
<xsl:call-template name="makeTable"/>
\end{longtable}
\par
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:table/head"/>

<xsl:template match="tei:table[@type='display']" mode="xref">
<xsl:text>Table </xsl:text>
<xsl:number level="any" count="table[@type='display']"/>
</xsl:template>

<xsl:template match="tei:table[@type='display']">
  \begin{table*}
  \caption{<xsl:apply-templates select="tei:head" mode="ok"/>}
  <xsl:if test="@id|@xml:id">\label{<xsl:value-of select="@id|@xml:id"/>}</xsl:if>
  \begin{small}
  \begin{center}
  \begin{tabular}
  <xsl:call-template name="makeTable"/>
  \end{tabular}
  \end{small}
  \end{center}
  \end{table*}
</xsl:template>

<xsl:template match="tei:text">
  <xsl:call-template name="extradefHook"/>
 <xsl:text disable-output-escaping="yes">
\catcode`\$=12\relax
\catcode`\^=12\relax
\catcode`\~=12\relax
\catcode`\#=12\relax
\catcode`\%=12\relax
</xsl:text>
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:titlePage">
  \begin{titlepage}
  <xsl:apply-templates/>
  \maketitle
  \end{titlepage}
  \cleardoublepage
</xsl:template>

<xsl:template match="tei:titlePart">
\title{<xsl:apply-templates/>}
</xsl:template>

<xsl:template match="tei:title[@level='a']">
  <xsl:text>``</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>''</xsl:text>
</xsl:template>

<xsl:template match="tei:title[@level='m']">
  <xsl:text>\textit{</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:title[@level='s']">
  <xsl:text>\textit{</xsl:text>
   <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:xref[@type='cite']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text()">
  <xsl:choose>
    <xsl:when test="contains(.,'\')">
      <xsl:call-template name="slasher">
        <xsl:with-param name="s">
          <xsl:value-of select="."/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template mode="gloss" match="tei:item">
 \item[<xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>]
 <xsl:apply-templates/>
</xsl:template>
<xsl:template name="begindocumentHook"/>
<xsl:template name="bibliography">
  <xsl:apply-templates select="//tei:xref[@type='cite'] | //tei:xptr[@type='cite']" mode="biblio"/>
</xsl:template>

<xsl:template name="extradefHook"/>

<xsl:template name="findFileName">
  <xsl:variable name="f">
    <xsl:value-of select="@file|@url"/>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="contains($f,'.')">
      <xsl:value-of select="$f"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="concat($f,'.png')"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="generateSimpleTitle">
 <xsl:choose>
    <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
         <xsl:value-of select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
 </xsl:when>
<xsl:otherwise>
<xsl:value-of
 select="ancestor-or-self::tei:TEI/teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="labelme">
 <xsl:if test="../@id|../@xml:id">\hypertarget{<xsl:value-of select="../@id|../@xml:id"/>}{}</xsl:if>
</xsl:template>

<xsl:template name="makeExternalLink">
  <xsl:param name="ptr"/>
  <xsl:param name="dest"/>
  <xsl:choose>
    <xsl:when test="$ptr='true'">
      <xsl:text>\url{</xsl:text>
      <xsl:value-of select="$dest"/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>\xref{</xsl:text>
      <xsl:value-of select="$dest"/>
      <xsl:text>}{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="makeInternalLink">
  <xsl:param name="ptr"/>
  <xsl:param name="dest"/>
  <xsl:param name="body"/>
  <xsl:choose>
    <xsl:when test="key('IDS',$dest)">
      <xsl:text>\hyperlink{</xsl:text>
      <xsl:value-of select="$dest"/>
      <xsl:text>}{\textit{</xsl:text>
      <xsl:choose>
	<xsl:when test="not($body='')">
	  <xsl:value-of select="$body"/>
	</xsl:when>
	<xsl:when test="$ptr='true'">
	  <xsl:apply-templates mode="xref" select="key('IDS',$dest)">
	    <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	  </xsl:apply-templates>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>}}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>&#x00AB;</xsl:text>
      <xsl:choose>
	<xsl:when test="not($body='')">
	  <xsl:value-of select="$body"/>
	</xsl:when>
	<xsl:when test="$ptr='true'">
	  <xsl:apply-templates mode="xref" select="key('IDS',$dest)">
	    <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	  </xsl:apply-templates>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#x00BB;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
<xsl:template name="makeTable">
  <xsl:variable name="r">
    <xsl:value-of select="@rend"/>
  </xsl:variable>
  <xsl:text>{</xsl:text>
  <xsl:if test="$r='rules'">|</xsl:if>
<xsl:variable name="tds">
 <xsl:for-each select=".//tei:cell">
  <xsl:variable name="stuff">
     <xsl:apply-templates/>
  </xsl:variable>
   <cell>
    <xsl:attribute name="col"><xsl:number/></xsl:attribute>
    <xsl:value-of select="string-length($stuff)"/>
   </cell>
 </xsl:for-each>
</xsl:variable>
<xsl:variable name="total">
  <xsl:value-of select="sum(exsl:node-set($tds)/cell)"/>
</xsl:variable>
<xsl:for-each select="exsl:node-set($tds)/cell">
  <xsl:sort select="@col" data-type="number"/>
  <xsl:variable name="c" select="@col"/>
  <xsl:if test="not(preceding-sibling::cell[$c=@col])">
   <xsl:variable name="len">
    <xsl:value-of select="sum(following-sibling::cell[$c=@col]) + current()"/>
   </xsl:variable>
 <xsl:text>P{</xsl:text>
 <xsl:value-of select="($len div $total) * 0.95" />
   <xsl:text>\textwidth}</xsl:text>
  <xsl:if test="$r='rules'">|</xsl:if>
</xsl:if>
</xsl:for-each>
  <xsl:text>}
</xsl:text>
<xsl:if test="$r='rules'">\hline </xsl:if>
<xsl:if test="tei:head and not(../@rend='display')">
  <xsl:text>\caption{</xsl:text>
  <xsl:apply-templates	select="tei:head" mode="ok"/>
  <xsl:text>}\\ </xsl:text>
</xsl:if>
<xsl:apply-templates/>
  <xsl:if test="$r='rules'">
    <xsl:text>\\ \hline </xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template name="preambleHook"/>

<xsl:template name="printTitleAndLogo">
\parbox[b]{.75\textwidth}{\fontsize{14pt}{16pt}\bfseries\sffamily\selectfont \@title}
\vskip20pt
\par{\fontsize{11pt}{13pt}\sffamily\itshape\selectfont\@author\hfill\TheDate}
\vspace{18pt}
</xsl:template>

<xsl:template name="sectionhead">
  <xsl:if test="tei:note">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="text()"/>
    <xsl:text>]</xsl:text>
  </xsl:if>
  <xsl:text>{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template name="slasher">
  <xsl:param name="s"/>
  <xsl:choose>
  <xsl:when test="contains($s,'\')">
  <xsl:value-of select="substring-before($s,'\')"/>
  <xsl:text>\char92 </xsl:text>
      <xsl:call-template name="slasher">
        <xsl:with-param name="s">
          <xsl:value-of select="substring-after($s,'\')"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$s"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="generateEndLink">
  <xsl:param name="where"/>
  <xsl:value-of select="$where"/>
</xsl:template>


<xsl:template match="p[@rend='display']">
\begin{quote}
   <xsl:apply-templates/>
\end{quote}
</xsl:template>

<xsl:template match="q[@rend='display']">
\begin{quote}
   <xsl:apply-templates/>
\end{quote}
</xsl:template>

<xsl:template match="tei:bibl/tei:title">
  <xsl:if test="preceding-sibling::tei:title"> </xsl:if>
  <xsl:choose>
    <xsl:when test="@level='a'">
      <xsl:text>`</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>'</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>

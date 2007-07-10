<xsl:stylesheet
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 xmlns:rss="http://purl.org/rss/1.0/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
 xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
 xmlns:exsl="http://exslt.org/common"
 xmlns:estr="http://exslt.org/strings"
 extension-element-prefixes="exsl estr"
 exclude-result-prefixes="exsl"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 version="1.0">
<!---->
<xsl:import href="../latex/tei.xsl"/>
<xsl:param name="baseURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="useHeaderFrontMatter"/>
<xsl:param name="institution">Text Encoding Initiative Consortium</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="authorWord">Author:</xsl:param>
<xsl:param name="revisedWord">revised</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="REQUEST"></xsl:param>
<xsl:param name="latexLogoFile">/Pictures/logo2004.pdf</xsl:param>

<xsl:key name="IDS" match="*[@id]" use="@id"/>

<xsl:output method="text" encoding="utf8"/>

<xsl:template match="hidden"/>


<xsl:template match="pagetitle">
\thispagestyle{empty}
\large
\vspace*{1.35in}
\setlength{\hoffset}{-1.50cm}
\today 
\begin{center}  
\vspace{1cm}
\hspace{2.5cm}
\textbf{<xsl:apply-templates/>} 
\end{center}
</xsl:template>

<xsl:template name="printTitleAndLogo">
<xsl:text>
\mbox{}\vskip-0.2in
\hskip-.8in\begin{tabular}{ll}
\raisebox{-0.5in}{\includegraphics[width=1.2in]{</xsl:text>
<xsl:choose>
<xsl:when test="$realFigures='true'">
<xsl:text>logo2004</xsl:text>
</xsl:when>
<xsl:otherwise>
<xsl:text>FIG0</xsl:text>
</xsl:otherwise>
</xsl:choose>
<xsl:text>}}
\tabcellsep \begin{tabular}{l}
\fontsize{14pt}{16pt}\bfseries\sffamily\selectfont \@title\\[10pt]
\fontsize{11pt}{13pt}\sffamily\itshape\selectfont\TheDate\\[2pt]
\end{tabular}\\
\end{tabular}
\hrule
\vspace{12pt}
</xsl:text>
</xsl:template>


<xsl:template match="body">
<xsl:apply-templates/>
<xsl:if test="//note[@place='end']">
\section*{Notes}
\begin{enumerate}
<xsl:for-each select="//note[@place='end']">
\item[<xsl:number level="any"/>] <xsl:apply-templates/>
</xsl:for-each>
\end{enumerate}
</xsl:if>
\vfill
\textbf{Author}: <xsl:call-template name="generateAuthor"/>

\copyright\ Text Encoding Initiative Consortium

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.2 or
any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.

A copy of the license is available at  \texttt{http://www.tei-c.org/fdl/}.

For further information on the TEI see \texttt{http://www.tei-c.org/}
</xsl:template>

<xsl:template match="list[item[@n]]">
\begin{itemize}
<xsl:apply-templates/>
\end{itemize}
</xsl:template>

<xsl:template match="item[@n]">
\item[\textbf{[<xsl:value-of select="@n"/>]}] <xsl:apply-templates/>
</xsl:template>

<xsl:template match="back">
\appendix
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="note[@place='foot']">
<xsl:text>\footnote{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text></xsl:template>

<xsl:template match="note[@place='end']">
<xsl:text>\footnotemark[</xsl:text>
<xsl:number level="any"/>
<xsl:text>]</xsl:text>
</xsl:template>

<xsl:template match="p[@rend='aside']">
<xsl:text>\begin{quote}{\itshape </xsl:text>
<xsl:apply-templates/>
<xsl:text>}\end{quote}</xsl:text></xsl:template>


<xsl:template name="headersAndFooters">
\fancyhf{} 

\fancyhead[LE]{\bfseries\leftmark} 
\fancyhead[LO]{\bfseries\thepage}
\fancyhead[RO]{\bfseries\rightmark}
\fancyhead[RE]{\bfseries\thepage}

\fancyfoot[RO]{\TheFullDate\hfill}
\fancyfoot[CO]{}
\fancyfoot[LO]{http://www.tei-c.org<xsl:value-of select="$REQUEST"/>}
\fancyfoot[LE]{\TheFullDate}
\fancyfoot[CE]{}
\fancyfoot[RE]{http://www.tei-c.org<xsl:value-of select="$REQUEST"/>\hfill}
%\fancypagestyle{plain}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0.4pt}
</xsl:template>


<xsl:template match="xptr[@type='transclude' and @rend='rss']">
 <xsl:for-each select="document(@url)/rdf:RDF">  
<xsl:if test="rss:item">
\begin{enumerate}
  <xsl:for-each select="rss:item">
<xsl:text>\item \url{</xsl:text>
     <xsl:value-of select="normalize-space(rss:link)"/>
     <xsl:text>}{</xsl:text>
      <xsl:apply-templates select="rss:title"/>
      <xsl:text>}</xsl:text>
     [<xsl:apply-templates select="dc:created"/>]\\
     <xsl:apply-templates select="rss:description"/> 
   </xsl:for-each>
\end{enumerate}
</xsl:if>
</xsl:for-each>
</xsl:template>

<xsl:template match="xptr[@type='transclude' and @rend='rsssummary']">
 <xsl:for-each select="document(@url)/rdf:RDF">  
<xsl:if test="rss:item">
\begin{enumerate}
  <xsl:for-each select="rss:item[position()&lt;6]">
<xsl:text>\item \url{</xsl:text>
     <xsl:value-of select="normalize-space(rss:link)"/>
     <xsl:text>}{</xsl:text>
      <xsl:apply-templates select="rss:title"/>
      <xsl:text>}</xsl:text>
     [<xsl:apply-templates select="dc:created"/>]\\
     <xsl:apply-templates select="rss:description"/> 
   </xsl:for-each>
\end{enumerate}
</xsl:if>
</xsl:for-each>
</xsl:template>


<xsl:template match="body/div/head">
\section<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="body/div/div/head">
  \subsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="body/div/div/div/head">
\subsubsection<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="body/div/div/div/div/head">
\paragraph<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template match="body/div/div/div/div/div/head">
\subparagraph<xsl:call-template name="sectionhead"/>
<xsl:call-template name="labelme"/>
</xsl:template>

<xsl:template name="sectionhead">
  <xsl:if test="note">
<xsl:text>[</xsl:text>
<xsl:apply-templates select="text()"/>
<xsl:text>]</xsl:text>
  </xsl:if>
<xsl:text>{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template name="labelme">
 <xsl:if test="../@id">\hypertarget{<xsl:value-of select="../@id"/>}{}</xsl:if>
</xsl:template>

</xsl:stylesheet>

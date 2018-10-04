<xsl:stylesheet
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
 xmlns:rss="http://purl.org/rss/1.0/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
 xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
 xmlns:exsl="http://exslt.org/common"
 xmlns:estr="http://exslt.org/strings"
 xmlns:cc="http://web.resource.org/cc/"
 extension-element-prefixes="exsl estr"
 exclude-result-prefixes="exsl cc rdf"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 version="1.0">
<!---->
<xsl:import href="/usr/share/xml/tei/stylesheet/latex/tei.xsl"/>
<xsl:param name="institution">TEI Consortium</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="authorWord">Author:</xsl:param>
<xsl:param name="revisedWord">revised</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="realFigures">true</xsl:param>
<xsl:param name="latexLogoFile">logo.pdf</xsl:param>
<xsl:param name="classParameters">10pt</xsl:param>

<xsl:key name="IDS" match="*[@id]" use="@id"/>

<xsl:output method="text" encoding="utf8"/>

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

<xsl:template name="preambleHook">
%\renewcommand\ttdefault{lmtt}
\fvset{numberblanklines=false,xleftmargin=0mm,xrightmargin=0mm}
\hypersetup{colorlinks=true}
</xsl:template>

<xsl:template name="printTitleAndLogo">
<xsl:text>
\mbox{}\vskip-0.5in
\raisebox{0.5in}{\hbox{\hskip-.8in\includegraphics[width=1in]{</xsl:text>
<xsl:choose>
<xsl:when test="$realFigures='true'">
<xsl:text>logo</xsl:text>
</xsl:when>
<xsl:otherwise>
<xsl:text>FIG0</xsl:text>
</xsl:otherwise>
</xsl:choose>
<xsl:text>}}}
\vskip-0.5in
\hrule
\parbox[b]{.75\textwidth}{\fontsize{14pt}{16pt}\bfseries\sffamily\selectfont \@title}
\vskip10pt
\par{\fontsize{11pt}{13pt}\sffamily\itshape\selectfont\TheDate}
\vskip 2pt
\hrule
\vspace{12pt}</xsl:text>
</xsl:template>


<xsl:template match="tei:output[@url]">
\VerbatimInput[frame=single,fillcolor=\color{yellow}{<xsl:value-of select="@url"/>}
</xsl:template>

<xsl:template match="tei:input[@url]">
\VerbatimInput[numbers=left]{<xsl:value-of select="@url"/>}
</xsl:template>

<xsl:template match="tei:output">
<xsl:choose>
<xsl:when test="@n">
\begin{Verbatim}[frame=single,fillcolor=\color{yellow},label={<xsl:value-of select="@n"/>}]
<xsl:apply-templates mode="eg"/>
\end{Verbatim}
</xsl:when>
<xsl:otherwise>
\begin{Verbatim}[frame=single,fillcolor=\color{yellow}]
<xsl:apply-templates mode="eg"/>
\end{Verbatim}
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:input">
<xsl:choose>
<xsl:when test="@n">
\begin{Verbatim}[numbers=left,label={<xsl:value-of select="@n"/>}]
<xsl:apply-templates mode="eg"/>
\end{Verbatim}
</xsl:when>
<xsl:otherwise>
\begin{Verbatim}[numbers=left]
<xsl:apply-templates mode="eg"/>
\end{Verbatim}
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:gi">
<xsl:text>\texttt{&lt;</xsl:text>
<xsl:apply-templates/>
<xsl:text>&gt;}</xsl:text>
</xsl:template>

<xsl:template match="tei:ident">
<xsl:text>\textsf{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
 </xsl:template>

<xsl:template match="tei:code">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:kw">
<xsl:text>{\ttfamily\underline{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}}</xsl:text>
</xsl:template>

<xsl:template match="tei:value">
<xsl:text>`{\ttfamily </xsl:text>
<xsl:apply-templates/>
<xsl:text>}'</xsl:text>
</xsl:template>


<xsl:template match="tei:path">
<xsl:text>\path{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
 </xsl:template>

<xsl:template match="tei:emph">
<xsl:text>\textit{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
 </xsl:template>

<xsl:template match="tei:command">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:software">
<xsl:text>\textit{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:url">
<xsl:text>\url{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
 </xsl:template>

<xsl:template match="tei:oList">
\begin{enumerate}
<xsl:apply-templates/>
\end{enumerate}
 </xsl:template>

<xsl:template match="tei:uList">
\begin{itemize}
<xsl:apply-templates/>
\end{itemize}
</xsl:template>


<xsl:template match="tei:glossList/tei:label"/>
<xsl:template match="tei:glossList/tei:item">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:glossList">
\begin{description}
<xsl:for-each select="tei:label">
  \item[<xsl:value-of select="."/>]
<xsl:apply-templates select="following-sibling::tei:item[1]"/>
</xsl:for-each>
\end{description}
</xsl:template>

</xsl:stylesheet>

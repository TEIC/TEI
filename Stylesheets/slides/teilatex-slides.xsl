<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->
<xsl:stylesheet 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="../latex/tei.xsl"/>
<xsl:import href="slides-common.xsl"/>

<xsl:output method="text" encoding="utf-8"/>
<xsl:variable name="docClass">beamer</xsl:variable>
<xsl:param name="classParameters"></xsl:param>
<xsl:param name="beamerClass">PaloAlto</xsl:param>
<xsl:param name="startRed">\color{red}</xsl:param>
<xsl:param name="endRed"></xsl:param>
<xsl:param name="startBold">\textbf{</xsl:param>
<xsl:param name="startItalic">\textit{</xsl:param>
<xsl:param name="endBold">}</xsl:param>
<xsl:param name="endItalic">}</xsl:param>
<xsl:param name="spaceCharacter">&#160;</xsl:param>
<xsl:template name="lineBreak">
  <xsl:param name="id"/>
  <xsl:text>\\&#10;</xsl:text>
</xsl:template>

<xsl:template name="latexPackages">
\usepackage{colortbl}
\usetheme{<xsl:value-of select="$beamerClass"/>}
\usepackage{times}
\usepackage{fancyvrb}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
\setbeamercovered{transparent}
\let\mainmatter\relax
\let\frontmatter\relax
\let\backmatter\relax
\let\endfoot\relax
\let\endlastfoot\relax
</xsl:template>

<xsl:template name="latexLayout">
\date{<xsl:value-of
select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition/tei:date"/>}
\institute{<xsl:value-of
select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority"/>}
<xsl:if test="not($latexLogo='')">
\pgfdeclareimage[height=.5cm]{logo}{FIG0}
\logo{\pgfuseimage{logo}}
</xsl:if>
</xsl:template>

<xsl:template name="latexBegin">
\frame{\maketitle}

<xsl:if test=".//tei:div0">
  \begin{frame} \frametitle{Outline} 
  \tableofcontents
  \end{frame}
</xsl:if>
</xsl:template>


<xsl:template match="tei:div/tei:head"/>
<xsl:template match="tei:div0/tei:head"/>
<xsl:template match="tei:div1/tei:head"/>

<xsl:template match="tei:div0">
  \section{<xsl:for-each select="tei:head"><xsl:apply-templates/></xsl:for-each>}
  \begin{frame} 
  \frametitle{<xsl:for-each
  select="tei:head"><xsl:apply-templates/></xsl:for-each>}
  <xsl:choose>
    <xsl:when test="tei:*[not(local-name(.)='div1')]">
      <xsl:apply-templates select="tei:*[not(local-name(.)='div1')]"/>
    </xsl:when>
    <xsl:otherwise>
      {\Huge…}
    </xsl:otherwise>
  </xsl:choose>
  \end{frame}
  <xsl:apply-templates select="tei:div1"/>
</xsl:template>

<xsl:template match="tei:div|tei:div1">
\begin{frame}<xsl:choose>
<xsl:when test="@rend='fragile'">[fragile]</xsl:when>
<xsl:when test=".//tei:eg">[fragile]</xsl:when>
<xsl:when test=".//tei:Output">[fragile]</xsl:when>
<xsl:when test=".//tei:Screen">[fragile]</xsl:when>
<xsl:when test=".//teix:egXML">[fragile]</xsl:when>
</xsl:choose>
<xsl:text>&#10;</xsl:text>
  \frametitle{<xsl:for-each select="tei:head"><xsl:apply-templates/></xsl:for-each>}
  <xsl:apply-templates/>
\end{frame}
</xsl:template>

  <xsl:template name="makePic">
  <xsl:if test="@xml:id">\hypertarget{<xsl:value-of select="@xml:id"/>}{}</xsl:if>
  <xsl:text>\includegraphics[</xsl:text>
  <xsl:call-template name="graphicsAttributes">
    <xsl:with-param name="mode">latex</xsl:with-param>
  </xsl:call-template>
  <xsl:if test="not(@width) and not (@height) and not(@scale)">
    <xsl:text>width=\textwidth</xsl:text>
  </xsl:if>
  <xsl:text>]{</xsl:text>
      <xsl:choose>
	<xsl:when test="@url">
	  <xsl:value-of select="@url"/>
	</xsl:when>
	<xsl:when test="@entity">
	  <xsl:value-of select="unparsed-entity-uri(@entity)"/>
	</xsl:when>
      </xsl:choose>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[not(@rend)]">
  <xsl:text>\alert{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:item[@rend='pause']">
\item <xsl:apply-templates/>\pause
</xsl:template>

<xsl:template match="tei:eg">
\begin{Verbatim}[fontsize=\scriptsize,frame=single,fillcolor=\color{yellow}]
<xsl:apply-templates mode="eg"/>
\end{Verbatim}
</xsl:template>

<xsl:template match="teix:egXML">
\begin{scriptsize}
\bgroup
\ttfamily\mbox{}
<xsl:apply-templates mode="verbatim"/>
\egroup
\end{scriptsize}
</xsl:template>
  

<xsl:template match="tei:table">
\par  
\begin{scriptsize}
\begin{longtable}
<xsl:call-template name="makeTable"/>
\end{longtable}
\end{scriptsize}
</xsl:template>

</xsl:stylesheet>

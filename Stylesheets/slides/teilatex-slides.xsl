<?xml version="1.0"?>
<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL LaTeX stylesheet to make slides

##LICENSE
-->
<xsl:stylesheet 		
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:atom="http://www.w3.org/2005/Atom"
    xmlns:rng="http://relaxng.org/ns/structure/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0">
  <xsl:import href="../latex/tei.xsl"/>
  <xsl:import href="slides-common.xsl"/>
  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:*"/>
  <xsl:output method="text" encoding="utf-8"/>
  <xsl:variable name="docClass">beamer</xsl:variable>
  <xsl:param name="classParameters"/>
  <xsl:param name="beamerClass">PaloAlto</xsl:param>
  <xsl:template name="newLine">
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:template name="latexPackages">
\usepackage{colortbl}
\usepackage{longtable}
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
\date{<xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition/tei:date"/>}
\institute{<xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority"/>}
<xsl:if test="not($latexLogo='')">
<xsl:text>\pgfdeclareimage[height=1cm]{logo}{</xsl:text>
<xsl:choose>
  <xsl:when test="$realFigures='true'">
    <xsl:value-of select="$latexLogo"/>}
  </xsl:when>
  <xsl:otherwise>
    <xsl:text>FIG0</xsl:text>
  </xsl:otherwise>
</xsl:choose>
<xsl:text>}</xsl:text>
\logo{\pgfuseimage{logo}}
</xsl:if>
</xsl:template>

<xsl:template name="latexBegin">
\frame{\maketitle}
</xsl:template>

<xsl:template match="tei:divGen[@type='toc']">
  \begin{frame} 
  \frametitle{Outline} 
  \tableofcontents
  \end{frame}
</xsl:template>

<xsl:template match="tei:div/tei:head"/>
<xsl:template match="tei:div0/tei:head"/>
<xsl:template match="tei:div1/tei:head"/>
<xsl:template match="tei:div2/tei:head"/>

<xsl:template match="tei:div0">
 <xsl:call-template name="makeOuterFrame"/>
</xsl:template>

<xsl:template match="tei:div1">
  <xsl:choose>
    <xsl:when test="parent::tei:div0">
      <xsl:call-template name="makeFrame"/>
    </xsl:when>
    <xsl:when test="tei:div2">
      <xsl:call-template name="makeOuterFrame"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="makeFrame"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:div2|tei:div3">
  <xsl:call-template name="makeFrame"/>
</xsl:template>

<xsl:template match="tei:div">
  <xsl:choose>
    <xsl:when test="tei:div and parent::tei:body">
      <xsl:call-template name="makeOuterFrame"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="makeFrame"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="makeOuterFrame">
  <xsl:text>&#10;\section{</xsl:text>
  <xsl:for-each select="tei:head">
    <xsl:apply-templates/>
  </xsl:for-each>
   <xsl:text>}</xsl:text>
  <xsl:text>&#10;\begin{frame} 
  \frametitle{</xsl:text>
    <xsl:for-each select="tei:head">
      <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:choose>
	<xsl:when test="tei:*[not(starts-with(local-name(.),'div'))]">
	  <xsl:apply-templates
	      select="tei:*[not(starts-with(local-name(.),'div'))]"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>{\Huge&#x2026;}</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;\end{frame}&#10;</xsl:text>
      <xsl:apply-templates select="tei:div1|tei:div2|tei:div"/>
  </xsl:template>

<xsl:template name="makeFrame">
  <xsl:text>&#10;\begin{frame}</xsl:text>
  <xsl:choose>
    <xsl:when test="@rend='fragile'">
      <xsl:text>[fragile]</xsl:text>
    </xsl:when>
    <xsl:when test=".//tei:eg">
      <xsl:text>[fragile]</xsl:text>
    </xsl:when>
    <xsl:when test=".//tei:Output">
      <xsl:text>[fragile]</xsl:text>
    </xsl:when>
    <xsl:when test=".//tei:Screen">
      <xsl:text>[fragile]</xsl:text>
    </xsl:when>
    <xsl:when test=".//teix:egXML">
      <xsl:text>[fragile]</xsl:text>
    </xsl:when>
  </xsl:choose>
  <xsl:text>&#10;\frametitle{</xsl:text>
  <xsl:for-each select="tei:head">
    <xsl:apply-templates/>
  </xsl:for-each>
  <xsl:text>}</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;\end{frame}&#10;</xsl:text>
</xsl:template>

<xsl:template name="makePic">
  <xsl:if test="@xml:id">
    <xsl:text>\hypertarget{</xsl:text>
    <xsl:value-of select="@xml:id"/>
    <xsl:text>}{}</xsl:text>
  </xsl:if>
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
  <xsl:text>\item </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>\pause </xsl:text>
</xsl:template>

<xsl:template match="tei:eg">
  <xsl:text>\begin{Verbatim}[fontsize=\scriptsize,frame=single,fillcolor=\color{yellow}]&#10;</xsl:text>
  <xsl:apply-templates mode="eg"/>
  <xsl:text>\end{Verbatim}</xsl:text>
</xsl:template>

<xsl:template match="teix:egXML">
  <xsl:text>\begin{scriptsize}
  \bgroup
  \ttfamily\mbox{}</xsl:text>
  <xsl:apply-templates mode="verbatim"/>
  <xsl:text>\egroup
  \end{scriptsize}</xsl:text>
</xsl:template>


<xsl:template match="tei:table">
  <xsl:text>\par  
  \begin{scriptsize}
  \begin{longtable}</xsl:text>
  <xsl:call-template name="makeTable"/>
  <xsl:text>\end{longtable}
  \end{scriptsize}</xsl:text>
</xsl:template>

  <xsl:template name="makeFigureStart">
    \noindent
  </xsl:template>

  <xsl:template name="makeFigureEnd">
  </xsl:template>

<xsl:template name="tableHline">
<xsl:text> \hline </xsl:text>
</xsl:template>


</xsl:stylesheet>

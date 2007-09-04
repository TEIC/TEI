<xsl:stylesheet
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" encoding="utf-8"/>

  <xsl:template match="/">
\documentclass[12pt]{article}
\usepackage{times}
\pagestyle{empty}
\usepackage{pst-all}
\renewcommand{\psedge}{\ncangle}
\psset{arrows=->,xbbd=1in,angleB=-180,angleA=0,levelsep=90pt,armA=1in,treemode=R, treesep=6pt}
%\psset{angleB=90,angleA=-90,levelsep=42pt,armB=14pt}
\def\XX#1{\Tr[ref=l]{\psframebox[fillstyle=solid,fillcolor=lightgray]{#1}}}
\begin{document}
\color{white}\fbox{\color{black}{<xsl:apply-templates select="*"/>}}
\end{document}
  </xsl:template>

<xsl:template match="*">
  <xsl:choose>
    <xsl:when test="*">
  <xsl:text>&#10;\pstree</xsl:text>
  <xsl:variable name="longones">
    <xsl:for-each select="*">
      <xsl:if test="string-length(local-name(.))&gt;12">x</xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <xsl:if test="string-length($longones)&gt;0">
    <xsl:text>[levelsep=150pt]</xsl:text>
  </xsl:if>
  <xsl:text>{\XX{</xsl:text>
  <xsl:call-template name="name"/>
  <xsl:text>}}</xsl:text>
  <xsl:text>{</xsl:text>
  <xsl:apply-templates select="*"/>
  <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>&#10;\XX{</xsl:text>
      <xsl:call-template name="name"/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="name">
  <xsl:variable name="n">
  <xsl:value-of select="local-name()"/>
  </xsl:variable>
  <xsl:text>\textbf{</xsl:text>
  <xsl:value-of select="$n"/>
  <xsl:text>}</xsl:text>
  <xsl:if test="ancestor::*">
<!--count(parent::*/*[local-name()=$n])&gt;1 -->
    <xsl:text>{}\textsuperscript{</xsl:text>
    <xsl:number/>
    <xsl:text>}</xsl:text>
  </xsl:if>
</xsl:template>
</xsl:stylesheet>


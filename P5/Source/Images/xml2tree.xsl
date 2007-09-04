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
%\psset{angleB=-180,angleA=0,levelsep=120pt,treemode=R}
\psset{angleB=90,angleA=-90,levelsep=36pt,armB=14pt}
\def\XX#1{\Tr{\psframebox{#1}}}
\begin{document}
\color{white}\fbox{\color{black}{<xsl:apply-templates select="*"/>}}
\end{document}
  </xsl:template>

<xsl:template match="*">
  <xsl:choose>
    <xsl:when test="*">
  <xsl:text>&#10;\pstree{\XX{</xsl:text>
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
  <xsl:text>\textbf{</xsl:text>
  <xsl:value-of select="local-name()"/>
  <xsl:text>}</xsl:text>
  <xsl:if test="@n">
    <xsl:text> </xsl:text>
    <xsl:value-of select="@n"/>
  </xsl:if>
</xsl:template>
</xsl:stylesheet>


<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

<xsl:output method="text"/>

<xsl:template match="tei:Button">
<xsl:text>{\ttfamily\bfseries </xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Command">
<xsl:text>{\ttfamily\bfseries </xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>


<xsl:template match="tei:Value">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Code">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Field">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Filespec">
<xsl:text>\path{</xsl:text>
<xsl:value-of select="."/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Input">
  <xsl:choose>
    <xsl:when test="contains(.,'://')">
     <xsl:text>\path{</xsl:text>
     <xsl:value-of select="."/>
     <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>\texttt{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="tei:Key">
<xsl:text>{\ttfamily\underline{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Link">
<xsl:text>{\ttfamily\underline{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Menu">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Keyword">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Output">
\begin{quote}\ttfamily\color{black}\obeylines
<xsl:apply-templates/>
\end{quote}
</xsl:template>

<xsl:template match="tei:head/Output">
<xsl:text>\texttt{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
</xsl:template>
<xsl:template match="tei:Screen">
\begin{verbatim}
<xsl:apply-templates/>
\end{verbatim}
</xsl:template>

<xsl:template match="tei:Program">
\begin{quote}\ttfamily\color{black}\obeylines
<xsl:apply-templates/>
\end{quote}
</xsl:template>

<xsl:template match="tei:Icon">
<xsl:text>{\ttfamily\underline{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
<xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="tei:Label">
<xsl:text>\textit{</xsl:text>
<xsl:apply-templates/>
<xsl:text>}</xsl:text>
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

</xsl:stylesheet>

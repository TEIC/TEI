<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to FO or HTML

##LICENSE
-->

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fotex="http://www.tug.org/fotex"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">


<!-- parameterization -->

<xsl:import href="../common/teicommon.xsl"/>
<xsl:import href="tei-param.xsl"/>

<xsl:strip-space elements="tei:cell"/>

<xsl:output indent="no"/>

<!-- overrides -->

<xsl:key name="IDS" match="tei:*[@id]|tei:*[@xml:id]" use="@id|@xml:id"/>

<xsl:variable name="top" select="/"/>
<xsl:variable name="tableSpecs">
  <xsl:choose>
  <xsl:when test="$readColSpecFile">
  <xsl:copy-of
      select="document($readColSpecFile,$top)/Info"/>
 </xsl:when>
 <xsl:otherwise> <Info></Info></xsl:otherwise>
</xsl:choose>
</xsl:variable>

<!-- example of different configuration
<xsl:variable name="activeLinebreaks">true</xsl:variable>
<xsl:variable name="activePagebreaks">true</xsl:variable>

<xsl:variable name="bodyMaster">12</xsl:variable>
<xsl:variable name="bodySize">
 <xsl:value-of select="$bodyMaster"/><xsl:text>pt</xsl:text>
</xsl:variable>
<xsl:variable name="smallSize">
 <xsl:value-of select="$bodyMaster * 0.9"/><xsl:text>pt</xsl:text>
</xsl:variable>
<xsl:variable name="exampleSize">
 <xsl:value-of select="$bodyMaster * 0.8"/><xsl:text>pt</xsl:text>
</xsl:variable>
<xsl:variable name="pageMarginBottom">325pt</xsl:variable>
<xsl:variable name="pageMarginRight">250pt</xsl:variable>
-->
<xsl:include href="tei-lib.xsl"/>
<xsl:include href="tei-bib.xsl"/>
<xsl:include href="tei-drama.xsl"/>
<xsl:include href="tei-figure.xsl"/>
<xsl:include href="tei-front.xsl"/>
<xsl:include href="tei-lists.xsl"/>
<xsl:include href="tei-notes.xsl"/>
<xsl:include href="tei-para.xsl"/>
<xsl:include href="tei-poetry.xsl"/>
<xsl:include href="tei-special.xsl"/>
<xsl:include href="tei-struct.xsl"/>
<xsl:include href="tei-table.xsl"/>
<xsl:include href="tei-xref.xsl"/>
<xsl:include href="tei-markers.xsl"/>
<xsl:include href="tei-math.xsl"/>
<xsl:include href="tei-makecolspec.xsl"/>


</xsl:stylesheet>

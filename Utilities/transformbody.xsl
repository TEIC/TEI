<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="exsl teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"
   cdata-section-elements="tei:eg"/>

<!--
<xsl:template match="teix:egXML">
    <xsl:variable name="x">
      <xsl:value-of select="ancestor::tei:div[@xml:id][1]/@xml:id"/>
      <xsl:text>-EGXML-</xsl:text>
      <xsl:number level="any"  from="tei:body/tei:div"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:attribute name="xml:id">
	<xsl:value-of select="$x"/>
      </xsl:attribute>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates 
	  select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
    </xsl:copy>
</xsl:template>
-->

<xsl:template match="tei:bibl">
  <xsl:variable name="ID">
    <xsl:value-of select="ancestor::tei:div/@xml:id"/>
    <xsl:text>-BIBL-</xsl:text>
    <xsl:number level="any" from="tei:body/tei:div"/>
  </xsl:variable>
<xsl:message>--------------------------------------</xsl:message>
  <xsl:choose>
    <xsl:when test="key('IDS',$ID)">
      <ptr xmlns="http://www.tei-c.org/ns/1.0" type="cit"
	   target="#{$ID}"/>
<xsl:message>Link <xsl:value-of select="."/> to <xsl:value-of
select="$ID"/>, <xsl:value-of select="key('IDS',$ID)"/></xsl:message>
    </xsl:when>
    <xsl:when test="$ID='DI-BIBL-12' or $ID='DI-BIBL-13'">
      <ptr xmlns="http://www.tei-c.org/ns/1.0" type="cit"
	   target="#DI-BIBL-11"/>
<xsl:message>Link <xsl:value-of select="."/> to DI-BIBL-11, <xsl:value-of select="key('IDS','DI-BIBL-11')"/></xsl:message>
    </xsl:when>
    <xsl:when test="ancestor::tei:div[@xml:id='BIB']">
      <xsl:copy>
	<xsl:apply-templates select="@*"/>
	<xsl:apply-templates 
	    select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
      </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
<xsl:message>NO Link <xsl:value-of select="$ID"/> for <xsl:value-of select="."/></xsl:message>
      <xsl:copy>
	<xsl:apply-templates select="@*"/>
	<xsl:apply-templates 
	    select="teix:*|tei:*|rng:*|comment()|processing-instruction()|text()"/>
      </xsl:copy>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>

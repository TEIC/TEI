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
   encoding="utf-8"
   indent="yes"
   cdata-section-elements="tei:eg teix:egXML"
   omit-xml-declaration="yes"/>


<xsl:template match="tei:div1|tei:div2|tei:div3|tei:div4">
<xsl:element name="div">

<xsl:if test="name(.) = 'div1'">
  <xsl:attribute name="xmlns">http://www.tei-c.org/ns/1.0</xsl:attribute>
</xsl:if>

<xsl:attribute name="type">
<xsl:value-of select="name(.)"/>
</xsl:attribute>
<xsl:if test="@xml:id">
<xsl:attribute name="xml:id">
<xsl:value-of select="@xml:id"/>
</xsl:attribute>
</xsl:if>
<xsl:if test="@n">
<xsl:attribute name="n">
<xsl:value-of select="@n"/>
</xsl:attribute>
</xsl:if>
<xsl:apply-templates/>
</xsl:element>
</xsl:template>

 <xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates 
      select="*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|text()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="comment()">
  <xsl:copy/>
</xsl:template>


</xsl:stylesheet>

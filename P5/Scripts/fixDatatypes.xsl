<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 xmlns="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="tei rng xsl teix"
 version="2.0">

<!-- run this against every *spec file containing a datatype -->

<xsl:output 
   method="xml"
   encoding="utf-8"
   indent="yes"
   omit-xml-declaration="yes"/>

  <xsl:template match="tei:datatype">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:choose>
        <xsl:when test="rng:ref">  <dataRef key="{concat('tei',rng:ref/@name)}"/>
        </xsl:when>
        <xsl:when test="rng:data">  <dataRef name="{rng:data/@type}"/>
        </xsl:when>
        <xsl:when test="rng:text">  <textNode/>
        </xsl:when>
        
       <xsl:otherwise>
          <xsl:message>ERROR : datatypes must be atomic</xsl:message>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:copy>
  </xsl:template>
  
<!-- copy everything else -->

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
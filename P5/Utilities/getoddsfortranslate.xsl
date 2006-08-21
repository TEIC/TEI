<xsl:stylesheet 
 xmlns:teix="http://www.tei-c.org/ns/Examples"
 xmlns:tei="http://www.tei-c.org/ns/1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:rng="http://relaxng.org/ns/structure/1.0"
 extension-element-prefixes="exsl"
 exclude-result-prefixes="teix rng tei"
 xmlns:exsl="http://exslt.org/common"
 version="1.0">

<xsl:param name="module">core</xsl:param>
<xsl:param name="translang">zh-tw</xsl:param>
<xsl:param name="transdate">2006-06-05</xsl:param>
<xsl:output method="xml"/>

<xsl:template match="/">
    <xsl:apply-templates select="//tei:elementSpec[@module=$module]"/>
</xsl:template>

<xsl:template match="tei:elementSpec">
  <exsl:document         
      method="xml"
      cdata-section-elements="tei:eg teix:egXML" 
      omit-doctype-declaration="yes"
      omit-xml-declaration="yes"
      href="fortranslate/{$module}/{@ident}.xml">
   <xsl:copy>
     <xsl:apply-templates select="@*"/>
     <xsl:apply-templates
	 select="*|comment()|processing-instruction()|text()"/>
   </xsl:copy>
 </exsl:document>
</xsl:template>

<xsl:template match="tei:valDesc|tei:desc|tei:gloss">
  <xsl:copy>
     <xsl:apply-templates select="@*"/>
     <xsl:apply-templates
	 select="*|comment()|processing-instruction()|text()"/>
  </xsl:copy>
  <xsl:copy>
     <xsl:attribute name="xml:lang">
       <xsl:value-of select="$translang"/>
     </xsl:attribute>
     <xsl:attribute name="notBefore">
       <xsl:value-of select="$transdate"/>
     </xsl:attribute>
     <xsl:if test="string-length(text())&gt;0">
       <xsl:text>TO BE TRANSLATED</xsl:text>
     </xsl:if>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|text()|comment()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="@*"/>
  <xsl:apply-templates       
      select="*|comment()|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

</xsl:stylesheet>








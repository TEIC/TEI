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
   cdata-section-elements="eg egXML"
   omit-xml-declaration="yes"/>

<xsl:param name="PREFIX" select="'newSource'"/>
<xsl:variable name="top" select="/"/>
<xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
<xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>

<xsl:include href="transformbody.xsl"/>

<xsl:key name="IDS" match="tei:*" use="@id"/>


<xsl:template match="/">
  <xsl:variable name="outName">
    <xsl:value-of select="$PREFIX"/>-driver.xml</xsl:variable>
    <xsl:message>write <xsl:value-of select="$outName"/></xsl:message>
 <exsl:document         
   method="xml"
   cdata-section-elements="eg egXML" 
   omit-doctype-declaration="yes"
   omit-xml-declaration="yes"
        href="{$outName}">

<xsl:text disable-output-escaping="yes">&lt;?xml version="1.0"?&gt;
&lt;!-- $Author$ $Date$ $Id$ --&gt;
&lt;!DOCTYPE TEI [
</xsl:text>
<xsl:for-each
 select=".//tei:tagDoc|.//tei:patternDoc|.//tei:classDoc|.//tei:div1[@id]">
  <xsl:sort select="tei:ident"/>
  <xsl:variable name="filename">
     <xsl:choose>
   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="translate(@id,$uc,$lc)"/>
        <xsl:text>.odd</xsl:text>
    </xsl:when>
<xsl:when test='@id'>
        <xsl:value-of select="translate(@id,$uc,$lc)"/>
        <xsl:text>.odd</xsl:text>
</xsl:when>
    <xsl:otherwise>
       <xsl:text>mixno.odd</xsl:text>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

  <xsl:variable name="ident">
     <xsl:choose>
   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="translate(@id,$uc,$lc)"/>
      <xsl:text>.odd</xsl:text>
    </xsl:when>
    <xsl:otherwise>
        <xsl:value-of select="tei:ident"/>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

<xsl:variable name="entname">
<xsl:choose>   <xsl:when test="name(.)='div1'">
        <xsl:value-of select="$ident"/>
    </xsl:when>

    <xsl:otherwise>
    <xsl:value-of select="$filename"/>
    </xsl:otherwise>
    </xsl:choose>
    </xsl:variable>

    <xsl:text disable-output-escaping="yes">
&lt;!ENTITY </xsl:text>
    <xsl:value-of select="$entname"/>
    <xsl:text disable-output-escaping="yes"> SYSTEM "</xsl:text>
    <xsl:value-of select="$PREFIX"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of  select="translate(ancestor-or-self::tei:div1/@id,$lc,$uc)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="$filename"/>
    <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
  </xsl:for-each>
<xsl:text disable-output-escaping="yes">
]>
</xsl:text>
  <xsl:apply-templates/> 
</exsl:document>

</xsl:template>


</xsl:stylesheet>

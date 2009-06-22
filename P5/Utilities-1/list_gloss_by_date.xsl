<xsl:stylesheet version="1.0"
		xmlns="http://www.tei-c.org/ns/1.0"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		extension-element-prefixes="exsl"
		exclude-result-prefixes="exsl tei"
		xmlns:exsl="http://exslt.org/common"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- extract every gloss and desc, writing them to a distinct file,
with the name composed by concatening @ident values -->
  <xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:param name="dir"/>
<xsl:key name="GLOSS" match="tei:gloss[not(@xml:lang) and string-length(.)&gt;0]" use="1"/>
<xsl:key name="DESC"  match="tei:desc[not(@xml:lang) and string-length(.)&gt;0]" use="1"/>


<xsl:template match="/">
<xsl:message>GLOSS <xsl:value-of select="count(key('GLOSS',1))"/></xsl:message>
<xsl:message>DESC <xsl:value-of select="count(key('DESC',1))"/></xsl:message>
<xsl:for-each select="key('GLOSS',1)">
  <xsl:call-template name="go"/>
  </xsl:for-each>
  <xsl:for-each select="key('DESC',1)">
  <xsl:call-template name="go"/>
  </xsl:for-each>
</xsl:template>

  <xsl:template name="go">
    <xsl:variable name="type">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:variable name="outName">
      <xsl:value-of select="$dir"/>
      <xsl:text>/</xsl:text>
    <xsl:for-each select="ancestor::tei:*[@ident]/@ident">
      <xsl:value-of select="."/>
      <xsl:text>_</xsl:text>
    </xsl:for-each>
      <xsl:value-of select="$type"/>
      <xsl:text>.xml</xsl:text>
    </xsl:variable>

 <exsl:document         
   method="xml"
   encoding="utf-8" href="{$outName}">
   <xsl:copy>
     <xsl:value-of select="translate(normalize-space(.),'ABCDEFGHIJKLMNOPQRSTUVWXYZ,-.','abcdefghijklmnopqrstuvwxyz')"/>
   </xsl:copy>
 </exsl:document>

</xsl:template>
</xsl:stylesheet>

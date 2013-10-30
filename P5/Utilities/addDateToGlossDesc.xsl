<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    version="2.0">
<xsl:param name="date">      
  <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
</xsl:param>
<xsl:param name="file"/>

<xsl:key name="I" match="elementSpec/gloss|elementSpec/desc|elementSpec/remarks" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="classSpec/gloss|elementSpec/desc|elementSpec/remarks" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="macroSpec/gloss|elementSpec/desc|elementSpec/remarks" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="attDef/gloss|attDef/desc|attDef/remarks" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="valItem/gloss|valItem/desc|valItem/remarks" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,parent::valItem/@ident,local-name(),@xml:lang)"/>


<xsl:output 
   method="xml"
   indent="yes"
   encoding="utf-8"/>

<xsl:template 
    match="@*|text()|comment()|processing-instruction()"  >
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="gloss[not(@xml:lang) or @xml:lang='en']|desc[not(@xml:lang) or @xml:lang='en']">
  <xsl:variable name="identifier">
      <xsl:value-of select="(ancestor::*[@ident]/@ident,local-name(),@xml:lang)"
		    separator=""/>
  </xsl:variable>
  <xsl:variable name="new" select="normalize-space(.)"/>
  <xsl:variable name="old" select="doc(resolve-uri($file,base-uri(/*)))/key('I',$identifier)"/>
  <xsl:copy>
    <xsl:choose>
      <xsl:when test="$old=$new">
	<xsl:message>Reset date for <xsl:value-of select="($identifier,$date)"
	separator=" to "/></xsl:message>
	<xsl:attribute name="versionDate" select="$date"/>
	<xsl:attribute name="xml:lang">en</xsl:attribute>
	<xsl:apply-templates 
	    select="*|processing-instruction()|comment()|text()"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates 
	    select="@*|*|processing-instruction()|comment()|text()"/>	
      </xsl:otherwise>
  </xsl:choose>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>

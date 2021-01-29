<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    version="2.0">
<!--
For a given *Spec file, make a basic copy,
but look at each English desc/gloss/remarks/valDesc and see
if its unchanged since what we find in a previous version
whose file name and date are passed as parameters;
bump the date in this file down to the date of the comparison file
if the string is unchanged. Done repeatedly, this means each file
has the earliest date on which the status quo existed.
-->
<xsl:param name="date">      
  <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
</xsl:param>
<xsl:param name="file"/>
<xsl:param name="lang">en</xsl:param>

<!-- 
     identify the context with concatenation of @idents up the tree,
     and the element name
-->
<xsl:key name="I" match="attDef/gloss[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="valItem/gloss[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,parent::valItem/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="attDef/desc[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="valItem/desc[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,parent::valItem/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="attDef/remarks[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="valItem/remarks[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,parent::valItem/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="attDef/valDesc[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="elementSpec/gloss[@xml:lang=$lang]" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="classSpec/gloss[@xml:lang=$lang]" use="concat(parent::classSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="macroSpec/gloss[@xml:lang=$lang]" use="concat(parent::macroSpec/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="elementSpec/desc[@xml:lang=$lang]" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="classSpec/desc[@xml:lang=$lang]" use="concat(parent::classSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="macroSpec/desc[@xml:lang=$lang]" use="concat(parent::macroSpec/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="elementSpec/remarks[@xml:lang=$lang]" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="classSpec/remarks[@xml:lang=$lang]" use="concat(parent::classSpec/@ident,local-name(),@xml:lang)"/>
<xsl:key name="I" match="macroSpec/remarks[@xml:lang=$lang]" use="concat(parent::macroSpec/@ident,local-name(),@xml:lang)"/>

<xsl:key name="I" match="attDef/exemplum[@xml:lang=$lang]" use="concat(ancestor::attDef/parent::attList/parent::*/@ident,ancestor::attDef/@ident,local-name(),@xml:lang,count(preceding-sibling::exemplum) + 1)"/>
<xsl:key name="I" match="elementSpec/exemplum[@xml:lang=$lang]" use="concat(parent::elementSpec/@ident,local-name(),@xml:lang,count(preceding-sibling::exemplum) + 1)"/>
<xsl:key name="I" match="classSpec/exemplum[@xml:lang=$lang]" use="concat(parent::classSpec/@ident,local-name(),@xml:lang,count(preceding-sibling::exemplum) + 1)"/>
<xsl:key name="I" match="macroSpec/exemplum[@xml:lang=$lang]" use="concat(parent::macroSpec/@ident,local-name(),@xml:lang,count(preceding-sibling::exemplum) + 1)"/>

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

<!--
<xsl:template match="exemplum[@xml:lang=$lang]">
  <xsl:call-template name="check"/>
</xsl:template>
-->
<xsl:template match="gloss[@xml:lang=$lang]">
  <xsl:call-template name="check"/>
</xsl:template>

<xsl:template match="desc[@xml:lang=$lang]">
  <xsl:call-template name="check"/>
</xsl:template>

<xsl:template match="remarks[@xml:lang=$lang]">
  <xsl:call-template name="check"/>
</xsl:template>

<xsl:template match="valDesc[@xml:lang=$lang]">
  <xsl:call-template name="check"/>
</xsl:template>

  <xsl:template name="check">
  <xsl:variable name="identifier">
      <xsl:value-of select="(ancestor::*[@ident]/@ident,local-name(),@xml:lang)"
		    separator=""/>
      <xsl:if test="self::exemplum">
	<xsl:value-of select="count(preceding-sibling::exemplum) + 1"/>
      </xsl:if>
  </xsl:variable>
  <xsl:variable name="new" select="tei:normalize(.)"/>
  <xsl:variable name="old"
		select="tei:normalize(doc(resolve-uri($file,base-uri(/*)))/key('I',$identifier))"/>
 <xsl:message>check <xsl:value-of select="($identifier,$date,$old,$new)"	separator=" | "/></xsl:message>
  <xsl:copy>
    <xsl:choose>
      <xsl:when test="$old=$new">
	<xsl:message>Reset date for <xsl:value-of select="($identifier,$lang,$date)"
	separator=" / "/></xsl:message>
	<xsl:attribute name="versionDate" select="$date"/>
	<xsl:attribute name="xml:lang"><xsl:value-of select="$lang"/></xsl:attribute>
	<xsl:apply-templates 
	    select="@*[not(name()='xml:lang' or name()='versionDate')]|*|processing-instruction()|comment()|text()"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates 
	    select="@*|*|processing-instruction()|comment()|text()"/>	
      </xsl:otherwise>
  </xsl:choose>
  </xsl:copy>
</xsl:template>

<xsl:function name="tei:normalize">
  <xsl:param name="text"/>
  <!-- for comparison purposes, ignore trailing space, letter-case and
  trailing -->
  <xsl:sequence select="lower-case(replace(normalize-space($text),'\.$',''))"/>
</xsl:function>

</xsl:stylesheet>

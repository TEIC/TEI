<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  extension-element-prefixes="exsl"
  exclude-result-prefixes="tei exsl" 
  version="1.0">
<xsl:output indent="yes" encoding="utf8"/>
<xsl:key name="TVALS" match="value" use="@ident"/>
<xsl:key name="TATTS" match="attribute" use="@ident"/>
<xsl:key name="TELEMS" match="element" use="@ident"/>
<xsl:key name="ATTS" match="tei:attDef" use="@ident"/>
<xsl:key name="VALS" match="tei:valItem" use="@ident"/>
<xsl:template match="/">
<xsl:comment>
Probleme: &lt;argument> 
               &lt;byline>
	       &lt;catRef>
	       &lt;classCode>
	       &lt;classDecl>
	       &lt;closer>
	       &lt;creation>
	       &lt;dateline>
	       &lt;del> Korrektor oder andere Fassung
	       &lt;div>
	       &lt;divGen>
	       &lt;editor>???
	       &lt;gi> -> &lt;en>?
	       &lt;ident>
	       &lt;lb> -> &lt;zu>
	       &lt;notesstmt> -> notesgroup
	       &lt;pb> &lt;su>
	       &lt;principal>
	       &lt;rs> Bezug nehmende Zeichenkette
	       &lt;s> -> Satz
	       &lt;samplingDecl>
	       &lt;sic>
	       tag != Tag
	       &lt;taxonomy>
	       &lt;trailer>
</xsl:comment>
<i18n>
  <xsl:for-each select=".//tei:elementSpec">
    <xsl:sort select="@ident"/>
    <element ident="{@ident}">
      <xsl:variable name="this" select="@ident"/>
      <xsl:for-each select="document('teinames.xml')/i18n">
	<xsl:for-each select="key('TELEMS',$this)">
	  <xsl:copy-of select="equiv"/>
	</xsl:for-each>
      </xsl:for-each>
      <desc><xsl:value-of select="tei:desc"/></desc>
    </element>
  </xsl:for-each>
  <xsl:for-each select=".//tei:attDef">
    <xsl:sort select="@ident"/>
    <xsl:if test="not(@ident='xmlns' or @ident='TEIform')">
      <xsl:if test="generate-id(.)=generate-id(key('ATTS',@ident)[1])">
	<attribute ident="{@ident}">
	  <xsl:variable name="this" select="@ident"/>
	  <xsl:for-each select="document('teinames.xml')/i18n">
	    <xsl:for-each select="key('TATTS',$this)">
	      <xsl:copy-of select="equiv"/>
	    </xsl:for-each>
	  </xsl:for-each>
	  <desc><xsl:value-of select="tei:desc"/></desc>
	</attribute>
      </xsl:if>
    </xsl:if>
  </xsl:for-each>
  <xsl:for-each select=".//tei:valItem">
    <xsl:sort select="@ident"/>
	  <xsl:variable name="this" select="@ident"/>
    <xsl:if test="generate-id(.)=generate-id(key('VALS',@ident)[1])">
      <value ident="{@ident}">
      <xsl:for-each select="document('teinames.xml')/i18n">
	<xsl:for-each select="key('TVALS',$this)">
	  <xsl:copy-of select="equiv"/>
	</xsl:for-each>
      </xsl:for-each>
      </value>
    </xsl:if>
  </xsl:for-each>
</i18n>
</xsl:template>
</xsl:stylesheet>
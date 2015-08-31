<xsl:stylesheet version="2.0"
	  xmlns:rng="http://relaxng.org/ns/structure/1.0"
	  xmlns:tei="http://www.tei-c.org/ns/1.0"
	  xmlns:teix="http://www.tei-c.org/ns/Examples"
	  exclude-result-prefixes="rng tei"
	  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" encoding="utf-8" indent="yes"/>
<xsl:param name="lang">zh-tw</xsl:param>
<xsl:param name="date">2009-02-20</xsl:param>
<xsl:key name="MODULES" match="tei:moduleSpec" use="1"/>
<xsl:key name="SPEC" match="tei:classSpec" use="@module"/>
<xsl:key name="SPEC" match="tei:elementSpec" use="@module"/>
<xsl:key name="SPEC" match="tei:macroSpec" use="@module"/>

<xsl:template match="/">
    <xsl:for-each select="key('MODULES',1)">
      <xsl:sort select="@ident"/>
      <xsl:result-document href="{@ident}_examples_{$lang}.xml">
	<TEI  xmlns="http://www.tei-c.org/ns/1.0">
	  <text>
	    <body>
	      <xsl:for-each select="key('SPEC',@ident)">
		<xsl:sort select="local-name()"/>
		<xsl:sort select="@ident"/>
		<xsl:apply-templates select="."/>
	      </xsl:for-each>
	    </body>
	  </text>
	</TEI>
      </xsl:result-document>
    </xsl:for-each>
</xsl:template>

<xsl:template match="tei:elementSpec|tei:macroSpec|tei:classSpec">
  <xsl:if test=".//tei:exemplum">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each select="tei:attList[.//tei:exemplum/teix:egXML/node()]">
	<xsl:copy>
	  <xsl:for-each select="tei:attDef[.//tei:exemplum/teix:egXML/node()]">
	    <xsl:copy>
	      <xsl:copy-of select="@*"/>
	      <xsl:for-each select="tei:valList[.//tei:exemplum/teix:egXML/node()]">
		<xsl:copy>
		  <xsl:for-each select="tei:valItem[.//tei:exemplum/teix:egXML/node()]">
		    <xsl:copy>
		      <xsl:copy-of select="@*"/>
		      <xsl:apply-templates select="tei:exemplum"/>
		    </xsl:copy>
		  </xsl:for-each>
		</xsl:copy>
	      </xsl:for-each>
	      <xsl:apply-templates select="tei:exemplum"/>
	    </xsl:copy>
	  </xsl:for-each>
	</xsl:copy>
      </xsl:for-each>
      <xsl:apply-templates select="tei:exemplum"/>
    </xsl:copy>
  </xsl:if>
</xsl:template>

<xsl:template match="tei:exemplum"> 
<xsl:choose>
  <xsl:when test="teix:egXML[not(node())]"/>
  <xsl:when test="@xml:lang=$lang or not(@xml:lang)">
    <xsl:call-template name="show"/>
  </xsl:when>
</xsl:choose>
</xsl:template>

<xsl:template name="show">
  <xsl:element name="{local-name(.)}"
	       xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:attribute name="notAfter" select="$date"/>
    <xsl:apply-templates
	select="@*|*|text()|comment()|processing-instruction()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="@*|text()|comment()|processing-instruction()">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates       
      select="@*|*|text()|comment()|processing-instruction()"/>
 </xsl:copy>
</xsl:template>

</xsl:stylesheet>

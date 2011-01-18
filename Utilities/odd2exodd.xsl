<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a html xs xsl"
  version="2.0"
  xmlns:xi="http://www.w3.org/2001/XInclude"
  xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="xml" indent="yes"/>

<xsl:key name="SPEC" match="tei:schemaSpec|tei:specGrp" use="1"/>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="text()|@*|comment()|processing-instruction()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="tei:text">
 <text>
  <body>
    <xsl:apply-templates select="key('SPEC',1)"/>
  </body>
 </text>
</xsl:template>

<xsl:template match="tei:elementSpec[@ident='egXML' and @mode='delete']"/>

<xsl:template match="tei:schemaSpec/@ident">
  <xsl:attribute name="ident">
    <xsl:value-of select="."/>
    <xsl:text>-examples</xsl:text>
  </xsl:attribute>
</xsl:template>

<xsl:template match="tei:schemaSpec/@ns"/>
<xsl:template match="tei:schemaSpec/@start"/>

<xsl:template match="tei:schemaSpec">
  <xsl:copy>
    <xsl:apply-templates select="@*"/>
    <xsl:attribute name="start">egXML</xsl:attribute>
    <xsl:attribute name="ns">
      <xsl:text>http://www.tei-c.org/ns/Examples</xsl:text>
    </xsl:attribute>
    <xsl:apply-templates
	select="*|processing-instruction()|comment()|text()"/>  
    <xsl:choose>
      <xsl:when
	  test="//tei:moduleRef[@key='tagdocs']"/>
      <xsl:when
	  test="//tei:elementRef[@key='egXML']"/>
      <xsl:when
	  test="//tei:elementSpec[@ident='egXML']"/>
      <xsl:otherwise>
	<elementRef key="egXML"/>
      </xsl:otherwise>
    </xsl:choose>
    <elementSpec ident="egXML" mode="change" ns="http://www.tei-c.org/ns/Examples">
      <content>
	<oneOrMore xmlns="http://relaxng.org/ns/structure/1.0">
	  <choice>
	    <text/>
	    <ref name="macro.anyElementDefined"/>
	  </choice>
	</oneOrMore>
      </content>
    </elementSpec>
    
    <macroSpec ident="macro.anyElementDefined" mode="add">
      <content>
	<xsl:processing-instruction name="NameList"/>
      </content>
    </macroSpec>
    
  </xsl:copy>
</xsl:template>
</xsl:stylesheet>



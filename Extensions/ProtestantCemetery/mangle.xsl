<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
>
<xsl:output method="xml" indent="yes" encoding="utf-8"/>

<xsl:key name="IDS" match="tei:*" use="@xml:id"/>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="comment()|processing-instruction()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="tei:stoneDescription">
 <stoneDescription xmlns="http://www.tei-c.org/ns/1.0">
   <stoneIdentifier xmlns="http://www.tei-c.org/ns/1.0">
     <settlement xmlns="http://www.tei-c.org/ns/1.0">Protestant Cemetery</settlement>
     <idno xmlns="http://www.tei-c.org/ns/1.0">
       <xsl:value-of select="ancestor::tei:TEI/@xml:id"/>
     </idno>
   </stoneIdentifier>
   <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"/>
 </stoneDescription>
</xsl:template>

<xsl:template match="tei:location"/>

<xsl:template match="tei:numElements"/>

<xsl:template match="tei:faces"/>

<xsl:template match="tei:physicalDescription">
  <physDesc xmlns="http://www.tei-c.org/ns/1.0">
  <objectDesc xmlns="http://www.tei-c.org/ns/1.0">
    <supportDesc xmlns="http://www.tei-c.org/ns/1.0">
    <p xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:value-of select="key('IDS',substring-after(tei:form/@target,'#'))/tei:catDesc"/>
    </p>
      <p xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates select="tei:material"/>
	<xsl:apply-templates select="tei:dimensions"/>
      </p>
      <xsl:apply-templates select="tei:condition"/>
    </supportDesc>
  </objectDesc>
  </physDesc>
</xsl:template>

<xsl:template match="tei:feature"/>
 <xsl:template match="tei:taxonomy[@xml:id='Condition']"/>
 <xsl:template match="tei:taxonomy[@xml:id='Form']"/>
 <xsl:template match="tei:taxonomy[@xml:id='Material']"/>
 <xsl:template match="tei:taxonomy[@xml:id='Feature']"/>

<xsl:template match="tei:condition">
  <p xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:value-of select="key('IDS',substring-after(@target,'#'))/tei:catDesc"/>
  </p>
</xsl:template>

<xsl:template match="tei:material">
  <xsl:copy>
    <xsl:value-of select="key('IDS',substring-after(@target,'#'))/tei:catDesc"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:photo">
  	<p xmlns="http://www.tei-c.org/ns/1.0">
	  <xsl:text>Photo </xsl:text>
	  <xsl:value-of select="@film"/>-<xsl:value-of select="@neg"/>
	</p>
</xsl:template>

<xsl:template match="tei:feature"/>

<xsl:template match="tei:p[.='Generated from database records originally']"/>

<xsl:template match="tei:person">
  <xsl:copy>
   <xsl:attribute name="sex"> 
  <xsl:choose>
    <xsl:when test="@sex='m'">1</xsl:when>
    <xsl:when test="@sex='f'">2</xsl:when>
    <xsl:when test="@sex='u'">0</xsl:when>
  </xsl:choose>
   </xsl:attribute>
   <xsl:copy-of select="@age"/>
   <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:breadth">
  <xsl:element name="depth" xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="tei:respStmt">
  <xsl:copy>
    <resp xmlns="http://www.tei-c.org/ns/1.0">compiler</resp>
    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:diam">
  <xsl:element name="width" xmlns="http://www.tei-c.org/ns/1.0">
    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="tei:history">
  <xsl:element name="additional" xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>

<xsl:template match="tei:death/@value">
  <xsl:attribute name="date">
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template match="tei:birth/@value">
  <xsl:attribute name="date">
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template match="tei:foreName">
  <xsl:element name="forename" xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:element>
</xsl:template>


</xsl:stylesheet>

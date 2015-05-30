<xsl:stylesheet version="2.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" >

  <xsl:key name="V" match="teix:egXML[@valid='true' or not(@valid)]" use="1"/>
  <xsl:key name="F" match="teix:egXML[@valid='feasible']" use="1"/>
  <xsl:output omit-xml-declaration="yes"/>
  <xsl:template match="/">
    <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE p [</xsl:text>
    <xsl:for-each select="key('V',1)">
      <xsl:variable name="N">
	<xsl:call-template name="loc"/>
      </xsl:variable>
      <xsl:text disable-output-escaping="yes">&lt;!ENTITY </xsl:text>
      <xsl:value-of select="$N"/>
      <xsl:text> SYSTEM "valid/</xsl:text>
      <xsl:value-of select="$N"/>
      <xsl:text disable-output-escaping="yes">"&gt;&#10;</xsl:text>
    </xsl:for-each>
    <xsl:text disable-output-escaping="yes">]&gt;</xsl:text>
    <TEI 
	xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
    <fileDesc>
      <titleStmt>
        <title>The title</title>
      </titleStmt>
      <editionStmt>
        <p/>
      </editionStmt>
      <publicationStmt>
        <p/>
      </publicationStmt>
      <sourceDesc>
        <p/>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p>
    <xsl:for-each select="key('V',1)">
      <xsl:variable name="N">
	<xsl:call-template name="loc"/>
      </xsl:variable>
      <xsl:result-document href="valid/{$N}">
	<xsl:copy-of select="."/>
      </xsl:result-document>
      <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
      <xsl:value-of select="$N"/>
      <xsl:text>;</xsl:text>
    </xsl:for-each>
</p>
    </body>
  </text>
</TEI>
  </xsl:template>

  <xsl:template name="loc">
    <xsl:for-each select="ancestor::tei:*|ancestor-or-self::teix:*">
      <xsl:value-of select="name(.)"/>
      <xsl:text>_</xsl:text>
      <xsl:choose>
	<xsl:when test="@ident">
	  <xsl:text></xsl:text>
	  <xsl:value-of select="replace(@ident,':','')"/>
	  <xsl:text></xsl:text>
	</xsl:when>
	<xsl:when test="@xml:id">
	  <xsl:text></xsl:text>
	  <xsl:value-of select="replace(@xml:id,':','')"/>
	  <xsl:text></xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:number/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>-</xsl:text>
    </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
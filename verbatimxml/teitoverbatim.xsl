<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    version="2.0">

  <xsl:template match="/">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <teiHeader>
	<fileDesc>
	  <xsl:copy-of select="/*/teiHeader/titleStmt"/>
	  <publicationStmt>
            <p/>
	  </publicationStmt>
	  <sourceDesc>
            <p>ab initio</p>
	  </sourceDesc>
	</fileDesc>
      </teiHeader>
      <text>
	<body>
	  <egXML xmlns="http://www.tei-c.org/ns/Examples">
	    <xsl:apply-templates mode="verbatim"/>
	  </egXML>
	</body>
      </text>
    </TEI>
  </xsl:template>

  <xsl:template match="@*|text()|comment" mode="verbatim">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="verbatim">
    <xsl:element name="{local-name()}"
	     xmlns="http://www.tei-c.org/ns/Examples">
      <xsl:apply-templates select="@*|*|text()|comment()" mode="verbatim"/>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>

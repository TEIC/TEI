<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    exclude-result-prefixes="tei tbx">

<xsl:key name="T" match="tbx:termEntry" use="1"/>

<xsl:output method="xml" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
  <martif type="TBX" xml:lang="en">
    <martifHeader>
        <fileDesc>
            <sourceDesc>
                <p>from an ISO standard</p>
            </sourceDesc>
        </fileDesc>
        <encodingDesc>
            <p type="XCSURI">http://www.lisa.org/fileadmin/standards/tbx/TBXXCSV02.XCS</p>
        </encodingDesc>
    </martifHeader>
    <text>
      <body>
	<xsl:for-each select="key('T',1)">
	  <xsl:copy>
	    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	  </xsl:copy>
	</xsl:for-each>
      </body>
    </text>
  </martif>
</xsl:template>


   <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
   </xsl:template>


   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="tei:*">
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
   </xsl:template>


</xsl:stylesheet>





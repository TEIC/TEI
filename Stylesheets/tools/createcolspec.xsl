<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
        xmlns:fo="http://www.w3.org/1999/XSL/Format">

<xsl:import href="tei-makecolspec.xsl"/>
<xsl:variable name="top" select="/"/>
<xsl:variable name="readColSpecFile"></xsl:variable>
<xsl:variable name="processor">
   <xsl:value-of select="system-property('xsl:vendor')"/>
</xsl:variable>


<xsl:output method="xml"
              indent="yes"/>

<xsl:template match="/" >
<Info>
 <xsl:for-each select=".//table">
 <xsl:variable name="no">
  <xsl:call-template name="generateTableID"/> 
 </xsl:variable>
 <TableSpec id="{$no}">
   <xsl:call-template name="deriveColSpecs"/>
 </TableSpec>
 </xsl:for-each>
</Info>
</xsl:template>

</xsl:stylesheet>

<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

#include LICENSE
-->
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
   xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >
<!-- paragraphs -->
 <xsl:template match="tei:p">
  <fo:block font-size="{$bodySize}">
     <xsl:if test="preceding-sibling::tei:p">
	<xsl:attribute name="text-indent">
              <xsl:value-of select="$parIndent"/>
        </xsl:attribute>
	<xsl:attribute name="space-before.optimum">
              <xsl:value-of select="$parSkip"/>
        </xsl:attribute>
	<xsl:attribute name="space-before.maximum">
              <xsl:value-of select="$parSkipmax"/>
        </xsl:attribute>
     </xsl:if>
 <xsl:if test="@xml:lang">
   <xsl:attribute name="country">
     <xsl:value-of select="substring-before(@xml:lang,'-')"/>
   </xsl:attribute>
   <xsl:attribute name="language">
     <xsl:value-of select="substring-after(@xml:lang,'-')"/>
   </xsl:attribute>
 </xsl:if>
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>
</xsl:stylesheet>

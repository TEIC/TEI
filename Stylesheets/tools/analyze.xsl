<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:XSL="http://www.w3.org/1999/XSL/Transform"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
   <xsl:output indent="yes" omit-xml-declaration="yes" encoding="utf-8"/>
   <xsl:param name="file"/>
   <xsl:template match="XSL:stylesheet">
      <params>
         <xsl:for-each select="XSL:param">
            <param file="{$file}" name="{@name}">
               <xsl:apply-templates/>
            </param>
         </xsl:for-each>
      </params>
   </xsl:template>


</xsl:stylesheet>
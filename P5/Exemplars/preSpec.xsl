<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:rng="http://relaxng.org/ns/structure/1.0"
    exclude-result-prefixes="rng" version="2.0">

   <xsl:template match="/">
       <div type="elements" ><list>
           <xsl:apply-templates select="//rng:element"/>
       </list></div>
   </xsl:template>
   
  <xsl:template match="rng:element">
     <item><xsl:value-of select="@name"/></item>

    </xsl:template>
</xsl:stylesheet>

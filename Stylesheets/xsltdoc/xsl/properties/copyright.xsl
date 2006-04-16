<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xd="http://www.pnp-software.com/XSLTdoc"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="#all"
                version="2.0">
  <xd:doc type="stylesheet">
    Stylesheet for xd:copyright property.
    <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id: copyright.xsl,v 1.2 2005/01/03 10:07:35 ibirrer Exp $</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>
               
  <xd:doc>Prints the xd:copyright property.</xd:doc>
  <xsl:template match="xd:copyright" mode="printProperty"> 
    <div class="property">
      <div class="propertyCaption">Copyright:</div>
      <div class="propertyContent"><xsl:value-of select="."/></div>
    </div>
  </xsl:template>
</xsl:stylesheet>
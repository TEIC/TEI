<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet exclude-result-prefixes="#all" version="2.0" xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet"> Stylesheet for xd:cvsId property. <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id: cvsId.xsl,v 1.2 2005/01/03 10:07:35 ibirrer Exp $</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>

  <xd:doc>Prints the xd:cvsId property.</xd:doc>
  <xsl:template match="xd:cvsId" mode="printProperty">
    <div class="property">
      <div class="propertyCaption">CVS Id:</div>
      <div class="propertyContent">
        <xsl:value-of select="substring(., 5, string-length(.) - 5 )"/>
      </div>
    </div>
  </xsl:template>
</xsl:stylesheet>

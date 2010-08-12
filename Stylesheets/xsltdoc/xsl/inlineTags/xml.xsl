<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet exclude-result-prefixes="#all" version="2.0" xmlns="http://www.w3.org/1999/xhtml"
  xmlns:util="http://www.pnp-software.com/util" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet"> Stylesheet to for the inline tag: xd:link. <xd:author>ibirrer</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2004, P&amp;P Software GmbH</xd:copyright>
  </xd:doc>

  <xd:doc>Converts a xd:link element to a html link.</xd:doc>
  <xsl:template match="xd:xml" mode="XdocTags">
    <div class="source">
      <pre><xsl:copy-of select="util:xmlToHtml(node())"/></pre>
    </div>
  </xsl:template>
</xsl:stylesheet>

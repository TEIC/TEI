<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0"
    xmlns="http://www.w3.org/1999/xhtml"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:template match="front|body|back">
    <div class="tei_{local-name()}">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

</xsl:stylesheet>
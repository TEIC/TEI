<xsl:stylesheet version="1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!-- Extracts all the *Spec elements, and only the -->
  <!-- *Spec elements -->

  <xsl:template match="/">
    <tei:TEI>
      <xsl:copy-of select=".//tei:elementSpec"/>
      <xsl:copy-of select=".//tei:macroSpec"/>
      <xsl:copy-of select=".//tei:classSpec"/>
      <xsl:copy-of select=".//tei:moduleSpec"/>
    </tei:TEI>
  </xsl:template>

</xsl:stylesheet>

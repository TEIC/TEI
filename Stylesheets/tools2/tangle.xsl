<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:fotex="http://www.tug.org/fotex"
                xmlns:edate="http://exslt.org/dates-and-times"
                xmlns:estr="http://exslt.org/strings"
                xmlns:exsl="http://exslt.org/common"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                
                xmlns:XSL="http://www.w3.org/1999/XSL/Transform"
                extension-element-prefixes="exsl estr edate"
                exclude-result-prefixes="exsl edate a fo rng tei teix fotex m html"
                version="1.0">
   <xsl:output indent="yes" encoding="utf-8"/>

   <xsl:template match="XSL:stylesheet">
      <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:copy-of select="XSL:*"/>
      </xsl:copy>
   </xsl:template>

</xsl:stylesheet>
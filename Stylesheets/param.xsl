<XSL:stylesheet 
     xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
     xmlns:xmlns="foo"
     version="1.0">
<XSL:output indent="yes" omit-xml-declaration="yes"/>

<XSL:template match="/">
 <XSL:element name="xsl:stylesheet">
 <XSL:attribute name="version">1.0</XSL:attribute>
   <XSL:apply-templates select=".//var"/>
 </XSL:element>
</XSL:template>

<XSL:template match="var">
 <XSL:choose>
  <XSL:when test="@method='template'">
   <XSL:element name="xsl:template">
    <XSL:attribute name="name"><XSL:value-of select="@name"/></XSL:attribute>
     <XSL:value-of disable-output-escaping="yes" select="default"/>
   </XSL:element>
  </XSL:when>
  <XSL:when test="@method='param'">
   <XSL:element name="xsl:param">
    <XSL:attribute name="name"><XSL:value-of select="@name"/></XSL:attribute>
     <XSL:value-of disable-output-escaping="yes" select="default"/>
   </XSL:element>
  </XSL:when>
  <XSL:otherwise>
   <XSL:element name="xsl:variable">
    <XSL:attribute name="name"><XSL:value-of select="@name"/></XSL:attribute>
         <XSL:value-of select="default"/>
   </XSL:element>
  </XSL:otherwise>
 </XSL:choose>
</XSL:template>

</XSL:stylesheet>


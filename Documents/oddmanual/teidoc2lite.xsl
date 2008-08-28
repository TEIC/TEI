<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    exclude-result-prefixes="xsl tei" 
    version="1.0"
>

<xsl:output method="xml" indent="yes"/>

<xsl:template match="*|@*|processing-instruction()">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()" mode="example">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="*" mode="example">
 <xsl:element name="{local-name()}">
   <xsl:copy-of select="@*"/>
  <xsl:apply-templates
      select="*|processing-instruction()|comment()|text()" mode="example"/>
 </xsl:element>
</xsl:template>

<xsl:template match="text()">
    <xsl:value-of select="."/> <!-- could normalize() here -->
</xsl:template>

<xsl:template match="text()" mode="example">
    <xsl:value-of select="."/> <!-- could normalize() here -->
</xsl:template>

 <xsl:template match="tei:output">
   <tei:eg rend="output">
     <xsl:apply-templates/>
   </tei:eg>
 </xsl:template>

 <xsl:template match="tei:input">
   <egXML xmlns="http://www.tei-c.org/ns/Examples">
     <xsl:apply-templates mode="example"/>
   </egXML>
 </xsl:template>

 <xsl:template match="tei:software">
   <tei:hi rend="software">
     <xsl:apply-templates/>
   </tei:hi>
 </xsl:template>
 
 <xsl:template match="tei:command">
   <tei:hi rend="command">
     <xsl:apply-templates/>
   </tei:hi>
 </xsl:template>

 <xsl:template match="tei:path">
   <tei:hi rend="path">
     <xsl:apply-templates/>
   </tei:hi>
 </xsl:template>
 
 <xsl:template match="tei:url">
   <tei:hi rend="url">
     <xsl:apply-templates/>
   </tei:hi>
 </xsl:template>
 
 <xsl:template match="tei:kw">
   <tei:hi rend="kw">
     <xsl:apply-templates/>
   </tei:hi>
 </xsl:template>
 
 <xsl:template match="tei:value">
   <tei:ident>
     <xsl:apply-templates/>
   </tei:ident>
 </xsl:template>
 
 <xsl:template match="tei:glossList">
   <tei:list type="gloss">
     <xsl:apply-templates/>
   </tei:list>

 </xsl:template>

 <xsl:template match="tei:oList">
   <tei:list type="ordered">
     <xsl:apply-templates/>
   </tei:list>
 </xsl:template>

 <xsl:template match="tei:uList">
   <tei:list type="unordered">
     <xsl:apply-templates/>
   </tei:list>
 </xsl:template>

</xsl:stylesheet>
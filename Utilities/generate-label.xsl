<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:key name="MNAMES"
	 match="tei:monogr/tei:author[tei:surname]|tei:monogr/tei:editor[tei:surname]" 
	 use ="ancestor::tei:biblStruct/@xml:id"/>
<xsl:key name="ANAMES"
	 match="tei:analytic/tei:author[tei:surname]|tei:analytic/tei:editor[tei:surname]" 
	 use ="ancestor::tei:biblStruct/@xml:id"/>

<xsl:template match="/">
  <xsl:for-each select=".//tei:biblStruct">
    <xsl:sort select=".//tei:*[1]"/>
    <xsl:sort select=".//tei:date[1]"/>
    <xsl:text>
</xsl:text>
 <xsl:choose>
   <xsl:when test="not(tei:monogr or tei:analytic)">
     <xsl:message terminate="yes">no monogr or analytic for <xsl:value-of select="@id"/>
     </xsl:message>
   </xsl:when>
   <xsl:when test="count(key('ANAMES',@xml:id))=1">
     <xsl:value-of select="key('ANAMES',@xml:id)/tei:surname"/>
   </xsl:when>
   <xsl:when test="count(key('ANAMES',@xml:id))=2">
     <xsl:value-of
	 select="key('ANAMES',@xml:id)[1]/tei:surname"/>
     <xsl:text> and </xsl:text>
     <xsl:value-of select="key('ANAMES',@xml:id)[2]/tei:surname"/>
   </xsl:when>
   <xsl:when test="count(key('ANAMES',@xml:id))&gt;2">
     <xsl:value-of
	 select="key('ANAMES',@xml:id)[1]/tei:surname"/>
     <xsl:text> et al.</xsl:text>
   </xsl:when>
   <xsl:when test="count(key('MNAMES',@xml:id))=1">
     <xsl:value-of select="key('MNAMES',@xml:id)/tei:surname"/>
   </xsl:when>
   <xsl:when test="count(key('MNAMES',@xml:id))=2">
     <xsl:value-of
	 select="key('MNAMES',@xml:id)[1]/tei:surname"/>
     <xsl:text> and </xsl:text>
     <xsl:value-of select="key('MNAMES',@xml:id)[2]/tei:surname"/>
   </xsl:when>
   <xsl:when test="count(key('MNAMES',@xml:id))&gt;2">
     <xsl:value-of
	 select="key('MNAMES',@xml:id)[1]/tei:surname"/>
     <xsl:text> et al.</xsl:text>
   </xsl:when>
   <xsl:when test=".//tei:author">
     <xsl:value-of select=".//tei:author[1]"/>
   </xsl:when>
   <xsl:otherwise>
     <emph>
     <xsl:value-of select=".//tei:title[1]"/>
     </emph>
   </xsl:otherwise>
 </xsl:choose>
 <xsl:choose>
 <xsl:when test="count(tei:monogr/tei:editor)=1">
   <xsl:text> (ed.)</xsl:text>
 </xsl:when>
 <xsl:when test="count(tei:monogr/tei:editor)&gt;1">
   <xsl:text> (eds.)</xsl:text>
 </xsl:when>
 </xsl:choose>
 <xsl:if test="tei:monogr/tei:imprint/tei:date">
   <xsl:text> (</xsl:text>
   <xsl:value-of select="tei:monogr/tei:imprint/tei:date"/>
   <xsl:text>)</xsl:text>
 </xsl:if>
</xsl:for-each>
</xsl:template>

</xsl:stylesheet>



<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
##LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">

<xsl:template match="tei:table[@rend='simple']">
  <table>
 <xsl:if test="@rend">
     <xsl:attribute name="class"><xsl:value-of
     select="@rend"/></xsl:attribute>
   </xsl:if>
 <xsl:for-each select="@*">
   <xsl:if test="name(.)='summary'
		 or name(.) = 'width'
		 or name(.) = 'border'
		 or name(.) = 'frame'
		 or name(.) = 'rules'
		 or name(.) = 'cellspacing'
		 or name(.) = 'cellpadding'">
     <xsl:copy-of select="."/>
   </xsl:if>
 </xsl:for-each>
 <xsl:call-template name="makeAnchor"/>
  <xsl:apply-templates/></table>
</xsl:template>

<xsl:template match='tei:table'>
 <div>
 <xsl:attribute name="align">
 <xsl:choose>
  <xsl:when test="@align">
      <xsl:value-of select="@align"/>
  </xsl:when>
  <xsl:otherwise>
      <xsl:value-of select="$tableAlign"/>
  </xsl:otherwise>
 </xsl:choose>
 </xsl:attribute>
 <xsl:if test="tei:head">
   <p><xsl:apply-templates select="." mode="header"/></p>
 </xsl:if>
 <table>
   <xsl:if test="@rend">
     <xsl:attribute name="class"><xsl:value-of
     select="@rend"/></xsl:attribute>
   </xsl:if>
 <xsl:if test="@rend='frame' or @rend='rules'">
  <xsl:attribute name="rules">all</xsl:attribute>
  <xsl:attribute name="border">1</xsl:attribute>
 </xsl:if>
 <xsl:for-each select="@*">
  <xsl:if test="name(.)='summary'
or name(.) = 'width'
or name(.) = 'border'
or name(.) = 'frame'
or name(.) = 'rules'
or name(.) = 'cellspacing'
or name(.) = 'cellpadding'">
    <xsl:copy-of select="."/>
 </xsl:if>
 </xsl:for-each>
 <xsl:apply-templates/>
 </table>
 </div>
</xsl:template>

<xsl:template match='tei:row'>
 <tr>
<xsl:if test="@rend and starts-with(@rend,'class:')">
 <xsl:attribute name="class">
    <xsl:value-of select="substring-after(@rend,'class:')"/>
 </xsl:attribute>
</xsl:if>
<xsl:if test="@role">
 <xsl:attribute name="class"><xsl:value-of select="@role"/></xsl:attribute>
</xsl:if>
 <xsl:apply-templates/>
 </tr>
</xsl:template>

<xsl:template match='tei:cell'>
 <td valign="top">
   <xsl:for-each select="@*">
     <xsl:choose>
       <xsl:when test="name(.) = 'width'
		       or name(.) = 'border'
		       or name(.) = 'cellspacing'
		       or name(.) = 'cellpadding'">
	 <xsl:copy-of select="."/>
       </xsl:when>
       <xsl:when test="name(.)='rend' and starts-with(.,'width:')">
	 <xsl:attribute name="width">
	   <xsl:value-of select="substring-after(.,'width:')"/>
	 </xsl:attribute>
       </xsl:when>
       <xsl:when test="name(.)='rend' and starts-with(.,'class:')">
	 <xsl:attribute name="class">
	   <xsl:value-of select="substring-after(.,'class:')"/>
	 </xsl:attribute>
       </xsl:when>
       <xsl:when test="name(.)='rend'">
	 <xsl:attribute name="bgcolor"><xsl:value-of select="."/></xsl:attribute>
       </xsl:when>
       <xsl:when test="name(.)='cols'">
	 <xsl:attribute name="colspan"><xsl:value-of select="."/></xsl:attribute>
       </xsl:when>
       <xsl:when test="name(.)='rows'">
	 <xsl:attribute name="rowspan"><xsl:value-of select="."/></xsl:attribute>
       </xsl:when>
       <xsl:when test="name(.)='align'">
	 <xsl:attribute name="align"><xsl:value-of select="."/></xsl:attribute>
       </xsl:when>
     </xsl:choose>
   </xsl:for-each>
   <xsl:if test="not(@align) and not($cellAlign='left')">
     <xsl:attribute name="align"><xsl:value-of select="$cellAlign"/></xsl:attribute>
   </xsl:if>
   
   <xsl:if test="@role">
     <xsl:attribute name="class"><xsl:value-of select="@role"/></xsl:attribute>
   </xsl:if>
   <xsl:if test="@id"><a name="{@id}"/></xsl:if>
   <xsl:apply-templates/>
 </td>
</xsl:template>



</xsl:stylesheet>

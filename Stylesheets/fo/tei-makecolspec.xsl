<?xml version="1.0" encoding="utf-8"?>
<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

##LICENSE
--> 
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fotex="http://www.tug.org/fotex"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="saxon exsl" 
  extension-element-prefixes="saxon exsl fotex"
  xmlns:saxon="http://icl.com/saxon"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0">


<xsl:template name="deriveColSpecs" >
<xsl:variable name="no">
 <xsl:call-template name="generateTableID"/>
</xsl:variable>


<xsl:choose>
<xsl:when test="$readColSpecFile">
<xsl:variable name="specs">
<xsl:value-of 
  select="count(exsl:node-set($tableSpecs)/Info/TableSpec[$no=@id])"/>
</xsl:variable>
<xsl:choose>
<xsl:when test="$specs &gt; 0">
    <xsl:for-each 
select="exsl:node-set($tableSpecs)/Info/TableSpec[$no=@id]/fo:table-column">
      <xsl:copy-of select="."/>
 </xsl:for-each>
</xsl:when>
<xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
 <xsl:call-template name="calculateTableSpecs"/>
</xsl:otherwise>
</xsl:choose>
</xsl:when>
<xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
 <xsl:call-template name="calculateTableSpecs"/>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="calculateTableSpecs">
<xsl:variable name="tds">
 <xsl:for-each select=".//tei:cell">
  <xsl:variable name="stuff">
     <xsl:apply-templates/>
  </xsl:variable>
   <cell>
    <xsl:attribute name="col"><xsl:number/></xsl:attribute>
    <xsl:value-of select="string-length($stuff)"/>
   </cell>
 </xsl:for-each>
</xsl:variable>
<xsl:variable name="total">
  <xsl:value-of select="sum(exsl:node-set($tds)/cell)"/>
</xsl:variable>
<xsl:for-each select="exsl:node-set($tds)/cell">
  <xsl:sort select="@col" data-type="number"/>
  <xsl:variable name="c" select="@col"/>
  <xsl:if test="not(preceding-sibling::tei:cell[$c=@col])">
   <xsl:variable name="len">
    <xsl:value-of select="sum(following-sibling::tei:cell[$c=@col]) + current()"/>
   </xsl:variable>
   <xsl:text>
 </xsl:text>
   <fo:table-column column-number="{@col}" fotex:column-align="L" column-width="{$len div $total * 100}%" />
  </xsl:if>
</xsl:for-each>
   <xsl:text>
</xsl:text>
</xsl:template>

<xsl:template name="generateTableID">
<xsl:choose>
 <xsl:when test="@xml:id">
   <xsl:value-of select="@xml:id"/>
 </xsl:when>
 <xsl:when test="@id">
   <xsl:value-of select="@id"/>
 </xsl:when>
 <xsl:otherwise>
   <xsl:text>Table-</xsl:text><xsl:number level='any'/>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>

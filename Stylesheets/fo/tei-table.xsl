<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fotex="http://www.tug.org/fotex"
  xmlns:exsl="http://exslt.org/common"
  exclude-result-prefixes="exsl"
  extension-element-prefixes="exsl"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"  >

<xsl:template match='tei:table' mode="xref">
  <xsl:if test="$xrefShowTitle">
     <xsl:value-of select="$tableWord"/>
     <xsl:text> </xsl:text>
   </xsl:if>
   <xsl:call-template name="calculateTableNumber"/>
</xsl:template>

<xsl:template match="tei:table">
<xsl:choose>
 <xsl:when test="@rend='eqnarray'">
   <fotex:eqnarray>
     <xsl:apply-templates select=".//tei:formula"/>
   </fotex:eqnarray>
 </xsl:when>
 <xsl:when test=".//tei:formula[@type='subeqn']">
   <fotex:eqnarray>
     <xsl:apply-templates select=".//tei:formula"/>
   </fotex:eqnarray>
 </xsl:when>
 <xsl:when test="$inlineTables or @rend='inline'">
  <xsl:if test="head">
     <fo:block>
   <xsl:call-template name="tableCaptionstyle"/>       
     <xsl:call-template name="addID"/>
     <xsl:if test="$makeTableCaption='true'">
     <xsl:value-of select="$tableWord"/>
     <xsl:call-template name="calculateTableNumber"/>
     <xsl:text>. </xsl:text>       
     </xsl:if>
     <xsl:apply-templates select="tei:head"/>       
     </fo:block>
  </xsl:if>
  <xsl:call-template name="blockTable"/>
 </xsl:when>
 <xsl:otherwise>
   <xsl:call-template name="floatTable"/>
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template name="floatTable">
 <fo:table-and-caption>
      <xsl:if test="rend='landscape'">
        <xsl:attribute name="reference-direction">-90</xsl:attribute>
      </xsl:if>
      <xsl:call-template name="addID"/>
   <fo:table-caption>
  <fo:block text-align="{$tableCaptionAlign}"
        space-after="{$spaceBelowCaption}">
     <xsl:value-of select="$tableWord"/>
     <xsl:call-template name="calculateTableNumber"/>
     <xsl:text>. </xsl:text>
     <xsl:apply-templates select="tei:head"/>
  </fo:block>
   </fo:table-caption>
   <xsl:call-template name="blockTable"/>
 </fo:table-and-caption>
</xsl:template>

<xsl:template name="blockTable">
 <fo:table  text-align="{$tableAlign}"
	     font-size="{$tableSize}">
   <xsl:call-template name="addID"/>
   <xsl:call-template name="deriveColSpecs"/>
   <fo:table-body text-indent="0pt">
     <xsl:for-each select="tei:row">
       <xsl:text>
</xsl:text>
       <fo:table-row>
         <xsl:apply-templates select="tei:cell"/>
       </fo:table-row>
      </xsl:for-each>
   </fo:table-body>
 </fo:table>
</xsl:template>

<xsl:template match="tei:cell">
  <fo:table-cell>
  <xsl:if test="@cols &gt; 1">
    <xsl:attribute name="number-columns-spanned">
      <xsl:value-of select="@cols"/>
    </xsl:attribute>
  </xsl:if>
  <xsl:if test="@rows &gt; 1">
    <xsl:attribute name="number-rows-spanned">
      <xsl:value-of select="@rows"/>
    </xsl:attribute>
  </xsl:if>
  <xsl:call-template name="cellProperties"/>
 <fo:block>
  <xsl:choose>
   <xsl:when test="@role='label' or parent::tei:row[@role='label']">
     <xsl:attribute name="font-weight">bold</xsl:attribute>
   </xsl:when>
  </xsl:choose>
     <xsl:apply-templates/>
 </fo:block>
</fo:table-cell>
</xsl:template>

<xsl:template name="cellProperties" >
  <xsl:if test="@role='hi' or @role='label' or parent::tei:row/@role='label'">
    <xsl:attribute name="background-color">silver</xsl:attribute>
  </xsl:if>
  <xsl:choose>
  <xsl:when test="ancestor::tei:table[1][@rend='frame']">
   <xsl:if test="not(parent::tei:row/preceding-sibling::tei:row)">
    <xsl:attribute name="border-before-style">solid</xsl:attribute>
   </xsl:if>
   <xsl:attribute name="border-after-style">solid</xsl:attribute>
   <xsl:if test="not(following-sibling::tei:cell)">
     <xsl:attribute name="border-end-style">solid</xsl:attribute>
   </xsl:if>
   <xsl:attribute name="border-start-style">solid</xsl:attribute>
  </xsl:when>
  <xsl:otherwise>
  </xsl:otherwise>
  </xsl:choose>
  <xsl:if test="not(ancestor::tei:table/@rend='tight')">   
  <xsl:attribute name="padding">
     <xsl:value-of select="$tableCellPadding"/>
  </xsl:attribute>
  </xsl:if>
  <xsl:choose>
  <xsl:when test="@halign">
    <xsl:attribute name="text-align">
       <xsl:value-of select="@halign"/>
    </xsl:attribute>
  </xsl:when>
  <xsl:otherwise>
    <xsl:variable name="thiscol">
       <xsl:value-of select="position()"/>
    </xsl:variable>
    <xsl:variable name="tid"><xsl:value-of select="ancestor::tei:table/@id"/></xsl:variable>
    <xsl:variable name="align">
    <xsl:value-of select="exsl:node-set($tableSpecs)/Info/TableSpec[@id=$tid]/fo:table-column[@column-number=$thiscol]/@fotex:column-align"/>
    </xsl:variable>
    <!--
    <xsl:message>    Cell: whats my position: <xsl:value-of select="$thiscol"/>, <xsl:value-of select="$align"/>, <xsl:value-of select="$tid"/>
</xsl:message>
-->
    <xsl:choose>
          <xsl:when test="$align='R'">
                <xsl:attribute name="text-align">right</xsl:attribute>
          </xsl:when>
          <xsl:when test="$align='L'">
                <xsl:attribute name="text-align">left</xsl:attribute>
          </xsl:when>
          <xsl:when test="$align='C'">
                <xsl:attribute name="text-align">center</xsl:attribute>
          </xsl:when>
          <xsl:when test="not($align='')">
	       <xsl:attribute name="text-align">
                   <xsl:value-of select="$align"/>
               </xsl:attribute>
	  </xsl:when>
          <xsl:otherwise>
                <xsl:attribute name="text-align">left</xsl:attribute>
          </xsl:otherwise>
    </xsl:choose>

  </xsl:otherwise>
  </xsl:choose>

</xsl:template>

<xsl:template name="calculateTableNumber">
     <xsl:number  from="text" level="any"/>
</xsl:template>

</xsl:stylesheet>

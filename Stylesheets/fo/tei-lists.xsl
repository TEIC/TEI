<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fotex="http://www.tug.org/fotex"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  >

<!-- lists, what joy -->

<xsl:template match="tei:list[@type='catalogue']">
  <fo:block
        space-before="{$spaceAroundTable}"
        space-after="{$spaceAroundTable}">
 <fo:table>
   <fo:table-column column-number="1" fotex:column-align="p" column-width="20%"/>
   <fo:table-column column-number="2" fotex:column-align="p" column-width="80%"/>
   <fo:table-body>
     <xsl:for-each select="tei:item">
       <fo:table-row>
         <xsl:apply-templates select="." mode="catalogue"/>
       </fo:table-row>
      </xsl:for-each>
   </fo:table-body>
 </fo:table>
  </fo:block>
</xsl:template>

<xsl:template match="tei:item" mode="catalogue">
<fo:table-cell><fo:block>
         <xsl:choose>
         <xsl:when test="tei:label">
            <fo:inline font-weight='bold'>
                 <xsl:apply-templates select="tei:label" mode="print"/>
            </fo:inline>
         </xsl:when>
         <xsl:otherwise>
          <fo:inline font-weight='bold'>
          <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
          </fo:inline>
         </xsl:otherwise>
         </xsl:choose>
</fo:block>
</fo:table-cell>
<fo:table-cell><fo:block><xsl:apply-templates/></fo:block></fo:table-cell>
</xsl:template>

<xsl:template match="tei:list">
 <xsl:if test="child::tei:head">
  <fo:block font-style="italic" 
	text-align="start"
	space-before.optimum="4pt">
    <xsl:apply-templates select="tei:head"/>
  </fo:block>
 </xsl:if>
 <fo:list-block   margin-right="{$listRightMargin}"
		  provisional-distance-between-starts="10mm"
		  provisional-label-separation="2mm"
		  >
  <xsl:call-template name="setListIndents"/>
  <xsl:choose>
   <xsl:when test="@type='gloss'">
    <xsl:attribute name="margin-left">
     <xsl:choose>
     <xsl:when test="ancestor::tei:list">
         <xsl:value-of select="$listLeftGlossInnerIndent"/>
     </xsl:when>
     <xsl:otherwise>
         <xsl:value-of select="$listLeftGlossIndent"/>
     </xsl:otherwise>
     </xsl:choose>
    </xsl:attribute>
  </xsl:when>
   <xsl:otherwise>
     <xsl:attribute name="margin-left">
         <xsl:value-of select="$listLeftIndent"/></xsl:attribute>
   </xsl:otherwise> 
  </xsl:choose>
  <xsl:apply-templates select="tei:item"/>
  </fo:list-block>
</xsl:template>

<xsl:template match="tei:item">
     <xsl:call-template name="makeItem"/>
</xsl:template>

<xsl:template name="makeItem">
<!-- item behaviour depends on the type attribute of our parent:
simple, bullets, ordered, gloss, unordered
-->
<xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
<fo:list-item>
  <xsl:if test="not(parent::tei:note[@place='foot'])">
    <xsl:attribute name="space-before.optimum">
      <xsl:value-of select="$listItemsep"/>
    </xsl:attribute>
  </xsl:if>
  <fo:list-item-label  end-indent="label-end()">
    <xsl:if test="@id">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@xml:id">
      <xsl:attribute name="id"><xsl:value-of select="@xml:id"/></xsl:attribute>
    </xsl:if>
    <fo:block>
      <xsl:attribute name="margin-right">2.5pt</xsl:attribute>
      <xsl:choose>
        <xsl:when test="@n">
          <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:value-of select="@n"/>
        </xsl:when>
        <xsl:when test="../@type='bibliography'">
          <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:apply-templates mode="xref" select="."/>
        </xsl:when>
        <xsl:when test="../@type='ordered'">
          <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:apply-templates mode="xref" select="."/>
          <xsl:text>.</xsl:text>
        </xsl:when>
        <xsl:when test="../@type='gloss'">
          <xsl:attribute name="text-align">start</xsl:attribute>
          <xsl:attribute name="font-weight">bold</xsl:attribute>
         <xsl:choose>
         <xsl:when test="tei:label">
          <xsl:apply-templates mode="print" select="tei:label"/>
         </xsl:when>
         <xsl:otherwise>
          <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
         </xsl:otherwise>
         </xsl:choose>
        </xsl:when>
        <xsl:when test="../@type='numbered'"> <!-- numbered support added rbl 26.3.2005 -->
          <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:number/>
	  <xsl:text>.</xsl:text>
        </xsl:when>
        <xsl:when test="../@type='ordered'"> <!-- numbered support added rbl 26.3.2005 -->
          <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:number/>
	  <xsl:text>.</xsl:text>
        </xsl:when>
        <xsl:otherwise>
           <xsl:attribute name="text-align">end</xsl:attribute>
          <xsl:choose>
            <xsl:when test="$listdepth=0">
              <xsl:value-of select="$bulletOne"/>
            </xsl:when>
            <xsl:when test="$listdepth=1">
              <xsl:value-of select="$bulletOne"/>
            </xsl:when>
            <xsl:when test="$listdepth=2">
              <xsl:value-of select="$bulletTwo"/>
            </xsl:when>
            <xsl:when test="$listdepth=3">
              <xsl:value-of select="$bulletThree"/>
            </xsl:when>
            <xsl:when test="$listdepth=4">
              <xsl:value-of select="$bulletFour"/>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
     </xsl:choose>
   </fo:block>
 </fo:list-item-label>
 <fo:list-item-body  start-indent="body-start()">
   <xsl:choose>
   <xsl:when test="tei:p">
     <xsl:apply-templates/>
   </xsl:when>
   <xsl:otherwise>
    <fo:block font-weight="normal"><xsl:apply-templates/></fo:block>
   </xsl:otherwise>
   </xsl:choose>
 </fo:list-item-body>
</fo:list-item>
</xsl:template>

<xsl:template mode="print" match="tei:label">
      <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:label"/>

<!--
<xsl:template match="tei:item/tei:label">
 <fo:inline font-weight='bold'><xsl:apply-templates/></fo:inline>
</xsl:template>
-->

<xsl:template match="tei:item" mode="xref">
<xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
<xsl:if test="parent::tei:list[@type='bibliography']">
  <xsl:text> [</xsl:text>
</xsl:if>
<xsl:variable name="listNFormat">
          <xsl:choose>
            <xsl:when test="$listdepth=1">
              <xsl:text>1</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=2">
              <xsl:text>i</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=3">
              <xsl:text>a</xsl:text>
            </xsl:when>
            <xsl:when test="$listdepth=4">
              <xsl:text>I</xsl:text>
            </xsl:when>
          </xsl:choose>
</xsl:variable>
<xsl:number format="{$listNFormat}"/>
<xsl:if test="parent::tei:list[@type='bibliography']">
  <xsl:text>]</xsl:text>
</xsl:if>
</xsl:template>

<xsl:template name="setListIndents">
 <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
          <xsl:choose>
            <xsl:when test="$listdepth=0">
               <xsl:attribute name="space-before">
                     <xsl:value-of select="$listAbove-1"/>
                </xsl:attribute>
               <xsl:attribute name="space-after">
                     <xsl:value-of select="$listBelow-1"/>
                </xsl:attribute>
            </xsl:when>
            <xsl:when test="$listdepth=1">
               <xsl:attribute name="space-before">
                     <xsl:value-of select="$listAbove-2"/>
                </xsl:attribute>
               <xsl:attribute name="space-after">
                     <xsl:value-of select="$listBelow-2"/>
                </xsl:attribute>
            </xsl:when>
            <xsl:when test="$listdepth=2">
               <xsl:attribute name="space-before">
                     <xsl:value-of select="$listAbove-3"/>
                </xsl:attribute>
               <xsl:attribute name="space-after">
                     <xsl:value-of select="$listBelow-3"/>
                </xsl:attribute>
            </xsl:when>
            <xsl:when test="$listdepth=3">
               <xsl:attribute name="space-before">
                     <xsl:value-of select="$listAbove-4"/>
                </xsl:attribute>
               <xsl:attribute name="space-after">
                     <xsl:value-of select="$listBelow-4"/>
                </xsl:attribute>
            </xsl:when>
          </xsl:choose>
</xsl:template>

</xsl:stylesheet>

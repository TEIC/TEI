<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL FO stylesheet to format TEI XML documents 

##LICENSE
-->

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:fotex="http://www.tug.org/tei:fotex"
  xmlns:m="http://www.w3.org/1998/Math/MathML" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

<xsl:template match="m:math">
 <m:math>
  <xsl:copy-of select="@*"/>
  <xsl:apply-templates mode="math"/>
 </m:math>
</xsl:template>

<xsl:template match="m:*|@*|comment()|processing-instruction()|text()" mode="math">
 <xsl:copy>
   <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="tei:formula">
  <fo:wrapper>
   <xsl:if test="@id">
    <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
   </xsl:if>
   <xsl:apply-templates/>
  </fo:wrapper>
</xsl:template>


<xsl:template match="tei:formula" mode="xref">
 <xsl:number/>
</xsl:template>

<xsl:template match="tei:formula[@type='subeqn']/m:math">
  <xsl:apply-templates mode="math"/>
</xsl:template>

<xsl:template match="tei:table[@rend='eqnarray']">
   <fotex:eqnarray>
     <xsl:for-each select="tei:row">
     <xsl:apply-templates select=".//tei:formula"/>
     <xsl:if test="following-sibling::tei:row">
       <!--        <fo:character character="&#x2028;"/>-->
       <xsl:processing-instruction name="xmltex">\\</xsl:processing-instruction>
     </xsl:if>
     </xsl:for-each>
   </fotex:eqnarray>
</xsl:template>


<xsl:template match="tei:formula[@type='display']/m:math">
 <m:math display="block">
  <xsl:copy-of select="@*"/>
  <xsl:apply-templates mode="math"/>
 </m:math>
</xsl:template>

</xsl:stylesheet>



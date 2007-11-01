<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    version="1.0">

 <xsl:key name="FILES"   match="dtdFrag[@file]"   use="@file"/>
 <xsl:key name="CONTIN"  match="dtdFrag[@contin]" use="@contin"/>
 <xsl:key name="IDS"     match="*[@id]"           use="@id"/>
 <xsl:key name="PARTS"   match="part"             use="name"/>
 <xsl:key name="CLAREFS" match="claDecl"          use="@classDoc"/>
 <xsl:key name="ENTREFS" match="entDecl"          use="@entDoc"/>
 <xsl:key name="TAGREFS" match="tagDecl"          use="@tagDoc"/>
 <xsl:key name="DTDREFS" match="dtdRef"           use="@dtdFrag"/>
<xsl:key name="E" match="tei:elementSpec" use="1"/>
<xsl:key name="E4" match="tagDoc" use="1"/>
<xsl:output method="text"/>
<xsl:template match="/">

    <xsl:for-each select="document('../Source/Guidelines/en/guidelines-en.xml')">
      <xsl:message>P5</xsl:message>
      <xsl:for-each select="key('E',1)">
	<xsl:sort select="@ident"/>
	<xsl:sort select="@module"/>
	<xsl:text>P5: </xsl:text>
	<xsl:value-of select="@ident"/>
	<xsl:text> | </xsl:text>
	<xsl:value-of select="@xml:id"/>
	<xsl:text> | </xsl:text>
	<xsl:value-of select="@module"/>
	<xsl:text>&#10;</xsl:text>
      </xsl:for-each>
    </xsl:for-each>

    <xsl:for-each select="document('../../../../P4/driver.xml')">
      <xsl:message>P4</xsl:message>
      <xsl:for-each select="key('E4',1)">
	<xsl:sort select="gi"/>
	<xsl:text>P4: </xsl:text>
	<xsl:value-of select="gi"/>
	<xsl:text> | </xsl:text>
	<xsl:value-of select="@id"/>
	<xsl:text> | </xsl:text>
	<xsl:apply-templates select="." mode="findfile"/>
	<xsl:text>&#10;</xsl:text>
      </xsl:for-each>
      
    </xsl:for-each>

</xsl:template>


<xsl:template match="entDoc|classDoc|tagDoc|dtdFrag" mode="findfile">
  <xsl:choose>
    <xsl:when test="@file">
      <xsl:text>Declared in file </xsl:text>
      <xsl:value-of select="@file"/>
      <xsl:text>; </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:choose>
      <xsl:when test="@contin">
         <xsl:apply-templates select="key('IDS',@contin)" mode="findfile"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:choose>
       <xsl:when test="count(key('DTDREFS',@id))&gt;0">
        <xsl:for-each select="key('DTDREFS',@id)">
         <xsl:apply-templates select=".." mode="findfile"/>
        </xsl:for-each>
       </xsl:when>
       <xsl:when test="count(key('TAGREFS',@id))&gt;0">
        <xsl:for-each select="key('TAGREFS',@id)">
         <xsl:apply-templates select=".." mode="findfile"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>NO FILE</xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
 </xsl:otherwise>
 </xsl:choose>
</xsl:template>


</xsl:stylesheet> 
<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"   version="2.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
   <xsl:import href="isotei-schema.xsl"/>
   <xsl:import href="isoutils.xsl"/>

   <xsl:param name="numberFormat">uk</xsl:param>
   <xsl:output method="text"/>

   <xsl:template match="/">
       <xsl:apply-templates mode="checkSchematron"/>
   </xsl:template>

   <xsl:template name="generateError">
     <xsl:param name="message"/>
     <xsl:text>&#10;&#10;ISO Error:&#10;</xsl:text>
     <xsl:text>   Clause: </xsl:text>
     <xsl:for-each select="ancestor::tei:div[1]">
       <xsl:choose>
	 <xsl:when test="ancestor::tei:front">
	   <xsl:number count="tei:div" from="tei:front" format="i" level="multiple"/>
	 </xsl:when>
	 <xsl:when test="ancestor::tei:body">
	   <xsl:number count="tei:div" from="tei:body" format="1" level="multiple"/>
	 </xsl:when>
	 <xsl:when test="ancestor::tei:back">
	   Annex <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
	 </xsl:when>
       </xsl:choose>
      <xsl:text> </xsl:text>
      <xsl:value-of select="tei:head"/>
     </xsl:for-each>
     <xsl:text>&#10;   Context: </xsl:text>
     <xsl:for-each select="parent::*">
       <xsl:call-template name="Identify"/>
     </xsl:for-each>
     <xsl:text>&#10;</xsl:text>
     <xsl:value-of select="$message"/>
   </xsl:template>

   <xsl:template name="copyMe"/>

   <xsl:template name="copyIt">
     <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
   </xsl:template>

   <xsl:template match="processing-instruction()[name()='ISOerror']"
		 mode="checkSchematron">
     <xsl:call-template name="generateError">
       <xsl:with-param name="message"><xsl:value-of
       select="."/></xsl:with-param>
     </xsl:call-template>
   </xsl:template>
</xsl:stylesheet>

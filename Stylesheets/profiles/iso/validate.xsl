<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"   version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
   <xsl:import href="isotei-schema.xsl"/>
   <xsl:param name="numberFormat">uk</xsl:param>
   <xsl:output method="text"/>

   <xsl:template match="/">
       <xsl:apply-templates mode="checkSchematron"/>
   </xsl:template>

   <xsl:template name="generateError">
     <xsl:param name="message"/>
     <xsl:message>ISO Error:  <xsl:value-of select="$message"/></xsl:message>
   </xsl:template>

   <xsl:template name="copyMe"/>

   <xsl:template name="copyIt">
     <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
   </xsl:template>

   <xsl:template match="processing-instruction()[name()='ISOerror']" mode="checkSchematron">
     <xsl:message>ISO Error:  <xsl:value-of select="."/></xsl:message>
   </xsl:template>
</xsl:stylesheet>

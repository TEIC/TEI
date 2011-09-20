<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

   <xsl:output encoding="utf-8" method="xml" indent="yes"/>

   <xsl:param name="newfile"/>
   <xsl:param name="newlang"/>
   <xsl:param name="overwrite">true</xsl:param>

   <xsl:key name="K" match="entry" use="key"/>

   <xsl:template match="@*|processing-instruction()|comment()|text()">
      <xsl:copy/>
   </xsl:template>

   <xsl:template match="text">
      <xsl:choose>
         <xsl:when test="not(@xml:lang) or not(@xml:lang=$newlang)">
            <xsl:copy-of select="."/>
         </xsl:when>
         <xsl:when test="$overwrite='false' and @xml:lang=$newlang">
            <xsl:copy-of select="."/>
         </xsl:when>
      </xsl:choose>

   </xsl:template>

   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="entry">
      <xsl:variable name="k" select="key"/>
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
         <xsl:for-each select="document($newfile)/i18n">
            <xsl:for-each select="key('K',$k)">
               <xsl:message>Go <xsl:value-of select="$k"/>
               </xsl:message>
	              <xsl:copy-of select="text[@xml:lang=$newlang]"/>
            </xsl:for-each>
         </xsl:for-each>
      </xsl:copy>

   </xsl:template>
</xsl:stylesheet>
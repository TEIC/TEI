<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns="http://www.tei-c.org/ns/1.0"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a t tei fo rng xs"
                version="2.0">
  <xsl:template match="tei:table[@rend='valList'         or @rend='attDef'        or @rend='attList'         or @rend='specDesc']">
      <list type="termlist">
         <xsl:apply-templates/>
      </list>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'      or @rend='valList'      or @rend='attDef'      or @rend='specDesc']/tei:row">
      <item>
         <xsl:apply-templates/>
      </item>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'       or @rend='specDesc'       or @rend='valList'       or @rend='attDef']/tei:row/tei:cell[1]">
      <xsl:choose>
         <xsl:when test="parent::tei:row/parent::tei:table[@rend='attList']">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[@rend='valList']">
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[@rend='specDesc']">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:otherwise>
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'        or @rend='valList'        or @rend='specDesc'        or @rend='attDef']/tei:row/tei:cell[2]">
      <c rend="tab">
         <xsl:text>	</xsl:text>
      </c>
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:index"/>
  <xsl:template match="processing-instruction()[name()='tex' and .='\ ']">
      <c xml:space="preserve"> </c>
  </xsl:template>
   <!-- identity transform -->
  <xsl:output method="xml" indent="yes"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
<?xml version="1.0" encoding="utf-8"?>
<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to process TEI documents using ODD markup

 
##LICENSE
--><xsl:stylesheet xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <xsl:output method="xml" indent="yes" encoding="utf-8"/>
  <xsl:param name="verbose"/>
  <xsl:param name="lang">es</xsl:param>
  <xsl:key name="ELEMENTS" match="element" use="@ident"/>
  <xsl:key name="ATTRIBUTES" match="attribute" use="@ident"/>
  <xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
  <xsl:param name="I18N">../i18n.xml</xsl:param>
  <xsl:variable name="i18n">
      <xsl:choose>
         <xsl:when test="$I18N=''">
            <xsl:value-of select="$TEISERVER"/>
            <xsl:text>i18n.xql</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$I18N"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:template match="/">
      <xsl:if test="$verbose='true'">
         <xsl:message>Translate database: <xsl:value-of select="$i18n"/>
         </xsl:message>
      </xsl:if>
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
      </xsl:copy>
  </xsl:template>
  <xsl:template match="@*|comment()|text()|processing-instruction()">
      <xsl:copy/>
  </xsl:template>
  <xsl:template match="tei:elementSpec">
      <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:variable name="me" select="@ident"/>
         <xsl:if test="not(tei:altIdent)">
            <xsl:for-each select="document($i18n)">
               <xsl:for-each select="key('ELEMENTS',$me)/equiv[@xml:lang=$lang][not(@value='')]">
                  <xsl:if test="$verbose='true'">
                     <xsl:message>
                        <xsl:value-of select="$me"/> ... <xsl:value-of select="@value"/>
                     </xsl:message>
                  </xsl:if>
                  <altIdent xmlns="http://www.tei-c.org/ns/1.0" type="lang">
                     <xsl:value-of select="@value"/>
                  </altIdent>
               </xsl:for-each>
            </xsl:for-each>
         </xsl:if>
         <xsl:apply-templates select="*|text()|comment()|processing-instruction()"/>
      </xsl:copy>
  </xsl:template>
   <!--
  <xsl:template match="tei:elementSpec/tei:desc">
    <xsl:variable name="me" select="../@ident"/>
    <xsl:variable name="trans">
      <xsl:for-each select="document($i18n)">
        <xsl:for-each select="key('ELEMENTS',$me)/desc[@xml:lang=$lang]">
          <xsl:if test="not(.='.')">
            <xsl:if test="$verbose='true'">
              <xsl:message><xsl:value-of select="$me"/> ... <xsl:value-of select="."/></xsl:message>
            </xsl:if>
            <desc xmlns="http://www.tei-c.org/ns/1.0">
              <xsl:value-of select="."/>
            </desc>
          </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$trans=''">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$trans"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
-->
  <xsl:template match="tei:attDef">
      <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:variable name="me" select="@ident"/>
         <xsl:if test="not(tei:altIdent)">
            <xsl:for-each select="document($i18n)">
               <xsl:for-each select="key('ATTRIBUTES',$me)/equiv[@xml:lang=$lang][not(@value='')]">
                  <xsl:if test="$verbose='true'">
                     <xsl:message>
                        <xsl:value-of select="$me"/> ... <xsl:value-of select="@value"/>
                     </xsl:message>
                  </xsl:if>
                  <altIdent xmlns="http://www.tei-c.org/ns/1.0" type="lang">
                     <xsl:value-of select="@value"/>
                  </altIdent>
               </xsl:for-each>
            </xsl:for-each>
         </xsl:if>
         <xsl:apply-templates select="*|text()|comment()|processing-instruction()"/>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
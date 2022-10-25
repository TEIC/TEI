<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="xs"
  expand-text="yes"
  version="3.0">
  <xsl:output method="text"/>
  
  <xsl:variable name="languages" select="('de','es','fr','it','ja','ko','zh-TW')"/>
  
  <xsl:template match="/">
    <xsl:variable name="result">
      <map xmlns="http://www.w3.org/2005/xpath-functions">
        <array key="elements">
          <xsl:call-template name="spec"><xsl:with-param name="list" select="//elementSpec"/></xsl:call-template>
        </array>
        <array key="att_classes">
          <xsl:call-template name="spec"><xsl:with-param name="list" select="//classSpec[@type='atts']"/></xsl:call-template>
        </array>
        <array key="model_classes">
          <xsl:call-template name="spec"><xsl:with-param name="list" select="//classSpec[@type='model']"/></xsl:call-template>
        </array>
        <array key="macros">
          <xsl:call-template name="spec"><xsl:with-param name="list" select="//macroSpec"/></xsl:call-template>
        </array>
        <array key="datatypes">
          <xsl:call-template name="spec"><xsl:with-param name="list" select="//dataSpec"/></xsl:call-template>
        </array>
      </map>
    </xsl:variable>
    <xsl:sequence select="xml-to-json($result)"/>
  </xsl:template>
    
  <xsl:template name="spec">
    <xsl:param name="list"/>
      <xsl:for-each select="$list">
        <xsl:variable name="current" select="."/>
        <map xmlns="http://www.w3.org/2005/xpath-functions">
          <string key="id">{@ident}</string>
          <string key="URL">{@ident}.xml</string>
          <xsl:for-each select="$languages">
            <map key="{.}">
              <boolean key="out-of-date"><xsl:value-of select="exists($current//*[@xml:lang=current() and xs:date(@versionDate) lt xs:date((preceding-sibling::*[@xml:lang='en'][1]/@versionDate))])"/></boolean>
              <boolean key="missing"><xsl:value-of select="exists($current//(desc|gloss|remarks)[@xml:lang='en' and not(local-name() = following-sibling::*[@versionDate and @xml:lang=current()]/local-name(.))])"/></boolean>
            </map>
          </xsl:for-each>
        </map>
      </xsl:for-each>
  </xsl:template>
  
  
  
</xsl:stylesheet>
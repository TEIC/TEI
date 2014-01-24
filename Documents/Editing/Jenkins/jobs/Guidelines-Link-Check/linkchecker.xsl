<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    exclude-result-prefixes="xs xd"
    version="2.0">
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Jan 21, 2014</xd:p>
            <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
            <xd:p>Transforms a link-checker log into something human-readable for a Jenkins console output.</xd:p>
        </xd:desc>
    </xd:doc>
    
    <xsl:output method="text" indent="no"/>
  
<!-- We are currently using linkchecker's ignore parameter to handle the differences between
     alpha/beta and release versions, but this param mechanism may be extended at some point to
     do more sophisticated things. -->
  
    <xsl:param name="p5version"></xsl:param>
  <xsl:variable name="ignore" select="if (matches($p5version, '[a-z]+$')) then '((readme-\d\.\d\.\d\.html$)|(Guidelines\.mobi)|(tei-c\.org/$)|(google\.com/search))' else '((Guidelines\.mobi)|(tei-c\.org/$)|(google\.com/search))'"/>
    
    <xsl:template match="/">
        <xsl:text>Broken external links:&#x0a;&#x0a;</xsl:text>
        <xsl:for-each select="//urldata[extern='1']">
            <xsl:sort select="parent/text()"/>
            <xsl:choose>
                <xsl:when test="matches(url, $ignore)"></xsl:when>
                <xsl:when test="matches(valid/@result, '^((40)|(URLError))')">
                    <xsl:text>ERROR:  page: </xsl:text><xsl:value-of select="parent"/><xsl:text>&#x0a;         target: </xsl:text><xsl:value-of select="url"/>
                    <xsl:if test="url/text() ne realurl/text()"><xsl:text> (</xsl:text><xsl:value-of select="realurl"/><xsl:text>)</xsl:text></xsl:if>
                    <xsl:text>&#x0a;</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:for-each>
        
        <xsl:text>&#x0a;&#x0a;Broken internal links:&#x0a;&#x0a;</xsl:text>
        <xsl:for-each select="//urldata[extern='0']">
            <xsl:sort select="parent/text()"/>
            <xsl:choose>
              <xsl:when test="matches(url, $ignore)"></xsl:when>
                <xsl:when test="matches(valid/@result, '^((40)|(URLError))')">
                    <xsl:text>ERROR:  page: </xsl:text><xsl:value-of select="parent"/><xsl:text>&#x0a;         target: </xsl:text><xsl:value-of select="url"/>
                    <xsl:if test="url/text() ne realurl/text()"><xsl:text> (</xsl:text><xsl:value-of select="realurl"/><xsl:text>)</xsl:text></xsl:if>
                    <xsl:text>&#x0a;</xsl:text>
                </xsl:when>
            </xsl:choose>
        </xsl:for-each>
        
        <xsl:text>&#x0a;&#x0a;Warnings:&#x0a;&#x0a;</xsl:text>
        <xsl:for-each select="//urldata[warnings][not(matches(url, $ignore))]">
            <xsl:sort select="parent/text()"/>
                    <xsl:text>WARNING: page: </xsl:text><xsl:value-of select="parent"/><xsl:text>&#x0a;         target: </xsl:text><xsl:value-of select="url"/>
                    <xsl:if test="url/text() ne realurl/text()"><xsl:text> (</xsl:text><xsl:value-of select="realurl"/><xsl:text>)</xsl:text></xsl:if>
                    <xsl:text>&#x0a;         </xsl:text>
                    <xsl:value-of select="normalize-space(infos)"/>
                    <xsl:text>&#x0a;         </xsl:text>
                    <xsl:value-of select="normalize-space(warnings)"/>
                    <xsl:text>&#x0a;</xsl:text>
        </xsl:for-each>
    </xsl:template>
    
</xsl:stylesheet>
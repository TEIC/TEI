<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:rng="http://relaxng.org/ns/structure/1.0" 
    xmlns:xlink="http://www.w3.org/1999/xlink" 
    xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    exclude-result-prefixes="#all"
    version="2.0">
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Jan 2, 2017</xd:p>
            <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
            <xd:p>
                This file processes the result of a test process in X(HT)ML format,
                to do a number of things:
                
                1. Indent the output (replacing calls to xmllint --format in previous
                   test Makefile).
                   
                2. Remove all comments (replacing various PERL commands and scripts 
                   in previous test Makefile).
                   
                3. Check all internal links in XHTML files (replacing checklinks.xsl
                   in previous test Makefile).
                   
                4. Remove any timestamps that are generated during processing, since
                   these will be different each time a test is run.
            </xd:p>
        </xd:desc>
    </xd:doc>
    
    <xsl:output method="xml" encoding="UTF-8" normalization-form="NFC" indent="yes" omit-xml-declaration="yes"
    exclude-result-prefixes="#all"/>
    
<!--  Key for checking of internal links in HTML documents.  -->
    <xsl:key name="IDS" use="@id" match="*"/>
    
<!--  Checking of internal links in HTML documents.  -->
    <xsl:template match="xhtml:a[starts-with(@href,'#')]">
        <xsl:if test="not(key('IDS',substring(@href,2)))">
            <xsl:message terminate="yes">Error: no target for link <xsl:value-of select="@href"/></xsl:message>
        </xsl:if>
        <xsl:copy-of select="."/>
    </xsl:template>
    
<!--  We don't want any comments.  -->
    <xsl:template match="comment()" mode="#all"/>
    
<!--  We must remove the text contents of timestamps in docx core.xml files.  -->
    <xsl:template match="dcterms:modified">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
        </xsl:copy>
    </xsl:template>
    
<!--  Similarly, we remove timestamps from <date> elements in TEI files.  -->
    <xsl:template match="tei:change/tei:date[contains(., ':')]">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
        </xsl:copy>
    </xsl:template>
    
<!--  Default identity transform.  -->
    <xsl:template match="@* | node()" priority="-1" mode="#all">
        <xsl:copy>
            <xsl:apply-templates mode="#current" select="@*|node()"/>
        </xsl:copy>
    </xsl:template>
    
</xsl:stylesheet>
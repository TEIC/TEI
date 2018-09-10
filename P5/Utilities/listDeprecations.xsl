<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:math="http://www.w3.org/2005/xpath-functions/math"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl" exclude-result-prefixes="#all"
    xmlns="http://www.tei-c.org/ns/1.0" xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    version="3.0">
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Sep 10, 2018</xd:p>
            <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
            <xd:p>The purpose of this stylesheet is to parse through p5subset.xml and generate a TEI
                document containing a table which lists all the deprecations current at the time of
                running it.</xd:p>
        </xd:desc>
    </xd:doc>

    <xsl:output method="xml" encoding="UTF-8" normalization-form="NFC" indent="yes"/>

    <xd:doc scope="component">
        <xd:desc>This parameter provides the commit # at which the process was run. If not supplied,
            ignored.</xd:desc>
    </xd:doc>
    <xsl:param as="xs:string" name="commit"/>

    <xd:doc scope="component">
        <xd:desc>This parameter provides the TEI Guidelines release version number. If not supplied,
            ignored.</xd:desc>
    </xd:doc>
    <xsl:param as="xs:string" name="guidelinesRelease"/>

    <xd:doc>
        <xd:desc>This parameter is the date to appear on the page, and defaults to "now".</xd:desc>
    </xd:doc>
    <xsl:param name="nowDate" as="xs:string"
        select="format-date(current-date(), '[D1o] [MNn] [Y0001]')"/>

    <xsl:template match="/">
        <TEI version="5.0">
            <teiHeader>
                <fileDesc>
                    <titleStmt>
                        <title>Current deprecations</title>
                    </titleStmt>
                    <publicationStmt>
                        <p>Text Encoding Initiative Consortium generated file.</p>
                    </publicationStmt>
                    <sourceDesc>
                        <p>Generated document created during build process.</p>
                    </sourceDesc>
                </fileDesc>
            </teiHeader>
            <text>
                <body>
                    <head>Current deprecations</head>
                    <div>
                        <table>
                            <row role="label">
                                <cell>Identifier</cell>
                                <cell>Component type</cell>
                                <cell>Valid until</cell>
                            </row>
                            <xsl:for-each select="//*[@validUntil]">
                                <row>
                                    <cell>
                                        <xsl:choose>
                                            <xsl:when test="self::attDef">
                                                <xsl:value-of select="concat(ancestor::*[ends-with(local-name(), 'Spec')][1]/@ident, ' / ', ancestor::attDef/@ident, ' / ', @ident)"/>
                                            </xsl:when>
                                            <xsl:when test="self::valItem or self::valDesc">
                                                <xsl:value-of select="concat(ancestor::*[ends-with(local-name(), 'Spec')][1]/@ident, ' / ', @ident)"/>
                                            </xsl:when>
                                            <xsl:otherwise><xsl:value-of select="@ident"/></xsl:otherwise>
                                        </xsl:choose>
                                    </cell>
                                    <cell>
                                        <xsl:value-of select="local-name(.)"/>
                                    </cell>
                                    <cell>
                                        <xsl:value-of select="@validUntil"/>
                                        <xsl:if test="xs:date(@validUntil) lt current-date()">!!!</xsl:if>
                                    </cell>
                                </row>
                            </xsl:for-each>
                        </table>
                    </div>
                </body>
            </text>
        </TEI>
    </xsl:template>

</xsl:stylesheet>

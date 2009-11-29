<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:test="http://www.jenitennison.com/xslt/unit-test"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:s="http://www.jenitennison.com/xslt/xspec"
                xmlns:o="http://www.w3.org/1999/XSL/TransformAliasAlias"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:x="http://www.jenitennison.com/xslt/xspec"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns="http://www.tei-c.org/ns/1.0"
                version="2.0">
   <xsl:import href="file:/usr/local/bin/xspec-v0.1/generate-tests-utils.xsl"/>
   <xsl:import href="file:/Users/amittelbach/EDV%20Dienstleistungen/Kunden/OUCS/TEIISO/workspace/Stylesheets/docx/from/dynamic/toc.xsl"/>
   <xsl:namespace-alias stylesheet-prefix="o" result-prefix="xsl"/>
   <xsl:variable name="s:stylesheet-uri" as="xs:string"
                 select="'file:/Users/amittelbach/EDV%20Dienstleistungen/Kunden/OUCS/TEIISO/workspace/Stylesheets/docx/from/dynamic/toc.xsl'"/>
   <xsl:output method="xml" indent="yes"/>
   <xsl:template name="s:main">
      <xsl:message>
         <xsl:text>Testing with </xsl:text>
         <xsl:value-of select="system-property('xsl:product-name')"/>
         <xsl:text> </xsl:text>
         <xsl:value-of select="system-property('xsl:product-version')"/>
      </xsl:message>
      <xsl:processing-instruction name="xml-stylesheet">type="text/xsl" href="file:/usr/local/bin/xspec-v0.1/format-xspec-report.xsl"</xsl:processing-instruction>
      <s:report stylesheet="{$s:stylesheet-uri}" date="{current-dateTime()}">
         <xsl:call-template name="s:d4e1"/>
      </s:report>
   </xsl:template>
   <xsl:template name="s:d4e1">
      <xsl:message>test creating table of contents</xsl:message>
      <s:scenario label="test creating table of contents">
         <s:call template="tocSection"/>
         <xsl:variable name="actual-result" as="item()*">
            <xsl:call-template name="tocSection"/>
         </xsl:variable>
         <xsl:call-template name="test:report-value">
            <xsl:with-param name="value" select="$actual-result"/>
            <xsl:with-param name="wrapper-name" select="'s:result'"/>
            <xsl:with-param name="wrapper-ns" select="'http://www.jenitennison.com/xslt/xspec'"/>
         </xsl:call-template>
         <xsl:call-template name="s:d4e3">
            <xsl:with-param name="actual-result" select="$actual-result"/>
         </xsl:call-template>
      </s:scenario>
   </xsl:template>
   <xsl:template name="s:d4e3">
      <xsl:param name="actual-result" as="item()*" required="yes"/>
      <xsl:message>    a divGen element should be created</xsl:message>
      <xsl:variable name="expected-result-doc" as="document-node()">
         <xsl:document>
            <divGen type="toc"/>
         </xsl:document>
      </xsl:variable>
      <xsl:variable name="expected-result" select="$expected-result-doc/node()"/>
      <xsl:variable name="successful" as="xs:boolean"
                    select="test:deep-equal($expected-result, $actual-result, 2)"/>
      <xsl:if test="not($successful)">
         <xsl:message>      FAILED</xsl:message>
      </xsl:if>
      <s:test label="a divGen element should be created" successful="{$successful}">
         <s:expect>
            <divGen type="toc"/>
         </s:expect>
      </s:test>
   </xsl:template>
</xsl:stylesheet>
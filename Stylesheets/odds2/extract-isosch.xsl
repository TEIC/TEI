<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xi="http://www.w3.org/2001/XInclude"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns="http://purl.oclc.org/dsdl/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:oxdoc="http://www.oxygenxml.com/ns/doc/xsl"
                version="2.0"
                exclude-result-prefixes="tei rng teix sch xi
                                         #default">
  <xsl:import href="../common2/i18n.xsl"/>  
  <xsl:import href="../common2/odds.xsl"/>  
  <oxdoc:doc scope="stylesheet" type="stylesheet">
    <oxdoc:desc>
      <oxdoc:p> TEI stylesheet for simplifying TEI ODD markup </oxdoc:p>
      <oxdoc:p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
                
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</oxdoc:p>
      <oxdoc:p>Author: See AUTHORS</oxdoc:p>
      <oxdoc:p>Id: $Id$</oxdoc:p>
      <oxdoc:p>Copyright: 2011, TEI Consortium</oxdoc:p>
      <oxdoc:p/>
      <oxdoc:p>Modified 2012-05 by Syd Bauman: It seems that ISO Schematron does not have
        a <oxdoc:pre>&lt;key></oxdoc:pre> element. In fact, ISO 19757-3:2006 explicitly
        says “The XSLT key element may be used, in the XSLT namespace, before the pattern
        elements.” So we could just ignore <oxdoc:pre>&lt;key></oxdoc:pre> elements in
        the (ISO) Schematron namespace, but since then the user will likely not be
        getting what is intended, we’ll issue an error message as well.</oxdoc:p>
    </oxdoc:desc>
  </oxdoc:doc>

  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:param name="lang"></xsl:param>

  <xsl:key name="NSs" 
           match="sch:ns"
           use="1"/>

  <xsl:key name="KEYs" 
           match="xsl:key"
           use="1"/>

  <xsl:key name="badKEYs" 
           match="sch:key"
           use="1"/>

  <xsl:key name="PATTERNs"
           match="sch:pattern"
           use="1"/>

  <xsl:key name="CONSTRAINTs"
           match="tei:constraint"
           use="1"/>

  <xsl:template match="/">
    <schema queryBinding="xslt2">
      <title>ISO Schematron rules</title>

      <xsl:comment>namespaces:</xsl:comment>
      <xsl:for-each select="key('NSs',1)">
        <xsl:choose>
          <xsl:when test="ancestor::teix:egXML"/>
          <xsl:when
            test="ancestor::tei:constraintSpec/@xml:lang
                 and not(ancestor::tei:constraintSpec/@xml:lang = $lang)"/>
          <xsl:otherwise>
            <xsl:apply-templates select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:if test="not(sch:ns[@prefix='tei'])">
	<sch:ns prefix="tei"
		uri="http://www.tei-c.org/ns/1.0"/>
      </xsl:if>


      <xsl:comment>keys:</xsl:comment>
      <xsl:for-each select="key('KEYs',1)">
        <xsl:choose>
          <xsl:when test="ancestor::teix:egXML"/>
          <xsl:when
            test="ancestor::tei:constraintSpec/@xml:lang
                 and not(ancestor::tei:constraintSpec/@xml:lang = $lang)"/>
          <xsl:otherwise>
            <xsl:apply-templates select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>

      <xsl:if test="key('badKEYs',1)">
        <xsl:message>WARNING: You have <xsl:value-of select="count(key('badKEYs',1))"/> &lt;key>
          elements in the ISO Schematron namespace — but ISO Schematron does not have a &lt;key>
          element, so they are being summarily ignored. This will likely result in an ISO Schematron
          schema that does not perform the desired constraint tests properly.</xsl:message>
      </xsl:if>

      <xsl:comment>patterns:</xsl:comment>
      <xsl:for-each select="key('PATTERNs',1)">
        <xsl:choose>
          <xsl:when test="ancestor::teix:egXML"/>
          <xsl:when
            test="ancestor::tei:constraintSpec/@xml:lang
                 and not(ancestor::tei:constraintSpec/@xml:lang = $lang)"/>
          <xsl:otherwise>
            <xsl:apply-templates select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>

      <xsl:comment>constraints:</xsl:comment>
      <xsl:for-each select="key('CONSTRAINTs',1)">
        <xsl:choose>
          <xsl:when test="ancestor::teix:egXML"/>
          <xsl:when
            test="ancestor::tei:constraintSpec/@xml:lang
                 and not(ancestor::tei:constraintSpec/@xml:lang = $lang)"/>
          <xsl:otherwise>
            <xsl:variable name="patID">
              <xsl:choose>
                <xsl:when test="ancestor::tei:elementSpec">
                  <xsl:value-of
                    select="concat(ancestor::tei:elementSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"
                  />
                </xsl:when>
                <xsl:when test="ancestor::tei:classSpec">
                  <xsl:value-of
                    select="concat(ancestor::tei:classSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"
                  />
                </xsl:when>
                <xsl:when test="ancestor::tei:macroSpec">
                  <xsl:value-of
                    select="concat(ancestor::tei:macroSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"
                  />
                </xsl:when>
                <xsl:otherwise>
                  <!-- Added 2010-07-03 by Syd Bauman to handle the case in which <constraintSpec> -->
                  <!-- is a direct child of <schemaSpec>. -->
                  <xsl:text>constraint-</xsl:text>
                  <xsl:value-of select="ancestor::tei:constraintSpec/@ident"/>
                  <xsl:if test="count( ../sch:rule ) > 1">
                    <xsl:text>-</xsl:text>
                    <xsl:number/>
                  </xsl:if>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>
            <xsl:if test="sch:rule">
              <pattern id="{$patID}">
                <xsl:apply-templates select="sch:rule"/>
              </pattern>
            </xsl:if>
            <xsl:if test="sch:assert|sch:report">
              <pattern id="{$patID}">
                <rule>
                  <xsl:attribute name="context">
                    <xsl:sequence select="tei:generate-nsprefix-schematron(.)"/>
                    <xsl:choose>
                      <xsl:when test="ancestor::tei:elementSpec">
                        <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
                      </xsl:when>
                      <xsl:otherwise>*</xsl:otherwise>
                    </xsl:choose>
                  </xsl:attribute>
                  <xsl:apply-templates select="sch:assert|sch:report"/>
                </rule>
              </pattern>
            </xsl:if>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </schema>
  </xsl:template>
  
  <xsl:template match="sch:rule[not(@context)]">
    <rule>
      <xsl:attribute name="context">
	<xsl:sequence select="tei:generate-nsprefix-schematron(.)"/>        
        <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </rule>
  </xsl:template>
    
  <xsl:template match="@*|text()|comment()|processing-instruction()">
    <xsl:copy/>
  </xsl:template>
  
  <xsl:template match="sch:*|xsl:key">
    <xsl:element name="{local-name()}" namespace="{namespace-uri(.)}">
      <xsl:apply-templates select="@*|node()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="sch:key"/>

</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xi="http://www.w3.org/2001/XInclude"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns="http://purl.oclc.org/dsdl/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei rng teix sch xi #default">
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:key name="SCHEMATRONS" match="sch:ns[parent::tei:constraint or parent::rng:*]"
            use="1"/>
  <xsl:key name="SCHEMATRON"
            match="sch:pattern[parent::tei:constraint or         parent::rng:*]"
            use="1"/>
  <xsl:key name="SCHEMATRON"
            match="sch:rule[parent::tei:constraint or         parent::rng:*]"
            use="1"/>
  <xsl:key name="SCHEMATRON"
            match="sch:assert[parent::tei:constraint or         parent::rng:*]"
            use="1"/>
  <xsl:key name="SCHEMATRON" match="sch:report[parent::tei:constraint or parent::rng:*]"
            use="1"/>
  <xsl:template match="/">
      <schema queryBinding="xslt2">
         <title>ISO Schematron rules</title>
         <xsl:for-each select="key('SCHEMATRONS',1)">
            <xsl:choose>
               <xsl:when test="ancestor::teix:egXML"/>
               <xsl:otherwise>
                  <xsl:apply-templates select="."/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:for-each>
         <xsl:for-each select="key('SCHEMATRON',1)">
            <xsl:choose>
               <xsl:when test="ancestor::teix:egXML"/>
               <xsl:when test="self::sch:pattern">
                  <xsl:apply-templates select="."/>
               </xsl:when>
               <xsl:when test="self::sch:rule[@context]">
                  <xsl:variable name="patID">
                     <xsl:choose>
                        <xsl:when test="ancestor::tei:elementSpec">
                           <xsl:value-of select="concat(ancestor::tei:elementSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"/>
                        </xsl:when>
                        <xsl:when test="ancestor::tei:classSpec">
                           <xsl:value-of select="concat(ancestor::tei:classSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"/>
                        </xsl:when>
                        <xsl:when test="ancestor::tei:macroSpec">
                           <xsl:value-of select="concat(ancestor::tei:macroSpec/@ident,'-constraint-',ancestor::tei:constraintSpec/@ident)"/>
                        </xsl:when>
                     </xsl:choose>
                  </xsl:variable>
                  <pattern id="{$patID}">
                     <xsl:apply-templates select="."/>
                  </pattern>
               </xsl:when>
               <xsl:when test="(self::sch:report and not(preceding-sibling::sch:report)) and ancestor::tei:elementSpec">
                  <pattern id="{ancestor::tei:elementSpec/@ident}-constraint-{ancestor::tei:constraintSpec/@ident}">
                     <rule>
                        <xsl:attribute name="context">
                           <xsl:text>tei:</xsl:text>
                           <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
                        </xsl:attribute>
                        <xsl:apply-templates select="../sch:report"/>
                     </rule>
                  </pattern>
               </xsl:when>
               <xsl:when test="(self::sch:assert and not(preceding-sibling::sch:assert)) and ancestor::tei:elementSpec">
                  <pattern id="{ancestor::tei:elementSpec/@ident}-constraint-{ancestor::tei:constraintSpec/@ident}">
                     <rule>
                        <xsl:attribute name="context">
                           <xsl:text>tei:</xsl:text>
                           <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
                        </xsl:attribute>
                        <xsl:apply-templates select="../sch:assert"/>
                     </rule>
                  </pattern>
               </xsl:when>
            </xsl:choose>
         </xsl:for-each>
      </schema>
  </xsl:template>
  
  
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="sch:*">
      <xsl:element name="{local-name()}">
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:element>
  </xsl:template>
  
</xsl:stylesheet>
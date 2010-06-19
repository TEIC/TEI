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

  <xsl:key name="NS" 
	   match="sch:ns"
	   use="1"/>

  <xsl:key name="PATTERNS"
	   match="sch:pattern"
	   use="1"/>

  <xsl:key name="CONSTRAINTS"
	   match="tei:constraint"
	   use="1"/>


  <xsl:template match="/">
      <schema queryBinding="xslt2">
         <title>ISO Schematron rules</title>
         <xsl:for-each select="key('NS',1)">
            <xsl:choose>
               <xsl:when test="ancestor::teix:egXML"/>
               <xsl:otherwise>
                  <xsl:apply-templates select="."/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:for-each>

         <xsl:for-each select="key('PATTERNS',1)">
            <xsl:choose>
               <xsl:when test="ancestor::teix:egXML"/>
               <xsl:otherwise>
                  <xsl:apply-templates select="."/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:for-each>

         <xsl:for-each select="key('CONSTRAINTS',1)">
            <xsl:choose>
               <xsl:when test="ancestor::teix:egXML"/>
               <xsl:otherwise>
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
		 <xsl:if test="sch:rule">
		   <pattern id="{$patID}">
		     <xsl:apply-templates select="sch:rule"/>
		   </pattern>
		 </xsl:if>
		 <xsl:if test="sch:assert|sch:report">
		   <pattern id="{$patID}">
		     <rule>
		       <xsl:attribute name="context">
			 <xsl:text>tei:</xsl:text>
			 <xsl:choose>
			   <xsl:when test="ancestor::tei:elementSpec">
			     <xsl:value-of
				 select="ancestor::tei:elementSpec/@ident"/>
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
	<xsl:text>tei:</xsl:text>
	<xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </rule>
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
<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml"
                xmlns:h="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="tei cals tbx"
                version="2.0">
   <xsl:import href="isoutils.xsl"/>

   <xsl:key name="frontDiv" match="tei:div[ancestor::tei:front]" use="1"/>
   <xsl:key name="bodyDiv" match="tei:div[ancestor::tei:body]" use="1"/>
   <xsl:key name="backDiv" match="tei:div[ancestor::tei:back]" use="1"/>
   <xsl:output method="xhtml" encoding="utf-8"/>
   <xsl:template match="/tei:TEI">
      <xsl:variable name="today">
         <xsl:call-template name="whatsTheDate"/>
      </xsl:variable>
      <xsl:variable name="isotitle">
         <xsl:call-template name="generateTitle"/>
      </xsl:variable>
      <xsl:variable name="isonumber">
         <xsl:call-template name="getiso_documentNumber"/>
      </xsl:variable>
      <xsl:variable name="isopart">
         <xsl:call-template name="getiso_partNumber"/>
      </xsl:variable>
      <xsl:variable name="isoyear">
         <xsl:call-template name="getiso_year"/>
      </xsl:variable>
      <html>
         <head>
            <title>Report on 
    <xsl:value-of select="$isotitle"/>:
    <xsl:value-of select="$isoyear"/>:
    <xsl:value-of select="$isonumber"/>:
    <xsl:value-of select="$isopart"/>
            </title>
            <link href="iso.css" rel="stylesheet" type="text/css"/>
  
         </head>
         <body>
            <h1 class="maintitle">
	
	              <xsl:value-of select="$isotitle"/>:
	<xsl:value-of select="$isoyear"/>:
	<xsl:value-of select="$isonumber"/>:
	<xsl:value-of select="$isopart"/>
            </h1>
      
            <xsl:for-each select="tei:text/tei:front">
	              <xsl:apply-templates/>
	              <hr/>
            </xsl:for-each>
            <xsl:for-each select="tei:text/tei:body">
	              <xsl:apply-templates/>
	              <hr/>
            </xsl:for-each>
            <xsl:for-each select="tei:text/tei:back">
	              <xsl:apply-templates/>
            </xsl:for-each>
         </body>
      </html>
   </xsl:template>

   <xsl:template match="tei:div[not(@type='termHeading')]">
      <xsl:variable name="depth" select="count(ancestor::tei:div)+2"/>
      <xsl:variable name="stuff">
         <xsl:apply-templates/>
      </xsl:variable>
      <xsl:if test="$stuff/h:p">
         <xsl:element name="h{$depth}">
            <xsl:call-template name="head"/>
         </xsl:element>
         <xsl:copy-of select="$stuff"/>
      </xsl:if>
   </xsl:template>

   <xsl:template match="*">
      <xsl:apply-templates select="*"/>
   </xsl:template>

   <xsl:template match="tei:add|tei:del">
      <xsl:if test="not(preceding-sibling::tei:del|preceding-sibling::tei:add)">
         <p>
            <xsl:for-each select="parent::*">
               <xsl:choose>
                  <xsl:when test="preceding-sibling::tei:*[1][self::tei:addSpan]">
	                    <span class="add">ADD</span>
	                    <xsl:text> </xsl:text>
                  </xsl:when>
                  <xsl:when test="preceding-sibling::tei:*[1][self::tei:delSpan]">
	                    <span class="add">DELETE</span>
	                    <xsl:text> </xsl:text>
                  </xsl:when>
               </xsl:choose>
               <xsl:choose>
                  <xsl:when test="self::tei:p">Paragraph
      <xsl:number/>
                  </xsl:when>
                  <xsl:when test="self::tei:item">List item </xsl:when>
                  <xsl:when test="self::tei:head">Heading </xsl:when>
                  <xsl:when test="self::cals:entry">Table row <xsl:for-each select="parent::cals:row">
                        <xsl:number/>
                     </xsl:for-each>, cell <xsl:number/> 
                  </xsl:when>
                  <xsl:when test="self::tei:term">Terminology entry <xsl:number/>
                  </xsl:when>
                  <xsl:when test="self::tei:bibl">Bibliographical entry <xsl:number/> 
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:value-of select="local-name()"/>
                     <xsl:text> </xsl:text>
                     <xsl:number/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:for-each>
  [<xsl:value-of select="@when"/>, <xsl:value-of select="substring-after(@resp,'#')"/>]
  </p>
         <blockquote>
            <xsl:for-each select="parent::*">
               <xsl:apply-templates mode="show"/>
            </xsl:for-each>
         </blockquote>
      </xsl:if>
   </xsl:template>


   <xsl:template match="*" mode="show">
      <xsl:apply-templates/>
   </xsl:template>


   <xsl:template match="tei:add|tei:del" mode="show">
      <span class="{local-name()}">
         <xsl:value-of select="."/>
      </span>
   </xsl:template>

   <xsl:template name="head">
      <xsl:choose>
         <xsl:when test="ancestor::tei:front">
            <xsl:number count="tei:div" from="tei:front" format="i" level="multiple"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:body">
            <xsl:number count="tei:div" from="tei:body" format="1" level="multiple"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:back">
            <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
         </xsl:when>
      </xsl:choose>
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="tei:head" mode="ok"/>
   </xsl:template>

   <xsl:template match="tei:del" mode="ok"/>

</xsl:stylesheet>
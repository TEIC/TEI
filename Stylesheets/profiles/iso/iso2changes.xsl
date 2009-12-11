<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xmlns:fn="http://www.w3.org/2005/xpath-functions"
		xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml"
                xmlns:h="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="tei cals tbx"
                version="2.0">
   <xsl:import href="isoutils.xsl"/>
   <xsl:param name="dpi">72</xsl:param>
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
	                    <span class="del">DELETE</span>
	                    <xsl:text> </xsl:text>
                  </xsl:when>
               </xsl:choose>
	      <xsl:call-template name="Identify"/>
            </xsl:for-each>
	 </p>
         <blockquote>
	   <xsl:for-each select="parent::*">
	     <xsl:apply-templates mode="show"/>
	   </xsl:for-each>
	 </blockquote>
      </xsl:if>
   </xsl:template>

   <xsl:template name="Identify">
     <xsl:for-each select="ancestor::cals:entry">
       <xsl:text> </xsl:text>
       <xsl:call-template name="Identify"/>
       <xsl:text> </xsl:text>
     </xsl:for-each>
     <xsl:choose>
       <xsl:when test="self::tei:p">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">Paragraph</xsl:with-param>
	   <xsl:with-param
	       name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:item">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">List item</xsl:with-param>
	   <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:head">
	 <xsl:choose>
	   <xsl:when
	       test="count(ancestor::tei:div)=1">Clause title</xsl:when>
	   <xsl:otherwise>Clause subtitle</xsl:otherwise>
	 </xsl:choose>
       </xsl:when>
       <xsl:when test="self::cals:entry">
	 <xsl:for-each select="ancestor::cals:table">
	   <xsl:text>Table </xsl:text>
	   <xsl:number level="any"/>
	   <xsl:text>, </xsl:text>
	 </xsl:for-each>
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">row</xsl:with-param>
	   <xsl:with-param name="N">
	     <xsl:for-each select="parent::cals:row">
	       <xsl:number/>
	   </xsl:for-each></xsl:with-param>
	 </xsl:call-template>
	 <xsl:text>, </xsl:text>
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">cell</xsl:with-param>
	   <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:term">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">Terminology entry</xsl:with-param>
	   <xsl:with-param
	       name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tbx:term">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">Term</xsl:with-param>
	   <xsl:with-param name="N">
	     <xsl:value-of select="substring-after(@id,'CDB_')"/>
	   </xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:bibl">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">Bibliographical entry</xsl:with-param>
	   <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:math">
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">math</xsl:with-param>
	   <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:call-template name="LabelChange">
	   <xsl:with-param name="What">
	     <xsl:value-of select="local-name()"/>
	   </xsl:with-param>
	   <xsl:with-param name="N"><xsl:number/></xsl:with-param>
	 </xsl:call-template>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>


   <xsl:template match="*" mode="show">
      <xsl:apply-templates mode="show"/>
   </xsl:template>


   <xsl:template match="tei:graphic" mode="show">

      <xsl:variable name="File">
	<xsl:value-of select="@url"/>
      </xsl:variable>
      <img src="{$File}">
	<xsl:if test="@width">
	  <xsl:call-template name="setDimension">
	    <xsl:with-param name="value">
	      <xsl:value-of select="@width"/>
	    </xsl:with-param>
	    <xsl:with-param name="name">width</xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
	<xsl:if test="@height">
	  <xsl:call-template name="setDimension">
	    <xsl:with-param name="value">
	      <xsl:value-of select="@height"/>
	    </xsl:with-param>
	    <xsl:with-param name="name">height</xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </img>
   </xsl:template>

   <xsl:template match="tei:add|tei:del" mode="show">
      <span class="{local-name()}">
         <xsl:apply-templates mode="show"/>
	 <span class="changeAttribution-{local-name()}">
	   <xsl:text> [</xsl:text>
	   <xsl:value-of select="@type"/>
	   <xsl:text> </xsl:text>
	   <xsl:value-of select="@n"/>
	   <xsl:text>]</xsl:text>
	 </span>
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
	   Annex <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
         </xsl:when>
      </xsl:choose>
      <xsl:text> </xsl:text>
      <xsl:apply-templates select="tei:head" mode="ok"/>
   </xsl:template>

   <xsl:template match="tei:del" mode="ok"/>

   <xsl:template name="LabelChange">
     <xsl:param name="What"/>
     <xsl:param name="N"/>
     <xsl:choose>
       <xsl:when test="not(number($N))">
	 <xsl:value-of select="$What"/>
	 <xsl:text> </xsl:text>
	 <xsl:value-of select="$N"/>
       </xsl:when>
       <xsl:when test="$N=1">     
	 <xsl:text>1st </xsl:text>
	 <xsl:value-of select="$What"/>
       </xsl:when>
       <xsl:when test="$N=2">     
	 <xsl:text>2nd </xsl:text>
	 <xsl:value-of select="$What"/>
       </xsl:when>
       <xsl:when test="$N=3">     
	 <xsl:text>3rd </xsl:text>
	 <xsl:value-of select="$What"/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:value-of select="$N"/>
	 <xsl:text>th </xsl:text>
	 <xsl:value-of select="$What"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

  <xsl:template name="setDimension">
      <xsl:param name="name"/>
      <xsl:param name="value"/>

      <xsl:variable name="calcvalue">
         <xsl:choose>
            <xsl:when test="contains($value,'in')">
               <xsl:value-of select="round($dpi * number(substring-before($value,'in')))"/>
            </xsl:when>
            <xsl:when test="contains($value,'pt')">
               <xsl:value-of select="round($dpi * (number(substring-before($value,'pt')) div 72))"/>
            </xsl:when>
            <xsl:when test="contains($value,'cm')">
               <xsl:value-of select="round($dpi * (number(substring-before($value,'cm')) div 2.54 ))"/>
            </xsl:when>
            <xsl:when test="contains($value,'px')">
               <xsl:value-of select="substring-before($value,'px')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$value"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:attribute name="{$name}">
         <xsl:value-of select="$calcvalue"/>
      </xsl:attribute>
  </xsl:template>

   <xsl:template match="tei:ref[@rend='TableFootnoteXref']" mode="show">
     <span class="superscript">
       <xsl:apply-templates mode="show"/>
     </span>
   </xsl:template>

</xsl:stylesheet>
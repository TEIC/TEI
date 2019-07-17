<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  exclude-result-prefixes="office style text table draw fo xlink dc
			   meta number tei svg chart dr3d math form
			   script ooo ooow oooc dom xforms xs xsd xsi"
  office:version="1.0" version="2.0" 
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
  xmlns:dc="http://purl.org/dc/elements/1.1/"
  xmlns:dom="http://www.w3.org/2001/xml-events"
  xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
  xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
  xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
  xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"
  xmlns:math="http://www.w3.org/1998/Math/MathML"
  xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
  xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
  xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
  xmlns:ooo="http://openoffice.org/2004/office"
  xmlns:oooc="http://openoffice.org/2004/calc"
  xmlns:ooow="http://openoffice.org/2004/writer"
  xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
  xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
  xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
  xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"
  xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
  xmlns:xforms="http://www.w3.org/2002/xforms"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:xsd="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	  <xsl:import href="../common/functions.xsl"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making TEI files from
	 OpenOffice. Originally derived from the OpenOffice /Docbook
	 conversion, but largely rewritten</p>
         <p> 
            <h1 xmlns="">License</h1>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


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
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:key match="style:style" name="STYLES" use="@style:name"/>

  <xsl:key name="LISTS" 
	 match="text:list-level-style-number" 
	 use="parent::text:list-style/@style:name"/>

  <xsl:key match="text:h" name="Headings" use="text:outline-level"/>

  <xsl:param name="debug">false</xsl:param>

  <xsl:param name="dir">.</xsl:param>

  <xsl:output encoding="utf-8" indent="yes"/>

  <!--  <xsl:strip-space elements="text:span"/>-->
  
  <xsl:variable name="META">
    <xsl:choose>
      <xsl:when test="doc-available(concat($dir,'/meta.xml'))">
        <xsl:copy-of select="document(concat($dir,'/meta.xml'))//office:meta"/>
      </xsl:when>
      <xsl:when test="/office:document/office:meta">
        <xsl:copy-of select="/office:document/office:meta"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="/office:document-meta/office:meta"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="document-title">
    <xsl:choose>
      <xsl:when
	  test="/office:document-content/office:body/office:text/text:p[@text:style-name='Title']">
        <xsl:value-of
	    select="/office:document-content/office:body/office:text/text:p[@text:style-name='Title'][1]"
	    />
      </xsl:when>
      <xsl:when test="$META/office:meta/dc:title">
        <xsl:value-of select="$META/office:meta/dc:title"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>Untitled Document</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:template match="*" mode="summary">
    <xsl:text>&#10;</xsl:text>
    <xsl:for-each select="ancestor::*"><xsl:text>.</xsl:text></xsl:for-each>
    <xsl:value-of select="name()"/>:
    <xsl:apply-templates mode="summary"/>
  </xsl:template>

  <xsl:template match="/">
    <xsl:if test="$debug='true'">
      <xsl:message>Look for metadata in <xsl:value-of select="concat($dir,'/meta.xml')"/></xsl:message>
    </xsl:if>
    <xsl:variable name="pass1">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:variable name="pass2">
      <xsl:apply-templates mode="pass2" select="$pass1"/>
    </xsl:variable>
    <xsl:apply-templates mode="pass3" select="$pass2"/>    
  </xsl:template>

  <xsl:template match="office:document-content|office:body">
    <xsl:for-each select="descendant::text:variable-decl">
      <xsl:variable name="name">
        <xsl:value-of select="@text:name"/>
      </xsl:variable>
      <xsl:if test="contains(@text:name,'entitydecl')">
        <xsl:text disable-output-escaping="yes">&lt;!DOCTYPE TEI [
	</xsl:text>
        <xsl:text disable-output-escaping="yes">&lt;!ENTITY </xsl:text>
        <xsl:value-of select="substring-after(@text:name,'entitydecl_')"/>
        <xsl:text> &quot;</xsl:text>
        <xsl:value-of
          select="/descendant::text:variable-set[@text:name= $name][1]"/>
        <xsl:text disable-output-escaping="yes">&quot;&gt;</xsl:text>
        <xsl:text disable-output-escaping="yes">]&gt;</xsl:text>
      </xsl:if>
    </xsl:for-each>
    <TEI>
	<xsl:for-each select="$META/office:meta/dc:language">
	  <xsl:attribute name="xml:lang">
	    <xsl:value-of select="normalize-space(.)"/>
	  </xsl:attribute>
	</xsl:for-each>  
      <xsl:call-template name="teiHeader"/>
      <text>
	<xsl:apply-templates/>
      </text>
    </TEI>
  </xsl:template>


  <xsl:template match="text:variable-set|text:variable-get">
    <xsl:choose>
      <xsl:when test="contains(@text:style-name,'entitydecl')">
        <xsl:text disable-output-escaping="yes">&amp;</xsl:text>
        <xsl:value-of select="substring-after(@text:style-name,'entitydecl_')"/>
        <xsl:text disable-output-escaping="yes">;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="teiHeader">
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            <xsl:value-of select="$document-title"/>
          </title>
          <author>
            <xsl:value-of
              select="$META/office:meta/meta:initial-creator"/>
          </author>
        </titleStmt>
        <editionStmt>
          <edition>
            <date>
              <xsl:value-of
                select="$META/office:meta/meta:creation-date"/>
            </date>
          </edition>
        </editionStmt>
        <publicationStmt>
          <p>no publication statement available</p>
        </publicationStmt>
        <sourceDesc>
          <p>
	    <xsl:apply-templates
              select="$META/office:meta/meta:generator"/>
	    <xsl:text>Written by OpenOffice</xsl:text>
	  </p>
        </sourceDesc>
      </fileDesc>
      <xsl:variable name="pD">
	<xsl:if test="$META/office:meta/dc:language">
	  <langUsage>
	    <language>
	      <xsl:attribute name="ident">
		<xsl:value-of select="$META/office:meta/dc:language"/>
	      </xsl:attribute>
	      <xsl:value-of select="$META/office:meta/dc:language"/>
	    </language>
	  </langUsage>
	</xsl:if>
	<xsl:if test="$META/office:meta/meta:keyword">
	  <textClass>
	    <keywords scheme="unknown">
	      <list>
		<xsl:for-each select="$META/office:meta/meta:keyword">
		  <item>
		    <xsl:value-of select="."/>
		  </item>
		</xsl:for-each>
	      </list>
	    </keywords>
	  </textClass>
	</xsl:if>
      </xsl:variable>
      <xsl:if test="$pD/*">
	<profileDesc>
	  <xsl:copy-of select="$pD"/>
	</profileDesc>
      </xsl:if>
      <revisionDesc>
	<listChange>
          <change>
	    <name>
	      <xsl:apply-templates
		  select="$META/office:meta/dc:creator"/>
	    </name>
	    <date>
	      <xsl:apply-templates select="$META/office:meta/dc:date"/>
	    </date>
          </change>
	</listChange>
      </revisionDesc>
    </teiHeader>
  </xsl:template>



  <xsl:template match="/office:document-content/office:body">
      <xsl:apply-templates/>
  </xsl:template>

  <!-- sections -->
  <xsl:template match="text:h">
    <xsl:choose>
      <xsl:when test="ancestor::text:note-body">
          <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@text:style-name='ArticleInfo'"> </xsl:when>
      <xsl:when test="@text:style-name='Abstract'">
        <div type="abstract">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="@text:style-name='Appendix'">
        <div type="appendix">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
	<HEAD level="1" style="{@text:style-name}">
	  <xsl:apply-templates/>
	</HEAD>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text:h[@text:outline-level]">
    <xsl:choose>
      <xsl:when test="ancestor::text:note-body">
	<p>
	  <xsl:attribute name="rend">
	    <xsl:choose>
	      <xsl:when test="@text:style-name">
		<xsl:value-of select="@text:style-name"/>
	      </xsl:when>
	      <xsl:otherwise>heading</xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>
          <xsl:apply-templates/>
	</p>
      </xsl:when>
      <xsl:otherwise>
	<HEAD level="{@text:outline-level}" >
	  <xsl:attribute name="style">
	    <xsl:choose>
	      <xsl:when test="@text:style-name">
		<xsl:value-of select="@text:style-name"/>
	      </xsl:when>
	      <xsl:otherwise>nostyle</xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>
	  <!--
	    We do not process hard page breaks here directly 
	    but defer their output until the sections get created.
	    Therefore we add a dedicated attribute '@page-break-before'
	    for flagging this.
	  -->
	  <xsl:attribute name="page-break-before" select="exists(tei:pagebreak-before(.))"/>
	  <xsl:apply-templates/>
	</HEAD>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- special case paragraphs -->
  <xsl:template match="text:p[@text:style-name='XMLComment']">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>

  <xsl:template match="office:annotation/text:p" priority="100">
    <note>
        <xsl:apply-templates/>
    </note>
  </xsl:template>


  <xsl:template match="text:p">
    <xsl:sequence select="tei:pagebreak-before(.)"/>

    <xsl:choose>
      <xsl:when test="draw:frame and parent::draw:text-box">
	<xsl:apply-templates select="draw:frame"/>
	<head>
	  <xsl:apply-templates select="text()|*[not(local-name(.)='frame')]"/>
	</head>
      </xsl:when>

      <xsl:when test="count(parent::text:note-body/text:p)=1">
          <xsl:apply-templates/>
      </xsl:when>

      <xsl:when test="count(parent::text:list-item/text:p)=1">
          <xsl:apply-templates/>
      </xsl:when>

      <xsl:when test="@text:style-name='Document Title'">
        <docTitle>
          <xsl:apply-templates/>
	</docTitle>
      </xsl:when>

      <xsl:when test="@text:style-name='Author'">
        <author>
          <xsl:apply-templates/>
        </author>
      </xsl:when>

      <xsl:when test="@text:style-name='lg'">
        <lg>
	  <xsl:for-each-group select="node()"
			      group-ending-with="text:line-break">
	    <l><xsl:apply-templates select="current-group()"/></l>
	  </xsl:for-each-group>
        </lg>
      </xsl:when>

      <xsl:when test="@text:style-name='Title'">
        <title>
          <xsl:apply-templates/>
        </title>
      </xsl:when>

      <xsl:when test="@text:style-name='Section Title'">
        <head>
          <xsl:apply-templates/>
        </head>
      </xsl:when>

      <xsl:when test="@text:style-name='Appendix Title'">
        <head>
          <xsl:apply-templates/>
        </head>
      </xsl:when>

      <xsl:when test="parent::text:list-item">
	<xsl:call-template name="applyStyle"/>
      </xsl:when>

      <xsl:when test="@text:style-name='Table'"/>

      <xsl:when test="text:span[@text:style-name = 'XrefLabel']"/>

      <xsl:when test="@text:style-name='Speech'">
        <sp>
          <speaker/>
	  <xsl:call-template name="applyStyle"/>
        </sp>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="applyStyle"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- lists -->
  <xsl:template match="text:list">
    <xsl:variable name="style">
      <xsl:for-each select="key('LISTS',@text:style-name)[1]">
	<xsl:value-of select="@text:style-name"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="text:list-item/text:h">
	<xsl:for-each select="text:list-item">
	  <xsl:apply-templates/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="@text:style-name='Var List'">
        <list>
          <xsl:apply-templates/>
        </list>
      </xsl:when>
      <xsl:when test="contains($style,'Number')">
        <list type="ordered">
          <xsl:apply-templates/>
        </list>
      </xsl:when>
      <xsl:otherwise>
        <list type="unordered">
          <xsl:apply-templates/>
        </list>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text:list-header">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>

  <xsl:template match="text:list-item">
    <xsl:choose>
      <xsl:when test="parent::text:list/@text:style-name='Var List'">
        <item>
          <xsl:for-each select="text:p[@text:style-name='VarList Term']">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
        </item>
      </xsl:when>
      <xsl:otherwise>
        <item>
	  <xsl:apply-templates/>
	</item>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template
    match="text:p[@text:style-name='VarList Item' or
	   @text:style-name='List Contents']">
    <GLOSS n="item">
      <xsl:apply-templates/>
    </GLOSS>
  </xsl:template>

  <xsl:template
    match="text:p[@text:style-name='VarList Term' or @text:style-name='List Heading']">
    <GLOSS n="label"> 
      <xsl:apply-templates/>
    </GLOSS>
  </xsl:template>

  <!-- inline -->

  <xsl:template match="text:span">
    <xsl:variable name="Style">
      <xsl:value-of select="@text:style-name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$Style='Emphasis'">
        <emph>
          <xsl:apply-templates/>
        </emph>
      </xsl:when>
      <xsl:when test="$Style='Underline'">
        <hi rend="ul">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='SmallCaps'">
        <hi rend="sc">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='Emphasis Bold'">
        <hi rend="bold">
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='Highlight'">
        <hi>
          <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='q'">
        <q>
          <xsl:choose>
            <xsl:when test="starts-with(.,'&#x2018;')">
              <xsl:value-of
                select="substring-before(substring-after(.,'&#x2018;'),'&#x2019;')"
              />
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates/>
            </xsl:otherwise>
          </xsl:choose>
        </q>
      </xsl:when>
      <xsl:when test="$Style='Internet Link'">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="$Style='SubScript'">
	<hi rend="sub">
	  <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="$Style='SuperScript'">
        <hi rend="sup">
	  <xsl:apply-templates/>
        </hi>
      </xsl:when>
      <xsl:when test="style:text-properties[@fo:text-transform='small-caps']">
	<hi rend="sc">
	  <xsl:apply-templates/>
	</hi>
      </xsl:when>
      <xsl:when test="../text:h">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="applyStyle"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="applyStyle">
    <xsl:variable name="name">
      <xsl:value-of select="replace(@text:style-name,'tei_5f_','')"/>
    </xsl:variable>
    <xsl:variable name="style">
      <xsl:value-of select="@text:style-name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length(.)=0 and not (text:s or draw:frame)"/>
      <!-- if the style name is the same as that of a known TEI
      element, make it that -->
      <xsl:when test="doc-available('../names.xml') and
		      doc('../names.xml')//tei:gi[.=$name]">
	<xsl:element name="{$name}">
          <xsl:call-template name="id.attribute"/>
	  <xsl:apply-templates/>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="rendition"
		      select="tei:inlineStyles($style,.)"/>
	<xsl:choose>
	  <xsl:when test="self::text:p and count(parent::table:table-cell/text:p)=1">
	    <xsl:if test="not($rendition='')">
	      <xsl:attribute name="rend" select="$rendition"/>
	    </xsl:if>
	    <xsl:apply-templates/>
	  </xsl:when>
	  <xsl:when test="self::text:p">
	    <p>
            <xsl:call-template name="id.attribute"/>
	      <xsl:if test="not($rendition='')">
		<xsl:attribute name="rend" select="$rendition"/>
	      </xsl:if>
	      <xsl:apply-templates/>
	    </p>
	  </xsl:when>
	  <xsl:when test="$rendition=''">
	    <xsl:apply-templates/>
	  </xsl:when>
	  <xsl:otherwise>
	    <hi rend="{$rendition}">
	      <xsl:apply-templates/>
	    </hi>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- tables -->
  <xsl:template match="table:table">
    <xsl:sequence select="tei:pagebreak-before(.)"/>
    <table rend="frame">
      <xsl:if test="@table:name and not(@table:name = 'local-table')">
        <xsl:attribute name="xml:id">
          <xsl:value-of select="@table:name"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="following-sibling::text:p[@text:style-name='Table']">
        <head>
          <xsl:value-of
            select="following-sibling::text:p[@text:style-name='Table']"/>
        </head>
      </xsl:if>
      <xsl:call-template name="generictable"/>
    </table>

  </xsl:template>


  <xsl:template name="generictable">
    <xsl:variable name="cells" select="count(descendant::table:table-cell)"/>
    <xsl:variable name="rows">
      <xsl:value-of select="count(descendant::table:table-row) "/>
    </xsl:variable>
    <xsl:variable name="cols">
      <xsl:value-of select="$cells div $rows"/>
    </xsl:variable>
    <xsl:variable name="numcols">
      <xsl:choose>
        <xsl:when
          test="child::table:table-column/@table:number-columns-repeated">
          <xsl:value-of
            select="number(table:table-column/@table:number-columns-repeated+1)"
          />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$cols"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="colspec">
    <xsl:param name="left"/>
    <xsl:if
      test="number($left &lt; ( table:table-column/@table:number-columns-repeated +2)  )">
      <colspec>
        <xsl:attribute name="colnum">
          <xsl:value-of select="$left"/>
        </xsl:attribute>
        <xsl:attribute name="colname">
          <xsl:text>c</xsl:text>
          <xsl:value-of select="$left"/>
        </xsl:attribute>
      </colspec>
      <xsl:call-template name="colspec">
        <xsl:with-param name="left" select="$left+1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="table:table-column">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="table:table-header-rows">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="table:table-header-rows/table:table-row">
    <row role="label">
      <xsl:apply-templates/>
    </row>
  </xsl:template>

  <xsl:template match="table:table/table:table-row">
    <row>
      <xsl:apply-templates/>
    </row>
  </xsl:template>

  <xsl:template match="table:table-cell/text:h">
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="table:table-cell">
    <cell>
      <xsl:if test="@table:number-columns-spanned &gt;'1'">
        <xsl:attribute name="cols">
          <xsl:value-of select="@table:number-columns-spanned"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@table:number-rows-spanned &gt;'1'">
        <xsl:attribute name="rows">
          <xsl:value-of select="@table:number-rows-spanned"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="text:h">
        <xsl:attribute name="role">label</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </cell>
  </xsl:template>


  <!-- notes -->
  <xsl:template match="text:note-citation"/>

  <xsl:template match="text:note-body">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="text:note-ref">
    <ref target="#{@text:ref-name}">
      <xsl:apply-templates/>
    </ref>
  </xsl:template>

  <xsl:template match="text:note">
    <note>
      <xsl:if test="@text:id">
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="@text:id"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="@text:note-class='endnote'">
          <xsl:attribute name="place">end</xsl:attribute>
        </xsl:when>
        <xsl:when test="@text:note-class='footnote'">
          <xsl:attribute name="place">foot</xsl:attribute>
        </xsl:when>
      </xsl:choose>
      <xsl:if test="text:note-citation">
	<xsl:attribute name="n">
	  <xsl:value-of select="text:note-citation"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </note>
  </xsl:template>

  <!-- drawing -->
  <xsl:template match="draw:plugin">
    <ptr target="{@xlink:href}"/>
  </xsl:template>

  <xsl:template match="draw:text-box">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="draw:frame">
    <xsl:choose>
      <xsl:when test="ancestor::draw:frame">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<figure>
	  <xsl:apply-templates/>
	</figure>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="draw:image">
    <xsl:choose>
      <xsl:when test="ancestor::draw:text-box">
	<xsl:call-template name="findGraphic"/>
      </xsl:when>
      <xsl:when test="ancestor::draw:frame">
	<xsl:call-template name="findGraphic"/>
      </xsl:when>
      <xsl:when test="parent::text:p[@text:style-name='Mediaobject']">
        <figure>
          <xsl:call-template name="findGraphic"/>
          <head>
            <xsl:value-of select="."/>
          </head>
        </figure>
      </xsl:when>
      <xsl:otherwise>
        <figure>
          <xsl:call-template name="findGraphic"/>
        </figure>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="findGraphic">
    <xsl:choose>
      <xsl:when test="office:binary-data">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@xlink:href">
        <graphic>
          <xsl:attribute name="url">
            <xsl:value-of select="@xlink:href"/>
          </xsl:attribute>
        </graphic>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="office:binary-data">    
    <binaryObject encoding="base64" mimeType="image/jpg">
      <xsl:value-of select="."/>
    </binaryObject>
  </xsl:template>


  <!-- linking -->
  <xsl:template match="text:a">
    <xsl:choose>
      <xsl:when test="starts-with(@xlink:href,'mailto:')">
        <xsl:choose>
          <xsl:when test=".=@xlink:href">
            <ptr target="{substring-after(@xlink:href,'mailto:')}"/>
          </xsl:when>
          <xsl:otherwise>
            <ref target="{@xlink:href}">
              <xsl:apply-templates/>
            </ref>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="contains(@xlink:href,'://')">
        <xsl:choose>
          <xsl:when test=".=@xlink:href">
            <ptr target="{@xlink:href}"/>
          </xsl:when>
          <xsl:otherwise>
            <ref target="{@xlink:href}">
              <xsl:apply-templates/>
            </ref>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="not(contains(@xlink:href,'#'))">
        <ref target="{@xlink:href}">
          <xsl:apply-templates/>
        </ref>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="linkvar" select="@xlink:href"/>
        <xsl:choose>
          <xsl:when test=".=$linkvar">
            <ptr target="{$linkvar}"/>
          </xsl:when>
          <xsl:otherwise>
            <ref target="{$linkvar}">
              <xsl:apply-templates/>
            </ref>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text:line-break">
    <xsl:choose>
      <xsl:when test="parent::text:span[@text:style-name='l']"/>
      <xsl:when test="parent::text:p[@text:style-name='lg']"/>
      <xsl:otherwise>
	<lb/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text:tab">
    <xsl:text>	</xsl:text>
  </xsl:template>

  <xsl:template match="text:reference-ref">
    <ptr target="#id_{@text:ref-name}"/>
  </xsl:template>

  <xsl:template name="id.attribute.literal">
    <xsl:if test="child::text:reference-mark-start">
      <xsl:text> xml:id=&quot;</xsl:text>
	<xsl:text>id_</xsl:text>
        <xsl:value-of select="child::text:reference-mark-start/@text:style-name"
        />
	<xsl:text>&quot;</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template name="id.attribute">
    <xsl:if test="child::text:reference-mark-start">
      <xsl:attribute name="xml:id">
	<xsl:text>id_</xsl:text>
        <xsl:value-of select="child::text:reference-mark-start/@text:style-name"
        />
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="text:reference-mark-start"/>

  <xsl:template match="text:reference-mark-end"/>

  <xsl:template match="comment">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>

  <xsl:template match="text:user-index-mark">
    <index indexName="{@text:index-name}">
      <term>
	<xsl:value-of select="@text:string-value"/>
      </term>
    </index>
  </xsl:template>

  <xsl:template match="text:alphabetical-index-mark">
    <xsl:if test="not(normalize-space(@text:string-value)='')">
    <index>
      <xsl:if test="@text:id">
	<xsl:attribute name="xml:id">
	  <xsl:value-of select="@text:id"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="@text:key1">
	  <term>
	    <xsl:value-of select="@text:key1"/>
	  </term>
	  <index>
	    <term>
	      <xsl:value-of select="@text:string-value"/>
	    </term>
	  </index>
	</xsl:when>
	<xsl:otherwise>
	  <term>
	    <xsl:value-of select="@text:string-value"/>
	  </term>
	</xsl:otherwise>
      </xsl:choose>
    </index>
    </xsl:if>
  </xsl:template>

  <xsl:template match="text:alphabetical-index">
    <index>
      <xsl:apply-templates select="text:index-body"/>
    </index>
  </xsl:template>

  <xsl:template match="text:index-body">
    <xsl:for-each select="text:p[@text:style-name = 'Index 1']">
      <index>
        <term>
          <xsl:value-of select="."/>
        </term>
      </index>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="text:bookmark-ref">
    <ref target="#id_{@text:ref-name}" type="{@text:reference-format}">
      <xsl:apply-templates/>
    </ref>
  </xsl:template>

  <xsl:template match="text:bookmark-start">
    <anchor type="bookmark-start">
      <xsl:attribute name="xml:id">
	<xsl:text>id_</xsl:text>
	<xsl:value-of select="@text:name"/>
      </xsl:attribute>
    </anchor>
  </xsl:template>

  <xsl:template match="text:bookmark-end">
    <ptr type="bookmark-end">
      <xsl:attribute name="target">
	<xsl:text>#id_</xsl:text>
	<xsl:value-of select="@text:name"/>
      </xsl:attribute>
    </ptr>
  </xsl:template>

  <xsl:template match="text:bookmark">
    <anchor>
      <xsl:attribute name="xml:id">
	<xsl:text>id_</xsl:text>
	<xsl:value-of select="@text:name"/>
      </xsl:attribute>
    </anchor>
  </xsl:template>
  <!--
These seem to have no obvious translation
-->

  <xsl:template match="text:endnotes-configuration"/>

  <xsl:template match="text:file-name"/>

  <xsl:template match="text:footnotes-configuration"/>

  <xsl:template match="text:linenumbering-configuration"/>

  <xsl:template match="text:list-level-style-bullet"/>

  <xsl:template match="text:list-level-style-number"/>

  <xsl:template match="text:list-style"/>

  <xsl:template match="text:outline-level-style"/>

  <xsl:template match="text:outline-style"/>

  <xsl:template match="text:s">
    <xsl:text> </xsl:text>
  </xsl:template>


  <xsl:template match="text:*"> [[[UNTRANSLATED <xsl:value-of
  select="name(.)"/>:     <xsl:apply-templates/>]]] </xsl:template>


  <!-- sections of the OO format we don't need at present -->

  <xsl:template match="office:automatic-styles"/>

  <xsl:template match="office:font-decls"/>

  <xsl:template match="office:meta"/>

  <xsl:template match="office:script"/>

  <xsl:template match="office:settings"/>

  <xsl:template match="office:styles"/>

  <xsl:template match="style:*"/>


  <xsl:template match="dc:*">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="meta:creation-date">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="meta:editing-cycles"/>

  <xsl:template match="meta:editing-duration"/>

  <xsl:template match="meta:generator"/>

  <xsl:template match="meta:user-defined"/>

<xsl:template match="text:section">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text:sequence-decl">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text:sequence-decls">
  <xsl:apply-templates/>
</xsl:template>


<xsl:template match="text:sequence">
  <xsl:apply-templates/>
</xsl:template>


<xsl:template match="text:section-source"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Since soft page breaks are an optional and application specific feature, we disregard them completely.
      See the <a href="http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part1.html#__RefHeading__1419322_253892949">OpenDocument specification</a>
      for further information.
    </desc>
  </doc>
  <xsl:template match="text:soft-page-break"/>

<xsl:template name="stars">
   <xsl:param name="n"/>
   <xsl:if test="$n &gt;0">
     <xsl:text>*</xsl:text>
     <xsl:call-template name="stars">
       <xsl:with-param name="n">
	 <xsl:value-of select="$n - 1"/>
       </xsl:with-param>
     </xsl:call-template>
   </xsl:if>
</xsl:template>

<xsl:template match="text:change|text:changed-region|text:change-end|text:change-start">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text:table-of-content"/>
<xsl:template match="text:index-entry-chapter"/>
<xsl:template match="text:index-entry-page-number"/>
<xsl:template match="text:index-entry-tab-stop"/>
<xsl:template match="text:index-entry-text"/>
<xsl:template match="text:index-title-template"/>
<xsl:template match="text:table-of-content-entry-template"/>
<xsl:template match="text:table-of-content-source"/>

  <xsl:template match="office:text">
    <body>
      <xsl:variable name="Body">
	<HEAD level="1" magic="true">Start</HEAD>
        <xsl:apply-templates/>
      </xsl:variable>
      <!-- debug
      <xsl:result-document href="/tmp/temp.xml">
	<xsl:copy-of select="$Body"/>
      </xsl:result-document>
      -->
      <xsl:variable name="Body2">
	<xsl:for-each select="$Body">
	  <xsl:apply-templates mode="pass1"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="$Body2">
        <xsl:for-each-group select="tei:*" group-starting-with="tei:HEAD[@level='1']">
          <xsl:choose>
            <xsl:when test="self::tei:HEAD[@level='1']">
	      <xsl:call-template name="group-by-section"/>
            </xsl:when>
            <xsl:otherwise>
	      <xsl:call-template name="inSection"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each-group>
      </xsl:for-each>
    </body>
  </xsl:template>

  <xsl:template name="group-by-section">
    <xsl:variable name="ThisHeader" select="number(@level)"/>
    <xsl:variable name="NextHeader" select="number(@level)+1"/>
    <xsl:choose>
      <xsl:when test="@magic">
	  <xsl:for-each-group select="current-group() except ."
			      group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
	    <xsl:choose>
	      <xsl:when test="self::tei:HEAD">
		<xsl:call-template name="group-by-section"/>
	      </xsl:when>
	    <xsl:otherwise>
	      <xsl:call-template name="inSection"/>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
        <!-- 
          hard page breaks before sections are passed on via the
          @page-break-before attribute and need to get injected here
        -->
        <xsl:if test="@page-break-before='true'">
          <pb/>
        </xsl:if>
	<div>
	  <xsl:choose>
	    <xsl:when test="starts-with(@style,'Heading')"/>
	    <xsl:when test="@style">
	      <xsl:attribute name="rend" select="@style"/>
	    </xsl:when>
	  </xsl:choose>
	  <xsl:if test="not(@interpolated='true')">
	    <head>
	      <xsl:apply-templates mode="pass1"/>
	    </head>
	  </xsl:if>
	  <xsl:for-each-group select="current-group() except ."
			      group-starting-with="tei:HEAD[number(@level)=$NextHeader]">
	    <xsl:choose>
	      <xsl:when test="self::tei:HEAD">
		<xsl:call-template name="group-by-section"/>
	      </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="inSection"/>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
	</div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="inSection">
    <xsl:for-each-group select="current-group()"
			group-adjacent="if (self::tei:GLOSS)
					then 1
					else 2">      
      <xsl:choose>
	<xsl:when test="current-grouping-key()=1">
	  <list type="gloss">
	    <xsl:for-each select="current-group()">
	      <xsl:element name="{@n}">
		<xsl:apply-templates mode="pass2"/>
	      </xsl:element>
	    </xsl:for-each>
	  </list>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="current-group()">
	    <xsl:apply-templates select="." mode="pass2"/>
	  </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>
		
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:p[not(node())]" mode="pass1">
  </xsl:template>

  <xsl:template match="tei:HEAD" mode="pass1">
    <xsl:if test="preceding-sibling::tei:HEAD">
      <xsl:variable name="prev"
		    select="xs:integer(number(preceding-sibling::tei:HEAD[1]/@level))"/>
      <xsl:variable name="current"
		    select="xs:integer(number(@level))"/>
	<xsl:if test="($current - $prev) &gt;1 ">
	  <!--<xsl:message>Walk from <xsl:value-of select="$prev"/> to <xsl:value-of select="$current"/></xsl:message>-->
	  <xsl:for-each
	      select="$prev + 1   to $current - 1 ">
	    <HEAD interpolated='true' level="{.}"/>
	  </xsl:for-each>
	</xsl:if>
    </xsl:if>
    <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()"
	mode="pass1"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*" mode="pass1">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass1"/>
    </xsl:copy>
  </xsl:template>


  <!-- second pass -->


  <xsl:template match="tei:p[not(*) and normalize-space(.)='']"
		mode="pass2">
  </xsl:template>
  
  <xsl:template match="@*|comment()|processing-instruction()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>
  
  
  <xsl:template match="text()" mode="pass2">
    <xsl:value-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="tei:title" mode="pass2">
    <xsl:choose>
      <xsl:when test="parent::tei:div|parent::tei:body">
	<head>
	  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
	</head>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- third pass -->



    <xsl:template match="@*|comment()|processing-instruction()" mode="pass3">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="pass3">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
        </xsl:copy>
    </xsl:template>
    
    
    <xsl:template match="text()" mode="pass3">
        <xsl:value-of select="."/>
    </xsl:template>
    
    <xsl:template match="tei:div[not(@type)]" mode="pass3">
      <div type="div{count(ancestor-or-self::tei:div)}">
	<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
      </div>
    </xsl:template>
    
    <xsl:function name="tei:inlineStyles"  as="xs:string">
      <xsl:param name="name"/>
      <xsl:param name="context"/>
      <xsl:choose>
	<xsl:when test="starts-with($name,'tei_5f_')">
	  <xsl:value-of select="substring($name,8)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:variable name="r">
	    <xsl:for-each select="$context/key('STYLES',$name)">
	      <xsl:if
		  test="style:paragraph-properties/@fo:text-align">
		<xsl:value-of
		    select="style:paragraph-properties/@fo:text-align"/>
		<xsl:text> </xsl:text>
	  </xsl:if>	  
	  <xsl:if
	      test="style:text-properties[starts-with(@style:text-position,'super')]">
	    <xsl:text>sup </xsl:text>
	  </xsl:if>
	  
	  <xsl:if test="style:text-properties/@fo:color and not(style:text-properties/@fo:color='transparent')">
	    <xsl:text> color(</xsl:text>
	    <xsl:value-of select="style:text-properties/@fo:color"/>
	    <xsl:text>)</xsl:text>
	  </xsl:if>
	  
	  <xsl:if test="style:text-properties/@fo:background-color and not(style:text-properties/@fo:background-color='transparent')">
	    <xsl:text> background-color(</xsl:text>
	    <xsl:value-of select="style:text-properties/@fo:background-color"/>
	    <xsl:text>)</xsl:text>
	  </xsl:if>
	  
	  <xsl:if
	      test="style:text-properties[starts-with(@style:text-position,'sub')]">
	    <xsl:text>sub </xsl:text>
	  </xsl:if>
	  
	  <xsl:if test="style:text-properties[@fo:font-weight='bold']">
	    <xsl:text>bold </xsl:text>
	 </xsl:if>
	 
	 <xsl:if
	     test="style:text-properties[@style:text-underline-type='double']">
	   <xsl:text>doubleunderline </xsl:text>
	 </xsl:if>
	 
	 <xsl:if
	     test="style:text-properties[@style:text-underline-style='solid']">
	   <xsl:text>underline </xsl:text>
	 </xsl:if>
	 
	 <xsl:if
	     test="style:text-properties[@style:text-line-through-style='solid']">
	   <xsl:text>strikethrough </xsl:text>
	 </xsl:if>
	 
	 <xsl:if
	     test="style:text-properties[@fo:font-variant='small-caps']">
	   <xsl:text>smallcaps </xsl:text>
	 </xsl:if>
	 
	 <xsl:if test="style:text-properties[@fo:font-style='italic']">
	   <xsl:text>italic </xsl:text>
	 </xsl:if>
	 
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:value-of select="normalize-space($r)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Check whether the given element (e.g. text:p) refers to a style that implies a hard page break before.
    </desc>
    <param name="curNode">The element for which to check the style. 
      The element's attribute @text:style-name or @table:style-name is used for looking up the style</param>
    <return>A tei:pb element if the lookup was succesful, the empty sequence otherwise</return>
  </doc>
  <xsl:function name="tei:pagebreak-before" as="element(tei:pb)?">
    <xsl:param name="curNode" as="element()?"/>
    <xsl:if test="($curNode/@text:style-name or $curNode/@table:style-name) and 
      key('STYLES', 
        ($curNode/@text:style-name, $curNode/@table:style-name), 
        $curNode/root()
        )/(style:paragraph-properties, style:table-properties)[@fo:break-before='page']">
      <pb/>
    </xsl:if>
  </xsl:function>
  
</xsl:stylesheet>

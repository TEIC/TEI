<?xml version="1.0" encoding="UTF-8"?>
<!--
 #  The Contents of this file are made available subject to the terms of
 # the GNU Lesser General Public License Version 2.1

 # Sebastian Rahtz / University of Oxford
 # copyright 2010

 # This stylesheet is derived from the OpenOffice to Docbook conversion
 #  Sun Microsystems Inc., October, 2000

 #  GNU Lesser General Public License Version 2.1
 #  =============================================
 #  Copyright 2000 by Sun Microsystems, Inc.
 #  901 San Antonio Road, Palo Alto, CA 94303, USA
 #
 #  This library is free software; you can redistribute it and/or
 #  modify it under the terms of the GNU Lesser General Public
 #  License version 2.1, as published by the Free Software Foundation.
 #
 #  This library is distributed in the hope that it will be useful,
 #  but WITHOUT ANY WARRANTY; without even the implied warranty of
 #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 #  Lesser General Public License for more details.
 #
 #  You should have received a copy of the GNU Lesser General Public
 #  License along with this library; if not, write to the Free Software
 #  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 #  MA  02111-1307  USA
 #
 #
-->
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

  <xsl:key match="style:style" name="STYLES" use="@style:name"/>

  <xsl:key match="text:h" name="Headings" use="text:outline-level"/>

  <xsl:param name="debug">false</xsl:param>

  <xsl:param name="dir">.</xsl:param>

  <xsl:output encoding="utf-8" indent="yes"/>

  <!--  <xsl:strip-space elements="text:span"/>-->
  
  <xsl:variable name="META">
    <xsl:if test="$debug='true'">
      <xsl:message>Look for metadata in <xsl:value-of select="concat($dir,'/meta.xml')"/></xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="doc-available(concat($dir,'/meta.xml'))">
        <xsl:copy-of select="document(concat($dir,'/meta.xml'))/office:document-meta/office:meta"/>
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


  <xsl:template match="/office:document-content">
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
      <xsl:apply-templates/>
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
          <authority/>
        </publicationStmt>
        <sourceDesc>
          <p>
	    <xsl:apply-templates
              select="$META/office:meta/meta:generator"/>
	    <xsl:text>Written by OpenOffice</xsl:text>
	  </p>
        </sourceDesc>
      </fileDesc>
      <xsl:if test="$META/office:meta/dc:language|$META/office:meta/meta:keyword">
	<profileDesc>
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
	      <keywords>
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
	</profileDesc>
      </xsl:if>
      <revisionDesc>
	<change>
	  <name>
	    <xsl:apply-templates
		select="$META/office:meta/dc:creator"/>
	  </name>
	  <date>
	    <xsl:apply-templates select="$META/office:meta/dc:date"/>
	  </date>
	</change>
      </revisionDesc>
    </teiHeader>
  </xsl:template>



  <xsl:template match="/office:document-content/office:body">
    <text>
      <xsl:apply-templates/>
    </text>
  </xsl:template>

  <!-- sections -->
  <xsl:template match="text:h">
    <xsl:choose>
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
    <HEAD level="{@text:outline-level}" style="{@text:style-name}">
      <xsl:apply-templates/>
    </HEAD>
  </xsl:template>


  
  <!-- special case paragraphs -->
  <xsl:template match="text:p[@text:style-name='XMLComment']">
    <xsl:comment>
      <xsl:value-of select="."/>
    </xsl:comment>
  </xsl:template>
  

  <xsl:template match="text:p[@text:style-name]">

    <xsl:choose>
      <xsl:when test="draw:frame and parent::draw:text-box">
	<xsl:apply-templates select="draw:frame"/>
	<head>
	  <xsl:apply-templates select="text()|*[not(local-name(.)='frame')]"/>
	</head>
      </xsl:when>
      <xsl:when test="parent::table:table-cell">
	<xsl:call-template name="applyStyle"/>
      </xsl:when>

      <xsl:when test="count(parent::text:note-body/text:p)=1">
          <xsl:apply-templates/>
      </xsl:when>

      <xsl:when test="count(parent::text:list-item/text:p)=1">
          <xsl:apply-templates/>
      </xsl:when>

      <xsl:when test="@text:style-name='Document Title'">
        <title>
          <xsl:apply-templates/>
        </title>
      </xsl:when>

      <xsl:when test="@text:style-name='Author'">
        <author>
          <xsl:apply-templates/>
        </author>
      </xsl:when>

      <xsl:when test="@text:style-name='lg'">
        <lg>
          <xsl:apply-templates/>
        </lg>
      </xsl:when>
      <xsl:when test="@text:style-name='Title'">
        <title>
          <xsl:apply-templates/>
        </title>
      </xsl:when>
      <xsl:when test="@text:style-name='Date'">
        <date>
          <xsl:apply-templates/>
        </date>
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
      <xsl:when test="@text:style-name='Screen'">
        <Screen>
          <xsl:apply-templates/>
        </Screen>
      </xsl:when>
      <xsl:when test="@text:style-name='Output'">
        <Output>
          <xsl:apply-templates/>
        </Output>
      </xsl:when>

      <xsl:otherwise>
        <p>
          <xsl:apply-templates/>
        </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="office:annotation/text:p">
    <note>
        <xsl:apply-templates/>
    </note>
  </xsl:template>


  <!-- normal paragraphs -->
  <xsl:template match="text:p">
    <xsl:choose>
      <xsl:when test="parent::text:list-item">
        <xsl:call-template name="applyStyle"/>
      </xsl:when>
      <xsl:when test="parent::table:table-cell">
        <xsl:call-template name="applyStyle"/>
      </xsl:when>
      <xsl:when test="@text:style-name='Table'"/>
      <xsl:when test="text:span[@text:style-name = 'XrefLabel']"/>
      <xsl:when test="@text:style-name='Speech'">
        <sp>
          <speaker/>
          <p>
            <xsl:call-template name="id.attribute"/>
            <xsl:call-template name="applyStyle"/>
          </p>
        </sp>
      </xsl:when>
      <xsl:otherwise>
        <p>
          <xsl:call-template name="id.attribute"/>
          <xsl:call-template name="applyStyle"/>
        </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- lists -->
  <xsl:template match="text:list">
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
      <xsl:when test="starts-with(@text:style-name,'P') or starts-with(text:list-item[1]/@text:style-name,'P')">
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
    match="text:p[@text:style-name='VarList Item' or @text:style-name='List Contents']">
    <xsl:if
      test="not(preceding-sibling::text:p[@text:style-name='VarList Item'])">
      <xsl:text disable-output-escaping="yes">&lt;item&gt;</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:if
      test="not(following-sibling::text:p[@text:style-name='VarList Item'])">
      <xsl:text disable-output-escaping="yes">&lt;/item&gt;</xsl:text>
    </xsl:if>
    <xsl:variable name="next">
      <xsl:for-each select="following-sibling::text:p[1]">
        <xsl:value-of select="@text:style-name"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$next='VarList Term'"/>
      <xsl:when test="$next='List Heading'"/>
      <xsl:when test="$next='VarList Item'"/>
      <xsl:when test="$next='List Contents'"/>
      <xsl:otherwise>
        <xsl:text disable-output-escaping="yes">&lt;/list&gt;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>

  </xsl:template>

  <xsl:template
    match="text:p[@text:style-name='VarList Term' or @text:style-name='List Heading']">
    <xsl:variable name="prev">
      <xsl:for-each select="preceding-sibling::text:p[1]">
        <xsl:value-of select="@text:style-name"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$prev='VarList Term'"/>
      <xsl:when test="$prev='List Heading'"/>
      <xsl:when test="$prev='VarList Item'"/>
      <xsl:when test="$prev='List Contents'"/>
      <xsl:otherwise>
        <xsl:text disable-output-escaping="yes">&lt;list type="gloss"&gt;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <label>
      <xsl:apply-templates/>
    </label>
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
        <hi>
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
      <xsl:when test="$Style='date'">
        <date>
          <xsl:apply-templates/>
        </date>
      </xsl:when>
      <xsl:when test="$Style='l'">
        <l>
          <xsl:apply-templates/>
        </l>
      </xsl:when>
      <xsl:when test="$Style='Filespec'">
        <Filespec>
          <xsl:apply-templates/>
        </Filespec>
      </xsl:when>
      <xsl:when test="$Style='gi'">
        <gi>
          <xsl:apply-templates/>
        </gi>
      </xsl:when>
      <xsl:when test="$Style='Code'">
        <Code>
          <xsl:apply-templates/>
        </Code>
      </xsl:when>
      <xsl:when test="$Style='Input'">
        <Input>
          <xsl:apply-templates/>
        </Input>
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
      <xsl:value-of select="@text:style-name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length(.)=0"/>
      <xsl:when test="text:note">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="key('STYLES',$name)">
        <xsl:variable name="contents">
          <xsl:apply-templates/>
        </xsl:variable>
        <xsl:for-each select="key('STYLES',$name)">

    <!--! <xsl:for-each select="style:text-properties/@*">
    <xsl:value-of select="name(.)"/>:        <xsl:value-of select="."/>&#10;
    </xsl:for-each>
    -->

          <xsl:choose>
            <xsl:when
              test="style:text-properties[starts-with(@style:text-position,'super')]">
              <hi rend="sup">
                <xsl:copy-of select="$contents"/>
              </hi>
            </xsl:when>
            <xsl:when
              test="style:text-properties[starts-with(@style:text-position,'sub')]">
              <hi rend="sub">
                <xsl:copy-of select="$contents"/>
              </hi>
            </xsl:when>
            <xsl:when test="style:text-properties[@fo:font-weight='bold']">
              <hi>
                <xsl:copy-of select="$contents"/>
              </hi>
            </xsl:when>
            <xsl:when
              test="style:text-properties[style:text-underline-style='solid']">
              <hi rend="underline">
                <xsl:copy-of select="$contents"/>
              </hi>
            </xsl:when>
            <xsl:when test="style:text-properties[@fo:font-style='italic']">
              <emph>
                <xsl:copy-of select="$contents"/>
              </emph>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$contents"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- tables -->
  <xsl:template match="table:table">
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
    <binaryObject mimeType="image/jpg">
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
    <xsl:if test="not(parent::text:span[@text:style-name='l'])">
      <lb/>
    </xsl:if>
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
        <xsl:if test="key('secondary_children', generate-id())">
          <index>
	    <term>
            <xsl:value-of select="key('secondary_children', generate-id())"/>
	    </term>
	  </index>
        </xsl:if>
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
    <anchor type="bookmark-end">
      <xsl:attribute name="corresp">
	<xsl:text>#id_</xsl:text>
	<xsl:value-of select="@text:name"/>
      </xsl:attribute>
    </anchor>
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

  <xsl:template match="text:s"/>


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

  <!--
<xsl:template match="text()">
  <xsl:apply-templates select="normalize-space(.)"/>
</xsl:template>
-->

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


<xsl:template match="text:soft-page-break">
</xsl:template>

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
      <xsl:variable name="Body2">
	<xsl:for-each select="$Body">
		<xsl:apply-templates select="." mode="pass1"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:for-each select="$Body2">
        <xsl:for-each-group select="tei:*" group-starting-with="tei:HEAD[@level='1']">
          <xsl:choose>
            <xsl:when test="self::tei:HEAD[@level='1']">
	      <xsl:call-template name="group-by-section"/>
            </xsl:when>
            <xsl:otherwise>
	      <xsl:for-each select="current-group()">
		<xsl:apply-templates select="." mode="pass1"/>
	      </xsl:for-each>
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
	      <xsl:for-each select="current-group()">
		<xsl:apply-templates select="." mode="pass1"/>
	      </xsl:for-each>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
	<div>
	  <xsl:if test="@style">
	    <xsl:attribute name="rend" select="@style"/>
	  </xsl:if>
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
	      <xsl:for-each select="current-group()">
		<xsl:apply-templates select="." mode="pass1"/>
	      </xsl:for-each>
	    </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
	</div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:p[not(node())]"
		mode="pass1"/>

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

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:p[not(*) and normalize-space(.)='']"
		mode="pass2"/>

  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
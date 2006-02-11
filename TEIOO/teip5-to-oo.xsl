<?xml version="1.0" encoding="UTF-8"?>
<!--
#  The Contents of this file are made available subject to the terms of
# the GNU Lesser General Public License Version 2.1

# Sebastian Rahtz / University of Oxford
# copyright 2003

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
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    office:version="1.0"
    >

  <xsl:output method="xml" omit-xml-declaration="no"/>

  <xsl:decimal-format name="staff" digit="D"/>
  <xsl:variable name="doc_type">TEI</xsl:variable>
  
  <xsl:key name='IDS' match="tei:*[@id]" use="@id"/>
  
  <xsl:template match="/">
    <office:document>
      <office:meta>
	<meta:generator>Open Office TEI to OO XSLT</meta:generator>
	<dc:title>
	  <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
	</dc:title>
	<dc:description/>
	<dc:subject/>
	<meta:creation-date>
	  <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition/tei:date"/>
	</meta:creation-date>
	<dc:date>
	  <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:date"/>
	</dc:date>
	<dc:language>
	  <xsl:value-of select="/tei:TEI/@xml:lang"/>
	</dc:language>
	<meta:editing-cycles>21</meta:editing-cycles>
	<meta:editing-duration>P1DT0H11M54S</meta:editing-duration>
	<meta:user-defined meta:name="Info 1"/>
	<meta:user-defined meta:name="Info 2"/>
	<meta:user-defined meta:name="Info 3"/>
	<meta:user-defined meta:name="Info 4"/>
	<meta:document-statistic meta:table-count="1" meta:image-count="0" meta:object-count="0" meta:page-count="1" meta:paragraph-count="42" meta:word-count="144" meta:character-count="820"/>
      </office:meta>
      <!--
	  <xsl:copy-of select="document('styles.xml')/office:document-styles"/>
      -->
      <office:body>
	<office:text>
	  <xsl:apply-templates select="(.//tei:TEI|tei:text|tei:div)[1]"/>
	</office:text>
      </office:body>
    </office:document>
  </xsl:template>
  
  <xsl:template match="tei:TEI">
    <xsl:apply-templates select="tei:text"/>
  </xsl:template>

  <xsl:template name="entities">
    <text:variable-decls>
      <xsl:for-each select="/descendant::entity">
	<xsl:variable name="entname">
	  <xsl:value-of select="@name"/>
	</xsl:variable>
	<xsl:if test="not(preceding::entity[@name = $entname])">
	  <text:variable-decl>
	    <xsl:attribute name="text:value-type">
	      <xsl:text>string</xsl:text>
	    </xsl:attribute>
	    <xsl:attribute name="text:name">
	      <xsl:text>entitydecl_</xsl:text>
	      <xsl:value-of select="@name"/>
	    </xsl:attribute>
	  </text:variable-decl>
	</xsl:if>
      </xsl:for-each>
    </text:variable-decls>
  </xsl:template>
  
  
  <xsl:template match="tei:entity">
    <xsl:variable name="entname">
      <xsl:value-of select="@name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(preceding::tei:entity[@name = $entname])">
	<text:variable-set>
	  <xsl:attribute name="text:value-type">
	    <xsl:text>string</xsl:text>
	  </xsl:attribute>
	  <xsl:attribute name="text:name">
	    <xsl:text>entitydecl_</xsl:text>
	    <xsl:value-of select="@name"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</text:variable-set>
      </xsl:when>
      <xsl:otherwise>
	<text:variable-get>
	  <xsl:attribute name="text:value-type">
	    <xsl:text>string</xsl:text>
	  </xsl:attribute>
	  <xsl:attribute name="text:name">
	    <xsl:text>entitydecl_</xsl:text>
	    <xsl:value-of select="@name"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</text:variable-get>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- table start -->
  
  
  <xsl:template match="tei:table">
    <xsl:call-template name="startHook"/>
    <xsl:variable name="tablenum">
      <xsl:choose>
	<xsl:when test="@xml:id"><xsl:value-of select="@xml:id"/></xsl:when>
	<xsl:otherwise>table<xsl:number level="any"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <table:table 
	table:name="{$tablenum}"
	table:style-name="Table1">
      <table:table-column 
	  table:style-name="Table1.col1">
	<xsl:attribute name="table:number-columns-repeated">
	  <xsl:value-of select="count(tei:row[1]/tei:cell)"/>
	</xsl:attribute>
      </table:table-column>
      <xsl:apply-templates/>
    </table:table>
    <xsl:if test="head">
      <text:p text:style-name="Table">
	<xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  
  <xsl:template match="tei:row[@role='label']">
    <table:table-header-rows>
      <table:table-row>
	<xsl:apply-templates/>
      </table:table-row>
    </table:table-header-rows>
  </xsl:template>
  
  
  <xsl:template match="tei:row">
    <table:table-row>
      <xsl:apply-templates/>
    </table:table-row>
  </xsl:template>
  
  
  <xsl:template match="tei:cell">
    <table:table-cell>
      <xsl:choose>
	<xsl:when test="parent::tei:row[@role='label']">
	  <xsl:attribute name="text:style-name">Table1.cellheading</xsl:attribute>
	</xsl:when>
	<xsl:when test="@role='label'">
	  <xsl:attribute name="text:style-name">Table1.cellheading</xsl:attribute>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="text:style-name">Table1.cellcontents</xsl:attribute>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
	<xsl:when test="not(child::tei:p)">
	  <text:p>
	    <xsl:choose>
	      <xsl:when test="parent::tei:row[@role='label']">
		<xsl:attribute name="text:style-name">Table Heading</xsl:attribute>
	      </xsl:when>
	      <xsl:when test="@role='label'">
		<xsl:attribute name="text:style-name">Table Heading</xsl:attribute>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:attribute name="text:style-name">Table Contents</xsl:attribute>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:apply-templates/>
	  </text:p>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </table:table-cell>
  </xsl:template>
  
  
  <xsl:template match="tei:head">
    <xsl:choose>
      <xsl:when test="parent::tei:figure"/>
      <xsl:when test="parent::tei:list"/>
      <xsl:when test="parent::tei:div"/>
      <xsl:when test="parent::tei:div0"/>
      <xsl:when test="parent::tei:div1"/>
      <xsl:when test="parent::tei:div2"/>
      <xsl:when test="parent::tei:div3"/>
      <xsl:when test="parent::tei:div4"/>
      <xsl:when test="parent::tei:div5"/>
      <xsl:when test="parent::tei:table">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<text:p>
	  <xsl:choose>
	    <xsl:when test="parent::tei:appendix">
	      <xsl:attribute name="text:style-name">Appendix Title</xsl:attribute>
	    </xsl:when>
	  </xsl:choose>
	  <xsl:apply-templates/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  
  <xsl:template match="tei:author">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:p">
    <text:p>
      <xsl:choose>
	<xsl:when test="ancestor::tei:note[@place='foot']">
	  <xsl:attribute name="text:style-name">
	    <xsl:text>Footnote</xsl:text>
	  </xsl:attribute>
	</xsl:when>
	<xsl:when test="ancestor::tei:row[@role='label']">
	  <xsl:attribute name="text:style-name">Table Heading</xsl:attribute>
	</xsl:when>
	<xsl:when test="ancestor::tei:row">
	  <xsl:attribute name="text:style-name">Table Contents</xsl:attribute>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="text:style-name">Text body</xsl:attribute>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  
  
  <xsl:template match="tei:div">
    <xsl:variable name="depth">
      <xsl:value-of select="count(ancestor::tei:div)"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:level">
	<xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:attribute name="text:style-name">
	<xsl:text>Heading </xsl:text><xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5">
    <xsl:variable name="depth">
      <xsl:value-of select="substring-after(name(.),'div')"/>
    </xsl:variable>
    <text:h>
      <xsl:attribute name="text:level">
	<xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:attribute name="text:style-name">
	<xsl:text>Heading </xsl:text><xsl:value-of select="$depth + 1"/>
      </xsl:attribute>
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:head" mode="show"/>
    </text:h>
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:figure">
    <xsl:call-template name="startHook"/>
    <text:p text:style-name="Standard">
      <xsl:call-template name="test.id"/>
      <xsl:apply-templates select="tei:graphic"/>
    </text:p>
    <xsl:if test="tei:head">
      <text:p text:style-name="Caption">
	<text:span text:style-name="Figurenum">Figure <text:sequence
	text:ref-name="refFigure0" text:name="Figure"
	text:formula="Figure+1"
	style:num-format="1"><xsl:number level="any"/>. </text:sequence>
	</text:span>
	<xsl:apply-templates select="tei:head" mode="show"/>
      </text:p>
    </xsl:if>
    <xsl:call-template name="endHook"/>
    
  </xsl:template>
  
  <xsl:template match="tei:graphic">
    <xsl:variable name="id">
      <xsl:choose>
	<xsl:when test="@xml:id">
	  <xsl:value-of select="@xml:id"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>Figure</xsl:text><xsl:number level="any"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
      <draw:image 
	  draw:style-name="fr3" 
	  draw:name="{$id}" 
	  svg:x="0inch" 
	  svg:y="0inch" 
	  draw:z-index="3" 
	  xlink:type="simple" 
	  text:anchor-type="as-char"
	  xlink:show="embed" 
	  xlink:actuate="onLoad" >
	<xsl:attribute name="xlink:href">
	  <xsl:value-of select="@url"/>
	</xsl:attribute>
	<xsl:attribute name="style:rel-width">scale</xsl:attribute>
	<xsl:attribute name="style:rel-height">scale</xsl:attribute>
	<xsl:attribute name="svg:width">
	  <xsl:choose>
	    <xsl:when test="@width">
	      <xsl:value-of select="@width"/>
	      <xsl:call-template name="checkunit">
		<xsl:with-param name="dim" select="@width"/>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>6inch</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
	<xsl:attribute name="svg:height">
	  <xsl:choose>
	    <xsl:when test="@height">
	      <xsl:value-of select="@height"/>
	      <xsl:call-template name="checkunit">
		<xsl:with-param name="dim" select="@height"/>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>6inch</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
      </draw:image>
  </xsl:template>

  <xsl:template match="tei:list">
    <xsl:call-template name="startHook"/>
    <xsl:if test="head">
      <text:p><xsl:apply-templates select="tei:head" mode="show"/></text:p>
    </xsl:if>
    <text:unordered-list text:style-name="Unordered List">
      <xsl:apply-templates/>
    </text:unordered-list>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  
  <xsl:template match="tei:list[@type='unordered']">
    <xsl:call-template name="startHook"/>
    <text:unordered-list text:style-name="Unordered List">
      <xsl:apply-templates/>
    </text:unordered-list>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  
  <xsl:template match="tei:list[@type='ordered']">
    <xsl:call-template name="startHook"/>
    <text:ordered-list text:style-name="Ordered List">
      <xsl:apply-templates/>
    </text:ordered-list>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  <xsl:template match="tei:list[@type='gloss']">
    <xsl:call-template name="startHook"/>
    <xsl:apply-templates/>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  <xsl:template match="tei:list[@type='gloss']/tei:item">
    <text:p text:style-name="List Contents">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  
  <xsl:template match="tei:list[@type='gloss']/tei:label">
    <text:p text:style-name="List Heading">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  
  <xsl:template name="startHook">
    <xsl:if test="parent::tei:p">
      <xsl:text disable-output-escaping="yes">&lt;/text:p&gt;</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="endHook">
    <xsl:if test="parent::tei:p">
      <xsl:text disable-output-escaping="yes">&lt;text:p&gt;</xsl:text>
    </xsl:if>
  </xsl:template>
  
  
  
  <xsl:template match="tei:item/tei:p">
    <xsl:apply-templates/>
  </xsl:template>
  
  
  <xsl:template match="tei:item">
    <text:list-item>
      <xsl:choose>
	<xsl:when test="list">
	  <xsl:apply-templates/>              
	</xsl:when>
	<xsl:otherwise>
	  <text:p>
	    <xsl:apply-templates/>
	  </text:p>
	</xsl:otherwise>
      </xsl:choose>
    </text:list-item>
  </xsl:template>
  
  
  <xsl:template match="tei:emph">
    <text:span text:style-name="Emphasis">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  
  <xsl:template match="tei:q">
    <text:span text:style-name="q">
      <xsl:text>&#x2018;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&#x2019;</xsl:text>
    </text:span>
  </xsl:template>
  
  
  <xsl:template match="tei:note[@place='foot']">
    <text:footnote-citation>
      <xsl:number level="any" count="tei:note[@place='foot']"/>
    </text:footnote-citation>
    <text:footnote>
      <text:footnote-body>
	<text:p text:style-name="Footnote">
	  <xsl:apply-templates/>
	</text:p>
      </text:footnote-body>
    </text:footnote>
  </xsl:template>
  
  <xsl:template match="tei:note[@place='end']">
    <text:endnote-citation>
      <xsl:number format="i" level="any" count="tei:note[@place='end']"/>
    </text:endnote-citation>
    <text:endnote>
      <text:endnote-body>
	<text:p text:style-name="Endnote">
	  <xsl:apply-templates/>
	</text:p>
      </text:endnote-body>
    </text:endnote>
  </xsl:template>
  
  
  <xsl:template match="tei:ref">
    <text:a xlink:type="simple" xlink:href="{@target}">
      <xsl:apply-templates/>
    </text:a>
  </xsl:template>
  
  <xsl:template match="tei:ptr">
    <text:a xlink:type="simple" xlink:href="{@target}">
      <xsl:choose>
	<xsl:when test="starts-with(@target,'#')">
	  <xsl:for-each select="key('IDS',substring-after(@target,'#'))">
	    <xsl:apply-templates select="." mode="crossref"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="@target"/>
	</xsl:otherwise>
      </xsl:choose>
    </text:a>
  </xsl:template>
  
  <xsl:template match="tei:table|tei:figure|tei:item" mode="crossref">
    <xsl:number level="any"/>
  </xsl:template>
  
  <xsl:template match="tei:div" mode="crossref">
    <xsl:number format="1.1.1.1.1" 
		level="multiple" count="tei:div"/>
  </xsl:template>
  
  <xsl:template name="test.id">
    <xsl:if test="@xml:id">
      <text:bookmark text:name="{@xml:id}"/>
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="tei:note">
    <office:annotation>
      <text:p>
	<xsl:apply-templates/>
      </text:p>
    </office:annotation>
  </xsl:template>
  
  
  <xsl:template match="tei:hi">
    <text:span>
      <xsl:attribute name="text:style-name">
	<xsl:choose>
	  <xsl:when test="@rend='sup'">
	    <xsl:text>SuperScript</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sub'">
	    <xsl:text>SubScript</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='bold'">
	    <xsl:text>Highlight</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='it'">
	    <xsl:text>Emphasis</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='i'">
	    <xsl:text>Emphasis</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='ul'">
	    <xsl:text>Underline</xsl:text>
	  </xsl:when>
	  <xsl:when test="@rend='sc'">
	    <xsl:text>SmallCaps</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>Highlight</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  
  <xsl:template match="tei:date">
    <text:span text:style-name="{name(.)}">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>
  
  
  <xsl:template match="tei:eg">
    <xsl:call-template name="startHook"/>
    <xsl:call-template name="Literal">
      <xsl:with-param name="Text">
	<xsl:value-of select="."/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="endHook"/>
  </xsl:template>
  
  <!-- safest to drop comments entirely, I think -->
  <xsl:template match="comment()"/>
  
  <xsl:template match="tei:head" mode="show">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:lb">
    <text:line-break/>
  </xsl:template>
  
  <xsl:template name="checkunit">
    <xsl:param name="dim"/>
    <xsl:if test="contains($dim,'in')">
      <xsl:text>ch</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <!-- curiously, no apparent markup for a page break -->
  <xsl:template match="tei:pb"/>
  
  <xsl:template match="tei:lg|tei:bibl|tei:signed">
    <text:p text:style-name="{name(.)}">
      <xsl:apply-templates/>
    </text:p>
  </xsl:template>
  
  <xsl:template match="tei:l">
    <text:span text:style-name="l">
      <xsl:apply-templates/>  
      <text:line-break/>
    </text:span>
  </xsl:template>
  
  <xsl:template name="Literal">
    <xsl:param name="Text"/>
    <xsl:choose>
      <xsl:when test="contains($Text,'&#10;')">
	<text:p text:style-name="Preformatted Text">      
	  <xsl:value-of 
	      select="translate(substring-before($Text,'&#10;'),' ','&#160;')"/>
	</text:p>
	<xsl:call-template name="Literal">
	  <xsl:with-param name="Text">
	    <xsl:value-of select="substring-after($Text,'&#10;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<text:p text:style-name="Preformatted Text">      
	  <xsl:value-of select="translate($Text,' ','&#160;')"/>
	</text:p>
      </xsl:otherwise>
    </xsl:choose>
    <!-- text:s c="6" to ident 6 spaces -->
  </xsl:template>


  <xsl:template match="tei:sp">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:stage">
    <text:span text:style-name="Stage">
      <xsl:apply-templates/>
    </text:span>
  </xsl:template>

  <xsl:template match="tei:speaker">
      <text:p text:style-name="Speaker">
	<xsl:apply-templates/>
      </text:p>
  </xsl:template>

  <xsl:template match="tei:body|tei:front|tei:back">
	<xsl:apply-templates/>
  </xsl:template>
</xsl:stylesheet>
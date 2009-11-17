<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
	xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:iso="http://www.iso.org/ns/1.0"
	xmlns:its="http://www.w3.org/2005/11/its" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0"
	version="2.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
	xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
	xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	exclude-result-prefixes="teidocx cals xd ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn its">
	<!-- import conversion style -->
	<xsl:import href="../../../docx/to/to.xsl" />
	<xsl:import href="../isoutils.xsl" />

	<!-- import functions -->
	<xsl:include href="iso-functions.xsl" />

	<doc type="stylesheet" xmlns="http://www.pnp-software.com/XSLTdoc">
		<short>TEI stylesheet to convert TEI XML to Word DOCX XML.</short>
		<detail>
			This library is free software; you can redistribute it and/or
			modify it under the terms of the GNU Lesser General Public
			License as
			published by the Free Software Foundation; either
			version 2.1 of the
			License, or (at your option) any later version.

			This library is
			distributed in the hope that it will be useful,
			but WITHOUT ANY
			WARRANTY; without even the implied warranty of
			MERCHANTABILITY or
			FITNESS FOR A PARTICULAR PURPOSE. See the GNU
			Lesser General Public
			License for more details.

			You should have received a copy of the GNU
			Lesser General Public
			License along with this library; if not, write
			to the Free Software
			Foundation, Inc., 59 Temple Place, Suite 330,
			Boston, MA 02111-1307
			USA
  
      </detail>
		<author>See AUTHORS</author>
		<cvsId>$Id: to.xsl 6832 2009-10-12 22:42:59Z rahtz $</cvsId>
		<copyright>2008, TEI Consortium</copyright>
	</doc>

	<xsl:param name="header-doc" as="item()+" required="yes"></xsl:param>
	<xsl:param name="document-doc"></xsl:param>
	<xsl:param name="debug">true</xsl:param>

	<!-- identity transform -->

	<xsl:template match="@*|text()|comment()|processing-instruction()">
		<xsl:copy-of select="." />
	</xsl:template>

	<xsl:template match="*">
		<xsl:copy>
			<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" />
		</xsl:copy>
	</xsl:template>

  <xsl:template match="w:placeholder|w:showingPlcHdr">
    <!-- remove placeholder references -->
  </xsl:template>
  
	<xsl:template match="w:sdtContent//w:t">
		<xsl:variable name="alias" select="ancestor::w:sdt/w:sdtPr/w:tag/@w:val" />
	  <xsl:copy>
	    <xsl:apply-templates select="@*"/>
	    <xsl:variable name="content">
	      <xsl:for-each select="$header-doc/*">
	        <xsl:value-of select="key('ISOMETA',$alias)" />
	      </xsl:for-each>
	    </xsl:variable>
	    <xsl:if test="$debug='true'">
	      <xsl:message select="concat('set sdtContent ',$alias,'=', $content)"/>
	    </xsl:if> 
	    <xsl:choose>
	    <xsl:when test="ancestor::w:sdt/w:sdtPr/w:dropDownList">
	       <xsl:value-of select="ancestor::w:sdt/w:sdtPr/w:dropDownList//w:listItem[@w:value=$content]/@w:displayText"/>
	    </xsl:when>
	    <xsl:otherwise>
	       <xsl:value-of select="$content"/>
	    </xsl:otherwise>
			</xsl:choose>
		</xsl:copy>
	</xsl:template>


	<xsl:template match="w:body">
		<xsl:copy>
		  <xsl:apply-templates select="@*" />
		  <xsl:for-each select="child::*">
		    <xsl:choose>
		      <xsl:when test="name(.) = 'w:p' and .//w:t/text() = '#front'">
		        <xsl:if test="$debug = 'true'">
		          <xsl:message>insert front section</xsl:message>
		        </xsl:if>
		        <xsl:copy-of select="$document-doc/w:document/w:body/front/*" />
		      </xsl:when>
		      <xsl:when test="name(.) = 'w:p' and .//w:t/text() = '#main'">
		        <xsl:if test="$debug = 'true'">
		          <xsl:message>insert main section</xsl:message>
		        </xsl:if>
		        <xsl:copy-of select="$document-doc/w:document/w:body/main/*" />
		      </xsl:when>
		      <xsl:when test="name(.) = 'w:p' and .//w:t/text() = '#back'">
		        <xsl:if test="$debug = 'true'">
		          <xsl:message>insert back section</xsl:message>
		        </xsl:if>
		        <xsl:copy-of select="$document-doc/w:document/w:body/back/*" />
		      </xsl:when>
          <xsl:when test="name(.) = 'w:p' and .//w:t/text() = '#remove'">
            <xsl:if test="$debug = 'true'">
              <xsl:message>remove empty placeholder paragraph</xsl:message>
            </xsl:if>
          </xsl:when>
		      <xsl:otherwise>
		        <xsl:apply-templates select="."/>
		      </xsl:otherwise>
		    </xsl:choose>
			</xsl:for-each>
		</xsl:copy>
	</xsl:template>
</xsl:stylesheet>

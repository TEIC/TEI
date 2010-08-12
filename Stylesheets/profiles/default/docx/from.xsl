<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                version="2.0"
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso">
    <!-- import base conversion style -->

    <xsl:import href="../../../docx/from/docxtotei.xsl"/>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MAINTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
    
    <xsl:output indent="no"/>

    <xsl:template match="@rend[.='Body Text']" mode="pass2"/>
    <xsl:template match="@rend[.='Body Text Indent']" mode="pass2"/>


  <xsl:template match="tei:hi[@rend]" mode="part2">
    <xsl:variable name="r" select="@rend"/>
    <xsl:choose>
      <xsl:when test="preceding-sibling::node()[1][self::tei:hi[@rend=$r]]">
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="part2"/>
	  <xsl:call-template name="nextHi">
	    <xsl:with-param name="r" select="$r"/>
	  </xsl:call-template>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
   </xsl:template>

   <xsl:template name="nextHi">
      <xsl:param name="r"/>
      <xsl:for-each select="following-sibling::node()[1]">
         <xsl:if test="self::tei:hi[@rend=$r]">
            <xsl:apply-templates mode="part2"/>
            <xsl:call-template name="nextHi">
	              <xsl:with-param name="r" select="$r"/>
            </xsl:call-template>
         </xsl:if>
      </xsl:for-each>
   </xsl:template>

</xsl:stylesheet>

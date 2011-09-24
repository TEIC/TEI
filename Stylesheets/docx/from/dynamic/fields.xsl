<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
                xmlns:mv="urn:schemas-microsoft-com:mac:vml"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns="http://www.tei-c.org/ns/1.0"
                version="2.0"
                exclude-result-prefixes="a cp dc dcterms dcmitype prop     iso m mml mo mv o pic r rel     tbx tei teidocx v xs ve w10 w wne wp">
        
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for converting Word docx files to TEI </p>
         <p> This library is free software; you can redistribute it and/or modify it under
            the terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        Handle simple fields
    </desc>
   </doc>
    <xsl:template match="w:fldSimple">
      <xsl:variable name="rstyle">
	<xsl:value-of select="w:r/w:rPr/w:rStyle/@w:val"/>
      </xsl:variable>
      <xsl:variable name="rends">
	<r>fldSimple</r>
	<xsl:if test="contains(@w:instr,'NOTEREF')"><r>noteref</r></xsl:if>
	<xsl:if test="contains(@w:instr,'MERGEFORMAT')"><r>mergeformat</r></xsl:if>
	<xsl:if test="contains(@w:instr,' \r')"><r>instr_r</r></xsl:if>
	<xsl:if test="contains(@w:instr,' \f')"><r>instr_f</r></xsl:if>
	<xsl:if test="contains(@w:instr,' \n')"><r>instr_n</r></xsl:if>
      </xsl:variable>
<!--      <xsl:message>rends=<xsl:copy-of select="$rends"/></xsl:message> -->
      <xsl:choose>
	<!-- cross ref -->
	<!-- <w:fldSimple w:instr=" REF _Ref260736521 \r \h  \* MERGEFORMAT ">            -->
	<xsl:when test="contains(@w:instr,'NOTEREF _Ref')">
	  <xsl:variable name="ref">
	    <xsl:value-of select="substring-before(substring-after(@w:instr,'_'),' ')"/>
	  </xsl:variable>
	  <ref>
	    <xsl:attribute name="rend">
	      <xsl:value-of select="string-join(($rends/tei:r),' ')"/>
	    </xsl:attribute>
	    <xsl:attribute name="target" select="concat('#',$ref)"/>
	    <xsl:if test="$rstyle!=''">
	      <xsl:attribute name="iso:class"><xsl:value-of select="$rstyle"/></xsl:attribute>
	    </xsl:if>
	    <xsl:apply-templates/>
	  </ref>	    
	</xsl:when>
	<xsl:when test="contains(@w:instr,'REF _Ref')">
	  <xsl:variable name="ref">
	    <xsl:value-of select="substring-before(substring-after(@w:instr,'_'),' ')"/>
	  </xsl:variable>
	  <ref>
	    <xsl:attribute name="rend">
	      <xsl:value-of select="string-join(($rends/tei:r),' ')"/>
	    </xsl:attribute>
	    <xsl:attribute name="target" select="concat('#',$ref)"/>
	    <xsl:if test="$rstyle!=''">
	      <xsl:attribute name="iso:class"><xsl:value-of select="$rstyle"/></xsl:attribute>
	    </xsl:if>
	    <xsl:apply-templates/>
	  </ref>	    
	</xsl:when>
	<xsl:when test="contains(@w:instr,'PAGE')"><!-- Page number -->
	  <tei:dynamicContent type="pagenumber"/>
	</xsl:when>
	<xsl:when test="contains(@w:instr,'SEQ')"/><!-- not sure -->
	<xsl:otherwise>
	  <xsl:message terminate="yes">fldSimple: unrecognized type <xsl:value-of select="@w:instr"/></xsl:message>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>
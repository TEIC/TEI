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
         <p>This software is dual-licensed:

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
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handle simple fields</desc>
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
	<xsl:when test="contains(@w:instr,'INCLUDETEXT')"/><!-- from docm to docx conversion? -->
	<xsl:when test="contains(@w:instr,'TEMPLATE')"/><!-- from docm to docx conversion? -->	
	<xsl:otherwise>
	  <xsl:message terminate="yes">fldSimple: unrecognized type <xsl:value-of select="@w:instr"/></xsl:message>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="#all">
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML </p>
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
      <desc>Dealing with sections</desc>
   </doc>
    <xsl:template match="tei:milestone">
        <xsl:param name="final-section">false</xsl:param>
        
        
        <!-- construct sectPr -->
        <xsl:variable name="sectPr">
            <w:sectPr>
	      <xsl:for-each select="tei:footer">
		<xsl:variable name="ref" select="@ref"/>
		<xsl:if test="count(key('FOOTERS',$ref))&gt;0">
		  <xsl:variable name="footernum">
		    <xsl:for-each select="key('FOOTERS',$ref)">
		      <xsl:number level="any"/>
		    </xsl:for-each>
		  </xsl:variable>
		  <w:footerReference w:type="{@type}" r:id="{concat('rId',100+$footernum)}"/>
		</xsl:if>
	      </xsl:for-each>
	      
	      <xsl:for-each select="tei:header">
		<xsl:variable name="ref" select="@ref"/>
		<xsl:if test="count(key('HEADERS',$ref))&gt;0">
		  <xsl:variable name="headernum">
		    <xsl:for-each select="key('HEADERS',$ref)">
		      <xsl:number level="any"/>
		    </xsl:for-each>
		  </xsl:variable>
		  <w:headerReference w:type="{@type}" r:id="{concat('rId',100+$headernum)}"/>
		</xsl:if>
	      </xsl:for-each>	      
	      <w:footnotePr>
		<w:numFmt w:val="{$footnoteNumbering}"/>
	      </w:footnotePr>
	      <w:pgSz>
		<xsl:choose>
		  <!-- landscape -->
		  <xsl:when test="tei:orientation/@type='landscape'">
		    <xsl:attribute name="w:orient">landscape</xsl:attribute>
		    <xsl:attribute name="w:h">11901</xsl:attribute>
		    <xsl:attribute name="w:w">16817</xsl:attribute>
		  </xsl:when>
		  <!-- portrait -->
		  <xsl:otherwise>
		    <xsl:attribute name="w:h">16817</xsl:attribute>
		    <xsl:attribute name="w:w">11901</xsl:attribute>
		  </xsl:otherwise>
		</xsl:choose>
	      </w:pgSz>
	      <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:gutter="0"
		       w:footer="720"
		       w:header="720"/>
	      <xsl:if test="tei:pageNumbering">
		<w:pgNumType>
		  <xsl:if test="tei:pageNumbering/@start">
		    <xsl:attribute name="w:start" select="tei:pageNumbering/@start"/>
		  </xsl:if>
		  <xsl:if test="tei:pageNumbering/@type">
		    <xsl:attribute name="w:fmt" select="tei:pageNumbering/@type"/>
		  </xsl:if>
		</w:pgNumType>
	      </xsl:if>
	      <xsl:if test="tei:header/@type='first' or tei:footer/@type='first'">
		<w:titlePg/>
	      </xsl:if>
	      <w:docGrid w:linePitch="360"/>
	    </w:sectPr>
	</xsl:variable>
        
        <!-- write out sectPr -->
        <xsl:choose>
            <xsl:when test="$final-section='false'">
                <w:p>
                    <w:pPr>
                        <xsl:copy-of select="$sectPr"/>
                    </w:pPr>
                </w:p>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="$sectPr"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
</xsl:stylesheet>

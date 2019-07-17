<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="teix ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn tei teidocx">
    <!-- import conversion style -->
    <xsl:import href="../../default/docx/to.xsl"/>
    

    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML (see tei-docx.xsl) for Vesta </p>
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

    <xsl:param name="shadowGraphics">true</xsl:param>
    <xsl:param name="glossListSeparator">break</xsl:param>
    <xsl:param name="pagebreakStyle">active</xsl:param>
    <xsl:template match="teix:egXML|tei:p[tei:match(@rend,'eg')]">
        <xsl:param name="simple">false</xsl:param>
        <xsl:param name="highlight"/>
        <xsl:call-template name="block-element">
            <xsl:with-param name="select">
                <tei:p rend="Special" 
		       iso:style="font-family:DejaVu Sans Mono; font-size:18; text-align:left;" >
                    <xsl:call-template name="create-egXML-section"/>
                </tei:p>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="headerParts">
    <xsl:for-each select="/*/tei:teiHeader/tei:revisionDesc">
    <w:tbl>
      <w:tblPr>
        <w:tblStyle w:val="revisionDesc"/>
        <w:tblW w:w="0" w:type="auto"/>
        <w:tblBorders>
          <w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>
          <w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>
          <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
          <w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>
          <w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>
          <w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>
        </w:tblBorders>
      </w:tblPr>
      <w:tblGrid>
        <w:gridCol w:w="2366"/>
        <w:gridCol w:w="2366"/>
        <w:gridCol w:w="2366"/>
        <w:gridCol w:w="2366"/>
      </w:tblGrid>
      <xsl:for-each select="tei:change">
	<w:tr>
	  <w:trPr>
	    <w:trHeight w:val="380"/>
	  </w:trPr>
	  <w:tc>
	    <w:p>
	      <w:r>
		<w:t><xsl:value-of select="@n"/></w:t>
	      </w:r>
	    </w:p>
	  </w:tc>
	  <w:tc>
	    <w:p>
	      <w:r>
		<w:t><xsl:value-of select="@when"/></w:t>
	      </w:r>
	    </w:p>
	  </w:tc>
	  <w:tc>
	    <w:p>
	      <w:r>
		<w:t><xsl:value-of select="@who"/></w:t>
	      </w:r>
	    </w:p>
	  </w:tc>
	  <w:tc>
	    <w:p>
	      <w:r>
		<w:t><xsl:value-of select="."/></w:t>
	      </w:r>
	    </w:p>
	  </w:tc>
	</w:tr>
      </xsl:for-each>
    </w:tbl>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="write-document-dot-xml-frontmatter">
    <xsl:call-template name="document-title"/>
    <xsl:call-template name="headerParts"/>
    <xsl:call-template name="titlepages"/>
    <xsl:apply-templates select=".//tei:text/tei:front"/>
  </xsl:template>

  <xsl:template name="write-document-dot-xml-maincontent">
    <xsl:apply-templates select=".//tei:text/tei:body"/>
  </xsl:template>


   
</xsl:stylesheet>

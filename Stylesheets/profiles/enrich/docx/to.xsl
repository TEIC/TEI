<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
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
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn tei teidocx">
    <!-- import conversion style -->
    <xsl:import href="../../../profiles/default/docx/to.xsl"/>
    
    <xsl:import href="../../../common2/msdescription.xsl"/>

 
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML (see tei-docx.xsl) for Vesta </p>
         <p> This library is free software; you can redistribute it and/or
            modify it under the terms of the GNU Lesser General Public License as
            published by the Free Software Foundation; either version 2.1 of the
            License, or (at your option) any later version. This library is
            distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
            without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
            details. You should have received a copy of the GNU Lesser General Public
            License along with this library; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    <xsl:template match="/">
      <xsl:choose>
	        <xsl:when test="tei:TEI">
	           <xsl:apply-templates/>
	        </xsl:when>
	        <xsl:when test="tei:teiHeader">
	           <xsl:variable name="x">
	              <tei:TEI>
	                 <xsl:copy-of select="tei:teiHeader"/>
	              </tei:TEI>
	           </xsl:variable>
	           <xsl:apply-templates select="$x"/>
	        </xsl:when>
      </xsl:choose>
    </xsl:template>


    <xsl:template name="msSection">
      <xsl:param name="level"/>
      <xsl:param name="implicitBlock"/>
      <xsl:param name="heading"/>
      <w:p>
	        <w:pPr>
	           <w:pStyle w:val="tei{local-name()}"/>
	        </w:pPr>
	        <w:r>
	           <w:t>
	              <xsl:value-of select="$heading"/>
	           </w:t>
	        </w:r>
      </w:p>
      <xsl:call-template name="block-element"/>
    </xsl:template>
    
    <xsl:template name="msInline">
      <xsl:param name="before"/>
      <xsl:param name="after"/>
      <xsl:param name="style"/>
      <w:r>
	        <w:rPr>
	           <w:rStyle w:val="tei{local-name()}"/>
	           <xsl:choose>
	              <xsl:when test="$style='italic'">
	                 <w:i/>
	              </xsl:when>
	              <xsl:when test="$style='bold'">
	                 <w:b/>
	              </xsl:when>
	           </xsl:choose>
	        </w:rPr>
	        <w:t>
	           <xsl:value-of select="$before"/>
	           <xsl:value-of select="."/>
	           <xsl:value-of select="$after"/>
	        </w:t>
      </w:r>
    </xsl:template>

    <xsl:template name="msBlock">
      <xsl:param name="style"/>
      <xsl:call-template name="block-element">
	        <xsl:with-param name="style">
	           <xsl:value-of select="$style"/>
	        </xsl:with-param>
      </xsl:call-template>
    </xsl:template>

    <xsl:template name="msLabelled">
      <xsl:param name="before"/>
      <w:r>
	        <w:rPr>
	           <w:i/>
	        </w:rPr>
	        <w:t>
	           <xsl:attribute name="xml:space">preserve</xsl:attribute>
	           <xsl:value-of select="$before"/>
	           <xsl:text>: </xsl:text>
	        </w:t>
      </w:r>
      <w:r>
	        <w:rPr>
	           <w:rStyle w:val="tei{local-name()}"/>
	        </w:rPr>
	        <w:t>
	           <xsl:value-of select="."/>
	        </w:t>
      </w:r>
    </xsl:template>

    <xsl:template name="msLiteral">
      <xsl:param name="text"/>
      <w:r>
	        <w:rPr/>
	        <w:t>
	           <xsl:attribute name="xml:space">preserve</xsl:attribute>
	           <xsl:value-of select="$text"/>
	        </w:t>
      </w:r>
    </xsl:template>


    <xsl:template name="headerParts">
      <xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc"/>
    </xsl:template>

</xsl:stylesheet>
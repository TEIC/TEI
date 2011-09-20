<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:its="http://www.w3.org/2005/11/its"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="teidocx cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn its">
    <!-- import conversion style -->
    <xsl:import href="../../../docx/to/teitodocx.xsl"/>
    <xsl:import href="../isoutils.xsl"/>
    
    <!-- import functions -->
    <xsl:include href="iso-functions.xsl"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert TEI XML to Word DOCX XML.</p>
         <p>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  
      </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

    <xsl:param name="header-file"/>
    <xsl:param name="document-file"/>
    <xsl:param name="debug">false</xsl:param>   
    
   <!-- identity transform -->

<xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
   </xsl:template>


   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>


   <xsl:template match="w:sdtContent[w:r]">
      <xsl:variable name="alias" select="../w:sdtPr/w:tag/@w:val"/>
      <xsl:copy>
         <xsl:apply-templates select="@*"/>
         <w:r>
	           <xsl:copy-of select="@w:rsidR"/>
	           <xsl:apply-templates select="w:r/w:rPr"/>
	           <w:t>
	              <xsl:attribute name="xml:space">preserve</xsl:attribute>
	              <xsl:for-each select="document($header-file)">
	                 <xsl:value-of select="key('ISOMETA',$alias)"/>
	              </xsl:for-each>
	           </w:t>
         </w:r>
      </xsl:copy>
   </xsl:template>


   <xsl:template match="w:body">
      <xsl:copy>
         <xsl:apply-templates/>
         <xsl:copy-of select="document($document-file)/w:document/w:body/*"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="w:sectPr"/>


 <xsl:template name="block-element">
     <xsl:param name="select"/>
     <xsl:param name="style"/>
     <xsl:param name="pPr"/>
     <xsl:param name="nop"/>
     <xsl:param name="bookmark-name"/>
     <xsl:param name="bookmark-id"/>
   </xsl:template>

   <xsl:template name="termNum"/>
  
</xsl:stylesheet>
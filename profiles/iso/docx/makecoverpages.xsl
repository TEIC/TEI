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

    <xsl:param name="headerFile"/>
    <xsl:param name="documentFile"/>
    <xsl:param name="debug">false</xsl:param>   
    
   <!-- identity transform -->

   <xsl:template match="/">
     <xsl:message>Reading new document from <xsl:value-of
     select="$documentFile"/> and header data from TEI file  <xsl:value-of select="$headerFile"/></xsl:message>
     <xsl:apply-templates/>
   </xsl:template>

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
	     <xsl:for-each select="doc($headerFile)">
	       <xsl:value-of select="key('ISOMETA',$alias)"/>
	     </xsl:for-each>
	   </w:t>
         </w:r>
      </xsl:copy>
   </xsl:template>


   <xsl:template match="w:body">
      <xsl:copy>
         <xsl:apply-templates/>
         <xsl:apply-templates
	     select="doc($documentFile)/w:document/w:body/*"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="w:sectPr"/>

   <xsl:template match="w:p">
     <xsl:copy>
       <xsl:apply-templates/>
     </xsl:copy>
   </xsl:template>

 <xsl:template name="block-element">
     <xsl:param name="select"/>
     <xsl:param name="style"/>
     <xsl:param name="pPr" as="node()*"/>
     <xsl:param name="nop"/>
     <xsl:param name="bookmark-name"/>
     <xsl:param name="bookmark-id"/>
   </xsl:template>

   <xsl:template name="termNum"/>
  
</xsl:stylesheet>

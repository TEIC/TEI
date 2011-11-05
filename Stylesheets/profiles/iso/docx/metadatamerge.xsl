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

	  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert TEI XML to Word DOCX XML.</p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

	  <xsl:param name="header-doc" as="item()+" required="yes"/>
	  <xsl:param name="debug">true</xsl:param>

	  <!-- identity transform -->
	<xsl:template match="@*|text()|comment()|processing-instruction()">
		    <xsl:copy-of select="."/>
	  </xsl:template>

	  <xsl:template match="*">
		    <xsl:copy>
			      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
		    </xsl:copy>
	  </xsl:template>

	  <xsl:template match="tei:titleStmt">
	     <xsl:if test="$debug = 'true'">
		       <xsl:message>replace titleStmt</xsl:message>
		    </xsl:if>
		    <xsl:copy-of select="$header-doc//tei:titleStmt"/>
	  </xsl:template>

	  <xsl:template match="tei:publicationStmt">
	     <xsl:if test="$debug = 'true'">
		       <xsl:message>replace publicationStmt</xsl:message>
		    </xsl:if>
		    <xsl:copy-of select="$header-doc//tei:publicationStmt"/>
	  </xsl:template>

  <xsl:template match="tei:encodingDesc">
      <xsl:copy>
	        <xsl:copy-of select="$header-doc//tei:appInfo"/>
	        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:appInfo">
	    <xsl:variable name="ident" select="tei:application/@ident"/>		
	    <xsl:choose>
		      <xsl:when test="$header-doc//tei:appInfo[tei:application/@ident=$ident]">
		         <xsl:if test="$debug = 'true'">
	             <xsl:message select="concat('replace appInfo with ident: ',$ident[1])"/>
	          </xsl:if>
		      </xsl:when>
		      <xsl:otherwise>
			        <xsl:copy-of select="."/>
		      </xsl:otherwise>
	    </xsl:choose>
  </xsl:template>
	
  <!-- currently we only allow one foreword -->
  <xsl:template match="tei:front/tei:div[@type='foreword']">
	    <xsl:variable name="currForeword" select="."/>
	    <xsl:variable name="templateForewordText">The boilerplate text and project metadata</xsl:variable>
	    <xsl:choose>
		      <xsl:when test="$header-doc//tei:front/tei:div[@type='foreword'] and not(contains($currForeword,$templateForewordText))">
		         <tei:div type="foreword">
		         <xsl:for-each select="$header-doc//tei:front/tei:div[@type='foreword']/*">
		            <xsl:choose>
		               <xsl:when test="name()='q'">
		                  <xsl:variable name="sdtName" select="@iso:meta"/>
		                  <xsl:message select="concat('found sdt element ', $sdtName)"/>
		                  <xsl:if test="$currForeword//*[@iso:meta=$sdtName]">
		                     <xsl:variable name="existingSdt" select="$currForeword//*[@iso:meta=$sdtName]"/>
		                     <xsl:if test="$debug = 'true'">
			                     <xsl:message select="concat('reuse existing sdt section ', $sdtName)"/>
			                     <xsl:message select="$existingSdt"/>
		                     </xsl:if>
		                     <xsl:copy-of select="$existingSdt"/>
		                  </xsl:if>
		               </xsl:when>
		               <xsl:otherwise>
		                 <xsl:if test="$debug = 'true'">
                       <xsl:message select="concat('use from new front ', .)"/>
                      </xsl:if>
		                 <xsl:copy-of select="."/>
		               </xsl:otherwise>
		            </xsl:choose>
		         </xsl:for-each>
		         </tei:div>
		      </xsl:when>
		      <xsl:when test="$header-doc//tei:front/tei:div[@type='foreword'] and contains($currForeword,$templateForewordText)">
		        <xsl:if test="$debug = 'true'">
               <xsl:message>found default foreword will replace all from new header</xsl:message>
             </xsl:if>
		        <xsl:copy-of select="$header-doc//tei:front/tei:div[@type='foreword']"/>
		      </xsl:when>
		      <xsl:otherwise>
			      <xsl:copy-of select="."/>
		      </xsl:otherwise>
	    </xsl:choose>
  </xsl:template>
	

</xsl:stylesheet>

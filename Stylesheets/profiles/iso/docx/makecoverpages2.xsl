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
	  <xsl:param name="document-doc"/>
	  <xsl:param name="debug">true</xsl:param>

	  <!-- identity transform -->
		<xsl:template match="@*|text()|comment()|processing-instruction()">
				<xsl:param name="tocDefinition"></xsl:param>
				<xsl:choose>
						<xsl:when test="ancestor-or-self::w:instrText and $tocDefinition != '' and starts-with(.,' TOC ')">
						    <xsl:message>set TOC definition to <xsl:value-of select="$tocDefinition" /></xsl:message>
								<xsl:value-of select="$tocDefinition"></xsl:value-of>
						</xsl:when>
						<xsl:otherwise>
								<xsl:copy-of select="." />
						</xsl:otherwise>
				</xsl:choose>
		</xsl:template>

	  <xsl:template match="*">
	   <xsl:param name="tocDefinition"/>
		    <xsl:copy>
			      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()">
			       <xsl:with-param name="tocDefinition" select="$tocDefinition"/>
			      </xsl:apply-templates>
		    </xsl:copy>
	  </xsl:template>
  
    <xsl:template match="w:sdt">
      <xsl:variable name="tagName" select="w:sdtPr/w:tag/@w:val"/>
      <xsl:variable name="alias">
        <xsl:choose>
          <xsl:when test="contains($tagName, '_')">
             <xsl:value-of select="substring-before($tagName, '_')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tagName"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="option">
        <xsl:choose>
          <xsl:when test="contains($tagName, '_')">
             <xsl:value-of select="substring-after($tagName, '_')"/>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="contains($option, 'ifdefined') and (not($header-doc//*[@iso:meta=$alias]) or $header-doc//*[@iso:meta=$alias] = '')">
           <xsl:message>remove optional sdt <xsl:value-of select="$alias"/></xsl:message>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy>
             <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()">
               <xsl:with-param name="alias" select="$alias"/>
             </xsl:apply-templates>
          </xsl:copy>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <xsl:template match="w:placeholder|w:showingPlcHdr">
    </xsl:template>
    
	  <xsl:template match="w:sdtContent">
	     <xsl:param name="alias"/>
	     <xsl:copy>
	        <xsl:apply-templates select="@*"/>
	        <xsl:choose>

	           <!-- if we find w:sdt section we don't replace the content -->
	           <xsl:when test=".//w:sdt">
	             <xsl:message>copy stdContent <xsl:value-of select="$alias"/> as is</xsl:message>
               <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	           </xsl:when>
	           
	           <xsl:when test="ancestor::w:sdt/w:sdtPr/w:dropDownList">
		          <xsl:variable name="content">
		            <xsl:apply-templates select="$header-doc//*[@iso:meta=$alias]" mode="tei"/>
		          </xsl:variable>
		          <xsl:if test="$debug='true'">
                <xsl:message>set sdtContent for dropdown <xsl:value-of select="$alias"/> = <xsl:value-of select="$content"/></xsl:message>
              </xsl:if>		          
		          <w:r>
		            <!-- copy any existing rPr, it may contain character formatting  -->
		            <xsl:copy-of select="w:r/w:rPr"/>
	              <w:t>
	              <xsl:value-of select="ancestor::w:sdt/w:sdtPr/w:dropDownList//w:listItem[@w:value=$content]/@w:displayText"/>
			          </w:t>
			         </w:r>
	           </xsl:when>
	           
	           <xsl:otherwise>
		          <xsl:variable name="content">
		            <xsl:apply-templates select="$header-doc//*[@iso:meta=$alias]" mode="tei">
		              <!-- pass an existing pPr and rPr to new elements since it may contain important character formatting  -->
		              <xsl:with-param name="rPr" select="./w:r[1]/w:rPr"/>
		            </xsl:apply-templates>
		          </xsl:variable>	           
              <xsl:if test="$debug='true'">
                <xsl:message>set sdtContent for textfield <xsl:value-of select="$alias"/> = <xsl:value-of select="$content"/></xsl:message>
              </xsl:if>
              <xsl:choose>
                <!-- content can contain w:p elements which must be caried over to produce a valid document -->
	              <xsl:when test="w:p">
	                <xsl:element name="w:p">
	                  <xsl:apply-templates select="w:p/@*"/>
	                  <xsl:apply-templates select="w:p[1]/w:pPr"/>
	                  <xsl:copy-of select="$content"/>
	                </xsl:element>
	              </xsl:when>
	              <xsl:otherwise>
	                <xsl:copy-of select="$content"/>
	              </xsl:otherwise>             
	              </xsl:choose>
	           </xsl:otherwise>
			    </xsl:choose>
		    </xsl:copy>
	  </xsl:template>

    <xsl:template match="*[@iso:meta]/text()" mode="tei">
      <xsl:param name="rPr"></xsl:param>
      <w:r>
        <xsl:copy-of select="$rPr"/>
        <w:t><xsl:value-of select="."/></w:t>
      </w:r>
    </xsl:template>
    <xsl:template match="tei:lb" mode="tei">
      <w:r>
        <w:br/>
      </w:r>
    </xsl:template>
   
	  <xsl:template match="w:body">
		    <xsl:copy>
		       <xsl:apply-templates select="@*"/>
		       <xsl:variable name="tocStyles">(TOC.?|zzContents)</xsl:variable>
		       <xsl:variable name="tocStylesToCopy">TOC.?</xsl:variable>
		       <xsl:variable name="tocElems" select="$document-doc/w:document/w:body/front/w:p[matches(w:pPr/w:pStyle/@w:val,$tocStylesToCopy)]"/>
		       <xsl:variable name="hasToc" select="not(empty($tocElems))"/>
		       <xsl:message select="concat('has TOC: ', $hasToc)"></xsl:message>
		       <xsl:for-each select="child::*">
		          <xsl:choose>
		             <xsl:when test="not($hasToc) and name(.) = 'w:p' and matches(w:pPr/w:pStyle/@w:val, $tocStyles)">
                    <xsl:message select="concat('skip TOC element from model doc because of style: ', w:pPr/w:pStyle/@w:val)"/>
		             </xsl:when>
                 <xsl:when test="$hasToc and name(.) = 'w:p' and starts-with(.,'#toc')">
                    <xsl:variable name="tocDefinition" select="substring-after(.,'#toc ')"/>
                    <xsl:if test="$debug = 'true'">
                       <xsl:message>insert toc section with TOC definition: <xsl:value-of select="$tocDefinition"/></xsl:message>
                    </xsl:if>
                    <xsl:for-each select="$tocElems">
                      <xsl:message>insert toc section para</xsl:message>
                      <!-- do not copy any section breaks -->
                      <xsl:if test="not(.//w:sectPr)">
                        <xsl:apply-templates select=".">
                          <xsl:with-param name="tocDefinition" select="$tocDefinition"/>
                        </xsl:apply-templates>
                      </xsl:if>
                    </xsl:for-each>
                 </xsl:when>
		             <xsl:when test="name(.) = 'w:p' and . = '#foreword'">
		                <xsl:if test="$debug = 'true'">
		                   <xsl:message>insert foreword section</xsl:message>
		                </xsl:if>
		                <!-- copy all front/w:p from zzForeword onwards -->
		                <xsl:variable name="fwHeader" select="$document-doc/w:document/w:body/front/w:p[w:pPr/w:pStyle/@w:val = 'zzForeword'][1]"/>
		                <xsl:copy-of select="$fwHeader"/>
		                <xsl:for-each select="$fwHeader/following-sibling::*">
		                  <!-- do not copy any section breaks -->
		                  <xsl:if test="not(.//w:sectPr)">
		                    <xsl:copy-of select="."/>
		                  </xsl:if>
		                </xsl:for-each>
		             </xsl:when>
		             <xsl:when test="name(.) = 'w:p' and . = '#main'">
		                <xsl:if test="$debug = 'true'">
		                   <xsl:message>insert main section</xsl:message>
		                </xsl:if>
		                <xsl:copy-of select="$document-doc/w:document/w:body/main/*"/>
		             </xsl:when>
		             <xsl:when test="name(.) = 'w:p' and . = '#back'">
		                <xsl:if test="$debug = 'true'">
		                   <xsl:message>insert back section</xsl:message>
		                </xsl:if>
		                <xsl:copy-of select="$document-doc/w:document/w:body/back/*"/>
		             </xsl:when>
               <xsl:when test="name(.) = 'w:p' and . = '#remove'">
                  <xsl:if test="$debug = 'true'">
                     <xsl:message>remove empty placeholder paragraph</xsl:message>
                  </xsl:if>
               </xsl:when>
	             <xsl:otherwise>
	               <xsl:message>copy <xsl:value-of select="name(.)"/></xsl:message>
	                <xsl:apply-templates select="."/>
	             </xsl:otherwise>
		          </xsl:choose>
			      </xsl:for-each>
		    </xsl:copy>
	  </xsl:template>
	  
</xsl:stylesheet>

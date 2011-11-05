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
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Calls the named template paragraph-wp that can be overriden.</p>
         <p>
            See comment at begin of document to understand why this template is calling
            a named template.
            
            This stylesheet is handling simple paragraphs that we know nothing else
            about.
        </p>
      </desc>
   </doc>
    <xsl:template match="w:p" mode="paragraph">
      <xsl:variable name="style" select="w:pPr/w:pStyle/@w:val"/>
      <xsl:choose>
	<xsl:when test="$style='tei_lg'"/>
	<xsl:when test="$style='GeneratedTitle'"/>
	<xsl:when test="$style='GeneratedSubTitle'"/>

	<xsl:when test="$style='tei_l'">
	  <l>
	    <xsl:apply-templates/>
	  </l>
	</xsl:when>
	<xsl:when test="$style='Tabletitle'">
	  <head>
	    <xsl:apply-templates/>
	  </head>
	</xsl:when>
	<xsl:when test="$style='dl'">
	  <GLOSSITEM>
	    <xsl:apply-templates/>
	  </GLOSSITEM>
	</xsl:when>
	<xsl:when test="$style='tei_signed'">
	  <signed>
	    <xsl:apply-templates/>
	  </signed>
	</xsl:when>
	<xsl:when test="$style='tei_speaker'">
	  <speaker>
	    <xsl:apply-templates/>
	  </speaker>
	</xsl:when>
	<xsl:when test="$style='tei_speech'">
	  <speech>
	    <xsl:apply-templates/>
	  </speech>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="paragraph-wp">
	    <xsl:with-param name="style" select="$style"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        Named template for handling w:p; we 
       use the Word style (if provided) to make a TEI rend attribute,
       and check for change records.
    </desc>
   </doc>
   <xsl:template name="paragraph-wp">
     <xsl:param name="style"/>
     <xsl:element name="p">
       <xsl:if test="$style and not($style='Default')">
	 <xsl:attribute name="rend">
	   <xsl:value-of select="$style"/>
	 </xsl:attribute>
       </xsl:if>
       <xsl:if test="w:pPr/w:pStyle/w:rPr/w:rtl">
	 <xsl:attribute name="dir"
			xmlns="http://www.w3.org/2005/11/its">
	   <xsl:text>rtl</xsl:text>
	 </xsl:attribute>
       </xsl:if>
       <xsl:choose>
	 <xsl:when test="w:pPr/w:rPr/w:ins">
	   <add when="{w:pPr/w:rPr/w:ins/@w:date}" 
		type="para">
	     <xsl:call-template name="identifyChange">
	       <xsl:with-param name="who"
			       select="w:pPr/w:rPr/w:ins/@w:author"/>
	     </xsl:call-template>
	     <xsl:call-template name="process-checking-for-crossrefs"/>
	   </add>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:call-template name="process-checking-for-crossrefs"/>
	 </xsl:otherwise>
       </xsl:choose>
     </xsl:element>
   </xsl:template>
   
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	Named template for handling processing any cross-references found.
    </desc>
    </doc>
    <xsl:template name="process-checking-for-crossrefs">
      <xsl:choose>
	<xsl:when
	    test="w:r/w:fldChar[@w:fldCharType='begin']">
	  <xsl:for-each-group select="w:*|m:*"
			      group-starting-with="w:r[w:fldChar/@w:fldCharType[matches(.,'begin|end')]]">
	    <xsl:choose>
	      <xsl:when test="self::w:r/w:fldChar[@w:fldCharType='begin']">
		<xsl:variable name="rends">
		  <!-- collect all the rends for concatenation later -->
		  <xsl:choose>
		    <xsl:when
			test="contains(following-sibling::w:r[w:instrText][1],'NOTEREF')"><r>noteref</r></xsl:when>
		    <xsl:when
			test="contains(following-sibling::w:r[w:instrText][1],'SEQ')"><r>SEQ</r></xsl:when>
		    <xsl:when test="contains(following-sibling::w:r[w:instrText][1],'XE')"><r>index</r></xsl:when>
		    <xsl:when test="contains(following-sibling::w:r[w:instrText][1],'REF')"><r>ref</r></xsl:when>
		  </xsl:choose>
		  <xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\r')"><r>instr_r</r></xsl:if>
		  <xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\f')"><r>instr_f</r></xsl:if>
		  <xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\n')"><r>instr_n</r></xsl:if>
		  <xsl:if test="contains(following-sibling::w:r[w:instrText][1],'MERGEFORMAT')"><r>mergeformat</r></xsl:if>
		</xsl:variable>
		<xsl:choose>
		  <xsl:when test="$rends/tei:r='index'">
		    <indexTerm>
		      <xsl:value-of select="current-group()//w:instrText[2]"/>
		    </indexTerm>
		  </xsl:when>
		  <xsl:when test="$rends/tei:r='SEQ'">
		    <xsl:variable name="What"
				  select="following-sibling::w:r/w:instrText[1]"/>
		    <xsl:number level="any" count="w:r[w:fldChar/@w:fldCharType='begin'][following-sibling::w:r/w:instrText=$What]"/>
		  </xsl:when>
		  <xsl:otherwise>
		    <ref>
		      <xsl:if test="$rends/tei:r">
			<xsl:attribute name="rend">
			  <xsl:value-of select="string-join(($rends/tei:r),' ')"/>
			</xsl:attribute>
		      </xsl:if>
		      <xsl:if test="following-sibling::w:r[w:rPr][1]/w:rStyle">
			<xsl:attribute name="iso:class">
			  <xsl:value-of select="following-sibling::w:r[w:rPr][1]/w:rStyle/w:rPr/w:rStyle/@w:val"/>
			</xsl:attribute>
		      </xsl:if> 
		      <xsl:for-each select="current-group()">
			<xsl:if test="self::w:r[w:instrText]">			    
			  <xsl:variable name="ref">
			    <xsl:choose>
			      <xsl:when test="contains(w:instrText,'REF _')"> <!-- this will also catch NOTEREF _ -->
				<xsl:text>#</xsl:text>
				<xsl:value-of select="substring-before(substring-after(w:instrText,'_'),'&#32;')"/>
			      </xsl:when>
			      <xsl:when test="contains(w:instrText,'HYPERLINK')">
				<xsl:value-of select="substring-before(substring-after(w:instrText,'&#x0022;'),'&#x0022;')"/>
			      </xsl:when>
			    </xsl:choose>
			  </xsl:variable>
			  <xsl:attribute name="target" select="$ref"/>
			</xsl:if>
		      </xsl:for-each>
		      <xsl:for-each select="current-group()">
			<xsl:choose>
			  <xsl:when  test="self::w:bookmarkStart">
			    <xsl:apply-templates select="."/>
			  </xsl:when>
			  <xsl:otherwise>
			    <xsl:apply-templates select="."/>
			  </xsl:otherwise>
			</xsl:choose>
		      </xsl:for-each>
		    </ref>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:when>
	      <xsl:when
		  test="self::w:r[w:fldChar/@w:fldCharType[matches(.,'end')]]">
		<xsl:for-each select="current-group()">
		  <xsl:choose>
		    <xsl:when
			test="self::w:r[w:fldChar/@w:fldCharType[matches(.,'end')]]">
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:apply-templates select="."/>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:apply-templates select="current-group()"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each-group>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

</xsl:stylesheet>

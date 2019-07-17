<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:fn="http://www.w3.org/2005/xpath-functions"
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
                exclude-result-prefixes="a cp dc dcterms dcmitype fn prop iso m mml mo mv o pic r rel tbx tei teidocx v xs ve w10 w wne wp">
    
    
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
    
   <xsl:variable name="dblq">"</xsl:variable>
   <xsl:variable name="usr">\r</xsl:variable>
   <xsl:variable name="ust">\t</xsl:variable>
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

       <xsl:when test="starts-with($style,'tei_')">
	 <xsl:element name="{substring($style,5)}">
	    <xsl:apply-templates/>
	 </xsl:element>
       </xsl:when>

       <xsl:when test="starts-with($style,'TEI ')">
	 <xsl:element name="{substring($style,5)}">
	    <xsl:apply-templates/>
	 </xsl:element>
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
	<xsl:otherwise>
		<!-- For unknown reason style name sometimes is enclosed with &lt; &gt; -->
		<xsl:variable name="st">
			<xsl:choose>
				<xsl:when test="substring($style, 1, 1)='&lt;'">
					<xsl:value-of select="substring($style, 2, string-length($style)-2)"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="$style"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:call-template name="paragraph-wp">
	    <xsl:with-param name="style" select="$st"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
	<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
		<desc>Named template to retrieve the formatting from named Word style to make into TEI style attribute elsewhere.</desc>
	</doc>
	<xsl:template name="retrieve-styles">
		<xsl:param name="style"/>
		<!-- check if styleDoc has style definition for current style -->
		<xsl:variable name="styleprop" select="doc($styleDoc)//w:style[@w:styleId=$style]"/>
		<!-- if yes, gather info about text alignment, bold, italic, font size, face, underlining, sub- and superscript into @style -->
		<xsl:if test="$styleprop/node()">
				<xsl:if test="count($styleprop/w:rPr[w:b])>0">
					<xsl:text>font-weight: bold; </xsl:text>
				</xsl:if>
				<xsl:if test="count($styleprop/w:rPr[w:i])>0">
					<xsl:text>font-style: italic; </xsl:text>
				</xsl:if>
				<xsl:if test="count($styleprop/w:rPr[w:u])>0">
					<xsl:text>text-decoration: underline; </xsl:text>
				</xsl:if>
				<xsl:if test="count($styleprop/w:pPr[w:jc])>0">
					<xsl:text>text-align:</xsl:text>
					<xsl:value-of select="$styleprop/w:pPr/w:jc/@w:val"/>
					<xsl:text>; </xsl:text>
				</xsl:if>
		</xsl:if>	
	</xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Named template for handling w:p; we 
       use the Word style (if provided) to make a TEI rend attribute,
       and check for change records.</desc>
   </doc>
   <xsl:template name="paragraph-wp">
   	<xsl:param name="style"/>
   	<xsl:element name="p">
       <xsl:if test="string($style) and not($style='Default' or $style='Default Style')">
	 <xsl:attribute name="rend">
	   <xsl:value-of select="$style"/>
	 </xsl:attribute>
       </xsl:if>
       <xsl:variable name="retrievedStyles">
         <!-- Do we want to preserve word styles? -->
         <xsl:if test="$preserveEffects='true' and not(normalize-space($style)='')">
     	   <xsl:call-template name="retrieve-styles">
             <xsl:with-param name="style" select="$style"/>
           </xsl:call-template>
         </xsl:if>
       </xsl:variable>
       <xsl:variable name="localStyles">
   	 <xsl:if test="$preserveEffects='true' and w:pPr/w:jc and
		       w:pPr/w:jc/@w:val !='both'">
           <xsl:text>text-align:</xsl:text>
	   <xsl:value-of select="w:pPr/w:jc/@w:val"/>
	            <xsl:text>;</xsl:text>
   	 </xsl:if>
       </xsl:variable>
       
       <!-- merge local and retrieved from styles.xml -->
       <xsl:if test="string($retrievedStyles) or string($localStyles)">
   	 <xsl:attribute name="style">
   	   <xsl:value-of select="$retrievedStyles"/>
   	   <xsl:value-of select="$localStyles"/>
   	 </xsl:attribute>
       </xsl:if>
       
       <xsl:if test="w:pPr/w:pStyle/w:rPr/w:rtl">
	 <xsl:attribute name="dir"
			xmlns="http://www.w3.org/2005/11/its">
	   <xsl:text>rtl</xsl:text>
	 </xsl:attribute>
       </xsl:if>
       <xsl:choose>
	 <xsl:when test="w:pPr/w:rPr/w:ins and $processChangeInformation='true'">
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
     <desc>Processing of any cross-references found.</desc>
   </doc>
   <xsl:template name="process-checking-for-crossrefs">
     <xsl:choose>
       <xsl:when
	   test="w:r/w:fldChar[@w:fldCharType='begin']">
	 <xsl:for-each-group select="w:*|m:*"
			     group-starting-with="w:r[w:fldChar/@w:fldCharType[matches(.,'begin|end')]]">
	   <xsl:variable name="instructions">
	     <xsl:value-of select="current-group()//w:instrText" separator=""/>
	   </xsl:variable>	   
	   <xsl:choose>
	      <xsl:when test="self::w:r/w:fldChar[@w:fldCharType='begin']">
		<xsl:variable name="jobs"> <!-- collect all the jobs for concatenation later -->
		  <xsl:choose>
		    <xsl:when test="matches($instructions,'^[ ]?NOTEREF')"><r>noteref</r></xsl:when>
		    <xsl:when test="matches($instructions,'^[ ]?SEQ')"><r>SEQ</r></xsl:when>
		    <xsl:when test="matches($instructions,'^[ ]?XE')"><r>index</r></xsl:when>
		    <xsl:when test="matches($instructions,'^[ ]?REF')"><r>ref</r></xsl:when>
		  </xsl:choose>
		  <xsl:if test="contains($instructions,'\r')"><r>instr_r</r></xsl:if>
		  <xsl:if test="contains($instructions,'\f')"><r>instr_f</r></xsl:if>
		  <xsl:if test="contains($instructions,'\n')"><r>instr_n</r></xsl:if>
		  <xsl:if test="contains($instructions,'MERGEFORMAT')"><r>mergeformat</r></xsl:if>
		</xsl:variable>
		<xsl:choose>
		  <xsl:when test="$jobs/tei:r='index'">
		    <xsl:call-template name="process-index-term">
		      <xsl:with-param name="term">
			<xsl:value-of
			    select="current-group()//w:instrText" separator=""/>
		      </xsl:with-param>
		    </xsl:call-template>
		  </xsl:when>
		  <!--
		  <xsl:when test="$jobs/tei:r='SEQ'">
		    <xsl:variable name="What"
		       select="following-sibling::w:r/w:instrText[1]"/>
		       <xsl:number level="any" count="w:r[w:fldChar/@w:fldCharType='begin'][following-sibling::w:r/w:instrText=$What]"/>
		  </xsl:when>
		  -->
		  <xsl:otherwise>
		    <ref>
		      <xsl:if test="$jobs/tei:r">
			<xsl:attribute name="rend">
			  <xsl:value-of select="string-join(($jobs/tei:r),' ')"/>
			</xsl:attribute>
		      </xsl:if>
		      <xsl:if test="following-sibling::w:r[w:rPr][1]/w:rStyle">
			<xsl:attribute name="iso:class">
			  <xsl:value-of select="following-sibling::w:r[w:rPr][1]/w:rStyle/w:rPr/w:rStyle/@w:val"/>
			</xsl:attribute>
		      </xsl:if> 
		      <xsl:if test="not($instructions='')">
			<xsl:attribute name="target" select="$instructions"/>
		      </xsl:if>
		      <xsl:apply-templates select="current-group()"/>
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

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Named template for handling processing of index terms.
	First insert main index enty, then recurse by index level</desc>
    </doc>
    <xsl:template name="process-index-term">
      <xsl:param name="term"/>
      <xsl:param name="xr"/>
      <xsl:choose>
	<xsl:when test="starts-with($term,'XE') or starts-with($term,' XE')">
	  <xsl:variable name="quoted-text" select="concat('[^',$dblq,']+',$dblq,'([^',$dblq,']+)',$dblq,'?.*')"/>
	  <xsl:variable name="clean-term" select="fn:replace($term,$quoted-text,'$1')"/>
	  <xsl:variable name="span" select="fn:replace(substring-after($term,$usr),$quoted-text,'$1')"/>
	  <xsl:variable name="see">
	    <xsl:value-of select="fn:replace(substring-after($term,$ust),$quoted-text,'$1')"/>
	  </xsl:variable>
	  <index indexName="XE">
	    <xsl:if test="normalize-space($span)">
	      <xsl:attribute name="spanTo">
		<xsl:text>#</xsl:text>
		<xsl:value-of select="normalize-space($span)"/>
	      </xsl:attribute>
	    </xsl:if>
	    <xsl:call-template name="process-index-term">
	      <xsl:with-param name="term"  select="normalize-space($clean-term)"/>
	      <xsl:with-param name="xr"  select="normalize-space($see)"/>
	    </xsl:call-template>
	  </index>
	</xsl:when>
	<xsl:when test="contains($term,':')">
	  <xsl:call-template name="process-index-term">
	    <xsl:with-param name="term" select="substring-before($term,':')"/>
	  </xsl:call-template>
	  <index>
	    <xsl:call-template name="process-index-term">
	      <xsl:with-param name="term" select="substring-after($term,':')"/>
	      <xsl:with-param name="xr"  select="normalize-space($xr)"/>
	    </xsl:call-template>
	  </index>
	</xsl:when>
	<xsl:when test="normalize-space($term)">
	  <term>
	    <xsl:value-of select="normalize-space($term)"/>
	    <xsl:if test="normalize-space($xr)">
	      <ref type="xr">
		<xsl:value-of select="normalize-space($xr)"/>
	      </ref>
	    </xsl:if>
	  </term>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:message>[4. <xsl:value-of select="$term"/>]</xsl:message>
	</xsl:otherwise>
      </xsl:choose> 
    </xsl:template>
      
</xsl:stylesheet>

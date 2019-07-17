<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xmlns:html="http://www.w3.org/1999/xhtml"
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
                exclude-result-prefixes="#all">
    
    
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
      <desc>Processing an inline run of text with its styling</desc>
   </doc>

    <xsl:template match="w:commentReference">
      <xsl:variable name="commentN" select="@w:id"/>
      <xsl:for-each
	  select="document(concat($wordDirectory,'/word/comments.xml'))/w:comments/w:comment[@w:id=$commentN]">
	<note place="comment" resp="{concat('#',translate(@w:author,' ','_'))}"><xsl:if test="@w:id"><xsl:attribute name="n"><xsl:value-of select="@w:id"/></xsl:attribute></xsl:if>
	  <date when="{@w:date}"/>
	  <xsl:apply-templates/>
	</note>
      </xsl:for-each>
    </xsl:template>
  
  <xsl:template match="w:commentRangeStart[@w:id]"><anchor type="commentRangeStart" n="{@w:id}"/></xsl:template>
  
  <xsl:template match="w:commentRangeEnd[@w:id]"><anchor type="commentRangeEnd" n="{@w:id}"/></xsl:template>
  
    <xsl:template match="w:r">
      <xsl:call-template name="processTextrun"/>
    </xsl:template>


   <xsl:template name="processTextrun">
     <xsl:variable name="style">
       <xsl:value-of select="w:rPr/w:rStyle/@w:val"/>
     </xsl:variable>
     <xsl:choose>
       <xsl:when test="$style='CommentReference'">
	   <xsl:apply-templates/>
       </xsl:when>

       <xsl:when test="$style='Hyperlink' and ancestor::w:hyperlink">
	 <xsl:call-template name="basicStyles"/>
       </xsl:when>

       <xsl:when test="$style='Hyperlink' and ancestor::w:fldSimple">
	 <xsl:call-template name="basicStyles"/>
       </xsl:when>
       
       <xsl:when test="$style='Hyperlink' and preceding-sibling::w:r[w:instrText][1]/w:instrText">
	 <ref>
	   <xsl:attribute name="target">
	     <xsl:for-each
		 select="preceding-sibling::w:r[w:instrText][1]/w:instrText">
	       <xsl:value-of select="substring-before(substring-after(.,'&#x0022;'),'&#x0022;')"/>
	     </xsl:for-each>
	   </xsl:attribute>
	   <xsl:call-template name="basicStyles">
	     <xsl:with-param name="parented">true</xsl:with-param>
	   </xsl:call-template>
	 </ref>
       </xsl:when>
       
       <xsl:when test="starts-with($style,'TEI ')">
	 <xsl:element name="{substring($style,5)}">
	   <xsl:call-template name="basicStyles">
	     <xsl:with-param name="parented">true</xsl:with-param>
	   </xsl:call-template>
	 </xsl:element>
       </xsl:when>

       <xsl:when test="starts-with($style,'tei_')">
	 <xsl:element name="{substring($style,5)}">
	   <xsl:call-template name="basicStyles">
	     <xsl:with-param name="parented">true</xsl:with-param>
	   </xsl:call-template>
	 </xsl:element>
       </xsl:when>

	<xsl:when test="doc-available('../../names.xml') and doc('../../names.xml')//tei:gi[.=$style]">
	  <xsl:element name="{$style}">
	   <xsl:call-template name="basicStyles">
	     <xsl:with-param name="parented">true</xsl:with-param>
	   </xsl:call-template>
	  </xsl:element>
	</xsl:when>
       
       <xsl:when test="not($style='')">
	 <hi rend="{replace($style,' ','_')}">
	   <xsl:call-template name="basicStyles">
	     <xsl:with-param name="parented">true</xsl:with-param>
	   </xsl:call-template>
	 </hi>
       </xsl:when>
       
       <xsl:otherwise>
	 <xsl:call-template name="basicStyles"/>
       </xsl:otherwise>
       
     </xsl:choose>
        
   </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Look at the Word
      underlying basic formatting. We can ignore the run's font change if 
      a) it's not a special para AND the font is the ISO default, OR 
      b) the font for the run is the same as its parent paragraph.</desc>
   </doc>
    <xsl:template name="basicStyles">
      <xsl:param name="parented">false</xsl:param>
      <xsl:param name="extrarow"  tunnel="yes"/>
     <xsl:param name="extracolumn"   tunnel="yes"/>     
     <xsl:variable name="styles">
       <xsl:choose>
	 <xsl:when test="w:rPr/w:rFonts  and not(w:rPr/w:rFonts/@w:ascii)"/>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii  and matches(parent::w:p/w:pPr/w:pStyle/@w:val,'Special')">
	   <s><xsl:text>font-family:</xsl:text>
	     <xsl:value-of select="w:rPr/w:rFonts/@w:ascii"/>
	   </s>
	 </xsl:when>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii='Cambria'"/>
	 <xsl:when test="matches(w:rPr/w:rFonts/@w:ascii,'^Times')"/>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii='Calibri'"/>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii='Arial'"/>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii='Verdana'"/>
	 <xsl:when test="w:rPr/w:rFonts/@w:ascii =
			 parent::w:p/w:pPr/w:rPr/w:rFonts/@w:ascii"/>
	 <xsl:when test="not(w:rPr/w:rFonts)"/>
	 <xsl:otherwise>
	   <s><xsl:text>font-family:</xsl:text>
	       <xsl:value-of select="w:rPr/w:rFonts/@w:ascii"/>
	   </s>
	   </xsl:otherwise>
	 </xsl:choose>
	 <!-- see also w:ascii="Courier New" w:hAnsi="Courier New" w:cs="Courier New" -->
	 <!-- what do we want to do about cs (Complex Scripts), hAnsi (high ANSI), eastAsia etc? -->
	 
	 <xsl:choose>
	   <xsl:when test="w:rPr/w:sz and $preserveFontSizeChanges='true'">
	     <s><xsl:text>font-size:</xsl:text>
	       <xsl:value-of select="number(w:rPr/w:sz/@w:val) div 2"/>
	       <xsl:text>pt</xsl:text>
	     </s>
	   </xsl:when>
	   <xsl:when test="ancestor::w:tc and $extrarow/w:rPr/w:sz">
	     <s><xsl:text>font-size:</xsl:text>
	       <xsl:value-of select="number($extrarow/w:rPr/w:sz/@w:val) div 2"/>
	       <xsl:text>pt</xsl:text>
	     </s>
	   </xsl:when>
	 <xsl:when test="ancestor::w:tc and $extracolumn/w:rPr/w:sz">
	   <s><xsl:text>font-size:</xsl:text>
	     <xsl:value-of select="number($extracolumn/w:rPr/w:sz/@w:val) div 2"/>
	     <xsl:text>pt</xsl:text>
	   </s>
	 </xsl:when>
	 </xsl:choose>
	 <xsl:if test="w:rPr/w:position/@w:val and not(w:rPr/w:position/@w:val='0')">
	 <s><xsl:text>position:</xsl:text>
	   <xsl:value-of select="w:rPr/w:position/@w:val"/>
	 </s>
	 </xsl:if>
     </xsl:variable>
     
     <xsl:variable name="dir">
	<!-- right-to-left text -->
	<xsl:if test="w:rPr/w:rtl or parent::w:p/w:pPr/w:rPr/w:rtl">
	  <xsl:text>rtl</xsl:text>
	</xsl:if>
      </xsl:variable>
     
      <xsl:variable name="effects">
	<xsl:call-template name="fromDocxEffectsHook"/>
	<xsl:if test="w:rPr/w:position[number(@w:val)&lt;-2] or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:position[number(@w:val)&lt;-2]
		      or $extrarow/w:rPr/w:position[number(@w:val)&lt;-2])
		      )">
	  <n>subscript</n>
	</xsl:if>
	
	<xsl:if test="w:rPr/w:i or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:i  or $extrarow/w:rPr/w:i)
		      )">
	  <n>italic</n>
	</xsl:if>

	<xsl:choose>
	  <xsl:when test="w:rPr/w:b/@w:val='0' or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:b/@w:val='0' or $extrarow/w:rPr/w:b/@w:val='0')
		      )">
	    <n>normalweight</n>
	  </xsl:when>
	  <xsl:when test="w:rPr/w:b or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:b  or $extrarow/w:rPr/w:b)
		      )">
	    <n>bold</n>
	  </xsl:when>
	</xsl:choose>

	<xsl:if test="w:rPr/w:position[number(@w:val)&gt;2] or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:position[number(@w:val)&gt;2]
		      or $extrarow/w:rPr/w:position[number(@w:val)&gt;2])
		      )">
	  <n>superscript</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:vertAlign or 
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:vertAlign  or $extrarow/w:rPr/w:vertAlign)
		      )">
	  <n>
	    <xsl:value-of select="w:rPr/w:vertAlign/@w:val"/>
	  </n>
	</xsl:if>

	<xsl:if test="w:rPr/w:strike or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:strike  or $extrarow/w:rPr/w:strike)
		      )">

	  <n>strikethrough</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:dstrike or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:dstrike  or $extrarow/w:rPr/w:dstrike)
		      )">
	  <n>doublestrikethrough</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:u[@w:val='single']">
	  <n>underline</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:u[@w:val='wave']">
	  <n>wavyunderline</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:u[@w:val='double']">
	  <n>doubleunderline</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:smallCaps or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:smallCaps  or $extrarow/w:rPr/w:smallCaps)
		      )">
	  <n>smallcaps</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:caps or
		      (ancestor::w:tc 
		      and
		      ($extracolumn/w:rPr/w:caps  or $extrarow/w:rPr/w:caps)
		      )">
	  <n>allcaps</n>
	</xsl:if>

	<xsl:if test="w:rPr/w:color and
		      not(w:rPr/w:color/@w:val='000000' or w:rPr/w:color/@w:val='auto')">
	  <n>
	    <xsl:text>color(</xsl:text>
	    <xsl:value-of select="w:rPr/w:color/@w:val"/>
	    <xsl:text>)</xsl:text>
	  </n>
	</xsl:if>

	<xsl:if test="w:rPr/w:highlight">
	  <n>
	    <xsl:text>background(</xsl:text>
	    <xsl:value-of select="w:rPr/w:highlight/@w:val"/>
	    <xsl:text>)</xsl:text>
	  </n>
	</xsl:if>
		
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="normalize-space(.)=''">
	    <xsl:apply-templates/>
	</xsl:when>
	<xsl:when test="$effects/* or ($styles/* and $preserveEffects='true')">
	  <xsl:element name="{if ($parented='true') then 'seg' else 'hi'}">
	    <xsl:if test="$dir!='' and $preserveEffects='true'">
	      <xsl:attribute name="dir"
			     xmlns="http://www.w3.org/2005/11/its"
			     select="$dir"/>
	    </xsl:if>
	    <xsl:choose>
	      <xsl:when test="$effects/*">
		<xsl:attribute name="rend">
		  <xsl:value-of select="$effects/*" separator=" "/>
		</xsl:attribute>
	      </xsl:when>
	    </xsl:choose>
	    <xsl:if test="$styles/* and $preserveEffects='true'">
	      <xsl:attribute name="style">
		<xsl:value-of select="($styles/*)" separator=";"/>
	      </xsl:attribute>
	    </xsl:if>
	    <xsl:if test="w:t[@xml:space='preserve']">
	      <xsl:attribute name="xml:space">preserve</xsl:attribute>
  	    </xsl:if>
	    <xsl:apply-templates/>
	  </xsl:element>
	</xsl:when>
	<xsl:otherwise>
	    <xsl:if test="w:t[@xml:space='preserve'] and $parented='true'">
	      <xsl:attribute name="xml:space">preserve</xsl:attribute>
	    </xsl:if>
	    <xsl:apply-templates/>
	</xsl:otherwise> 
      </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handle current page breaks inserted by Word</desc>
   </doc>
    <xsl:template match="w:lastRenderedPageBreak">
      <xsl:if test="$preserveSoftPageBreaks='true'">
	<pb type="soft"/>
      </xsl:if>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Handle Text, Comments, Tabs, Symbols etc. 
    </desc>
   </doc>
    <xsl:template match="w:t">
        <xsl:variable name="t">
            <xsl:choose>
                <xsl:when test="@xml:space='preserve' and string-length(normalize-space(.))=0">
		  <seg><xsl:value-of select="."/></seg>
                </xsl:when>
                <xsl:when test="@xml:space='preserve'">
                    <xsl:value-of select="."/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="normalize-space(.)"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="parent::w:r/w:rPr/w:rFonts[starts-with(@w:ascii,'ISO')]">
                <seg style="font-family:{parent::w:r/w:rPr/w:rFonts/@w:ascii};">
                    <xsl:value-of select="$t"/>
                </seg>
            </xsl:when>
        	<xsl:when test="parent::w:r/w:rPr/w:rFonts[starts-with(@w:ascii,'Symbol')]">
        		<seg style="font-family:{parent::w:r/w:rPr/w:rFonts/@w:ascii};">
        			<xsl:choose>
        				<!-- Fix for non-Unicode characters available in Symbol font -->
        				<!-- List from http://www.fileformat.info/info/unicode/font/symbol/nonunicode.htm -->
        				<xsl:when test="string-to-codepoints($t)=61472">&#xF020;</xsl:when>
        				<xsl:when test="string-to-codepoints($t)=61473">&#xF021;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61474">&#xF022;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61475">&#xF023;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61476">&#xF024;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61477">&#xF025;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61478">&#xF026;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61479">&#xF027;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61480">&#xF028;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61481">&#xF029;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61482">&#xF02A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61483">&#xF02B;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61484">&#xF02C;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61485">&#xF02D;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61486">&#xF02E;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61487">&#xF02F;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61488">&#xF030;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61489">&#xF031;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61490">&#xF032;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61491">&#xF033;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61492">&#xF034;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61493">&#xF035;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61494">&#xF036;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61495">&#xF037;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61496">&#xF038;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61497">&#xF039;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61498">&#xF03A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61499">&#xF03B;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61500">&#xF03C;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61501">&#xF03D;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61502">&#xF03E;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61503">&#xF03F;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61504">&#xF040;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61505">&#xF041;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61506">&#xF042;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61507">&#xF043;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61508">&#xF044;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61509">&#xF045;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61510">&#xF046;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61511">&#xF047;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61512">&#xF048;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61513">&#xF049;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61514">&#xF04A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61515">&#xF04B;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61516">&#xF04C;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61517">&#xF04D;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61518">&#xF04E;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61519">&#xF04F;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61520">&#xF050;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61521">&#xF051;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61522">&#xF052;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61523">&#xF053;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61524">&#xF054;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61525">&#xF055;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61526">&#xF056;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61527">&#xF057;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61528">&#xF058;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61529">&#xF059;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61530">&#xF05A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61531">&#xF05B;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61532">&#xF05C;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61533">&#xF05D;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61534">&#xF05E;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61535">&#xF05F;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61536">&#xF060;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61537">&#xF061;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61538">&#xF062;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61539">&#xF063;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61540">&#xF064;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61541">&#xF065;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61542">&#xF066;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61543">&#xF067;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61544">&#xF068;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61545">&#xF069;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61546">&#xF06A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61547">&#xF06B;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61548">&#xF06C;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61549">&#xF06D;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61550">&#xF06E;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61551">&#xF06F;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61552">&#xF070;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61553">&#xF071;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61554">&#xF072;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61555">&#xF073;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61556">&#xF074;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61557">&#xF075;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61558">&#xF076;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61559">&#xF077;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61560">&#xF078;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61561">&#xF079;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61562">&#xF07A;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61601">&#xF0A1;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61602">&#xF0A2;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61603">&#xF0A3;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61604">&#xF0A4;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61605">&#xF0A5;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61606">&#xF0A6;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61607">&#xF0A7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61608">&#xF0A8;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61609">&#xF0A9;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61610">&#xF0AA;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61611">&#xF0AB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61612">&#xF0AC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61613">&#xF0AD;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61614">&#xF0AE;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61615">&#xF0AF;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61616">&#xF0B0;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61617">&#xF0B1;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61618">&#xF0B2;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61619">&#xF0B3;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61620">&#xF0B4;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61621">&#xF0B5;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61622">&#xF0B6;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61623">&#xF0B7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61627">&#xF0BB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61628">&#xF0BC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61629">&#xF0BD;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61630">&#xF0BE;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61631">&#xF0BF;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61632">&#xF0C0;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61633">&#xF0C1;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61634">&#xF0C2;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61635">&#xF0C3;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61636">&#xF0C4;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61637">&#xF0C5;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61638">&#xF0C6;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61639">&#xF0C7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61640">&#xF0C8;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61641">&#xF0C9;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61642">&#xF0CA;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61643">&#xF0CB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61644">&#xF0CC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61645">&#xF0CD;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61646">&#xF0CE;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61647">&#xF0CF;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61648">&#xF0D0;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61649">&#xF0D1;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61655">&#xF0D7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61656">&#xF0D8;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61657">&#xF0D9;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61658">&#xF0DA;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61659">&#xF0DB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61660">&#xF0DC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61661">&#xF0DD;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61662">&#xF0DE;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61663">&#xF0DF;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61664">&#xF0E0;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61665">&#xF0E1;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61666">&#xF0E2;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61667">&#xF0E3;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61668">&#xF0E4;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61669">&#xF0E5;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61670">&#xF0E6;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61671">&#xF0E7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61672">&#xF0E8;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61673">&#xF0E9;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61674">&#xF0EA;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61675">&#xF0EB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61676">&#xF0EC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61677">&#xF0ED;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61684">&#xF0F4;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61685">&#xF0F5;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61686">&#xF0F6;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61687">&#xF0F7;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61688">&#xF0F8;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61689">&#xF0F9;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61690">&#xF0FA;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61691">&#xF0FB;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61692">&#xF0FC;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61693">&#xF0FD;</xsl:when> 
        				<xsl:when test="string-to-codepoints($t)=61694">&#xF0FE;</xsl:when> 
        				
        				<xsl:otherwise>
        					<xsl:value-of select="$t"/>
        					<xsl:message>Warning: Some Symbol fonts may not convert properly. <xsl:value-of select="$t"/></xsl:message>
        				</xsl:otherwise>
        			</xsl:choose>
        		</seg>
        	</xsl:when>
        	<xsl:otherwise>
                <xsl:copy-of select="$t"/>
            </xsl:otherwise>            
        </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Convert special characters (w:syms) into Unicode characters or
	<gi>g</gi> elements. Symbol to Unicode mapping from http://unicode.org/Public/MAPPINGS/VENDORS/ADOBE/symbol.txt</desc>
   </doc>
    <xsl:template match="w:sym">
      <xsl:choose>
	<xsl:when test="@w:font='Symbol' or @w:font='MT Symbol'">
	  <xsl:choose>
<xsl:when test="@w:char='F022'">&#x2200;</xsl:when><!--	# FOR ALL # universal -->
<xsl:when test="@w:char='F024'">&#x2203;</xsl:when><!--	# THERE EXISTS # existential -->
<xsl:when test="@w:char='F025'">&#x0025;</xsl:when><!--	# PERCENT SIGN # percent -->
<xsl:when test="@w:char='F026'">&#x0026;</xsl:when><!--	# AMPERSAND # ampersand -->
<xsl:when test="@w:char='F027'">&#x220B;</xsl:when><!--	# CONTAINS AS MEMBER	# suchthat -->
<xsl:when test="@w:char='F028'">&#x0028;</xsl:when><!--	# LEFT PARENTHESIS	# parenleft -->
<xsl:when test="@w:char='F029'">&#x0029;</xsl:when><!--	# RIGHT PARENTHESIS	# parenright -->
<xsl:when test="@w:char='F02A'">&#x2217;</xsl:when><!--	# ASTERISK OPERATOR	# asteriskmath -->
<xsl:when test="@w:char='F02B'">&#x002B;</xsl:when><!--	# PLUS SIGN # plus -->
<xsl:when test="@w:char='F02C'">&#x002C;</xsl:when><!--	# COMMA	# comma -->
<xsl:when test="@w:char='F02D'">&#x2212;</xsl:when><!--	# MINUS SIGN # minus -->
<xsl:when test="@w:char='F02E'">&#x002E;</xsl:when><!--	# FULL STOP # period -->
<xsl:when test="@w:char='F02F'">&#x002F;</xsl:when><!--	# SOLIDUS # slash -->
<xsl:when test="@w:char='F030'">&#x0030;</xsl:when>
<xsl:when test="@w:char='F031'">&#x0031;</xsl:when><!--	# DIGIT ONE # one -->
<xsl:when test="@w:char='F032'">&#x0032;</xsl:when><!--	# DIGIT TWO # two -->
<xsl:when test="@w:char='F033'">&#x0033;</xsl:when><!--	# DIGIT THREE # three -->
<xsl:when test="@w:char='F034'">&#x0034;</xsl:when><!--	# DIGIT FOUR # four -->
<xsl:when test="@w:char='F035'">&#x0035;</xsl:when><!--	# DIGIT FIVE # five -->
<xsl:when test="@w:char='F036'">&#x0036;</xsl:when><!--	# DIGIT SIX # six -->
<xsl:when test="@w:char='F037'">&#x0037;</xsl:when><!--	# DIGIT SEVEN # seven -->
<xsl:when test="@w:char='F038'">&#x0038;</xsl:when>
<xsl:when test="@w:char='F039'">&#x0039;</xsl:when><!--	# DIGIT NINE # nine -->
<xsl:when test="@w:char='F03A'">&#x003A;</xsl:when><!--	# COLON	# colon -->
<xsl:when test="@w:char='F03B'">&#x003B;</xsl:when><!--	# SEMICOLON # semicolon -->
<xsl:when test="@w:char='F03C'">&#x003C;</xsl:when><!--	# LESS-THAN SIGN	# less -->
<xsl:when test="@w:char='F03D'">&#x003D;</xsl:when><!--	# EQUALS SIGN # equal -->
<xsl:when test="@w:char='F03E'">&#x003E;</xsl:when><!--	# GREATER-THAN SIGN	# greater -->
<xsl:when test="@w:char='F03F'">&#x003F;</xsl:when><!--	# QUESTION MARK	# question -->
<xsl:when test="@w:char='F040'">&#x2245;</xsl:when><!--	# APPROXIMATELY EQUAL TO	# congruent -->
<xsl:when test="@w:char='F041'">&#x0391;</xsl:when><!--	# GREEK CAPITAL LETTER ALPHA	# Alpha -->
<xsl:when test="@w:char='F042'">&#x0392;</xsl:when><!--	# GREEK CAPITAL LETTER BETA	# Beta -->
<xsl:when test="@w:char='F043'">&#x03A7;</xsl:when><!--	# GREEK CAPITAL LETTER CHI	# Chi -->
<xsl:when test="@w:char='F044'">&#x0394;</xsl:when><!--	# GREEK CAPITAL LETTER DELTA	# Delta -->
<xsl:when test="@w:char='F044'">&#x2206;</xsl:when><!--	# INCREMENT # Delta -->
<xsl:when test="@w:char='F045'">&#x0395;</xsl:when><!--	# GREEK CAPITAL LETTER EPSILON	# Epsilon -->
<xsl:when test="@w:char='F046'">&#x03A6;</xsl:when><!--	# GREEK CAPITAL LETTER PHI	# Phi -->
<xsl:when test="@w:char='F047'">&#x0393;</xsl:when><!--	# GREEK CAPITAL LETTER GAMMA	# Gamma -->
<xsl:when test="@w:char='F048'">&#x0397;</xsl:when><!--	# GREEK CAPITAL LETTER ETA	# Eta -->
<xsl:when test="@w:char='F049'">&#x0399;</xsl:when><!--	# GREEK CAPITAL LETTER IOTA	# Iota -->
<xsl:when test="@w:char='F04A'">&#x03D1;</xsl:when><!--	# GREEK THETA SYMBOL	# theta1 -->
<xsl:when test="@w:char='F04B'">&#x039A;</xsl:when><!--	# GREEK CAPITAL LETTER KAPPA	# Kappa -->
<xsl:when test="@w:char='F04C'">&#x039B;</xsl:when><!--	# GREEK CAPITAL LETTER LAMDA	# Lambda -->
<xsl:when test="@w:char='F04D'">&#x039C;</xsl:when><!--	# GREEK CAPITAL LETTER MU	# Mu -->
<xsl:when test="@w:char='F04E'">&#x039D;</xsl:when><!--	# GREEK CAPITAL LETTER NU	# Nu -->
<xsl:when test="@w:char='F04F'">&#x039F;</xsl:when><!--	# GREEK CAPITAL LETTER OMICRON	# Omicron -->
<xsl:when test="@w:char='F050'">&#x03A0;</xsl:when><!--	# GREEK CAPITAL LETTER PI	# Pi -->
<xsl:when test="@w:char='F051'">&#x0398;</xsl:when><!--	# GREEK CAPITAL LETTER THETA	# Theta -->
<xsl:when test="@w:char='F052'">&#x03A1;</xsl:when><!--	# GREEK CAPITAL LETTER RHO	# Rho -->
<xsl:when test="@w:char='F053'">&#x03A3;</xsl:when><!--	# GREEK CAPITAL LETTER SIGMA	# Sigma -->
<xsl:when test="@w:char='F054'">&#x03A4;</xsl:when><!--	# GREEK CAPITAL LETTER TAU	# Tau -->
<xsl:when test="@w:char='F055'">&#x03A5;</xsl:when><!--	# GREEK CAPITAL LETTER UPSILON	# Upsilon -->
<xsl:when test="@w:char='F056'">&#x03C2;</xsl:when>
<xsl:when test="@w:char='F057'">&#x03A9;</xsl:when><!--	# GREEK CAPITAL LETTER OMEGA	# Omega -->
<xsl:when test="@w:char='F057'">&#x2126;</xsl:when><!--	# OHM SIGN # Omega -->
<xsl:when test="@w:char='F058'">&#x039E;</xsl:when><!--	# GREEK CAPITAL LETTER XI	# Xi -->
<xsl:when test="@w:char='F059'">&#x03A8;</xsl:when><!--	# GREEK CAPITAL LETTER PSI	# Psi -->
<xsl:when test="@w:char='F05A'">&#x0396;</xsl:when><!--	# GREEK CAPITAL LETTER ZETA	# Zeta -->
<xsl:when test="@w:char='F05B'">&#x005B;</xsl:when><!--	# LEFT SQUARE BRACKET	# bracketleft -->
<xsl:when test="@w:char='F05C'">&#x2234;</xsl:when><!--	# THEREFORE # therefore -->
<xsl:when test="@w:char='F05D'">&#x005D;</xsl:when><!--	# RIGHT SQUARE BRACKET	# bracketright -->
<xsl:when test="@w:char='F05E'">&#x22A5;</xsl:when><!--	# UP TACK # perpendicular -->
<xsl:when test="@w:char='F05F'">&#x005F;</xsl:when><!--	# LOW LINE # underscore -->
<xsl:when test="@w:char='F060'">&#xF8E5;</xsl:when><!--	# RADICAL EXTENDER	# radicalex (CUS) -->
<xsl:when test="@w:char='F061'">&#x03B1;</xsl:when><!--	# GREEK SMALL LETTER ALPHA	# alpha -->
<xsl:when test="@w:char='F062'">&#x03B2;</xsl:when><!--	# GREEK SMALL LETTER BETA	# beta -->
<xsl:when test="@w:char='F063'">&#x03C7;</xsl:when><!--	# GREEK SMALL LETTER CHI	# chi -->
<xsl:when test="@w:char='F064'">&#x03B4;</xsl:when><!--	# GREEK SMALL LETTER DELTA	# delta -->
<xsl:when test="@w:char='F065'">&#x03B5;</xsl:when><!--	# GREEK SMALL LETTER EPSILON	# epsilon -->
<xsl:when test="@w:char='F066'">&#x03C6;</xsl:when><!--	# GREEK SMALL LETTER PHI	# phi -->
<xsl:when test="@w:char='F067'">&#x03B3;</xsl:when><!--	# GREEK SMALL LETTER GAMMA	# gamma -->
<xsl:when test="@w:char='F068'">&#x03B7;</xsl:when><!--	# GREEK SMALL LETTER ETA	# eta -->
<xsl:when test="@w:char='F069'">&#x03B9;</xsl:when><!--	# GREEK SMALL LETTER IOTA	# iota -->
<xsl:when test="@w:char='F06A'">&#x03D5;</xsl:when><!--	# GREEK PHI SYMBOL	# phi1 -->
<xsl:when test="@w:char='F06B'">&#x03BA;</xsl:when><!--	# GREEK SMALL LETTER KAPPA	# kappa -->
<xsl:when test="@w:char='F06C'">&#x03BB;</xsl:when><!--	# GREEK SMALL LETTER LAMDA	# lambda -->
<xsl:when test="@w:char='F06D'">&#x00B5;</xsl:when><!--	# MICRO SIGN # mu -->
<xsl:when test="@w:char='F06D'">&#x03BC;</xsl:when><!--	# GREEK SMALL LETTER MU	# mu -->
<xsl:when test="@w:char='F06E'">&#x03BD;</xsl:when><!--	# GREEK SMALL LETTER NU	# nu -->
<xsl:when test="@w:char='F06F'">&#x03BF;</xsl:when><!--	# GREEK SMALL LETTER OMICRON	# omicron -->
<xsl:when test="@w:char='F070'">&#x03C0;</xsl:when><!--	# GREEK SMALL LETTER PI	# pi -->
<xsl:when test="@w:char='F071'">&#x03B8;</xsl:when>
<xsl:when test="@w:char='F072'">&#x03C1;</xsl:when><!--	# GREEK SMALL LETTER RHO	# rho -->
<xsl:when test="@w:char='F073'">&#x03C3;</xsl:when><!--	# GREEK SMALL LETTER SIGMA	# sigma -->
<xsl:when test="@w:char='F074'">&#x03C4;</xsl:when><!--	# GREEK SMALL LETTER TAU	# tau -->
<xsl:when test="@w:char='F075'">&#x03C5;</xsl:when><!--	# GREEK SMALL LETTER UPSILON	# upsilon -->
<xsl:when test="@w:char='F076'">&#x03D6;</xsl:when><!--	# GREEK PI SYMBOL	# omega1 -->
<xsl:when test="@w:char='F077'">&#x03C9;</xsl:when><!--	# GREEK SMALL LETTER OMEGA	# omega -->
<xsl:when test="@w:char='F078'">&#x03BE;</xsl:when><!--	# GREEK SMALL LETTER XI	# xi -->
<xsl:when test="@w:char='F079'">&#x03C8;</xsl:when><!--	# GREEK SMALL LETTER PSI	# psi -->
<xsl:when test="@w:char='F07A'">&#x03B6;</xsl:when><!--	# GREEK SMALL LETTER ZETA	# zeta -->
<xsl:when test="@w:char='F07B'">&#x007B;</xsl:when><!--	# LEFT CURLY BRACKET	# braceleft -->
<xsl:when test="@w:char='F07C'">&#x007C;</xsl:when><!--	# VERTICAL LINE	# bar -->
<xsl:when test="@w:char='F07D'">&#x007D;</xsl:when><!--	# RIGHT CURLY BRACKET	# braceright -->
<xsl:when test="@w:char='F07E'">&#x223C;</xsl:when><!--	# TILDE OPERATOR	# similar -->
<xsl:when test="@w:char='F0A0'">&#x20AC;</xsl:when><!--	# EURO SIGN # Euro -->
<xsl:when test="@w:char='F0A1'">&#x03D2;</xsl:when><!--	# GREEK UPSILON WITH HOOK SYMBOL	# Upsilon1 -->
<xsl:when test="@w:char='F0A2'">&#x2032;</xsl:when><!--	# PRIME	# minute -->
<xsl:when test="@w:char='F0A3'">&#x2264;</xsl:when><!--	# LESS-THAN OR EQUAL TO	# lessequal -->
<xsl:when test="@w:char='F0A4'">&#x2044;</xsl:when><!--	# FRACTION SLASH	# fraction -->
<xsl:when test="@w:char='F0A4'">&#x2215;</xsl:when><!--	# DIVISION SLASH	# fraction -->
<xsl:when test="@w:char='F0A5'">&#x221E;</xsl:when><!--	# INFINITY # infinity -->
<xsl:when test="@w:char='F0A6'">&#x0192;</xsl:when><!--	# LATIN SMALL LETTER F WITH HOOK	# florin -->
<xsl:when test="@w:char='F0A7'">&#x2663;</xsl:when><!--	# BLACK CLUB SUIT	# club -->
<xsl:when test="@w:char='F0A8'">&#x2666;</xsl:when><!--	# BLACK DIAMOND SUIT	# diamond -->
<xsl:when test="@w:char='F0A9'">&#x2665;</xsl:when><!--	# BLACK HEART SUIT	# heart -->
<xsl:when test="@w:char='F0AA'">&#x2660;</xsl:when><!--	# BLACK SPADE SUIT	# spade -->
<xsl:when test="@w:char='F0AB'">&#x2194;</xsl:when><!--	# LEFT RIGHT ARROW	# arrowboth -->
<xsl:when test="@w:char='F0AC'">&#x2190;</xsl:when><!--	# LEFTWARDS ARROW	# arrowleft -->
<xsl:when test="@w:char='F0AD'">&#x2191;</xsl:when><!--	# UPWARDS ARROW	# arrowup -->
<xsl:when test="@w:char='F0AE'">&#x2192;</xsl:when><!--	# RIGHTWARDS ARROW	# arrowright -->
<xsl:when test="@w:char='F0AF'">&#x2193;</xsl:when><!--	# DOWNWARDS ARROW	# arrowdown -->
<xsl:when test="@w:char='F0B0'">&#x00B0;</xsl:when><!--	# DEGREE SIGN # degree -->
<xsl:when test="@w:char='F0B1'">&#x00B1;</xsl:when><!--	# PLUS-MINUS SIGN	# plusminus -->
<xsl:when test="@w:char='F0B2'">&#x2033;</xsl:when><!--	# DOUBLE PRIME # second -->
<xsl:when test="@w:char='F0B3'">&#x2265;</xsl:when><!--	# GREATER-THAN OR EQUAL TO	# greaterequal -->
<xsl:when test="@w:char='F0B4'">&#x00D7;</xsl:when><!--	# MULTIPLICATION SIGN	# multiply -->
<xsl:when test="@w:char='F0B5'">&#x221D;</xsl:when><!--	# PROPORTIONAL TO	# proportional -->
<xsl:when test="@w:char='F0B6'">&#x2202;</xsl:when><!--	# PARTIAL DIFFERENTIAL	# partialdiff -->
<xsl:when test="@w:char='F0B7'">&#x2022;</xsl:when><!--	# BULLET # bullet -->
<xsl:when test="@w:char='F0B8'">&#x00F7;</xsl:when><!--	# DIVISION SIGN	# divide -->
<xsl:when test="@w:char='F0B9'">&#x2260;</xsl:when><!--	# NOT EQUAL TO # notequal -->
<xsl:when test="@w:char='F0BA'">&#x2261;</xsl:when><!--	# IDENTICAL TO # equivalence -->
<xsl:when test="@w:char='F0BB'">&#x2248;</xsl:when><!--	# ALMOST EQUAL TO	# approxequal -->
<xsl:when test="@w:char='F0BC'">&#x2026;</xsl:when><!--	# HORIZONTAL ELLIPSIS	# ellipsis -->
<xsl:when test="@w:char='F0BD'">&#xF8E6;</xsl:when><!--	# VERTICAL ARROW EXTENDER	# arrowvertex (CUS) -->
<xsl:when test="@w:char='F0BE'">&#xF8E7;</xsl:when><!--	# HORIZONTAL ARROW EXTENDER	# arrowhorizex (CUS) -->
<xsl:when test="@w:char='F0BF'">&#x21B5;</xsl:when><!--	# DOWNWARDS ARROW WITH CORNER LEFTWARDS	# carriagereturn -->
<xsl:when test="@w:char='F0C0'">&#x2135;</xsl:when><!--	# ALEF SYMBOL # aleph -->
<xsl:when test="@w:char='F0C1'">&#x2111;</xsl:when><!--	# BLACK-LETTER CAPITAL I	# Ifraktur -->
<xsl:when test="@w:char='F0C2'">&#x211C;</xsl:when><!--	# BLACK-LETTER CAPITAL R	# Rfraktur -->
<xsl:when test="@w:char='F0C3'">&#x2118;</xsl:when><!--	# SCRIPT CAPITAL P	# weierstrass -->
<xsl:when test="@w:char='F0C4'">&#x2297;</xsl:when><!--	# CIRCLED TIMES	# circlemultiply -->
<xsl:when test="@w:char='F0C5'">&#x2295;</xsl:when><!--	# CIRCLED PLUS # circleplus -->
<xsl:when test="@w:char='F0C6'">&#x2205;</xsl:when><!--	# EMPTY SET # emptyset -->
<xsl:when test="@w:char='F0C7'">&#x2229;</xsl:when><!--	# INTERSECTION # intersection -->
<xsl:when test="@w:char='F0C8'">&#x222A;</xsl:when><!--	# UNION	# union -->
<xsl:when test="@w:char='F0C9'">&#x2283;</xsl:when><!--	# SUPERSET OF # propersuperset -->
<xsl:when test="@w:char='F0CA'">&#x2287;</xsl:when><!--	# SUPERSET OF OR EQUAL TO	# reflexsuperset -->
<xsl:when test="@w:char='F0CB'">&#x2284;</xsl:when><!--	# NOT A SUBSET OF	# notsubset -->
<xsl:when test="@w:char='F0CC'">&#x2282;</xsl:when><!--	# SUBSET OF # propersubset -->
<xsl:when test="@w:char='F0CD'">&#x2286;</xsl:when><!--	# SUBSET OF OR EQUAL TO	# reflexsubset -->
<xsl:when test="@w:char='F0CE'">&#x2208;</xsl:when><!--	# ELEMENT OF # element -->
<xsl:when test="@w:char='F0CF'">&#x2209;</xsl:when><!--	# NOT AN ELEMENT OF	# notelement -->
<xsl:when test="@w:char='F0D0'">&#x2220;</xsl:when><!--	# ANGLE	# angle -->
<xsl:when test="@w:char='F0D1'">&#x2207;</xsl:when><!--	# NABLA	# gradient -->
<xsl:when test="@w:char='F0D2'">&#xF6DA;</xsl:when><!--	# REGISTERED SIGN SERIF	# registerserif (CUS) -->
<xsl:when test="@w:char='F0D3'">&#xF6D9;</xsl:when><!--	# COPYRIGHT SIGN SERIF	# copyrightserif (CUS) -->
<xsl:when test="@w:char='F0D4'">&#xF6DB;</xsl:when><!--	# TRADE MARK SIGN SERIF	# trademarkserif (CUS) -->
<xsl:when test="@w:char='F0D5'">&#x220F;</xsl:when><!--	# N-ARY PRODUCT	# product -->
<xsl:when test="@w:char='F0D6'">&#x221A;</xsl:when><!--	# SQUARE ROOT # radical -->
<xsl:when test="@w:char='F0D7'">&#x22C5;</xsl:when><!--	# DOT OPERATOR # dotmath -->
<xsl:when test="@w:char='F0D8'">&#x00AC;</xsl:when><!--	# NOT SIGN # logicalnot -->
<xsl:when test="@w:char='F0D9'">&#x2227;</xsl:when><!--	# LOGICAL AND # logicaland -->
<xsl:when test="@w:char='F0DA'">&#x2228;</xsl:when><!--	# LOGICAL OR # logicalor -->
<xsl:when test="@w:char='F0DB'">&#x21D4;</xsl:when><!--	# LEFT RIGHT DOUBLE ARROW	# arrowdblboth -->
<xsl:when test="@w:char='F0DC'">&#x21D0;</xsl:when><!--	# LEFTWARDS DOUBLE ARROW	# arrowdblleft -->
<xsl:when test="@w:char='F0DD'">&#x21D1;</xsl:when><!--	# UPWARDS DOUBLE ARROW	# arrowdblup -->
<xsl:when test="@w:char='F0DE'">&#x21D2;</xsl:when><!--	# RIGHTWARDS DOUBLE ARROW	# arrowdblright -->
<xsl:when test="@w:char='F0DF'">&#x21D3;</xsl:when><!--	# DOWNWARDS DOUBLE ARROW	# arrowdbldown -->
<xsl:when test="@w:char='F0E0'">&#x25CA;</xsl:when><!--	# LOZENGE # lozenge -->
<xsl:when test="@w:char='F0E1'">&#x2329;</xsl:when><!--	# LEFT-POINTING ANGLE BRACKET	# angleleft -->
<xsl:when test="@w:char='F0E2'">&#xF8E8;</xsl:when><!--	# REGISTERED SIGN SANS SERIF	# registersans (CUS) -->
<xsl:when test="@w:char='F0E3'">&#xF8E9;</xsl:when><!--	# COPYRIGHT SIGN SANS SERIF	# copyrightsans (CUS) -->
<xsl:when test="@w:char='F0E4'">&#xF8EA;</xsl:when><!--	# TRADE MARK SIGN SANS SERIF	# trademarksans (CUS) -->
<xsl:when test="@w:char='F0E5'">&#x2211;</xsl:when><!--	# N-ARY SUMMATION	# summation -->
<xsl:when test="@w:char='F0E6'">&#xF8EB;</xsl:when><!--	# LEFT PAREN TOP	# parenlefttp (CUS) -->
<xsl:when test="@w:char='F0E7'">&#xF8EC;</xsl:when><!--	# LEFT PAREN EXTENDER	# parenleftex (CUS) -->
<xsl:when test="@w:char='F0E8'">&#xF8ED;</xsl:when><!--	# LEFT PAREN BOTTOM	# parenleftbt (CUS) -->
<xsl:when test="@w:char='F0E9'">&#xF8EE;</xsl:when><!--	# LEFT SQUARE BRACKET TOP	# bracketlefttp (CUS) -->
<xsl:when test="@w:char='F0EA'">&#xF8EF;</xsl:when><!--	# LEFT SQUARE BRACKET EXTENDER	# bracketleftex (CUS) -->
<xsl:when test="@w:char='F0EB'">&#xF8F0;</xsl:when><!--	# LEFT SQUARE BRACKET BOTTOM	# bracketleftbt (CUS) -->
<xsl:when test="@w:char='F0EC'">&#xF8F1;</xsl:when><!--	# LEFT CURLY BRACKET TOP	# bracelefttp (CUS) -->
<xsl:when test="@w:char='F0ED'">&#xF8F2;</xsl:when><!--	# LEFT CURLY BRACKET MID	# braceleftmid (CUS) -->
<xsl:when test="@w:char='F0EE'">&#xF8F3;</xsl:when><!--	# LEFT CURLY BRACKET BOTTOM	# braceleftbt (CUS) -->
<xsl:when test="@w:char='F0EF'">&#xF8F4;</xsl:when><!--	# CURLY BRACKET EXTENDER	# braceex (CUS) -->
<xsl:when test="@w:char='F0F1'">&#x232A;</xsl:when><!--	# RIGHT-POINTING ANGLE BRACKET	# angleright -->
<xsl:when test="@w:char='F0F2'">&#x222B;</xsl:when><!--	# INTEGRAL # integral -->
<xsl:when test="@w:char='F0F3'">&#x2320;</xsl:when><!--	# TOP HALF INTEGRAL	# integraltp -->
<xsl:when test="@w:char='F0F4'">&#xF8F5;</xsl:when><!--	# INTEGRAL EXTENDER	# integralex (CUS) -->
<xsl:when test="@w:char='F0F5'">&#x2321;</xsl:when><!--	# BOTTOM HALF INTEGRAL	# integralbt -->
<xsl:when test="@w:char='F0F6'">&#xF8F6;</xsl:when><!--	# RIGHT PAREN TOP	# parenrighttp (CUS) -->
<xsl:when test="@w:char='F0F7'">&#xF8F7;</xsl:when><!--	# RIGHT PAREN EXTENDER	# parenrightex (CUS) -->
<xsl:when test="@w:char='F0F8'">&#xF8F8;</xsl:when><!--	# RIGHT PAREN BOTTOM	# parenrightbt (CUS) -->
<xsl:when test="@w:char='F0F9'">&#xF8F9;</xsl:when><!--	# RIGHT SQUAREBRACKET TOP	# bracketrighttp (CUS) -->
<xsl:when test="@w:char='F0FA'">&#xF8FA;</xsl:when><!--	# RIGHT SQUARE BRACKET EXTENDER	# bracketrightex (CUS) -->
<xsl:when test="@w:char='F0FB'">&#xF8FB;</xsl:when><!--	# RIGHT SQUARE BRACKET BOTTOM	# bracketrightbt (CUS) -->
<xsl:when test="@w:char='F0FC'">&#xF8FC;</xsl:when><!--	# RIGHT CURLY BRACKET TOP	# bracerighttp (CUS) -->
<xsl:when test="@w:char='F0FD'">&#xF8FD;</xsl:when><!--	# RIGHT CURLY BRACKET MID	# bracerightmid (CUS) -->
<xsl:when test="@w:char='F0FE'">&#xF8FE;</xsl:when><!--	# RIGHT CURLY BRACKET BOTTOM	# bracerightbt (CUS) -->
	<xsl:otherwise> 	  
	  <g style="font-family:{@w:font};" n="{@w:char}"/>
	</xsl:otherwise>       
      </xsl:choose> 	
    </xsl:when>
    <xsl:when test="@w:font='Wingdings 2' and @w:char='F050'">&#x2713;</xsl:when><!-- tick mark-->
      	<xsl:when test="@w:font='Wingdings' and @w:char='F05B'">&#x262F;</xsl:when><!-- yin-yang -->
      	<xsl:otherwise> 	  
	  <g style="font-family:{@w:font};" n="{@w:char}"/>
	</xsl:otherwise>       
      </xsl:choose>
    </xsl:template>     
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>handle tabs</desc>
   </doc>
    <xsl:template match="w:r/w:tab">
      <xsl:text>	</xsl:text>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>handle ptabs (absolute position tab character)
    </desc>
   </doc>
    <xsl:template match="w:r/w:ptab">
        <c rend="ptab" type="{@w:alignment}">
            <xsl:text>	</xsl:text>
        </c>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>capture line breaks</desc>
   </doc>
    <xsl:template match="w:br">
      <xsl:choose>
	<xsl:when test="@w:type='page'">
	  <pb/>
	</xsl:when>
	<xsl:when test="@w:type='column'">
	  <cb/>
	</xsl:when>
	<xsl:otherwise>
	  <lb/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Contains text that has been tracked as a revision. 
    </desc>
   </doc>

    <xsl:template match="w:del">
      <xsl:choose>
	<xsl:when test="$processChangeInformation='true'">
	  <del when="{@w:date}">
	    <xsl:call-template name="identifyChange">
	      <xsl:with-param name="who" select="@w:author"/>
	    </xsl:call-template>
	    <xsl:apply-templates/>
	  </del>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="w:rPr/w:del"/>

    <xsl:template match="w:delText">
      <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="w:ins">
      <xsl:choose>
	<xsl:when test="$processChangeInformation='true'">
	  <add when="{@w:date}">
	    <xsl:call-template name="identifyChange">
	      <xsl:with-param name="who" select="@w:author"/>
	    </xsl:call-template>
	    <xsl:call-template name="processTextrun"/>
	  </add>
	</xsl:when>
	<xsl:when test="w:r">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="processTextrun"/>
	</xsl:otherwise>
      </xsl:choose>      
    </xsl:template>
	
    <xsl:template match="w:rPr/w:ins"/>
 
    
  <xsl:template match="w:noBreakHyphen">
    <xsl:text>&#x2011;</xsl:text>
  </xsl:template>


  <xsl:template name="fromDocxEffectsHook"/>

</xsl:stylesheet>

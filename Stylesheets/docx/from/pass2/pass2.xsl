<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
		xmlns:html="http://www.w3.org/1999/xhtml"
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
                exclude-result-prefixes="a cp dc dcterms dcmitype prop  html   iso m mml mo mv o pic r rel cals     tbx tei teidocx v xs ve w10 w wne wp">
    
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
    
    
    <xsl:template match="@*|comment()|processing-instruction()" mode="pass2">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="pass2">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
        </xsl:copy>
    </xsl:template>
    
    
    <xsl:template match="text()" mode="pass2">
	  <xsl:value-of select="."/>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Zap empty p and item </p>
      </desc>
    </doc>
 
   <xsl:template match="tei:p[not(*) and string-length(.)=0]"
		 mode="pass2" priority="99"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Inner lists in lists must be moved to inside items
	 </p>
      </desc>
    </doc>
    <xsl:template match="tei:list/tei:list" mode="pass2"/>
    <xsl:template match="tei:item" mode="pass2">
      <xsl:choose>
	<xsl:when test="not(*) and string-length(.)=0"/>
	<xsl:otherwise>
	  <item>
	    <xsl:copy-of select="@*"/>
	    <xsl:variable name="me" select="generate-id()"/>
	    <xsl:apply-templates mode="pass2"/>
	    <!-- find following sibling lists and notes -->
	    <xsl:for-each select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
	      <list>
		<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
	      </list>
	    </xsl:for-each>
	  </item>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Zap emdashes at start of head </p>
      </desc>
    </doc>
    <xsl:template match="tei:head/text()" mode="pass2">
        <xsl:choose>
            <xsl:when test="starts-with(.,'— ')">
                <xsl:value-of select="substring(.,3)"/>
            </xsl:when>
            <xsl:when test="starts-with(.,'&#160;— ')">
                <xsl:value-of select="substring(.,4)"/>
            </xsl:when>
            <xsl:when test="starts-with(.,' — ')">
                <xsl:value-of select="substring(.,4)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A &lt;seg&gt; which does nothing is not worth having </p>
      </desc>
    </doc>
    <xsl:template match="tei:seg[not(@*)]" mode="pass2">
        <xsl:choose>
            <xsl:when test="parent::tei:formula and normalize-space(.)=''"/>
	    <xsl:when test=".=' ' and following-sibling::node()[1][self::tei:hi]/@rend=
			    preceding-sibling::node()[1][self::tei:hi]/@rend"/>

            <xsl:when test="parent::*/text()">
                <xsl:value-of select="."/>
            </xsl:when>
	    <xsl:when test="parent::tei:hi[count(*)=1]">
	      <xsl:value-of select="."/>

	    </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="."/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Look at the sections we have generated, and put
        them in &lt;front&gt; or &lt;body&gt; as appropriate</p>
      </desc>
    </doc>

    <xsl:template match="tei:text" mode="pass2">
        <text>
            <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
            </xsl:for-each>
            <body>
                <xsl:for-each select="tei:body/tei:*">
                    <xsl:apply-templates select="." mode="pass2"/>
                </xsl:for-each>
            </body>
        </text>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A &lt;p&gt; inside a listBibl is moved out</p>
      </desc>
    </doc>
    <xsl:template match="tei:listBibl/tei:p" mode="pass2"/>
    
    <xsl:template match="tei:listBibl" mode="pass2">
        <xsl:for-each select="tei:p">
            <p>
                <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
            </p>
        </xsl:for-each>
        <listBibl>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
        </listBibl>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>  Gloss list from tei to docx</p>
      </desc>
    </doc>
    <!-- <GLOSSITEM>
	 <hi rend="bold">100</hi>
	 <lb/>first item </GLOSSITEM>
    -->
    <xsl:template match="tei:GLOSSITEM" mode="pass2">
      <label>
	<xsl:for-each select="tei:hi">
	  <xsl:apply-templates/>
	</xsl:for-each>
      </label>
      <item>
	<xsl:apply-templates mode="inglossitem"/>
      </item>
    </xsl:template>

    <xsl:template match="*" mode="inglossitem">
      <xsl:apply-templates select="." mode="pass2"/>
    </xsl:template>

    <xsl:template match="tei:lb" mode="inglossitem"/>
    <xsl:template match="tei:hi[@rend='bold']" mode="inglossitem"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     Top of a weird gloss list </p>
      </desc>
    </doc>
    <xsl:template match="tei:list[@type='gloss']/tei:label[.='where']" mode="pass2">
        <head>
            <xsl:apply-templates/>
        </head>
    </xsl:template>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;bibl&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:bibl/tei:g[@ref='x:tab']" mode="pass2"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;gloss&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:gloss//tei:g[@ref='x:tab']" mode="pass2"/>
    
    
    <!-- removed 2010-03-15, seems to screw up formulae
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;formula&gt;? no. </p>
      </desc>
    </doc>

    <xsl:template match="tei:formula//tei:g[@ref='x:tab']" mode="pass2"/>
    -->
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     A tab in a &lt;head&gt;? no. </p>
      </desc>
    </doc>
    <xsl:template match="tei:head/tei:g[@ref='x:tab']" mode="pass2"/>
    

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>     An empty item</p>
      </desc>
    </doc>
    <xsl:template match="tei:item[not(*) and not(text())]" mode="pass2"/>
    

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;bibl&gt; </desc></doc>
    
    <xsl:template match="tei:bibl[not(*) and not(text())]" mode="pass2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;availability&gt; </desc></doc>
   
    <xsl:template match="tei:availability[not(*) and not(text())]"
		  mode="pass2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;note&gt; </desc></doc>
   
    <xsl:template match="tei:note[not(*) and not(text())]"
		  mode="pass2">
    </xsl:template>

    <xsl:template match="tei:list[@type='gloss']/tei:item/tei:g[@ref='x:tab']" mode="pass2"/>
    

    <xsl:template match="tei:hi[@rend='footnote_reference' and
			 count(*)=1 and tei:note]" mode="pass2" priority="99">
          <xsl:apply-templates mode="pass2"/>
    </xsl:template>

  <xsl:template match="tei:hi[not(@rend) and not(*) and string-length(.)=0]" mode="pass2">
  </xsl:template>

  <xsl:template match="tei:hi[@rend='Endnote_anchor']" mode="pass2" priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>

  <xsl:template match="tei:hi[@rend='EndnoteReference']" mode="pass2"  priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>

  <xsl:template match="tei:hi[@rend='EndnoteCharacters']" mode="pass2"
		 priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>
 
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Clean up <gi>hi</gi> by merging adjacent &lt;hi&gt;s </desc>
  </doc>

  <xsl:template match="tei:hi[@rend]" mode="pass2">
    <xsl:variable name="r" select="@rend"/>
    <xsl:choose>
      <xsl:when test="count(parent::tei:speaker/*)=1 and not
		      (parent::tei:speaker/text())">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="parent::tei:head and .='&#160;'"/>
      <xsl:when test ="not(*) and string-length(.)=0"/>
      <xsl:when test="parent::tei:item/parent::tei:list[@type='gloss']
		      and tei:g[@ref='x:tab']"/>
      <xsl:when
	  test="preceding-sibling::node()[1][self::tei:hi[@rend=$r]]">
      </xsl:when>
      <xsl:when
	  test="preceding-sibling::node()[1][self::tei:seg and .=' ']
		and
		preceding-sibling::node()[2][self::tei:hi[@rend=$r]]">
      </xsl:when>
      <xsl:when test="@rend='bold' and .=' '">
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="@rend='italic' and .=' '">
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="ename">
	  <xsl:choose>
	    <xsl:when test="@rend='italic' and
			    ancestor::tei:bibl">title</xsl:when>
	    <xsl:otherwise>hi</xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<xsl:element name="{$ename}">
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="pass2"/>
	  <xsl:call-template name="nextHi">
	    <xsl:with-param name="r" select="$r"/>
	  </xsl:call-template>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
   </xsl:template>

   <xsl:template name="nextHi">
      <xsl:param name="r"/>
      <xsl:for-each select="following-sibling::node()[1]">
         <xsl:choose>
	   <xsl:when test="self::tei:hi[@rend=$r]">
            <xsl:apply-templates mode="pass2"/>
            <xsl:call-template name="nextHi">
	              <xsl:with-param name="r" select="$r"/>
            </xsl:call-template>
	   </xsl:when>
	   <xsl:when test="self::tei:seg and .=' ' and
			   following-sibling::node()[1][self::tei:hi[@rend=$r]]">
            <xsl:apply-templates mode="pass2"/>
            <xsl:call-template name="nextHi">
	              <xsl:with-param name="r" select="$r"/>
	    </xsl:call-template>
	   </xsl:when>
	 </xsl:choose>
      </xsl:for-each>
   </xsl:template>

   <xsl:template match="tei:div[tei:head/tei:ANCHOR]" mode="pass2">
     <xsl:copy>
       <xsl:attribute name="xml:id"
		      select="tei:head/tei:ANCHOR[1]/@xml:id"/>
       <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
     </xsl:copy>
   </xsl:template>

   <xsl:template match="tei:ANCHOR" mode="pass2"/>

   <xsl:template match="w:bookmarkStart" mode="pass2">
       <anchor>
        <xsl:attribute name="xml:id">
            <xsl:value-of select="substring(@w:name,2)"/>
        </xsl:attribute>
	</anchor>
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Paragraphs in cells not allowed</desc></doc>
   
    <xsl:template match="tei:cell/tei:p" mode="pass2">
      <xsl:if test="preceding-sibling::tei:p">
	<lb/>
      </xsl:if>
      <xsl:apply-templates mode="pass2"/>
    </xsl:template>

    <xsl:template match="tei:speech" mode="pass2"/>
    <xsl:template match="tei:speech" mode="keep">
      <p>
	<xsl:apply-templates mode="pass2"/>
      </p>
    </xsl:template>

    <xsl:template match="tei:speaker" mode="pass2">
      <sp>
	<speaker>
	  <xsl:choose>	
	    <xsl:when test="count(*)=1 and not(text()) and tei:hi[@rend]">
	      <xsl:attribute name="rend" select="tei:hi/@rend"/>
	      <xsl:for-each select="tei:hi">
	      <xsl:apply-templates mode="pass2"/>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates mode="pass2"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</speaker>
	<xsl:apply-templates
	    select="following-sibling::tei:speech[1]" mode="keep"/>
      </sp>
    </xsl:template>

    <xsl:template match="tei:figure/tei:p[tei:graphic and
			 count(*)=1]" mode="pass2">
      <xsl:apply-templates mode="pass2"/>      
    </xsl:template>

    <xsl:template match="tei:p[@rend='caption' or @rend='Figure title']" mode="pass2">
      <head>
	<xsl:apply-templates mode="pass2"/>      
      </head>
    </xsl:template>

    <xsl:template match="tei:div[count(*)=1 and tei:head[not(text())]]" mode="pass2"/>

    <xsl:template
	match="tei:figure/tei:p[@rend='caption' or @rend='Figure title']/text()[starts-with(.,'Figure  ')]"
	mode="pass2">
      <xsl:value-of select="substring(.,9)"/>
    </xsl:template>


</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main" xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns="http://www.tei-c.org/ns/1.0" version="2.0" exclude-result-prefixes="#all">
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
      <p>Id: $Id: pass2.xsl 12235 2013-06-10 11:44:57Z rahtz $</p>
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:variable name="dq">"</xsl:variable>
  <xsl:template match="@*|comment()|processing-instruction()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass2">
    <xsl:variable name="temp">
      <xsl:copy>
	<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
      </xsl:copy>
    </xsl:variable>
    <xsl:apply-templates mode="spacepass" select="$temp"/>
</xsl:template>

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="spacepass">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="spacepass">
      <xsl:copy>
	<xsl:apply-templates select="@*" mode="spacepass"/>
	<xsl:choose>
	  <xsl:when test="$preserveSpace='true'">
	  </xsl:when>
	  <xsl:when test="tei:isInline(.)  and (starts-with(text()[1],' ') or
		      ends-with(text()[last()],' ')) and not(*)">
	    <xsl:attribute name="xml:space">preserve</xsl:attribute>
	  </xsl:when>
	</xsl:choose>
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="spacepass"/>
      </xsl:copy>
  </xsl:template>
  
  <xsl:template match="@xml:space[.='preserve']" mode="spacepass">
    <xsl:if test="$preserveSpace='true'">
      <xsl:copy-of select="."/>
    </xsl:if>
  </xsl:template>

    <xsl:template match="text()" mode="pass2">
    <xsl:value-of select="."/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>zap empty p</p>
    </desc>
  </doc>
  <xsl:template match="tei:p[not(.//tei:pb) and       normalize-space(.)='']" mode="pass2"		priority="99"/>
  <xsl:template match="tei:figure/tei:p[tei:graphic and count(*)=1]" mode="pass2" priority="101">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Singleton paragraphs in cells dropped</desc>
  </doc>
  <xsl:template match="tei:cell[count(*)=1]/tei:p" mode="pass2">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>

  <xsl:template match="tei:list[normalize-space(.)='']" mode="pass2"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Inner lists in lists must be moved to inside items
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
      <p>Zap emdashes at start of head </p>
    </desc>
  </doc>
  <xsl:template match="tei:head/text()" mode="pass2">
    <xsl:choose>
      <xsl:when test="starts-with(.,'— ')">
        <xsl:value-of select="substring(.,3)"/>
      </xsl:when>
      <xsl:when test="starts-with(.,' — ')">
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
      <p>A &lt;p&gt; or &lt;seg&gt; which does nothing is not worth
      having; and if its reverts to just space, its a space </p>
    </desc>
  </doc>
  <xsl:template match="tei:seg[not(@*)]" mode="pass2">
    <xsl:choose>
      <xsl:when test="parent::tei:formula and normalize-space(.)=''"/>
      <xsl:when test=".=' ' and
		      following-sibling::node()[1][self::tei:hi]/@rend=preceding-sibling::node()[1][self::tei:hi]/@rend">
      </xsl:when>
      <xsl:when test="parent::*/text()">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="parent::tei:hi[count(*)=1]">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test=".=' '">
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
      <xsl:apply-templates select="//tei:front" mode="pass2"/>
      <body>
        <xsl:for-each select="tei:body/tei:*[not(local-name(.)='front')]">
          <xsl:apply-templates select="." mode="pass2"/>
        </xsl:for-each>
      </body>
      <xsl:apply-templates select="tei:back" mode="pass2"/>
    </text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A &lt;p&gt; inside a listBibl is moved out</p>
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
  <xsl:template match="tei:GLOSSITEM" mode="pass2">
    <label>
      <xsl:for-each select="tei:hi">
        <xsl:apply-templates mode="pass2"/>
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
  <xsl:template match="tei:hi[tei:match(@rend,'bold')]" mode="inglossitem"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Top of a weird gloss list </p>
    </desc>
  </doc>
  <xsl:template match="tei:list[@type='gloss']/tei:label[.='where']" mode="pass2">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A tab in a &lt;bibl&gt;? no. </p>
    </desc>
  </doc>
  <xsl:template match="tei:bibl/tei:g[@ref='x:tab']" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A tab in a &lt;gloss&gt;? no. </p>
    </desc>
  </doc>
  <xsl:template match="tei:gloss//tei:g[@ref='x:tab']" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A tab in a &lt;head&gt;? no. </p>
    </desc>
  </doc>
  <xsl:template match="tei:head/tei:g[@ref='x:tab']" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>An empty item</p>
    </desc>
  </doc>
  <xsl:template match="tei:item[not(*) and not(text())]" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Zap empty &lt;bibl&gt; </desc>
  </doc>
  <xsl:template match="tei:bibl[not(*) and not(text())]" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Zap empty &lt;availability&gt; </desc>
  </doc>
  <xsl:template match="tei:availability[not(*) and not(text())]" mode="pass2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Zap empty &lt;note&gt; </desc>
  </doc>
  <xsl:template match="tei:note[not(*) and not(text())]" mode="pass2">
    </xsl:template>
  <xsl:template match="tei:list[@type='gloss']/tei:item/tei:g[@ref='x:tab']" mode="pass2"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>zap footnote reference which only contains a space</desc>
  </doc>
  <xsl:template match="tei:hi[tei:match(@rend,'footnote_reference') and
		       count(*)=1 and tei:seg and
		       normalize-space(.)='']" mode="pass2"
		priority="99">
    <xsl:text> </xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>A footnote reference with a footnote inside it is in fact
    just a footnote</desc>
  </doc>
  <xsl:template match="tei:hi[tei:match(@rend,'footnote_reference') and count(*)=1 and tei:note]" mode="pass2" priority="99">
    <xsl:apply-templates select="tei:note" mode="pass2"/>
  </xsl:template>

  <xsl:template match="tei:hi[tei:match(@rend,'Endnote_anchor')]" mode="pass2" priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'EndnoteReference')]" mode="pass2" priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'EndnoteCharacters')]" mode="pass2" priority="99">
    <xsl:apply-templates mode="pass2"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>a &lt;hi&gt; with just white space is ignored</desc>
  </doc>
  <xsl:template match="tei:hi[not(@rend) and not(*) and
		       string-length(.)=0]" mode="pass2">
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Clean up by merging adjacent &lt;hi&gt;s with the same rend
    value into one.</desc>
  </doc>

  <xsl:template match="tei:hi[not (* or text())]"		mode="pass2"/>

  <xsl:template match="tei:hi[(@rend or @style) and (* or text())]"
		mode="pass2">
    <xsl:variable name="r" select="concat(@rend,@style)"/>

    <xsl:choose>
      <xsl:when test="count(parent::tei:speaker/*)=1 and not         (parent::tei:speaker/text())">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="parent::tei:head and .=' '"/>
      <xsl:when test="parent::tei:item/parent::tei:list[@type='gloss']  and tei:g[@ref='x:tab']"/>
      <xsl:when test="preceding-sibling::node()[1][self::tei:hi[concat(@rend,@style)=$r]]"/>
      <xsl:when test="preceding-sibling::node()[1][self::tei:seg and .=' ']   and  preceding-sibling::node()[2][self::tei:hi[concat(@rend,@style)=$r]]"/>
      <xsl:when test="($r='bold' or $r='italic') and .=' '">
        <xsl:text> </xsl:text>
	<xsl:if test="following-sibling::node()[1][self::tei:hi[concat(@rend,@style)=$r]]">
	  <xsl:variable name="ename" select="tei:nameOutputElement(.)"/>
	    <xsl:element name="{$ename}">
	    <xsl:copy-of select="@*[not(starts-with(.,'tei:'))]"/>
	    <xsl:call-template name="nextHi">
	      <xsl:with-param name="r" select="$r"/>
	    </xsl:call-template>
	    </xsl:element>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="ename" select="tei:nameOutputElement(.)"/>
            <xsl:element name="{$ename}">
              <xsl:apply-templates mode="pass2" select="@*[not(starts-with(.,'tei:'))]"/>
	      <xsl:choose>
		<xsl:when test="$ename='gap'">
		  <desc>
		    <xsl:apply-templates mode="pass2"/>
		    <xsl:call-template name="nextHi">
		      <xsl:with-param name="r" select="$r"/>
		    </xsl:call-template>
		  </desc>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates mode="pass2"/>
		  <xsl:call-template name="nextHi">
		    <xsl:with-param name="r" select="$r"/>
		  </xsl:call-template>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
     
  <xsl:template name="nextHi">
    <xsl:param name="r"/>
    <xsl:for-each select="following-sibling::node()[1]">
      <xsl:choose>
        <xsl:when test="self::tei:hi[concat(@rend,@style)=$r]">
          <xsl:apply-templates mode="pass2"/>
          <xsl:call-template name="nextHi">
            <xsl:with-param name="r" select="$r"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="self::tei:seg and .=' ' and       following-sibling::node()[1][self::tei:hi[concat(@rend,@style)=$r]]">
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
      <xsl:attribute name="xml:id" select="tei:head/tei:ANCHOR[1]/@xml:id"/>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:ANCHOR[parent::tei:head]" mode="pass2"/>

  <xsl:template match="tei:ANCHOR[not(parent::tei:head)]" mode="pass2">
    <anchor xml:id="{@xml:id}"/>
  </xsl:template>

  <xsl:template match="w:bookmarkStart" mode="pass2">
    <anchor>
      <xsl:attribute name="xml:id">
        <xsl:value-of select="replace(@w:name,'^_','')"/>
      </xsl:attribute>
    </anchor>
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
      <xsl:apply-templates select="following-sibling::tei:speech[1]" mode="keep"/>
    </sp>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> &lt;CAPTION&gt; belongs to nearest figure or table</desc>
  </doc>
  <xsl:template match="tei:CAPTION" mode="pass2"/>
  <xsl:template match="tei:table|tei:figure" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="pass2"/>
      <xsl:apply-templates mode="pass2"/>
      <xsl:if test="preceding-sibling::*[1][self::tei:CAPTION] and 
        following-sibling::*[1][self::tei:CAPTION]">
        <xsl:message>
          <xsl:text>WARN: Possible confusion on where these captions belong: [[</xsl:text>
          <xsl:value-of select="concat(preceding-sibling::*[1],']] and [[',
            following-sibling::*[1],']]')"/>
        </xsl:message>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="following-sibling::*[1][self::tei:CAPTION]">
          <xsl:apply-templates
            select="following-sibling::*[1][self::tei:CAPTION]/*" mode="pass2"/>
        </xsl:when>
        <xsl:when test="preceding-sibling::*[1][self::tei:CAPTION]">
          <xsl:apply-templates
            select="preceding-sibling::*[1][self::tei:CAPTION]/*" mode="pass2"/>
        </xsl:when>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  
  

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Rename &lt;p&gt; to a more specific name based on @rend</desc>
  </doc>
  <xsl:template match="tei:p[starts-with(@rend,'tei:')]" mode="pass2">
    <xsl:element name="{tei:nameOutputElement(.)}">
      <xsl:copy-of select="@*[not(starts-with(.,'tei:'))]"/>
      <xsl:apply-templates mode="pass2"/>      
    </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>a div with no content but an empty head isn't needed</desc>
  </doc>
  <xsl:template match="tei:div[count(*)=1 and tei:head[not(node())]]"
		mode="pass2" priority="21"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>a singleton div with an empty head and no div children is bypassed</desc>
  </doc>
  <xsl:template match="tei:div[count(parent::*/tei:div)=1 and
		       not(tei:div) and
		       tei:head[not(node())]]" mode="pass2">
    <xsl:apply-templates select="*[not(self::tei:head)]" mode="pass2"/>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>an empty head is bypassed</desc>
  </doc>
  <xsl:template match="tei:head[not(node())]" mode="pass2"/>

  <xsl:template match="tei:figure/tei:p[tei:match(@rend,'caption') or tei:match(@rend,'Figure title')]/text()[starts-with(.,'Figure  ')]" mode="pass2">
    <xsl:value-of select="substring(.,9)"/>
  </xsl:template>

  <xsl:template match="@rend[.='Body_Text']" mode="pass2"/>

  <xsl:template match="@rend[.='Normal (Web)']" mode="pass2"/>


  <xsl:template match="tei:p[tei:match(@rend,'caption') or tei:match(@rend,'Figure title')]"
		mode="pass2">
    <head>
      <xsl:apply-templates mode="pass2"/>
    </head>
  </xsl:template>

  
  <xsl:template match="tei:p[matches(@rend, '.+[Tt]itle') and ancestor::tei:front]"
    mode="pass2">
 
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Title is supposed to be the document title, its siblings form other title parts</desc>
  </doc>
  <xsl:template match="tei:p[tei:match(@rend,'Title') and ancestor::tei:front]"
    mode="pass2">
    <docTitle>
      <titlePart type="{@rend}">
      <xsl:apply-templates mode="pass2"/>
      </titlePart>
      <xsl:for-each select="following-sibling::*[self::tei:p[matches(@rend,'.*[Tt]itle')]]">
        <titlePart type="{@rend}"><xsl:apply-templates mode="pass2"/></titlePart>
      </xsl:for-each>
    </docTitle>
  </xsl:template>
  
  <xsl:template match="tei:p[tei:match(@rend,'Author') and ancestor::tei:front]"
    mode="pass2">
    <docAuthor>
      <xsl:apply-templates mode="pass2"/>
    </docAuthor>
  </xsl:template>
  
  <xsl:template match="tei:p[tei:match(@rend,'Date') and ancestor::tei:front]"
    mode="pass2">
    <docDate>
      <xsl:apply-templates mode="pass2"/>
    </docDate>
  </xsl:template>
  
  <xsl:template match="tei:titlePage[not(ancestor::tei:front)]"
    mode="pass2">
    <!-- strip unnecessary title Pages -->
    <xsl:message>STRIP TP</xsl:message>
       <xsl:apply-templates mode="pass2"/>
   </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Name for output element. If @rend starts with "TEI " or "tei:", rename the
    &lt;hi&gt; to the element name instead</desc>
  </doc>
  <xsl:function name="tei:nameOutputElement">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<xsl:when test="tei:match(@rend,'italic') and ancestor::tei:bibl">title</xsl:when>
  <xsl:when test="starts-with(@rend,'tei:')">
	  <xsl:value-of select="substring(@rend,5)"/>
	</xsl:when>
	<xsl:when test="starts-with(@rend,'TEI ')">
	  <xsl:value-of select="substring(@rend,5)"/>
	</xsl:when>
	<xsl:when test="self::tei:p">
	  <xsl:text>p</xsl:text>
	</xsl:when>
	<xsl:when test="tei:match(@rend,'foreign')">
	  <xsl:text>foreign</xsl:text>
	</xsl:when>
	<xsl:otherwise>hi</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>clean up mess left by w:instrText in the ref element</desc>
  </doc>
  <xsl:template match="tei:ref" mode="pass2">
    <xsl:variable name="target">
      <xsl:choose>
	<xsl:when test="tei:discardInstruction(@target)"/>
	<xsl:otherwise>
	  <xsl:sequence select="tei:processInstruction(@target)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="tei:biblioInstruction($target)">
	<xsl:processing-instruction name="biblio">
	  <xsl:value-of select="$target"/>
	</xsl:processing-instruction>
	<xsl:choose>
          <xsl:when test="count(text()) &lt; 2">
            <xsl:apply-templates mode="pass2"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="text()[1]"/>
            <xsl:apply-templates select="*" mode="pass2"/>
            <xsl:value-of select="remove(text(), 1)"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="matches(@target,'^LINK Excel.Sheet.')">
	<xsl:sequence select="tei:docxError('cannot embed Excel  spreadsheet')"/>
      </xsl:when>
      <xsl:when test="tei:ref">
	<xsl:apply-templates mode="pass2"/>
      </xsl:when>
      <xsl:when test=".=''"/>
      <xsl:when test="$target='' and @type">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="pass2"/>
	</xsl:copy>
      </xsl:when>
      <xsl:when test="$target=''">
	<xsl:apply-templates mode="pass2"/>
      </xsl:when>
      <xsl:otherwise>
	<ref target="{$target}">
	  <xsl:copy-of select="@rend"/>
	  <xsl:apply-templates mode="pass2"/>
	</ref>
      </xsl:otherwise>
    </xsl:choose>  
  </xsl:template>

</xsl:stylesheet>

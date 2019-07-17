<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:its="http://www.w3.org/2005/11/its"

                exclude-result-prefixes="tei html t a rng s iso tbx
					 cals teix w"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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

   <xsl:template name="myi18n">
      <xsl:param name="word"/>
      <xsl:choose>
         <xsl:when test="$word='appendixWords'">
            <xsl:text>Annex</xsl:text>
    </xsl:when>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="processing-instruction()[name()='ISOerror']">
     <span style="border: solid red 1pt; color:red">
       <xsl:value-of select="."/>
     </span>
   </xsl:template>

   <xsl:template match="tei:note[@place='comment']">
     <span style="border: solid red 1pt; color:red">
       <xsl:value-of select="."/>
     </span>
   </xsl:template>

  <xsl:template name="divClassAttribute">
      <xsl:param name="depth"/>
      <xsl:choose>
         <xsl:when test="@type">
            <xsl:attribute name="class">
               <xsl:value-of select="@type"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:otherwise>
            <xsl:attribute name="class">
               <xsl:text>teidiv</xsl:text>
               <xsl:value-of select="$depth"/>
	              <xsl:text> from-</xsl:text>
	              <xsl:value-of select="local-name(ancestor::tei:body|ancestor::tei:front|ancestor::tei:back)"/>
            </xsl:attribute>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:variable name="ident">
         <xsl:apply-templates mode="ident" select="."/>
      </xsl:variable>
      <xsl:attribute name="id">
         <xsl:value-of select="$ident"/>
      </xsl:attribute>
  </xsl:template>



   <xsl:template match="tei:note[@place='foot']/tei:p">
     <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:note[tei:match(@rend,'example')]">
      <p>EXAMPLE <xsl:apply-templates/>
      </p>
   </xsl:template>


   <xsl:template match="tei:p[count(*)=1 and tei:gloss]">
      <p style="margin-left: 1em">
         <xsl:apply-templates/>
      </p>
   </xsl:template>

   <xsl:template match="tei:num">
      <span class="isonum">
         <xsl:choose>
            <xsl:when test="$numberFormat='fr'">
	              <xsl:value-of select="."/>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="translate(.,',&#160;','.,')"/>
            </xsl:otherwise>
         </xsl:choose>
      </span>
   </xsl:template>

   <xsl:template match="tei:g[@ref='x:tab']">
      <xsl:text>	</xsl:text>
   </xsl:template>

   <xsl:template match="tei:c[@iso:font]">
      <xsl:value-of select="@n"/>
   </xsl:template>

   <xsl:template match="tei:seg[@iso:provision]">
      <span class="provision_{@iso:provision}">
         <xsl:apply-templates/>
      </span>
   </xsl:template>


   <xsl:template name="block-element">
     <xsl:param name="pPr" as="node()*"/>
     <xsl:param name="style"/>
     <xsl:param name="select" select="."/>
     <xsl:for-each select="$select">
       <p>
	 <xsl:choose>
	 <xsl:when test="not($style='')">
	   <xsl:attribute name="class">
	     <xsl:value-of select="$style"/>
	   </xsl:attribute>
	 </xsl:when>
	 <xsl:when test="w:pPr/w:pStyle">
	   <xsl:attribute name="class">
	     <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	   </xsl:attribute>
	 </xsl:when>
	 </xsl:choose>
	 <xsl:apply-templates/>
       </p>
     </xsl:for-each>
   </xsl:template>

   <xsl:template name="generateError">
     <xsl:param name="message"/>
     <xsl:processing-instruction name="ISOerror">
       <xsl:value-of select="$message"/>
     </xsl:processing-instruction>
   </xsl:template>

   <xsl:template name="copyIt">
      <xsl:copy>
	<xsl:apply-templates select="@*" mode="checkSchematron"/>
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template name="copyMe">
      <xsl:copy-of select="."/>
   </xsl:template>

   <xsl:template match="text()">
       <xsl:value-of select="translate(.,'&#2011;','-')"/>
   </xsl:template>

  <xsl:template name="simpleRun">
    <xsl:param name="text"/>
    <xsl:param name="prefix"/>
    <xsl:param name="italic"/>
    <xsl:value-of select="$prefix"/>
    <xsl:choose>
      <xsl:when test="$italic='true'">
	<i>
	  <xsl:value-of select="$text"/>
	</i>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:hi[@rend]" priority="101">
    <span>
      <xsl:if test="../@xml:lang">
	<xsl:attribute name="lang" select="../@xml:lang"/>
      </xsl:if>
      <xsl:variable name="style">
	<xsl:for-each select="tokenize(@iso:style,';')">
	  <xsl:choose>
	    <xsl:when test=".=''"/>
	    <xsl:otherwise>
	      <s>
		<xsl:value-of select="."/><xsl:text>;</xsl:text>
	      </s>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
	<xsl:for-each select="tokenize(@rend,' ')">
	  <xsl:choose>
	    <xsl:when test=".='bold'">
	      <s>font-weight:bold;</s>
	    </xsl:when>
	    <xsl:when test=".='italic'">
	      <s>font-style:italic;</s>
	    </xsl:when>
	    <xsl:when test=".='smallcaps'">
	      <s>font-variant:small-caps;</s>
	    </xsl:when>
	    <xsl:when test=".='allcaps'">
	      <s>text-transform:capitalize;</s>
	    </xsl:when>
	    <xsl:when test=".='strikethrough'">
	      <s>text-decoration:line-through;</s>
	    </xsl:when>
	    <xsl:when test=".='doublestrikethrough'">
	      <s>text-decoration:line-through;</s>
	    </xsl:when>
	    <xsl:when test=".='wavyunderline'">
	      <s>text-decoration:wavy;</s>
	    </xsl:when>
	    <xsl:when test=".='underline'">
	      <s>text-decoration:underline;</s>
	    </xsl:when>
	    <xsl:when test=".='doubleunderline'">
	      <s>border-bottom: 3px double;</s>
	    </xsl:when>
	    <xsl:when test="starts-with(.,'color(')">
	      <xsl:value-of select="translate(.,'()',':;')"/>
	    </xsl:when>
	    <xsl:when test=".='superscript'">
	      <s>vertical-align: top;font-size: 70%;</s>
	    </xsl:when>
	    <xsl:when test=".='subscript'">
	      <s>vertical-align: bottom;font-size: 70%;</s>
	    </xsl:when>
	    <xsl:when test="starts-with(.,'background(')">
	      <s>background-color:<xsl:value-of
		  select="substring-before(substring-after(.,'('),')')"/>
	      </s>
	    </xsl:when>
	  </xsl:choose>
	</xsl:for-each>
	<xsl:if test="@its:dir">
	  <d><xsl:value-of select="@its:dir"/></d>
	</xsl:if>
      </xsl:variable>
      <xsl:for-each select="$style">
	<xsl:if test="html:s">
	  <xsl:attribute name="style">
	    <xsl:value-of select="html:s"/>
	  </xsl:attribute>
	</xsl:if>
	<xsl:if test="html:d">
	  <xsl:attribute name="dir">
	    <xsl:value-of select="html:d"/>
	  </xsl:attribute>
	</xsl:if>
      </xsl:for-each>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template name="makeHTMLHeading">
    <xsl:param name="text"/>
    <xsl:param name="class">title</xsl:param>
    <xsl:param name="level">1</xsl:param>
    <xsl:if test="$class = 'maintitle'">
      <div class="healthwarning">This extract from an ISO document has been created for test purposes only. The design and layout are subject to change by ISO.</div>
    </xsl:if>
       <xsl:choose>
	 <xsl:when test="key('ISOMETA','docReference')">
	   <xsl:element name="h{$level}">
	     <xsl:attribute name="class">
	       <xsl:value-of select="$class"/>
	     </xsl:attribute>
	     <xsl:value-of
		 select="substring-before(key('ISOMETA','docReference'),'(')"/><br></br><xsl:value-of
		 select="key('ISOMETA','fullTitle')"/>
	   </xsl:element>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:copy-of select="$text"/>
	 </xsl:otherwise>
       </xsl:choose>
  </xsl:template>

   <xsl:template match="tei:titleStmt/tei:title" priority="99">
     <h1 class="maintitle">
       <xsl:apply-templates/>
     </h1>
   </xsl:template>

  <xsl:template name="rendToClass">
    <xsl:param name="id">true</xsl:param>
    <xsl:param name="default">.</xsl:param>
    <xsl:if test="$id='true' and @xml:id">
      <xsl:attribute name="id">
        <xsl:value-of select="@xml:id"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="../@xml:lang">
      <xsl:attribute name="lang" select="../@xml:lang"/>
    </xsl:if>
    <xsl:variable name="style">
      <xsl:for-each select="tokenize(@iso:style,';')">
	<xsl:choose>
	  <xsl:when test=".=''"/>
	  <xsl:when test="starts-with(.,'direction')">
	    <d>
	      <xsl:value-of select="substring-after(.,':')"/>
	    </d>
	  </xsl:when>
	  <xsl:otherwise>
	    <s>
	      <xsl:value-of select="."/><xsl:text>;</xsl:text>
	    </s>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
      <xsl:for-each select="tokenize(@rend,' ')">
	<c>
	  <xsl:value-of select="."/>
	</c>
      </xsl:for-each>
      <xsl:for-each select="@type">
	<c>
	  <xsl:value-of select="."/>
	</c>
      </xsl:for-each>
      <c>
	<xsl:value-of select="local-name()"/>
      </c>
    </xsl:variable>
    <xsl:for-each select="$style">
      <xsl:if test="html:s">
	<xsl:attribute name="style">
	  <xsl:value-of select="html:s"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="html:d">
	<xsl:attribute name="dir">
	  <xsl:value-of select="html:d"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:if test="html:c">
	<xsl:attribute name="class">
	  <xsl:value-of select="html:c[1]"/>
	</xsl:attribute>
      </xsl:if>
    </xsl:for-each>    
  </xsl:template>

</xsl:stylesheet>

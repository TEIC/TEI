<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei xs"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the linking module. </p>
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
  <xsl:param name="linkElement">a</xsl:param>
  <xsl:param name="linkAttribute">href</xsl:param>
  <xsl:param name="linkElementNamespace"></xsl:param>
  <xsl:param name="urlMarkup">span</xsl:param>
  <xsl:param name="linkAttributeNamespace">http://www.w3.org/1999/xlink</xsl:param>
  <xsl:param name="outputSuffix">.xml</xsl:param>

  <xsl:key name="MNAMES"
            match="tei:monogr/tei:author[tei:surname]|tei:monogr/tei:editor[tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  <xsl:key name="ANAMES"
            match="tei:analytic/tei:author[tei:surname]|tei:analytic/tei:editor[tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  <xsl:key name="MNAMES"
            match="tei:monogr/tei:author[tei:persName/tei:surname]|tei:monogr/tei:editor[tei:persName/tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  <xsl:key name="ANAMES"
            match="tei:analytic/tei:author[tei:persName/tei:surname]|tei:analytic/tei:editor[tei:persName/tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element  in xref mode</desc>
   </doc>
  <xsl:template match="tei:TEI" mode="xref">
      <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements text in xref mode</desc>
   </doc>
  <xsl:template match="tei:text" mode="xref">
   <xsl:choose>
     <xsl:when test="tei:head">
       <xsl:apply-templates select="tei:head" mode="plain"/>
     </xsl:when>
     <xsl:when test="tei:body/tei:head">
       <xsl:apply-templates select="tei:body/tei:head" mode="plain"/>
     </xsl:when>
     <xsl:when test="tei:front//tei:titlePart/tei:title">
       <xsl:apply-templates select="tei:front//tei:titlePart/tei:title" mode="plain"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:text>Text </xsl:text>
       <xsl:number/>
     </xsl:otherwise>
   </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element anchor|tei:p in xref mode</desc>
   </doc>
  <xsl:template match="tei:anchor|tei:p" mode="xref">
      <xsl:text>here</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Crossref element bibl</desc>
   </doc>
  <xsl:template match="tei:bibl" mode="xref">
      <xsl:text>[</xsl:text>
      <xsl:number/>
      <xsl:text>]</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Crossref element biblStruct</desc>
   </doc>
   <xsl:template match="tei:biblStruct" mode="xref">
      <xsl:choose>
         <xsl:when test="count(key('ANAMES',@xml:id))=1">
	           <xsl:value-of select="normalize-space(key('ANAMES',@xml:id)//tei:surname)"/>
         </xsl:when>
         <xsl:when test="count(key('ANAMES',@xml:id))=2">
	           <xsl:value-of select="key('ANAMES',@xml:id)[1]//tei:surname"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> and </xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="key('ANAMES',@xml:id)[2]//tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('ANAMES',@xml:id))&gt;2">
	           <xsl:value-of select="key('ANAMES',@xml:id)[1]//tei:surname"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> et al.</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))=1">
	           <xsl:value-of select="key('MNAMES',@xml:id)//tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))=2">
	           <xsl:value-of select="key('MNAMES',@xml:id)[1]//tei:surname"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> and </xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="key('MNAMES',@xml:id)[2]//tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))&gt;2">
	           <xsl:value-of select="key('MNAMES',@xml:id)[1]//tei:surname"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> et al.</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test=".//tei:author[tei:surname]">
	           <xsl:value-of select=".//tei:author//tei:surname[1]"/>
         </xsl:when>
         <xsl:when test=".//tei:author[tei:orgName]">
	           <xsl:value-of select=".//tei:author/tei:orgName[1]"/>
         </xsl:when>
         <xsl:when test=".//tei:author">
	           <xsl:value-of select=".//tei:author[1]"/>
         </xsl:when>
         <xsl:when test=".//tei:editor[tei:surname]">
	           <xsl:value-of select=".//tei:editor//tei:surname[1]"/>
         </xsl:when>
         <xsl:when test=".//tei:editor">
	           <xsl:value-of select=".//tei:editor[1]"/>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:value-of select=".//tei:title[1]"/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test="count(tei:*[1]/tei:editor)=1">
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> (ed.)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="count(tei:*[1]/tei:editor)&gt;1">
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> (eds.)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test="tei:monogr/tei:imprint/tei:date/@when">
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> (</xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="substring-before(tei:monogr/tei:imprint/tei:date/@when,'-')"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters">)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="tei:monogr/tei:imprint/tei:date">
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters"> (</xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="tei:monogr/tei:imprint/tei:date"/>
	           <xsl:call-template name="makeText">
		     <xsl:with-param name="letters">)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
      </xsl:choose>
   </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element s
      tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6 in
      xref mode<param name="minimal">whether to make a link with just numbers or with
      text too</param>
      </desc>
   </doc>
  <xsl:template match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
                 mode="xref">
      <xsl:param name="minimal">false</xsl:param>
      <xsl:call-template name="header">
         <xsl:with-param name="minimal" select="$minimal"/>
         <xsl:with-param name="display">plain</xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process ptr and ref elements, hypertext pointers</desc>
   </doc>
  <xsl:template match="tei:ptr|tei:ref">
    <xsl:if test="parent::tei:analytic or parent::tei:monogr">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='transclude' and self::tei:ptr">
	<xsl:apply-templates select="doc(@target)"/>
      </xsl:when>
    	<!-- omit empty ref elements that do not have @target -->
    	<xsl:when test="self::tei:ref and not(@target) and not(descendant-or-self::text())"/>
    	<xsl:otherwise>
	<xsl:variable name="ptr" select="if (self::tei:ptr) then
					     true() else false()"/>
	<xsl:variable name="xmllang" select="@xml:lang"/>
	<xsl:variable name="here" select="."/>
	<xsl:choose>
	  <xsl:when test="not(@target) and self::tei:ref">
	    <xsl:call-template name="makeInline">
	      <xsl:with-param name="style">ref</xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="@cRef and self::tei:ptr">
	    <xsl:call-template name="makeInline">
	      <xsl:with-param name="style">ptr</xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="tokenize(normalize-space(@target),' ')">
	      <xsl:variable name="a" select="."/>
	      <xsl:for-each select="$here">
		<xsl:choose>
		  <!-- If there is a target attribute starting with #, it is always a local reference -->
		  <xsl:when test="starts-with($a,'#')">
		    <xsl:call-template name="makeInternalLink">
		      <xsl:with-param name="target" select="substring($a,2)"/>
		      <xsl:with-param name="ptr" select="$ptr"/>
		      <xsl:with-param name="dest">
			<xsl:call-template name="generateEndLink">
			  <xsl:with-param name="where">
			    <xsl:value-of select="substring($a,2)"/>
			  </xsl:with-param>
			</xsl:call-template>
		      </xsl:with-param>
		    </xsl:call-template>
		  </xsl:when>
		  <!-- if we are doing TEI P4, all targets are local -->
		  <xsl:when test="$teiP4Compat='true'">
		    <xsl:call-template name="makeInternalLink">
		      <xsl:with-param name="target" select="$a"/>
		      <xsl:with-param name="ptr" select="$ptr"/>
		      <xsl:with-param name="dest">
			<xsl:call-template name="generateEndLink">
			  <xsl:with-param name="where">
			    <xsl:value-of select="$a"/>
			  </xsl:with-param>
			</xsl:call-template>
		      </xsl:with-param>
		    </xsl:call-template>
		  </xsl:when>
		  <!-- other uses of target means it is external -->
		  <xsl:otherwise>
		    <xsl:call-template name="makeExternalLink">
		      <xsl:with-param name="ptr" select="$ptr"/>
		      <xsl:with-param name="title" select="@n"/>
		      <xsl:with-param name="dest">
			<xsl:sequence select="tei:resolveURI($here,$a)"/>
		      </xsl:with-param>
		    </xsl:call-template>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:for-each> 
	      <xsl:call-template name="multiTargetSeparator">
		<xsl:with-param name="xmllang" select="$xmllang"/>
	      </xsl:call-template>
	    </xsl:for-each>      
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:if test="parent::tei:analytic or parent::tei:monogr">
      <xsl:text> </xsl:text>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[common] Making a heading for something
         <param name="minimal">whether to display headings</param>
         <param name="toc">whether this is making a TOC entry</param>
         <param name="display">detail of display (full, simple, plain), ie
    whether markup is followed</param>
      </desc>
   </doc>
  <xsl:template name="header">
      <xsl:param name="minimal">false</xsl:param>
      <xsl:param name="toc"/>
      <xsl:param name="display">full</xsl:param>
      <xsl:variable name="depth">
         <xsl:apply-templates mode="depth" select="."/>
      </xsl:variable>
      <xsl:variable name="headingNumber">
	<xsl:call-template name="formatHeadingNumber">
	  <xsl:with-param name="toc">
	    <xsl:value-of select="$toc"/>
	  </xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:choose>
	      <xsl:when test="local-name(.) = 'TEI'">
		<xsl:if test="@n">
		  <xsl:value-of select="@n"/>
		</xsl:if>
	      </xsl:when>
	      <xsl:when test="$depth &gt; $numberHeadingsDepth"> </xsl:when>
	      <xsl:when test="self::tei:text">
		<xsl:number/>
		<xsl:call-template name="headingNumberSuffix"/>
	      </xsl:when>
	      <xsl:when test="ancestor::tei:back">
		<xsl:if test="not($numberBackHeadings='')">
		  <xsl:sequence select="tei:i18n('appendixWords')"/>
		  <xsl:text> </xsl:text>
		  <xsl:call-template name="numberBackDiv"/>
		  <xsl:if test="$minimal='false'">
		    <xsl:value-of select="$numberSpacer"/>
		  </xsl:if>
		</xsl:if>
	      </xsl:when>
	      <xsl:when test="ancestor::tei:front">
		<xsl:if test="not($numberFrontHeadings='')">
		  <xsl:call-template name="numberFrontDiv">
		    <xsl:with-param name="minimal">
		      <xsl:value-of select="$minimal"/>
		    </xsl:with-param>
		  </xsl:call-template>
		</xsl:if>
	      </xsl:when>
	      <xsl:when test="$numberHeadings ='true'">
		<xsl:choose>
		  <xsl:when test="$prenumberedHeadings='true'">
		    <xsl:value-of select="@n"/>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:call-template name="numberBodyDiv"/>
		  </xsl:otherwise>
		</xsl:choose>
		<xsl:if test="$minimal='false'">
		  <xsl:call-template name="headingNumberSuffix"/>
		</xsl:if>
	      </xsl:when>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:variable>
      <xsl:copy-of select="$headingNumber"/>
      <xsl:if test="$minimal='false'">
	<xsl:choose>
	  <xsl:when test="local-name(.) = 'TEI'">
	    <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]"/>
	  </xsl:when>
	  <xsl:when test="not($toc='')">
	    <xsl:call-template name="makeInternalLink">
	      <xsl:with-param name="dest">
		<xsl:value-of select="$toc"/>
	      </xsl:with-param>
	      <xsl:with-param name="class">
		<xsl:value-of select="$class_toc"/>
		<xsl:text> </xsl:text>
		<xsl:value-of select="($class_toc,$depth)"
			      separator="_"/>
	      </xsl:with-param>
	      <xsl:with-param name="body">
		<xsl:choose>
		  <xsl:when test="self::tei:text">
		    <xsl:value-of select="if (@n) then @n else concat('[',position(),']')"/>
		  </xsl:when>
		  <xsl:when test="not(tei:head) and tei:front/tei:head">
		    <xsl:apply-templates  mode="plain" select="tei:front/tei:head"/>
		  </xsl:when>
		  <xsl:when test="not(tei:head) and tei:body/tei:head">
		    <xsl:apply-templates mode="plain" select="tei:body/tei:head"/>
		  </xsl:when>
		  <xsl:when test="not(tei:head) and tei:front//tei:titlePart/tei:title">
		    <xsl:apply-templates mode="plain" select="tei:front//tei:titlePart/tei:title"/>
		  </xsl:when>	
		  <xsl:when test="tei:head and count(tei:head/*)=1 and tei:head/tei:figure">
		    <xsl:text>[</xsl:text>
		    <xsl:sequence select="tei:i18n('figureWord')"/>
		    <xsl:text>]</xsl:text>
		  </xsl:when>
		  <xsl:when test="tei:head[not(.='')] and
				  not(tei:head[count(*)=1 and
				  tei:figure])">
		    <xsl:apply-templates mode="plain" select="tei:head"/>
		  </xsl:when>
		  <xsl:when test="@type='title_page'">Title page</xsl:when>
		  <xsl:when test="@type='index'">Index</xsl:when>
		  <xsl:when test="@type='section'">§</xsl:when>
		  <xsl:when test="$autoHead='true'">
		    <xsl:call-template name="autoMakeHead">
		      <xsl:with-param name="display" select="$display"/>
		    </xsl:call-template>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:number/>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="tei:head and ($display='plain' or $display='simple')">
	    <xsl:for-each select="tei:head">
	      <xsl:apply-templates mode="plain"/>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="tei:head">
	    <xsl:apply-templates mode="makeheading" select="tei:head"/>
	  </xsl:when>
	  <xsl:when test="tei:front/tei:head">
	    <xsl:apply-templates  mode="plain" select="tei:front/tei:head"/>
	  </xsl:when>
	  <xsl:when test="tei:body/tei:head">
	    <xsl:apply-templates mode="plain" select="tei:body/tei:head"/>
		  </xsl:when>
	  <xsl:when test="self::tei:index">
	    <xsl:value-of select="substring(tei:term,1,10)"/>
	    <xsl:text>…</xsl:text>
	  </xsl:when>
	  <xsl:when test="$autoHead='true'">
	    <xsl:call-template name="autoMakeHead">
	      <xsl:with-param name="display" select="$display"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:when test="not(self::tei:div) and $headingNumber=''">
	    <xsl:value-of select="(normalize-space(substring(.,1,20)),'…')"/>
	  </xsl:when>
	</xsl:choose>
      </xsl:if>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>[common] processing the number portion of a heading</p>
         <p>By default, the text is printed as is. You may
    wish to colour it, align it, etc</p>
         <param name="text"/>
      </desc>
   </doc>
  <xsl:template name="formatHeadingNumber">
      <xsl:param name="text"/>
      <xsl:param name="toc"/>
      <xsl:copy-of select="$text"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>[common] Making a heading for something, and making sure it has
      contents</p>
         <p>This is a wrapper around the "header" template which ensures that
      some text is returned; if all else fails, the element name is used.</p>
         <param name="minimal">false</param>
      </desc>
   </doc>
  <xsl:template name="headerLink">
      <xsl:param name="minimal">false</xsl:param>
      <xsl:variable name="Text">
         <xsl:call-template name="header">
	   <xsl:with-param name="minimal" select="$minimalCrossRef"/>
	   <xsl:with-param name="display">plain</xsl:with-param>
         </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$Text=''">
	   <!--
	       <xsl:text>&lt;</xsl:text>
	       <xsl:value-of select="local-name(.)"/>
	       <xsl:text>&gt;</xsl:text>
	   -->
         </xsl:when>
         <xsl:otherwise>
            <xsl:copy-of select="$Text"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>[common] separate two links in one target attribute</p>
      </desc>
   </doc>
  <xsl:template name="multiTargetSeparator">
    <xsl:param name="xmllang"/>
    <xsl:choose>
      <xsl:when test="$xmllang='zh-TW'">
	<xsl:if test="position()&lt;last()">
	  <xsl:text>&#x3001;</xsl:text>
	</xsl:if>
      </xsl:when>
      <xsl:when test="position() eq last()"/>
      <xsl:when test="position() eq last()-1">
	<xsl:if test="position()&gt;1">
	  <xsl:text>,</xsl:text>
	</xsl:if>
	<xsl:text> </xsl:text>
	<xsl:sequence select="tei:i18n('and')"/>
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>, </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] create external link<param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="class">class</param>
      </desc>
   </doc>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="title"/>
      <xsl:param name="class">link_<xsl:value-of select="local-name(.)"/>
      </xsl:param>
      <xsl:element name="{$linkElement}" namespace="{$linkElementNamespace}">
	<xsl:if test="(self::tei:ptr or self::tei:ref) and @xml:id">
	  <xsl:attribute name="id" select="@xml:id"/>
	</xsl:if>
	<xsl:if test="$title">
	  <xsl:attribute name="title" select="$title"/>
	</xsl:if>
	<xsl:call-template name="makeRendition">
	  <xsl:with-param name="default" select="$class"/>
	</xsl:call-template>
	<xsl:if test="@type and not($outputTarget=('epub3', 'html', 'html5'))">
            <xsl:attribute name="type">
               <xsl:value-of select="@type"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:attribute name="{$linkAttribute}" namespace="{$linkAttributeNamespace}">
            <xsl:sequence select="$dest"/>
            <xsl:if test="contains(@from,'id (')">
               <xsl:text>#</xsl:text>
               <xsl:value-of select="substring(@from,5,string-length(normalize-space(@from))-1)"/>
            </xsl:if>
         </xsl:attribute>
	 <xsl:choose>
	   <xsl:when test="@n">
	     <xsl:attribute name="title"  namespace="{$linkAttributeNamespace}">
	       <xsl:value-of select="@n"/>
	     </xsl:attribute>
	   </xsl:when>
	 </xsl:choose>
         <xsl:call-template name="xrefHook"/>
         <xsl:choose>
	   <xsl:when test="$dest=''">??</xsl:when>
            <xsl:when test="$ptr">
               <xsl:element name="{$urlMarkup}" namespace="{$linkElementNamespace}">
                  <xsl:choose>
                     <xsl:when test="starts-with($dest,'mailto:')">
                        <xsl:value-of select="substring-after($dest,'mailto:')"/>
                     </xsl:when>
                     <xsl:when test="starts-with($dest,'file:')">
                        <xsl:value-of select="substring-after($dest,'file:')"/>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="$dest"/>
                     </xsl:otherwise>
                  </xsl:choose>
               </xsl:element>
            </xsl:when>
            <xsl:otherwise>
               <xsl:apply-templates/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] create an internal link<param name="target">target</param>
         <param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="body">body</param>
         <param name="class">class</param>
      </desc>
   </doc>
  <xsl:template name="makeInternalLink">
      <xsl:param name="target"/>
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="body"/>
      <xsl:param name="class">
         <xsl:text>link_</xsl:text>
         <xsl:value-of select="local-name(.)"/>
      </xsl:param>
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="$target">
               <xsl:value-of select="$target"/>
            </xsl:when>
            <xsl:when test="contains($dest,'#')">
               <xsl:value-of select="substring-after($dest,'#')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$dest"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>

      <xsl:choose>
         <xsl:when test="$dest=''">
            <xsl:choose>
               <xsl:when test="not($body='')">
                  <xsl:value-of select="$body"/>
               </xsl:when>
               <xsl:when test="$ptr">
                  <xsl:apply-templates mode="xref" select="id($W)">
                     <xsl:with-param name="minimal" select="$minimalCrossRef"/>
                  </xsl:apply-templates>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:apply-templates/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:variable name="eventualtarget">
	     <xsl:choose>
	       <xsl:when test="starts-with($dest,'#') or  contains($dest,$outputSuffix) or contains($dest,'ID=')">
		 <xsl:value-of select="$dest"/>
	       </xsl:when>
	       <xsl:when test="id($W)"/>
	       <xsl:otherwise>
		 <xsl:apply-templates mode="generateLink" select="id($W)"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:variable name="linktext">
	     <xsl:choose>
	       <xsl:when test="not($body='')">
		 <xsl:value-of select="$body"/>
	       </xsl:when>
               <xsl:when test="$ptr and @type='footnote'">
		 <xsl:text>[</xsl:text>
		 <xsl:number level="any"/>
		 <xsl:text>]</xsl:text>
	       </xsl:when>
	       <xsl:when test="$ptr and id($W)">
		 <xsl:apply-templates mode="xref" select="id($W)">
		   <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		 </xsl:apply-templates>
	       </xsl:when>
	       <xsl:when test="$ptr">
		 <xsl:value-of select="$dest"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="$eventualtarget=''">
	       <xsl:copy-of select="$linktext"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:element name="{$linkElement}"
			    namespace="{$linkElementNamespace}">
		 <xsl:if test="(self::tei:ptr or self::tei:ref) and @xml:id">
		   <xsl:attribute name="id" select="@xml:id"/>
		 </xsl:if>
		 <xsl:call-template name="makeRendition">
		   <xsl:with-param name="default" select="$class"/>
		 </xsl:call-template>		 
		 <xsl:if test="$outputTarget='odt'">
		   <xsl:attribute name="type" namespace="{$linkAttributeNamespace}">simple</xsl:attribute>
		 </xsl:if>
		 <xsl:attribute  name="{$linkAttribute}"
				 namespace="{$linkAttributeNamespace}"
				 select="$eventualtarget"/>
		 <xsl:call-template name="htmlAttributes"/>
		 <xsl:for-each select="id($W)">
		   <xsl:attribute name="title" namespace="{$linkAttributeNamespace}">
		     <xsl:choose>
		       <xsl:when test="@n">
			 <xsl:value-of select="@n"/>
		       </xsl:when>
		       <xsl:when
			   test="starts-with(local-name(.),'div') and tei:head">
			 <xsl:value-of select="tei:sanitize(tei:head)"/>
		       </xsl:when>
		       <xsl:otherwise>
			 <xsl:value-of select="tei:sanitize(./string())"/>
                       </xsl:otherwise>
		     </xsl:choose>
		   </xsl:attribute>
		 </xsl:for-each>
		 <xsl:copy-of select="$linktext"/>
	       </xsl:element>
	     </xsl:otherwise>
	   </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="htmlAttributes"/>
  <xsl:template name="xrefHook"/>
  <xsl:template name="makeRendition">
    <xsl:param name="default"/>
  </xsl:template>


</xsl:stylesheet>

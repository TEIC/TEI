<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 xmlns:m="http://www.w3.org/1998/Math/MathML"
				xmlns="http://www.w3.org/1999/xhtml"
				xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
				xmlns:fo="http://www.w3.org/1999/XSL/Format"
				xmlns:xs="http://www.w3.org/2001/XMLSchema"
				xmlns:html="http://www.w3.org/1999/xhtml"
				xmlns:rng="http://relaxng.org/ns/structure/1.0"
				xmlns:tei="http://www.tei-c.org/ns/1.0"
				xmlns:teix="http://www.tei-c.org/ns/Examples"
				xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
				xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
				exclude-result-prefixes="m a fo html
							 rng tei teix
							 teidocx xs" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the core module, making
      HTML output. </p>
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
      <p>Copyright: 2011, TEI Consortium</p>
    </desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process elements tei:*</p>
      <p>
        <p xmlns="http://www.w3.org/1999/xhtml"> anything with a head can go in the TOC </p>
      </p>
      <param name="forcedepth">forcedepth</param>
    </desc>
  </doc>
  <xsl:template match="tei:*" mode="maketoc">
    <xsl:param name="forcedepth"/>
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:if test="tei:head or $autoHead='true'">
      <xsl:variable name="Depth">
        <xsl:choose>
          <xsl:when test="not($forcedepth='')">
            <xsl:value-of select="$forcedepth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tocDepth"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="thislevel">
        <xsl:choose>
          <xsl:when test="$myName = 'div'">
            <xsl:value-of select="count(ancestor::tei:div)"/>
          </xsl:when>
          <xsl:when test="starts-with($myName,'div')">
            <xsl:value-of select="number(substring-after($myName,'div')) - 1"/>
          </xsl:when>
          <xsl:otherwise>99</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="pointer">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:variable>
      <li>
        <xsl:attribute name="class">
          <xsl:text>toc</xsl:text>
          <xsl:if test="not($autoHead='true') and not(tei:head or @n)"> headless</xsl:if>
	  <xsl:if test=".//m:math and  $outputTarget='epub3'">
	      <xsl:attribute
		  name="class"> contains-mathml</xsl:attribute>
	  </xsl:if>
        </xsl:attribute>
        <xsl:call-template name="header">
          <xsl:with-param name="toc" select="$pointer"/>
          <xsl:with-param name="minimal">false</xsl:with-param>
          <xsl:with-param name="display">plain</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="$thislevel &lt; $Depth">
          <xsl:call-template name="continuedToc"/>
        </xsl:if>
      </li>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element ab</desc>
  </doc>
  <xsl:template match="tei:ab">
    <xsl:choose>
      <xsl:when test="ancestor::tei:head or parent::tei:title or parent::tei:stage">
        <xsl:apply-templates/>
        <xsl:if test="following-sibling::tei:ab">
          <br/>
        </xsl:if>
      </xsl:when>
      <xsl:when test="parent::tei:sp">
        <div>
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">spProse</xsl:with-param>
	  </xsl:call-template>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">false</xsl:with-param>
	  </xsl:call-template>
          <xsl:apply-templates/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element addrLine</desc>
  </doc>
  <xsl:template match="tei:addrLine">
    <xsl:apply-templates/>
    <br/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element address</desc>
  </doc>
  <xsl:template match="tei:address">
    <address>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </address>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element argument</desc>
  </doc>
  <xsl:template match="tei:argument">
    <div class="argument">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element author</desc>
  </doc>
  <xsl:template match="tei:author">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-imports/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element bibl</desc>
  </doc>
  <xsl:template match="tei:bibl">
    <xsl:choose>
      <xsl:when test="parent::tei:q/parent::tei:head or parent::tei:q[@rend='inline']">
        <span class="citbibl">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:when test="parent::tei:cit[@rend='display'] or
		      (parent::tei:cit and tei:p) or  parent::tei:q">
        <div class="citbibl">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="not(tei:is-inline(.))">
        <div class="biblfree">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <span>
          <xsl:attribute name="class">
            <xsl:text>bibl</xsl:text>
            <xsl:if test="@type">
              <xsl:text> bibl-</xsl:text>
              <xsl:value-of select="@type"/>
            </xsl:if>
          </xsl:attribute>
	  <xsl:if test="parent::tei:cit">
	    <xsl:text> (</xsl:text>
	  </xsl:if>
          <xsl:apply-templates/>
	  <xsl:if test="parent::tei:cit">
	    <xsl:text>)</xsl:text>
	  </xsl:if>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element change</desc>
  </doc>
  <xsl:template match="tei:change">
    <tr>
      <td style="vertical-align:top;" width="15%">
        <xsl:value-of select="tei:date"/>
      </td>
      <td width="85%">
        <xsl:value-of select="tei:item"/>
      </td>
    </tr>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element choice</desc>
  </doc>
  <xsl:template match="tei:choice">
    <xsl:choose>
      <xsl:when test="tei:abbr and tei:expan">
        <xsl:apply-templates select="tei:expan"/>
        <xsl:text> (</xsl:text>
        <xsl:apply-templates select="tei:abbr"/>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element choice in plain mode - selects "critical" reading.</desc>
  </doc>
  <xsl:template match="tei:choice" mode="plain">
    <xsl:value-of select="tei:reg"/>
    <xsl:value-of select="tei:expan"/>
    <xsl:value-of select="tei:corr"/>
    <xsl:apply-templates select="tei:choice"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element choice</desc>
  </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process element cit</p>
    </desc>
  </doc>
  <xsl:template match="tei:cit">
    <xsl:choose>
      <xsl:when test="(@rend='display' and tei:quote) or
		      tei:quote/tei:l or tei:quote/tei:p">
        <div>
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="auto">cit</xsl:with-param>
	  </xsl:call-template>
          <xsl:if test="@n">
            <xsl:text>(</xsl:text>
            <xsl:value-of select="@n"/>
            <xsl:text>) </xsl:text>
          </xsl:if>
          <xsl:apply-templates select="tei:q|tei:quote"/>
          <xsl:apply-templates select="tei:*[not(self::tei:q or self::tei:quote)]"/>
        </div>
      </xsl:when>
      <xsl:when test="@rend='display'">
        <blockquote>
          <xsl:call-template name="makeRendition"/>
          <xsl:variable name="contents">
            <xsl:if test="@n">
              <xsl:text>(</xsl:text>
              <xsl:value-of select="@n"/>
              <xsl:text>) </xsl:text>
            </xsl:if>
            <xsl:apply-templates select="tei:q|tei:quote"/>
            <xsl:apply-templates select="tei:*[not(self::tei:q or self::tei:quote)]"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$outputTarget='html5'">
              <xsl:copy-of select="$contents"/>
            </xsl:when>
            <xsl:otherwise>
              <p>
                <xsl:copy-of select="$contents"/>
              </p>
            </xsl:otherwise>
          </xsl:choose>
        </blockquote>
      </xsl:when>
      <xsl:when test="tei:bibl">
        <span>
          <xsl:call-template name="makeRendition"/>
          <xsl:if test="@n">
            <xsl:text>(</xsl:text>
            <xsl:value-of select="@n"/>
            <xsl:text>) </xsl:text>
          </xsl:if>
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:call-template name="makeRendition"/>
          <xsl:if test="@n">
            <xsl:text>(</xsl:text>
            <xsl:value-of select="@n"/>
            <xsl:text>) </xsl:text>
          </xsl:if>
          <xsl:apply-templates/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element code</desc>
  </doc>
  <xsl:template match="tei:code">
    <code>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">false</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </code>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
    Correction 
  </desc>
  </doc>
  <xsl:template match="tei:corr">
    <span>
      <xsl:apply-templates/>
      <xsl:call-template name="makeRendition"/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Decorate date</desc>
  </doc>
  <xsl:template match="tei:date">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-imports/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>The del element</desc>
  </doc>
  <xsl:template match="tei:del">
    <del>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">false</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </del>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>The add element</desc>
  </doc>
  <xsl:template match="tei:add">
      <xsl:choose>
         <xsl:when test="@place='sup' or @place='above'">
            <sup>
	      <xsl:call-template name="makeRendition">
		<xsl:with-param name="default">false</xsl:with-param>
	      </xsl:call-template>
               <xsl:apply-templates/>
            </sup>
         </xsl:when>
         <xsl:when test="@place='sub' or @place='below'">
            <sub>
	      <xsl:call-template name="makeRendition">
		<xsl:with-param name="default">false</xsl:with-param>
	      </xsl:call-template>
               <xsl:apply-templates/>
            </sub>
         </xsl:when>
	 <xsl:otherwise>
	   <span style="color:green;">
	     <xsl:text>&#10216;</xsl:text>
	   </span>
	   <xsl:apply-templates/>
	   <span style="color:green;">
	     <xsl:text>&#10217;</xsl:text>
	   </span>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element eg</desc>
  </doc>
  <xsl:template match="tei:eg">
    <div>
      <xsl:if test="$cssFile">
        <xsl:attribute name="class">
          <xsl:text>pre_eg</xsl:text>
          <xsl:if test="not(*)">
            <xsl:text> cdata</xsl:text>
          </xsl:if>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element emph</desc>
  </doc>
  <xsl:template match="tei:emph">
    <em>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">false</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element epigraph</desc>
  </doc>
  <xsl:template match="tei:epigraph">
    <div>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element epigraph/lg</desc>
  </doc>
  <xsl:template match="tei:epigraph/tei:lg">
    <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element foreign and unclear</desc>
  </doc>
  <xsl:template match="tei:foreign|tei:unclear">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element caesura</desc>
  </doc>
  <xsl:template match="tei:caesura">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:text>    </xsl:text>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element gap</desc>
  </doc>
  <xsl:template match="tei:gap">
    <xsl:element name="{if (not(tei:is-inline(.))) then 'div' else 'span'}">
      <xsl:attribute name="class">gap</xsl:attribute>
      <xsl:if test="tei:desc">
        <xsl:attribute name="title">
          <xsl:value-of select="tei:desc"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="starts-with(@rend,'content:')">
          <xsl:value-of select="substring-after(@rend,'content:')"/>
        </xsl:when>
        <xsl:otherwise>
	  <xsl:call-template name="makeRendition"/>
          <xsl:text> [...]</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process element att</p>
      <p>
        <p xmlns="http://www.w3.org/1999/xhtml"> special purpose </p>
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:att">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:text>@</xsl:text>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process gloss element</desc>
  </doc>
  <xsl:template match="tei:gloss">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process element head</p>
      <p xmlns="http://www.w3.org/1999/xhtml"> headings etc </p>
    </desc>
  </doc>
  <xsl:template match="tei:head">
    <xsl:variable name="parent" select="local-name(..)"/>
    <xsl:choose>
      <xsl:when test="parent::tei:group or parent::tei:body or parent::tei:front or parent::tei:back">
        <h1>
          <xsl:apply-templates/>
        </h1>
      </xsl:when>
      <xsl:when test="parent::tei:argument">
        <div>
          <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">false</xsl:with-param>
	  </xsl:call-template>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="not(starts-with($parent,'div'))">
        <xsl:apply-templates/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element head in plain mode</desc>
  </doc>
  <xsl:template match="tei:head" mode="plain">
    <xsl:if test="preceding-sibling::tei:head">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element head in heading mode</desc>
  </doc>
  <xsl:template match="tei:head" mode="makeheading">
    <xsl:if test="preceding-sibling::tei:head">
      <br/>
    </xsl:if>
    <span>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">head</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element hi</desc>
  </doc>
  <xsl:template match="tei:hi">
    <xsl:variable name="container" select="if (@rend='sup') then 'sup' 
					   else if (@rend='sub') then 'sub' 
					   else if (@rend='subscript') then 'sub' 
					   else if (@rend='superscript') then 'sup' 
					   else if (@rend='code') then 'code' else 'span'"/>
    <xsl:choose>
      <xsl:when test="tei:note[@place='margin']">
	<xsl:for-each-group select="*|text()"
			    group-adjacent="if (self::tei:note)  then 1  else 2">
	  <xsl:choose>
	    <xsl:when test="current-grouping-key()=1">
	      <xsl:apply-templates select="current-group()"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:element name="{$container}">
		<xsl:call-template name="makeRendition"/>
		<xsl:apply-templates select="current-group()"/>
	      </xsl:element>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{$container}">
	  <xsl:call-template name="makeRendition"/>
          <xsl:apply-templates/>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item" mode="bibl">
    <p>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item" mode="glosstable">
    <tr>
      <td style="vertical-align:top;">
        <strong>
          <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
        </strong>
      </td>
      <td>
        <xsl:call-template name="makeAnchor"/>
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item" mode="gloss">
    <dt>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates mode="print" select="preceding-sibling::tei:label[1]"/>
    </dt>
    <dd>
      <xsl:apply-templates/>
    </dd>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item">
    <li>
      <xsl:call-template name="makeRendition"/>
      <xsl:choose>
        <xsl:when test="@xml:id">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name">
              <xsl:value-of select="@xml:id"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="$generateParagraphIDs='true'">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name">
              <xsl:value-of select="generate-id()"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>
      <xsl:apply-templates/>
    </li>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item" mode="inline">
    <xsl:if test="preceding-sibling::tei:item">, </xsl:if>
    <xsl:if test="not(following-sibling::tei:item) and preceding-sibling::tei:item">
      and </xsl:if>
    <xsl:text>• </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item/tei:label</desc>
  </doc>
  <xsl:template match="tei:item/tei:label">
    <span>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">false</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element label</desc>
  </doc>
  <xsl:template match="tei:label">
    <span>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element label</desc>
  </doc>
  <xsl:template match="tei:label" mode="print">
    <span>
      <xsl:call-template name="makeAnchor"/>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default">false</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element lb in plain mode</desc>
  </doc>
  <xsl:template match="tei:lb" mode="plain">
    <xsl:text> </xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element lb. ignore if at very start or very end of a
    block-level context</desc>
  </doc>
  <xsl:template match="tei:lb">
    <xsl:choose>
      <xsl:when test="parent::tei:body"/>
      <xsl:when test="parent::tei:back"/>
      <xsl:when test="parent::tei:front"/>
      <xsl:when test="@type='hyphenInWord' and @rend='hidden'"/>
      <xsl:when test="@rend='hidden'">
        <xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="@rend='-' or @type='hyphenInWord'">
        <xsl:text>-</xsl:text>
        <br/>
      </xsl:when>
      <xsl:when test="@rend='above'">
        <xsl:text>⌜</xsl:text>
      </xsl:when>
      <xsl:when test="@rend='below'">
        <xsl:text>⌞</xsl:text>
      </xsl:when>
      <xsl:when test="not(tei:is-inline(..)) and (tei:is-last(.) or
		      tei:is-first(.))"/>
      <xsl:otherwise>
        <br>
	  <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">false</xsl:with-param>
	  </xsl:call-template>
	</br>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element l</desc>
  </doc>
  <xsl:template match="tei:l">
    <xsl:element name="{if (ancestor::tei:head or ancestor::tei:hi) then 'span' else 'div'}">
      <xsl:call-template name="makeRendition"/>
      <xsl:choose>
        <xsl:when test="ancestor::tei:div[contains(@rend,'linenumber')]">
          <xsl:variable name="n">
            <xsl:number/>
          </xsl:variable>
          <div class="numbering">
            <xsl:choose>
              <xsl:when test="$n mod 5 = 0">
                <xsl:value-of select="$n"/>
              </xsl:when>
              <xsl:otherwise> </xsl:otherwise>
            </xsl:choose>
          </div>
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element lg</desc>
  </doc>
  <xsl:template match="tei:lg">
    <xsl:choose>
      <xsl:when test="$filePerPage='true'">
        <xsl:for-each-group select="node()" group-starting-with="tei:pb">
          <xsl:choose>
            <xsl:when test="self::tei:pb">
              <xsl:apply-templates select="."/>
              <div>
                <xsl:for-each select="..">
                  <xsl:call-template name="makeRendition"/>
		  <xsl:attribute name="id">
		    <xsl:choose>
		      <xsl:when test="@xml:id">
			<xsl:value-of select="@xml:id"/>
			<xsl:text>continued</xsl:text>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:text>false</xsl:text>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:attribute>
                </xsl:for-each>
                <xsl:apply-templates select="current-group() except ."/>
              </div>
            </xsl:when>
            <xsl:otherwise>
              <div>
                <xsl:for-each select="..">
                  <xsl:call-template name="makeRendition"/>
                </xsl:for-each>
                <xsl:apply-templates select="current-group()"/>
              </div>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each-group>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:call-template name="makeRendition"/>
          <xsl:apply-templates/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process line inside line group</desc>
  </doc>
  <xsl:template match="tei:lg/tei:l">
    <div>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process element list</p>
      <p>
        <p xmlns="http://www.w3.org/1999/xhtml">Lists. Depending on the value of the 'type' attribute, various HTML
        lists are generated: <dl><dt>bibl</dt><dd>Items are processed in mode 'bibl'</dd><dt>catalogue</dt><dd>A gloss list is created, inside a paragraph</dd><dt>gloss</dt><dd>A gloss list is created, expecting alternate label and item
            elements</dd><dt>glosstable</dt><dd>Label and item pairs are laid out in a two-column table</dd><dt>inline</dt><dd>A comma-separate inline list</dd><dt>runin</dt><dd>An inline list with bullets between items</dd><dt>unordered</dt><dd>A simple unordered list</dd><dt>ordered</dt><dd>A simple ordered list</dd><dt>valList</dt><dd>(Identical to glosstable)</dd></dl>
            </p>
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:list">
    <xsl:if test="tei:head">
      <xsl:element name="{if (not(tei:is-inline(.))) then 'div' else 'span' }">
        <xsl:attribute name="class">listhead</xsl:attribute>
        <xsl:apply-templates select="tei:head"/>
      </xsl:element>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='catalogue'">
        <p>
          <dl>
            <xsl:call-template name="makeRendition">
              <xsl:with-param name="default">false</xsl:with-param>
            </xsl:call-template>
            <xsl:for-each select="tei:item">
              <p/>
              <xsl:apply-templates mode="gloss" select="."/>
            </xsl:for-each>
          </dl>
        </p>
      </xsl:when>
      <xsl:when test="@type='gloss' and @rend='multicol'">
        <xsl:variable name="nitems">
          <xsl:value-of select="count(tei:item)div 2"/>
        </xsl:variable>
        <p>
          <table>
            <xsl:call-template name="makeRendition">
              <xsl:with-param name="default">false</xsl:with-param>
	    </xsl:call-template>
            <tr>
              <td style="vertical-align:top;">
                <dl>
                  <xsl:apply-templates mode="gloss" select="tei:item[position()&lt;=$nitems ]"/>
                </dl>
              </td>
              <td style="vertical-align:top;">
                <dl>
                  <xsl:apply-templates mode="gloss" select="tei:item[position() &gt;$nitems]"/>
                </dl>
              </td>
            </tr>
          </table>
        </p>
      </xsl:when>
      <xsl:when test="@type='gloss' or  tei:label">
        <dl>
          <xsl:call-template name="makeRendition">
            <xsl:with-param name="default">false</xsl:with-param>
          </xsl:call-template>
          <xsl:apply-templates mode="gloss" select="tei:item"/>
        </dl>
      </xsl:when>
      <xsl:when test="@type='glosstable' or @type='valList'">
        <table>
          <xsl:call-template name="makeRendition">
            <xsl:with-param name="default">false</xsl:with-param>
          </xsl:call-template>
          <xsl:apply-templates mode="glosstable" select="tei:item"/>
        </table>
      </xsl:when>
      <xsl:when test="@type='inline' or ancestor::tei:head or parent::tei:label">
        <!--<xsl:if test="not(tei:item)">None</xsl:if>-->
        <xsl:apply-templates mode="inline" select="tei:item"/>
      </xsl:when>
      <xsl:when test="@type='runin'">
        <p>
          <xsl:apply-templates mode="runin" select="tei:item"/>
        </p>
      </xsl:when>
      <xsl:when test="@type='unordered' or @type='simple'">
        <ul>
          <xsl:call-template name="makeRendition">
            <xsl:with-param name="default">false</xsl:with-param>
          </xsl:call-template>
          <xsl:apply-templates select="tei:item"/>
        </ul>
      </xsl:when>
      <xsl:when test="@type='bibl'">
        <xsl:apply-templates mode="bibl" select="tei:item"/>
      </xsl:when>
      <xsl:when test="starts-with(@type,'ordered')">
        <ol>
          <xsl:call-template name="makeRendition">
            <xsl:with-param name="default">false</xsl:with-param>
          </xsl:call-template>
          <xsl:if test="starts-with(@type,'ordered:')">
            <xsl:attribute name="start">
              <xsl:value-of select="substring-after(@type,':')"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:apply-templates select="tei:item"/>
        </ol>
      </xsl:when>
      <xsl:otherwise>
        <ul>
          <xsl:call-template name="makeRendition">
            <xsl:with-param name="default">false</xsl:with-param>
          </xsl:call-template>
          <xsl:apply-templates select="tei:item"/>
        </ul>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element list/tei:label</desc>
  </doc>
  <xsl:template match="tei:list/tei:label"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element listBibl</desc>
  </doc>
  <xsl:template match="tei:listBibl">
    <xsl:choose>
      <xsl:when test="tei:biblStruct and $biblioStyle='mla'">
	<div type="listBibl" xmlns="http://www.w3.org/1999/xhtml">	  
	<xsl:for-each select="tei:biblStruct">
	  <p class="hang" xmlns="http://www.w3.org/1999/xhtml">
	    <xsl:apply-templates select="tei:analytic" mode="mla"/>
	    <xsl:apply-templates select="tei:monogr" mode="mla"/>
	    <xsl:apply-templates select="tei:relatedItem" mode="mla"/>
	    <xsl:choose>
	      <xsl:when test="tei:note">
		<xsl:apply-templates select="tei:note"/>
	      </xsl:when>
	      <xsl:when test="*//tei:ref/@target and not(contains(*//tei:ref/@target, '#'))">
		<xsl:text>Web.&#10;</xsl:text>
		<xsl:if test="*//tei:imprint/tei:date/@type='access'">
		  <xsl:value-of select="*//tei:imprint/tei:date[@type='access']"/>
		  <xsl:text>.</xsl:text>
		</xsl:if>
	      </xsl:when>
	      <xsl:when test="tei:analytic/tei:title[@level='u'] or tei:monogr/tei:title[@level='u']"/>
	      <xsl:otherwise>Print.&#10;</xsl:otherwise>
	    </xsl:choose>
	    <xsl:if test="tei:monogr/tei:imprint/tei:extent"><xsl:value-of select="tei:monogr/tei:imprint/tei:extent"/>. </xsl:if>
	    <xsl:if test="tei:series/tei:title[@level='s']">
	      <xsl:apply-templates select="tei:series/tei:title[@level='s']"/>
	      <xsl:text>. </xsl:text>
	    </xsl:if>
	  </p>
	</xsl:for-each>
	</div>
      </xsl:when>
      <xsl:when test="tei:biblStruct">
        <ol class="listBibl">
          <xsl:for-each select="tei:biblStruct">
            <xsl:sort select="lower-case((tei:*/tei:author/tei:surname|tei:*[1]/tei:author/tei:orgName|tei:*[1]/tei:author/tei:name|tei:*[1]/tei:author|tei:*[1]/tei:editor/tei:surname|tei:*[1]/tei:editor/tei:name|tei:*[1]/tei:editor|tei:*[1]/tei:title[1])[1])"/>
            <xsl:sort select="tei:monogr/tei:imprint/tei:date"/>
            <li>
              <xsl:call-template name="makeAnchor"/>
              <xsl:apply-templates select="."/>
            </li>
          </xsl:for-each>
        </ol>
      </xsl:when>
      <xsl:otherwise>
        <ol class="listBibl">
          <xsl:for-each select="tei:bibl|tei:biblItem">
            <li>
              <xsl:call-template name="makeAnchor">
                <xsl:with-param name="name">
                  <xsl:apply-templates mode="ident" select="."/>
                </xsl:with-param>
              </xsl:call-template>
              <xsl:apply-templates select="."/>
            </li>
          </xsl:for-each>
        </ol>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element mentioned</desc>
  </doc>
  <xsl:template match="tei:mentioned">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element name in mode "plain"</desc>
  </doc>
  <xsl:template match="tei:name" mode="plain">
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <span>
      <xsl:call-template name="makeAnchor">
        <xsl:with-param name="name">
          <xsl:value-of select="$ident"/>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process plain note without any @place attribute</desc>
  </doc>

  <xsl:template name="plainNote">
    <xsl:variable name="identifier">
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <span>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="auto">note</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="makeAnchor">
	<xsl:with-param name="name" select="$identifier"/>
      </xsl:call-template>
      <span class="noteLabel">
	<xsl:choose>
	  <xsl:when test="@n">
	    <xsl:value-of select="@n"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Note</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text>: </xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </span>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process footnotes and endnotes</desc>
  </doc>
  <xsl:template name="endNote">
    <xsl:call-template name="footNote"/>
  </xsl:template>

  <xsl:template name="footNote">
    <xsl:variable name="identifier">
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <xsl:element name="{if (parent::tei:head or parent::tei:hi)  then 'span'           else if (parent::tei:l) then 'span'           else if (parent::tei:bibl/parent::tei:head) then 'span' else if (parent::tei:stage/parent::tei:q) then 'span'           else if  (parent::tei:body or *[not(tei:is-inline(.))]) then 'div' else 'span' }">
      <xsl:call-template name="makeAnchor">
	<xsl:with-param name="name" select="concat($identifier,'_return')"/>
      </xsl:call-template>
      <xsl:variable name="note-title">
	<xsl:variable name="note-text">
	  <xsl:apply-templates mode="plain"/>
	</xsl:variable>
	<xsl:value-of select="substring($note-text,1,500)"/>
	<xsl:if test="string-length($note-text) &gt; 500">
	  <xsl:text>…</xsl:text>
	</xsl:if>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="$footnoteFile='true'">
	  <a class="notelink" title="{normalize-space($note-title)}" href="{$masterFile}-notes.html#{$identifier}">
	    <xsl:element name="{if (@rend='nosup') then 'span' else 'sup'}">
	      <xsl:call-template name="noteN"/>
	    </xsl:element>
	  </a>
	  <xsl:if test="following-sibling::node()[1][self::tei:note]">
	    <xsl:element name="{if (@rend='nosup') then 'span' else 'sup'}">
	      <xsl:text>,</xsl:text>
	    </xsl:element>
	  </xsl:if>
	</xsl:when>
	<xsl:otherwise>
	  <a class="notelink" title="{normalize-space($note-title)}" href="#{$identifier}">
	    <xsl:element name="{if (@rend='nosup') then 'span' else 'sup'}">				  
	      <xsl:call-template name="noteN"/>
	    </xsl:element>
	  </a>
	  <xsl:if test="following-sibling::node()[1][self::tei:note]">
	    <xsl:element name="{if (@rend='nosup') then 'span' else 'sup'}">
	      <xsl:text>,</xsl:text>
	    </xsl:element>
	  </xsl:if>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process display-style note</desc>
  </doc>
  <xsl:template name="displayNote">
    <xsl:variable name="identifier">
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="parent::tei:div or parent::tei:p or parent::tei:body">
	<div class="note">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
          </xsl:call-template>
          <span class="noteLabel">
            <xsl:choose>
              <xsl:when test="@n">
                <xsl:value-of select="@n"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Note</xsl:with-param>
                </xsl:call-template>
                <xsl:text>: </xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </span>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <blockquote>
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
          </xsl:call-template>
          <xsl:call-template name="makeRendition"/>
          <xsl:choose>
            <xsl:when test="$outputTarget='html5'">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:when test="tei:q">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
              <p>
                <xsl:apply-templates/>
              </p>
            </xsl:otherwise>
          </xsl:choose>
        </blockquote>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process  note with a @place attribute in the margin</desc>
  </doc>
  <xsl:template name="marginalNote">
    <xsl:variable name="identifier">
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="@place='margin' and parent::tei:hi and not(*)">
        <span class="margnote">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
          </xsl:call-template>
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:when test="@place='margin' and (tei:p or tei:list or
		      tei:table or tei:lg or *[not(tei:is-inline(.))])">
        <div class="margnote">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
          </xsl:call-template>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="@place='margin'	or    @place='marginOuter' or
		      @place='marginLeft' or
		      @place='marginRight'">
        <span class="note{@place}">
          <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
          </xsl:call-template>
          <xsl:apply-templates/>
        </span>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Check whether a note should be processed if we are at the top</desc>
  </doc>
  <xsl:template match="tei:note" mode="printallnotes">
    <xsl:choose>
      <xsl:when test="number($splitLevel)=-1">
        <xsl:call-template name="makeaNote"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:group"/>
      <xsl:when test="not(ancestor::tei:div or ancestor::tei:div1)">
        <xsl:call-template name="makeaNote"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Check whether a note should be processed. If we are
    splitting, check that we are in the correct file</desc>
  </doc>
  <xsl:template match="tei:note" mode="printnotes">
    <xsl:param name="whence" select="."/>
    <xsl:choose>
      <xsl:when test="ancestor::tei:listBibl"/>
      <xsl:when test="number($splitLevel)=-1"/>
      <xsl:when test="@place='foot' or @place='bottom' or @place='end'         or $autoEndNotes='true'">
        <xsl:variable name="parent">
	  <xsl:for-each select="ancestor::tei:*[local-name()='div'
	    or local-name()='div1'
	    or local-name()='div2'
	    or local-name()='div3'
	    or local-name()='div4'
	    or local-name()='div5'
	    or local-name()='div6'][1]">
	    <xsl:call-template name="locateParentDiv"/>
	  </xsl:for-each>
        </xsl:variable>

        <xsl:if test="$whence = $parent">
          <xsl:call-template name="makeaNote"/>
        </xsl:if>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element note</desc>
  </doc>
  <xsl:template name="makeaNote">
    <xsl:variable name="identifier">
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message>Make note <xsl:value-of select="$identifier"/></xsl:message>
    </xsl:if>
    <div class="note">
      <xsl:call-template name="makeAnchor">
        <xsl:with-param name="name" select="$identifier"/>
      </xsl:call-template>
      <span class="noteLabel">
        <xsl:call-template name="noteN"/>
        <xsl:if test="matches(@n,'[0-9]')">
          <xsl:text>.</xsl:text>
        </xsl:if>
        <xsl:text> </xsl:text>
      </span>
      <div class="noteBody">
        <xsl:apply-templates/>
      </div>
      <xsl:if test="$footnoteBackLink= 'true'">
        <xsl:text> </xsl:text>
        <a class="link_return" title="Go back to text" href="#{concat($identifier,'_return')}">↵</a>
      </xsl:if>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element note[@type='action']</desc>
  </doc>
  <xsl:template match="tei:note[@type='action']">
    <div class="right"><b>Action <xsl:number count="tei:note[@type='action']" level="any"/>
         </b>: <i><xsl:apply-templates/></i></div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process element pb</p>
      <p>Indication of a page break. For the purposes of HTML, we simply
      make it an anchor if it has an ID.</p>
    </desc>
  </doc>
  <xsl:template match="tei:pb" mode="ident">
    <xsl:choose>
      <xsl:when test="@xml:id">
        <xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>page</xsl:text>
        <!--
	<xsl:for-each select="ancestor::tei:div[1]">
	  <xsl:number level="multiple" format="1.1.1.1.1"/>
	  <xsl:text>-</xsl:text>
	</xsl:for-each>
-->
        <xsl:number level="any"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:pb">
    <xsl:choose>
      <xsl:when test="$filePerPage='true'">
        <PAGEBREAK>
          <xsl:attribute name="name">
            <xsl:apply-templates select="." mode="ident"/>
          </xsl:attribute>
          <xsl:copy-of select="@facs"/>
        </PAGEBREAK>
      </xsl:when>
      <xsl:when test="@facs and not(@rend='none')">
        <xsl:variable name="IMG">
          <xsl:choose>
            <xsl:when test="starts-with(@facs,'#')">
              <xsl:for-each select="id(substring(@facs,2))">
                <xsl:value-of select="tei:graphic[1]/@url"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@facs"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:element name="{if (tei:is-inline(..)) then 'span' else 'div'}">
          <xsl:call-template name="makeRendition"/>
          <img src="{$IMG}" alt="page image"/>
        </xsl:element>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='active'">
        <div>
          <xsl:call-template name="makeRendition">
	    <xsl:with-param  name="default">pagebreak</xsl:with-param>
	  </xsl:call-template>
        </div>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='visible' and (parent::tei:body         or parent::tei:front or parent::tei:back or parent::tei:group)">
        <div class="pagebreak">
          <xsl:call-template name="makeAnchor"/>
          <xsl:text> [</xsl:text>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">page</xsl:with-param>
          </xsl:call-template>
          <xsl:if test="@n">
            <xsl:text> </xsl:text>
            <xsl:value-of select="@n"/>
          </xsl:if>
          <xsl:text>] </xsl:text>
        </div>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='visible'">
        <span class="pagebreak">
          <xsl:call-template name="makeAnchor"/>
          <xsl:text> [</xsl:text>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">page</xsl:with-param>
          </xsl:call-template>
          <xsl:if test="@n">
            <xsl:text> </xsl:text>
            <xsl:value-of select="@n"/>
          </xsl:if>
          <xsl:text>] </xsl:text>
        </span>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element p</desc>
  </doc>
  <xsl:template match="tei:p">
    <xsl:variable name="wrapperElement" select="tei:is-DivOrP(.)"/>
    <xsl:choose>
      <xsl:when test="$filePerPage='true'">
        <xsl:for-each-group select="node()" group-starting-with="tei:pb">
          <xsl:choose>
            <xsl:when test="self::tei:pb">
              <xsl:apply-templates select="."/>
              <xsl:element name="{$wrapperElement}">
                <xsl:for-each select="..">
                  <xsl:call-template name="makeRendition">
		    <xsl:with-param name="default">false</xsl:with-param>
		  </xsl:call-template>
		  <xsl:attribute name="id">
		    <xsl:choose>
		      <xsl:when test="@xml:id">
			<xsl:value-of select="@xml:id"/>
			<xsl:text>continued</xsl:text>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:text>false</xsl:text>
		      </xsl:otherwise>
                      </xsl:choose>
		  </xsl:attribute>
                </xsl:for-each>
                <xsl:apply-templates select="current-group() except ."/>
              </xsl:element>
            </xsl:when>
            <xsl:otherwise>
              <xsl:element name="{$wrapperElement}">
                <xsl:for-each select="..">
                  <xsl:call-template name="makeRendition">
		    <xsl:with-param name="default">false</xsl:with-param>
		  </xsl:call-template>
                </xsl:for-each>
                <xsl:apply-templates select="current-group()"/>
              </xsl:element>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each-group>
      </xsl:when>
      <xsl:when test="$generateDivFromP='true' or teix:egXML">
	<div>
	  <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">p</xsl:with-param>
	  </xsl:call-template>
	  <xsl:choose>
	    <xsl:when test="@xml:id">
	      <xsl:call-template name="makeAnchor">
		<xsl:with-param name="name">
		  <xsl:value-of select="@xml:id"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:when test="$generateParagraphIDs='true'">
	      <xsl:call-template name="makeAnchor">
		<xsl:with-param name="name">
		  <xsl:value-of select="generate-id()"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:when>
	  </xsl:choose>
	  <xsl:if test="$numberParagraphs='true'">
	    <xsl:call-template name="numberParagraph"/>
	  </xsl:if>
	  <xsl:apply-templates/>
	</div>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="CLASS">
	  <freddy>
	    <xsl:call-template name="makeRendition">
	      <xsl:with-param name="default">false</xsl:with-param>
	    </xsl:call-template>
	  </freddy>
	</xsl:variable>
	<xsl:variable name="ID">
	  <freddy>
	    <xsl:choose>
	      <xsl:when test="@xml:id">
		<xsl:call-template name="makeAnchor">
		  <xsl:with-param name="name">
		    <xsl:value-of select="@xml:id"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:when test="$generateParagraphIDs='true'">
		<xsl:call-template name="makeAnchor">
		  <xsl:with-param name="name">
		    <xsl:value-of select="generate-id()"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:when>
	    </xsl:choose>
	  </freddy>
	</xsl:variable>
	<xsl:variable name="pContent">
	  <xsl:if test="$numberParagraphs='true'">
	    <xsl:call-template name="numberParagraph"/>
	  </xsl:if>
	  <xsl:apply-templates/>
	</xsl:variable>
	<xsl:for-each-group select="$pContent/node()" 		
			      group-adjacent="if (self::html:ol or
					    self::html:ul or
					    self::html:dl or
					    self::html:figure or
					    self::html:blockquote or
					    self::html:div) then 1
					    else 2">
	      <xsl:choose>
		<xsl:when test="current-grouping-key()=1">
		  <xsl:copy-of select="current-group()"/>
		</xsl:when>
		<xsl:otherwise>
		  <p>
		    <xsl:copy-of select="$CLASS/html:freddy/@*"/>
		    <xsl:if test="position()=1">
		      <xsl:copy-of select="$ID/html:freddy/@*"/>
		    </xsl:if>
		    <xsl:copy-of select="current-group()"/>
		  </p>
		</xsl:otherwise>
	      </xsl:choose>
	  </xsl:for-each-group>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>How to number paragraphs</desc>
  </doc>
  <xsl:template name="numberParagraph">
    <span class="numberParagraph">
      <xsl:number/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element q and tei:said</desc>
  </doc>
  <xsl:template match="tei:q[@rend='inline margQuotes' or
		       @rend='inline margSglQuotes']" priority="99">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:q[not(@place) and tei:l]">
    <xsl:choose>
      <xsl:when test="not(tei:is-inline(.))">
        <div class="blockquote">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:q|tei:said">
    <xsl:choose>
      <xsl:when test="not(tei:is-inline(.)) or *[not(tei:is-inline(.))]">
        <div>
	  <xsl:call-template name="makeRendition">
	    <xsl:with-param name="auto">blockquote</xsl:with-param>
	  </xsl:call-template>
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <span>
	  <xsl:call-template name="makeRendition"/>
          <xsl:call-template name="makeQuote"/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element quote</desc>
  </doc>
  <xsl:template match="tei:quote">
    <xsl:choose>
      <xsl:when test="parent::tei:cit[@rend='display'] or
		      parent::tei:cit and (tei:p or tei:l)">
        <div class="citquote">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="not(tei:is-inline(.))">
        <blockquote>
          <xsl:call-template name="makeRendition"/>
          <xsl:choose>
            <xsl:when test="$outputTarget='html5'">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:when test="tei:p|tei:l|tei:lg">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
              <p>
                <xsl:apply-templates/>
              </p>
            </xsl:otherwise>
          </xsl:choose>
        </blockquote>
      </xsl:when>
      <xsl:otherwise>
	<span>
	  <xsl:call-template name="makeRendition"/>
	  <xsl:call-template name="makeQuote"/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element resp</desc>
  </doc>
  <xsl:template match="tei:resp">
    <xsl:apply-templates/>
    <xsl:text> </xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element respStmt</desc>
  </doc>
  <xsl:template match="tei:respStmt">
    <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element salute</desc>
  </doc>
  <xsl:template match="tei:salute">
    <xsl:choose>
      <xsl:when test="parent::tei:closer">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <div class="left">
          <xsl:apply-templates/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element seg, pass through @type and @rend as @class</desc>
  </doc>
  <xsl:template match="tei:seg">
    <span>
      <xsl:choose>
        <xsl:when test="@type">
	  <xsl:call-template name="makeLang"/>
          <xsl:attribute name="class">
            <xsl:value-of select="@type"/>
          </xsl:attribute>
        </xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="makeRendition">
	    <xsl:with-param name="default">false</xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
    put (sic) after text
  </desc>
  </doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/>
    <xsl:text> (sic)</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element signed</desc>
  </doc>
  <xsl:template match="tei:signed">
    <div class="signed">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element space</desc>
  </doc>
  <xsl:template match="tei:space">
    <span style="display:inline-block">
      <xsl:if test="@quantity">
        <xsl:variable name="unit">
          <xsl:choose>
            <xsl:when test="@unit='chars'">
              <xsl:text>em</xsl:text>
            </xsl:when>
            <xsl:when test="@unit">
              <xsl:value-of select="@unit"/>
            </xsl:when>
            <xsl:otherwise>em</xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:attribute name="width">
          <xsl:value-of select="@quantity"/>
          <xsl:value-of select="$unit"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:text> </xsl:text>
    </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element term</desc>
  </doc>
  <xsl:template match="tei:term">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element title in "withbr" mode</desc>
  </doc>
  <xsl:template match="tei:title" mode="withbr">
    <xsl:value-of select="."/>
    <br/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element title when it is a child of body</desc>
  </doc>
  <xsl:template match="tei:body/tei:title">
    <div class="title">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element witList</desc>
  </doc>
  <xsl:template match="tei:witList">
    <xsl:apply-templates select="./witness"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element witness</desc>
  </doc>
  <xsl:template match="tei:witness">
    <p id="{@sigil}">
      <b>Sigle: <xsl:value-of select="@sigil"/>
         </b>
      <br/>
      <xsl:value-of select="text()"/>
      <br/>
      <xsl:apply-templates select="tei:biblStruct"/>
      <xsl:if test="child::tei:note"><br/>See: <xsl:apply-templates select="child::tei:note"/></xsl:if>
    </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] sections in mode for table of contents</desc>
  </doc>
  <xsl:template name="continuedToc">
    <xsl:if test="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
      <ul class="toc">
        <xsl:apply-templates mode="maketoc" select="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"/>
      </ul>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Show relevant footnotes <param name="currentID">currentID</param>
      </desc>
  </doc>
  <xsl:template name="partialFootNotes">
    <xsl:param name="currentID"/>
    <xsl:choose>
      <xsl:when test="$currentID='current'"/>
      <xsl:when test="$currentID='' and number($splitLevel)=-1">
        <xsl:call-template name="printNotes"/>
      </xsl:when>
      <xsl:when test="$currentID=''">
        <xsl:for-each select="descendant::tei:text">
          <xsl:call-template name="printNotes"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(id($currentID))&gt;0">
            <xsl:for-each select="id($currentID)">
              <xsl:call-template name="printNotes"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="xpath" select="ancestor-or-self::tei:TEI/descendant::tei:text">
              <xsl:with-param name="xpath" select="$currentID"/>
              <xsl:with-param name="action">notes</xsl:with-param>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] </desc>
  </doc>
  <xsl:template name="printNotes">
    <xsl:if test="count(key('NOTES',1)) or ($autoEndNotes='true' and count(key('ALLNOTES',1)))">
      <xsl:choose>
        <xsl:when test="$footnoteFile='true'">
          <xsl:variable name="BaseFile">
            <xsl:value-of select="$masterFile"/>
            <xsl:call-template name="addCorpusID"/>
          </xsl:variable>
          <xsl:variable name="outName">
            <xsl:call-template name="outputChunkName">
              <xsl:with-param name="ident">
                <xsl:value-of select="concat($BaseFile,'-notes')"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:variable>
          <xsl:if test="$verbose='true'">
            <xsl:message>Opening file <xsl:value-of select="$outName"/>
               </xsl:message>
          </xsl:if>
          <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}" encoding="{$outputEncoding}" href="{$outName}" method="{$outputMethod}">
            <html>
              <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. DO NOT EDIT (11)</xsl:comment>
              <xsl:call-template name="addLangAtt"/>
              <head>
                <title>
                  <xsl:apply-templates select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/>
                  <xsl:text>: </xsl:text>
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">noteHeading</xsl:with-param>
                  </xsl:call-template>
                </title>
                <xsl:call-template name="includeCSS"/>
                <xsl:call-template name="cssHook"/>
              </head>
              <body>
                <xsl:call-template name="bodyMicroData"/>
                <xsl:call-template name="bodyJavascriptHook"/>
                <xsl:call-template name="bodyHook"/>
                <div class="stdheader">
                  <xsl:call-template name="stdheader">
                    <xsl:with-param name="title">
                      <xsl:apply-templates select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/>
                      <xsl:text>: </xsl:text>
                      <xsl:call-template name="i18n">
                        <xsl:with-param name="word">noteHeading</xsl:with-param>
                      </xsl:call-template>
                    </xsl:with-param>
                  </xsl:call-template>
                </div>
                <div class="notes">
                  <div class="noteHeading">
                    <xsl:call-template name="i18n">
                      <xsl:with-param name="word">noteHeading</xsl:with-param>
                    </xsl:call-template>
                  </div>
                  <xsl:choose>
                    <xsl:when test="$autoEndNotes='true'">
                      <xsl:apply-templates mode="printnotes" select="key('ALLNOTES',1)"/>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:apply-templates mode="printnotes" select="key('NOTES',1)"/>
                    </xsl:otherwise>
                  </xsl:choose>
                </div>
                <xsl:call-template name="stdfooter"/>
                <xsl:call-template name="bodyEndHook"/>
              </body>
            </html>
          </xsl:result-document>
          <xsl:if test="$verbose='true'">
            <xsl:message>Closing file <xsl:value-of select="$outName"/>
               </xsl:message>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
	  
          <xsl:variable name="me">
            <xsl:apply-templates select="." mode="ident"/>
          </xsl:variable>
          <xsl:variable name="NOTES">
            <xsl:choose>
              <xsl:when test="self::tei:TEI">
                <xsl:choose>
                  <xsl:when test="$autoEndNotes='true'">
                    <xsl:apply-templates mode="printallnotes" select="key('ALLNOTES',1)"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates mode="printallnotes" select="key('NOTES',1)"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:when>
              <xsl:when test="parent::tei:group and tei:group">
	      </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates mode="printnotes" select=".//tei:note">
                  <xsl:with-param name="whence" select="$me"/>
                </xsl:apply-templates>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:for-each select="$NOTES">
            <xsl:if test="html:div">
              <div class="notes">
                <div class="noteHeading">
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">noteHeading</xsl:with-param>
                  </xsl:call-template>
                </div>
                <xsl:copy-of select="*"/>
              </div>
            </xsl:if>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:if test="ancestor-or-self::tei:TEI/tei:text/descendant::tei:app">
      <div class="notes">
        <div class="noteHeading">
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">noteHeading</xsl:with-param>
          </xsl:call-template>
        </div>
        <xsl:apply-templates mode="printnotes" select="descendant::tei:app"/>
      </div>
    </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Create a point to which we can link in the HTML<param name="name">value for identifier</param>
      </desc>
  </doc>
  <xsl:template name="makeAnchor">
    <xsl:param name="name"/>
    <xsl:choose>
      <xsl:when test="self::tei:anchor and $name">
        <a id="{$name}">
          <xsl:comment>anchor</xsl:comment>
        </a>
      </xsl:when>
      <xsl:when test="self::tei:anchor">
        <a id="{@xml:id}">
          <xsl:comment>anchor</xsl:comment>
        </a>
      </xsl:when>
      <xsl:when test="self::tei:index and $name">
        <a id="{$name}">
          <xsl:comment>index</xsl:comment>
        </a>
      </xsl:when>
      <xsl:when test="self::tei:index">
        <a id="{@xml:id}">
          <xsl:comment>index</xsl:comment>
        </a>
      </xsl:when>
      <xsl:when test="$name">
        <xsl:attribute name="id" select="$name"/>
      </xsl:when>
      <xsl:when test="@xml:id">
        <xsl:attribute name="id" select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="me"><xsl:value-of select="$masterFile"/>-<xsl:value-of select="local-name(.)"/>-<xsl:value-of select="generate-id()"/></xsl:variable>
        <xsl:attribute name="id" select="$me"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element soCalled</desc>
  </doc>
  <xsl:template match="tei:soCalled">
    <xsl:choose>
      <xsl:when test="@rend or @rendition or @style">
	<span>
	  <xsl:call-template name="makeRendition"/>
	  <xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$preQuote"/>
        <xsl:apply-templates/>
        <xsl:value-of select="$postQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Copy all attributes in HTML namespace</desc>
  </doc>
  <xsl:template name="htmlAttributes">
    <xsl:for-each select="@*">
      <xsl:if test="namespace-uri(.)='http://www.w3.org/1999/xhtml'">
        <xsl:attribute name="{local-name(.)}">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="makeSpan">
    <xsl:element name="{if (not(tei:is-inline(.))) then 'div' else 'span'}">
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <xsl:template name="microdata"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] processing analytic element as needed for MLA style (from Laura Mandell> </desc>
  </doc>
  <xsl:template match="tei:analytic" mode="mla">
    <xsl:variable name="refIdwHash">
      <xsl:value-of select="following-sibling::tei:monogr/tei:ref/@target"/>
    </xsl:variable>
    <xsl:variable name="refId">
      <xsl:value-of select="substring-after($refIdwHash, '#')"/>
    </xsl:variable>
    <xsl:apply-templates/>
    <xsl:if test="not(following-sibling::tei:monogr/tei:title[@level='m']) and $refId!=''">
      <xsl:text> </xsl:text>
      <xsl:if test="following-sibling::tei:monogr/tei:imprint/tei:date">
	<xsl:value-of select="following-sibling::tei:monogr/tei:imprint/tei:date"/>
	<xsl:text>. </xsl:text>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[1]">
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[1], ',')"/>
	</xsl:when>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][1]">
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][1], ',')"/>
	</xsl:when>
      </xsl:choose>
      <xsl:choose>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[3]">
	  <xsl:text>, </xsl:text>
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[2], ',')"/>
	  <xsl:text>, and </xsl:text>
	</xsl:when>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][3]">
	  <xsl:text>, </xsl:text>
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][2], ',')"/>
	  <xsl:text>, and </xsl:text>
	</xsl:when>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[2]">
	  <xsl:text> and </xsl:text>
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[2], ',')"/>
	</xsl:when>
	<xsl:when test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][2]">
	  <xsl:text> and </xsl:text>
	  <xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][2], ',')"/>
	</xsl:when>
      </xsl:choose>
      <xsl:if test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[3]">
	<xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:author[3], ',')"/>
      </xsl:if>
      <xsl:if test="ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][3]">
	<xsl:value-of select="substring-before(ancestor::tei:listBibl/tei:biblStruct[@xml:id=$refId]/tei:monogr/tei:editor[@role='editor'][3], ',')"/>
      </xsl:if>
      <xsl:text> </xsl:text>
      <xsl:value-of select="following-sibling::tei:monogr/tei:imprint/tei:biblScope[@type='pp']"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] processing monogr element as needed for MLA style (from Laura Mandell> </desc>
  </doc>
  <xsl:template match="tei:monogr" mode="mla">
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:analytic">
	<xsl:choose>
	  <xsl:when test="tei:author = parent::tei:biblStruct/tei:analytic/tei:author">
	    <xsl:if test="tei:author[2]">
	      <xsl:apply-templates select="tei:author"/>
	    </xsl:if>
	    <xsl:apply-templates select="tei:title"/>
	    <xsl:if test="tei:edition"><xsl:apply-templates select="tei:edition"/></xsl:if>
	    <xsl:apply-templates select="tei:editor[@role='editor']"/>
	    <xsl:if test="tei:editor[@role='translator']">
	      <xsl:apply-templates select="tei:editor[@role='translator']"/>
	    </xsl:if>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="tei:author"/>
	    <xsl:apply-templates select="tei:title"/>
	    <xsl:if test="tei:edition"><xsl:apply-templates select="tei:edition"/></xsl:if>
	    <xsl:apply-templates select="tei:editor[@role='editor']"/>
	    <xsl:if test="tei:editor[@role='translator']">
	      <xsl:apply-templates select="tei:editor[@role='translator']"/>
	    </xsl:if>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="tei:editor[@role='editor'] and not(preceding-sibling::tei:analytic) and not(tei:author)">
	<xsl:apply-templates select="tei:editor[@role='editor']"/>
	<xsl:apply-templates select="tei:title"/>
	<xsl:if test="tei:edition"><xsl:apply-templates select="tei:edition"/></xsl:if>
	<xsl:if test="tei:editor[@role='translator']">
	  <xsl:apply-templates select="tei:editor[@role='translator']"/>
	</xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="tei:author"/>
	<xsl:apply-templates select="tei:title"/>
	<xsl:if test="tei:edition"><xsl:apply-templates select="tei:edition"/></xsl:if>
	<xsl:apply-templates select="tei:editor[@role='editor']"/>
	<xsl:if test="tei:editor[@role='translator']">
	  <xsl:apply-templates select="tei:editor[@role='translator']"/>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="*//tei:ref/@target and not(contains(*//tei:ref/@target, '#'))">
	<xsl:if test="tei:imprint/tei:date[@type='update']"><xsl:value-of select="tei:imprint/tei:date[@type='update']"/></xsl:if>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:biblStruct/*/tei:title/@level='u'">
	<xsl:value-of select="tei:imprint"/>
      </xsl:when>
      <xsl:when test="tei:title/@level='m'">
	<xsl:if test="tei:imprint/tei:biblScope/@type='vol'">
	<xsl:value-of select="tei:imprint/tei:biblScope[@type='vol']"/>. </xsl:if>
	<xsl:choose>
	  <xsl:when test="tei:imprint/tei:pubPlace"><xsl:value-of select="tei:imprint/tei:pubPlace"/>: </xsl:when>
	  <xsl:otherwise>[n.p.]: </xsl:otherwise>
	</xsl:choose>
	<xsl:choose>
	  <xsl:when test="tei:imprint/tei:publisher"><xsl:value-of select="tei:imprint/tei:publisher"/>, </xsl:when>
	  <xsl:otherwise>[n.p.], </xsl:otherwise>
	</xsl:choose>
	<xsl:choose>
	  <xsl:when test="tei:imprint/tei:date"><xsl:value-of select="tei:imprint/tei:date"/>. </xsl:when>
	  <xsl:otherwise>[n.d.]  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="tei:title/@level='j'">
	<xsl:if test="tei:imprint/tei:biblScope/@type='vol'"><xsl:value-of select="tei:imprint/tei:biblScope[@type='vol']"/></xsl:if>
	<xsl:if test="tei:imprint/tei:biblScope/@type='no'"><xsl:text>.</xsl:text><xsl:value-of select="tei:imprint/tei:biblScope[@type='no']"/></xsl:if>
	<xsl:if test="tei:imprint/tei:date"><xsl:text>&#10;</xsl:text>(<xsl:value-of select="tei:imprint/tei:date"/>)</xsl:if>
	<xsl:if test="tei:imprint/tei:biblScope/@type='pp'">: <xsl:value-of select="tei:imprint/tei:biblScope[@type='pp']"/></xsl:if>
	<xsl:text>. </xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] processing relatedItem element as needed for MLA style (from Laura Mandell> </desc>
  </doc>
  
  <xsl:template match="tei:relatedItem" mode="mla">
    <xsl:if test="@type='otherEdition'"><xsl:text>Rpt. </xsl:text></xsl:if>
    <xsl:if test="tei:biblStruct/tei:analytic"><xsl:apply-templates select="tei:biblStruct/tei:analytic" mode="mla"/></xsl:if>
    <xsl:if test="tei:biblStruct/tei:monogr"><xsl:apply-templates select="tei:biblStruct/tei:monogr" mode="mla"/></xsl:if>
  </xsl:template>

</xsl:stylesheet>
<!--
      <xsl:when test="$value='quoted'">
        <xsl:text>‘</xsl:text>
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
        <xsl:text>’</xsl:text>
      </xsl:when>
      <xsl:when test="$value='interlinMarks'">
        <xsl:text>`</xsl:text>
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
        <xsl:text>´</xsl:text>
      </xsl:when>
-->
<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"                  
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="#all"
                version="2.0">
  <xsl:import href="../common/common.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:import href="html_param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making HTML output.
      </p>
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
  <xsl:include href="html_core.xsl"/>
  <xsl:include href="html_corpus.xsl"/>
  <xsl:include href="html_dictionaries.xsl"/>
  <xsl:include href="html_drama.xsl"/>
  <xsl:include href="html_figures.xsl"/>
  <xsl:include href="html_header.xsl"/>
  <xsl:include href="html_linking.xsl"/>
  <xsl:include href="html_namesdates.xsl"/>
  <xsl:include href="html_nets.xsl"/>
  <xsl:include href="html_tagdocs.xsl"/>
  <xsl:include href="html_textstructure.xsl"/>
  <xsl:include href="html_textcrit.xsl"/>
  <xsl:include href="html_transcr.xsl"/>
  <xsl:include href="html_verse.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>Stylesheet constant setting the name of the main output file.</desc>
   </doc>

  <xsl:variable name="masterFile">
      <xsl:choose>
         <xsl:when test="not($outputName ='')">
            <xsl:choose>
               <xsl:when test="$STDOUT='true'">
                  <xsl:value-of select="$outputName"/>
               </xsl:when>
               <xsl:when test="contains($outputName,'.xml')">
                  <xsl:value-of select="substring-before($outputName,'.xml')"/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="$outputName"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:when test="contains($REQUEST,'.ID=')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="substring-before($REQUEST,'.ID=')"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="not($REQUEST='')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="$REQUEST"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$STDOUT='true'">
            <xsl:text>index.xml</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>index</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] How to work out the filename component of a path<param name="file">filename</param>
      </desc>
   </doc>
  <xsl:template name="get-basename">
      <xsl:param name="file"/>
      <xsl:choose>
         <xsl:when test="contains($file,'/')">
            <xsl:call-template name="get-basename">
               <xsl:with-param name="file">
                  <xsl:value-of select="substring-after($file,'/')"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="$STDOUT='true'">
                  <xsl:value-of select="$file"/>
               </xsl:when>
               <xsl:when test="contains($file,'.xml')">
                  <xsl:value-of select="substring-before($file,'.xml')"/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="$file"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="bodyMicroData"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Work out language code </desc>
   </doc>
  <xsl:template name="makeLang">
    <xsl:if test="@xml:lang">
      <xsl:attribute name="lang" select="@xml:lang"/>
    </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Work out rendition. In order of precedence, we first
    look at @rend; if that does not exist, @rendition and @style values are
    merged together; if neither of those exist, we look at default
    renditions in tagUsage; if <value>default</value> is set to false,
    we do nothing; if <value>default</value> has a value, use that for
    @class; otherwise, use the element name as a value for @class.</desc>
  </doc>
  <xsl:template name="makeRendition">
    <xsl:param name="default"/>
    <xsl:param name="auto"/>
    <xsl:call-template name="makeLang"/>
    <xsl:choose>
      <xsl:when
        test="
          (self::tei:q or self::tei:said or
          self::tei:quote) and (tei:match(@rend, 'inline') or
          tei:match(@rend, 'display')) and
          not(@rendition) and not(key('TAGREND', local-name(.)))">
        <xsl:sequence select="tei:processClass(local-name(), '')"/>
      </xsl:when>
      <xsl:when test="@rend">
        <xsl:sequence select="tei:processRend(@rend, $auto, .)"/>
      </xsl:when>
      <xsl:when test="@rendition or @style">
        <xsl:for-each select="@rendition">
          <xsl:sequence select="tei:processRendition(., $auto)"/>
        </xsl:for-each>
        <xsl:for-each select="@style">
          <xsl:sequence select="tei:processStyle(.)"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="key('TAGREND', local-name(.))">
        <xsl:for-each select="key('TAGREND', local-name(.))">
          <xsl:sequence select="tei:processRendition(@render, $auto)"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="$default = 'false'"/>
      <xsl:when test="not($default = '')">
        <xsl:sequence select="tei:processClass($default, $auto)"/>
      </xsl:when>
      <xsl:when test="parent::tei:item/parent::tei:list[@rend]">
        <xsl:sequence select="tei:processClass(parent::tei:item/parent::tei:list/@rend, $auto)"/>
      </xsl:when>
      <xsl:when test="parent::tei:item[@rend]">
        <xsl:sequence select="tei:processClass(parent::tei:item/@rend, $auto)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="tei:processClass(local-name(), '')"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$outputTarget = 'html5'">
      <xsl:call-template name="microdata"/>
    </xsl:if>
  </xsl:template>



  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Look up rendition value <param
    name="value">value</param>. They are either turned directly into
    @class values, or by following the link to an external document
    and using the @xml:id from there to make a class.</desc>
  </doc>
  <xsl:function name="tei:processRendition" as="node()*">
    <xsl:param name="value"/>
    <xsl:param name="auto"/>
    <xsl:attribute name="class">
      <xsl:if test="not($auto='')">
	<xsl:value-of select="$auto"/>
	<xsl:text> </xsl:text>
      </xsl:if>      
      <xsl:variable name="values">
	<xsl:for-each select="tokenize(normalize-space($value),' ')">
	  <xsl:choose>
	    <xsl:when test="starts-with(.,'#')">
	      <xsl:sequence select="substring-after(.,'#')"/>
	    </xsl:when>
	    <xsl:when test="starts-with(.,'simple:')">
	      <xsl:value-of select="substring(.,8)"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:for-each select="document(.)">
		<xsl:sequence select="@xml:id"/>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:variable>
      <xsl:value-of select="string-join($values,' ')"/>
    </xsl:attribute>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Look up @rend value. We recognize some simple cases, and
    turn them into an "s" element, otherwise we use the @rend to make
    a "c" element. The "s" and "c" are processed into appropriate
    @class and @style attributes at the end.  The special cases for super and
    subscript have been dealt with separately, so ignore them.</desc>
  </doc>
  <xsl:function name="tei:processRend" as="node()*">
    <xsl:param name="value"/>
    <xsl:param name="auto"/>
    <xsl:param name="context"/>
    <xsl:variable name="values">
      <values xmlns="">
	<xsl:if test="not($auto='')">	    
	  <c><xsl:value-of select="$auto"/></c>
	</xsl:if>
	<xsl:for-each select="tokenize($context/@style,';')">
	  <s><xsl:value-of select="."/></s>
	</xsl:for-each>
	<xsl:for-each select="tokenize(normalize-space($value),' ')">
	  <xsl:choose>
	    <xsl:when test=".='calligraphic' or .='cursive'"><s>font-family:cursive</s></xsl:when>
	    <xsl:when test=".='center' or .='centered' or .='centred'"><s>text-align: center</s></xsl:when>
	    <xsl:when test=".='expanded'"><s>letter-spacing: 0.15em</s></xsl:when>
	    <xsl:when test=".='gothic'"><s>font-family: Papyrus, fantasy</s></xsl:when>
	    <xsl:when test=".='indent'"><s>text-indent: 5em</s></xsl:when>
	    <xsl:when test=".='large'"><s>font-size: 130%</s></xsl:when>
	    <xsl:when test=".='larger'"><s>font-size: 200%</s></xsl:when>
	    <xsl:when test=".='overbar'"><s>text-decoration:overline</s></xsl:when>
	    <xsl:when test=".='plain'"/>
	    <xsl:when test=".='ro' or .='roman'"><s>font-style: normal</s></xsl:when>
	    <xsl:when test=".='sc' or .='smcap' or .='smallcaps'"><s>font-variant: small-caps</s></xsl:when>
	    <xsl:when test=".='small'"><s>font-size: 75%</s></xsl:when>
	    <xsl:when test=".='smaller'"><s>font-size: 50%</s></xsl:when>
	    <xsl:when test=".='spaced'"><s>letter-spacing: 0.15em</s></xsl:when>
	    <xsl:when test=".='sub' or .='sup' or .='code' or	.='superscript' or .='subscript'"/>
	    <xsl:when test="starts-with(.,'background(')"><s><xsl:text>background-color:</xsl:text><xsl:value-of select="substring-before(substring-after(.,'('),')')"/></s></xsl:when>
	    <xsl:when test="starts-with(.,'align(')"><s><xsl:text>text-align:</xsl:text><xsl:value-of select="substring-before(substring-after(.,'('),')')"/></s></xsl:when>
	    <xsl:when test="starts-with(.,'color(')"><s><xsl:text>color:</xsl:text><xsl:value-of select="substring-before(substring-after(.,'('),')')"/></s></xsl:when>
	    <xsl:when test="starts-with(.,'post(')"><xsl:message terminate="yes">no support for post() pattern in @rend</xsl:message></xsl:when>
	    <xsl:when test="starts-with(.,'pre(')"><xsl:message   terminate="yes">no support for pre() pattern in  @rend</xsl:message></xsl:when>
	    <xsl:when test=".='bold' or . ='b'"><s>font-weight:bold</s></xsl:when>
	    <xsl:when test=".='italic' or .='i'"><s>font-style:italic</s></xsl:when>
	    <xsl:when test=".='strikethrough' or .='strike'"><s>text-decoration: line-through</s></xsl:when>
	    <xsl:when test=".='underline'"><s>text-decoration:underline</s></xsl:when>
	    <xsl:otherwise><c><xsl:value-of select="."/></c></xsl:otherwise>
	  </xsl:choose>	  
	</xsl:for-each>
      </values>
    </xsl:variable>
    <xsl:if test="$values/values/c">
      <xsl:attribute name="class">
	<xsl:value-of select="$values/values/c" separator=' '/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="$values/values/s">
      <xsl:attribute name="style">
	<xsl:value-of select="$values/values/s" separator=';'/>
      </xsl:attribute>
    </xsl:if>    
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Process @style attribute. It is simply
    mapped to an HTML @style attribute. If it is not using CSS, that
    will fail.</desc>
  </doc>
  <xsl:function name="tei:processStyle" as="node()*">
    <xsl:param name="value"/>
    <xsl:attribute name="style">
      <xsl:value-of select="$value"/>
    </xsl:attribute>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Is given a TEI paragraph element and says what the HTML wrapper element
    should be (ie plain &lt;p&gt; or &lt;div class="p"&gt; if the
    content is complex).</desc>
  </doc>
  <xsl:function name="tei:isDivOrP" as="node()*">
    <xsl:param name="element"/>
    <xsl:for-each select="$element">
      <xsl:choose>
        <xsl:when test="tei:specList">div</xsl:when>
        <xsl:when test="parent::tei:note[@place='display']">div</xsl:when>
        <xsl:when test="parent::tei:note[@place='block']">div</xsl:when>
        <xsl:when test="parent::tei:figure and (tei:q/tei:l or tei:figure or parent::tei:figure/parent::tei:div)">div</xsl:when>
        <xsl:when test="ancestor::tei:notesStmt">div</xsl:when>
        <xsl:when test="tei:table">div</xsl:when>
        <xsl:when test="$outputTarget='epub' or $outputTarget='epub3'">div</xsl:when>
        <xsl:when test="tei:eg">div</xsl:when>
        <xsl:when test="tei:figure">div</xsl:when>
        <xsl:when test="tei:floatingText">div</xsl:when>
        <xsl:when test="tei:l">div</xsl:when>
        <xsl:when test="tei:list">div</xsl:when>
        <xsl:when test="tei:moduleSpec">div</xsl:when>
        <xsl:when test="tei:note[@place='display'  or @place='block' or tei:isMarginal(@place)]">div</xsl:when>
        <xsl:when test="tei:note[tei:q]">div</xsl:when>
        <xsl:when test="tei:q/tei:figure">div</xsl:when>
        <xsl:when test="tei:q/tei:list">div</xsl:when>
        <xsl:when test="tei:q[tei:match(@rend,'display')]">div</xsl:when>
        <xsl:when test="tei:q[tei:match(@rend,'inline') and tei:note/@place]">div</xsl:when>
        <xsl:when test="tei:q[tei:l]">div</xsl:when>
        <xsl:when test="tei:q[tei:lg]">div</xsl:when>
        <xsl:when test="tei:q[tei:p]">div</xsl:when>
        <xsl:when test="tei:q[tei:sp]">div</xsl:when>
        <xsl:when test="tei:q[tei:floatingText]">div</xsl:when>
        <xsl:when test="tei:quote[not(tei:isInline(.))]">div</xsl:when>
        <xsl:when test="tei:specGrp">div</xsl:when>
        <xsl:when test="tei:specGrpRef">div</xsl:when>
        <xsl:when test="tei:specList">div</xsl:when>
        <xsl:when test="tei:table">div</xsl:when>
        <xsl:when test="teix:egXML">div</xsl:when>
        <xsl:when test="ancestor::tei:floatingText">div</xsl:when>
        <xsl:when test="ancestor::tei:closer">div</xsl:when>
        <xsl:when test="parent::tei:p">div</xsl:when>
        <xsl:when test="parent::tei:q">div</xsl:when>
        <xsl:when test="parent::tei:note">div</xsl:when>
        <xsl:when test="parent::tei:remarks">div</xsl:when>
        <xsl:otherwise>
          <xsl:text>p</xsl:text>
        </xsl:otherwise>
      </xsl:choose>    
    </xsl:for-each>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Create a class attribute.</desc>
  </doc>
  <xsl:function name="tei:processClass" as="node()*">
    <xsl:param name="value"/>
    <xsl:param name="auto"/>
    <xsl:attribute name="class">
      <xsl:if test="not($auto='')">
	<xsl:value-of select="$auto"/>
	<xsl:text> </xsl:text>
      </xsl:if>
      <xsl:value-of select="$value"/>
    </xsl:attribute>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>what to do with an inline object</desc>
  </doc>
  <xsl:template name="makeInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <xsl:element name="{if ($style='sup') then 'sup' else if
      ($style='sub') then 'sub' else 'span'}">
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default" select="$style"/>
      </xsl:call-template>
      <xsl:if test="@xml:id">
	<xsl:attribute name="id">
	  <xsl:value-of select="@xml:id"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:value-of select="$before"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$after"/>
    </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>how to make a horizontal rule</desc>
  </doc>
  <xsl:template name="horizontalRule">
    <hr/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>what to do with a block-level object</desc>
  </doc>
  <xsl:template name="makeBlock">
    <xsl:param name="style"/>
    <xsl:element name="{if (tei:isInline(.)) then 'span' else 'div'}">
      <xsl:call-template name="microdata"/>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default" select="$style"/>
      </xsl:call-template>
      <xsl:if test="@xml:id">
	<xsl:attribute name="id">
	  <xsl:value-of select="@xml:id"/>
	</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Section-like object</desc>
  </doc>
    <xsl:template name="makeSection">
      <xsl:param name="level"/>
      <xsl:param name="heading"/>
      <xsl:param name="implicitBlock">false</xsl:param>
      <xsl:element name="h{$level + 2}">
	        <xsl:value-of select="$heading"/>
      </xsl:element>
      <xsl:choose>
	<xsl:when test="$implicitBlock='true'">
	  <p>
	    <xsl:apply-templates/>
	  </p>
	</xsl:when>
	<xsl:when test="*">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <p>
	    <xsl:apply-templates/>
	  </p>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <xsl:template name="makeWithLabel">
      <xsl:param name="before"/>
      <i>
         <xsl:value-of select="$before"/>
      </i>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process any element and work out a unique identifying string</desc>
   </doc>
  <xsl:template match="*" mode="ident">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    
    <xsl:choose>
      <xsl:when test="@xml:id and $useIDs='true'">
	<xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:when test="starts-with(local-name(.),'div') or
		      self::tei:text">
	<xsl:variable name="xpath">
	  <xsl:for-each select="ancestor-or-self::tei:*">
	    <xsl:value-of select="local-name()"/>
	    <xsl:text>.</xsl:text>
	    <xsl:number/>
	    <xsl:if test="not(position() = last())">
	      <xsl:text>_</xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:value-of select="$BaseFile"/>
	<xsl:text>-</xsl:text>
	    <xsl:value-of
		select="substring-after(substring-after($xpath,'_text.'),'_')"/>
      </xsl:when>
      <xsl:when test="self::tei:TEI and parent::tei:teiCorpus">
	<xsl:value-of select="$masterFile"/>
	<xsl:call-template name="addCorpusID"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:sequence select="concat($BaseFile,'-',local-name(),'-',generate-id())"/>
      </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process any element, trying to make the right hyperlink
      for it</desc>
   </doc>
  <xsl:template match="*" mode="generateLink">
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <xsl:variable name="depth">
      <xsl:apply-templates mode="depth" select="."/>
    </xsl:variable>
    <xsl:variable name="keep" select="tei:keepDivOnPage(.)"/>
    <xsl:variable name="LINK">
      <xsl:choose>
        <xsl:when test="$filePerPage='true'">
          <xsl:choose>
            <xsl:when test="preceding::tei:pb">
              <xsl:apply-templates select="preceding::tei:pb[1]"
                mode="ident"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>index</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="$standardSuffix"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:elementSpec and not($STDOUT='true')">
          <xsl:sequence select="concat('ref-',ancestor::tei:elementSpec/@ident,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:classSpec and not($STDOUT='true')">
          <xsl:sequence select="concat('ref-',ancestor::tei:classSpec/@ident,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:dataSpec and not($STDOUT='true')">
          <xsl:sequence select="concat('ref-',ancestor::tei:dataSpec/@ident,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:macroSpec and not($STDOUT='true')">
          <xsl:sequence select="concat('ref-',ancestor::tei:macroSpec/@ident,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="not ($STDOUT='true') and ancestor::tei:back and not($splitBackmatter='true')">
          <xsl:value-of select="concat($masterFile,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="not($STDOUT='true') and ancestor::tei:front
          and not($splitFrontmatter='true')">
          <xsl:value-of select="concat($masterFile,$standardSuffix,'#',$ident)"/>
        </xsl:when>
        <xsl:when test="not($keep) and $STDOUT='true' and
          number($depth) &lt;= number($splitLevel)">
          <xsl:sequence select="concat($masterFile,$standardSuffix,$urlChunkPrefix,$ident)"/>
        </xsl:when>
        <xsl:when test="self::tei:text and $splitLevel=0">
          <xsl:value-of select="concat($ident,$standardSuffix)"/>
        </xsl:when>
        <xsl:when test="number($splitLevel)= -1 and
          ancestor::tei:teiCorpus">
          <xsl:value-of select="$masterFile"/>
          <xsl:call-template name="addCorpusID"/>
          <xsl:value-of select="$standardSuffix"/>
          <xsl:value-of select="concat('#',$ident)"/>
        </xsl:when>
        <xsl:when test="number($splitLevel)= -1">
          <xsl:value-of select="concat('#',$ident)"/>
        </xsl:when>
        <xsl:when test="number($depth) &lt;= number($splitLevel) and not($keep)">
          <xsl:value-of select="concat($ident,$standardSuffix)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="parent">
            <xsl:call-template name="locateParentDiv"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$STDOUT='true'">
              <xsl:sequence select="concat($masterFile,$urlChunkPrefix,$parent,'#',$ident)"/>
            </xsl:when>
            <xsl:when test="ancestor::tei:group">
              <xsl:sequence select="concat($parent,$standardSuffix,'#',$ident)"/>
            </xsl:when>
            <xsl:when test="ancestor::tei:floatingText">
              <xsl:sequence select="concat($parent,$standardSuffix,'#',$ident)"/>
            </xsl:when>
            <xsl:when test="$keep and number($depth=0)">
              <xsl:sequence select="concat('#',$ident)"/>
            </xsl:when>
            <xsl:when test="$keep">
              <xsl:sequence select="concat($masterFile,$standardSuffix,'#',$ident)"/>
            </xsl:when>
            <xsl:when test="ancestor::tei:div and tei:keepDivOnPage(ancestor::tei:div[last()])">
              <xsl:sequence select="concat('#',$ident)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:sequence select="concat($parent,$standardSuffix,'#',$ident)"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <!--
      <xsl:message>GENERATELINK <xsl:value-of
      select="(name(),$ident,$depth,string($keep),$LINK)"
      separator="|"/></xsl:message>
    -->
    <xsl:value-of select="$LINK"/>
    
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element TEI in generateLink mode</p>
	 <p xmlns="http://www.w3.org/1999/xhtml"> when a &lt;div&gt; is referenced, see whether its plain
        anchor, or needs a parent HTML name prepended </p>
      </desc>
   </doc>
  <xsl:template match="tei:TEI" mode="generateLink">
      <xsl:variable name="BaseFile">
	<xsl:apply-templates select="." mode="ident"/>
      </xsl:variable>
      <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process elements tei:* in toc mode</p>
      <p xmlns="http://www.w3.org/1999/xhtml"> anything with a head can go in the TOC </p>
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
      <desc>Process extra elements in generateLink mode</desc>
   </doc>
  <xsl:template match="tei:label|tei:figure|tei:table|tei:item|tei:p|tei:title|tei:bibl|tei:anchor|tei:cell|tei:lg|tei:list|tei:sp"
                 mode="generateLink">
      <xsl:variable name="ident">
         <xsl:apply-templates mode="ident" select="."/>
      </xsl:variable>
      <xsl:variable name="file">
	<xsl:choose>
	  <xsl:when test="ancestor::tei:floatingText">
	    <xsl:apply-templates mode="generateLink"
				 select="ancestor::tei:floatingText/ancestor::tei:*[starts-with(local-name(),'div')][1]"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates mode="generateLink"
				 select="ancestor::tei:*[starts-with(local-name(),'div')][1]"/>
	  </xsl:otherwise>
	</xsl:choose>
	</xsl:variable>

      <xsl:choose>
         <xsl:when test="starts-with($file,'#')">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:when>
         <xsl:when test="contains($file,'#')">
            <xsl:value-of select="substring-before($file,'#')"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$file"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element note</desc>
   </doc>
  <xsl:template match="tei:note" mode="generateLink">
    <xsl:variable name="file">
      <xsl:apply-templates mode="generateLink"
			   select="ancestor::tei:*[starts-with(local-name(),'div')][1]"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="starts-with($file,'#')"/>
      <xsl:when test="contains($file,'#')">
            <xsl:value-of select="substring-before($file,'#')"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$file"/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>#</xsl:text>
      <xsl:call-template name="noteID"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] <param name="where">where</param>
      </desc>
   </doc>
  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:choose>
	<xsl:when test="id($where)">
	  <xsl:apply-templates mode="generateLink" select="id($where)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>[[undefined </xsl:text>
	  <xsl:value-of select="$where"/>
	  <xsl:text>]]</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] Find the name of the outermost container for the
      current object which would create an output file</desc>
   </doc>
  <xsl:template name="locateParentDiv">
    
    <xsl:choose>
      
      <xsl:when
        test="ancestor-or-self::tei:body/parent::tei:text/ancestor::tei:group">
        <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
      </xsl:when>
      
      <xsl:when test="ancestor-or-self::tei:front/parent::tei:text/ancestor::tei:group">
        <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
      </xsl:when>
      
      <xsl:when test="ancestor-or-self::tei:back/parent::tei:text/ancestor::tei:group">
        <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
      </xsl:when>
      
      <xsl:when test="ancestor-or-self::tei:div and number($splitLevel) &lt; 0">
        <xsl:apply-templates mode="ident" select="ancestor::tei:div[last()]"/>
      </xsl:when>
      
      <xsl:when test="ancestor-or-self::tei:div">
        <xsl:variable name="ancestors" select="count(ancestor-or-self::tei:div)"/>
        <xsl:variable name="diff" select="$ancestors - number($splitLevel)"/>
        <xsl:variable name="what" select="if ($diff &lt;= 1) then 1
          else $diff "/>
        <xsl:apply-templates mode="ident" select="ancestor-or-self::tei:div[$what]"/>
      </xsl:when>
      
      <xsl:otherwise>
        <xsl:variable name="ancestors" select="count(ancestor::tei:*[local-name()='div1'
          or local-name()='div2'
          or local-name()='div3'
          or local-name()='div4'
          or local-name()='div5'
          or local-name()='div6'])"/>
        <xsl:variable name="what"
          select="if
          ($ancestors &lt; number($splitLevel)) then 1 else
          $ancestors - number($splitLevel) +1"/>
        <xsl:apply-templates mode="ident"
          select="ancestor-or-self::tei:*[local-name()='div1'
          or local-name()='div2'
          or local-name()='div3'
          or local-name()='div4'
          or local-name()='div5'
          or local-name()='div6'][$what]"/>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process cross-ref to note</desc>
   </doc>
  <xsl:template match="tei:note" mode="xref">
      <xsl:number level="any"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>formatting of the number part of a header</desc>
   </doc>
  <xsl:template name="formatHeadingNumber">
      <xsl:param name="text"/>
      <xsl:param name="toc"/>
      <xsl:if test="not($text='')">
	<span class="headingNumber">
	  <xsl:copy-of select="$text"/>
	</span>
      </xsl:if>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>whether a div starts a new page</desc>
  </doc>
  <xsl:function name="tei:keepDivOnPage" as="xs:boolean">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:choose>
	<!-- 4. we are part of an inner text -->
	<xsl:when test="ancestor::tei:floatingText">true</xsl:when>
	<!-- 3. we have special rendering on the document -->
	<xsl:when test="ancestor::tei:TEI/tei:match(@rend,'all') 
			or ancestor::tei:TEI/tei:match(@rend,'frontpage') 
			or ancestor::tei:TEI/tei:match(@rend,'nosplit')">true</xsl:when>
	<!-- 2. we are a singleton -->
	<xsl:when test="parent::tei:body[count(*)=1] and not(tei:div or
			tei:div2)">true</xsl:when>
	<!-- 1. we have no proceding sections at top level -->
	<xsl:when test="not(ancestor::tei:group) and parent::tei:body and
			not(parent::tei:body/preceding-sibling::tei:front)
			and not	(preceding-sibling::*)">true</xsl:when>
	<!-- 0. we are down the hierarchy -->
	<xsl:when test="tei:match(@rend,'nosplit')">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:function>


</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"                  
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
		xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="html a fo rng tei teix teidocx"
                version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making HTML output.
      </p>
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
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="dictionaries.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xsl:include href="../common2/verbatim.xsl"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
Stylesheet constant setting the name of the main output file.
</desc>
   </doc>
  <xsl:variable name="top" select="/"/>
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
    <xsl:choose>
      <xsl:when test="@rend">
	<xsl:sequence select="tei:processRend(@rend,$auto)"/>
      </xsl:when>
      <xsl:when test="@rendition or @style">
	<xsl:for-each select="@rendition">	  
	  <xsl:sequence select="tei:processRendition(.,$auto)"/>
	</xsl:for-each>
	<xsl:for-each select="@style">
	  <xsl:sequence select="tei:processStyle(.)"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="key('TAGREND',local-name(.))">
	<xsl:for-each select="key('TAGREND',local-name(.))">
	  <xsl:sequence select="tei:processRendition(@render,$auto)"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="$default='false'"/>
      <xsl:when test="not($default='')">
	  <xsl:sequence select="tei:processClass($default,$auto)"/>
      </xsl:when>
      <xsl:when test="parent::tei:item/parent::tei:list[@rend]">
	  <xsl:sequence select="tei:processClass(parent::tei:item/parent::tei:list/@rend,$auto)"/>
      </xsl:when>
      <xsl:when test="parent::tei:item[@rend]">
	  <xsl:sequence select="tei:processClass(parent::tei:item/@rend,$auto)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:sequence select="tei:processClass(local-name(),'')"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$outputTarget='html5'">
      <xsl:call-template name="microdata"/>
    </xsl:if>  
  </xsl:template>



  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Look up rendition value <param
    name="value">value</param>. They are either turned directly into
    @class values, or by following the link to an external document
    and using the @xml:id from there to make a class.
      </desc>
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
	      <xsl:value-of select="substring-after(.,'#')"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:for-each select="document(.)">
		<xsl:apply-templates select="@xml:id"/>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text> </xsl:text>
	</xsl:for-each>
      </xsl:variable>
      <xsl:value-of select="normalize-space($values)"/>
    </xsl:attribute>
  </xsl:function>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] Look up @rend value. We recognize some simple cases, and
    turn them into an "s" element, otherwise we use the @rend to make
    a "c" element. The "s" and "c" are processed into appropriate
    @class and @style attributes at the end.  The special cases for super and
    subscript have been dealt with separately, so ignore them.
      </desc>
  </doc>
  <xsl:function name="tei:processRend" as="node()*">
    <xsl:param name="value"/>
    <xsl:param name="auto"/>
    <xsl:variable name="values">
      <values xmlns="">
	<xsl:if test="not($auto='')">	    
	  <c><xsl:value-of select="$auto"/></c>
	</xsl:if>
	<xsl:for-each select="tokenize(normalize-space($value),' ')">
	  <xsl:choose>
	    <xsl:when test=".='bold' or .='bo'"><s>font-weight:bold</s></xsl:when>
	    <xsl:when test=".='calligraphic' or .='cursive'"><s>font-family:cursive</s></xsl:when>
	    <xsl:when test="starts-with(.,'color(')"><s>
	      <xsl:text>color:</xsl:text>
	      <xsl:value-of select="substring-before(substring-after(.,'('),')')"/>
	    </s>
	    </xsl:when>
	    <xsl:when test=".='center'"><s>text-align: center</s></xsl:when>
	    <xsl:when test=".='expanded'"><s>letter-spacing: 0.15em</s></xsl:when>
	    <xsl:when test=".='gothic'"><s>font-family: Papyrus, fantasy</s></xsl:when>
	    <xsl:when test=".='italics' or .='italic' or  .='cursive' or .='it' or .='ital'"><s>font-style: italic</s></xsl:when>
	    <xsl:when test=".='large'"><s>font-size: 150%</s></xsl:when>
	    <xsl:when test=".='larger'"><s>font-size: 200%</s></xsl:when>
	    <xsl:when test=".='overbar'"><s>text-decoration:overline</s></xsl:when>
	    <xsl:when test=".='ro' or .='roman'"><s>font-style: normal</s></xsl:when>
	    <xsl:when test=".='sc' or .='smcap' or .='smallcaps'"><s>font-variant: small-caps</s></xsl:when>
	    <xsl:when test=".='small'"><s>font-size: 75%</s></xsl:when>
	    <xsl:when test=".='smaller'"><s>font-size: 50%</s></xsl:when>
	    <xsl:when test=".='strike'"><s>text-decoration: line-through</s></xsl:when>
	    <xsl:when test=".='ul'"><s>text-decoration:underline</s></xsl:when>
	    <xsl:when test=".='underline'"><s>text-decoration:underline</s></xsl:when>
	    <xsl:when test=".='sub' or .='sup' or .='code' or	.='superscript' or .='subscript'"/>
	    <xsl:when test=".='plain'"/>
	    <xsl:otherwise><c><xsl:value-of select="."/></c></xsl:otherwise>
	  </xsl:choose>	  
	</xsl:for-each>
      </values>
    </xsl:variable>
    <xsl:if test="$values/values/c">
      <xsl:attribute name="class">
	<xsl:value-of select="$values/values/c"/>
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

  <xsl:template name="processInline">
    <xsl:param name="before"/>
    <xsl:param name="after"/>
    <xsl:param name="style"/>
    <span>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default" select="$style"/>
      </xsl:call-template>
      <xsl:value-of select="$before"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$after"/>
    </span>
  </xsl:template>

  <xsl:template name="processBlock">
    <xsl:param name="style"/>
    <div>
      <xsl:call-template name="makeRendition">
	<xsl:with-param name="default" select="$style"/>
      </xsl:call-template>
      <xsl:apply-templates/>
    </div>
  </xsl:template>

</xsl:stylesheet>

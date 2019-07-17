<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the textstructure
      module. </p>
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
      <desc>Establish nesting depth of sections</desc>
   </doc>
  <xsl:template match="tei:text" mode="depth">
    <xsl:value-of select="count(ancestor::tei:text)-1"/>
  </xsl:template>
  <xsl:template match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
                 mode="depth">
      <xsl:choose>
	<xsl:when test="ancestor::tei:text/parent::tei:group and
			self::tei:div">
	   <xsl:value-of select="count(ancestor::tei:div) + 1"/>
	</xsl:when>
         <xsl:when test="local-name(.) = 'div'">
            <xsl:value-of select="count(ancestor::tei:div)"/>
         </xsl:when>
	<xsl:when test="ancestor::tei:text/parent::tei:group">
	   <xsl:value-of select="number(substring-after(local-name(.),'div')) "/>
	</xsl:when>
	<xsl:when test="ancestor::tei:text/parent::tei:group">
	  <xsl:value-of select="number(substring-after(local-name(.),'div'))"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="number(substring-after(local-name(.),'div')) - 1"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Generate revision description</desc>
  </doc>
  <xsl:template match="tei:divGen[@type='revHist']">
    <xsl:variable name="r">
      <div xmlns="http://www.tei-c.org/ns/1.0" rend='nonumber'>
	<head>Revision history</head>
	<table rend="rules" >
	  <xsl:for-each
	      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change">
	    <row>
	      <cell><xsl:value-of select="@when"/></cell>
	      <cell><xsl:value-of select="@who"/></cell>
	      <cell><xsl:value-of select="."/></cell>
	    </row>
	  </xsl:for-each>
	</table>
      </div>
    </xsl:variable>
    <xsl:for-each select="$r">
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="/tei:text" priority="999">
    <xsl:call-template name="wrapRootText"/>
  </xsl:template>

  <xsl:template name="wrapRootText">
    <xsl:variable name="complete">
      <TEI  xmlns="http://www.tei-c.org/ns/1.0">
	<teiHeader>
	  <fileDesc>
	    <titleStmt>
	      <title>Unknown title</title>
	    </titleStmt>
	    <publicationStmt>
	      <p/>
	    </publicationStmt>
	    <sourceDesc>
	      <p/>
	    </sourceDesc>
	  </fileDesc>
	</teiHeader>
	<text>
	  <xsl:apply-templates mode="tite"/>
	</text>
      </TEI>
    </xsl:variable>
    <xsl:apply-templates select="$complete"/>
  </xsl:template>


  <xsl:template match="tei:titlePage">
    <xsl:call-template name="makeBlock">
      <xsl:with-param name="style">titlePage</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:titlePart" mode="simple">
    <xsl:if test="preceding-sibling::tei:titlePart">
      <xsl:call-template name="makeText">
	<xsl:with-param name="letters"> — </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="tei:titlePart">
    <xsl:call-template name="makeBlock">
      <xsl:with-param name="style">titlePart</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:titlePage/tei:byline">
    <xsl:call-template name="makeBlock">
      <xsl:with-param name="style">byline</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template
      match="tei:docTitle|tei:docAuthor|tei:docImprint|tei:docDate">
    <xsl:choose>
      <xsl:when test="parent::tei:titlePage">
	<xsl:call-template name="makeBlock">
	  <xsl:with-param name="style" select="local-name()"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style" select="local-name()"/>
	</xsl:call-template>
	<xsl:if
	    test="local-name(following-sibling::tei:*[1])=local-name(.)">
	  <xsl:choose>
	    <xsl:when test="self::tei:docAuthor">
	      <xsl:choose>
		<xsl:when test="count(following-sibling::tei:docAuthor)=1"> and </xsl:when>
		<xsl:when test="following-sibling::tei:docAuthor">, </xsl:when>
	      </xsl:choose>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:call-template name="makeText">
		<xsl:with-param name="letters"> — </xsl:with-param>
	      </xsl:call-template>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:titlePart[@type='running']"/>


  
</xsl:stylesheet>

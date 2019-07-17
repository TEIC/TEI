<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the header module. </p>
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
    <desc> Omit docAuthor found outside front matter</desc>
  </doc>
  <xsl:template match="tei:div/tei:docAuthor"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> Omit docDate if found outside front matter</desc>
  </doc>
  <xsl:template match="tei:div/tei:docDate"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Ignore docTitle in a div</desc>
  </doc>
  <xsl:template match="tei:div/tei:docTitle"/>

  <xsl:template match="tei:docAuthor" mode="heading">
    <xsl:if test="preceding-sibling::tei:docAuthor">
      <xsl:choose>
        <xsl:when test="not(following-sibling::tei:docAuthor)">
          <xsl:text> and </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>, </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:idno[ lower-case( @type ) = ('url','uri') ]">
    <xsl:text> &lt;</xsl:text>
    <xsl:call-template name="makeExternalLink">
      <xsl:with-param name="ptr" select="true()"/>
      <xsl:with-param name="dest">
        <xsl:value-of select="normalize-space(.)"/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:text>&gt;.</xsl:text>
  </xsl:template>

  <xsl:template match="tei:idno">
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:idno[ lower-case( @type ) = 'doi']">
    <xsl:text> </xsl:text>
    <xsl:call-template name="makeExternalLink">
      <xsl:with-param name="ptr" select="false()"/>
      <xsl:with-param name="dest">
        <xsl:value-of select="concat( $DOIResolver, normalize-space(.) )"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>

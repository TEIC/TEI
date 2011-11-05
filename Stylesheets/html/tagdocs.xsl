<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet  xmlns:s="http://www.ascc.net/xml/schematron" xmlns="http://www.w3.org/1999/xhtml" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="s html a fo rng tei teix" version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the tagdocs module,
      making HTML output. </p>
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
    <desc>Process elements teix:egXML</desc>
  </doc>
  <xsl:template match="teix:egXML">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"/>
    <div>
      <xsl:attribute name="id">
        <xsl:apply-templates mode="ident" select="."/>
      </xsl:attribute>
      <xsl:attribute name="class">
	<xsl:text>pre</xsl:text>
	<xsl:if test="not(*)">
	  <xsl:text> cdata</xsl:text>
	</xsl:if>
	<xsl:choose>
	  <xsl:when test="@valid='feasible'">
	    <xsl:text> egXML_feasible</xsl:text>
	  </xsl:when>
	  <xsl:when test="@valid='false'">
	    <xsl:text> egXML_invalid</xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text> egXML_valid</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="$simple='true'">
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="egXMLStartHook"/>
          <xsl:apply-templates mode="verbatim">
            <xsl:with-param name="highlight">
              <xsl:value-of select="$highlight"/>
            </xsl:with-param>
          </xsl:apply-templates>
          <xsl:call-template name="egXMLEndHook"/>
        </xsl:otherwise>
      </xsl:choose>
    </div>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element ident</desc>
  </doc>
  <xsl:template match="tei:ident">
    <xsl:choose>
      <xsl:when test="@type">
        <span class="ident-{@type}">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="ident">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element gi</desc>
  </doc>
  <xsl:template match="tei:gi">
    <span class="gi">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </span>
  </xsl:template>
</xsl:stylesheet>

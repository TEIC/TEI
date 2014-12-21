<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    exclude-result-prefixes="tei xd"
    version="1.0">

<xd:doc type="stylesheet">
    <xd:short>
      TEI stylesheet for doing neat (but not indented) verbatim layout of XSL fragments
      </xd:short>
    <xd:detail>This software is dual-licensed:

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
</xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>

<xsl:template match="text()" mode="verbatim">
    <xsl:value-of select="normalize-space(.)"/>
</xsl:template>

<xsl:template match="*" mode="verbatim">
<xsl:text>&#10;&lt;</xsl:text>
<xsl:if test="namespace-uri(.)='http://www.w3.org/1999/XSL/Transform'">xsl:</xsl:if>
<xsl:value-of select="local-name()"/>
<xsl:for-each select="@*">
  <xsl:text>&#10; </xsl:text>
  <xsl:value-of select="normalize-space(name(.))"/>
  <xsl:text>="</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>"</xsl:text>
</xsl:for-each>
<xsl:if test="not(*|text())">/</xsl:if>
<xsl:text>&gt;</xsl:text>
<xsl:if test="*|text()">
  <xsl:apply-templates select="*|text()" mode="verbatim"/>
  <xsl:text>&lt;/</xsl:text>
<xsl:if test="namespace-uri(.)='http://www.w3.org/1999/XSL/Transform'">xsl:</xsl:if>
  <xsl:value-of select="local-name(.)"/>
  <xsl:text>&gt;</xsl:text>
</xsl:if>
</xsl:template>
</xsl:stylesheet>
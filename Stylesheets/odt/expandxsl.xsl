<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
    xpath-default-namespace="http://www.w3.org/1999/XSL/Transform" 
    xmlns:doc="http://www.oxygenxml.com/ns/doc/xsl"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0"
>

<!--This software is dual-licensed:

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

$Id$

2008, TEI Consortium
-->

<xsl:template match="import">
  <xsl:message>expand import of <xsl:value-of select="@href"/></xsl:message>
  <xsl:for-each select="doc(resolve-uri(@href,base-uri(/)))/stylesheet">
    <xsl:apply-templates/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="doc:*"/>

<xsl:template match="stylesheet">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates/>
</xsl:copy>
</xsl:template>

<xsl:template match="text()|@*|comment()">
  <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>

<xsl:template match="*" mode="import">
  <xsl:copy>
    <xsl:attribute name="priority">-10</xsl:attribute>
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
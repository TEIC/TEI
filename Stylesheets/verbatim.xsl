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
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
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
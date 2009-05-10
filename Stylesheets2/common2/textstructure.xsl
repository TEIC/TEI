<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd tei"
    version="2.0"
  
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the textstructure
      module. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Establish nesting depth of sections</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template
    match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
    mode="depth">
    <xsl:choose>
      <xsl:when test="local-name(.) = 'div'">
        <xsl:value-of select="count(ancestor::tei:div)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="number(substring-after(local-name(.),'div')) - 1"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

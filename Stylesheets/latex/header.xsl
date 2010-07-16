<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd exsl estr edate a rng tei teix"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the header module,
      making LaTeX output. </xd:short>
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
    <xd:short>Process elements tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docAuthor"> 
    <xsl:if test="not(preceding-sibling::tei:docAuthor)">
      <xsl:text>\author{</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="count(following-sibling::tei:docAuthor)=1"> and </xsl:when>
      <xsl:when test="following-sibling::tei:docAuthor">, </xsl:when>
    </xsl:choose>
    <xsl:if test="not(following-sibling::tei:docAuthor)">
      <xsl:text>}</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:docAuthor" mode="author">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="count(following-sibling::tei:docAuthor)=1"> and </xsl:when>
      <xsl:when test="following-sibling::tei:docAuthor">, </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:docDate</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docDate">
  \date{<xsl:apply-templates/>}</xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:docImprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docImprint">
  </xsl:template>
</xsl:stylesheet>

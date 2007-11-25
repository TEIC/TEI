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
    <xd:short> TEI stylesheet dealing with elements from the tagdocs module,
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
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>

  <xd:doc>
    <xd:short>Example element</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="teix:egXML">
    <xsl:choose>
      <xsl:when test="parent::tei:cell">
	<xsl:text>\mbox{}\hfill\newline\bgroup\exampleFontSet\begin{shaded}</xsl:text>
	<xsl:apply-templates mode="verbatim"/>
	<xsl:text>\end{shaded}\egroup </xsl:text>
      </xsl:when>
      <xsl:otherwise>
      <xsl:text>\par\bgroup\exampleFontSet
\begin{shaded}\noindent\mbox{}</xsl:text>
<xsl:apply-templates mode="verbatim"/>
\end{shaded}\egroup\par
<xsl:if test="parent::tei:p and following-sibling::node()">\noindent </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<xsl:template match="tei:seg[@rend='specChildren']">
<xsl:text>\mbox{ }\\ \begin{description}</xsl:text>
<xsl:apply-templates/>
<xsl:text>\end{description}</xsl:text>
</xsl:template>

<xsl:template match="tei:seg[@rend='specChild']">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:seg[@rend='specChildModule']">
    \item[<xsl:apply-templates/>]
</xsl:template>

<xsl:template match="tei:seg[@rend='specChildElements']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='parent']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers1']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers2']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers3']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers4']">
  <xsl:apply-templates/>
</xsl:template>

</xsl:stylesheet>

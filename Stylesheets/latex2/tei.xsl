<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet   
    xmlns:s="http://www.ascc.net/xml/schematron"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="s xd" 
    version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <xsl:import href="../common2/verbatim.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet for making LaTeX output.
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
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:output method="text" encoding="utf8"/>
  <xsl:strip-space elements="*"/>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xsl:include href="textcrit.xsl"/>

  <xsl:param name="startNamespace">\color{red}</xsl:param>
  <xsl:param name="startElement">{</xsl:param>
  <xsl:param name="highlightStartElementName">\textcolor{red}{</xsl:param>
  <xsl:param name="highlightEndElementName">}</xsl:param>
  <xsl:param name="startElementName">\textbf{</xsl:param>
  <xsl:param name="startAttribute">{</xsl:param>
  <xsl:param name="startAttributeValue">{</xsl:param>
  <xsl:param name="startComment">\textit{</xsl:param>
  <xsl:param name="endElement">}</xsl:param>
  <xsl:param name="endElementName">}</xsl:param>
  <xsl:param name="endComment">}</xsl:param>
  <xsl:param name="endAttribute">}</xsl:param>
  <xsl:param name="endAttributeValue">}</xsl:param>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter">\hspace*{1em}</xsl:param>
  <xsl:variable name="docClass">
    <xsl:choose>
      <xsl:when test="/tei:TEI[@rend='letter']">
        <xsl:text>letter</xsl:text>
      </xsl:when>
      <xsl:when test="/tei:TEI[@rend='book']">
        <xsl:text>book</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>article</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xd:doc>
    <xd:short>Process elements  processing-instruction()[name(.)='tex']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="processing-instruction()[name(.)='tex']">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template name="verbatim-lineBreak">
    <xsl:param name="id"/>
    <xsl:text>\mbox{}\newline &#10;</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-createElement">
    <xsl:param name="name"/>
    <xsl:param name="special"/>
    <xsl:text>\textbf{</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-newLine"/>

  <xsl:template name="verbatim-createAttribute">
    <xsl:param name="name"/>
    <xsl:value-of select="$name"/>
  </xsl:template>

  <xsl:template name="verbatim-Text">
    <xsl:param name="words"/>
    <xsl:value-of select="translate($words,'\{}','&#8421;&#10100;&#10101;')"/>
  </xsl:template>


</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:fotex="http://www.tug.org/fotex" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="xd tei fotex fo m" version="1.0">
  <xsl:import href="../common/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet making XSL-FO output.
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc type="string">
    Stylesheet constant for the input document.
  </xd:doc>
  <xsl:variable name="top" select="/"/>
  <xd:doc type="string">
    Stylesheet constant for table specifications
  </xd:doc>
  <xsl:variable name="tableSpecs">
    <xsl:choose>
      <xsl:when test="$readColSpecFile">
        <xsl:copy-of select="document($readColSpecFile,$top)/Info"/>
      </xsl:when>
      <xsl:otherwise>
        <Info/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:output indent="no" encoding="utf-8"/>
  <xsl:strip-space elements="tei:cell"/>
  <xsl:key name="DIVS" match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5" use="'1'"/>
  <xsl:include href="tei-makecolspec.xsl"/>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="verse.xsl"/>
</xsl:stylesheet>

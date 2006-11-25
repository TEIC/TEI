<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:fotex="http://www.tug.org/fotex" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="xd exsl estr edate" exclude-result-prefixes="xd exsl estr edate a fo rng tei teix" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet for making table specifications, making XSL-FO output.
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
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="calculateTableSpecs">
    <xsl:variable name="tds">
      <xsl:for-each select=".//tei:cell">
        <xsl:variable name="stuff">
          <xsl:apply-templates/>
        </xsl:variable>
        <cell>
          <xsl:attribute name="col">
            <xsl:number/>
          </xsl:attribute>
          <xsl:value-of select="string-length($stuff)"/>
        </cell>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="total">
      <xsl:value-of select="sum(exsl:node-set($tds)/cell)"/>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($tds)/cell">
      <xsl:sort select="@col" data-type="number"/>
      <xsl:variable name="c" select="@col"/>
      <xsl:if test="not(preceding-sibling::cell[$c=@col])">
        <xsl:variable name="len">
          <xsl:value-of select="sum(following-sibling::cell[$c=@col]) + current()"/>
        </xsl:variable>
        <xsl:text>&#10;</xsl:text>
        <fo:table-column column-number="{@col}" column-width="{$len div $total * 100}%">
          <xsl:if test="$foEngine='passivetex'">
            <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">L</xsl:attribute>
          </xsl:if>
        </fo:table-column>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="deriveColSpecs">
    <xsl:variable name="no">
      <xsl:call-template name="generateTableID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$readColSpecFile">
        <xsl:variable name="specs">
          <xsl:value-of select="count(exsl:node-set($tableSpecs)/Info/TableSpec[$no=@xml:id])"/>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$specs &gt; 0">
            <xsl:for-each select="exsl:node-set($tableSpecs)/Info/TableSpec[$no=@xml:id]/fo:table-column">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
            <xsl:call-template name="calculateTableSpecs"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
        <xsl:call-template name="calculateTableSpecs"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateTableID">
    <xsl:choose>
      <xsl:when test="@xml:id">
        <xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>Table-</xsl:text>
        <xsl:number level="any"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="xd exsl estr edate a fo rng tei teix" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    dealing  with elements from the
      header module, making XSL-FO output.
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
  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docAuthor"/>
  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docDate</xd:short>
    <xd:detail>
      <p> omit if found outside front matter </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docDate"/>
  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docTitle</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docTitle"/>
  <xd:doc>
    <xd:short>Process elements  tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docAuthor" mode="heading">
    <xsl:if test="preceding-sibling::tei:docAuthor">
      <xsl:choose>
        <xsl:when test="not(following-sibling::tei:docAuthor)">
          <xsl:text> and </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>, </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docAuthor">
    <fo:block font-size="{$authorSize}">
      <fo:inline font-style="italic">
        <xsl:apply-templates/>
      </fo:inline>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:docDate</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docDate">
    <fo:block font-size="{$dateSize}">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:docImprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docImprint"/>
  <xd:doc>
    <xd:short>Process elements  tei:docTitle</xd:short>
    <xd:detail>
      <p> author and title </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docTitle">
    <fo:block text-align="left" font-size="{$titleSize}">
      <xsl:if test="ancestor::tei:group/tei:text/tei:front">
        <xsl:attribute name="id">
          <xsl:choose>
            <xsl:when test="ancestor::tei:text/@xml:id">
              <xsl:value-of select="translate(ancestor::tei:text/@xml:id,'_','-')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="generate-id()"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
      </xsl:if>
      <fo:inline font-weight="bold">
        <xsl:apply-templates select="tei:titlePart"/>
      </fo:inline>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:teiHeader</xd:short>
    <xd:detail>
      <p> ignore the header </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:teiHeader">
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="textTitle">
    <xsl:apply-templates select="tei:front"/>
  </xsl:template>
</xsl:stylesheet>

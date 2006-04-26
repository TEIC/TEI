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
    <xd:short> TEI stylesheet dealing with elements from the linking module,
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:anchor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:anchor">
    <xsl:text>\hypertarget{</xsl:text>
    <xsl:value-of select="@xml:id"/>
    <xsl:text>}{}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:param name="where">where</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateEndLink">
    <xsl:param name="where"/>
    <xsl:value-of select="$where"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="labelme">
    <xsl:if test="../@xml:id">\hypertarget{<xsl:value-of select="../@xml:id"
      />}{}</xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:param name="ptr">ptr</xd:param>
    <xd:param name="dest">dest</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr"/>
    <xsl:param name="dest"/>
    <xsl:choose>
      <xsl:when test="$ptr='true'">
        <xsl:text>\url{</xsl:text>
        <xsl:value-of select="$dest"/>
        <xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\xref{</xsl:text>
        <xsl:value-of select="$dest"/>
        <xsl:text>}{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:param name="target">target</xd:param>
    <xd:param name="ptr">ptr</xd:param>
    <xd:param name="dest">dest</xd:param>
    <xd:param name="body">body</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeInternalLink">
    <xsl:param name="target"/>
    <xsl:param name="ptr"/>
    <xsl:param name="dest"/>
    <xsl:param name="body"/>
    <xsl:choose>
      <xsl:when test="key('IDS',$dest)">
        <xsl:text>\hyperlink{</xsl:text>
        <xsl:value-of select="$dest"/>
        <xsl:text>}{\textit{</xsl:text>
        <xsl:choose>
          <xsl:when test="not($body='')">
            <xsl:value-of select="$body"/>
          </xsl:when>
          <xsl:when test="$ptr='true'">
            <xsl:apply-templates mode="xref" select="key('IDS',$dest)">
              <xsl:with-param name="minimal" select="$minimalCrossRef"/>
            </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>}}</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>«</xsl:text>
        <xsl:choose>
          <xsl:when test="not($body='')">
            <xsl:value-of select="$body"/>
          </xsl:when>
          <xsl:when test="$ptr='true'">
            <xsl:value-of select="$dest"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>»</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

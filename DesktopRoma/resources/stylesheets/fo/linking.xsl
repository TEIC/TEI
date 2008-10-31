<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="xd exsl estr edate a fo rng tei teix" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    dealing  with elements from the
      linking module, making XSL-FO output.
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
    <xd:cvsId>$Id: linking.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:param name="where">target of link</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateEndLink">
    <xsl:param name="where"/>
    <xsl:value-of select="$where"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:param name="ptr">whether the destination URL is also the body
    of the link</xd:param>
    <xd:param name="dest">destination URL</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr"/>
    <xsl:param name="dest"/>
    <fo:basic-link external-destination="url({$dest})">
      <xsl:choose>
        <xsl:when test="$ptr='true'">
          <xsl:call-template name="showXrefURL">
            <xsl:with-param name="dest">
              <xsl:value-of select="$dest"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </fo:basic-link>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:param name="ptr">ptr</xd:param>
    <xd:param name="target">target</xd:param>
    <xd:param name="dest">destination</xd:param>
    <xd:param name="body">body</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeInternalLink">
    <xsl:param name="ptr"/>
    <xsl:param name="target"/>
    <xsl:param name="dest"/>
    <xsl:param name="body"/>
    <xsl:variable name="W">
      <xsl:choose>
        <xsl:when test="$target">
          <xsl:value-of select="$target"/>
        </xsl:when>
        <xsl:when test="contains($dest,'#')">
          <xsl:value-of select="substring-after($dest,'#')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$dest"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <fo:basic-link internal-destination="{$W}">
      <xsl:call-template name="linkStyle"/>
      <xsl:choose>
        <xsl:when test="not($body='')">
          <xsl:value-of select="$body"/>
        </xsl:when>
        <xsl:when test="$ptr='true'">
          <xsl:apply-templates mode="xref" select="key('IDS',$W)">
            <xsl:with-param name="minimal" select="$minimalCrossRef"/>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </fo:basic-link>
  </xsl:template>
</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:local="http://www.pantor.com/ns/local" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="html xd exsl estr edate a fo local rng tei teix" version="1.0">
  <xsl:import href="../common/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet for making HTML output.
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
    <xd:cvsId>$Id: tei.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="dictionaries.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xd:doc type="string">
Stylesheet constant setting the name of the main output file.
</xd:doc>
  <xsl:variable name="top" select="/"/>
  <xsl:variable name="masterFile">
    <xsl:choose>
      <xsl:when test="not($outputName ='')">
        <xsl:choose>
          <xsl:when test="$STDOUT='true'">
            <xsl:value-of select="$outputName"/>
          </xsl:when>
          <xsl:when test="contains($outputName,'.xml')">
            <xsl:value-of select="substring-before($outputName,'.xml')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$outputName"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="contains($REQUEST,'.ID=')">
        <xsl:call-template name="get-basename">
          <xsl:with-param name="file">
            <xsl:value-of select="substring-before($REQUEST,'.ID=')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="not($REQUEST='')">
        <xsl:call-template name="get-basename">
          <xsl:with-param name="file">
            <xsl:value-of select="$REQUEST"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON 8')">
        <xsl:call-template name="get-basename">
          <xsl:with-param name="file">
            <xsl:value-of xmlns:sax8="http://saxon.sf.net/" select="substring-after(sax8:system-id(),':')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON 6')">
        <xsl:call-template name="get-basename">
          <xsl:with-param name="file">
            <xsl:value-of xmlns:saxon="http://icl.com/saxon" select="substring-after(saxon:system-id(),':')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$STDOUT='true'">
        <xsl:text>index.xml</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>index</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xd:doc>
    <xd:short>[html] How to work out the filename component of a path</xd:short>
    <xd:param name="file">filename</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="get-basename">
    <xsl:param name="file"/>
    <xsl:choose>
      <xsl:when test="contains($file,'/')">
        <xsl:call-template name="get-basename">
          <xsl:with-param name="file">
            <xsl:value-of select="substring-after($file,'/')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$STDOUT='true'">
            <xsl:value-of select="$file"/>
          </xsl:when>
          <xsl:when test="contains($file,'.xml')">
            <xsl:value-of select="substring-before($file,'.xml')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$file"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

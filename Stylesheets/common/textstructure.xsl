<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
 xmlns:xd="http://www.pnp-software.com/XSLTdoc"
 xmlns:tei="http://www.tei-c.org/ns/1.0" 
 xmlns:edate="http://exslt.org/dates-and-times" 
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
 extension-element-prefixes="edate" 
 exclude-result-prefixes="xd tei edate" 
 version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet dealing  with elements from the textstructure module.
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
    <xd:short>Establish nesting depth of sections</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6" mode="depth">
    <xsl:choose>
      <xsl:when test="local-name(.) = 'div'">
        <xsl:value-of select="count(ancestor::tei:div)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="ancestor-or-self::tei:div0">
            <xsl:value-of select="substring-after(local-name(.),'div')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="substring-after(local-name(.),'div') - 1"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
 

</xsl:stylesheet>

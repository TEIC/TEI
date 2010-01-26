<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the textstructure
      module. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Establish nesting depth of sections</desc>
   </doc>
  <xsl:template match="tei:text" mode="depth">
    <xsl:value-of select="count(ancestor::tei:text)-1"/>
  </xsl:template>
  <xsl:template match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
                 mode="depth">
      <xsl:choose>
	<xsl:when test="ancestor::tei:text/parent::tei:group and local-name(.) = 'div'">
	   <xsl:value-of select="count(ancestor::tei:div) "/>
	</xsl:when>
         <xsl:when test="local-name(.) = 'div'">
            <xsl:value-of select="count(ancestor::tei:div)"/>
         </xsl:when>
	<xsl:when test="ancestor::tei:text/parent::tei:group">
	   <xsl:value-of select="number(substring-after(local-name(.),'div')) "/>
	</xsl:when>
	<xsl:otherwise>
	   <xsl:value-of select="number(substring-after(local-name(.),'div')) - 1"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
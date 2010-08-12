<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns="http://www.tei-c.org/ns/1.0"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a t tei fo rng xs"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
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

  <xsl:template match="tei:table[@rend='valList'         or @rend='attDef'        or @rend='attList'         or @rend='specDesc']">
      <list type="termlist">
         <xsl:apply-templates/>
      </list>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'      or @rend='valList'      or @rend='attDef'      or @rend='specDesc']/tei:row">
      <item>
         <xsl:apply-templates/>
      </item>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'       or @rend='specDesc'       or @rend='valList'       or @rend='attDef']/tei:row/tei:cell[1]">
      <xsl:choose>
         <xsl:when test="parent::tei:row/parent::tei:table[@rend='attList']">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[@rend='valList']">
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:when test="ancestor::tei:table[@rend='specDesc']">
            <hi rend="bold">@<xsl:apply-templates/>
            </hi>
         </xsl:when>
         <xsl:otherwise>
            <hi rend="bold">
               <xsl:apply-templates/>
            </hi>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:table[@rend='attList'        or @rend='valList'        or @rend='specDesc'        or @rend='attDef']/tei:row/tei:cell[2]">
      <c rend="tab">
         <xsl:text>	</xsl:text>
      </c>
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:index"/>
  <xsl:template match="processing-instruction()[name()='tex' and .='\ ']">
      <c xml:space="preserve"> </c>
  </xsl:template>
   <!-- identity transform -->
  <xsl:output method="xml" indent="yes"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
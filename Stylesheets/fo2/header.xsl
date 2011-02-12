<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    dealing  with elements from the
      header module, making XSL-FO output.
      </p>
         <p>
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

   
   
      </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:docAuthor">
      <block font-size="{$authorSize}">
         <inline font-style="italic">
            <xsl:apply-templates/>
         </inline>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:docDate">
      <block font-size="{$dateSize}">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:docTitle">
      <block text-align="left" font-size="{$titleSize}">
         <xsl:if test="ancestor::tei:group/tei:text/tei:front">
            <xsl:attribute name="id">
               <xsl:choose>
                  <xsl:when test="ancestor::tei:text[1]/@xml:id">
                     <xsl:value-of select="translate(ancestor::tei:text[1]/@xml:id,'_','-')"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:value-of select="generate-id()"/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates select="tei:titlePart"/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
    
         <p xmlns="http://www.w3.org/1999/XSL/Format"> Ignore the header </p>
    
      </desc>
   </doc>
  <xsl:template match="tei:teiHeader">
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="textTitle">
      <xsl:param name="N"/>
      <xsl:apply-templates select="tei:front"/>
  </xsl:template>
</xsl:stylesheet>
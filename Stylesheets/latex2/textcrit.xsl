<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
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
    dealing with elements from the
      textcrit module, making LaTeX output.
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
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element app</p>
         <p>Process tei:lem and tei:rdg within tei:app; first, first, rudimentary attempt. Sends lots of information
    to a footnote. If a tei:lem is not found, the first tei:rdg is used as the base text. Witness sigils in attribute
    wit are assumed all to start with # (this should be parametrized).</p>
      </desc>
   </doc>
  <xsl:template match="tei:app"> <!-- Still needs a lot of work MLF 20070725 -->
  <xsl:choose>
         <xsl:when test="tei:lem">
            <xsl:value-of select="tei:lem"/>
            <xsl:text>\footnote{</xsl:text>
            <xsl:call-template name="i18n">
                <xsl:with-param name="word">asfoundin</xsl:with-param>
                </xsl:call-template>
            <xsl:text> </xsl:text>
            <xsl:value-of select="translate(substring-after(tei:lem/@wit,'#'),' #',', ')"/>
            <xsl:text>. </xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">otherreadings</xsl:with-param>
                </xsl:call-template>
            <xsl:text>: </xsl:text>
            <xsl:for-each select="tei:rdg">
     
               <xsl:text>\emph{</xsl:text>
               <xsl:value-of select="."/>
               <xsl:text>} </xsl:text>
               <xsl:text>(</xsl:text>
               <xsl:value-of select="translate(substring-after(./@wit,'#'),' #',', ')"/>
               <xsl:text>);</xsl:text>
            </xsl:for-each>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="tei:rdg[1]"/> 
            <!-- Select first reading in the absence of tei:lem -->
    <xsl:text>\footnote{</xsl:text>
            <xsl:call-template name="i18n">
                <xsl:with-param name="word">asfoundin</xsl:with-param>
                </xsl:call-template>
            <xsl:text> </xsl:text>
            <xsl:value-of select="translate(substring-after(tei:rdg[1]/@wit,'#'),' #',', ')"/>
            <xsl:text>. </xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">otherreadings</xsl:with-param>
                </xsl:call-template>
            <xsl:text>: </xsl:text>
            <xsl:for-each select="tei:rdg[position()&gt;1]">
     
               <xsl:text>\emph{</xsl:text>
               <xsl:value-of select="."/>
               <xsl:text>} </xsl:text>
               <xsl:text>(</xsl:text>
               <xsl:value-of select="translate(substring-after(./@wit,'#'),' #',', ')"/>
               <xsl:text>);</xsl:text>
            </xsl:for-each>
            <xsl:text>}</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
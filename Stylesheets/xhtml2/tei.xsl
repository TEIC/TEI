<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"                  
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="html a fo rng tei teix"
                version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making HTML output.
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
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="dictionaries.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
  <xsl:include href="../common2/verbatim.xsl"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
Stylesheet constant setting the name of the main output file.
</desc>
   </doc>
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
         <xsl:when test="$STDOUT='true'">
            <xsl:text>index.xml</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>index</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] How to work out the filename component of a path<param name="file">filename</param>
      </desc>
   </doc>
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
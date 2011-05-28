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
      textcrit module.
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
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:key name="APP" match="tei:app" use="1"/>
   
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element app</p>
         <p>Process lem and rdg within app. Sends lots of information
	 to a footnote. If a lem is not found, the first rdg is
	 used as the base text. 
	 </p>
      </desc>
   </doc>
  <xsl:template match="tei:app">
    <xsl:call-template name="appReading">
      <xsl:with-param name="lemma">
	<xsl:choose>
	  <xsl:when test="tei:lem">
	    <xsl:value-of select="tei:lem"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="tei:rdg[1]"/>
	</xsl:otherwise>
      </xsl:choose>
      </xsl:with-param>
      <xsl:with-param name="lemmawitness">
      <xsl:value-of select="@wit"/>
      </xsl:with-param>
      <xsl:with-param name="readings">
	<xsl:for-each select="tei:rdg">
	  <xsl:choose>
	    <xsl:when test="not(../tei:lem) and position()=1"/>
	    <xsl:otherwise>
	      <xsl:value-of select="."/>
	      <xsl:text>(</xsl:text>
	      <xsl:value-of select="translate(substring-after(./@wit,'#'),' #',', ')"/>
	      <xsl:text>)</xsl:text>
	  </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test="following-sibling::tei:rdg">; </xsl:if>
	</xsl:for-each>
      </xsl:with-param>
    </xsl:call-template>
    </xsl:template>

</xsl:stylesheet>
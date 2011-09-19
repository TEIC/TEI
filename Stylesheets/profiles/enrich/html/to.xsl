<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei m"
                version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../xhtml2/tei.xsl"/>
    <xsl:import href="../../../common2/msdescription.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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

  

    <xsl:output indent="no"/>
    <!--  -->
    <xsl:template name="msSection">
      <xsl:param name="level"/>
      <xsl:param name="heading"/>
      <xsl:param name="implicitBlock"  as="xs:boolean" select="false()"/>
      <xsl:element name="h{$level}">
	        <xsl:value-of select="$heading"/>
      </xsl:element>
      <xsl:choose>
	        <xsl:when test="$implicitBlock='true'">
	           <p>
	              <xsl:apply-templates/>
	           </p>
	        </xsl:when>
	        <xsl:when test="*">
	           <xsl:apply-templates/>
	        </xsl:when>
	        <xsl:otherwise>
	           <p>
	              <xsl:apply-templates/>
	           </p>
	        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
    <xsl:template name="msLabelled">
      <xsl:param name="before"/>
      <i>
         <xsl:value-of select="$before"/>
      </i>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="normalize-space(.)"/>
    </xsl:template>

    <xsl:template name="msInline">
      <xsl:param name="before"/>
      <xsl:param name="after"/>
      <xsl:param name="style"/>
      <span class="{local-name()}">
	        <xsl:value-of select="$before"/>
	        <xsl:choose>
	           <xsl:when test="$style='italic'">
	              <i>
	                 <xsl:value-of select="normalize-space(.)"/>
	              </i>
	           </xsl:when>
	           <xsl:when test="$style='bold'">
	              <b>
	                 <xsl:value-of select="normalize-space(.)"/>
	              </b>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:value-of select="normalize-space(.)"/>
	           </xsl:otherwise>
	        </xsl:choose>
	        <xsl:value-of select="$after"/>
      </span>
    </xsl:template>

    <xsl:template name="msBlock">
      <xsl:param name="style"/>
      <div class="{$style}">
	        <xsl:apply-templates/>
      </div>
    </xsl:template>

    <xsl:template match="tei:body">
      <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="tei:teiHeader">
      <xsl:call-template name="pageLayoutSimple"/>
    </xsl:template>

    <xsl:template name="bodyHook">
      <xsl:apply-templates select="//tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:msDesc"/>
    </xsl:template>
    <xsl:param name="cssSecondaryFile">http://tei.oucs.ox.ac.uk/ENRICH/msdescription.css</xsl:param>
    <!-- <xsl:param name="cssFile">tei.css</xsl:param>-->


    <xsl:template match="tei:choice">
      <xsl:apply-templates/>
    </xsl:template>
</xsl:stylesheet>
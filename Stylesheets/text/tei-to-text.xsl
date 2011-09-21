<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0">

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

   <xsl:output method="text"/>
   <xsl:param name="makeCSV">false</xsl:param>
   <xsl:variable name="q">"</xsl:variable>

   <xsl:template match="/">
     <xsl:apply-templates select="tei:preflight(*)"/>
   </xsl:template>

   <xsl:template match="teiHeader"/>

   <xsl:template match="figDesc"/>

   <xsl:template match="choice">
     <xsl:apply-templates select="*[1]"/>
   </xsl:template>

   <xsl:template match="speaker"/>

   <xsl:template match="facsimile"/>

   <!-- for when we need some context -->
   <xsl:template match="text()">
     <xsl:choose>
       <xsl:when test="normalize-space()=''"/>
       <xsl:when test="$makeCSV='true'">
	 <xsl:text>"</xsl:text>
	 <xsl:value-of select="replace(normalize-space(),'$q','$q$q')"/>
	 <xsl:text>","</xsl:text>
	 <xsl:for-each select="ancestor::*">
	   <xsl:value-of select="name()"/>
	   <xsl:text>[</xsl:text>
	   <xsl:number/>
	   <xsl:text>]/</xsl:text>
	 </xsl:for-each>
	 <xsl:text>"&#10;</xsl:text>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:value-of select="normalize-space()"/>
	 <xsl:text>&#10;</xsl:text>
       </xsl:otherwise>
       </xsl:choose>
   </xsl:template>

   
   <xsl:function name="tei:preflight" as="element()+">
     <xsl:param name="n" as="element()"/>
     <xsl:apply-templates select="$n" mode="preflight"/>
   </xsl:function>
   
   <xsl:template match="@*|text()" mode="preflight">
     <xsl:copy-of select="."/>
   </xsl:template>
   
   <xsl:template match="lb|pb" mode="preflight"/>

   <xsl:template match="*" mode="preflight">
     <xsl:copy>
       <xsl:apply-templates select="@*|*|text()" mode="preflight"/>
     </xsl:copy>
   </xsl:template>
   
</xsl:stylesheet>

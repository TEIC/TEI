<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:tiff="http://ns.adobe.com/tiff/1.0/"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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

  <xsl:output method="xml" indent="yes"/>
  <xsl:param name="DIR"/>
  <xsl:key name="W" match="tiff:ImageWidth" use="1"/>
  <xsl:key name="H" match="tiff:ImageLength" use="1"/>
  <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
  </xsl:template>
  
  
  <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
  </xsl:template>
  
  
  <xsl:template match="tei:graphic">
      <xsl:copy>
	        <xsl:variable name="newName">
	           <xsl:text>media/resource</xsl:text>
	           <xsl:number level="any"/>
	           <xsl:text>.</xsl:text>
	           <xsl:value-of select="tokenize(@url,'\.')[last()]"/>
	        </xsl:variable>
	        <xsl:attribute name="url">
	           <xsl:value-of select="$newName"/>
	        </xsl:attribute>
	        <xsl:copy-of select="@n"/>
	        <xsl:copy-of select="@height"/>
	        <xsl:copy-of select="@width"/>
	        <xsl:copy-of select="@scale"/>
		
	        <xsl:if test="doc-available(concat($DIR,'/',$newName,'.xmp'))">
	           <xsl:attribute name="teidocx:width">
	              <xsl:for-each select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	                 <xsl:value-of select="(number(key('W',1)) div 72) * 9144"/>
	              </xsl:for-each>
	           </xsl:attribute>
	  
	           <xsl:attribute name="teidocx:height">
	              <xsl:for-each select="document(concat($DIR,'/',$newName,'.xmp'),/)">
	                 <xsl:value-of select="(number(key('H',1)) div 72) * 9144"/>
	              </xsl:for-each>
	           </xsl:attribute>
	        </xsl:if>
      </xsl:copy>
  </xsl:template>
</xsl:stylesheet>

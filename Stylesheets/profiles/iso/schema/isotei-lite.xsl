<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:saxon="http://icl.com/saxon"
                xmlns:sch="http://www.ascc.net/xml/schematron"
                xmlns:xj="http://xml.apache.org/xalan/java"
                xmlns:loc="http://www.thaiopensource.com/ns/location"
                xmlns:err="http://www.thaiopensource.com/ns/error"
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
   <xsl:template match="/">
      <xsl:apply-templates select="/" mode="all"/>
   </xsl:template>
   <xsl:template match="*|/" mode="all">
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template name="location"/>
   <xsl:template match="node() | @*" mode="schematron-get-full-path-2">
      <xsl:text>

* section </xsl:text>
      <xsl:for-each select="ancestor-or-self::tei:div">/<xsl:number level="multiple"/>
         <xsl:text> - </xsl:text>
         <xsl:value-of select="translate(substring(tei:head,1,20),' ',' ')"/>
      </xsl:for-each>
      <xsl:text> (element </xsl:text>
      <xsl:value-of select="local-name()"/>
      <xsl:text>)

</xsl:text>
   </xsl:template>
</xsl:stylesheet>
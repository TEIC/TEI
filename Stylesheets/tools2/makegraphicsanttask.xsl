<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet to get list of graphics files and make an
	 ant task to copy them</p>
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
         <p>Id: $Id: listgraphics.xsl 8700 2011-02-26 18:27:04Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
   <xsl:key match="tei:graphic[not(ancestor::teix:egXML)]" use="1" name="G"/>
   <xsl:param name="inputDir">.</xsl:param>
   
   <xsl:template match="/">
     <project xmlns="" basedir="." default="dist" name="imagecopy">
       <target name="dist">
	 <xsl:for-each select="key('G',1)">
	   <xsl:variable name="F">
	     <xsl:value-of select="@url"/>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:text>${outputTempDir}/word/media/image</xsl:text>
	     <xsl:number level="any"/>
	     <xsl:text>.</xsl:text>
	     <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="starts-with($F,'http')">
	       <get src="{@url}" dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with($F,'/')">
	       <copy toFile="{$target}" file="{@url}"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <copy toFile="{$target}" file="{$inputDir}/{@url}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>
       </target>
     </project>
 </xsl:template>
   
 </xsl:stylesheet>
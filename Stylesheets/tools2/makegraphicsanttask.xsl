<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:smil="http://www.w3.org/ns/SMIL"
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
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
   <xsl:key name="G" match="tei:graphic[not(ancestor::teix:egXML)]"  use="1"/>
   <xsl:key name="PB" match="tei:pb[@facs]" use="1"/>
   <xsl:key name="Timeline" match="tei:timeline" use="1"/>
   <xsl:param name="mediaoverlay">true</xsl:param>
   <xsl:param name="inputDir">.</xsl:param>
   <xsl:param name="mediaDir">word/media</xsl:param>
   <xsl:template match="/">
     <project xmlns="" basedir="." default="dist" name="imagecopy">
       <target name="dist">
	 <xsl:if test="key('PB',1) or key('G',1)">
	   <mkdir>
	     <xsl:attribute name="dir">
	       <xsl:text>${outputTempDir}/</xsl:text>
	       <xsl:value-of select="$mediaDir"/>
	     </xsl:attribute>
	   </mkdir>
	 </xsl:if>
	 <xsl:if test="$mediaoverlay='true' and key('Timeline',1)">
	   <xsl:for-each select="key('Timeline',1)">
	     <xsl:variable name="target">
	       <xsl:text>${outputTempDir}/</xsl:text>
	       <xsl:value-of select="$mediaDir"/>
	       <xsl:text>/audio</xsl:text>
	       <xsl:number level="any"/>
	       <xsl:text>.</xsl:text>
	       <xsl:value-of select="tokenize(@corresp,'\.')[last()]"/>
	     </xsl:variable>
	     <copy toFile="{$target}" file="{$inputDir}/{@corresp}"/>
	   </xsl:for-each>
	 </xsl:if>
	 <xsl:for-each select="key('PB',1)">
	   <xsl:variable name="F">
	     <xsl:choose>
	       <xsl:when test="starts-with(@facs,'#')">
		 <xsl:for-each select="key('IDS',substring(@facs,2))">
		   <xsl:value-of select="tei:graphic[1]/@url"/>
		 </xsl:for-each>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:value-of select="@facs"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:text>${outputTempDir}/</xsl:text>
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/pageimage</xsl:text>
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
	       <copy toFile="{$target}" file="{$inputDir}/{$F}"/>
	     </xsl:otherwise>
	   </xsl:choose>

	 </xsl:for-each>
	 <xsl:for-each select="key('G',1)">
	   <xsl:variable name="F">
	     <xsl:value-of select="@url"/>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:text>${outputTempDir}/</xsl:text>
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/resource</xsl:text>
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
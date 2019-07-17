<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:smil="http://www.w3.org/ns/SMIL"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0">
  <xsl:import href="../common/functions.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet to get list of graphics files and make an
	 ant task to copy them</p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
   <xsl:key name="G" match="tei:graphic[not(ancestor::teix:egXML)]"  use="1"/>
   <xsl:key name="G" match="tei:media[not(ancestor::teix:egXML)]"  use="1"/>
   <xsl:key name="PB" match="tei:pb[@facs]" use="1"/>
   <xsl:key name="Timeline" match="tei:timeline" use="1"/>
   <xsl:param name="mediaoverlay">false</xsl:param>
   <xsl:param name="coverimage"/>
   <xsl:param name="coverDir"/>
   <xsl:param name="filePerPage">false</xsl:param>
   <xsl:param name="inputDir">.</xsl:param>
   <xsl:param name="outputDir">${outputTempDir}</xsl:param>
   <xsl:param name="mediaDir">word/media</xsl:param>
   <xsl:template match="/">
     <project xmlns="" basedir="." default="dist" name="imagecopy">
       <target name="dist">
	 <xsl:if test="key('PB',1) or key('G',1)">
	   <mkdir>
	     <xsl:attribute name="dir">
	       <xsl:value-of select="$outputDir"/>
	       <xsl:text>/</xsl:text>
	       <xsl:value-of select="$mediaDir"/>
	     </xsl:attribute>
	   </mkdir>
	 </xsl:if>
	 <xsl:if test="not($coverimage='')">
	     <copy toFile="{$coverDir}/{tokenize($coverimage,'/')[last()]}" file="{$coverimage}"/>
	 </xsl:if>
	 <xsl:if test="$mediaoverlay='true' and key('Timeline',1)">
	   <xsl:for-each select="key('Timeline',1)">
	     <xsl:variable name="target">
	       <xsl:value-of select="$outputDir"/>
	       <xsl:text>/</xsl:text>
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
	   <xsl:choose>
	     <xsl:when test="tei:match(@rend,'none')"/>
	     <xsl:when test="starts-with(@facs,'tcp:')"/>
	     <xsl:when test="starts-with(@facs,'unknown:')"/>
	     <xsl:otherwise>
	       <xsl:variable name="F">
		 <xsl:choose>
		   <xsl:when test="starts-with(@facs,'#')">
		     <xsl:for-each
			 select="id(substring(@facs,2))">
		       <xsl:value-of select="tei:resolveURI(.,descendant-or-self::*[@url][1]/@url)"/>
		     </xsl:for-each>
		   </xsl:when>
		   <xsl:otherwise>
		     <xsl:value-of select="tei:resolveURI(.,@facs)"/>
		   </xsl:otherwise>
		 </xsl:choose>
	       </xsl:variable>
	       <xsl:variable name="target">
		 <xsl:value-of select="$outputDir"/>
		 <xsl:text>/</xsl:text>
		 <xsl:value-of select="$mediaDir"/>
		 <xsl:text>/pageimage</xsl:text>
		 <xsl:number level="any"/>
		 <xsl:text>.</xsl:text>
		 <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	       </xsl:variable>
	       <xsl:choose>
		 <xsl:when test="contains($F,':')">
		   <get src="{$F}" dest="{$target}"/>
		 </xsl:when>
		 <xsl:when test="starts-with($F,'/')">
		   <copy toFile="{$target}" file="{@url}"/>
		 </xsl:when>
		 <xsl:otherwise>
		   <copy toFile="{$target}" file="{$inputDir}/{$F}"/>
		 </xsl:otherwise>
	       </xsl:choose>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>

	 <xsl:for-each select="key('G',1)">
	   <xsl:variable name="F">
	     <xsl:value-of select="@url"/>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:value-of select="$outputDir"/>
	     <xsl:text>/</xsl:text>
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/resource</xsl:text>
	     <xsl:number level="any"/>
	     <xsl:text>.</xsl:text>
	     <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="contains($F,':')">
	       <get src="{$F}" dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with($F,'/')">
	       <copy toFile="{$target}" file="{@url}"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <copy overwrite="yes" toFile="{$target}" file="{$inputDir}/{@url}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>
       </target>
     </project>
 </xsl:template>
   
 </xsl:stylesheet>

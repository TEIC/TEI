<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:v="urn:schemas-microsoft-com:vml"
		xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0">
  <xsl:import href="../../common/functions.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet to make a script to copy graphics files</p>
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

  <xsl:output method="xml"/>
  <xsl:param name="mediaDir"/>
  <xsl:param name="inputDir">.</xsl:param>

  <xsl:key match="tei:graphic[not(ancestor::teix:egXML)]" use="1" name="G"/>
  <xsl:key match="tei:media[not(ancestor::teix:egXML)]" use="1" name="G"/>

  <xsl:template match="/">
     <project xmlns="" basedir="." default="dist" name="imagecopy">
       <target name="dist">
	 <mkdir dir="{$mediaDir}"/>
	 <xsl:for-each select="key('G',1)">
	   <xsl:variable name="F">
	     <xsl:sequence select="tei:resolveURI(.,@url)"/>
	   </xsl:variable>
	   <xsl:variable name="target">
	     <xsl:value-of select="$mediaDir"/>
	     <xsl:text>/image</xsl:text>
	     <xsl:number level="any"/>
	     <xsl:text>.</xsl:text>
	     <xsl:value-of select="tokenize($F,'\.')[last()]"/>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="starts-with($F,'http')">
	       <get src="{$F}"  dest="{$target}"/>
	     </xsl:when>
	     <xsl:when test="starts-with($F,'/')">
	       <copy file="{$F}" toFile="$target"/>
	     </xsl:when>
	     <xsl:otherwise> 
	       <copy file="{concat($inputDir,'/',$F)}" toFile="{$target}"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:for-each>
       </target>
     </project>
  </xsl:template>

</xsl:stylesheet>

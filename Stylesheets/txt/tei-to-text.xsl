<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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

   <xsl:template match="gap/desc"/>

   <xsl:template match="choice">
     <xsl:apply-templates select="*[1]"/>
   </xsl:template>

   <xsl:template match="speaker"/>

   <xsl:template match="facsimile"/>

   <!-- for when we need some context -->
  <xsl:function name="tei:escapeChars" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
     <xsl:choose>
       <xsl:when test="normalize-space($letters)=''">
	 <xsl:text/>
       </xsl:when>
       <xsl:when test="$makeCSV='true'">
	 <xsl:variable name="result">
	 <xsl:text>"</xsl:text>
	 <xsl:value-of select="replace(normalize-space($letters),'$q','$q$q')"/>
	 <xsl:text>","</xsl:text>
	 <xsl:for-each select="$context/ancestor::*">
	   <xsl:value-of select="name()"/>
	   <xsl:text>[</xsl:text>
	   <xsl:number/>
	   <xsl:text>]/</xsl:text>
	 </xsl:for-each>
	 <xsl:text>"&#10;</xsl:text>
	 </xsl:variable>
	 <xsl:value-of select="$result"/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:value-of select="concat(normalize-space($letters),'&#10;')"/>
       </xsl:otherwise>
       </xsl:choose>
  </xsl:function>

   
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

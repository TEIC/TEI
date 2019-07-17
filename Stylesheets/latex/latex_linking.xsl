<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix xs"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the linking module,
      making LaTeX output. </p>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element anchor</desc>
   </doc>
  <xsl:template match="tei:anchor">
    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="where">where</param>
      </desc>
   </doc>

  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:value-of select="$where"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="ptr">ptr</param>
         <param name="dest">dest</param>
      </desc>
   </doc>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="title"/>
      <xsl:choose>
         <xsl:when test="$ptr">
            <xsl:text>\url{</xsl:text>
	    <xsl:sequence select="tei:escapeChars($dest,.)"/>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>\xref{</xsl:text>
            <xsl:value-of select="tei:escapeCharsPartial($dest)"/>
            <xsl:text>}{</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>}</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="target">target</param>
         <param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="body">body</param>
      </desc>
   </doc>
  <xsl:template name="makeInternalLink">
      <xsl:param name="target"/>
      <xsl:param name="class"/>
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="body"/>
      <xsl:choose>
	<xsl:when test="$dest=''">
	  <xsl:value-of select="$body"/>
	</xsl:when>
         <xsl:when test="id($dest)">
            <xsl:choose>
               <xsl:when test="parent::tei:label">
		 <xsl:text>\hyperlink{</xsl:text>
		 <xsl:value-of select="$dest"/>
		 <xsl:text>}{</xsl:text>
		 <xsl:value-of select="$body"/>
		 <xsl:text>}</xsl:text>
               </xsl:when>
               <xsl:when test="not($body='')">
		 <xsl:text>\hyperref[</xsl:text>
		 <xsl:value-of select="$dest"/>
		 <xsl:text>]{</xsl:text>
		 <xsl:value-of select="$body"/>
		 <xsl:text>}</xsl:text>
               </xsl:when>
               <xsl:when test="$ptr">
		 <xsl:for-each select="id($dest)">
		   <xsl:choose>
		     <xsl:when test="$class='pageref'">
		       <xsl:text>\pageref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:note[@xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:figure[tei:head and @xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:table[tei:head and @xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		      <!-- Check whether the current ptr is a reference to a bibliographic object 
		          and use \cite rather than \hyperref -->
		      <xsl:when test="self::tei:biblStruct[ancestor::tei:div/@xml:id='BIB']">
		         <xsl:text>\cite{</xsl:text>
		         <xsl:value-of select="$dest"/>
		         <xsl:text>}</xsl:text>
		      </xsl:when>
		     <xsl:when test="starts-with(local-name(.),'div')">
		       <xsl:text>\textit{\hyperref[</xsl:text>
		       <xsl:value-of select="$dest"/>
		       <xsl:text>]{</xsl:text>
		       <xsl:apply-templates mode="xref" select=".">
			 <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		       </xsl:apply-templates>
		       <xsl:text>}}</xsl:text>
		     </xsl:when>
		     <xsl:otherwise>
		       <xsl:text>\hyperref[</xsl:text>
		       <xsl:value-of select="$dest"/>
		       <xsl:text>]{</xsl:text>
		       <xsl:value-of select="$body"/>
		       <xsl:apply-templates mode="xref" select=".">
			 <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		       </xsl:apply-templates>
		       <xsl:text>}</xsl:text>
		     </xsl:otherwise>
		   </xsl:choose>
		 </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
		 <xsl:text>\hyperref[</xsl:text>
		 <xsl:value-of select="$dest"/>
		 <xsl:text>]{</xsl:text>
		 <xsl:value-of select="$body"/>
		 <xsl:apply-templates/>
		 <xsl:text>}</xsl:text>
	       </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:choose>
	     <xsl:when test="not($body='')">
	       <xsl:value-of select="$body"/>
	     </xsl:when>
	     <xsl:when test="$ptr">
	       <xsl:value-of select="$dest"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:apply-templates/>
	     </xsl:otherwise>
	   </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process cross-ref to note</desc>
   </doc>
  <xsl:template match="tei:note" mode="xref">
    <xsl:number level="any"/>
  </xsl:template>

</xsl:stylesheet>

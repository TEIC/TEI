<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    dealing with elements from the
      textcrit module.
      </p>
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

   <xsl:key name="APPREADINGS" match="tei:app[starts-with(@from,'#')]" use="substring(@from,2)"/>
   
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element app</p>
         <p>Process lem and rdg within app. Sends lots of information
	 to a footnote. If a lem is not found, the first rdg is
	 used as the base text. 
	 </p>
      </desc>
   </doc>
  <xsl:template match="tei:app">
    <xsl:if test="not(@from)">
      <xsl:call-template name="makeAppEntry">
	<xsl:with-param name="lemma">
	  <xsl:call-template name="appLemma"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
    <xsl:template match="tei:w">
      <xsl:choose>
	<xsl:when test="not(tei:app) and key('APPREADINGS',@xml:id)">
	  <xsl:call-template name="findApp"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="findApp">
      <xsl:variable name="sourcelem" select="."/>
      <xsl:for-each select="key('APPREADINGS',@xml:id)">
	<xsl:call-template name="makeAppEntry">
	  <xsl:with-param name="lemma" select="$sourcelem"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:template>
	 

  <xsl:template match="tei:back/tei:div[@type='apparatus']"
		priority="9999"/>

   <xsl:template match="tei:metamark"/>
   <xsl:template match="tei:sourceDoc"/>
   <xsl:template match="tei:facsimile"/>

   <xsl:template match="tei:listWit">
     <xsl:variable name="l">
       <list rend="unordered" xmlns="http://www.tei-c.org/ns/1.0">
	 <xsl:for-each select="tei:witness">
	   <item>
	     <xsl:copy-of select="@*|*|text()"/>
	   </item>
	 </xsl:for-each>
       </list>
     </xsl:variable>
     <xsl:apply-templates select="$l"/>
   </xsl:template>

   <xsl:template name="appN">
      <xsl:choose>
         <xsl:when test="@n">
            <xsl:value-of select="@n"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:number from="tei:text" level="any"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>


   <xsl:template name="appLemmaWitness">
     <xsl:choose>
       <xsl:when test="tei:lem">
	 <xsl:value-of select="tei:getWitness(tei:lem/@wit)"/>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:value-of select="tei:getWitness(tei:rdg[1]/@wit)"/>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <xsl:template name="appLemma">
	<xsl:choose>
	  <xsl:when test="tei:lem">
	    <xsl:value-of select="tei:lem"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="tei:rdg[1]"/>
	  </xsl:otherwise>
	</xsl:choose>
   </xsl:template>

   <xsl:template name="appReadings">
      <xsl:variable name="start" select="if (not(tei:lem)) then 1 else 0"/>
      <xsl:for-each select="tei:rdg[position() &gt; $start]">
	<xsl:text>; </xsl:text>
	<xsl:apply-templates/>
	<xsl:if test="@cause='omission'">[]</xsl:if>
	<xsl:text> (</xsl:text>
	<xsl:value-of select="tei:getWitness(@wit)"/>
	<xsl:text>)</xsl:text>
	<xsl:if test="following-sibling::tei:rdg">; </xsl:if>
      </xsl:for-each>
    </xsl:template>


   <xsl:template name="makeAppEntry">
     <xsl:param name="lemma"/>
     <xsl:call-template name="appLemma"/>
     <xsl:text>] </xsl:text>
     <xsl:call-template name="appLemmaWitness"/>
     <xsl:call-template name="appReadings"/>
   </xsl:template>

</xsl:stylesheet>

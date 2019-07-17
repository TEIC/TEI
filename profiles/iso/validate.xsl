<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"   version="2.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
   <xsl:import href="isotei-schema.xsl"/>
   <xsl:import href="isoutils.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
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
  

   <xsl:param name="numberFormat">uk</xsl:param>
   <xsl:output method="text"/>

   <xsl:template match="/">
       <xsl:apply-templates mode="checkSchematron"/>
   </xsl:template>

   <xsl:template name="generateError">
     <xsl:param name="message"/>
     <xsl:text>&#10;&#10;ISO Error:&#10;</xsl:text>
     <xsl:text>   Clause: </xsl:text>
     <xsl:for-each select="ancestor::tei:div[1]">
       <xsl:choose>
	 <xsl:when test="ancestor::tei:front">
	   <xsl:number count="tei:div" from="tei:front" format="i" level="multiple"/>
	 </xsl:when>
	 <xsl:when test="ancestor::tei:body">
	   <xsl:number count="tei:div" from="tei:body" format="1" level="multiple"/>
	 </xsl:when>
	 <xsl:when test="ancestor::tei:back">
	   Annex <xsl:number count="tei:div" from="tei:back" format="A.1.1" level="multiple"/>
	 </xsl:when>
       </xsl:choose>
      <xsl:text> </xsl:text>
      <xsl:value-of select="tei:head"/>
     </xsl:for-each>
     <xsl:text>&#10;   Context: </xsl:text>
     <xsl:for-each select="parent::*">
       <xsl:call-template name="Identify"/>
     </xsl:for-each>
     <xsl:text>&#10;</xsl:text>
     <xsl:value-of select="$message"/>
   </xsl:template>

   <xsl:template name="copyMe"/>

   <xsl:template name="copyIt">
     <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
   </xsl:template>

   <xsl:template match="processing-instruction()[name()='ISOerror']"
		 mode="checkSchematron">
     <xsl:call-template name="generateError">
       <xsl:with-param name="message"><xsl:value-of
       select="."/></xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tbx:termEntry">
     <xsl:if test="not(preceding-sibling::tbx:termEntry)">
       <table>
	 <xsl:apply-templates select="." mode="go"/>
	 <xsl:apply-templates select="following-sibling::tbx:termEntry" mode="go"/>
       </table>
     </xsl:if>
   </xsl:template>

   <xsl:template match="tbx:termEntry" mode="go">
      <xsl:for-each select="tbx:langSet">
	<tr>
	  <td>
         <xsl:choose>
            <xsl:when test="starts-with(../@id,'autoTermNum')">
            </xsl:when>
            <xsl:otherwise>
                   <xsl:value-of select="substring-after(../@id,'user_')"/>:
            </xsl:otherwise>
         </xsl:choose>
	  </td>
	  <td>
         <xsl:for-each select="tbx:ntig">
	   <xsl:apply-templates/>
	   <xsl:if test="following-sibling::tbx:ntig"><br/></xsl:if>
         </xsl:for-each>
	  </td>
	  <td>
	    <xsl:apply-templates select="tbx:descripGrp/tbx:descrip[@type='definition']"/>
	    <xsl:apply-templates select="tbx:note"/>
	  </td>
	</tr>
      </xsl:for-each>
      <xsl:if test="tbx:descripGrp/tbx:descrip[@type='definition']">
      <tr>
	<td>&#10;</td>
	  <td colspan="2">
	  <xsl:apply-templates
	      select="tbx:descripGrp/tbx:descrip[@type='definition']"/>
	  </td>
      </tr>
      </xsl:if>
      <xsl:if test="tbx:note">
	<tr>
	  <td>&#10;</td>
	    <td colspan="2">
	      <xsl:apply-templates select="tbx:note"/>
	  </td>
	</tr>
      </xsl:if>

   </xsl:template>
   
   <xsl:template match="tbx:termNote"/>

 <xsl:template name="block-element">
     <xsl:param name="select"/>
     <xsl:param name="style"/>
     <xsl:param name="pPr" as="node()*"/>
     <xsl:param name="nop"/>
     <xsl:param name="bookmark-name"/>
     <xsl:param name="bookmark-id"/>
   </xsl:template>

   <xsl:template name="termNum"/>


</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes=" a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making table specifications, making XSL-FO output.
      </p>
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
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="calculateTableSpecs">
      <xsl:variable name="tds">
         <xsl:for-each select=".//tei:cell">
            <xsl:variable name="stuff">
               <xsl:apply-templates/>
            </xsl:variable>
            <cell>
               <xsl:attribute name="col">
                  <xsl:number/>
               </xsl:attribute>
               <xsl:value-of select="string-length($stuff)"/>
            </cell>
         </xsl:for-each>
      </xsl:variable>
      <xsl:variable name="total">
         <xsl:value-of select="sum($tds/cell)"/>
      </xsl:variable>
      <xsl:for-each select="$tds/cell">
         <xsl:sort select="@col" data-type="number"/>
         <xsl:variable name="c" select="@col"/>
         <xsl:if test="not(preceding-sibling::cell[$c=@col])">
            <xsl:variable name="len">
               <xsl:value-of select="sum(following-sibling::cell[$c=@col]) + current()"/>
            </xsl:variable>
            <xsl:text>&#10;</xsl:text>
            <table-column column-number="{@col}" column-width="{$len div $total * 100}%">
               <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">L</xsl:attribute>
               </xsl:if>
            </table-column>
         </xsl:if>
      </xsl:for-each>
      <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="deriveColSpecs">
      <xsl:variable name="no">
         <xsl:call-template name="generateTableID"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$readColSpecFile">
            <xsl:variable name="specs">
               <xsl:value-of select="count($tableSpecs/Info/TableSpec[$no=@xml:id])"/>
            </xsl:variable>
            <xsl:choose>
               <xsl:when test="$specs &gt; 0">
                  <xsl:for-each select="$tableSpecs/Info/TableSpec[$no=@xml:id]/table-column">
                     <xsl:copy-of select="."/>
                  </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
            <xsl:call-template name="calculateTableSpecs"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
<!--
 <xsl:message>Build specs for Table <xsl:value-of select="$no"/></xsl:message>
-->
        <xsl:call-template name="calculateTableSpecs"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[fo] </desc>
   </doc>
  <xsl:template name="generateTableID">
      <xsl:choose>
         <xsl:when test="@xml:id">
            <xsl:value-of select="@xml:id"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>Table-</xsl:text>
            <xsl:number level="any"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
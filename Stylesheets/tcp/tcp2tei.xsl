<?xml version="1.0" encoding="utf-8"?>
<!--
$Date$ $Author$

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns:tei="http://www.tei-c.org/ns/1.0"
version="2.0">	
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>XSLT script for cleaning up ECCO texts to P4, then running P4 to P5</p>
         <p> 
            <h1 xmlns="">License</h1>This software is dual-licensed:

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
  <xsl:output cdata-section-elements="eg" indent="yes" method="xml" encoding="utf-8" omit-xml-declaration="yes"/>
  <xsl:param name="ID"/>
  <xsl:param name="intype"> ',)</xsl:param>
  <xsl:variable name="HERE" select="/"/>
  <xsl:template match="/">
    <xsl:variable name="phase1">
      <xsl:apply-templates mode="tcp"/>
    </xsl:variable>
    <xsl:for-each select="$phase1">
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>
  <!-- TCP copy -->
  <xsl:template match="@*" mode="tcp">
    <xsl:attribute name="{lower-case(local-name(.))}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="processing-instruction()|comment()|text()" mode="tcp">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="*" mode="tcp">
    <xsl:element name="{lower-case(local-name(.))}">
      <xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </xsl:element>
  </xsl:template>

  <!-- TCP discards -->
  <xsl:template match="FIGDESC/HI" mode="tcp">
    <xsl:apply-templates mode="tcp"/>
  </xsl:template>
  <xsl:template match="PB/@MS" mode="tcp"/>
  <xsl:template match="LABEL/@ROLE" mode="tcp"/>
  <xsl:template match="TITLE/@TYPE" mode="tcp"/>
  <xsl:template match="TEMPHEAD" mode="tcp"/>
  <xsl:template match="TITLE/@I2" mode="tcp"/>
  <xsl:template match="IDG" mode="tcp"/>
  <xsl:template match="@LANG[.='eng dut']" mode="tcp"/>
  <xsl:template match="@LANG[.='eng san']" mode="tcp"/>
  <xsl:template match="@LANG[.='fre eng']" mode="tcp"/>
  <xsl:template match="@LANG[.='lat eng']" mode="tcp"/>
  <xsl:template match="@LANG[.='lat gre']" mode="tcp"/>
  <xsl:template match="@PLACE[.='marg']" mode="tcp">
    <xsl:attribute name="place">margin</xsl:attribute>
  </xsl:template>
  <!-- TCP controversial changes -->
  <!--
      <xsl:template match="HEAD/L" mode="tcp">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/><lb/>
      </xsl:template>
      <xsl:template match="TRAILER/L" mode="tcp">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/><lb/>
      </xsl:template>
  -->
  <xsl:template match="HEAD[@TYPE='sub'][Q/L and not(P)]" mode="tcp">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
  </xsl:template>
  <xsl:template match="HEAD[@TYPE='sub']/Q[L]" mode="tcp">
      <epigraph>
	<xsl:apply-templates
	    select="*|processing-instruction()|comment()|text()"
	    mode="tcp"/>
      </epigraph>
  </xsl:template>
  <xsl:template match="P[not(parent::SP) and count(*)=1 and not(text()) and (LETTER or
		       LIST or TABLE or FIGURE)]" mode="tcp">
	<xsl:apply-templates select="*|processing-instruction()|comment()"
	    mode="tcp"/>
  </xsl:template>
  <xsl:template match="CELL[count(*)=1 and not(text()) and P]" mode="tcp">
    <cell>
	<xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:for-each select="p">
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()"
	    mode="tcp"/>
      </xsl:for-each>
    </cell>
</xsl:template>

  <xsl:template match="NOTE[count(*)=1 and not(text())]/Q" mode="tcp">
	<xsl:apply-templates
	    select="*|processing-instruction()|comment()|text()"
	    mode="tcp"/>
  </xsl:template>
  <xsl:template match="Q[count(*)=1 and not(text()) and LG/L]"
		mode="tcp" priority="10">
    <q>
      <xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:for-each select="lg">
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()"
			     mode="tcp"/>
      </xsl:for-each>
    </q>
  </xsl:template>

  <xsl:template match="TITLESTMT/TITLE/text()[last()]" mode="tcp">
    <xsl:choose>
      <xsl:when test="matches(.,':$')">
	<xsl:value-of select="substring(.,1,string-length(.)-1)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="HEADNOTE[P/FIGURE]" mode="tcp">
      <xsl:apply-templates mode="tcp"/>
  </xsl:template>

  <xsl:template match="HEADNOTE" mode="tcp">
    <argument>
      <xsl:apply-templates mode="tcp"/>
    </argument>
  </xsl:template>

  <xsl:template match="TAILNOTE" mode="tcp">
    <argument>
      <xsl:apply-templates mode="tcp"/>
    </argument>
  </xsl:template>

  <xsl:template match="STAGE/STAGE" mode="tcp">
      <xsl:apply-templates mode="tcp"/>
  </xsl:template>

<!--
  <xsl:template match="CELL/L" mode="tcp">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/><lb/>
  </xsl:template>
  <xsl:template match="CELL/LG" mode="tcp">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
  </xsl:template>
  <xsl:template match="CELL/LG/L" mode="tcp">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/><lb/>
  </xsl:template>
-->
  <!-- TCP non-controversial transforms -->
  <xsl:template match="ROW/PB" mode="tcp"/>
  <xsl:template match="ROW[PB]" mode="tcp">
    <row>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" mode="tcp"/>
    </row>
    <xsl:for-each select="PB">
      <pb>
	<xsl:apply-templates select="@*" mode="tcp"/>
      </pb>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="ROW/TABLE" mode="tcp">
    <cell>
      <table>
	<xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" mode="tcp"/>
      </table>
    </cell>
  </xsl:template>

  <xsl:template match="EEBO" mode="tcp">
    <xsl:apply-templates select="*" mode="tcp"/>
  </xsl:template>

  <xsl:template match="ETS" mode="tcp">
    <TEI.2>
      <xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:for-each select="document(concat($ID,'.hdr'),$HERE)">
        <xsl:apply-templates select="*" mode="tcp"/>
      </xsl:for-each>
      <xsl:apply-templates mode="tcp"/>
    </TEI.2>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT" mode="tcp">
    <publicationStmt>
      <xsl:apply-templates select="*" mode="tcp"/>
      <xsl:if test="parent::FILEDESC">
	<xsl:call-template name="makeID"/>
	<xsl:for-each select="$HERE">
	  <xsl:for-each select="/ETS/EEBO/IDG">
	    <xsl:for-each select="STC">
	      <idno type="STC">
		<xsl:value-of select="."/>
	      </idno>
	    </xsl:for-each>
	    <idno type="TCP"><xsl:value-of select="@ID"/></idno>
	    <idno type="BIBNO"><xsl:value-of select="BIBNO"/></idno>
	    <xsl:for-each select="VID">
		<xsl:if test="@SET">
		  <idno type="{@SET}">
		    <xsl:value-of select="."/>
		  </idno>
		</xsl:if>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:call-template name="idnoHook"/>
      </xsl:if>
    </publicationStmt>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT/IDNO"  mode="tcp"/>
  <xsl:template match="FILEDESC/EXTENT" mode="tcp"/>

  <xsl:template match="EEBO/GROUP" mode="tcp">
    <text>
      <group>
        <xsl:apply-templates select="@*" mode="tcp"/>
        <xsl:apply-templates select="*" mode="tcp"/>
      </group>
    </text>
  </xsl:template>
  <xsl:template match="LETTER" mode="tcp">
    <floatingText type="letter">
      <body>
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
      </body>
    </floatingText>
  </xsl:template>
  <xsl:template match="TEXT" mode="tcp">
    <xsl:choose>
      <xsl:when test="parent::ETS or parent::EEBO or parent::GROUP">
        <text>
          <xsl:apply-templates select="@*" mode="tcp"/>
          <xsl:apply-templates select="*" mode="tcp"/>
        </text>
      </xsl:when>
      <xsl:otherwise>
        <floatingText>
          <xsl:apply-templates select="@*" mode="tcp"/>
          <xsl:apply-templates select="*" mode="tcp"/>
        </floatingText>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="DIV2|DIV3|DIV4|DIV5|DIV6|DIV7" mode="tcp">
    <div>
      <xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:apply-templates select="*" mode="tcp"/>
    </div>
  </xsl:template>
  <xsl:template match="DIV1" mode="tcp">
    <xsl:choose>
      <xsl:when test="count(parent::BODY/*)=1">
        <xsl:apply-templates select="*" mode="tcp"/>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:apply-templates select="@*" mode="tcp"/>
          <xsl:apply-templates select="*" mode="tcp"/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="LANGUSAGE/@ID" mode="tcp"/>
  <xsl:template match="LANGUAGE[not(@ID)]" mode="tcp">
    <language id="{../@ID}">
      <xsl:apply-templates select="@*|text()" mode="tcp"/>
    </language>
  </xsl:template>
  <xsl:template match="GAP/@DISP" mode="tcp">
    <xsl:attribute name="rend">
      <xsl:text>content:</xsl:text>
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="ITEM/@ROLE" mode="tcp">
    <xsl:attribute name="rend">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="PB/@REF" mode="tcp">
    <xsl:attribute name="facs">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="rend">
      <xsl:text>none</xsl:text>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="NOTE[@PLACE='foot;']" mode="tcp">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='foor']" mode="tcp">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='foot']" mode="tcp">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='‖']" mode="tcp">
    <note n="‖">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='‡']" mode="tcp">
    <note n="‡">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='†']" mode="tcp">
    <note n="†">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="tcp"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </note>
  </xsl:template>
  <xsl:template match="KEYWORDS" mode="tcp">
    <keywords>
      <xsl:if test="not(@SCHEME)">
        <xsl:attribute name="scheme">
          <xsl:text>http://authorities.loc.gov/</xsl:text>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="tcp"/>
    </keywords>
  </xsl:template>
  <xsl:template match="SUP" mode="tcp">
    <hi rend="sup">
      <xsl:apply-templates mode="tcp"/>
    </hi>
  </xsl:template>
  <xsl:template match="SUB" mode="tcp">
    <hi rend="sub">
      <xsl:apply-templates mode="tcp"/>
    </hi>
  </xsl:template>
  <xsl:template match="BELOW" mode="tcp">
    <hi rend="below">
      <xsl:apply-templates mode="tcp"/>
    </hi>
  </xsl:template>
  <xsl:template match="ABOVE" mode="tcp">
    <hi rend="above">
      <xsl:apply-templates mode="tcp"/>
    </hi>
  </xsl:template>
  <xsl:template match="HEADER" mode="tcp">
    <teiHeader>
      <xsl:apply-templates select="@*" mode="tcp"/>
      <xsl:apply-templates select="*" mode="tcp"/>
    </teiHeader>
  </xsl:template>

  <xsl:template match="TEI.2|OTA" mode="tcp">
    <TEI.2>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </TEI.2>
  </xsl:template>
  <xsl:template match="ADDNAME" mode="tcp">
    <addName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </addName>
  </xsl:template>
  <xsl:template match="ADDSPAN" mode="tcp">
    <addSpan>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </addSpan>
  </xsl:template>
  <xsl:template match="ADDRLINE" mode="tcp">
    <addrLine>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </addrLine>
  </xsl:template>
  <xsl:template match="ALTGRP" mode="tcp">
    <altGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </altGrp>
  </xsl:template>
  <xsl:template match="ATTDEF" mode="tcp">
    <attDef>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </attDef>
  </xsl:template>
  <xsl:template match="ATTLIST" mode="tcp">
    <attList>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </attList>
  </xsl:template>
  <xsl:template match="ATTNAME" mode="tcp">
    <attName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </attName>
  </xsl:template>
  <xsl:template match="ATTLDECL" mode="tcp">
    <attlDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </attlDecl>
  </xsl:template>
  <xsl:template match="BASEWSD" mode="tcp">
    <baseWsd>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </baseWsd>
  </xsl:template>
  <xsl:template match="BIBLFULL" mode="tcp">
    <biblFull>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </biblFull>
  </xsl:template>
  <xsl:template match="BIBLSCOPE" mode="tcp">
    <biblScope>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </biblScope>
  </xsl:template>
  <xsl:template match="BIBLSTRUCT" mode="tcp">
    <biblStruct>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </biblStruct>
  </xsl:template>
  <xsl:template match="CASTGROUP" mode="tcp">
    <castGroup>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </castGroup>
  </xsl:template>
  <xsl:template match="CASTITEM" mode="tcp">
    <castItem>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </castItem>
  </xsl:template>
  <xsl:template match="CASTLIST" mode="tcp">
    <castList>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </castList>
  </xsl:template>
  <xsl:template match="CATDESC" mode="tcp">
    <catDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </catDesc>
  </xsl:template>
  <xsl:template match="CATREF" mode="tcp">
    <catRef>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </catRef>
  </xsl:template>
  <xsl:template match="CLASSCODE" mode="tcp">
    <classCode>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </classCode>
  </xsl:template>
  <xsl:template match="CLASSDECL" mode="tcp">
    <classDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </classDecl>
  </xsl:template>
  <xsl:template match="CLASSDOC" mode="tcp">
    <classDoc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </classDoc>
  </xsl:template>
  <xsl:template match="CODEDCHARSET" mode="tcp">
    <codedCharSet>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </codedCharSet>
  </xsl:template>
  <xsl:template match="DATADESC" mode="tcp">
    <dataDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </dataDesc>
  </xsl:template>
  <xsl:template match="DATERANGE" mode="tcp">
    <dateRange>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </dateRange>
  </xsl:template>
  <xsl:template match="DATESTRUCT" mode="tcp">
    <dateStruct>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </dateStruct>
  </xsl:template>
  <xsl:template match="DELSPAN" mode="tcp">
    <delSpan>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </delSpan>
  </xsl:template>
  <xsl:template match="DIVGEN" mode="tcp">
    <divGen>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </divGen>
  </xsl:template>
  <xsl:template match="DOCAUTHOR|DAUTHOR" mode="tcp">
    <docAuthor>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </docAuthor>
  </xsl:template>
  <xsl:template match="DOCDATE" mode="tcp">
    <docDate>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </docDate>
  </xsl:template>
  <xsl:template match="DOCEDITION" mode="tcp">
    <docEdition>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </docEdition>
  </xsl:template>
  <xsl:template match="DOCIMPRINT" mode="tcp">
    <docImprint>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </docImprint>
  </xsl:template>
  <xsl:template match="DOCTITLE|DTITLE" mode="tcp">
    <docTitle>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </docTitle>
  </xsl:template>
  <xsl:template match="ELEAF" mode="tcp">
    <eLeaf>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </eLeaf>
  </xsl:template>
  <xsl:template match="ETREE" mode="tcp">
    <eTree>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </eTree>
  </xsl:template>
  <xsl:template match="EDITIONSTMT" mode="tcp">
    <editionStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </editionStmt>
  </xsl:template>
  <xsl:template match="EDITORIALDECL" mode="tcp">
    <editorialDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </editorialDecl>
  </xsl:template>
  <xsl:template match="ELEMDECL" mode="tcp">
    <elemDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </elemDecl>
  </xsl:template>
  <xsl:template match="ENCODINGDESC" mode="tcp">
    <encodingDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="ENTDOC" mode="tcp">
    <entDoc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </entDoc>
  </xsl:template>
  <xsl:template match="ENTNAME" mode="tcp">
    <entName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </entName>
  </xsl:template>
  <xsl:template match="ENTITYSET" mode="tcp">
    <entitySet>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </entitySet>
  </xsl:template>
  <xsl:template match="ENTRYFREE" mode="tcp">
    <entryFree>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </entryFree>
  </xsl:template>
  <xsl:template match="EXTFIGURE" mode="tcp">
    <extFigure>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </extFigure>
  </xsl:template>
  <xsl:template match="FALT" mode="tcp">
    <fAlt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fAlt>
  </xsl:template>
  <xsl:template match="FDECL" mode="tcp">
    <fDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fDecl>
  </xsl:template>
  <xsl:template match="FDESCR" mode="tcp">
    <fDescr>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fDescr>
  </xsl:template>
  <xsl:template match="FLIB" mode="tcp">
    <fLib>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fLib>
  </xsl:template>
  <xsl:template match="FIGDESC" mode="tcp">
    <figDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </figDesc>
  </xsl:template>
  <xsl:template match="FILEDESC" mode="tcp">
    <fileDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fileDesc>
  </xsl:template>
  <xsl:template match="FIRSTLANG" mode="tcp">
    <firstLang>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </firstLang>
  </xsl:template>
  <xsl:template match="FORENAME" mode="tcp">
    <foreName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </foreName>
  </xsl:template>
  <xsl:template match="FORESTGRP" mode="tcp">
    <forestGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </forestGrp>
  </xsl:template>
  <xsl:template match="FSCONSTRAINTS" mode="tcp">
    <fsConstraints>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fsConstraints>
  </xsl:template>
  <xsl:template match="FSDECL" mode="tcp">
    <fsDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fsDecl>
  </xsl:template>
  <xsl:template match="FSDESCR" mode="tcp">
    <fsDescr>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fsDescr>
  </xsl:template>
  <xsl:template match="FSLIB" mode="tcp">
    <fsLib>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fsLib>
  </xsl:template>
  <xsl:template match="FSDDECL" mode="tcp">
    <fsdDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fsdDecl>
  </xsl:template>
  <xsl:template match="FVLIB" mode="tcp">
    <fvLib>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </fvLib>
  </xsl:template>
  <xsl:template match="GENNAME" mode="tcp">
    <genName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </genName>
  </xsl:template>
  <xsl:template match="GEOGNAME" mode="tcp">
    <geogName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </geogName>
  </xsl:template>
  <xsl:template match="GRAMGRP" mode="tcp">
    <gramGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </gramGrp>
  </xsl:template>
  <xsl:template match="HANDLIST" mode="tcp">
    <handList>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </handList>
  </xsl:template>
  <xsl:template match="HANDSHIFT" mode="tcp">
    <handShift>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </handShift>
  </xsl:template>
  <xsl:template match="HEADITEM" mode="tcp">
    <headItem>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </headItem>
  </xsl:template>
  <xsl:template match="HEADLABEL" mode="tcp">
    <headLabel>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </headLabel>
  </xsl:template>
  <xsl:template match="INODE" mode="tcp">
    <iNode>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </iNode>
  </xsl:template>
  <xsl:template match="INTERPGRP" mode="tcp">
    <interpGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </interpGrp>
  </xsl:template>
  <xsl:template match="JOINGRP" mode="tcp">
    <joinGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </joinGrp>
  </xsl:template>
  <xsl:template match="LACUNAEND" mode="tcp">
    <lacunaEnd>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </lacunaEnd>
  </xsl:template>
  <xsl:template match="LACUNASTART" mode="tcp">
    <lacunaStart>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </lacunaStart>
  </xsl:template>
  <xsl:template match="LANGKNOWN" mode="tcp">
    <langKnown>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </langKnown>
  </xsl:template>
  <xsl:template match="LANGUSAGE" mode="tcp">
    <langUsage>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </langUsage>
  </xsl:template>
  <xsl:template match="LINKGRP" mode="tcp">
    <linkGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </linkGrp>
  </xsl:template>
  <xsl:template match="LISTBIBL">
    <listBibl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </listBibl>
  </xsl:template>
  <xsl:template match="METDECL" mode="tcp">
    <metDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </metDecl>
  </xsl:template>
  <xsl:template match="NAMELINK" mode="tcp">
    <nameLink>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </nameLink>
  </xsl:template>
  <xsl:template match="NOTESSTMT" mode="tcp">
    <notesStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </notesStmt>
  </xsl:template>
  <xsl:template match="OREF" mode="tcp">
    <oRef>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </oRef>
  </xsl:template>
  <xsl:template match="OVAR" mode="tcp">
    <oVar>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </oVar>
  </xsl:template>
  <xsl:template match="OFFSET" mode="tcp">
    <offSet>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </offSet>
  </xsl:template>
  <xsl:template match="ORGDIVN" mode="tcp">
    <orgDivn>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </orgDivn>
  </xsl:template>
  <xsl:template match="ORGNAME" mode="tcp">
    <orgName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </orgName>
  </xsl:template>
  <xsl:template match="ORGTITLE" mode="tcp">
    <orgTitle>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </orgTitle>
  </xsl:template>
  <xsl:template match="ORGTYPE" mode="tcp">
    <orgType>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </orgType>
  </xsl:template>
  <xsl:template match="OTHERFORM" mode="tcp">
    <otherForm>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </otherForm>
  </xsl:template>
  <xsl:template match="PREF" mode="tcp">
    <pRef>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </pRef>
  </xsl:template>
  <xsl:template match="PVAR" mode="tcp">
    <pVar>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </pVar>
  </xsl:template>
  <xsl:template match="PARTICDESC" mode="tcp">
    <particDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </particDesc>
  </xsl:template>
  <xsl:template match="PARTICLINKS" mode="tcp">
    <particLinks>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </particLinks>
  </xsl:template>
  <xsl:template match="PERSNAME" mode="tcp">
    <persName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </persName>
  </xsl:template>
  <xsl:template match="PERSONGRP" mode="tcp">
    <personGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </personGrp>
  </xsl:template>
  <xsl:template match="PLACENAME" mode="tcp">
    <placeName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </placeName>
  </xsl:template>
  <xsl:template match="POSTBOX" mode="tcp">
    <postBox>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </postBox>
  </xsl:template>
  <xsl:template match="POSTCODE" mode="tcp">
    <postCode>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </postCode>
  </xsl:template>
  <xsl:template match="PROFILEDESC" mode="tcp">
    <profileDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </profileDesc>
  </xsl:template>
  <xsl:template match="PROJECTDESC" mode="tcp">
    <projectDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </projectDesc>
  </xsl:template>
  <xsl:template match="PUBPLACE" mode="tcp">
    <pubPlace>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </pubPlace>
  </xsl:template>
  <xsl:template match="RDGGRP" mode="tcp">
    <rdgGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </rdgGrp>
  </xsl:template>
  <xsl:template match="RECORDINGSTMT" mode="tcp">
    <recordingStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </recordingStmt>
  </xsl:template>
  <xsl:template match="REFSDECL" mode="tcp">
    <refsDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </refsDecl>
  </xsl:template>
  <xsl:template match="RESPSTMT" mode="tcp">
    <respStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </respStmt>
  </xsl:template>
  <xsl:template match="REVISIONDESC|revdesc" mode="tcp">
    <revisionDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </revisionDesc>
  </xsl:template>
  <xsl:template match="ROLEDESC" mode="tcp">
    <roleDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </roleDesc>
  </xsl:template>
  <xsl:template match="ROLENAME" mode="tcp">
    <roleName>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </roleName>
  </xsl:template>
  <xsl:template match="SAMPLINGDECL" mode="tcp">
    <samplingDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </samplingDecl>
  </xsl:template>
  <xsl:template match="SCRIPTSTMT" mode="tcp">
    <scriptStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </scriptStmt>
  </xsl:template>
  <xsl:template match="SERIESSTMT" mode="tcp">
    <seriesStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </seriesStmt>
  </xsl:template>
  <xsl:template match="SETTINGDESC" mode="tcp">
    <settingDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </settingDesc>
  </xsl:template>
  <xsl:template match="SOCALLED" mode="tcp">
    <soCalled>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </soCalled>
  </xsl:template>
  <xsl:template match="SOCECSTATUS" mode="tcp">
    <socecStatus>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </socecStatus>
  </xsl:template>
  <xsl:template match="SOURCEDESC" mode="tcp">
    <sourceDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </sourceDesc>
  </xsl:template>
  <xsl:template match="SPANGRP" mode="tcp">
    <spanGrp>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </spanGrp>
  </xsl:template>
  <xsl:template match="STDVALS" mode="tcp">
    <stdVals>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </stdVals>
  </xsl:template>
  <xsl:template match="TAGDOC" mode="tcp">
    <tagDoc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </tagDoc>
  </xsl:template>
  <xsl:template match="TAGUSAGE" mode="tcp">
    <tagUsage>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </tagUsage>
  </xsl:template>
  <xsl:template match="TAGSDECL" mode="tcp">
    <tagsDecl>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </tagsDecl>
  </xsl:template>
  <xsl:template match="TEICORPUS.2" mode="tcp">
    <teiCorpus.2>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </teiCorpus.2>
  </xsl:template>
  <xsl:template match="TEIFSD2" mode="tcp">
    <teiFsd2>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </teiFsd2>
  </xsl:template>
  <xsl:template match="TEIHEADER" mode="tcp">
    <teiHeader>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </teiHeader>
  </xsl:template>
  <xsl:template match="TERMENTRY" mode="tcp">
    <termEntry>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </termEntry>
  </xsl:template>
  <xsl:template match="TEXTCLASS" mode="tcp">
    <textClass>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </textClass>
  </xsl:template>
  <xsl:template match="TEXTDESC" mode="tcp">
    <textDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </textDesc>
  </xsl:template>
  <xsl:template match="TIMERANGE" mode="tcp">
    <timeRange>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </timeRange>
  </xsl:template>
  <xsl:template match="TIMESTRUCT" mode="tcp">
    <timeStruct>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </timeStruct>
  </xsl:template>
  <xsl:template match="TITLEPAGE|tpage" mode="tcp">
    <titlePage>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </titlePage>
  </xsl:template>
  <xsl:template match="TITLEPART" mode="tcp">
    <titlePart>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </titlePart>
  </xsl:template>
  <xsl:template match="TITLESTMT" mode="tcp">
    <titleStmt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </titleStmt>
  </xsl:template>
  <xsl:template match="VALT" mode="tcp">
    <vAlt>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </vAlt>
  </xsl:template>
  <xsl:template match="VDEFAULT" mode="tcp">
    <vDefault>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </vDefault>
  </xsl:template>
  <xsl:template match="VRANGE" mode="tcp">
    <vRange>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </vRange>
  </xsl:template>
  <xsl:template match="VALDESC" mode="tcp">
    <valDesc>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </valDesc>
  </xsl:template>
  <xsl:template match="VALLIST" mode="tcp">
    <valList>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </valList>
  </xsl:template>
  <xsl:template match="VARIANTENCODING" mode="tcp">
    <variantEncoding>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </variantEncoding>
  </xsl:template>
  <xsl:template match="WITDETAIL" mode="tcp">
    <witDetail>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </witDetail>
  </xsl:template>
  <xsl:template match="WITEND" mode="tcp">
    <witEnd>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </witEnd>
  </xsl:template>
  <xsl:template match="WITLIST" mode="tcp">
    <witList>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </witList>
  </xsl:template>
  <xsl:template match="WITSTART" mode="tcp">
    <witStart>
      <xsl:apply-templates mode="tcp" select="@*"/>
      <xsl:apply-templates mode="tcp"/>
    </witStart>
  </xsl:template>
  <xsl:template match="@TEI" mode="tcp">
    <xsl:attribute name="TEI">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJFROM" mode="tcp">
    <xsl:attribute name="adjFrom">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJTO" mode="tcp">
    <xsl:attribute name="adjTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ASSERTEDVALUE" mode="tcp">
    <xsl:attribute name="assertedValue">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@BASETYPE" mode="tcp">
    <xsl:attribute name="baseType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@COPYOF" mode="tcp">
    <xsl:attribute name="copyOf">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@DEPPTR" mode="tcp">
    <xsl:attribute name="depPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYLOC" mode="tcp">
    <xsl:attribute name="entityLoc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYSTD" mode="tcp">
    <xsl:attribute name="entityStd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@FVAL" mode="tcp">
    <xsl:attribute name="fVal">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@GRPPTR" mode="tcp">
    <xsl:attribute name="grpPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@INDEGREE" mode="tcp">
    <xsl:attribute name="inDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@MUTEXCL" mode="tcp">
    <xsl:attribute name="mutExcl">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@OUTDEGREE" mode="tcp">
    <xsl:attribute name="outDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@SAMEAS" mode="tcp">
    <xsl:attribute name="sameAs">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGFUNC" mode="tcp">
    <xsl:attribute name="targFunc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGORDER" mode="tcp">
    <xsl:if test="not(. = 'u')">
      <xsl:attribute name="targOrder">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="TEIHEADER/@TYPE" mode="tcp"/>
  <xsl:template match="@TARGTYPE">
    <xsl:attribute name="targType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGETEND" mode="tcp">
    <xsl:attribute name="targetEnd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VALUETO" mode="tcp">
    <xsl:attribute name="valueTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VARSEQ" mode="tcp">
    <xsl:attribute name="varSeq">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@WSCALE" mode="tcp">
    <xsl:attribute name="wScale">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TEIFORM" mode="tcp"/>
  <xsl:template match="@OPT" mode="tcp">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="opt">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@TO" mode="tcp">
    <xsl:if test="not(. = 'DITTO')">
      <xsl:attribute name="to">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@DEFAULT" mode="tcp">
    <xsl:if test="not(. = 'no')">
      <xsl:attribute name="default">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@PART" mode="tcp">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="part">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FULL" mode="tcp">
    <xsl:if test="not(. = 'yes')">
      <xsl:attribute name="full">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FROM" mode="tcp">
    <xsl:if test="not(. = 'ROOT')">
      <xsl:attribute name="from">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@STATUS" mode="tcp">
    <xsl:choose>
      <xsl:when test="parent::TEIHEADER">
        <xsl:if test="not(. = 'new')">
          <xsl:attribute name="status">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:if>
      </xsl:when>
      <xsl:when test="parent::DEL">
        <xsl:if test="not(. = 'unremarkable')">
          <xsl:attribute name="status">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="status">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@PLACE" mode="tcp">
    <xsl:if test="not(. = 'unspecified')">
      <xsl:attribute name="place">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@SAMPLE" mode="tcp">
    <xsl:if test="not(. = 'complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@ORG" mode="tcp">
    <xsl:if test="not(. = 'uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="ENCDESC" mode="tcp">
    <encodingDesc>
      <xsl:apply-templates mode="tcp" select="*|@*|processing-instruction()|comment()|text()"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="EDSTMT" mode="tcp">
    <editorialStmt>
      <xsl:apply-templates mode="tcp" select="*|@*|processing-instruction()|comment()|text()"/>
    </editorialStmt>
  </xsl:template>
  <xsl:template match="TITLSTMT" mode="tcp">
    <titleStmt>
      <xsl:apply-templates mode="tcp" select="*|@*|processing-instruction()|comment()|text()"/>
    </titleStmt>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc><p>
	a) if there is no @n, just @unit   == marginal note
	b) if there is no @unit, just a @n, and the @n is not numeric of some kind  == marginal note, @type='milestone'

	c) if @unit is from a closed list of words (page, line, folio), it
	seems editorial, add as subtype on @note

	d) otherwise, make a  label from @unit + @n, and put in a
	marginal note

Dodgy values for @n:
*
*,
Answ.
Answer.
Answere.
Answere:
Arte.
A☜
Chorus.
Conclus.
Correction.
Doctrine.
E. &amp; 116. f. &amp;c.
Explana∣tion.
Maior.
Minor
Minor.
Nature.
Note:
Obiect.
Obiection.
Practise.
Probation
Probation.
Prouerbe.
Quest.
Question.
Question:
Temperatur
Temperature and Vertue.
Temperature.
The Cure.
The Texte.
The cause.
The reasone
The signe.
The text.
The texte
The texte,
The texte.
The texte:
Verse
Verse.
Vertue.
Vse.
chorus
ibid
ibid.
prouerb.
&amp;.

</p>
</desc>
</doc>
  <xsl:template match="MILESTONE" mode="tcp">
    <xsl:choose>
      <xsl:when test="@UNIT and (not(@N) or @N='')">
	<note place="margin">
	  <xsl:value-of select="@UNIT"/>
	</note>
      </xsl:when>
      <xsl:when test="not(@UNIT) and @N">
	<note place="margin" type="milestone">
	  <xsl:value-of select="@N"/>
	</note>
      </xsl:when>
      <xsl:when test="@UNIT='unspec' and @N">
	<note place="margin" type="milestone">
	  <xsl:value-of select="@N"/>
	</note>
      </xsl:when>
      <!-- this short list seem like editorial words. are there more? -->
      <xsl:when test="@UNIT='date' or @UNIT='folio' or  @UNIT='line'">
	<note place="margin" type="milestone" subtype="{@UNIT}">
<xsl:message>Milestone 1: <xsl:value-of select="@UNIT"/>/<xsl:value-of select="@N"/></xsl:message>
	  <xsl:value-of select="@N"/>
	</note>
      </xsl:when>
      <xsl:otherwise>
<xsl:message>Milestone 2: <xsl:value-of select="@UNIT"/><xsl:text> </xsl:text><xsl:value-of select="@N"/></xsl:message>
	<note place="margin" type="milestone">
	  <label>
	    <xsl:value-of select="@UNIT"/>
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="@N"/>
	  </label>
	</note>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@TYPE" mode="tcp">
    <xsl:if test="not(.='')">
      <xsl:attribute name="type">
	<xsl:value-of select="translate(translate(.,'(','_'),$intype,'')"/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="@UNIT" mode="tcp">
    <xsl:attribute name="unit">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <!-- =============== p4top5 =================== -->
  <xsl:template match="*">
    <xsl:choose>
      <xsl:when test="namespace-uri()=''">
        <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="{local-name(.)}">
          <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@*|processing-instruction()|comment()">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="text()">
    <xsl:analyze-string regex="([^∣]*)∣" select=".">
      <xsl:matching-substring>
	<xsl:value-of select="regex-group(1)"/>
	<lb  xmlns="http://www.tei-c.org/ns/1.0" rend="hidden" type="hyphenInWord"/>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
	<xsl:value-of select="."/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
  <xsl:template match="teiCorpus.2">
    <teiCorpus xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </teiCorpus>
  </xsl:template>
  <xsl:template match="witness/@sigil">
    <xsl:attribute name="xml:id">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="witList">
    <listWit xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </listWit>
  </xsl:template>
  <xsl:template match="TEI.2">
    <TEI xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </TEI>
  </xsl:template>
  <xsl:template match="xref">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="ref">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="xptr">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="ptr">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="figure[@url]">
    <figure xmlns="http://www.tei-c.org/ns/1.0">
      <graphic>
        <xsl:apply-templates select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  <xsl:template match="figure/@entity"/>
  <xsl:template match="figure[@entity]">
    <figure xmlns="http://www.tei-c.org/ns/1.0">
      <graphic>
        <xsl:attribute name="url">
          <xsl:choose>
            <xsl:when test="unparsed-entity-uri(@entity)=''">
              <xsl:text>ENTITY_</xsl:text>
              <xsl:value-of select="@entity"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="unparsed-entity-uri(@entity)"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <xsl:apply-templates select="@*"/>
      </graphic>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  <xsl:template match="event">
    <incident xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </incident>
  </xsl:template>
  <xsl:template match="state">
    <refState xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </refState>
  </xsl:template>
  <!-- lost elements -->
  <xsl:template match="dateRange">
    <date xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </date>
  </xsl:template>
  <xsl:template match="dateRange/@from">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="dateRange/@to">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="language">
    <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="language">
      <xsl:if test="@id">
        <xsl:attribute name="ident">
          <xsl:value-of select="@id"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  <!-- attributes lost -->
  <!-- dropped from TEI. Added as new change records later -->
  <xsl:template match="@date.created"/>
  <xsl:template match="@date.updated"/>
  <!-- dropped from TEI. No replacement -->
  <xsl:template match="refsDecl/@doctype"/>
  <!-- attributes changed name -->
  <xsl:template match="date/@value">
    <xsl:attribute name="when">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="xref/@url">
    <xsl:attribute name="target">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="xptr/@url">
    <xsl:attribute name="target">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="figure/@url">
    <xsl:attribute name="url">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@doc">
    <xsl:attribute name="target">
      <xsl:value-of select="unparsed-entity-uri(.)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@id">
    <xsl:choose>
      <xsl:when test="parent::lang">
        <xsl:attribute name="ident">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="xml:id">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@lang">
    <xsl:attribute name="xml:lang">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="change/@date"/>
  <xsl:template match="date/@certainty">
    <xsl:attribute name="cert">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <!-- all pointing attributes preceded by # -->
  <xsl:template match="variantEncoding/@location">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="@ana|@active|@adj|@adjFrom|@adjTo|@children|@class|@code|@copyOf|@corresp|@decls|@domains|@end|@exclude|@fVal|@feats|@follow|@hand|@inst|@langKey|@location|@mergedin|@new|@next|@old|@origin|@otherLangs|@parent|@passive|@perf|@prev|@render|@resp|@sameAs|@scheme|@script|@select|@since|@start|@synch|@target|@targetEnd|@value|@value|@who|@wit">
    <xsl:attribute name="{name(.)}">
      <xsl:call-template name="splitter">
        <xsl:with-param name="val">
          <xsl:value-of select="."/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:attribute>
  </xsl:template>
  <xsl:template name="splitter">
    <xsl:param name="val"/>
    <xsl:choose>
      <xsl:when test="contains($val,' ')">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="substring-before($val,' ')"/>
        <xsl:text> </xsl:text>
        <xsl:call-template name="splitter">
          <xsl:with-param name="val">
            <xsl:value-of select="substring-after($val,' ')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="starts-with($val,'http')">
        <xsl:value-of select="$val"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>#</xsl:text>
        <xsl:value-of select="$val"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- fool around with selected elements -->
  <!-- imprint is no longer allowed inside bibl -->
  <xsl:template match="bibl/imprint">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="editionStmt/editor">
    <respStmt xmlns="http://www.tei-c.org/ns/1.0">
      <resp>
        <xsl:value-of select="@role"/>
      </resp>
      <name>
        <xsl:apply-templates/>
      </name>
    </respStmt>
  </xsl:template>
  <!-- header -->
  <xsl:template match="teiHeader">
    <teiHeader xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      <xsl:if test="not(revisionDesc) and (@date.created or @date.updated)">
        <revisionDesc>
          <xsl:if test="@date.updated">
            <change>&gt;
	    <label>updated</label>
	    <date><xsl:value-of select="@date.updated"/></date>
	    <label>Date edited</label>
	    </change>
          </xsl:if>
          <xsl:if test="@date.created">
            <change>
              <label>created</label>
              <date>
                <xsl:value-of select="@date.created"/>
              </date>
              <label>Date created</label>
            </change>
          </xsl:if>
        </revisionDesc>
      </xsl:if>
    </teiHeader>
  </xsl:template>
  <xsl:template match="revisionDesc">
    <revisionDesc xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
    </revisionDesc>
  </xsl:template>
  <!-- space does not have @extent any more -->
  <xsl:template match="space/@extent">
    <xsl:attribute name="quantity">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <!-- tagsDecl has a compulsory namespace child now -->
  <xsl:template match="tagsDecl">
    <xsl:if test="*">
      <tagsDecl xmlns="http://www.tei-c.org/ns/1.0">
        <namespace name="http://www.tei-c.org/ns/1.0">
          <xsl:apply-templates select="*|comment()|processing-instruction"/>
        </namespace>
      </tagsDecl>
    </xsl:if>
  </xsl:template>
  <!-- orgTitle inside orgName? redundant -->
  <xsl:template match="orgName/orgTitle">
    <xsl:apply-templates/>
  </xsl:template>
  <!-- no need for empty <p> in sourceDesc -->
  <xsl:template match="sourceDesc/p[string-length(.)=0]"/>
  <xsl:template match="gap/@desc"/>
  <xsl:template match="gap">
    <gap xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="@*"/>
      <xsl:if test="@desc">
        <desc>
          <xsl:value-of select="@desc"/>
        </desc>
      </xsl:if>
    </gap>
  </xsl:template>
  <!--  creating a choice element -->
  <xsl:template match="corr[@sic]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <corr>
        <xsl:value-of select="text()"/>
      </corr>
      <sic>
        <xsl:value-of select="@sic"/>
      </sic>
    </choice>
  </xsl:template>
  <xsl:template match="sic[@corr]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <sic>
        <xsl:apply-templates/>
      </sic>
      <corr>
        <xsl:value-of select="@corr"/>
      </corr>
    </choice>
  </xsl:template>
  <xsl:template match="orig[@reg]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <orig>
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </orig>
      <reg>
        <xsl:value-of select="@reg"/>
      </reg>
    </choice>
  </xsl:template>
  <xsl:template match="reg[@orig]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <reg>
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </reg>
      <orig>
        <xsl:value-of select="@orig"/>
      </orig>
    </choice>
  </xsl:template>
  <xsl:template match="@orig|@reg"/>
  <xsl:template match="abbr[@expan]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <abbr>
        <xsl:apply-templates/>
      </abbr>
      <expan>
        <xsl:value-of select="@expan"/>
      </expan>
    </choice>
  </xsl:template>
  <xsl:template match="expan[@abbr]">
    <choice xmlns="http://www.tei-c.org/ns/1.0">
      <expan>
        <xsl:apply-templates/>
      </expan>
      <abbr>
        <xsl:value-of select="@abbr"/>
      </abbr>
    </choice>
  </xsl:template>
  <!-- special consideration for <change> element -->
  <xsl:template match="change">
    <change xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="item/@*"/>
      <xsl:apply-templates select="date"/>
      <xsl:if test="respStmt/resp">
        <label>
          <xsl:value-of select="respStmt/resp/text()"/>
        </label>
      </xsl:if>
      <xsl:for-each select="respStmt/name">
        <name>
          <xsl:apply-templates select="@*|*|comment()|processing-instruction()|text()"/>
        </name>
      </xsl:for-each>
      <xsl:for-each select="item">
        <xsl:apply-templates select="*|comment()|processing-instruction()|text()"/>
      </xsl:for-each>
    </change>
  </xsl:template>
  <xsl:template match="respStmt[resp]">
    <respStmt xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:choose>
        <xsl:when test="resp/name">
          <resp>
            <xsl:value-of select="resp/text()"/>
          </resp>
          <xsl:for-each select="resp/name">
            <name>
              <xsl:apply-templates/>
            </name>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
          <name>
	  </name>
        </xsl:otherwise>
      </xsl:choose>
    </respStmt>
  </xsl:template>
  <xsl:template match="q/@direct"/>
  <!-- if we are reading the P4 with a DTD,
       we need to avoid copying the default values
       of attributes -->
  <xsl:template match="@targOrder">
    <xsl:if test="not(lower-case(.) ='u')">
      <xsl:attribute name="targOrder">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@opt">
    <xsl:if test="not(lower-case(.) ='n')">
      <xsl:attribute name="opt">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@to">
    <xsl:if test="not(lower-case(.) ='ditto')">
      <xsl:attribute name="to">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@default">
    <xsl:choose>
      <xsl:when test="lower-case(.)= 'no'"/>
      <xsl:otherwise>
        <xsl:attribute name="default">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@part">
    <xsl:if test="not(lower-case(.) ='n')">
      <xsl:attribute name="part">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@full">
    <xsl:if test="not(lower-case(.) ='yes')">
      <xsl:attribute name="full">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@from">
    <xsl:if test="not(lower-case(.) ='root')">
      <xsl:attribute name="from">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@status">
    <xsl:choose>
      <xsl:when test="parent::teiHeader">
        <xsl:if test="not(lower-case(.) ='new')">
          <xsl:attribute name="status">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:if>
      </xsl:when>
      <xsl:when test="parent::del">
        <xsl:if test="not(lower-case(.) ='unremarkable')">
          <xsl:attribute name="status">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="status">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@place">
    <xsl:if test="not(lower-case(.) ='unspecified')">
      <xsl:attribute name="place">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@sample">
    <xsl:if test="not(lower-case(.) ='complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@org">
    <xsl:if test="not(lower-case(.) ='uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="teiHeader/@type">
    <xsl:if test="not(lower-case(.) ='text')">
      <xsl:attribute name="type">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <!-- yes|no to boolean -->
  <xsl:template match="@anchored">
    <xsl:attribute name="anchored">
      <xsl:choose>
        <xsl:when test="lower-case(.)='yes'">true</xsl:when>
        <xsl:when test="lower-case(.)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="sourceDesc/@default"/>
  <xsl:template match="@tei">
    <xsl:attribute name="tei">
      <xsl:choose>
        <xsl:when test="lower-case(.)='yes'">true</xsl:when>
        <xsl:when test="lower-case(.)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@langKey"/>
  <xsl:template match="@TEIform"/>
  <!-- assorted atts -->
  <xsl:template match="gi/@TEI">
    <xsl:if test=".='yes'">
      <xsl:attribute name="scheme">TEI</xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@old"/>

  <xsl:template match="ref/@from"/>
  <xsl:template match="@mergedin">
    <xsl:attribute name="mergedIn">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <!-- deal with the loss of div0 -->
  <xsl:template match="div1|div2|div3|div4|div5|div6">
    <xsl:variable name="divName">
      <xsl:choose>
        <xsl:when test="ancestor::div0">
          <xsl:text>div</xsl:text>
          <xsl:value-of select="number(substring-after(local-name(.),'div')) + 1"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="local-name()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$divName}" namespace="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="div0">
    <div1 xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </div1>
  </xsl:template>

  <!-- remove default values for attributes -->
  <xsl:template match="row/@role[.='data']"/>
  <xsl:template match="cell/@role[.='data']"/>
  <xsl:template match="cell/@rows[.='1']"/>
  <xsl:template match="cell/@cols[.='1']"/>
  <xsl:template match="q/@broken[.='no']"/>


  <xsl:template match="encodingDesc/projectDesc">
    <projectDesc xmlns="http://www.tei-c.org/ns/1.0">
      <p>Created by converting TCP files to TEI P5 using tcp2tei.xsl,
      TEI @ Oxford.
      </p>
    </projectDesc>
  </xsl:template>
  <xsl:template match="fileDesc/publicationStmt">
    <publicationStmt  xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:for-each select="idno">
	<idno>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates/>
	</idno>
      </xsl:for-each>
    </publicationStmt>
  </xsl:template>

  <xsl:template match="closer[postscript[not(following-sibling::*)]]">
    <closer xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="text()|*[not(self::postscript)]"/>
    </closer>
    <xsl:apply-templates select="postscript"/>
  </xsl:template>

  <xsl:template match="head/stage">
    <hi rend="stage"  xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:apply-templates/>
    </hi>
  </xsl:template>

  <xsl:template match="closer[postscript[following-sibling::*]]">
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="figDesc/hi[@rend='sup']">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="makeID"/>
  <xsl:template name="idnoHook"/>
	
</xsl:stylesheet>

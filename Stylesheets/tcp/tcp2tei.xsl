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
      <xsl:apply-templates mode="ecco"/>
    </xsl:variable>
    <xsl:for-each select="$phase1">
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>
  <!-- ECCO copy -->
  <xsl:template match="@*" mode="ecco">
    <xsl:attribute name="{lower-case(local-name(.))}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="processing-instruction()|comment()|text()" mode="ecco">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="*" mode="ecco">
    <xsl:element name="{lower-case(local-name(.))}">
      <xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </xsl:element>
  </xsl:template>

  <!-- ECCO discards -->
  <xsl:template match="FIGDESC/HI" mode="ecco">
    <xsl:apply-templates mode="ecco"/>
  </xsl:template>
  <xsl:template match="PB/@MS" mode="ecco"/>
  <xsl:template match="LABEL/@ROLE" mode="ecco"/>
  <xsl:template match="TITLE/@TYPE" mode="ecco"/>
  <xsl:template match="TEMPHEAD" mode="ecco"/>
  <xsl:template match="TITLE/@I2" mode="ecco"/>
  <xsl:template match="IDG" mode="ecco"/>
  <xsl:template match="@LANG[.='eng dut']" mode="ecco"/>
  <xsl:template match="@LANG[.='eng san']" mode="ecco"/>
  <xsl:template match="@LANG[.='fre eng']" mode="ecco"/>
  <xsl:template match="@LANG[.='lat eng']" mode="ecco"/>
  <xsl:template match="@LANG[.='lat gre']" mode="ecco"/>
  <xsl:template match="@PLACE[.='marg']" mode="ecco">
    <xsl:attribute name="place">margin</xsl:attribute>
  </xsl:template>
  <!-- ECCO controversial changes -->
  <xsl:template match="FIGURE/SIGNED" mode="ecco">
    <ab type="signed">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </ab>
  </xsl:template>
  <!--
      <xsl:template match="HEAD/L" mode="ecco">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/><lb/>
      </xsl:template>
      <xsl:template match="TRAILER/L" mode="ecco">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/><lb/>
      </xsl:template>
  -->
  <xsl:template match="HEAD[@TYPE='sub'][Q/L and not(P)]" mode="ecco">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
  </xsl:template>
  <xsl:template match="HEAD[@TYPE='sub']/Q[L]" mode="ecco">
      <epigraph>
	<xsl:apply-templates
	    select="*|processing-instruction()|comment()|text()"
	    mode="ecco"/>
      </epigraph>
  </xsl:template>
  <xsl:template match="P[not(parent::SP) and count(*)=1 and not(text()) and (LETTER or
		       LIST or TABLE or FIGURE)]" mode="ecco">
	<xsl:apply-templates select="*|processing-instruction()|comment()"
	    mode="ecco"/>
  </xsl:template>
  <xsl:template match="CELL[count(*)=1 and not(text()) and P]" mode="ecco">
    <cell>
	<xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:for-each select="p">
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()"
	    mode="ecco"/>
      </xsl:for-each>
    </cell>
</xsl:template>

  <xsl:template match="NOTE[count(*)=1 and not(text())]/Q" mode="ecco">
	<xsl:apply-templates
	    select="*|processing-instruction()|comment()|text()"
	    mode="ecco"/>
  </xsl:template>
  <xsl:template match="Q[count(*)=1 and not(text()) and LG/L]"
		mode="ecco" priority="10">
    <q>
      <xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:for-each select="lg">
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()"
			     mode="ecco"/>
      </xsl:for-each>
    </q>
  </xsl:template>

  <xsl:template match="TITLESTMT/TITLE/text()[last()]" mode="ecco">
    <xsl:choose>
      <xsl:when test="matches(.,':$')">
	<xsl:value-of select="substring(.,1,string-length(.)-1)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="HEADNOTE[P/FIGURE]" mode="ecco">
      <xsl:apply-templates mode="ecco"/>
  </xsl:template>

  <xsl:template match="HEADNOTE" mode="ecco">
    <argument>
      <xsl:apply-templates mode="ecco"/>
    </argument>
  </xsl:template>

  <xsl:template match="TAILNOTE" mode="ecco">
    <argument>
      <xsl:apply-templates mode="ecco"/>
    </argument>
  </xsl:template>

  <xsl:template match="STAGE/STAGE" mode="ecco">
      <xsl:apply-templates mode="ecco"/>
  </xsl:template>

<!--
  <xsl:template match="CELL/L" mode="ecco">
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/><lb/>
  </xsl:template>
  <xsl:template match="CELL/LG" mode="ecco">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
  </xsl:template>
  <xsl:template match="CELL/LG/L" mode="ecco">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/><lb/>
  </xsl:template>
-->
  <!-- ECCO non-controversial transforms -->
  <xsl:template match="ROW/PB" mode="ecco"/>
  <xsl:template match="ROW[PB]" mode="ecco">
    <row>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" mode="ecco"/>
    </row>
    <xsl:for-each select="PB">
      <pb>
	<xsl:apply-templates select="@*" mode="ecco"/>
      </pb>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="EEBO" mode="ecco">
    <xsl:apply-templates select="*" mode="ecco"/>
  </xsl:template>

  <xsl:template match="ETS" mode="ecco">
    <TEI.2>
      <xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:for-each select="document(concat($ID,'.hdr'),$HERE)">
        <xsl:apply-templates select="*" mode="ecco"/>
      </xsl:for-each>
      <xsl:apply-templates mode="ecco"/>
    </TEI.2>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT" mode="ecco">
    <publicationStmt>
      <xsl:apply-templates select="*" mode="ecco"/>
      <xsl:if test="parent::FILEDESC">
	<xsl:call-template name="makeID"/>
	<xsl:for-each select="$HERE">
	  <xsl:for-each select="/ETS/EEBO/IDG">
	    <idno type="TCP"><xsl:value-of select="@ID"/></idno>
	    <idno type="BIBNO"><xsl:value-of select="BIBNO"/></idno>
	    <xsl:for-each select="VID">
	      <idno type="{@SET}">
		<xsl:value-of select="."/>
	      </idno>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:for-each>
	<xsl:call-template name="idnoHook"/>
      </xsl:if>
    </publicationStmt>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT/IDNO"  mode="ecco"/>
  <xsl:template match="FILEDESC/EXTENT" mode="ecco"/>

  <xsl:template match="EEBO/GROUP" mode="ecco">
    <text>
      <group>
        <xsl:apply-templates select="@*" mode="ecco"/>
        <xsl:apply-templates select="*" mode="ecco"/>
      </group>
    </text>
  </xsl:template>
  <xsl:template match="LETTER" mode="ecco">
    <floatingText type="letter">
      <body>
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
      </body>
    </floatingText>
  </xsl:template>
  <xsl:template match="TEXT" mode="ecco">
    <xsl:choose>
      <xsl:when test="parent::ETS or parent::EEBO or parent::GROUP">
        <text>
          <xsl:apply-templates select="@*" mode="ecco"/>
          <xsl:apply-templates select="*" mode="ecco"/>
        </text>
      </xsl:when>
      <xsl:otherwise>
        <floatingText>
          <xsl:apply-templates select="@*" mode="ecco"/>
          <xsl:apply-templates select="*" mode="ecco"/>
        </floatingText>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="DIV2|DIV3|DIV4|DIV5|DIV6|DIV7" mode="ecco">
    <div>
      <xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:apply-templates select="*" mode="ecco"/>
    </div>
  </xsl:template>
  <xsl:template match="DIV1" mode="ecco">
    <xsl:choose>
      <xsl:when test="count(parent::BODY/*)=1">
        <xsl:apply-templates select="*" mode="ecco"/>
      </xsl:when>
      <xsl:otherwise>
        <div>
          <xsl:apply-templates select="@*" mode="ecco"/>
          <xsl:apply-templates select="*" mode="ecco"/>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="LANGUSAGE/@ID" mode="ecco"/>
  <xsl:template match="LANGUAGE[not(@ID)]" mode="ecco">
    <language id="{../@ID}">
      <xsl:apply-templates select="@*|text()" mode="ecco"/>
    </language>
  </xsl:template>
  <xsl:template match="GAP/@DISP" mode="ecco">
    <xsl:attribute name="rend">
      <xsl:text>content:</xsl:text>
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="ITEM/@ROLE" mode="ecco">
    <xsl:attribute name="rend">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="PB/@REF" mode="ecco">
    <xsl:attribute name="facs">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="rend">
      <xsl:text>none</xsl:text>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="NOTE[@PLACE='foot;']" mode="ecco">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='foor']" mode="ecco">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='foot']" mode="ecco">
    <note place="bottom">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='‖']" mode="ecco">
    <note n="‖">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='‡']" mode="ecco">
    <note n="‡">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="NOTE[@PLACE='†']" mode="ecco">
    <note n="†">
      <xsl:apply-templates select="@*[not(name()='PLACE')]" mode="ecco"/>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </note>
  </xsl:template>
  <xsl:template match="KEYWORDS" mode="ecco">
    <keywords>
      <xsl:if test="not(@SCHEME)">
        <xsl:attribute name="scheme">
          <xsl:text>http://authorities.loc.gov/</xsl:text>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="ecco"/>
    </keywords>
  </xsl:template>
  <xsl:template match="SUP" mode="ecco">
    <hi rend="sup">
      <xsl:apply-templates mode="ecco"/>
    </hi>
  </xsl:template>
  <xsl:template match="SUB" mode="ecco">
    <hi rend="sub">
      <xsl:apply-templates mode="ecco"/>
    </hi>
  </xsl:template>
  <xsl:template match="BELOW" mode="ecco">
    <hi rend="below">
      <xsl:apply-templates mode="ecco"/>
    </hi>
  </xsl:template>
  <xsl:template match="ABOVE" mode="ecco">
    <hi rend="above">
      <xsl:apply-templates mode="ecco"/>
    </hi>
  </xsl:template>
  <xsl:template match="HEADER" mode="ecco">
    <teiHeader>
      <xsl:apply-templates select="@*" mode="ecco"/>
      <xsl:apply-templates select="*" mode="ecco"/>
    </teiHeader>
  </xsl:template>

  <xsl:template match="TEI.2|OTA" mode="ecco">
    <TEI.2>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </TEI.2>
  </xsl:template>
  <xsl:template match="ADDNAME" mode="ecco">
    <addName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </addName>
  </xsl:template>
  <xsl:template match="ADDSPAN" mode="ecco">
    <addSpan>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </addSpan>
  </xsl:template>
  <xsl:template match="ADDRLINE" mode="ecco">
    <addrLine>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </addrLine>
  </xsl:template>
  <xsl:template match="ALTGRP" mode="ecco">
    <altGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </altGrp>
  </xsl:template>
  <xsl:template match="ATTDEF" mode="ecco">
    <attDef>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </attDef>
  </xsl:template>
  <xsl:template match="ATTLIST" mode="ecco">
    <attList>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </attList>
  </xsl:template>
  <xsl:template match="ATTNAME" mode="ecco">
    <attName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </attName>
  </xsl:template>
  <xsl:template match="ATTLDECL" mode="ecco">
    <attlDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </attlDecl>
  </xsl:template>
  <xsl:template match="BASEWSD" mode="ecco">
    <baseWsd>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </baseWsd>
  </xsl:template>
  <xsl:template match="BIBLFULL" mode="ecco">
    <biblFull>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </biblFull>
  </xsl:template>
  <xsl:template match="BIBLSCOPE" mode="ecco">
    <biblScope>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </biblScope>
  </xsl:template>
  <xsl:template match="BIBLSTRUCT" mode="ecco">
    <biblStruct>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </biblStruct>
  </xsl:template>
  <xsl:template match="CASTGROUP" mode="ecco">
    <castGroup>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </castGroup>
  </xsl:template>
  <xsl:template match="CASTITEM" mode="ecco">
    <castItem>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </castItem>
  </xsl:template>
  <xsl:template match="CASTLIST" mode="ecco">
    <castList>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </castList>
  </xsl:template>
  <xsl:template match="CATDESC" mode="ecco">
    <catDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </catDesc>
  </xsl:template>
  <xsl:template match="CATREF" mode="ecco">
    <catRef>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </catRef>
  </xsl:template>
  <xsl:template match="CLASSCODE" mode="ecco">
    <classCode>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </classCode>
  </xsl:template>
  <xsl:template match="CLASSDECL" mode="ecco">
    <classDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </classDecl>
  </xsl:template>
  <xsl:template match="CLASSDOC" mode="ecco">
    <classDoc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </classDoc>
  </xsl:template>
  <xsl:template match="CODEDCHARSET" mode="ecco">
    <codedCharSet>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </codedCharSet>
  </xsl:template>
  <xsl:template match="DATADESC" mode="ecco">
    <dataDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </dataDesc>
  </xsl:template>
  <xsl:template match="DATERANGE" mode="ecco">
    <dateRange>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </dateRange>
  </xsl:template>
  <xsl:template match="DATESTRUCT" mode="ecco">
    <dateStruct>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </dateStruct>
  </xsl:template>
  <xsl:template match="DELSPAN" mode="ecco">
    <delSpan>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </delSpan>
  </xsl:template>
  <xsl:template match="DIVGEN" mode="ecco">
    <divGen>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </divGen>
  </xsl:template>
  <xsl:template match="DOCAUTHOR|DAUTHOR" mode="ecco">
    <docAuthor>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </docAuthor>
  </xsl:template>
  <xsl:template match="DOCDATE" mode="ecco">
    <docDate>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </docDate>
  </xsl:template>
  <xsl:template match="DOCEDITION" mode="ecco">
    <docEdition>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </docEdition>
  </xsl:template>
  <xsl:template match="DOCIMPRINT" mode="ecco">
    <docImprint>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </docImprint>
  </xsl:template>
  <xsl:template match="DOCTITLE|DTITLE" mode="ecco">
    <docTitle>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </docTitle>
  </xsl:template>
  <xsl:template match="ELEAF" mode="ecco">
    <eLeaf>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </eLeaf>
  </xsl:template>
  <xsl:template match="ETREE" mode="ecco">
    <eTree>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </eTree>
  </xsl:template>
  <xsl:template match="EDITIONSTMT" mode="ecco">
    <editionStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </editionStmt>
  </xsl:template>
  <xsl:template match="EDITORIALDECL" mode="ecco">
    <editorialDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </editorialDecl>
  </xsl:template>
  <xsl:template match="ELEMDECL" mode="ecco">
    <elemDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </elemDecl>
  </xsl:template>
  <xsl:template match="ENCODINGDESC" mode="ecco">
    <encodingDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="ENTDOC" mode="ecco">
    <entDoc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </entDoc>
  </xsl:template>
  <xsl:template match="ENTNAME" mode="ecco">
    <entName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </entName>
  </xsl:template>
  <xsl:template match="ENTITYSET" mode="ecco">
    <entitySet>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </entitySet>
  </xsl:template>
  <xsl:template match="ENTRYFREE" mode="ecco">
    <entryFree>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </entryFree>
  </xsl:template>
  <xsl:template match="EXTFIGURE" mode="ecco">
    <extFigure>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </extFigure>
  </xsl:template>
  <xsl:template match="FALT" mode="ecco">
    <fAlt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fAlt>
  </xsl:template>
  <xsl:template match="FDECL" mode="ecco">
    <fDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fDecl>
  </xsl:template>
  <xsl:template match="FDESCR" mode="ecco">
    <fDescr>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fDescr>
  </xsl:template>
  <xsl:template match="FLIB" mode="ecco">
    <fLib>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fLib>
  </xsl:template>
  <xsl:template match="FIGDESC" mode="ecco">
    <figDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </figDesc>
  </xsl:template>
  <xsl:template match="FILEDESC" mode="ecco">
    <fileDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fileDesc>
  </xsl:template>
  <xsl:template match="FIRSTLANG" mode="ecco">
    <firstLang>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </firstLang>
  </xsl:template>
  <xsl:template match="FORENAME" mode="ecco">
    <foreName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </foreName>
  </xsl:template>
  <xsl:template match="FORESTGRP" mode="ecco">
    <forestGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </forestGrp>
  </xsl:template>
  <xsl:template match="FSCONSTRAINTS" mode="ecco">
    <fsConstraints>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fsConstraints>
  </xsl:template>
  <xsl:template match="FSDECL" mode="ecco">
    <fsDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fsDecl>
  </xsl:template>
  <xsl:template match="FSDESCR" mode="ecco">
    <fsDescr>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fsDescr>
  </xsl:template>
  <xsl:template match="FSLIB" mode="ecco">
    <fsLib>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fsLib>
  </xsl:template>
  <xsl:template match="FSDDECL" mode="ecco">
    <fsdDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fsdDecl>
  </xsl:template>
  <xsl:template match="FVLIB" mode="ecco">
    <fvLib>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </fvLib>
  </xsl:template>
  <xsl:template match="GENNAME" mode="ecco">
    <genName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </genName>
  </xsl:template>
  <xsl:template match="GEOGNAME" mode="ecco">
    <geogName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </geogName>
  </xsl:template>
  <xsl:template match="GRAMGRP" mode="ecco">
    <gramGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </gramGrp>
  </xsl:template>
  <xsl:template match="HANDLIST" mode="ecco">
    <handList>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </handList>
  </xsl:template>
  <xsl:template match="HANDSHIFT" mode="ecco">
    <handShift>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </handShift>
  </xsl:template>
  <xsl:template match="HEADITEM" mode="ecco">
    <headItem>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </headItem>
  </xsl:template>
  <xsl:template match="HEADLABEL" mode="ecco">
    <headLabel>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </headLabel>
  </xsl:template>
  <xsl:template match="INODE" mode="ecco">
    <iNode>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </iNode>
  </xsl:template>
  <xsl:template match="INTERPGRP" mode="ecco">
    <interpGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </interpGrp>
  </xsl:template>
  <xsl:template match="JOINGRP" mode="ecco">
    <joinGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </joinGrp>
  </xsl:template>
  <xsl:template match="LACUNAEND" mode="ecco">
    <lacunaEnd>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </lacunaEnd>
  </xsl:template>
  <xsl:template match="LACUNASTART" mode="ecco">
    <lacunaStart>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </lacunaStart>
  </xsl:template>
  <xsl:template match="LANGKNOWN" mode="ecco">
    <langKnown>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </langKnown>
  </xsl:template>
  <xsl:template match="LANGUSAGE" mode="ecco">
    <langUsage>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </langUsage>
  </xsl:template>
  <xsl:template match="LINKGRP" mode="ecco">
    <linkGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </linkGrp>
  </xsl:template>
  <xsl:template match="LISTBIBL">
    <listBibl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </listBibl>
  </xsl:template>
  <xsl:template match="METDECL" mode="ecco">
    <metDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </metDecl>
  </xsl:template>
  <xsl:template match="NAMELINK" mode="ecco">
    <nameLink>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </nameLink>
  </xsl:template>
  <xsl:template match="NOTESSTMT" mode="ecco">
    <notesStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </notesStmt>
  </xsl:template>
  <xsl:template match="OREF" mode="ecco">
    <oRef>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </oRef>
  </xsl:template>
  <xsl:template match="OVAR" mode="ecco">
    <oVar>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </oVar>
  </xsl:template>
  <xsl:template match="OFFSET" mode="ecco">
    <offSet>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </offSet>
  </xsl:template>
  <xsl:template match="ORGDIVN" mode="ecco">
    <orgDivn>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </orgDivn>
  </xsl:template>
  <xsl:template match="ORGNAME" mode="ecco">
    <orgName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </orgName>
  </xsl:template>
  <xsl:template match="ORGTITLE" mode="ecco">
    <orgTitle>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </orgTitle>
  </xsl:template>
  <xsl:template match="ORGTYPE" mode="ecco">
    <orgType>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </orgType>
  </xsl:template>
  <xsl:template match="OTHERFORM" mode="ecco">
    <otherForm>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </otherForm>
  </xsl:template>
  <xsl:template match="PREF" mode="ecco">
    <pRef>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </pRef>
  </xsl:template>
  <xsl:template match="PVAR" mode="ecco">
    <pVar>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </pVar>
  </xsl:template>
  <xsl:template match="PARTICDESC" mode="ecco">
    <particDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </particDesc>
  </xsl:template>
  <xsl:template match="PARTICLINKS" mode="ecco">
    <particLinks>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </particLinks>
  </xsl:template>
  <xsl:template match="PERSNAME" mode="ecco">
    <persName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </persName>
  </xsl:template>
  <xsl:template match="PERSONGRP" mode="ecco">
    <personGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </personGrp>
  </xsl:template>
  <xsl:template match="PLACENAME" mode="ecco">
    <placeName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </placeName>
  </xsl:template>
  <xsl:template match="POSTBOX" mode="ecco">
    <postBox>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </postBox>
  </xsl:template>
  <xsl:template match="POSTCODE" mode="ecco">
    <postCode>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </postCode>
  </xsl:template>
  <xsl:template match="PROFILEDESC" mode="ecco">
    <profileDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </profileDesc>
  </xsl:template>
  <xsl:template match="PROJECTDESC" mode="ecco">
    <projectDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </projectDesc>
  </xsl:template>
  <xsl:template match="PUBPLACE" mode="ecco">
    <pubPlace>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </pubPlace>
  </xsl:template>
  <xsl:template match="RDGGRP" mode="ecco">
    <rdgGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </rdgGrp>
  </xsl:template>
  <xsl:template match="RECORDINGSTMT" mode="ecco">
    <recordingStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </recordingStmt>
  </xsl:template>
  <xsl:template match="REFSDECL" mode="ecco">
    <refsDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </refsDecl>
  </xsl:template>
  <xsl:template match="RESPSTMT" mode="ecco">
    <respStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </respStmt>
  </xsl:template>
  <xsl:template match="REVISIONDESC|revdesc" mode="ecco">
    <revisionDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </revisionDesc>
  </xsl:template>
  <xsl:template match="ROLEDESC" mode="ecco">
    <roleDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </roleDesc>
  </xsl:template>
  <xsl:template match="ROLENAME" mode="ecco">
    <roleName>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </roleName>
  </xsl:template>
  <xsl:template match="SAMPLINGDECL" mode="ecco">
    <samplingDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </samplingDecl>
  </xsl:template>
  <xsl:template match="SCRIPTSTMT" mode="ecco">
    <scriptStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </scriptStmt>
  </xsl:template>
  <xsl:template match="SERIESSTMT" mode="ecco">
    <seriesStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </seriesStmt>
  </xsl:template>
  <xsl:template match="SETTINGDESC" mode="ecco">
    <settingDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </settingDesc>
  </xsl:template>
  <xsl:template match="SOCALLED" mode="ecco">
    <soCalled>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </soCalled>
  </xsl:template>
  <xsl:template match="SOCECSTATUS" mode="ecco">
    <socecStatus>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </socecStatus>
  </xsl:template>
  <xsl:template match="SOURCEDESC" mode="ecco">
    <sourceDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </sourceDesc>
  </xsl:template>
  <xsl:template match="SPANGRP" mode="ecco">
    <spanGrp>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </spanGrp>
  </xsl:template>
  <xsl:template match="STDVALS" mode="ecco">
    <stdVals>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </stdVals>
  </xsl:template>
  <xsl:template match="TAGDOC" mode="ecco">
    <tagDoc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </tagDoc>
  </xsl:template>
  <xsl:template match="TAGUSAGE" mode="ecco">
    <tagUsage>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </tagUsage>
  </xsl:template>
  <xsl:template match="TAGSDECL" mode="ecco">
    <tagsDecl>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </tagsDecl>
  </xsl:template>
  <xsl:template match="TEICORPUS.2" mode="ecco">
    <teiCorpus.2>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </teiCorpus.2>
  </xsl:template>
  <xsl:template match="TEIFSD2" mode="ecco">
    <teiFsd2>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </teiFsd2>
  </xsl:template>
  <xsl:template match="TEIHEADER" mode="ecco">
    <teiHeader>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </teiHeader>
  </xsl:template>
  <xsl:template match="TERMENTRY" mode="ecco">
    <termEntry>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </termEntry>
  </xsl:template>
  <xsl:template match="TEXTCLASS" mode="ecco">
    <textClass>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </textClass>
  </xsl:template>
  <xsl:template match="TEXTDESC" mode="ecco">
    <textDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </textDesc>
  </xsl:template>
  <xsl:template match="TIMERANGE" mode="ecco">
    <timeRange>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </timeRange>
  </xsl:template>
  <xsl:template match="TIMESTRUCT" mode="ecco">
    <timeStruct>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </timeStruct>
  </xsl:template>
  <xsl:template match="TITLEPAGE|tpage" mode="ecco">
    <titlePage>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </titlePage>
  </xsl:template>
  <xsl:template match="TITLEPART" mode="ecco">
    <titlePart>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </titlePart>
  </xsl:template>
  <xsl:template match="TITLESTMT" mode="ecco">
    <titleStmt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </titleStmt>
  </xsl:template>
  <xsl:template match="VALT" mode="ecco">
    <vAlt>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </vAlt>
  </xsl:template>
  <xsl:template match="VDEFAULT" mode="ecco">
    <vDefault>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </vDefault>
  </xsl:template>
  <xsl:template match="VRANGE" mode="ecco">
    <vRange>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </vRange>
  </xsl:template>
  <xsl:template match="VALDESC" mode="ecco">
    <valDesc>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </valDesc>
  </xsl:template>
  <xsl:template match="VALLIST" mode="ecco">
    <valList>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </valList>
  </xsl:template>
  <xsl:template match="VARIANTENCODING" mode="ecco">
    <variantEncoding>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </variantEncoding>
  </xsl:template>
  <xsl:template match="WITDETAIL" mode="ecco">
    <witDetail>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </witDetail>
  </xsl:template>
  <xsl:template match="WITEND" mode="ecco">
    <witEnd>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </witEnd>
  </xsl:template>
  <xsl:template match="WITLIST" mode="ecco">
    <witList>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </witList>
  </xsl:template>
  <xsl:template match="WITSTART" mode="ecco">
    <witStart>
      <xsl:apply-templates mode="ecco" select="@*"/>
      <xsl:apply-templates mode="ecco"/>
    </witStart>
  </xsl:template>
  <xsl:template match="@TEI" mode="ecco">
    <xsl:attribute name="TEI">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJFROM" mode="ecco">
    <xsl:attribute name="adjFrom">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJTO" mode="ecco">
    <xsl:attribute name="adjTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ASSERTEDVALUE" mode="ecco">
    <xsl:attribute name="assertedValue">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@BASETYPE" mode="ecco">
    <xsl:attribute name="baseType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@COPYOF" mode="ecco">
    <xsl:attribute name="copyOf">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@DEPPTR" mode="ecco">
    <xsl:attribute name="depPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYLOC" mode="ecco">
    <xsl:attribute name="entityLoc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYSTD" mode="ecco">
    <xsl:attribute name="entityStd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@FVAL" mode="ecco">
    <xsl:attribute name="fVal">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@GRPPTR" mode="ecco">
    <xsl:attribute name="grpPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@INDEGREE" mode="ecco">
    <xsl:attribute name="inDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@MUTEXCL" mode="ecco">
    <xsl:attribute name="mutExcl">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@OUTDEGREE" mode="ecco">
    <xsl:attribute name="outDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@SAMEAS" mode="ecco">
    <xsl:attribute name="sameAs">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGFUNC" mode="ecco">
    <xsl:attribute name="targFunc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGORDER" mode="ecco">
    <xsl:if test="not(. = 'u')">
      <xsl:attribute name="targOrder">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="TEIHEADER/@TYPE" mode="ecco"/>
  <xsl:template match="@TARGTYPE">
    <xsl:attribute name="targType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGETEND" mode="ecco">
    <xsl:attribute name="targetEnd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VALUETO" mode="ecco">
    <xsl:attribute name="valueTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VARSEQ" mode="ecco">
    <xsl:attribute name="varSeq">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@WSCALE" mode="ecco">
    <xsl:attribute name="wScale">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TEIFORM" mode="ecco"/>
  <xsl:template match="@OPT" mode="ecco">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="opt">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@TO" mode="ecco">
    <xsl:if test="not(. = 'DITTO')">
      <xsl:attribute name="to">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@DEFAULT" mode="ecco">
    <xsl:if test="not(. = 'no')">
      <xsl:attribute name="default">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@PART" mode="ecco">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="part">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FULL" mode="ecco">
    <xsl:if test="not(. = 'yes')">
      <xsl:attribute name="full">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FROM" mode="ecco">
    <xsl:if test="not(. = 'ROOT')">
      <xsl:attribute name="from">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@STATUS" mode="ecco">
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
  <xsl:template match="@PLACE" mode="ecco">
    <xsl:if test="not(. = 'unspecified')">
      <xsl:attribute name="place">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@SAMPLE" mode="ecco">
    <xsl:if test="not(. = 'complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@ORG" mode="ecco">
    <xsl:if test="not(. = 'uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="ENCDESC" mode="ecco">
    <encodingDesc>
      <xsl:apply-templates mode="ecco" select="*|@*|processing-instruction()|comment()|text()"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="EDSTMT" mode="ecco">
    <editorialStmt>
      <xsl:apply-templates mode="ecco" select="*|@*|processing-instruction()|comment()|text()"/>
    </editorialStmt>
  </xsl:template>
  <xsl:template match="TITLSTMT" mode="ecco">
    <titleStmt>
      <xsl:apply-templates mode="ecco" select="*|@*|processing-instruction()|comment()|text()"/>
    </titleStmt>
  </xsl:template>
  <xsl:template match="MILESTONE[not(@UNIT)]" mode="ecco">
    <milestone unit="unknown">
      <xsl:apply-templates mode="ecco" select="@*"/>
    </milestone>
  </xsl:template>
  <xsl:template match="@TYPE" mode="ecco">
    <xsl:attribute name="type">
      <xsl:value-of select="translate(translate(.,'(','_'),$intype,'')"/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="@UNIT" mode="ecco">
    <xsl:attribute name="unit">
      <xsl:value-of select="translate(.,' ','_')"/>
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
    <xsl:analyze-string regex="([^∣]+)∣" select=".">
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
  <!-- start creating the new choice element -->
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
  <!-- from Conal Tuohy -->
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
      <p>unknown</p>
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


   <xsl:template name="makeID"/>
   <xsl:template name="idnoHook"/>
	
</xsl:stylesheet>

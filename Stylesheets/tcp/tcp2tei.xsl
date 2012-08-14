<?xml version="1.0" encoding="utf-8"?>
<!--
$Date$ $Author$

-->
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns="http://www.tei-c.org/ns/1.0" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    exclude-namespace-prefixes="tei"
    version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>XSLT script for cleaning up ECCO texts to P4, then running a
      P4 to P5 conversion</p>
      <p><h1 xmlns="">License</h1>This software is dual-licensed:

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
  <xsl:key name="ROLES" match="P/@ROLE" use="1"/>
  <xsl:key name="ROLES" match="ITEM/@ROLE" use="1"/>
  <xsl:param name="intype"> ',)</xsl:param>
  <xsl:param name="debug">false</xsl:param>
  <xsl:param name="headerDirectory">headers/</xsl:param>
  <xsl:variable name="HERE" select="/"/>
  <xsl:variable name="Rendition">
    <tagsDecl>
      <xsl:for-each-group select="//GAP/@DISP" group-by=".">
	<rendition xml:id="{position()}"><xsl:value-of select="current-grouping-key()"/></rendition>
      </xsl:for-each-group>
    </tagsDecl>
  </xsl:variable>

  <xsl:template match="/">
    <xsl:if test="$debug='true'">
	<xsl:message>processing <xsl:value-of select="base-uri()"/></xsl:message>
    </xsl:if>
      <xsl:apply-templates/>
  </xsl:template>

  <!-- default identity transform -->

  <xsl:template match="*">
    <xsl:choose>
      <xsl:when test="namespace-uri()=''">
        <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="{lower-case(local-name(.))}">
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="@*">
    <xsl:attribute name="{lower-case(local-name())}">
      <xsl:copy-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="processing-instruction()|comment()">
    <xsl:copy/>
  </xsl:template>
  
  <xsl:template match="text()">
    <xsl:analyze-string regex="([^∣]*)∣" select="translate(.,'¦','∣')">
      <xsl:matching-substring>
	<xsl:value-of select="regex-group(1)"/>
	<lb rend="hidden" type="hyphenInWord"/>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
	<xsl:value-of select="."/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>

  <!-- TCP discards -->
  <xsl:template match="FIGDESC/HI">
    <xsl:apply-templates />
  </xsl:template>

  <!-- TCP controversial changes -->
  <xsl:template match="PB/@MS" />
  <xsl:template match="LABEL/@ROLE" />
  <xsl:template match="TITLE/@TYPE" />
  <xsl:template match="GROUP/@TYPE" />
  <xsl:template match="TEMPHEAD" />
  <xsl:template match="TITLE/@I2" />
  <xsl:template match="IDG" />

  <!-- lose all the multi-language xml:lang things -->
  <xsl:template match="@LANG[.='32']" />
  <xsl:template match="@LANG[contains(.,' ')]" />
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Milestones:
	a) if there is no @n, just @unit   == marginal note
	b) if there is no @unit, just a @n,  == marginal note, @type='milestone'

	c) if @unit is from a closed list of words (page, line, folio), it
	seems editorial, add as subtype on @note

	d) otherwise, make a  label from @unit + @n, and put in a
	marginal note, @type='milestone'
      </p>
    </desc>
  </doc>
  <xsl:template match="MILESTONE">
    <xsl:choose>
      <xsl:when test="parent::NOTE and not(@N)"/>
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
      <xsl:when test="
         @UNIT='article' or
         @UNIT='canon' or
         @UNIT='chapter' or 
         @UNIT='commandment' or 
         @UNIT='date' or 
	 @UNIT='day' or 
	 @UNIT='folio' or 
	 @UNIT='ground of' or 
	 @UNIT='indulgence' or 
	 @UNIT='leaf' or 
	 @UNIT='line' or 
	 @UNIT='monarch' or 
	 @UNIT='motive' or 
	 @UNIT='month' or 
	 @UNIT='reason'  or 
	 @UNIT='verse'  or 
	 @UNIT='year'           ">
        <note place="margin" type="milestone" subtype="{@UNIT}">
<!--
	  <xsl:if test="$debug='true'">
	    <xsl:message>Milestone 1: <xsl:value-of
	    select="@UNIT"/>/<xsl:value-of select="@N"/></xsl:message>
	  </xsl:if>
-->
          <xsl:value-of select="@N"/>
        </note>
      </xsl:when>
      <xsl:otherwise>
<!--
	<xsl:if test="$debug='true'">
	  <xsl:message>Milestone 2: <xsl:value-of
	  select="@UNIT"/><xsl:text> </xsl:text><xsl:value-of
	  select="@N"/></xsl:message>
	</xsl:if>
-->
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


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A HEAD/@TYPE='sub' can lose itself if it consists of
      Q with L inside, though if thats all there is, looks like
      an epigraph
      </p>
    </desc>
  </doc>
  <xsl:template match="HEAD[@TYPE='sub']">
    <xsl:choose>
      <xsl:when test="following-sibling::HEAD or following-sibling::OPENER">
	<head type="sub">
	  <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
	</head>
      </xsl:when>
      <xsl:when test="Q/L and not(P|GAP|text())">
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
      </xsl:when>
      <xsl:when test="Q/L and P|GAP">
	<head type="sub">
	  <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
	</head>
	</xsl:when>
      <xsl:when test="Q[L] and not(text())">
	<epigraph>
	  <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
	</epigraph>
      </xsl:when>
      <xsl:otherwise>
	<head type="sub">
	  <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
	</head>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A paragraph with some types of child only can lose itself
      </p>
    </desc>
  </doc>
  <xsl:template match="P[not(parent::SP or parent::HEADNOTE or
		       parent::POSTSCRIPT or parent::ARGUMENT) and count(*)=1 and
		       not(text()) and 
		       (LETTER or LIST or TABLE)]" 
		>
    <xsl:apply-templates select="*|text()|processing-instruction()|comment()" />
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A paragraph inside ADD is lost, just a line-break added
      </p>
    </desc>
  </doc>
  <xsl:template match="ADD/P">
    <lb/>
    <xsl:apply-templates select="*|text()|processing-instruction()|comment()" />
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Remove gratuitous p layer inside cell</p>
    </desc>
</doc>
  <xsl:template match="CELL[count(*)=1 and not(text()) and P]">
    <cell>
      <xsl:apply-templates select="@*" />
      <xsl:for-each select="P">
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
      </xsl:for-each>
    </cell>
  </xsl:template>

  <xsl:template match="NOTE[count(*)=1 and not(text())]/Q">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
  </xsl:template>

  <xsl:template match="TITLESTMT/TITLE/text()[last()]">
    <xsl:choose>
      <xsl:when test="matches(.,':$')">
        <xsl:value-of select="substring(.,1,string-length(.)-1)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="HEADNOTE[P/FIGURE and
		       not(following-sibling::HEAD or following-sibling::OPENER)]">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="ARGUMENT[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates />
    </p>
  </xsl:template>

  <xsl:template match="HEADNOTE[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates />
    </p>
  </xsl:template>

  <xsl:template match="HEADNOTE">
    <argument>
      <xsl:apply-templates />
    </argument>
  </xsl:template>

  <xsl:template match="TAILNOTE[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates />
    </p>
  </xsl:template>

  <xsl:template match="TAILNOTE">
    <argument>
      <xsl:apply-templates />
    </argument>
  </xsl:template>

  <xsl:template match="FIGURE/BYLINE">
    <signed>
      <xsl:apply-templates />
    </signed>
  </xsl:template>

  <xsl:template match="STAGE/STAGE">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="STAGE[following-sibling::HEAD]">
    <head type="sub">
      <stage>
	<xsl:apply-templates />
      </stage>
    </head>
  </xsl:template>

  <!-- TCP non-controversial transforms -->
  <xsl:template match="ROW/PB" />
  <xsl:template match="ROW[PB]">
    <row>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" />
    </row>
    <xsl:for-each select="PB">
      <pb>
        <xsl:apply-templates select="@*" />
      </pb>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="ROW/TABLE">
    <cell>
      <table>
        <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" />
      </table>
    </cell>
  </xsl:template>
  <xsl:template match="EEBO">
    <xsl:apply-templates select="*" />
  </xsl:template>
  <xsl:template match="ETS">
    <TEI>
      <xsl:apply-templates select="@*" />
      <xsl:variable name="name">
	<xsl:choose>
	  <xsl:when test="$ID=''">
	    <xsl:value-of select="//IDG/@ID"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$ID"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:for-each select="document(concat($headerDirectory,$name,'.hdr'),$HERE)">
        <xsl:apply-templates select="*" />
      </xsl:for-each>
      <xsl:apply-templates />
    </TEI>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT">
    <publicationStmt>
      <xsl:apply-templates select="*" />
      <xsl:if test="parent::FILEDESC">
        <xsl:call-template name="makeID"/>
        <xsl:for-each select="$HERE">
          <xsl:for-each select="/ETS/EEBO/IDG">
            <xsl:for-each select="STC">
              <idno type="STC">
                <xsl:value-of select="."/>
              </idno>
            </xsl:for-each>
            <idno type="TCP">
              <xsl:value-of select="@ID"/>
            </idno>
            <idno type="BIBNO">
              <xsl:value-of select="BIBNO"/>
            </idno>
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
  <xsl:template match="PUBLICATIONSTMT/IDNO" />
  <xsl:template match="FILEDESC/EXTENT" />
  <xsl:template match="EEBO/GROUP">
    <text>
      <group>
        <xsl:apply-templates select="@*" />
        <xsl:apply-templates select="*" />
      </group>
    </text>
  </xsl:template>
  <xsl:template match="LETTER">
    <floatingText type="letter">
      <body>
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()" />
      </body>
    </floatingText>
  </xsl:template>
  <xsl:template match="TEXT">
    <xsl:choose>
      <xsl:when test="parent::ETS or parent::EEBO or parent::GROUP">
        <text>
          <xsl:apply-templates select="@*" />
          <xsl:apply-templates select="*" />
        </text>
      </xsl:when>
      <xsl:otherwise>
        <floatingText>
          <xsl:apply-templates select="@*" />
          <xsl:apply-templates select="*" />
        </floatingText>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="LANGUSAGE/@ID" />
  <xsl:template match="PB/@REF">
    <xsl:attribute name="facs">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="rend">
      <xsl:text>none</xsl:text>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="KEYWORDS">
    <xsl:if test="*">
      <keywords>
	<xsl:if test="not(@SCHEME)">
	  <xsl:attribute name="scheme">
	    <xsl:text>http://authorities.loc.gov/</xsl:text>
	  </xsl:attribute>
	</xsl:if>
	<xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" />
      </keywords>
    </xsl:if>
  </xsl:template>

  <xsl:template match="SUP">
    <hi rend="sup">
      <xsl:apply-templates />
    </hi>
  </xsl:template>
  <xsl:template match="SUB">
    <hi rend="sub">
      <xsl:apply-templates />
    </hi>
  </xsl:template>
  <xsl:template match="BELOW">
    <hi rend="below">
      <xsl:apply-templates />
    </hi>
  </xsl:template>
  <xsl:template match="ABOVE">
    <hi rend="above">
      <xsl:apply-templates />
    </hi>
  </xsl:template>
  <xsl:template match="HEADER">
    <teiHeader>
      <xsl:apply-templates  select="@*|*|comment()|processing-instruction()"/>
    </teiHeader>
  </xsl:template>
  <xsl:template match="TEI.2|OTA">
    <TEI>
      <xsl:apply-templates  select="@*|*|comment()|processing-instruction()"/>
    </TEI>
  </xsl:template>
  <xsl:template match="ADDNAME">
    <addName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </addName>
  </xsl:template>
  <xsl:template match="ADDSPAN">
    <addSpan>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </addSpan>
  </xsl:template>
  <xsl:template match="ADDRLINE">
    <addrLine>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </addrLine>
  </xsl:template>
  <xsl:template match="ALTGRP">
    <altGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </altGrp>
  </xsl:template>
  <xsl:template match="ATTDEF">
    <attDef>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </attDef>
  </xsl:template>
  <xsl:template match="ATTLIST">
    <attList>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </attList>
  </xsl:template>
  <xsl:template match="ATTNAME">
    <attName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </attName>
  </xsl:template>
  <xsl:template match="ATTLDECL">
    <attlDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </attlDecl>
  </xsl:template>
  <xsl:template match="BASEWSD">
    <baseWsd>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </baseWsd>
  </xsl:template>
  <xsl:template match="BIBLFULL">
    <biblFull>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </biblFull>
  </xsl:template>
  <xsl:template match="BIBLSCOPE">
    <biblScope>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </biblScope>
  </xsl:template>
  <xsl:template match="BIBLSTRUCT">
    <biblStruct>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </biblStruct>
  </xsl:template>
  <xsl:template match="CASTGROUP">
    <castGroup>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </castGroup>
  </xsl:template>
  <xsl:template match="CASTITEM">
    <castItem>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </castItem>
  </xsl:template>
  <xsl:template match="CASTLIST">
    <castList>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </castList>
  </xsl:template>
  <xsl:template match="CATDESC">
    <catDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </catDesc>
  </xsl:template>
  <xsl:template match="CATREF">
    <catRef>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </catRef>
  </xsl:template>
  <xsl:template match="CLASSCODE">
    <classCode>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </classCode>
  </xsl:template>
  <xsl:template match="CLASSDECL">
    <classDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </classDecl>
  </xsl:template>
  <xsl:template match="CLASSDOC">
    <classDoc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </classDoc>
  </xsl:template>
  <xsl:template match="CODEDCHARSET">
    <codedCharSet>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </codedCharSet>
  </xsl:template>
  <xsl:template match="DATADESC">
    <dataDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </dataDesc>
  </xsl:template>
  <xsl:template match="DATERANGE">
    <dateRange>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </dateRange>
  </xsl:template>
  <xsl:template match="DATESTRUCT">
    <dateStruct>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </dateStruct>
  </xsl:template>
  <xsl:template match="DELSPAN">
    <delSpan>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </delSpan>
  </xsl:template>
  <xsl:template match="DIVGEN">
    <divGen>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </divGen>
  </xsl:template>
  <xsl:template match="DOCAUTHOR|DAUTHOR">
    <docAuthor>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </docAuthor>
  </xsl:template>
  <xsl:template match="DOCDATE">
    <docDate>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </docDate>
  </xsl:template>
  <xsl:template match="DOCEDITION">
    <docEdition>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </docEdition>
  </xsl:template>
  <xsl:template match="DOCIMPRINT">
    <docImprint>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </docImprint>
  </xsl:template>
  <xsl:template match="DOCTITLE|DTITLE">
    <docTitle>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </docTitle>
  </xsl:template>
  <xsl:template match="ELEAF">
    <eLeaf>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </eLeaf>
  </xsl:template>
  <xsl:template match="ETREE">
    <eTree>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </eTree>
  </xsl:template>
  <xsl:template match="EDITIONSTMT">
    <editionStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </editionStmt>
  </xsl:template>
  <xsl:template match="EDITORIALDECL">
    <editorialDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </editorialDecl>
  </xsl:template>
  <xsl:template match="ELEMDECL">
    <elemDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </elemDecl>
  </xsl:template>
  <xsl:template match="ENCODINGDESC">
    <encodingDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </encodingDesc>
  </xsl:template>
  <xsl:template match="ENTDOC">
    <entDoc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </entDoc>
  </xsl:template>
  <xsl:template match="ENTNAME">
    <entName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </entName>
  </xsl:template>
  <xsl:template match="ENTITYSET">
    <entitySet>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </entitySet>
  </xsl:template>
  <xsl:template match="ENTRYFREE">
    <entryFree>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </entryFree>
  </xsl:template>
  <xsl:template match="EXTFIGURE">
    <extFigure>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </extFigure>
  </xsl:template>
  <xsl:template match="FALT">
    <fAlt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fAlt>
  </xsl:template>
  <xsl:template match="FDECL">
    <fDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fDecl>
  </xsl:template>
  <xsl:template match="FDESCR">
    <fDescr>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fDescr>
  </xsl:template>
  <xsl:template match="FLIB">
    <fLib>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fLib>
  </xsl:template>
  <xsl:template match="FIGDESC">
    <figDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:value-of select="translate(.,'∣','')"/>
    </figDesc>
  </xsl:template>
  <xsl:template match="FILEDESC">
    <fileDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fileDesc>
  </xsl:template>
  <xsl:template match="FIRSTLANG">
    <firstLang>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </firstLang>
  </xsl:template>
  <xsl:template match="FORENAME">
    <foreName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </foreName>
  </xsl:template>
  <xsl:template match="FORESTGRP">
    <forestGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </forestGrp>
  </xsl:template>
  <xsl:template match="FSCONSTRAINTS">
    <fsConstraints>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fsConstraints>
  </xsl:template>
  <xsl:template match="FSDECL">
    <fsDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fsDecl>
  </xsl:template>
  <xsl:template match="FSDESCR">
    <fsDescr>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fsDescr>
  </xsl:template>
  <xsl:template match="FSLIB">
    <fsLib>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fsLib>
  </xsl:template>
  <xsl:template match="FSDDECL">
    <fsdDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fsdDecl>
  </xsl:template>
  <xsl:template match="FVLIB">
    <fvLib>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </fvLib>
  </xsl:template>
  <xsl:template match="GENNAME">
    <genName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </genName>
  </xsl:template>
  <xsl:template match="GEOGNAME">
    <geogName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </geogName>
  </xsl:template>
  <xsl:template match="GRAMGRP">
    <gramGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </gramGrp>
  </xsl:template>
  <xsl:template match="HANDLIST">
    <handList>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </handList>
  </xsl:template>
  <xsl:template match="HANDSHIFT">
    <handShift>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </handShift>
  </xsl:template>
  <xsl:template match="HEADITEM">
    <headItem>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </headItem>
  </xsl:template>
  <xsl:template match="HEADLABEL">
    <headLabel>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </headLabel>
  </xsl:template>
  <xsl:template match="INODE">
    <iNode>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </iNode>
  </xsl:template>
  <xsl:template match="INTERPGRP">
    <interpGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </interpGrp>
  </xsl:template>
  <xsl:template match="JOINGRP">
    <joinGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </joinGrp>
  </xsl:template>
  <xsl:template match="LACUNAEND">
    <lacunaEnd>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </lacunaEnd>
  </xsl:template>
  <xsl:template match="LACUNASTART">
    <lacunaStart>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </lacunaStart>
  </xsl:template>
  <xsl:template match="LANGKNOWN">
    <langKnown>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </langKnown>
  </xsl:template>
  <xsl:template match="LANGUSAGE">
    <langUsage>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </langUsage>
  </xsl:template>
  <xsl:template match="LINKGRP">
    <linkGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </linkGrp>
  </xsl:template>
  <xsl:template match="LISTBIBL">
    <listBibl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </listBibl>
  </xsl:template>
  <xsl:template match="METDECL">
    <metDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </metDecl>
  </xsl:template>
  <xsl:template match="NAMELINK">
    <nameLink>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </nameLink>
  </xsl:template>
  <xsl:template match="NOTESSTMT">
    <notesStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </notesStmt>
  </xsl:template>
  <xsl:template match="OREF">
    <oRef>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </oRef>
  </xsl:template>
  <xsl:template match="OVAR">
    <oVar>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </oVar>
  </xsl:template>
  <xsl:template match="OFFSET">
    <offSet>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </offSet>
  </xsl:template>
  <xsl:template match="ORGDIVN">
    <orgDivn>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </orgDivn>
  </xsl:template>
  <xsl:template match="ORGNAME">
    <orgName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </orgName>
  </xsl:template>
  <xsl:template match="ORGTITLE">
    <orgTitle>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </orgTitle>
  </xsl:template>
  <xsl:template match="ORGTYPE">
    <orgType>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </orgType>
  </xsl:template>
  <xsl:template match="OTHERFORM">
    <otherForm>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </otherForm>
  </xsl:template>
  <xsl:template match="PREF">
    <pRef>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </pRef>
  </xsl:template>
  <xsl:template match="PVAR">
    <pVar>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </pVar>
  </xsl:template>
  <xsl:template match="PARTICDESC">
    <particDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </particDesc>
  </xsl:template>
  <xsl:template match="PARTICLINKS">
    <particLinks>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </particLinks>
  </xsl:template>
  <xsl:template match="PERSNAME">
    <persName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </persName>
  </xsl:template>
  <xsl:template match="PERSONGRP">
    <personGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </personGrp>
  </xsl:template>
  <xsl:template match="PLACENAME">
    <placeName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </placeName>
  </xsl:template>
  <xsl:template match="POSTBOX">
    <postBox>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </postBox>
  </xsl:template>
  <xsl:template match="POSTCODE">
    <postCode>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </postCode>
  </xsl:template>
  <xsl:template match="PROFILEDESC">
    <profileDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </profileDesc>
  </xsl:template>
  <xsl:template match="PROJECTDESC">
    <projectDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </projectDesc>
  </xsl:template>
  <xsl:template match="PUBPLACE">
    <pubPlace>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </pubPlace>
  </xsl:template>
  <xsl:template match="RDGGRP">
    <rdgGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </rdgGrp>
  </xsl:template>
  <xsl:template match="RECORDINGSTMT">
    <recordingStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </recordingStmt>
  </xsl:template>
  <xsl:template match="REFSDECL">
    <refsDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </refsDecl>
  </xsl:template>
  <xsl:template match="RESPSTMT">
    <respStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </respStmt>
  </xsl:template>
  <xsl:template match="REVISIONDESC|REVDESC">
    <revisionDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </revisionDesc>
  </xsl:template>
  <xsl:template match="ROLEDESC">
    <roleDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </roleDesc>
  </xsl:template>
  <xsl:template match="ROLENAME">
    <roleName>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </roleName>
  </xsl:template>
  <xsl:template match="SAMPLINGDECL">
    <samplingDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </samplingDecl>
  </xsl:template>
  <xsl:template match="SCRIPTSTMT">
    <scriptStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </scriptStmt>
  </xsl:template>
  <xsl:template match="SERIESSTMT">
    <seriesStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </seriesStmt>
  </xsl:template>
  <xsl:template match="SETTINGDESC">
    <settingDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </settingDesc>
  </xsl:template>
  <xsl:template match="SOCALLED">
    <soCalled>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </soCalled>
  </xsl:template>
  <xsl:template match="SOCECSTATUS">
    <socecStatus>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </socecStatus>
  </xsl:template>
  <xsl:template match="SOURCEDESC">
    <sourceDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </sourceDesc>
  </xsl:template>
  <xsl:template match="SPANGRP">
    <spanGrp>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </spanGrp>
  </xsl:template>
  <xsl:template match="STDVALS">
    <stdVals>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </stdVals>
  </xsl:template>
  <xsl:template match="TAGDOC">
    <tagDoc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </tagDoc>
  </xsl:template>
  <xsl:template match="TAGUSAGE">
    <tagUsage>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </tagUsage>
  </xsl:template>
  <xsl:template match="TAGSDECL">
    <tagsDecl>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </tagsDecl>
  </xsl:template>
  <xsl:template match="TEICORPUS.2">
    <teiCorpus.2>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </teiCorpus.2>
  </xsl:template>
  <xsl:template match="TEIFSD2">
    <teiFsd2>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </teiFsd2>
  </xsl:template>
  <xsl:template match="TEIHEADER">
    <teiHeader>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </teiHeader>
  </xsl:template>
  <xsl:template match="TERMENTRY">
    <termEntry>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </termEntry>
  </xsl:template>
  <xsl:template match="TEXTCLASS">
    <textClass>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </textClass>
  </xsl:template>
  <xsl:template match="TEXTDESC">
    <textDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </textDesc>
  </xsl:template>
  <xsl:template match="TIMERANGE">
    <timeRange>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </timeRange>
  </xsl:template>
  <xsl:template match="TIMESTRUCT">
    <timeStruct>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </timeStruct>
  </xsl:template>
  <xsl:template match="TITLEPAGE|TPAGE">
    <titlePage>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </titlePage>
  </xsl:template>
  <xsl:template match="TITLEPART">
    <titlePart>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </titlePart>
  </xsl:template>
  <xsl:template match="TITLESTMT">
    <titleStmt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </titleStmt>
  </xsl:template>
  <xsl:template match="VALT">
    <vAlt>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </vAlt>
  </xsl:template>
  <xsl:template match="VDEFAULT">
    <vDefault>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </vDefault>
  </xsl:template>
  <xsl:template match="VRANGE">
    <vRange>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </vRange>
  </xsl:template>
  <xsl:template match="VALDESC">
    <valDesc>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </valDesc>
  </xsl:template>
  <xsl:template match="VALLIST">
    <valList>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </valList>
  </xsl:template>
  <xsl:template match="VARIANTENCODING">
    <variantEncoding>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </variantEncoding>
  </xsl:template>
  <xsl:template match="WITDETAIL">
    <witDetail>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </witDetail>
  </xsl:template>
  <xsl:template match="WITEND">
    <witEnd>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </witEnd>
  </xsl:template>
  <xsl:template match="WITLIST">
    <witList>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </witList>
  </xsl:template>
  <xsl:template match="WITSTART">
    <witStart>
      <xsl:apply-templates  select="@*"/>
      <xsl:apply-templates />
    </witStart>
  </xsl:template>
  <xsl:template match="@TEI">
    <xsl:attribute name="TEI">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJFROM">
    <xsl:attribute name="adjFrom">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ADJTO">
    <xsl:attribute name="adjTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ASSERTEDVALUE">
    <xsl:attribute name="assertedValue">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@BASETYPE">
    <xsl:attribute name="baseType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@COPYOF">
    <xsl:attribute name="copyOf">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@DEPPTR">
    <xsl:attribute name="depPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYLOC">
    <xsl:attribute name="entityLoc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ENTITYSTD">
    <xsl:attribute name="entityStd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@FVAL">
    <xsl:attribute name="fVal">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@GRPPTR">
    <xsl:attribute name="grpPtr">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@INDEGREE">
    <xsl:attribute name="inDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@MUTEXCL">
    <xsl:attribute name="mutExcl">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@OUTDEGREE">
    <xsl:attribute name="outDegree">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@SAMEAS">
    <xsl:attribute name="sameAs">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGFUNC">
    <xsl:attribute name="targFunc">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGORDER">
    <xsl:if test="not(. = 'u')">
      <xsl:attribute name="targOrder">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="TEIHEADER/@TYPE" />
  <xsl:template match="@TARGTYPE">
    <xsl:attribute name="targType">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TARGETEND">
    <xsl:attribute name="targetEnd">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VALUETO">
    <xsl:attribute name="valueTo">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@VARSEQ">
    <xsl:attribute name="varSeq">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@WSCALE">
    <xsl:attribute name="wScale">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@TEIFORM" />
  <xsl:template match="@OPT">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="opt">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@TO">
    <xsl:if test="not(. = 'DITTO')">
      <xsl:attribute name="to">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@DEFAULT">
    <xsl:if test="not(. = 'no')">
      <xsl:attribute name="default">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@PART">
    <xsl:if test="not(. = 'n')">
      <xsl:attribute name="part">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FULL">
    <xsl:if test="not(. = 'yes')">
      <xsl:attribute name="full">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@FROM">
    <xsl:if test="not(. = 'ROOT')">
      <xsl:attribute name="from">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@STATUS">
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
      @PLACE has both inconsistencies and mistakes; some values
      should obviously be @n</p>
    </desc>
  </doc>
  <xsl:template match="@PLACE">
    <xsl:choose>
      <xsl:when test=".='marg' or .='marg;' or .='marg)' or .='marg='         or .='ma / rg' or .='6marg'">
        <xsl:attribute name="place">margin</xsl:attribute>
      </xsl:when>
      <xsl:when test=". = 'unspecified'"/>
      <xsl:when test=".='foot;' or .='foor;' or .='foot'">
        <xsl:attribute name="place">bottom</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='‡' or .='†' or .='‖' or .='6'  or .='“' or         .='1' or .='*'">
        <xsl:attribute name="n">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="place">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@SAMPLE">
    <xsl:if test="not(. = 'complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@ORG">
    <xsl:if test="not(. = 'uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="ENCDESC">
    <encodingDesc>
      <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="EDSTMT">
    <editorialStmt>
      <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"/>
    </editorialStmt>
  </xsl:template>
  <xsl:template match="TITLSTMT">
    <titleStmt>
      <xsl:apply-templates  select="*|@*|processing-instruction()|comment()|text()"/>
    </titleStmt>
  </xsl:template>

  <xsl:template match="@N">
    <xsl:if test="not(normalize-space(.)='')">
      <xsl:attribute name="n">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>

  <xsl:template match="@TYPE">
    <xsl:choose>    
      <xsl:when test=".='poem (rebus)'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">rebus</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem(s)'">
	<xsl:attribute name="type">poems</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem and response'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">response</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem collection'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">collection</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem fragment'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">fragment</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem fragments'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">fragments</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem from author to the reader'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">from_author_to_the_reader</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem in honor of Gustavus'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">in_honor_of_Gustavus</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem incorporating anagrams'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">incorporating_anagrams</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem incorporating the Creed'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">incorporating_the_Creed</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem on frontispiece'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">on_frontispiece</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem on the seven virtues'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">on_the_seven_virtues</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to Archpapist'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_Archpapist</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to God from second edition'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_God_from_second_edition</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to author'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_author</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to book'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_book</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to king'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_king</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to pupils'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_pupils</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to readers'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_readers</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to subjects'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_subjects</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to the author'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_the_author</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to the censorious reader'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_the_censorious_reader</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to the censors'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_the_censors</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to the pious reader'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_the_pious_reader</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem to the reader'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">to_the__reader</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem with commentary'">
	<xsl:attribute name="type">poem</xsl:attribute>
	<xsl:attribute name="subtype">commentary</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poem(s) by one author'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">by_one_author</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poems and commentary'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">commentary</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poems gratulatory'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">gratulatory</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poems of acknowledgment'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">acknowledgment</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poems on the Symbols'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">on_the_Symbols</xsl:attribute>
      </xsl:when>
      <xsl:when test=".='poems to the reader'">
	<xsl:attribute name="type">poems</xsl:attribute>
	<xsl:attribute name="subtype">to_the_reader</xsl:attribute>
      </xsl:when>
      
      <xsl:when test="not(normalize-space(.)='')">
	<xsl:attribute name="type">
	  <xsl:analyze-string 
	      regex="([0-9]+)(.*)" 
	      select="translate(translate(.,'( &amp;/', '____'),$intype,'')">
	    <xsl:matching-substring>
	      <xsl:text>n</xsl:text>
	      <xsl:value-of select="regex-group(1)"/>
	      <xsl:value-of select="regex-group(2)"/>
	    </xsl:matching-substring>
	    <xsl:non-matching-substring>
	      <xsl:value-of select="."/>
	    </xsl:non-matching-substring>
	  </xsl:analyze-string>
	</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	You sometimes find a milestone inside a marginal note, where
	the note has the same value for @type as the milestone has for unit.
	Kill the @type  on note in this situation.
      </p>
    </desc>
  </doc>
  
  <xsl:template match="NOTE/@TYPE[.=../MILESTONE/@UNIT]" />
  
  <xsl:template match="@UNIT">
    <xsl:attribute name="unit">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="TEICORPUS.2">
    <teiCorpus>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </teiCorpus>
  </xsl:template>
  <xsl:template match="WITNESS/@SIGIL">
    <xsl:attribute name="xml:id">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="WITLIST">
    <listWit>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </listWit>
  </xsl:template>
  <xsl:template match="TEI.2">
    <TEI>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </TEI>
  </xsl:template>
  <xsl:template match="XREF">
    <ref>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </ref>
  </xsl:template>
  <xsl:template match="XPTR">
    <ptr>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </ptr>
  </xsl:template>
  <xsl:template match="FIGURE/@ENTITY"/>
  <xsl:template match="FIGURE">
    <figure>
      <xsl:if test="@ENTITY">
	<graphic>
	  <xsl:attribute name="url">
	    <xsl:choose>
	      <xsl:when test="unparsed-entity-uri(@ENTITY)=''">
		<xsl:text>ENTITY_</xsl:text>
		<xsl:value-of select="@ENTITY"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="unparsed-entity-uri(@ENTITY)"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>
	  <xsl:apply-templates select="@*"/>
	</graphic>
      </xsl:if>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	Figures inside paragraphs can generally be free-standing,
	unless they are the only paragraph of this type (ie inside a
	div consisting only of pictures).
      </p>
    </desc>
  </doc>

  <xsl:template match="P[parent::*/count(P[not(FIGURE)])&gt;1][FIGURE]">
    <xsl:apply-templates select="FIGURE"/>
  </xsl:template>

  <xsl:template match="EVENT">
    <incident>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </incident>
  </xsl:template>
  <xsl:template match="STATE">
    <refState>
      <xsl:apply-templates select="@*|*|text()|comment()|processing-instruction()"/>
    </refState>
  </xsl:template>
  <!-- lost elements -->
  <xsl:template match="DATERANGE">
    <date>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </date>
  </xsl:template>
  <xsl:template match="DATERANGE/@FROM">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="DATERANGE/@TO">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="LANGUAGE">
    <language>
      <xsl:attribute name="ident">
	<xsl:choose>
	<xsl:when test="@ID">
          <xsl:value-of select="@ID"/>
	</xsl:when>
	<xsl:when test="../@ID">
          <xsl:value-of select="../@ID"/>
	</xsl:when>
	</xsl:choose>
    </xsl:attribute>
      <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
    </language>
  </xsl:template>
  <!-- attributes lost -->
  <!-- dropped from TEI. Added as new change records later -->
  <xsl:template match="@DATE.CREATED"/>
  <xsl:template match="@DATE.UPDATED"/>
  <!-- dropped from TEI. No replacement -->
  <xsl:template match="REFSDECL/@DOCTYPE"/>
  <!-- attributes changed name -->
  <xsl:template match="DATE/@VALUE">
    <xsl:attribute name="when">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@DOC">
    <xsl:attribute name="target">
      <xsl:value-of select="unparsed-entity-uri(.)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@ID">
    <xsl:choose>
      <xsl:when test="parent::LANG">
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
  <xsl:template match="@LANG">
    <xsl:attribute name="xml:lang">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="CHANGE/@DATE"/>
  <xsl:template match="DATE/@CERTAINTY">
    <xsl:attribute name="cert">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="@FACS">
    <xsl:attribute name="facs">
      <xsl:value-of select="translate(.,' :[]','_')"/>
    </xsl:attribute>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	Process pointing attributes, which may me full URLs or not, and may
      contain multiple values.</p>
    </desc>
  </doc>
  <!-- all pointing attributes preceded by # -->
  <xsl:template match="VARIANTENCODING/@LOCATION">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@ANA|@ACTIVE|@ADJ|@ADJFROM|@ADJTO|@CHILDREN|@CLASS|@CODE|@COPYOF|@CORRESP|@DECLS|@DOMAINS|@END|@EXCLUDE|@FVAL|@FEATS|@FOLLOW|@HAND|@INST|@LANGKEY|@LOCATION|@MERGEDIN|@NEW|@NEXT|@OLD|@ORIGIN|@OTHERLANGS|@PARENT|@PASSIVE|@PERF|@PREV|@RENDER|@RESP|@SAMEAS|@SCHEME|@SCRIPT|@SELECT|@SINCE|@START|@SYNCH|@TARGET|@TARGETEND|@VALUE|@VALUE|@WHO|@WIT">
    <xsl:attribute name="{lower-case(name(.))}">
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
        <xsl:choose>
          <xsl:when test="starts-with($val,'http') or starts-with($val,'ftp') or starts-with($val,'mailto')">
            <xsl:value-of select="$val"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>#</xsl:text>
	    <xsl:value-of select="substring-before($val,' ')"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text> </xsl:text>
        <xsl:call-template name="splitter">
          <xsl:with-param name="val">
            <xsl:value-of select="substring-after($val,' ')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="starts-with($val,'http') or starts-with($val,'ftp') or starts-with($val,'mailto')">
            <xsl:value-of select="$val"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$val"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- fool around with selected elements -->
  <!-- imprint is no longer allowed inside bibl -->
  <xsl:template match="BIBL/IMPRINT">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="EDITIONSTMT/EDITOR">
    <respStmt>
      <resp>
        <xsl:value-of select="@ROLE"/>
      </resp>
      <name>
        <xsl:apply-templates/>
      </name>
    </respStmt>
  </xsl:template>
  <!-- header -->
  <xsl:template match="TEIHEADER">
    <teiHeader>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
      <xsl:choose>
	<xsl:when test="not(REVISIONDESC) and (@DATE.CREATED or @DATE.UPDATED)">
	  <revisionDesc>
	    <xsl:if test="@DATE.UPDATED">
            <change>&gt;
	    <label>updated</label>
	    <date><xsl:value-of select="@DATE.UPDATED"/></date>
	    <label>Date edited</label>
	    </change>
          </xsl:if>
          <xsl:if test="@DATE.CREATED">
            <change>
              <label>created</label>
              <date>
                <xsl:value-of select="@DATE.CREATED"/>
              </date>
              <label>Date created</label>
            </change>
          </xsl:if>
        </revisionDesc>
	</xsl:when>
	<xsl:when test="not(REVISIONDESC)">
	  <xsl:call-template name="Decls"/>
	</xsl:when>
      </xsl:choose>
    </teiHeader>
  </xsl:template>
  <xsl:template match="@ROLE">
    <xsl:attribute name="ana">
      <xsl:text>#role_</xsl:text>
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="REVISIONDESC">
    <xsl:call-template name="Decls"/>
    <revisionDesc>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
    </revisionDesc>
  </xsl:template>
  <!-- space does not have @EXTENT any more -->
  <xsl:template match="SPACE/@EXTENT">
    <xsl:attribute name="quantity">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>
  <!-- tagsDecl has a compulsory namespace child now -->
  <xsl:template match="TAGSDECL">
    <xsl:if test="*">
      <tagsDecl>
        <namespace name="http://www.tei-c.org/ns/1.0">
          <xsl:apply-templates select="*|comment()|processing-instruction"/>
        </namespace>
      </tagsDecl>
    </xsl:if>
  </xsl:template>
  <!-- orgTitle inside orgName? redundant -->
  <xsl:template match="ORGNAME/ORGTITLE">
    <xsl:apply-templates/>
  </xsl:template>
  <!-- no need for empty <p> in sourceDesc -->
  <xsl:template match="SOURCEDESC/p[string-length(.)=0]"/>

  <xsl:template match="GAP/@DESC">
    <xsl:attribute name="reason">
      <xsl:value-of  select="."/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="GAP/@DISP">
    <desc>
      <xsl:value-of  select="."/>
    </desc>
  </xsl:template>
  <xsl:template match="GAP">
    <gap>
      <xsl:apply-templates select="@*"/>
    </gap>
  </xsl:template>
  <!--  creating a choice element -->
  <xsl:template match="CORR[@SIC]">
    <choice>
      <corr>
        <xsl:value-of select="text()"/>
      </corr>
      <sic>
        <xsl:value-of select="@SIC"/>
      </sic>
    </choice>
  </xsl:template>
  <xsl:template match="SIC[@CORR]">
    <choice>
      <sic>
        <xsl:apply-templates/>
      </sic>
      <corr>
        <xsl:value-of select="@CORR"/>
      </corr>
    </choice>
  </xsl:template>
  <xsl:template match="ORIG[@REG]">
    <choice>
      <orig>
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </orig>
      <reg>
        <xsl:value-of select="@REG"/>
      </reg>
    </choice>
  </xsl:template>
  <xsl:template match="REG[@ORIG]">
    <choice>
      <reg>
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </reg>
      <orig>
        <xsl:value-of select="@ORIG"/>
      </orig>
    </choice>
  </xsl:template>
  <xsl:template match="@ORIG|@REG"/>
  <xsl:template match="ABBR[@EXPAN]">
    <choice>
      <abbr>
        <xsl:apply-templates/>
      </abbr>
      <expan>
        <xsl:value-of select="@EXPAN"/>
      </expan>
    </choice>
  </xsl:template>
  <xsl:template match="EXPAN[@ABBR]">
    <choice>
      <expan>
        <xsl:apply-templates/>
      </expan>
      <abbr>
        <xsl:value-of select="@ABBR"/>
      </abbr>
    </choice>
  </xsl:template>
  <!-- special consideration for <change> element -->
  <xsl:template match="CHANGE">
    <change>
      <xsl:apply-templates select="ITEM/@*"/>
      <xsl:apply-templates select="DATE"/>
      <xsl:if test="RESPSTMT/RESP">
        <label>
          <xsl:value-of select="RESPSTMT/RESP/text()"/>
        </label>
      </xsl:if>
      <xsl:for-each select="RESPSTMT/NAME">
        <name>
          <xsl:apply-templates select="@*|*|comment()|processing-instruction()|text()"/>
        </name>
      </xsl:for-each>
      <xsl:for-each select="ITEM">
        <xsl:apply-templates select="*|comment()|processing-instruction()|text()"/>
      </xsl:for-each>
    </change>
  </xsl:template>
  <xsl:template match="RESPSTMT[RESP]">
    <respStmt>
      <xsl:choose>
        <xsl:when test="RESP/NAME">
          <resp>
            <xsl:value-of select="resp/text()"/>
          </resp>
          <xsl:for-each select="RESP/NAME">
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
  <xsl:template match="Q/@DIRECT"/>
  <xsl:template match="@STATUS">
    <xsl:choose>
      <xsl:when test="parent::TEIHEADER">
        <xsl:if test="not(lower-case(.) ='new')">
          <xsl:attribute name="status">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:if>
      </xsl:when>
      <xsl:when test="parent::DEL">
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
  <xsl:template match="@SAMPLE">
    <xsl:if test="not(lower-case(.) ='complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@ORG">
    <xsl:if test="not(lower-case(.) ='uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="TEIHEADER/@TYPE">
    <xsl:if test="not(lower-case(.) ='text')">
      <xsl:attribute name="type">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <!-- yes|no to boolean -->
  <xsl:template match="@ANCHORED">
    <xsl:attribute name="anchored">
      <xsl:choose>
        <xsl:when test="lower-case(.)='yes'">true</xsl:when>
        <xsl:when test="lower-case(.)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="SOURCEDESC/@DEFAULT"/>
  <xsl:template match="@TEI">
    <xsl:attribute name="tei">
      <xsl:choose>
        <xsl:when test="lower-case(.)='yes'">true</xsl:when>
        <xsl:when test="lower-case(.)='no'">false</xsl:when>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="@LANGKEY"/>
  <xsl:template match="@TEIFORM"/>
  <xsl:template match="@OLD"/>
  <xsl:template match="REF/@FROM"/>
  <xsl:template match="@MERGEDIN">
    <xsl:attribute name="mergedIn">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="DIV0|DIV1|DIV2|DIV3|DIV4|DIV5|DIV6|DIV7">
    <div>
      <xsl:apply-templates select="@*" />
      <xsl:apply-templates select="*" />
    </div>
  </xsl:template>

  <!-- remove default values for attributes -->
  <xsl:template match="ROW/@ROLE[.='data']"/>
  <xsl:template match="CELL/@ROLE[.='data']"/>
  <xsl:template match="CELL/@ROWS[.='1']"/>
  <xsl:template match="CELL/@COLS[.='1']"/>
  <xsl:template match="Q/@BROKEN[.='no']"/>
  <xsl:template match="ENCODINGDESC/PROJECTDESC">
    <projectDesc>
      <p>Created by converting TCP files to TEI P5 using tcp2tei.xsl,
      TEI @ Oxford.
      </p>
    </projectDesc>
  </xsl:template>


  <xsl:template match="HEAD/STAGE">
    <hi rend="stage">
      <xsl:apply-templates/>
    </hi>
  </xsl:template>

  <xsl:template match="FIGDESC/HI[@REND='sup']">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="PUBLICATIONSTMT[not(*)]">
    <publicationStmt>
      <p>unknown</p>
    </publicationStmt>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	If we have made some @role into @ana, we need taxonomy
	elements to point to
      </p>
    </desc>
  </doc>
  <xsl:template name="Decls">
    <xsl:if test="key('ROLES',1) or $Rendition/tei:tagsDecl/tei:rendition">
      <encodingDesc>
	<xsl:if test="key('ROLES',1)">
	  <classDecl>
	    <taxonomy>
	      <xsl:for-each-group select="key('ROLES',1)" group-by=".">
		<category xml:id="role_{.}">
		  <catDesc><xsl:value-of select="."/></catDesc>
		</category>
	      </xsl:for-each-group>
	    </taxonomy>
	  </classDecl>
	</xsl:if>
	<xsl:if test="$Rendition/tei:tagsDecl/tei:rendition">
	  <tagsDecl>
	    <xsl:for-each select="$Rendition/tei:tagsDecl/tei:rendition">
	      <rendition scheme="css">
		<xsl:apply-templates select="@xml:id"/>
		<xsl:text>content:</xsl:text>
		<xsl:value-of select="."/>
		<xsl:text>;</xsl:text>
	      </rendition>
	    </xsl:for-each>
	  </tagsDecl>
	</xsl:if>
      </encodingDesc>
    </xsl:if>
  </xsl:template>

  <xsl:template name="makeID"/>
  <xsl:template name="idnoHook"/>
</xsl:stylesheet>

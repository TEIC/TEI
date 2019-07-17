<?xml version="1.0" encoding="utf-8"?>
<!--
$Date$ $Author$

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="tei" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p>XSLT script for cleaning up ECCO texts TEI P5 conversion</p>
      <p><h1 xmlns="">License</h1>This software is dual-licensed:

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
  <xsl:output cdata-section-elements="eg" indent="yes" method="xml" encoding="utf-8" omit-xml-declaration="yes"/>
  <xsl:param name="ID"/>
  <xsl:key name="ROLES" match="P/@ROLE" use="1"/>
  <xsl:key name="ROLES" match="ITEM/@ROLE" use="1"/>
  <xsl:param name="intype"> ',)?</xsl:param>
  <xsl:param name="debug">false</xsl:param>
  <xsl:param name="headerDirectory"/>
  <xsl:variable name="HERE" select="/"/>
  <xsl:variable name="Rendition">
    <tagsDecl>
      <xsl:for-each-group select="//GAP/@DISP" group-by=".">
        <rendition xml:id="{position()}">
          <xsl:value-of select="current-grouping-key()"/>
        </rendition>
      </xsl:for-each-group>
    </tagsDecl>
  </xsl:variable>

<!-- multi-stage transform, 3 passes on each text -->
  <xsl:template match="/">
    <xsl:variable name="pass1">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:variable name="pass2">
      <xsl:apply-templates select="$pass1" mode="pass2"/>
    </xsl:variable>
    <xsl:apply-templates select="$pass2" mode="pass3"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>default identity transform</desc></doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>text nodes are examined to find soft-hyphen characters,
      which are replaced an empty &lt;g&gt;. To be on the safe side,
      apply Unicode  NFC normalization to text (some decomposed
      characters seen in headers).      </p>
    </desc>
  </doc>
  <xsl:template match="text()">
    <xsl:variable name="parent" select="local-name(parent::*)"/>
    <xsl:analyze-string regex="([^∣¦]*)([∣¦])" select=".">
      <xsl:matching-substring>
        <xsl:value-of select="normalize-unicode(regex-group(1),'NFC')"/>
	<xsl:choose>
	<xsl:when test="regex-group(2)='¦'">
	  <g ref="char:EOLunhyphen"/>
	</xsl:when>
	<xsl:otherwise>
	  <g ref="char:EOLhyphen"/>
	</xsl:otherwise>
	</xsl:choose>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
	<xsl:value-of select="normalize-unicode(.,'NFC')"/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>TCP simple discard, you cannot use hi in a description</desc>
  </doc>
  <xsl:template match="FIGDESC/HI">
    <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>discard temporary header material</desc>
  </doc>
  <xsl:template match="TEMPHEAD|IDG"/>
  <xsl:template match="PB/@MS">
    <xsl:if test=".='Y'">
      <xsl:attribute name="rendition">simple:additions</xsl:attribute>
    </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>TCP controversial discards. Drop a set of attributes which
    don't work in P5</desc>
  </doc>
  <xsl:template match="LABEL/@ROLE"/>
  <xsl:template match="TITLE/@TYPE"/>
  <xsl:template match="GROUP/@TYPE"/>
  <xsl:template match="TITLE/@I2"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>multiple values for @lang are discarded</desc>
  </doc>
  <xsl:template match="@LANG[.='32' or contains(.,' ')]"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Milestones: convert to label, unless it has no @unit or occurs in some funny places</p>
    </desc>
  </doc>
  <xsl:template match="MILESTONE">
    <xsl:choose>
      <xsl:when test="not(@UNIT) or ( parent::LABEL or (parent::LIST and preceding-sibling::ITEM))">
	<xsl:call-template name="makenewmilestone"/>
      </xsl:when>
      <xsl:when test="parent::LIST and not(preceding-sibling::ITEM)">
	<head type="tcpmilestone">
	  <seg type="milestoneunit">
	    <xsl:value-of select="@UNIT"/>
	    <xsl:text> </xsl:text>
	  </seg>
	  <xsl:value-of select="@N"/>
	</head>
      </xsl:when>
      <xsl:when test="parent::BIBL and @UNIT">
	<note type="tcpmilestone">
	  <seg type="milestoneunit">
	    <xsl:value-of select="@UNIT"/>
	    <xsl:text> </xsl:text>
	  </seg>
	  <xsl:value-of select="@N"/>
	</note>
      </xsl:when>
      <xsl:when test="@UNIT='Ans;w.' and not(@N)">
	<milestone type="tcpmilestone" n="Ans;w." unit="unspecified"/>
      </xsl:when>
      <xsl:when test="@UNIT and (parent::SP or parent::SPEAKER or parent::DIV1 or parent::DIV2 or parent::DIV3 or parent::DIV4 or parent::DIV5 or parent::BODY)">
	<xsl:call-template name="makenewmilestone"/>
      </xsl:when>
      <xsl:otherwise>
	<label type="milestone">
	  <xsl:apply-templates select="@ID"/>
	  <xsl:apply-templates select="@REND"/>
	  <seg type="milestoneunit">
	    <xsl:value-of select="@UNIT"/>
	    <xsl:text> </xsl:text>
	  </seg>
	  <xsl:value-of select="@N"/>
	</label>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="makenewmilestone">
	<milestone type="tcpmilestone">
	  <xsl:choose>
	    <xsl:when test="@UNIT='years after chriſt' or @UNIT='years before chriſt'">
	      <xsl:attribute name="unit">unspecified</xsl:attribute>
	      <xsl:attribute name="n">
		<xsl:value-of select="(@N,@UNIT)"/>
	      </xsl:attribute>
	    </xsl:when>	
	    <xsl:when test="matches(@UNIT,'[\-A-z0-9]')">
	      <xsl:apply-templates select="@N"/>
	      <xsl:apply-templates select="@UNIT"/>
	    </xsl:when>
	    <xsl:when test="not(@UNIT)">
	      <xsl:attribute name="unit">unspecified</xsl:attribute>
	      <xsl:attribute name="n">
		<xsl:value-of select="@N"/>
	      </xsl:attribute>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:attribute name="unit">unspecified</xsl:attribute>
	      <xsl:attribute name="n">
		<xsl:value-of select="(@UNIT,@N)"/>
	      </xsl:attribute>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:apply-templates select="@*[not(name()='@N' or name()='UNIT')]|*|processing-instruction()|comment()|text()"/>
	</milestone>
      </xsl:template>
	
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Previous way of doing milestones:
	a) if there is no @n, just @unit  == marginal note

	b) if there is no @unit, just a @n,  == marginal note, @type='milestone'

	c) if @unit is from a closed list of words (page, line, folio), it
	seems editorial, add as subtype on @note

	d) otherwise, make a  label from @unit + @n, and put in a
	marginal note, @type='milestone'
      </p>
    </desc>
  </doc>

  <xsl:template match="OLDMILESTONE">
    <xsl:choose>
      <xsl:when test="parent::NOTE and not(@N)"/>
      <xsl:when test="@UNIT and (not(@N) or @N='')">
        <note place="margin" type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@UNIT"/>
        </note>
      </xsl:when>
      <xsl:when test="parent::L and @ID">
        <label type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@N"/>
        </label>
      </xsl:when>
      <xsl:when test="not(@UNIT) and @N">
        <note place="margin" type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@N"/>
        </note>
      </xsl:when>
      <xsl:when test="@UNIT='unspec' and @N">
        <note place="margin" type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@N"/>
        </note>
      </xsl:when>
      <!-- this short list seem like editorial words. are there more? -->
      <xsl:when test="          @UNIT='article' or          @UNIT='canon' or          @UNIT='chapter' or           @UNIT='commandment' or           @UNIT='date' or    @UNIT='day' or    @UNIT='folio' or    @UNIT='ground of' or    @UNIT='indulgence' or    @UNIT='leaf' or    @UNIT='line' or    @UNIT='monarch' or    @UNIT='motive' or    @UNIT='month' or    @UNIT='reason'  or    @UNIT='verse'  or    @UNIT='year'           ">
        <note place="margin" type="milestone" subtype="{@UNIT}">
          <xsl:apply-templates select="@ID"/>
          <!--
	  <xsl:if test="$debug='true'">
	    <xsl:message>Milestone 1: <xsl:value-of
	    select="@UNIT"/>/<xsl:value-of select="@N"/></xsl:message>
	  </xsl:if>
-->
          <xsl:value-of select="@N"/>
        </note>
      </xsl:when>
      <xsl:when test="parent::SP or parent::LIST or parent::SPEAKER or parent::LABEL   or parent::BIBL">
        <note place="margin" type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@UNIT"/>
          <xsl:text> </xsl:text>
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
        <label place="margin" type="milestone">
          <xsl:apply-templates select="@ID"/>
          <xsl:value-of select="@UNIT"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@N"/>
        </label>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A Q with just a Q inside it is redundant</p>
    </desc>
  </doc>
  <xsl:template match="Q[not(text()) and count(*)=1]/Q">
    <xsl:apply-templates/>
  </xsl:template>
  
<doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>A HEAD/@TYPE='sub' can lose itself if it consists of
      Q with L inside; though if thats all there is, it looks like
      an epigraph      </p>
    </desc>
  </doc>
  <xsl:template match="HEAD[@TYPE='sub']">
    <xsl:choose>
      <xsl:when test="following-sibling::HEAD or following-sibling::OPENER">
        <head type="sub">
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </head>
      </xsl:when>
      <xsl:when test="Q/L and not(P|GAP|text())">
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
      </xsl:when>
      <xsl:when test="Q/L and P|GAP">
        <head type="sub">
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </head>
      </xsl:when>
      <xsl:when test="Q[L] and not(text())">
        <epigraph>
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </epigraph>
      </xsl:when>
      <xsl:otherwise>
        <head type="sub">
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </head>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Strip $ from end of title</desc>
  </doc>
  <xsl:template match="TITLESTMT/TITLE/text()[last()]">
    <xsl:choose>
      <xsl:when test="matches(.,':$')">
	<xsl:value-of select="normalize-unicode(substring(.,1,string-length(.)-1),'NFC')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="normalize-unicode(.,'NFC')"/>
       </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>the HEADNOTE element can be bypassed if it just has a figure
    in, and no following head or opener</desc>
  </doc>
  <xsl:template match="HEADNOTE[P/FIGURE and          not(following-sibling::HEAD or following-sibling::OPENER)]">
    <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Just a HEAD inside an ARGUMENT can be replaced by a paragraph</desc>
  </doc>
  <xsl:template match="ARGUMENT[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Just a HEAD inside an HEADNOTE can be replaced by a paragraph</desc>
  </doc>
  <xsl:template match="HEADNOTE[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>A HEADNOTE is an ARGUMENT </desc>
  </doc>
  <xsl:template match="HEADNOTE">
    <argument>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </argument>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Just a HEAD inside an TAILNOTE can be replaced by a paragraph</desc>
  </doc>
  <xsl:template match="TAILNOTE[count(*)=1]/HEAD">
    <p>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>A TAILNOTE is an ARGUMENT </desc>
  </doc>
  <xsl:template match="TAILNOTE">
    <argument>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </argument>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>STAGE with a HEAD _after_ it brings the stage inside the head</desc>
  </doc>
  <xsl:template match="STAGE[following-sibling::HEAD]">
    <head type="sub">
      <stage>
        <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
      </stage>
    </head>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>put POSTSCRIPT as sibling of CLOSER, not child</desc>
  </doc>
  <xsl:template match="CLOSER">
    <xsl:choose>
      <xsl:when test="POSTSCRIPT">
        <closer>
          <xsl:apply-templates select="@*|*[not(self::POSTSCRIPT)]|processing-instruction()|comment()|text()"/>
        </closer>
        <xsl:apply-templates select="POSTSCRIPT"/>
      </xsl:when>
      <xsl:otherwise>
        <closer>
          <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
        </closer>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>no type attribute on postscript</desc>
  </doc>
  <xsl:template match="POSTSCRIPT/@TYPE"/>
  <!-- TCP non-controversial transforms -->
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>move PB outside ROW</desc>
  </doc>
  <xsl:template match="ROW/PB"/>
  <xsl:template match="ROW[PB]">
    <row>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
    </row>
    <xsl:for-each select="PB">
      <pb>
        <xsl:apply-templates select="@*"/>
      </pb>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="ROW/TABLE">
    <cell>
      <table>
        <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
      </table>
    </cell>
  </xsl:template>
  <xsl:template match="EEBO">
    <xsl:apply-templates select="*"/>
  </xsl:template>
  <xsl:template match="ETS">
    <TEI>
      <xsl:apply-templates select="@*"/>
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
      <xsl:variable name="hfile" select="concat($headerDirectory,$name,'.hdr')"/>
      <xsl:choose>
	<xsl:when test="TEIHEADER"/>
        <xsl:when test="doc-available($hfile)">
	  <xsl:message> attempt to load header <xsl:value-of select="$hfile"/></xsl:message>
          <xsl:for-each select="doc($hfile)">
            <xsl:apply-templates select="*"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="not(static-base-uri()='') and doc-available(resolve-uri($hfile,base-uri(/*)))">
          <xsl:for-each select="doc(resolve-uri($hfile,base-uri(/*)))">
            <xsl:apply-templates select="*"/>
          </xsl:for-each>
        </xsl:when>
      </xsl:choose>
      <xsl:apply-templates/>
    </TEI>
  </xsl:template>
  <xsl:template match="PUBLICATIONSTMT">
    <publicationStmt>
      <xsl:choose>
        <xsl:when test="PUBLISHER or AUTHORITY or DISTRIBUTOR">
          <xsl:apply-templates select="PUBLISHER|AUTHORITY|DISTRIBUTOR"/>
          <xsl:apply-templates select="*[not(self::PUBLISHER or            self::DISTRIBUTOR or            self::AUTHORITY)]"/>
          <xsl:if test="parent::FILEDESC">
            <xsl:call-template name="makeID"/>
          </xsl:if>
          <xsl:call-template name="idnoHook"/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:apply-templates/>
            <xsl:if test="parent::FILEDESC">
              <xsl:call-template name="makeID"/>
            </xsl:if>
            <xsl:call-template name="idnoHook"/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </publicationStmt>
  </xsl:template>
  <xsl:template match="IDNO/@TYPE">
    <xsl:attribute name="type">
      <xsl:value-of select="translate(upper-case(.),' ','-')"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="IDNO">
    <idno>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </idno>
  </xsl:template>
  <xsl:template match="FILEDESC/EXTENT">
    <extent>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </extent>
  </xsl:template>
  <xsl:template match="EEBO/GROUP">
    <text>
      <group>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates select="*"/>
      </group>
    </text>
  </xsl:template>
  <xsl:template match="FLOATEXT|LICENSE|LETTER">
    <floatingText>
      <xsl:if test="not(@TYPE)">
	<xsl:attribute name="type" select="lower-case(name())"/>
      </xsl:if>
      <xsl:apply-templates select="@*"/>
      <body>
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
      </body>
    </floatingText>
  </xsl:template>
  <xsl:template match="TEXT">
    <xsl:choose>
      <xsl:when test="parent::ETS or parent::EEBO or parent::GROUP">
        <text>
        <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
        </text>
      </xsl:when>
      <xsl:otherwise>
        <floatingText>
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
        </floatingText>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="LANGUSAGE/@ID"/>
  <xsl:template match="PB/@REF">
        <xsl:attribute name="facs">
	  <xsl:choose>
	    <xsl:when test="//HEADER//IDNO[@TYPE='evans citation']">
	      <xsl:value-of select="concat('unknown:',normalize-space(.))"/>
	    </xsl:when>
	    <xsl:when test="string-length(/ETS/EEBO/IDG/VID)&gt;0">
              <xsl:value-of select="('tcp',translate(normalize-space(/ETS/EEBO/IDG/VID),'  ',''),normalize-space(replace(.,'^\.','')))" separator=":"/>
	    </xsl:when>
	    <xsl:otherwise>
              <xsl:value-of select="normalize-space(.)"/>
	    </xsl:otherwise>
	  </xsl:choose>
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
        <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"/>
      </keywords>
    </xsl:if>
  </xsl:template>
  <xsl:template match="SUP">
    <seg rend="sup">
      <xsl:apply-templates/>
    </seg>
  </xsl:template>
  <xsl:template match="SUB">
    <seg rend="sub">
      <xsl:apply-templates/>
    </seg>
  </xsl:template>
  <xsl:template match="BELOW">
    <seg rend="below">
      <xsl:apply-templates/>
    </seg>
  </xsl:template>
  <xsl:template match="ABOVE">
    <seg rend="above">
      <xsl:apply-templates/>
    </seg>
  </xsl:template>
  <xsl:template match="HEADER">
    <teiHeader>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
    </teiHeader>
  </xsl:template>
  <xsl:template match="TEI.2|OTA">
    <TEI>
      <xsl:apply-templates select="@*|*|comment()|processing-instruction()"/>
    </TEI>
  </xsl:template>
  <xsl:template match="ADDNAME">
    <addName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </addName>
  </xsl:template>
  <xsl:template match="ADDSPAN">
    <addSpan>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </addSpan>
  </xsl:template>
  <xsl:template match="ADDRLINE">
    <addrLine>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </addrLine>
  </xsl:template>
  <xsl:template match="ALTGRP">
    <altGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </altGrp>
  </xsl:template>
  <xsl:template match="ATTDEF">
    <attDef>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </attDef>
  </xsl:template>
  <xsl:template match="ATTLIST">
    <attList>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </attList>
  </xsl:template>
  <xsl:template match="ATTNAME">
    <attName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </attName>
  </xsl:template>
  <xsl:template match="ATTLDECL">
    <attlDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </attlDecl>
  </xsl:template>
  <xsl:template match="BASEWSD">
    <baseWsd>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </baseWsd>
  </xsl:template>
  <xsl:template match="BIBLFULL">
    <biblFull>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </biblFull>
  </xsl:template>
  <xsl:template match="BIBLSCOPE">
    <biblScope>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </biblScope>
  </xsl:template>
  <xsl:template match="BIBLSTRUCT">
    <biblStruct>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </biblStruct>
  </xsl:template>
  <xsl:template match="CASTGROUP">
    <castGroup>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </castGroup>
  </xsl:template>
  <xsl:template match="CASTITEM">
    <castItem>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </castItem>
  </xsl:template>
  <xsl:template match="CASTLIST">
    <castList>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </castList>
  </xsl:template>
  <xsl:template match="CATDESC">
    <catDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </catDesc>
  </xsl:template>
  <xsl:template match="CATREF">
    <catRef>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </catRef>
  </xsl:template>
  <xsl:template match="CLASSCODE">
    <classCode>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </classCode>
  </xsl:template>
  <xsl:template match="CLASSDECL">
    <classDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </classDecl>
  </xsl:template>
  <xsl:template match="CLASSDOC">
    <classDoc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </classDoc>
  </xsl:template>
  <xsl:template match="CODEDCHARSET">
    <codedCharSet>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </codedCharSet>
  </xsl:template>
  <xsl:template match="DATADESC">
    <dataDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </dataDesc>
  </xsl:template>
  <xsl:template match="DATESTRUCT">
    <dateStruct>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </dateStruct>
  </xsl:template>
  <xsl:template match="DELSPAN">
    <delSpan>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </delSpan>
  </xsl:template>
  <xsl:template match="DIVGEN">
    <divGen>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </divGen>
  </xsl:template>
  <xsl:template match="DOCAUTHOR|DAUTHOR">
    <docAuthor>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </docAuthor>
  </xsl:template>
  <xsl:template match="DOCDATE">
    <docDate>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </docDate>
  </xsl:template>
  <xsl:template match="DOCEDITION">
    <docEdition>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </docEdition>
  </xsl:template>
  <xsl:template match="DOCIMPRINT">
    <docImprint>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </docImprint>
  </xsl:template>
  <xsl:template match="DOCTITLE|DTITLE">
    <docTitle>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </docTitle>
  </xsl:template>
  <xsl:template match="ELEAF">
    <eLeaf>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </eLeaf>
  </xsl:template>
  <xsl:template match="ETREE">
    <eTree>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </eTree>
  </xsl:template>
  <xsl:template match="EDITIONSTMT">
    <editionStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </editionStmt>
  </xsl:template>
  <xsl:template match="ELEMDECL">
    <elemDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </elemDecl>
  </xsl:template>
  <xsl:template match="EDITORIALDECL">
            <editorialDecl>
               <p>EEBO-TCP is a partnership between the Universities of Michigan and Oxford and the publisher ProQuest to create accurately transcribed and encoded texts based on the image sets published by ProQuest via their Early English Books Online (EEBO) database (http://eebo.chadwyck.com). The general aim of EEBO-TCP is to encode one copy (usually the first edition) of every monographic English-language title published between 1473 and 1700 available in EEBO.</p>
               <p>EEBO-TCP aimed to produce large quantities of textual data within the usual project restraints of time and funding, and therefore chose to create diplomatic transcriptions (as opposed to critical editions) with light-touch, mainly structural encoding based on the Text Encoding Initiative (http://www.tei-c.org).</p>
               <p>The EEBO-TCP project was divided into two phases. The 25,363 texts created during Phase 1 of the project have been released into the public domain as of 1 January 2015. Anyone can now take and use these texts for their own purposes, but we respectfully request that due credit and attribution is given to their original source.</p>
               <p>Users should be aware of the process of creating the TCP texts, and therefore of any assumptions that can be made about the data.</p>
               <p>Text selection was based on the New Cambridge Bibliography of English Literature (NCBEL). If an author (or for an anonymous work, the title) appears in NCBEL, then their works are eligible for inclusion. Selection was intended to range over a wide variety of subject areas, to reflect the true nature of the print record of the period. In general, first editions of a works in English were prioritized, although there are a number of works in other languages, notably Latin and Welsh, included and sometimes a second or later edition of a work was chosen if there was a compelling reason to do so.</p>
               <p>Image sets were sent to external keying companies for transcription and basic encoding. Quality assurance was then carried out by editorial teams in Oxford and Michigan. 5% (or 5 pages, whichever is the greater) of each text was proofread for accuracy and those which did not meet QA standards were returned to the keyers to be redone. After proofreading, the encoding was enhanced and/or corrected and characters marked as illegible were corrected where possible up to a limit of 100 instances per text. Any remaining illegibles were encoded as &lt;gap&gt;s. Understanding these processes should make clear that, while the overall quality of TCP data is very good, some errors will remain and some readable characters will be marked as illegible. Users should bear in mind that in all likelihood such instances will never have been looked at by a TCP editor.</p>
               <p>The texts were encoded and linked to page images in accordance with level 4 of the TEI in Libraries guidelines.</p>
            <p>Copies of the texts have been issued variously as SGML (TCP schema; ASCII text with mnemonic sdata character entities); displayable XML (TCP schema; characters represented either as UTF-8 Unicode or text strings within braces); or lossless XML (TEI P5, characters represented either as UTF-8 Unicode or TEI g elements).</p>
               <p>Keying and markup guidelines are available at the <ref target="http://www.textcreationpartnership.org/docs/.">Text Creation Partnership web site</ref>.</p>
         </editorialDecl>
  </xsl:template>
  <xsl:template match="ENCODINGDESC">
    <encodingDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
      <listPrefixDef>
        <prefixDef ident="tcp" matchPattern="([0-9\-]+):([0-9IVX]+)" replacementPattern="http://eebo.chadwyck.com/downloadtiff?vid=$1&amp;page=$2">
	</prefixDef>
        <prefixDef ident="char" matchPattern="(.+)" replacementPattern="https://raw.githubusercontent.com/textcreationpartnership/Texts/master/tcpchars.xml#$1">
	</prefixDef>
      </listPrefixDef>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="ENTDOC">
    <entDoc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </entDoc>
  </xsl:template>
  <xsl:template match="ENTNAME">
    <entName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </entName>
  </xsl:template>
  <xsl:template match="ENTITYSET">
    <entitySet>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </entitySet>
  </xsl:template>
  <xsl:template match="ENTRYFREE">
    <entryFree>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </entryFree>
  </xsl:template>
  <xsl:template match="EXTFIGURE">
    <figure>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>
  <xsl:template match="FALT">
    <fAlt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fAlt>
  </xsl:template>
  <xsl:template match="FDECL">
    <fDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fDecl>
  </xsl:template>
  <xsl:template match="FDESCR">
    <fDescr>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fDescr>
  </xsl:template>
  <xsl:template match="FLIB">
    <fLib>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fLib>
  </xsl:template>
  <xsl:template match="FIGDESC">
    <figDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:value-of select="translate(.,'∣','')"/>
    </figDesc>
  </xsl:template>
  <xsl:template match="FILEDESC">
    <fileDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fileDesc>
  </xsl:template>
  <xsl:template match="FIRSTLANG">
    <firstLang>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </firstLang>
  </xsl:template>
  <xsl:template match="FORENAME">
    <foreName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </foreName>
  </xsl:template>
  <xsl:template match="FORESTGRP">
    <forestGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </forestGrp>
  </xsl:template>
  <xsl:template match="FSCONSTRAINTS">
    <fsConstraints>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fsConstraints>
  </xsl:template>
  <xsl:template match="FSDECL">
    <fsDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fsDecl>
  </xsl:template>
  <xsl:template match="FSDESCR">
    <fsDescr>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fsDescr>
  </xsl:template>
  <xsl:template match="FSLIB">
    <fsLib>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fsLib>
  </xsl:template>
  <xsl:template match="FSDDECL">
    <fsdDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fsdDecl>
  </xsl:template>
  <xsl:template match="FVLIB">
    <fvLib>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </fvLib>
  </xsl:template>
  <xsl:template match="GENNAME">
    <genName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </genName>
  </xsl:template>
  <xsl:template match="GEOGNAME">
    <geogName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </geogName>
  </xsl:template>
  <xsl:template match="GRAMGRP">
    <gramGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </gramGrp>
  </xsl:template>
  <xsl:template match="HANDLIST">
    <handList>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </handList>
  </xsl:template>
  <xsl:template match="HANDSHIFT">
    <handShift>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </handShift>
  </xsl:template>
  <xsl:template match="HEADITEM">
    <headItem>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </headItem>
  </xsl:template>
  <xsl:template match="HEADLABEL">
    <headLabel>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </headLabel>
  </xsl:template>
  <xsl:template match="INODE">
    <iNode>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </iNode>
  </xsl:template>
  <xsl:template match="INTERPGRP">
    <interpGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </interpGrp>
  </xsl:template>
  <xsl:template match="JOINGRP">
    <joinGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </joinGrp>
  </xsl:template>
  <xsl:template match="LACUNAEND">
    <lacunaEnd>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </lacunaEnd>
  </xsl:template>
  <xsl:template match="LACUNASTART">
    <lacunaStart>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </lacunaStart>
  </xsl:template>
  <xsl:template match="LANGKNOWN">
    <langKnown>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </langKnown>
  </xsl:template>
  <xsl:template match="LANGUSAGE">
    <langUsage>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </langUsage>
  </xsl:template>
  <xsl:template match="LINKGRP">
    <linkGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </linkGrp>
  </xsl:template>
  <xsl:template match="LISTBIBL">
    <listBibl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </listBibl>
  </xsl:template>
  <xsl:template match="METDECL">
    <metDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </metDecl>
  </xsl:template>
  <xsl:template match="NAMELINK">
    <nameLink>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </nameLink>
  </xsl:template>
  <xsl:template match="NOTESSTMT">
    <notesStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </notesStmt>
  </xsl:template>
  <xsl:template match="OREF">
    <oRef>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </oRef>
  </xsl:template>
  <xsl:template match="OVAR">
    <oVar>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </oVar>
  </xsl:template>
  <xsl:template match="OFFSET">
    <offSet>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </offSet>
  </xsl:template>
  <xsl:template match="ORGDIVN">
    <orgDivn>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </orgDivn>
  </xsl:template>
  <xsl:template match="ORGNAME">
    <orgName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </orgName>
  </xsl:template>
  <xsl:template match="ORGTITLE">
    <orgTitle>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </orgTitle>
  </xsl:template>
  <xsl:template match="ORGTYPE">
    <orgType>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </orgType>
  </xsl:template>
  <xsl:template match="OTHERFORM">
    <otherForm>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </otherForm>
  </xsl:template>
  <xsl:template match="PREF">
    <pRef>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </pRef>
  </xsl:template>
  <xsl:template match="PVAR">
    <pVar>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </pVar>
  </xsl:template>
  <xsl:template match="PARTICDESC">
    <particDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </particDesc>
  </xsl:template>
  <xsl:template match="PARTICLINKS">
    <particLinks>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </particLinks>
  </xsl:template>
  <xsl:template match="PERSNAME">
    <persName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </persName>
  </xsl:template>
  <xsl:template match="PERSONGRP">
    <personGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </personGrp>
  </xsl:template>
  <xsl:template match="PLACENAME">
    <placeName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </placeName>
  </xsl:template>
  <xsl:template match="POSTBOX">
    <postBox>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </postBox>
  </xsl:template>
  <xsl:template match="POSTCODE">
    <postCode>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </postCode>
  </xsl:template>
  <xsl:template match="PROFILEDESC">
    <profileDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </profileDesc>
  </xsl:template>
  <xsl:template match="PROJECTDESC">
    <projectDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </projectDesc>
  </xsl:template>
  <xsl:template match="PUBPLACE">
    <xsl:choose>
      <xsl:when test="parent::PUBLICATIONSTMT/PUBLISHER or    parent::PUBLICATIONSTMT/AUTHORITY or    parent::PUBLICATIONSTMT/DISTRIBUTOR">
        <pubPlace>
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates/>
        </pubPlace>
      </xsl:when>
      <xsl:otherwise>
        <name type="place">
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates/>
        </name>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="RDGGRP">
    <rdgGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </rdgGrp>
  </xsl:template>
  <xsl:template match="RECORDINGSTMT">
    <recordingStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </recordingStmt>
  </xsl:template>
  <xsl:template match="REFSDECL">
    <refsDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </refsDecl>
  </xsl:template>
  <xsl:template match="RESPSTMT">
    <respStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </respStmt>
  </xsl:template>
  <xsl:template match="REVISIONDESC|REVDESC">
    <revisionDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </revisionDesc>
  </xsl:template>
  <xsl:template match="ROLEDESC">
    <roleDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </roleDesc>
  </xsl:template>
  <xsl:template match="ROLENAME">
    <roleName>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </roleName>
  </xsl:template>
  <xsl:template match="SAMPLINGDECL">
    <samplingDecl>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </samplingDecl>
  </xsl:template>
  <xsl:template match="SCRIPTSTMT">
    <scriptStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </scriptStmt>
  </xsl:template>
  <xsl:template match="SERIESSTMT">
    <seriesStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </seriesStmt>
  </xsl:template>
  <xsl:template match="SETTINGDESC">
    <settingDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </settingDesc>
  </xsl:template>
  <xsl:template match="SOCALLED">
    <soCalled>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </soCalled>
  </xsl:template>
  <xsl:template match="SOCECSTATUS">
    <socecStatus>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </socecStatus>
  </xsl:template>
  <xsl:template match="SOURCEDESC">
    <sourceDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </sourceDesc>
  </xsl:template>
  <xsl:template match="SPANGRP">
    <spanGrp>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </spanGrp>
  </xsl:template>
  <xsl:template match="STDVALS">
    <stdVals>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </stdVals>
  </xsl:template>
  <xsl:template match="TAGDOC">
    <tagDoc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </tagDoc>
  </xsl:template>
  <xsl:template match="TAGUSAGE">
    <tagUsage>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </tagUsage>
  </xsl:template>
  <xsl:template match="TEIFSD2">
    <teiFsd2>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </teiFsd2>
  </xsl:template>
  <xsl:template match="TERMENTRY">
    <termEntry>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </termEntry>
  </xsl:template>
  <xsl:template match="TEXTCLASS">
    <textClass>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </textClass>
  </xsl:template>
  <xsl:template match="TEXTDESC">
    <textDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </textDesc>
  </xsl:template>
  <xsl:template match="TIMERANGE">
    <timeRange>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </timeRange>
  </xsl:template>
  <xsl:template match="TIMESTRUCT">
    <timeStruct>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </timeStruct>
  </xsl:template>
  <xsl:template match="TITLEPAGE|TPAGE">
    <titlePage>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </titlePage>
  </xsl:template>
  <xsl:template match="TITLEPART">
    <titlePart>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </titlePart>
  </xsl:template>
  <xsl:template match="TITLESTMT">
    <titleStmt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </titleStmt>
  </xsl:template>
  <xsl:template match="VALT">
    <vAlt>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </vAlt>
  </xsl:template>
  <xsl:template match="VDEFAULT">
    <vDefault>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </vDefault>
  </xsl:template>
  <xsl:template match="VRANGE">
    <vRange>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </vRange>
  </xsl:template>
  <xsl:template match="VALDESC">
    <valDesc>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </valDesc>
  </xsl:template>
  <xsl:template match="VALLIST">
    <valList>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </valList>
  </xsl:template>
  <xsl:template match="VARIANTENCODING">
    <variantEncoding>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </variantEncoding>
  </xsl:template>
  <xsl:template match="WITDETAIL">
    <witDetail>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </witDetail>
  </xsl:template>
  <xsl:template match="WITEND">
    <witEnd>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </witEnd>
  </xsl:template>
  <xsl:template match="WITSTART">
    <witStart>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
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
  <xsl:template match="TEIHEADER/@TYPE"/>
  <xsl:template match="@TARGTYPE">
    <xsl:attribute name="type">
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
  <xsl:template match="@TEIFORM"/>
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
    <xsl:variable name="p" select="lower-case(.)"/>
    <xsl:choose>
      <xsl:when test="$p='marg' or $p='marg;' or $p='marg)' or $p='marg='         or $p='ma / rg' or $p='6marg'">
        <xsl:attribute name="place">margin</xsl:attribute>
      </xsl:when>
      <xsl:when test="$p = 'unspecified'"/>
      <xsl:when test="$p='foot;' or $p='foor;' or $p='foot'">
        <xsl:attribute name="place">bottom</xsl:attribute>
      </xsl:when>
      <xsl:when test="$p='foot1' or $p='foot2'">
        <xsl:attribute name="place">bottom</xsl:attribute>
        <xsl:attribute name="type" select="$p"/>
      </xsl:when>
      <xsl:when test="$p='inter'">
        <xsl:attribute name="rend" select="$p"/>
      </xsl:when>
      <xsl:when test="$p='‡' or $p='†' or $p='‖' or $p='6'  or $p='“' or         $p='1' or $p='*'">
        <xsl:attribute name="n">
          <xsl:value-of select="$p"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
        <xsl:attribute name="place">
          <xsl:value-of select="$p"/>
        </xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@SAMPLE">
    <xsl:if test="not(. = 'COMPLETE')">
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
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </encodingDesc>
  </xsl:template>
  <xsl:template match="EDSTMT">
    <editorialStmt>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </editorialStmt>
  </xsl:template>
  <xsl:template match="TITLSTMT">
    <titleStmt>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
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
          <xsl:analyze-string regex="([0-9]+)(.*)" select="translate(translate(.,'( &amp;/', '____'),$intype,'')">
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
  <xsl:template match="NOTE/@TYPE[.=../MILESTONE/@UNIT]"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	a note following by a closer as content of body? please, wrap
	it in a p
      </p>
    </desc>
  </doc>
  <xsl:template match="LETTER/NOTE">
    <xsl:choose>
      <xsl:when test="count(parent::LETTER/*)=2 and         following-sibling::CLOSER">
        <p>
          <note>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
          </note>
        </p>
      </xsl:when>
      <xsl:otherwise>
        <note>
          <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </note>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@UNIT">
    <xsl:attribute name="unit">
      <xsl:value-of select="translate(.,'ſ','s')"/>
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
  <xsl:template match="P[count(FIGURE)=count(*) and not (text()) and parent::*/count(P[not(FIGURE)])&gt;1]">
    <xsl:apply-templates/>
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
    <xsl:variable name="vals">
      <xsl:for-each select="tokenize(.,' ')">
        <a>
          <xsl:choose>
            <xsl:when test="starts-with(.,'http') or starts-with(.,'ftp') or starts-with(.,'mailto')">
              <xsl:sequence select="."/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>#</xsl:text>
              <xsl:sequence select="."/>
            </xsl:otherwise>
          </xsl:choose>
        </a>
      </xsl:for-each>
    </xsl:variable>
    <xsl:attribute name="{lower-case(name(.))}" select="string-join($vals/tei:a,' ')"/>
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
  <xsl:template match="ROW/@ROLE">
    <xsl:attribute name="role">
      <xsl:value-of select="lower-case(.)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="CELL/@ROLE">
    <xsl:attribute name="role">
      <xsl:value-of select="lower-case(.)"/>
    </xsl:attribute>
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
      <xsl:value-of select="lower-case(.)"/>
      <xsl:if test="parent::GAP/@REASON">
	<xsl:text>: </xsl:text>
	<xsl:value-of select="parent::GAP/@REASON"/>
      </xsl:if>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="GAP/@DISP">
    <desc>
      <xsl:value-of select="."/>
    </desc>
  </xsl:template>
  <xsl:template match="GAP">
    <gap>
      <xsl:apply-templates select="@DESC"/>
      <xsl:apply-templates select="@RESP"/>
      <xsl:apply-templates select="@EXTENT"/>
      <xsl:apply-templates select="@DISP"/>
    </gap>
  </xsl:template>
  <!--  creating a choice element -->
  <xsl:template match="CORR[@SIC]">
    <choice>
      <corr>
        <xsl:apply-templates select="text()"/>
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
  <xsl:template match="FILEDESC/PUBLICATIONSTMT/DATE">
    <date>
      <xsl:variable name="d">
        <xsl:choose>
          <xsl:when test="contains(.,' (')">
            <xsl:value-of select="substring-before(.,' (')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="replace(.,'[\?\.\[\]]','')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:attribute name="when">
        <xsl:analyze-string select="$d" regex="([0-9][0-9][0-9][0-9]) ([A-z]+)">
          <xsl:matching-substring>
            <xsl:value-of select="regex-group(1)"/>
            <xsl:text>-</xsl:text>
            <xsl:choose>
              <xsl:when test="regex-group(2)='January'">01</xsl:when>
              <xsl:when test="regex-group(2)='February'">02</xsl:when>
              <xsl:when test="regex-group(2)='March'">03</xsl:when>
              <xsl:when test="regex-group(2)='April'">04</xsl:when>
              <xsl:when test="regex-group(2)='May'">05</xsl:when>
              <xsl:when test="regex-group(2)='June'">06</xsl:when>
              <xsl:when test="regex-group(2)='July'">07</xsl:when>
              <xsl:when test="regex-group(2)='August'">08</xsl:when>
              <xsl:when test="regex-group(2)='September'">09</xsl:when>
              <xsl:when test="regex-group(2)='October'">10</xsl:when>
              <xsl:when test="regex-group(2)='November'">11</xsl:when>
              <xsl:when test="regex-group(2)='December'">12</xsl:when>
            </xsl:choose>
          </xsl:matching-substring>
          <xsl:non-matching-substring>
            <xsl:value-of select="."/>
          </xsl:non-matching-substring>
        </xsl:analyze-string>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </date>
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
            <xsl:apply-templates select="resp/text()"/>
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
      <xsl:apply-templates select="*|@*|comment()|processing-instruction()|text()"/>
    </div>
  </xsl:template>
  <!-- remove default values for attributes -->
  <xsl:template match="ROW/@ROLE[.='data']"/>
  <xsl:template match="CELL/@ROLE[.='data']"/>
  <xsl:template match="ROW/@ROLE[.='DATA']"/>
  <xsl:template match="CELL/@ROLE[.='DATA']"/>
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
  <xsl:template match="FIGDESC/HI[@rend='sup']">
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
                  <catDesc>
                    <xsl:value-of select="."/>
                  </catDesc>
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
  <!-- second pass to clean up -->
  <xsl:template match="@*|comment()|processing-instruction()|text()" mode="pass2">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|comment()|processing-instruction()|text()" mode="pass2"/>
    </xsl:copy>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	zap empty postscript
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:closer[not(* or text())]"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	The subtype attribute cant have spaces
      </p>
    </desc>
  </doc>
  <xsl:template match="@subtype" mode="pass2">
    <xsl:attribute name="subtype">
      <xsl:value-of select="translate(.,' ','_')"/>
    </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	The facs attribute should not have spaces or square brackets in
      </p>
    </desc>
  </doc>
  <xsl:template match="@facs" mode="pass2">
    <xsl:attribute name="facs">
      <xsl:value-of select="translate(.,' []','_()')"/>
    </xsl:attribute>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A p with list, floatingText or table as singletons can lose itself
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:p[not(parent::tei:sp or parent::tei:headnote or          parent::tei:postscript or parent::tei:argument) and count(*)=1 and          not(text()) and   (tei:list or tei:table)]">
    <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A singleton p inside a note is bypassed
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:note[count(*)=1 and not(text())]/tei:p">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A singleton floatingText inside a q can skip the q
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:q[count(*)=1 and not(text()) and tei:floatingText]">
    <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	A singleton p inside a cell is bypassed
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:cell[count(*)=1 and not(text()) and tei:p]" mode="pass2">
    <cell>
      <xsl:apply-templates select="@*"/>
      <xsl:for-each select="tei:p">
        <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="pass2"/>
      </xsl:for-each>
    </cell>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	p inside add means we must make an addSpan
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:add[tei:p]" mode="pass2">
    <xsl:choose>
      <xsl:when test="parent::tei:p">
        <xsl:for-each select="tei:p">
          <p>
            <xsl:apply-templates select="@*" mode="pass2"/>
            <add>
              <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
            </add>
          </p>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="count(tei:p)=1">
        <add>
          <xsl:for-each select="tei:p">
            <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
          </xsl:for-each>
        </add>
      </xsl:when>
      <xsl:otherwise>
        <addSpan>
          <xsl:attribute name="spanTo">
            <xsl:text>#addSpan</xsl:text>
            <xsl:number level="any"/>
          </xsl:attribute>
        </addSpan>
        <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
        <anchor>
          <xsl:attribute name="xml:id">
            <xsl:text>addSpan</xsl:text>
            <xsl:number level="any"/>
          </xsl:attribute>
        </anchor>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <desc>
    <p>
	a paragraph containing add and nothing else, where those adds
	themselves contains paragraphs, can be bypassed
      </p>
  </desc>
  <xsl:template match="tei:p[tei:add/tei:p and not(text())]" mode="pass2">
    <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	a list inside a label in a gloss list will have to turn into a table
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:list[tei:label/tei:list]" mode="pass2">
    <table rend="braced">
      <xsl:for-each select="tei:label">
        <row>
          <cell>
            <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
          </cell>
          <cell>
            <xsl:for-each select="following-sibling::tei:item[1]">
              <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
            </xsl:for-each>
          </cell>
        </row>
      </xsl:for-each>
    </table>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>
	a list inside a label in a gloss list (alternate way of doing
	gloss lists) will have to turn into a table
      </p>
    </desc>
  </doc>
  <xsl:template match="tei:list[tei:item/tei:label/tei:list]" mode="pass2">
    <table rend="braced">
      <xsl:for-each select="tei:item">
        <row>
          <cell>
            <xsl:for-each select="tei:label">
              <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
            </xsl:for-each>
          </cell>
          <cell>
            <xsl:apply-templates select="*[not(self::tei:label)]|text()|processing-instruction()|comment()" mode="pass2"/>
          </cell>
        </row>
      </xsl:for-each>
    </table>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>a singleton label inside a paragraph, containing a list, can
	be ignored.      </p>
    </desc>
  </doc>
  <xsl:template match="tei:label[tei:list and parent::tei:p]" mode="pass2">
    <xsl:apply-templates select="*|text()|processing-instruction()|comment()" mode="pass2"/>
  </xsl:template>
  <xsl:template match="tei:label[following-sibling::*[1][self::tei:head]]" mode="pass2"/>
  <xsl:template match="tei:head[preceding-sibling::*[1][self::tei:label]]" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|comment()|processing-instruction()|text()" mode="pass2"/>
    </xsl:copy>
    <xsl:for-each select="preceding-sibling::*[1][self::tei:label]">
      <note>
        <xsl:apply-templates select="*|@*|comment()|processing-instruction()|text()" mode="pass2"/>
      </note>
    </xsl:for-each>
  </xsl:template>
  <!-- pass 3 -->
  <xsl:template match="@*|comment()|processing-instruction()|text()" mode="pass3">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass3">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|comment()|processing-instruction()|text()" mode="pass3"/>
    </xsl:copy>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>split up nested paragraphs</desc>
  </doc>
  <xsl:template match="tei:p[tei:p]" mode="pass3">
    <xsl:variable name="here" select="."/>
    <xsl:for-each-group select="node()" group-adjacent="if (self::tei:p) then 1 else 2">
      <xsl:choose>
        <xsl:when test="current-grouping-key()=1">
          <xsl:apply-templates select="current-group()" mode="pass3"/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:copy-of select="$here/@*"/>
            <xsl:apply-templates select="current-group()" mode="pass3"/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>generate appropriate availability</desc>
  </doc>

  <xsl:template match="tei:availability" mode="pass3">
<xsl:variable name="d" select="/*/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
            <availability>
	      <xsl:choose>
		<xsl:when test="contains($d,'Phase 2')">
               <p>This keyboarded and encoded edition of the work
	       described above is co-owned by the institutions
	       providing financial support to the Early English Books
	       Online Text Creation Partnership. Searching, reading,
	       printing, or downloading EEBO-TCP texts is reserved for
	       the authorized users of these project partner
	       institutions. Permission must be granted for subsequent
	       distribution, in print or electronically, of this
	       EEBO-TCP Phase II text, in whole or in part.</p>
		</xsl:when>
		<xsl:otherwise>
               <p>This keyboarded and encoded edition of the
	       work described above is co-owned by the institutions
	       providing financial support to the Early English Books
	       Online Text Creation Partnership. This Phase I text is
	       available for reuse, according to the terms of <ref
	       target="https://creativecommons.org/publicdomain/zero/1.0/">Creative
	       Commons 0 1.0 Universal</ref>. The text can be copied,
	       modified, distributed and performed, even for
	       commercial purposes, all without asking permission.</p>
		</xsl:otherwise>
	      </xsl:choose>
            </availability>   
  </xsl:template>

</xsl:stylesheet>

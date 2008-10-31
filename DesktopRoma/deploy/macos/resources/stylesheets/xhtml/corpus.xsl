<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="xd exsl estr edate a fo local rng tei teix"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the corpus module,
      making HTML output. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id: corpus.xsl 4808 2008-09-17 12:09:06Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:catRef</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:catRef"><xsl:variable name="W">
      <xsl:choose>
        <xsl:when test="starts-with(@target,'#')">
          <xsl:value-of select="substring-after(@target,'#')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@target"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable><xsl:if test="preceding-sibling::tei:catRef">
      <xsl:text> 
    </xsl:text>
    </xsl:if><em>
      <xsl:value-of select="@scheme"/>
    </em>: <xsl:apply-templates select="key('IDS',$W)/catDesc"/></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:teiCorpus</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:teiCorpus">
    <html>
      <xsl:call-template name="addLangAtt"/>
      <head>
        <title>
          <xsl:apply-templates
            select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/>
        </title>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
      </head>
      <body class="simple">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
	<div class="stdheader">
        <xsl:call-template name="stdheader">
          <xsl:with-param name="title">
            <xsl:apply-templates
              select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
          </xsl:with-param>
        </xsl:call-template>
	</div>
        <xsl:call-template name="corpusBody"/>
        <xsl:call-template name="stdfooter"/>
        <xsl:call-template name="bodyEndHook"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="tei:teiCorpus" mode="split">
    <xsl:variable name="BaseFile">
      <xsl:value-of select="$masterFile"/>
      <xsl:call-template name="addCorpusID"/>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML: run start hook template teiStartHook</xsl:message>
    </xsl:if>
    <xsl:call-template name="teiStartHook"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML in corpus splitting mode, base file is <xsl:value-of
          select="$BaseFile"/>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="outputChunk">
      <xsl:with-param name="ident">
        <xsl:value-of select="$BaseFile"/>
      </xsl:with-param>
      <xsl:with-param name="content">
    <html>
      <xsl:call-template name="addLangAtt"/>
      <xsl:call-template name="includeCSS"/>
      <head>
        <title>
          <xsl:apply-templates
            select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/>
        </title>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
      </head>
      <body class="simple">
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
	<div class="stdheader">
        <xsl:call-template name="stdheader">
          <xsl:with-param name="title">
            <xsl:apply-templates
              select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]"/>
          </xsl:with-param>
        </xsl:call-template>
	</div>
        <xsl:call-template name="corpusBody"/>
        <xsl:call-template name="stdfooter"/>
        <xsl:call-template name="bodyEndHook"/>
      </body>
    </html>

      </xsl:with-param>
    </xsl:call-template>
    <xsl:if test="$verbose='true'">
      <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
    </xsl:if>
    <xsl:call-template name="teiEndHook"/>
    <xsl:apply-templates select="tei:TEI" mode="split"/>

  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="corpusBody">
    <ul>
      <xsl:for-each select="tei:TEI">
        <li>
          <a>
            <xsl:attribute name="href">
              <xsl:apply-templates mode="generateLink" select="."/>
            </xsl:attribute>
            <xsl:call-template name="header">
              <xsl:with-param name="minimal">false</xsl:with-param>
	      <xsl:with-param name="display">plain</xsl:with-param>
            </xsl:call-template>
          </a>
        </li>
      </xsl:for-each>
    </ul>
  </xsl:template>
</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings"
  xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  extension-element-prefixes="exsl estr edate" 
  exclude-result-prefixes="xd exsl estr edate a fo local rng tei teix" 
  version="1.0">
  
<xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    dealing  with elements from the
      corpus module, making HTML output.
      </xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </xd:detail>
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  
<xd:doc>
    <xd:short>Process elements  tei:catRef</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:catRef">
  <xsl:variable name="W">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
	<xsl:value-of select="substring-after(@target,'#')"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@target"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="preceding-sibling::tei:catRef">
    <xsl:text> 
    </xsl:text>
  </xsl:if>
  <em><xsl:value-of select="@scheme"/></em>: <xsl:apply-templates select="key('IDS',$W)/catDesc"/>
</xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:teiCorpus</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:teiCorpus">
    <xsl:for-each select="tei:TEI">
      <xsl:if test="$verbose">
        <xsl:message>Process <xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/></xsl:message>
      </xsl:if>
      <xsl:apply-templates select="." mode="split"/>
    </xsl:for-each>
    <html>
      <xsl:call-template name="addLangAtt"/>
      <head>
        <title>
          <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/>
        </title>
        <xsl:call-template name="includeCSS"/>
	<xsl:call-template name="cssHook"/>
      </head>
      <body class="simple">
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavaScriptHook"/>
        <xsl:call-template name="stdheader">
          <xsl:with-param name="title">
            <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="corpusBody"/>
        <xsl:call-template name="stdfooter">
          <xsl:with-param name="date">
            <xsl:choose>
              <xsl:when test="tei:teiHeader/tei:revisionDesc//tei:date[1]">
                <xsl:value-of select="tei:teiHeader/tei:revisionDesc//tei:date[1]"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>(undated)</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
          <xsl:with-param name="author"/>
        </xsl:call-template>
      </body>
    </html>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>&#160;</xd:detail>
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
            </xsl:call-template>
          </a>
        </li>
      </xsl:for-each>
    </ul>
  </xsl:template>
</xsl:stylesheet>

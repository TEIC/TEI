<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="exsl edate estr tei t a rng s xd xs"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:t="http://www.thaiopensource.com/ns/annotations"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:import href="teiodds.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for making Relax NG schema from ODD </xd:short>
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:output encoding="utf-8" indent="yes" method="xml"/>
  <xsl:param name="verbose"/>
  <xsl:param name="outputDir">Schema</xsl:param>
  <xsl:param name="appendixWords"/>
  <xsl:variable name="headingNumberSuffix"/>
  <xsl:variable name="numberBackHeadings"/>
  <xsl:variable name="numberFrontHeadings"/>
  <xsl:variable name="numberHeadings"/>
  <xsl:variable name="numberHeadingsDepth"/>
  <xsl:variable name="prenumberedHeadings"/>
  <xsl:template name="italicize"/>
  <xsl:template name="makeAnchor"/>
  <xsl:template name="makeLink"/>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:variable name="oddmode">dtd</xsl:variable>
  <xsl:variable name="filesuffix"/>
  <!-- get list of output files -->
  <xsl:variable name="linkColor"/>
  <xsl:template match="tei:moduleSpec[@type='decls']"/>
  <xsl:template match="/">
    <xsl:choose>
      <xsl:when test=".//tei:schemaSpec">
        <xsl:apply-templates select=".//tei:schemaSpec"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="key('Modules',1)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
    <xsl:variable name="filename" select="@ident"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> process schemaSpec [<xsl:value-of select="@ident"/>]
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="generateOutput">
      <xsl:with-param name="method">xml</xsl:with-param>
      <xsl:with-param name="suffix">.rng</xsl:with-param>
      <xsl:with-param name="body">
        <rng:grammar
          datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes"
          xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
          xmlns:rng="http://relaxng.org/ns/structure/1.0"
          xmlns:t="http://www.thaiopensource.com/ns/annotations"
	  xmlns:xlink="http://www.w3.org/1999/xlink"
          xmlns:teix="http://www.tei-c.org/ns/Examples">
          <xsl:attribute name="ns">
            <xsl:choose>
              <xsl:when test="@ns">
                <xsl:value-of select="@ns"/>
              </xsl:when>
              <xsl:when test="$TEIC='true'">
                <xsl:text>http://www.tei-c.org/ns/1.0</xsl:text>
              </xsl:when>
              <xsl:otherwise/>
            </xsl:choose>
          </xsl:attribute>
          <xsl:comment><xsl:text>Schema generated from ODD source </xsl:text><xsl:call-template
              name="showDate"/>. <xsl:apply-templates mode="doc"
              select="tei:desc"/></xsl:comment>
          <xsl:if test="$TEIC='true'">
            <xsl:comment>
              <xsl:call-template name="copyright"/>
            </xsl:comment>
          </xsl:if>
          <xsl:apply-templates mode="tangle" select="tei:specGrpRef"/>
          <xsl:apply-templates mode="tangle" select="tei:moduleRef"/>
          <xsl:for-each select="tei:macroSpec">
            <xsl:apply-templates mode="tangle" select="."/>
            <!--
	      <xsl:choose>
		<xsl:when test="@predeclare='true'"/>
		<xsl:when test="key('PredeclareMacros',@ident)"/>
		<xsl:otherwise>
		  <xsl:apply-templates select="." mode="tangle"/>
		</xsl:otherwise>
	    </xsl:choose>
-->
          </xsl:for-each>
          <xsl:apply-templates mode="tangle"
            select="tei:elementSpec|tei:classSpec"/>
          <xsl:choose>
            <xsl:when test="@start and @start=''"/>
            <xsl:when test="@start and contains(@start,' ')">
              <rng:start>
                <rng:choice>
                  <xsl:call-template name="startNames">
                    <xsl:with-param name="toks" select="@start"/>
                  </xsl:call-template>
                </rng:choice>
              </rng:start>
            </xsl:when>
            <xsl:when test="@start">
              <rng:start>
                <rng:ref name="{$patternPrefixText}{@start}"/>
              </rng:start>
            </xsl:when>
            <xsl:when test="key('IDENTS','teiCorpus')">
              <rng:start>
                <rng:choice>
                  <rng:ref name="{$patternPrefixText}TEI"/>
                  <rng:ref name="{$patternPrefixText}teiCorpus"/>
                </rng:choice>
              </rng:start>
            </xsl:when>
            <xsl:otherwise>
              <rng:start>
                <rng:ref name="{$patternPrefixText}TEI"/>
              </rng:start>
            </xsl:otherwise>
          </xsl:choose>
        </rng:grammar>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="startNames">
    <xsl:param name="toks"/>
    <xsl:if test="not($toks='')">
      <xsl:choose>
        <xsl:when test="contains($toks,' ')">
          <ref name="{$patternPrefixText}{substring-before($toks, ' ')}"
            xmlns="http://relaxng.org/ns/structure/1.0"/>
          <xsl:call-template name="startNames">
            <xsl:with-param name="toks" select="substring-after($toks, ' ')"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <ref name="{$patternPrefixText}{$toks}"
            xmlns="http://relaxng.org/ns/structure/1.0"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:moduleSpec">
    <xsl:if
      test="@ident and not(@mode='change' or @mode='replace' or   @mode='delete')">
      <xsl:choose>
        <xsl:when test="parent::tei:schemaSpec">
          <xsl:call-template name="moduleSpec-body"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="generateOutput">
            <xsl:with-param name="method">xml</xsl:with-param>
            <xsl:with-param name="suffix">.rng</xsl:with-param>
            <xsl:with-param name="body">
              <rng:grammar
                datatypeLibrary="http://www.w3.org/2001/XMLSchema-datatypes"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations">
                <xsl:comment><xsl:text>Schema generated </xsl:text><xsl:call-template
                    name="showDate"/>.. <xsl:if test="$TEIC='true'">
                    <xsl:call-template name="copyright"/>
                  </xsl:if><xsl:apply-templates mode="doc" select="tei:desc"/></xsl:comment>
                <xsl:call-template name="moduleSpec-body"/>
              </rng:grammar>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  <xsl:template name="moduleSpec-body">
    <xsl:variable name="filename" select="@ident"/>
    <xsl:comment>Definitions from module <xsl:value-of select="@ident"/>
    </xsl:comment>
    <xsl:comment>Set global predeclared macros</xsl:comment>
    <xsl:if test="@type='core'">
      <xsl:call-template name="NameList"/>
      <xsl:for-each select="key('PredeclareAllMacros','1')">
        <define name="{@ident}" xmlns="http://relaxng.org/ns/structure/1.0">
          <choice>
            <notAllowed/>
          </choice>
        </define>
      </xsl:for-each>
    </xsl:if>
    <xsl:comment>Set predeclared macros</xsl:comment>
    <xsl:for-each select="key('PredeclareMacrosModule',@ident)">
      <xsl:apply-templates mode="tangle" select="."/>
    </xsl:for-each>
    <xsl:if test="@type='core'">
      <xsl:call-template name="predeclare-classes"/>
    </xsl:if>
    <xsl:comment>0. predeclared macros</xsl:comment>
    <xsl:for-each select="key('PredeclareMacrosModule',@ident)">
      <xsl:apply-templates mode="tangle" select="."/>
    </xsl:for-each>
    <xsl:comment>1. classes</xsl:comment>
    <xsl:for-each select="key('ClassModule',@ident)">
      <xsl:choose>
        <xsl:when test="@module='core' and @predeclare='true'"> </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="tangle" select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:comment>2. elements</xsl:comment>
    <xsl:apply-templates mode="tangle" select="key('ElementModule',@ident)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
    <xsl:comment>3. macros</xsl:comment>
    <xsl:for-each select="key('MacroModule',@ident)">
      <xsl:choose>
        <xsl:when test="@predeclare='true'"/>
        <!--	<xsl:when test="key('PredeclareMacros',@ident)"/>-->
        <xsl:otherwise>
          <xsl:apply-templates mode="tangle" select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="NameList">
    <!-- walk over all the elementSpec elements and make list of 
       elements -->
    <xsl:for-each select="//tei:elementSpec">
      <xsl:sort select="@ident"/>
      <rng:define combine="choice" name="{@ident}"><rng:notAllowed/></rng:define>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="predeclare-classes">
    <xsl:comment>0. predeclared classes</xsl:comment>
    <xsl:for-each select="key('predeclaredClasses',1)">
      <xsl:choose>
        <xsl:when test="@type='model'">
          <xsl:apply-templates mode="processModel" select=".">
            <xsl:with-param name="declare">true</xsl:with-param>
          </xsl:apply-templates>
        </xsl:when>
        <xsl:when test="@type='atts'">
          <xsl:apply-templates mode="processDefaultAtts" select="."/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="tei:specGrpRef" mode="tangle">
    <xsl:param name="filename"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> spec grp ref to <xsl:value-of select="@target"
      /></xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
        <xsl:for-each select="key('IDS',substring-after(@target,'#'))">
          <xsl:apply-templates mode="ok" select=".">
            <xsl:with-param name="filename" select="$filename"/>
          </xsl:apply-templates>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="document(@target)/tei:specGrp">
          <xsl:apply-templates mode="ok" select=".">
            <xsl:with-param name="filename" select="$filename"/>
          </xsl:apply-templates>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="TAG"/>
    <xsl:param name="content"/>
    <xsl:for-each select="exsl:node-set($content)/Wrapper">
      <xsl:copy-of select="*"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="refdoc"/>
  <xsl:template name="typewriter"/>
  <xsl:template name="ttembolden"/>
  <xsl:template match="processing-instruction()"/>
  <xsl:template match="processing-instruction()" mode="tangle"/>
  <xsl:template match="s:*"/>
</xsl:stylesheet>

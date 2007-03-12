<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
    version="1.0"
  extension-element-prefixes="edate exsl estr"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" 
  xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  exclude-result-prefixes="exsl estr edate teix a s tei rng xd">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for simplifying TEI ODD markup </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MAINTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:output encoding="utf-8" indent="yes"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="verbose"/>
  <xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
  <xsl:param name="localsource"/>
  <xsl:key match="tei:*[@xml:id]" name="IDS" use="@xml:id"/>
  <xsl:key match="tei:classSpec[@type='atts' and @mode='add']"
    name="NEWATTCLASSES" use="@ident"/>
  <xsl:key
    match="tei:classSpec[(tei:attList or @type='atts') and not(@ident='tei.TEIform')]"
    name="ATTCLASSES" use="@ident"/>
  <xsl:key match="tei:attDef[@mode='delete']" name="DELETEATT"
    use="concat(../../@ident,'_',@ident)"/>
  <xsl:key match="tei:attDef[@mode='replace']" name="REPLACEATT"
    use="concat(../../@ident,'_',@ident)"/>
  <xsl:key match="tei:attDef[@mode='change']" name="CHANGEATT"
    use="concat(../../@ident,'_',@ident)"/>
  <xsl:key match="tei:elementSpec[@mode='delete']" name="DELETE" use="@ident"/>
  <xsl:key match="tei:elementSpec[@mode='replace']" name="REPLACE" use="@ident"/>
  <xsl:key match="tei:elementSpec[@mode='change']" name="CHANGE" use="@ident"/>
  <xsl:key match="tei:classSpec[@mode='delete']" name="DELETE" use="@ident"/>
  <xsl:key match="tei:classSpec[@mode='replace']" name="REPLACE" use="@ident"/>
  <xsl:key match="tei:classSpec[@mode='change']" name="CHANGE" use="@ident"/>
  <xsl:key match="tei:macroSpec[@mode='delete']" name="DELETE" use="@ident"/>
  <xsl:key match="tei:macroSpec[@mode='replace']" name="REPLACE" use="@ident"/>
  <xsl:key match="tei:macroSpec[@mode='change']" name="CHANGE" use="@ident"/>
  <xsl:key match="tei:moduleRef" name="MODULES" use="@key"/>
  <xsl:key match="tei:attRef" name="ATTREFS"
    use="concat(@name,'_',../../@ident)"/>
  <xsl:variable name="ODD" select="/"/>

  <xsl:variable name="AnonymousModule">
    <xsl:text>module-from-</xsl:text>
    <xsl:value-of select="//tei:schemaSpec/@ident"/>
  </xsl:variable>
  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:if test="$verbose='true'">
        <xsl:message>Schema <xsl:value-of select="@ident"/></xsl:message>
      </xsl:if>
      <!-- 
it is important to process "tei" and "core" first 
because of the order of declarations
-->
      <xsl:for-each select="tei:moduleRef[@key='tei']">
        <xsl:call-template name="phase1"/>
      </xsl:for-each>
      <xsl:for-each select="tei:moduleRef[@key='core']">
        <xsl:call-template name="phase1"/>
      </xsl:for-each>
      <xsl:for-each select="tei:moduleRef[@key]">
        <xsl:if test="not(@key='core' or @key='tei')">
          <xsl:call-template name="phase1"/>
        </xsl:if>
      </xsl:for-each>
      <xsl:copy-of select="tei:moduleRef[@url]"/>
      <xsl:call-template name="phase2"/>
      <xsl:copy-of select="s:*"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="phase2">
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 2: add elementSpec, classSpec, macroSpec</xsl:message>
    </xsl:if>
    <xsl:for-each select="tei:classSpec[@mode='add' or not(@mode)]">
      <xsl:call-template name="createCopy"/>
    </xsl:for-each>
    <xsl:for-each select="tei:macroSpec[@mode='add' or not(@mode)]">
      <xsl:call-template name="createCopy"/>
    </xsl:for-each>
    <xsl:for-each select="tei:elementSpec[@mode='add' or not(@mode)]">
      <xsl:apply-templates mode="copy" select="."/>
    </xsl:for-each>
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 2: expand specGrpRef</xsl:message>
    </xsl:if>
    <xsl:for-each select="tei:specGrpRef">
      <xsl:choose>
        <xsl:when test="starts-with(@target,'#')">
          <xsl:for-each select="key('IDS',substring-after(@target,'#'))">
            <xsl:for-each select="tei:classSpec[not(@mode) or @mode='add']">
              <xsl:call-template name="createCopy"/>
            </xsl:for-each>
            <xsl:apply-templates mode="copy"
              select="tei:elementSpec[not(@mode) or @mode='add']"/>
            <xsl:for-each select="tei:macroSpec[not(@mode) or @mode='add']">
              <xsl:call-template name="createCopy"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="document(@target)/tei:specGrp">
            <xsl:for-each select="tei:classSpec[not(@mode) or @mode='add']">
              <xsl:call-template name="createCopy"/>
            </xsl:for-each>
            <xsl:apply-templates mode="copy"
              select="tei:elementSpec[not(@mode) or @mode='add']"/>
            <xsl:for-each select="tei:macroSpec[not(@mode) or @mode='add']">
              <xsl:call-template name="createCopy"/>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="phase1">
    <!--for every module:
        for every object
         - if its in DELETE list, ignore
         - if its in REPLACE list, use that
         - if its in CHANGE list
           (do the hard merge bit)
         - otherwise copy 
        done
  -->
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 1: expand moduleRef <xsl:value-of select="@key"
      /></xsl:message>
    </xsl:if>
    <xsl:variable name="moduleName" select="@key"/>
    <xsl:variable name="KD" select="concat(@key,'-decl')"/>
    <xsl:choose>
      <xsl:when test="$TEIC='false'"/>
      <xsl:when test="not($localsource='')">
        <xsl:variable name="Local">
          <List>
            <xsl:for-each select="document($localsource)/tei:TEI">
              <xsl:for-each select="tei:*[@module=$moduleName]">
                <xsl:element name="{local-name()}"
                  xmlns="http://www.tei-c.org/ns/1.0">
                  <xsl:copy-of select="@*|*"/>
                </xsl:element>
              </xsl:for-each>
              <xsl:for-each select="tei:*[@module=$KD]">
                <xsl:element name="{local-name()}"
                  xmlns="http://www.tei-c.org/ns/1.0">
                  <xsl:copy-of select="@*|*"/>
                </xsl:element>
              </xsl:for-each>
            </xsl:for-each>
          </List>
        </xsl:variable>
        <xsl:for-each select="exsl:node-set($Local)/List">
          <xsl:call-template name="phase1a"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="Remote">
          <xsl:value-of select="$TEISERVER"/>
          <xsl:text>allbymod.xq?module=</xsl:text>
          <xsl:value-of select="$moduleName"/>
        </xsl:variable>
        <xsl:for-each select="document($Remote)/List">
          <xsl:call-template name="phase1a"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="phase1a">
    <xsl:for-each select="*">
      <xsl:variable name="Current" select="."/>
      <xsl:variable name="elementName" select="@ident"/>
      <xsl:variable name="N" select="local-name(.)"/>
      <xsl:for-each select="$ODD">
        <xsl:choose>
          <xsl:when test="key('DELETE',$elementName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: remove <xsl:value-of select="$elementName"
                /></xsl:message>
            </xsl:if>
            <!--
	      <xsl:element name="{$N}" xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:attribute name="ident"><xsl:value-of select="$elementName"/></xsl:attribute>
	      <xsl:attribute name="mode">delete</xsl:attribute>
	      </xsl:element>
	  -->
          </xsl:when>
          <xsl:when test="key('REPLACE',$elementName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: replace <xsl:value-of select="$elementName"
                /></xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="copy"
              select="key('REPLACE',$elementName)"/>
          </xsl:when>
          <xsl:when test="key('CHANGE',$elementName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: change <xsl:value-of select="$elementName"
                /></xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="change" select="$Current"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="copy" select="$Current"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="@*|processing-instruction()|comment()|text()"
    mode="change">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="*" mode="change">
    <xsl:copy>
      <xsl:apply-templates mode="change"
        select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|processing-instruction()|comment()|text()" mode="copy">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="tei:memberOf" mode="copy">
    <xsl:variable name="k" select="@key"/>
    <xsl:for-each select="$ODD">
      <xsl:choose>
	<xsl:when test="key('DELETE',$k)"/>
	<xsl:otherwise>
	  <tei:memberOf key="{$k}"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="*" mode="copy">
    <xsl:copy>
      <xsl:apply-templates mode="copy"
        select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>

  </xsl:template>

  <xsl:template match="tei:listRef" mode="copy"/>
  <xsl:template match="tei:elementSpec/@mode" mode="copy"/>
  <xsl:template match="tei:macroSpec/@mode" mode="copy"/>
  <xsl:template match="tei:classSpec/@mode" mode="copy"/>
  <xsl:template match="tei:elementSpec/@mode" mode="change"/>
  <xsl:template match="tei:elementSpec" mode="copy">
    <xsl:variable name="elementName">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="orig" select="."/>
    <xsl:copy>
      <xsl:if test="not(@module)">
        <xsl:attribute name="module">
          <xsl:value-of select="$AnonymousModule"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates mode="copy" select="@*"/>
      <xsl:copy-of select="tei:altIdent"/>
      <xsl:copy-of select="tei:equiv"/>
      <xsl:copy-of select="tei:gloss"/>
      <xsl:copy-of select="tei:desc"/>
      <xsl:copy-of select="tei:classes"/>
      <xsl:apply-templates mode="copy" select="tei:content"/>
      <attList xmlns="http://www.tei-c.org/ns/1.0">
        <xsl:if test="not(@ns) or @ns='http://www.tei-c.org/ns/1.0'">
          <xsl:call-template name="classAttributes">
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="'att.global'"/>
          </xsl:call-template>
        </xsl:if>
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:call-template name="classAttributes">
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="@key"/>
          </xsl:call-template>
        </xsl:for-each>
        <xsl:choose>
          <xsl:when test="tei:attList">
            <xsl:for-each select="tei:attList">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:copy-of select="tei:attDef[@mode='add' or not(@mode)]"/>
                <xsl:copy-of select="tei:attRef"/>
                <xsl:copy-of select="tei:attList"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of
              select="tei:attList/tei:attDef[@mode='add' or not(@mode)]"/>
            <xsl:copy-of select="tei:attList/tei:attRef"/>
            <xsl:copy-of select="tei:attList/tei:attList"/>
          </xsl:otherwise>
        </xsl:choose>
      </attList>
      <xsl:copy-of select="tei:exemplum"/>
      <xsl:copy-of select="tei:remarks"/>
      <xsl:copy-of select="tei:listRef"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:elementSpec" mode="change">
    <xsl:variable name="elementName">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- 
For each element, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. Only the attributes are identifiable
for change individually.
 -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('CHANGE',$elementName)">
          <!-- if there is an altIdent, use it -->
          <xsl:copy-of select="tei:altIdent"/>
          <!-- equiv, gloss, desc trio -->
          <xsl:choose>
            <xsl:when test="tei:equiv">
              <xsl:copy-of select="tei:equiv"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:equiv"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:gloss">
              <xsl:copy-of select="tei:gloss"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:gloss"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:desc">
              <xsl:copy-of select="tei:desc"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:desc"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- classes -->
          <xsl:choose>
            <xsl:when test="tei:classes">
              <xsl:copy-of select="tei:classes"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
		<xsl:for-each select="tei:classes">
		  <xsl:copy>
		    <xsl:for-each select="tei:memberOf">
		      <xsl:variable name="me">
			<xsl:value-of select="@key"/>
		      </xsl:variable>
		      <xsl:for-each select="$ODD">
			<xsl:if test="not(key('DELETE',@key))">
			  <tei:memberOf key="{$me}"/>
			</xsl:if>
		      </xsl:for-each>
		    </xsl:for-each>
		  </xsl:copy>
		</xsl:for-each>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- element content -->
          <xsl:choose>
            <xsl:when test="tei:content">

              <xsl:apply-templates mode="copy" select="tei:content"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:content"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- attList -->
          <tei:attList>
            <xsl:copy-of select="tei:attList/@org"/>
            <xsl:call-template name="processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="elementName" select="$elementName"/>
            </xsl:call-template>
          </tei:attList>
          <!-- exemplum, remarks and listRef are either replacements or not -->
          <xsl:choose>
            <xsl:when test="tei:exemplum">
              <xsl:copy-of select="tei:exemplum"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:exemplum"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:remarks">
              <xsl:copy-of select="tei:remarks"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:remarks"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:listRef">
              <xsl:copy-of select="tei:listRef"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:listRef"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:macroSpec" mode="change">
    <xsl:variable name="elementName">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- 
For each macro, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. 
 -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('CHANGE',$elementName)">
          <!-- if there is an altIdent, use it -->
          <xsl:copy-of select="tei:altIdent"/>
          <!-- equiv, gloss, desc trio -->
          <xsl:choose>
            <xsl:when test="tei:equiv">
              <xsl:copy-of select="tei:equiv"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:equiv"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:gloss">
              <xsl:copy-of select="tei:gloss"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:gloss"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:desc">
              <xsl:copy-of select="tei:desc"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:desc"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- content -->
          <xsl:choose>
            <xsl:when test="tei:content">
              <xsl:copy-of select="tei:content"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:content"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:stringVal">
              <xsl:copy-of select="tei:stringVal"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:stringVal"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- exemplum, remarks and listRef are either replacements or not -->
          <xsl:choose>
            <xsl:when test="tei:exemplum">
              <xsl:copy-of select="tei:exemplum"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:exemplum"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:remarks">
              <xsl:copy-of select="tei:remarks"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:remarks"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:listRef">
              <xsl:copy-of select="tei:listRef"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:listRef"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:classSpec" mode="change">
    <xsl:variable name="className">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- for each section of the class spec, 
     go through the sections one by one
     and see if they are present in the change mode version -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('CHANGE',$className)">
<!-- context is now a classSpec in change mode in the ODD spec -->
          <!-- description -->
          <xsl:choose>
            <xsl:when test="tei:desc">
              <xsl:copy-of select="tei:desc"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="tei:desc"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>

          <!-- classes -->
          <xsl:choose>
            <xsl:when test="tei:classes">
		<xsl:copy-of select="tei:classes"/>
            </xsl:when>
	    <xsl:otherwise>
	      <xsl:for-each select="$ORIGINAL">
		<tei:classes>
		  <xsl:for-each select="tei:classes/tei:memberOf">
		    <xsl:variable name="this">
		      <xsl:value-of select="@key"/>
		    </xsl:variable>
		    <xsl:for-each select="$ODD">
		      <xsl:choose>
			<xsl:when test="key('DELETE',$this)"/>
			<xsl:otherwise>
			  <tei:memberOf key="{$this}"/>
			</xsl:otherwise>
		      </xsl:choose>
		    </xsl:for-each>
		  </xsl:for-each>
		</tei:classes>
	      </xsl:for-each>
	    </xsl:otherwise>
	  </xsl:choose>
          <!-- attList -->
          <tei:attList>
            <xsl:call-template name="processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="elementName" select="''"/>
            </xsl:call-template>
          </tei:attList>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template
    match="rng:choice|rng:list|rng:group|rng:oneOrMore|rng:optional|rng:zeroOrMore"
    mode="copy">
    <xsl:call-template name="simplifyRelax">
      <xsl:with-param name="element">
        <xsl:value-of select="local-name(.)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="simplifyRelax">
    <!-- 
for each Relax NG content model,
remove reference to any elements which have been
deleted, or to classes which are empty.
This may make the container empty,
so that is only put back in if there is some content
-->
    <xsl:param name="element"/>
    <xsl:variable name="contents">
      <WHAT>
        <xsl:for-each select="rng:*|processing-instruction()">
          <xsl:choose>
            <xsl:when test="self::processing-instruction()">
              <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:when test="local-name(.)='element'">
              <element xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </element>
            </xsl:when>
            <xsl:when test="local-name(.)='name'">
              <name xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </name>
            </xsl:when>
            <xsl:when test="local-name(.)='attribute'">
              <attribute xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </attribute>
            </xsl:when>
            <xsl:when test="local-name(.)='data'">
              <data xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </data>
            </xsl:when>
            <xsl:when test="local-name(.)='text'">
              <text xmlns="http://relaxng.org/ns/structure/1.0"/>
            </xsl:when>
            <xsl:when test="local-name(.)='value'">
              <value xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:apply-templates/>
              </value>
            </xsl:when>
            <xsl:when test="local-name(.)='ref'">
              <xsl:variable name="N" select="@name"/>
              <xsl:for-each select="$ODD">
                <xsl:if test="not(key('DELETE',$N))">
                  <ref name="{$N}" xmlns="http://relaxng.org/ns/structure/1.0"/>
                </xsl:if>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="simplifyRelax">
                <xsl:with-param name="element">
                  <xsl:value-of select="local-name(.)"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </WHAT>
    </xsl:variable>
    <xsl:variable name="entCount">
      <xsl:for-each select="exsl:node-set($contents)/WHAT">
        <xsl:value-of select="count(*)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when
        test="$entCount=1 and local-name(exsl:node-set($contents)/WHAT/*)=$element">
        <xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
      </xsl:when>
      <xsl:when
        test="$element='optional' and $entCount=1 and
		      local-name(exsl:node-set($contents)/WHAT/*)='zeroOrMore'">
        <xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
      </xsl:when>
      <xsl:when
        test="$element='optional' and $entCount=1 and
		      local-name(exsl:node-set($contents)/WHAT/*)='oneOrMore'">
        <xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
      </xsl:when>
      <xsl:when
        test="self::rng:zeroOrMore/rng:ref/@name='model.global'
		and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name='model.global']"/>

      <xsl:when test="$entCount&gt;0">
        <xsl:element name="{$element}"
          xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
        </xsl:element>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="classAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <!-- 
    On entry, we are sitting on an <elementSpec> or <classSpec> 
    and seeing if we can pick up some attributes for 
    $elementName. We travel to the ODD first
    to see if it has some overrides
    -->
    <xsl:for-each select="$ODD">
      <xsl:choose>
        <xsl:when test="$TEIC='false'"/>
        <!-- the class is referenced in the ODD and has redefined <classes>-->
        <xsl:when test="key('ATTCLASSES',$className)/tei:classes">
          <xsl:for-each select="key('ATTCLASSES',$className)">
            <xsl:call-template name="processClassAttributes">
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="$className"/>
              <xsl:with-param name="fromOdd">true</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <!-- the class is referenced in the ODD and has redefined <attList>-->
        <xsl:when test="key('ATTCLASSES',$className)/tei:attList">
          <xsl:for-each select="key('ATTCLASSES',$className)">
            <xsl:call-template name="processClassAttributes">
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="$className"/>
              <xsl:with-param name="fromOdd">true</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <!-- otherwise, we'll revert to source
	     (assuming the class is of type 'atts')
	-->
        <xsl:when test="not($localsource='')">
          <xsl:for-each select="document($localsource)/tei:TEI">
            <xsl:for-each select="key('ATTCLASSES',$className)">
              <xsl:call-template name="processClassAttributes">
                <xsl:with-param name="elementName" select="$elementName"/>
                <xsl:with-param name="className" select="$className"/>
		<xsl:with-param name="fromOdd">false</xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="ATTCLASSDOC">
            <xsl:value-of select="$TEISERVER"/>
            <xsl:text>classspecs.xq</xsl:text>
          </xsl:variable>
          <xsl:for-each select="document($ATTCLASSDOC)/List">
            <xsl:for-each select="key('ATTCLASSES',$className)">
              <xsl:call-template name="processClassAttributes">
                <xsl:with-param name="elementName" select="$elementName"/>
                <xsl:with-param name="className" select="$className"/>
		<xsl:with-param name="fromOdd">false</xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="processClassAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="fromOdd"/>
    <!-- we are sitting on a classSpec, could be in the ODD
	 or could be in the source -->
    <xsl:variable name="M" select="@module"/>
    <xsl:variable name="use">
      <xsl:choose>
        <xsl:when test="not(@module)">
          <xsl:text>true</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="$ODD">
            <xsl:choose>
              <xsl:when test="key('DELETE',$className)"/>
              <xsl:when
                test="key('MODULES',$M) or
		      key('MODULES',substring-before($M,'-decl'))">
                <xsl:text>true</xsl:text>
              </xsl:when>
            </xsl:choose>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="$use='true'">
      <!-- 
	   We need to put in the class attributes. We'll 
	   use the presence of @mode to see whether this is in the ODD.
	   
	   a) the class is new in this customization, add all attributes regardless
	   b) the class is marked for deletion. do nothing
	   c) the class is marked for replacement. reference attributes from the replacement
	   d) the class is marked for change. compare attributes (tedious)
	   e) the class has no replacement, but we need to check if its in a
	   module which has been loaded. if so, reference its attributes
	   In each case, once we have a potential attribute, we have to check
	   back to see if it is changed in the element (mergeClassAttribute)
      -->
      <xsl:choose>
        <!-- a) new class in ODD -->
        <xsl:when test="@mode='add'">
          <xsl:for-each select="tei:attList/tei:attDef">
            <tei:attRef name="{$className}.attribute.{translate(@ident,':','')}"
            />
          </xsl:for-each>
        </xsl:when>
        <!-- b) its deleted -->
        <xsl:when test="@mode='delete'"/>
        <!-- c) its a replacement -->
        <xsl:when test="@mode='replace'">
          <xsl:for-each select="tei:attList/tei:attDef">
            <xsl:call-template name="mergeClassAttribute">
              <xsl:with-param name="source">1</xsl:with-param>
              <xsl:with-param name="element" select="$elementName"/>
              <xsl:with-param name="class" select="$className"/>
              <xsl:with-param name="att" select="@ident"/>
	      <xsl:with-param name="fromOdd">
		<xsl:value-of select="$fromOdd"/>
	      </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="tei:attList/tei:attList">
            <tei:attList>
              <xsl:copy-of select="@org"/>
              <xsl:for-each select="tei:attDef">
                <xsl:call-template name="mergeClassAttribute">
                  <xsl:with-param name="source">2</xsl:with-param>
                  <xsl:with-param name="element" select="$elementName"/>
                  <xsl:with-param name="class" select="$className"/>
                  <xsl:with-param name="att" select="@ident"/>
		  <xsl:with-param name="fromOdd">
		    <xsl:value-of select="$fromOdd"/>
		  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </tei:attList>
          </xsl:for-each>
        </xsl:when>
        <!-- d) there are changes to the class spec itself -->
        <xsl:when test="@mode='change'">
          <!-- always references attributes in add mode -->
          <xsl:for-each select="tei:attList/tei:attDef[@mode='add']">
            <tei:attRef name="{$className}.attribute.{translate(@ident,':','')}"
            />
          </xsl:for-each>
          <!-- go back to original and proceed from there -->
          <xsl:choose>
            <xsl:when test="not($localsource='')">
              <xsl:for-each select="document($localsource)/tei:TEI">
                <xsl:for-each select="key('ATTCLASSES',$className)">
                  <xsl:call-template name="tryAttributes">
                    <xsl:with-param name="elementName" select="$elementName"/>
                    <xsl:with-param name="className" select="$className"/>
		    <xsl:with-param name="fromOdd">
		      <xsl:value-of select="$fromOdd"/>
		    </xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="ATTCLASSDOC">
                <xsl:value-of select="$TEISERVER"/>
                <xsl:text>classspecs.xq</xsl:text>
              </xsl:variable>
              <xsl:for-each select="document($ATTCLASSDOC)/List">
                <xsl:for-each select="key('ATTCLASSES',$className)">
                  <xsl:call-template name="tryAttributes">
                    <xsl:with-param name="elementName" select="$elementName"/>
                    <xsl:with-param name="className" select="$className"/>
		    <xsl:with-param name="fromOdd">
		      <xsl:value-of select="$fromOdd"/>
		    </xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:for-each select="tei:attList/tei:attRef">
            <xsl:copy-of select="."/>
          </xsl:for-each>
        </xsl:when>

        <!-- e) its in the source. maybe do some merging -->
        <xsl:otherwise>
          <xsl:for-each select="tei:attList/tei:attDef">
            <xsl:call-template name="mergeClassAttribute">
              <xsl:with-param name="source">7</xsl:with-param>
              <xsl:with-param name="element" select="$elementName"/>
              <xsl:with-param name="class" select="$className"/>
              <xsl:with-param name="att" select="@ident"/>
	      <xsl:with-param name="fromOdd">
		<xsl:value-of select="$fromOdd"/>
	      </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="tei:attList/tei:attList">
            <tei:attList>
              <xsl:copy-of select="@org"/>
              <xsl:for-each select="tei:attDef">
                <xsl:call-template name="mergeClassAttribute">
                  <xsl:with-param name="source">8</xsl:with-param>
                  <xsl:with-param name="element" select="$elementName"/>
                  <xsl:with-param name="class" select="$className"/>
                  <xsl:with-param name="att" select="@ident"/>
		  <xsl:with-param name="fromOdd">
		    <xsl:value-of select="$fromOdd"/>
		  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </tei:attList>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>

      <!-- Now attributes referenced from classes we are a member of
      -->
      <xsl:for-each select="tei:classes/tei:memberOf">
        <xsl:call-template name="classAttributes">
          <xsl:with-param name="elementName" select="$elementName"/>
          <xsl:with-param name="className" select="@key"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xsl:template name="tryAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="fromOdd"/>
    <xsl:for-each select="tei:attList/tei:attDef">
      <xsl:call-template name="mergeClassAttribute">
        <xsl:with-param name="source">3</xsl:with-param>
        <xsl:with-param name="element" select="$elementName"/>
        <xsl:with-param name="class" select="$className"/>
        <xsl:with-param name="att" select="@ident"/>
	<xsl:with-param name="fromOdd">
	  <xsl:value-of select="$fromOdd"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select="tei:attList/tei:attList">
      <tei:attList>
        <xsl:copy-of select="@org"/>
        <xsl:for-each select="tei:attDef">
          <xsl:call-template name="mergeClassAttribute">
            <xsl:with-param name="source">4</xsl:with-param>
            <xsl:with-param name="element" select="$elementName"/>
            <xsl:with-param name="class" select="$className"/>
            <xsl:with-param name="att" select="@ident"/>
	    <xsl:with-param name="fromOdd">
	      <xsl:value-of select="$fromOdd"/>
	    </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </tei:attList>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="mergeClassAttribute">
    <!-- sitting on a source class. go over
every attribute and see whether the attribute has changed-->

    <xsl:param name="source"/>
    <xsl:param name="element"/>
    <xsl:param name="class"/>
    <xsl:param name="att"/>
    <xsl:param name="fromOdd"/>
    <xsl:variable name="orig" select="."/>
    <xsl:variable name="A" select="@ident"/>
    <xsl:variable name="attRef">
      <xsl:value-of
        select="concat($class,'.attribute.' ,translate($att,':',''),'_',$element)"
      />
    </xsl:variable>
    <xsl:variable name="lookingAt">
      <xsl:value-of select="concat($element,'_',$A)"/>
    </xsl:variable>
    <xsl:variable name="wherefrom" select="."/>
    <xsl:for-each select="$ODD">
      <xsl:choose>
        <xsl:when test="key('DELETEATT',concat($class,'_',$att))"/>
        <xsl:when test="key('DELETEATT',$lookingAt)"/>
        <xsl:when test="key('REPLACEATT',$lookingAt)"/>
        <xsl:when test="key('CHANGEATT',$lookingAt)">
          <xsl:call-template name="mergeAttribute">
            <xsl:with-param name="New" select="key('CHANGEATT',$lookingAt)"/>
            <xsl:with-param name="Old" select="$orig"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
	  <xsl:choose>
	    <xsl:when test="fromOdd='false'">
	      <xsl:for-each select="$wherefrom">
		<xsl:call-template name="changedAtt">
		  <xsl:with-param name="lookingAt">
		    <xsl:value-of select="$lookingAt"/>
		  </xsl:with-param>
		  <xsl:with-param name="att">
		    <xsl:value-of select="$att"/>
		  </xsl:with-param>
		  <xsl:with-param name="attRef">
		    <xsl:value-of select="$attRef"/>
		  </xsl:with-param>
		  <xsl:with-param name="class">
		    <xsl:value-of select="$class"/>
		  </xsl:with-param>
		  <xsl:with-param name="orig">
		    <xsl:value-of select="$orig"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:call-template name="changedAtt">
		<xsl:with-param name="lookingAt">
		  <xsl:value-of select="$lookingAt"/>
		</xsl:with-param>
		<xsl:with-param name="att">
		  <xsl:value-of select="$att"/>
		</xsl:with-param>
		  <xsl:with-param name="attRef">
		    <xsl:value-of select="$attRef"/>
		  </xsl:with-param>
		<xsl:with-param name="class">
		  <xsl:value-of select="$class"/>
		</xsl:with-param>
		<xsl:with-param name="orig">
		  <xsl:value-of select="$orig"/>
		  </xsl:with-param>
	      </xsl:call-template>
	    </xsl:otherwise>
	  </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:for-each>
  </xsl:template>

<xsl:template name="changedAtt">
 <xsl:param name="lookingAt"/>
 <xsl:param name="att"/>
 <xsl:param name="class"/>
 <xsl:param name="orig"/>
 <xsl:param name="attRef"/>
 <xsl:choose>
    <!-- don't make another reference to a class attribute 
	 if we already have an attRef -->
    <xsl:when test="key('ATTREFS',$attRef)"/>
    <xsl:when test="key('DELETEATT',$lookingAt)"/>
    <xsl:when test="key('REPLACEATT',$lookingAt)">
      <xsl:comment>element replacement of class attribute named
      <xsl:value-of select="$att"/></xsl:comment>
      <xsl:for-each select="key('REPLACEATT',$lookingAt)">
	<tei:attDef ident="{$att}">
	  <xsl:copy-of select="@ns"/>
	  <xsl:copy-of select="@usage"/>
	  <xsl:copy-of select="tei:*"/>
	</tei:attDef>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="key('CHANGEATT',$lookingAt)">
      <xsl:comment>element override of class attribute named
      <xsl:value-of select="$att"/></xsl:comment>
      <xsl:call-template name="mergeAttribute">
	<xsl:with-param name="New"
			select="key('CHANGEATT',$lookingAt)"/>
	<xsl:with-param name="Old" select="$orig"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <tei:attRef name="{$class}.attribute.{translate($att,':','')}"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

  <xsl:template name="processAttributes">
    <xsl:param name="ORIGINAL"/>
    <xsl:param name="elementName"/>
    <!-- first put in the ones we know take precedence -->
    <xsl:copy-of select="tei:attList/tei:attDef[@mode='add' or not(@mode)]"/>
    <xsl:copy-of select="tei:attList/tei:attDef[@mode='replace']"/>
    <xsl:for-each select="$ORIGINAL/tei:attList">
      <!-- original source  context -->
      <xsl:for-each select="tei:attList">
        <tei:attList>
          <xsl:copy-of select="@org"/>
          <xsl:for-each select="tei:attDef">
            <xsl:variable name="ATT" select="."/>
            <xsl:variable name="lookingAt">
              <xsl:value-of select="concat(../../../@ident,'_',@ident)"/>
            </xsl:variable>
            <xsl:for-each select="$ODD">
              <xsl:choose>
                <xsl:when test="key('DELETEATT',$lookingAt)"/>
                <xsl:when test="key('REPLACEATT',$lookingAt)"/>
                <xsl:when test="key('CHANGEATT',$lookingAt)">
                  <xsl:call-template name="mergeAttribute">
                    <xsl:with-param name="New"
                      select="key('CHANGEATT',$lookingAt)"/>
                    <xsl:with-param name="Old" select="$ATT"/>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:copy-of select="$ATT"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
          </xsl:for-each>
        </tei:attList>
      </xsl:for-each>
      <xsl:for-each select="tei:attDef">
        <xsl:variable name="ATT" select="."/>
        <xsl:variable name="lookingAt">
          <xsl:value-of select="concat(../../@ident,'_',@ident)"/>
        </xsl:variable>
        <xsl:for-each select="$ODD">
          <xsl:choose>
            <xsl:when test="key('DELETEATT',$lookingAt)"/>
            <xsl:when test="key('REPLACEATT',$lookingAt)"/>
            <xsl:when test="key('CHANGEATT',$lookingAt)">
              <xsl:call-template name="mergeAttribute">
                <xsl:with-param name="New" select="key('CHANGEATT',$lookingAt)"/>
                <xsl:with-param name="Old" select="$ATT"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$ATT"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
    <!-- now we need to go back to the classes of which this 
       element is a member and reference their untouched attributes -->
    <xsl:choose>
      <xsl:when test="$elementName=''"/>
      <xsl:when test="tei:classes">
        <xsl:call-template name="classAttributes">
          <xsl:with-param name="elementName" select="$elementName"/>
          <xsl:with-param name="className" select="'att.global'"/>
        </xsl:call-template>
        <xsl:call-template name="inheritedClassAttributes">
          <xsl:with-param name="elementName" select="$elementName"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$ORIGINAL">
          <xsl:call-template name="classAttributes">
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="'att.global'"/>
          </xsl:call-template>
          <xsl:if test="tei:classes">
            <xsl:call-template name="inheritedClassAttributes">
              <xsl:with-param name="elementName" select="$elementName"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="inheritedClassAttributes">
    <xsl:param name="elementName"/>
    <xsl:variable name="orig" select="."/>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:call-template name="classAttributes">
        <xsl:with-param name="elementName" select="$elementName"/>
        <xsl:with-param name="className" select="@key"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="mergeAttribute">
    <xsl:param name="New"/>
    <xsl:param name="Old"/>
    <tei:attDef ident="{$Old/@ident}">
      <xsl:for-each select="$New">
        <xsl:attribute name="usage">
          <xsl:choose>
            <xsl:when test="@usage">
              <xsl:value-of select="@usage"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$Old/@usage"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <xsl:if test="tei:altIdent">
          <xsl:copy-of select="tei:altIdent"/>
        </xsl:if>
        <!-- equiv, gloss, desc trio -->
        <xsl:choose>
          <xsl:when test="tei:equiv">
            <xsl:copy-of select="tei:equiv"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:copy-of select="tei:equiv"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:gloss">
            <xsl:copy-of select="tei:gloss"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:copy-of select="tei:gloss"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:desc">
            <xsl:copy-of select="tei:desc"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:copy-of select="tei:desc"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:datatype">
            <xsl:copy-of select="tei:datatype"/>
          </xsl:when>
          <xsl:when test="$Old/tei:datatype">
            <xsl:copy-of select="$Old/tei:datatype"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:defaultVal">
            <xsl:copy-of select="tei:defaultVal"/>
          </xsl:when>
          <xsl:when test="$Old/tei:defaultVal">
            <xsl:copy-of select="$Old/tei:defaultVal"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:valDesc">
            <xsl:copy-of select="tei:valDesc"/>
          </xsl:when>
          <xsl:when test="$Old/tei:valDesc">
            <xsl:copy-of select="$Old/tei:valDesc"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:valList[@mode='add' or @mode='replace']">
            <xsl:for-each select="tei:valList">
              <xsl:copy>
                <xsl:copy-of select="@type"/>
                <xsl:copy-of select="@repeatable"/>
                <xsl:copy-of select="*"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="tei:valList[@mode='change']">
            <xsl:for-each select="tei:valList">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:for-each select="$Old/tei:valList/tei:valItem">
                  <xsl:variable name="thisme" select="@ident"/>
                  <xsl:if
                    test="not($New/tei:valList/tei:valItem[@ident=$thisme and (@mode='delete' or @mode='replace')])">
                    <xsl:copy>
                      <xsl:copy-of select="@*"/>
                      <xsl:for-each
                        select="$New/tei:valList/tei:valItem[@ident=$thisme]">
                        <xsl:choose>
                          <xsl:when test="tei:equiv">
                            <xsl:copy-of select="tei:equiv"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:for-each
                              select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                              <xsl:copy-of select="tei:equiv"/>
                            </xsl:for-each>
                          </xsl:otherwise>
                        </xsl:choose>
                        <xsl:choose>
                          <xsl:when test="tei:gloss">
                            <xsl:copy-of select="tei:gloss"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:for-each
                              select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                              <xsl:copy-of select="tei:gloss"/>
                            </xsl:for-each>
                          </xsl:otherwise>
                        </xsl:choose>
                        <xsl:choose>
                          <xsl:when test="tei:desc">
                            <xsl:copy-of select="tei:desc"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:for-each
                              select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                              <xsl:copy-of select="tei:desc"/>
                            </xsl:for-each>
                          </xsl:otherwise>
                        </xsl:choose>
                      </xsl:for-each>
                    </xsl:copy>
                  </xsl:if>
                </xsl:for-each>
                <xsl:copy-of select="tei:valItem[@mode='add']"/>
                <xsl:copy-of select="tei:valItem[@mode='replace']"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="$Old/tei:valList">
            <xsl:copy-of select="$Old/tei:valList"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:exemplum">
            <xsl:copy-of select="tei:exemplum"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:copy-of select="tei:exemplum"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="tei:remarks">
            <xsl:copy-of select="tei:remarks"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:copy-of select="tei:remarks"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </tei:attDef>
  </xsl:template>
  <xsl:template match="tei:specGrp">
    <xsl:choose>
      <xsl:when test="ancestor::tei:schemaSpec">
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:specGrpRef"/>

  <xsl:template match="tei:macroSpec|tei:classSpec">
    <xsl:if test="not(ancestor::tei:schemaSpec)">
      <xsl:copy-of select="."/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:attDef[@mode]"/>

  <xsl:template match="tei:elementSpec">
    <xsl:if test="not(//tei:schemaSpec)">
      <xsl:variable name="elementName">
        <xsl:value-of select="@ident"/>
      </xsl:variable>
      <xsl:copy>
        <xsl:apply-templates mode="copy" select="@*"/>
        <xsl:copy-of select="tei:altIdent"/>
        <xsl:copy-of select="tei:equiv"/>
        <xsl:copy-of select="tei:gloss"/>
        <xsl:copy-of select="tei:desc"/>
        <xsl:copy-of select="tei:classes"/>
        <xsl:apply-templates mode="copy" select="tei:content"/>
        <tei:attList>
          <xsl:comment>1.</xsl:comment>
          <xsl:call-template name="classAttributesSimple">
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="'att.global'"/>
          </xsl:call-template>
          <xsl:comment>2.</xsl:comment>
          <xsl:for-each select="tei:classes/tei:memberOf">
            <xsl:comment>3: <xsl:value-of select="@key"/></xsl:comment>
            <xsl:call-template name="classAttributesSimple">
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="@key"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:comment>4.</xsl:comment>
          <xsl:apply-templates select="tei:attList"/>
          <xsl:comment>5.</xsl:comment>
        </tei:attList>
        <xsl:copy-of select="tei:exemplum"/>
        <xsl:copy-of select="tei:remarks"/>
        <xsl:copy-of select="tei:listRef"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@url]">
    <p>Include external module <xsl:value-of select="@url"/>.</p>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@key]">
    <p>Internal module <xsl:value-of select="@key"/> was located and
    expanded.</p>
  </xsl:template>
  <xsl:template match="@*|processing-instruction()|comment()|text()">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates
        select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="classAttributesSimple">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:comment>START on <xsl:value-of select="$className"/></xsl:comment>
    <xsl:for-each select="key('ATTCLASSES',$className)">
      <xsl:variable name="CURRENTCLASS" select="."/>
      <xsl:for-each select="tei:attList/tei:attDef">
        <xsl:call-template name="mergeClassAttribute">
          <xsl:with-param name="source">9</xsl:with-param>
          <xsl:with-param name="element" select="$elementName"/>
          <xsl:with-param name="class" select="$className"/>
          <xsl:with-param name="att" select="@ident"/>
          <xsl:with-param name="original" select="$CURRENTCLASS"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:if test="tei:classes/tei:memberOf">
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:variable name="cName" select="@key"/>
          <xsl:call-template name="classAttributesSimple">
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="$cName"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:if>
    </xsl:for-each>
    <xsl:comment>FINISH <xsl:value-of select="$className"/></xsl:comment>
  </xsl:template>

  <xsl:template name="createCopy">
    <xsl:element name="{local-name()}" xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:if test="not(@module)">
        <xsl:attribute name="module">
          <xsl:value-of select="$AnonymousModule"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if
        test="local-name()='classSpec' and @type='model' and not(@predeclare)">
        <xsl:attribute name="predeclare">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates mode="copy" select="@*|*"/>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>

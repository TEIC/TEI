<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:edate="http://exslt.org/dates-and-times"
    xmlns:exsl="http://exslt.org/common"
    xmlns:estr="http://exslt.org/strings"
    exclude-result-prefixes="exsl estr edate teix fo a tei xs rng s xd" 
    extension-element-prefixes="edate exsl estr"
    version="1.0">

<xd:doc type="stylesheet">
    <xd:short>
      TEI stylesheet for simplifying TEI ODD markup
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
  <xsl:output encoding="utf-8" indent="yes"/>
  <xsl:param name="verbose"></xsl:param>
  <xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
  <xsl:param name="localsource"/>
  
  <xsl:key name="IDS" match="tei:*[@xml:id]"  use="@xml:id"/>
  <xsl:key name="IDENTS"  match="tei:classSpec[(@type='atts' or @type='both') and not(@ident='tei.TEIform')]"   use="@ident"/>

  <xsl:key name="DELETEATT" match="tei:attDef[@mode='delete']"
	   use="concat(../../@ident,'_',@ident)"/>
  <xsl:key name="REPLACEATT" match="tei:attDef[@mode='replace']" 
	   use="concat(../../@ident,'_',@ident)"/>
  <xsl:key name="CHANGEATT" match="tei:attDef[@mode='change']" 
	   use="concat(../../@ident,'_',@ident)"/>

  <xsl:key name="DELETE" match="tei:elementSpec[@mode='delete']"
	   use="@ident"/>
  <xsl:key name="REPLACE" match="tei:elementSpec[@mode='replace']"
	   use="@ident"/>
  <xsl:key name="CHANGE" match="tei:elementSpec[@mode='change']" use="@ident"/>

  <xsl:key name="DELETE" match="tei:classSpec[@mode='delete']"
	   use="@ident"/>
  <xsl:key name="REPLACE" match="tei:classSpec[@mode='replace']"
	   use="@ident"/>
  <xsl:key name="CHANGE" match="tei:classSpec[@mode='change']" use="@ident"/>

  <xsl:key name="DELETE" match="tei:macroSpec[@mode='delete']"
	   use="@ident"/>
  <xsl:key name="REPLACE" match="tei:macroSpec[@mode='replace']"
	   use="@ident"/>
  <xsl:key name="CHANGE" match="tei:macroSpec[@mode='change']" use="@ident"/>

  <xsl:key name="MODULES" match="tei:moduleRef" use="@key"/>


  <xsl:variable name="ODD" select="/"/>

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
      </xsl:copy>
  </xsl:template>  
  
  
  <xsl:template name="phase2">
  <xsl:if test="$verbose='true'">
    <xsl:message>Phase 2: add elementSpec, classSpec,  macroSpec</xsl:message>
  </xsl:if>
  <xsl:for-each select="tei:classSpec[@mode='add']">
    <xsl:copy-of select="."/>
  </xsl:for-each>
  <xsl:for-each select="tei:classSpec[not(@mode)]">
    <xsl:copy-of select="."/>
  </xsl:for-each>
  <xsl:for-each select="tei:macroSpec[@mode='add']">
    <xsl:copy-of select="."/>
  </xsl:for-each>
  <xsl:for-each select="tei:macroSpec[not(@mode)]">
    <xsl:copy-of select="."/>
  </xsl:for-each>
  <xsl:for-each select="tei:elementSpec[@mode='add']">
    <xsl:apply-templates select="." mode="copy"/>
  </xsl:for-each>
  <xsl:for-each select="tei:elementSpec[not(@mode)]">
    <xsl:apply-templates select="." mode="copy"/>
  </xsl:for-each>
  <xsl:if test="$verbose='true'">
    <xsl:message>Phase 2: expand specGrpRef</xsl:message>
  </xsl:if>
  <xsl:for-each select="tei:specGrpRef">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
	<xsl:for-each select="key('IDS',substring-after(@target,'#'))">
	  <xsl:apply-templates select="tei:*[not(@mode) or @mode='add']" mode="copy"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="document(@target)/tei:specGrp">
	  <xsl:apply-templates select="tei:elementSpec[not(@mode) or @mode='add']" mode="copy"/>
	  <xsl:apply-templates select="tei:classSpec[not(@mode) or @mode='add']" mode="copy"/>
	  <xsl:apply-templates select="tei:macroSpec[not(@mode) or @mode='add']" mode="copy"/>
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
  <xsl:message>Phase 1: expand moduleRef <xsl:value-of
  select="@key"/></xsl:message>
</xsl:if>
<xsl:variable name="K" select="@key"/>
<xsl:variable name="KD" select="concat(@key,'-decl')"/>

  <xsl:choose>
    <xsl:when test="$TEIC='false'"/>
    <xsl:when test="not($localsource='')">
      <xsl:variable name="Local">
	<List>
	  <xsl:for-each select="document($localsource)/tei:TEI">
	    <xsl:for-each select="tei:*[@module=$K]">
	      <xsl:element name="{local-name()}" xmlns="http://www.tei-c.org/ns/1.0">
		<xsl:copy-of select="@*|*"/>
	      </xsl:element>
	    </xsl:for-each>
	    <xsl:for-each select="tei:*[@module=$KD]">
	      <xsl:element name="{local-name()}" xmlns="http://www.tei-c.org/ns/1.0">
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
	<xsl:value-of select="$K"/>
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
    <xsl:variable name="I" select="@ident"/>
    <xsl:variable name="N" select="local-name(.)"/>
    <xsl:for-each select="$ODD">
      <xsl:choose>
	<xsl:when test="key('DELETE',$I)">
	  <xsl:if test="$verbose='true'">
	    <xsl:message>  Phase 3: remove <xsl:value-of
	    select="$I"/></xsl:message>
	  </xsl:if>
	  <!--
	      <xsl:element name="{$N}" xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:attribute name="ident"><xsl:value-of select="$I"/></xsl:attribute>
	      <xsl:attribute name="mode">delete</xsl:attribute>
	      </xsl:element>
	  -->
	</xsl:when>
	<xsl:when test="key('REPLACE',$I)">
	  <xsl:if test="$verbose='true'">
	      <xsl:message>  Phase 3: replace <xsl:value-of
	      select="$I"/></xsl:message>
	    </xsl:if>
	    <xsl:apply-templates select="key('REPLACE',$I)" mode="copy"/>
	  </xsl:when>
	  <xsl:when test="key('CHANGE',$I)">
	    <xsl:if test="$verbose='true'">
	      <xsl:message>  Phase 3: change <xsl:value-of
	      select="$I"/></xsl:message>
	    </xsl:if>
	    <xsl:apply-templates select="$Current" mode="change"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates select="$Current" mode="copy"/>
	  </xsl:otherwise>
	  </xsl:choose>
      </xsl:for-each>
  </xsl:for-each>
</xsl:template>

<xsl:template match="@*|processing-instruction()|comment()|text()" mode="change">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*" mode="change">
  <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()" mode="change"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|processing-instruction()|comment()|text()" mode="copy">
  <xsl:copy/>
</xsl:template>

<xsl:template match="*" mode="copy">
  <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()" mode="copy"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:listRef" mode="copy"/>

<xsl:template match="tei:elementSpec/@mode" mode="copy"/>

<xsl:template match="tei:elementSpec/@mode" mode="change"/>

<xsl:template match="tei:elementSpec" mode="copy">
  <xsl:variable name="I">
    <xsl:value-of select="@ident"/>
  </xsl:variable>
  <xsl:copy>
    <xsl:apply-templates select="@*" mode="copy"/>
    <xsl:copy-of select="tei:altIdent"/>
    <xsl:copy-of select="tei:equiv"/>
    <xsl:copy-of select="tei:gloss"/>
    <xsl:copy-of select="tei:desc"/>
    <xsl:copy-of select="tei:classes"/>
    <xsl:apply-templates select="tei:content" mode="copy"/>
      <attList xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:call-template name="classAttributes">
	<xsl:with-param name="I" select="@ident"/>
	<xsl:with-param name="K" select="'att.global'"/>
	</xsl:call-template>
	<xsl:for-each select="tei:classes/tei:memberOf"> 
	  <xsl:variable name="K" select="@key"/>
	  <xsl:call-template name="classAttributes">
	    <xsl:with-param name="I" select="$I"/>
	    <xsl:with-param name="K" select="@key"/>
	  </xsl:call-template>
      </xsl:for-each>
      <xsl:copy-of select="tei:attList/tei:attDef[not(@mode)]"/>
      <xsl:copy-of select="tei:attList/tei:attDef[@mode='add']"/>
      <xsl:copy-of select="tei:attList/tei:attRef"/>
      <xsl:copy-of select="tei:attList/tei:attList"/>
      </attList>

    <xsl:copy-of select="tei:exemplum"/>
    <xsl:copy-of select="tei:remarks"/>
    <xsl:copy-of select="tei:listRef"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:elementSpec" mode="change">
  <xsl:variable name="I">
    <xsl:value-of select="@ident"/>
  </xsl:variable>

  <xsl:variable name="ORIGINAL" select="."/>

  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
<!-- 
For each element, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. Only the attributes are identifiable
for change individually.
 -->
    <xsl:for-each select="$ODD">
      <xsl:for-each select="key('CHANGE',$I)">

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
	      <xsl:copy-of select="tei:classes"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>

<!-- element content -->

	<xsl:choose>
	  <xsl:when test="tei:content">
	    <xsl:apply-templates select="tei:content" mode="copy"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="$ORIGINAL">
	      <xsl:apply-templates select="tei:content" mode="copy"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>

<!-- attList -->
        <tei:attList>
	  <xsl:call-template name="processAttributes">
	    <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
	    <xsl:with-param name="I" select="$I"/>
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
  <xsl:variable name="I">
    <xsl:value-of select="@ident"/>
  </xsl:variable>

  <xsl:variable name="ORIGINAL" select="."/>

  <xsl:copy>
    <xsl:apply-templates select="@*" mode="change"/>
<!-- 
For each macro, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. 
 -->
    <xsl:for-each select="$ODD">
      <xsl:for-each select="key('CHANGE',$I)">

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
	      <xsl:apply-templates select="tei:content" mode="copy"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>

	<xsl:choose>
	  <xsl:when test="tei:stringVal">
	    <xsl:copy-of select="tei:stringVal"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="$ORIGINAL">
	      <xsl:apply-templates select="tei:stringVal" mode="copy"/>
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
  <xsl:variable name="I">
    <xsl:value-of select="@ident"/>
  </xsl:variable>
  <xsl:variable name="ORIGINAL" select="."/>
  <xsl:copy>
    <xsl:apply-templates
	select="@*" mode="change"/>
<!-- for each class, go through the sections one by one
and see if they are present in the change mode version -->
    <xsl:for-each select="$ODD">
      <xsl:for-each select="key('CHANGE',$I)">
<!-- classes -->
	<xsl:choose>
	  <xsl:when test="tei:classes">
	    <xsl:copy-of select="tei:classes"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:for-each select="$ORIGINAL">
	      <xsl:copy-of select="tei:classes"/>
	    </xsl:for-each>
	  </xsl:otherwise>
	</xsl:choose>
<!-- attList -->
        <tei:attList>
	  <xsl:call-template name="processAttributes">
	    <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
	    <xsl:with-param name="I" select="$I"/>
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
		  <ref name="{$N}"
		       xmlns="http://relaxng.org/ns/structure/1.0"/>
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
    <xsl:when test="$entCount=1 and
		    local-name(exsl:node-set($contents)/WHAT/*)=$element">
	<xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
    </xsl:when>
    <xsl:when test="$entCount&gt;0">
      <xsl:element name="{$element}" xmlns="http://relaxng.org/ns/structure/1.0">
	<xsl:copy-of select="exsl:node-set($contents)/WHAT/node()"/>
      </xsl:element>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template name="classAttributes">
  <xsl:param name="I"/>
  <xsl:param name="K"/>
  <xsl:choose>
    <xsl:when test="$TEIC='false'"/>
    <xsl:when test="not($localsource='')">
      <xsl:for-each select="document($localsource)/tei:TEI">
	<xsl:call-template name="classAttributesA">
	  <xsl:with-param name="I" select="$I"/>
	  <xsl:with-param name="K" select="$K"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="ATTCLASSDOC">
	<xsl:value-of select="$TEISERVER"/>
	<xsl:text>classspecs.xq</xsl:text>
      </xsl:variable>
      <xsl:for-each select="document($ATTCLASSDOC)/List">
	<xsl:call-template name="classAttributesA">
	  <xsl:with-param name="I" select="$I"/>
	  <xsl:with-param name="K" select="$K"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="classAttributesA">
  <xsl:param name="I"/>
  <xsl:param name="K"/>
    <!-- 
	 We need to put in the class attributes. the possibilities are
	 
	 a) the class is marked for deletion. do nothing
	 b) the class is marked for replacement. reference attributes from the replacement
	 c) the class is marked for change. compare attributes (tedious)
	 d) the class has no replacement, but we need to check if its in a
	 module which has been loaded. if so, reference its attributes
	 
	 In each case, once we have a potential attribute, we have to check
	 back to see if it is changed in the element (mergeClassAttribute)
    -->
    <xsl:for-each select="key('IDENTS',$K)">

      <xsl:variable name="M" select="@module"/>
      <xsl:variable name="CURRENTCLASS" select="."/>
      <xsl:for-each select="$ODD">
	<xsl:choose>
	  <xsl:when test="key('DELETE',$K)"></xsl:when>
	  <xsl:when test="key('REPLACE',$K)">
	    <xsl:for-each select="key('REPLACE',$K)/tei:attList/tei:attDef">
	      <xsl:call-template name="mergeClassAttribute">
		<xsl:with-param name="element" select="$I"/>
		<xsl:with-param name="class" select="$K"/>
		<xsl:with-param name="att" select="@ident"/>
		<xsl:with-param name="original" select="$CURRENTCLASS"/>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:when>
<!-- there are changes to the class spec itself -->
	  <xsl:when test="key('CHANGE',$K)">
	    <xsl:for-each select="key('CHANGE',$K)">
<!-- add in the new attributes -->
	      <xsl:for-each select="tei:attList/tei:attDef[@mode='add']">
		<xsl:call-template name="mergeClassAttribute">
		  <xsl:with-param name="element" select="$I"/>
		  <xsl:with-param name="class" select="$K"/>
		  <xsl:with-param name="att" select="@ident"/>
		  <xsl:with-param name="original" select="$CURRENTCLASS"/>
		</xsl:call-template>
	      </xsl:for-each>
	    </xsl:for-each>
	    <xsl:for-each select="$CURRENTCLASS">
	      <xsl:for-each select="tei:attList/tei:attDef">
		<xsl:call-template name="mergeClassAttribute">
		  <xsl:with-param name="element" select="$I"/>
		  <xsl:with-param name="class" select="$K"/>
		  <xsl:with-param name="att" select="@ident"/>
		  <xsl:with-param name="original" select="$CURRENTCLASS"/>
		</xsl:call-template>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="key('MODULES',$M) or
			  key('MODULES',substring-before($M,'-decl'))">
	    <xsl:for-each select="$CURRENTCLASS">
	      <xsl:for-each select="tei:attList/tei:attDef">
		<xsl:call-template name="mergeClassAttribute">
		  <xsl:with-param name="element" select="$I"/>
		  <xsl:with-param name="class" select="$K"/>
		  <xsl:with-param name="att" select="@ident"/>
		  <xsl:with-param name="original" select="$CURRENTCLASS"/>
		</xsl:call-template>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>  
      
      <xsl:if test="tei:classes/tei:memberOf">
	<xsl:for-each select="tei:classes/tei:memberOf">
	  <xsl:variable name="K" select="@key"/>
	  <xsl:call-template name="classAttributes">
	    <xsl:with-param name="I" select="$I"/>
	    <xsl:with-param name="K" select="$K"/>
	  </xsl:call-template>
	</xsl:for-each>
      </xsl:if>

    </xsl:for-each>

</xsl:template>

<xsl:template name="mergeClassAttribute">
  <xsl:param name="element"/>
  <xsl:param name="class"/>
  <xsl:param name="att"/>
  <xsl:param name="original"/>
  <xsl:variable name="orig" select="."/>
  <xsl:variable name="A" select="@ident"/>
  <xsl:variable name="lookingAt">
    <xsl:value-of select="concat($element,'_',$A)"/>
  </xsl:variable>
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
	<xsl:for-each select="$original">
	  <xsl:choose>
	    <xsl:when test="key('DELETEATT',$lookingAt)"/>
	    <xsl:when test="key('REPLACEATT',$lookingAt)">
	      <xsl:comment>element replacement of class attribute named <xsl:value-of 
		  select="$att"/></xsl:comment>
	      <xsl:for-each select="key('REPLACEATT',$lookingAt)">
		<tei:attDef ident="{$att}">
		  <xsl:copy-of select="@ns"/>
		  <xsl:copy-of select="@usage"/>
		  <xsl:copy-of select="tei:*"/>
		</tei:attDef>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:when test="key('CHANGEATT',$lookingAt)">
	      <xsl:comment>element override of class attribute named <xsl:value-of 
		  select="$att"/></xsl:comment>
	      <xsl:call-template name="mergeAttribute">
		<xsl:with-param name="New" select="key('CHANGEATT',$lookingAt)"/>
		<xsl:with-param name="Old" select="$orig"/>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
		<tei:attRef class="{$class}"
			    key="{translate($att,':','')}"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template name="processAttributes">
  <xsl:param name="ORIGINAL"/>
  <xsl:param name="I"/>
  <!-- first put in the ones we know take precedence -->
  <xsl:copy-of select="tei:attList/tei:attDef[@mode='add']"/>
  <xsl:copy-of select="tei:attList/tei:attDef[@mode='replace']"/>
  <xsl:for-each select="$ORIGINAL/tei:attList"> <!-- original source  context -->
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
  <xsl:for-each select="$ORIGINAL">
    <xsl:if test="local-name(.)='elementSpec'">
      <xsl:call-template name="classAttributes">
	<xsl:with-param name="I" select="$I"/>
	<xsl:with-param name="K" select="'att.global'"/>
      </xsl:call-template>
      <xsl:for-each select="tei:classes/tei:memberOf"> 
	<xsl:variable name="K" select="@key"/>
	<xsl:call-template name="classAttributes">
	  <xsl:with-param name="I" select="$I"/>
	  <xsl:with-param name="K" select="@key"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:if>
  </xsl:for-each>
</xsl:template>


<xsl:template name="mergeAttribute">
  <xsl:param name="New"/>
  <xsl:param name="Old"/>
  <tei:attDef ident="{$Old/@ident}">
    <xsl:for-each select="$New">
      <xsl:attribute name="usage">
	<xsl:choose>
	  <xsl:when test="@usage"><xsl:value-of select="@usage"/></xsl:when>
	  <xsl:otherwise><xsl:value-of select="$Old/@usage"/></xsl:otherwise>
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
		  <xsl:for-each select="$New/tei:valList/tei:valItem[@ident=$thisme]">
		  <xsl:choose>
		    <xsl:when test="tei:equiv">
		      <xsl:copy-of select="tei:equiv"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
			<xsl:copy-of select="tei:equiv"/>
		      </xsl:for-each>
		    </xsl:otherwise>
		  </xsl:choose>
		  
		  <xsl:choose>
		    <xsl:when test="tei:gloss">
		      <xsl:copy-of select="tei:gloss"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
			<xsl:copy-of select="tei:gloss"/>
		      </xsl:for-each>
		    </xsl:otherwise>
		  </xsl:choose>
		  
		  <xsl:choose>
		    <xsl:when test="tei:desc">
		      <xsl:copy-of select="tei:desc"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
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
  <xsl:when test="//tei:schemaSpec">
    <tei:list>
      <xsl:for-each select="tei:*">
	<tei:item>Specification for <xsl:value-of
	select="substring-before(name(.),'Spec')"/>
	<xsl:text> </xsl:text>
	<tei:ref target="#{@ident}">
	  <tei:ident>
	    <xsl:value-of select="@ident"/>
	  </tei:ident>
	</tei:ref>
	</tei:item>
      </xsl:for-each>
    </tei:list>
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
    <xsl:variable name="I">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="copy"/>
      <xsl:copy-of select="tei:altIdent"/>
      <xsl:copy-of select="tei:equiv"/>
      <xsl:copy-of select="tei:gloss"/>
      <xsl:copy-of select="tei:desc"/>
      <xsl:copy-of select="tei:classes"/>
      <xsl:apply-templates select="tei:content" mode="copy"/>
      <tei:attList>
	<xsl:comment>1.</xsl:comment>
	<xsl:call-template name="classAttributesSimple">
	  <xsl:with-param name="I" select="$I"/>
	  <xsl:with-param name="K" select="'att.global'"/>
	</xsl:call-template>
	<xsl:comment>2.</xsl:comment>
	<xsl:for-each select="tei:classes/tei:memberOf"> 
	<xsl:comment>3: <xsl:value-of select="@key"/></xsl:comment>
	  <xsl:call-template name="classAttributesSimple">
	    <xsl:with-param name="I" select="$I"/>
	    <xsl:with-param name="K" select="@key"/>
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
<p>Internal module <xsl:value-of select="@key"/> was located
and expanded.</p>  
</xsl:template>

<xsl:template match="@*|processing-instruction()|comment()|text()" >
  <xsl:copy/>
</xsl:template>

<xsl:template match="*" >
  <xsl:copy>
    <xsl:apply-templates
	select="*|@*|processing-instruction()|comment()|text()" />
  </xsl:copy>
</xsl:template>

<xsl:template name="classAttributesSimple">
  <xsl:param name="I"/>
  <xsl:param name="K"/>
  <xsl:comment>START on <xsl:value-of select="$K"/></xsl:comment>
  <xsl:for-each select="key('IDENTS',$K)">    
    <xsl:variable name="CURRENTCLASS" select="."/>
    <xsl:for-each select="tei:attList/tei:attDef">
<xsl:comment>looking at <xsl:value-of select="$I"/> + <xsl:value-of
select="$K"/> + <xsl:value-of select="@ident"/></xsl:comment>
      <xsl:call-template name="mergeClassAttribute">
	<xsl:with-param name="element" select="$I"/>
	<xsl:with-param name="class" select="$K"/>
	<xsl:with-param name="att" select="@ident"/>
	<xsl:with-param name="original" select="$CURRENTCLASS"/>
      </xsl:call-template>
    </xsl:for-each>
  <xsl:if test="tei:classes/tei:memberOf">
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:variable name="K" select="@key"/>
      <xsl:call-template name="classAttributesSimple">
	<xsl:with-param name="I" select="$I"/>
	<xsl:with-param name="K" select="$K"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:if>
  </xsl:for-each>
  <xsl:comment>FINISH <xsl:value-of select="$K"/></xsl:comment>
</xsl:template>

</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0" 
    exclude-result-prefixes="teix a s tei rng sch xsi">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MAINTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:output encoding="utf-8" indent="yes"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="selectedSchema"/>
  <xsl:param name="verbose"/>
  <xsl:param name="useVersionFromTEI">true</xsl:param>
  <xsl:param name="stripped">false</xsl:param>
  <xsl:param name="defaultSource">http://www.tei-c.org/release/xml/tei/odd/p5subset.xml</xsl:param>
  <xsl:key name="ALLSCHEMASPECS" match="tei:schemaSpec" use="1"/>
  <xsl:key name="ATTCLASSES" match="tei:classSpec[(tei:attList or @type='atts') and not(@ident='tei.TEIform')]" use="@ident"/>
  <xsl:key name="ATTREFS" match="tei:attRef" use="concat(@name,'_',../../@ident)"/>
  <xsl:key name="CHANGE" match="tei:classSpec[@mode='change']" use="@ident"/>
  <xsl:key name="CHANGE" match="tei:elementSpec[@mode='change']" use="@ident"/>
  <xsl:key name="CHANGE" match="tei:macroSpec[@mode='change']" use="@ident"/>
  <xsl:key name="CHANGEATT" match="tei:attDef[@mode='change']" use="concat(../../@ident,'_',@ident)"/>
  <xsl:key name="CHANGECONSTRAINT" match="tei:constraintSpec[@mode='change']" use="concat(../@ident,'_',@ident)"/>
  <xsl:key name="CLASS_MEMBERED" use="tei:classes/tei:memberOf/@key" match="tei:classSpec"/>
  <xsl:key name="DELETE" match="tei:classSpec[@mode='delete']" use="@ident"/>
  <xsl:key name="DELETE" match="tei:elementSpec[@mode='delete']" use="@ident"/>
  <xsl:key name="DELETE" match="tei:macroSpec[@mode='delete']" use="@ident"/>
  <xsl:key name="DELETEATT" match="tei:attDef[@mode='delete']" use="concat(ancestor::tei:elementSpec/@ident,'_',@ident)"/>
  <xsl:key name="DELETEATT" match="tei:attDef[@mode='delete']" use="concat(ancestor::tei:classSpec/@ident,'_',@ident)"/>
  <xsl:key name="DELETECONSTRAINT" match="tei:constraintSpec[@mode='delete']" use="concat(../@ident,'_',@ident)"/>
  <xsl:key name="ELEMENT_MEMBERED" use="tei:classes/tei:memberOf/@key" match="tei:elementSpec"/>
  <xsl:key name="IDS" match="tei:*[@xml:id]" use="@xml:id"/>
  <xsl:key name="MACROS" use="@ident" match="tei:macroSpec"/>
  <xsl:key name="MEMBEROFADD" match="tei:memberOf[not(@mode='delete')]" use="concat(../../@ident,@key)"/>
  <xsl:key name="MEMBEROFDELETE" match="tei:memberOf[@mode='delete']" use="concat(../../@ident,@key)"/>
  <xsl:key name="MODULES" match="tei:moduleRef" use="@key"/>
  <xsl:key name="NEWATTCLASSES" match="tei:classSpec[@type='atts' and @mode='add']" use="@ident"/>
  <xsl:key name="REFED" use="@name" match="rng:ref[ancestor::tei:elementSpec]"/>
  <xsl:key name="REFED" use="@name" match="rng:ref[ancestor::tei:macroSpec and not(@name=ancestor::tei:macroSpec/@ident)]"/>
  <xsl:key name="REFED" use="substring-before(@name,'.attribute')" match="tei:attRef"/>
  <xsl:key name="REFED" use="substring-before(@name,'_')" match="rng:ref[contains(@name,'_')]"/>
  <xsl:key name="REPLACE" match="tei:classSpec[@mode='replace']" use="@ident"/>
  <xsl:key name="REPLACE" match="tei:elementSpec[@mode='replace']" use="@ident"/>
  <xsl:key name="REPLACE" match="tei:macroSpec[@mode='replace']" use="@ident"/>
  <xsl:key name="REPLACEATT" match="tei:attDef[@mode='replace']" use="concat(../../@ident,'_',@ident)"/>
  <xsl:key name="REPLACECONSTRAINT" match="tei:constraintSpec[@mode='replace']" use="concat(../@ident,'_',@ident)"/>

  <xsl:key name="IDENTS" match="tei:macroSpec" use="@ident"/>
  <xsl:key name="IDENTS" match="tei:classSpec" use="@ident"/>
  <xsl:key name="IDENTS" match="tei:elementSpec" use="@ident"/>
  <xsl:key name="MODULE_MEMBERS" match="tei:macroSpec"  use="@module"/>
  <xsl:key name="MODULE_MEMBERS" match="tei:classSpec"  use="@module"/>
  <xsl:key name="MODULE_MEMBERS" match="tei:elementSpec" use="@module"/>

  <xsl:key name="SCHEMASPECS" match="tei:schemaSpec" use="@ident"/>

  <xsl:variable name="AnonymousModule">
    <xsl:text>derived-module-</xsl:text>
    <xsl:value-of select="$selectedSchema"/>
  </xsl:variable>

  <xsl:variable name="ODD">
    <xsl:for-each select="/tei:TEI">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:if test="$useVersionFromTEI='true'">
          <xsl:processing-instruction name="TEIVERSION">
            <xsl:call-template name="getversion"/>
          </xsl:processing-instruction>
        </xsl:if>
        <xsl:apply-templates mode="pass0"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="top" select="/"/>

  <xsl:template match="/">
    <xsl:for-each select="$ODD">
      <xsl:apply-templates mode="pass1"/>
    </xsl:for-each>
  </xsl:template>

  <!-- ******************* Pass 0, follow and expand specGrp ********************************* -->
  <xsl:template match="tei:specGrp" mode="pass0">
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 0: summarize specGrp <xsl:value-of select="@xml:id"/>
      </xsl:message>
    </xsl:if>
    <table xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:for-each select="*">
        <row>
          <xsl:choose>
            <xsl:when test="self::tei:specGrpRef">
              <cell>
                <ref target="#{@target}">reference <xsl:value-of select="@target"/></ref>
              </cell>
              <cell/>
            </xsl:when>
            <xsl:when test="self::tei:elementSpec">
              <cell>
		Element <gi><xsl:value-of select="@ident"/></gi>
	      </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:classSpec">
              <cell>
		Class <ident type="class"><xsl:value-of select="@ident"/></ident>
	      </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:macroSpec">
              <cell>
		Macro <ident type="macro"><xsl:value-of select="@ident"/></ident>
	      </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:moduleRef">
              <cell>
		Module <xsl:value-of select="@key"/>
	      </cell>
	    </xsl:when>
          </xsl:choose>
        </row>
      </xsl:for-each>
    </table>
  </xsl:template>

  <xsl:template match="tei:schemaSpec" mode="pass0">
    <xsl:choose>
      <xsl:when test="@ident=$selectedSchema">
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
        </xsl:copy>
      </xsl:when>
      <xsl:when test="$selectedSchema='' and not(preceding-sibling::tei:schemaSpec)">
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
        </xsl:copy>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:specGrpRef" mode="pass0">
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 0: expand specGrpRef <xsl:value-of select="@target"/>
         </xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
        <xsl:for-each select="key('IDS',substring-after(@target,'#'))">
          <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="document(@target)/tei:specGrp">
          <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text()|@*|comment()|processing-instruction()" mode="pass0">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="pass0">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
    </xsl:copy>
  </xsl:template>


  <!-- ******************* Pass 1, follow schemaSpec ********************************* -->
  <xsl:template match="@*|processing-instruction()|comment()|text()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="pass1">
    <xsl:copy>
      <xsl:apply-templates mode="pass1" select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:schemaSpec" mode="pass1">
    <xsl:variable name="oddsource">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:if test="$verbose='true'">
          <xsl:message>Schema <xsl:value-of select="@ident"/></xsl:message>
        </xsl:if>
        <!-- 
	     it is important to process "tei" first 
	     because of the order of declarations
	-->
	<xsl:for-each select="tei:moduleRef[@key='tei']">
	  <xsl:call-template name="expandModule"/>
	</xsl:for-each>
	
	<xsl:for-each select="tei:moduleRef[@key]">
	  <xsl:if test="not(@key='tei')">
	    <xsl:call-template name="expandModule"/>
	  </xsl:if>
	</xsl:for-each>
	
	<xsl:for-each select="tei:macroRef|tei:classRef|tei:elementRef">
	    <xsl:call-template name="followRef"/>
	</xsl:for-each>

	<xsl:for-each select="tei:classSpec[@mode='add' or not(@mode)]">
	  <xsl:call-template name="createCopy"/>
	</xsl:for-each>

	<xsl:for-each select="tei:macroSpec[@mode='add' or not(@mode)]">
	  <xsl:call-template name="createCopy"/>
	</xsl:for-each>

	<xsl:for-each select="tei:elementSpec[@mode='add' or not(@mode)]">
	  <xsl:call-template name="createCopy"/>
	</xsl:for-each>

      </xsl:copy>

      <xsl:copy-of select="tei:moduleRef[@url]"/>

    </xsl:variable>
    <xsl:for-each select="$oddsource">
      <xsl:apply-templates mode="pass3"/>
    </xsl:for-each>
    <!-- constraints -->
    <xsl:apply-templates mode="copy" select="tei:constraintSpec"/>
  </xsl:template>


  <xsl:template name="expandModule">
    <xsl:variable name="name" select="@key"/>
    <xsl:variable name="exc"
		  select="concat(' ',normalize-space(@except),' ')"/>
    <xsl:variable name="inc"  select="tokenize(normalize-space(@include),' ')"/>
    <xsl:variable name="source">
      <xsl:choose>
	<xsl:when test="@source">
	  <xsl:value-of select="@source"/>
	</xsl:when>
	<xsl:when test="parent::tei:schemaSpec/@source">
	  <xsl:value-of select="parent::tei:schemaSpec/@source"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$defaultSource"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not(doc-available($source))">
      <xsl:message terminate="yes">Error: Source <xsl:value-of
      select='$source'/> not readable</xsl:message>
    </xsl:if>
      <xsl:choose>
	<xsl:when test="not(@except) and not(@include)">
	  <xsl:for-each select="document($source,$top)">
	    <xsl:for-each select="key('MODULE_MEMBERS',$name)">
	      <xsl:call-template name="checkObject">
		  <xsl:with-param name="why"> module <xsl:value-of select="$name"/></xsl:with-param>
		  <xsl:with-param name="source" select="$source"/>
	      </xsl:call-template>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:when>
	<xsl:when test="exists($inc)">
	  <xsl:for-each select="$inc">
	    <xsl:variable name="i" select="."/>
	    <xsl:for-each select="document($source,$top)">
	      <xsl:for-each select="key('IDENTS',$i)">
		<xsl:call-template name="checkObject">
		  <xsl:with-param name="why">(inclusion) module <xsl:value-of select="$name"/></xsl:with-param>
		  <xsl:with-param name="source" select="$source"/>
		</xsl:call-template>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="document($source,$top)">
	    <xsl:for-each select="key('MODULE_MEMBERS',$name)">
	      <xsl:if test="not(contains($exc,concat(' ',@ident,' ')))">
		<xsl:call-template name="checkObject">
		  <xsl:with-param name="why">(exclusion) module <xsl:value-of select="$name"/></xsl:with-param>
		  <xsl:with-param name="source" select="$source"/>
		</xsl:call-template>
	      </xsl:if>
	    </xsl:for-each>
	    </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>

  </xsl:template>

  <xsl:template name="followRef">
    <xsl:variable name="name" select="@key"/>
    <xsl:variable name="source">
      <xsl:choose>
	<xsl:when test="@source">
	  <xsl:value-of select="@source"/>
	</xsl:when>
	<xsl:when test="parent::tei:schemaSpec/@source">
	  <xsl:value-of select="parent::tei:schemaSpec/@source"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$defaultSource"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="not(doc-available($source))">
      <xsl:message terminate="yes">Error: Source <xsl:value-of
      select='$source'/> not readable</xsl:message>
    </xsl:if>
    <xsl:for-each select="document($source,$top)">
      <xsl:for-each select="key('IDENTS',$name)">
	<xsl:call-template name="checkObject">
	  <xsl:with-param name="why">direct reference</xsl:with-param>
	  <xsl:with-param name="source"  select="$source"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

<!-- pass3 -->      
  <xsl:template match="rng:ref" mode="pass3">
    <xsl:variable name="N">
      <xsl:value-of select="@name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="starts-with($N,'macro.') and $stripped='true'">
        <xsl:for-each select="key('MACROS',$N)/tei:content/*">
          <xsl:call-template name="simplifyRelax"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="starts-with($N,'data.')">
        <xsl:apply-templates select="key('MACROS',$N)/tei:content/*" mode="pass3"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:valDesc|tei:equiv|tei:gloss|tei:desc|tei:remarks|tei:exemplum|tei:listRef" mode="pass3">
    <xsl:choose>
      <xsl:when test="$stripped='true'"> </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="*" mode="pass3">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass3"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:content" mode="pass3">
    <xsl:variable name="content">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates mode="pass3"/>
      </xsl:copy>
    </xsl:variable>
    <xsl:apply-templates select="$content" mode="pass4"/>
  </xsl:template>
  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="pass4">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="pass4">
    <xsl:choose>
      <xsl:when test="self::rng:optional     and count(rng:zeroOrMore)=2    and count(*)=2">
        <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
      </xsl:when>
      <xsl:when test="count(*)=1">
        <xsl:variable name="element" select="local-name()"/>
        <xsl:choose>
          <xsl:when test="*[local-name()=$element]">
            <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:when test="$element='optional'         and rng:zeroOrMore">
            <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:when test="$element='optional'         and rng:oneOrMore">
            <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy>
              <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
            </xsl:copy>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="*|@*|text()|comment()|processing-instruction()" mode="pass4"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="pass3">
    <xsl:variable name="used">
      <xsl:call-template name="amINeeded"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$used=''">
        <xsl:if test="$verbose='true'">
          <xsl:message>reject <xsl:value-of select="@ident"/>
               </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates mode="pass3"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="amINeeded">
    <!--
How can a class be ok?
  a) if an element is a member of it
  b) if its referred to in a content model
  c) if some other class is a member of it, and that class is OK
-->
    <xsl:variable name="k" select="@ident"/>
    <xsl:choose>
      <xsl:when test="self::tei:classSpec and         $stripped='true'">y</xsl:when>
      <xsl:when test="starts-with(@ident,'att.global')">y</xsl:when>
      <xsl:when test="key('ELEMENT_MEMBERED',$k)">y</xsl:when>
      <xsl:when test="key('REFED',$k)">y</xsl:when>
      <xsl:when test="key('CLASS_MEMBERED',$k)">
        <xsl:for-each select="key('CLASS_MEMBERED',$k)">
          <xsl:call-template name="amINeeded"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:macroSpec" mode="pass3">
    <xsl:variable name="k">
      <xsl:value-of select="@prefix"/>
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$stripped='true' and starts-with($k,'macro.')"/>
      <xsl:when test="starts-with($k,'data.')"/>
      <xsl:when test="key('REFED',$k)">
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates mode="pass3"/>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>reject <xsl:value-of select="$k"/>
	              </xsl:message>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="checkObject">
    <xsl:param name="why"/>
    <xsl:param name="source"/>
    <!--
        for every object
         - if its in DELETE list, ignore
         - if its in REPLACE list, use that
         - if its in CHANGE list
           (do the hard merge bit)
         - otherwise copy 
        done
  -->
      <xsl:variable name="Current" select="."/>
      <xsl:variable name="specName" select="@ident"/>
      <xsl:variable name="N" select="local-name(.)"/>
      <xsl:for-each select="$ODD">
        <xsl:choose>
          <xsl:when test="key('DELETE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 1: remove <xsl:value-of select="$specName"/></xsl:message>
            </xsl:if>
          </xsl:when>
	  <xsl:when test="key('REPLACE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 1: replace <xsl:value-of select="$specName"/></xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="copy" select="key('REPLACE',$specName)"/>
          </xsl:when>
          <xsl:when test="key('CHANGE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 1: change <xsl:value-of select="$specName"/></xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="change" select="$Current"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:if test="$verbose='true'">
	      <xsl:message> Phase 1: include <xsl:value-of
	      select="$specName"/> from <xsl:value-of
	      select="$source"/> (<xsl:value-of select="$why"/>)</xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="copy" select="$Current"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
  </xsl:template>

  <xsl:template match="@*|processing-instruction()|comment()|text()" mode="change">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="change">
    <xsl:copy>
      <xsl:apply-templates mode="change" select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|processing-instruction()|comment()|text()" mode="copy">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:memberOf" mode="copy">
    <xsl:variable name="k" select="@key"/>
    <xsl:for-each select="$ODD">
      <xsl:choose>
        <xsl:when test="key('DELETE',$k)"/>
        <xsl:otherwise>
          <memberOf xmlns="http://www.tei-c.org/ns/1.0" key="{$k}"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="*" mode="copy">
    <xsl:copy>
      <xsl:apply-templates mode="copy" select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:elementSpec/@mode" mode="change">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="tei:listRef" mode="copy"/>
  <xsl:template match="tei:elementSpec/@mode" mode="copy">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="tei:macroSpec/@mode" mode="copy">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="tei:classSpec/@mode" mode="copy">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="tei:elementSpec" mode="copy">
    <xsl:variable name="orig" select="."/>
    <xsl:copy>
      <xsl:if test="not(@module)">
        <xsl:attribute name="module">
          <xsl:value-of select="$AnonymousModule"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates mode="copy" select="@*"/>
      <xsl:copy-of select="tei:altIdent"/>
      <xsl:if test="$stripped='false'">
        <xsl:apply-templates mode="copy" select="tei:equiv"/>
        <xsl:copy-of select="tei:gloss"/>
        <xsl:copy-of select="tei:desc"/>
      </xsl:if>
      <xsl:copy-of select="tei:classes"/>
      <xsl:apply-templates mode="copy" select="tei:content"/>
      <xsl:apply-templates mode="copy" select="tei:constraintSpec"/>
      <attList xmlns="http://www.tei-c.org/ns/1.0">
        <xsl:call-template name="addClassAttsToCopy"/>
        <xsl:choose>
          <xsl:when test="tei:attList[@org='choice']">
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
            <xsl:copy-of select="tei:attList/tei:attDef[@mode='add' or not(@mode)]"/>
            <xsl:copy-of select="tei:attList/tei:attRef"/>
            <xsl:copy-of select="tei:attList/tei:attList"/>
          </xsl:otherwise>
        </xsl:choose>
      </attList>
      <xsl:if test="$stripped='false'">
        <xsl:copy-of select="tei:exemplum"/>
        <xsl:copy-of select="tei:remarks"/>
        <xsl:copy-of select="tei:listRef"/>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="addClassAttsToCopy">
    <xsl:if test="not(@ns) or @ns='http://www.tei-c.org/ns/1.0' or @ns='http://www.tei-c.org/ns/Examples'">
      <xsl:call-template name="classAttributes">
        <xsl:with-param name="whence">1</xsl:with-param>
        <xsl:with-param name="elementName" select="@ident"/>
        <xsl:with-param name="className" select="'att.global'"/>
      </xsl:call-template>
    </xsl:if>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:call-template name="classAttributes">
        <xsl:with-param name="whence">2</xsl:with-param>
        <xsl:with-param name="elementName" select="../../@ident"/>
        <xsl:with-param name="className" select="@key"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="tei:elementSpec" mode="change">
    <xsl:variable name="elementName">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:attribute name="rend">change</xsl:attribute>
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- 
For each element, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. The constraints and attributes are identifiable
for change individually.
 -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('CHANGE',$elementName)">
          <!-- if there is an altIdent, use it -->
          <xsl:copy-of select="@ns"/>
          <xsl:copy-of select="tei:altIdent"/>
          <!-- equiv, gloss, desc trio -->
          <xsl:choose>
            <xsl:when test="tei:equiv">
              <xsl:apply-templates mode="copy" select="tei:equiv"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:equiv"/>
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
            <xsl:when test="$stripped='true'"/>
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
          <classes xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:classes[@mode='change']">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:choose>
                    <xsl:when test="@mode='delete'"/>
                    <xsl:when test="@mode='add' or not (@mode)">
                      <memberOf key="{@key}"/>
                    </xsl:when>
                  </xsl:choose>
                </xsl:for-each>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:variable name="metoo">
                      <xsl:value-of select="concat(../../@ident,@key)"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:choose>
                        <xsl:when test="key('DELETE',$me)"> </xsl:when>
                        <xsl:when test="key('MEMBEROFDELETE',$metoo)"> </xsl:when>
                        <xsl:when test="key('MEMBEROFADD',$metoo)"> </xsl:when>
                        <xsl:otherwise>
                          <memberOf key="{$me}"/>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="tei:classes">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:if test="not(key('DELETE',$me))">
                        <memberOf key="{$me}"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </classes>
          <!-- valList -->
          <xsl:choose>
            <xsl:when test="tei:valList">
              <xsl:apply-templates mode="copy" select="tei:valList"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:valList"/>
              </xsl:for-each>
            </xsl:otherwise>
          </xsl:choose>
          <!-- element content -->
          <content xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:content/rng:*">
                <xsl:apply-templates mode="copy" select="tei:content/*"/>
              </xsl:when>
              <xsl:when test="tei:content/tei:*">
                <xsl:apply-templates mode="copy" select="tei:content/*"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:apply-templates mode="copy" select="tei:content/*"/>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </content>
          <!-- element constraints -->
          <xsl:call-template name="processConstraints">
            <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
            <xsl:with-param name="elementName" select="$elementName"/>
          </xsl:call-template>
          <!-- attList -->
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:copy-of select="tei:attList/@org"/>
            <xsl:call-template name="processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="elementName" select="$elementName"/>
            </xsl:call-template>
          </attList>
          <!-- exemplum, remarks and listRef are either replacements or not -->
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
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
            <xsl:when test="$stripped='true'"/>
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
      <xsl:attribute name="rend">change</xsl:attribute>
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
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:equiv">
              <xsl:apply-templates mode="copy" select="tei:equiv"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:apply-templates mode="copy" select="tei:equiv"/>
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
            <xsl:when test="$stripped='true'"/>
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
            <xsl:when test="$stripped='true'"/>
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
            <xsl:when test="$stripped='true'"/>
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
      <xsl:attribute name="rend">change</xsl:attribute>
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- for each section of the class spec, 
     go through the sections one by one
     and see if they are present in the change mode version -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('CHANGE',$className)">
          <!-- context is now a classSpec in change mode in the ODD spec -->
          <!-- description -->
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
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
          <classes xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:classes[@mode='change']">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:choose>
                    <xsl:when test="@mode='delete'"/>
                    <xsl:when test="@mode='add' or not (@mode)">
                      <memberOf key="{@key}"/>
                    </xsl:when>
                  </xsl:choose>
                </xsl:for-each>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:variable name="metoo">
                      <xsl:value-of select="concat(../../@ident,@key)"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:choose>
                        <xsl:when test="key('DELETE',$me)"> </xsl:when>
                        <xsl:when test="key('MEMBEROFDELETE',$metoo)"> </xsl:when>
                        <xsl:when test="key('MEMBEROFADD',$metoo)"> </xsl:when>
                        <xsl:otherwise>
                          <memberOf key="{$me}"/>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="tei:classes">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:if test="not(key('DELETE',$me))">
                        <memberOf key="{$me}"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </classes>
          <!-- attList -->
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:call-template name="processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="elementName" select="''"/>
            </xsl:call-template>
            <xsl:copy-of select="tei:attList/tei:attRef"/>
          </attList>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="rng:choice|rng:list|rng:group|rng:optional|rng:oneOrMore|rng:zeroOrMore" mode="copy">
    <xsl:call-template name="simplifyRelax"/>
  </xsl:template>
  <xsl:template name="simplifyRelax">
    <xsl:variable name="element">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <!-- 
for each RELAX NG content model,
remove reference to any elements which have been
deleted, or to classes which are empty.
This may make the container empty,
so that is only put back in if there is some content
-->
    <xsl:variable name="contents">
      <WHAT>
        <xsl:for-each select="rng:*|processing-instruction()">
          <xsl:choose>
            <xsl:when test="self::processing-instruction()">
              <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:when test="self::rng:element">
              <element xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </element>
            </xsl:when>
            <xsl:when test="self::rng:name">
              <name xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </name>
            </xsl:when>
            <xsl:when test="self::rng:attribute">
              <attribute xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </attribute>
            </xsl:when>
            <xsl:when test="self::rng:data">
              <data xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="copy"/>
              </data>
            </xsl:when>
            <xsl:when test="self::rng:text">
              <text xmlns="http://relaxng.org/ns/structure/1.0"/>
            </xsl:when>
            <xsl:when test="self::rng:value">
              <value xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:apply-templates/>
              </value>
            </xsl:when>
            <xsl:when test="self::rng:ref">
              <xsl:variable name="N" select="@name"/>
              <xsl:for-each select="$ODD">
                <xsl:choose>
                  <xsl:when test="$stripped='true'">
                    <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$N}"/>
                  </xsl:when>
                  <xsl:when test="starts-with($N,'data.')">
                    <xsl:apply-templates select="key('MACROS',$N)/tei:content/*" mode="pass3"/>
                  </xsl:when>
                  <xsl:when test="key('DELETE',$N)"/>
                  <xsl:otherwise>
                    <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$N}"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="simplifyRelax"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </WHAT>
    </xsl:variable>
    <xsl:variable name="entCount">
      <xsl:for-each select="$contents/WHAT">
        <xsl:value-of select="count(*)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:for-each select="$contents/WHAT">
      <xsl:choose>
        <xsl:when test="$entCount=1     and local-name(*)=$element">
          <xsl:copy-of select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='optional'     and $entCount=1     and rng:zeroOrMore">
          <xsl:copy-of select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='optional'     and $entCount=1     and rng:oneOrMore">
          <xsl:copy-of select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='oneOrMore'     and $entCount=1     and rng:zeroOrMore">
          <oneOrMore xmlns="http://relaxng.org/ns/structure/1.0">
            <xsl:copy-of select="rng:zeroOrMore/*"/>
          </oneOrMore>
        </xsl:when>
        <xsl:when test="self::rng:zeroOrMore/rng:ref/@name='model.global'        and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name='model.global']"/>
        <xsl:when test="$entCount&gt;0 or $stripped='true'">
          <xsl:element xmlns="http://relaxng.org/ns/structure/1.0" name="{$element}">
            <xsl:copy-of select="*|@*|text()|comment()|processing-instruction()"/>
          </xsl:element>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="classAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="whence"/>
    <!-- 
    On entry, we are sitting on an <elementSpec> or <classSpec> 
    and seeing if we can pick up some attributes for 
    $elementName. We travel to the ODD first
    to see if it has some overrides
    -->
    <xsl:for-each select="$ODD">
      <xsl:choose>
        <xsl:when test="key('MEMBEROFDELETE',concat($elementName,$className))"> </xsl:when>
        <!-- the class is referenced in the ODD and has redefined <classes>-->
        <xsl:when test="key('ATTCLASSES',$className)/tei:classes">
          <xsl:for-each select="key('ATTCLASSES',$className)">
            <xsl:call-template name="processClassAttributes">
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="$className"/>
              <xsl:with-param name="whence" select="$whence"/>
              <xsl:with-param name="fromODD">true</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <!-- the class is referenced in the ODD and has redefined <attList>-->
        <xsl:when test="key('ATTCLASSES',$className)/tei:attList">
          <xsl:for-each select="key('ATTCLASSES',$className)">
            <xsl:call-template name="processClassAttributes">
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="$className"/>
              <xsl:with-param name="whence" select="$whence"/>
              <xsl:with-param name="fromODD">true</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <!-- otherwise, we'll revert to source
	     (assuming the class is of type 'atts')
	-->
        <xsl:when test="not($defaultSource='')">
          <xsl:for-each select="document($defaultSource)/tei:TEI">
            <xsl:for-each select="key('ATTCLASSES',$className)">
              <xsl:call-template name="processClassAttributes">
                <xsl:with-param name="elementName" select="$elementName"/>
                <xsl:with-param name="className" select="$className"/>
                <xsl:with-param name="whence" select="$whence"/>
                <xsl:with-param name="fromODD">false</xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:for-each>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="processClassAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="fromODD"/>
    <xsl:param name="whence"/>
    <!-- we are sitting on a classSpec, could be in the ODD
	 or could be in the source -->
    <xsl:variable name="M" select="@module"/>
    <!-- decide whether to proceed with this this class. if we don't
    have this module, or we have deleted the class, we can bypass it now.-->
    <xsl:variable name="use">
      <xsl:choose>
        <xsl:when test="$fromODD='true' and @mode='add'">
          <xsl:text>true</xsl:text>
        </xsl:when>
        <xsl:when test="not(@module)">
          <xsl:text>true</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="$ODD">
            <xsl:choose>
              <xsl:when test="key('DELETE',$className)"/>
              <xsl:when test="key('MODULES',$M) or         key('MODULES',substring-before($M,'-decl'))">
                <xsl:text>true</xsl:text>
              </xsl:when>
            </xsl:choose>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!-- DEBUG
<xsl:message>START <xsl:value-of select="$whence"/>: <xsl:value-of select="$elementName"/> + <xsl:value-of
select="$className"/> + <xsl:value-of
select="$fromODD"/>+<xsl:value-of select="$use"/>+<xsl:value-of
select="$M"/></xsl:message>
 -->
    <xsl:if test="$use='true'">
      <!-- 
	   We need to put in the class attributes. We'll 
	   use the value of $fromODD to see whether this is in the ODD.
	   
	   a) the class is new in this customization, add all attributes regardless
	   b) the class is marked for deletion. do nothing
	   c) the class is marked for replacement. reference attributes from the replacement
	   d) the class is marked for change. compare attributes (tedious)
	   e) the class has no replacement; reference its attributes
	   
	   In each case, once we have a potential attribute, we have to check
	   back to see if it is changed in the element (mergeClassAttribute)
	   
	   First, establish whether any attributes in the class,
	   inherited or otherwise, are changed in the ODD. We will
	   use this information later
      -->
      <xsl:variable name="anyChanged">
        <xsl:choose>
          <xsl:when test="$fromODD='false'">
            <xsl:call-template name="checkClassAttribute">
              <xsl:with-param name="element" select="$elementName"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="not($defaultSource='')">
            <xsl:for-each select="document($defaultSource)/tei:TEI">
              <xsl:for-each select="key('ATTCLASSES',$className)">
                <xsl:call-template name="checkClassAttribute">
                  <xsl:with-param name="element" select="$elementName"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <!--
    <xsl:message>Class <xsl:value-of select="$className"/>, element
    <xsl:value-of select="$elementName"/>: has changes: <xsl:value-of
    select="$anyChanged"/></xsl:message>
-->
      <xsl:choose>
        <!-- a) new class in ODD -->
        <xsl:when test="$fromODD='true' and @mode='add'">
          <attRef xmlns="http://www.tei-c.org/ns/1.0" n="1" name="{$className}.attributes"/>
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
              <xsl:with-param name="fromODD">
                <xsl:value-of select="$fromODD"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:for-each select="tei:attList/tei:attList">
            <attList xmlns="http://www.tei-c.org/ns/1.0">
              <xsl:copy-of select="@org"/>
              <xsl:for-each select="tei:attDef">
                <xsl:call-template name="mergeClassAttribute">
                  <xsl:with-param name="source">2</xsl:with-param>
                  <xsl:with-param name="element" select="$elementName"/>
                  <xsl:with-param name="class" select="$className"/>
                  <xsl:with-param name="fromODD">
                    <xsl:value-of select="$fromODD"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </attList>
          </xsl:for-each>
        </xsl:when>
        <!-- d) there are changes to attributes in the class spec itself,
	but the element makes no override -->
        <xsl:when test="@mode='change' and tei:attList and not (contains($anyChanged,':element-'))">
          <xsl:if test="$verbose='true'">
            <xsl:message>d) Class <xsl:value-of select="$className"/> for <xsl:value-of select="$elementName"/> has no changes in element, refer by name</xsl:message>
          </xsl:if>
          <attRef xmlns="http://www.tei-c.org/ns/1.0" n="5" name="{$className}.attributes"/>
        </xsl:when>
        <!-- e) there are changes to attributes in the class spec itself -->
        <xsl:when test="@mode='change' and tei:attList">
          <!-- always references attributes in add mode -->
          <xsl:for-each select="tei:attList/tei:attDef[@mode='add']">
            <attRef xmlns="http://www.tei-c.org/ns/1.0" n="2" name="{$className}.attribute.{translate(@ident,':','')}"/>
          </xsl:for-each>
          <!-- go back to original and proceed from there -->
          <xsl:choose>
            <xsl:when test="not($defaultSource='')">
              <xsl:for-each select="document($defaultSource)/tei:TEI">
                <xsl:for-each select="key('ATTCLASSES',$className)">
                  <xsl:call-template name="tryAttributes">
                    <xsl:with-param name="elementName" select="$elementName"/>
                    <xsl:with-param name="className" select="$className"/>
                    <xsl:with-param name="fromODD">
                      <xsl:value-of select="$fromODD"/>
                    </xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:when>
          </xsl:choose>
          <xsl:for-each select="tei:attList/tei:attRef">
            <xsl:copy-of select="."/>
          </xsl:for-each>
        </xsl:when>
        <!-- there are no changes to the attributes in the odd-->
        <xsl:when test="$anyChanged=''">
          <xsl:if test="$verbose='true'">
            <xsl:message>f) Class <xsl:value-of select="$className"/> for <xsl:value-of select="$elementName"/> has no changes, refer by name</xsl:message>
          </xsl:if>
          <attRef xmlns="http://www.tei-c.org/ns/1.0" n="4" name="{$className}.attributes"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="$verbose='true'">
            <xsl:message>g) Class <xsl:value-of select="$className"/> for <xsl:value-of select="$elementName"/> has changes in odd, refer by values</xsl:message>
          </xsl:if>
          <!-- attributes here -->
          <xsl:for-each select="tei:attList/tei:attDef">
            <xsl:call-template name="mergeClassAttribute">
              <xsl:with-param name="source">7</xsl:with-param>
              <xsl:with-param name="element" select="$elementName"/>
              <xsl:with-param name="class" select="$className"/>
              <xsl:with-param name="fromODD">
                <xsl:value-of select="$fromODD"/>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
          <!-- embedded attribute lists  -->
          <xsl:for-each select="tei:attList/tei:attList">
            <attList xmlns="http://www.tei-c.org/ns/1.0">
              <xsl:copy-of select="@org"/>
              <xsl:for-each select="tei:attDef">
                <xsl:call-template name="mergeClassAttribute">
                  <xsl:with-param name="source">8</xsl:with-param>
                  <xsl:with-param name="element" select="$elementName"/>
                  <xsl:with-param name="class" select="$className"/>
                  <xsl:with-param name="fromODD">
                    <xsl:value-of select="$fromODD"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </attList>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
      <!-- Now attributes referenced from classes we are a member
     of. Again, check whether we are in ODD or not. If
     we have not tinkered with the superclass, this is not
     needed, obviously.
      -->
      <!-- DEBUG
      <xsl:message>Now time to look at subclasses of <xsl:value-of
      select="@ident"/> whose changes status was <xsl:value-of
      select="$anyChanged"/>; we are in fromOdd <xsl:value-of
      select="$fromODD"/></xsl:message>
-->
      <xsl:choose>
        <xsl:when test="@mode='change' and tei:attList and not (contains($anyChanged,':element-'))"/>
        <xsl:when test="$fromODD='false' and $anyChanged =''"/>
        <xsl:when test="$fromODD='true' and not(.//tei:attDef)"/>
        <xsl:when test="$fromODD='true' and tei:classes[@mode='replace']">
          <xsl:for-each select="tei:classes/tei:memberOf">
            <xsl:call-template name="classAttributes">
              <xsl:with-param name="whence">3</xsl:with-param>
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="@key"/>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="$fromODD='true' and tei:classes[@mode='change']">
            <xsl:for-each select="tei:classes/tei:memberOf[@mode='add']">
              <xsl:call-template name="classAttributes">
                <xsl:with-param name="whence">11</xsl:with-param>
                <xsl:with-param name="elementName" select="$elementName"/>
                <xsl:with-param name="className" select="@key"/>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:if>
          <xsl:choose>
            <xsl:when test="not($defaultSource='')">
              <xsl:for-each select="document($defaultSource)/tei:TEI">
                <xsl:for-each select="key('ATTCLASSES',$className)">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:call-template name="classAttributes">
                      <xsl:with-param name="whence">5</xsl:with-param>
                      <xsl:with-param name="elementName" select="$elementName"/>
                      <xsl:with-param name="className" select="@key"/>
                    </xsl:call-template>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:when>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  <xsl:template name="tryAttributes">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="fromODD"/>
    <xsl:for-each select="tei:attList/tei:attDef">
      <xsl:call-template name="mergeClassAttribute">
        <xsl:with-param name="source">3</xsl:with-param>
        <xsl:with-param name="element" select="$elementName"/>
        <xsl:with-param name="class" select="$className"/>
        <xsl:with-param name="fromODD">
          <xsl:value-of select="$fromODD"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:for-each select="tei:attList/tei:attList">
      <attList xmlns="http://www.tei-c.org/ns/1.0">
        <xsl:copy-of select="@org"/>
        <xsl:for-each select="tei:attDef">
          <xsl:call-template name="mergeClassAttribute">
            <xsl:with-param name="source">4</xsl:with-param>
            <xsl:with-param name="element" select="$elementName"/>
            <xsl:with-param name="class" select="$className"/>
            <xsl:with-param name="fromODD">
              <xsl:value-of select="$fromODD"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:for-each>
      </attList>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="mergeClassAttribute">
    <!-- 
	 sitting on a source class. look at the 
	 attribute and see whether it has changed
	 in the customization
    -->
    <xsl:param name="source"/>
    <xsl:param name="element"/>
    <xsl:param name="class"/>
    <xsl:param name="fromODD"/>
    <xsl:variable name="att" select="@ident"/>
    <xsl:variable name="wherefrom" select="."/>
    <xsl:variable name="attRef">
      <xsl:value-of select="concat($class,'.attribute.' ,translate($att,':',''),'_',$element)"/>
    </xsl:variable>
    <xsl:variable name="lookingAt">
      <xsl:value-of select="concat($element,'_',@ident)"/>
    </xsl:variable>
    <xsl:for-each select="$ODD">
      <xsl:choose>
        <!-- deleted in the customization at the class level -->
        <xsl:when test="key('DELETEATT',concat($class,'_',$att))"/>
        <!-- deleted in the customization at the element level -->
        <xsl:when test="key('DELETEATT',$lookingAt)"/>
        <!-- replaced in the customization at the element level -->
        <xsl:when test="key('REPLACEATT',$lookingAt)"/>
        <!-- changed in the customization by the element -->
        <xsl:when test="key('CHANGEATT',$lookingAt)">
          <xsl:call-template name="mergeAttribute">
            <xsl:with-param name="New" select="key('CHANGEATT',$lookingAt)"/>
            <xsl:with-param name="Old" select="$wherefrom"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:choose>
            <xsl:when test="$fromODD='false'">
              <xsl:for-each select="$wherefrom">
                <xsl:call-template name="unChangedAtt">
                  <xsl:with-param name="debug">1</xsl:with-param>
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
                    <xsl:value-of select="$wherefrom"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="unChangedAtt">
                <xsl:with-param name="debug">2</xsl:with-param>
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
                  <xsl:value-of select="$wherefrom"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="unChangedAtt">
    <xsl:param name="lookingAt"/>
    <xsl:param name="att"/>
    <xsl:param name="class"/>
    <xsl:param name="orig"/>
    <xsl:param name="attRef"/>
    <xsl:param name="debug"/>
    <xsl:choose>
      <!-- don't make another reference to a class attribute 
	 if we already have an attRef -->
      <xsl:when test="key('ATTREFS',$attRef)"/>
      <xsl:otherwise>
        <attRef xmlns="http://www.tei-c.org/ns/1.0" n="3-{$debug}" name="{$class}.attribute.{translate($att,':','')}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="checkClassAttribute">
    <xsl:param name="element"/>
    <!-- look at each attribute in turn, and then repeat for any
	 inherited classes -->
    <xsl:variable name="class" select="@ident"/>
    <xsl:variable name="mode" select="@mode"/>
    <xsl:for-each select="tei:attList//tei:attDef">
      <xsl:variable name="att" select="@ident"/>
      <xsl:for-each select="$ODD">
        <xsl:choose>
          <!-- deleted in the customization at the class level -->
          <xsl:when test="key('DELETEATT',concat($class,'_',$att))">
            <xsl:text>:class-del-</xsl:text>
            <xsl:value-of select="$att"/>
          </xsl:when>
          <!-- deleted in the customization at the element level -->
          <xsl:when test="key('DELETEATT',concat($element,'_',$att))">
            <xsl:text>:element-del-</xsl:text>
            <xsl:value-of select="$att"/>
          </xsl:when>
          <!-- replaced in the customization at the element level -->
          <xsl:when test="key('REPLACEATT',concat($element,'_',$att))">
            <xsl:text>:element-replace-</xsl:text>
            <xsl:value-of select="$att"/>
          </xsl:when>
          <!-- changed in the customization by the element -->
          <xsl:when test="key('CHANGEATT',concat($element,'_',$att))">
            <xsl:text>:element-change-</xsl:text>
            <xsl:value-of select="$att"/>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </xsl:for-each>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('ATTCLASSES',@key)">
        <xsl:call-template name="checkClassAttribute">
          <xsl:with-param name="element" select="$element"/>
        </xsl:call-template>
      </xsl:for-each>
    </xsl:for-each>
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
        <attList xmlns="http://www.tei-c.org/ns/1.0">
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
        </attList>
      </xsl:for-each>
      <xsl:variable name="atts">
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
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="@org">
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:copy-of select="@org"/>
            <xsl:copy-of select="$atts"/>
          </attList>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$atts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <!-- now we need to go back to the classes of which this 
       element is a member and reference their untouched attributes -->
    <xsl:choose>
      <xsl:when test="$elementName=''"/>
      <xsl:otherwise>
        <xsl:call-template name="classAttributes">
          <xsl:with-param name="whence">7</xsl:with-param>
          <xsl:with-param name="elementName" select="$elementName"/>
          <xsl:with-param name="className" select="'att.global'"/>
        </xsl:call-template>
        <xsl:variable name="classMembership">
          <x>
            <xsl:choose>
              <xsl:when test="tei:classes[@mode='change']">
                <xsl:for-each select="tei:classes/tei:memberOf[not(@mode='delete')]">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:copy-of select="."/>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="tei:classes">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:copy-of select="."/>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </x>
        </xsl:variable>
        <xsl:for-each select="$classMembership/x/tei:memberOf">
          <xsl:if test="not(preceding-sibling::tei:memberOf[@key=current()/@key])">
            <xsl:call-template name="classAttributes">
              <xsl:with-param name="whence">8</xsl:with-param>
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="@key"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="mergeAttribute">
    <xsl:param name="New"/>
    <xsl:param name="Old"/>
    <attDef xmlns="http://www.tei-c.org/ns/1.0" ident="{$Old/@ident}">
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
          <xsl:when test="$stripped='true'"/>
          <xsl:when test="tei:equiv">
            <xsl:apply-templates mode="copy" select="tei:equiv"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:apply-templates mode="copy" select="tei:equiv"/>
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
          <xsl:when test="$stripped='true'"/>
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
                  <xsl:if test="not($New/tei:valList/tei:valItem[@ident=$thisme and (@mode='delete' or @mode='replace')])">
                    <xsl:copy>
                      <xsl:copy-of select="@*"/>
                      <xsl:for-each select="$New/tei:valList/tei:valItem[@ident=$thisme]">
                        <xsl:choose>
                          <xsl:when test="tei:equiv">
                            <xsl:apply-templates mode="copy" select="tei:equiv"/>
                          </xsl:when>
                          <xsl:otherwise>
                            <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                              <xsl:apply-templates mode="copy" select="tei:equiv"/>
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
                          <xsl:when test="$stripped='true'"/>
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
          <xsl:when test="$stripped='true'"/>
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
          <xsl:when test="$stripped='true'"/>
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
    </attDef>
  </xsl:template>
  <xsl:template match="tei:specGrp">
    <xsl:choose>
      <xsl:when test="ancestor::tei:schemaSpec"> </xsl:when>
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
        <xsl:if test="$stripped='false'">
          <xsl:apply-templates mode="copy" select="tei:equiv"/>
          <xsl:copy-of select="tei:gloss"/>
          <xsl:copy-of select="tei:desc"/>
        </xsl:if>
        <xsl:copy-of select="tei:classes"/>
        <xsl:apply-templates mode="copy" select="tei:content"/>
        <xsl:apply-templates mode="copy" select="tei:constraintSpec"/>
        <attList xmlns="http://www.tei-c.org/ns/1.0">
          <xsl:comment>1.</xsl:comment>
          <xsl:call-template name="classAttributesSimple">
            <xsl:with-param name="whence">9</xsl:with-param>
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="'att.global'"/>
          </xsl:call-template>
          <xsl:comment>2.</xsl:comment>
          <xsl:for-each select="tei:classes/tei:memberOf">
            <xsl:comment>3: <xsl:value-of select="@key"/>
                  </xsl:comment>
            <xsl:call-template name="classAttributesSimple">
              <xsl:with-param name="whence">10</xsl:with-param>
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="@key"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:comment>4.</xsl:comment>
          <xsl:apply-templates select="tei:attList"/>
          <xsl:comment>5.</xsl:comment>
        </attList>
        <xsl:if test="$stripped='false'">
          <xsl:copy-of select="tei:exemplum"/>
          <xsl:copy-of select="tei:remarks"/>
          <xsl:copy-of select="tei:listRef"/>
        </xsl:if>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@url]">
    <p>Include external module <xsl:value-of select="@url"/>.</p>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@key]">
    <p>Internal module <xsl:value-of select="@key"/> was located and expanded.</p>
  </xsl:template>
  <xsl:template match="@*|processing-instruction()|comment()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="classAttributesSimple">
    <xsl:param name="elementName"/>
    <xsl:param name="className"/>
    <xsl:param name="whence"/>
    <xsl:comment>START on <xsl:value-of select="$className"/>
      </xsl:comment>
    <xsl:for-each select="key('ATTCLASSES',$className)">
      <xsl:variable name="CURRENTCLASS" select="."/>
      <xsl:for-each select="tei:attList/tei:attDef">
        <xsl:call-template name="mergeClassAttribute">
          <xsl:with-param name="source">9</xsl:with-param>
          <xsl:with-param name="element" select="$elementName"/>
          <xsl:with-param name="class" select="$className"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:if test="tei:classes/tei:memberOf">
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:variable name="cName" select="@key"/>
          <xsl:call-template name="classAttributesSimple">
            <xsl:with-param name="whence">11</xsl:with-param>
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="$cName"/>
          </xsl:call-template>
        </xsl:for-each>
      </xsl:if>
    </xsl:for-each>
    <xsl:comment>FINISH <xsl:value-of select="$className"/>
      </xsl:comment>
  </xsl:template>

  <xsl:template name="createCopy">
    <xsl:if test="$verbose='true'">
      <xsl:message>Create <xsl:value-of select="@ident"/>            </xsl:message>
    </xsl:if>
    <xsl:element xmlns="http://www.tei-c.org/ns/1.0" name="{local-name()}">
      <xsl:if test="not(@module)">
        <xsl:attribute name="module">
          <xsl:value-of select="$AnonymousModule"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="local-name()='classSpec' and @type='model' and not(@predeclare)">
        <xsl:attribute name="predeclare">true</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates mode="copy" select="@*|*|processing-instruction()|comment()|text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="getversion">
    <xsl:choose>
      <xsl:when test="not($defaultSource='')">
        <xsl:for-each select="document($defaultSource)/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
          <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="processConstraints">
    <xsl:param name="ORIGINAL"/>
    <xsl:param name="elementName"/>
    <!-- first put in the ones we know take precedence -->
    <xsl:copy-of select="tei:constraintSpec[@mode='add' or not(@mode)]"/>
    <xsl:copy-of select="tei:constraintSpec[@mode='replace']"/>
    <xsl:copy-of select="tei:constraintSpec[@mode='change']"/>
    <xsl:for-each select="$ORIGINAL">
      <!-- original source  context -->
      <xsl:for-each select="tei:constraintSpec">
        <xsl:variable name="CONSTRAINT" select="."/>
        <xsl:variable name="lookingAt">
          <xsl:value-of select="concat(../@ident,'_',@ident)"/>
        </xsl:variable>
        <xsl:for-each select="$ODD">
          <xsl:choose>
            <xsl:when test="key('DELETECONSTRAINT',$lookingAt)"/>
            <xsl:when test="key('REPLACECONSTRAINT',$lookingAt)"/>
            <xsl:when test="key('CHANGECONSTRAINT',$lookingAt)"/>
            <xsl:otherwise>
              <xsl:copy-of select="$CONSTRAINT"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

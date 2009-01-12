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
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:output encoding="utf-8" indent="yes"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="selectedSchema"/>
  <xsl:param name="verbose"/>
  <xsl:param name="useVersionFromTEI">true</xsl:param>
  <xsl:param name="stripped">false</xsl:param>
  <xsl:param name="TEISERVER">http://localhost/Query/</xsl:param>
  <xsl:param name="localsource"/>
  <xsl:key name="MEMBEROFDELETE" match="tei:memberOf[@mode='delete']" use="concat(../../@ident,@key)"/>
  <xsl:key name="MEMBEROFADD" match="tei:memberOf[not(@mode='delete')]" use="concat(../../@ident,@key)"/>
  <xsl:key name="MACROS" use="@ident" match="tei:macroSpec"/>
  <xsl:key name='REFED' use="@name" match="rng:ref"/>
  <xsl:key name='REFED' use="substring-before(@name,'_')" match="rng:ref[contains(@name,'_')]"/>
  <xsl:key name='REFED' use="@key" match="tei:memberOf"/>
  <xsl:key name='REFED' use="substring-before(@name,'.attribute')" match="tei:attRef"/>
  <xsl:key match="tei:schemaSpec" name="SCHEMASPECS" use="@ident"/>
  <xsl:key match="tei:schemaSpec" name="ALLSCHEMASPECS" use="1"/>
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

  <xsl:variable name="AnonymousModule">
    <xsl:text>derived-module-</xsl:text>
    <xsl:value-of  select="$selectedSchema"/>
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
	<xsl:apply-templates mode="flattenSchemaSpec"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:template match="tei:specGrp" mode="flattenSchemaSpec"/>

  <xsl:template match="tei:schemaSpec" mode="flattenSchemaSpec">
    <xsl:choose>
      <xsl:when test="@ident=$selectedSchema">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates 
	      select="*|text()|comment()|processing-instruction()"
	      mode="flattenSchemaSpec"/>
	</xsl:copy>
      </xsl:when>
      <xsl:when test="$selectedSchema='' and not(preceding-sibling::tei:schemaSpec)">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates 
	      select="*|text()|comment()|processing-instruction()"
	      mode="flattenSchemaSpec"/>
	</xsl:copy>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:specGrpRef" mode="flattenSchemaSpec">
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 0: expand specGrpRef <xsl:value-of select="@target"/></xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
	<xsl:for-each select="key('IDS',substring-after(@target,'#'))">
	  <xsl:apply-templates
	      select="*|text()|comment()|processing-instruction()"
	      mode="flattenSchemaSpec"/>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="document(@target)/tei:specGrp">
	  <xsl:apply-templates
	      select="*|text()|comment()|processing-instruction()"
	      mode="flattenSchemaSpec"/>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>


  <xsl:template match="text()|@*|comment()|processing-instruction()" 
		mode="flattenSchemaSpec">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="flattenSchemaSpec">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates
	  select="*|text()|comment()|processing-instruction()"
	  mode="flattenSchemaSpec"/>
    </xsl:copy>
  </xsl:template>


<!-- **************************************************** -->
  <xsl:template match="/">
    <xsl:for-each select="exsl:node-set($ODD)">
      <xsl:apply-templates mode="iden"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tei:schemaSpec" mode="iden">

      <xsl:variable name="compiled">
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
    </xsl:variable>

    <xsl:for-each select="exsl:node-set($compiled)">
      <xsl:apply-templates mode="final"/>
    </xsl:for-each>

  </xsl:template>
  
  <xsl:template match="rng:ref" mode="final">
    <xsl:variable name="N">
      <xsl:value-of select="@name"/>
    </xsl:variable>
    <xsl:choose>
    <xsl:when test="starts-with($N,'macro.') and
		    $stripped='true'">
      <xsl:for-each select="key('MACROS',$N)/tei:content/*">
	<xsl:call-template name="simplifyRelax"/>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="starts-with($N,'data.')">
      <xsl:apply-templates 
	  select="key('MACROS',$N)/tei:content/*" 
	  mode="final"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="."/>      
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template
      match="tei:valDesc|tei:equiv|tei:gloss|tei:desc|tei:remarks|tei:exemplum|tei:listRef"
      mode="final">
    <xsl:choose>
      <xsl:when test="$stripped='true'">
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*" mode="final">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="final"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="tei:classSpec" mode="final">
    <xsl:variable name="k" select="@ident"/>
    <xsl:choose>
      <xsl:when test="key('REFED',$k) or $stripped='true'">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="final"/>
	</xsl:copy>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="$verbose='true'">
	  <xsl:message>reject <xsl:value-of select="$k"/> </xsl:message>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:macroSpec" mode="final">
    <xsl:variable name="k" select="@ident"/>
    <xsl:choose>
      <xsl:when test="$stripped='true' and
		      starts-with(@ident,'macro.')"/>
      <xsl:when test="starts-with(@ident,'data.')"/>
      <xsl:when test="key('REFED',$k)">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="final"/>
	</xsl:copy>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="$verbose='true'">
	  <xsl:message>reject <xsl:value-of select="$k"/>	</xsl:message>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
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
          <xsl:text>allbymod.xql?module=</xsl:text>
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
      <xsl:variable name="specName" select="@ident"/>
      <xsl:variable name="N" select="local-name(.)"/>
      <xsl:for-each select="exsl:node-set($ODD)">
        <xsl:choose>
          <xsl:when test="key('DELETE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: remove <xsl:value-of select="$specName"
                /></xsl:message>
            </xsl:if>
            <!--
	      <xsl:element name="{$N}" xmlns="http://www.tei-c.org/ns/1.0">
	      <xsl:attribute name="ident"><xsl:value-of select="$specName"/></xsl:attribute>
	      <xsl:attribute name="mode">delete</xsl:attribute>
	      </xsl:element>
	  -->
          </xsl:when>
          <xsl:when test="key('REPLACE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: replace <xsl:value-of select="$specName"
                /></xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="copy"
              select="key('REPLACE',$specName)"/>
          </xsl:when>
          <xsl:when test="key('CHANGE',$specName)">
            <xsl:if test="$verbose='true'">
              <xsl:message> Phase 3: change <xsl:value-of select="$specName"
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
    <xsl:for-each select="exsl:node-set($ODD)">
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
	<xsl:copy-of select="tei:equiv"/>
	<xsl:copy-of select="tei:gloss"/>
	<xsl:copy-of select="tei:desc"/>
      </xsl:if>
      <xsl:copy-of select="tei:classes"/>
      <xsl:apply-templates mode="copy" select="tei:content"/>
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
	    <xsl:copy-of
		select="tei:attList/tei:attDef[@mode='add' or not(@mode)]"/>
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
    <xsl:if test="not(@ns) or @ns='http://www.tei-c.org/ns/1.0'">
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
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- 
For each element, go through most of the sections one by one
and see if they are present in the change mode version.
If so, use them as is. Only the attributes are identifiable
for change individually.
 -->
      <xsl:for-each select="exsl:node-set($ODD)">
        <xsl:for-each select="key('CHANGE',$elementName)">
          <!-- if there is an altIdent, use it -->
	  <xsl:copy-of select="@ns"/>
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
	  <tei:classes>
	    <xsl:choose>
	      <xsl:when test="tei:classes[@mode='change']">
		<xsl:for-each select="tei:classes/tei:memberOf">
		  <xsl:choose>
		    <xsl:when test="@mode='delete'"/>
		    <xsl:when test="@mode='add' or not (@mode)">
		      <tei:memberOf key="{@key}"/>
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
		    <xsl:for-each select="exsl:node-set($ODD)">
		      <xsl:choose>
			<xsl:when test="key('DELETE',$me)">
			</xsl:when>
			<xsl:when test="key('MEMBEROFDELETE',$metoo)">
			</xsl:when>
			<xsl:when test="key('MEMBEROFADD',$metoo)">
			</xsl:when>
			<xsl:otherwise>
			  <tei:memberOf key="{$me}"/>
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
		    <xsl:for-each select="exsl:node-set($ODD)">
		      <xsl:if test="not(key('DELETE',@key))">
			<tei:memberOf key="{$me}"/>
		      </xsl:if>
		    </xsl:for-each>		
		  </xsl:for-each>
		</xsl:for-each>
	      </xsl:otherwise>
	    </xsl:choose>  
	  </tei:classes>
          <!-- element content -->
	  <tei:content>
	      <xsl:choose>
		<xsl:when test="tei:content/rng:*">
		  <xsl:apply-templates mode="copy" select="tei:content/*"/>
		</xsl:when>
		<xsl:when test="tei:content/tei:*">
		  <xsl:apply-templates mode="copy" select="tei:content/*"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:copy-of select="tei:content/s:*"/>
		  <xsl:for-each select="$ORIGINAL">
		    <xsl:apply-templates mode="copy" select="tei:content/*"/>
		  </xsl:for-each>
	      </xsl:otherwise>
	      </xsl:choose>
	  </tei:content>
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
          <xsl:choose>
            <xsl:when test="s:pattern">
              <xsl:copy-of select="s:pattern"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:for-each select="$ORIGINAL">
                <xsl:copy-of select="s:pattern"/>
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
      <xsl:for-each select="exsl:node-set($ODD)">
        <xsl:for-each select="key('CHANGE',$elementName)">
          <!-- if there is an altIdent, use it -->
          <xsl:copy-of select="tei:altIdent"/>
          <!-- equiv, gloss, desc trio -->
          <xsl:choose>
	    <xsl:when test="$stripped='true'"/>
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
      <xsl:apply-templates mode="change" select="@*"/>
      <!-- for each section of the class spec, 
     go through the sections one by one
     and see if they are present in the change mode version -->
      <xsl:for-each select="exsl:node-set($ODD)">
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
	  <tei:classes>
	    <xsl:choose>
	      <xsl:when test="tei:classes[@mode='change']">
		<xsl:for-each select="tei:classes/tei:memberOf">
		  <xsl:choose>
		    <xsl:when test="@mode='delete'"/>
		    <xsl:when test="@mode='add' or not (@mode)">
		      <tei:memberOf key="{@key}"/>
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
		    <xsl:for-each select="exsl:node-set($ODD)">
		      <xsl:choose>
			<xsl:when test="key('DELETE',$me)">
			</xsl:when>
			<xsl:when test="key('MEMBEROFDELETE',$metoo)">
			</xsl:when>
			<xsl:when test="key('MEMBEROFADD',$metoo)">
			</xsl:when>
			<xsl:otherwise>
			  <tei:memberOf key="{$me}"/>
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
		    <xsl:for-each select="exsl:node-set($ODD)">
		      <xsl:if test="not(key('DELETE',@key))">
			<tei:memberOf key="{$me}"/>
		      </xsl:if>
		    </xsl:for-each>		
		  </xsl:for-each>
		</xsl:for-each>
	      </xsl:otherwise>
	    </xsl:choose>
	  </tei:classes>
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
    match="rng:choice|rng:list|rng:group|rng:optional|rng:oneOrMore|rng:zeroOrMore"
    mode="copy">
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
	      <xsl:for-each select="exsl:node-set($ODD)">
		<xsl:choose>
		  <xsl:when test="$stripped='true'">
		    <ref name="{$N}"
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </xsl:when>
		  <xsl:when test="starts-with($N,'data.')">
		    <xsl:apply-templates
			select="key('MACROS',$N)/tei:content/*" mode="final"/>
		  </xsl:when>
		  <xsl:when test="key('DELETE',$N)"/>
		  <xsl:otherwise>
		    <ref name="{$N}" 
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
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
      <xsl:when test="$element='oneOrMore' and $entCount=1 and
		      local-name(exsl:node-set($contents)/WHAT/*)='zeroOrMore'">
        <oneOrMore
          xmlns="http://relaxng.org/ns/structure/1.0">
	  <xsl:copy-of
	      select="exsl:node-set($contents)/WHAT/rng:zeroOrMore/*"/>
	</oneOrMore>
      </xsl:when>
      <xsl:when
        test="self::rng:zeroOrMore/rng:ref/@name='model.global'
		and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name='model.global']"/>

      <xsl:when test="$entCount&gt;0 or $stripped='true'">
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
    <xsl:param name="whence"/>
    <!-- 
    On entry, we are sitting on an <elementSpec> or <classSpec> 
    and seeing if we can pick up some attributes for 
    $elementName. We travel to the ODD first
    to see if it has some overrides
    -->
    <xsl:for-each select="exsl:node-set($ODD)">
      <xsl:choose>
        <xsl:when test="$TEIC='false'"/>
	<xsl:when
	    test="key('MEMBEROFDELETE',concat($elementName,$className))">
	</xsl:when>
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
        <xsl:when test="not($localsource='')">
          <xsl:for-each select="document($localsource)/tei:TEI">
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
        <xsl:otherwise>
          <xsl:variable name="ATTCLASSDOC">
            <xsl:value-of select="$TEISERVER"/>
            <xsl:text>classspecs.xql</xsl:text>
          </xsl:variable>
          <xsl:for-each select="document($ATTCLASSDOC)/List">
            <xsl:for-each select="key('ATTCLASSES',$className)">
              <xsl:call-template name="processClassAttributes">
                <xsl:with-param name="elementName" select="$elementName"/>
                <xsl:with-param name="className" select="$className"/>
		<xsl:with-param name="whence" select="$whence"/>
		<xsl:with-param name="fromODD">false</xsl:with-param>
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
    <xsl:param name="fromODD"/>
    <xsl:param name="whence"/>
    <!-- we are sitting on a classSpec, could be in the ODD
	 or could be in the source -->
    <xsl:variable name="M" select="@module"/>
    <xsl:variable name="use">
      <xsl:choose>
        <xsl:when test="$fromODD='true' and @mode='add'">
          <xsl:text>true</xsl:text>
        </xsl:when>
        <xsl:when test="not(@module)">
          <xsl:text>true</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="exsl:node-set($ODD)">
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
	   e) the class has no replacement, but we need to check if its in a
	   module which has been loaded. if so, reference its
	   attributes
	   
	   In each case, once we have a potential attribute, we have to check
	   back to see if it is changed in the element (mergeClassAttribute)
      -->

      <!-- first, establish whether any attributes in classes,
	   inherited or otherwise, are changed in the ODD -->
	  <xsl:variable name="anyChanged">
	    <xsl:call-template name="checkClassAttribute">
	      <xsl:with-param name="element" select="$elementName"/>
	    </xsl:call-template>
	  </xsl:variable>

      <xsl:choose>
	<!-- a) new class in ODD -->
        <xsl:when test="$fromODD='true' and @mode='add'">
	    <tei:attRef n="1" name="{$className}.attributes"/>
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
	    <tei:attList>
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
	    </tei:attList>
	  </xsl:for-each>
	</xsl:when>

	<!-- d) there are changes to attributes in the class spec itself -->
	<xsl:when test="@mode='change' and tei:attList">
	  <!-- always references attributes in add mode -->
	  <xsl:for-each select="tei:attList/tei:attDef[@mode='add']">
	    <tei:attRef n="2" name="{$className}.attribute.{translate(@ident,':','')}"/>
	  </xsl:for-each>
	  <!-- go back to original and proceed from there -->
	  <xsl:choose>
	    <xsl:when test="not($localsource='')">
	      <xsl:for-each select="document($localsource)/tei:TEI">
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
	    <xsl:otherwise>
              <xsl:variable name="ATTCLASSDOC">
                <xsl:value-of select="$TEISERVER"/>
                <xsl:text>classspecs.xql</xsl:text>
              </xsl:variable>
              <xsl:for-each select="document($ATTCLASSDOC)/List">
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
            </xsl:otherwise>
          </xsl:choose>
          <xsl:for-each select="tei:attList/tei:attRef">
            <xsl:copy-of select="."/>
          </xsl:for-each>
        </xsl:when>

        <!-- e) its in the source. maybe do some merging -->
        <xsl:otherwise>
	  <!--
	      <xsl:message>!!<xsl:value-of select="$elementName"/> + <xsl:value-of
	      select="$className"/> + (fromODD: <xsl:value-of
	      select="$fromODD"/>) + (use: <xsl:value-of select="$use"/>) + (M: <xsl:value-of
	      select="$M"/>)</xsl:message>
	  -->
	  <xsl:choose>
	    <xsl:when test="contains($anyChanged,'true')">
	      <xsl:if test="$verbose">
		<xsl:message>Class <xsl:value-of 
		select="$className"/> for <xsl:value-of 
		select="$elementName"/> has changes in odd, refer by values</xsl:message>
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
		<tei:attList>
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
		</tei:attList>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:if test="$verbose">
		<xsl:message>Class <xsl:value-of 
		select="$className"/> for <xsl:value-of 
		select="$elementName"/> has no changes,  refer by name</xsl:message>
	      </xsl:if>
	      <tei:attRef n="4" name="{$className}.attributes"/>
	    </xsl:otherwise>
	  </xsl:choose>
	  
	</xsl:otherwise>
      </xsl:choose>
      
      <!-- Now attributes referenced from classes we are a member
	   of. Again, check whether we are in ODD or not
      -->

<!-- DEBUG
      <xsl:message>Now time to look at subclasses of <xsl:value-of
      select="@ident"/> whose changes status was <xsl:value-of
      select="$anyChanged"/>; we are in fromOdd <xsl:value-of
      select="$fromODD"/></xsl:message>
-->
      <xsl:choose>
	<xsl:when test="$fromODD='false' and
			not(contains($anyChanged,'true'))"/>
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
            <xsl:when test="not($localsource='')">
              <xsl:for-each select="document($localsource)/tei:TEI">
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
            <xsl:otherwise>
              <xsl:variable name="ATTCLASSDOC">
                <xsl:value-of select="$TEISERVER"/>
                <xsl:text>classspecs.xql</xsl:text>
              </xsl:variable>
              <xsl:for-each select="document($ATTCLASSDOC)/List">
                <xsl:for-each select="key('ATTCLASSES',$className)">
		  <xsl:for-each select="tei:classes/tei:memberOf">
		    <xsl:call-template name="classAttributes">
		      <xsl:with-param name="whence">6</xsl:with-param>
		      <xsl:with-param name="elementName" select="$elementName"/>
		      <xsl:with-param name="className" select="@key"/>
		    </xsl:call-template>
		  </xsl:for-each>
                </xsl:for-each>
              </xsl:for-each>
            </xsl:otherwise>
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
      <tei:attList>
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
      </tei:attList>
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
      <xsl:value-of
        select="concat($class,'.attribute.' ,translate($att,':',''),'_',$element)"
      />
    </xsl:variable>
    <xsl:variable name="lookingAt">
      <xsl:value-of select="concat($element,'_',@ident)"/>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($ODD)">
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
      <tei:attRef n="3-{$debug}" name="{$class}.attribute.{translate($att,':','')}"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

  <xsl:template name="checkClassAttribute">
    <xsl:param name="element"/>
    <!-- look at each attribute in turn, and then repeat for any
	 inherited classes -->
    <xsl:variable name="class" select="@ident"/>
    <xsl:variable name="all">
      <xsl:for-each select="tei:attList//tei:attDef">
	<xsl:variable name="return">
	  <xsl:variable name="att" select="@ident"/>
	  <xsl:for-each select="exsl:node-set($ODD)">
	    <xsl:choose>
	      <!-- deleted in the customization at the class level -->
	      <xsl:when test="key('DELETEATT',concat($class,'_',$att))">true</xsl:when>
	      <!-- deleted in the customization at the element level -->
	      <xsl:when test="key('DELETEATT',concat($element,'_',$att))">true</xsl:when>
	      <!-- replaced in the customization at the element level -->
	      <xsl:when test="key('REPLACEATT',concat($element,'_',$att))">true</xsl:when>
	      <!-- changed in the customization by the element -->
	      <xsl:when test="key('CHANGEATT',concat($element,'_',$att))">true</xsl:when>
	      <xsl:otherwise>false</xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</xsl:variable>
<!-- DEBUG
	<xsl:message> Return is <xsl:value-of select="$return"/> for
	<xsl:value-of
	    select="concat($element,'_',@ident)"/></xsl:message>
-->
	<xsl:value-of select="$return"/>	
      </xsl:for-each>
      <xsl:for-each select="tei:classes/tei:memberOf">
	<xsl:for-each select="key('ATTCLASSES',@key)">
	    <xsl:call-template name="checkClassAttribute">
	      <xsl:with-param name="element" select="$element"/>
	    </xsl:call-template>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$all"/>
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
            <xsl:for-each select="exsl:node-set($ODD)">
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
        <xsl:for-each select="exsl:node-set($ODD)">
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
	<xsl:for-each select="exsl:node-set($classMembership)/x/tei:memberOf">
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
	  <xsl:when test="$stripped='true'"/>
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
			  <xsl:when test="$stripped='true'"/>
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
	<xsl:if test="$stripped='false'">
	  <xsl:copy-of select="tei:equiv"/>
	  <xsl:copy-of select="tei:gloss"/>
	  <xsl:copy-of select="tei:desc"/>
	</xsl:if>
        <xsl:copy-of select="tei:classes"/>
        <xsl:apply-templates mode="copy" select="tei:content"/>
        <tei:attList>
          <xsl:comment>1.</xsl:comment>
          <xsl:call-template name="classAttributesSimple">
	    <xsl:with-param name="whence">9</xsl:with-param>
            <xsl:with-param name="elementName" select="$elementName"/>
            <xsl:with-param name="className" select="'att.global'"/>
          </xsl:call-template>
          <xsl:comment>2.</xsl:comment>
          <xsl:for-each select="tei:classes/tei:memberOf">
            <xsl:comment>3: <xsl:value-of select="@key"/></xsl:comment>
            <xsl:call-template name="classAttributesSimple">
	      <xsl:with-param name="whence">10</xsl:with-param>
              <xsl:with-param name="elementName" select="$elementName"/>
              <xsl:with-param name="className" select="@key"/>
            </xsl:call-template>
          </xsl:for-each>
          <xsl:comment>4.</xsl:comment>
          <xsl:apply-templates select="tei:attList"/>
          <xsl:comment>5.</xsl:comment>
        </tei:attList>
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
    <xsl:param name="whence"/>
    <xsl:comment>START on <xsl:value-of select="$className"/></xsl:comment>
    <xsl:for-each select="key('ATTCLASSES',$className)">
      <xsl:variable name="CURRENTCLASS" select="."/>
      <xsl:for-each select="tei:attList/tei:attDef">
        <xsl:call-template name="mergeClassAttribute">
          <xsl:with-param name="source">9</xsl:with-param>
          <xsl:with-param name="element" select="$elementName"/>
          <xsl:with-param name="class" select="$className"/>
          <xsl:with-param name="att" select="@ident"/>
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


  <xsl:template 
      match="@*|processing-instruction()|comment()|text()"
      mode="iden">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="iden">
    <xsl:copy>
      <xsl:apply-templates mode="iden"
        select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>

  </xsl:template>

  <xsl:template name="getversion">
    <xsl:choose>
      <xsl:when test="$TEIC='false'">
	<xsl:text>unknown</xsl:text>
      </xsl:when>
      <xsl:when test="not($localsource='')">
	<xsl:for-each
	    select="document($localsource)/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
	  <xsl:value-of select="."/>
	</xsl:for-each>
        </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="Remote">
          <xsl:value-of select="$TEISERVER"/>
          <xsl:text>getversion.xql</xsl:text>
        </xsl:variable>
        <xsl:for-each select="document($Remote)/tei:edition">
	  <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

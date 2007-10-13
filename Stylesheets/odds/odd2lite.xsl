<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
  exclude-result-prefixes="exsl estr edate fo a xd tei html rng local teix xs"
  extension-element-prefixes="edate exsl estr" version="1.0"
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
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="../common/tei.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for making TEI Lite XML from ODD </xd:short>
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
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:param name="startAttribute"/>
  <xsl:param name="endAttribute"/>
  <xsl:param name="startAttributeValue"/>
  <xsl:param name="endAttributeValue"/>
  <xsl:param name="startComment"/>
  <xsl:param name="endComment"/>
  <xsl:param name="startElement"/>
  <xsl:param name="endElement"/>
  <xsl:param name="startElementName"/>
  <xsl:param name="endElementName"/>
  <xsl:param name="startNamespace"/>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter"> </xsl:param>
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>
  <xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:variable name="top" select="/"/>
  <xsl:template match="tei:tag">
    <tei:code>
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </tei:code>
  </xsl:template>
  <xsl:template match="tei:attRef">
    <tei:label>
      <tei:ref target="#{@name}">
        <xsl:value-of select="@name"/>
      </tei:ref>
    </tei:label>
    <tei:item> </tei:item>
  </xsl:template>
  <xsl:template match="tei:att">
    <tei:hi rend="att">
      <xsl:apply-templates/>
    </tei:hi>
  </xsl:template>
  <xsl:template match="tei:val">
    <tei:hi rend="val">
      <xsl:apply-templates/>
    </tei:hi>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@key]">
    <tei:ptr target="#{@key}"/>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@url]">
    <tei:ptr target="{@url}"/>
  </xsl:template>
  <xsl:template match="tei:elementSpec">
    <xsl:choose>
      <xsl:when test="parent::tei:specGrp">
        <tei:label>
          <xsl:value-of select="@ident"/>
        </tei:label>
        <tei:item>
          <xsl:apply-templates mode="tangle" select="."/>
        </tei:item>
      </xsl:when>
      <xsl:otherwise> </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:classSpec">
    <xsl:if test="parent::tei:specGrp">
      <tei:label>
        <xsl:value-of select="@ident"/>
      </tei:label>
      <tei:item>
        <xsl:apply-templates mode="tangle" select="."/>
      </tei:item>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:macroSpec">
    <xsl:if test="parent::tei:specGrp">
      <tei:label>
        <xsl:value-of select="@ident"/>
      </tei:label>
      <tei:item>
        <xsl:apply-templates mode="tangle" select="."/>
      </tei:item>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:specGrpRef"/>
<!--
  <xsl:template match="tei:specGrpRef">
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
    <xsl:choose>
      <xsl:when test="parent::tei:specGrp">
        <tei:label/>
        <tei:item>
          <tei:emph>specification group <tei:ref target="#{$W}">
              <xsl:for-each select="key('IDS',$W)">
                <xsl:number level="any"/>
                <xsl:if test="@n">
                  <xsl:text>: </xsl:text>
                  <xsl:value-of select="@n"/>
                </xsl:if>
              </xsl:for-each>
            </tei:ref> appears here</tei:emph>
        </tei:item>
      </xsl:when>
      <xsl:when test="parent::tei:p">
        <xsl:text>« </xsl:text>
        <tei:emph>include <tei:ref target="#{$W}">
            <xsl:for-each select="key('IDS',$W)">
              <xsl:number level="any"/>
              <xsl:if test="@n">
                <xsl:text>: </xsl:text>
                <xsl:value-of select="@n"/>
              </xsl:if>
            </xsl:for-each>
          </tei:ref>
        </tei:emph>
        <xsl:text> » </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <tei:p>
          <xsl:text>« </xsl:text>
          <tei:emph>include <tei:ref target="#{$W}">
              <xsl:for-each select="key('IDS',$W)">
                <xsl:number level="any"/>
                <xsl:if test="@n">
                  <xsl:text>: </xsl:text>
                  <xsl:value-of select="@n"/>
                </xsl:if>
              </xsl:for-each>
            </tei:ref>
          </tei:emph>
          <xsl:text> » </xsl:text>
        </tei:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
-->

  <xsl:template
    match="rng:*|tei:*|@*|processing-instruction()|tei:author|tei:title">
    <xsl:copy>
      <xsl:apply-templates
       select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:specGrp/tei:p">
    <tei:label/>
    <tei:item>
      <xsl:apply-templates/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:altIdent"/>
  <xsl:template match="tei:attDef" mode="summary">
    <tei:label>
      <tei:code>
        <xsl:call-template name="identifyMe"/>
      </tei:code>
    </tei:label>
    <tei:item>
      <xsl:call-template name="makeDescription"/>
      <xsl:apply-templates select="tei:valList" mode="weave"/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:attDef">
    <tei:label>
      <tei:code>
        <xsl:call-template name="identifyMe"/>
      </tei:code>
    </tei:label>
    <tei:item>
      <xsl:call-template name="makeDescription"/>
      <xsl:apply-templates select="tei:valList" mode="weave"/>
      <xsl:apply-templates select="tei:exemplum" mode="weave"/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:attDef/tei:datatype" mode="weave">
    <tei:label>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Datatype</xsl:with-param>
        </xsl:call-template>
        <xsl:text>:</xsl:text>
      </tei:hi>
    </tei:label>
    <tei:item>
      <xsl:call-template name="Literal"/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:attList" mode="show">
    <xsl:call-template name="displayAttList">
      <xsl:with-param name="mode">summary</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:attList" mode="summary">
    <xsl:if test="tei:attDef or tei:attList/tei:attDef">
      <tei:list type="gloss">
        <xsl:apply-templates mode="summary"/>
      </tei:list>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:attList[@org='choice']">
    <xsl:choose>
      <xsl:when test="parent::tei:attList">
	<tei:label>Choice:</tei:label>
	<tei:item>
	  <tei:list type="gloss">
	    <xsl:apply-templates mode="summary"/>
	  </tei:list>
	</tei:item>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> Choice: </xsl:text>
	  <tei:list type="gloss">
	    <xsl:apply-templates mode="summary"/>
	  </tei:list>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:attList[@org='choice']" mode="summary">
    <xsl:choose>
      <xsl:when test="parent::tei:attList">
	<tei:label>Choice:</tei:label>
	<tei:item>
	  <tei:list type="gloss">
	    <xsl:apply-templates mode="summary"/>
	  </tei:list>
	</tei:item>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text> Choice: </xsl:text>
	  <tei:list type="gloss">
	    <xsl:apply-templates mode="summary"/>
	  </tei:list>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:equiv" mode="weave">
    <xsl:if test="@name">
      <tei:p>
        <xsl:text> Equivalent</xsl:text>
        <xsl:if test="@uri"> in \texttt{<xsl:value-of select="@uri"/>}</xsl:if>
        <xsl:text>: </xsl:text>
        <xsl:value-of select="@name"/>
      </tei:p>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:attList" mode="weave">
    <tei:p>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Attributes</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
      </tei:hi>
      <xsl:call-template name="displayAttList">
        <xsl:with-param name="mode">all</xsl:with-param>
      </xsl:call-template>
    </tei:p>
  </xsl:template>
  <xsl:template match="tei:body">
    <xsl:copy>
      <xsl:apply-templates
       select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="weavebody">
    <xsl:apply-templates mode="weave"/>
    <tei:p>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Class</xsl:with-param>
        </xsl:call-template>
      </tei:hi>
      <xsl:text>: </xsl:text>
      <xsl:call-template name="generateClassParents"/>
    </tei:p>
    <tei:p>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Members</xsl:with-param>
        </xsl:call-template>
      </tei:hi>
      <xsl:text>: </xsl:text>
      <xsl:call-template name="generateMembers"/>
    </tei:p>
    <xsl:call-template name="moduleInfo"/>
  </xsl:template>
  <xsl:template match="tei:classes" mode="weave">
    <xsl:if test="tei:memberOf">
      <tei:p>
        <tei:hi>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Class</xsl:with-param>
          </xsl:call-template>
        </tei:hi>
        <xsl:for-each select="tei:memberOf">
          <xsl:choose>
            <xsl:when test="key('IDENTS',@key)">
              <xsl:variable name="Key">
                <xsl:value-of select="@key"/>
              </xsl:variable>
              <xsl:for-each select="key('IDENTS',@key)">
                <xsl:text>: </xsl:text>
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name" select="@ident"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>: </xsl:text>
              <xsl:value-of select="@key"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </tei:p>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:defaultVal" mode="weave">
    <tei:label>
      <xsl:call-template name="i18n">
        <xsl:with-param name="word">Default</xsl:with-param>
      </xsl:call-template>
    </tei:label>
    <tei:item>
      <xsl:apply-templates/>
    </tei:item>
  </xsl:template>

  <xsl:template match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4">
    <tei:div>
      <xsl:apply-templates
       select="tei:*|rng:*|@*|processing-instruction()|comment()|text()"/>
    </tei:div>
  </xsl:template>
  <xsl:template match="tei:elementSpec" mode="weavebody">
    <xsl:if test="not(tei:attList)">
      <tei:p>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Attributes</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:call-template name="showAttClasses"/>
      </tei:p>
    </xsl:if>
    <xsl:apply-templates mode="weave"/>
    <xsl:call-template name="moduleInfo"/>
  </xsl:template>
  <xsl:template match="tei:elementSpec/tei:content" mode="weave">
    <tei:p>
      <xsl:call-template name="i18n">
        <xsl:with-param name="word">Declaration</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="bitOut">
        <xsl:with-param name="grammar"/>
        <xsl:with-param name="content">
          <Wrapper>
            <rng:element name="{../@ident}">
              <xsl:if test="not(ancestor::tei:schemaSpec/@ns)">
                <rng:ref name="att.global.attributes"/>
              </xsl:if>
              <xsl:for-each select="../tei:classes/tei:memberOf">
                <xsl:for-each select="key('IDENTS',@key)">
                  <xsl:if test="tei:attList">
                    <rng:ref name="{$patternPrefixText}{@ident}.attributes"/>
                  </xsl:if>
                </xsl:for-each>
              </xsl:for-each>
              <xsl:apply-templates mode="tangle" select="../tei:attList"/>
              <xsl:copy-of select="rng:*"/>
            </rng:element>
          </Wrapper>
        </xsl:with-param>
      </xsl:call-template>
    </tei:p>
  </xsl:template>
  <xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec" mode="show">
    <xsl:param name="atts"/>
    <xsl:choose>
      <xsl:when test="self::tei:elementSpec">
	<tei:hi>
	  <xsl:text>&lt;</xsl:text>
	  <xsl:call-template name="identifyMe"/>
	  <xsl:text>&gt; </xsl:text>
	</tei:hi>
      </xsl:when>
      <xsl:otherwise>
	<tei:hi>
	  <xsl:call-template name="identifyMe"/>
	</tei:hi>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="makeDescription"/>
    <xsl:choose>
      <xsl:when test="$atts='-'"/>
      <xsl:when test="self::tei:macroSpec"/>
      <xsl:when test="not(tei:attList/tei:attDef)"/>
      <xsl:when test="not($atts='')">
        <tei:list type="gloss">
          <xsl:variable name="HERE" select="."/>
          <xsl:for-each select="estr:tokenize(concat(' ',$atts,' '))">
            <xsl:variable name="TOKEN" select="."/>
            <xsl:choose>
              <xsl:when test="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
                <xsl:for-each
                 select="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
                  <xsl:call-template name="showAnAttribute"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$HERE/tei:classes/tei:memberOf">
                  <xsl:for-each
                   select="key('IDENTS',@key)/tei:attList//tei:attDef[@ident=$TOKEN]">
                    <xsl:call-template name="showAnAttribute"/>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </tei:list>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates mode="summary" select="tei:attList"/>
        <xsl:text>(</xsl:text>
        <xsl:call-template name="showAttClasses"/>
        <xsl:text>)</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="showAnAttribute">
    <tei:label>
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </tei:label>
    <tei:item>
      <xsl:call-template name="makeDescription"/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:equiv"/>
  <xsl:template match="tei:exemplum" mode="doc">
    <xsl:choose>
      <xsl:when test="parent::tei:attDef">
	<tei:list type="gloss">
	  <tei:label>
	    <tei:hi>
	      <xsl:call-template name="i18n">
		<xsl:with-param name="word">Example</xsl:with-param>
	      </xsl:call-template>
	    </tei:hi>
	  </tei:label>
	  <tei:item>
	    <xsl:apply-templates/>
	  </tei:item>
	</tei:list>
      </xsl:when>
      <xsl:otherwise>
	<tei:p>
	  <tei:hi>
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Example</xsl:with-param>
	    </xsl:call-template>
	  </tei:hi>
	</tei:p>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:macroSpec" mode="weavebody">
    <xsl:apply-templates mode="weave"/>
    <xsl:call-template name="moduleInfo"/>
  </xsl:template>
  <xsl:template match="tei:macroSpec/tei:content" mode="weave">
    <tei:p>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Declaration</xsl:with-param>
        </xsl:call-template>
      </tei:hi>
      <xsl:call-template name="bitOut">
        <xsl:with-param name="grammar">true</xsl:with-param>
        <xsl:with-param name="content">
          <Wrapper>
            <rng:define name="{../@ident}">
              <xsl:if test="starts-with(.,'component')">
                <xsl:attribute name="combine">choice</xsl:attribute>
              </xsl:if>
              <xsl:copy-of select="rng:*"/>
            </rng:define>
          </Wrapper>
        </xsl:with-param>
      </xsl:call-template>
    </tei:p>
  </xsl:template>
  <xsl:template match="tei:moduleSpec">
    <xsl:choose>
      <xsl:when test="parent::tei:p"><xsl:call-template name="i18n">
          <xsl:with-param name="word">Module</xsl:with-param>
        </xsl:call-template><xsl:text> </xsl:text><tei:hi>
          <xsl:value-of select="@ident"/>
        </tei:hi>
	<xsl:text>: </xsl:text>
	<xsl:call-template name="makeDescription"/>
      </xsl:when>
      <xsl:otherwise>
        <tei:p><xsl:call-template name="i18n">
            <xsl:with-param name="word">Module</xsl:with-param>
          </xsl:call-template><xsl:text> </xsl:text><tei:hi>
            <xsl:value-of select="@ident"/>
          </tei:hi>
	  <xsl:text>: </xsl:text>
	  <xsl:call-template name="makeDescription"/>
	</tei:p>
      </xsl:otherwise>
    </xsl:choose>
    <tei:list>
      <tei:item>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Elements defined</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:for-each select="key('ElementModule',@ident)">
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="name" select="@ident"/>
          </xsl:call-template>
          <xsl:text>: </xsl:text>
        </xsl:for-each>
      </tei:item>
      <tei:item>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Classes defined</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:for-each select="key('ClassModule',@ident)">
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="name" select="@ident"/>
          </xsl:call-template>
          <xsl:text>: </xsl:text>
        </xsl:for-each>
      </tei:item>
      <tei:item>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Macros defined</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:for-each select="key('MacroModule',@ident)">
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="name" select="@ident"/>
          </xsl:call-template>
          <xsl:text>: </xsl:text>
        </xsl:for-each>
      </tei:item>
    </tei:list>
  </xsl:template>
  <xsl:template match="tei:p">
    <tei:p>
      <xsl:apply-templates/>
    </tei:p>
  </xsl:template>
  <xsl:template match="tei:remarks/tei:p">
      <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="tei:remarks" mode="doc">
    <xsl:if test="string-length(.)&gt;0">
      <tei:p>
	<tei:hi>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Note</xsl:with-param>
	  </xsl:call-template>
	</tei:hi>
	<xsl:text>: </xsl:text>
      <xsl:apply-templates/>
      </tei:p>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:specDesc">
    <tei:item>
      <xsl:call-template name="processSpecDesc"/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:specList">
    <tei:list rend="specList">
      <xsl:apply-templates/>
    </tei:list>
  </xsl:template>
  <xsl:template match="tei:valDesc" mode="weave">
    <tei:label>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Values</xsl:with-param>
        </xsl:call-template>
      </tei:hi>
    </tei:label>
    <tei:item>
      <xsl:apply-templates/>
    </tei:item>
  </xsl:template>
  <xsl:template match="tei:valList" mode="contents">
    <xsl:text> </xsl:text>
    <xsl:choose>
      <xsl:when test="@type='semi'">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Suggested values include</xsl:with-param>
        </xsl:call-template>
        <xsl:text>:</xsl:text>
      </xsl:when>
      <xsl:when test="@type='open'">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Sample values include</xsl:with-param>
        </xsl:call-template>
        <xsl:text>:</xsl:text>
      </xsl:when>
      <xsl:when test="@type='closed'">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Legal values are</xsl:with-param>
        </xsl:call-template>
        <xsl:text>:</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Values are</xsl:with-param>
        </xsl:call-template>
        <xsl:text>:</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <tei:list type="gloss">
      <xsl:for-each select="tei:valItem">
        <tei:label>
          <xsl:call-template name="identifyMe"/>
        </tei:label>
        <tei:item>
	  <xsl:call-template name="makeDescription"/>
        </tei:item>
      </xsl:for-each>
    </tei:list>
  </xsl:template>

  <xsl:template match="tei:valList" mode="weave">
    <xsl:apply-templates mode="contents" select="."/>
  </xsl:template>

  <xsl:template match="teix:egXML">
    <xsl:call-template name="verbatim">
      <xsl:with-param name="label">
        <xsl:if test="not(parent::tei:exemplum)">
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Example</xsl:with-param>
          </xsl:call-template>
          <xsl:text> </xsl:text>
          <xsl:call-template name="compositeNumber"/>
        </xsl:if>
      </xsl:with-param>
      <xsl:with-param name="text">
        <xsl:apply-templates mode="verbatim"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="moduleInfo">
    <tei:p>
      <tei:hi>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Module</xsl:with-param>
        </xsl:call-template>
      </tei:hi>
      <xsl:text>: </xsl:text>
      <xsl:call-template name="makeTagsetInfo"/>
    </tei:p>
  </xsl:template>
  <xsl:template name="Literal">
    <tei:eg rend="literal">
      <xsl:apply-templates mode="verbatim"/>
    </tei:eg>
  </xsl:template>
  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">eg</xsl:param>
    <tei:q rend="eg">
      <xsl:choose>
        <xsl:when test="$displayMode='rng'">
          <xsl:for-each select="exsl:node-set($content)/Wrapper">
            <xsl:apply-templates mode="verbatim"/>
          </xsl:for-each>
        </xsl:when>
        <xsl:when test="$displayMode='rnc'">
          <xsl:call-template name="make-body-from-r-t-f">
            <xsl:with-param name="schema">
              <xsl:for-each select="exsl:node-set($content)/Wrapper">
                <xsl:call-template name="make-compact-schema"/>
              </xsl:for-each>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="exsl:node-set($content)/Wrapper">
            <xsl:apply-templates mode="verbatim"/>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </tei:q>
  </xsl:template>
  <xsl:template name="displayAttList">
    <xsl:param name="mode"/>
    <xsl:call-template name="showAttClasses"/>
    <tei:list type="gloss">
      <xsl:choose>
        <xsl:when test="$mode='all'">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="summary"/>
        </xsl:otherwise>
      </xsl:choose>
    </tei:list>
  </xsl:template>
  <xsl:template name="embolden">
    <xsl:param name="text"/>
    <tei:hi>
      <xsl:copy-of select="$text"/>
    </tei:hi>
  </xsl:template>
  <xsl:template name="identifyMe">
    <xsl:choose>
      <xsl:when test="tei:altIdent">
        <xsl:value-of select="tei:altIdent"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="italicize">
    <xsl:param name="text"/>
    <tei:hi>
      <xsl:copy-of select="$text"/>
    </tei:hi>
  </xsl:template>
  <xsl:template name="logoFramePicture"/>

  <xsl:template match="tei:specGrp"/>
<!--
  <xsl:template match="tei:specGrp">
    <xsl:variable name="text">
      <xsl:text>The following declarations constitute
      specification group</xsl:text>
      <xsl:number level="any"/>
      <xsl:text>: </xsl:text>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="parent::tei:p">
      <tei:list type="gloss">
	<xsl:copy-of select="@xml:id"/>
	<xsl:apply-templates/>
      </tei:list>
      </xsl:when>
      <xsl:otherwise>
	<tei:p>
	  <tei:list type="gloss">
	    <xsl:copy-of select="@xml:id"/>
	    <xsl:apply-templates/>
	  </tei:list>
	</tei:p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
-->

  <xsl:template name="makeAnchor">
    <xsl:param name="name"/>
  </xsl:template>
  <xsl:template name="makeLink">
    <xsl:param name="class"/>
    <xsl:param name="id"/>
    <xsl:param name="name"/>
    <xsl:param name="text"/>
    <tei:ref rend="{$class}" target="#{$name}">
      <xsl:copy-of select="$text"/>
    </tei:ref>
  </xsl:template>
  <xsl:template name="refdoc">
    <xsl:param name="name"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> refdoc for <xsl:value-of select="name(.)"/> - <xsl:value-of
         select="@ident"/>
      </xsl:message>
    </xsl:if>
    <tei:div>
      <xsl:attribute name="id" namespace="http://www.w3.org/XML/1998/namespace">
        <xsl:value-of select="@ident"/>
      </xsl:attribute>
      <tei:head>
	<xsl:choose>
	  <xsl:when test="self::tei:macroSpec">
	    <xsl:text>Macro </xsl:text>
	    <xsl:call-template name="identifyMe"/>
	  </xsl:when>
	  <xsl:when test="self::tei:classSpec">
	    <xsl:text>Class </xsl:text>
	    <xsl:call-template name="identifyMe"/>
	  </xsl:when>
	  <xsl:when test="self::tei:elementSpec">
	    <tei:gi>
	      <xsl:call-template name="identifyMe"/>
	    </tei:gi>	      
	  </xsl:when>
	</xsl:choose>
      </tei:head>
      <tei:p>
	<xsl:call-template name="makeDescription"/>
      </tei:p>
      <xsl:apply-templates mode="weavebody" select="."/>
    </tei:div>
  </xsl:template>
  <xsl:template name="teiStartHook"/>
  <xsl:template name="ttembolden">
    <xsl:param name="text"/>
    <tei:hi>
      <tei:code>
        <xsl:copy-of select="$text"/>
      </tei:code>
    </tei:hi>
  </xsl:template>
  <xsl:template name="typewriter">
    <xsl:param name="text"/>
    <tei:code>
      <xsl:copy-of select="$text"/>
    </tei:code>
  </xsl:template>
  <xsl:template name="verbatim">
    <xsl:param name="label"/>
    <xsl:param name="text"/>
    <xsl:param name="startnewline">false</xsl:param>
    <xsl:param name="autowrap">false</xsl:param>
    <tei:eg rend="verbatim">
      <xsl:if test="not($label='')">
        <xsl:attribute name="n">
          <xsl:value-of select="$label"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates mode="verbatim"/>
    </tei:eg>
  </xsl:template>
  <xsl:template name="makeInternalLink">
    <xsl:param name="ptr"/>
    <xsl:param name="target"/>
    <xsl:param name="dest"/>
    <xsl:param name="body"/>
    <xsl:variable name="W">
      <xsl:choose>
        <xsl:when test="$target">
          <xsl:value-of select="$target"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$dest"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not($body='')">
        <tei:ref target="#{$W}">
          <xsl:value-of select="$body"/>
        </tei:ref>
      </xsl:when>
      <xsl:when test="$ptr='true'">
        <tei:ptr target="#{$W}"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="makeExternalLink">
    <xsl:param name="ptr"/>
    <xsl:param name="dest"/>
    <xsl:choose>
      <xsl:when test="$ptr='true'">
        <tei:ptr target="{$dest}"/>
      </xsl:when>
      <xsl:otherwise>
        <tei:ref target="{$dest}">
          <xsl:apply-templates/>
        </tei:ref>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="generateEndLink">
    <xsl:param name="where"/>
    <xsl:value-of select="$where"/>
  </xsl:template>
  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <xsl:if test="tei:classSpec">
      <tei:div>
        <tei:head>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Classes defined</xsl:with-param>
          </xsl:call-template>
        </tei:head>
        <xsl:apply-templates mode="weave" select="tei:classSpec">
          <xsl:sort select="tei:altIdent"/>
          <xsl:sort select="@ident"/>
        </xsl:apply-templates>
      </tei:div>
    </xsl:if>
    <tei:div>
      <tei:head>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Elements defined</xsl:with-param>
        </xsl:call-template>
      </tei:head>
      <xsl:apply-templates mode="weave" select="tei:elementSpec">
        <xsl:sort select="tei:altIdent"/>
        <xsl:sort select="@ident"/>
      </xsl:apply-templates>
    </tei:div>
    <xsl:if test="tei:macroSpec">
      <tei:div>
        <tei:head>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Macros defined</xsl:with-param>
          </xsl:call-template>
        </tei:head>
        <xsl:apply-templates mode="weave" select="tei:macroSpec">
          <xsl:sort select="tei:altIdent"/>
          <xsl:sort select="@ident"/>
        </xsl:apply-templates>
      </tei:div>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
    <xsl:call-template name="processSchemaFragment"/>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='attclasscat']">
    <xsl:apply-templates mode="weave" select="key('ATTCLASSDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='modelclasscat']">
    <xsl:apply-templates mode="weave" select="key('MODELCLASSDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='macrocat']">
    <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='elementcat']">
    <xsl:apply-templates mode="weave" select="key('ELEMENTDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>

</xsl:stylesheet>

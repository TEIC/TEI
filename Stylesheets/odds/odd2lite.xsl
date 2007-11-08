<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" 
	xmlns:edate="http://exslt.org/dates-and-times" 
	xmlns:estr="http://exslt.org/strings" 
	xmlns:exsl="http://exslt.org/common" 
	xmlns:fo="http://www.w3.org/1999/XSL/Format" 
	xmlns:html="http://www.w3.org/1999/xhtml" 
	xmlns:local="http://www.pantor.com/ns/local" 
	xmlns:rng="http://relaxng.org/ns/structure/1.0" 
	xmlns:tei="http://www.tei-c.org/ns/1.0" 
	xmlns:teix="http://www.tei-c.org/ns/Examples" 
	xmlns:xd="http://www.pnp-software.com/XSLTdoc" 
	xmlns:xs="http://www.w3.org/2001/XMLSchema" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="exsl estr edate fo a xd tei html rng local teix xs" extension-element-prefixes="edate exsl estr" version="1.0">
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
    <xsl:text>
</xsl:text>
  </xsl:template>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>
  <xsl:variable name="uc">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="lc">abcdefghijklmnopqrstuvwxyz</xsl:variable>
  <xsl:param name="displayMode">rnc</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:variable name="top" select="/"/>
  <xsl:template match="@*|comment()|processing-instruction()">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*|teix:egXML|tei:author|tei:title">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="showSpace">
    <xsl:text> </xsl:text>
    <xsl:processing-instruction name="tex">\ </xsl:processing-instruction>
  </xsl:template>
  <!--
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

FOO

-->
  <xsl:param name="cellName">cell</xsl:param>
  <xsl:param name="xrefName">ref</xsl:param>
  <xsl:param name="urlName">target</xsl:param>
  <xsl:param name="ulName">list</xsl:param>
  <xsl:param name="codeName">code</xsl:param>
  <xsl:param name="colspan">cols</xsl:param>
  <xsl:param name="ddName">item</xsl:param>
  <xsl:param name="dtName">label</xsl:param>
  <xsl:param name="hiName">hi</xsl:param>
  <xsl:param name="itemName">item</xsl:param>
  <xsl:param name="rendName">rend</xsl:param>
  <xsl:param name="rowName">row</xsl:param>
  <xsl:param name="tableName">table</xsl:param>
  <xsl:param name="outputNS">http://www.tei-c.org/ns/1.0</xsl:param>

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
  
<xd:doc>
    <xd:short>[html] Document an element, macro, or class</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="refdoc">
    <xsl:if test="$verbose='true'">
      <xsl:message> refdoc for <xsl:value-of select="name(.)"/> - <xsl:value-of select="@ident"/>
      </xsl:message>
    </xsl:if>
    <xsl:variable name="objectname">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="name">
      <xsl:value-of select="$objectname"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="self::tei:classSpec and not(@ident='att.global') and         count(key('CLASSMEMBERS',@ident))=0">
        <xsl:if test="$verbose='true'">
          <xsl:message> class <xsl:value-of select="@ident"/> omitted as it has no members
      </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates mode="weavebody" select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="makeAnchor"/>
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="ttembolden">
    <xsl:param name="text"/>
    <hi>
      <code>
        <xsl:copy-of select="$text"/>
      </code>
    </hi>
  </xsl:template>
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="typewriter">
    <xsl:param name="text"/>
    <code>
      <xsl:copy-of select="$text"/>
    </code>
  </xsl:template>
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="embolden">
    <xsl:param name="text"/>
    <hi>
      <xsl:copy-of select="$text"/>
    </hi>
  </xsl:template>
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="italicize">
    <xsl:param name="text"/>
    <emph>
      <xsl:copy-of select="$text"/>
    </emph>
  </xsl:template>

<xd:doc>
    <xd:short>[odds] make a link</xd:short>
    <xd:param name="class">class</xd:param>
    <xd:param name="id">id</xd:param>
    <xd:param name="name">name</xd:param>
    <xd:param name="text">text</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="makeLink">
    <xsl:param name="class"/>
    <xsl:param name="name"/>
    <xsl:param name="text"/>
    <ref>
      <xsl:attribute name="target">
        <xsl:choose>
          <xsl:when test="$splitLevel=-1">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$name"/>
          </xsl:when>
          <xsl:when test="$STDOUT='true'">
            <xsl:for-each select="key('IDENTS',$name)">
              <xsl:call-template name="getSpecURL">
                <xsl:with-param name="name">
                  <xsl:value-of select="$name"/>
                </xsl:with-param>
                <xsl:with-param name="type">
                  <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>ref-</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:value-of select="$outputSuffix"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:copy-of select="$text"/>
    </ref>
  </xsl:template>

 <xsl:template name="makeSection">
   <xsl:param name="name"/>
   <xsl:param name="id"/>
   <xsl:param name="contents"/>
   <div>
     <xsl:attribute name="xml:id">
       <xsl:value-of select="$id"/>
     </xsl:attribute>
     <head>
       <xsl:value-of select="$name"/>
     </head>
     <xsl:copy-of select="$contents"/>
   </div>
 </xsl:template>

<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="grammar">grammar</xd:param>
    <xd:param name="content">content</xd:param>
    <xd:param name="element">element</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <eg rend="eg_rnc">
      <xsl:call-template name="make-body-from-r-t-f">
	<xsl:with-param name="schema">
	  <xsl:for-each select="exsl:node-set($content)/*">
	    <xsl:call-template name="make-compact-schema"/>
	  </xsl:for-each>
	</xsl:with-param>
      </xsl:call-template>
    </eg>
  </xsl:template>

  <!-- ............................ -->
  <xsl:template match="tei:attDef" mode="summary">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_label</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$codeName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>att</xsl:text>
          </xsl:attribute>
          <xsl:value-of select="$name"/>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_value</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="makeDescription"/>
        <xsl:apply-templates select="valList"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:attDef</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:attDef">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_label</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>att</xsl:text>
          </xsl:attribute>
          <xsl:value-of select="$name"/>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_value</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="makeDescription"/>
        <xsl:element namespace="{$outputNS}"  name="{$tableName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>attDef</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$rowName}">
            <xsl:element namespace="{$outputNS}"  name="{$cellName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>odd_label</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Status</xsl:with-param>
              </xsl:call-template>
              <xsl:text> </xsl:text>
            </xsl:element>
            <xsl:element namespace="{$outputNS}"  name="{$cellName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>odd_value</xsl:text>
              </xsl:attribute>
              <xsl:choose>
                <xsl:when test="@usage='mwa'">
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">Mandatory when applicable</xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:when test="@usage='opt'">
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">Optional</xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:when test="@usage='rec'">
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">Recommended</xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:when test="@usage='req'">
                  <hi>
                    <xsl:call-template name="i18n">
                      <xsl:with-param name="word">Required</xsl:with-param>
                    </xsl:call-template>
                  </hi>
                </xsl:when>
                <xsl:when test="@usage='rwa'">
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">Required when
		    applicable</xsl:with-param>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:call-template name="i18n">
                    <xsl:with-param name="word">Optional</xsl:with-param>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:element>
          </xsl:element>
          <xsl:apply-templates mode="weave"/>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:attDef/tei:datatype</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:attDef/tei:datatype" mode="weave">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_label</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Datatype</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_value</xsl:text>
        </xsl:attribute>
        <xsl:variable name="minOccurs">
          <xsl:choose>
            <xsl:when test="@minOccurs">
              <xsl:value-of select="@minOccurs"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="maxOccurs">
          <xsl:choose>
            <xsl:when test="@maxOccurs='unbounded'">
              <xsl:text>∞</xsl:text>
            </xsl:when>
            <xsl:when test="@maxOccurs">
              <xsl:value-of select="@maxOccurs"/>
            </xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:if test="$minOccurs != 1  or  $maxOccurs != 1">
          <xsl:text> </xsl:text>
          <xsl:value-of select="$minOccurs"/>
          <xsl:text>–</xsl:text>
          <xsl:value-of select="$maxOccurs"/>
          <xsl:text> occurrences of </xsl:text>
        </xsl:if>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar"/>
          <xsl:with-param name="content">
            <Wrapper>
              <xsl:copy-of select="rng:*"/>
            </Wrapper>
          </xsl:with-param>
          <xsl:with-param name="element">code</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="$minOccurs != 1  or  $maxOccurs != 1">
          <xsl:text>separated by whitespace</xsl:text>
        </xsl:if>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:attList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:attList" mode="show">
    <xsl:call-template name="displayAttList">
      <xsl:with-param name="mode">summary</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:attList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:attList" mode="weave"/>
  
<xd:doc>
    <xd:short>Process elements tei:classSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:classSpec">
    <xsl:if test="parent::tei:specGrp">
      <xsl:element namespace="{$outputNS}"  name="{$dtName}">
	<xsl:element namespace="{$outputNS}"  name="{$hiName}">
	  <xsl:attribute name="{$rendName}">
	    <xsl:text>label</xsl:text></xsl:attribute>
	    <xsl:call-template name="i18n">
	      <xsl:with-param  name="word">Class</xsl:with-param>
	      </xsl:call-template>
	      </xsl:element>:
      <xsl:value-of select="@ident"/></xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$ddName}">
        <xsl:apply-templates mode="tangle" select="."/>
        <xsl:text>(</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Members</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:call-template name="generateMembers"/>
        <xsl:text>)</xsl:text>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:classSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:classSpec" mode="weavebody">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:call-template name="makeSection">
      <xsl:with-param name="id">
          <xsl:value-of select="@ident"/>
      </xsl:with-param>
      <xsl:with-param name="name">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
      <xsl:with-param name="contents">

    <xsl:element namespace="{$outputNS}"  name="{$tableName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>wovenodd</xsl:text>
      </xsl:attribute>
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$colspan}">2</xsl:attribute>
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col2</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
            <xsl:value-of select="$name"/>
          </xsl:element>
          <xsl:text> </xsl:text>
          <xsl:call-template name="makeDescription"/>
          <xsl:if test="tei:listRef">
            <xsl:for-each select="tei:listRef/tei:ptr">
              <xsl:text> </xsl:text>
              <xsl:apply-templates select="." mode="weave"/>
            </xsl:for-each>
          </xsl:if>
        </xsl:element>
      </xsl:element>
      <xsl:if test="@generate">
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">
                <xsl:text>Classes defined</xsl:text>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:element>
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:value-of select="@generate"/>
          </xsl:element>
        </xsl:element>
      </xsl:if>
      <xsl:if test="@module">
        <xsl:call-template name="moduleInfo"/>
      </xsl:if>
      <xsl:if test="@type='model'">
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}"  name="{$hiName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>label</xsl:text>
              </xsl:attribute>
	      <xsl:call-template name="i18n">
		<xsl:with-param name="word">Used by</xsl:with-param>
	      </xsl:call-template>
            </xsl:element>
            <xsl:element namespace="{$outputNS}"  name="{$cellName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>wovenodd-col2</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="generateParents"/>
            </xsl:element>
          </xsl:element>
        </xsl:element>
      </xsl:if>
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col1</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Members</xsl:with-param>
            </xsl:call-template>
          </xsl:element>
        </xsl:element>
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col2</xsl:text>
          </xsl:attribute>
          <xsl:call-template name="generateMembers"/>
        </xsl:element>
      </xsl:element>
      <xsl:if test="@type='atts'">
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}"  name="{$hiName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>label</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Attributes</xsl:with-param>
              </xsl:call-template>
            </xsl:element>
          </xsl:element>
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:choose>
              <xsl:when test="not(tei:attList)">
                <xsl:call-template name="showAttClasses"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="tei:attList">
                  <xsl:call-template name="displayAttList">
                    <xsl:with-param name="mode">all</xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:element>
        </xsl:element>
      </xsl:if>
      <xsl:apply-templates mode="weave"/>
    </xsl:element>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:classes</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:classes" mode="weave">
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:defaultVal</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:defaultVal" mode="weave"/>
  
<xd:doc>
    <xd:short>Process elements tei:desc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:desc">
    <xsl:apply-templates/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:elementSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:elementSpec">
    <xsl:if test="parent::tei:specGrp">
      <xsl:element namespace="{$outputNS}"  name="{$dtName}">
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Element</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@ident"/>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$ddName}">
        <xsl:apply-templates mode="tangle" select="."/>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:elementSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:elementSpec" mode="weavebody">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="tei:content/rng:empty">
        <xsl:text>/</xsl:text>
      </xsl:if>
    </xsl:variable>

    <xsl:call-template name="makeSection">
      <xsl:with-param name="id">
	<xsl:value-of select="@ident"/>
      </xsl:with-param>
      <xsl:with-param name="name">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
      <xsl:with-param name="contents">
	

    <xsl:element namespace="{$outputNS}"  name="{$tableName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>wovenodd</xsl:text>
      </xsl:attribute>
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$colspan}">2</xsl:attribute>
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col2</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
            <xsl:text>&lt;</xsl:text>
            <xsl:value-of select="$name"/>
            <xsl:if test="tei:content/rng:empty">
              <xsl:text>/</xsl:text>
            </xsl:if>
            <xsl:text>&gt; </xsl:text>
          </xsl:element>
          <xsl:call-template name="makeDescription"/>
          <xsl:if test="tei:listRef">
            <xsl:for-each select="tei:listRef/tei:ptr">
              <xsl:text> </xsl:text>
              <xsl:apply-templates mode="weave" select="."/>
            </xsl:for-each>
          </xsl:if>
        </xsl:element>
      </xsl:element>
      <xsl:if test="@module">
        <xsl:call-template name="moduleInfo"/>
      </xsl:if>
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col1</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
	      <xsl:call-template name="i18n">
		<xsl:with-param name="word">Used by</xsl:with-param>
	      </xsl:call-template>
          </xsl:element>
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="generateParents"/>
          </xsl:element>
        </xsl:element>
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}"  name="{$hiName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>label</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Attributes</xsl:with-param>
              </xsl:call-template>
            </xsl:element>
          </xsl:element>
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:choose>
              <xsl:when test="not(tei:attList)">
                <xsl:call-template name="showAttClasses"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="tei:attList">
                  <xsl:call-template name="displayAttList">
                    <xsl:with-param name="mode">all</xsl:with-param>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:element>
        </xsl:element>
        <xsl:apply-templates mode="weave"/>
      </xsl:element>
    </xsl:element>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:elementSpec/tei:content</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:elementSpec/tei:content" mode="weave">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="../tei:altIdent">
          <xsl:value-of select="../tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="../@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col1</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$hiName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>label</xsl:text>
          </xsl:attribute>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Declaration</xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col2</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar"/>
          <xsl:with-param name="content">
            <Wrapper>
              <rng:element name="{$name}">
                <xsl:if test="not(ancestor::tei:schemaSpec)">
                  <rng:ref name="att.global.attributes"/>
                  <xsl:for-each select="..">
                    <xsl:call-template name="showClassAtts"/>
                  </xsl:for-each>
                </xsl:if>
                <xsl:apply-templates mode="tangle" select="../tei:attList"/>
                <xsl:copy-of select="rng:*"/>
              </rng:element>
            </Wrapper>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template name="showClassAtts">
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('IDENTS',@key)">
        <xsl:if test="tei:attList">
          <rng:ref name="{@ident}.attributes"/>
        </xsl:if>
        <xsl:call-template name="showClassAtts"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process the specification elements elements, classes and macros</xd:short>
    <xd:param name="atts">attributes we have been asked to display</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec" mode="show">
    <xsl:param name="atts"/>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>specList-</xsl:text>
	<xsl:value-of select="local-name(.)"/>
      </xsl:attribute>
      <xsl:element name="{$xrefName}">
        <xsl:attribute name="{$urlName}">
          <xsl:choose>
            <xsl:when test="$splitLevel=-1">
              <xsl:text>#</xsl:text>
              <xsl:value-of select="$name"/>
            </xsl:when>
            <xsl:when test="$STDOUT='true'">
              <xsl:for-each select="key('IDENTS',$name)">
                <xsl:call-template name="getSpecURL">
                  <xsl:with-param name="name">
                    <xsl:value-of select="$name"/>
                  </xsl:with-param>
                  <xsl:with-param name="type">
                    <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>ref-</xsl:text>
              <xsl:value-of select="$name"/>
              <xsl:text>.html</xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
        <xsl:value-of select="$name"/>
        <xsl:for-each select="key('IDENTS',$name)">
          <xsl:if test="tei:content/rng:empty">
            <xsl:text>/</xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:element>
    </xsl:element>
    <xsl:text> </xsl:text>
    <xsl:call-template name="makeDescription"/>
    <xsl:choose>
      <xsl:when test="$atts='-'"/>
      <xsl:when test="$atts='+'">
        <xsl:call-template name="showAttClasses">
          <xsl:with-param name="minimal">true</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$atts=''"/>
      <xsl:when test="string-length($atts)&gt;0">
        <xsl:element namespace="{$outputNS}"  name="{$tableName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>specDesc</xsl:text>
          </xsl:attribute>
          <xsl:variable name="HERE" select="."/>
          <xsl:call-template name="splitAttTokens">
            <xsl:with-param name="HERE" select="$HERE"/>
            <xsl:with-param name="atts" select="$atts"/>
          </xsl:call-template>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="tei:attList//tei:attDef">
          <xsl:element namespace="{$outputNS}"  name="{$tableName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>attList</xsl:text>
            </xsl:attribute>
            <xsl:apply-templates mode="summary" select="tei:attList//tei:attDef"/>
          </xsl:element>
        </xsl:if>
        <xsl:call-template name="showAttClasses">
          <xsl:with-param name="minimal">true</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Show a selected attribute</xd:short>
    <xd:param name="HERE">the starting node </xd:param>
    <xd:param name="TOKEN">attribute we have been asked to display</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="doAnAttToken">
    <xsl:param name="HERE"/>
    <xsl:param name="TOKEN"/>
    <xsl:choose>
      <xsl:when test="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
        <xsl:for-each select="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
          <xsl:call-template name="showAnAttribute"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$HERE/tei:classes/tei:memberOf">
          <xsl:for-each select="key('IDENTS',@key)/tei:attList//tei:attDef[@ident=$TOKEN]">
            <xsl:call-template name="showAnAttribute"/>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Split up and process a space-separated list of attribute names</xd:short>
    <xd:param name="HERE">the starting node </xd:param>
    <xd:param name="TOKEN">attributes we have been asked to display</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="splitAttTokens">
    <xsl:param name="HERE"/>
    <xsl:param name="atts"/>
    <xsl:choose>
      <xsl:when test="contains($atts,' ')">
        <xsl:call-template name="doAnAttToken">
          <xsl:with-param name="HERE" select="$HERE"/>
          <xsl:with-param name="TOKEN" select="substring-before($atts,' ')"/>
        </xsl:call-template>
        <xsl:call-template name="splitAttTokens">
          <xsl:with-param name="HERE" select="$HERE"/>
          <xsl:with-param name="atts" select="substring-after($atts,' ')"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="doAnAttToken">
          <xsl:with-param name="HERE" select="$HERE"/>
          <xsl:with-param name="TOKEN" select="$atts"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Display of an attribute</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="showAnAttribute">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>Attribute</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$hiName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>att</xsl:text>
          </xsl:attribute>
          <xsl:choose>
            <xsl:when test="tei:altIdent">
              <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@ident"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:call-template name="makeDescription"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:exemplum</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:exemplum" mode="doc">
    <xsl:choose>
      <xsl:when test="parent::tei:attDef">
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$colspan}">
              <xsl:text>2</xsl:text>
            </xsl:attribute>
            <xsl:apply-templates/>
          </xsl:element>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}"  name="{$hiName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>label</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">Example</xsl:with-param>
              </xsl:call-template>
            </xsl:element>
          </xsl:element>
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:apply-templates/>
          </xsl:element>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item">
    <xsl:choose>
      <xsl:when test="parent::tei:list[@type='gloss']">
        <xsl:element namespace="{$outputNS}"  name="{$ddName}">
	  <xsl:apply-templates/>
	</xsl:element>
      </xsl:when>
      <xsl:when test="parent::tei:list[@type='elementlist']">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element namespace="{$outputNS}"  name="{$itemName}">
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:macroSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:macroSpec">
    <xsl:if test="parent::tei:specGrp">
      <xsl:element namespace="{$outputNS}"  name="{$dtName}">
        <xsl:value-of select="@ident"/>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$ddName}">
        <xsl:apply-templates mode="tangle" select="."/>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:macroSpec in weavebody mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:macroSpec" mode="weavebody">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="tei:altIdent"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:call-template name="makeSection">
      <xsl:with-param name="id">
          <xsl:value-of select="@ident"/>
      </xsl:with-param>
      <xsl:with-param name="name">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
      <xsl:with-param name="contents">

    <xsl:element namespace="{$outputNS}"  name="{$tableName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>wovenodd</xsl:text>
      </xsl:attribute>
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$colspan}">2</xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
            <xsl:value-of select="$name"/>
          </xsl:element>
          <xsl:text> </xsl:text>
          <xsl:call-template name="makeDescription"/>
          <xsl:if test="tei:listRef">
            <xsl:for-each select="tei:listRef/tei:ptr">
              <xsl:text> </xsl:text>
              <xsl:apply-templates select="." mode="weave"/>
            </xsl:for-each>
          </xsl:if>
        </xsl:element>
      </xsl:element>
      <xsl:if test="@module">
        <xsl:call-template name="moduleInfo"/>
      </xsl:if>
      <xsl:if test="@type='pe'">
        <xsl:element namespace="{$outputNS}"  name="{$rowName}">
          <xsl:element namespace="{$outputNS}"  name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}"  name="{$hiName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>label</xsl:text>
              </xsl:attribute>
            </xsl:element>
            <xsl:element namespace="{$outputNS}"  name="{$cellName}">
              <xsl:attribute name="{$rendName}">
                <xsl:text>wovenodd-col2</xsl:text>
              </xsl:attribute>
              <xsl:call-template name="generateParents"/>
            </xsl:element>
          </xsl:element>
        </xsl:element>
      </xsl:if>
      <xsl:apply-templates mode="weave"/>
    </xsl:element>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:macroSpec/tei:content</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:macroSpec/tei:content" mode="weave">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col1</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$hiName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>label</xsl:text>
          </xsl:attribute>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Declaration</xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col2</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar">true</xsl:with-param>
          <xsl:with-param name="content">
            <Wrapper>
              <xsl:variable name="entCont">
                <Stuff>
                  <xsl:apply-templates select="rng:*"/>
                </Stuff>
              </xsl:variable>
              <xsl:variable name="entCount">
                <xsl:for-each select="exsl:node-set($entCont)/html:Stuff">
                  <xsl:value-of select="count(*)"/>
                </xsl:for-each>
              </xsl:variable>
              <xsl:choose>
                <xsl:when test=".=&quot;TEI.singleBase&quot;"/>
                <xsl:otherwise>
                  <rng:define name="{../@ident}">
                    <xsl:if test="starts-with(.,'component')">
                      <xsl:attribute name="combine">choice</xsl:attribute>
                    </xsl:if>
                    <xsl:copy-of select="rng:*"/>
                  </rng:define>
                </xsl:otherwise>
              </xsl:choose>
            </Wrapper>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:moduleSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:moduleSpec">
    <div class="moduleSpec">
      <xsl:element namespace="{$outputNS}"  name="{$hiName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>moduleSpecHead</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Module</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@ident"/>
        <xsl:text>: </xsl:text>
        <xsl:call-template name="makeDescription"/>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$ulName}">
        <xsl:if test="key('ElementModule',@ident)">
          <xsl:element namespace="{$outputNS}"  name="{$itemName}">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Elements defined</xsl:with-param>
            </xsl:call-template>
            <xsl:text>: </xsl:text>
            <xsl:for-each select="key('ElementModule',@ident)">
              <xsl:sort select="@ident"/>
              <xsl:call-template name="linkTogether">
                <xsl:with-param name="name" select="@ident"/>
              </xsl:call-template>
              <xsl:text> </xsl:text>
            </xsl:for-each>
          </xsl:element>
        </xsl:if>
        <xsl:if test="key('ClassModule',@ident)">
          <xsl:element namespace="{$outputNS}"  name="{$itemName}">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Classes defined</xsl:with-param>
            </xsl:call-template>
            <xsl:text>: </xsl:text>
            <xsl:for-each select="key('ClassModule',@ident)">
              <xsl:sort select="@ident"/>
              <xsl:call-template name="linkTogether">
                <xsl:with-param name="name" select="@ident"/>
              </xsl:call-template>
              <xsl:text> </xsl:text>
            </xsl:for-each>
          </xsl:element>
        </xsl:if>
        <xsl:if test="key('MacroModule',@ident)">
          <xsl:element namespace="{$outputNS}"  name="{$itemName}">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Macros defined</xsl:with-param>
            </xsl:call-template>
            <xsl:text>: </xsl:text>
            <xsl:for-each select="key('MacroModule',@ident)">
              <xsl:sort select="@ident"/>
              <xsl:call-template name="linkTogether">
                <xsl:with-param name="name" select="@ident"/>
              </xsl:call-template>
              <xsl:text> </xsl:text>
            </xsl:for-each>
          </xsl:element>
        </xsl:if>
      </xsl:element>
    </div>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process tei:remarks</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:remarks" mode="doc">
    <xsl:if test="string-length(.)&gt;0">
      <xsl:element namespace="{$outputNS}"  name="{$rowName}">
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col1</xsl:text>
          </xsl:attribute>
          <xsl:element namespace="{$outputNS}"  name="{$hiName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>label</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">Note</xsl:with-param>
            </xsl:call-template>
          </xsl:element>
        </xsl:element>
        <xsl:element namespace="{$outputNS}"  name="{$cellName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>wovenodd-col2</xsl:text>
          </xsl:attribute>
          <xsl:comment> </xsl:comment>
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:specDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:specDesc">
    <xsl:element namespace="{$outputNS}"  name="{$itemName}">
      <xsl:call-template name="processSpecDesc"/>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:specGrp</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:specGrp"/>
  
<xd:doc>
    <xd:short>Process elements tei:specGrpRef</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:specGrpRef"/>
  
<xd:doc>
    <xd:short>Process elements tei:specList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:specList">
    <xsl:element namespace="{$outputNS}"  name="{$ulName}">
      <xsl:attribute name="{$rendName}">specList</xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:valDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:valDesc" mode="weave">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_label</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Values</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>attribute</xsl:text>
        </xsl:attribute>
        <xsl:apply-templates/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:val</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:val">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>val</xsl:text>
      </xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:att</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:att">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>att</xsl:text>
      </xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:tag</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:tag">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>tag</xsl:text>
      </xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements tei:valList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:valList" mode="contents">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_label</xsl:text>
        </xsl:attribute>
        <xsl:choose>
          <xsl:when test="@type='semi'"><xsl:call-template name="i18n"><xsl:with-param name="word">Suggested values include</xsl:with-param></xsl:call-template>:</xsl:when>
          <xsl:when test="@type='open'"><xsl:call-template name="i18n"><xsl:with-param name="word">Sample values include</xsl:with-param></xsl:call-template>:</xsl:when>
          <xsl:when test="@type='closed'"><xsl:call-template name="i18n"><xsl:with-param name="word">Legal values are</xsl:with-param></xsl:call-template>:</xsl:when>
          <xsl:otherwise>Values are:</xsl:otherwise>
        </xsl:choose>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>odd_value</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$tableName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>valList</xsl:text>
          </xsl:attribute>
          <xsl:for-each select="tei:valItem">
            <xsl:variable name="name">
              <xsl:choose>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="tei:altIdent"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>
            <xsl:element namespace="{$outputNS}"  name="{$rowName}">
              <xsl:element namespace="{$outputNS}"  name="{$cellName}">
                <xsl:attribute name="{$rendName}">
                  <xsl:text>odd_label</xsl:text>
                </xsl:attribute>
                <xsl:value-of select="$name"/>
              </xsl:element>
              <xsl:element namespace="{$outputNS}"  name="{$cellName}">
                <xsl:attribute name="{$rendName}">
                  <xsl:text>odd_value</xsl:text>
                </xsl:attribute>
                <xsl:call-template name="makeDescription"/>
                <xsl:if test="@ident=../../tei:defaultVal">
                  <xsl:element namespace="{$outputNS}"  name="{$hiName}">
                    <xsl:attribute name="{$rendName}">
                      <xsl:text>defaultVal</xsl:text>
                    </xsl:attribute>
                    <xsl:text> [</xsl:text>
                    <xsl:call-template name="i18n">
                      <xsl:with-param name="word">Default</xsl:with-param>
                    </xsl:call-template>
                    <xsl:text>]</xsl:text>
                  </xsl:element>
                </xsl:if>
              </xsl:element>
            </xsl:element>
          </xsl:for-each>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="moduleInfo">
    <xsl:element namespace="{$outputNS}"  name="{$rowName}">
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col1</xsl:text>
        </xsl:attribute>
        <xsl:element namespace="{$outputNS}"  name="{$hiName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>label</xsl:text>
          </xsl:attribute>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Module</xsl:with-param>
          </xsl:call-template>
        </xsl:element>
      </xsl:element>
      <xsl:element namespace="{$outputNS}"  name="{$cellName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>wovenodd-col2</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="makeTagsetInfo"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  
  
<xd:doc>
    <xd:short>[odds] </xd:short>
    <xd:param name="mode">mode</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="displayAttList">
    <xsl:param name="mode"/>
    <xsl:call-template name="showAttClasses"/>
    <xsl:if test=".//tei:attDef">
      <xsl:element namespace="{$outputNS}"  name="{$tableName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>attList</xsl:text>
        </xsl:attribute>
        <xsl:choose>
          <xsl:when test="$mode='all'">
            <xsl:apply-templates/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="summary"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  
  <!-- pretty printing of RNC -->
  <xsl:template match="nc" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_nc</xsl:text>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
  <xsl:template match="declaration" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_decl</xsl:text>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
  <xsl:template match="prefix" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_prefix</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="param" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_param</xsl:text>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>
  <xsl:template match="op" mode="keep">
    <xsl:value-of select="translate (., ' ', ' ')"/>
  </xsl:template>
  <xsl:template match="atom" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_atom</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="t" mode="keep">
    <xsl:choose>
      <xsl:when test=". = '[' or . = ']'">
        <xsl:element namespace="{$outputNS}"  name="{$hiName}">
          <xsl:attribute name="{$rendName}">
            <xsl:text>rnc_annot</xsl:text>
          </xsl:attribute>
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="doc" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_comment</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="annot" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_annot</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="type" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_type</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="keyword" mode="keep">
    <xsl:element namespace="{$outputNS}"  name="{$hiName}">
      <xsl:attribute name="{$rendName}">
        <xsl:text>rnc_keyword</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>

  <xsl:template name="generateParents">
      <xsl:element name="{$hiName}">
	<xsl:attribute name="{$rendName}">parent</xsl:attribute>
	<xsl:call-template name="generateParentsByElement"/>
	<xsl:call-template name="generateParentsByMacro"/>
	<xsl:call-template name="generateParentsByClass"/>
      </xsl:element>
  </xsl:template>

  <xsl:template name="generateMembers">
    <xsl:param name="depth">1</xsl:param>
    <xsl:param name="me"></xsl:param>
    <xsl:variable name="this" select="@ident"/>
    <xsl:choose>
      <xsl:when test="$this=$me"/>
      <xsl:when test="key('CLASSMEMBERS',$this)">
      <xsl:element name="{$hiName}">
	<xsl:attribute name="{$rendName}">
	  <xsl:text>showmembers</xsl:text>
	  <xsl:value-of select="$depth"/>
	</xsl:attribute>
	  <xsl:if test="$depth &gt; 1"> [</xsl:if>
	  <xsl:for-each select="key('CLASSMEMBERS',$this)">
	    <xsl:sort select="local-name()"/>
	    <xsl:sort select="@ident"/>
	    <xsl:call-template name="showSpace"/>
	    <xsl:variable name="cl">
	      <xsl:choose>
		<xsl:when test="self::tei:elementSpec">
		  <xsl:text>link_odd_element</xsl:text>
		</xsl:when>
		<xsl:when test="self::tei:classSpec">
		  <xsl:text>link_odd_class</xsl:text>
		</xsl:when>
	      </xsl:choose>
	    </xsl:variable>
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	      <xsl:with-param     name="class">
		<xsl:value-of select="$cl"/>
	      </xsl:with-param>
	    </xsl:call-template>
	    <xsl:call-template name="generateMembers">
	      <xsl:with-param name="depth">
		<xsl:value-of select="$depth + 1"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:for-each>
	  <xsl:if test="$depth &gt; 1">] </xsl:if>
      </xsl:element>
      </xsl:when>
      <xsl:when test="$lookupDatabase='true'">
        <xsl:choose>
          <xsl:when test="not($localsource='')">
            <xsl:for-each select="document($localsource)/tei:TEI">
              <xsl:for-each
                select="tei:elementSpec[tei:classes/tei:memberOf[@key=$this]]">
                <xsl:call-template name="showElement">
                  <xsl:with-param name="name" select="@ident"/>
                </xsl:call-template>
                <xsl:text> </xsl:text>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="address">
              <xsl:value-of select="$TEISERVER"/>
              <xsl:text>classmembers.xq?class=</xsl:text>
              <xsl:value-of select="@ident"/>
            </xsl:variable>
            <xsl:if test="$verbose='true'">
              <xsl:message>Accessing TEISERVER: <xsl:value-of select="$address"
                /></xsl:message>
            </xsl:if>
            <xsl:for-each select="document($address)/list/item">
              <xsl:call-template name="showElement">
                <xsl:with-param name="name" select="."/>
              </xsl:call-template>
              <xsl:if test="following::item">
                <xsl:text> &#10;</xsl:text>
              </xsl:if>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="generateParentsByElement">
    <xsl:variable name="this" select="@ident"/>
    <xsl:for-each
	select="key('REFS',$this)/ancestor::tei:elementSpec">
      <xsl:sort select="@ident"/>
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	  <xsl:with-param name="class">link_odd_element</xsl:with-param>
	</xsl:call-template>
      <xsl:text> </xsl:text>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="generateParentsByMacro">
    <xsl:variable name="this" select="@ident"/>
    <xsl:if test="key('MACROREFS',$this)">
      <xsl:for-each
	  select="key('MACROREFS',$this)/ancestor::tei:macroSpec">
	<xsl:sort select="@ident"/>
      <xsl:text>  </xsl:text>
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	  <xsl:with-param name="class">link_odd_macro</xsl:with-param>
	</xsl:call-template>
	<xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:if>
<!--
      <xsl:for-each select="key('REFS',@ident)/ancestor::tei:elementSpec">
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:call-template name="generateParentsByMacro"/>
-->
  </xsl:template>

  <xsl:template name="generateParentsByClass">
    <xsl:variable name="this" select="@ident"/>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('CLASSES',@key)">
	<xsl:sort select="@ident"/>
	<xsl:if test="@type='model'">
	  <xsl:text> </xsl:text>
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	      <xsl:with-param name="class">link_odd_class</xsl:with-param>
	    </xsl:call-template>
	</xsl:if>
	<!--
	  <xsl:for-each select="key('REFS',@ident)/ancestor::tei:elementSpec">
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	    </xsl:call-template>
	  </xsl:for-each>
	    <xsl:call-template name="generateParentsByClass"/>
	    <xsl:call-template name="generateParentsByMacro"/>
	-->
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>


</xsl:stylesheet>

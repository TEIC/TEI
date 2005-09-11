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
    exclude-result-prefixes="exsl estr edate a fo local rng tei teix xd" 
    version="1.0">
  
<xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet dealing  with elements from the
      tagdocs module, making HTML output.
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
    <xd:short>Process elements  tei:attDef</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
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
    <tr>
      <td valign="top">
        <tt>
          <b>
            <xsl:value-of select="$name"/>
          </b>
        </tt>
      </td>
      <td colspan="2">
        <xsl:apply-templates select="tei:desc" mode="show"/>
      </td>
    </tr>
    <xsl:apply-templates select="valList"/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attDef</xd:short>
    <xd:detail>&#160;</xd:detail>
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
    <tr>
      <td valign="top">
        <tt>
          <b>
            <xsl:value-of select="$name"/>
          </b>
        </tt>
      </td>
      <td colspan="2">
        <xsl:apply-templates select="tei:desc" mode="show"/>
      </td>
    </tr>
    <tr>
      <td/>
      <td>
	<i>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Status</xsl:with-param>
	  </xsl:call-template>
	  <xsl:text>: </xsl:text>
	</i>
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
	    <xsl:call-template  name="i18n">
	      <xsl:with-param name="word">Required when applicable</xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	</xsl:choose>
      </td>
    </tr>
    <xsl:apply-templates/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attDef/tei:datatype</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:attDef/tei:datatype">
    <tr>
      <td/>
      <td colspan="2" valign="top">
        <i>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">Datatype</xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
	</i>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar"/>
          <xsl:with-param name="content">
            <Wrapper>
              <xsl:copy-of select="rng:*"/>
            </Wrapper>
          </xsl:with-param>
          <xsl:with-param name="element">code</xsl:with-param>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attDef/tei:exemplum</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:attDef/tei:exemplum">
    <tr>
      <td/>
      <td valign="top" colspan="2">
        <i>
	<xsl:call-template name="i18n">
	<xsl:with-param name="word">Example</xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
	</i>
        <xsl:call-template name="verbatim">
          <xsl:with-param name="text">
            <xsl:apply-templates/>
          </xsl:with-param>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attDef/tei:remarks</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:attDef/tei:remarks">
    <tr>
      <td/>
      <td>
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attList</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:attList" mode="show">
    <xsl:call-template name="displayAttList">
      <xsl:with-param name="mode">summary</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:attList</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:attList" mode="weave">
    <tr>
      <td valign="top">
        <i>
	<xsl:call-template name="i18n">
	<xsl:with-param name="word">Attributes</xsl:with-param>
	</xsl:call-template> 
	</i>
	<xsl:text> </xsl:text>
      </td>
      <td>
        <xsl:call-template name="displayAttList">
          <xsl:with-param name="mode">all</xsl:with-param>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:classSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:classSpec">
    <xsl:if test="parent::tei:specGrp">
      <dt>
	<i>
	  <xsl:call-template name="i18n"><xsl:with-param
					     name="word">Class</xsl:with-param></xsl:call-template>
	  </i>:
      <xsl:value-of select="@ident"/></dt>
      <dd>
        <xsl:apply-templates select="." mode="tangle"/>
      </dd>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:classSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
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
    <tr>
      <td valign="top">
        <tt>
          <b>
            <xsl:value-of select="$name"/>
          </b>
        </tt>
      </td>
      <td colspan="2">
        <xsl:apply-templates select="tei:desc" mode="show"/>
      </td>
    </tr>
    <xsl:apply-templates mode="weave"/>
<!--
  <tr>
    <td valign="top"><i>Member of classes</i></td>
    <td colspan="2">
      <xsl:call-template name="generateClassParents"/>
      &#160;
    </td>
  </tr>
-->
    <tr>
      <td valign="top">
        <i>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">Members</xsl:with-param>
	</xsl:call-template></i>
      </td>
      <td colspan="2">
        <xsl:call-template name="generateMembers"/>
      </td>
    </tr>
    <xsl:call-template name="HTMLmakeTagsetInfo"/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:classes</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:classes" mode="weave">
    <xsl:if test="tei:memberOf">
      <tr>
        <td valign="top">
          <i>
	  <xsl:call-template name="i18n"><xsl:with-param name="word">Class</xsl:with-param></xsl:call-template></i>
        </td>
        <td colspan="2">
          <xsl:for-each select="tei:memberOf">
            <xsl:choose>
              <xsl:when test="key('IDENTS',@key)">
                <xsl:variable name="Key">
                  <xsl:value-of select="@key"/>
                </xsl:variable>
                <xsl:for-each select="key('IDENTS',@key)">
                  <xsl:if test="not(generate-id(.)=generate-id(key('IDENTS',$Key)[1]))">
                    <xsl:text> |  </xsl:text>
                  </xsl:if>
                  <xsl:call-template name="linkTogether">
                    <xsl:with-param name="name" select="@ident"/>
                  </xsl:call-template>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name" select="@key"/>
                </xsl:call-template>
              </xsl:otherwise>
            </xsl:choose>
            <xsl:text> </xsl:text>
          </xsl:for-each>
        </td>
      </tr>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:defaultVal</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:defaultVal">
    <tr>
      <td/>
      <td valign="top" colspan="2">
        <i>
	<xsl:call-template name="i18n"><xsl:with-param name="word">Default</xsl:with-param></xsl:call-template> </i>
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:desc</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:desc" mode="weave"/>
  
<xd:doc>
    <xd:short>Process elements  tei:eg</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg">
    <xsl:call-template name="verbatim">
      <xsl:with-param name="autowrap">false</xsl:with-param>
      <xsl:with-param name="startnewline">
        <xsl:if test="parent::tei:exemplum">true</xsl:if>
      </xsl:with-param>
      <xsl:with-param name="text">
        <xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:elementSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:elementSpec">
    <xsl:if test="parent::tei:specGrp">
      <dt>
      <xsl:call-template name="i18n">
	<xsl:with-param name="word">Element</xsl:with-param>
      </xsl:call-template> 
      <xsl:text> </xsl:text>
      <xsl:value-of select="@ident"/></dt>
      <dd>
        <xsl:apply-templates select="." mode="tangle"/>
      </dd>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:elementSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
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
    </xsl:variable>
    <tr>
      <td valign="top">
        <tt>
          <b>
            <xsl:value-of select="$name"/>
          </b>
        </tt>
      </td>
      <td colspan="2">
        <xsl:apply-templates select="tei:desc" mode="show"/>
      </td>
    </tr>
    <xsl:if test="not(tei:attList)">
      <tr>
        <td valign="top">
          <i>
	  <xsl:call-template name="i18n"><xsl:with-param name="word">Attributes</xsl:with-param></xsl:call-template>: </i>
        </td>
        <td>
          <xsl:choose>
            <xsl:when test="count(tei:classes/tei:memberOf)&gt;0">
	      <xsl:call-template name="i18n">
	      <xsl:with-param name="word">Global attributes and those  inherited from</xsl:with-param>
	      </xsl:call-template>
              <xsl:text> </xsl:text>
              <xsl:for-each select="..">
                <xsl:call-template name="generateClassParents"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
	  
	      <xsl:call-template name="i18n"><xsl:with-param name="word">Global attributes only</xsl:with-param></xsl:call-template>
	</xsl:otherwise>
          </xsl:choose>
        </td>
      </tr>
    </xsl:if>
    <xsl:apply-templates mode="weave"/>
    <xsl:call-template name="HTMLmakeTagsetInfo"/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:elementSpec/tei:content</xd:short>
    <xd:detail>&#160;</xd:detail>
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
    <tr>
      <td valign="top">
        <i>
	<xsl:call-template name="i18n"><xsl:with-param name="word">Declaration</xsl:with-param></xsl:call-template></i>
      </td>
      <td colspan="2">
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar"/>
          <xsl:with-param name="content">
            <Wrapper>
              <rng:element name="{$name}">
		<xsl:if test="not(ancestor::tei:schemaSpec)">
		  <rng:ref name="tei.global.attributes"/>
		  <xsl:for-each select="../tei:classes/tei:memberOf">
		    <xsl:for-each select="key('IDENTS',@key)">
		      <xsl:if test="tei:attList">
			<rng:ref name="{@ident}.attributes"/>
		      </xsl:if>
		    </xsl:for-each>
		  </xsl:for-each>
		</xsl:if>
                <xsl:apply-templates select="../tei:attList" mode="tangle"/>
                <xsl:copy-of select="rng:*"/>
              </rng:element>
            </Wrapper>
          </xsl:with-param>
        </xsl:call-template>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process the specification elements  elements, classes
    and macros</xd:short>
    <xd:param name="atts">attributes we have been asked to display</xd:param>
    <xd:detail>&#160;</xd:detail>
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
    <b>&lt;<a>
      <xsl:attribute name="href">
	<xsl:choose>
	  <xsl:when test="$splitLevel=-1">
	    <xsl:text>#</xsl:text>
	    <xsl:value-of select="$name"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>ref-</xsl:text>
	    <xsl:value-of select="$name"/>
	    <xsl:text>.html</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="$name"/>
    </a>&gt; </b>
    <xsl:value-of select="tei:desc"/>
    <xsl:choose>
      <xsl:when test="not($atts='')">
	<table class="attList">
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
		  <xsl:for-each select="key('IDENTS',@key)/tei:attList//tei:attDef[@ident=$TOKEN]">
		    <xsl:call-template name="showAnAttribute"/>
		  </xsl:for-each>
		</xsl:for-each>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</table>
      </xsl:when>
      <xsl:when test="tei:attList//tei:attDef">
	<table class="attList">
	  <xsl:apply-templates select="tei:attList" mode="summary"/>
	</table>
	<xsl:if test="tei:classes/tei:memberOf">
	  <xsl:call-template name="showAttClasses"/>
	</xsl:if>
      </xsl:when>
      <xsl:when test="tei:classes/tei:memberOf">
	<xsl:call-template name="showAttClasses"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  

<xd:doc>
    <xd:short>Display of an attribute</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
<xsl:template name="showAnAttribute">
  <tr>
    <td valign="top">
      <b>
	<xsl:choose>
	  <xsl:when test="tei:altIdent">
	    <xsl:value-of select="tei:altIdent"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@ident"/>
	  </xsl:otherwise>
	</xsl:choose>
      </b>
    </td>
    <td colspan="2">
      <xsl:apply-templates select="tei:desc" mode="show"/>
    </td>
  </tr>
</xsl:template>

<xd:doc>
    <xd:short>Process elements  tei:exemplum</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:exemplum" mode="weave">
    <tr>
      <td valign="top">
        <i>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Example</xsl:with-param>
	  </xsl:call-template>
	</i>
      </td>
      <td colspan="2">
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:gloss</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:gloss" mode="weave"/>
  
<xd:doc>
    <xd:short>Process elements  tei:gloss</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:gloss"/>
  
<xd:doc>
    <xd:short>Process elements  tei:item</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:item">
    <xsl:choose>
      <xsl:when test="parent::tei:list[@type='gloss']"> 
     	<xsl:apply-templates/>
   </xsl:when>
      <xsl:when test="parent::tei:list[@type='elementlist']"> 
     	<xsl:apply-templates/>
   </xsl:when>
      <xsl:otherwise>
        <li>
          <xsl:apply-templates/>
        </li>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:macroSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:macroSpec">
    <xsl:if test="parent::tei:specGrp">
      <dt><xsl:value-of select="@ident"/></dt>
      <dd>
        <xsl:apply-templates select="." mode="tangle"/>
      </dd>
    </xsl:if>
  </xsl:template>
  

<xd:doc>
    <xd:short>Process elements  tei:macroSpec in weavebody mode</xd:short>
    <xd:detail>&#160;</xd:detail>
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
    <tr>
      <td valign="top">
        <tt>
          <b>
            <xsl:value-of select="$name"/>
          </b>
        </tt>
      </td>
      <td colspan="2">
        <xsl:apply-templates select="tei:desc" mode="show"/>
      </td>
    </tr>
    <xsl:apply-templates mode="weave"/>
    <xsl:call-template name="HTMLmakeTagsetInfo"/>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:macroSpec/tei:content</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:macroSpec/tei:content" mode="weave">
    <tr>
      <td valign="top">
        <i>
	<xsl:call-template name="i18n"><xsl:with-param name="word">Declaration</xsl:with-param></xsl:call-template></i>
      </td>
      <td colspan="2">
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
                <xsl:for-each select="exsl:node-set($entCont)/Stuff">
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
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:moduleSpec</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:moduleSpec">
    <hr/>
    <p>
      <strong>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">Module</xsl:with-param>
	</xsl:call-template>
      </strong>
      <xsl:text> </xsl:text>
      <em><xsl:value-of select="@ident"/></em>:
      <xsl:apply-templates select="tei:desc" mode="show"/>
      <ul>
	<li>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Elements defined</xsl:with-param>
	  </xsl:call-template>:
	  <xsl:for-each select="key('ElementModule',@ident)">
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	    </xsl:call-template>
	    <xsl:text> </xsl:text>
	  </xsl:for-each>
	</li>
	<li>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Classes defined</xsl:with-param>
	    </xsl:call-template>:
	  <xsl:for-each select="key('ClassModule',@ident)">
	    <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	    </xsl:call-template>
	    <xsl:text> </xsl:text>
	  </xsl:for-each>
      </li>
      <li>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">Macros defined</xsl:with-param>
	  </xsl:call-template>:
	  <xsl:for-each select="key('MacroModule',@ident)">
	  <xsl:call-template name="linkTogether">
	    <xsl:with-param name="name" select="@ident"/>
	  </xsl:call-template>
	  <xsl:text> </xsl:text>
	</xsl:for-each>
      </li>
      </ul>
      <hr/>
    </p>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:remarks</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:remarks" mode="weave">
    <xsl:if test="*//text()">
      <tr>
        <td valign="top">
          <i>
	  <xsl:call-template name="i18n"><xsl:with-param name="word">Note</xsl:with-param></xsl:call-template></i>
        </td>
        <td colspan="2">
          <xsl:apply-templates/>
        </td>
      </tr>
    </xsl:if>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:specDesc</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:specDesc">
    <li>
      <xsl:call-template name="processSpecDesc"/>
    </li>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:specGrp</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:specGrp">
    <div class="specgrp">
      <p><b>Specification group <xsl:number level="any"/>
      <xsl:if test="@n"><xsl:text>: </xsl:text><xsl:value-of select="@n"/></xsl:if>
    </b>
    <a name="{@xml:id}"/>
      </p>
      <dl>
	<xsl:apply-templates/>
      </dl>
    </div>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:specGrp/tei:p</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:specGrp/tei:p">
    <dt/>
    <dd>
      <xsl:apply-templates/>
    </dd>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:specGrpRef</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:specGrpRef">
    <xsl:choose>
      <xsl:when test="parent::tei:specGrp">
        <dt/>
        <dd>
	  <xsl:text>« </xsl:text>
          <a href="{@target}">
	    <i>
	      <xsl:call-template name="i18n"><xsl:with-param name="word">include</xsl:with-param></xsl:call-template> 
	      <xsl:text> </xsl:text>
	    <xsl:choose>
	      <xsl:when test="starts-with(@target,'#')">
		<xsl:for-each select="key('IDS',substring-after(@target,'#'))">
		  <xsl:number level="any"/>
		  <xsl:if test="@n">
		    <xsl:text>: </xsl:text><xsl:value-of select="@n"/>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@target"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    </i>
	  </a>
	  <xsl:text> » </xsl:text>
	</dd>
      </xsl:when>
      <xsl:otherwise>
        <p>
          <a href="{@target}">
	    <xsl:text>« </xsl:text>
	    <i><xsl:call-template name="i18n"><xsl:with-param  name="word">include</xsl:with-param></xsl:call-template>
	    <xsl:text> </xsl:text>
	    <xsl:choose>
	      <xsl:when test="starts-with(@target,'#')">
		<xsl:for-each select="key('IDS',substring-after(@target,'#'))">
		  <xsl:number level="any"/>
		  <xsl:if test="@n">
		    <xsl:text>: </xsl:text><xsl:value-of select="@n"/>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="@target"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    </i>
	  </a>
	  <xsl:text> » </xsl:text>
      </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:specList</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:specList">
    <ul class="specList">
      <xsl:apply-templates/>
    </ul>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:valDesc</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:valDesc">
    <tr>
      <td/>
      <td>
        <i>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">Values</xsl:with-param>
	  </xsl:call-template>
	  <xsl:text>: </xsl:text>
	</i>
	<xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:valList</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:valList" mode="contents">
    <xsl:choose>
      <xsl:when test="@type='semi'">
      <xsl:call-template name="i18n">
	<xsl:with-param name="word">Suggested values include</xsl:with-param>
      </xsl:call-template>:</xsl:when>
      <xsl:when test="@type='open'">
      <xsl:call-template name="i18n">
	<xsl:with-param name="word">Sample values include</xsl:with-param>
      </xsl:call-template>:</xsl:when>
      <xsl:when test="@type='closed'">
      <xsl:call-template name="i18n">
	<xsl:with-param name="word">Legal values are</xsl:with-param>
      </xsl:call-template>:</xsl:when>
      <xsl:otherwise>Values are:</xsl:otherwise>
    </xsl:choose>
    <table class="valList">
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
        <tr>
          <td valign="top">
            <b>
              <xsl:value-of select="$name"/>
            </b>
          </td>
          <td valign="top">
            <xsl:value-of select="tei:gloss"/>
          </td>
        </tr>
      </xsl:for-each>
    </table>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  tei:valList</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:valList">
    <xsl:choose>
      <xsl:when test="ancestor::tei:elementSpec or ancestor::tei:classSpec or ancestor::tei:macroSpec">
        <tr>
          <td/>
          <td valign="top">
            <xsl:apply-templates select="." mode="contents"/>
          </td>
        </tr>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="." mode="contents"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<xd:doc>
    <xd:short>Process elements  teix:egXML</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="teix:egXML">
    <pre>
      <xsl:apply-templates mode="verbatim"/>
    </pre>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="HTMLmakeTagsetInfo">
    <tr>
      <td valign="top">
        <i>
	<xsl:call-template name="i18n"><xsl:with-param name="word">Module</xsl:with-param></xsl:call-template></i>
      </td>
      <td colspan="2">
        <xsl:call-template name="makeTagsetInfo"/>
      </td>
    </tr>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="grammar">grammar</xd:param>
    <xd:param name="content">content</xd:param>
    <xd:param name="element">element</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="bitOut">
    <xsl:param name="grammar"/>
    <xsl:param name="content"/>
    <xsl:param name="element">pre</xsl:param>
    <xsl:element name="{$element}">
      <xsl:attribute name="class">eg</xsl:attribute>
      <xsl:choose>
        <xsl:when test="$displayMode='rng'">
          <xsl:apply-templates select="exsl:node-set($content)/Wrapper/*" mode="verbatim"/>
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
            <xsl:apply-templates mode="literal"/>
          </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="mode">mode</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="displayAttList">
    <xsl:param name="mode"/>
    <table class="attList">
      <tr>
        <td>
          <xsl:choose>
            <xsl:when test=".//tei:attDef">
              <xsl:choose>
                <xsl:when test="count(../tei:classes/tei:memberOf)&gt;0">
                  <xsl:text>(</xsl:text>
		  <xsl:call-template name="i18n">
		    <xsl:with-param name="word">In addition to global  attributes and those inherited
		    from</xsl:with-param>
		  </xsl:call-template>
		  <xsl:text> </xsl:text>
                  <xsl:for-each select="..">
                    <xsl:call-template name="generateClassParents"/>
                  </xsl:for-each>
                  <xsl:text>)</xsl:text>
                </xsl:when>
                <xsl:otherwise>
		<xsl:text> (</xsl:text>
		<xsl:call-template name="i18n">
		    <xsl:with-param name="word">In addition to global attributes</xsl:with-param></xsl:call-template>
		<xsl:text>)</xsl:text>
	      </xsl:otherwise>
              </xsl:choose>
              <table>
                <xsl:choose>
                  <xsl:when test="$mode='all'">
                    <xsl:apply-templates/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates mode="summary"/>
                  </xsl:otherwise>
                </xsl:choose>
              </table>
            </xsl:when>
            <xsl:otherwise>
              <xsl:choose>
                <xsl:when test="count(../tei:classes/tei:memberOf)&gt;0">
                  <xsl:call-template name="i18n">
		    <xsl:with-param name="word">Global attributes and those inherited from</xsl:with-param>
		  </xsl:call-template>
		  <xsl:text> </xsl:text>
                  <xsl:for-each select="..">
                    <xsl:call-template name="generateClassParents"/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
		  <xsl:call-template name="i18n"><xsl:with-param name="word">Global attributes only</xsl:with-param></xsl:call-template>
	      </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </td>
      </tr>
    </table>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="embolden">
    <xsl:param name="text"/>
    <b>
      <xsl:copy-of select="$text"/>
    </b>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="italicize">
    <xsl:param name="text"/>
    <i>
      <xsl:copy-of select="$text"/>
    </i>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] make a link</xd:short>
    <xd:param name="class">class</xd:param>
    <xd:param name="id">id</xd:param>
    <xd:param name="name">name</xd:param>
    <xd:param name="text">text</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="makeLink">
    <xsl:param name="class"/>
    <xsl:param name="name"/>
    <xsl:param name="text"/>
    <a class="{$class}">
      <xsl:attribute name="href">
	<xsl:choose>
	  <xsl:when test="$splitLevel=-1">
	    <xsl:text>#</xsl:text>
	    <xsl:value-of select="$name"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:text>ref-</xsl:text>
	    <xsl:value-of select="$name"/>
	    <xsl:text>.html</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
      <xsl:copy-of select="$text"/>
    </a>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] Document an element, macro, or class</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="refdoc">
  <xsl:if test="$verbose='true'">
    <xsl:message>   refdoc for <xsl:value-of select="name(.)"/> -  <xsl:value-of select="@ident"/>     </xsl:message>
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
    <xsl:choose>
      <xsl:when test="local-name(.)='elementSpec'">
	<xsl:text>&lt;</xsl:text>
	<xsl:value-of select="$objectname"/>
	<xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$objectname"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$splitLevel=-1">
	<h2><a name="{@ident}"/><xsl:value-of select="$name"/></h2>
         <table class="wovenodd" border="1">
	  <xsl:apply-templates select="." mode="weavebody"/>
	</table>

    </xsl:when>
    <xsl:otherwise>
      [<a href="ref-{@ident}.html">
      <xsl:value-of select="$name"/></a>]
      <xsl:variable name="BaseFile">
	<xsl:value-of select="$masterFile"/>
	<xsl:if test="ancestor::tei:teiCorpus">
	<xsl:text>-</xsl:text>
	<xsl:choose>
	  <xsl:when test="@xml:id">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:number/>
	  </xsl:otherwise>
	</xsl:choose>
	</xsl:if>
      </xsl:variable>
      <xsl:call-template name="outputChunk">
	<xsl:with-param name="ident">
	  <xsl:text>ref-</xsl:text>
	  <xsl:value-of select="@ident"/>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <html>
	    <xsl:comment>THIS IS A GENERATED FILE. DO NOT EDIT (7) </xsl:comment>
	    <head>
	      <title>
	      <xsl:value-of select="$name"/>
	      </title>
	      <xsl:if test="not($cssFile = '')">
		<link rel="stylesheet" type="text/css" href="{$cssFile}"/>
	      </xsl:if>
	    </head>
	    <body>
	      <xsl:call-template name="bodyHook"/>
	      <a name="TOP"/>
	      <div id="hdr">
		<xsl:call-template name="stdheader">
		  <xsl:with-param name="title">
		    <xsl:value-of select="$name"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </div>
	      <p>
		<a name="{@ident}"/>
		<table class="wovenodd" border="1">
		  <xsl:apply-templates select="." mode="weavebody"/>
		</table>
	      </p>
	    </body>
	  </html>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="ttembolden">
    <xsl:param name="text"/>
    <b>
      <tt>
        <xsl:copy-of select="$text"/>
      </tt>
    </b>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="typewriter">
    <xsl:param name="text"/>
    <tt>
      <xsl:copy-of select="$text"/>
    </tt>
  </xsl:template>
  
<xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="text">text</xd:param>
    <xd:param name="startnewline">startnewline</xd:param>
    <xd:param name="autowrap">autowrap</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="verbatim">
    <xsl:param name="text"/>
    <xsl:param name="startnewline">false</xsl:param>
    <xsl:param name="autowrap">true</xsl:param>
    <pre class="eg">
      <xsl:if test="$startnewline='true'">
        <xsl:text>
</xsl:text>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="$autowrap='false'">
          <xsl:value-of select="."/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:variable name="lines" select="estr:tokenize($text,'&#10;')"/>
          <xsl:apply-templates select="$lines[1]" mode="normalline"/>
        </xsl:otherwise>
      </xsl:choose>
    </pre>
  </xsl:template>
</xsl:stylesheet>

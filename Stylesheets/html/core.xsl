<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr edate a fo local rng tei teix xd"
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
    <xd:short> TEI stylesheet dealing with elements from the core module, making
      HTML output. </xd:short>
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
  <xd:doc>
    <xd:short>Process elements tei:*</xd:short>
    <xd:param name="forcedepth">forcedepth</xd:param>
    <xd:detail>
      <p> anything with a head can go in the TOC </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="maketoc">
    <xsl:param name="forcedepth"/>
    <xsl:variable name="myName">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:if test="tei:head or $autoHead='true'">
      <xsl:variable name="Depth">
        <xsl:choose>
          <xsl:when test="not($forcedepth='')">
            <xsl:value-of select="$forcedepth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tocDepth"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="thislevel">
        <xsl:choose>
          <xsl:when test="$myName = 'div'">
            <xsl:value-of select="count(ancestor::tei:div)"/>
          </xsl:when>
          <xsl:when test="starts-with($myName,'div')">
            <xsl:choose>
              <xsl:when test="ancestor-or-self::tei:div0">
                <xsl:value-of select="substring-after($myName,'div')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="substring-after($myName,'div') - 1"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>99</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="pointer">
        <xsl:apply-templates mode="generateLink" select="."/>
      </xsl:variable>
      <li class="toc">
        <xsl:call-template name="header">
          <xsl:with-param name="toc" select="$pointer"/>
          <xsl:with-param name="minimal">false</xsl:with-param>
        </xsl:call-template>
        <xsl:if test="$thislevel &lt; $Depth">
          <xsl:call-template name="continuedToc"/>
        </xsl:if>
      </li>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:ab</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ab">
    <div>
      <xsl:call-template name="rendToClass">
	<xsl:with-param name="default">ab</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:addrLine</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:addrLine">
    <xsl:apply-templates/>
    <br/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:address</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:address">
    <div class="address">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:analytic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:analytic">
    <xsl:apply-templates mode="biblStruct" select="tei:author"/>
    <i>
      <xsl:apply-templates mode="withbr" select="tei:title[not(@type='short')]"
      />
    </i>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:author</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:author" mode="biblStruct"><xsl:value-of
      select="tei:name/@reg"/><xsl:for-each select="name[position()&gt;1]">,
      <xsl:apply-templates/>
    </xsl:for-each>. <br/></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:author</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:author" mode="first">
    <xsl:value-of select="tei:name/@reg"/>
    <xsl:if test="name[position()&gt;1]">
      <xsl:text>(e.a.)</xsl:text>
    </xsl:if>
    <xsl:text>: </xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:bibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:bibl">
    <xsl:choose>
      <xsl:when test="parent::tei:cit">
        <div class="citbibl">
          <xsl:text>(</xsl:text>
          <xsl:apply-templates/>
          <xsl:text>)</xsl:text>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="ident">
          <xsl:apply-templates mode="ident" select="."/>
        </xsl:variable>
        <a name="{$ident}"/>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:biblScope</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:biblScope">
    <xsl:apply-templates/>
    <xsl:if test="ancestor::tei:biblStruct">. </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:biblStruct</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:biblStruct">
    <xsl:if test="@xml:id">
      <a name="{@xml:id}"/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@copyOf">
        <a class="biblink" href="{concat('#',substring(@copyOf,5,2))}">Zie
            <xsl:value-of select="substring(@copyOf,5,2)"/></a>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="descendant::tei:analytic">
            <br/>
            <xsl:apply-templates select="tei:analytic"/>
            <center>
              <table border="0" width="90%">
                <xsl:apply-templates mode="monograll" select="tei:monogr"/>
              </table>
            </center>
          </xsl:when>
          <xsl:otherwise>
            <br/>
            <xsl:apply-templates mode="monogrfirst" select="tei:monogr"/>
            <center>
              <table border="0" width="90%">
                <xsl:apply-templates mode="monogrrest" select="tei:monogr"/>
              </table>
            </center>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:byline</xd:short>
    <xd:detail>
      <p> </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:byline">
    <div class="byline">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:change</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:change">
    <tr>
      <td valign="top" width="15%">
        <xsl:value-of select="tei:date"/>
      </td>
      <td width="85%">
        <xsl:value-of select="tei:item"/>
      </td>
    </tr>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:cit</xd:short>
    <xd:detail>
      <p> quoting </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:cit">
    <xsl:choose>
      <xsl:when test="@rend='display'">
        <blockquote>

	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>cit</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>
          <p>
            <xsl:apply-templates select="tei:q|tei:quote"/>
            <xsl:apply-templates select="tei:bibl"/>
          </p>
        </blockquote>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:code</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:code">
    <tt>
      <xsl:apply-templates/>
    </tt>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:edition</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:edition"><xsl:apply-templates/>.<br/></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:editor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:editor" mode="first">
    <xsl:value-of select="tei:name/@reg"/>
    <xsl:text> (ed.)</xsl:text>
    <xsl:if test="tei:name[position()&gt;1]">
      <xsl:text> (e.a.)</xsl:text>
    </xsl:if>
    <xsl:text>: </xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:editor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:editor"><xsl:apply-templates
      select="tei:name[position()=1]"/><xsl:for-each
      select="tei:name[position()&gt;1]">, <xsl:apply-templates/>
    </xsl:for-each> (ed).<br/></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:eg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg">
    <div class="pre">
      <xsl:if test="$cssFile">
        <xsl:attribute name="class">pre_eg</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:emph</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:emph">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <em>
          <xsl:apply-templates/>
        </em>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:epigraph</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph">
    <div class="epigraph">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:epigraph/lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph/tei:lg">
    <table>
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:foreign</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:foreign">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gap</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gap"> [...]<xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gi</xd:short>
    <xd:detail>
      <p> special purpose </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi">
    <code>
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </code>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi" mode="plain">
    <xsl:text>&lt;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:head</xd:short>
    <xd:detail>
      <p> headings etc </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:head">
    <xsl:variable name="parent" select="local-name(..)"/>
    <xsl:if test="not(starts-with($parent,'div'))">
      <xsl:apply-templates/>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:head</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:head" mode="plain">
    <xsl:if test="preceding-sibling::tei:head">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:hi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:hi">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <strong>
          <xsl:apply-templates/>
        </strong>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:ident</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ident">
    <xsl:choose>
      <xsl:when test="@type">
        <span class="ident-{@type}">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <strong>
          <xsl:apply-templates/>
        </strong>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:imprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:imprint"><xsl:apply-templates select="tei:biblScope"
      /><xsl:apply-templates select="tei:pubPlace"/>, <xsl:apply-templates
      select="tei:date"/>. <xsl:apply-templates select="publisher"/></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="bibl">
    <p>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="glosstable">
    <tr>
      <td valign="top">
        <strong>
          <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"
          />
        </strong>
      </td>
      <td>
        <xsl:call-template name="makeAnchor"/>
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="gloss">
    <dt>
      <xsl:call-template name="makeAnchor"/>
        <xsl:apply-templates mode="print"
          select="preceding-sibling::tei:label[1]"/>
    </dt>
    <dd>
      <xsl:apply-templates/>
    </dd>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item">
    <li>
      <xsl:call-template name="rendToClass"/>
      <xsl:if test="@n">
        <xsl:attribute name="value">
          <xsl:value-of select="@n"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="@xml:id">
	  <xsl:choose>
	    <xsl:when test="$xhtml='true'">
	      <xsl:attribute name="id">
		<xsl:value-of select="@xml:id"/>
	      </xsl:attribute>
	    </xsl:when>
	    <xsl:otherwise>
	      <a name="{@xml:id}"/>
	    </xsl:otherwise>
	  </xsl:choose>
        </xsl:when>
        <xsl:when test="$generateParagraphIDs='true'">
	  <xsl:choose>
	    <xsl:when test="$xhtml='true'">
	      <xsl:attribute name="id">
		<xsl:value-of select="generate-id()"/>
	      </xsl:attribute>
	    </xsl:when>
	    <xsl:otherwise>
	      <a name="{generate-id()}"/>
	    </xsl:otherwise>
	  </xsl:choose>
        </xsl:when>
      </xsl:choose>
      <xsl:apply-templates/>
    </li>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="inline">
    <xsl:if test="preceding-sibling::tei:item">, </xsl:if>
    <xsl:if
      test="not(following-sibling::tei:item) and preceding-sibling::tei:item">
      and </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item/label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item/label">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <strong>
          <xsl:apply-templates/>
        </strong>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:kw</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:kw">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l" mode="Copying">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:l[@copyOf]|lg[@copyOf]</xd:short>
    <xd:detail>
      <p> copyOf handling </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l[@copyOf]|lg[@copyOf]">
    <xsl:variable name="W">
      <xsl:choose>
        <xsl:when test="starts-with(@copyof,'#')">
          <xsl:value-of select="substring-after(@copyof,'#')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@copyof"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:apply-templates mode="Copying" select="key('IDS',$W)"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label">
    <xsl:if test="@xml:id">
      <a name="{@xml:id}"/>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label" mode="print">
    <xsl:if test="@xml:id">
      <a name="{@xml:id}"/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lb">
    <br/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l">
    <div class="l">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg">
    <div class="lg">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lg/tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg/tei:l">
    <div>
      <xsl:attribute name="class">
        <xsl:choose>
          <xsl:when test="@rend='Alignr'">
            <xsl:text>right</xsl:text>
          </xsl:when>
          <xsl:when test="@rend='Alignc'">
            <xsl:text>center</xsl:text>
          </xsl:when>
          <xsl:when test="starts-with(@rend,'indent(')">
            <xsl:text>indent</xsl:text>
            <xsl:value-of
              select="concat(substring-before(substring-after(@rend,'('),')'),'em')"
            />
          </xsl:when>
          <xsl:when test="@rend='indent'">
            <xsl:text>indent1</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>left</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg" mode="Copying">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:list</xd:short>
    <xd:detail>
      <p>Lists. Depending on the value of the 'type' attribute, various HTML
        lists are generated: <dl>
          <dt>bibl</dt>
          <dd>Items are processed in mode 'bibl'</dd>
          <dt>catalogue</dt>
          <dd>A gloss list is created, inside a paragraph</dd>
          <dt>gloss</dt>
          <dd>A gloss list is created, expecting alternate label and item
            elements</dd>
          <dt>glosstable</dt>
          <dd>Label and item pairs are laid out in a two-column table</dd>
          <dt>inline</dt>
          <dd>A comma-separate inline list</dd>
          <dt>runin</dt>
          <dd>An inline list with bullets between items</dd>
          <dt>unordered</dt>
          <dd>A simple unordered list</dd>
          <dt>ordered</dt>
          <dd>A simple ordered list</dd>
          <dt>vallist</dt>
          <dd>(Identical to glosstable)</dd>
        </dl>
      </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list">
    <xsl:if test="tei:head">
      <p>
        <em>
          <xsl:apply-templates select="tei:head"/>
        </em>
      </p>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='catalogue'">
        <p>
          <dl>
            <xsl:call-template name="rendToClass"/>
            <xsl:for-each select="tei:item">
              <p/>
              <xsl:apply-templates mode="gloss" select="."/>
            </xsl:for-each>
          </dl>
        </p>
      </xsl:when>
      <xsl:when test="@type='gloss' and @rend='multicol'">
        <xsl:variable name="nitems">
          <xsl:value-of select="count(tei:item)div 2"/>
        </xsl:variable>
        <p>
          <table>
            <xsl:call-template name="rendToClass"/>
            <tr>
              <td valign="top">
                <dl>
                  <xsl:apply-templates mode="gloss"
                    select="tei:item[position()&lt;=$nitems ]"/>
                </dl>
              </td>
              <td valign="top">
                <dl>
                  <xsl:apply-templates mode="gloss"
                    select="tei:item[position() &gt;$nitems]"/>
                </dl>
              </td>
            </tr>
          </table>
        </p>
      </xsl:when>
      <xsl:when test="@type='gloss'">
        <dl>
          <xsl:call-template name="rendToClass"/>
          <xsl:apply-templates mode="gloss" select="tei:item"/>
        </dl>
      </xsl:when>
      <xsl:when test="@type='glosstable' or @type='vallist'">
        <table>
          <xsl:call-template name="rendToClass"/>
          <xsl:apply-templates mode="glosstable" select="tei:item"/>
        </table>
      </xsl:when>
      <xsl:when test="@type='inline'">
        <xsl:if test="not(item)">None</xsl:if>
        <xsl:apply-templates mode="inline" select="tei:item"/>
      </xsl:when>
      <xsl:when test="@type='runin'">
        <p>
          <xsl:apply-templates mode="runin" select="tei:item"/>
        </p>
      </xsl:when>
      <xsl:when test="@type='unordered' or @type='simple'">
        <ul>
          <xsl:call-template name="rendToClass"/>
          <xsl:apply-templates select="tei:item"/>
        </ul>
      </xsl:when>
      <xsl:when test="@type='bibl'">
        <xsl:apply-templates mode="bibl" select="tei:item"/>
      </xsl:when>
      <xsl:when test="starts-with(@type,'ordered')">
        <ol>
          <xsl:call-template name="rendToClass"/>
          <xsl:if test="starts-with(@type,'ordered:')">
            <xsl:attribute name="start">
              <xsl:value-of select="substring-after(@type,':')"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:call-template name="rendToClass"/>
          <xsl:apply-templates select="tei:item"/>
        </ol>
      </xsl:when>
      <xsl:otherwise>
        <ul>
          <xsl:call-template name="rendToClass"/>
          <xsl:apply-templates select="tei:item"/>
        </ul>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:list/tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list/tei:label"/>
  <xd:doc>
    <xd:short>Process elements tei:listBibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:listBibl">
    <ol>
      <xsl:for-each select="tei:bibl">
        <li>
          <xsl:apply-templates select="."/>
        </li>
      </xsl:for-each>
    </ol>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:mentioned</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:mentioned">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:monogr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:monogr" mode="monograll">
    <tr>
      <td>
        <xsl:choose>
          <xsl:when test="preceding-sibling::tei:monogr"> Also in: </xsl:when>
          <xsl:otherwise> In: </xsl:otherwise>
        </xsl:choose>
      </td>
    </tr>
    <tr>
      <td>
        <xsl:apply-templates mode="biblStruct" select="tei:author"/>
        <i>
          <xsl:apply-templates mode="withbr" select="tei:title"/>
        </i>
        <xsl:apply-templates select="tei:respStmt"/>
        <xsl:apply-templates select="tei:editor"/>
        <xsl:apply-templates select="tei:edition"/>
        <xsl:apply-templates select="tei:imprint"/>
      </td>
    </tr>
    <tr>
      <td>
        <xsl:apply-templates select="tei:biblScope"/>
      </td>
    </tr>
    <xsl:apply-templates select="following-sibling::tei:series"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:monogr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:monogr" mode="monogrfirst">
    <xsl:apply-templates mode="biblStruct" select="tei:author"/>
    <i>
      <xsl:apply-templates mode="withbr" select="tei:title[not(@type='short')]"
      />
    </i>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:monogr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:monogr" mode="monogrrest">
    <tr>
      <td>
        <xsl:apply-templates select="tei:respStmt"/>
        <xsl:apply-templates select="tei:editor"/>
        <xsl:apply-templates select="tei:edition"/>
        <xsl:apply-templates select="tei:imprint"/>
        <xsl:if test="child::tei:note">
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">Note</xsl:with-param>
          </xsl:call-template>
          <xsl:text>: </xsl:text>
          <xsl:apply-templates select="child::tei:note"/>
        </xsl:if>
      </td>
    </tr>
    <tr>
      <td>
        <xsl:apply-templates select="tei:biblScope"/>
      </td>
    </tr>
    <xsl:apply-templates select="following-sibling::tei:series"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:name</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:name" mode="plain">
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <a name="{$ident}"/>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note">
    <xsl:variable name="identifier">
      <xsl:text>Note</xsl:text>
      <xsl:call-template name="noteID"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="ancestor::tei:bibl"> (<xsl:apply-templates/>) </xsl:when>
      <xsl:when test="@place='inline'">
	<xsl:call-template name="addIdentification">
	  <xsl:with-param name="id" select="$identifier"/>
	</xsl:call-template>
        <xsl:text> (</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="@place='display'">
	<xsl:call-template name="addIdentification">
	  <xsl:with-param name="id" select="$identifier"/>
	</xsl:call-template>
        <blockquote>

	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>note</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>  
        <p>
            <xsl:apply-templates/>
          </p>
        </blockquote>
      </xsl:when>
      <xsl:when test="@place='foot' or @place='end'">
        <xsl:choose>
          <xsl:when test="$footnoteFile='true'">
            <a class="notelink" href="{$masterFile}-notes.html#{$identifier}">
              <sup>
                <xsl:call-template name="noteN"/>
              </sup>
            </a>
          </xsl:when>
          <xsl:otherwise>
            <a class="notelink" href="#{$identifier}">
              <sup>
                <xsl:call-template name="noteN"/>
              </sup>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="addIdentification">
	  <xsl:with-param name="id" select="$identifier"/>
	</xsl:call-template>
        <xsl:text> [</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">Note</xsl:with-param>
        </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:apply-templates/>
        <xsl:text>]</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note" mode="printnotes">
    <xsl:if test="not(ancestor::tei:bibl)">
      <xsl:variable name="identifier">
	<xsl:text>Note</xsl:text>
        <xsl:call-template name="noteID"/>
      </xsl:variable>
      <xsl:variable name="parent">
        <xsl:call-template name="locateParentdiv"/>
      </xsl:variable>
      <xsl:if test="$verbose='true'">
        <xsl:message>Note <xsl:value-of select="$identifier"/> with parent
            <xsl:value-of select="$parent"/></xsl:message>
      </xsl:if>
      <div class="note">
	  <xsl:call-template name="addIdentification">
	    <xsl:with-param name="id" select="$identifier"/>
	  </xsl:call-template>
	  <span class="noteLabel">
	    <xsl:call-template name="noteN"/>
	    <xsl:text>. </xsl:text>
	  </span>
	<span class="noteBody">
	  <xsl:apply-templates/>
	</span>
      </div>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:note[@type='action']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note[@type='action']">
    <div class="right"><b>Action <xsl:number count="tei:note[@type='action']"
          level="any"/></b>: <i>
        <xsl:apply-templates/>
      </i></div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:pb</xd:short>
    <xd:detail>Indication of a page break. For the purposes of HTML, we simply
      make it an anchor if it has an ID.</xd:detail>
  </xd:doc>
  <xsl:template match="tei:pb">
    <xsl:if test="@xml:id">
      <a name="{@xml:id}"/>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p">
    <xsl:variable name="wrapperElement">
      <xsl:choose>
        <xsl:when
          test="tei:specList|tei:moduleSpec|tei:list|tei:eg|teix:egXML|tei:table|tei:specGrp|tei:specGrpRef|tei:q[@rend='display']|tei:figure">
          <xsl:text>div</xsl:text>
        </xsl:when>
        <xsl:when test="parent::tei:p">
          <xsl:text>div</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>p</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:element name="{$wrapperElement}">
      <xsl:call-template name="rendToClass">
	<xsl:with-param name="default">
	  <xsl:if test="$wrapperElement='div'">p</xsl:if>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:choose>
        <xsl:when test="@xml:id and $xhtml='true'">
          <xsl:attribute name="id">
            <xsl:value-of select="@xml:id"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="@xml:id">
          <a name="{@xml:id}"/>
        </xsl:when>
        <xsl:when test="$generateParagraphIDs='true'">
          <xsl:choose>
            <xsl:when test="$xhtml='true'">
              <xsl:attribute name="id">
                <xsl:value-of select="generate-id()"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <a name="{generate-id()}"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
      </xsl:choose>
      <xsl:if test="$numberParagraphs='true'">
        <xsl:number/>
        <xsl:text> </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:p[@rend='box']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p[@rend='box']">
    <p class="box">
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:publisher</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:publisher"> (<xsl:apply-templates/>).</xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:q</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q">
    <xsl:choose>
      <xsl:when test="tei:p">
        <blockquote>
	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>q</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>
          <xsl:apply-templates/>
        </blockquote>
      </xsl:when>
      <xsl:when test="@rend='display'">
        <p class="blockquote">
          <xsl:apply-templates/>
        </p>
      </xsl:when>
      <xsl:when test="tei:text">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="tei:lg">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="pre">
          <xsl:choose>
            <xsl:when test="contains(@rend,'PRE')">
              <xsl:choose>
                <xsl:when test="contains(@rend,'POST')">
                  <xsl:call-template name="getQuote">
                    <xsl:with-param name="quote"
                      select="normalize-space(substring-before(substring-after(@rend,'PRE'),'POST'))"
                    />
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:call-template name="getQuote">
                    <xsl:with-param name="quote"
                      select="normalize-space(substring-after(@rend,'PRE'))"/>
                  </xsl:call-template>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$preQuote"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="post">
          <xsl:choose>
            <xsl:when test="contains(@rend,'POST')">
              <xsl:call-template name="getQuote">
                <xsl:with-param name="quote"
                  select="normalize-space(substring-after(@rend,'POST'))"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$postQuote"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="$pre"/>
        <xsl:apply-templates/>
        <xsl:value-of select="$post"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:q[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q[@rend='display']">
    <blockquote>

	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>q</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>
      <xsl:choose>
        <xsl:when test="tei:p">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:apply-templates/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </blockquote>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:q[@rend='eg']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q[@rend='eg']">
    <div class="pre">
      <xsl:if test="$cssFile">
        <xsl:attribute name="class">eg</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:quote</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:quote">
    <xsl:choose>
      <xsl:when test="parent::tei:cit">
        <div class="citquote">
          <xsl:apply-templates/>
        </div>
      </xsl:when>
      <xsl:when test="@rend='quoted'">
        <xsl:value-of select="$preQuote"/>
        <xsl:apply-templates/>
        <xsl:value-of select="$postQuote"/>
        <xsl:if test="following-sibling::tei:bibl">
          <span class="quotedbibl">
            <xsl:text>(</xsl:text>
            <xsl:apply-templates select="following-sibling::tei:bibl"/>
            <xsl:text>)</xsl:text>
          </span>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <blockquote>
	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>quote</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>
          <xsl:choose>
            <xsl:when test="tei:p">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
              <p>
                <xsl:apply-templates/>
              </p>
            </xsl:otherwise>
          </xsl:choose>
        </blockquote>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:resp</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:resp">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:respStmt</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:respStmt"><xsl:apply-templates select="tei:resp"
      /><xsl:for-each select="tei:name[position()&lt;last()]"
      ><xsl:apply-templates/>, </xsl:for-each><xsl:apply-templates
      select="child::tei:name[position()=last()]"/>. <xsl:if
      test="ancestor::tei:biblStruct">
      <br/>
    </xsl:if></xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:salute</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:salute">
    <p class="left">
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:seg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:seg">
    <span class="{@type}">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:series</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:series">
    <tr>
      <td>
        <xsl:apply-templates/>
      </td>
    </tr>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:signed</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:signed">
    <p class="left">
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:soCalled</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:soCalled">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>‘</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>’</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:space</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:space">
    <xsl:choose>
      <xsl:when test="@extent">
        <xsl:call-template name="space_loop">
          <xsl:with-param name="extent" select="@extent"/>
        </xsl:call-template>
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:term</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:term">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:otherwise>
        <em>
          <xsl:apply-templates/>
        </em>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:title" mode="withbr">
    <xsl:value-of select="."/>
    <br/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:titleStmt/tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:titleStmt/tei:title">
    <xsl:if test="preceding-sibling::tei:title">
      <br/>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:title">
    <xsl:choose>
      <xsl:when test="@rend='plain'">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="@level='a'">
        <xsl:text>‘</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>’ </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <i>
          <xsl:value-of select="."/>
        </i>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:witList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:witList">
    <xsl:apply-templates select="./witness"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:witness</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:witness">
    <p>
      <a name="{@sigil}"/>
      <b>Sigle: <xsl:value-of select="@sigil"/></b>
      <br/>
      <xsl:value-of select="text()"/>
      <br/>
      <xsl:apply-templates select="tei:biblStruct"/>
      <xsl:if test="child::tei:note"><br/>Zie noot: <xsl:apply-templates
          select="child::tei:note"/></xsl:if>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="value">value</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="applyRend">
    <xsl:param name="value"/>
    <xsl:choose>
      <xsl:when test="not($value='')">
        <xsl:variable name="thisparm"
          select="substring-before($value,$rendSeparator)"/>
        <xsl:call-template name="renderingInner">
          <xsl:with-param name="value" select="$thisparm"/>
          <xsl:with-param name="rest"
            select="substring-after($value,$rendSeparator)"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="continuedToc">
    <xsl:if
      test="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6">
      <ul class="toc">
        <xsl:apply-templates mode="maketoc"
          select="tei:div|tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5|tei:div6"
        />
      </ul>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] How to identify a note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="noteID">
    <xsl:choose>
      <xsl:when test="@xml:id">
        <xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:when test="@n">
        <xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:number count="tei:note" level="any"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] How to label a note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="noteN">
    <xsl:choose>
      <xsl:when test="@n">
        <xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:when test="not(@place) and $consecutiveFootnoteNumbers='true'">
            <xsl:number 
		count="tei:note[not(@place)]" 
		level="any"/>
      </xsl:when>
      <xsl:when test="not(@place)">
        <xsl:choose>
          <xsl:when test="ancestor::tei:front">
            <xsl:number count="tei:note[not(@place)]" 
			from="tei:front"
			level="any"/>
          </xsl:when>
          <xsl:when test="ancestor::tei:back">
            <xsl:number count="tei:note[not(@place)]" 
			from="tei:back"
			level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:number 
		count="tei:note[not(@place)]" 
		from="tei:body"
		level="any"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="@place='end'">
        <xsl:choose>
          <xsl:when test="$consecutiveFootnoteNumbers = 'true'">
            <xsl:number 
		count="tei:note[./@place='end']" 
		level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:choose>
              <xsl:when test="ancestor::tei:front">
                <xsl:number count="tei:note[./@place='end']" from="tei:front"
                  level="any"/>
              </xsl:when>
              <xsl:when test="ancestor::tei:back">
                <xsl:number count="tei:note[./@place='end']" from="tei:back"
                  level="any"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number count="tei:note[./@place='end']" from="tei:body"
                  level="any"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$consecutiveFootnoteNumbers = 'true'">
            <xsl:number 
		count="tei:note[./@place='foot']" 
		level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:choose>
              <xsl:when test="ancestor::tei:front">
                <xsl:number count="tei:note[./@place='foot']" from="tei:front"
                  level="any"/>
              </xsl:when>
              <xsl:when test="ancestor::tei:back">
                <xsl:number count="tei:note[./@place='foot']" from="tei:back"
                  level="any"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number count="tei:note[./@place='foot']" from="tei:body"
                  level="any"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] Show relevant footnotes</xd:short>
    <xd:param name="currentID">currentID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="partialFootNotes">
    <xsl:param name="currentID"/>
    <xsl:choose>
      <xsl:when test="$currentID='current'"/>
      <xsl:when test="$currentID='' and $splitLevel=-1">
        <xsl:call-template name="printNotes"/>
      </xsl:when>
      <xsl:when test="$currentID=''">
        <xsl:for-each select=" descendant::tei:text">
          <xsl:call-template name="printNotes"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
            <xsl:for-each select="key('IDS',$currentID)">
              <xsl:call-template name="printNotes"/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="xpath"
              select="ancestor-or-self::tei:TEI/descendant::tei:text">
              <xsl:with-param name="xpath" select="$currentID"/>
              <xsl:with-param name="action">notes</xsl:with-param>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="processFootnotes">
    <xsl:apply-templates mode="printnotes" select="//tei:note"/>
  </xsl:template>

  <xsl:template name="printNotes">
    <xsl:if test="ancestor-or-self::tei:TEI/tei:text/descendant::tei:note[@place!='inline']">
      <xsl:choose>
        <xsl:when test="$footnoteFile='true'">
          <xsl:variable name="BaseFile">
            <xsl:value-of select="$masterFile"/>
            <xsl:call-template name="addCorpusID"/>
          </xsl:variable>
          <xsl:call-template name="outputChunk">
            <xsl:with-param name="ident">
              <xsl:value-of select="concat($BaseFile,'-notes')"/>
            </xsl:with-param>
            <xsl:with-param name="content">
              <xsl:call-template name="writeNotes"/>
            </xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <div class="notes">
            <div class="noteHeading">
              <xsl:call-template name="i18n">
                <xsl:with-param name="word">noteHeading</xsl:with-param>
              </xsl:call-template>
            </div>
            <xsl:apply-templates mode="printnotes"
              select="descendant::tei:note[@place!='inline']"/>
          </div>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail>
      <p> rendering. support for multiple rendition elements added by Nick
        Nicholas </p>
    </xd:detail>
  </xd:doc>
  <xsl:template name="rendering">
    <xsl:call-template name="applyRend">
      <xsl:with-param name="value" select="concat(@rend,$rendSeparator)"/>
    </xsl:call-template>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="value">the current segment of the value of the rend
      attribute</xd:param>
    <xd:param name="rest">the remainder of the attribute</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="renderingInner">
    <xsl:param name="value"/>
    <xsl:param name="rest"/>
    <xsl:choose>
      <xsl:when test="$value='bold'">
        <b>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </b>
      </xsl:when>
      <xsl:when test="$value='center'">
        <center>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </center>
      </xsl:when>
      <xsl:when test="$value='code'">
        <b>
          <tt>
            <xsl:call-template name="applyRend">
              <xsl:with-param name="value" select="$rest"/>
            </xsl:call-template>
          </tt>
        </b>
      </xsl:when>
      <xsl:when test="$value='ital'">
        <i>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </i>
      </xsl:when>
      <xsl:when test="$value='italic'">
        <i>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </i>
      </xsl:when>
      <xsl:when test="$value='it'">
        <i>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </i>
      </xsl:when>
      <xsl:when test="$value='italics'">
        <i>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </i>
      </xsl:when>
      <xsl:when test="$value='i'">
        <i>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </i>
      </xsl:when>
      <xsl:when test="$value='sc'">
        <!--   <small>
	   <xsl:value-of
	   select="translate(.,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
	   </small>
      -->
        <span style="font-variant: small-caps">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='plain'">
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$value='quoted'">
        <xsl:text>‘</xsl:text>
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
        <xsl:text>’</xsl:text>
      </xsl:when>
      <xsl:when test="$value='sub'">
        <sub>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </sub>
      </xsl:when>
      <xsl:when test="$value='sup'">
        <sup>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </sup>
      </xsl:when>
      <xsl:when test="$value='important'">
        <span class="important">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <!-- NN added -->
      <xsl:when test="$value='ul'">
        <u>
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </u>
      </xsl:when>
      <!-- NN added -->
      <xsl:when test="$value='interlinMarks'">
        <xsl:text>`</xsl:text>
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
        <xsl:text>´</xsl:text>
      </xsl:when>
      <xsl:when test="$value='overbar'">
        <span style="text-decoration:overline">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='expanded'">
        <span style="letter-spacing: 0.15em">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='strike'">
        <span style="text-decoration: line-through">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='small'">
        <span style="font-size: 75%">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='large'">
        <span style="font-size: 150%">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='smaller'">
        <span style="font-size: 50%">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='larger'">
        <span style="font-size: 200%">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='calligraphic'">
        <span style="font-family: cursive">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='gothic'">
        <span style="font-family: fantasy">
          <xsl:call-template name="applyRend">
            <xsl:with-param name="value" select="$rest"/>
          </xsl:call-template>
        </span>
      </xsl:when>
      <xsl:when test="$value='noindex'">
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rest"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="local-name(.)='p'">
            <xsl:call-template name="unknownRendBlock">
              <xsl:with-param name="rest" select="$rest"/>
              <xsl:with-param name="value" select="$value"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="unknownRendInline">
              <xsl:with-param name="rest" select="$rest"/>
              <xsl:with-param name="value" select="$value"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="extent">extent</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="space_loop">
    <xsl:param name="extent"/>
    <xsl:choose>
      <xsl:when test="$extent &lt; 1"> </xsl:when>
      <xsl:otherwise>
        <xsl:text> </xsl:text>
        <xsl:variable name="newextent">
          <xsl:value-of select="$extent - 1"/>
        </xsl:variable>
        <xsl:call-template name="space_loop">
          <xsl:with-param name="extent" select="$newextent"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="value">current value</xd:param>
    <xd:param name="rest">remaining values</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="unknownRendBlock">
    <xsl:param name="value"/>
    <xsl:param name="rest"/>
    <xsl:message>Unknown rend attribute <xsl:value-of select="$value"/></xsl:message>
    <code class="undone">[Unknown rendering: <xsl:value-of select="$value"/>]</code>
    <xsl:call-template name="applyRend">
      <xsl:with-param name="value" select="$rest"/>
    </xsl:call-template>
    <code class="undone">[End rendering]</code>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="value">value</xd:param>
    <xd:param name="rest">rest</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="unknownRendInline">
    <xsl:param name="value"/>
    <xsl:param name="rest"/>
    <xsl:message>Unknown rend attribute <xsl:value-of select="$value"/></xsl:message>
    <code class="undone">[Unknown rendering: <xsl:value-of select="$value"/>]</code>
    <xsl:call-template name="applyRend">
      <xsl:with-param name="value" select="$rest"/>
    </xsl:call-template>
    <code class="undone">[End rendering]</code>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="writeNotes">
    <html>
      <xsl:call-template name="addLangAtt"/>
      <head>
        <title>
          <xsl:apply-templates
            select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/>
          <xsl:text>: </xsl:text>
          <xsl:call-template name="i18n">
            <xsl:with-param name="word">noteHeading</xsl:with-param>
          </xsl:call-template>
        </title>
        <xsl:call-template name="includeCSS"/>
        <xsl:call-template name="cssHook"/>
      </head>
      <body>
        <xsl:attribute name="onload">
          <xsl:text>startUp()</xsl:text>
        </xsl:attribute>
        <xsl:call-template name="bodyHook"/>
        <xsl:call-template name="bodyJavascriptHook"/>
	<div class="stdheader">
        <xsl:call-template name="stdheader">
          <xsl:with-param name="title">
            <xsl:apply-templates
              select="descendant-or-self::tei:text/tei:front//tei:docTitle//text()"/>
            <xsl:text>: </xsl:text>
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">noteHeading</xsl:with-param>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
	</div>
        <div class="notes">
          <div class="noteHeading">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">noteHeading</xsl:with-param>
            </xsl:call-template>
          </div>
          <xsl:apply-templates mode="printnotes"
            select="descendant::tei:note[@place]"/>
        </div>
        <xsl:call-template name="stdfooter"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template name="rendToClass">
    <xsl:param name="default"/>
    <xsl:choose>
      <xsl:when test="@rend and starts-with(@rend,'class:')">
        <xsl:attribute name="class">
          <xsl:value-of select="substring-after(@rend,'class:')"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="@rend">
        <xsl:attribute name="class">
          <xsl:value-of select="@rend"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="not($default='')">
        <xsl:attribute name="class">
	  <xsl:value-of select="$default"/>
        </xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="addIdentification">
    <xsl:param name="id"/>
    <xsl:choose>
      <xsl:when test="$xhtml='true'">
	<span>
	  <xsl:attribute name="id">
	    <xsl:value-of select="$id"/>
	  </xsl:attribute>
	</span>
      </xsl:when>
      <xsl:otherwise>
	<a name="{$id}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

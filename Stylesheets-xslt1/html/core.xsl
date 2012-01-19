<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr edate a fo local rng tei html teix xd"
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
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
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
	  <xsl:with-param name="display">plain</xsl:with-param>
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
      <xsl:call-template name="rendToClass"/>
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
    <address>
      <xsl:call-template name="rendToClass"/>
      <xsl:apply-templates/>
    </address>
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
	<span class="bibl">
	  <xsl:apply-templates/>
	</span>
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
    <div>
      <xsl:call-template name="rendToClass"/>
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
    <xd:short>Process element tei:choice</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:choice">
    <xsl:choose>
      <xsl:when test="tei:abbr and tei:expan">
	<xsl:apply-templates select="tei:expan"/>
	<xsl:text> (</xsl:text>
	<xsl:apply-templates select="tei:abbr"/>
	<xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
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
	  <xsl:call-template name="rendToClass"/>
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
    <code>
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements  tei:corr</xd:short>
    <xd:detail>Does not do anything yet.</xd:detail>
  </xd:doc>
  <xsl:template match="tei:corr">
    <xsl:apply-templates/>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements  tei:del</xd:short>
    <xd:detail></xd:detail>
  </xd:doc>
  <xsl:template match="tei:del">
    <del>
      <xsl:apply-templates/>
    </del>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements tei:eg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg">
    <div>
      <xsl:if test="$cssFile">
        <xsl:attribute name="class">
	  <xsl:text>pre_eg</xsl:text>
	  <xsl:if test="not(*)">
	  <xsl:text> cdata</xsl:text>
	  </xsl:if>
	</xsl:attribute>
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
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
	</span>
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
    <div>
      <xsl:call-template name="rendToClass"/>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:epigraph/lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph/tei:lg">
      <xsl:apply-templates/>
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
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
	<xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:otherwise>
	<span class="foreign">
	      <xsl:apply-templates/>
	</span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gap</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gap">
    <span class="gap">
      <xsl:if test="tei:desc">
	<xsl:attribute name="title">
	  <xsl:value-of select="tei:desc" />
	</xsl:attribute>
      </xsl:if>
      <xsl:text> [...]</xsl:text>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gi</xd:short>
    <xd:detail>
      <p> special purpose </p >
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi">
    <span class="gi">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi" mode="plain">
    <span class="gi">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:att</xd:short>
    <xd:detail>
      <p> special purpose </p >
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:att">
    <span class="gi">
      <b>@<xsl:apply-templates/></b>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:gloss</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gloss">
    <span class="gloss">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:head</xd:short>
    <xd:detail>
      <p> headings etc </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:head">
    <xsl:variable name="parent" select="local-name(..)"/>
    <xsl:choose>
      <xsl:when test="parent::tei:body">
	<h1>
	  <xsl:call-template name="rendToClass"/>
	  <xsl:apply-templates/>
	</h1>
      </xsl:when>
      <xsl:when test="not(starts-with($parent,'div'))">
	<xsl:apply-templates/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process element tei:head in heading mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:head" mode="makeheading">
    <xsl:choose>
    <xsl:when test="preceding-sibling::tei:head">
      <br/>
      <span>
	<xsl:call-template name="rendToClass">
	  <xsl:with-param name="default">secondaryHead</xsl:with-param>
	</xsl:call-template>
	<xsl:apply-templates/>
      </span>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
    </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process element tei:head in plain mode</xd:short>
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
      <xsl:when test="@rendition">
      <span>
	<xsl:call-template name="applyRendition"/>
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
	  <xsl:call-template name="makeAnchor">
	    <xsl:with-param name="name">
	      <xsl:value-of select="@xml:id"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="$generateParagraphIDs='true'">
	  <xsl:call-template name="makeAnchor">
	    <xsl:with-param name="name">
	      <xsl:value-of select="generate-id()"/>
	    </xsl:with-param>
	  </xsl:call-template>
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
    <xsl:text>&#x2022; </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item/label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item/tei:label">
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
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
    <span class="label">
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label" mode="print">
    <span class="label">
    <xsl:call-template name="makeAnchor"/>
    <xsl:choose>
      <xsl:when test="@rend">
        <xsl:call-template name="rendering"/>
      </xsl:when>
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
        <xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
    </span>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lb">
    <br>
      <xsl:if test="@rend">
	<xsl:attribute name="class"><xsl:value-of select="@rend"/></xsl:attribute>
      </xsl:if>
    </br>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l">
    <div>
      <xsl:call-template name="rendToClass"/>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg">
    <div>
      <xsl:call-template name="rendToClass"/>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lg/tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg/tei:l">
    <div>
        <xsl:choose>
	  <xsl:when test="@rend">
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
		<xsl:when test="@rend">
		  <xsl:attribute name="class">
		    <xsl:value-of select="@rend"/>
		  </xsl:attribute>
		</xsl:when>
	      </xsl:choose>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="@rendition">
	    <xsl:call-template name="applyRendition"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:attribute name="class">
	      <xsl:text>l</xsl:text>
	    </xsl:attribute>
	  </xsl:otherwise>
	</xsl:choose>
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
          <dt>valList</dt>
          <dd>(Identical to glosstable)</dd>
        </dl>
      </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list">
    <xsl:if test="tei:head">
      <p class="listhead">
	<xsl:apply-templates select="tei:head"/>
      </p>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='catalogue'">
        <p>
          <dl>
            <xsl:call-template name="rendToClass">
	    <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
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
            <xsl:call-template name="rendToClass">
	    <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
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
      <xsl:when test="@type='gloss' or  tei:label">
        <dl>
          <xsl:call-template name="rendToClass">
	  <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
          <xsl:apply-templates mode="gloss" select="tei:item"/>
        </dl>
      </xsl:when>
      <xsl:when test="@type='glosstable' or @type='valList'">
        <table>
          <xsl:call-template name="rendToClass">
	  <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
          <xsl:apply-templates mode="glosstable" select="tei:item"/>
        </table>
      </xsl:when>
      <xsl:when test="@type='inline'">
	<!--<xsl:if test="not(tei:item)">None</xsl:if>-->
        <xsl:apply-templates mode="inline" select="tei:item"/>
      </xsl:when>
      <xsl:when test="@type='runin'">
        <p>
          <xsl:apply-templates mode="runin" select="tei:item"/>
        </p>
      </xsl:when>
      <xsl:when test="@type='unordered' or @type='simple'">
        <ul>
          <xsl:call-template name="rendToClass">
	  <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
          <xsl:apply-templates select="tei:item"/>
        </ul>
      </xsl:when>
      <xsl:when test="@type='bibl'">
        <xsl:apply-templates mode="bibl" select="tei:item"/>
      </xsl:when>
      <xsl:when test="starts-with(@type,'ordered')">
        <ol>
          <xsl:call-template name="rendToClass">
	  <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
          <xsl:if test="starts-with(@type,'ordered:')">
            <xsl:attribute name="start">
              <xsl:value-of select="substring-after(@type,':')"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:apply-templates select="tei:item"/>
        </ol>
      </xsl:when>
      <xsl:otherwise>
        <ul>
          <xsl:call-template name="rendToClass">
	  <xsl:with-param name="default"></xsl:with-param></xsl:call-template>
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
    <xsl:choose>
      <xsl:when test="tei:biblStruct">
	<dl class="listBibl">
	  <xsl:for-each select="tei:biblStruct">
	    <xsl:sort select="translate(tei:*/tei:author/tei:surname|tei:*[1]/tei:author/tei:orgName|tei:*[1]/tei:author/tei:name|tei:*[1]/tei:editor/tei:surname|tei:*[1]/tei:editor/tei:name|tei:*[1]/tei:title,$uc,$lc)"/>
	    <xsl:sort select="tei:monogr/tei:imprint/tei:date"/>
	    <dt>
	      <xsl:call-template name="makeAnchor"/>
	      <xsl:apply-templates select="." mode="xref"/>
	    </dt>
	    <dd>
	      <xsl:apply-templates select="."/>
	    </dd>
	  </xsl:for-each>
	</dl>
      </xsl:when>
      <xsl:otherwise>
	<ol class="listBibl">
	  <xsl:for-each select="tei:bibl|tei:biblItem">
	    <li>
	      <xsl:call-template name="makeAnchor">
		<xsl:with-param name="name">
		  <xsl:apply-templates mode="ident" select="."/>
		</xsl:with-param>
	      </xsl:call-template>
	      <xsl:apply-templates select="."/>
	    </li>
	  </xsl:for-each>
	</ol>
      </xsl:otherwise>
    </xsl:choose>
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
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
        <xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:otherwise>
	<span class="mentioned">
	  <xsl:apply-templates/>
	</span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:name in mode "plain"</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:name" mode="plain">
    <xsl:variable name="ident">
      <xsl:apply-templates mode="ident" select="."/>
    </xsl:variable>
    <span class="name">
      <xsl:call-template name="makeAnchor">
	<xsl:with-param name="name">
	  <xsl:value-of select="$ident"/>
	</xsl:with-param>
      </xsl:call-template>
    <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:name and tei:persName</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:name|tei:persName">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="not(ancestor::tei:person|ancestor::tei:biblStruct)"/>
      <xsl:when test="following-sibling::tei:name|following-sibling::tei:persName">
	<xsl:text>, </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>. </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
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
      <xsl:when test="ancestor::tei:bibl"> (<xsl:apply-templates/>)
      </xsl:when>
      <xsl:when test="@place='marg'">
	<span class="margnote">
	  <xsl:call-template name="makeAnchor">
	    <xsl:with-param name="name" select="$identifier"/>
	  </xsl:call-template>
	  <xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:when test="@place='inline'">
	<xsl:call-template name="makeAnchor">
	  <xsl:with-param name="name" select="$identifier"/>
	</xsl:call-template>
        <xsl:text> (</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="@place='display'">
	<xsl:call-template name="makeAnchor">
	  <xsl:with-param name="name" select="$identifier"/>
	</xsl:call-template>
        <blockquote>
	  <xsl:call-template name="rendToClass"/>
	  <p>
            <xsl:apply-templates/>
          </p>
        </blockquote>
      </xsl:when>
      <xsl:when test="@place='foot' or @place='bottom' or @place='end' or $autoEndNotes='true'">
	<xsl:call-template name="makeAnchor">
	  <xsl:with-param name="name" select="concat($identifier,'_return')"/>
	</xsl:call-template>
        <xsl:choose>
          <xsl:when test="$footnoteFile='true'">
            <a class="notelink" 
	       title="Go to note"
	       href="{$masterFile}-notes.html#{$identifier}">
              <sup>
                <xsl:call-template name="noteN"/>
              </sup>
            </a>
          </xsl:when>
          <xsl:otherwise>
            <a class="notelink" 
	       title="Go to note"
	       href="#{$identifier}">
              <sup>
                <xsl:call-template name="noteN"/>
              </sup>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeAnchor">
	  <xsl:with-param name="name" select="$identifier"/>
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
        <xsl:message>Note <xsl:value-of select="$identifier"/>
	<xsl:text> with parent </xsl:text>
	<xsl:value-of select="$parent"/></xsl:message>
      </xsl:if>
      <div class="note">
	<xsl:call-template name="makeAnchor">
	  <xsl:with-param name="name" select="$identifier"/>
	</xsl:call-template>
	<span class="noteLabel">
	  <xsl:call-template name="noteN"/>
	  <xsl:text>. </xsl:text>
	</span>
	<div class="noteBody">
	  <xsl:apply-templates/>
	</div>
	<xsl:if test="$footnoteBackLink= 'true'">
	  <xsl:text> </xsl:text>
	  <a class="link_return"
	     title="Go back to text"
	     href="#{concat($identifier,'_return')}">&#8629;</a>
	</xsl:if>
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
    <xsl:choose>
      <xsl:when test="$pagebreakStyle='active'">
        <div class="pagebreak">
	  <xsl:call-template name="makeAnchor"/>
	</div>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='visible'">
	<span class="pagebreak">
	  <xsl:text> [</xsl:text>
	  <xsl:call-template name="i18n">
	    <xsl:with-param name="word">page</xsl:with-param>
	  </xsl:call-template>
	  <xsl:if test="@n">
	    <xsl:text> </xsl:text>
	    <xsl:value-of select="@n"/>
	  </xsl:if>
	  <xsl:call-template name="makeAnchor"/>
	  <xsl:text>] </xsl:text>
	</span>
	</xsl:when>
      <xsl:otherwise> </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process element tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p">
    <xsl:variable name="wrapperElement">
      <xsl:choose>
        <xsl:when
          test="tei:specList|tei:quote|tei:moduleSpec|tei:list|tei:eg|teix:egXML|tei:table|tei:specGrp|tei:specGrpRef|tei:q[@rend='display']|tei:figure">
          <xsl:text>div</xsl:text>
        </xsl:when>
        <xsl:when test="parent::tei:p">
          <xsl:text>div</xsl:text>
        </xsl:when>
        <xsl:when test="parent::tei:remarks">
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
	<xsl:when test="@xml:id">
	  <xsl:call-template name="makeAnchor">
	    <xsl:with-param name="name">
	      <xsl:value-of select="@xml:id"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:when test="$generateParagraphIDs='true'">
	  <xsl:call-template name="makeAnchor">
	    <xsl:with-param name="name">
	      <xsl:value-of select="generate-id()"/>
	    </xsl:with-param>
	  </xsl:call-template>
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
    <xd:short>Process elements tei:q and tei:said</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q|tei:said">
    <xsl:choose>
      <xsl:when test="tei:p|tei:l">
        <blockquote>
	  <xsl:call-template name="rendToClass"/>
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
	<xsl:call-template name="makeQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:q[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q[@rend='display']">
    <blockquote>
      <xsl:call-template name="rendToClass"/>
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
    <div>
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
      <xsl:when test="contains(concat(' ', @rend, ' '), ' quoted ')">
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
      <xsl:when test="parent::tei:p or parent::tei:note">
	<div class="blockquote">
	  <xsl:apply-templates/>
	</div>
      </xsl:when>
      <xsl:otherwise>
        <blockquote>
	  <xsl:call-template name="rendToClass"/>
          <xsl:choose>
            <xsl:when test="tei:p">
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:when test="tei:l">
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
    <xsl:text> </xsl:text>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:respStmt</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:respStmt">
    <xsl:apply-templates/>
  </xsl:template>
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
    <span>
	<xsl:choose>
	  <xsl:when test="@type">
	    <xsl:attribute name="class">
	      <xsl:value-of select="@type"/>
	    </xsl:attribute>
	  </xsl:when>
	  <xsl:when test="@rend">
	    <xsl:attribute name="class">
	      <xsl:value-of select="@rend"/>
	    </xsl:attribute>
	  </xsl:when>
	</xsl:choose>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements  tei:sic</xd:short>
    <xd:detail>Does not do anything yet.</xd:detail>
  </xd:doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/>
    <xsl:text> (sic)</xsl:text>
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
    <xd:short>Process elements tei:space</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:space">
    <xsl:choose>
      <xsl:when test="@quantity|@extent">
        <xsl:call-template name="space_loop">
          <xsl:with-param name="extent" select="@quantity|@extent"/>
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
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
        <xsl:apply-templates/>
	</span>
      </xsl:when>      
      <xsl:otherwise>
        <span class="term">
          <xsl:apply-templates/>
        </span>
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
    <xd:short>[html] Activate a value for @rendition</xd:short>
    <xd:param name="value">value</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="applyRendition">
    <xsl:attribute name="class">
  
      <xsl:choose>
	<xsl:when test="@rendition=''"/>
	<xsl:when test="contains(normalize-space(@rendition),' ')">
	  <xsl:call-template name="splitRendition">
	    <xsl:with-param name="value">
	      <xsl:value-of select="normalize-space(@rendition)"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="findRendition">
	    <xsl:with-param name="value">
	      <xsl:value-of select="@rendition"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
   </xsl:template>

  <xd:doc>
    <xd:short>[html] Get another value from a space-separated list</xd:short>
    <xd:param name="value">value</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="splitRendition">
    <xsl:param name="value"/>
    <xsl:choose>
      <xsl:when test="$value=''"/>
      <xsl:when test="contains($value,' ')">
	<xsl:call-template name="findRendition">
	  <xsl:with-param name="value">
	    <xsl:value-of select="substring-before($value,' ')"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="splitRendition">
	  <xsl:with-param name="value">
	    <xsl:value-of select="substring-after($value,' ')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="findRendition">
	  <xsl:with-param name="value">
	    <xsl:value-of select="$value"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="findRendition">
    <xsl:param name="value"/>
    <xsl:choose>
      <xsl:when test="starts-with($value,'#')">
	<xsl:value-of select="substring-after($value,'#')"/>
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="document($value)">
	  <xsl:apply-templates select="@xml:id"/>
	  <xsl:text> </xsl:text>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] Active a value for @rend</xd:short>
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
      <xsl:when test="not(@place) and $consecutiveFNs='true'">
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
          <xsl:when test="$consecutiveFNs = 'true'">
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
          <xsl:when test="$consecutiveFNs = 'true'">
            <xsl:number 
		count="tei:note[@place='foot' or @place='bottom']" 
		level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:choose>
              <xsl:when test="ancestor::tei:front">
                <xsl:number count="tei:note[@place='foot' or @place='bottom']" from="tei:front"
                  level="any"/>
              </xsl:when>
              <xsl:when test="ancestor::tei:back">
                <xsl:number count="tei:note[@place='foot' or @place='bottom']" from="tei:back"
                  level="any"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number count="tei:note[@place='foot' or @place='bottom']" from="tei:body"
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

  <xsl:template name="printNotes">
      <xsl:choose>
	<xsl:when test="$footnoteFile='true' and
			ancestor-or-self::tei:TEI/tei:text/descendant::tei:note[@place='foot'
			or @place='bottom'
			or @place='end']">
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
	<xsl:when test="self::tei:div">
	  <xsl:variable name="depth">
	    <xsl:apply-templates mode="depth" select="."/>
	  </xsl:variable>
	  <xsl:choose>
	    <xsl:when test="$depth &lt; $splitLevel ">
	  <xsl:if
	      test="child::*[not(self::tei:div)]/descendant::tei:note[@place='foot'
		    or @place='bottom' or @place='end']">
	    <div class="notes">
	      <div class="noteHeading">
		<xsl:call-template name="i18n">
		  <xsl:with-param name="word">noteHeading</xsl:with-param>
		</xsl:call-template>
	      </div>
	      <xsl:apply-templates 
		  mode="printnotes"
		  select="child::*[not(self::tei:div)]/descendant::tei:note[@place='foot'
			  or @place='bottom' or @place='end']"/>
	    </div>
	  </xsl:if>
	    </xsl:when>
	    <xsl:otherwise>
	  <xsl:if test="descendant::tei:note[@place='foot' or @place='bottom' or @place='end']">
	    <div class="notes">
	      <div class="noteHeading">
		<xsl:call-template name="i18n">
		  <xsl:with-param name="word">noteHeading</xsl:with-param>
		</xsl:call-template>
	      </div>
	      <xsl:apply-templates 
		  mode="printnotes"
		  select="descendant::tei:note[@place='foot' or @place='bottom' or @place='end']"/>
	    </div>
	  </xsl:if>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:when>
	
	<xsl:otherwise>
	  <xsl:if test="descendant::tei:note[@place='foot' or @place='bottom' or @place='end']">
	    <div class="notes">
	      <div class="noteHeading">
		<xsl:call-template name="i18n">
		  <xsl:with-param name="word">noteHeading</xsl:with-param>
		</xsl:call-template>
	      </div>
	      <xsl:apply-templates 
		  mode="printnotes"
		  select="descendant::tei:note[@place='foot' or @place='bottom'  or @place='end']"/>
	    </div>
	  </xsl:if>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:if
	  test="ancestor-or-self::tei:TEI/tei:text/descendant::tei:app">
	
	<div class="notes">
	  <div class="noteHeading">
	    <xsl:call-template name="i18n">
	      <xsl:with-param name="word">noteHeading</xsl:with-param>
	    </xsl:call-template>
	  </div>
	  <xsl:apply-templates mode="printnotes"
			       select="descendant::tei:app"/>
	</div>
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
    <xd:short>[html] Process unknown rend attribute by turning it into
    an HTML class</xd:short>
    <xd:param name="value">current value</xd:param>
    <xd:param name="rest">remaining values</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="unknownRendBlock">
    <xsl:param name="value"/>
    <xsl:param name="rest"/>
    <xsl:if test="not($value='')">
      <xsl:attribute name="class">
	<xsl:value-of select="$value"/>
      </xsl:attribute>
      <xsl:call-template name="applyRend">
	<xsl:with-param name="value" select="$rest"/>
    </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
  <xd:doc>
    <xd:short>[html]  Process unknown rend attribute by turning it into
    an HTML class</xd:short>
    <xd:param name="value">value</xd:param>
    <xd:param name="rest">rest</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template name="unknownRendInline">
    <xsl:param name="value"/>
    <xsl:param name="rest"/>
    <xsl:if test="not($value='')">
      <span class="{$value}">
	<xsl:call-template name="applyRend">
	  <xsl:with-param name="value" select="$rest"/>
	</xsl:call-template>
      </span>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] create external notes file</xd:short>
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
        <xsl:call-template name="bodyEndHook"/>
      </body>
    </html>
  </xsl:template>


  <xd:doc>
    <xd:short>[html] Create a point to which we can link in the HTML</xd:short>
    <xd:param name="name">value for identifier</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeAnchor">
    <xsl:param name="name"/>
    <xsl:choose>
      <xsl:when test="$name and $xhtml='true'">
	  <xsl:attribute name="id">
	    <xsl:value-of select="$name"/>
	  </xsl:attribute>
      </xsl:when>
      <xsl:when test="$name">
	<a name="{$name}">
	  <xsl:comment>anchor</xsl:comment>
	</a>
      </xsl:when>
      <xsl:when test="@xml:id and $xhtml='true'">
	  <xsl:attribute name="id">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:attribute>
      </xsl:when>
      <xsl:when test="@xml:id">
        <a name="{@xml:id}">
          <xsl:comment>anchor</xsl:comment>
        </a>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="me">
	<xsl:value-of select="$masterFile"/>-<xsl:value-of
          select="local-name(.)"/>-<xsl:value-of
	  select="generate-id()"/>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="$xhtml='true'">
	      <xsl:attribute name="id">
		<xsl:value-of select="$me"/>
	      </xsl:attribute>
	  </xsl:when>
	  <xsl:otherwise>
	    <a name="{$me}">
	      <xsl:comment>anchor</xsl:comment>
	    </a>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>

    </xsl:choose>
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
      <xsl:when test="@rendition">
	<span>
	<xsl:call-template name="applyRendition"/>
	<xsl:apply-templates/>
	</span>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$preQuote"/>
	<xsl:apply-templates/>
	<xsl:value-of select="$postQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xd:doc>
    <xd:short>Copy all attributes in HTML namespace</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="htmlAttributes">
    <xsl:for-each select="@*">
      <xsl:if test="namespace-uri(.)='http://www.w3.org/1999/xhtml'">
      <xsl:attribute name="{local-name(.)}">
	<xsl:value-of select="."/>
      </xsl:attribute>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xd:doc>
    <xd:short>[html] convert rend attribute to HTML class</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rendToClass">
    <xsl:param name="id">true</xsl:param>
    <xsl:param name="default">.</xsl:param>
    <xsl:if test="$id='true'">
      <xsl:choose>
	<xsl:when test="@id">
	  <xsl:copy-of select="@id"/>
	</xsl:when>
	<xsl:when test="@xml:id">
	  <xsl:attribute name="id">
	    <xsl:value-of select="@xml:id"/>
	  </xsl:attribute>
	</xsl:when>
      </xsl:choose>
    </xsl:if>

    <xsl:variable name="class">
      <xsl:choose>
	<xsl:when test="$default=''"/>
	<xsl:when test="not($default='.')">
	  <xsl:value-of select="$default"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="local-name()"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="rend">
      <xsl:choose>
	<xsl:when test="@rend">
	  <xsl:value-of select="@rend"/>
	</xsl:when>
	<xsl:when test="@rendition">
	  <xsl:call-template name="applyRendition"/>
	</xsl:when>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="concat($class,$rend)=''"/>
      <xsl:when test="$class=''">
	<xsl:attribute name="class">
	  <xsl:value-of select="$rend"/>
	</xsl:attribute>
      </xsl:when>
      <xsl:when test="$rend=''">
	<xsl:attribute name="class">
	  <xsl:value-of select="$class"/>
	</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="class">
	  <xsl:value-of select="$class"/>
	  <xsl:text> </xsl:text>
	  <xsl:value-of select="$rend"/>
	</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>


    <xsl:call-template name="rendToClassHook"/>

  </xsl:template>

  <xd:doc>
    <xd:short>[html] allow for local extensions to rendToClass</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rendToClassHook"/>

</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd tei edate"
  extension-element-prefixes="edate" version="1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the core module. </xd:short>
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
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process all elements in depth</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="depth">99</xsl:template>
  <xd:doc>
    <xd:short>Process all elements in plain mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="plain">
    <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <xsl:template match="tei:note" mode="plain"/>
  <xsl:template match="tei:app" mode="plain"/>
  <xsl:template match="tei:pb" mode="plain"/>
  <xsl:template match="tei:lb" mode="plain"/>
  <xsl:template match="tei:ptr" mode="plain"/>
  <xd:doc>
    <xd:short>Process tei:sic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process tei:corr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:corr"/>
  <xd:doc>
    <xd:short>Process tei:item in runin mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="runin">
    <xsl:text> • </xsl:text>
   <xsl:apply-templates/> 
  </xsl:template>


<xd:doc>
  <xd:short>tei:author inside a bibl</xd:short>
  <xd:detail> </xd:detail>
</xd:doc>
<xsl:template match="tei:bibl/tei:author">
<xsl:apply-templates/>
<xsl:choose>
  <xsl:when test="count(following-sibling::tei:author)=1">
    <xsl:text> and </xsl:text>
  </xsl:when>
  <xsl:when test="following-sibling::tei:author">
    <xsl:text>, </xsl:text>
  </xsl:when>
</xsl:choose>
</xsl:template>

<xd:doc>
  <xd:short>tei:editor</xd:short>
  <xd:detail> </xd:detail>
</xd:doc>
<xsl:template match="tei:editor">
  <xsl:choose>
    <xsl:when test="tei:name">
      <xsl:apply-templates select="tei:name[position()=1]"/>
      <xsl:for-each select="tei:name[position()&gt;1]">
	<xsl:text>, </xsl:text>
	<xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text> (ed).&#10;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
      <xsl:choose>
	<xsl:when test="count(following-sibling::tei:editor)=1">
	  <xsl:text> and </xsl:text>
	</xsl:when>
	<xsl:when test="following-sibling::tei:author">
	  <xsl:text>, </xsl:text>
	</xsl:when>
	<xsl:when test="preceding-sibling::tei:editor and
			not(following-sibling::tei:editor)">
	  <xsl:text> (eds.)</xsl:text>
	</xsl:when>
	<xsl:when test="not(following-sibling::tei:editor)">
	  <xsl:text> (ed.)</xsl:text>
	</xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:edition</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:edition">
    <xsl:apply-templates/>
    <xsl:text>.&#10;</xsl:text>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements tei:biblScope</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:biblScope">
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:imprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:imprint">
    <xsl:apply-templates select="tei:biblScope"/>
    <xsl:if test="tei:publisher">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:publisher"/>
    </xsl:if>
    <xsl:if test="tei:pubPlace">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:pubPlace"/>
    </xsl:if>
    <xsl:apply-templates select="tei:date"/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:publisher</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:publisher">
    <xsl:apply-templates/>
  </xsl:template>


<xsl:template name="makeQuote">
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
</xsl:template>

</xsl:stylesheet>

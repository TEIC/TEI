<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd exsl s estr edate a rng tei teix"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the tagdocs module,
      making LaTeX output. </xd:short>
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
    <xd:short>Example element</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="teix:egXML">
    <xsl:choose>
      <xsl:when test="parent::tei:cell">
	<xsl:text>\mbox{}\\\bgroup</xsl:text>
	<xsl:call-template name="egXMLStartHook"/>
	<xsl:text>\exampleFontSet\begin{shaded}</xsl:text>
	<xsl:apply-templates mode="verbatim"/>
	<xsl:text>\end{shaded}</xsl:text>
	<xsl:call-template name="egXMLEndHook"/>
	<xsl:text>\egroup </xsl:text>
      </xsl:when>
      <xsl:otherwise>
      <xsl:text>\par\bgroup</xsl:text>
      <xsl:call-template name="egXMLStartHook"/>
      <xsl:text>\exampleFontSet\begin{shaded}\noindent\mbox{}</xsl:text>
      <xsl:apply-templates mode="verbatim"/>
      <xsl:text>\end{shaded}</xsl:text>
      <xsl:call-template name="egXMLEndHook"/>
      <xsl:text>\egroup\par </xsl:text>
      <xsl:if test="parent::tei:p and following-sibling::node()">\noindent </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


<xsl:template name="egXMLStartHook"/>
<xsl:template name="egXMLEndHook"/>

<xsl:template match="tei:seg[@rend='specChildren']">
  <xsl:choose>
    <xsl:when test=".//tei:seg[@rend='specChildModule']">
<xsl:text>\hfil\\[-10pt]\begin{sansreflist}</xsl:text>
<xsl:apply-templates/>
<xsl:text>\end{sansreflist}</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!--
<xsl:template match="tei:seg[@rend='specChildren']">
<xsl:text>\mbox{ }\\ \begin{description}</xsl:text>
<xsl:apply-templates/>
<xsl:text>\end{description}</xsl:text>
</xsl:template>
-->

<xsl:template match="tei:seg[@rend='specChild']">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:seg[@rend='specChildModule']">
    \item[<xsl:apply-templates/>]
</xsl:template>

<xsl:template match="tei:seg[@rend='specChildElements']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='parent']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers1']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers2']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers3']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:hi[@rend='showmembers4']">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' or @rend='attDef']" >
  <xsl:text>
\begin{reflist}</xsl:text>
<xsl:apply-templates/>
  <xsl:text>
\end{reflist}  </xsl:text>
</xsl:template>

<xsl:template match="tei:table[@rend='valList' 
     or @rend='attList' 
     or @rend='specDesc']">
<xsl:text>\hfil\\[-10pt]\begin{sansreflist}</xsl:text>
<xsl:apply-templates/>
  <xsl:text>
\end{sansreflist}  </xsl:text>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
    or @rend='attList' 
    or @rend='valList' 
    or @rend='attDef' 
    or @rend='specDesc']/tei:row">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
     or @rend='attList' 
     or @rend='specDesc' 
     or @rend='valList' 
     or @rend='attDef']/tei:row/tei:cell[1]">
<xsl:choose>
  <xsl:when test="parent::tei:row/parent::tei:table[@rend='attList']">
    \item[@<xsl:apply-templates/>]
  </xsl:when>
  <xsl:when test="ancestor::tei:table[@rend='valList']">
    \item[<xsl:apply-templates/>]
  </xsl:when>
  <xsl:when test="ancestor::tei:table[@rend='specDesc']">
    \item[@<xsl:apply-templates/>]
  </xsl:when>
  <xsl:when test="@cols='2' and not(parent::tei:row/preceding-sibling::tei:row)">
   <xsl:text>&#10;\item[]\begin{specHead}{</xsl:text>
   <xsl:value-of select="ancestor::tei:div[1]/@xml:id"/>
   <xsl:text>}</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{specHead} </xsl:text>
  </xsl:when>
  <xsl:when test="@cols='2'">
    \item[]<xsl:apply-templates/>
  </xsl:when>
  <xsl:otherwise>
    \item[<xsl:apply-templates/>]
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="tei:div[@type='refdoc']/tei:head"/>

<xsl:template match="tei:div[@type='refdoc']">
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:table[@rend='wovenodd' 
      or @rend='attList' 
      or @rend='valList' 
      or @rend='specDesc' 
      or @rend='attDef']/tei:row/tei:cell[2]">
  <xsl:apply-templates/>
</xsl:template>


<xsl:template match="tei:list[@rend='specList']">
\begin{sansreflist}
  <xsl:apply-templates/>
\end{sansreflist}
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-elementSpec']">
  <xsl:text>[\textbf{&lt;</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>&gt;}]</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-macroSpec']">
 <xsl:text>[\textbf{</xsl:text>
  <xsl:value-of select="."/>
 <xsl:text>}]</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='specList-classSpec']">
 <xsl:text>[\textbf{</xsl:text>
 <xsl:value-of select="."/>
 <xsl:text>}]</xsl:text>
</xsl:template>

<xsl:template match="tei:hi[@rend='label']">
 <xsl:text>{</xsl:text>
 <xsl:value-of select="."/>
 <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template match="tei:hi[@rend='attribute']">
  <xsl:text>\textit{</xsl:text>
 <xsl:value-of select="."/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template name="specHook">
  <xsl:param name="name"/>
</xsl:template>


<xsl:template match="tei:index[@indexName='ODDS']">
  <xsl:for-each select="tei:term">
    <xsl:text>\index{</xsl:text>
    <xsl:choose>
      <xsl:when test="@sortBy">
	<xsl:value-of select="@sortBy"/>
	<xsl:text>=</xsl:text>
	<xsl:value-of select="."/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>|oddindex</xsl:text>
    <xsl:text>}</xsl:text>
  </xsl:for-each>
  <xsl:for-each select="tei:index/tei:term">
    <xsl:text>\index{</xsl:text>
    <xsl:choose>
      <xsl:when test="@sortBy">
	<xsl:value-of select="@sortBy"/>
	<xsl:text>=</xsl:text>
	<xsl:value-of select="."/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>!</xsl:text>
    <xsl:value-of select="../../tei:term"/>
    <xsl:text>|oddindex</xsl:text>
    <xsl:text>}</xsl:text>
  </xsl:for-each>

</xsl:template>


</xsl:stylesheet>

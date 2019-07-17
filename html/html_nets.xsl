<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:html="http://www.w3.org/1999/xhtml" xmlns="http://www.w3.org/1999/xhtml" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="#all" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the nets module,
      making HTML output. </p>
      <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
      <p>Author: See AUTHORS</p>
      
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:variable name="quo">'</xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element eTree</desc>
  </doc>
  <xsl:template match="tei:forest|tei:eTree|tei:eLeaf">
    <xsl:variable name="thistreestyle" select="if (ancestor-or-self::eTree/@rend) then ancestor-or-self::eTree[last()]/@rend
					       else $treestyle"/>
    <xsl:choose>
      <xsl:when test="$thistreestyle='googlechart'">
        <xsl:if test="not(preceding::tei:eTree or preceding::tei:forest)">
          <script type="text/javascript" src="https://www.google.com/jsapi"/>
          <script type="text/javascript">
	google.setOnLoadCallback(drawCharts);
	function drawCharts() {
	<xsl:for-each select="key('TREES',1)"><xsl:variable name="TREEID" select="generate-id()"/><xsl:text>var data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>= new google.visualization.DataTable();
	  data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>.addColumn('string', 'Person');
	  data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>.addColumn('string', 'Parent');
	  data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>.addColumn('string', 'Note');
	  data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>.addRows([
	  [{v:'</xsl:text><xsl:value-of select="generate-id()"/><xsl:text>',  f:'</xsl:text><xsl:for-each select="tei:label"><xsl:apply-templates/></xsl:for-each><xsl:text>'}, '', ''],
</xsl:text><xsl:for-each select=".//tei:eTree|.//tei:eLeaf"><xsl:text>[{v:'</xsl:text><xsl:value-of select="generate-id()"/><xsl:text>',  f:'</xsl:text><xsl:for-each select="tei:label"><xsl:apply-templates/></xsl:for-each><xsl:text>'}, '</xsl:text><xsl:for-each select="parent::tei:*"><xsl:value-of select="generate-id()"/></xsl:for-each><xsl:text>', ''],
</xsl:text></xsl:for-each><xsl:text>]);
	  var chart</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>= new	google.visualization.OrgChart(document.getElementById('chart</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>'));
	  chart</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>.draw(data</xsl:text><xsl:value-of select="$TREEID"/><xsl:text>, {allowCollapse:true,nodeClass:'teinode',allowHtml:true});
	  </xsl:text></xsl:for-each>
	};
      </script>
        </xsl:if>
        <xsl:if test="not(ancestor::tei:eTree or ancestor::tei:forest)">
          <xsl:variable name="TREEID" select="generate-id()"/>
          <span id="chart{$TREEID}"/>
        </xsl:if>
      </xsl:when>
      <xsl:when test="$thistreestyle='d3VerticalTree'">
        <xsl:choose>
          <xsl:when test="not(ancestor::tei:eTree or ancestor::tei:forest)">
	    <xsl:variable name="maxlabel"
			  select="(max(descendant::tei:label/string-length()))"/>
            <xsl:variable name="treewidth"
			  select="max(descendant-or-self::*[self::tei:eTree
				  or
				  self::tei:eLeaf]/(count(tei:eLeaf)+count(tei:eTree)))
				  * 175"/>
            <xsl:variable name="treedepth"
			  select="max(descendant::*[(self::tei:eTree
				  or self::tei:eLeaf) and
				  not(tei:eTree or
				  tei:eLeaf)]/count(ancestor-or-self::*[self::tei:eTree
				  or self::tei:eLeaf]))"/>
            <xsl:variable name="TREEID" select="generate-id()"/>
            <div class="treediagram" style="width:{$treewidth} {@style}" id="viz{$TREEID}"/>
            <script type="text/javascript">
	      <xsl:choose>
		<xsl:when test="$maxlabel &gt; 150">
	      downoffset= 75;
	      down2offset=5;
	      yoffset = -75;
	      treewidth = <xsl:value-of select="$treewidth"/>;
	      treedepth = <xsl:value-of select="$treedepth * 100"/>;
		</xsl:when>
		<xsl:when test="$maxlabel &gt; 50">
	      downoffset= 50;
	      down2offset=5;
	      yoffset = -45;
	      treewidth = <xsl:value-of select="$treewidth"/>;
	      treedepth = <xsl:value-of select="$treedepth * 100"/>;
		</xsl:when>
		<xsl:when test="$maxlabel &gt; 10">
	      downoffset= 40;
	      down2offset=5;
	      yoffset = -35;
	      treewidth = <xsl:value-of select="$treewidth"/>;
	      treedepth = <xsl:value-of select="$treedepth * 75"/>;
		</xsl:when>
		<xsl:otherwise>
	      downoffset= 10;
	      down2offset=5;
	      yoffset = -10;
	      treewidth = <xsl:value-of select="$treewidth"/>;
	      treedepth = <xsl:value-of select="$treedepth * 40"/>;
		</xsl:otherwise>
	      </xsl:choose>
	      <xsl:variable name="extray">
	      <xsl:choose>
		<xsl:when test="$maxlabel &gt; 150">150</xsl:when>
		<xsl:when test="$maxlabel &gt; 50">100</xsl:when>
		<xsl:when test="$maxlabel &gt; 10">50</xsl:when>
		<xsl:otherwise>25</xsl:otherwise>
	      </xsl:choose>
	      </xsl:variable>
      var treeData = {<xsl:call-template name="treelabel"/>};
      visMe("#viz<xsl:value-of select="$TREEID"/>",<xsl:value-of select="$extray"/>);
    </script>
          </xsl:when>
          <xsl:otherwise>{<xsl:call-template name="treelabel"/><xsl:text>}, 
	    </xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>

      <xsl:when test="$thistreestyle='d3DragDropTree'">
        <xsl:variable name="TREEID" select="generate-id()"/>
	
        <xsl:choose>
          <xsl:when test="not(ancestor::tei:eTree or ancestor::tei:forest)">
	    <div  id="viz{$TREEID}"></div>
	    <script src="dndTree.js"></script>
            <script type="text/javascript">
	      treeData = {<xsl:call-template name="treelabel"/>};
	      dragndrop("#viz<xsl:value-of select="$TREEID"/>");
	    </script>
	  </xsl:when>
          <xsl:otherwise>{<xsl:call-template
	  name="treelabel"/><xsl:text>} </xsl:text>
	  <xsl:if test="following-sibling::tei:*">, 
	  </xsl:if>
	  </xsl:otherwise>
        </xsl:choose>
      </xsl:when>

      <xsl:when test="$thistreestyle='d3CollapsableTree'">
        <xsl:choose>
          <xsl:when test="not(ancestor::tei:eTree or ancestor::tei:forest)">
	    <xsl:variable name="maxlabel"
			  select="(max(descendant::tei:label/string-length()))"/>
            <xsl:variable name="treeheight"
			  select="max(descendant-or-self::*[self::tei:eTree
				  or
				  self::tei:eLeaf]/(count(tei:eLeaf)+count(tei:eTree)))
				  * 155"/>
            <xsl:variable name="treewidth"
			  select="count(descendant-or-self::*[self::tei:eTree]) * 130"/>
            <xsl:variable name="treedepth"
			  select="max(descendant::*[(self::tei:eTree
				  or self::tei:eLeaf) and
				  not(tei:eTree or
				  tei:eLeaf)]/count(ancestor-or-self::*[self::tei:eTree
				  or self::tei:eLeaf]))"/>
            <xsl:variable name="TREEID" select="generate-id()"/>
            <div class="treediagram" style="width:{$treewidth} {@style}" id="viz{$TREEID}"/>
            <script type="text/javascript">
     treeData = {<xsl:call-template name="treelabel"/>};
     drawCollapsibleTree("#viz<xsl:value-of select="$TREEID"/>",<xsl:value-of select="$treewidth"/>,<xsl:value-of select="$treeheight"/>);

     d3.select(self.frameElement).style("height", "500px");

    </script>
          </xsl:when>
          <xsl:otherwise>{<xsl:call-template
	  name="treelabel"/><xsl:text>} </xsl:text>
	  <xsl:if test="following-sibling::tei:*">, 
	  </xsl:if>
	  </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="treelabel">
    <xsl:text>"name":'</xsl:text>
      <xsl:for-each select="tei:label">
	<xsl:apply-templates/>
      </xsl:for-each>
    <xsl:text>', </xsl:text>
    <xsl:if test="tei:label/@rend"><xsl:text>"style":"</xsl:text>
    <xsl:value-of select="tei:label/@rend"/>
    <xsl:text>", </xsl:text></xsl:if>
    <xsl:choose>
      <xsl:when test="tei:label/@xml:id">
	<xsl:text>"id":"</xsl:text>
	<xsl:value-of select="tei:label/@xml:id"/>
	<xsl:text>", </xsl:text>
      </xsl:when>
      <xsl:when test="@xml:id">
	<xsl:text>"id":"</xsl:text>
	<xsl:value-of select="@xml:id"/>
	<xsl:text>", </xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="tei:label/@synch">
	<xsl:text>"synch":"</xsl:text>
	<xsl:value-of select="tei:label/@synch"/>
	<xsl:text>", </xsl:text>
      </xsl:when>
      <xsl:when test="@corresp">
	<xsl:text>"synch":"</xsl:text>
	<xsl:value-of select="@corresp"/>
	<xsl:text>", </xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:text>"showlink":"</xsl:text>
    <xsl:value-of select="if (self::tei:forest) then 'invisible' else ''"/>
    <xsl:text>", </xsl:text>
    <xsl:text>"type":"</xsl:text>
    <xsl:value-of select="(if (@type) then @type else '', if (self::tei:eLeaf) then 'leaf'  else '')"/>
    <xsl:text>" </xsl:text>
    <xsl:if test="tei:eTree|tei:eLeaf">
      <xsl:text>,"children":[</xsl:text>
      <xsl:apply-templates select="*[not(self::tei:label)]"/>
      <xsl:text>]</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:eLeaf/tei:ptr"/>
  <xsl:template match="tei:eTree/tei:label/tei:lb">
    <br/>
  </xsl:template>
  <xsl:template match="tei:eLeaf/tei:label/tei:lb">
    <br/>
  </xsl:template>

  <xsl:template match="tei:eLeaf/tei:label/text()|tei:eTree/tei:label/text()">
    <xsl:value-of select="replace(.,$quo,concat('\\',$quo))"/>
  </xsl:template>


</xsl:stylesheet>

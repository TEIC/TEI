<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html xs"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../html/html.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id: to.xsl 12482 2013-07-28 18:39:41Z louburnard $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"/>

<!-- set stylesheet parameters -->
<xsl:param name="numberHeadings">false</xsl:param>
<xsl:param name="numberFigures">false</xsl:param>
<xsl:param name="numberTables">false</xsl:param>
<xsl:param name="numberParagraphs">false</xsl:param>
<xsl:param name="lang">fr</xsl:param>

<!--<xsl:param
    name="cssInlineFiles">../profiles/oulipo/html/oulipo.css</xsl:param>-->

<xsl:variable name="OU"></xsl:variable>

<xsl:param name="cssSecondaryFile"><xsl:value-of select="$OU"/>jquery-ui-1.10.3.custom.css</xsl:param>
<xsl:param name="cssFile"><xsl:value-of select="$OU"/>oulipo.css</xsl:param>
<xsl:param name="institution">OuLiPo</xsl:param>
<xsl:param name="bottomNavigationPanel">false</xsl:param>
<xsl:param name="linkPanel">false</xsl:param>
<xsl:param name="footnoteBackLink">true</xsl:param>
<xsl:param name="homeURL"></xsl:param>
<xsl:param name="feedbackURL"></xsl:param>
<xsl:param name="homeWords">OuLiPo</xsl:param>
<xsl:param name="pagebreakStyle">display</xsl:param>

<xsl:template name="copyrightStatement">
This page is made available under the Creative Commons General Public License "Attribution, Non-Commercial, Share-Alike", version 3.0 (CCPL BY-NC-SA) 
</xsl:template>

<xsl:template match="tei:divGen[@type='wordlist']">
  <xsl:variable name="punc">'".,:!?()«»0123456789 []+’</xsl:variable>
  <xsl:variable name="space"><xsl:text> </xsl:text></xsl:variable>
  <xsl:variable name="contract">([A-z]+’)</xsl:variable>
  <xsl:variable name="replace">$1 </xsl:variable>
  <xsl:variable name="wurds">
    <xsl:for-each select="//tei:body//text() [not(parent::tei:persName)]">
      <xsl:variable name="n" select="preceding::tei:pb[@n][1]/@n|ancestor::tei:TEI[1]/@n"/>
      <xsl:for-each
	  select="normalize-space(translate(translate(.,$punc,''),'-',$space))">
	<xsl:for-each select="tokenize(replace(.,$contract,$replace),' ')">
	  <xsl:if test="not(.='')">
	    <w n="{$n}"><xsl:value-of select="lower-case(.)"/></w>
	  </xsl:if>
	</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:variable>
  <table class="sort">
    <thead>
      <tr><th>Mot</th><th>Occurrences</th><th>ou</th></tr>
    </thead>
    <xsl:for-each-group select="$wurds/html:w" group-by=".">
      <xsl:sort select="count(current-group())"/>
      <xsl:sort select="."/>
      <tr>
      <td>
	<xsl:value-of select="current-grouping-key()"/>
      </td>
      <td>
	<xsl:value-of select="count(current-group())"/>
      </td>
      <td>
	<xsl:for-each-group select="current-group()" group-by="@n">
	  <xsl:value-of select="current-grouping-key()"/><xsl:text> </xsl:text>
	</xsl:for-each-group>
      </td>
      </tr>
    </xsl:for-each-group>
  </table>
</xsl:template>

<xsl:template match="tei:front">
  <div class="front">
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="tei:back">
  <div class="back">
    <xsl:apply-templates/>
  </div>

</xsl:template>

<xsl:template match="tei:divGen[@type='namelist']">
  <table class="sort">
    <thead>
      <tr><th>Code</th><th>Occurrences</th><th>Noms</th></tr>
    </thead>
    <xsl:for-each-group select="//tei:text//tei:persName" group-by="tei:Maybe(@key)">
      <xsl:sort select="."/>
      <tr>
	<td><xsl:value-of select="current-grouping-key()"/></td>
	<td><xsl:value-of select="count(current-group())"/></td>
	<td><xsl:for-each-group select="current-group()" group-by=".">
	  <xsl:sort select="."/>
	  <xsl:value-of select="current-grouping-key()"/>
	  <xsl:text> (</xsl:text>
	  <xsl:value-of select="count(current-group())"/>
	  <xsl:text>) </xsl:text>
	</xsl:for-each-group>
	</td>
      </tr>
    </xsl:for-each-group>
  </table>
</xsl:template>

  <xsl:template name="javascriptHook">
    <script src="{$OU}jquery-1.10.2.min.js" type="text/javascript">
      <xsl:comment>brk</xsl:comment>
    </script>
    <script src="{$OU}jquery-ui-1.10.3.custom.js" type="text/javascript">
      <xsl:comment>brk</xsl:comment>
    </script>
    <script src="{$OU}jquery.dataTables.min.js" type="text/javascript">
      <xsl:comment>brk</xsl:comment>
    </script>
    <script type="text/javascript">
      $(document).ready(function() {
	var oTable = $('.sort').dataTable( {
	"sPaginationType": "full_numbers",
	"bPaginate": true,
	"bLengthChange": true,
	"bAutoWidth": false,
	"bFilter": true,
	"bSort": true,
	"bInfo": true,
	"aaSorting": [ ],
	"bScrollCollapse": true,
	"bJQueryUI": true,
	"sDom": 'flprtip'})});
    </script>
  </xsl:template>

  <xsl:function name="tei:Maybe" as="xs:string*">
    <xsl:param name="val"/>
    <xsl:choose>
      <xsl:when test="$val=''">-</xsl:when>
      <xsl:otherwise><xsl:value-of select="$val"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

</xsl:stylesheet>

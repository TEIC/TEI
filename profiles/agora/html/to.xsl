<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
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
  <xsl:param name="numberBackHeadings"></xsl:param>
   <xsl:param name="numberFigures">false</xsl:param>
   <xsl:param name="numberTables">false</xsl:param>
  <xsl:param name="numberParagraphs">false</xsl:param>
  <xsl:param name="generateParagraphIDs">true</xsl:param>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="cssInlineFiles">../profiles/agora/html/tei-agora.css</xsl:param>
  <xsl:param name="cssFile"/>
  <xsl:param name="institution">AGORA Project</xsl:param>
   <xsl:param name="bottomNavigationPanel">false</xsl:param>
  <xsl:param name="linkPanel">false</xsl:param>
   <xsl:param name="footnoteBackLink">true</xsl:param>
   <xsl:param name="homeURL"></xsl:param>
  <xsl:param name="feedbackURL"></xsl:param>
   <xsl:param name="homeWords">AGORA</xsl:param>
 

<!-- dont wrap quotes in quotes -->
<xsl:param name="preQuote"/>
<xsl:param name="postQuote"/>


<xsl:template name="copyrightStatement">
<xsl:value-of select="//tei:sourceDesc/tei:title[@level='a']"/>
<xsl:text>This page is made available under the Creative Commons General Public License "Attribution, Non-Commercial, Share-Alike", version 3.0 (CCPL BY-NC-SA) </xsl:text>
</xsl:template>


<!-- number paragraphs -->
  <xsl:template name="numberParagraph">
    <xsl:if test="ancestor::tei:body">
      <span class="numberParagraph">      
	<xsl:number level="any" from="tei:body"/>
      </span>
    </xsl:if>
  </xsl:template>

<!-- suppress pb -->
<xsl:template match="tei:pb"/>
    
<!-- deal with weird @rend values -->
<xsl:template match="tei:hi[tei:match(@rend,'del')]">
<s><xsl:apply-templates/></s>
</xsl:template>
<xsl:template match="tei:hi[tei:match(@rend,'ul2')]">
<u style="border-bottom: 1px double #000"><xsl:apply-templates/></u>
</xsl:template>

<xsl:template match="tei:hi[tei:match(@rend,'ulw')]">
<u style="border-bottom: 1px dotted #000"><xsl:apply-templates/></u>
</xsl:template>

<xsl:template match="tei:hi[tei:match(@rend,'shadow')]">
<u style="background-color: gray"><xsl:apply-templates/></u>
</xsl:template>

<xsl:template match="tei:lb[tei:match(@rend,'indent')]">
<br/><xsl:text>    </xsl:text>
</xsl:template>

<!-- also weird list types -->

<xsl:template match="tei:list[@type='number']">
<ol>
          <xsl:apply-templates select="tei:item"/>
</ol></xsl:template>

<xsl:template match="tei:list[@type='simple']">
<ul style="list-style:none">
          <xsl:apply-templates select="tei:item"/>
</ul></xsl:template>


<!--
<xsl:template match="tei:ref">
  <span class="ref"><xsl:apply-templates/></span>
  <span class="contextaRef"><xsl:value-of select="@cRef"/></span>
</xsl:template>
-->

<!-- add a space in front of surname inside author -->

<xsl:template
    match="tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author/tei:surname">
<xsl:text> </xsl:text>
<xsl:apply-templates/>
</xsl:template>

  <xsl:template match="tei:div[@type='bibliography']">
    <div class="refs">
      <xsl:if test='not(tei:head)'>
	<h2><span class="head">References</span></h2>
      </xsl:if>
      <xsl:apply-templates/>
    </div>  
  </xsl:template>


  <xsl:template match="tei:div[@type='abstract']">
    <div class="abstract">
      <xsl:if test='not(tei:head)'>
	<h2><span class="head">Abstract</span></h2>
      </xsl:if>
      <xsl:apply-templates/>
    </div>  
  </xsl:template>

<xsl:template match="tei:byLine">
<xsl:message>suppressing and</xsl:message>
<xsl:apply-templates/>
</xsl:template>


<!-- these seem to be inherited -->
    <xsl:template match="html:*">
      <xsl:element name="{local-name()}">
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates/>
      </xsl:element>
    </xsl:template>
    
    <xsl:template match="html:*/comment()">
      <xsl:copy-of select="."/>
    </xsl:template>

  <xsl:template match="tei:div[@type='frontispiece']">
      <xsl:apply-templates/>
  </xsl:template>


</xsl:stylesheet>

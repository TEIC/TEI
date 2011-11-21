<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../xhtml2/tei.xsl"/>

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
         <p>Id: $Id: to.xsl 8923 2011-05-25 13:11:45Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"/>

<!-- set stylesheet parameters -->
   <xsl:param name="numberHeadings">false</xsl:param>
   <xsl:param name="numberFigures">false</xsl:param>
   <xsl:param name="numberTables">false</xsl:param>
  <xsl:param name="numberParagraphs">false</xsl:param>
  <xsl:param name="generateParagraphIDs">true</xsl:param>
  <xsl:param name="autoToc">false</xsl:param>
  
  <xsl:param name="CSSFile">tei-agora.css</xsl:param>
  <xsl:param name="institution">AGORA Project</xsl:param>

   <xsl:param name="bottomNavigationPanel">false</xsl:param>
   <xsl:param name="footnoteBackLink">true</xsl:param>
  
   <xsl:param name="homeURL"></xsl:param>
  <xsl:param name="feedbackURL"></xsl:param>
  <xsl:param name="searchURL"></xsl:param>
   <xsl:param name="homeWords"></xsl:param>
   <xsl:param name="parentWords"></xsl:param>


<!-- suppress pb -->
<xsl:template match="tei:pb"/>
    
<!-- deal with weird @rend values -->
<xsl:template match="tei:hi[@rend='del']">
<s><xsl:apply-templates/></s>
</xsl:template>
<xsl:template match="tei:hi[@rend='ul2']">
<u style="border-bottom: 1px double #000"><xsl:apply-templates/></u>
</xsl:template>
<xsl:template match="tei:hi[@rend='ulw']">
<u style="border-bottom: 1px dotted #000"><xsl:apply-templates/></u>
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

<xsl:template match="tei:ref">
  <xsl:message>hello</xsl:message>
  <span class="ref"><xsl:value-of select="."></xsl:value-of></span>
  <span class="contextaRef"><xsl:value-of select="@cRef"/></span>
  
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

  <xsl:template match="tei:div[@type='illustration']">
      <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>

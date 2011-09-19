<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
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
         <p>Id: $Id: to.xsl 8352 2011-01-07 17:41:55Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xhtml" omit-xml-declaration="yes"/>
    
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="numberHeadings"  as="xs:boolean" select="true()"/>
  <xsl:param name="googlestylesheet">oxford</xsl:param>
  <xsl:param name="cssPrintFile">/ota-print.css</xsl:param>
  <xsl:param name="ROOT">http://www.oucs.ox.ac.uk</xsl:param>
  <xsl:param name="OUCSBASE">http://www.oucs.ox.ac.uk</xsl:param>
  <xsl:param name="cssFile">../profiles/oepack/html/otatext.css</xsl:param>
  <xsl:param name="sort">author</xsl:param>
  <xsl:param name="htmlTitlePrefix">[OTA] </xsl:param>

  <xsl:template name="additionalMenu">
    <ul class="OTAnav">
      <li class="navLabel"><span class="bold">O</span>xford <span class="bold"
        >T</span>ext <span class="bold">A</span>rchive: </li>
      <li class="navLink">
        <a href="/">Home</a> | </li>
      <li class="navLink">
        <a href="/about/">About</a> | </li>
      <li class="navLink">
        <a href="/about/news.xml">News</a> | </li>
      <li class="navLink">
        <a href="/catalogue/index-id.html">Catalogue</a> | </li>
      <li class="navLink">
        <a href="/about/contact.xml">Contact</a> | </li>
      <li class="navLink">
        <a href="/about/faq.xml">Help and FAQ</a> | </li>
      <li class="navLink">
        <a href="/about/search.xml">Search OTA</a>
      </li>
    </ul>
  </xsl:template>

  <xsl:template name="stdfooter">
    <div class="clear" id="footer">
      <p>
        <a href="/about/contact.xml">Contact and Feedback</a> | <a
          href="http://www.oucs.ox.ac.uk/enable/acstatement.xml"
        >Accessibility</a> | <a href="http://www.oucs.ox.ac.uk/">OUCS</a> | <a
          href="http://www.ox.ac.uk/copyright/">&#169;</a> University of
        Oxford. <span class="toclist">
          <a href="http://www.ox.ac.uk/">
            <img alt="University Crest" height="71"
              src="http://www.oucs.ox.ac.uk/images/crest-outline.gif"
              title="Oxford University home page" width="60"/>
          </a>
        </span></p>
    </div>
  </xsl:template>

  <xsl:template name="mainPage">
    <xsl:param name="currentID"/>
    <div class="show-all" id="main">
      <!-- header -->

      <div id="hdr">
        <xsl:call-template name="hdr"/>
      </div>

      <div id="mainMenu">
	<xsl:call-template name="additionalMenu"/>
      </div>


      <div id="onecol" class="main-content">
        <h1>
          <xsl:call-template name="generateTitle"/>
        </h1>

	<xsl:choose>
	  <xsl:when test="local-name(.)='div'">
	    <h1>
	      <xsl:apply-templates mode="section" select="tei:head"/>
	    </h1>
	    <xsl:apply-templates/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="mainFrame">
	      <xsl:with-param name="currentID" select="$currentID"/>
	      <xsl:with-param name="minimal" select="true()"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </div>


      <div class="clear" id="em"/>
    </div>

    <div>
      <xsl:call-template name="stdfooter"/>
    </div>

  </xsl:template>

</xsl:stylesheet>

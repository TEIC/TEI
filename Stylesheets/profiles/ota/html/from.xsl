<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../tools/html2tei.xsl"/>

  <xsl:param name="NAME"/>
  <xsl:variable name="Q">'</xsl:variable>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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

  <xsl:template match="head">
    <xsl:variable name="T" select="replace(translate(title,$Q,''), ' /  Rudyard Kipling','')"/>
    <teiHeader>
      <fileDesc>
        <titleStmt>
          <title>
            <xsl:value-of select="title"/>
          </title>
          <author>
            <xsl:value-of select="meta[@name='dc.Creator']/@content"/>
          </author>
        </titleStmt>
        <publicationStmt>
          <distributor>
            <name>Oxford Text Archive</name>
            <address>
              <addrLine>Oxford University Computing Services</addrLine>
              <addrLine>13 Banbury Road</addrLine>
              <addrLine>Oxford</addrLine>
              <addrLine>OX2 6NN</addrLine>
            </address>
            <email>ota@oucs.ox.ac.uk</email>
          </distributor>
          <idno type="ota">http://ota.ox.ac.uk/id/3001</idno>
          <availability status="restricted">
            <p>
              <ref type="license" target="http://creativecommons.org/licenses/by-nc-sa/3.0/"><graphic url="http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png"/>
	      Distributed by the University of Oxford under a Creative Commons
	    Attribution-NonCommercial-ShareAlike 3.0 Unported License</ref>
            </p>
          </availability>
        </publicationStmt>
        <sourceDesc>
          <bibl>TEI XML version of  
	<relatedItem type="otherVersion"><xsl:attribute name="target"><xsl:value-of select="meta[@name='dc.Identifier']/@content"/></xsl:attribute></relatedItem>
	</bibl>
        </sourceDesc>
      </fileDesc>
      <profileDesc>
        <creation>
          <date>
</date>
        </creation>
        <langUsage>
          <language ident="eng">English</language>
        </langUsage>
      </profileDesc>
    </teiHeader>
  </xsl:template>

  <xsl:template match="div[@class='colophon']"/>
  <xsl:template match="div[@class='titlepage']"/>
  <xsl:template match="div[@class='titleverso']"/>
  <xsl:template match="div[@class='contents']"/>

  <xsl:template match="div[@class='section center c1']">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="div[@class='section center']">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="div[@class='c1']">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="div[@class='center' or @class=' center']">
    <xsl:choose>
      <xsl:when test="p">
        <xsl:for-each select="p">
          <p rend="center">
            <xsl:apply-templates/>
          </p>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <p rend="center">
          <xsl:apply-templates/>
        </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="div[@class='c2']">
    <signed>
      <xsl:apply-templates/>
    </signed>
  </xsl:template>
  <xsl:template match="div[@class='epigraph']">
    <xsl:choose>
      <xsl:when test="preceding-sibling::p">
        <quote rend="display">
          <xsl:apply-templates/>
        </quote>
      </xsl:when>
      <xsl:otherwise>
        <epigraph>
          <xsl:choose>
            <xsl:when test="count(p)&gt;1">
              <cit>
                <quote>
                  <xsl:for-each select="p[not(position()=last())]">
                    <p>
                      <xsl:apply-templates/>
                    </p>
                  </xsl:for-each>
                </quote>
                <bibl>
                  <xsl:for-each select="p[last()]">
                    <xsl:apply-templates/>
                  </xsl:for-each>
                </bibl>
              </cit>
            </xsl:when>
            <xsl:otherwise>
              <quote rend="display">
                <xsl:apply-templates/>
              </quote>
            </xsl:otherwise>
          </xsl:choose>
        </epigraph>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="div[@class='quote']">
    <xsl:choose>
      <xsl:when test="count(p)&gt;1">
        <cit>
          <quote>
            <xsl:for-each select="p[not(last())]">
              <xsl:apply-templates/>
            </xsl:for-each>
          </quote>
          <bibl>
            <xsl:for-each select="p[last()]">
              <xsl:apply-templates/>
            </xsl:for-each>
          </bibl>
        </cit>
      </xsl:when>
      <xsl:when test="p/br and not(preceding-sibling::p or preceding-sibling::div) ">
        <xsl:for-each select="p">
          <epigraph>
            <xsl:call-template name="linesofverse"/>
          </epigraph>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="preceding-sibling::p or preceding-sibling::h3">
        <quote rend="display">
          <xsl:apply-templates/>
        </quote>
      </xsl:when>
      <xsl:when test="p/br">
        <xsl:for-each select="p">
          <lg>
            <xsl:call-template name="linesofverse"/>
          </lg>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <quote rend="display">
          <xsl:apply-templates/>
        </quote>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="div[@class='stanza']">
    <quote rend="display" type="stanza">
      <xsl:for-each select="p">
        <l>
          <xsl:apply-templates/>
        </l>
      </xsl:for-each>
    </quote>
  </xsl:template>
  <xsl:template match="div[@class='inscription']">
    <quote rend="display" type="inscription">
      <xsl:apply-templates/>
    </quote>
  </xsl:template>
  <xsl:template match="div[@class='notice']">
    <quote rend="display" type="notice">
      <xsl:apply-templates/>
    </quote>
  </xsl:template>
  <xsl:template match="div[@class='letter quote']">
    <quote rend="display" type="letter">
      <xsl:apply-templates/>
    </quote>
  </xsl:template>
  <xsl:template match="div[@class='poem']">
    <xsl:for-each select="p">
      <lg>
        <xsl:call-template name="linesofverse"/>
      </lg>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="div[@class='illustration']">
    <figure>
      <xsl:apply-templates/>
    </figure>
  </xsl:template>

  <xsl:template match="div/text()">
    <xsl:choose>
      <xsl:when test="string-length(normalize-space(.))=0"/>
      <xsl:when test="starts-with(.,'*')">
        <p>
          <xsl:value-of select="."/>
        </p>
      </xsl:when>
      <xsl:when test="starts-with(.,'. .')">
        <p>
          <xsl:value-of select="."/>
        </p>
      </xsl:when>
      <xsl:when test="parent::div[@class='c2']">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="parent::div[@class='center']">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="parent::div[@class='section center']">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <head>
          <xsl:value-of select="."/>
        </head>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="linesofverse">
    <xsl:for-each-group select="*|text()" group-ending-with="br">
      <l>
        <xsl:apply-templates select="current-group()[not(self::br)]">
	  </xsl:apply-templates>
      </l>
    </xsl:for-each-group>
  </xsl:template>


  <xsl:template match="div[@class='section']">
    <xsl:choose>
      <xsl:when test="h3|h4|h5|h6">
        <div>
          <xsl:apply-templates select="@*|*|text()"/>
        </div>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="div">
    <div>
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xsl:template match="h1">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  <xsl:template match="h2">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  <xsl:template match="h3">
    <head>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  <xsl:template match="h4|h5|h6">
    <xsl:choose>
      <xsl:when test="starts-with(.,'. .')">
        <p>
          <xsl:apply-templates/>
        </p>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::*)">
        <head>
          <xsl:apply-templates/>
        </head>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::p)">
        <byline>
          <xsl:apply-templates/>
        </byline>
      </xsl:when>
      <xsl:otherwise>
        <p rend="head">
          <xsl:apply-templates/>
        </p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

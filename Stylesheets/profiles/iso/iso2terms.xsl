<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="tei"
                version="2.0">
   <xsl:import href="isoutils.xsl"/>
   <xsl:import href="html/to.xsl"/>
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
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>



   <xsl:output method="xhtml" encoding="utf-8"/>


   <xsl:key name="DIV" match="tei:div" use="@type"/>

   <xsl:template match="tei:TEI">
      <xsl:variable name="today">
         <xsl:call-template name="whatsTheDate"/>
      </xsl:variable>
      <xsl:variable name="isotitle">
         <xsl:call-template name="generateTitle"/>
      </xsl:variable>
      <xsl:variable name="isonumber">
         <xsl:call-template name="getiso_documentNumber"/>
      </xsl:variable>
      <xsl:variable name="isopart">
         <xsl:call-template name="getiso_partNumber"/>
      </xsl:variable>
      <xsl:variable name="isoyear">
         <xsl:call-template name="getiso_year"/>
      </xsl:variable>
      <html>
         <head>
            <title>Report on 
      <xsl:value-of select="$isotitle"/>:
      <xsl:value-of select="$isoyear"/>:
      <xsl:value-of select="$isonumber"/>:
      <xsl:value-of select="$isopart"/>
            </title>
            <link href="iso.css" rel="stylesheet" type="text/css"/>
         </head>
         <body>
            <h1 class="maintitle">
               <xsl:value-of select="$isotitle"/>:
      <xsl:value-of select="$isoyear"/>:
      <xsl:value-of select="$isonumber"/>:
      <xsl:value-of select="$isopart"/>
            </h1>
    
            <xsl:for-each select="key('DIV','termsAndDefinitions')">
	              <xsl:apply-templates select="."/>
            </xsl:for-each>

            <xsl:for-each select="key('DIV','normativeReferences')">
	              <xsl:apply-templates select="."/>
            </xsl:for-each>
         </body>
      </html>
   </xsl:template>

   <xsl:template match="tei:div/tei:head"/>

   <xsl:template match="tei:div">
      <h3>
         <xsl:value-of select="tei:head"/>
      </h3>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:list[@type='termlist']/tei:item">
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:list[@type='termlist']/tei:item/tei:term">
      <dt>
    (<xsl:value-of select="parent::tei:item/@n"/>)
    <xsl:apply-templates/>
      </dt>
   </xsl:template>

   <xsl:template match="tei:list[@type='termlist']">
      <dl>
         <xsl:apply-templates/>
      </dl>
   </xsl:template>


   <xsl:template match="tei:list[@type='termlist']/tei:item/tei:gloss">
      <dd>
         <xsl:apply-templates/>
      </dd>
   </xsl:template>

   <xsl:template match="tei:note">
      <xsl:text>[</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>]</xsl:text>
   </xsl:template>

   <xsl:template match="tei:p">
      <p>  
         <xsl:apply-templates/>
      </p>
   </xsl:template>

   <xsl:template match="tei:bibl">
      <li>
         <xsl:apply-templates/>
      </li>
   </xsl:template>

   <xsl:template match="tei:listBibl">
      <ol>
         <xsl:apply-templates/>
      </ol>
   </xsl:template>

</xsl:stylesheet>
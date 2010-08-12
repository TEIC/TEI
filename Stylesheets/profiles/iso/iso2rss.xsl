<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns="http://www.w3.org/2005/Atom"
                exclude-result-prefixes="xhtml tei"
                version="2.0">
   <xsl:import href="isoutils.xsl"/>
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


  <xsl:variable name="today">
      <xsl:call-template name="whatsTheDate"/>
  </xsl:variable>

   <xsl:key name="TEI" use="1" match="tei:TEI"/>

   <xsl:template match="/">
      <feed>
         <id>http://tei.oucs.ox.ac.uk/TEIISO/iso2atom.rss</id>
         <link href="http://tei.oucs.ox.ac.uk/TEIISO"/>
         <link rel="self" href="http://tei.oucs.ox.ac.uk/TEIISO/iso2atom.rss"/>
         <title>ISO Standards</title>
         <subtitle>  </subtitle>
         <updated>
            <xsl:value-of select="$today"/>
         </updated>
         <category term="standard"/>
         <xsl:apply-templates select="key('TEI',1)"/>
      </feed>
   </xsl:template>

   <xsl:template match="tei:TEI">
      <xsl:variable name="isotitle">
         <xsl:call-template name="generateTitle"/>
      </xsl:variable>
      <xsl:variable name="isoauthority">
         <xsl:call-template name="getiso_authority"/>
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

      <entry>
         <author>
            <name>
	              <xsl:value-of select="$isoauthority"/>
            </name>
         </author>
         <content type="xhtml">
            <xhtml:div>
	              <xsl:for-each select="ancestor-or-self::tei:TEI/tei:text/tei:body/tei:div[@type='scope']">
	                 <xsl:apply-templates/>
	              </xsl:for-each>
            </xhtml:div>
         </content>
         <id>
            <xsl:value-of select="$isoauthority"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$isonumber"/>
            <xsl:if test="not($isopart='')">
	              <xsl:text>-</xsl:text>
	              <xsl:value-of select="$isopart"/>
            </xsl:if>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$isoyear"/>
         </id>
         <link>  
            <xsl:attribute name="href">
	              <xsl:text>http://tei.oucs.ox.ac.uk/TEIISO/samples/</xsl:text>
	              <xsl:value-of select="$isonumber"/>
	              <xsl:text>/</xsl:text>
	              <xsl:value-of select="$isonumber"/>
	              <xsl:text>.xml</xsl:text>
            </xsl:attribute>
         </link>

         <published>
            <xsl:value-of select="$today"/>
         </published>
    
         <summary>
            <xsl:value-of select="$isotitle"/>
         </summary>
         <title>
            <xsl:value-of select="$isonumber"/>
            <xsl:if test="not($isopart='')">
	              <xsl:text>-</xsl:text>
	              <xsl:value-of select="$isopart"/>
            </xsl:if>
            <xsl:text> </xsl:text>
            <xsl:value-of select="$isoyear"/>
         </title>
         <updated>
            <xsl:value-of select="$today"/>
         </updated>
      </entry>

   </xsl:template>
</xsl:stylesheet>
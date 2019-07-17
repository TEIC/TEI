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
         <xsl:sequence select="tei:generateTitle(.)"/>
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

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    exclude-result-prefixes="tei tbx">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
      <p> This library is free software; you can redistribute it and/or modify it under the
      terms of the GNU Lesser General Public License as published by the Free Software Foundation;
      either version 2.1 of the License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
      implied warranty of MAINTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
      General Public License for more details. You should have received a copy of the GNU Lesser
      General Public License along with this library; if not, write to the Free Software Foundation,
      Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
      <p>Author: See AUTHORS</p>
      <p>Id: $Id$</p>
      <p>Copyright: 2008, TEI Consortium</p>
    </desc>
  </doc>

<xsl:key name="T" match="tbx:termEntry" use="1"/>

<xsl:output method="xml" indent="yes" encoding="utf-8"/>

<xsl:template match="/">
  <martif type="TBX" xml:lang="en">
    <martifHeader>
        <fileDesc>
            <sourceDesc>
                <p>from an ISO standard</p>
            </sourceDesc>
        </fileDesc>
        <encodingDesc>
            <p type="XCSURI">http://www.lisa.org/fileadmin/standards/tbx/TBXXCSV02.XCS</p>
        </encodingDesc>
    </martifHeader>
    <text>
      <body>
	<xsl:for-each select="key('T',1)">
	  <xsl:copy>
	    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
	  </xsl:copy>
	</xsl:for-each>
      </body>
    </text>
  </martif>
</xsl:template>


   <xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
   </xsl:template>


   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="tei:*">
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()"/>
   </xsl:template>


</xsl:stylesheet>





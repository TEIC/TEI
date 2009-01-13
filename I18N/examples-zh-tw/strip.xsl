<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  exclude-result-prefixes="tei teix"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  version="1.0"
>
<!-- This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
      02111-1307 USA 

$Id: iden.xsl 4549 2008-04-23 16:40:01Z rahtz $

2008, TEI Consortium
-->


<!-- identity transform -->
<xsl:output method="xml" encoding="utf-8" indent="yes"/>

<xsl:template match="@*|text()|comment()|processing-instruction()">
 <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="teix:egXML[not(@xml:lang)]"/>

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()"/>
  </xsl:copy>
</xsl:template>


</xsl:stylesheet>

<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
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

$Id$

2008, TEI Consortium
-->
<xsl:output method="xml" indent="yes"/>
<xsl:param name="start">1</xsl:param>
<xsl:param name="end">2</xsl:param>

<xsl:template match="tei:pb">
  <xsl:if test="@n=$start">
    <xsl:copy-of select="."/>
  </xsl:if>
</xsl:template>

<xsl:template match="text()">
  <xsl:choose>
    <xsl:when test="preceding::tei:pb[@n=$end]">
    </xsl:when>
    <xsl:when test="following::tei:pb[@n=$start]">
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="*">
  <xsl:choose>
    <xsl:when test=".//tei:pb[@n=$start or @n=$end]">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates/>
      </xsl:copy>
    </xsl:when>
    <xsl:when test="following::tei:pb[@n=$end] and
		    preceding::tei:pb[@n=$start]">
      <xsl:copy-of select="."/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
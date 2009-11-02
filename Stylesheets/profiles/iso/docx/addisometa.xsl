<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei iso">

  <doc type="stylesheet" xmlns="http://www.pnp-software.com/XSLTdoc">
    <short>TEI stylesheet to convert TEI XML to Word DOCX XML.</short>
    <detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  
      </detail>
    <author>See AUTHORS</author>
    <cvsId>$Id$</cvsId>
    <copyright>2008, TEI Consortium</copyright>
  </doc>
  
<!-- identity transform -->

<xsl:template match="@*|text()|comment()|processing-instruction()" >
  <xsl:copy-of select="."/>
</xsl:template>


<xsl:template match="*" >
  <xsl:copy>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()" />
  </xsl:copy>
</xsl:template>


<xsl:template match="tei:title[@type='introductory']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">introductory_title</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:title[@type='main']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">main_title</xsl:with-param>
  </xsl:call-template>
  <xsl:copy>
    <xsl:attribute name="iso:meta">main_title</xsl:attribute>
    <xsl:apply-templates 
	select="*|@*|processing-instruction()|comment()|text()" />
  </xsl:copy>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:date">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">docdate</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:publisher">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">organization</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:authority">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">secretariat</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='partNumber']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">partNumber</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='documentNumber']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">referenceNumber</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='scnum']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">scnum</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='serialNumber']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">serialNumber</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='tcnum']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">tcnum</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="tei:publicationStmt/tei:idno[@type='wgNumber']">
  <xsl:call-template name="meta">
    <xsl:with-param name="tag">wgNumber</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template name="meta">
  <xsl:param name="tag"/>
    <xsl:copy>
      <xsl:attribute name="iso:meta">
	<xsl:value-of select="$tag"/>
      </xsl:attribute>
      <xsl:apply-templates 
	  select="*|@*|processing-instruction()|comment()|text()" />
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>

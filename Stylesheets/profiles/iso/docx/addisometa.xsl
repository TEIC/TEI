<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:iso="http://www.iso.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei iso">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert TEI XML to Word DOCX XML.</p>
         <p>
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
  
      </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  
   <!-- identity transform -->

<xsl:template match="@*|text()|comment()|processing-instruction()">
      <xsl:copy-of select="."/>
   </xsl:template>


   <xsl:template match="*">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>


   <xsl:template match="tei:titleStmt/tei:title[@type='introductory']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">introductoryTitle</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:titleStmt/tei:title[@type='main']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">mainTitle</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:publicationStmt/tei:date">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">docDate</xsl:with-param>
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

   <xsl:template match="tei:publicationStmt/tei:idno[@type='docPartNumber']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">docPartNumber</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:publicationStmt/tei:idno[@type='docNumber']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">docNumber</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:publicationStmt/tei:idno[@type='scNumber']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">scNumber</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:publicationStmt/tei:idno[@type='serialNumber']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">serialNumber</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:publicationStmt/tei:idno[@type='tcNumber']">
      <xsl:call-template name="meta">
         <xsl:with-param name="tag">tcNumber</xsl:with-param>
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
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
   </xsl:template>

</xsl:stylesheet>
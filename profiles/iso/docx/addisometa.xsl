<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:iso="http://www.iso.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="tei iso">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert TEI XML to Word DOCX XML.</p>
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

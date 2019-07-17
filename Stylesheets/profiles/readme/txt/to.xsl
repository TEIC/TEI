<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->
    <xsl:import href="../../../txt/tei-to-text.xsl"/>

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
         <p>Id: $Id: to.xsl 8923 2011-05-25 13:11:45Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output indent="yes" method="text" />

   <xsl:template match="text()">
     <xsl:choose>
       <xsl:when test= "normalize-space()=''"/>
       <xsl:otherwise>
	 <xsl:value-of select="normalize-space()"/>
	 <xsl:text> </xsl:text>
       </xsl:otherwise>

</xsl:choose>    </xsl:template>

<xsl:template match="tei:gi">&lt;<xsl:value-of select="."/>&gt; </xsl:template>
<xsl:template match="tei:hi">*<xsl:value-of select="."/>* </xsl:template>

<xsl:template match="tei:TEI">
<xsl:value-of select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
<xsl:text>
</xsl:text><xsl:apply-templates/>
</xsl:template>


<xsl:template match="tei:div">
<xsl:text>

</xsl:text>
<xsl:number/><xsl:text>  </xsl:text>
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:head">
<xsl:apply-templates/>
<xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="tei:item">
<xsl:text>
 * </xsl:text><xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:list[tei:isGlossList(.)]/tei:item">
<xsl:text>
  </xsl:text><xsl:number/><xsl:text>  </xsl:text><xsl:apply-templates/>
</xsl:template>

<xsl:template match="tei:p">
<xsl:text>
  </xsl:text><xsl:apply-templates/>
</xsl:template>
</xsl:stylesheet>

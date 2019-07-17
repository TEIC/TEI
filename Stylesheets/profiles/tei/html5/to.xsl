<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:skos="http://www.w3.org/2004/02/skos/core#"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html skos"
    version="2.0">
    <!-- import base conversion style -->
    <xsl:import href="../../../html5/html5.xsl"/>
    <xsl:import href="../../../html5/microdata.xsl"/>

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

  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="institution"></xsl:param>
  <xsl:param name="feedbackURL"/>
  <xsl:param name="searchURL"/>
  <xsl:template name="copyrightStatement"></xsl:template>
  <xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
  <xsl:param name="parentWords">TEI</xsl:param>

<xsl:template match="skos:exactMatch">
  <tt>'<xsl:value-of select="."/>'</tt>
</xsl:template>

<xsl:template match="tei:valItem">
  <tr>
    <td><xsl:sequence select="tei:showMode(@ident,@mode)"/>
	  <xsl:if test="tei:paramList">
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="tei:paramList/tei:paramSpec/@ident" separator=","/>
	    <xsl:text>)</xsl:text>
	  </xsl:if>
    </td>
    <td><xsl:value-of select="tei:desc"/>
    <xsl:if test="skos:exactMatch">
      [also   <xsl:apply-templates select="skos:exactMatch"/>]
    </xsl:if>
    </td>
  </tr>
</xsl:template>

<xsl:template match="tei:att">
    <span>
      <xsl:call-template name="makeRendition"/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

</xsl:stylesheet>

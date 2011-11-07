<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei fotex m"
                version="2.0">
  <xsl:import href="../common2/tei.xsl"/>
  <xsl:import href="tei-param.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet making XSL-FO output.
      </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
    Stylesheet constant for the input document.
  </desc>
   </doc>
  <xsl:variable name="top" select="/"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="string">
      <desc>
    Stylesheet constant for table specifications
  </desc>
   </doc>
  <xsl:variable name="tableSpecs">
      <xsl:choose>
         <xsl:when test="$readColSpecFile">
            <xsl:copy-of select="document($readColSpecFile,$top)/Info"/>
         </xsl:when>
         <xsl:otherwise>
            <Info/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:output indent="no" encoding="utf-8"/>
  <xsl:strip-space elements="tei:cell"/>
  <xsl:key name="DIVS"
            match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5"
            use="'1'"/>
  <xsl:include href="tei-makecolspec.xsl"/>
  <xsl:include href="core.xsl"/>
  <xsl:include href="corpus.xsl"/>
  <xsl:include href="drama.xsl"/>
  <xsl:include href="figures.xsl"/>
  <xsl:include href="header.xsl"/>
  <xsl:include href="linking.xsl"/>
  <xsl:include href="namesdates.xsl"/>
  <xsl:include href="tagdocs.xsl"/>
  <xsl:include href="textcrit.xsl"/>
  <xsl:include href="textstructure.xsl"/>
  <xsl:include href="transcr.xsl"/>
  <xsl:include href="verse.xsl"/>
</xsl:stylesheet>
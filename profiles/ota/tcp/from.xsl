<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet                 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    version="2.0"
    exclude-result-prefixes="tei">

    <xsl:import href="../../../tcp/tcp2tei.xsl"/>

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

   <xsl:param name="OTANUMBER"/>
   <xsl:template name="makeID">
     <idno type="ota">
       <xsl:text>http://ota.ox.ac.uk/id/</xsl:text>a
       <xsl:value-of select="$OTANUMBER"/>
     </idno>
   </xsl:template>

  <xsl:template match="fileDesc/publicationStmt">
    <publicationStmt  xmlns="http://www.tei-c.org/ns/1.0">
      <distributor>
	<name>Oxford Text Archive</name>
	<address>
	  <addrLine>Oxford University Computing Services</addrLine>
	  <addrLine>13 Banbury Road</addrLine>
	  <addrLine>Oxford</addrLine>
            <addrLine>OX2 6NN</addrLine>
	</address>
	<email>ota@oucs.ox.ac.uk</email>
      </distributor>
      <xsl:for-each select="idno">
	<idno>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates/>
	</idno>
      </xsl:for-each>
      <availability status="restricted" xmlns="http://www.tei-c.org/ns/1.0">
	<licence target="http://creativecommons.org/licenses/by-sa/3.0/">
	  Distributed by the University of Oxford under a Creative Commons
	  Attribution-ShareAlike 3.0 Unported License
	</licence>
      </availability>
    </publicationStmt>
  </xsl:template>

  <xsl:template name="idnoHook">
    <xsl:for-each select="doc('../web/isbns.xml')">
      <xsl:message>check ISBN for <xsl:value-of select="$OTANUMBER"/></xsl:message>
      <xsl:for-each select="//tei:isbn[@n=$OTANUMBER]">
       	<idno type="isbn10">
	  <xsl:value-of select="@isbn10"/>
	</idno>
	<idno type="isbn13">
	  <xsl:value-of select="@isbn13"/>
	</idno>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

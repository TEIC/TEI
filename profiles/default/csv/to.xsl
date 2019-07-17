<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:tei="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="xs tei">

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
         <p>Author: James Cummings </p>
         
         <p>Copyright: 2010, TEI Consortium</p>
	<p>
		Usage:  saxon filename.xsl tei2csv.xsl 
		<ul>
		<li> This produces files named tableX.csv
		where X is the number of the table in the 
		document as a whole.</li>
		<li> All other content is discarded</li>
		<li> All output is in UTF-8, input-encoding is 
		based on XML declaration in the original file.
		</li>
		</ul>
	</p>
      </desc>
   </doc>
   <xsl:output omit-xml-declaration="yes"/>
   <xsl:param name="filePrefix">/table</xsl:param>
   <xsl:variable name="dq">"</xsl:variable>
   <xsl:variable name="qdq">""</xsl:variable>

  <xsl:param name="directory">.</xsl:param>

   <xsl:template match="/">
     <xsl:for-each select="//tei:table">
       <xsl:variable name="tableNum">
	 <xsl:number level="any"/>
       </xsl:variable>
       <xsl:variable name="dest" select="concat($directory,$filePrefix,$tableNum, '.csv')"/>
       <xsl:message>Writing <xsl:value-of select="$dest"/></xsl:message>
       <xsl:result-document href="{$dest}"
			    encoding="UTF-8" method="text">
	 <xsl:for-each select="tei:row">
	   <xsl:for-each select="tei:cell">
	     <xsl:value-of select="concat($dq,replace(normalize-space(.),$dq,$qdq),$dq)"/>
	     <xsl:if test="following-sibling::tei:cell[1]">,</xsl:if>
	   </xsl:for-each>
	   <xsl:text>&#xa;</xsl:text>
	 </xsl:for-each>
       </xsl:result-document>
     </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>

<?xml version="1.0"?>

<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:tei="http://www.tei-c.org/ns/1.0"
	xmlns:jc="http://james.blushingbunny.net/ns.html"
	exclude-result-prefixes="xs tei jc">
	<xsl:param name="input-encoding" select="'UTF-8'" as="xs:string"/>
	<xsl:output indent="yes" encoding="UTF-8"/>
	<xsl:param name="input-uri" select="'filename.csv'"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
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
         <p>Author: James Cummings</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2010, TEI Consortium</p>
<p>
Usage:  saxon -it main styesheet.xsl input-uri=filename.csv

<ul>
<li> It assumes cells are comma separated and 
rows are on individual lines...technically a linebreak 
is allowed to occur inside a quoted CSV field, but in 
this stylesheet it will be treated as two separate lines.</li>
<li>If surrounded in double-quotes, it will remove 
them.</li>
<li>If there are extra or final commas, these will be changed 
into empty <gi>cell/</gi> elements </li>
<li> It assumes things are in UTF-8 but this can be 
changed with the input-encoding parameter.  
All output is converted to UTF-8.</li>
</ul>
</p>
      </desc>
   </doc>

   <xsl:function name="jc:splitCSV" as="xs:string+">
     <xsl:param name="str" as="xs:string"/>
     <xsl:analyze-string select="concat($str, ',')"
			 regex="((&quot;[^&quot;]*&quot;)+|[^,]*),">
       <xsl:matching-substring>
	 <xsl:sequence
	     select="replace(regex-group(1), &quot;^&quot;&quot;|&quot;&quot;$|(&quot;&quot;)&quot;&quot;&quot;, &quot;$1&quot;)"
	     />
       </xsl:matching-substring>
     </xsl:analyze-string>
   </xsl:function>
   
   <xsl:template match="/" name="main">
     <xsl:choose>
       <xsl:when test="unparsed-text-available($input-uri, $input-encoding)">
	 <xsl:variable name="csv"
		       select="unparsed-text($input-uri, $input-encoding)"/>
	 <xsl:variable name="lines" select="tokenize($csv, '&#xa;')"
		       as="xs:string+"/>
	 <TEI xmlns="http://www.tei-c.org/ns/1.0">
	   <teiHeader>
	     <fileDesc>
	       <titleStmt>
		 <title>A TEI file automatically converted from CSV</title>
	       </titleStmt>
	       <publicationStmt>
		 <p>No publication statement</p>
	       </publicationStmt>
	       <sourceDesc>
		 <p>A TEI file automatically converted from a CSV file.</p>
		 
	       </sourceDesc>
	     </fileDesc>
	   </teiHeader>
	   <text>
	     <body>
	       <table>
		 <xsl:for-each select="$lines">
		   <row>
		     <xsl:variable name="lineItems" select="jc:splitCSV(.)"
				   as="xs:string+"/>
		     
		     <xsl:for-each select="$lineItems">
		       <xsl:variable name="pos" select="position()"/>
		       <cell n="{$pos}">
			 <xsl:value-of select="$lineItems[$pos]"/>
		       </cell>
		     </xsl:for-each>
		   </row>
		 </xsl:for-each>
	       </table>
	     </body>
	   </text>
	 </TEI>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:message terminate="yes">
	   <xsl:text>Cannot find the input csv file: </xsl:text>
	   <xsl:value-of select="$input-uri"/>
	 </xsl:message>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>
</xsl:stylesheet>

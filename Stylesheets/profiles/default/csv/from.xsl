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
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: James Cummings</p>
         <p>Id: $Id: from.xsl 7953 2010-08-12 21:41:00Z rahtz $</p>
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
       <xsl:when test="unparsed-text-available($input-uri)">
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

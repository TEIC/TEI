<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:tei="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="xs tei">

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
         <p>Author: James Cummings </p>
         <p>Id: $Id: from.xsl 7953 2010-08-12 21:41:00Z rahtz $</p>
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

  <xsl:param name="directory"></xsl:param>

   <xsl:template match="/">
     <xsl:for-each select="//tei:table">
       <xsl:variable name="tableNum">
	 <xsl:number level="any"/>
       </xsl:variable>
       <xsl:result-document href="{concat($directory,'table',$tableNum, '.csv')}"
			    encoding="UTF-8" method="text">
	 <xsl:for-each select="tei:row">
	   <xsl:for-each select="tei:cell">
	     <xsl:value-of select="concat('&quot;',.,'&quot;')"/>
	     <xsl:if test="following-sibling::tei:cell[1]">, </xsl:if>
	   </xsl:for-each>
	   <xsl:text>&#xa;</xsl:text>
	 </xsl:for-each>
       </xsl:result-document>
     </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:iso="http://www.iso.org/ns/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="tei iso"
                version="2.0">
  <xsl:import href="isoutils.xsl"/>
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
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>


  <xsl:output method="xhtml" encoding="utf-8"/>

  <xsl:key name="DIV" match="tei:div" use="@type"/>

  <xsl:template match="tei:TEI">
      <html>
         <head>
            <title>Report on ISO document</title>
            <link href="iso.css" rel="stylesheet" type="text/css"/>

         </head>
         <body>
            <h1 class="maintitle">Report on metadata</h1>
            <table>
               <tr>
                  <td>Today's date</td>
                  <td>
                     <xsl:call-template name="whatsTheDate"/>
                  </td>
               </tr>
	              <xsl:for-each select="key('ALLMETA',1)">
	                 <xsl:sort select="@iso:meta"/>
	                 <tr>
	                    <td>
                        <xsl:value-of select="@iso:meta"/>
                     </td>
	                    <td>
                        <xsl:value-of select="key('ISOMETA',@iso:meta)"/>
                     </td>
	                 </tr>
	              </xsl:for-each>
            </table>

         </body>
      </html>
  </xsl:template>

 <xsl:template name="block-element">
     <xsl:param name="select"/>
     <xsl:param name="style"/>
     <xsl:param name="pPr"/>
     <xsl:param name="nop"/>
     <xsl:param name="bookmark-name"/>
     <xsl:param name="bookmark-id"/>
   </xsl:template>

   <xsl:template name="termNum"/>

</xsl:stylesheet>
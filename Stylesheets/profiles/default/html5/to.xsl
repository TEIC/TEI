<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../xhtml2/tei.xsl"/>
    <xsl:import href="../../../html5/microdata.xsl"/>

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
         <p>Id: $Id: to.xsl 8923 2011-05-25 13:11:45Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="xml" omit-xml-declaration="yes" doctype-system="about:legacy-compat" />
   <xsl:param name="outputTarget">html5</xsl:param>
   <xsl:param name="doctype-system">about:legacy-compat</xsl:param>
   <xsl:param name="doctype-public"/>

   <xsl:template match="/">
     <xsl:variable name="html">
       <xsl:call-template name="processTEI"/>
     </xsl:variable>
     <xsl:for-each select="$html">
       <xsl:apply-templates mode="html5"/>
     </xsl:for-each>
   </xsl:template>
   <xsl:template match="@*|text()|comment()|processing-instruction()"  mode="html5">
      <xsl:copy-of select="."/>
   </xsl:template>


   <xsl:template match="*" mode="html5">
      <xsl:element name="{local-name()}">
         <xsl:apply-templates
	     select="*|@*|processing-instruction()|comment()|text()"  mode="html5"/>
      </xsl:element>
   </xsl:template>


</xsl:stylesheet>

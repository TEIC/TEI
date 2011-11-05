<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../html/tei.xsl"/>

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
         <p>Id: $Id: to.xsl 9329 2011-09-20 09:47:43Z rahtz $</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="cssFile"></xsl:param>
  <xsl:param name="cssInlineFile">../tei.css</xsl:param>
  <xsl:param name="institution"></xsl:param>
  <xsl:param name="feedbackURL"/>
  <xsl:param name="searchURL"/>
  <xsl:template name="copyrightStatement"></xsl:template>
  <xsl:param name="parentURL">http://www.tei-c.org/</xsl:param>
  <xsl:param name="parentWords">TEI</xsl:param>

</xsl:stylesheet>

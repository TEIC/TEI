<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="2.0">

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

   <xsl:output method="text"/>

   <xsl:template match="teiHeader"/>

   <xsl:template match="figDesc"/>

   <xsl:template match="choice">
     <xsl:apply-templates select="*[1]"/>
   </xsl:template>

   <xsl:template match="note"/>

   <xsl:template match="facsimile"/>

</xsl:stylesheet>

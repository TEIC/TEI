<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                exclude-result-prefixes="tei xsl rng"
                version="2.0">
<!-- This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
      02111-1307 USA 

$Id$

2008, TEI Consortium
-->

<xsl:output method="text"/>
   <xsl:key name="SPECS" match="tei:*[@ident]" use="@ident"/>

   <xsl:template match="/">
      <xsl:for-each select="//rng:ref">
         <xsl:if test="not(key('SPECS',@name))">
            <xsl:message>No *Spec defined for <xsl:value-of select="@name"/>
            </xsl:message>
         </xsl:if>
      </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>
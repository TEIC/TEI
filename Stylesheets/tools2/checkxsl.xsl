<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
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
   <xsl:key name="TEMPLATES" match="xsl:template[@name]" use="1"/>
   <xsl:key name="CALLED_TEMPLATES" match="xsl:call-template[@name]" use="@name"/>

   <xsl:template match="xsl:stylesheet">
      <xsl:variable name="all">
         <xsl:copy-of select="xsl:template"/>
         <xsl:call-template name="inclusions"/>
      </xsl:variable>
      <xsl:for-each select="$all">
         <xsl:for-each select="key('TEMPLATES',1)">
            <xsl:if test="count(key('CALLED_TEMPLATES',@name))=0">
	Template <xsl:value-of select="@name"/> is not used
      </xsl:if>
         </xsl:for-each>
      </xsl:for-each>
   </xsl:template>

   <xsl:template name="inclusions">
      <xsl:for-each select="xsl:import|xsl:include">
         <xsl:for-each select="document(@href)/xsl:stylesheet">
            <xsl:copy-of select="xsl:template"/>
            <xsl:call-template name="inclusions"/>
         </xsl:for-each>
      </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>
<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    exclude-result-prefixes="xd">

    <xd:doc type="stylesheet">
        <xd:short> TEI utility stylesheet for identity transformation </xd:short>
        <xd:detail> 
            This library is free software; you can redistribute it and/or modify it under
            the terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA
        </xd:detail>
        <xd:author>See AUTHORS</xd:author>
        <xd:cvsId>$Id: from.xsl 6832 2009-10-12 22:42:59Z rahtz $</xd:cvsId>
        <xd:copyright>2008, TEI Consortium</xd:copyright>
    </xd:doc>

    <!-- identity transform -->
    
    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="iden">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="iden">
        <xsl:element name="{name()}">
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="iden"
            />
        </xsl:element>
    </xsl:template>
        
</xsl:stylesheet>
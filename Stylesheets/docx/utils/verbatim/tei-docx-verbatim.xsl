<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:sch="http://www.ascc.net/xml/schematron"
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:atom="http://www.w3.org/2005/Atom"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:dbk="http://docbook.org/ns/docbook"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:its="http://www.w3.org/2005/11/its"
		xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                version="2.0"
                exclude-result-prefixes="xlink xhtml dbk iso its rng sch m tei teix atom">
    
    <xsl:import href="../../../common2/verbatim.xsl"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI Utility stylesheet to create verbatim XML for the TEI to Word conversion </p>
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
    
    <!--
        <xsl:param name="startComment">&lt;span class="comment"&gt;</xsl:param>
    <xsl:param name="endComment">&lt;/span&gt;</xsl:param>
    <xsl:param name="startElement">&lt;span class="element"&gt;</xsl:param>
    <xsl:param name="endElement">&lt;/span&gt;</xsl:param>
    <xsl:param name="startElementName">&lt;span class="elementname"&gt;</xsl:param>
    <xsl:param name="endElementName">&lt;/span&gt;</xsl:param>
    <xsl:param name="startAttribute">&lt;span class="attribute"&gt;</xsl:param>
    <xsl:param name="endAttribute">&lt;/span&gt;</xsl:param>
    <xsl:param name="startAttributeValue">&lt;span class="attributevalue"&gt;</xsl:param>
    <xsl:param name="endAttributeValue">&lt;/span&gt;</xsl:param>
    <xsl:param name="startNamespace">&lt;span class="namespace"&gt;</xsl:param>
    <xsl:param name="endNamespace">&lt;/span&gt;</xsl:param>
    
    <xsl:param name="spaceCharacter">&#xA0;</xsl:param>
    <xsl:param name="showNamespaceDecls">true</xsl:param> -->
    
    <xsl:param name="startComment"/>
    <xsl:param name="endComment"/>
    <xsl:param name="startElement"/>
    <xsl:param name="endElement"/>
    <xsl:param name="startElementName"/>
    <xsl:param name="endElementName"/>
    <xsl:param name="startAttribute"/>
    <xsl:param name="endAttribute"/>
    <xsl:param name="startAttributeValue"/>
    <xsl:param name="endAttributeValue"/>
    <xsl:param name="startNamespace"/>
    <xsl:param name="endNamespace"/>
    
    <xsl:param name="spaceCharacter"> </xsl:param>
    <xsl:param name="showNamespaceDecls">true</xsl:param>
    
    <xsl:template name="verbatim-lineBreak">
        <xsl:param name="id"/>
        <tei:lb/>
    </xsl:template>
    
    <xsl:template name="verbatim-createElement">
        <xsl:param name="name"/>
        <xsl:param name="special"/>
        <tei:hi rend="bold">
            <xsl:value-of select="$name"/>
        </tei:hi>
    </xsl:template>
    
    <xsl:template name="verbatim-createAttribute">
        <xsl:param name="name"/>
        <tei:hi rend="bold">
            <xsl:value-of select="$name"/>
        </tei:hi>
    </xsl:template>    
    
    
    
    <xsl:template name="create-egXML-section">
        <xsl:apply-templates mode="verbatim"/>
    </xsl:template>
    
</xsl:stylesheet>
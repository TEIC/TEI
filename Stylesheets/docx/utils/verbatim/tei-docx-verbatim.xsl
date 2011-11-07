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
		xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                version="2.0"
                exclude-result-prefixes="xlink xhtml dbk iso rng sch m tei teix atom">
    
    <xsl:import href="../../../common2/verbatim.xsl"/>
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI Utility stylesheet to create verbatim XML for the TEI to Word conversion </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
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
    
    <xsl:param name="spaceCharacter">&#160;</xsl:param>
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
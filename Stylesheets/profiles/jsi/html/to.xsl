<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns="http://www.w3.org/1999/xhtml"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="tei html"
    version="2.0">
    <!-- import base conversion style -->
    <!--xsl:import href="../default/html/to.xsl"/-->
    <xsl:import href="../../default/html/to.xsl"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


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
         <p>Id: $Id: to.xsl 12078 2013-05-05 12:51:58Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <!-- Indent only for debugging! -->
   <xsl:output method="xhtml" indent="yes" omit-xml-declaration="yes"/>
   <xsl:preserve-space elements="head p li span hi"/>

   <xsl:param name="cssFile">http://nl.ijs.si/tei/convert/profiles/test/html/jsi-tei.css</xsl:param>

   <xsl:template match="tei:name[@type='org']">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="class">tei_orgName</xsl:with-param>
     </xsl:call-template>
   </xsl:template>
   <xsl:template match="tei:name[@type='person']">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="class">tei_persName</xsl:with-param>
     </xsl:call-template>
   </xsl:template>
   <xsl:template match="tei:name[@type='place']">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="class">tei_placeName</xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:gap">
     <span>
       <xsl:call-template name="makeRendition">
	 <xsl:with-param name="default">tei_gap</xsl:with-param>
       </xsl:call-template>
       <xsl:apply-templates select=".//text()"/>
     </span>
   </xsl:template>

   <xsl:template match="tei:fw[@type='catch']">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">p</xsl:with-param>
       <xsl:with-param name="class">tei_fwCatch</xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:note[starts-with(@place,'margin')]">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">div</xsl:with-param>
       <xsl:with-param name="class" select="@place"/>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:cit | tei:lg | tei:sp">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">div</xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:l | tei:speaker">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">p</xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:listBibl">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">ol</xsl:with-param>
     </xsl:call-template>
   </xsl:template>
   <xsl:template match="tei:listBibl/tei:bibl">
     <xsl:call-template name="makeHTML">
       <xsl:with-param name="element">li</xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:abbr | tei:add | tei:bibl | tei:choice | tei:corr | tei:damage | tei:del | tei:expan | tei:foreign | tei:name | tei:orig | tei:q | tei:reg | tei:sic | tei:subst | tei:supplied | tei:surplus | tei:unclear">
     <xsl:call-template name="makeHTML"/>
   </xsl:template>

   <xsl:template name="makeHTML">
     <xsl:param name="element">span</xsl:param>
     <xsl:param name="content"/>
     <xsl:param name="class" select="concat('tei_',name())"/>
     <xsl:param name="title">
       <xsl:value-of select="concat('tei:',name())"/>
       <xsl:if test="@type">
	 <xsl:value-of select="concat('[@type=&quot;',@type,'&quot;]')"/>
       </xsl:if>
       <xsl:if test="@place">
	 <xsl:value-of select="concat('[@place=&quot;',@place,'&quot;]')"/>
       </xsl:if>
     </xsl:param>
     <xsl:element  name="{$element}">
       <xsl:attribute name="title" select="$title"/>
       <xsl:call-template name="makeRendition">
	 <xsl:with-param name="default" select="$class"/>
       </xsl:call-template>
       <xsl:apply-templates/>
     </xsl:element>
   </xsl:template>

</xsl:stylesheet>

<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet
    dealing  with elements from the
      drama module, making XSL-FO output.
      </p>
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
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:actor">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:camera">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:caption">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:castGroup">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:castItem">
      <xsl:call-template name="makeItem"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements castItem (when @type is 'list')</desc>
   </doc>
  <xsl:template match="tei:castItem[@type='list']">
      <list-item>
         <xsl:attribute name="space-before.optimum">
            <xsl:value-of select="$listItemsep"/>
         </xsl:attribute>
         <list-item-label end-indent="label-end()">
            <xsl:if test="@xml:id">
               <xsl:attribute name="id">
                  <xsl:value-of select="@xml:id"/>
               </xsl:attribute>
            </xsl:if>
            <xsl:text>&#10;</xsl:text>
            <block/>
         </list-item-label>
         <list-item-body start-indent="body-start()">
            <block>
               <xsl:call-template name="rend"/>
               <xsl:text>(</xsl:text>
               <xsl:apply-templates/>
               <xsl:text>)</xsl:text>
            </block>
         </list-item-body>
      </list-item>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:castList">
      <xsl:if test="child::tei:head">
         <block font-style="italic" text-align="start" space-before.optimum="4pt">
            <xsl:for-each select="tei:head">
               <xsl:apply-templates/>
            </xsl:for-each>
         </block>
      </xsl:if>
      <list-block>
         <xsl:call-template name="setListIndents"/>
         <xsl:apply-templates/>
      </list-block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:sp">
      <block text-align="justify" start-indent="1em" text-indent="-1em" space-before="3pt">
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
    
         <p xmlns="http://www.w3.org/1999/XSL/Format"> paragraphs inside speeches do very little</p>
    
      </desc>
   </doc>
  <xsl:template match="tei:sp/tei:p">
      <inline>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:speaker">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
         <xsl:text> </xsl:text>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:stage">
      <block>
         <xsl:attribute name="text-indent">1em</xsl:attribute>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:p/tei:stage">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:sp/tei:stage">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:tech">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:view">
      <inline>
         <xsl:call-template name="rend"/>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
</xsl:stylesheet>

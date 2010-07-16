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
         <p>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:actor">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('normal')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:camera">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('normal')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:caption">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('normal')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
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
            <xsl:text>
</xsl:text>
            <block/>
         </list-item-label>
         <list-item-body start-indent="body-start()">
            <block>
               <xsl:call-template name="rend">
                  <xsl:with-param name="defaultvalue" select="string('italic')"/>
                  <xsl:with-param name="defaultstyle" select="string('font-style')"/>
               </xsl:call-template>
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
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
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
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </block>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:p/tei:stage">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:sp/tei:stage">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:tech">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:view">
      <inline>
         <xsl:call-template name="rend">
            <xsl:with-param name="defaultvalue" select="string('italic')"/>
            <xsl:with-param name="defaultstyle" select="string('font-style')"/>
         </xsl:call-template>
         <xsl:apply-templates/>
      </inline>
  </xsl:template>
</xsl:stylesheet>
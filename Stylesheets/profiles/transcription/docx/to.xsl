<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="teix ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn tei teidocx">
    <!-- import conversion style -->
    <xsl:import href="../../default/docx/to.xsl"/>
    

    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML for EHESS </p>
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

    <xsl:param name="shadowGraphics">true</xsl:param>
    <xsl:param name="useNSPrefixes">false</xsl:param>    


    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>line breaks are discarded in first pass</desc>
    </doc>
    <xsl:template match="lb" mode="pass0"/>



  <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Page breaks changed to notes</desc>
  </doc>
    <xsl:template match="pb" mode="pass0">
      <note place="marginOuter"  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:text>[p. </xsl:text>
	<xsl:value-of select="@n"/>
	<xsl:text>]</xsl:text>
      </note>      
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>lists without a type attribute are assumed to be ordered</desc>
    </doc>
    <xsl:template match="list[not(@type)]" mode="pass0">
      <list xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:attribute name="type">ordered</xsl:attribute>
	<xsl:apply-templates mode="pass0" select="@*|*"/>
      </list>
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>items are numbered sequentially passim</desc>
    </doc>
    <xsl:template match="item" mode="pass0">
      <item  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:attribute name="n"><xsl:number level="any" from="tei:text"/></xsl:attribute>
	<xsl:apply-templates mode="pass0" select="text()|@*|*"/>
      </item>
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>add footnote for interlinear addition</desc>
    </doc>
    <xsl:template match="add[@place='interlinear']" mode="pass0">
      <xsl:apply-templates mode="pass0"/>
      <note place="foot"  xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates mode="pass0"/>
        <xsl:text>] </xsl:text> <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">ajouté en interligne</hi>
      </note>
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>add footnote for unclear reading</desc>
    </doc>
    <xsl:template match="unclear" mode="pass0">
      <xsl:apply-templates mode="pass0"/>
      <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates mode="pass0"/>
        <xsl:text>] </xsl:text>  <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">lecture incertaine</hi>
      </note>
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>add footnote for supplied text</desc>
    </doc>
<xsl:template match="supplied[@reason]" mode="pass0">
      <xsl:apply-templates mode="pass0"/>
      <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:apply-templates mode="pass0"/>
	<xsl:text>] </xsl:text>
	<xsl:value-of select="@reason"/>
      </note>
    </xsl:template>

    <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>add footnote for damage</desc>
    </doc>
    <xsl:template match="damage" mode="pass0">
      <xsl:apply-templates mode="pass0"/>
      <note place="foot"  xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:choose>
	<xsl:when test="@extent and @type">
	  <xsl:value-of select="@type"/>
	  <xsl:text> </xsl:text> <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">sur 
	  <xsl:value-of select="@extent"/>
	  </hi>
	</xsl:when>
	<xsl:otherwise>
	  <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">support endommagé</hi>
	</xsl:otherwise>
      </xsl:choose>
      </note>
    </xsl:template>



  <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Changing additions and left/right rendering to notes placed in margin</desc>
  </doc>
    <xsl:template match="add[@place='leftMargin' or
			 @place='rightMargin']|hi[tei:match(@rend,'leftMargin') or
			 tei:match(@rend,'rightMargin')]" mode="pass0">
      <note xmlns="http://www.tei-c.org/ns/1.0">
	<xsl:attribute name="place">
	  <xsl:choose>
	    <xsl:when test="@place='margin'">marginInner</xsl:when>
	    <xsl:otherwise>
	      <xsl:text>margin</xsl:text>
	      <xsl:value-of select="upper-case(substring(@place|@rend,1,1))"/>
	      <xsl:value-of
		  select="replace(substring(@place|@rend,2),'Margin','')"/>
	    </xsl:otherwise>
	  </xsl:choose>	  
	</xsl:attribute>
	<xsl:apply-templates mode="pass0"/>
      </note>
    </xsl:template>
  
  
  <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>sic text added</desc>
  </doc>
  <xsl:template match="sic" mode="pass0">
    <xsl:apply-templates mode="pass0"/><xsl:text> </xsl:text><hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">(sic)</hi>
    </xsl:template>
  
  
  <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Deleted text noted as footnote, different text depending on @rend</desc>
  </doc>
    <xsl:template match="del[@rend]" mode="pass0">
      <xsl:choose>
        <xsl:when test="tei:match(@rend,'strikethrough')">
          <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:apply-templates mode="pass0"/> <xsl:text> </xsl:text>
            <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">biffé</hi>
          </note>
        </xsl:when>
        <xsl:when test="tei:match(@rend,'underlined')">
          <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:apply-templates mode="pass0"/> <xsl:text> </xsl:text>
            <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">annulé par soulignement</hi>
          </note>
        </xsl:when>
        <xsl:when test="tei:match(@rend,'dottedunderline')">
          <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:apply-templates mode="pass0"/> <xsl:text> </xsl:text>
            <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">annulé par exponctuation</hi>
          </note>
        </xsl:when>
        <!-- JC: Not sure if this xsl:otherwise a good idea but adding some default probably is -->
        <xsl:otherwise>
          <note place="foot" xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:apply-templates mode="pass0"/>
            <hi rend="italics" xmlns="http://www.tei-c.org/ns/1.0">annulé</hi>
          </note>
          </xsl:otherwise>
          </xsl:choose>
      </xsl:template>

  <xsl:template name="marginalFloat">
    <w:r>
      <w:pict>
	<!--          <v:shapetype id="_x0000_t202" coordsize="21600,21600" o:spt="202" path="m,l,21600r21600,l21600,xe">
	     <v:stroke joinstyle="miter"/>
	     <v:path gradientshapeok="t" o:connecttype="rect"/>
	     </v:shapetype>
	     
	     id="_x0000_s1028" 
	-->
	<v:shape type="#_x0000_t202" style="position:static;left:0;text-align:left;margin-left:100pt;margin-top:0pt;width:50pt;height:20pt;mso-height-percent:200;mso-position-horizontal-relative:margin;mso-position-vertical-relative:margin;mso-height-percent:200;mso-width-relative:margin;mso-height-relative:margin" o:allowincell="f" o:allowoverlap="f">
            <v:textbox style="mso-fit-shape-to-text:t">
              <w:txbxContent>
                <w:p>
		  <xsl:apply-templates/>
                </w:p>
              </w:txbxContent>
            </v:textbox>
            <w10:wrap type="square" anchorx="margin" anchory="margin"/>
          </v:shape>
        </w:pict>
      </w:r>

  </xsl:template>
  
  
  <doc type="template" xmlns="http://www.oxygenxml.com/ns/doc/xsl">
  <desc>Change foreign without @xml:lang to vernacular style</desc>
  </doc>
  <xsl:template match="foreign[not(@xml:lang)]">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">vernacular</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  
  
</xsl:stylesheet>

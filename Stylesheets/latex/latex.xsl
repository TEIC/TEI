<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:s="http://www.ascc.net/xml/schematron"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="s xs tei"
                version="2.0">
  <xsl:import href="../common/common.xsl"/>
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:import href="latex_core.xsl"/>
  <xsl:import href="latex_corpus.xsl"/>
  <xsl:import href="latex_drama.xsl"/>
  <xsl:import href="latex_figures.xsl"/>
  <xsl:import href="latex_header.xsl"/>
  <xsl:import href="latex_linking.xsl"/>
  <xsl:import href="latex_namesdates.xsl"/>
  <xsl:import href="latex_nets.xsl"/>
  <xsl:import href="latex_tagdocs.xsl"/>
  <xsl:import href="latex_textstructure.xsl"/>
  <xsl:import href="latex_transcr.xsl"/>
  <xsl:import href="latex_verse.xsl"/>
  <xsl:import href="latex_textcrit.xsl"/>
  <xsl:import href="latex_param.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet for making LaTeX output.
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
  <xsl:output method="text" encoding="utf8"/>
  <xsl:variable name="top" select="/"/>
  <xsl:param name="outputTarget">latex</xsl:param>
  <xsl:param name="documentclass">article</xsl:param>
  <xsl:param name="spaceCharacter">\hspace*{6pt}</xsl:param>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements  processing-instruction()[name(.)='tex']</desc>
   </doc>
  <xsl:template match="processing-instruction()[name(.)='tex']">
      <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template name="verbatim-lineBreak">
      <xsl:param name="id"/>
      <xsl:text>\mbox{}\newline 
</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-createElement">
      <xsl:param name="name"/>
      <xsl:param name="special"/>
      <xsl:text>\textbf{</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>}</xsl:text>
  </xsl:template>

  <xsl:template name="verbatim-newLine"/>

  <xsl:template name="verbatim-Text">
      <xsl:param name="words"/>
      <xsl:choose>
         <xsl:when test="lang('zh-TW') or lang('zh')">
	           <xsl:text>{\textChinese </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="lang('ja')">
	           <xsl:text>{\textJapanese </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="lang('kr')">
	           <xsl:text>{\textKorean </xsl:text>
		   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:value-of select="tei:escapeCharsVerbatim($words)"/>
         </xsl:otherwise>
      </xsl:choose>

  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Overrides the nsList template in common/verbatim.xsl, escaping the namespace URIs for latex.</desc>
  </doc>
  <xsl:template name="nsList">
    <xsl:variable name="ns">
      <all>
        <names>
          <xsl:apply-templates select="." mode="ns"/>
        </names>
        <text>
          <xsl:copy-of select="."/>
        </text>
      </all>
    </xsl:variable>
    
    <xsl:for-each select="$ns/all/names">
      <xsl:for-each-group select="ns" group-by="@name">
        <xsl:if test="key('NSUsed',@value)">
          <xsl:call-template name="verbatim-lineBreak">
            <xsl:with-param name="id">22</xsl:with-param>
          </xsl:call-template>
          <xsl:sequence select="concat('&#160;&#160;&#160;xmlns:',@name,'=',$dq,tei:escapeCharsVerbatim(@value),$dq)"/>
        </xsl:if>
      </xsl:for-each-group>
    </xsl:for-each>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Overrides the NameSpaceURI template in common/verbatim.xsl, escaping the namespace URIs for latex.</desc>
  </doc>
  <xsl:template name="NamespaceURI">
    <xsl:param name="uri"/>
    <xsl:value-of select="tei:escapeCharsVerbatim($uri)"/>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc><p>We need the backslash and two curly braces to insert LaTeX
      commands into the output, so these characters need to replaced when they
      are found in running text. </p></desc>
  </doc>
  <xsl:function name="tei:escapeCharsVerbatim" as="xs:string">
    <xsl:param name="letters"/>
    <xsl:value-of select="replace(replace(replace(replace(replace(translate($letters, '\{}','⃥❴❵'),
		  '&#10;','\\newline&#10;'),
		  '_','\\textunderscore '),
		  '\^','\\textasciicircum '),
		  '~','\\textasciitilde '),
		  '([%&amp;\$#])','\\$1')"/>
  </xsl:function>

  <xsl:function name="tei:escapeChars" as="xs:string" override-extension-function="yes">
    <xsl:param name="letters"/>
    <xsl:param name="context"/>
      <xsl:value-of
	  select="replace(replace(replace(replace(replace(translate($letters,'ſ&#10;','s '), 
		  '\\','\\textbackslash '),
		  '_','\\textunderscore '),
		  '\^','\\textasciicircum '),
		  '~','\\textasciitilde '),
		  '([\}\{%&amp;\$#])','\\$1')"/>

  </xsl:function>

  <xsl:function name="tei:escapeCharsPartial" as="xs:string" override-extension-function="yes">
    <xsl:param name="letters"/>
      <xsl:value-of
	  select="replace($letters,'([%#])','\\$1')"/>

  </xsl:function>


   <xsl:template name="emphasize">
      <xsl:param name="class"/>
      <xsl:param name="content"/>
      <xsl:choose>
         <xsl:when test="$class='titlem'">
            <xsl:text>\textit{</xsl:text>
            <xsl:copy-of select="$content"/>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="$class='titlej'">
            <xsl:text>\textit{</xsl:text>
            <xsl:copy-of select="$content"/>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="$class='titlea'">
            <xsl:text>‘</xsl:text>
	           <xsl:copy-of select="$content"/>
            <xsl:text>’</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:copy-of select="$content"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

  <xsl:template name="Text">
      <xsl:param name="words"/>
      <xsl:value-of select="tei:escapeChars($words,.)"/>
  </xsl:template>

  <xsl:template name="applyRendition"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process text (in example mode)</desc>
   </doc>
  <xsl:template match="text()" mode="eg">
      <xsl:choose>
         <xsl:when test="starts-with(.,'&#xA;')">
            <xsl:value-of select="substring-after(.,'&#xA;')"/>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:value-of select="."/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="makeSpan">
    <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>inline objects</desc>
   </doc>

   <xsl:template name="makeInline">
      <xsl:param name="before"/>
      <xsl:param name="style"/>
      <xsl:param name="after"/>
      <xsl:value-of select="$before"/>
      <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:choose>
	<xsl:when test="$style=('add','unclear','bibl','docAuthor','titlem','italic','mentioned','term','foreign')">
	  <xsl:text>\textit{</xsl:text>
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	  <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:when test="$style='supplied'">
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	</xsl:when>
	<xsl:when test="$style='bold'">
	  <xsl:text>\textbf{</xsl:text>
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	  <xsl:text>}</xsl:text>	    
	</xsl:when>
	<xsl:when test="$style='strikethrough'">
	  <xsl:text>\sout{</xsl:text>
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	  <xsl:text>}</xsl:text>	    
	</xsl:when>
	<xsl:when test="$style='sup'">
	  <xsl:text>\textsuperscript{</xsl:text>
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	  <xsl:text>}</xsl:text>	    
	</xsl:when>
	<xsl:when test="$style='sub'">
	  <xsl:text>\textsubscript{</xsl:text>
	  <xsl:value-of select="tei:escapeChars(normalize-space(.),.)"/>
	  <xsl:text>}</xsl:text>	    
	</xsl:when>
	<xsl:when test="local-name()='label'">
	  <xsl:text>\textbf{</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:when test="not($style)">
	  <xsl:sequence select="concat('{\',local-name(),' ')"/>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:sequence select="concat('{\',$style[1], ' ')"/>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="$after"/>
    </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>how to make a horizontal rule</desc>
  </doc>
  <xsl:template name="horizontalRule">
    <xsl:text>\\\rule[0.5ex]{\textwidth}{0.5pt}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>block objects</desc>
   </doc>
    <xsl:template name="makeBlock">
      <xsl:param name="style"/>
      <xsl:sequence select="concat('\begin{',$style,'}')"/>
      <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:apply-templates/>
      <xsl:sequence select="concat('\end{',$style,'}')"/>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>division-like object</desc>
   </doc>

   <xsl:template name="makeSection">
     <xsl:param name="level"/>
      <xsl:param name="heading"/>
      <xsl:param name="implicitBlock">false</xsl:param>
      <xsl:text>&#10;</xsl:text>
      <xsl:choose>
	<xsl:when test="$level=1">\section</xsl:when>
	<xsl:when test="$level=2">\subsection</xsl:when>
	<xsl:when test="$level=3">\subsubsection</xsl:when>
	<xsl:when test="$level=4">\paragraph</xsl:when>
      </xsl:choose>
      <xsl:text>{</xsl:text>
      <xsl:value-of select="$heading"/>
      <xsl:text>}&#10;</xsl:text>
      <xsl:choose>
	<xsl:when test="$implicitBlock='true'">
	  <xsl:text>\par&#10;</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>\par&#10;</xsl:text>
	</xsl:when>
	<xsl:when test="*">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>\par&#10;</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>\par&#10;</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>text with a label</desc>
   </doc>
    <xsl:template name="makeWithLabel">
      <xsl:param name="before"/>
      <xsl:text>\textit{</xsl:text>
      <xsl:value-of select="$before"/>
      <xsl:text>}: </xsl:text>
      <xsl:apply-templates/>
    </xsl:template>


    <xsl:function name="tei:makeHyperTarget" as="xs:string">
      <xsl:param name="id"/>
      <xsl:value-of select=" if ($id) then concat('\label{',$id,'}') else ''"/>
    </xsl:function>

</xsl:stylesheet>

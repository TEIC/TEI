<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="#all"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>
    TEI stylesheet dealing  with elements from the
    textstructure module, making LaTeX output.
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
      <desc>Process elements  * in inner mode</desc>
   </doc>
  <xsl:template match="*" mode="innertext">
      <xsl:apply-templates select="."/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Root template</desc>
   </doc>


  <xsl:template match="/tei:TEI|/tei:teiCorpus">
      <xsl:call-template name="mainDocument"/>
  </xsl:template>

  <xsl:template match="tei:teiCorpus/tei:TEI">
    <xsl:apply-templates/>
    <xsl:text>&#10;\par\noindent\rule{\textwidth}{2pt}&#10;\par </xsl:text>
  </xsl:template>

  <xsl:template name="mainDocument">
      <xsl:text>\documentclass[</xsl:text>
      <xsl:value-of select="$classParameters"/>
      <xsl:text>]{</xsl:text>
      <xsl:value-of select="$documentclass"/>
      <xsl:text>}</xsl:text>
      <xsl:text>\makeatletter&#10;</xsl:text>
      <xsl:call-template name="latexSetup"/>
      <xsl:call-template name="latexPackages"/>
      <xsl:call-template name="latexLayout"/>
      <xsl:call-template name="latexOther"/>
      <xsl:text>&#10;\begin{document}&#10;</xsl:text>
      <xsl:if test="not(tei:text/tei:front/tei:titlePage)">
         <xsl:call-template name="printTitleAndLogo"/>
      </xsl:if>
      <xsl:call-template name="beginDocumentHook"/>
      <!-- certainly don't touch this line -->
      <xsl:text disable-output-escaping="yes">\let\tabcellsep&amp;</xsl:text>
      <xsl:apply-templates/>
      <xsl:call-template name="latexEnd"/>
      <xsl:if test="key('ENDNOTES',1)">
	<xsl:text>&#10;\theendnotes</xsl:text>
      </xsl:if>
      <xsl:text>&#10;\end{document}&#10;</xsl:text>
   </xsl:template>

   <xsl:template name="latexOther">
      <xsl:text>\def\TheFullDate{</xsl:text>
      <xsl:sequence select="tei:generateDate(.)"/>
      <xsl:if test="not($useFixedDate='true')">
      <xsl:variable name="revdate">
         <xsl:sequence select="tei:generateRevDate(.)"/>
      </xsl:variable>
      <xsl:if test="not($revdate='')">
         <xsl:text> (</xsl:text>
         <xsl:sequence select="tei:i18n('revisedWord')"/>
	 <xsl:text>: </xsl:text>
	 <xsl:value-of select="$revdate"/>
         <xsl:text>)</xsl:text>
      </xsl:if>
      </xsl:if>
      <xsl:text>}&#10;</xsl:text>
      <xsl:text>\def\TheID{</xsl:text>
      <xsl:choose>
         <xsl:when test="not($REQUEST='')">
            <xsl:value-of select="not($REQUEST='')"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno[1]"/>
         </xsl:when>
      </xsl:choose>
      <xsl:text>\makeatother </xsl:text>
      <xsl:text>}&#10;\def\TheDate{</xsl:text>
      <xsl:sequence select="tei:generateDate(/*)"/>
      <xsl:text>}&#10;\title{</xsl:text>
      <xsl:sequence select="tei:generateSimpleTitle(/*)"/>
      <xsl:text>}&#10;\author{</xsl:text>
      <xsl:sequence select="tei:generateAuthor(/*)"/>
      <xsl:text>}</xsl:text>
      <xsl:text>\makeatletter </xsl:text>
      <xsl:call-template name="latexBegin"/>
      <xsl:text>\makeatother </xsl:text>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Processing teiHeader elements</desc>
   </doc>
   <xsl:template match="tei:teiHeader"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:back">
      <xsl:if test="not(preceding::tei:back)">
         <xsl:text>\backmatter </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:body">
      <xsl:if test="not(ancestor::tei:floatingText) and not(preceding::tei:body) and preceding::tei:front">
         <xsl:text>\mainmatter </xsl:text>
      </xsl:if>
      <xsl:if test="count(key('APP',1))&gt;0">
\beginnumbering
\def\endstanzaextra{\pstart\centering---------\skipnumbering\pend}
</xsl:if>
      <xsl:apply-templates/>
      <xsl:if test="count(key('APP',1))&gt;0">
\endnumbering
</xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:body|tei:back|tei:front" mode="innertext">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:closer">
    <xsl:text>&#10;&#10;\begin{quote}</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{quote}&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:dateline">
    <xsl:text>\rightline{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process the tei:div elements</desc>
   </doc>
  <xsl:template
      match="tei:div|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5">
    <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Table of contents</desc>
   </doc>
   <xsl:template match="tei:divGen[@type='toc']">
     \tableofcontents
   </xsl:template>
   
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Bibliography</desc>
  </doc>
  <xsl:template match="tei:divGen[@type='bibliography']">
    <xsl:text>&#10;\begin{thebibliography}{1}&#10;</xsl:text>
    <xsl:call-template name="bibliography"/>
    <xsl:text>&#10;\end{thebibliography}&#10;</xsl:text>    
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:front">
      <xsl:if test="not(preceding::tei:front)">
         <xsl:text>\frontmatter </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:opener">
    <xsl:text>&#10;&#10;\begin{quote}</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{quote}</xsl:text>
</xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>the main TEI text</desc>
   </doc>
  <xsl:template match="tei:text">
      <xsl:choose>
         <xsl:when test="parent::tei:TEI">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="parent::tei:group">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:otherwise>
	\par
	\hrule
	\begin{quote}
	\begin{small}
	<xsl:apply-templates mode="innertext"/>
	\end{small}
	\end{quote}
	\hrule
	\par
      </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>

  <xsl:template match="tei:titlePage">
  \begin{titlepage}
  <xsl:apply-templates/>
  \end{titlepage}
  \cleardoublepage
</xsl:template>

  <xsl:template match="tei:trailer">
      <xsl:text>&#10;&#10;\begin{raggedleft}</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>\end{raggedleft}&#10;</xsl:text>
   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] make a bibliography</desc>
   </doc>
  <xsl:template name="bibliography">
      <xsl:apply-templates mode="biblio"
                           select="//tei:ref[@type='cite'] | //tei:ptr[@type='cite']"/>
  </xsl:template>


  <xsl:template match="/tei:text" priority="999">
    <xsl:call-template name="wrapRootText"/>
  </xsl:template>


</xsl:stylesheet>

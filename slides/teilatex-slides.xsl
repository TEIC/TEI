<?xml version="1.0" encoding="utf-8"?>
<!--
This software is dual-licensed:

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
-->
<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL LaTeX stylesheet to make slides

   
--><xsl:stylesheet xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:atom="http://www.w3.org/2005/Atom"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:map="http://apache.org/cocoon/sitemap/1.0"
                exclude-result-prefixes="a s map atom xhtml teix tei xsl rng"
                version="2.0">
  <xsl:import href="../latex/latex.xsl"/>
  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:*"/>
  <xsl:output method="text" encoding="utf-8"/>
  <xsl:param name="documentclass">beamer</xsl:param>
  <xsl:param name="classParameters"/>
  <xsl:param name="beamerClass">PaloAlto</xsl:param>
  <xsl:param name="pause">true</xsl:param>
  <xsl:param name="attLength">35</xsl:param>
  <xsl:param name="spaceCharacter">\hspace*{4pt}</xsl:param>

  <xsl:template name="verbatim-lineBreak">
      <xsl:param name="id"/>
      <xsl:text>\mbox{}\newline &#10;</xsl:text>
  </xsl:template>

  <xsl:template name="latexPackages">
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.95}
\usepackage{colortbl}
\usepackage{longtable}
\usepackage[normalem]{ulem}
\usetheme{<xsl:value-of select="$beamerClass"/>}
\usepackage{times}
\usepackage{fancyvrb}
\setromanfont{Times Roman}
\setsansfont{Helvetica}
\usepackage{fancyhdr}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
\xdefinecolor{blue1}{rgb}{0, 0, 0.7}
\xdefinecolor{blue2}{rgb}{0, 0, 1}
\setbeamercovered{transparent}
\let\mainmatter\relax
\let\frontmatter\relax
\let\backmatter\relax
\let\endfoot\relax
\let\endlastfoot\relax
<xsl:if test="key('ENDNOTES',1)">
  \usepackage{endnotes}
  <xsl:choose>
    <xsl:when test="key('FOOTNOTES',1)">
      \def\theendnote{\@alph\c@endnote}
    </xsl:when>
    <xsl:otherwise>
      \def\theendnote{\@arabic\c@endnote}
    </xsl:otherwise>
  </xsl:choose>
</xsl:if>

</xsl:template>

   <xsl:template name="latexBegin"/>

  <xsl:template name="latexLayout">
\date{<xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition/tei:date"/>}
\institute{<xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority"/>}
<xsl:if test="not($latexLogo='')">
         <xsl:text>\pgfdeclareimage[height=1cm]{logo}{</xsl:text>
         <xsl:choose>
            <xsl:when test="$realFigures='true'">
               <xsl:value-of select="$latexLogo"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>FIG0</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
         <xsl:text>}</xsl:text>
\logo{\pgfuseimage{logo}}
</xsl:if>
   </xsl:template>

   <xsl:template match="tei:authority/tei:address/tei:addrLine">
 \newline <xsl:apply-templates/>
   </xsl:template> 

   <xsl:template name="printTitleAndLogo">
     \frame{\maketitle}
   </xsl:template>

   <xsl:template match="tei:divGen[@type='toc']">
  \begin{frame} 
  \frametitle{Outline} 
  \tableofcontents
  \end{frame}
</xsl:template>

   <xsl:template match="tei:div/tei:head"/>
   <xsl:template match="tei:div1/tei:head"/>
   <xsl:template match="tei:div2/tei:head"/>

   <xsl:template match="tei:div1">
      <xsl:choose>
         <xsl:when test="tei:div2">
            <xsl:call-template name="makeOuterFrame"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="makeFrame"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="tei:div2|tei:div3">
      <xsl:call-template name="makeFrame"/>
   </xsl:template>

   <xsl:template match="tei:div">
      <xsl:choose>
         <xsl:when test="tei:div and parent::tei:body">
            <xsl:call-template name="makeOuterFrame"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="makeFrame"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template name="makeOuterFrame">
      <xsl:text>&#10;\section{</xsl:text>
      <xsl:for-each select="tei:head">
         <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:text>&#10;\begin{frame}&#10;\frametitle{</xsl:text>
      <xsl:for-each select="tei:head">
         <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:choose>
	        <xsl:when test="tei:*[not(starts-with(local-name(.),'div'))]">
	           <xsl:apply-templates select="tei:*[not(starts-with(local-name(.),'div'))]"/>
	        </xsl:when>
	        <xsl:otherwise>
	           <xsl:text>{\Huge…}</xsl:text>
	        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;\end{frame}&#10;</xsl:text>
      <xsl:apply-templates select="tei:div1|tei:div2|tei:div"/>
  </xsl:template>

   <xsl:template name="makeFrame">
      <xsl:text>&#10;\begin{frame}</xsl:text>
      <xsl:choose>
         <xsl:when test="tei:match(@rend,'fragile')">
            <xsl:text>[fragile]</xsl:text>
         </xsl:when>
         <xsl:when test=".//tei:eg">
            <xsl:text>[fragile]</xsl:text>
         </xsl:when>
         <xsl:when test=".//tei:Output">
            <xsl:text>[fragile]</xsl:text>
         </xsl:when>
         <xsl:when test=".//tei:Screen">
            <xsl:text>[fragile]</xsl:text>
         </xsl:when>
         <xsl:when test=".//teix:egXML">
            <xsl:text>[fragile]</xsl:text>
         </xsl:when>
      </xsl:choose>
      <xsl:text>&#10;\frametitle{</xsl:text>
      <xsl:for-each select="tei:head">
         <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:choose>
	<xsl:when test="tei:cb">
	  <xsl:variable name="prop" select="1 div number(count(tei:cb[preceding-sibling::*[not(self::tei:head)]]) + 1)"/>
	  <xsl:text>\begin{columns}[t]
	  \begin{column}[T]{</xsl:text>
	  <xsl:value-of select="$prop"/>
	  <xsl:text>\textwidth}
	  </xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>\end{column}
	  \end{columns}</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;\end{frame}&#10;</xsl:text>
   </xsl:template>
 
   <xsl:template match="tei:cb">
     <xsl:if test="preceding-sibling::*[not(self::tei:head)]">
       <xsl:variable name="prop" select="1 div number(count(../tei:cb[preceding-sibling::*[not(self::tei:head)]]) + 1)"/>
     <xsl:text>\end{column}
     \begin{column}[T]{</xsl:text>
     <xsl:value-of select="$prop"/>
	  <xsl:text>\textwidth}
     </xsl:text>
     </xsl:if>
   </xsl:template>

  <xsl:template name="makePic">
    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:if test="tei:match(@rend,'centre')">
         <xsl:text>\centerline{</xsl:text>
      </xsl:if>
      <xsl:text>\includegraphics[</xsl:text>
      <xsl:call-template name="graphicsAttributes">
         <xsl:with-param name="mode">latex</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="not(@width) and not (@height) and not(@scale)">
         <xsl:text>width=\textwidth</xsl:text>
      </xsl:if>
      <xsl:text>]{</xsl:text>
      <xsl:sequence select="tei:resolveURI(.,@url)"/>
      <xsl:text>}</xsl:text>
      <xsl:if test="tei:match(@rend,'centre')">
         <xsl:text>}</xsl:text>
      </xsl:if>
   </xsl:template>

   <xsl:template match="tei:hi[not(@rend)]">
      <xsl:text>\textcolor{red}{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
   </xsl:template>

   <xsl:template match="tei:item[tei:match(@rend,'pause') or parent::tei:list/tei:match(@rend,'pause')]">
      <xsl:if test="$pause='true'">
         <xsl:text>\pause </xsl:text>
      </xsl:if>
      <xsl:text>\item </xsl:text>
      <xsl:apply-templates/>
   </xsl:template>

<!--
   <xsl:template match="tei:eg">
      <xsl:text>\begin{Verbatim}[fontsize=\scriptsize,frame=single,fillcolor=\color{yellow}]&#10;</xsl:text>
      <xsl:apply-templates mode="eg"/>
      <xsl:text>\end{Verbatim}&#10;</xsl:text>
   </xsl:template>
-->
  <xsl:template match="text()" mode="eg">
      <xsl:choose>
         <xsl:when test="starts-with(.,'&#xA;')">
            <xsl:value-of select="tei:escapeCharsVerbatim(substring-after(.,'&#xA;'))"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="tei:escapeCharsVerbatim(.)"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
      <xsl:variable name="fontsize">
         <xsl:choose>
            <xsl:when test="tei:match(@rend,'teeny')">
	              <xsl:text>{5.5pt}{6pt}</xsl:text>
            </xsl:when>
            <xsl:when test="tei:match(@rend,'tiny')">
	              <xsl:text>{6.5pt}{7pt}</xsl:text>
            </xsl:when>
            <xsl:when test="tei:match(@rend,'small')">
	              <xsl:text>{7pt}{8pt}</xsl:text>
            </xsl:when>
            <xsl:when test="tei:match(@rend,'smaller')">
	              <xsl:text>{7.5pt}{8pt}</xsl:text>
            </xsl:when>
            <xsl:when test="tei:match(@rend,'larger')">
	              <xsl:text>{9.5pt}{10.5pt}</xsl:text>
            </xsl:when>
            <xsl:when test="tei:match(@rend,'large')">
	              <xsl:text>{10.5pt}{11.5pt}</xsl:text>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:text>{8.5pt}{9pt}</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
\bgroup\ttfamily\fontsize<xsl:value-of select="$fontsize"/>\selectfont\par
\begin{exampleblock}{}
<xsl:text>\noindent\ttfamily\mbox{}</xsl:text>
      <xsl:apply-templates mode="verbatim">
         <xsl:with-param name="highlight">
            <xsl:value-of select="$highlight"/>
         </xsl:with-param>
      </xsl:apply-templates>
\end{exampleblock}
\par\egroup
  </xsl:template>

  <xsl:template match="tei:p[tei:match(@rend,'box')]">
      <xsl:text>\par\begin{exampleblock}{}&#10;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>\end{exampleblock}\par&#10;</xsl:text>
  </xsl:template>


   <xsl:template match="tei:table">
      <xsl:text>\par  
  \begin{scriptsize}
  \begin{longtable}</xsl:text>
      <xsl:call-template name="makeTable"/>
      <xsl:text>\end{longtable}
  \end{scriptsize}</xsl:text>
   </xsl:template>

  <xsl:template name="makeFigureStart">
    \noindent
  </xsl:template>

  <xsl:template name="makeFigureEnd">
      <xsl:choose>
         <xsl:when test="tei:head">
            <xsl:text>&#10;\par\noindent\textit{</xsl:text>
	    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
            <xsl:for-each select="tei:head">
	      <xsl:apply-templates/>
	    </xsl:for-each>
            <xsl:text>}\par</xsl:text>
         </xsl:when>
      </xsl:choose>

  </xsl:template>

   <xsl:template name="tableHline">
      <xsl:text> \hline </xsl:text>
   </xsl:template>

   <xsl:template match="tei:att">
      <xsl:text>\emph{@</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>

   </xsl:template>


  <xsl:template name="Text">
      <xsl:param name="words"/>
      <xsl:analyze-string select="$words" regex="(&amp;)">
	<xsl:matching-substring>
            <xsl:text>&amp;amp;</xsl:text>
	</xsl:matching-substring>
	<xsl:non-matching-substring>
	  <xsl:value-of select="tei:escapeChars(.,.)"/>
	</xsl:non-matching-substring>
      </xsl:analyze-string>
  </xsl:template>

  <xsl:template name="Element">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue1}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML element name in a verbatim context</desc>
  </doc>
  <xsl:template name="ElementName">
    <xsl:param name="content"/>
    <xsl:text>\textbf{\color{blue1}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute value in a verbatim context</desc>
  </doc>

  <xsl:template name="AttributeValue">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue2}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[latex] show an XML attribute in a verbatim context</desc>
  </doc>

  <xsl:template name="Attribute">
    <xsl:param name="content"/>
    <xsl:text>{\color{blue2}</xsl:text>
      <xsl:copy-of select="$content"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  
  <xsl:template match="tei:affiliation">
      <xsl:text>\mbox{}\\(\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>})</xsl:text>
  </xsl:template>


</xsl:stylesheet>

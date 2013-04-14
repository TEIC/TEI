<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"                
    xmlns:xi="http://www.w3.org/2003/XInclude"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:m="http://www.w3.org/1998/Math/MathML"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="#all"
    version="2.0">
  <!-- import base conversion style -->

  <xsl:import href="../../../latex2/tei.xsl"/>
  <xsl:import href="../../../bibtex/convertbib.xsl"/>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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
         <p>Id: $Id: to.xsl 10345 2012-05-15 08:37:59Z rahtz $</p>
         <p>Copyright: 2008, tei Consortium</p>
      </desc>
   </doc>

  <xsl:param name="reencode">false</xsl:param>
  <xsl:param name="classParameters"></xsl:param>
  <xsl:param name="longtables">false</xsl:param>
  <xsl:param name="attsOnSameLine">2</xsl:param>
  <xsl:param name="documentclass">acm_proc_article-sp</xsl:param>  
  <xsl:template name="latexSetup"/>
  <xsl:template name="latexPackages">   
\usepackage{color,framed,times}
\definecolor{shadecolor}{gray}{0.95}
  </xsl:template>
  <xsl:template name="latexLayout"/>
  <xsl:template name="latexBegin">
    \let\frontmatter\relax
    \let\mainmatter\relax
    \let\backmatter\relax
  </xsl:template>
  <xsl:template name="printTitleAndLogo">

\title{<xsl:sequence select="tei:generateTitle(.)"/>}
\numberofauthors{<xsl:value-of select="count(ancestor-or-self::TEI/teiHeader/fileDesc/titleStmt/author)"/>}

\author{<xsl:for-each
    select="ancestor-or-self::TEI/teiHeader/fileDesc/titleStmt/author">\alignauthor
<xsl:apply-templates select="persName"/>\\
\affaddr{<xsl:apply-templates select="affiliation"/>}\\
\affaddr{<xsl:apply-templates select="email"/>}
</xsl:for-each>}
\date{<xsl:apply-templates select="ancestor-or-self::TEI/teiHeader/fileDesc/editionStmt/edition/date"/>}
\maketitle
\begin{abstract}
<xsl:for-each
    select="ancestor-or-self::TEI/front/div[@type='abstract']">
  <xsl:apply-templates/>
</xsl:for-each>
\end{abstract}
</xsl:template>


  <xsl:template match="div[@type='abstract']"/>

  <xsl:template match="table">
\begin{table}
      <xsl:if test="@xml:id">
         <xsl:text>\label{</xsl:text>
         <xsl:value-of select="@xml:id"/>
         <xsl:text>}</xsl:text>
      </xsl:if>
      <xsl:text> \par </xsl:text>
      <xsl:text>\begin{tabular}</xsl:text>
      <xsl:call-template name="makeTable"/> 
      <xsl:text>\end{tabular}</xsl:text>
\end{table}
  </xsl:template>

  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="dest"/>
      <xsl:choose>
         <xsl:when test="$ptr">
            <xsl:text>{\small\ttfamily </xsl:text>
	    <xsl:sequence select="$dest"/>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
   <xsl:template name="tableHline">
   <xsl:text>\hline </xsl:text>
   </xsl:template>


  <xsl:template name="makeFigureStart">
      <xsl:choose>
	<xsl:when test="@place='inline' and head">
            <xsl:text>\begin{figure}[H]&#10;</xsl:text>
	</xsl:when>
	<xsl:when test="@rend='display' or not(@place='inline') or head or p">
	  <xsl:text>\begin{figure*}[htbp]&#10;</xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:choose>
	<xsl:when test="@rend='center'">
	  <xsl:text>\begin{center}</xsl:text>
	</xsl:when>
	<xsl:otherwise>\noindent</xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] Make figure (end)</desc>
   </doc>
  <xsl:template name="makeFigureEnd">
      <xsl:choose>
         <xsl:when test="head or p">
            <xsl:text>&#10;\caption{</xsl:text>
            <xsl:if test="@xml:id">\label{<xsl:value-of select="@xml:id"/>}</xsl:if>
            <xsl:for-each select="head">
	      <xsl:apply-templates/>
	    </xsl:for-each>
            <xsl:text>}</xsl:text>
         </xsl:when>
      </xsl:choose>
      <xsl:if test="@rend='center'">
            <xsl:text>\end{center}</xsl:text>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="@place='inline' and head">
            <xsl:text>\end{figure}&#10;</xsl:text>
	</xsl:when>
         <xsl:when test="@rend='display' or not(@place='inline')">
	   <xsl:text>\end{figure*}&#10;</xsl:text>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="latexEnd">
    \balancecolumns
  </xsl:template>
      
   <xsl:template name="exampleFontSet">
     <xsl:text>\ttfamily\fontsize{7pt}{8.5pt}\selectfont </xsl:text>
   </xsl:template>

   <xsl:template match="ptr[@type='bibl']">
     <xsl:sequence select="concat('\cite{',substring-after(@target,'#'),'}')"/>
       <!--
	<xsl:variable name="place" select="replace(@target,'#.*','')"/>
	 <xsl:variable name="doc">
	   <xsl:choose>
	     <xsl:when test="not($ORIGDIR='')">
	       <xsl:sequence select="doc(resolve-uri($place,$ORIGDIR))"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:sequence select="doc(resolve-uri($place,base-uri(/)))"/>
	     </xsl:otherwise>
	   </xsl:choose>
	 </xsl:variable>
	 -->
   </xsl:template>

   <xsl:template match="listBibl">
     <xsl:message>Generate foracmbib.bib</xsl:message>
     <xsl:result-document href="foracmbib.bib" method="text">
       <xsl:for-each select="biblStruct">
	 <xsl:call-template name="biblStruct2bibtex"/>
       </xsl:for-each>
     </xsl:result-document>
     \bibliographystyle{abbrv}
     \bibliography{foracmbib}

   </xsl:template>

   <xsl:template match="xi:include">
     <xsl:for-each
	 select="doc(resolve-uri(@href,base-uri(/)))//text/*">
       <xsl:apply-templates/>
     </xsl:for-each>
   </xsl:template>
</xsl:stylesheet>
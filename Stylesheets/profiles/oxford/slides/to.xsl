<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../slides/teilatex-slides.xsl"/>

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
         <p>Id: $Id: to.xsl 10466 2012-06-08 18:47:50Z rahtz $</p>
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:output method="text"/>
<xsl:param name="latexLogo"></xsl:param>
<xsl:param name="logoFile"></xsl:param>
<xsl:param name="spaceCharacter">\hspace*{6pt}</xsl:param>
<xsl:param name="beamerClass">Singapore</xsl:param>
<xsl:param name="showNamespaceDecls">false</xsl:param>

  <xsl:param name="omitNSDecls">
    xmlns="http://purl.org/NET/crm-owl#
    xmlns="http://www.w3.org/1999/02/22-rdf-syntax-ns#
    http://www.tei-c.org/ns/1.0
    http://www.w3.org/1999/xhtml
  </xsl:param>
  <xsl:template name="latexPreambleHook">
\usepackage{pdfpages}
  </xsl:template>

<xsl:template name="latexPackages">
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.95}
\setmonofont{DejaVu Sans Mono}
\setromanfont{DejaVuSerif}
\setsansfont{DejaVuSans}
\usetheme{<xsl:value-of select="$beamerClass"/>}
\useinnertheme[shadow=true]{rounded}
\usecolortheme{orchid}  
\setbeamercolor*{frametitle}{parent=palette primary}
\usepackage{fancyvrb,fancyhdr,longtable}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
\setbeamercovered{transparent}
\usenavigationsymbolstemplate{}
\xdefinecolor{blue1}{rgb}{0, 0, 0.7}
\xdefinecolor{blue2}{rgb}{0, 0, 1}
\let\mainmatter\relax
\let\frontmatter\relax
\let\backmatter\relax
\let\endfoot\relax
\let\endlastfoot\relax
\parskip3pt
\setbeamertemplate{footline}
{\hspace{1em}%
\IfFileExists{oxlogo.png}{\includegraphics[height=10ex]{oxlogo.png}}{}%
\textcolor{gray}{\insertframenumber/\inserttotalframenumber}\hfill\includegraphics[height=5ex]{<xsl:value-of select="$logoFile"/>}\hspace{1em}}
</xsl:template>

<xsl:template match="tei:gi">
  <xsl:text>{\color{blue2}&lt;</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>&gt;}</xsl:text>
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
  

</xsl:stylesheet>

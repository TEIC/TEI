<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="xd exsl estr edate a rng tei teix" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet dealing  with elements from the
    textstructure module, making LaTeX output.
      </xd:short>
    <xd:detail>
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

   
   
      </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements  * in inner mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="innertext">
    <xsl:apply-templates select="."/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:TEI</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:TEI">
    
    <xsl:if test="not($realFigures='true')">
      <xsl:text>%BEGINFIGMAP</xsl:text>
      <xsl:if test="not($latexLogo='')">
	<xsl:text>&#10;%FIGMAP </xsl:text>
	<xsl:value-of select="$latexLogo"/>
	<xsl:text> FIG0 </xsl:text>
      </xsl:if>
      <xsl:for-each select="//tei:figure">
	<xsl:variable name="c">
	  <xsl:number level="any"/>
	</xsl:variable>
	<xsl:text>&#10;%FIGMAP </xsl:text>
	<xsl:variable name="f">
	  <xsl:choose>
	    <xsl:when test="@url">
	      <xsl:value-of select="@url"/>
	    </xsl:when>
	    <xsl:when test="@entity">
	      <xsl:value-of select="unparsed-entity-uri(@entity)"/>
	    </xsl:when>
	    <xsl:when test="tei:graphic">
	      <xsl:value-of select="tei:graphic/@url"/>
	    </xsl:when>
	  </xsl:choose>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="contains($f,'.')">
	    <xsl:value-of select="$f"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="concat($f,'.png')"/>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:text> FIG</xsl:text>
	<xsl:value-of select="$c + 1000"/>
	<xsl:text>&#10;</xsl:text>
      </xsl:for-each>
      <xsl:text>&#10;%ENDFIGMAP&#10;</xsl:text>
    </xsl:if>
    <xsl:text>&#10;\documentclass[</xsl:text>
    <xsl:value-of select="$classParameters"/>
    <xsl:text>]{</xsl:text>
    <xsl:value-of select="$docClass"/>
    <xsl:text>}</xsl:text>
    <xsl:text>\makeatletter&#10;</xsl:text>
    <xsl:call-template name="latexSetup"/>
    <xsl:call-template name="latexPackages"/>
    <xsl:call-template name="latexLayout"/>
    <xsl:text>
\@ifundefined{chapter}{%
    \def\DivI{\section}
    \def\DivII{\subsection}
    \def\DivIII{\subsubsection}
    \def\DivIV{\paragraph}
    \def\DivV{\subparagraph}
    \def\DivIStar[#1]#2{\section*{#2}}
    \def\DivIIStar[#1]#2{\subsection*{#2}}
    \def\DivIIIStar[#1]#2{\subsubsection*{#2}}
    \def\DivIVStar[#1]#2{\paragraph*{#2}}
    \def\DivVStar[#1]#2{\subparagraph*{#2}}
}{%
    \def\DivI{\chapter}
    \def\DivII{\section}
    \def\DivIII{\subsection}
    \def\DivIV{\subsubsection}
    \def\DivV{\paragraph}
    \def\DivIStar[#1]#2{\chapter*{#2}}
    \def\DivIIStar[#1]#2{\section*{#2}}
    \def\DivIIIStar[#1]#2{\subsection*{#2}}
    \def\DivIVStar[#1]#2{\subsubsection*{#2}}
    \def\DivVStar[#1]#2{\paragraph*{#2}}
}
\makeatother
\def\TheFullDate{</xsl:text>
<xsl:call-template name="generateDate"/>
<xsl:variable name="revdate">
  <xsl:call-template name="generateRevDate"/>
</xsl:variable>
<xsl:if test="not($revdate='')">
  <xsl:text> (</xsl:text>
  <xsl:call-template name="i18n">
    <xsl:with-param name="word">revisedWord</xsl:with-param>
    </xsl:call-template>: 
    <xsl:value-of select="$revdate"/>
    <xsl:text>)</xsl:text>
</xsl:if>
<xsl:text>}
\def\TheDate{</xsl:text>
<xsl:call-template name="generateDate"/>
<xsl:text>}
\title{</xsl:text>
<xsl:call-template name="generateTitle"/>
<xsl:text>}
\author{</xsl:text>
<xsl:call-template name="generateAuthor"/>
<xsl:text>}
\begin{document}
</xsl:text>
<xsl:call-template name="latexBegin"/>
<!-- certainly don't touch the next few lines -->
<xsl:text disable-output-escaping="yes">
\catcode`\$=12\relax
\catcode`\^=12\relax
\catcode`\~=12\relax
\catcode`\#=12\relax
\catcode`\%=12\relax&#10;</xsl:text>
<xsl:text disable-output-escaping="yes">\let\tabcellsep&amp;
\catcode`\&amp;=12\relax&#10;</xsl:text>
<xsl:apply-templates select="tei:text"/>
<xsl:call-template name="latexEnd"/>
<xsl:text>
\end{document}
</xsl:text>
</xsl:template>

  <xd:doc>
    <xd:short>Process elements  tei:back</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:back">
    <xsl:if test="not(preceding::tei:back)">
   \backmatter
 </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:body</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:body">
    <xsl:if test="not(preceding::tei:body)">
   \mainmatter
 </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:body in inner mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:body|tei:back|tei:front" mode="innertext">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:closer</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:closer">
 \begin{quote}<xsl:apply-templates/>\end{quote}
</xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:dateline</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:dateline">
 \rightline{<xsl:apply-templates/>}
</xsl:template>
  <xd:doc>
    <xd:short>Process the tei:div elements</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div0|tei:div1|tei:div2|tei:div3|tei:div4|tei:div5">
    <xsl:choose>
      <xsl:when test="@type='letter'">
        <xsl:text>\subsection*{</xsl:text>
        <xsl:for-each select="tei:head">
          <xsl:apply-templates/>
        </xsl:for-each>
        <xsl:text>}</xsl:text>
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@type='bibliography'">
      \begin{thebibliography}{1}
      <xsl:call-template name="bibliography"/>
      \end{thebibliography}  
    </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:divGen[@type='toc']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:divGen[@type='toc']">
\tableofcontents
</xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:front</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:front">
    <xsl:if test="not(preceding::tei:front)">
   \frontmatter
 </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:opener</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:opener">
 \begin{quote}<xsl:apply-templates/>\end{quote}
</xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l">
  \leftline{<xsl:apply-templates/>}
</xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:text</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
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
  <xd:doc>
    <xd:short>Process elements  tei:titlePage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:titlePage">
  \begin{titlepage}
<xsl:apply-templates/>
  \maketitle
  \end{titlepage}
  \cleardoublepage
</xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateSimpleTitle">
    <xsl:choose>
      <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
        <xsl:value-of select="normalize-space(ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="normalize-space(ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="sectionhead">
    <xsl:text>[</xsl:text>
    <xsl:value-of select="normalize-space(.)"/>
    <xsl:text>]</xsl:text>
    <xsl:text>{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
</xsl:stylesheet>

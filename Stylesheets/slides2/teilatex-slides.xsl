<?xml version="1.0" encoding="utf-8"?>
<!-- 
TEI XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL LaTeX stylesheet to make slides

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
  <xsl:import href="../latex2/tei.xsl"/>
  <xsl:import href="../common2/verbatim.xsl"/>
  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:*"/>
  <xsl:output method="text" encoding="utf-8"/>
  <xsl:variable name="docClass">beamer</xsl:variable>
  <xsl:param name="startNamespace">\color{black}</xsl:param>
  <xsl:param name="startElement">{\color{blue}</xsl:param>
  <xsl:param name="startElementName">\textbf{\color{blue}</xsl:param>
  <xsl:param name="startAttribute">{\color{blue}</xsl:param>
  <xsl:param name="startAttributeValue">{\color{blue}</xsl:param>
  <xsl:param name="startComment">\textit{</xsl:param>
  <xsl:param name="endElement">}</xsl:param>
  <xsl:param name="endElementName">}</xsl:param>
  <xsl:param name="endComment">}</xsl:param>
  <xsl:param name="endAttribute">}</xsl:param>
  <xsl:param name="endAttributeValue">}</xsl:param>
  <xsl:param name="endNamespace"/>
  <xsl:param name="spaceCharacter">\hspace*{1em}</xsl:param>
  <xsl:param name="classParameters"/>
  <xsl:param name="beamerClass">PaloAlto</xsl:param>
  <xsl:param name="pause">true</xsl:param>

  <xsl:template name="verbatim-lineBreak">
      <xsl:param name="id"/>
      <xsl:text>\mbox{}\newline &#10;</xsl:text>
  </xsl:template>

  <xsl:template name="latexPackages">
\usepackage{framed}
\definecolor{shadecolor}{gray}{0.95}
\usepackage{colortbl}
\usepackage{longtable}
\usetheme{<xsl:value-of select="$beamerClass"/>}
\usepackage{times}
\usepackage{fancyvrb}
\usepackage{fancyhdr}
\def\Gin@extensions{.pdf,.png,.jpg,.mps,.tif}
\setbeamercovered{transparent}
\let\mainmatter\relax
\let\frontmatter\relax
\let\backmatter\relax
\let\endfoot\relax
\let\endlastfoot\relax
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
      <xsl:text>
\section{</xsl:text>
      <xsl:for-each select="tei:head">
         <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:text>
\begin{frame} 
  \frametitle{</xsl:text>
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
      <xsl:text>
\end{frame}&#10;</xsl:text>
      <xsl:apply-templates select="tei:div1|tei:div2|tei:div"/>
  </xsl:template>

   <xsl:template name="makeFrame">
      <xsl:text>
\begin{frame}</xsl:text>
      <xsl:choose>
         <xsl:when test="@rend='fragile'">
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
      <xsl:text>
\frametitle{</xsl:text>
      <xsl:for-each select="tei:head">
         <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>
\end{frame}&#10;</xsl:text>
   </xsl:template>

   <xsl:template name="makePic">
      <xsl:if test="@xml:id">
         <xsl:text>\hypertarget{</xsl:text>
         <xsl:value-of select="@xml:id"/>
         <xsl:text>}{}</xsl:text>
      </xsl:if>
      <xsl:if test="@rend='centre'">
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
      <xsl:choose>
         <xsl:when test="@url">
            <xsl:value-of select="@url"/>
         </xsl:when>
         <xsl:when test="@entity">
            <xsl:value-of select="unparsed-entity-uri(@entity)"/>
         </xsl:when>
      </xsl:choose>
      <xsl:text>}</xsl:text>
      <xsl:if test="@rend='centre'">
         <xsl:text>}</xsl:text>
      </xsl:if>
   </xsl:template>

   <xsl:template match="tei:hi[not(@rend)]">
      <xsl:text>\alert{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
   </xsl:template>

   <xsl:template match="tei:item[@rend='pause' or parent::tei:list/@rend='pause']">
      <xsl:if test="$pause='true'">
         <xsl:text>\pause </xsl:text>
      </xsl:if>
      <xsl:text>\item </xsl:text>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:eg">
      <xsl:text>\begin{Verbatim}[fontsize=\scriptsize,frame=single,fillcolor=\color{yellow}]&#10;</xsl:text>
      <xsl:apply-templates mode="eg"/>
      <xsl:text>\end{Verbatim}&#10;</xsl:text>
   </xsl:template>

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

  <xsl:template match="teix:egXML">
      <xsl:param name="simple">false</xsl:param>
      <xsl:param name="highlight"/>
      <xsl:variable name="fontsize">
         <xsl:choose>
            <xsl:when test="@rend='teeny'">
	              <xsl:text>{5.5pt}{6pt}</xsl:text>
            </xsl:when>
            <xsl:when test="@rend='tiny'">
	              <xsl:text>{6.5pt}{7pt}</xsl:text>
            </xsl:when>
            <xsl:when test="@rend='small'">
	              <xsl:text>{7.5pt}{8pt}</xsl:text>
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

  <xsl:template match="tei:p[@rend='box']">
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
	  <xsl:value-of select="tei:escapeChars(.)"/>
	</xsl:non-matching-substring>
      </xsl:analyze-string>
  </xsl:template>


   <xsl:template match="@*" mode="attributetext">
      <xsl:choose>
         <xsl:when test="string-length(.)&gt;$attLength and contains(.,' ')">
            <xsl:call-template name="verbatim-reformatText">
	              <xsl:with-param name="sofar">0</xsl:with-param>
	              <xsl:with-param name="indent">
	                 <xsl:text> </xsl:text>
	              </xsl:with-param>
	              <xsl:with-param name="text">
	                 <xsl:value-of select="normalize-space(tei:escapeChars(.))"/>
	              </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:value-of select="tei:escapeChars(.)"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

</xsl:stylesheet>
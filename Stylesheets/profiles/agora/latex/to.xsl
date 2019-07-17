<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
                xmlns:m="http://www.w3.org/1998/Math/MathML"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei m"
                version="2.0">
    <!-- import base conversion style -->

    <xsl:import href="../../../latex/latex.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:param name="numberHeadings">false</xsl:param>


<!-- deal with weird @rend values -->
<xsl:template match="tei:hi[tei:match(@rend,'bo')]">
\textbf{<xsl:apply-templates/>}
</xsl:template>
<xsl:template match="tei:hi[tei:match(@rend,'smcap')]">
\textsc{<xsl:apply-templates/>}
</xsl:template>
<xsl:template match="tei:hi[tei:match(@rend,'del')]">
\sout{<xsl:apply-templates/>}
</xsl:template>
<xsl:template match="tei:hi[tei:match(@rend,'ul2')]">
\uuline{<xsl:apply-templates/>}
</xsl:template>
<xsl:template match="tei:hi[tei:match(@rend,'ulw')]">
\uwave{<xsl:apply-templates/>}
</xsl:template>

<!-- do weird list types -->
<xsl:template match="tei:list[@type='number']">
\begin{enumerate}
          <xsl:apply-templates select="tei:item"/>
\end{enumerate}
</xsl:template>

<xsl:template match="tei:list[@type='simple']">
\begin{list}{\quad}{}
          <xsl:apply-templates select="tei:item"/>
\end{list}
</xsl:template>


<!-- xsl:template match="tei:byline/text()">
\author{<xsl:value-of select="."/>}
</xsl:template-->

  <xsl:template match="tei:docAuthor"> 
<xsl:message>hello!</xsl:message>
      <xsl:if test="not(preceding-sibling::tei:docAuthor)">
         <xsl:text>\author{</xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
      <xsl:choose>
         <xsl:when test="count(following-sibling::tei:docAuthor)=1"> and </xsl:when>
         <xsl:when test="following-sibling::tei:docAuthor">, </xsl:when>
      </xsl:choose>

      <xsl:if test="parent::tei:byline and (following-sibling::text())">
         <xsl:value-of select="following-sibling::text()"/>
      </xsl:if>

      <xsl:if test="not(following-sibling::tei:docAuthor)">
         <xsl:text>}</xsl:text>
      </xsl:if>
  </xsl:template>

<xsl:template match="tei:byline/text()"/>


    
</xsl:stylesheet>

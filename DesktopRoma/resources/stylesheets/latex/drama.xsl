<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd exsl estr edate a rng tei teix"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the drama module,
      making LaTeX output. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id: drama.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:actor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:actor">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:camera</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:camera">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:caption</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:caption">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castGroup</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castGroup"> 
    <xsl:text>\begin{itemize} </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#10; \end{itemize}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castItem</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castItem">&#10;\item 
  <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castList"><xsl:if test="tei:head">
      <xsl:text> \par\textit{</xsl:text>
      <xsl:for-each select="tei:head">
        <xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text>}&#10;</xsl:text>
    </xsl:if> \begin{itemize} <xsl:apply-templates/> \end{itemize} </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:p/tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p/tei:stage">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:role</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:role">
    <xsl:text>\textbf{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:roleDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:roleDesc">
    <xsl:text>\begin{quote}</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{quote}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:set</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:set">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sound</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sound">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sp</xd:short>
    <xd:detail/>
  </xd:doc>
  <xsl:template match="tei:sp"> \begin{description} \item[<xsl:apply-templates
      select="tei:speaker"/>] <xsl:apply-templates
      select="tei:p | tei:l | tei:lg | tei:seg |      tei:ab | tei:stage"/>
    <xsl:text>\end{description}&#10;</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sp/tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sp/tei:p">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:stage">
    <xsl:text>&#10;\par&#10;</xsl:text>
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}\par </xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sp/tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sp/tei:stage">
    <xsl:text/>
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>} </xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:tech</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:tech">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:view</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:view">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
</xsl:stylesheet>

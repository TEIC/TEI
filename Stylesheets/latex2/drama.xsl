<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the drama module,
      making LaTeX output. </p>
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
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementactor</desc>
   </doc>
  <xsl:template match="tei:actor">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementcamera</desc>
   </doc>
  <xsl:template match="tei:camera">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementcaption</desc>
   </doc>
  <xsl:template match="tei:caption">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementcastGroup</desc>
   </doc>
  <xsl:template match="tei:castGroup"> 
      <xsl:text>\begin{itemize} </xsl:text>
      <xsl:apply-templates/>
      <xsl:text>
 \end{itemize}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementcastItem</desc>
   </doc>
  <xsl:template match="tei:castItem">
\item 
  <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementcastList</desc>
   </doc>
  <xsl:template match="tei:castList">
      <xsl:if test="tei:head">
         <xsl:text> \par\textit{</xsl:text>
         <xsl:for-each select="tei:head">
            <xsl:apply-templates/>
         </xsl:for-each>
         <xsl:text>}
</xsl:text>
      </xsl:if> \begin{itemize} <xsl:apply-templates/> \end{itemize} </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementp/tei:stage</desc>
   </doc>
  <xsl:template match="tei:p/tei:stage">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementrole</desc>
   </doc>
  <xsl:template match="tei:role">
      <xsl:text>\textbf{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementroleDesc</desc>
   </doc>
  <xsl:template match="tei:roleDesc">
      <xsl:text>\begin{quote}</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>\end{quote}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementset</desc>
   </doc>
  <xsl:template match="tei:set">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementsound</desc>
   </doc>
  <xsl:template match="tei:sound">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementsp</desc>
   </doc>
  <xsl:template match="tei:sp"> \begin{description} \item[<xsl:apply-templates select="tei:speaker"/>] <xsl:apply-templates select="tei:p | tei:l | tei:lg | tei:seg |      tei:ab | tei:stage"/>
      <xsl:text>\end{description}
</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementsp/tei:p</desc>
   </doc>
  <xsl:template match="tei:sp/tei:p">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementstage</desc>
   </doc>
  <xsl:template match="tei:stage">
      <xsl:text>
\par
</xsl:text>
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}\par </xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementsp/tei:stage</desc>
   </doc>
  <xsl:template match="tei:sp/tei:stage">
      <xsl:text/>
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>} </xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementtech</desc>
   </doc>
  <xsl:template match="tei:tech">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementview</desc>
   </doc>
  <xsl:template match="tei:view">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
</xsl:stylesheet>
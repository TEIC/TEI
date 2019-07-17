<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the figures module,
      making LaTeX output. </p>
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
      <desc>Process element cell</desc>
   </doc>
  <xsl:template match="tei:cell">
      <xsl:if test="preceding-sibling::tei:cell">\tabcellsep </xsl:if>
      <xsl:choose>
         <xsl:when test="@role='label'">
            <xsl:text>\Panel{</xsl:text>
            <xsl:if test="starts-with(normalize-space(.),'[')">
               <xsl:text>{}</xsl:text>
            </xsl:if>
            <xsl:apply-templates/>
            <xsl:text>}{label}{</xsl:text>
            <xsl:choose>
               <xsl:when test="@cols">
                  <xsl:value-of select="@cols"/>
               </xsl:when>
               <xsl:otherwise>1</xsl:otherwise>
            </xsl:choose>
            <xsl:text>}{</xsl:text>
            <xsl:choose>
               <xsl:when test="@align='right'">r</xsl:when>
               <xsl:when test="@align='centre'">c</xsl:when>
               <xsl:when test="@align='center'">c</xsl:when>
               <xsl:when test="@align='left'">l</xsl:when>
               <xsl:otherwise>l</xsl:otherwise>
            </xsl:choose>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="@cols &gt; 1">
            <xsl:text>\multicolumn{</xsl:text>
            <xsl:value-of select="@cols"/>
            <xsl:text>}{</xsl:text>
	    <xsl:if test="@role='label' or
			  parent::tei:row/@role='label'">
	      <xsl:text>){\columncolor{label}}</xsl:text>
	    </xsl:if>
            <xsl:choose>
               <xsl:when test="@align='right'">r</xsl:when>
               <xsl:when test="@align='centre'">c</xsl:when>
               <xsl:when test="@align='center'">c</xsl:when>
               <xsl:when test="@align='left'">l</xsl:when>
               <xsl:otherwise>l</xsl:otherwise>
            </xsl:choose>
	    <xsl:text>}{</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>}</xsl:text>
         </xsl:when>
	 <xsl:when test="@align">
            <xsl:text>\multicolumn{1}{</xsl:text>
	    <xsl:if test="@role='label' or
			  parent::tei:row/@role='label'">
	      <xsl:text>){\columncolor{label}}</xsl:text>
	    </xsl:if>
            <xsl:choose>
               <xsl:when test="@align='right'">r</xsl:when>
               <xsl:when test="@align='centre'">c</xsl:when>
               <xsl:when test="@align='center'">c</xsl:when>
               <xsl:when test="@align='left'">l</xsl:when>
               <xsl:otherwise>l</xsl:otherwise>
            </xsl:choose>
	    <xsl:text>}{</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>}</xsl:text>
	 </xsl:when>
         <xsl:otherwise>
            <xsl:if test="starts-with(normalize-space(.),'[')">
               <xsl:text>{}</xsl:text>
            </xsl:if>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figDesc</desc>
   </doc>
  <xsl:template match="tei:figDesc"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element figure</desc>
   </doc>
  <xsl:template match="tei:figure">
      <xsl:call-template name="makeFigureStart"/>
      <xsl:apply-templates/>
      <xsl:call-template name="makeFigureEnd"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element graphic</desc>
   </doc>
  <xsl:template match="tei:graphic|tei:media">
      <xsl:call-template name="makePic"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element row</desc>
   </doc>
  <xsl:template match="tei:row">
      <xsl:if test="@role='label'">\rowcolor{label}</xsl:if>
      <xsl:apply-templates/>
      <xsl:if test="following-sibling::tei:row">
         <xsl:text>\\</xsl:text>
         <xsl:if test="@role='label' or parent::tei:table/tei:match(@rend,'rules')">\hline </xsl:if>
         <xsl:text>&#10;</xsl:text>
      </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table</desc>
   </doc>
  <xsl:template match="tei:table" mode="xref">
      <xsl:text>the table on p. \pageref{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table</desc>
   </doc>
  <xsl:template match="tei:table">
    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:text> \par </xsl:text>
      <xsl:choose>
         <xsl:when test="ancestor::tei:table or $longtables='false'"> 
	           <xsl:text>\begin{tabular}</xsl:text>
	           <xsl:call-template name="makeTable"/> 
	           <xsl:text>\end{tabular}</xsl:text>
         </xsl:when>
         <xsl:otherwise> 
	           <xsl:text>&#10;\begin{longtable}</xsl:text>
	           <xsl:call-template name="makeTable"/>
	           <xsl:text>\end{longtable} \par&#10; </xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table[tei:match(@rend,'display')]</desc>
   </doc>
  <xsl:template match="tei:table[tei:match(@rend,'display')]" mode="xref">
      <xsl:text>Table </xsl:text>
      <xsl:number count="tei:table[tei:match(@rend,'display')]" level="any"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element table[tei:match(@rend,'display')]</desc>
   </doc>
  <xsl:template match="tei:table[tei:match(@rend,'display')]">
      <xsl:text>\begin{table}</xsl:text>
      <xsl:text>\begin{center} \begin{small} \begin{tabular}</xsl:text>
      <xsl:call-template name="makeTable"/> 
     <xsl:text>\end{tabular} 
      \caption{</xsl:text>
      <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:apply-templates mode="ok" select="tei:head"/>
      <xsl:text>}
     \end{small} 
     \end{center}
     \end{table}</xsl:text>
     </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] Make figure (start)</desc>
   </doc>
  <xsl:template name="makeFigureStart">
      <xsl:choose>
	<xsl:when test="@place='inline' and tei:head">
            <xsl:text>\begin{figure}[H]&#10;</xsl:text>
	</xsl:when>
	<xsl:when test="tei:match(@rend,'display') or not(@place='inline') or tei:head or tei:p">
	  <xsl:text>\begin{figure}[htbp]&#10;</xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:choose>
	<xsl:when test="tei:match(@rend,'center')">
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
         <xsl:when test="tei:head or tei:p">
            <xsl:text>&#10;\caption{</xsl:text>
	    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
            <xsl:for-each select="tei:head">
	      <xsl:apply-templates/>
	    </xsl:for-each>
            <xsl:text>}</xsl:text>
         </xsl:when>
      </xsl:choose>
      <xsl:if test="tei:match(@rend,'center')">
            <xsl:text>\end{center}</xsl:text>
      </xsl:if>
      <xsl:choose>
	<xsl:when test="@place='inline' and tei:head">
            <xsl:text>\end{figure}&#10;</xsl:text>
	</xsl:when>
         <xsl:when test="tei:match(@rend,'display') or not(@place='inline')">
	   <xsl:text>\end{figure}&#10;</xsl:text>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] Make picture</desc>
   </doc>
  <xsl:template name="makePic">
    <xsl:sequence select="tei:makeHyperTarget(@xml:id)"/>
      <xsl:choose>
	<xsl:when test="tei:match(@rend,'noindent')">
	  <xsl:text>\noindent</xsl:text>
	</xsl:when>
	<xsl:when test="not(preceding-sibling::*)">
	  <xsl:text>\noindent</xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:variable name="pic">
	<xsl:text>\includegraphics[</xsl:text>
	<xsl:call-template name="graphicsAttributes">
          <xsl:with-param name="mode">latex</xsl:with-param>
	</xsl:call-template>
	<xsl:text>]{</xsl:text>
	<xsl:choose>
          <xsl:when test="$realFigures='true'">
	    <xsl:sequence select="tei:resolveURI(.,@url)"/>
          </xsl:when>
          <xsl:otherwise>
	    <xsl:variable name="F">
	      <xsl:sequence select="tei:resolveURI(.,@url)"/>
	    </xsl:variable>
	    <xsl:text>Pictures/resource</xsl:text>
	    <xsl:number level="any"/>
	    <xsl:text>.</xsl:text>
	    <xsl:value-of select="tokenize($F,'\.')[last()]"/>
          </xsl:otherwise>
	</xsl:choose>
	<xsl:text>}</xsl:text>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="parent::tei:ref">
	  <xsl:value-of select="$pic"/>
	</xsl:when>
	<xsl:when test="not
			(following-sibling::tei:graphic[tei:match(@rend,'alignright')])
			and tei:match(@rend,'alignright')">
	  <xsl:text>\begin{wrapfigure}{r}{</xsl:text>
	  <xsl:value-of select="@width"/>
	  <xsl:text>}</xsl:text>
	  <xsl:value-of select="$pic"/>
	  <xsl:text>\end{wrapfigure}</xsl:text>
	</xsl:when>
	<xsl:when test="not (following-sibling::tei:graphic[tei:match(@rend,'alignleft')])
			and tei:match(@rend,'alignleft')">
	  <xsl:text>\begin{wrapfigure}{l}{</xsl:text>
	  <xsl:value-of select="@width"/>
	  <xsl:text>}</xsl:text>
	  <xsl:value-of select="$pic"/>
	  <xsl:text>\end{wrapfigure}</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$pic"/>
	</xsl:otherwise>

      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] </desc>
   </doc>
  <xsl:template name="makeTable">
      <xsl:variable name="r">
         <xsl:value-of select="@rend"/>
      </xsl:variable>
      <xsl:text>{</xsl:text>
      <xsl:if test="contains($r,'rules')">|</xsl:if>
      <xsl:choose>
         <xsl:when test="@preamble">
            <xsl:value-of select="@preamble"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="makePreamble-complex">
               <xsl:with-param name="r" select="$r"/>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>}&#10;</xsl:text>
      <xsl:if test="contains($r,'rules') or tei:head">
	<xsl:call-template name="tableHline"/>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="tei:head and not(tei:match(@rend,'display'))">
            <xsl:if test="not(ancestor::tei:table or $longtables='false')">
               <xsl:text>\endfirsthead </xsl:text>
               <xsl:text>\multicolumn{</xsl:text>
               <xsl:value-of select="count(tei:row[1]/tei:cell)"/>
               <xsl:text>}{c}{</xsl:text>
               <xsl:apply-templates mode="ok" select="tei:head"/>
               <xsl:text>(cont.)}\\\hline \endhead </xsl:text>
            </xsl:if>
            <xsl:text>\caption{</xsl:text>
            <xsl:apply-templates mode="ok" select="tei:head"/>
            <xsl:text>}\\ </xsl:text>
         </xsl:when>
         <xsl:otherwise> </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="contains($r,'rules') or tei:row[1][@role='label']">\hline </xsl:if>
      <xsl:apply-templates/>
      <xsl:if test="contains($r,'rules')">
         <xsl:text>\\ \hline </xsl:text>
      </xsl:if>
  </xsl:template>

   <xsl:template name="tableHline">
      <xsl:choose>
         <xsl:when test="ancestor::tei:table or $longtables='false' or tei:match(@rend,'display')"> \hline </xsl:when>
         <xsl:otherwise> \hline\endfoot\hline\endlastfoot </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] </desc>
   </doc>
  <xsl:template name="makePreamble-complex">
      <xsl:param name="r"/>
      <xsl:variable name="valign">
         <xsl:choose>
	   <xsl:when test="contains($r,'bottomAlign')">B</xsl:when>
	   <xsl:when test="contains($r,'midAlign')">M</xsl:when>
	   <xsl:otherwise>P</xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="tds">
	<xsl:for-each select="tei:row">
	  <xsl:variable name="row">
	    <xsl:for-each select="tei:cell">
	      <xsl:variable name="stuff">
		<xsl:apply-templates/>
	      </xsl:variable>
	      <cell>
		<xsl:value-of select="string-length($stuff)"/>
	      </cell>
	      <xsl:if test="@cols">
		<xsl:variable name="c" select="xs:integer(@cols) - 1 "/>
		<xsl:for-each select="1 to $c">
		  <cell>0</cell>
		</xsl:for-each>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:for-each select="$row/cell">
	    <cell col="{position()}">
	      <xsl:value-of select="."/>
	    </cell>
	  </xsl:for-each>
	</xsl:for-each>
      </xsl:variable>
      <xsl:variable name="total">
	<xsl:value-of select="sum($tds/cell)"/>
      </xsl:variable>
      <xsl:for-each select="$tds/cell">
	<xsl:variable name="c" select="@col"/>
	<xsl:if test="not(preceding-sibling::cell[$c=@col])">
	  <xsl:variable name="len">
	    <xsl:value-of select="sum(following-sibling::cell[$c=@col]) + current()"/>
	  </xsl:variable>
	  <xsl:value-of select="$valign"/>
	  <xsl:text>{</xsl:text>
	  <xsl:value-of select="($len div $total) * $tableMaxWidth"/>
	  <xsl:text>\textwidth}</xsl:text>
	  <xsl:if test="contains($r,'rules')">|</xsl:if>
	</xsl:if>
      </xsl:for-each>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] </desc>
   </doc>
  <xsl:template name="makePreamble-simple">
      <xsl:param name="r"/>
      <xsl:for-each select="tei:row[1]/tei:cell">
         <xsl:text>l</xsl:text>
         <xsl:if test="contains($r,'rules')">|</xsl:if>
      </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

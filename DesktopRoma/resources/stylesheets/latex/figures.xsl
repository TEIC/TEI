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
    <xd:short> TEI stylesheet dealing with elements from the figures module,
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
    <xd:cvsId>$Id: figures.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:cell</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
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
        <xsl:text>}{P{.99\textwidth}}{</xsl:text>
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
  <xd:doc>
    <xd:short>Process elements tei:figDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figDesc"/>
  <xd:doc>
    <xd:short>Process elements tei:figure</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figure">
    <xsl:call-template name="makeFigureStart"/>
    <xsl:choose>
      <xsl:when test="@url or @entity">
        <xsl:call-template name="makePic"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="makeFigureEnd"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:graphic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:graphic">
    <xsl:call-template name="makePic"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:row</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:row">
    <xsl:if test="@role='label'">\rowcolor{label}</xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="following-sibling::tei:row">
      <xsl:text>\\</xsl:text>
      <xsl:if test="@role='label'">\hline </xsl:if>
      <xsl:text>&#10;</xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table" mode="xref">
    <xsl:text>the table on p. \pageref{</xsl:text>
    <xsl:value-of select="@xml:id"/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table">
    <xsl:if test="@xml:id">
      <xsl:text>\label{</xsl:text>
      <xsl:value-of   select="@xml:id"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:text> \par </xsl:text>
    <xsl:choose>
      <xsl:when test="ancestor::tei:table"> 
	<xsl:text>\begin{tabular}</xsl:text>
	<xsl:call-template  name="makeTable"/> 
	<xsl:text>\end{tabular}</xsl:text>
      </xsl:when>
      <xsl:otherwise> 
	<xsl:text>\begin{longtable}</xsl:text>
	<xsl:call-template name="makeTable"/>
	<xsl:text>\end{longtable} \par </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table[@rend='display']" mode="xref">
    <xsl:text>Table </xsl:text>
    <xsl:number count="tei:table[@rend='display']" level="any"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table[@rend='display']">
    <xsl:text>\begin{table}</xsl:text>
      <xsl:if  test="@xml:id">
	<xsl:text>\hypertarget{</xsl:text>
	<xsl:value-of select="@xml:id"/>
	<xsl:text>}{}</xsl:text>
      </xsl:if>
      <xsl:text>\begin{center} \begin{small} \begin{tabular}</xsl:text>
      <xsl:call-template  name="makeTable"/> 
     <xsl:text>\end{tabular} 
      \caption{</xsl:text>
      <xsl:apply-templates mode="ok" select="tei:head"/>
      <xsl:text>}
     \end{small} 
     \end{center}
     \end{table}</xsl:text>
     </xsl:template>
  <xd:doc>
    <xd:short>[latex] Make figure (start)</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeFigureStart">
    <xsl:choose>
      <xsl:when test="@rend='display' or not(@place='inline') or tei:head or tei:p">
        <xsl:text>\begin{figure}[htbp]
      </xsl:text>
      </xsl:when>
      <xsl:when test="@rend='centre'">
        <xsl:text>\par\centerline{</xsl:text>
      </xsl:when>
      <xsl:otherwise>\noindent</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] Make figure (end)</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeFigureEnd">
    <xsl:choose>
      <xsl:when test="tei:head or tei:p">
        <xsl:text>&#10;	\caption{</xsl:text>
        <xsl:for-each select="tei:head">
	  <xsl:apply-templates/>
	</xsl:for-each>
        <xsl:text>}</xsl:text>
        <xsl:if test="@xml:id">\hypertarget{<xsl:value-of select="@xml:id"/>}{}</xsl:if>
      </xsl:when>
      <xsl:when test="@rend='centre'">
        <xsl:text>}\par </xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="@rend='display' or not(@place='inline')">
	<xsl:text>\end{figure}
	</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] Make picture</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makePic">
    <xsl:if test="@xml:id">\hypertarget{<xsl:value-of select="@xml:id"/>}{}</xsl:if>
    <xsl:if test="@rend='centre'">
      <xsl:text>\centerline{</xsl:text>
    </xsl:if>
    <xsl:text>\includegraphics[</xsl:text>
    <xsl:call-template name="graphicsAttributes">
      <xsl:with-param name="mode">latex</xsl:with-param>
    </xsl:call-template>
    <xsl:text>]{</xsl:text>
    <xsl:choose>
      <xsl:when test="$realFigures='true'">
        <xsl:choose>
          <xsl:when test="@url">
            <xsl:value-of select="@url"/>
          </xsl:when>
          <xsl:when test="@entity">
            <xsl:value-of select="unparsed-entity-uri(@entity)"/>
          </xsl:when>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="c">
          <xsl:for-each select="ancestor-or-self::tei:figure[1]">
            <xsl:number level="any"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:text>FIG</xsl:text>
        <xsl:value-of select="$c+1000"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}</xsl:text>
    <xsl:if test="@rend='centre'">
      <xsl:text>}</xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
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
      <xsl:when test="function-available('exsl:node-set')">
        <xsl:call-template name="makePreamble-complex">
          <xsl:with-param name="r" select="$r"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="makePreamble-simple">
          <xsl:with-param name="r" select="$r"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>}&#10;</xsl:text>
    <xsl:call-template name="tableHline"/>
    <xsl:choose>
      <xsl:when test="tei:head and not(@rend='display')">
        <xsl:if test="not(ancestor::tei:table)">
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
    <xsl:if test="contains($r,'rules')">\hline </xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="contains($r,'rules')">
      <xsl:text>\\ \hline </xsl:text>
    </xsl:if>
  </xsl:template>

<xsl:template name="tableHline">
    <xsl:choose>
      <xsl:when test="ancestor::tei:table or @rend='display'"> \hline </xsl:when>
      <xsl:otherwise> \hline\endfoot\hline\endlastfoot </xsl:otherwise>
    </xsl:choose>
</xsl:template>

  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
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
      <xsl:for-each select=".//tei:cell">
        <xsl:variable name="stuff">
          <xsl:apply-templates/>
        </xsl:variable>
        <cell>
          <xsl:attribute name="col">
            <xsl:number/>
          </xsl:attribute>
          <xsl:value-of select="string-length($stuff)"/>
        </cell>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="total">
      <xsl:value-of select="sum(exsl:node-set($tds)/cell)"/>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($tds)/cell">
      <xsl:sort data-type="number" select="@col"/>
      <xsl:variable name="c" select="@col"/>
      <xsl:if test="not(preceding-sibling::cell[$c=@col])">
        <xsl:variable name="len">
          <xsl:value-of
            select="sum(following-sibling::cell[$c=@col]) + current()"/>
        </xsl:variable>
	<xsl:value-of select="$valign"/>
        <xsl:text>{</xsl:text>
        <xsl:value-of select="($len div $total) * $tableMaxWidth"/>
        <xsl:text>\textwidth}</xsl:text>
        <xsl:if test="contains($r,'rules')">|</xsl:if>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makePreamble-simple">
    <xsl:param name="r"/>
    <xsl:for-each select="tei:row[1]/tei:cell">
      <xsl:text>l</xsl:text>
      <xsl:if test="contains($r,'rules')">|</xsl:if>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>

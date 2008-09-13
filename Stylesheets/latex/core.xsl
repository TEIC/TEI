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
    <xd:short> TEI stylesheet dealing with elements from the core module, making
      LaTeX output. </xd:short>
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
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:ab</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ab">
    <xsl:apply-templates/>
    <xsl:if test="following-sibling::tei:ab">\par </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:bibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:bibl" mode="cite">
    <xsl:apply-templates select="text()[1]"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:bibl/tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:bibl/tei:title">
    <xsl:if test="preceding-sibling::tei:title"> </xsl:if>
    <xsl:choose>
      <xsl:when test="@level='a'">
        <xsl:text>`</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>'</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\textit{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:code</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:code">\texttt{<xsl:apply-templates/>}</xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:corr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:corr">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="@sic">
      <xsl:text>\footnote{</xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">appearsintheoriginalas</xsl:with-param>
                </xsl:call-template>
                <xsl:text> \emph{</xsl:text>
                <xsl:value-of select="./@sic"/><xsl:text>}.}</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:supplied</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:supplied">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
    <xsl:choose>
      <xsl:when test="@reason">
        <xsl:text>\footnote{</xsl:text>
        <xsl:value-of select="./@reason"/>
        <xsl:text>}</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:sic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/><xsl:text> (sic)</xsl:text>
    <xsl:choose>
      <xsl:when test="@corr">
      <xsl:text>\footnote{</xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">shouldbereadas</xsl:with-param>
                </xsl:call-template>
                <xsl:text> \emph{</xsl:text>
                <xsl:value-of select="./@corr"/><xsl:text>}.}</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:eg|tei:q[@rend='eg']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg|tei:q[@rend='eg']">
    <xsl:choose>
      <xsl:when test="ancestor::tei:cell and count(*)=1 and string-length(.)&lt;60">
	<xsl:variable name="stuff">
	  <xsl:apply-templates mode="eg"/>
	</xsl:variable>
	<xsl:text>\fbox{\ttfamily </xsl:text>
	<xsl:value-of select="translate($stuff,
	  '\{}','&#8421;&#10100;&#10101;')"/>
	<xsl:text>} </xsl:text>
      </xsl:when>
      <xsl:when test="ancestor::tei:cell and not(*)  and string-length(.)&lt;60">
	<xsl:variable name="stuff">
	  <xsl:apply-templates mode="eg"/>
	</xsl:variable>
	<xsl:text>\fbox{\ttfamily </xsl:text>
	<xsl:value-of select="translate($stuff,
	  '\{}','&#8421;&#10100;&#10101;')"/>
	<xsl:text>} </xsl:text>
      </xsl:when>
      <xsl:when test="ancestor::tei:cell">
<xsl:text>\mbox{}\hfill\\[-10pt]\begin{Verbatim}[fontsize=\small]&#10;</xsl:text>
	<xsl:apply-templates mode="eg"/>
	<xsl:text>&#10;\end{Verbatim}&#10;</xsl:text>
<!--
<xsl:text>\mbox{}\newline
\bgroup\exampleFontSet
\noindent\obeylines\obeyspaces </xsl:text>
<xsl:apply-templates mode="eg"/>
<xsl:text>\egroup </xsl:text>
-->
      </xsl:when>
      <xsl:when test="ancestor::tei:list[@type='gloss']">
	<xsl:text>\hspace{1em}\hfill\linebreak</xsl:text>
<xsl:text>\bgroup\exampleFontSet\vskip 10pt
\begin{shaded}
\noindent\obeylines\obeyspaces </xsl:text>
<xsl:apply-templates mode="eg"/>
<xsl:text>\end{shaded}
\egroup </xsl:text>
      </xsl:when>
      <xsl:otherwise>
<xsl:text>\par\bgroup\exampleFontSet\vskip 10pt
\begin{shaded}
\obeylines\obeyspaces </xsl:text>
<xsl:apply-templates mode="eg"/>
<xsl:text>\end{shaded}
\par\egroup </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
<!--
    <xsl:choose>
      <xsl:when test="@n">
	<xsl:text>&#10;\begin{Verbatim}[fontsize=\scriptsize,numbers=left,label={</xsl:text>
	<xsl:value-of select="@n"/>
      <xsl:text>}]&#10;</xsl:text>
      <xsl:apply-templates mode="eg"/> 
      <xsl:text>&#10;\end{Verbatim}&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>&#10;\begin{Verbatim}[fontsize=\scriptsize,frame=single]&#10;</xsl:text>
	<xsl:apply-templates mode="eg"/>
	<xsl:text>&#10;\end{Verbatim}&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
-->
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:emph</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:emph">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:foreign</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:foreign">
    <xsl:text>\textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:gi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi">\texttt{&lt;<xsl:apply-templates/>&gt;}</xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:head</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:head">
    <xsl:choose>
      <xsl:when test="parent::tei:castList"/>
      <xsl:when test="parent::tei:figure"/>
      <xsl:when test="parent::tei:list"/>
      <xsl:when test="parent::tei:lg"> \subsection*{<xsl:apply-templates/>} </xsl:when>
      <xsl:when test="parent::tei:table"/>
      <xsl:when test="parent::tei:div1[@type='letter']"/>
      <xsl:when test="parent::tei:div[@type='letter']"/>
      <xsl:when test="parent::tei:div[@type='bibliography']"/>
      <xsl:otherwise>
        <xsl:variable name="depth">
          <xsl:apply-templates mode="depth" select=".."/>
        </xsl:variable>
        <xsl:text>&#10;\Div</xsl:text>
        <xsl:choose>
          <xsl:when test="$depth=0">I</xsl:when>
          <xsl:when test="$depth=1">II</xsl:when>
          <xsl:when test="$depth=2">III</xsl:when>
          <xsl:when test="$depth=3">IV</xsl:when>
          <xsl:when test="$depth=4">V</xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="ancestor::tei:floatingText">Star</xsl:when>
          <xsl:when test="parent::tei:div/@rend='nonumber'">Star</xsl:when>
          <xsl:when
            test="ancestor::tei:back and $numberBackHeadings='false'"
            >Star</xsl:when>
	  <xsl:when test="$numberHeadings='false' and
			  ancestor::tei:body">Star</xsl:when>
          <xsl:when
            test="ancestor::tei:front and $numberFrontHeadings='false'"
            >Star</xsl:when>
        </xsl:choose>
	<xsl:text>[</xsl:text>
	<xsl:value-of select="normalize-space(.)"/>
	<xsl:text>]</xsl:text>
	<xsl:text>{</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>}</xsl:text>
	<xsl:if test="../@xml:id">
	  <xsl:text>\label{</xsl:text>
	  <xsl:value-of select="../@xml:id"/>
	  <xsl:text>}</xsl:text>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:gloss</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gloss">
    <xsl:text> \textit{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:hi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:hi">
    <xsl:call-template name="rendering"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Rendering rules, turning @rend into LaTeX commands</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rendering">
    <xsl:variable name="cmd">
      <xsl:choose>
        <xsl:when test="@rend='bold'">textbf</xsl:when>
        <xsl:when test="@rend='center'">centerline</xsl:when>
        <xsl:when test="@rend='code'">texttt</xsl:when>
        <xsl:when test="@rend='ital'">textit</xsl:when>
        <xsl:when test="@rend='italic'">textit</xsl:when>
        <xsl:when test="@rend='it'">textit</xsl:when>
        <xsl:when test="@rend='italics'">textit</xsl:when>
        <xsl:when test="@rend='i'">textit</xsl:when>
        <xsl:when test="@rend='sc'">textsc</xsl:when>
        <xsl:when test="@rend='plain'">textrm</xsl:when>
        <xsl:when test="@rend='quoted'">textquoted</xsl:when>
        <xsl:when test="@rend='sup'">textsuperscript</xsl:when>
        <xsl:when test="@rend='sub'">textsubscript</xsl:when>
        <xsl:when test="@rend='important'">textbf</xsl:when>
        <xsl:when test="@rend='ul'">uline</xsl:when>
        <xsl:when test="@rend='overbar'">textoverbar</xsl:when>
        <xsl:when test="@rend='expanded'">textsc</xsl:when>
        <xsl:when test="@rend='strike'">sout</xsl:when>
        <xsl:when test="@rend='small'">textsmall</xsl:when>
        <xsl:when test="@rend='large'">textlarge</xsl:when>
        <xsl:when test="@rend='smaller'">textsmaller</xsl:when>
        <xsl:when test="@rend='larger'">textlarger</xsl:when>
        <xsl:when test="@rend='calligraphic'">textcal</xsl:when>
        <xsl:when test="@rend='gothic'">textgothic</xsl:when>
        <xsl:when test="@rend='noindex'">textrm</xsl:when>
        <xsl:otherwise>textbf</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>\</xsl:text>
    <xsl:value-of select="$cmd"/>
    <xsl:text>{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:hr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:hr"> \hline </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:ident</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ident">
    <xsl:text>\textsf{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item"> 
    <xsl:text>&#10;\item</xsl:text>
    <xsl:if test="@n">[<xsl:value-of
        select="@n"/>]</xsl:if>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="gloss"> 
      <xsl:text>\item[</xsl:text>
      <xsl:apply-templates 
	  select="preceding-sibling::tei:label[1]" mode="gloss"/>
      <xsl:text>]</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:label in normal mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label"/>

  <xd:doc>
    <xd:short>Process elements tei:label in normal mode, inside an item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item/tei:label">
    <xsl:text>\textbf{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:label in gloss mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label" mode="gloss">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lb">
    <xsl:text>{\hskip1pt}\newline </xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:list</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list">
    <xsl:if test="tei:head"> \leftline{\textbf{<xsl:for-each select="tei:head">
        <xsl:apply-templates/>
      </xsl:for-each>}} </xsl:if>
      <xsl:if test="@xml:id">
	<xsl:text>\label{</xsl:text>
	<xsl:value-of  select="@xml:id"/>
	<xsl:text>}</xsl:text>
      </xsl:if>
    <xsl:choose>
      <xsl:when test="not(tei:item)"/>
      <xsl:when test="@type='gloss' or tei:label"> \begin{description}<xsl:apply-templates
          mode="gloss" select="tei:item"/> \end{description} </xsl:when>
      <xsl:when test="@type='unordered'"> \begin{itemize}<xsl:apply-templates/>
        \end{itemize} </xsl:when>
      <xsl:when test="@type='ordered'"> \begin{enumerate}<xsl:apply-templates/>
        \end{enumerate} </xsl:when>
      <xsl:when test="@type='runin'">
        <xsl:apply-templates mode="runin" select="tei:item"/>
      </xsl:when>
      <xsl:otherwise> \begin{itemize}<xsl:apply-templates/> \end{itemize}
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:listBibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="tei:listBibl">
    <xsl:choose>
      <xsl:when test="tei:biblStruct">
	<xsl:text>\begin{bibitemlist}{1}&#10;</xsl:text>
	  <xsl:for-each select="tei:biblStruct">
	    <xsl:sort select="translate(tei:*/tei:author/tei:surname|tei:*[1]/tei:author/tei:orgName|tei:*[1]/tei:author/tei:name|tei:*[1]/tei:editor/tei:surname|tei:*[1]/tei:editor/tei:name|tei:*[1]/tei:title,$uc,$lc)"/>
	    <xsl:sort select="tei:monogr/tei:imprint/tei:date"/>
	    <xsl:text>\bibitem[</xsl:text>
	      <xsl:apply-templates select="." mode="xref"/>
	      <xsl:text>]{</xsl:text>
	      <xsl:value-of select="@xml:id"/>
	      <xsl:text>}\hypertarget{</xsl:text>
	      <xsl:value-of select="@xml:id"/>
	      <xsl:text>}{}</xsl:text>
	      <xsl:apply-templates select="."/>
	  </xsl:for-each>
	  <xsl:text>&#10;\end{bibitemlist}&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>\begin{bibitemlist}{1}&#10;</xsl:text>
	<xsl:apply-templates/> 
	<xsl:text>&#10;\end{bibitemlist}&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements tei:listBibl/tei:bibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:listBibl/tei:bibl"> \bibitem {<xsl:choose>
      <xsl:when test="@xml:id">
        <xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>bibitem-<xsl:number level="any"/></xsl:otherwise>
    </xsl:choose>}<xsl:apply-templates/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:mentioned</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:mentioned">
    <xsl:text>\emph{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note">
    <xsl:if test="@xml:id">
      <xsl:text>\hypertarget{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}{}</xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@place='inline' or ancestor::tei:bibl or ancestor::tei:biblStruct"> 
	<xsl:text>(</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>) </xsl:text>
      </xsl:when>
      <xsl:when test="@place='end'">
        <xsl:text>\endnote{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:when test="@target">
        <xsl:text>\footnotetext{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>\footnote{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p">
  <xsl:text>\par </xsl:text>
  <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element tei:pb</xd:short>
    <xd:detail>Indication of a page break. We make it an anchor if it has an
    ID.</xd:detail>
  </xd:doc>
  
<xsl:template match="tei:pb">
   <!-- string " Page " is now managed through the i18n file -->
    <xsl:choose>
      <xsl:when test="$pagebreakStyle='active'">
        <xsl:text>\clearpage </xsl:text>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='visible'">
        <xsl:text>✁[</xsl:text>
        <xsl:value-of select="@unit"/>
        <xsl:text> </xsl:text>
        <xsl:call-template name="i18n">
           <xsl:with-param name="word">page</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@n"/>
        <xsl:text>]✁</xsl:text>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='bracketsonly'"> <!-- To avoid trouble with the scisssors character "✁" -->
        <xsl:text>[</xsl:text>
        <xsl:value-of select="@unit"/>
        <xsl:text> </xsl:text>
        <xsl:call-template name="i18n">
           <xsl:with-param name="word">page</xsl:with-param>
        </xsl:call-template>
        <xsl:text> </xsl:text>
        <xsl:value-of select="@n"/>
        <xsl:text>]</xsl:text>
      </xsl:when>
      <xsl:otherwise> </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="@xml:id">
      <xsl:text>\hypertarget{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}{}</xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:q</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q">
    <xsl:choose>
      <xsl:when test="tei:p"> \begin{quote}<xsl:apply-templates/> \end{quote} </xsl:when>
      <xsl:when test="tei:text">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="tei:lg"> \begin{quote}<xsl:apply-templates/> \end{quote} </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:quote</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:quote">
    <xsl:choose>
      <xsl:when test="parent::tei:cit">
        <xsl:text>`</xsl:text>
          <xsl:apply-templates/>
        <xsl:text>'</xsl:text>
      </xsl:when>
      <xsl:when test="contains(concat(' ', @rend, ' '), ' quoted ')">
        <xsl:value-of select="$preQuote"/>
        <xsl:apply-templates/>
        <xsl:value-of select="$postQuote"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>\begin{quote}</xsl:text>
	<xsl:apply-templates/>
	<xsl:text>\end{quote}</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements p[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p[@rend='display']"> \begin{quote}
    <xsl:apply-templates/> \end{quote}</xsl:template>
  <xd:doc>
    <xd:short>Process elements q[@rend='display']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q[@rend='display']"> \begin{quote}
    <xsl:apply-templates/> \end{quote}</xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:titlePart</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:titlePart">
    <xsl:if test="ancestor::tei:group"> \part{<xsl:apply-templates/>} </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:xref[@type='cite']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:xref[@type='cite']">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process text(), escaping the LaTeX command characters.</xd:short>
    <xd:detail>We need the backslash and two curly braces to insert LaTeX
      commands into the output, so these characters need to replaced when they
      are found in running text. They are translated to Unicode COMBINING
      REVERSE SOLIDUS OVERLAY, MEDIUM LEFT CURLY BRACKET ORNAMENT and MEDIUM
      RIGHT CURLY BRACKET ORNAMENT; if these are used in real text, the escape
      will have to be changed. They are translated back to the correct
      characters by appropriate definitions in the preamble (see the template
      called latexSetup in tei-param.xsl).</xd:detail>
  </xd:doc>
  <xsl:template match="text()"> 
    <xsl:value-of
	select="translate(.,'\{}','&#8421;&#10100;&#10101;')"/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process attributes in text mode, escaping the LaTeX
    command characters.</xd:short>
    <xd:detail>as with text()</xd:detail>
  </xd:doc>
  <xsl:template match="@*" mode="attributetext">
    <xsl:value-of
	select="translate(.,'\{}','&#8421;&#10100;&#10101;')"/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements text()</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="text()" mode="eg">
    <xsl:choose>
      <xsl:when test="starts-with(.,'&#10;')">
        <xsl:value-of select="substring-after(translate(.,'\{}','&#8421;&#10100;&#10101;'),'&#10;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of 
	    select="translate(.,'\{}','&#8421;&#10100;&#10101;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[latex] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="bibliography">
    <xsl:apply-templates mode="biblio"
      select="//tei:xref[@type='cite'] | //tei:xptr[@type='cite'] | //tei:ref[@type='cite'] | //tei:ptr[@type='cite']"
    />
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:soCalled</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:soCalled">    
    <xsl:value-of select="$preQuote"/>
    <xsl:apply-templates/>
    <xsl:value-of select="$postQuote"/>
  </xsl:template>

<xsl:template name="emphasize">
  <xsl:param name="class"/>
  <xsl:param name="content"/>
  <xsl:choose>
    <xsl:when test="$class='titlem'">
      <xsl:text>\textit{</xsl:text>
      <xsl:copy-of select="$content"/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:when test="$class='titlej'">
      <xsl:text>\textit{</xsl:text>
      <xsl:copy-of select="$content"/>
      <xsl:text>}</xsl:text>
    </xsl:when>
    <xsl:when test="$class='titlea'">
      <xsl:text>‘</xsl:text>
	<xsl:copy-of select="$content"/>
      <xsl:text>’</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$content"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


  <xsl:template name="Text">
    <xsl:param name="words"/>
    <xsl:value-of select="translate($words,'\{}','&#8421;&#10100;&#10101;')"/>
  </xsl:template>

  <xsl:template name="applyRendition"/>

<xsl:template match="tei:seg">
  <xsl:apply-templates/>
</xsl:template>

</xsl:stylesheet>

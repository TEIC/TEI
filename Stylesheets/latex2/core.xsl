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
         <p> TEI stylesheet dealing with elements from the core module, making
      LaTeX output. </p>
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
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element ab</desc>
   </doc>
  <xsl:template match="tei:ab">
      <xsl:apply-templates/>
      <xsl:if test="following-sibling::tei:ab">\par </xsl:if>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element bibl</desc>
   </doc>
  <xsl:template match="tei:bibl" mode="cite">
      <xsl:apply-templates select="text()[1]"/>
  </xsl:template>


  <xsl:template match="tei:cit">
    <xsl:choose>
      <xsl:when test="@rend='display' or tei:q/tei:p or
		      tei:quote/tei:l or tei:quote/tei:p">
        <xsl:text>&#10;\begin{quote}&#10;</xsl:text>
            <xsl:if test="@n">
              <xsl:text>(</xsl:text>
              <xsl:value-of select="@n"/>
              <xsl:text>) </xsl:text>
            </xsl:if>
            <xsl:apply-templates select="tei:q|tei:quote"/>
	    <xsl:text>\par&#10;</xsl:text>
            <xsl:apply-templates select="tei:bibl"/>
        <xsl:text>&#10;\end{quote}&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$preQuote"/>
	<xsl:if test="@n">
	  <xsl:text>(</xsl:text>
	  <xsl:value-of select="@n"/>
	  <xsl:text>) </xsl:text>
	</xsl:if>
	<xsl:apply-templates/>
	<xsl:value-of select="$postQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element code</desc>
   </doc>
  <xsl:template match="tei:code">
    <xsl:text>\texttt{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process &lt;corr&gt;</desc></doc>
  <xsl:template match="tei:corr">
      <xsl:apply-templates/>
      <xsl:choose>
         <xsl:when test="@sic">
            <xsl:text>\footnote{</xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">appearsintheoriginalas</xsl:with-param>
                </xsl:call-template>
                <xsl:text> \emph{</xsl:text>
                <xsl:value-of select="@sic"/>
            <xsl:text>}.}</xsl:text>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process &lt;supplied&gt;</desc></doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
            <desc>Process &lt;sic&gt;</desc></doc>

  <xsl:template match="tei:sic">
      <xsl:apply-templates/>
      <xsl:text> (sic)</xsl:text>
      <xsl:choose>
         <xsl:when test="@corr">
            <xsl:text>\footnote{</xsl:text>
                <xsl:call-template name="i18n">
                <xsl:with-param name="word">shouldbereadas</xsl:with-param>
                </xsl:call-template>
                <xsl:text> \emph{</xsl:text>
                <xsl:value-of select="./@corr"/>
            <xsl:text>}.}</xsl:text>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element eg|tei:q[@rend='eg']</desc>
   </doc>
  <xsl:template match="tei:seg[@rend='pre']|tei:eg|tei:q[@rend='eg']">
      <xsl:choose>
         <xsl:when test="ancestor::tei:cell and count(*)=1 and string-length(.)&lt;60">
	           <xsl:variable name="stuff">
	              <xsl:apply-templates mode="eg"/>
	           </xsl:variable>
	           <xsl:text>\fbox{\ttfamily </xsl:text>
	           <xsl:value-of select="tei:escapeCharsVerbatim($stuff)"/>
	           <xsl:text>} </xsl:text>
         </xsl:when>
         <xsl:when test="ancestor::tei:cell and not(*)  and string-length(.)&lt;60">
	           <xsl:variable name="stuff">
	              <xsl:apply-templates mode="eg"/>
	           </xsl:variable>
	           <xsl:text>\fbox{\ttfamily </xsl:text>
	           <xsl:value-of select="tei:escapeCharsVerbatim($stuff)"/>
	           <xsl:text>} </xsl:text>
         </xsl:when>
         <xsl:when test="ancestor::tei:cell or @rend='pre'">
	           <xsl:text>\mbox{}\hfill\\[-10pt]\begin{Verbatim}[fontsize=\small]
</xsl:text>
	           <xsl:apply-templates mode="eg"/>
	           <xsl:text>
\end{Verbatim}
</xsl:text>
         </xsl:when>
         <xsl:when test="ancestor::tei:list[@type='gloss']">
	           <xsl:text>\hspace{1em}\hfill\linebreak</xsl:text>
	           <xsl:text>\bgroup</xsl:text>
	           <xsl:call-template name="exampleFontSet"/>
	           <xsl:text>\vskip 10pt\begin{shaded}
\noindent\obeylines\obeyspaces </xsl:text>
	           <xsl:apply-templates mode="eg"/>
	           <xsl:text>\end{shaded}
\egroup 

</xsl:text>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:text>\par\bgroup</xsl:text>
	           <xsl:call-template name="exampleFontSet"/>
	           <xsl:text>\vskip 10pt\begin{shaded}
\obeylines\obeyspaces </xsl:text>
	           <xsl:apply-templates mode="eg"/>
	           <xsl:text>\end{shaded}
\par\egroup 

</xsl:text>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element emph</desc>
   </doc>
  <xsl:template match="tei:emph">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element foreign</desc>
   </doc>
  <xsl:template match="tei:foreign">
      <xsl:text>\textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element head</desc>
   </doc>
  <xsl:template match="tei:head">
      <xsl:choose>
         <xsl:when test="parent::tei:castList"/>
         <xsl:when test="parent::tei:figure"/>
         <xsl:when test="parent::tei:list"/>
         <xsl:when test="parent::tei:lg"> \subsection*{<xsl:apply-templates/>} </xsl:when>
         <xsl:when test="parent::tei:table"/>
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
	       <xsl:otherwise>I</xsl:otherwise>
            </xsl:choose>
            <xsl:choose>
               <xsl:when test="ancestor::tei:floatingText">Star</xsl:when>
               <xsl:when test="parent::tei:div/@rend='nonumber'">Star</xsl:when>
               <xsl:when test="ancestor::tei:back and not($numberBackHeadings='true')">Star</xsl:when>
	       <xsl:when test="not($numberHeadings='true') and ancestor::tei:body">Star</xsl:when>
               <xsl:when test="ancestor::tei:front and not($numberFrontHeadings='true')">Star</xsl:when>
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

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element head in heading mode</desc>
   </doc>
  <xsl:template match="tei:head" mode="makeheading">
      <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
            <desc>Process &lt;gloss&gt;</desc></doc>

  <xsl:template match="tei:gloss">
      <xsl:text> \textit{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element hi</desc>
   </doc>
  <xsl:template match="tei:hi">
      <xsl:call-template name="rendering"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Rendering rules, turning @rend into LaTeX commands</desc>
   </doc>
  <xsl:template name="rendering">
      <xsl:variable name="cmd">
         <xsl:choose>
            <xsl:when test="not(@rend)">\textit</xsl:when>
            <xsl:when test="starts-with(@rend,'color')">\textcolor</xsl:when>
            <xsl:when test="@rend='bold'">\textbf</xsl:when>
            <xsl:when test="@rend='calligraphic'">\textcal</xsl:when>
            <xsl:when test="@rend='capsall'">\uppercase</xsl:when>
            <xsl:when test="@rend='center'">\centerline</xsl:when>
            <xsl:when test="@rend='code'">\texttt</xsl:when>
            <xsl:when test="@rend='expanded'">\textsc</xsl:when>
            <xsl:when test="@rend='gothic'">\textgothic</xsl:when>
            <xsl:when test="@rend='i'">\textit</xsl:when>
            <xsl:when test="@rend='important'">\textbf</xsl:when>
            <xsl:when test="@rend='it'">\textit</xsl:when>
            <xsl:when test="@rend='ital'">\textit</xsl:when>
            <xsl:when test="@rend='italic'">\textit</xsl:when>
            <xsl:when test="@rend='italics'">\textit</xsl:when>
            <xsl:when test="@rend='large'">\textlarge</xsl:when>
            <xsl:when test="@rend='larger'">\textlarger</xsl:when>
            <xsl:when test="@rend='noindex'">\textrm</xsl:when>
            <xsl:when test="@rend='overbar'">\textoverbar</xsl:when>
            <xsl:when test="@rend='plain'">\textrm</xsl:when>
            <xsl:when test="@rend='quoted'">\textquoted</xsl:when>
            <xsl:when test="@rend='sc'">\textsc</xsl:when>
            <xsl:when test="@rend='small'">\textsmall</xsl:when>
            <xsl:when test="@rend='smallcaps'">\textsc</xsl:when>
            <xsl:when test="@rend='smaller'">\textsmaller</xsl:when>
            <xsl:when test="@rend='strikethrough'">\sout</xsl:when>
            <xsl:when test="@rend='sub'">\textsubscript</xsl:when>
            <xsl:when test="@rend='subscript'">\textsubscript</xsl:when>
            <xsl:when test="@rend='sup'">\textsuperscript</xsl:when>
            <xsl:when test="@rend='superscript'">\textsuperscript</xsl:when>
            <xsl:when test="@rend='typewriter'">\texttt</xsl:when>
            <xsl:when test="@rend='ul'">\uline</xsl:when>
            <xsl:when test="@rend='underwavyline'">\uwave</xsl:when>
            <xsl:when test="@rend='underdoubleline'">\uuline</xsl:when>
            <xsl:when test="@rend='underline'">\uline</xsl:when>
         </xsl:choose>
      </xsl:variable>
      <xsl:value-of select="$cmd"/>
      <xsl:choose>
	<xsl:when test="starts-with(@rend,'color(')">
	        <xsl:text>{</xsl:text>
	        <xsl:value-of select="substring-before(substring-after(@rend,'color('),')')"/>
	        <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:when test="starts-with(@rend,'color')">
	        <xsl:text>{</xsl:text>
	        <xsl:value-of select="substring-after(@rend,'color')"/>
	        <xsl:text>}</xsl:text>
	</xsl:when>
      </xsl:choose>
      <xsl:text>{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element hr</desc>
   </doc>
  <xsl:template match="tei:hr"> \hline </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element ident</desc>
   </doc>
  <xsl:template match="tei:ident">
      <xsl:text>\textsf{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element item</desc>
   </doc>
  <xsl:template match="tei:item"> 
      <xsl:text>
\item</xsl:text>
      <xsl:if test="@n">[<xsl:value-of select="@n"/>]</xsl:if>
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element item</desc>
   </doc>
  <xsl:template match="tei:item" mode="gloss"> 
      <xsl:text>
\item[</xsl:text>
      <xsl:apply-templates select="preceding-sibling::tei:label[1]" mode="gloss"/>
      <xsl:text>]</xsl:text>
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element label in normal mode</desc>
   </doc>
  <xsl:template match="tei:label"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element label in normal mode, inside an item</desc>
   </doc>
  <xsl:template match="tei:item/tei:label">
      <xsl:text>\textbf{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element label in gloss mode</desc>
   </doc>
  <xsl:template match="tei:label" mode="gloss">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element lb</desc>
   </doc>
  <xsl:template match="tei:lb">
    <xsl:choose>
      <xsl:when test="parent::tei:body"/>
      <xsl:when test="parent::tei:back"/>
      <xsl:when test="parent::tei:front"/>
      <xsl:when test="@type='hyphenInWord' and @rend='hidden'"/>
      <xsl:when test="@rend='hidden'">
        <xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="@rend='-' or @type='hyphenInWord'">
        <xsl:text>-</xsl:text>
	<xsl:text>{\hskip1pt}\newline </xsl:text>
      </xsl:when>
      <xsl:when test="not(tei:is-inline(..)) and (tei:is-last(.) or tei:is-first(.))"/>
      <xsl:otherwise>
      <xsl:text>{\hskip1pt}\newline </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element list</desc>
   </doc>
  <xsl:template match="tei:list">
      <xsl:if test="tei:head"> 
	<xsl:text>\leftline{\textbf{</xsl:text>
	<xsl:for-each select="tei:head">
            <xsl:apply-templates/>
         </xsl:for-each>
	 <xsl:text>}} </xsl:text>
      </xsl:if>
      <xsl:if test="@xml:id">
	        <xsl:text>\label{</xsl:text>
	        <xsl:value-of select="@xml:id"/>
	        <xsl:text>}</xsl:text>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="not(tei:item)"/>
         <xsl:when test="@type='gloss' or tei:label"> 
	   <xsl:text>\begin{description}&#10;</xsl:text>
	   <xsl:apply-templates mode="gloss" select="tei:item"/>
	   <xsl:text>&#10;\end{description} </xsl:text>
	 </xsl:when>
         <xsl:when test="@type='unordered'">
	   <xsl:text>\begin{itemize}&#10;</xsl:text>
	   <xsl:apply-templates/>
	   <xsl:text>&#10;\end{itemize} </xsl:text>
	 </xsl:when>
         <xsl:when test="@type='ordered'">
	   <xsl:text>\begin{enumerate}&#10;</xsl:text>
	   <xsl:apply-templates/>
	   <xsl:text>&#10;\end{enumerate}</xsl:text>
	 </xsl:when>
         <xsl:when test="@type='runin' or @rend='runon'">
            <xsl:apply-templates mode="runin" select="tei:item"/>
         </xsl:when>
         <xsl:otherwise> 
	   <xsl:text>\begin{itemize}&#10;</xsl:text>
	   <xsl:apply-templates/> 
	   <xsl:text>&#10;\end{itemize} </xsl:text>
      </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element listBibl</desc>
   </doc>
   <xsl:template match="tei:listBibl">
     <xsl:choose>
       <xsl:when test="tei:biblStruct">
	 <xsl:text>\begin{bibitemlist}{1}&#10;</xsl:text>
	 <xsl:for-each select="tei:biblStruct">
	   <xsl:sort select="lower-case(string(tei:*[1]/tei:author/tei:surname or  tei:*[1]/tei:author/tei:orgName or  tei:*[1]/tei:author/tei:name or  tei:*[1]/tei:editor/tei:surname or  tei:*[1]/tei:editor/tei:name or  tei:*[1]/tei:title))"/>
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
      <xsl:when test="tei:msDesc">
	<xsl:apply-templates/>
      </xsl:when>
       <xsl:otherwise>
	 <xsl:text>\begin{bibitemlist}{1}&#10;</xsl:text>
	 <xsl:apply-templates/> 
	 <xsl:text>&#10;\end{bibitemlist}&#10;</xsl:text>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element listBibl/tei:bibl</desc>
   </doc>
  <xsl:template match="tei:listBibl/tei:bibl"> \bibitem {<xsl:choose>
         <xsl:when test="@xml:id">
	           <xsl:value-of select="@xml:id"/>
         </xsl:when>
         <xsl:otherwise>bibitem-<xsl:number level="any"/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>}</xsl:text>
      <xsl:choose>
         <xsl:when test="parent::tei:listBibl/@xml:lang='zh-TW' or @xml:lang='zh-TW'">
	           <xsl:text>{\textChinese </xsl:text>
	           <xsl:apply-templates/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:listBibl/@xml:lang='ja' or @xml:lang='ja'">
	           <xsl:text>{\textJapanese </xsl:text>
	           <xsl:apply-templates/>
	           <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element mentioned</desc>
   </doc>
  <xsl:template match="tei:mentioned">
      <xsl:text>\emph{</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a note element which has a @place attribute
      pointing to margin</desc>
   </doc>
  <xsl:template name="marginalNote">
    <xsl:text>\marginnote{</xsl:text>
    <xsl:if test="@xml:id">
      <xsl:text>\label{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a note element which has a @place attribute for
      display style note</desc>
   </doc>
  <xsl:template name="displayNote">
    <xsl:text>&#10;\begin{quote}&#10;</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>\end{quote}&#10;</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a note element which has a @place attribute for endnote</desc>
   </doc>
  <xsl:template name="endNote">
    <xsl:text>\endnote{</xsl:text>
    <xsl:if test="@xml:id">
      <xsl:text>\label{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}</xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a plain note element</desc>
   </doc>
  <xsl:template name="plainNote">
    <xsl:text> {\small\itshape [</xsl:text>
    <xsl:choose>
      <xsl:when test="@n">
	<xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">Note</xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:text>]} </xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a note element which has a @place for footnote</desc>
   </doc>
  <xsl:template name="footNote">
    <xsl:if test="@xml:id">
      <xsl:text>\hypertarget{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}{}</xsl:text>
    </xsl:if>
    <xsl:choose>
	<xsl:when test="@target">
	  <xsl:text>\footnotetext{</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:when test="count(key('APP',1))&gt;0">
	  <xsl:text>\footnote{</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>\footnote{</xsl:text>
	  <xsl:if test="@xml:id">
	    <xsl:text>\label{</xsl:text>
	    <xsl:value-of select="@xml:id"/>
	    <xsl:text>}</xsl:text>
	  </xsl:if>
	  <xsl:apply-templates/>
	  <xsl:text>}</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element &lt;p&gt;</desc>
   </doc>
  <xsl:template match="tei:p">
    <xsl:choose>
      <xsl:when test="parent::tei:note and not(preceding-sibling::tei:p)">
      </xsl:when>
      <xsl:when test="count(key('APP',1))&gt;0">
	<xsl:text>\pstart&#10;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>\par&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="$numberParagraphs='true'">
      <xsl:call-template name="numberParagraph"/>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="count(key('APP',1))&gt;0">
	<xsl:text>&#10;\pend&#10;</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>How to number a paragraph</desc>
   </doc>
  <xsl:template name="numberParagraph">
      <xsl:text>\textit{\footnotesize[</xsl:text>
      <xsl:number/>
      <xsl:text>]} </xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element pb</p>
         <p>Indication of a page break. We make it an anchor if it has an
    ID.</p>
      </desc>
   </doc>
  
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elementq</desc>
   </doc>
  <xsl:template match="tei:q|tei:said">
      <xsl:choose>
	<xsl:when test="not(tei:is-inline(.))">
	  <xsl:text>&#10;\begin{quote}</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>\end{quote}&#10;</xsl:text>
	</xsl:when>
         <xsl:otherwise>
	   <xsl:call-template name="makeQuote"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element quote</desc>
   </doc>
  <xsl:template match="tei:quote">
      <xsl:choose>
	<xsl:when test="parent::tei:cit">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:when test="not(tei:is-inline(.))">
	  <xsl:text>\begin{quote}</xsl:text>
	  <xsl:apply-templates/>
	  <xsl:text>\end{quote}</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	   <xsl:call-template name="makeQuote"/>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element p with @rend='display'</desc>
   </doc>
  <xsl:template match="tei:p[@rend='display']"> 
      <xsl:text>&#10;\begin{quote}&#10;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>\end{quote}&#10;</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element q with @rend='display'</desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element ref[@type='cite']</desc>
   </doc>
  <xsl:template match="tei:ref[@type='cite']">
      <xsl:apply-templates/>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element signed</desc>
   </doc>
  <xsl:template match="tei:signed">
      <xsl:text>&#10;\begin{quote}&#10;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>\end{quote}&#10;</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element soCalled</desc>
   </doc>
  <xsl:template match="tei:soCalled">    
      <xsl:value-of select="$preQuote"/>
      <xsl:apply-templates/>
      <xsl:value-of select="$postQuote"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element unclear</desc>
   </doc>
  <xsl:template match="tei:unclear">
    <xsl:text>\textbf{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
    </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element space</desc>
   </doc>
  <xsl:template match="tei:space">
    <xsl:variable name="unit">
      <xsl:choose>
	<xsl:when test="@unit">
	  <xsl:value-of select="@unit"/>
	</xsl:when>
	<xsl:otherwise>em</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="quantity">
      <xsl:choose>
	<xsl:when test="@quantity">
	  <xsl:value-of select="@quantity"/>
	</xsl:when>
	<xsl:otherwise>1</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:text>\hspace{</xsl:text>
    <xsl:value-of select="$quantity"/>
    <xsl:value-of select="$unit"/>
    <xsl:text>}</xsl:text>
    </xsl:template>
    
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
    If verseNumbering is requested,
 counts all the verse lines since the last container (<gi xmlns="">div1</gi> by
 default) and labels every fifth verse using a LaTeX box 3 ems wide.

  </desc>
   </doc>
  <xsl:template match="tei:l">
      <xsl:choose>
         <xsl:when test="$verseNumbering='true'">
            <xsl:variable name="id" select="generate-id()"/>
            <xsl:variable name="pos">
               <xsl:for-each select="ancestor::*[name()=$resetVerseLineNumbering]//l">
                  <xsl:if test="generate-id()=$id">
                     <xsl:value-of select="position()"/>
                  </xsl:if>
               </xsl:for-each>
            </xsl:variable>
            <xsl:choose>
               <xsl:when test="$pos mod $everyHowManyLines = 0">
                  <xsl:text>\leftline{\makebox[3em][r]{</xsl:text>
                  <xsl:value-of select="$pos"/>
                  <xsl:text>}\quad{}</xsl:text>
                  <xsl:apply-templates/>
                  <xsl:text>}</xsl:text> 
               </xsl:when>
               <xsl:otherwise>
                  <xsl:text>&#10;\leftline{\makebox[3em][r]{}\quad{}</xsl:text>
                  <xsl:apply-templates/>
                  <xsl:text>}</xsl:text> 
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:when test="ancestor::tei:quote and following-sibling::tei:l">
            <xsl:apply-templates/>\\
	 </xsl:when>
	 <xsl:when test="parent::tei:sp">
	   <xsl:apply-templates/>
	   <xsl:text>\hfill\\</xsl:text>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:text>&#10;\leftline{</xsl:text>
	   <xsl:apply-templates/>
	   <xsl:text>}</xsl:text>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:del[@rend='overstrike']">
    <xsl:text>\sout{</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>}</xsl:text>
  </xsl:template>
</xsl:stylesheet>

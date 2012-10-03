<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"                
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="a fo rng tei html teix xs">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the linking module,
      making HTML output. </p>
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
      <desc>Process any element and work out a unique identififying string</desc>
   </doc>
  <xsl:template match="*" mode="ident">
      <xsl:variable name="BaseFile">
         <xsl:value-of select="$masterFile"/>
         <xsl:call-template name="addCorpusID"/>
      </xsl:variable>

      <xsl:choose>
         <xsl:when test="@xml:id and $useIDs='true'">
            <xsl:value-of select="@xml:id"/>
         </xsl:when>
         <xsl:when test="starts-with(local-name(.),'div') or
			 self::tei:text">
            <xsl:variable name="xpath">
               <xsl:for-each select="ancestor-or-self::tei:*">
	                 <xsl:value-of select="local-name()"/>
	                 <xsl:text>.</xsl:text>
	                 <xsl:number/>
	                 <xsl:if test="not(position() = last())">
	                    <xsl:text>_</xsl:text>
	                 </xsl:if>
	              </xsl:for-each>
	           </xsl:variable>
	           <xsl:value-of select="$BaseFile"/>
	           <xsl:text>-</xsl:text>
            <xsl:value-of select="substring-after(substring-after($xpath,'_text.'),'_')"/>
         </xsl:when>
         <xsl:when test="self::tei:TEI and parent::tei:teiCorpus">
	   <xsl:value-of select="$masterFile"/>
	   <xsl:call-template name="addCorpusID"/>
	 </xsl:when>
         <xsl:otherwise>
	   <xsl:value-of select="$BaseFile"/>
	   <xsl:text>-</xsl:text>
	   <xsl:value-of select="local-name(.)"/>
	   <xsl:text>-</xsl:text>
	   <xsl:value-of select="generate-id()"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process elements *</desc>
   </doc>
  <xsl:template match="*" mode="generateLink">
      <xsl:variable name="ident">
         <xsl:apply-templates mode="ident" select="."/>
      </xsl:variable>
      <xsl:variable name="depth">
         <xsl:apply-templates mode="depth" select="."/>
      </xsl:variable>
      <xsl:variable name="Hash">
         <xsl:text>#</xsl:text>
      </xsl:variable>
      <xsl:variable name="result">
      <xsl:choose>
	<xsl:when test="$filePerPage='true'">
	  <xsl:choose>
	    <xsl:when test="preceding::tei:pb">
	      <xsl:apply-templates select="preceding::tei:pb[1]"
				   mode="ident"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>index</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:value-of select="$standardSuffix"/>
	</xsl:when>
         <xsl:when test="$STDOUT='true' and number($depth) &lt;= number($splitLevel)">
            <xsl:value-of select="$masterFile"/>
            <xsl:value-of select="$standardSuffix"/>
            <xsl:value-of select="$urlChunkPrefix"/>
            <xsl:value-of select="$ident"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:elementSpec and not($STDOUT='true')">
	           <xsl:text>ref-</xsl:text>
	           <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
            <xsl:value-of select="$standardSuffix"/>
	           <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:classSpec and not($STDOUT='true')">
	           <xsl:text>ref-</xsl:text>
	           <xsl:value-of select="ancestor::tei:classSpec/@ident"/>
            <xsl:value-of select="$standardSuffix"/>
	           <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:back and not($splitBackmatter)">
            <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:front and not($splitFrontmatter)">
            <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
	 <xsl:when test="self::tei:text and $splitLevel=0">
	   <xsl:value-of select="$ident"/>
	   <xsl:value-of select="$standardSuffix"/>
	 </xsl:when>
         <xsl:when test="number($splitLevel)= -1 and
			 ancestor::tei:teiCorpus">
            <xsl:value-of select="$masterFile"/>
            <xsl:call-template name="addCorpusID"/>
            <xsl:value-of select="$standardSuffix"/>
            <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
         <xsl:when test="number($splitLevel)= -1">
            <xsl:value-of select="concat($Hash,$ident)"/>
         </xsl:when>
         <xsl:when test="number($depth) &lt;= number($splitLevel)">
            <xsl:value-of select="concat($ident,$standardSuffix)"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="parent">
               <xsl:call-template name="locateParentDiv"/>
            </xsl:variable>
            <xsl:choose>
               <xsl:when test="$STDOUT='true'">
                  <xsl:value-of select="$masterFile"/>
                  <xsl:value-of select="$urlChunkPrefix"/>
                  <xsl:value-of select="$parent"/>
                  <xsl:text>#</xsl:text>
                  <xsl:value-of select="$ident"/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="$parent"/>
                  <xsl:value-of select="concat($standardSuffix,'#')"/>
                  <xsl:value-of select="$ident"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
</xsl:variable>
<!--
<xsl:message><xsl:value-of select="$ident"/>: <xsl:value-of
select="$depth"/>: <xsl:value-of select="$splitLevel"/>: <xsl:value-of
select="$result"/></xsl:message>
-->
<xsl:value-of select="$result"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element TEI in generateLink mode</p>
         <p>
            <p xmlns="http://www.w3.org/1999/xhtml"> when a &lt;div&gt; is referenced, see whether its plain
        anchor, or needs a parent HTML name prepended </p>
         </p>
      </desc>
   </doc>
  <xsl:template match="tei:TEI" mode="generateLink">
      <xsl:variable name="BaseFile">
	<xsl:apply-templates select="." mode="ident"/>
      </xsl:variable>
      <xsl:value-of select="concat($BaseFile,$standardSuffix)"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element anchor</desc>
   </doc>
  <xsl:template match="tei:anchor">
      <xsl:call-template name="makeAnchor"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process extra elements in generateLink mode</desc>
   </doc>
  <xsl:template match="tei:label|tei:figure|tei:table|tei:item|tei:p|tei:bibl|tei:anchor|tei:cell|tei:lg|tei:list|tei:sp"
                 mode="generateLink">
      <xsl:variable name="ident">
         <xsl:apply-templates mode="ident" select="."/>
      </xsl:variable>
      <xsl:variable name="file">
         <xsl:apply-templates mode="generateLink"
                              select="ancestor::tei:*[starts-with(local-name(),'div')][1]"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="starts-with($file,'#')">
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:when>
         <xsl:when test="contains($file,'#')">
            <xsl:value-of select="substring-before($file,'#')"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$file"/>
            <xsl:text>#</xsl:text>
            <xsl:value-of select="$ident"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element note</desc>
   </doc>
  <xsl:template match="tei:note" mode="generateLink">
    <xsl:variable name="file">
      <xsl:apply-templates mode="generateLink"
			   select="ancestor::tei:*[starts-with(local-name(),'div')][1]"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="starts-with($file,'#')"/>
      <xsl:when test="contains($file,'#')">
            <xsl:value-of select="substring-before($file,'#')"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$file"/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>#</xsl:text>
      <xsl:call-template name="noteID"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] <param name="where">where</param>
      </desc>
   </doc>
  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:choose>
	<xsl:when test="id($where)">
	  <xsl:apply-templates mode="generateLink" select="id($where)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text>[[undefined </xsl:text>
	  <xsl:value-of select="$where"/>
	  <xsl:text>]]</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] Find the name of the outermost container for the
      current object which would create an output file</desc>
   </doc>
  <xsl:template name="locateParentDiv">

      <xsl:choose>

	<xsl:when
	     test="ancestor-or-self::tei:body/parent::tei:text/ancestor::tei:group">
            <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
         </xsl:when>

	 <xsl:when test="ancestor-or-self::tei:front/parent::tei:text/ancestor::tei:group">
            <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
         </xsl:when>

	 <xsl:when test="ancestor-or-self::tei:back/parent::tei:text/ancestor::tei:group">
            <xsl:apply-templates mode="ident" select="ancestor::tei:text[1]"/>
         </xsl:when>

         <xsl:when test="ancestor-or-self::tei:div and number($splitLevel) &lt; 0">
            <xsl:apply-templates mode="ident" select="ancestor::tei:div[last()]"/>
         </xsl:when>

         <xsl:when test="ancestor-or-self::tei:div">
	   <xsl:variable name="ancestors" select="count(ancestor-or-self::tei:div)"/>
	   <xsl:variable name="diff" select="$ancestors - number($splitLevel)"/>
	   <xsl:variable name="what" select="if ($diff &lt;= 1) then 1
					     else $diff "/>
	   <xsl:apply-templates mode="ident" select="ancestor-or-self::tei:div[$what]"/>
         </xsl:when>

         <xsl:otherwise>
	   <xsl:variable name="ancestors" select="count(ancestor::tei:*[local-name()='div1'
				 or local-name()='div2'
				 or local-name()='div3'
				 or local-name()='div4'
				 or local-name()='div5'
				 or local-name()='div6'])"/>
	   <xsl:variable name="what"
			 select="if
				 ($ancestors &lt; number($splitLevel)) then 1 else
				 $ancestors - number($splitLevel) +1"/>
            <xsl:apply-templates mode="ident"
				 select="ancestor-or-self::tei:*[local-name()='div1'
				 or local-name()='div2'
				 or local-name()='div3'
				 or local-name()='div4'
				 or local-name()='div5'
				 or local-name()='div6'][$what]"/>
         </xsl:otherwise>
      </xsl:choose>

  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] create external link<param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="class">class</param>
      </desc>
   </doc>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr" as="xs:boolean"  select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="class">link_<xsl:value-of select="local-name(.)"/>
      </xsl:param>
      <a>
        <xsl:choose>
            <xsl:when test="@rend">
	              <xsl:attribute name="class">
	                 <xsl:value-of select="@rend"/>
	              </xsl:attribute>
            </xsl:when>
	           <xsl:when test="@rendition">
	              <xsl:call-template name="applyRendition"/>
	           </xsl:when>
            <xsl:when test="parent::tei:item/parent::tei:list[@rend]">
	              <xsl:attribute name="class">
	                 <xsl:value-of select="parent::tei:item/parent::tei:list/@rend"/>
	              </xsl:attribute>
            </xsl:when>
	           <xsl:when test="parent::tei:item[@rend]">
	              <xsl:attribute name="class">
	                 <xsl:value-of select="parent::tei:item/@rend"/>
	              </xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:attribute name="class">
	                 <xsl:value-of select="$class"/>
	              </xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>

         <xsl:if test="@type and not($outputTarget='epub3' or $outputTarget='html5')">
            <xsl:attribute name="type">
               <xsl:value-of select="@type"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:attribute name="href">
            <xsl:value-of select="$dest"/>
            <xsl:if test="contains(@from,'id (')">
               <xsl:text>#</xsl:text>
               <xsl:value-of select="substring(@from,5,string-length(normalize-space(@from))-1)"/>
            </xsl:if>
         </xsl:attribute>
	 <xsl:choose>
	   <xsl:when test="@n">
	     <xsl:attribute name="title">
	       <xsl:value-of select="@n"/>
	     </xsl:attribute>
	   </xsl:when>
	 </xsl:choose>
         <xsl:call-template name="xrefHook"/>
         <xsl:choose>
	   <xsl:when test="$dest=''">??</xsl:when>
            <xsl:when test="$ptr">
               <xsl:element name="{$urlMarkup}">
                  <xsl:choose>
                     <xsl:when test="starts-with($dest,'mailto:')">
                        <xsl:value-of select="substring-after($dest,'mailto:')"/>
                     </xsl:when>
                     <xsl:when test="starts-with($dest,'file:')">
                        <xsl:value-of select="substring-after($dest,'file:')"/>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="$dest"/>
                     </xsl:otherwise>
                  </xsl:choose>
               </xsl:element>
            </xsl:when>
            <xsl:otherwise>
               <xsl:apply-templates/>
            </xsl:otherwise>
         </xsl:choose>
      </a>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] create an internal link<param name="target">target</param>
         <param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="body">body</param>
         <param name="class">class</param>
      </desc>
   </doc>
  <xsl:template name="makeInternalLink">
      <xsl:param name="target"/>
      <xsl:param name="ptr" as="xs:boolean" select="false()"/>
      <xsl:param name="dest"/>
      <xsl:param name="body"/>
      <xsl:param name="class">
         <xsl:text>link_</xsl:text>
         <xsl:value-of select="local-name(.)"/>
      </xsl:param>
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="$target">
               <xsl:value-of select="$target"/>
            </xsl:when>
            <xsl:when test="contains($dest,'#')">
               <xsl:value-of select="substring-after($dest,'#')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$dest"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>

      <xsl:choose>
         <xsl:when test="$dest=''">
            <xsl:choose>
               <xsl:when test="not($body='')">
                  <xsl:value-of select="$body"/>
               </xsl:when>
               <xsl:when test="$ptr">
                  <xsl:apply-templates mode="xref" select="id($W)">
                     <xsl:with-param name="minimal" select="$minimalCrossRef"/>
                  </xsl:apply-templates>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:apply-templates/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:variable name="eventualtarget">
	     <xsl:choose>
	       <xsl:when test="starts-with($dest,'#') or  contains($dest,$outputSuffix) or contains($dest,'ID=')">
		 <xsl:value-of select="$dest"/>
	       </xsl:when>
	       <xsl:when test="id($W)"/>
	       <xsl:otherwise>
		 <xsl:apply-templates mode="generateLink" select="id($W)"/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:variable name="linktext">
	     <xsl:choose>
	       <xsl:when test="not($body='')">
		 <xsl:value-of select="$body"/>
	       </xsl:when>
               <xsl:when test="$ptr and @type='footnote'">
		 <xsl:text>[</xsl:text>
		 <xsl:number level="any"/>
		 <xsl:text>]</xsl:text>
	       </xsl:when>
	       <xsl:when test="$ptr and id($W)">
		 <xsl:apply-templates mode="xref" select="id($W)">
		   <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		 </xsl:apply-templates>
	       </xsl:when>
	       <xsl:when test="$ptr">
		 <xsl:value-of select="$dest"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:variable>
	   <xsl:choose>
	     <xsl:when test="$eventualtarget=''">
	       <xsl:copy-of select="$linktext"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <a href="{$eventualtarget}">
		 <xsl:call-template name="htmlAttributes"/>
		 <xsl:choose>
		   <xsl:when test="@rend">
		     <xsl:attribute name="class">
		       <xsl:value-of select="@rend"/>
		     </xsl:attribute>
		   </xsl:when>
		   <xsl:when test="@rendition">
		     <xsl:call-template name="applyRendition"/>
		   </xsl:when>
		   <xsl:when test="string-length($class)=0"/>
		   <xsl:otherwise>
		     <xsl:attribute name="class">
		       <xsl:value-of select="$class"/>
		     </xsl:attribute>
		   </xsl:otherwise>
		 </xsl:choose>
		 
		 <xsl:for-each select="id($W)">
		     <xsl:choose>
		       <xsl:when test="starts-with(local-name(.),'div')">
			 <xsl:attribute name="title">
			   <xsl:value-of
			       select="translate(normalize-space(tei:head[1]),'&gt;&lt;','')"/>
			 </xsl:attribute>
		       </xsl:when>
		     </xsl:choose>
		 </xsl:for-each>
	       <xsl:copy-of select="$linktext"/>
	       </a>
	     </xsl:otherwise>
	   </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process cross-ref to note</desc>
   </doc>
  <xsl:template match="tei:note" mode="xref">
      <xsl:number level="any"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>formatting of the number part of a header</desc>
   </doc>
  <xsl:template name="formatHeadingNumber">
      <xsl:param name="text"/>
      <xsl:param name="toc"/>
      <xsl:if test="not($text='')">
	<span class="headingNumber">
	  <xsl:copy-of select="$text"/>
	</span>
      </xsl:if>
  </xsl:template>

</xsl:stylesheet>

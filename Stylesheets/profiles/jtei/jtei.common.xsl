<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:eg="http://www.tei-c.org/ns/Examples"
  xmlns:i18n="i18n"
  xmlns:local="local"
  xmlns="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="#all"
  version="2.0">
  
  <!-- This stylesheet module contains shared functions and templates for all jTEI conversions -->
  
  <xsl:import href="i18n.xsl"/>
  
  <!-- This parameter controls if footnotes are numbered continously throughout the document --> 
  <xsl:param name="footnote.number.continuous" select="true()"/>
  
  <!-- This parameter controls if the footnote numbering format should be differentiated for foonotes occurring in tei:front (i), tei:body (1), or tei:back (a) -->
  <xsl:param name="footnote.numberformat.differentiate" select="false()"/>
  
  <!-- This variable specifies the different footnote formats -->
  <xsl:variable name="footnote.formats">
    <local:context name="front" format="i"/>
    <local:context name="body" format="1"/>
    <local:context name="back" format="a"/>
  </xsl:variable>
  
  <!-- This key builds an index of elements with @xml:id attribute. -->
  <xsl:key name="ids" match="*" use="@xml:id"/>    

  <!-- This key builds an index of elements for which quotation marks have 
    to be generated. -->
  <xsl:key name="quotation.elements" match="tei:quote|tei:q|tei:soCalled|tei:title[@level = ('a', 'u')]" use="local-name()"/>

  <!-- This variable defines the current document. -->
  <xsl:variable name="docRoot" select="/"/>
  
  <!-- This variable defines typographic quotation marks. -->
  <xsl:variable name="lsquo">‘</xsl:variable>
  <xsl:variable name="rsquo">’</xsl:variable>
  <xsl:variable name="ldquo">“</xsl:variable>
  <xsl:variable name="rdquo">”</xsl:variable>
  
  <!-- This variable defines start delimiters for the different XML node types. -->
  <xsl:variable name="delimiter.start">
    <local:delim n="gi">&lt;</local:delim>
    <local:delim n="val">"</local:delim>
    <local:delim n="att">@</local:delim>
    <local:delim n="tag.end">&lt;/</local:delim>
    <local:delim n="tag.pi">&lt;?</local:delim>
    <local:delim n="tag.comment">&lt;!--</local:delim>
    <local:delim n="tag.ms">&lt;![CDATA[</local:delim>
    <local:delim n="tag">&lt;</local:delim>
  </xsl:variable>
  
  <!-- This variable defines end delimiters for the different XML node types. -->
  <xsl:variable name="delimiter.end">
    <local:delim n="gi">&gt;</local:delim>
    <local:delim n="val">"</local:delim>
    <local:delim n="tag.pi">?&gt;</local:delim>
    <local:delim n="tag.comment">--&gt;</local:delim>
    <local:delim n="tag.ms">]]&gt;</local:delim>
    <local:delim n="tag.empty">/&gt;</local:delim>
    <local:delim n="tag">&gt;</local:delim>
  </xsl:variable>
  
  <!-- This variable lists all possible @type values for divisions in the 
       front section (in processing order). -->
  <xsl:variable name="div.types.front" select="('abstract', 'corrections', 'dedication', 'editorNotes', 'authorNotes', 'acknowledgements')"/>
  
  <!-- This template generates labels for headers. -->
  <xsl:template match="*" mode="label">
    <xsl:param name="crossref.ptr" select="()" as="node()*"/>
    <xsl:variable name="label" select="local:get.label.name(., $crossref.ptr)"/>
    <xsl:variable name="number" select="local:get.label.number(.)"/>
    <xsl:variable name="postfix" select="local:get.label.postfix(., $crossref.ptr, $number)"/>
    <xsl:value-of select="string-join(($label[normalize-space()], concat($number, $postfix)), ' ')"/>
  </xsl:template>
  
  <xsl:template name="punctuate-head">
    <xsl:if test="not(matches(normalize-space((descendant::text()[not(ancestor::tei:note)][last()])), '[\p{P}-[\p{Pe}]]$'))">
      <xsl:text>.</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <!-- This template pulls subsequent punctuation into generated quotation, or before a 
       footnote marker. -->
  <xsl:template name="include.punctuation">
    <xsl:choose>
      <!-- quotation elements: only place following comma and period before the closing quotation mark -->
      <!-- condition: the element should not end in a nesting "pulling punctuation quotes" context, 
           since subsequent punctuation should be pulled inside the innermost quotation marks.
      -->
      <xsl:when test="
        not(self::tei:note) 
        and
        not(
          some $text in descendant::text()[normalize-space()][last()]
          (: descendant::*: don't include context node itself :)
          satisfies descendant::*
            [descendant::text() intersect $text]
            [. intersect key('quotation.elements', local-name())]
        )
      ">
        <xsl:value-of select="following::node()[not(ancestor-or-self::tei:note[not(current() intersect descendant::*)])][1]/self::text()[matches(., '^\s*[.,]')]/replace(., '^\s*([.,]+).*', '$1', 's')"/>
      </xsl:when>
      <!-- footnotes: place all following punctuation marks, except dash, before the footnote marker --> 
      <!-- condition: the first preceding non-note sibling should not end in a "pulling punctuation 
           quotes" context, unless the following text node starts with a question or quotation mark.
      -->
      <xsl:when test="
        self::tei:note 
        and (
          not(
            preceding-sibling::node()[not(self::tei:note)][1]
            [local:endsWithPullingPunctuationQuotes(.)]
          )
          or 
          following::node()[not(ancestor-or-self::tei:note
            [not(current() intersect descendant::*)])][1]/self::text()
            [matches(., '^\s*[\p{P}-[.,\p{Ps}\p{Pe}—]]')]
        )
      ">
        <xsl:value-of select="following::node()[not(ancestor-or-self::tei:note[not(current() intersect descendant::*)])][1]/self::text()[matches(., '^\s*[\p{P}-[\p{Ps}\p{Pe}—]]')]/replace(., '^\s*([\p{P}-[\p{Ps}\p{Pe}—]]+).*', '$1', 's')"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  
  <!-- This template creates correct enumerations. -->
  <xsl:template name="enumerate">
    <xsl:if test="position() > 1 and last() > 2">
      <xsl:text>,</xsl:text>
    </xsl:if>
    <xsl:if test="position() > 1">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:if test="position() > 1 and position() = last()">
      <xsl:value-of select="concat(i18n:key('and'), ' ')"/>
    </xsl:if>
  </xsl:template>
  
  <!-- This template generate labels for cross-references. -->
  <xsl:template name="get.crossref.labels">
    <xsl:variable name="current" select="."/>
    <xsl:for-each select="tokenize(@target, '\s+')">
      <xsl:variable name="target" select="key('ids', substring-after(., '#'), $docRoot)"/>
      <label type="{$target/name()}" n="{if ($target/self::tei:note) then concat('note', local:get.note.nr($target)) else substring-after(current(), '#')}">
        <xsl:apply-templates select="$target" mode="label">
          <xsl:with-param name="crossref.ptr" select="$current"/>
        </xsl:apply-templates>
      </label>
    </xsl:for-each>
  </xsl:template>
  
  <!-- This template formats labels for cross-references. -->
  <xsl:template name="format.crossref.labels">
    <xsl:choose>
      <!-- pluralize if there are multiple targets of the same type -->
      <xsl:when test="not(@type = preceding-sibling::*[1]/@type) and @type = following-sibling::*[1]/@type">
        <!-- if no specific plural can be found, just add an -s -->
        <xsl:value-of select="(
          for $i in 
          i18n:plural(lower-case(normalize-space(replace(., '\d', ''))))[@pl]
          return replace(., substring($i, 2), substring($i/@pl, 2))
          , replace(., '^(\w+)', '$1s'))[1]"/>
      </xsl:when>
      <xsl:when test="@type = preceding-sibling::*[1]/@type">
        <xsl:value-of select="normalize-space(replace(., '^(\w+)', ''))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- This template determines correct list marker for inline lists. -->
  <xsl:template name="get.inline.list.marker">
    <xsl:variable name="rend" select="parent::tei:list/@rend"/>
    <xsl:variable name="type" select="parent::tei:list/@type"/>
    <xsl:choose>
      <xsl:when test="$type eq 'gloss'">
        <xsl:value-of select="preceding-sibling::tei:label[1]"/>
      </xsl:when>
      <xsl:when test="tokenize($rend, '\s+') = 'ordered'">
        <xsl:text>(</xsl:text>
        <xsl:variable name="number"><xsl:number level="multiple" format="1.a"/></xsl:variable>
        <xsl:value-of select="replace($number, '\.', '')"/>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="tokenize($rend, '\s+') = 'bulleted'"><xsl:text>*</xsl:text></xsl:when>
      <xsl:otherwise/>
    </xsl:choose>
  </xsl:template>
  
  <!-- This template creates start and end tags for elements inside <egXML>. --> 
  <xsl:template name="createTag">    
    <xsl:param name="type"/>
    <xsl:variable name="tag"><tag type="{$type}"/></xsl:variable>
    <xsl:variable name="parent-nss" select="../namespace::*"/>
    <xsl:variable name="isTop">
      <xsl:if
        test="parent::eg:egXML and (self::tei:TEI or parent::*/parent::tei:div[@type='example'])">
        <xsl:value-of select="'true'"/>
      </xsl:if>
    </xsl:variable>
    <!-- first, create "abstract" form of output, that will be serialized 
      in specific output formats -->
    <xsl:variable name="abstract.tag">
      <seg type="abstract.egXML.tag">
        <xsl:value-of select="local:get.delimiter('start', $tag/*)"/>
        <xsl:value-of select="name()"/>
      </seg>
      <xsl:if test="$type != 'end'">
        <xsl:for-each
          select="namespace::*">
          <xsl:variable name="ns-prefix" select="replace(name(), '_\d+$', '')"/>
          <xsl:variable name="ns-uri" select="string(.)"/>
          <xsl:if
            test="not($parent-nss[replace(name(), '_\d+$', '') = $ns-prefix and string(.) = $ns-uri]) or $isTop = 'true'">
            <!-- This namespace node doesn't exist on the parent, at least not with that URI, so we need to add a declaration. -->
            <seg type="abstract.egXML.attribute.name">
              <xsl:text> xmlns</xsl:text>
              <xsl:if test="$ns-prefix != ''">
                <xsl:text>:</xsl:text>
                <xsl:value-of select="$ns-prefix"/>
              </xsl:if>
            </seg>
            <seg type="abstract.egXML.attribute.value">
              <xsl:text>="</xsl:text>
              <xsl:value-of select="$ns-uri"/>
              <xsl:text>"</xsl:text>
            </seg>
          </xsl:if>
        </xsl:for-each>
        <xsl:for-each select="@*">
          <xsl:text> </xsl:text>
          <seg type="abstract.egXML.attribute.name">
            <xsl:value-of select="name()"/>
          </seg>
          <seg type="abstract.egXML.attribute.value">
            <xsl:text>="</xsl:text>
            <xsl:value-of select="local:escapeEntitiesForEgXMLAttribute(.)"/>
            <xsl:text>"</xsl:text>
          </seg>
        </xsl:for-each>
      </xsl:if>
      <seg type="abstract.egXML.tag">
        <xsl:value-of select="local:get.delimiter('end', $tag/*)"/>
      </seg>
    </xsl:variable>
    <!-- next, serialize $abstract.tag for specific output formats -->
    <xsl:apply-templates select="$abstract.tag" mode="serialize"/>
  </xsl:template>

  <!-- This template creates blockquotes. --> 
  <xsl:template name="blockquote">
    <xsl:param name="node" select="."/>
    <!-- first, create "abstract" form of output, that will be serialized 
      in specific output formats -->
    <xsl:variable name="abstract.blockquote">
      <seg type="abstract.blockquote">
        <xsl:if test="not($node/ancestor::tei:note)">
          <xsl:attribute name="rend">quotation</xsl:attribute>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="$node/self::tei:quote">
            <xsl:apply-templates select="$node/node()"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="$node"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text> </xsl:text>
        <xsl:apply-templates select="$node/ancestor::tei:cit[1]/*[not(self::tei:quote)]"/>
      </seg>
    </xsl:variable>
    <!-- next, serialize $abstract.blockquote for specific output formats -->
    <xsl:apply-templates select="$abstract.blockquote" mode="serialize"/>
  </xsl:template>

  <!-- This template creates paragraphs, and normalizes whitespace for 
  text preceding or following paragraph-splitting content (tables, block
  quotes,...). -->
  <xsl:template name="p.create">
    <xsl:param name="current"/>
    <xsl:param name="context"/>
    <xsl:if test="some $i in $context satisfies ($i/normalize-space() or $i/@*)">
      <p>
        <xsl:apply-templates select="$current/@*"/>
        <xsl:choose>
          <xsl:when test="position() > 1">
            <xsl:attribute name="rend">noindent</xsl:attribute>
          </xsl:when>
          <xsl:when test="$current/parent::tei:div and $current[preceding-sibling::tei:p]">
          </xsl:when>
        </xsl:choose>
        <xsl:for-each select="$context">
          <xsl:variable name="processed">
            <xsl:apply-templates select="." mode="#current"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="self::text()">
              <xsl:choose>
                <xsl:when test="position() = 1">
                  <xsl:value-of select="replace(replace($processed, '^\s+', ''), '\s+$', ' ')"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="replace($processed, '\s+$', ' ')"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$processed"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </p>
    </xsl:if>
  </xsl:template>
  
  <!-- This template copies author(ing instance)/s if they're different from 
    those of the preceding bibliographical entry. If the're equal to those of
    the preceding bibliographical entry, the're abbreviated to "———". -->
  <xsl:template name="get.author.instance">
    <xsl:param name="dateOrTitle"/>
    <xsl:variable name="authorInstance.current" select="node()[. &lt;&lt; $dateOrTitle]"/>
    <xsl:variable name="bibl.prev" select="preceding-sibling::*[1]/self::tei:bibl"/>
    <xsl:variable name="authorInstance.prev" select="preceding-sibling::*[1]/self::tei:bibl/node()[. &lt;&lt; $bibl.prev/(tei:date|tei:title)[1]]"/>
    <xsl:choose>
      <xsl:when test="not($authorInstance.current)"/>
      <xsl:when test="$bibl.prev and deep-equal(local:strip-space($authorInstance.current), local:strip-space($authorInstance.prev))">
        <xsl:text>———. </xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="$authorInstance.current"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <!-- text() starting with punctuation, and following either an element for which smart quotes 
       or a footnote marker are being generated: skip starting punctuation (this is pulled into 
       the quotation marks or before the footnote) -->
  <xsl:template match="text()[matches(., '^\s*[\p{P}-[\p{Ps}\p{Pe}—]]')]" mode="#all">
    <xsl:choose>
      <!-- text following a valid "quotation element": skip starting comma or period -->
      <xsl:when test="
        self::text()[matches(., '^\s*[.,]')]
        [preceding-sibling::node()[not(self::tei:note)][1]
          [local:endsWithPullingPunctuationQuotes(.)]
        ]
      ">
        <xsl:value-of select="replace(., '^(\s*)[.,]+', '$1', 's')"/>
      </xsl:when>
      <!-- text following a valid footnote marker: skip all punctuation except dash -->
      <xsl:when test="self::text()[preceding::node()[1][ancestor::tei:note[not(current() intersect descendant::node())]]]">
        <xsl:value-of select="replace(., '^(\s*)[\p{P}-[\p{Ps}\p{Pe}—]]+', '$1', 's')"/>
      </xsl:when>
      <!-- other text: just copy -->
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- This function checks if a node ends with quotation marks that pull in subsequent punctuation. -->
  <!-- For the context node and all its descendants containing the last non-empty text node, it is
       tested if any of these is a "quotation element". -->
  <xsl:function name="local:endsWithPullingPunctuationQuotes" as="xs:boolean">
    <xsl:param name="node"/>
    <xsl:value-of select="
      some $text in $node/descendant::text()[normalize-space()][last()]
      (: descendant-or-self::*: include preceding sibling node itself, too :)
      satisfies $node/descendant-or-self::*
        [descendant::text() intersect $text]
        [. intersect key('quotation.elements', local-name())]
    "/>
  </xsl:function>
  
  <!-- This function creates a space-stripped copy of an author(ing instance) in
    a bibliography that can be compared to other author(ing instance)s, when
    determining if an abbreviated form should be used. -->
  <xsl:function name="local:strip-space">
    <xsl:param name="node" as="node()*"/>
    <xsl:for-each select="$node">
      <xsl:choose>
        <xsl:when test="self::text()"><xsl:value-of select="replace(., '\s', '')"/></xsl:when>
        <xsl:otherwise>
          <xsl:copy>
            <xsl:copy-of select="local:strip-space(@*|node())"/>
          </xsl:copy>
        </xsl:otherwise>
      </xsl:choose>      
    </xsl:for-each>
  </xsl:function>
  
  <!-- This function determines the name when labeling headings for 
    text structures. -->
  <xsl:function name="local:get.label.name">
    <xsl:param name="node"/>
    <xsl:param name="crossref.ptr"/>
    <xsl:variable name="rawLabel">
      <xsl:choose>
        <xsl:when test="$node/self::tei:div[@type eq 'appendix']">
          <xsl:value-of select="i18n:key('appendix-label')"/>
        </xsl:when>
        <xsl:when test="$node/self::tei:div"><xsl:if test="$crossref.ptr">
          <xsl:value-of select="i18n:key('section-label')"/></xsl:if>
        </xsl:when>
        <xsl:when test="$node/self::tei:figure[tei:graphic]">
          <xsl:value-of select="i18n:key('figure-label')"/>
        </xsl:when>
        <xsl:when test="$node/self::tei:figure[tei:eg|eg:egXML]">
          <xsl:value-of select="i18n:key('example-label')"/>
        </xsl:when>
        <xsl:otherwise><xsl:value-of select="i18n:key(concat($node/local-name(), '-label'))"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="contextLabel">
      <xsl:variable name="immediatePrecedingText" select="($crossref.ptr/preceding-sibling::node()/descendant-or-self::text()[not(ancestor::tei:note[following::* intersect $crossref.ptr])][normalize-space()])[last()]"/>
      <xsl:variable name="capitalize" select="if ($jtei.lang = ('de') or not($crossref.ptr) or not($immediatePrecedingText) or $immediatePrecedingText[matches(., '[\.!?]\s*$')]) then true() else false()"/>
      <xsl:choose>
        <xsl:when test="$capitalize">
          <xsl:value-of select="concat(upper-case(substring($rawLabel, 1, 1)), substring($rawLabel, 2))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$rawLabel"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="$contextLabel"/>
  </xsl:function>
  
  <!-- This function determines the positional number when labeling 
    headings for text structures. -->
  <xsl:function name="local:get.label.number">
    <xsl:param name="node"/>
    <xsl:choose>
      <xsl:when test="$node/self::tei:div[@type eq 'appendix']"><xsl:for-each select="$docRoot//tei:div[@type eq 'appendix'][deep-equal((@*|node()), $node/(@*|node()))]"><xsl:number count="tei:div[@type eq 'appendix'][tei:head]" level="multiple"/></xsl:for-each></xsl:when>
      <xsl:when test="$node/self::tei:div"><xsl:for-each select="$docRoot//tei:div[deep-equal((@*|node()), $node/(@*|node()))]"><xsl:number count="tei:div[tei:head]" level="multiple"/></xsl:for-each></xsl:when>
      <xsl:when test="$node/self::tei:figure[tei:graphic]"><xsl:for-each select="$docRoot//tei:figure[tei:graphic][deep-equal((@*|node()), $node/(@*|node()))]"><xsl:number count="tei:figure[tei:head[not(@type='license')]][tei:graphic]" level="any"/></xsl:for-each></xsl:when>
      <xsl:when test="$node/self::tei:figure[tei:eg|eg:egXML]"><xsl:for-each select="$docRoot//tei:figure[tei:eg|eg:egXML][deep-equal((@*|node()), $node/(@*|node()))]"><xsl:number count="tei:figure[tei:head[not(@type='license')]][tei:eg|eg:egXML]" level="any"/></xsl:for-each></xsl:when>
      <xsl:when test="$node/self::tei:note"><xsl:for-each select="$docRoot//tei:note[deep-equal((@*|node()), $node/(@*|node()))]"><xsl:value-of select="local:get.note.nr(.)"/></xsl:for-each></xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$docRoot//*[deep-equal((@*|node()), $node/(@*|node()))]"><xsl:number count="*[name() eq $node/name()][tei:head]" level="any"/></xsl:for-each></xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <!-- This function determines the postfix when labeling headings for
    text structures. -->
  <xsl:function name="local:get.label.postfix">
    <xsl:param name="node"/>
    <xsl:param name="crossref"/>
    <xsl:param name="number"/>
    <xsl:choose>
      <xsl:when test="$node/self::tei:div and not($crossref)">
        <xsl:value-of select="concat(if (not(contains($number, '.'))) then '.' else (), ' ')"/>
      </xsl:when>
      <xsl:when test="not($crossref)">
        <xsl:text>. </xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:function>
  
  <!-- This function computes the number for footnotes. -->
  <xsl:function name="local:get.note.nr" as="xs:integer">
    <xsl:param name="node"/>
    <xsl:choose>
      <!-- Count footnotes continuously throughout the document is this is set in the stylesheet parameter -->
      <xsl:when test="$footnote.number.continuous">
        <xsl:number select="$node" level="any"/>
      </xsl:when>
      <!-- Otherwise, restart footnote numbering for each front, body, or back section -->
      <xsl:otherwise>
        <xsl:number select="$node" level="any" from="tei:front|tei:body|tei:back"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <!-- This function determines the number format for footnotes. -->
  <xsl:function name="local:format.note.nr" as="xs:string">
    <xsl:param name="note.context" as="element()"/>
    <xsl:choose>
      <!-- Format footnotes numbers differently when occurring inside the front (i), body (1), or back (a) section, if this is set in the stylesheet parameter -->
      <xsl:when test="$footnote.numberformat.differentiate">
        <xsl:value-of select="$footnote.formats//local:context[@name = local-name($note.context)]/@format"/>
      </xsl:when>
      <!-- Otherwise, just format all footnote numbers with Arabic numerals -->
      <xsl:otherwise>
        <xsl:value-of select="'1'"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <!-- This function is designed to double-escape entities that need to be 
     displayed as escapes in egXML text nodes. -->
  <xsl:function name="local:escapeEntitiesForEgXML" as="xs:string">
    <xsl:param name="inStr" as="xs:string"/>
    <xsl:value-of select="local:unescapeAmpersandsForEgXMLEscapes(replace(replace(replace($inStr, '&amp;', '&amp;amp;'), '&lt;', '&amp;lt;'), '&gt;', '&amp;gt;'))"/>
  </xsl:function>
  
  <!-- This function is designed to unescape ampersands that should be displayed 
    literally as part of escapes in egXML text nodes. -->
  <xsl:function name="local:unescapeAmpersandsForEgXMLEscapes" as="xs:string">
    <xsl:param name="inStr" as="xs:string"/>
    <xsl:value-of select="replace($inStr, '&amp;amp;([^;\s&amp;]+?;)', '&amp;$1')"/>
  </xsl:function>

  <!-- This function is designed to double-escape entities that need to be 
     displayed as escapes in egXML attribute values. -->
  <xsl:function name="local:escapeEntitiesForEgXMLAttribute" as="xs:string">
    <xsl:param name="inStr" as="xs:string"/>
    <xsl:value-of select="replace(local:escapeEntitiesForEgXML($inStr),'&quot;', '&amp;quot;')"/>
  </xsl:function>
  
  <!-- This function looks up the correct delimiter for an XML node 
    (either for constructing the contents of <egXML> elements, or for 
    processing <gi> / <tag> / <att> / <val>) elements. -->
  <xsl:function name="local:get.delimiter">
    <xsl:param name="position"/>
    <xsl:param name="node"/>
    <xsl:variable name="current.name" select="if ($node/(self::comment()|self::processing-instruction())) then 'tag' else local-name($node)"/>
    <xsl:variable name="current.type" select="if ($node/self::comment()) then 'comment' else if ($node/self::processing-instruction()) then 'pi' else $node/@type"/>
    <xsl:variable name="delimiter.type" select="string-join(($current.name, $current.type), '.')"/>
    <xsl:choose>
      <xsl:when test="$position = 'start'">
        <xsl:value-of select="$delimiter.start/(*[@n = $delimiter.type], *[@n = $current.name])[1]"/>
      </xsl:when>
      <xsl:when test="$position = 'end'">
        <xsl:value-of select="$delimiter.end/(*[@n = $delimiter.type], *[@n = $current.name])[1]"/>        
      </xsl:when>
    </xsl:choose>
  </xsl:function>
  
  <!-- This function calculates the depth of quotable elements 
    inside other quotable elements. -->
  <xsl:function name="local:get.quoteLevel">
    <xsl:param name="current"/>
    <xsl:value-of select="count($current/ancestor::*[. intersect key('quotation.elements', local-name())])"/>
  </xsl:function>
  
</xsl:stylesheet>
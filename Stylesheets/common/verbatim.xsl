<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:i="http://www.iso.org/ns/1.0" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:s="http://www.ascc.net/xml/schematron" xmlns:sch="http://purl.oclc.org/dsdl/schematron" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xi="http://www.w3.org/2001/XInclude" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:m="http://www.w3.org/1998/Math/MathML" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:dbk="http://docbook.org/ns/docbook" exclude-result-prefixes="a fo html i rng s sch tei teix xi xs xsl         m atom xlink xhtml dbk" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet dealing with elements from the core module. </p>
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
  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:* m:*"/>
  <xsl:param name="useNSPrefixes">true</xsl:param>
  <xsl:param name="spaceCharacter">&#160;</xsl:param>
  <xsl:param name="showNamespaceDecls">true</xsl:param>
  <xsl:param name="forceWrap">false</xsl:param>
  <xsl:param name="wrapLength">65</xsl:param>
  <xsl:param name="attLength">40</xsl:param>
  <xsl:param name="omitNSDecls">http://www.tei-c.org/ns/1.0</xsl:param>
  <xsl:key name="NSUsed" match="*" use="namespace-uri()"/>
  <xsl:key name="NSUsed" match="@*" use="namespace-uri()"/>
  <xsl:variable name="dq">"</xsl:variable>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Work out a namespace prefix for verbatim elements</p>
      <p>[common] </p>
    </desc>
  </doc>
  <xsl:template name="verbatim-getNamespacePrefix">
    <xsl:variable name="ns" select="namespace-uri()"/>
    <xsl:variable name="prefix"
		  select="prefix-from-QName(node-name(ancestor-or-self::*[1]))"/>
    <xsl:choose>
      <xsl:when test="$prefix !=''">
        <xsl:value-of select="$prefix"/>
      </xsl:when>
      <xsl:when test="$ns='http://docbook.org/ns/docbook'">dbk</xsl:when>
      <xsl:when test="$ns='http://earth.google.com/kml/2.1'">kml</xsl:when>
      <xsl:when test="$ns='http://purl.oclc.org/dsdl/nvdl/ns/structure/1.0'">nvdl</xsl:when>
      <xsl:when test="$ns='http://purl.org/rss/1.0/modules/event/'">ev</xsl:when>
      <xsl:when test="$ns='http://purl.oclc.org/dsdl/schematron'">s</xsl:when>
      <xsl:when test="$ns='http://relaxng.org/ns/compatibility/annotations/1.0'">a</xsl:when>
      <xsl:when test="$ns='http://relaxng.org/ns/structure/1.0'">rng</xsl:when>
      <xsl:when test="$ns='http://www.ascc.net/xml/schematron'">sch</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/1998/Math/MathML'">m</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/1999/XSL/Transform'">xsl</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/1999/xhtml'">xhtml</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/1999/xlink'">xlink</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/2001/XMLSchema'">xsd</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/2005/11/its'">its</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/2005/Atom'">atom</xsl:when>
      <xsl:when test="$ns='http://www.w3.org/XML/1998/namespace'">xml</xsl:when>
      <xsl:when test="$ns='http://www.tei-c.org/ns/geneticEditions'">ge</xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="verbatim-newLine"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Make newline</p>
      <p>[common] </p>
      <param name="id">identifier for debugging only</param>
    </desc>
  </doc>
  <xsl:template name="verbatim-lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process comments in verbatim mode</p>
      <p>[common] </p>
    </desc>
  </doc>
  <xsl:template match="comment()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="ancestor::Wrapper"/>
      <xsl:when test="ancestor::xhtml:Wrapper"/>
      <xsl:otherwise>
        <xsl:call-template name="verbatim-lineBreak">
          <xsl:with-param name="id">21</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="Comment">
	  <xsl:with-param name="content">
	    <xsl:text>&lt;!--</xsl:text>
	    <xsl:choose>
	      <xsl:when test="$forceWrap='true'">
		<xsl:call-template name="verbatim-reformatText">
		  <xsl:with-param name="sofar">0</xsl:with-param>
		  <xsl:with-param name="indent">
		    <xsl:text> </xsl:text>
		  </xsl:with-param>
		  <xsl:with-param name="text">
		    <xsl:value-of select="normalize-space(.)"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="verbatim-Text">
		  <xsl:with-param name="words">
		    <xsl:value-of select="."/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:text>--&gt;</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process text nodes in verbatim mode</p>
      <p>[common] </p>
    </desc>
  </doc>
  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="parent::teix:egXML and not(parent::teix:egXML/*)">
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
	    <xsl:value-of select="translate(.,' ','&#160;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when
	  test="ancestor::*[@xml:space][1]/@xml:space='preserve'">
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
	    <xsl:value-of select="translate(.,' ','&#160;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$forceWrap='true'">
        <xsl:variable name="indent">
          <xsl:for-each select="parent::*">
            <xsl:call-template name="verbatim-makeIndent"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:if test="string-length(.)&gt;$wrapLength or parent::sch:assert">
          <xsl:text>&#10;</xsl:text>
          <xsl:value-of select="$indent"/>
        </xsl:if>
        <xsl:call-template name="verbatim-reformatText">
          <xsl:with-param name="sofar">0</xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="normalize-space(.)"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="string-length(.)&gt;$wrapLength or parent::sch:assert">
          <xsl:text>&#10;</xsl:text>
          <xsl:value-of select="$indent"/>
        </xsl:if>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node() or         contains(.,'&#10;'))">
        <xsl:if test="starts-with(.,' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="normalize-space(.)"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="substring(.,string-length(.),1)=' '">
          <xsl:text> </xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:when test="normalize-space(.)=''">
        <xsl:for-each select="following-sibling::*[1]">
          <xsl:call-template name="verbatim-lineBreak">
            <xsl:with-param name="id">7</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="verbatim-makeIndent"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="verbatim-wraptext">
          <xsl:with-param name="count">0</xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:for-each select="parent::*">
              <xsl:call-template name="verbatim-makeIndent"/>
            </xsl:for-each>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:choose>
              <xsl:when test="starts-with(.,'&#10;') and not          (preceding-sibling::node())">
                <xsl:value-of select="translate(substring(.,2),'&#10;','⌤')"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="translate(.,'&#10;','⌤')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
        <!--
	<xsl:if test="substring(.,string-length(.))=' '">
	  <xsl:text> </xsl:text>
	</xsl:if>
	-->
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="verbatim-reformatText">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:param name="sofar"/>
    <xsl:choose>
      <xsl:when test="number($sofar) &gt; $wrapLength">
        <xsl:text>&#10;</xsl:text>
        <xsl:value-of select="$indent"/>
        <xsl:call-template name="verbatim-reformatText">
          <xsl:with-param name="text">
            <xsl:value-of select="$text"/>
          </xsl:with-param>
          <xsl:with-param name="sofar">
            <xsl:text>0</xsl:text>
          </xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="not(contains($text,' '))">
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="$text"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="chunk">
          <xsl:value-of select="substring-before($text,' ')"/>
        </xsl:variable>
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="$chunk"/>
            <xsl:text> </xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="verbatim-reformatText">
          <xsl:with-param name="text">
            <xsl:value-of select="substring-after($text,' ')"/>
          </xsl:with-param>
          <xsl:with-param name="sofar">
            <xsl:value-of select="$sofar + string-length($chunk) + 1"/>
          </xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="verbatim-wraptext">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:param name="count">0</xsl:param>
    <xsl:variable name="finalSpace">
      <xsl:choose>
        <xsl:when test="substring($text,string-length($text),1)=' '">
          <xsl:text> </xsl:text>
        </xsl:when>
        <xsl:when test="substring($text,string-length($text),1)='⌤'">
          <xsl:text> </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!--
<xsl:message>my text is [<xsl:value-of select="$text"/>]</xsl:message>
<xsl:message>my space is [<xsl:value-of select="$finalSpace"/>]</xsl:message>
-->
    <xsl:choose>
      <xsl:when test="normalize-space($text)=''"/>
      <xsl:when test="contains($text,'⌤')">
        <xsl:if test="$count &gt; 0">
          <xsl:value-of select="$indent"/>
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:if test="starts-with($text,' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="normalize-space(substring-before($text,'⌤'))"/>
          </xsl:with-param>
        </xsl:call-template>
        <!--	<xsl:if test="not(substring-after($text,'&#10;')='')">-->
        <xsl:call-template name="verbatim-lineBreak">
          <xsl:with-param name="id">6</xsl:with-param>
        </xsl:call-template>
        <xsl:value-of select="$indent"/>
        <xsl:call-template name="verbatim-wraptext">
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="normalize-space(substring-after($text,'⌤'))"/>
            <xsl:value-of select="$finalSpace"/>
          </xsl:with-param>
          <xsl:with-param name="count">
            <xsl:value-of select="$count + 1"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="starts-with($text,' ')">
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:if test="$count &gt; 0 and parent::*">
          <xsl:value-of select="$indent"/>
          <xsl:text> </xsl:text>
        </xsl:if>
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="normalize-space($text)"/>
            <xsl:value-of select="$finalSpace"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process text and process ampersands</p>
      <p>[common] </p>
    </desc>
  </doc>
  <xsl:template name="verbatim-Text">
    <xsl:param name="words"/>
    <xsl:param name="escape">true</xsl:param>
    <xsl:analyze-string select="$words" regex="(&amp;)(.?)">
      <xsl:matching-substring>
        <xsl:choose>
          <xsl:when test="regex-group(2)='#'">
            <xsl:text>&amp;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>&amp;amp;</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:value-of select="regex-group(2)"/>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
        <xsl:value-of select="."/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>Process default elements in verbatim mode</p>
      <p>[common] </p>
    </desc>
  </doc>
  <xsl:template match="*" mode="verbatim">
    <xsl:param name="highlight"/>
    <xsl:choose>
      <xsl:when test="parent::xhtml:Wrapper"/>
      <xsl:when test="parent::Wrapper"/>
      <!--      <xsl:when test="child::node()[last()]/self::text()[not(.='')] and child::node()[1]/self::text()[not(.='')]"/>-->
      <xsl:when test="ancestor::*[@xml:space][1]/@xml:space = 'preserve'"/>
      <xsl:when test="not(parent::*) or parent::teix:egXML or parent::PureODD">
        <xsl:choose>
          <xsl:when
            test="
              preceding-sibling::node()[1][self::text()]
              and
              following-sibling::node()[1][self::text()]"/>
          <xsl:when test="preceding-sibling::*">
            <xsl:call-template name="verbatim-lineBreak">
              <xsl:with-param name="id">-1</xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="verbatim-newLine"/>
            <!-- <xsl:call-template name="makeIndent"/>-->
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node())">
        <xsl:call-template name="verbatim-lineBreak">
          <xsl:with-param name="id">-2</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="verbatim-makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::*">
        <xsl:call-template name="verbatim-lineBreak">
          <xsl:with-param name="id">1</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="verbatim-makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::text()"> </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="verbatim-lineBreak">
          <xsl:with-param name="id">9</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="verbatim-makeIndent"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:variable name="eContents">
      <xsl:text>&lt;</xsl:text>
      <xsl:call-template name="verbatim-makeElementName">
        <xsl:with-param name="start">true</xsl:with-param>
        <xsl:with-param name="highlight">
          <xsl:value-of select="$highlight"/>
        </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="verbatim-processAttributes"/>
      <xsl:if
        test="(local-name(.) = 'TEI' and not(local-name(parent::*) = 'teiCorpus')) or local-name(.) = 'teiCorpus'">
        <xsl:text> xmlns="http://www.tei-c.org/ns/1.0"</xsl:text>
      </xsl:if>
      <xsl:if test="local-name(.) eq 'egXML'">
	<xsl:text> xmlns="http://www.tei-c.org/ns/Examples"</xsl:text>
      </xsl:if>
      <xsl:if test="$showNamespaceDecls = 'true' or parent::teix:egXML[tei:match(@rend, 'full')]">
        <xsl:choose>
          <xsl:when test="not(parent::*)">
            <xsl:call-template name="nsList"/>
          </xsl:when>
          <xsl:when test="parent::teix:egXML and not(preceding-sibling::*)">
            <xsl:call-template name="nsList"/>
          </xsl:when>
        </xsl:choose>
      </xsl:if>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:call-template name="Element">
          <xsl:with-param name="content">
            <xsl:copy-of select="$eContents"/>
            <xsl:text>&gt;</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:apply-templates mode="verbatim">
          <xsl:with-param name="highlight">
            <xsl:value-of select="$highlight"/>
          </xsl:with-param>
        </xsl:apply-templates>
        <xsl:choose>
          <xsl:when test="ancestor::*[@xml:space][1]/@xml:space = 'preserve'"/>
          <xsl:when test="child::node()[last()]/self::text()[normalize-space(.) = '']">
            <xsl:call-template name="verbatim-lineBreak">
              <xsl:with-param name="id">3</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="verbatim-makeIndent"/>
          </xsl:when>
          <!-- Don't insert a linebreak for mixed content?? -->
          <xsl:when test="child::node()[last()]/self::text() and child::node()[1]/self::text()"/>
          <xsl:when test="not(parent::*) or parent::teix:egXML or parent::PureODD">
            <xsl:call-template name="verbatim-lineBreak">
              <xsl:with-param name="id">23</xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::comment()">
            <xsl:call-template name="verbatim-lineBreak">
              <xsl:with-param name="id">4</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="verbatim-makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::*">
            <xsl:call-template name="verbatim-lineBreak">
              <xsl:with-param name="id">5</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="verbatim-makeIndent"/>
          </xsl:when>
        </xsl:choose>
        <xsl:call-template name="Element">
          <xsl:with-param name="content">
            <xsl:text>&lt;/</xsl:text>
            <xsl:call-template name="verbatim-makeElementName">
              <xsl:with-param name="start">false</xsl:with-param>
              <xsl:with-param name="highlight">
                <xsl:value-of select="$highlight"/>
              </xsl:with-param>
            </xsl:call-template>
            <xsl:text>&gt;</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="Element">
          <xsl:with-param name="content">
            <xsl:copy-of select="$eContents"/>
            <xsl:text>/&gt;</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="verbatim-createElement">
    <xsl:param name="name"/>
    <xsl:param name="special"/>
    <xsl:choose>
      <xsl:when test="$special='true'">
	<xsl:call-template name="HighlightElementName">
	  <xsl:with-param name="content">
	    <xsl:value-of select="$name"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="verbatim-makeElementName">
    <xsl:param name="start"/>
    <xsl:param name="highlight"/>
    <!-- get namespace prefix -->
    <xsl:variable name="ns-prefix">
      <xsl:call-template name="verbatim-getNamespacePrefix"/>
    </xsl:variable>
    <xsl:variable name="highlightMe">
      <xsl:choose>
        <xsl:when test="$highlight=local-name()">true</xsl:when>
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="namespace-uri()='http://www.tei-c.org/ns/Examples'">
        <xsl:call-template name="verbatim-createElement">
          <xsl:with-param name="name" select="local-name(.)"/>
          <xsl:with-param name="special">
            <xsl:value-of select="$highlightMe"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="contains($omitNSDecls,namespace-uri())">
        <xsl:call-template name="verbatim-createElement">
          <xsl:with-param name="name" select="local-name(.)"/>
          <xsl:with-param name="special">
            <xsl:value-of select="$highlightMe"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$useNSPrefixes='true' and string-length($ns-prefix) &gt; 0">
        <xsl:call-template name="verbatim-createElement">
          <xsl:with-param name="name" select="concat($ns-prefix,':',local-name(.))"/>
          <xsl:with-param name="special">
            <xsl:value-of select="$highlightMe"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="not(namespace-uri()='')">
        <xsl:call-template name="verbatim-createElement">
          <xsl:with-param name="name" select="local-name(.)"/>
          <xsl:with-param name="special">
            <xsl:value-of select="$highlightMe"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="$start='true' and not(namespace-uri()=namespace-uri(..))">
          <xsl:text> xmlns="</xsl:text>
          <xsl:call-template name="NamespaceURI">
            <xsl:with-param name="uri" select="namespace-uri()"/>
          </xsl:call-template>
          <xsl:text>"</xsl:text>
          <!-- 
	       <xsl:call-template name="verbatim-lineBreak">
	       <xsl:with-param name="id">5</xsl:with-param>
	       </xsl:call-template>
	       <xsl:call-template name="verbatim-makeIndent"/>
	  -->
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="ElementName">
	  <xsl:with-param name="content">
	    <xsl:call-template name="verbatim-createElement">
	      <xsl:with-param name="name" select="local-name(.)"/>
	      <xsl:with-param name="special">
		<xsl:value-of select="$highlightMe"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="verbatim-makeIndent">
    <xsl:if
	test="not(ancestor::*[@xml:space][1]/@xml:space='preserve')">
      <xsl:variable name="depth" select="count(ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')])-1"/>
      <xsl:sequence select="for $i in 1 to $depth return $spaceCharacter"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="verbatim-processAttributes">
    <xsl:variable name="esize">
      <xsl:value-of select="string-length(name())+count(ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')])-1"/>
    </xsl:variable>
    <xsl:variable name="indent">
      <xsl:call-template name="verbatim-makeIndent"/>
    </xsl:variable>
    <xsl:variable name="Atts">
      <xsl:for-each select="@*">
        <xsl:variable name="value">
          <xsl:choose>
            <xsl:when test="ancestor::*[@xml:space][1]/@xml:space='preserve'"><xsl:value-of select="."/></xsl:when>
            <xsl:otherwise><xsl:value-of select="replace(.,'\s+',' ')"/></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
	<a size="{string-length(name())+string-length(.)+3}"
	   name="{name()}" 
	   value="{$value}">
	</a>
      </xsl:for-each>
    </xsl:variable>
    <!-- only make links for Pure ODD content models -->
    <xsl:variable name="link-refs" select="ancestor::PureODD"/>
      <xsl:choose>
	<xsl:when
	    test="parent::*/ancestor::tei:cell[not(tei:match(@rend,'wovenodd-col2'))]">
	  <xsl:for-each select="$Atts/*[1]">
	    <xsl:call-template name="verbatim-nextAttribute">
	      <xsl:with-param name="force" select="true()"/>
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="sofar" select="$esize + @size"/>
	    </xsl:call-template>
	  </xsl:for-each>
	  </xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="$Atts/*[1]">
	    <xsl:choose>
	      <xsl:when test="@name='key'">
	        <xsl:call-template name="verbatim-nextAttribute">
	          <xsl:with-param name="indent" select="$indent"/>
	          <xsl:with-param name="sofar" select="$esize + @size"/>
	          <xsl:with-param name="ref-link" select="$link-refs"/>
	        </xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
          <xsl:call-template name="verbatim-nextAttribute">
    	      <xsl:with-param name="indent" select="$indent"/>
    	      <xsl:with-param name="sofar" select="$esize + @size"/>
    	    </xsl:call-template>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>

  </xsl:template>


  <xsl:template name="verbatim-nextAttribute">
    <xsl:param name="indent"/>
    <xsl:param name="force" select="false()"/>
    <xsl:param name="sofar">0</xsl:param>
    <xsl:param name="ref-link" select="false()"/>
    <xsl:value-of select="$spaceCharacter"/>
    <xsl:call-template name="Attribute">
      <xsl:with-param name="content" select="string(@name)"/>
    </xsl:call-template>
    <xsl:text>="</xsl:text>
    <xsl:choose>
      <xsl:when test="$ref-link">
        <xsl:call-template name="RefLinkAttributeValue">
          <xsl:with-param name="content">
            <xsl:apply-templates select="@value" mode="attributetext"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="AttributeValue">
          <xsl:with-param name="content">
	           <xsl:apply-templates select="@value" mode="attributetext"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>"</xsl:text>
    <xsl:for-each select="following-sibling::a[1]">
      <xsl:choose>
	<xsl:when test="$force or ($sofar + @size &gt;$attLength)">
	  <xsl:call-template name="verbatim-lineBreak">
            <xsl:with-param name="id">5</xsl:with-param>
	  </xsl:call-template>
	  <xsl:copy-of select="$indent"/>
	  <xsl:call-template name="verbatim-nextAttribute">
	    <xsl:with-param name="indent" select="$indent"/>
	    <xsl:with-param name="force" select="$force"/>
	    <xsl:with-param name="sofar">0</xsl:with-param>
	  </xsl:call-template>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="verbatim-nextAttribute">
	    <xsl:with-param name="indent" select="$indent"/>
	    <xsl:with-param name="sofar" select="$sofar +@size"/>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
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
		<xsl:value-of select="."/>
	  </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="verbatim-Text">
          <xsl:with-param name="words">
            <xsl:value-of select="."/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="text()|comment()|processing-instruction()" mode="ns"/>
  <xsl:template match="*" mode="ns">
    <xsl:for-each select="namespace::*">
      <xsl:variable name="ns" select="."/>
      <xsl:choose>
        <xsl:when test=".='http://relaxng.org/ns/structure/1.0'"/>
        <xsl:when test=".='http://www.w3.org/2001/XInclude'"/>
        <xsl:when test=".='http://www.tei-c.org/ns/Examples'"/>
        <xsl:when test=".='http://www.ascc.net/xml/schematron'"/>
        <xsl:when test=".='http://relaxng.org/ns/compatibility/annotations/1.0'"/>
        <xsl:when test="name(.)=''"/>
        <xsl:when test=".='http://www.w3.org/XML/1998/namespace'"/>
        <xsl:otherwise>
          <ns name="{name(.)}" value="{.}"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:apply-templates mode="ns"/>
  </xsl:template>
  <xsl:template name="nsList">
    <xsl:variable name="ns">
      <all>
        <names>
          <xsl:apply-templates select="." mode="ns"/>
        </names>
        <text>
          <xsl:copy-of select="."/>
        </text>
      </all>
    </xsl:variable>

    <xsl:for-each select="$ns/all/names">
      <xsl:for-each-group select="ns" group-by="@name">
        <xsl:if test="key('NSUsed',@value)">
          <xsl:call-template name="verbatim-lineBreak">
            <xsl:with-param name="id">22</xsl:with-param>
          </xsl:call-template>
          <xsl:sequence select="concat('&#160;&#160;&#160;xmlns:',@name,'=',$dq,@value,$dq)"/>
        </xsl:if>
      </xsl:for-each-group>
    </xsl:for-each>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element in a verbatim context</desc>
  </doc>

  <xsl:template name="Element">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element name in a verbatim context</desc>
  </doc>
  <xsl:template name="ElementName">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML element name highlighted in a verbatim context</desc>
  </doc>
 <xsl:template name="HighlightElementName">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>
  
  <xsl:template name="RefLinkAttributeValue">
    <xsl:param name="content"/>
    <xsl:call-template name="Attribute">
      <xsl:with-param name="content" select="$content"/>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML attribute value in a verbatim context</desc>
  </doc>

  <xsl:template name="AttributeValue">
    <xsl:param name="content"/>
    <xsl:copy-of select="$content"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML attribute in a verbatim context</desc>
  </doc>

  <xsl:template name="Attribute">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template> 

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML namespace in a verbatim context</desc>
  </doc>
  <xsl:template name="Namespace">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Print the namespace URI</desc>
  </doc>
  <xsl:template name="NamespaceURI">
    <xsl:param name="uri"/>
    <xsl:value-of select="$uri"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>show an XML comment in a verbatim context</desc>
  </doc>
  <xsl:template name="Comment">
    <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>


</xsl:stylesheet>

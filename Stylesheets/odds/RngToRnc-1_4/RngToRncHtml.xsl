<?xml version="1.0"?><!--*- XML -*-->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- $Id$ -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     Copyright (c) 2002, Pantor Engineering AB
     All rights reserved.
     
     Redistribution and use in source and binary forms, with or
     without modification, are permitted provided that the following
     conditions are met:
     
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer. 

     * Redistributions in binary form must reproduce the above
       copyright notice, this list of conditions and the following
       disclaimer in the documentation and/or other materials provided
       with the distribution.

     * Neither the name of Pantor Engineering AB nor the names of its
       contributors may be used to endorse or promote products derived
       from this software without specific prior written permission.
     
     THIS   SOFTWARE   IS PROVIDED BY    THE   COPYRIGHT  HOLDERS  AND
     CONTRIBUTORS "AS IS"   AND ANY  EXPRESS OR  IMPLIED   WARRANTIES,
     INCLUDING,  BUT  NOT LIMITED  TO,   THE  IMPLIED  WARRANTIES   OF
     MERCHANTABILITY    AND  FITNESS  FOR   A  PARTICULAR  PURPOSE ARE
     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
     BE  LIABLE   FOR ANY    DIRECT, INDIRECT,   INCIDENTAL,  SPECIAL,
     EXEMPLARY, OR CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT  LIMITED
     TO, PROCUREMENT  OF  SUBSTITUTE GOODS OR  SERVICES;  LOSS OF USE,
     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
     ANY THEORY OF  LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,
     OR  TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING  IN ANY WAY
     OUT OF  THE  USE OF   THIS  SOFTWARE,  EVEN IF ADVISED   OF   THE
     POSSIBILITY OF SUCH DAMAGE.

     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     Created by David.Rosenborg@pantor.com
     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     RngToRncHtml.xsl converts a RELAX NG schema in XML syntax to an
     HTML document. The result is a hypertext representation of
     the schema in the compact syntax. The document will contain
     links between references and named patterns (defines). Also,
     an index is generated at the end, containing all definitions
     in alphabetic order with reverse references and links to
     possible multiple definitions.

     If the source document is a RELAX NG schema, an extension
     function converting result tree fragments to node sets is
     required. If the processor supports the exslt:node-set function,
     no configuration should be needed. See RngToRncProcessorConfig.xsl
     for further information.

     This stylesheet can also be applied to the intermediate XML
     representation of a compact schema as output by the
     RngToRncXml.xsl stylesheet. In this case, no extension function
     is required.

     For a description of the underlying XML to compact syntax
     transformation, see RngToRncXml.xsl.

     Current limitations/known deficiencies:

     * Inconsitent use of URIs in externalRef patterns can cause
       multiple renditions of the same schema.

     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
>

  <xsl:import href="RngToRncXml.xsl"/>
  <xsl:import href="RngToRncProcessorConfig.xsl"/>

  <xsl:output method="html"/>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <!-- Parameters -->
  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <!-- indent-width:
     The number of characters to indent at each indentation level -->

  <xsl:param name="indent-width" select="3"/>

  <!-- line-width:
     see the group-lines parameter. -->

  <xsl:param name="line-width" select="120"/>

  <!-- collapse-lines:
     If true, output constructs spanning multiple lines will be
     grouped into a single line unless it exceeds $line-width chars. -->

  <xsl:param name="collapse-lines" select="true ()"/>

  <!-- recursive:
     See RngToRncXml.xsl -->

  <xsl:param name="recursive" select="true ()"/>

  <!-- title:
     The title of the document. If not specified, a title is looked
     for in /rng:grammar/xhtml:title. If not present the string 'Schema'
     will be used. -->

  <xsl:param name="title"/>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <xsl:template match="/">
    <xsl:variable name="doc-title">
      <xsl:choose>
	<xsl:when test="$title">
	  <xsl:value-of select="$title"/>
	</xsl:when>
	<xsl:when test="/rng:grammar/xhtml:title">
	  <xsl:value-of select="/rng:grammar/xhtml:title"/>
	</xsl:when>
	<xsl:otherwise>Schema</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <html>
      <head>
	<title><xsl:value-of select="$doc-title"/></title>
	<style type="text/css">
	  BODY
	  {
	    margin-left: 10px;
	    font-family: Verdana, sans-serif;
	    font-size: 11px;
	  }
	  TD
	  {
	    font-family: Verdana, sans-serif;
	    font-size: 11px;
	  }
	  .decl { color: #006400; }
	  A.def:link, A.def:visited
	  { font-weight: bold; color: #001188; text-decoration: none; }
	  A.def:hover
	  { font-weight: bold; color: #0055ff; text-decoration: underline; }
	  A.ref:link, A.ref:visited { color: #000000; text-decoration: none; }
	  A.ref:hover { color: #0055ff; text-decoration: underline; }
	  A.ix-ref:link, A.ix-ref:visited { color: #0000bb; }
	  A.ix-ref:hover { color: #0055ff; }
	  .nc { color: #0000aa; }
	  .str { color: #8b0000; }
	  A.href:link, A.href:visited 
	  { color: #8b0000; text-decoration: none; }
	  A.href:hover { color: #0055ff; text-decoration: underline; }
	  .prefix { color: #0000aa; }
	  .param { color: #000000; }
	  .comment { color: #008b00; }
	  .annot { color: #666666; }
	  .title
	  { 
	    font-family: Arial, Verdana, sans-serif; font-size: 18pt; 
	    margin-left: 10px;
	    margin-top: 10px;
	    margin-bottom: 10px;
	  }
	  .heading
	  { 
	    font-family: Arial, Verdana, sans-serif; font-size: 14pt; 
	    margin-left: 10px;
	    margin-top: 4px;
	    margin-bottom: 4px;
	  }
	  TD.head { color: #ffffff; background-color: #29445a; }
	  TD.line { background-color: #6d8090; font-size: 3px;}
	  .title-line TD { background-color: #6d8090; }
	  DIV.section 
	  { 
	    margin-left: 10px; 
	    margin-top: 15px;
            margin-bottom: 15px;
          }
	  DIV.ix-definitions 
	  { 
	    margin-left: 26px; 
	    text-indent: -10px; 
            margin-bottom: 4px;
          }
	  DIV.ix-section 
	  { 
	    margin-left: 16px; 
            margin-bottom: 4px;
          }
	  TABLE.back-ref { margin-left: 10px; }
	  TABLE.back-ref TD { padding-right: 8px; }
	  .ix-entry 
	  { 
	    margin-bottom: 4px; 
            width: 100%;
	    background-color: #f0f0f0;
	    border-style: solid;
            border-width: 1px;
            border-color: #6d8090;
	    margin-right: 10px;
	    padding: 4px;
	  }
	  .ix-heading
	  { margin-bottom: 4px; font-weight: bold; color: #001188; }
	  .no-ref { color: red; font-weight: bold; }
	  .index-ref
	  {
	    margin-right: 10px;
	    margin-bottom: 2px;
	    font-family: Arial, Verdana, sans-serif; 
            font-size: 10pt;
	  }
	  .index-ref A:link, .index-ref A:visited { color: #ffffff; }
	  .index-ref A:hover { color: #aaffff; }
	</style>
      </head>
      <body>
	<table cellpadding="0" cellspacing="0" border="0" width="100%">
	  <tr>
	    <td class="head">
	      <div class="title"><xsl:value-of select="$doc-title"/></div>
	    </td>
	  </tr>
	  <tr class="title-line">
	    <td align="right">
	      <div class="index-ref"><a href="#index">Index</a></div>
	    </td>
	  </tr>
	</table>

	<xsl:choose>
	  <xsl:when test="rng:*">
	    <xsl:call-template name="make-body-from-r-t-f">
	      <xsl:with-param name="schema">
		<xsl:apply-imports/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="make-body"/>
	  </xsl:otherwise>
	</xsl:choose>
      </body>
    </html>
  </xsl:template>

  <xsl:template name="make-body">
    <div class="section">
      <xsl:apply-templates mode="keep"/>
    </div>

    <xsl:for-each select=".//include">
      <xsl:call-template name="make-heading">
	<xsl:with-param name="content">
	  <xsl:call-template name="rewrite-suffix">
	    <xsl:with-param name="href" select="@href"/>
	  </xsl:call-template>
	</xsl:with-param>
	<xsl:with-param name="id" select="@id"/>
      </xsl:call-template>
      <div class="section">
	<xsl:apply-templates mode="keep"/>
      </div>
    </xsl:for-each>

    <xsl:for-each select=".//external">
      <xsl:if test="count (key ('external', @href)[1] | .) = 1">
	<xsl:call-template name="make-heading">
	  <xsl:with-param name="content">
	    <xsl:call-template name="rewrite-suffix">
	      <xsl:with-param name="href" select="@href"/>
	    </xsl:call-template>
	  </xsl:with-param>
	  <xsl:with-param name="id" select="generate-id ()"/>
	</xsl:call-template>
	<div class="section">
	  <xsl:apply-templates mode="keep"/>
	</div>
      </xsl:if>
    </xsl:for-each>

    <xsl:call-template name="make-heading">
      <xsl:with-param name="content">Index</xsl:with-param>
      <xsl:with-param name="id">index</xsl:with-param>
    </xsl:call-template>
    <div class="section">
      <xsl:call-template name="make-index"/>
    </div>
  </xsl:template>

  <xsl:template name="make-ref-column">
    <xsl:param name="refs"/>
    <xsl:param name="from"/>
    <xsl:param name="to"/>
    <td>
      <xsl:for-each select="$refs">
	<xsl:sort select="@name"/>
	<xsl:if test="(position () > $from and ($to >= position ()))">
	  <nobr>
	    <a class="ix-ref" href="#{generate-id ()}">
	      <xsl:value-of select="."/>
	    </a>
	  </nobr>
	  <br/>
	</xsl:if>
      </xsl:for-each>
    </td>
  </xsl:template>

  <xsl:template name="make-columns2">
    <xsl:param name="refs"/>
    <xsl:param name="part"/>
    <xsl:param name="cols"/>
    <xsl:param name="col"/>
    <xsl:if test="$cols >= $col">
      <xsl:call-template name="make-ref-column">
	<xsl:with-param name="refs" select="$refs"/>
	<xsl:with-param name="from" select="$part * ($col - 1)"/>
	<xsl:with-param name="to" select="$part * $col"/>
      </xsl:call-template>

      <xsl:call-template name="make-columns2">
	<xsl:with-param name="refs" select="$refs"/>
	<xsl:with-param name="part" select="$part"/>
	<xsl:with-param name="cols" select="$cols"/>
	<xsl:with-param name="col" select="$col + 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="make-ref-columns">
    <xsl:param name="refs"/>
    <xsl:param name="nref"/>
    <xsl:param name="cols"/>
    <xsl:variable name="part" select="ceiling ($nref div $cols)"/>
    <xsl:call-template name="make-columns2">
      <xsl:with-param name="refs" select="$refs"/>
      <xsl:with-param name="part" select="$part"/>
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="col" select="1"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="make-index">
    <xsl:for-each select="//define">
      <xsl:sort select="@name"/>
      <xsl:variable name="current" select="."/>
      <xsl:variable name="key" 
	select="concat (@name, '-', generate-id (ancestor::grammar [1]))"/>
      <xsl:variable name="defs" select="key ('html-define', $key)"/>
      <xsl:variable name="ndefs" select="count ($defs)"/>
      <xsl:if test="count ($defs [1] | .) = 1">
	<div class="ix-entry" id="{generate-id ()}-ix">
	  <div class="ix-heading"><xsl:value-of select="."/></div>
	  <div class="ix-definitions">
	    <b>Definitions: </b>
	    <xsl:for-each select="$defs">
	      <xsl:if test="position () != 1">, </xsl:if>
	      <a class="ix-ref" href="#{generate-id ()}">
		<xsl:value-of select="position ()"/>
	      </a>
	    </xsl:for-each>
	  </div>
	  <xsl:variable name="back-refs" select="key ('html-ref', $key)
	    /ancestor::group[define][1]/define"/>
	  
	  <xsl:choose>
	    <xsl:when test="$back-refs">
	      <div class="ix-section">
		<b>Referenced from: </b>
		<xsl:variable name="nref" select="count ($back-refs)"/>

		<table cellpadding="0" cellspacing="0" border="0" 
		  class="back-ref">
		  <tr valign="top">
		    <xsl:choose>
		      <xsl:when test="8 > $nref">
			<xsl:call-template name="make-ref-columns">
			  <xsl:with-param name="refs" select="$back-refs"/>
			  <xsl:with-param name="nref" select="$nref"/>
			  <xsl:with-param name="cols" select="1"/>
			</xsl:call-template>
		      </xsl:when>
		      <xsl:when test="16 > $nref">
			<xsl:call-template name="make-ref-columns">
			  <xsl:with-param name="refs" select="$back-refs"/>
			  <xsl:with-param name="nref" select="$nref"/>
			  <xsl:with-param name="cols" select="2"/>
			</xsl:call-template>
		      </xsl:when>
		      <xsl:when test="24 > $nref">
			<xsl:call-template name="make-ref-columns">
			  <xsl:with-param name="refs" select="$back-refs"/>
			  <xsl:with-param name="nref" select="$nref"/>
			  <xsl:with-param name="cols" select="3"/>
			</xsl:call-template>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:call-template name="make-ref-columns">
			  <xsl:with-param name="refs" select="$back-refs"/>
			  <xsl:with-param name="nref" select="$nref"/>
			  <xsl:with-param name="cols" select="4"/>
			</xsl:call-template>
		      </xsl:otherwise>
		    </xsl:choose>
		  </tr>
		</table>
	      </div>
	    </xsl:when>
	    <xsl:when test=". != 'start'">
	      <div class="ix-section">
		<span class="no-ref">Not referenced from any pattern</span>
	      </div>
	    </xsl:when>
	  </xsl:choose>
	</div>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="make-heading">
    <xsl:param name="content"/>
    <xsl:param name="class" select="'heading'"/>
    <xsl:param name="line-class" select="'line'"/>
    <xsl:param name="id"/>
    <table cellpadding="0" cellspacing="0" border="0" width="100%">
      <xsl:if test="$id">
	<xsl:attribute name="id"><xsl:value-of select="$id"/></xsl:attribute>
      </xsl:if>
      <tr>
	<td class="head">
	  <div class="{$class}"><xsl:value-of select="$content"/></div>
	</td>
      </tr>
      <tr>
	<td class="{$line-class}" align="right">&#160;</td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="external | include" mode="keep"/>

  <xsl:template match="group" mode="keep">
    <xsl:choose>
      <xsl:when 
	test="
	not ($collapse-lines) or 
	@collapse = 'no' or
	.//doc or 
	.//group [@collapse = 'no']">
	<xsl:apply-templates mode="keep"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="size" select="sum (.//*/@size)"/>
	<xsl:variable name="root" 
	  select="ancestor::*[self::include or self::external][1]"/>

	<xsl:variable name="level">
	  <xsl:choose>
	    <xsl:when test="$root">
	      <xsl:value-of
		select="count (ancestor::indent 
		[ancestor::*[self::include or self::external] and 
		count (ancestor::*[self::include or self::external][1] | 
		$root) = 1])"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="count (ancestor::indent)"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>

	<xsl:choose>
	  <xsl:when test="$line-width > ($size + ($level * $indent-width))">
	    <xsl:apply-templates mode="flatten"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:apply-templates mode="keep"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sp" mode="keep">
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:template match="nl" mode="keep">
    <br/>
    <xsl:variable name="root" 
      select="ancestor::*[self::include or self::external][1]"/>

    <xsl:variable name="level">
      <xsl:choose>
	<xsl:when test="$root">
	  <xsl:value-of
	    select="count (ancestor::indent 
	    [ancestor::*[self::include or self::external] and
	    count (ancestor::*[self::include or self::external][1] | 
	    $root) = 1])"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="count (ancestor::indent)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="following-op"
      select="following-sibling::*[1][self::op]"/>
    <xsl:variable name="indent">
      <xsl:choose>
	<xsl:when test="$following-op">
	  <xsl:value-of 
	    select="$level * $indent-width - $following-op/@size"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of 
	    select="$level * $indent-width"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <span style="margin-left: {format-number ($indent * 0.85, '###.##')}ex;"/>
  </xsl:template>

  <xsl:template match="sp | nl" mode="flatten">
    <xsl:text> </xsl:text>
  </xsl:template>

  <xsl:key name="external-href" match="external" use="@id"/>
  <xsl:key name="external" match="external" use="@href"/>
  <xsl:key name="html-define" match="define"
    use="concat (@name, '-', generate-id (ancestor::grammar [1]))"/>
  <xsl:key name="html-ref" match="ref"
    use="concat (@name, '-', generate-id (ancestor::grammar [1]))"/>
  <xsl:key name="html-ref" match="parent-ref"
    use="concat (@name, '-', generate-id (ancestor::grammar [2]))"/>

  <xsl:template match="define" mode="keep">
    <xsl:variable name="key" 
      select="concat (@name, '-', generate-id (ancestor::grammar [1]))"/>
    <xsl:variable name="defs" select="key ('html-define', $key)"/>
    <a href="#{generate-id ($defs [1])}-ix" class="def" id="{generate-id ()}">
      <xsl:value-of select="."/>
    </a>
  </xsl:template>

  <xsl:template match="ref" mode="keep">
    <xsl:variable name="def"
      select="key ('html-define', concat (@name, '-', 
      generate-id (ancestor::grammar [1])))"/>
    <a class="ref" href="#{generate-id ($def)}"><xsl:value-of select="."/></a>
  </xsl:template>

  <xsl:template match="parent-ref" mode="keep">
    <xsl:variable name="def"
      select="key ('html-define', concat (@name, '-', 
      generate-id (ancestor::grammar [2])))"/>
    <a class="ref" href="#{generate-id ($def)}"><xsl:value-of select="."/></a>
  </xsl:template>

  <xsl:template match="nc" mode="keep">
    <span class="nc"><xsl:value-of select="."/></span>
  </xsl:template>

  <xsl:template match="declaration" mode="keep">
    <b class="decl"><xsl:value-of select="."/></b>
  </xsl:template>

  <xsl:template match="prefix" mode="keep">
    <span class="prefix"><xsl:value-of select="."/></span>
  </xsl:template>

  <xsl:template match="param" mode="keep">
    <spam class="param"><xsl:value-of select="."/></spam>
  </xsl:template>

  <xsl:template match="op" mode="keep">
    <xsl:value-of select="translate (., ' ', '&#160;')"/>
  </xsl:template>

  <xsl:template match="atom" mode="keep">
    <i><xsl:value-of select="."/></i>
  </xsl:template>

  <xsl:template match="t" mode="keep">
    <xsl:choose>
      <xsl:when test=". = '[' or . = ']'">
	<span class="annot"><xsl:value-of select="."/></span>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="doc" mode="keep">
    <span class="comment"><xsl:value-of select="."/></span>
  </xsl:template>

  <xsl:template match="annot" mode="keep">
    <span class="annot"><xsl:value-of select="."/></span>
  </xsl:template>

  <xsl:template match="type" mode="keep">
    <i><xsl:value-of select="."/></i>
  </xsl:template>

  <xsl:template match="keyword" mode="keep">
    <b><xsl:value-of select="."/></b>
  </xsl:template>

  <xsl:template match="str [parent::include-href]" mode="keep">
    <a href="#{../@ref}" class="href"><xsl:value-of select="."/></a>
  </xsl:template>

  <xsl:template match="str [parent::external-href]" mode="keep">
    <a 
      href="#{generate-id (key ('external', 
      key ('external-href', ../@ref)/@href))}" 
      class="href"><xsl:value-of select="."/></a>
  </xsl:template>

  <xsl:template name="nl-to-br">
    <xsl:param name="str"/>
    <xsl:param name="pos" select="1"/>
    <xsl:param name="len" select="string-length ($str)"/>
    <xsl:if test="$len >= $pos">
      <xsl:variable name="c" select="substring ($str, $pos, 1)"/>
      <xsl:choose>
	<xsl:when test="$c = '&#10;'">
	  <br/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$c"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="nl-to-br">
	<xsl:with-param name="str" select="$str"/>
	<xsl:with-param name="len" select="$len"/>
	<xsl:with-param name="pos" select="$pos + 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="str [@multiline = 'yes']" mode="keep">
    <span class="str">
      <xsl:call-template name="nl-to-br">
	<xsl:with-param name="str" select="string (.)"/>
      </xsl:call-template>
    </span>
  </xsl:template>

  <xsl:template match="str" mode="keep">
    <span class="str"><xsl:value-of select="."/></span>
  </xsl:template>

  <xsl:template match="indent | group" mode="flatten">
    <xsl:apply-templates mode="flatten"/>
  </xsl:template>

  <xsl:template match="*" mode="flatten">
    <xsl:apply-templates select="." mode="keep"/>
  </xsl:template>

</xsl:transform>

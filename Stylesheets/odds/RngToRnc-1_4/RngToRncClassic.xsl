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

     RngToRnc.xsl converts a RELAX NG schema in XML syntax to the
     compact syntax.

     The default namespace declaration is chosen from the first
     (in document order) occurrence of an ns attribute unless
     supplied as the parameter default-ns.

     If the value of an href attribute ends with '.rng', the suffix
     will be replace by the string '.rnc'. This feature can be
     disabled by setting the stylesheet parameter rewrite-suffix to
     false.

     Current limitations/known deficiencies:

     * Annotations are not handled in name classes

     * Uses of namespace prefixes must be consistent throughout the
       source schema. That is, a namespace prefix used in two QNames
       must be declared to the same namespace URI for both
       occurrences.

     * Whitespace in string literals (value patterns, data parameters
       etc) accidentally get normalized. If this causes problems, you
       can set the collapse-lines parameter to false.

     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

<xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <!-- Parameters -->
  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <!-- rewrite-suffix:
     If true and the value of an href attribute ends with '.rng', the
     suffix will be replace by the string '.rnc'. -->

  <xsl:param name="rewrite-suffix" select="true ()"/>

  <!-- retain-prefixes:
     If true, namespace declarations will use prefixes from the source
     document if possible. (Doesn't work with MSXML 4.0) -->

  <xsl:param name="retain-prefixes"
    select="system-property ('xsl:vendor') != 'Microsoft'"/>

  <!-- default-ns:
     The uri of the default namespace. There must be at least one
     ns attribute in the schema containing this uri. -->

  <xsl:param name="default-ns"
    select="string (/descendant::rng:*[@ns][1]/@ns)"/>

  <!-- prefix-map:
     A node set containing element nodes like:

     <ns prefix="x">http://example.org/x</ns>

     The name of the element is not significant. Prefixes from this
     map will be used in favor of generated and the ones found in the
     source schema. A prefix must not be the empty string. Also, it
     must not conflict with any other prefixes in the map or in the
     schema. -->

  <xsl:param name="prefix-map" select="/.."/>

  <!-- collapse-lines:
     If true, output constructs spanning multiple lines will be
     collapsed into a single line unless it exceeds $line-width chars. -->

  <xsl:param name="collapse-lines" select="true ()"/>

  <!-- line-width:
     see the collapse-lines parameter. -->

  <xsl:param name="line-width" select="80"/>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <xsl:key name="prefix" match="
    rng:element/@name [contains (., ':')] | 
    rng:attribute/@name [contains (., ':')]"
    use="substring-before (., ':')"/>

  <xsl:key name="ns" match="*" use="namespace::*"/>
  
  <xsl:key name="annot-ns" match="*[not (self::rng:*)]"
    use="namespace-uri (.)"/>

  <xsl:output method="text"/>
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="rng:value rng:param"/>
  <xsl:variable name="xsd" 
    select="'http://www.w3.org/2001/XMLSchema-datatypes'"/>

  <xsl:variable name="default-ns-nd"
    select="/descendant::rng:*[@ns = $default-ns][1]/@ns"/>
  <xsl:variable name="default-ns-id-rtf">
    <xsl:call-template name="get-prefix">
      <xsl:with-param name="nd" select="$default-ns-nd"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="default-ns-id" select="string ($default-ns-id-rtf)"/>
  <xsl:variable name="has-default-ns" select="boolean ($default-ns-nd)"/>
  <xsl:variable name="has-local" select="not (key ('prefix', 'local'))"/>

  <xsl:template match="/">
    <!-- Declarations -->

    <xsl:if test="$has-default-ns">
      <xsl:text>default namespace </xsl:text>
      <xsl:value-of select="$default-ns-id"/>
      <xsl:text> = &quot;</xsl:text>
      <xsl:value-of select="$default-ns"/>
      <xsl:text>&quot;&#10;</xsl:text>
    </xsl:if>

    <xsl:if test="$has-local">namespace local = ""&#10;</xsl:if>
    <xsl:text>namespace inh = inherit&#10;</xsl:text>

    <xsl:for-each select="//@ns">
      <xsl:if test="not ($retain-prefixes and key ('ns', string (.))) and 
	(not ($has-default-ns) or string (.) != $default-ns) and
	not ($has-local and . = '')">
	<xsl:text>namespace </xsl:text>
	<xsl:call-template name="get-prefix"/>
	<xsl:text> = &quot;</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>&quot;&#10;</xsl:text>
      </xsl:if>
    </xsl:for-each>

    <xsl:choose>
      <xsl:when test="$retain-prefixes">
	<xsl:for-each select="//*">
	  <xsl:variable name="elm" select="self::*"/>
	  <xsl:for-each select="namespace::*">
	    <xsl:variable name="ns" select="string (.)"/>
	    <xsl:if test="(not ($has-default-ns) or $ns != $default-ns) and
	      count (key ('ns', $ns)[1] | $elm) = 1 and
	      name (.) != 'xml' and 
	      ((not ($ns = 'http://relaxng.org/ns/structure/1.0') and 
	      //*[namespace-uri (.) = $ns or @ns = $ns or 
	      @*[namespace-uri (.) = $ns]]) or 
	      //*[namespace-uri (.) != 'http://relaxng.org/ns/structure/1.0']//
	      *[namespace-uri (.) = $ns or @*[namespace-uri (.) = $ns]] or
	      key ('prefix', name (.)))">
	      <xsl:text>namespace </xsl:text>
	      <xsl:variable name="mapped"
		select="$prefix-map [string (.) = string (current ())][1]"/>
	      <xsl:choose>
		<xsl:when test="$mapped">
		  <xsl:value-of select="$mapped/@prefix"/>
		</xsl:when>
		<xsl:when test="name (.)">
		  <xsl:value-of select="name (.)"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="generate-id ($elm)"/>
		</xsl:otherwise>
	      </xsl:choose>
	      <xsl:text> = &quot;</xsl:text>
	      <xsl:value-of select="."/>
	      <xsl:text>&quot;&#10;</xsl:text>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:for-each>
      </xsl:when>

      <xsl:otherwise>
	<xsl:for-each select="//rng:element | //rng:attribute">
	  <xsl:if test="@name [contains (., ':')]">
	    <xsl:variable name="pfx" select="substring-before (@name, ':')"/>
	    <xsl:if test="count (key ('prefix', $pfx)[1] | @name) = 1">
	      <xsl:text>namespace </xsl:text>
	      <xsl:variable name="ns"
		select="string (namespace::*[name () = $pfx])"/>
	      <xsl:variable name="mapped"
		select="$prefix-map [string (.) = $ns][1]"/>
	      <xsl:choose>
		<xsl:when test="$mapped">
		  <xsl:value-of select="$mapped/@prefix"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="$pfx"/>
		</xsl:otherwise>
	      </xsl:choose>
	      <xsl:text> = &quot;</xsl:text>
	      <xsl:value-of select="$ns"/>
	      <xsl:text>&quot;&#10;</xsl:text>
	    </xsl:if>
	  </xsl:if>
	</xsl:for-each>

	<xsl:for-each select="//*[not (self::rng:*)]">
	  <xsl:variable name="ns" select="namespace-uri (.)"/>
	  <xsl:if test="count (key ('annot-ns', $ns)[1] | .) = 1">
	    <xsl:text>namespace </xsl:text>
	    <xsl:variable name="mapped"
	      select="$prefix-map [string (.) = $ns][1]"/>
	    <xsl:choose>
	      <xsl:when test="$mapped">
		<xsl:value-of select="$mapped/@prefix"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="generate-id (.)"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:text> = &quot;</xsl:text>
	    <xsl:value-of select="$ns"/>
	    <xsl:text>&quot;&#10;</xsl:text>
	  </xsl:if>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:for-each select="//@datatypeLibrary">
      <xsl:if test=". != $xsd">
	<xsl:text>datatypes </xsl:text>
	<xsl:value-of select="generate-id (.)"/>
	<xsl:text> = &quot;</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>&quot;&#10;</xsl:text>
      </xsl:if>
    </xsl:for-each>

    <xsl:text>&#10;</xsl:text>

    <!-- Pattern -->
    
    <xsl:apply-templates>
      <xsl:with-param name="indent" select="0"/>
    </xsl:apply-templates>

    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template match="/rng:grammar">
    <xsl:apply-templates>
      <xsl:with-param name="indent" select="0"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="rng:grammar">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="0"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="block-start">
	  <xsl:with-param name="text">grammar</xsl:with-param>
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
	<xsl:apply-templates>
	  <xsl:with-param name="indent" select="$indent + 1"/>
	</xsl:apply-templates>
	<xsl:call-template name="block-end">
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:list | rng:mixed">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="0"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="block-start">
	  <xsl:with-param name="text" select="local-name (.)"/>
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>

	<xsl:call-template name="group-body">
	  <xsl:with-param name="indent" select="$indent + 1"/>
	</xsl:call-template>

	<xsl:call-template name="block-end">
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:start">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="indented">
	  <xsl:with-param name="text">
	    <xsl:text>start</xsl:text>
	    <xsl:call-template name="assignment-op"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>

	<xsl:call-template name="group-body">
	  <xsl:with-param name="indent" select="$indent + 1"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="spacer"/>
  </xsl:template>

  <xsl:template name="assignment-op">
    <xsl:choose>
      <xsl:when test="@combine = 'choice'"> |= </xsl:when>
      <xsl:when test="@combine = 'interleave'"> &amp;= </xsl:when>
      <xsl:otherwise> = </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:define">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">
	    <xsl:call-template name="quote-keyword">
	      <xsl:with-param name="name" select="@name"/>
	    </xsl:call-template>
	    <xsl:call-template name="assignment-op"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>

	<xsl:call-template name="group-body">
	  <xsl:with-param name="indent" select="$indent + 1"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>

    <xsl:call-template name="spacer"/>
  </xsl:template>

  <xsl:template match="rng:div">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="block-start">
      <xsl:with-param name="text">div</xsl:with-param>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:apply-templates>
      <xsl:with-param name="indent" select="$indent + 1"/>
    </xsl:apply-templates>
    <xsl:call-template name="block-end">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="spacer"/>
  </xsl:template>

  <xsl:template match="rng:include">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:variable name="head">
	  <xsl:text>include </xsl:text>
	  <xsl:call-template name="external"/>
	</xsl:variable>

	<xsl:choose>
	  <xsl:when test="*">
	    <xsl:call-template name="block-start">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text" select="$head"/>
	    </xsl:call-template>
	    <xsl:apply-templates>
	      <xsl:with-param name="indent" select="$indent + 1"/>
	    </xsl:apply-templates>
	    <xsl:call-template name="block-end">
	      <xsl:with-param name="indent" select="$indent"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text" select="$head"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="spacer"/>
  </xsl:template>

  <xsl:template match="rng:externalRef">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">
	    <xsl:text>external </xsl:text>
	    <xsl:call-template name="external"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="external">
    <xsl:text>&quot;</xsl:text>
    <xsl:variable name="suffix"
      select="substring (@href, (string-length (@href) - 4) + 1, 4)"/>
    <xsl:choose>
      <xsl:when test="$rewrite-suffix and $suffix = '.rng'">
	<xsl:value-of
	  select="substring (@href, 1, string-length (@href) - 4)"/>
	<xsl:text>.rnc</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@href"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&quot;</xsl:text>
    <xsl:variable name="ns" select="ancestor-or-self::*[@ns][1]/@ns"/>
    <xsl:choose>
      <xsl:when test="$ns and $has-default-ns and $ns = $default-ns"/>
      <xsl:when test="$ns and $has-local and $ns = ''">
	<xsl:text> inherit = local</xsl:text>
      </xsl:when>
      <xsl:when test="$ns">
	<xsl:call-template name="get-prefix">
	  <xsl:with-param name="nd" select="$ns"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$has-default-ns">
	<xsl:text> inherit = inh</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:ref">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">
	<xsl:call-template name="quote-keyword">
	  <xsl:with-param name="name" select="@name"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:parentRef">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">
	<xsl:text>parent </xsl:text>
	<xsl:call-template name="quote-keyword">
	  <xsl:with-param name="name" select="@name"/>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:element | rng:attribute">
    <xsl:param name="indent"/>

    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>

    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:variable name="head">
	  <xsl:value-of select="local-name (.)"/>
	  <xsl:text> </xsl:text>
	  <xsl:call-template name="name-class">
	    <xsl:with-param name="is-attr"
	      select="boolean (self::rng:attribute)"/>
	  </xsl:call-template>
	</xsl:variable>
	
	<xsl:choose>
	  <xsl:when test="not (rng:*) or (count (rng:*) = 1 and not (@name))">
	    <xsl:call-template name="indented">
	      <xsl:with-param name="text">
		<xsl:value-of select="$head"/>
		<xsl:text> { text }</xsl:text>
	      </xsl:with-param>
	      <xsl:with-param name="indent" select="$indent"/>
	    </xsl:call-template>
	  </xsl:when>

	  <xsl:otherwise>
	    <xsl:call-template name="block-start">
	      <xsl:with-param name="text" select="$head"/>
	      <xsl:with-param name="indent" select="$indent"/>
	    </xsl:call-template>
	    <xsl:choose>
	      <xsl:when test="@name">
		<xsl:call-template name="group-body">
		  <xsl:with-param name="indent" select="$indent + 1"/>
		</xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:call-template name="group-body">
		  <xsl:with-param name="patterns"
		    select="rng:*[1]/following-sibling::rng:*"/>
		  <xsl:with-param name="indent" select="$indent + 1"/>
		</xsl:call-template>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:call-template name="block-end">
	      <xsl:with-param name="indent" select="$indent"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="combine">
    <xsl:param name="indent"/>
    <xsl:param name="connector"/>

    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>

    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">

	<xsl:variable name="need-paren-rtf">
	  <xsl:call-template name="need-paren"/>
	</xsl:variable>
	<xsl:variable name="need-paren" 
	  select="boolean (string ($need-paren-rtf))"/>

	<xsl:choose>
	  <xsl:when test="$need-paren">
	    <xsl:call-template name="parenthesis">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="body">
		<xsl:call-template name="combine-body">
		  <xsl:with-param name="indent" select="$indent + 1"/>
		  <xsl:with-param name="connector" select="$connector"/>
		</xsl:call-template>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="combine-body">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="connector" select="$connector"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:group">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="','"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:choice" name="make-choice">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="' | '"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:interleave">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="' &amp; '"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:optional">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="','"/>
    </xsl:call-template>
    <xsl:text>?</xsl:text>
  </xsl:template>

  <xsl:template match="rng:zeroOrMore">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="','"/>
    </xsl:call-template>
    <xsl:text>*</xsl:text>
  </xsl:template>

  <xsl:template match="rng:oneOrMore">
    <xsl:param name="indent"/>
    <xsl:call-template name="combine">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="connector" select="','"/>
    </xsl:call-template>
    <xsl:text>+</xsl:text>
  </xsl:template>

  <xsl:template match="rng:text">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text" select="'text'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:empty">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text" select="'empty'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:notAllowed">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text" select="'notAllowed'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:data">
    <xsl:param name="indent"/>

    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>

    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">
	    <xsl:call-template name="type-name"/>
	  </xsl:with-param>
	</xsl:call-template>

	<xsl:if test="rng:param">
	  <xsl:call-template name="block-start">
	    <xsl:with-param name="indent" select="$indent"/>
	  </xsl:call-template>
	  <xsl:for-each select="rng:param">
	    <xsl:if test="position () != 1"><xsl:text>&#10;</xsl:text></xsl:if>
	    <xsl:call-template name="annotations">
	      <xsl:with-param name="indent" select="$indent"/>
	    </xsl:call-template>
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent + 1"/>
	      <xsl:with-param name="text">
		<xsl:value-of select="@name"/>
		<xsl:text> = </xsl:text>
		<xsl:call-template name="make-string-literal"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:for-each>
	  <xsl:call-template name="block-end">
	    <xsl:with-param name="indent" select="$indent"/>
	  </xsl:call-template>
	</xsl:if>

	<xsl:for-each select="rng:except">
	  <xsl:text> -&#10;</xsl:text>
	  <xsl:choose>
	    <xsl:when test="count (rng:*) > 1">
	      <xsl:call-template name="make-choice">
		<xsl:with-param name="indent" select="$indent"/>
	      </xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="*">
		<xsl:with-param name="indent" select="$indent"/>
	      </xsl:apply-templates>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:value">
    <xsl:param name="indent"/>
    
    <xsl:call-template name="annotations">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>

    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">
	<xsl:variable name="type">
	  <xsl:call-template name="type-name"/>
	</xsl:variable>
	<xsl:if test="$type != 'token'">
	  <xsl:value-of select="$type"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="make-string-literal"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template name="quote-quotes">
    <xsl:param name="str"/>
    <xsl:param name="pos" select="1"/>
    <xsl:variable name="c" select="substring ($str, $pos, 1)"/>
    <xsl:if test="$c">
      <xsl:choose>
	<xsl:when test="$c = '&quot;'">&quot; '&quot;' &quot;</xsl:when>
	<xsl:when test="$c = '&#10;'">\x{A}</xsl:when>
	<xsl:when test="$c = '&#13;'">\x{D}</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$c"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="quote-quotes">
	<xsl:with-param name="str" select="$str"/>
	<xsl:with-param name="pos" select="$pos + 1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="make-string-literal">
    <xsl:choose>
      <xsl:when test="not (contains (., '&quot;') or 
	contains (., &quot;&apos;&quot;) or 
	contains (., '&#10;') or contains (., '&#13;'))">
	<xsl:text>&quot;</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>&quot;</xsl:text>
      </xsl:when>
      <xsl:when test="not (contains (., '&quot;&quot;&quot;'))">
	<xsl:text>&quot;&quot;&quot;</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>&quot;&quot;&quot;</xsl:text>
      </xsl:when>
      <xsl:when test='not (contains (., "&apos;&apos;&apos;"))'>
	<xsl:text>&apos;&apos;&apos;</xsl:text>
	<xsl:value-of select="."/>
	<xsl:text>&apos;&apos;&apos;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>&quot;</xsl:text>
	<xsl:call-template name="quote-quotes">
	  <xsl:with-param name="str" select="."/>
	</xsl:call-template>
	<xsl:text>&quot;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="type-name">
    <xsl:variable name="dt"
      select="ancestor-or-self::rng:*[@datatypeLibrary][1]/@datatypeLibrary"/>
    <xsl:variable name="dts" select="string ($dt)"/>
    <xsl:choose>
      <xsl:when test="self::rng:value and not (@type)">token</xsl:when>
      <xsl:when test="$dts = '' and @type= 'string'">string</xsl:when>
      <xsl:when test="$dts = '' and @type= 'token'">token</xsl:when>
      <xsl:when test="$dts = $xsd">
	<xsl:text>xsd:</xsl:text>
	<xsl:value-of select="@type"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="generate-id ($dt)"/>
	<xsl:text>:</xsl:text>
	<xsl:value-of select="@type"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  
  <xsl:template name="need-paren">
    <xsl:choose>
      <xsl:when test="self::rng:optional or
	self::rng:zeroOrMore or
	self::rng:oneOrMore">
	<xsl:if test="count (rng:*) > 1">true</xsl:if>
      </xsl:when>
      <xsl:when test="parent::rng:element[not (@name)] or 
	parent::rng:attribute [not (@name)]">
	<xsl:if test="count (../rng:*) > 2">true</xsl:if>
      </xsl:when>
      <xsl:when test="count (../rng:*) > 1">true</xsl:when>
      <xsl:when test="count (rng:*) > 1 and (parent::rng:optional or 
	parent::rng:zeroOrMore or parent::rng:oneOrMore)">true</xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="combine-body">
    <xsl:param name="indent"/>
    <xsl:param name="patterns" select="rng:*"/>
    <xsl:param name="connector"/>
    <xsl:for-each select="$patterns">
      <xsl:if test="position () != 1">
	<xsl:choose>
	  <xsl:when test="substring ($connector, 1, 1) = ' '">
	    <xsl:text>&#10;</xsl:text>
	    <xsl:call-template name="connector">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text" select="$connector"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$connector"/>
	    <xsl:text>&#10;</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:if>
      <xsl:apply-templates select=".">
	<xsl:with-param name="indent" select="$indent"/>
      </xsl:apply-templates>
      <xsl:call-template name="follow-annotations">
	<xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="group-body">
    <xsl:param name="indent"/>
    <xsl:param name="patterns" select="rng:*"/>
    <xsl:call-template name="combine-body">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="patterns" select="$patterns"/>
      <xsl:with-param name="connector" select="','"/>
    </xsl:call-template>
  </xsl:template>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <!-- Name class -->
  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <xsl:template name="name-class">
    <xsl:param name="is-attr" select="false ()"/>
    <xsl:choose>
      <xsl:when test="@name">
	<xsl:choose>
	  <xsl:when test="not ($is-attr) and not (contains (@name, ':'))">
	    <xsl:call-template name="make-name-nc">
	      <xsl:with-param name="name" select="@name"/>
	      <xsl:with-param name="is-attr" select="false ()"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="@name"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="rng:*[1]" mode="name-class">
	  <xsl:with-param name="is-attr" select="$is-attr"/>
	</xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:name" mode="name-class">
    <xsl:param name="is-attr"/>
    <xsl:call-template name="make-name-nc">
      <xsl:with-param name="name" select="."/>
      <xsl:with-param name="is-attr" select="$is-attr"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:nsName" mode="name-class">
    <xsl:param name="is-attr"/>
    <xsl:variable name="ns" select="ancestor-or-self::rng:*[@ns][1]/@ns"/>
    <xsl:choose>
      <xsl:when test="$ns and $has-default-ns and $ns = $default-ns">
	<xsl:value-of select="$default-ns-id"/>
      </xsl:when>
      <xsl:when test="$ns and $has-local and $ns = ''">local</xsl:when>
      <xsl:when test="$ns">
	<xsl:call-template name="get-prefix">
	  <xsl:with-param name="nd" select="$ns"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>inh</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>:*</xsl:text>
    <xsl:call-template name="nc-except">
      <xsl:with-param name="is-attr" select="$is-attr"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="rng:anyName" mode="name-class">
    <xsl:param name="is-attr"/>
    <xsl:text>*</xsl:text>
    <xsl:call-template name="nc-except">
      <xsl:with-param name="is-attr" select="$is-attr"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="nc-except">
    <xsl:param name="is-attr"/>
    <xsl:for-each select="rng:except">
      <xsl:text> - </xsl:text>
      <xsl:choose>
	<xsl:when test="count (rng:*) > 1">
	  <xsl:text>(</xsl:text>
	  <xsl:call-template name="make-nc-choice">
	    <xsl:with-param name="is-attr" select="$is-attr"/>
	  </xsl:call-template>
	  <xsl:text>)</xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="rng:*" mode="name-class">
	    <xsl:with-param name="is-attr" select="$is-attr"/>
	  </xsl:apply-templates>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="rng:attribute/rng:choice | rng:element/rng:choice" 
    mode="name-class" name="make-nc-choice">
    <xsl:param name="is-attr"/>
    <xsl:for-each select="rng:*">
      <xsl:if test="position () != 1">
	<xsl:text> | </xsl:text>
      </xsl:if>
      <xsl:apply-templates select="." mode="name-class">
	<xsl:with-param name="is-attr" select="$is-attr"/>
      </xsl:apply-templates>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="rng:choice" mode="name-class">
    <xsl:param name="is-attr"/>
    <xsl:text>(</xsl:text>
    <xsl:call-template name="make-nc-choice">
      <xsl:with-param name="is-attr" select="$is-attr"/>
    </xsl:call-template>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="make-name-nc">
    <xsl:param name="is-attr"/>
    <xsl:param name="name"/>
    <xsl:variable name="ns" select="ancestor-or-self::rng:*[@ns][1]/@ns"/>
    <xsl:choose>
      <xsl:when test="not ($is-attr) and $ns and $has-default-ns and
	$ns = $default-ns"/>
      <xsl:when test="$is-attr and $ns and $ns = ''"/>
      <xsl:when test="$is-attr and $has-default-ns and $ns = $default-ns">
	<xsl:value-of select="$default-ns-id"/>
	<xsl:text>:</xsl:text>
      </xsl:when>
      <xsl:when test="$ns">
	<xsl:call-template name="get-prefix">
	  <xsl:with-param name="nd" select="$ns"/>
	</xsl:call-template>
	<xsl:text>:</xsl:text>
      </xsl:when>
      <xsl:when test="$is-attr or $has-default-ns">
	<xsl:text>inh:</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:value-of select="$name"/>
  </xsl:template>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <!-- Util -->
  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <xsl:template name="get-prefix">
    <xsl:param name="nd" select="."/>
    <xsl:variable name="ns" select="string ($nd)"/>
    <xsl:variable name="mapped" select="$prefix-map [string (.) = $ns][1]"/>
    <xsl:choose>
      <xsl:when test="$mapped">
	<xsl:value-of select="$mapped/@prefix"/>
      </xsl:when>
      <xsl:when test="not ($retain-prefixes)">
	<xsl:value-of select="generate-id ($nd)"/>
      </xsl:when>
      <xsl:when test="key ('ns', $ns)">
	<xsl:variable name="pfx"
	  select="name (key ('ns', $ns)[1]/namespace::*[. = $ns])"/>
	<xsl:choose>
	  <xsl:when test="$pfx">
	    <xsl:value-of select="$pfx"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="generate-id (key ('ns', $ns)[1])"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="generate-id ($nd)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="spacer">
    <xsl:if test="following-sibling::*">
      <xsl:text>&#10;</xsl:text>
      <xsl:if test="not (parent::rng:grammar/../..)">
	<xsl:text>&#10;</xsl:text>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <xsl:template name="make-line">
    <xsl:param name="indent"/>
    <xsl:param name="body"/>
    <xsl:choose>
      <xsl:when test="not ($collapse-lines) or descendant::a:documentation">
	<xsl:call-template name="collapse-prefix-connectors">
	  <xsl:with-param name="str" select="string ($body)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="line" select="normalize-space ($body)"/>
	<xsl:choose>
	  <xsl:when test="$line-width > (string-length ($line) + $indent * 3)">
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text" select="$line"/>
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="collapse-prefix-connectors">
	      <xsl:with-param name="str" select="string ($body)"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="collapse-prefix-connectors">
    <xsl:param name="str"/>
    <xsl:variable name="head1" select="substring-before ($str, '|  ')"/>
    <xsl:variable name="head2" 
      select="substring-before ($str, '&amp;  ')"/>
    <xsl:choose>
      <xsl:when test="$head1">
	<xsl:value-of select="$head1"/>
	<xsl:text>| </xsl:text>
	<xsl:call-template name="remove-leading-space">
	  <xsl:with-param name="str" select="substring-after ($str, '|  ')"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="$head2">
	<xsl:value-of select="$head2"/>
	<xsl:text>&amp; </xsl:text>
	<xsl:call-template name="remove-leading-space">
	  <xsl:with-param name="str"
	    select="substring-after ($str, '&amp;  ')"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$str"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="remove-leading-space">
    <xsl:param name="str"/>
    <xsl:choose>
      <xsl:when test="starts-with ($str, ' ')">
	<xsl:call-template name="remove-leading-space">
	  <xsl:with-param name="str" select="substring ($str, 2)"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="collapse-prefix-connectors">
	  <xsl:with-param name="str" select="$str"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="parenthesis">
    <xsl:param name="indent"/>
    <xsl:param name="body"/>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">(</xsl:with-param>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
    <xsl:value-of select="$body"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">)</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="block-start">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text" select="$text"/>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">{</xsl:with-param>
    </xsl:call-template>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xsl:template name="block-end">
    <xsl:param name="indent"/>
    <xsl:text>&#10;</xsl:text>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">}</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:variable name="spaces" 
    select="concat (
    '                                        ',
    '                                        '
    )"/>

  <xsl:template name="indented">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:value-of select="substring ($spaces, 1, $indent * 3)"/>
    <xsl:value-of select="$text"/>
  </xsl:template>

  <xsl:template name="connector">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:value-of select="substring ($spaces, 1, ($indent - 1) * 3)"/>
    <xsl:value-of select="$text"/>
  </xsl:template>

  <local:keywords xmlns="">
    <kw>attribute</kw>
    <kw>default</kw>
    <kw>datatypes</kw>
    <kw>div</kw>
    <kw>element</kw>
    <kw>empty</kw>
    <kw>external</kw>
    <kw>grammar</kw>
    <kw>include</kw>
    <kw>inherit</kw>
    <kw>list</kw>
    <kw>mixed</kw>
    <kw>namespace</kw>
    <kw>notAllowed</kw>
    <kw>parent</kw>
    <kw>start</kw>
    <kw>string</kw>
    <kw>text</kw>
    <kw>token</kw>
  </local:keywords>

  <xsl:variable name="keywords" select="document ('')/*/local:keywords/*"/>

  <xsl:template name="quote-keyword">
    <xsl:param name="name"/>
    <xsl:if test="$name = $keywords">\</xsl:if>
    <xsl:value-of select="$name"/>
  </xsl:template>

  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <!-- Annotations -->
  <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->

  <xsl:template name="leading-documentation">
    <xsl:param name="indent"/>
    <xsl:param name="nd"/>

    <xsl:choose>
      <xsl:when test="not ($nd) or $nd/self::rng:*"/>

      <xsl:when test="$nd/self::a:documentation">
	<xsl:call-template name="doc-comment">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="comment" select="string ($nd)"/>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="nd" select="$nd/following-sibling::*[1]"/>
	</xsl:call-template>
      </xsl:when>

      <xsl:otherwise>
	<xsl:call-template name="make-line">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="body">
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text">[</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text>&#10;</xsl:text>
	    <xsl:call-template name="annotation-attributes">
	      <xsl:with-param name="indent" select="$indent + 1"/>
	    </xsl:call-template>
	    <xsl:if test="@*[namespace-uri () != ''] and not ($nd/self::rng)">
	      <xsl:text>&#10;</xsl:text>
	    </xsl:if>
	    <xsl:call-template name="leading-annotations">
	      <xsl:with-param name="indent" select="$indent + 1"/>
	      <xsl:with-param name="nd" select="$nd"/>
	    </xsl:call-template>
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text">]</xsl:with-param>
	    </xsl:call-template>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="leading-annotations">
    <xsl:param name="indent"/>
    <xsl:param name="nd"/>

    <xsl:if test="$nd and not ($nd/self::rng:*)">
      <xsl:for-each select="$nd">
	<xsl:call-template name="annotation-element">
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:call-template name="leading-annotations">
	<xsl:with-param name="indent" select="$indent"/>
	<xsl:with-param name="nd" select="$nd/following-sibling::*[1]"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="annotations">
    <xsl:param name="indent"/>
    
    <xsl:choose>
      <xsl:when test="(self::rng:value or self::rng:param) and
	following-sibling::*[1][not (self::rng:*)]">
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="nd" select="following-sibling::*[1]"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="*[1][not (self::rng:*)]">
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="nd" select="*[1]"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="@*[namespace-uri () != '']">
	<xsl:call-template name="make-line">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="body">
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text">[</xsl:with-param>
	    </xsl:call-template>
	    <xsl:text>&#10;</xsl:text>
	    <xsl:call-template name="annotation-attributes">
	      <xsl:with-param name="indent" select="$indent + 1"/>
	    </xsl:call-template>
	    <xsl:text>&#10;</xsl:text>
	    <xsl:call-template name="indented">
	      <xsl:with-param name="indent" select="$indent"/>
	      <xsl:with-param name="text">]</xsl:with-param>
	    </xsl:call-template>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="doc-comment">
    <xsl:param name="comment"/>
    <xsl:param name="indent"/>
    <xsl:if test="$comment">
      <xsl:variable name="head" select="substring-before ($comment, '&#10;')"/>
      <xsl:variable name="line">
	<xsl:choose>
	  <xsl:when test="$head">
	    <xsl:value-of select="$head"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$comment"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      
      <xsl:call-template name="indented">
	<xsl:with-param name="indent" select="$indent"/>
	<xsl:with-param name="text">
	  <xsl:text>## </xsl:text>
	  <xsl:value-of select="$line"/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:text>&#10;</xsl:text>
      <xsl:call-template name="doc-comment">
	<xsl:with-param name="comment"
	  select="substring-after ($comment, '&#10;')"/>
	<xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="rng:grammar/a:documentation | rng:div/a:documentation |
    rng:include/a:documentation" priority="-15">
    <xsl:param name="indent"/>
    <xsl:if test="not (../../..) or 
      preceding-sibling::*[not (self::a:documentation)]">
      <xsl:call-template name="annotation-element">
	<xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template match="rng:grammar/* | rng:div/* | rng:include/*"
    priority="-20">
    <xsl:param name="indent"/>
    <xsl:call-template name="annotation-element">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="follow-annotations">
    <xsl:param name="indent"/>
    <xsl:param name="nd"
      select="self::*[not (self::rng:value or self::rng:param)]
      /following-sibling::*[1][not (self::rng:*)]"/>
    <xsl:if test="$nd">
      <xsl:for-each select="$nd">
	<xsl:text>&#10;</xsl:text>
	<xsl:call-template name="follow-annotation">
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
	<xsl:call-template name="follow-annotations">
	  <xsl:with-param name="indent" select="$indent"/>
	</xsl:call-template>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xsl:template match="*" priority="-100" name="follow-annotation">
    <xsl:param name="indent"/>
    <xsl:if test="preceding-sibling::rng:*">
      <xsl:call-template name="indented">
	<xsl:with-param name="indent" select="$indent"/>
	<xsl:with-param name="text" select="'>>'"/>
      </xsl:call-template>
      <xsl:text>&#10;</xsl:text>
      <xsl:call-template name="annotation-element">
	<xsl:with-param name="indent" select="$indent"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="annotation-name">
    <xsl:variable name="ns" select="namespace-uri (.)"/>
    <xsl:if test="$ns">
      <xsl:choose>
	<xsl:when test="$retain-prefixes">
	  <xsl:variable name="mapped"
	    select="$prefix-map [string (.) = $ns][1]"/>
	  <xsl:choose>
	    <xsl:when test="$mapped">
	      <xsl:value-of select="$mapped/@prefix"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:variable name="pfx"
		select="name (key ('ns', $ns)[1]/namespace::*[. = $ns])"/>
	      <xsl:choose>
		<xsl:when test="$pfx">
		  <xsl:value-of select="$pfx"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="generate-id (key ('ns', $ns)[1])"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:when>

	<xsl:otherwise>
	  <xsl:value-of select="generate-id (key ('annot-ns', $ns)[1])"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:text>:</xsl:text>
    </xsl:if>
    <xsl:value-of select="local-name (.)"/>
  </xsl:template>

  <xsl:template name="annotation-attributes">
    <xsl:param name="indent"/>
    <xsl:for-each select="@*[namespace-uri () != '']">
      <xsl:if test="position () != 1"><xsl:text>&#10;</xsl:text></xsl:if>
      <xsl:call-template name="indented">
	<xsl:with-param name="indent" select="$indent"/>
	<xsl:with-param name="text">
	  <xsl:call-template name="annotation-name"/>
	  <xsl:text> = </xsl:text>
	  <xsl:call-template name="make-string-literal"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="*" name="annotation-element" mode="annot">
    <xsl:param name="indent"/>
    <xsl:call-template name="make-line">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="body">
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">
	    <xsl:call-template name="annotation-name"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">[</xsl:with-param>
	</xsl:call-template>
	<xsl:text>&#10;</xsl:text>

	<xsl:for-each select="@*">
	  <xsl:if test="position () != 1"><xsl:text>&#10;</xsl:text></xsl:if>
	  <xsl:call-template name="indented">
	    <xsl:with-param name="indent" select="$indent + 1"/>
	    <xsl:with-param name="text">
	      <xsl:call-template name="annotation-name"/>
	      <xsl:text> = </xsl:text>
	      <xsl:call-template name="make-string-literal"/>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:for-each>

	<xsl:apply-templates mode="annot">
	  <xsl:with-param name="indent" select="$indent + 1"/>
	</xsl:apply-templates>
	<xsl:text>&#10;</xsl:text>
	<xsl:call-template name="indented">
	  <xsl:with-param name="indent" select="$indent"/>
	  <xsl:with-param name="text">]</xsl:with-param>
	</xsl:call-template>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:if test="following-sibling::*">
      <xsl:text>&#10;</xsl:text>
      <xsl:if test="parent::rng:grammar or parent::rng:div or 
	parent::rng:include">
	<xsl:text>&#10;</xsl:text>
      </xsl:if>
    </xsl:if>
  </xsl:template>

  <xsl:template match="text ()" mode="annot">
    <xsl:param name="indent"/>
    <xsl:call-template name="indented">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="text">
	<xsl:call-template name="make-string-literal"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

</xsl:transform>

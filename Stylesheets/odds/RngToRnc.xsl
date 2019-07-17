<?xml version="1.0" encoding="utf-8"?>
<!--*- XML -*-->
<!-- adapted by Sebastian Rahtz from: -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- $Id$ -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- 
  modified 2010-08-14 by Syd Bauman:
     bug-fix: modified expressions defining 'pfx' variables
     so that only 1 namespace is handed to name().
-->
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

     RngToRncText.xsl converts a RELAX NG schema in XML syntax to the
     compact syntax.

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

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<xsl:transform xmlns:rng="http://relaxng.org/ns/structure/1.0"
               xmlns:sch="http://purl.oclc.org/dsdl/schematron"
               xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
               xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
               version="2.0"
               exclude-result-prefixes="rng sch a">
  <xsl:param name="top"/>
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- Parameters -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- collapse-lines:
     If true, output constructs spanning multiple lines will be
     groupd into a single line unless it exceeds $line-width chars. -->
  <xsl:param name="collapse-lines" select="true ()"/>
   <!-- indent-width:
     The number of characters to indent at each indentation level -->
  <xsl:param name="indent-width" select="3"/>
   <!-- line-width:
     see the group-lines parameter. -->
  <xsl:param name="line-width" select="80"/>
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

     RngToRncXml.xsl converts a RELAX NG schema in XML syntax to an
     XML representation of the compact syntax.

     The output is an intermediate format suitable for further
     transformation into, for example, plain text or HTML.

     The default namespace declaration is chosen from the first
     (in document order) occurrence of an ns attribute, unless
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

     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- Parameters -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- rewrite-suffix:
     If true and the value of an href attribute ends with '.rng', the
     suffix will be replace by the string '.rnc'. -->
  <xsl:param name="rewrite-suffix" select="true ()"/>
   <!-- retain-prefixes:
     If true, namespace declarations will use prefixes from the source
     document if possible. (Doesn't work with MSXML 4.0 or Xalan-J 2.4.0) -->
  <xsl:param name="retain-prefixes">true</xsl:param>

   <!-- default-ns:
     The uri of the default namespace. There must be at least one
     ns attribute in the schema containing this uri. -->
  <xsl:param name="default-ns" select="string (/descendant::rng:*[@ns][1]/@ns)"/>
   <!-- prefixes:
    A space separated list of prefix mappings: prefix '=' namespace-uri.
    Note: Since space is used as a delimiter, no space is allowed 
    immediately before or after the equal sign. Example: 

    "foo=http://example.org/ns/foo bar=http://example.org/ns/bar"

    The prefixes will be used whenever a prefix is needed for the
    corresponding namespace URI. Prefixes declared in this
    parameter has higher precedence than the prefixes declared
    in the prefix-map parameter and those found in the schema.
  -->
  <xsl:param name="prefixes">tei=http://www.tei-c.org/ns/1.0 teix=http://www.tei-c.org/ns/Examples rng=http://relaxng.org/ns/structure/1.0</xsl:param>
   <!-- prefix-map:
     A node set containing element nodes like:

     <ns prefix="x">http://example.org/x</ns>

     The name of the element is not significant. Prefixes from this
     map will be used in favor of generated and the ones found in the
     source schema. A prefix must not be the empty string. Also, it
     must not conflict with any other prefixes in the map or in the
     schema. -->
  <xsl:param name="prefix-map" select="()"/>
   <!-- recursive:
     If true, recursively process includes and external
     references. Note: no include logic is applied. The external
     schemas are simply structurally included, wrapped in an <external>
     element. -->
  <xsl:param name="recursive" select="false ()"/>
  <xsl:template name="str">
      <xsl:param name="text"/>
      <xsl:param name="quote" select="'&#34;'"/>
      <xsl:param name="is-multiline" select="false ()"/>
      <xsl:variable name="lit" select="concat ($quote, $text, $quote)"/>
      <str size="{string-length ($lit)}">
         <xsl:if test="$is-multiline">
            <xsl:attribute name="multiline">yes</xsl:attribute>
         </xsl:if>
         <xsl:value-of select="$lit"/>
      </str>
  </xsl:template>
  <xsl:template name="text">
      <xsl:param name="text"/>
      <xsl:attribute name="size">
         <xsl:value-of select="string-length ($text)"/>
      </xsl:attribute>
      <xsl:value-of select="$text"/>
  </xsl:template>
   <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <xsl:key name="prefix"
            match="rng:element/@name [contains (., ':')] | rng:attribute/@name [contains (., ':')]"
            use="substring-before (., ':')"/>
  <xsl:key name="ns" match="*" use="namespace::*"/>
  <xsl:key name="annot-ns" match="*[not (self::rng:*)]" use="namespace-uri (.)"/>
  <xsl:key name="annot-ns" match="@*[namespace-uri (.) != '']" use="namespace-uri (.)"/>
  <xsl:output method="xml" indent="yes"/>
  <xsl:strip-space elements="rng:*"/>
  <xsl:preserve-space elements="rng:value rng:param"/>
  <xsl:variable name="xsd" select="'http://www.w3.org/2001/XMLSchema-datatypes'"/>
  <xsl:variable name="default-ns-nd" select="/descendant::rng:*[@ns = $default-ns][1]/@ns"/>
  <xsl:variable name="default-ns-id-rtf">
      <xsl:call-template name="get-prefix">
         <xsl:with-param name="nd" select="$default-ns-nd"/>
      </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="default-ns-id" select="string ($default-ns-id-rtf)"/>
  <xsl:variable name="has-default-ns" select="boolean ($default-ns-nd)"/>
  <xsl:variable name="has-local" select="not (key ('prefix', 'local'))"/>
  <xsl:template name="make-compact-schema">
      <compact-schema>
<!-- Declarations -->
      <xsl:if test="$has-default-ns">
            <xsl:call-template name="make-ns-declaration">
               <xsl:with-param name="is-default" select="true()"/>
               <xsl:with-param name="prefix" select="$default-ns-id"/>
               <xsl:with-param name="uri" select="$default-ns"/>
            </xsl:call-template>
         </xsl:if>
         <xsl:if test="$has-local">
            <xsl:call-template name="make-ns-declaration">
               <xsl:with-param name="prefix" select="'local'"/>
               <xsl:with-param name="uri" select="''"/>
            </xsl:call-template>
         </xsl:if>
         <xsl:call-template name="inhnamespace"/>
         <!--
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
		//*[namespace-uri (.) != 
		'http://relaxng.org/ns/structure/1.0']//
		*[namespace-uri (.) = $ns or @*[namespace-uri (.) = $ns]] or
		key ('prefix', name (.)))">
		<xsl:call-template name="make-ns-declaration">
		  <xsl:with-param name="prefix">
		    <xsl:variable name="mapped">
		      <xsl:call-template name="mapped-prefix">
			<xsl:with-param name="ns" select="string (.)"/>
		      </xsl:call-template>
		    </xsl:variable>
		    <xsl:choose>
		      <xsl:when test="string ($mapped)">
			<xsl:value-of select="$mapped"/>
		      </xsl:when>
		      <xsl:when test="name (.)">
			<xsl:value-of select="name (.)"/>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:value-of select="generate-id ($elm)"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:with-param>
		  <xsl:with-param name="uri" select="."/>
		</xsl:call-template>
	      </xsl:if>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:when>

	<xsl:otherwise>
	  <xsl:for-each select=".//rng:element | .//rng:attribute">
	    <xsl:if test="@name [contains (., ':')]">
	      <xsl:variable name="pfx" select="substring-before (@name, ':')"/>
	      <xsl:if test="count (key ('prefix', $pfx)[1] | @name) = 1">
		<xsl:variable name="ns"
		  select="string (namespace::*[name () = $pfx])"/>
		<xsl:call-template name="make-ns-declaration">
		  <xsl:with-param name="prefix">
		    <xsl:variable name="mapped">
		      <xsl:call-template name="mapped-prefix">
			<xsl:with-param name="ns" select="$ns"/>
		      </xsl:call-template>
		    </xsl:variable>
		    <xsl:choose>
		      <xsl:when test="string ($mapped)">
			<xsl:value-of select="$mapped"/>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:value-of select="$pfx"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:with-param>
		  <xsl:with-param name="uri" select="$ns"/>
		</xsl:call-template>
	      </xsl:if>
	    </xsl:if>
	  </xsl:for-each>

	  <xsl:for-each select="//*[not (self::rng:*)] | 
	                        //*/@*[namespace-uri (.) != '']">
	    <xsl:variable name="ns" select="namespace-uri (.)"/>
	    <xsl:if test="count (key ('annot-ns', $ns)[1] | .) = 1">
	      <xsl:call-template name="make-ns-declaration">
		<xsl:with-param name="prefix">
		  <xsl:variable name="mapped">
		    <xsl:call-template name="mapped-prefix">
		      <xsl:with-param name="ns" select="$ns"/>
		    </xsl:call-template>
		  </xsl:variable>
		  <xsl:choose>
		    <xsl:when test="string ($mapped)">
		      <xsl:value-of select="$mapped"/>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:value-of select="generate-id (.)"/>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:with-param>
		<xsl:with-param name="uri" select="$ns"/>
	      </xsl:call-template>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:otherwise>
      </xsl:choose>
      
      <xsl:for-each select="//@datatypeLibrary">
	<xsl:if test=". != $xsd and . != ''">
	  <declaration size="9">datatypes</declaration>
	  <sp size="1"/>
	  <prefix>
	    <xsl:call-template name="text">
	      <xsl:with-param name="text" select="generate-id (.)"/>
	    </xsl:call-template>
	  </prefix>
	  <t size="3"> = </t>
	  <xsl:call-template name="str">
	    <xsl:with-param name="text" select="."/>
	  </xsl:call-template>
	  <nl size="1"/>
	</xsl:if>
      </xsl:for-each>
-->
      <nl size="1"/>
         <!-- Pattern -->
      <xsl:apply-templates mode="RNC"/>
      </compact-schema>
  </xsl:template>
  <xsl:template match="/rng:grammar">
      <grammar>
         <xsl:apply-templates mode="RNC"/>
      </grammar>
  </xsl:template>
  <xsl:template name="make-block">
      <xsl:param name="head"/>
      <xsl:param name="body"/>
      <xsl:param name="collapse" select="true ()"/>
      <group>
         <xsl:if test="not ($collapse)">
            <xsl:attribute name="collapse">no</xsl:attribute>
         </xsl:if>
         <xsl:copy-of select="$head"/>
         <nl size="1"/>
         <t size="1">{</t>
         <indent>
            <nl size="1"/>
            <xsl:copy-of select="$body"/>
         </indent>
         <nl size="1"/>
         <t size="1">}</t>
      </group>
  </xsl:template>
  <xsl:template name="make-parenthesis">
      <xsl:param name="body"/>
      <t size="1">(</t>
      <indent>
         <nl size="1"/>
         <xsl:copy-of select="$body"/>
      </indent>
      <nl size="1"/>
      <t size="1">)</t>
  </xsl:template>
  <xsl:template match="rng:grammar" mode="RNC">
      <grammar>
         <group>
            <xsl:call-template name="annotations"/>
            <xsl:call-template name="make-block">
               <xsl:with-param name="collapse" select="false ()"/>
               <xsl:with-param name="head">
                  <keyword size="7">grammar</keyword>
               </xsl:with-param>
               <xsl:with-param name="body">
                  <xsl:apply-templates mode="RNC"/>
               </xsl:with-param>
            </xsl:call-template>
         </group>
      </grammar>
  </xsl:template>
  <xsl:template match="rng:list | rng:mixed" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <xsl:call-template name="make-block">
            <xsl:with-param name="head">
               <keyword>
                  <xsl:call-template name="text">
                     <xsl:with-param name="text" select="local-name (.)"/>
                  </xsl:call-template>
               </keyword>
            </xsl:with-param>
            <xsl:with-param name="body">
               <xsl:call-template name="group-body"/>
            </xsl:with-param>
         </xsl:call-template>
      </group>
  </xsl:template>
  <xsl:template match="rng:start" mode="RNC">
      <xsl:call-template name="annotations"/>
      <group>
         <define size="5">
            <xsl:copy-of select="@combine"/>
            <xsl:text>start</xsl:text>
         </define>
         <xsl:call-template name="assignment-op"/>
         <indent>
            <nl size="1"/>
            <xsl:call-template name="group-body"/>
         </indent>
      </group>
      <xsl:call-template name="spacer"/>
  </xsl:template>
  <xsl:template name="assignment-op">
      <xsl:choose>
         <xsl:when test="@combine = 'choice'">
            <t size="3"> |=</t>
         </xsl:when>
         <xsl:when test="@combine = 'interleave'">
            <t size="3"> &amp;=</t>
         </xsl:when>
         <xsl:otherwise>
            <t size="2"> =</t>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:define" mode="RNC">
      <xsl:variable name="grammar" select="ancestor::rng:grammar [1]"/>
      <xsl:call-template name="annotations"/>
      <group>
         <define name="{@name}">
            <xsl:copy-of select="@combine"/>
            <xsl:call-template name="text">
               <xsl:with-param name="text">
                  <xsl:call-template name="quote-keyword">
                     <xsl:with-param name="name" select="@name"/>
                  </xsl:call-template>
               </xsl:with-param>
            </xsl:call-template>
         </define>
         <xsl:call-template name="assignment-op"/>
         <indent>
            <nl size="1"/>
            <xsl:call-template name="group-body"/>
         </indent>
      </group>
      <xsl:call-template name="spacer"/>
  </xsl:template>
  <xsl:template match="rng:div" mode="RNC">
      <xsl:call-template name="annotations"/>
      <xsl:call-template name="make-block">
         <xsl:with-param name="collapse" select="false ()"/>
         <xsl:with-param name="head">
            <keyword size="3">div</keyword>
         </xsl:with-param>
         <xsl:with-param name="body">
            <xsl:apply-templates mode="RNC"/>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="spacer"/>
  </xsl:template>
  <xsl:template match="rng:include" mode="RNC">
      <xsl:call-template name="annotations"/>
      <group collapse="no">
         <keyword size="7">include</keyword>
         <sp size="1"/>
         <include-href ref="{generate-id ()}">
            <xsl:call-template name="external"/>
         </include-href>
         <xsl:if test="*">
            <nl size="1"/>
            <t size="1">{</t>
            <indent>
               <nl size="1"/>
               <xsl:apply-templates mode="RNC"/>
            </indent>
            <nl size="1"/>
            <t size="1">}</t>
         </xsl:if>
      </group>
      <xsl:call-template name="spacer"/>
      <xsl:if test="$recursive">
         <include href="{@href}" id="{generate-id ()}">
            <xsl:for-each select="document (@href)/rng:grammar">
               <xsl:call-template name="make-compact-schema"/>
            </xsl:for-each>
         </include>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:externalRef" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <keyword size="8">external</keyword>
         <sp size="1"/>
         <external-href ref="{generate-id ()}">
            <xsl:call-template name="external"/>
         </external-href>
      </group>
      <xsl:if test="$recursive">
         <external href="{@href}" id="{generate-id ()}">
            <xsl:for-each select="document (@href)">
               <xsl:call-template name="make-compact-schema"/>
            </xsl:for-each>
         </external>
      </xsl:if>
  </xsl:template>
  <xsl:template name="make-inherit">
      <xsl:param name="prefix"/>
      <sp size="1"/>
      <keyword size="7">inherit</keyword>
      <t size="3"> = </t>
      <prefix>
         <xsl:call-template name="text">
            <xsl:with-param name="text" select="$prefix"/>
         </xsl:call-template>
      </prefix>
  </xsl:template>
  <xsl:template name="rewrite-suffix">
      <xsl:param name="href" select="@href"/>
      <xsl:variable name="suffix" select="substring ($href, (string-length ($href) - 4) + 1, 4)"/>
      <xsl:choose>
         <xsl:when test="$rewrite-suffix and $suffix = '.rng'">
            <xsl:value-of select="substring ($href, 1, string-length ($href) - 4)"/>
            <xsl:text>.rnc</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$href"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="external">
      <xsl:call-template name="str">
         <xsl:with-param name="text">
            <xsl:call-template name="rewrite-suffix"/>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:variable name="ns" select="ancestor-or-self::*[@ns][1]/@ns"/>
      <xsl:choose>
         <xsl:when test="$has-default-ns and $ns = $default-ns"/>
         <xsl:when test="$has-local and $ns = ''">
            <xsl:call-template name="make-inherit">
               <xsl:with-param name="prefix">local</xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="not($ns='')">
            <xsl:call-template name="make-inherit">
               <xsl:with-param name="prefix">
                  <xsl:call-template name="get-prefix">
                     <xsl:with-param name="nd" select="$ns"/>
                  </xsl:call-template>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$has-default-ns">
            <xsl:call-template name="make-inherit">
               <xsl:with-param name="prefix">inh</xsl:with-param>
            </xsl:call-template>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:ref" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <ref name="{@name}">
            <xsl:call-template name="text">
               <xsl:with-param name="text">
                  <xsl:call-template name="quote-keyword">
                     <xsl:with-param name="name" select="@name"/>
                  </xsl:call-template>
               </xsl:with-param>
            </xsl:call-template>
         </ref>
      </group>
  </xsl:template>
  <xsl:template match="rng:parentRef" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <keyword size="6">parent</keyword>
         <sp size="1"/>
         <parent-ref name="{@name}">
            <xsl:call-template name="text">
               <xsl:with-param name="text">
                  <xsl:call-template name="quote-keyword">
                     <xsl:with-param name="name" select="@name"/>
                  </xsl:call-template>
               </xsl:with-param>
            </xsl:call-template>
         </parent-ref>
      </group>
  </xsl:template>
  <xsl:template match="rng:element | rng:attribute" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <group>
            <xsl:variable name="head">
               <keyword>
                  <xsl:call-template name="text">
                     <xsl:with-param name="text" select="local-name (.)"/>
                  </xsl:call-template>
               </keyword>
               <sp size="1"/>
               <xsl:call-template name="name-class">
                  <xsl:with-param name="is-attr" select="boolean (self::rng:attribute)"/>
               </xsl:call-template>
            </xsl:variable>
            <xsl:choose>
               <xsl:when test="not (rng:*) or (count (rng:*) = 1 and not (@name))">
                  <xsl:copy-of select="$head"/>
                  <t size="3"> { </t>
                  <atom size="4">text</atom>
                  <t size="2"> }</t>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:call-template name="make-block">
                     <xsl:with-param name="head">
                        <xsl:copy-of select="$head"/>
                     </xsl:with-param>
                     <xsl:with-param name="body">
                        <xsl:choose>
                           <xsl:when test="@name">
                              <xsl:call-template name="group-body"/>
                           </xsl:when>
                           <xsl:otherwise>
                              <xsl:call-template name="group-body">
                                 <xsl:with-param name="patterns" select="rng:*[1]/following-sibling::rng:*"/>
                              </xsl:call-template>
                           </xsl:otherwise>
                        </xsl:choose>
                     </xsl:with-param>
                  </xsl:call-template>
               </xsl:otherwise>
            </xsl:choose>
         </group>
      </group>
  </xsl:template>
  <xsl:template name="expression">
      <xsl:param name="operator"/>
      <xsl:param name="is-prefix-operator" select="false ()"/>
      <group>
         <xsl:call-template name="annotations"/>
         <group>
            <xsl:variable name="need-paren-rtf">
               <xsl:call-template name="need-paren"/>
            </xsl:variable>
            <xsl:variable name="need-paren" select="boolean (string ($need-paren-rtf))"/>
            <xsl:choose>
               <xsl:when test="$need-paren">
                  <xsl:call-template name="make-parenthesis">
                     <xsl:with-param name="body">
                        <xsl:call-template name="expression-body">
                           <xsl:with-param name="operator" select="$operator"/>
                           <xsl:with-param name="is-prefix-operator" select="$is-prefix-operator"/>
                        </xsl:call-template>
                     </xsl:with-param>
                  </xsl:call-template>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:call-template name="expression-body">
                     <xsl:with-param name="operator" select="$operator"/>
                     <xsl:with-param name="is-prefix-operator" select="$is-prefix-operator"/>
                  </xsl:call-template>
               </xsl:otherwise>
            </xsl:choose>
         </group>
      </group>
  </xsl:template>
  <xsl:template match="rng:group" mode="RNC">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="','"/>
      </xsl:call-template>
  </xsl:template>
  <xsl:template match="rng:choice" mode="RNC" name="make-choice">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="'| '"/>
         <xsl:with-param name="is-prefix-operator" select="true ()"/>
      </xsl:call-template>
  </xsl:template>
  <xsl:template match="rng:interleave" mode="RNC">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="'&amp; '"/>
         <xsl:with-param name="is-prefix-operator" select="true ()"/>
      </xsl:call-template>
  </xsl:template>
  <xsl:template match="rng:optional" mode="RNC">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="','"/>
      </xsl:call-template>
      <t size="1">?</t>
  </xsl:template>
  <xsl:template match="rng:zeroOrMore" mode="RNC">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="','"/>
      </xsl:call-template>
      <t size="1">*</t>
  </xsl:template>
  <xsl:template match="rng:oneOrMore" mode="RNC">
      <xsl:call-template name="expression">
         <xsl:with-param name="operator" select="','"/>
      </xsl:call-template>
      <t size="1">+</t>
  </xsl:template>
  <xsl:template match="rng:text" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <atom size="4">text</atom>
      </group>
  </xsl:template>
  <xsl:template match="rng:empty" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <atom size="5">empty</atom>
      </group>
  </xsl:template>
  <xsl:template match="rng:notAllowed" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <atom size="10">notAllowed</atom>
      </group>
  </xsl:template>
  <xsl:template match="rng:data" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <group>
            <xsl:call-template name="type-name"/>
            <xsl:if test="rng:param">
               <nl size="1"/>
               <t size="1">{</t>
               <indent>
                  <xsl:for-each select="rng:param">
                     <nl size="1"/>
                     <group>
                        <xsl:call-template name="annotations"/>
                        <param>
                           <xsl:call-template name="text">
                              <xsl:with-param name="text" select="@name"/>
                           </xsl:call-template>
                        </param>
                        <t size="3"> = </t>
                        <xsl:call-template name="make-string-literal"/>
                     </group>
                  </xsl:for-each>
               </indent>
               <nl size="1"/>
               <t size="1">}</t>
            </xsl:if>
            <xsl:for-each select="rng:except">
               <nl size="1"/>
               <t size="1">-</t>
               <nl size="1"/>
               <xsl:choose>
                  <xsl:when test="count (rng:*) &gt; 1">
                     <xsl:call-template name="make-choice"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:apply-templates select="*" mode="RNC"/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:for-each>
         </group>
      </group>
  </xsl:template>
  <xsl:template match="rng:value" mode="RNC">
      <group>
         <xsl:call-template name="annotations"/>
         <xsl:variable name="type">
            <xsl:call-template name="type-name"/>
         </xsl:variable>
         <xsl:if test="$type != 'token'">
            <xsl:copy-of select="$type"/>
            <sp size="1"/>
         </xsl:if>
         <xsl:call-template name="make-string-literal"/>
      </group>
  </xsl:template>
  <xsl:template name="quote-quotes">
      <xsl:param name="str"/>
      <xsl:param name="pos" select="1"/>
      <xsl:variable name="c" select="substring ($str, $pos, 1)"/>
      <xsl:if test="$c">
         <xsl:choose>
            <xsl:when test="$c = '&#34;'">" '"' "</xsl:when>
            <xsl:when test="$c = '&#xA;'">\x{A}</xsl:when>
            <xsl:when test="$c = '&#xD;'">\x{D}</xsl:when>
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
         <xsl:when test="not (contains (., '&#34;') or   contains (., &#34;'&#34;) or   contains (., '&#xA;') or contains (., '&#xD;'))">
            <xsl:call-template name="str">
               <xsl:with-param name="text" select="."/>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="not (contains (., '&#34;&#34;&#34;'))">
            <xsl:call-template name="str">
               <xsl:with-param name="quote" select="'&#34;&#34;&#34;'"/>
               <xsl:with-param name="text" select="."/>
               <xsl:with-param name="is-multiline" select="true ()"/>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="not (contains (., &#34;'''&#34;))">
            <xsl:call-template name="str">
               <xsl:with-param name="quote">'''</xsl:with-param>
               <xsl:with-param name="text" select="."/>
               <xsl:with-param name="is-multiline" select="true ()"/>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="str">
               <xsl:with-param name="text">
                  <xsl:call-template name="quote-quotes">
                     <xsl:with-param name="str" select="."/>
                  </xsl:call-template>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="type-name">
      <xsl:variable name="dt"
                    select="ancestor-or-self::rng:*[@datatypeLibrary][1]/@datatypeLibrary"/>
      <xsl:variable name="dts" select="string ($dt)"/>
      <type>
         <xsl:call-template name="text">
            <xsl:with-param name="text">
               <xsl:choose>
                  <xsl:when test="self::rng:value and not (@type)">token</xsl:when>
                  <xsl:when test="$dts = '' and @type= 'string'">string</xsl:when>
                  <xsl:when test="$dts = '' and @type= 'token'">token</xsl:when>
                  <xsl:when test="$dts = ''">
                     <xsl:text>xsd:</xsl:text>
                     <xsl:value-of select="@type"/>
                  </xsl:when>
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
            </xsl:with-param>
         </xsl:call-template>
      </type>
  </xsl:template>
  <xsl:template name="need-paren">
      <xsl:choose>
         <xsl:when test="self::rng:optional or  self::rng:zeroOrMore or  self::rng:oneOrMore">
            <xsl:if test="count (rng:*) &gt; 1">true</xsl:if>
         </xsl:when>
         <xsl:when test="parent::rng:element[not (@name)] or   parent::rng:attribute [not (@name)]">
            <xsl:if test="count (../rng:*) &gt; 2">true</xsl:if>
         </xsl:when>
         <xsl:when test="count (../rng:*) &gt; 1">true</xsl:when>
         <xsl:when test="count (rng:*) &gt; 1 and (parent::rng:optional or   parent::rng:zeroOrMore or parent::rng:oneOrMore)">true</xsl:when>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="expression-body">
      <xsl:param name="patterns" select="rng:*"/>
      <xsl:param name="operator"/>
      <xsl:param name="is-prefix-operator" select="false ()"/>
      <xsl:choose>
         <xsl:when test="$is-prefix-operator">
            <xsl:for-each select="$patterns">
               <xsl:if test="position () != 1">
                  <nl size="1"/>
                  <op size="{string-length ($operator)}">
                     <xsl:value-of select="$operator"/>
                  </op>
               </xsl:if>
               <xsl:apply-templates select="." mode="RNC"/>
               <xsl:call-template name="follow-annotations"/>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="$patterns">
               <xsl:if test="position () != 1">
                  <op size="{string-length ($operator)}">
                     <xsl:value-of select="$operator"/>
                  </op>
                  <nl size="1"/>
               </xsl:if>
               <xsl:apply-templates select="." mode="RNC"/>
               <xsl:call-template name="follow-annotations"/>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="group-body">
      <xsl:param name="patterns" select="rng:*"/>
      <xsl:call-template name="expression-body">
         <xsl:with-param name="patterns" select="$patterns"/>
         <xsl:with-param name="operator" select="','"/>
      </xsl:call-template>
  </xsl:template>
   <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- Name class -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <xsl:template name="name-class">
      <xsl:param name="is-attr" select="false ()"/>
      <nc>
         <xsl:call-template name="text">
            <xsl:with-param name="text">
               <xsl:choose>
                  <xsl:when test="@name">
                     <xsl:choose>
                        <xsl:when test="not ($is-attr) and not (contains (@name, ':'))">
                           <xsl:call-template name="make-name-nc">
                              <xsl:with-param name="name" select="@name"/>
                              <xsl:with-param name="is-attr" select="false ()"/>
                           </xsl:call-template>
                        </xsl:when>
                        <xsl:when test="@ns">
                           <xsl:text>[</xsl:text>
                           <xsl:value-of select="@ns"/>
                           <xsl:text>]</xsl:text>
                           <xsl:value-of select="@name"/>
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
            </xsl:with-param>
         </xsl:call-template>
      </nc>
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
         <xsl:when test="$has-default-ns and $ns = $default-ns">
            <xsl:value-of select="$default-ns-id"/>
         </xsl:when>
         <xsl:when test="$has-local and $ns = ''">local</xsl:when>
         <xsl:when test="not($ns='')">
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
            <xsl:when test="count (rng:*) &gt; 1">
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
  <xsl:template match="rng:attribute/rng:choice | rng:element/rng:choice" mode="name-class"
                 name="make-nc-choice">
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
         <xsl:when test="not ($is-attr) and $has-default-ns and  $ns = $default-ns"/>
         <xsl:when test="$is-attr and $ns = ''"/>
         <xsl:when test="$is-attr and not($default-ns-id='') and $has-default-ns and $ns = $default-ns">
            <xsl:value-of select="$default-ns-id"/>
            <xsl:text>:</xsl:text>
         </xsl:when>
         <xsl:when test="string-length($ns)&gt;0">
            <xsl:call-template name="get-prefix">
               <xsl:with-param name="nd" select="$ns"/>
            </xsl:call-template>
            <xsl:text>:</xsl:text>
         </xsl:when>
         <!--
      <xsl:when test="$is-attr or $has-default-ns">
        <xsl:text>inh:</xsl:text>
      </xsl:when>
-->
    </xsl:choose>
      <xsl:value-of select="$name"/>
  </xsl:template>
   <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- Util -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <xsl:template name="last-token">
      <xsl:param name="str"/>
      <xsl:param name="pos" select="string-length ($str)"/>
      <xsl:choose>
         <xsl:when test="$pos = 0">
            <xsl:value-of select="$str"/>
         </xsl:when>
         <xsl:when test="substring ($str, $pos, 1) = ' '">
            <xsl:value-of select="substring ($str, $pos + 1)"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="last-token">
               <xsl:with-param name="str" select="$str"/>
               <xsl:with-param name="pos" select="$pos - 1"/>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:variable name="simple-prefix-map" select="concat (' ', $prefixes, ' ')"/>
  <xsl:template name="mapped-prefix">
      <xsl:param name="ns"/>
      <xsl:variable name="simple-mapped"
                    select="substring-before ($simple-prefix-map, concat ('=', $ns, ' '))"/>
      <xsl:choose>
         <xsl:when test="$simple-mapped">
            <xsl:call-template name="last-token">
               <xsl:with-param name="str" select="$simple-mapped"/>
            </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="mapped" select="$prefix-map [string (.) = $ns][1]"/>
            <xsl:if test="$mapped">
               <xsl:value-of select="$mapped/@prefix"/>
            </xsl:if>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="get-prefix">
      <xsl:param name="nd" select="."/>
      <xsl:variable name="ns" select="string ($nd)"/>
      <xsl:variable name="mapped">
         <xsl:call-template name="mapped-prefix">
            <xsl:with-param name="ns" select="$ns"/>
         </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="string ($mapped)">
            <xsl:value-of select="$mapped"/>
         </xsl:when>
         <xsl:when test="not ($retain-prefixes)">
            <xsl:value-of select="generate-id ($nd)"/>
         </xsl:when>
         <xsl:when test="key ('ns', $ns)">
           <xsl:variable name="pfx" select="name ( (key ('ns', $ns)/namespace::*[. = $ns])[1] )"/>
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
         <nl size="1"/>
         <xsl:if test="not (parent::rng:include) and  not (parent::rng:grammar/../..) and   not (self::rng:include and   following-sibling::*[1][self::rng:include])">
            <nl size="1"/>
         </xsl:if>
      </xsl:if>
  </xsl:template>
  <keywords xmlns="http://www.example.com">
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
  </keywords>
  <xsl:variable xmlns:foo="http://www.example.com" name="keywords"  select="document ('')/*/foo:keywords/*"/>
  <xsl:template name="quote-keyword">
      <xsl:param name="name"/>
      <xsl:if test="$name = $keywords">\</xsl:if>
      <xsl:value-of select="$name"/>
  </xsl:template>
   <!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
<!-- Annotations -->
<!-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ -->
  <xsl:template name="leading-documentation">
      <xsl:param name="nd"/>
      <!--
    <xsl:choose>
      <xsl:when test="not ($nd) or $nd/self::rng:*"/>

      <xsl:when test="$nd/self::a:documentation">
	<xsl:call-template name="doc-comment">
	  <xsl:with-param name="comment" select="string ($nd)"/>
	</xsl:call-template>
	<nl size="1"/>
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="nd" select="$nd/following-sibling::*[1]"/>
	</xsl:call-template>
      </xsl:when>

      <xsl:otherwise>
	<group>
	  <t size="1">[</t>
	  <indent>
	    <xsl:call-template name="annotation-attributes"/>

	    <xsl:call-template name="leading-annotations">
	      <xsl:with-param name="nd" select="$nd"/>
	    </xsl:call-template>
	  </indent>
	  <nl size="1"/>
	  <t size="1">]</t>
	</group>
	<nl size="1"/>
      </xsl:otherwise>
    </xsl:choose>
-->
  </xsl:template>
  <xsl:template name="leading-annotations">
      <xsl:param name="nd"/>
      <xsl:if test="$nd and not ($nd/self::rng:*)">
         <xsl:for-each select="$nd">
            <xsl:call-template name="annotation-element"/>
         </xsl:for-each>
         <xsl:call-template name="leading-annotations">
            <xsl:with-param name="nd" select="$nd/following-sibling::*[1]"/>
         </xsl:call-template>
      </xsl:if>
  </xsl:template>
  <xsl:template name="annotations">
<!--
    <xsl:choose>
      <xsl:when test="(self::rng:value or self::rng:param) and
	following-sibling::*[1][not (self::rng:*)]">
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="nd" select="following-sibling::*[1]"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="*[1][not (self::rng:*)]">
	<xsl:call-template name="leading-documentation">
	  <xsl:with-param name="nd" select="*[1]"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="@*[namespace-uri () != '']">
	<group>
	  <t size="1">[</t>
	  <indent>
	    <xsl:call-template name="annotation-attributes"/>
	  </indent>
	  <nl size="1"/>
	  <t size="1">]</t>
	</group>
	<nl size="1"/>
      </xsl:when>
    </xsl:choose>
-->
  </xsl:template>
  <xsl:template name="doc-comment">
      <xsl:param name="comment"/>
      <!--
    <xsl:if test="$comment">
      <xsl:variable name="head" select="substring-before ($comment, '&#10;')"/>
      <xsl:variable name="line">
	<xsl:text>## </xsl:text>
	<xsl:choose>
	  <xsl:when test="$head">
	    <xsl:value-of select="$head"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$comment"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      
      <doc>
	<xsl:call-template name="text">
	  <xsl:with-param name="text" select="$line"/>
	</xsl:call-template>
      </doc>
      <nl size="1"/>
      <xsl:call-template name="doc-comment">
	<xsl:with-param name="comment"
	  select="substring-after ($comment, '&#10;')"/>
      </xsl:call-template>
    </xsl:if>
-->
  </xsl:template>
  <xsl:template match="rng:grammar/a:documentation | rng:div/a:documentation |     rng:include/a:documentation"
                 priority="-15">
      <xsl:if test="not (../../..) or        preceding-sibling::*[not (self::a:documentation)]">
         <xsl:call-template name="annotation-element"/>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:grammar/* | rng:div/* | rng:include/*" priority="-20">
      <xsl:call-template name="annotation-element"/>
  </xsl:template>
  <xsl:template name="follow-annotations">
      <xsl:param name="nd"
                 select="self::*[not (self::rng:value or self::rng:param)]       /following-sibling::*[1][not (self::rng:*)]"/>
      <xsl:if test="$nd">
         <xsl:for-each select="$nd">
            <nl size="1"/>
            <xsl:call-template name="follow-annotation"/>
            <xsl:call-template name="follow-annotations"/>
         </xsl:for-each>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:*" priority="-100" name="follow-annotation">
      <xsl:if test="preceding-sibling::rng:*">
         <t size="2">&gt;&gt;</t>
         <xsl:call-template name="annotation-element"/>
      </xsl:if>
  </xsl:template>
  <xsl:template name="annotation-name">
      <annot>
         <xsl:call-template name="text">
            <xsl:with-param name="text">
               <xsl:variable name="ns" select="namespace-uri (.)"/>
               <xsl:if test="not($ns='')">
                  <xsl:variable name="mapped">
                     <xsl:call-template name="mapped-prefix">
                        <xsl:with-param name="ns" select="$ns"/>
                     </xsl:call-template>
                  </xsl:variable>
                  <xsl:choose>
                     <xsl:when test="string ($mapped)">
                        <xsl:value-of select="$mapped"/>
                     </xsl:when>
                     <xsl:when test="$retain-prefixes">
                       <xsl:variable name="pfx" select="name ( (key ('ns', $ns)/namespace::*[. = $ns])[1] )"/>
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
                        <xsl:value-of select="generate-id (key ('annot-ns', $ns)[1])"/>
                     </xsl:otherwise>
                  </xsl:choose>
                  <xsl:text>:</xsl:text>
               </xsl:if>
               <xsl:value-of select="local-name (.)"/>
            </xsl:with-param>
         </xsl:call-template>
      </annot>
  </xsl:template>
  <xsl:template name="annotation-attributes">
      <xsl:for-each select="@*[namespace-uri () != '']">
         <nl size="1"/>
         <xsl:call-template name="annotation-name"/>
         <t size="3"> = </t>
         <xsl:call-template name="make-string-literal"/>
      </xsl:for-each>
  </xsl:template>
  <xsl:template match="rng:*" name="annotation-element" mode="annot">
      <nl size="1"/>
      <group>
         <xsl:call-template name="annotation-name"/>
         <nl size="1"/>
         <t size="1">[</t>
         <indent>
            <xsl:for-each select="@*">
               <nl size="1"/>
               <xsl:call-template name="annotation-name"/>
               <t size="3"> = </t>
               <xsl:call-template name="make-string-literal"/>
            </xsl:for-each>
            <xsl:apply-templates mode="annot"/>
         </indent>
         <nl size="1"/>
         <t size="1">]</t>
      </group>
      <xsl:if test="following-sibling::*">
         <xsl:if test="parent::rng:grammar or parent::rng:div or   parent::rng:include">
            <nl size="1"/>
            <xsl:if test="following-sibling::*[1][self::rng:*]">
               <nl size="1"/>
            </xsl:if>
         </xsl:if>
      </xsl:if>
  </xsl:template>
  <xsl:template match="text ()" mode="annot">
      <nl size="1"/>
      <xsl:call-template name="make-string-literal"/>
  </xsl:template>
  <xsl:template name="make-body-from-r-t-f">
      <xsl:param name="schema"/>
      <xsl:for-each select="$schema">
         <xsl:call-template name="make-body"/>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="make-body">
      <xsl:apply-templates mode="keep"/>
  </xsl:template>
  <xsl:template match="group" mode="keep">
      <xsl:choose>
         <xsl:when test="  not ($collapse-lines) or   @collapse = 'no' or  .//doc or   .//group [@collapse = 'no']">
            <xsl:apply-templates mode="keep"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="size" select="sum (.//*/@size)"/>
            <xsl:variable name="level" select="count (ancestor::indent)"/>
            <xsl:choose>
               <xsl:when test="$line-width &gt; ($size + ($level * $indent-width))">
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
  <xsl:variable name="spaces"
                 select="concat (     '                                        ',     '                                        '     )"/>
  <xsl:template match="nl" mode="keep">
      <xsl:text>&#10;</xsl:text>
      <xsl:variable name="level" select="count (ancestor::indent)"/>
      <xsl:variable name="following-op" select="following-sibling::*[1][self::op]"/>
      <xsl:choose>
         <xsl:when test="$following-op">
            <xsl:value-of select="substring ($spaces, 1,     $level * $indent-width - $following-op/@size)"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="substring ($spaces, 1, $level * $indent-width)"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="sp | nl" mode="flatten">
      <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:template match="ref" mode="flatten">
      <xsl:variable name="me">
         <xsl:choose>
            <xsl:when test="contains(@name,'.attributes')">
               <xsl:value-of select="substring-before(@name,'.attributes')"/>
            </xsl:when>
            <xsl:when test="contains(@name,'.content')">
               <xsl:value-of select="substring-before(.,'.content')"/>
            </xsl:when>
            <xsl:when test="contains(@name,'.attribute.')">
               <xsl:value-of select="substring-before(@name,'.attribute.')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@name"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <!--
<xsl:message>FROM <xsl:value-of select="@name"/> to <xsl:value-of
    select="$me"/>, <xsl:for-each select="$top"><xsl:value-of 
select="count(key('IDENTS',$me))"/></xsl:for-each></xsl:message>
-->
    <xsl:variable name="n" select="@name"/>
      <xsl:choose>
         <xsl:when test="contains(.,'.localattributes')">
            <xsl:value-of select="$n"/>
         </xsl:when>
         <xsl:when test="contains(@name,'.content')">
            <xsl:value-of select="$n"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="$top">
               <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name">
                     <xsl:value-of select="$me"/>
                  </xsl:with-param>
                  <xsl:with-param name="reftext">
                     <xsl:value-of select="$n"/>
                  </xsl:with-param>
               </xsl:call-template>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
</xsl:transform>
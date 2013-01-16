<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:fo="http://www.w3.org/1999/XSL/Format" 
    xmlns:html="http://www.w3.org/1999/xhtml" 
    xmlns:i="http://www.iso.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xi="http://www.w3.org/2001/XInclude"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="a fo html i rng s sch tei teix xi xs xsl" 
  version="2.0">
  <xsl:import href="../common2/odds.xsl"/>
  <xsl:import href="oddfunctions.xsl"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for processing TEI ODD markup </p>
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

  <xsl:include href="RngToRnc.xsl"/>
  <xsl:param name="idPrefix"/>
  <xsl:param name="oddmode">tei</xsl:param>
  <xsl:param name="STDOUT">true</xsl:param>
  <xsl:param name="outputSuffix">.html</xsl:param>
  <xsl:param name="selectedSchema"/>
  <xsl:param name="outputDir"/>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="localsource"/>
  <xsl:param name="lang"/>
  <xsl:param name="doclang"/>
  <xsl:param name="patternPrefix"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="autoGlobal">false</xsl:param>
  <xsl:param name="lookupDatabase">false</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:param name="schemaBaseURL">http://localhost/schema/relaxng/</xsl:param>
  <xsl:key match="tei:*" name="LOCALIDENTS" use="@ident"/>
  <xsl:key match="tei:macroSpec" name="MACROS" use="@ident"/>
  <xsl:key match="tei:elementSpec" name="ELEMENTS" use="@ident"/>
  <xsl:key match="tei:elementSpec" name="ELEMENTS" use="tei:altIdent"/>
  <xsl:key match="tei:classSpec" name="CLASSES" use="@ident"/>
  <xsl:key match="rng:ref" name="REFS"  use="@name"/>
  <xsl:key match="rng:ref[contains(@name,'_')]" name="REFS" use="substring-before(@name,'_')"/>

  <xsl:key match="tei:elementSpec/tei:attList/tei:attDef/tei:datatype/rng:ref"
    name="ATTREFS-ELEMENT" use="@name"/>
  <xsl:key match="tei:classSpec/tei:attList/tei:attDef/tei:datatype/rng:ref" name="ATTREFS-CLASS"
    use="@name"/>
  <xsl:key match="tei:elementSpec/tei:attList/tei:attList/tei:attDef/tei:datatype/rng:ref"
    name="ATTREFS-ELEMENT" use="@name"/>
  <xsl:key match="tei:classSpec/tei:attList/tei:attList/tei:attDef/tei:datatype/rng:ref"
    name="ATTREFS-CLASS" use="@name"/>
  <xsl:key match="tei:macroSpec/tei:content//rng:ref" name="MACROREFS"  use="@name"/>

  <xsl:key match="tei:elementSpec|tei:classSpec" name="CLASSMEMBERS" use="tei:classes/tei:memberOf/@key"/>
  <xsl:key match="tei:elementSpec" name="CLASSMEMBERS-ELEMENTS" use="tei:classes/tei:memberOf/@key"/>
  <xsl:key match="tei:classSpec" name="CLASSMEMBERS-CLASSES" use="tei:classes/tei:memberOf/@key"/>
  <xsl:key match="tei:elementSpec|tei:classSpec|tei:macroSpec" name="IDENTS" use="@ident"/>

  <xsl:key match="tei:macroSpec[@type='dt']" name="DATATYPES" use="1"/>
  <xsl:key match="tei:macroSpec" name="MACRODOCS" use="1"/>
  <xsl:key match="tei:attDef" name="ATTDOCS" use="1"/>
  <xsl:key match="tei:attDef" name="ATTRIBUTES" use="@ident"/>
  <xsl:key match="tei:classSpec//tei:attDef" name="ATTRIBUTES-CLASS" use="@ident"/>
  <xsl:key match="tei:elementSpec//tei:attDef" name="ATTRIBUTES-ELEMENT" use="@ident"/>
  <xsl:key match="tei:schemaSpec" name="SCHEMASPECS" use="1"/>
  <xsl:key match="tei:schemaSpec" name="LISTSCHEMASPECS" use="@ident"/>
  <xsl:key match="tei:classSpec[@type='atts']" name="ATTCLASSDOCS" use="1"/>
  <xsl:key match="tei:classSpec[@type='model']" name="MODELCLASSDOCS" use="1"/>
  <xsl:key match="tei:elementSpec" name="ELEMENTDOCS" use="1"/>
  <xsl:key match="tei:*" name="NameToID" use="@ident"/>
  <xsl:key match="tei:elementSpec" name="ElementModule" use="@module"/>
  <xsl:key match="tei:classSpec" name="ClassModule" use="@module"/>
  <xsl:key match="tei:macroSpec" name="MacroModule" use="@module"/>
  <xsl:key match="tei:moduleSpec" name="Modules" use="1"/>
  <xsl:key match="tei:moduleSpec" name="MODULES" use="@ident"/>
  <xsl:key match="tei:classSpec[@predeclare='true']" name="predeclaredClasses" use="1"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']" name="PredeclareMacros" use="@ident"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']" name="PredeclareMacrosModule" use="@module"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']" name="PredeclareAllMacros" use="1"/>

  <xsl:variable name="BASE" select="base-uri(/tei:TEI)"/>

  <xsl:variable name="parameterize">
    <xsl:choose>
      <xsl:when test="key('SCHEMASPECS',1)">false</xsl:when>
      <xsl:otherwise>true</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="whichSchemaSpec">
    <xsl:choose>
      <xsl:when test="$selectedSchema">
        <xsl:value-of select="$selectedSchema"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="key('SCHEMASPECS',1)[1]/@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- lookup table of element contents, and templates to access the result -->
  <xsl:key match="Contains" name="ELEMENTPARENTS" use="."/>

  <xsl:variable name="generalPrefix">
    <xsl:choose>
      <xsl:when test="string-length($patternPrefix)&gt;0">
        <xsl:value-of select="$patternPrefix"/>
      </xsl:when>
      <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)[@prefix]">
        <xsl:value-of select="key('LISTSCHEMASPECS',$whichSchemaSpec)/@prefix"/>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="targetLanguage">
    <xsl:choose>
      <xsl:when test="string-length($lang)&gt;0">
        <xsl:value-of select="$lang"/>
      </xsl:when>
      <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)[@targetLang]">
        <xsl:value-of select="key('LISTSCHEMASPECS',$whichSchemaSpec)/@targetLang"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>


  <xsl:template name="generateDocumentationLang">
    <xsl:choose>
      <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)/@docLang">
        <xsl:value-of select="key('LISTSCHEMASPECS',$whichSchemaSpec)/@docLang"/>
      </xsl:when>
      <xsl:when test="string-length($doclang)&gt;0">
        <xsl:value-of select="$doclang"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="processing-instruction()">
    <xsl:choose>
      <xsl:when test="name(.) = 'odds'">
	<xsl:choose>
	  <xsl:when test=".='date'"> This formatted version of the Guidelines was created on
	  <xsl:call-template name="showDate"/>. </xsl:when>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="*" mode="literal">
    <xsl:text>&#10;</xsl:text>
    <xsl:for-each select="ancestor::rng:*">
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*">
      <xsl:text> </xsl:text>
      <xsl:value-of select="local-name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:apply-templates mode="literal"/>
        <xsl:if test="node()[last()]/self::rng:*">
          <xsl:text>&#10;</xsl:text>
        </xsl:if>
        <xsl:for-each select="ancestor::rng:*">
          <xsl:text> </xsl:text>
        </xsl:for-each>
        <xsl:text>&lt;/</xsl:text>
        <xsl:value-of select="local-name(.)"/>
        <xsl:text>&gt;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/&gt;</xsl:text>
        <xsl:if test="node()[last()]/self::rng:*">
          <xsl:text>&#10;	  </xsl:text>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="rng:ref">
    <xsl:call-template name="makeRef">
      <xsl:with-param name="lookup" select="replace(@name,'_(alternation|sequenceOptionalRepeatable|sequenceOptional|sequenceRepeatable|sequence)','')"/>
    </xsl:call-template>
  </xsl:template>

    <xsl:template name="makeRef">
      <xsl:param name="lookup"/>
      <xsl:variable name="fullname" select="@name"/>
      <xsl:variable name="myprefix"
		    select="ancestor::*[@prefix][1]/@prefix"/>
      <xsl:choose>
	<xsl:when test="ancestor::tei:content[@autoPrefix='false']">
	  <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$fullname}"/>
	</xsl:when>
	<xsl:when test="count(key('IDENTS',$lookup))&gt;1">
	  <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$myprefix}{$fullname}"/>
	</xsl:when>
	<xsl:when test="key('IDENTS',$lookup)">
	  <xsl:for-each select="key('IDENTS',$lookup)">
	    <xsl:variable name="localprefix">
	      <xsl:choose>
		<xsl:when test="@prefix">
		<xsl:value-of select="@prefix"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="$generalPrefix"/>
	      </xsl:otherwise>
	      </xsl:choose>
	    </xsl:variable>
	    <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$localprefix}{$fullname}"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{@name}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="rng:*">
    <xsl:element name="{local-name()}" xmlns="http://relaxng.org/ns/structure/1.0" >
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:element>
  </xsl:template>


  <xsl:template match="rng:zeroOrMore">
    <xsl:choose>
      <xsl:when
        test="rng:ref/@name='model.global'   and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name='model.global']"/>
      <xsl:when test="count(rng:*)=1 and rng:zeroOrMore">
        <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
	<zeroOrMore xmlns="http://relaxng.org/ns/structure/1.0" >
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	</zeroOrMore>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="rng:choice">
    <xsl:choose>
      <xsl:when test="count(rng:*)=1">
        <xsl:apply-templates select="a:*|rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
	<choice xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="a:*|rng:*|tei:*|text()|comment()"/>
	</choice>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="rng:group">
    <!-- check if this group is identical to the last -->
    <xsl:choose>
      <xsl:when
        test="count(rng:*)=1 and local-name(preceding-sibling::rng:*[1])='group' and rng:zeroOrMore">
        <xsl:variable name="that">
          <xsl:for-each select="preceding-sibling::rng:*[1]">
            <xsl:apply-templates mode="decomposed"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="this">
          <xsl:apply-templates mode="decomposed"/>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$that=$this"/>
	  <xsl:otherwise>
	    <group xmlns="http://relaxng.org/ns/structure/1.0">
              <xsl:copy-of select="@*"/>
              <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	    </group>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="{local-name()}" xmlns="http://relaxng.org/ns/structure/1.0" >
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="rng:*" mode="decomposed">
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*">
      <xsl:text>@</xsl:text>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:apply-templates mode="decomposed"/>
  </xsl:template>


  <xsl:template match="tei:*" mode="tangle"/>

  <xsl:template match="tei:attRef" mode="tangle">
    <xsl:call-template name="makeRef">
      <xsl:with-param name="lookup" select="substring-before(@name,'.attribute')"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:attDef" mode="tangle">
    <xsl:param name="element"/>
    <xsl:variable name="I">
      <xsl:value-of select="translate(@ident,':','')"/>
    </xsl:variable>
    <xsl:if test="not(starts-with(@ident,'xmlns'))">
      <xsl:choose>
        <xsl:when test="ancestor::tei:elementSpec">
          <xsl:call-template name="makeAnAttribute"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:classSpec">
          <define xmlns="http://relaxng.org/ns/structure/1.0"
            name="{$element}.attribute.{translate(@ident,':','')}">
            <xsl:call-template name="makeAnAttribute"/>
          </define>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates select="tei:constraintSpec"/>

  </xsl:template>

  <xsl:template match="tei:attList" mode="tangle">
    <xsl:param name="element"/>
    <xsl:choose>
      <xsl:when test="count(*)=0"/>
      <xsl:when test="@org='group' and         parent::tei:attList[@org='choice']">
        <group xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:apply-templates mode="tangle" select="tei:*">
            <xsl:with-param name="element" select="$element"/>
          </xsl:apply-templates>
        </group>
      </xsl:when>

      <xsl:when test="@org='choice'">
        <choice xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:apply-templates mode="tangle" select="tei:*">
            <xsl:with-param name="element" select="$element"/>
          </xsl:apply-templates>
        </choice>
      </xsl:when>

      <xsl:otherwise>
        <xsl:apply-templates mode="tangle" select="tei:*">
          <xsl:with-param name="element" select="$element"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:classSpec" mode="tangle">
    <xsl:variable name="c">
      <xsl:choose>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$generalPrefix"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="@ident"/>
    </xsl:variable>

    <xsl:if test="$verbose='true'">
      <xsl:message> classSpec <xsl:value-of select="@ident"/> (type <xsl:value-of select="@type"
        />)</xsl:message>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='model'">
        <xsl:apply-templates mode="processModel" select=".">
          <xsl:with-param name="declare">false</xsl:with-param>
          <!--	    <xsl:choose>
	      <xsl:when test="@module='tei'">true</xsl:when>
	      <xsl:otherwise>false</xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
-->
        </xsl:apply-templates>
      </xsl:when>
      <xsl:when test="@type='atts'">
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar">true</xsl:with-param>
          <xsl:with-param name="content">
            <Wrapper>
              <xsl:variable name="contents">
                <ROOT>
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:for-each select="key('IDENTS',@key)[1]">
                      <xsl:if test="@type='atts'">
                        <ref  xmlns="http://relaxng.org/ns/structure/1.0">
			  <xsl:attribute name="name">
			    <xsl:choose>
			      <xsl:when test="@prefix">
				<xsl:value-of select="@prefix"/>
			      </xsl:when>
			      <xsl:otherwise>
				<xsl:value-of select="$generalPrefix"/>
			      </xsl:otherwise>
			    </xsl:choose>
			    <xsl:value-of select="@ident"/>
			    <xsl:text>.attributes</xsl:text>
			  </xsl:attribute>
			</ref>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                  <xsl:for-each select="tei:attList//tei:attDef">
                    <xsl:if test="not(starts-with(@ident,'xmlns'))">
                      <ref xmlns="http://relaxng.org/ns/structure/1.0"
                        name="{$c}.attribute.{translate(@ident,':','')}"/>
                    </xsl:if>
                  </xsl:for-each>
                  <xsl:for-each select="tei:attList//tei:attRef">
		    <xsl:call-template name="makeRef">
		      <xsl:with-param name="lookup" select="substring-before(@name,'.attribute')"/>
		    </xsl:call-template>
                  </xsl:for-each>
                </ROOT>
              </xsl:variable>
              <define xmlns="http://relaxng.org/ns/structure/1.0"
		      name="{$c}.attributes">
		<xsl:for-each select="$contents/ROOT">
		  <xsl:apply-templates mode="justcopy"/>
		  <xsl:if test="not($contents/ROOT/*)">
		    <empty/>
		  </xsl:if>
		</xsl:for-each>
              </define>
              <xsl:apply-templates mode="tangle" select="tei:attList//tei:attDef">
                <xsl:with-param name="element" select="$c"/>
              </xsl:apply-templates>
            </Wrapper>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:classSpec" mode="processModel">
    <xsl:param name="declare">false</xsl:param>
    <xsl:if test="$verbose='true'">
      <xsl:message> .... model class <xsl:value-of select="@ident"/>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
        <Wrapper>
          <xsl:call-template name="processClassDefinition">
            <xsl:with-param name="type">
              <xsl:choose>
                <xsl:when test="@generate">
                  <xsl:value-of select="@generate"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:text>&#10;			     NULL
			     alternation
			     sequence
			     sequenceOptional
			     sequenceOptionalRepeatable
			   sequenceRepeatable</xsl:text>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:with-param>
            <xsl:with-param name="declare" select="$declare"/>
          </xsl:call-template>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>


  <xsl:template name="processClassDefinition">
    <xsl:param name="type"/>
    <xsl:param name="declare"/>
    <xsl:variable name="Type">
      <xsl:value-of select="normalize-space($type)"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="string-length($Type)=0">
        <xsl:call-template name="makeClassDefinition">
          <xsl:with-param name="type">NULL</xsl:with-param>
          <xsl:with-param name="declare" select="$declare"/>
        </xsl:call-template>
      </xsl:when>

      <xsl:when test="contains($Type,' ')">
        <xsl:call-template name="makeClassDefinition">
          <xsl:with-param name="type" select="substring-before($Type,' ')"/>
          <xsl:with-param name="declare" select="$declare"/>
        </xsl:call-template>
        <xsl:call-template name="processClassDefinition">
          <xsl:with-param name="type" select="substring-after($Type,' ')"/>
          <xsl:with-param name="declare" select="$declare"/>
        </xsl:call-template>
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="makeClassDefinition">
          <xsl:with-param name="type" select="$Type"/>
          <xsl:with-param name="declare" select="$declare"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="makeClassDefinition">
    <xsl:param name="type"/>
    <xsl:param name="declare"/>
    <!--
      alternation
      sequence
      sequenceOptional
      sequenceOptionalRepeatable
      sequenceRepeatable
  -->

    <xsl:variable name="thisClass">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:variable name="localprefix">
      <xsl:choose>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$generalPrefix"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="suffix">
      <xsl:choose>
        <xsl:when test="$type='NULL'"> </xsl:when>
        <xsl:otherwise>
          <xsl:text>_</xsl:text>
          <xsl:value-of select="$type"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$declare='true'">
        <xsl:apply-templates mode="tangleModel" select="tei:classes/tei:memberOf"/>
        <define xmlns="http://relaxng.org/ns/structure/1.0"
          name="{$localprefix}{$thisClass}{$suffix}">
          <xsl:if test="@predeclare='true'">
            <xsl:attribute name="combine">choice</xsl:attribute>
          </xsl:if>
          <notAllowed/>
        </define>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="makeDecls">
          <xsl:call-template name="findUses">
            <xsl:with-param name="pattern" select="$type"/>
            <xsl:with-param name="class" select="$thisClass"/>
          </xsl:call-template>
        </xsl:variable>

        <!--<xsl:message>

DEBUG <xsl:value-of select="$thisClass"/><xsl:value-of
select="$suffix"/> generated <xsl:value-of
select="$makeDecls"/></xsl:message>
-->
        <xsl:choose>
          <!--
               <xsl:when test="$makeDecls=''">
                  <xsl:if test="$verbose='true'">
                     <xsl:message>Do NOT generate <xsl:value-of select="$thisClass"/>
                        <xsl:value-of select="$suffix"/> (<xsl:value-of select="$type"/>)                     </xsl:message>
                  </xsl:if>
               </xsl:when>
-->
          <xsl:when test="count(key('CLASSMEMBERS',$thisClass))&gt;0">
            <xsl:if test="$verbose='true'">
              <xsl:message> .... ... generate model <xsl:value-of select="$thisClass"/>
                <xsl:value-of select="$suffix"/> (<xsl:value-of select="$type"/>) </xsl:message>
            </xsl:if>
            <define xmlns="http://relaxng.org/ns/structure/1.0"
              name="{$localprefix}{$thisClass}{$suffix}">
              <xsl:choose>
                <xsl:when test="$type='sequence'">
                  <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
                    <xsl:apply-templates select="."
					 mode="classmember">
                      <xsl:with-param name="theClass" select="$thisClass"/>
                      <xsl:with-param name="suffix" select="$type"/>
                    </xsl:apply-templates>
                  </xsl:for-each>
                </xsl:when>
                <xsl:when test="$type='sequenceOptional'">
                  <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
                    <optional>
                      <xsl:apply-templates select="."  mode="classmember">
			<xsl:with-param name="theClass" select="$thisClass"/>
                        <xsl:with-param name="suffix" select="$type"/>
                      </xsl:apply-templates>
                    </optional>
                  </xsl:for-each>
                </xsl:when>

                <xsl:when test="$type='sequenceRepeatable'">
                  <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
                    <oneOrMore>
                      <xsl:apply-templates select="."  mode="classmember">
			<xsl:with-param name="theClass" select="$thisClass"/>		   
                        <xsl:with-param name="suffix" select="$type"/>
                      </xsl:apply-templates>
                    </oneOrMore>
                  </xsl:for-each>
                </xsl:when>

                <xsl:when test="$type='sequenceOptionalRepeatable'">
                  <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
                    <zeroOrMore>
                      <xsl:apply-templates select="." mode="classmember">
                        <xsl:with-param name="suffix" select="$type"/>
			<xsl:with-param name="theClass" select="$thisClass"/>
                      </xsl:apply-templates>
                    </zeroOrMore>
                  </xsl:for-each>
                </xsl:when>

                <xsl:otherwise>
                  <choice>
                    <xsl:for-each select="key('CLASSMEMBERS',$thisClass)">
                      <xsl:apply-templates select="." mode="classmember">
                        <xsl:with-param name="suffix" select="$type"/>
			<xsl:with-param name="theClass" select="$thisClass"/>
                      </xsl:apply-templates>
                    </xsl:for-each>
                  </choice>
                </xsl:otherwise>
              </xsl:choose>
            </define>
          </xsl:when>
          <xsl:otherwise>
            <define xmlns="http://relaxng.org/ns/structure/1.0"
              name="{$localprefix}{$thisClass}{$suffix}">
              <xsl:choose>
                <xsl:when
                  test="$type='sequence' or     $type='sequenceOptional' or      $type='sequenceOptionalRepeatable'">
                  <empty/>
                </xsl:when>
                <xsl:otherwise>
                  <notAllowed/>
                </xsl:otherwise>
              </xsl:choose>
            </define>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



  <xsl:template name="findUses">
    <xsl:param name="pattern"/>
    <xsl:param name="class"/>
    <xsl:variable name="suffix">
      <xsl:choose>
        <xsl:when test="$pattern='NULL'"/>
        <xsl:otherwise>
          <xsl:text>_</xsl:text>
          <xsl:value-of select="$pattern"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="not(ancestor::tei:schemaSpec)">x</xsl:when>
      <xsl:when test="key('REFS',concat($class,$suffix))">x</xsl:when>
      <xsl:when test="key('REFS',$class)">x</xsl:when>
      <xsl:when test="not($suffix='')"/>
      <xsl:when test="tei:classes/tei:memberOf">
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:for-each select="key('CLASSES',@key)">
            <xsl:call-template name="findUses">
              <xsl:with-param name="pattern"/>
              <xsl:with-param name="class" select="@ident"/>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:classSpec" mode="tangleadd">
    <xsl:apply-templates mode="tangleadd"/>
  </xsl:template>


  <xsl:template match="tei:classSpec/@ident"/>


  <xsl:template match="tei:code">
    <xsl:call-template name="typewriter">
      <xsl:with-param name="text">
        <xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="text()" mode="doc">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="tei:desc" mode="tangle"/>

  <xsl:template match="tei:classSpec" mode="classmember">
    <xsl:param name="suffix"/>
    <xsl:variable name="localprefix">
      <xsl:choose>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$generalPrefix"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$suffix='' or $suffix='NULL'">
        <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$localprefix}{@ident}"/>
      </xsl:when>
      <xsl:otherwise>
        <ref xmlns="http://relaxng.org/ns/structure/1.0"
	     name="{$localprefix}{@ident}_{$suffix}"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:elementSpec" mode="classmember">
    <xsl:param name="theClass"/>

    <xsl:variable name="min" select="tei:classes/tei:memberOf[@key=$theClass]/@min"/>
    <xsl:variable name="max" select="tei:classes/tei:memberOf[@key=$theClass]/@max"/>

    <xsl:variable name="mini" as="xs:integer">
      <xsl:choose>
        <xsl:when test="not($min castable as xs:integer)">1</xsl:when>
        <xsl:otherwise><xsl:value-of select="$min"/></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="elementPrefix">
      <xsl:choose>
        <xsl:when test="@prefix">
          <xsl:value-of select="@prefix"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$generalPrefix"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="ident" select="@ident"/>
    
    <xsl:for-each select="for $i in 1 to $mini return $i">
      <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$elementPrefix}{$ident}"/>
    </xsl:for-each> 
    <xsl:choose>
      <xsl:when test="$max='unbounded'">
        <zeroOrMore xmlns="http://relaxng.org/ns/structure/1.0">
          <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$elementPrefix}{$ident}"/>
        </zeroOrMore>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="maxi" as="xs:integer">
          <xsl:choose>
            <xsl:when test="not($max castable as xs:integer)">1</xsl:when>
            <xsl:otherwise><xsl:value-of select="$max"/></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:for-each select="for $i in ($mini+1) to $maxi return $i">
          <optional xmlns="http://relaxng.org/ns/structure/1.0">
            <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$elementPrefix}{$ident}"/>
          </optional>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>

  <xsl:template match="tei:elementSpec" mode="tangle">
    <xsl:variable name="elementPrefix">
      <xsl:choose>
        <xsl:when test="@prefix">
          <xsl:value-of select="@prefix"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$generalPrefix"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message> elementSpec [<xsl:value-of select="$elementPrefix"/>]<xsl:value-of select="@ident"/>
        <xsl:if test="@xml:id">: <xsl:value-of select="@xml:id"/>
        </xsl:if>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar"/>
      <xsl:with-param name="content">
        <Wrapper>
          <xsl:variable name="name">
            <xsl:choose>
              <xsl:when test="tei:altIdent=@ident">
                <xsl:value-of select="@ident"/>
              </xsl:when>
              <xsl:when test="tei:altIdent">
                <xsl:value-of select="normalize-space(tei:altIdent)"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@ident"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="tei:content/rng:notAllowed">
              <define xmlns="http://relaxng.org/ns/structure/1.0" name="{$elementPrefix}{@ident}">
                <notAllowed/>
              </define>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="Attributes">
                <xsl:call-template name="summarizeAttributes"/>
              </xsl:variable>
              <define xmlns="http://relaxng.org/ns/structure/1.0" name="{$elementPrefix}{@ident}">
                <element name="{$name}">
                  <xsl:if test="@ns">
                    <xsl:attribute name="ns">
                      <xsl:value-of select="@ns"/>
                    </xsl:attribute>
                  </xsl:if>
                  <xsl:if test="not($oddmode='tei')">
                    <a:documentation>
                      <xsl:call-template name="makeDescription">
                        <xsl:with-param name="includeValList">true</xsl:with-param>
                        <xsl:with-param name="coded">false</xsl:with-param>
                      </xsl:call-template>
                    </a:documentation>
                  </xsl:if>
                  <xsl:choose>
                    <xsl:when test="$parameterize='true'">
                      <ref name="{$elementPrefix}{@ident}.content"/>
                      <xsl:if test="not($Attributes='')">
			<xsl:if test="$verbose='true'">
			  <xsl:message>   refer to attributes: </xsl:message>
			</xsl:if>
                        <ref name="{$elementPrefix}{@ident}.localattributes"/>
                      </xsl:if>
		    </xsl:when>
		    <xsl:otherwise>
                      <xsl:call-template name="defineContent"/>
                      <xsl:if test="not($Attributes='')">
			<xsl:call-template name="defineAttributes"/>
                      </xsl:if>
                    </xsl:otherwise>
                  </xsl:choose>
                </element>
              </define>
              <xsl:if test="$parameterize='true'">
                <define xmlns="http://relaxng.org/ns/structure/1.0"
                  name="{$elementPrefix}{@ident}.content">
                  <xsl:call-template name="defineContent"/>
                </define>
                <xsl:if test="not($Attributes='')">
                  <define xmlns="http://relaxng.org/ns/structure/1.0"
                    name="{$elementPrefix}{@ident}.localattributes">
                    <xsl:call-template name="defineAttributes"/>
                  </define>
                </xsl:if>
                <xsl:apply-templates mode="tangleModel" select="tei:classes/tei:memberOf"/>
              </xsl:if>
            </xsl:otherwise>
          </xsl:choose>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="summarizeAttributes">
    <xsl:for-each select=".//tei:attDef">x</xsl:for-each>
    <xsl:for-each select=".//tei:attRef">x</xsl:for-each>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('CLASSES',@key)">
        <xsl:if test="@type='atts'">x</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="defineAttributes">
    <xsl:variable name="name">
      <xsl:choose>
	<xsl:when test="@prefix">
	  <xsl:value-of select="@prefix"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$generalPrefix"/>
	</xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message>   now define attributes for <xsl:value-of
      select="@ident"/> (parameterize=<xsl:value-of select="$parameterize"/>)</xsl:message>
    </xsl:if>
    <xsl:if test="$parameterize='true'">
      <xsl:if test="$autoGlobal='true'">
        <ref xmlns="http://relaxng.org/ns/structure/1.0" name="att.global.attributes"/>
      </xsl:if>
      <xsl:for-each select="tei:classes/tei:memberOf">
        <xsl:for-each select="key('CLASSES',@key)">
          <xsl:if test="@type='atts'">
            <ref xmlns="http://relaxng.org/ns/structure/1.0">
	      <xsl:attribute name="name">
		<xsl:choose>
		  <xsl:when test="@prefix">
		    <xsl:value-of select="@prefix"/>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:value-of select="$generalPrefix"/>
		  </xsl:otherwise>
		</xsl:choose>
		<xsl:value-of select="@ident"/>
		<xsl:text>.attributes</xsl:text>
	      </xsl:attribute>
	    </ref>
          </xsl:if>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates mode="tangle" select="tei:attList">
      <xsl:with-param name="element">
	<xsl:value-of select="$name"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <!-- place holder to make sure something gets into the
	 pattern -->
    <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
  </xsl:template>

  <xsl:template name="defineContent">
    <xsl:variable name="Contents">
      <TEMPTREE>
        <xsl:choose>
          <xsl:when test="tei:valList[@type='closed' and @repeatable='true']">
            <list xmlns="http://relaxng.org/ns/structure/1.0">
              <oneOrMore>
                <choice>
                  <xsl:call-template name="valListChildren"/>
                </choice>
              </oneOrMore>
            </list>
          </xsl:when>
          <xsl:when test="tei:valList[@type='closed']">
            <xsl:call-template name="valListChildren"/>
          </xsl:when>
          <xsl:when test="tei:content">
            <xsl:apply-templates select="tei:content/*|tei:content/processing-instruction()"/>
          </xsl:when>
          <xsl:otherwise>
            <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
          </xsl:otherwise>
        </xsl:choose>
      </TEMPTREE>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="count($Contents/TEMPTREE/*)=0">
        <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="$Contents/TEMPTREE">
          <xsl:apply-templates mode="justcopy"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="tei:constraintSpec"/>

  </xsl:template>


  <xsl:template name="valListChildren">
    <choice xmlns="http://relaxng.org/ns/structure/1.0">
      <xsl:for-each select="tei:valList/tei:valItem">
        <value>
          <xsl:choose>
            <xsl:when test="tei:altIdent=@ident">
              <xsl:value-of select="@ident"/>
            </xsl:when>
            <xsl:when test="tei:altIdent">
              <xsl:value-of select="normalize-space(tei:altIdent)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@ident"/>
            </xsl:otherwise>
          </xsl:choose>
        </value>
        <xsl:if test="not($oddmode='tei')">
          <a:documentation>
            <xsl:call-template name="makeDescription">
              <xsl:with-param name="includeValList">true</xsl:with-param>
              <xsl:with-param name="coded">false</xsl:with-param>
            </xsl:call-template>
          </a:documentation>
        </xsl:if>
      </xsl:for-each>
    </choice>
  </xsl:template>


  <xsl:template match="tei:elementSpec/@ident"/>

  <xsl:template match="tei:elementSpec/tei:desc"/>

  <xsl:template match="tei:classSpec/tei:desc"/>

  <xsl:template match="tei:macroSpec/tei:desc"/>

  <xsl:template match="tei:elementSpec/tei:gloss"/>

  <xsl:template match="tei:classSpec/tei:gloss"/>

  <xsl:template match="tei:macroSpec/tei:gloss"/>


  <xsl:template match="tei:index">
      <xsl:call-template name="makeAnchor">
	<xsl:with-param name="name">IDX-<xsl:number level="any"/>
	</xsl:with-param>
      </xsl:call-template>
  </xsl:template>


  <xsl:template match="tei:macroSpec" mode="tangle">
    <xsl:param name="filename"/>
    <xsl:variable name="macroPrefix">
      <xsl:choose>
        <xsl:when test="@prefix">
          <xsl:value-of select="@prefix"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$generalPrefix"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="entityContent">
      <TEMPTREE>
        <xsl:choose>
	  <xsl:when test="tei:valList[@type='closed']">
            <xsl:call-template name="valListChildren"/>
	  </xsl:when>
          <xsl:when test="tei:content/rng:group/rng:ref">
	    <xsl:apply-templates select="tei:content/*|tei:content/processing-instruction()"/>	    
	  </xsl:when>
          <xsl:when test="tei:content/rng:group">
            <choice xmlns="http://relaxng.org/ns/structure/1.0">
              <xsl:apply-templates select="tei:content/rng:group/*"/>
            </choice>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="tei:content/*|tei:content/processing-instruction()"/>
          </xsl:otherwise>
        </xsl:choose>
      </TEMPTREE>
    </xsl:variable>
    <xsl:variable name="entityCount">
      <xsl:for-each select="$entityContent/TEMPTREE">
        <xsl:value-of select="count(rng:*|processing-instruction())"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="@ident=&#34;TEI.singleBase&#34;"/>
      <xsl:when test="starts-with($entityContent,&#34;'&#34;)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Omit <xsl:value-of select="$entityContent"/> for <xsl:value-of select="@ident"/>
          </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="$entityCount = 0 and starts-with($entityContent,&#34;-&#34;)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Omit <xsl:value-of select="$entityContent"/> for <xsl:value-of select="@ident"/>
          </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message> macroSpec <xsl:value-of select="@ident"/>
          </xsl:message>
        </xsl:if>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar">true</xsl:with-param>
          <xsl:with-param name="content">
            <Wrapper>
              <define xmlns="http://relaxng.org/ns/structure/1.0" name="{$macroPrefix}{@ident}">
                <xsl:if test="$parameterize='true'">
                  <xsl:if test="starts-with(@ident,'macro.component')     or @predeclare='true'">
                    <xsl:attribute name="combine">choice</xsl:attribute>
                  </xsl:if>
                </xsl:if>
                <xsl:choose>
                  <xsl:when test="starts-with(@ident,'type')">
                    <xsl:apply-templates mode="justcopy" select="$entityContent/TEMPTREE/node()"/>
                  </xsl:when>
                  <xsl:when test="$entityCount=0">
                    <choice>
                      <empty/>
                    </choice>
                  </xsl:when>
                  <xsl:when test="$entityCount=1">
                    <xsl:apply-templates mode="justcopy" select="$entityContent/TEMPTREE/node()"/>
                  </xsl:when>
                  <xsl:when test="tei:content/rng:text|tei:content/rng:ref">
                    <choice>
                      <xsl:apply-templates mode="justcopy" select="$entityContent/TEMPTREE/node()"/>
                    </choice>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:apply-templates mode="justcopy"  select="$entityContent/TEMPTREE/node()"/>
                  </xsl:otherwise>
                </xsl:choose>
              </define>
            </Wrapper>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:macroSpec/@ident"/>

  <xsl:template match="tei:macroSpec/content/rng:*"/>

  <xsl:template match="tei:memberOf" mode="tangleModel">
    <!--
    <xsl:variable name="owner">
      <xsl:value-of
        select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"
      />
    </xsl:variable>
    <xsl:for-each select="key('IDENTS',@key)">
      <xsl:if test="@type='model'">
        <define combine="choice" name="{@ident}"
          xmlns="http://relaxng.org/ns/structure/1.0">
          <ref name="{$generalPrefix}{$owner}"
            xmlns="http://relaxng.org/ns/structure/1.0"/>
        </define>
      </xsl:if>
    </xsl:for-each>
-->
  </xsl:template>


  <xsl:template match="tei:moduleRef" mode="tangle">
    <!-- save a reference to myself so I can access my attrs and -->
    <!-- generated node ID later -->
    <xsl:variable name="me-the-moduleRef" select="."/>
    <xsl:variable name="This" select="@key"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> .... import module [<xsl:value-of select="$This"/> <xsl:value-of select="@url"/>] </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
        <Wrapper>
          <xsl:choose>
            <xsl:when test="@url and $parameterize='true'">
              <include xmlns="http://relaxng.org/ns/structure/1.0" href="{@url}">
                <xsl:apply-templates mode="justcopy"  select="tei:content/*"/>
              </include>
            </xsl:when>
            <xsl:when test="@url and $parameterize='false'">
              <xsl:comment>Start of import of <xsl:value-of select="@url"/>
              </xsl:comment>
              <div xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:for-each select="doc(resolve-uri(@url,$BASE))/rng:grammar">
                  <!-- the "expandRNG" processing changed 2011-08-25 by Syd Bauman: -->
                  <!-- added a 'prefix' parameter which value is prefixed to pattern -->
                  <!-- names in the included schema. This prevents collisions in the -->
                  <!-- output RNG. -->
                  <xsl:apply-templates mode="expandRNG" select="@*|node()">
                    <xsl:with-param name="prefix">
		      <xsl:if test="$me-the-moduleRef/@prefix">
			<xsl:value-of select="$me-the-moduleRef/@prefix"/>
		      </xsl:if>
                    </xsl:with-param>
                  </xsl:apply-templates>
                </xsl:for-each>
                <xsl:apply-templates mode="justcopy"  select="tei:content/*"/>
              </div>
              <xsl:comment>End of import of <xsl:value-of select="@url"/>
              </xsl:comment>
            </xsl:when>
            <xsl:otherwise>
              <include xmlns="http://relaxng.org/ns/structure/1.0"
                href="{$schemaBaseURL}{$This}.rng">
                <xsl:attribute name="ns">
                  <xsl:choose>
                    <xsl:when test="ancestor::tei:schemaSpec/@ns">
                      <xsl:value-of select="ancestor::tei:schemaSpec/@ns"/>
                    </xsl:when>
                    <xsl:otherwise>http://www.tei-c.org/ns/1.0</xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
                <xsl:for-each select="../tei:*[@module=$This and not(@mode='add')]">
                  <xsl:apply-templates mode="tangle" select="."/>
                </xsl:for-each>
              </include>
            </xsl:otherwise>
          </xsl:choose>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- begin expand RELAX NG section -->

  <xsl:template match="@*|text()|comment()|processing-instruction" mode="expandRNG">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="rng:start" mode="expandRNG"/>
  
  <xsl:template match="rng:include" mode="expandRNG">
    <xsl:param name="prefix"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> .... import <xsl:value-of select="@href"/></xsl:message>
    </xsl:if>
    <xsl:comment>Start of import of <xsl:value-of select="@href"/></xsl:comment>
    <div xmlns="http://relaxng.org/ns/structure/1.0">
    <xsl:for-each
	  select="doc(resolve-uri(@href,base-uri(/)))/rng:grammar">
        <xsl:apply-templates mode="expandRNG" select="@*|node()">
          <xsl:with-param name="prefix" select="$prefix"/>
        </xsl:apply-templates>
      </xsl:for-each>
    </div>
    <xsl:comment>End of import of <xsl:value-of select="@href"/>
    </xsl:comment>
  </xsl:template>
  
  <xsl:template match="rng:define | rng:ref" mode="expandRNG">
    <xsl:param name="prefix"/>
    <xsl:if test="$verbose='true'">
      <xsl:message>expanding rng:<xsl:value-of select="local-name(.)"/> name=<xsl:value-of select="@name"/>, giving it a prefix of '<xsl:value-of select="$prefix"/>'.</xsl:message>
    </xsl:if>
    <!-- generate a copy of this <define> or <ref> -->
    <xsl:copy>
      <!-- copy over all attributes (including @name) -->
      <xsl:apply-templates select="@*" mode="expandRNG"/>
      <xsl:if test="@name">
        <!-- then replace the copied @name with our own that is the same -->
        <!-- except has our prefix in the value -->
        <xsl:attribute name="name" select="concat( $prefix, @name )"/>
      </xsl:if>
      <!-- then copy over any content -->
      <xsl:apply-templates select="node()" mode="expandRNG">
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="*" mode="expandRNG">
    <xsl:param name="prefix">erng_</xsl:param>
    <xsl:copy>
      <xsl:apply-templates mode="expandRNG" select="@*|node()">
        <xsl:with-param name="prefix" select="$prefix"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <!-- end expand RELAX NG section -->

  <xsl:template match="tei:remarks" mode="tangle"/>


  <xsl:template match="tei:specGrp" mode="ok">
    <xsl:param name="filename"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> processing specGrp <xsl:value-of select="@xml:id"/>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="processSchemaFragment">
      <xsl:with-param name="filename" select="$filename"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:tag">
    <xsl:call-template name="typewriter">
      <xsl:with-param name="text">
        <xsl:text>&lt;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&gt;</xsl:text>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="compositeNumber">
    <xsl:choose>
      <xsl:when test="ancestor::tei:div1">
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number/>
        </xsl:for-each>
        <xsl:text>.</xsl:text>
        <xsl:number from="tei:div1" level="any"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="ancestor::tei:div[1]">
          <xsl:number count="tei:div" from="tei:text" level="multiple"/>
        </xsl:for-each>
        <xsl:text>.</xsl:text>
        <xsl:number from="tei:div"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:p" mode="copyrighttext">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:list" mode="copyrighttext">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tei:item" mode="copyrighttext">
    <xsl:text>&#10; *</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>



  <xsl:template name="attributeData">
    <xsl:choose>
      <xsl:when test="tei:valList[@type='closed']">
        <choice xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:for-each select="tei:valList/tei:valItem">
            <value>
              <xsl:choose>
                <xsl:when test="tei:altIdent=@ident">
                  <xsl:value-of select="@ident"/>
                </xsl:when>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="normalize-space(tei:altIdent)"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </value>
            <xsl:if test="not($oddmode='tei')">
              <a:documentation>
                <xsl:call-template name="makeDescription">
                  <xsl:with-param name="includeValList">true</xsl:with-param>
                  <xsl:with-param name="coded">false</xsl:with-param>
                </xsl:call-template>
              </a:documentation>
            </xsl:if>
          </xsl:for-each>
        </choice>
      </xsl:when>
      <xsl:when test="tei:valList[@type='semi']">
        <choice xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:for-each select="tei:valList/tei:valItem">
            <value>
              <xsl:choose>
                <xsl:when test="tei:altIdent=@ident">
                  <xsl:value-of select="@ident"/>
                </xsl:when>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="normalize-space(tei:altIdent)"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </value>
            <xsl:if test="not($oddmode='tei')">
              <a:documentation>
                <xsl:call-template name="makeDescription">
                  <xsl:with-param name="includeValList">true</xsl:with-param>
                  <xsl:with-param name="coded">false</xsl:with-param>
                </xsl:call-template>
              </a:documentation>
            </xsl:if>
          </xsl:for-each>
          <xsl:choose>
            <xsl:when test="tei:datatype/rng:ref[@name='data.enumerated']">
              <data type="Name"/>
            </xsl:when>
	    <xsl:when test="not(tei:datatype)">
              <data type="Name"/>
	    </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates select="tei:datatype/rng:*"/>
            </xsl:otherwise>
          </xsl:choose>
        </choice>
      </xsl:when>
      <xsl:when test="tei:datatype/rng:*">
        <xsl:apply-templates select="tei:datatype/rng:*"/>
      </xsl:when>
      <xsl:otherwise>
        <text xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="makeSimpleAttribute">
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="tei:altIdent=@ident">
          <xsl:value-of select="@ident"/>
        </xsl:when>
        <xsl:when test="tei:altIdent">
          <xsl:value-of select="normalize-space(tei:altIdent)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:if test="@ns='http://www.w3.org/XML/1998/namespace'">xml:</xsl:if>
          <xsl:value-of select="@ident"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <attribute xmlns="http://relaxng.org/ns/structure/1.0" name="{$name}">
      <xsl:if test="@ns">
        <xsl:copy-of select="@ns"/>
      </xsl:if>
      <xsl:if test="tei:defaultVal and not(tei:defaultVal='')">
        <xsl:attribute name="a:defaultValue">
          <xsl:value-of select="normalize-space(tei:defaultVal)"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="not($oddmode='tei')">
        <a:documentation>
          <xsl:call-template name="makeDescription">
            <xsl:with-param name="includeValList">true</xsl:with-param>
            <xsl:with-param name="coded">false</xsl:with-param>
          </xsl:call-template>
        </a:documentation>
      </xsl:if>
      <!-- ************************************ -->
      <!-- Ascertain minOccurs= and maxOccurs=. -->
      <!-- ************************************ -->
      <!-- get the value of minOccurs=, defaulting to "1" -->
      <xsl:variable name="minOccurs">
        <xsl:choose>
          <xsl:when test="tei:datatype/@minOccurs">
            <xsl:value-of select="tei:datatype/@minOccurs"/>
          </xsl:when>
          <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <!-- get the value of maxOccurs=, defaulting to "1" -->
      <xsl:variable name="maxOccurs">
        <xsl:choose>
          <xsl:when test="tei:datatype/@maxOccurs">
            <xsl:value-of select="tei:datatype/@maxOccurs"/>
          </xsl:when>
          <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <!-- We now have two _string_ representations of the attrs, but -->
      <!-- we need integers. So cast them, converting "unbounded" to  -->
      <!-- a special flag value (-1): -->
      <xsl:variable name="min" select="xs:integer( $minOccurs )"/>
      <xsl:variable name="max">
        <xsl:choose>
          <xsl:when test="$maxOccurs='unbounded'">
            <xsl:value-of select="xs:integer( -1 )"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="xs:integer( $maxOccurs )"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="tei:datatype/rng:text  or  not( tei:datatype )	or  $max=1">
          <!-- If there is only going to be one output RELAX NG node   --> 
          <!-- in the attribute definition, then we don't need to      -->
          <!-- bother with the complex min & max code below (in the    -->
          <!-- <xsl:otherwise>). Although it would generate the right  -->
          <!-- number of RELAX NG constructs, it wraps them in a       -->
          <!-- <list>, which makes no sense in some cases, and will    -->
          <!-- cause an error if the RELAX NG node inside the list is  -->
          <!-- <text>.                                                 -->
          <!-- Cases we handle here:                                   -->
          <!-- * my <datatype> child has an <rng:text> child: only 1   -->
          <!--   output node (<rng:text>), which can't be wrapped in   -->
          <!--   a <list>                                              -->
          <!-- * I don't have a <datatype> child: the 'attributeData'  -->
          <!--   template will spit out a single <rng:text> node (see  -->
          <!--   the outermost <xsl:otherwise> of that template)       -->
          <!-- * @maxOccurs=1: whatever ends up being generated, there -->
          <!--   will only be 1 of them, so no need for a <list>       -->
          <xsl:call-template name="attributeData"/>
        </xsl:when>
        <xsl:otherwise>
          <!-- Note that in the (erroneous) event   -->
          <!-- that minOccurs= is greater than      -->
          <!-- maxOccurs=, the latter is simply     -->
          <!-- ignored.                             -->
          <!-- Hack(?): -->
          <!-- The 'attributeData' template needs to operate from this node; -->
          <!-- However, once we've used for-each to "loop", we've lost the -->
          <!-- current node (it has become one of the integers in the select= -->
          <!-- range). So here we remember the current node, and re-set it -->
          <!-- before calling 'attributeData'. Perhaps it would be better to -->
          <!-- send it as a parameter to 'attributeData' and have it set the -->
          <!-- current node, but since I didn't write 'attributeData', I've -->
          <!-- chosen this method so I don't have to muck with it. -Syd -->
          <xsl:variable name="thisNode" select="."/>
	  <list>
            <xsl:choose>
              <xsl:when test="$max= -1 and $min=1">
                <oneOrMore>
                  <xsl:for-each select="$thisNode">
                    <xsl:call-template name="attributeData"/>
                  </xsl:for-each>
                </oneOrMore>
              </xsl:when>
	      <xsl:otherwise>
		  <xsl:if test="$min > 0">
		    <xsl:for-each select="1 to $min">
		      <xsl:for-each select="$thisNode">
			<xsl:call-template name="attributeData"/>
		      </xsl:for-each>
		    </xsl:for-each>
		  </xsl:if>
		  <xsl:choose>
		    <xsl:when test="$max= -1"><!-- i.e., unbounded -->
		      <zeroOrMore>
			<xsl:for-each select="$thisNode">
			  <xsl:call-template name="attributeData"/>
			</xsl:for-each>
		      </zeroOrMore>
		    </xsl:when>
		    <xsl:otherwise>
		      <xsl:for-each select="xs:integer( $min + 1 ) to $max">
			<optional>
			  <xsl:for-each select="$thisNode">
			    <xsl:call-template name="attributeData"/>
			  </xsl:for-each>
			</optional>
		      </xsl:for-each>
		    </xsl:otherwise>
		  </xsl:choose>
	      </xsl:otherwise>
	    </xsl:choose>
	  </list>
        </xsl:otherwise>
      </xsl:choose>
    </attribute>
  </xsl:template>

  <xsl:template name="makeAnAttribute">
    <xsl:choose>
      <xsl:when test="@usage='req'">
        <xsl:call-template name="makeSimpleAttribute"/>
      </xsl:when>
      <!--
      <xsl:when test="parent::tei:attList[@org='choice']">
        <xsl:call-template name="makeSimpleAttribute"/>
      </xsl:when>
-->
      <xsl:otherwise>
        <optional xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:call-template name="makeSimpleAttribute"/>
        </optional>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="generateClassParents">
    <xsl:choose>
      <xsl:when test="not(tei:classes)"> (none) </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:if test="preceding-sibling::tei:memberOf">
            <xsl:text>: </xsl:text>
          </xsl:if>
          <xsl:choose>
            <xsl:when test="key('CLASSES',@key)">
              <xsl:for-each select="key('CLASSES',@key)">
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name" select="@ident"/>
                </xsl:call-template>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@key"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="linkStyle"/>


  <xsl:template name="getSpecURL">
    <xsl:param name="name"/>
    <xsl:param name="type"/>
    <xsl:choose>
      <xsl:when test="$type='macro'">
        <xsl:for-each select="id('REFENT')">
          <xsl:apply-templates mode="generateLink" select="."/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="$type='element'">
        <xsl:for-each select="id('REFTAG')">
          <xsl:apply-templates mode="generateLink" select="."/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="$type='class'">
        <xsl:for-each select="id('REFCLA')">
          <xsl:apply-templates mode="generateLink" select="."/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
    <xsl:text>#</xsl:text>
    <xsl:value-of select="$name"/>
  </xsl:template>


  <xsl:template name="linkTogether">
    <xsl:param name="name"/>
    <xsl:param name="reftext"/>
    <xsl:param name="class">link_odd</xsl:param>

    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDocumentationLang"/>
    </xsl:variable>
    <xsl:variable name="partialname">
      <xsl:choose>
        <xsl:when test="contains($name,'_')">
          <xsl:value-of select="substring-before($name,'_')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$name"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="link">
      <xsl:choose>
        <xsl:when test="$reftext=''">
          <xsl:value-of select="$name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$reftext"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="not(key('IDENTS',$partialname))">
        <xsl:value-of select="$link"/>
      </xsl:when>
      <xsl:when test="$oddmode='html' and number($splitLevel)=-1">
        <a xmlns="http://www.w3.org/1999/xhtml" class="{$class}" href="#{$partialname}">
          <xsl:value-of select="$link"/>
        </a>
      </xsl:when>
      <xsl:when test="$oddmode='html' and $STDOUT='true'">
        <a xmlns="http://www.w3.org/1999/xhtml" class="{$class}">
          <xsl:attribute name="href">
            <xsl:for-each select="key('IDENTS',$partialname)">
              <xsl:call-template name="getSpecURL">
                <xsl:with-param name="name">
                  <xsl:value-of select="$partialname"/>
                </xsl:with-param>
                <xsl:with-param name="type">
                  <xsl:value-of select="substring-before(local-name(),'Spec')"/>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
          </xsl:attribute>
          <xsl:value-of select="$link"/>
        </a>
      </xsl:when>


      <xsl:when test="$oddmode='html'">
        <a xmlns="http://www.w3.org/1999/xhtml" class="{$class}"
          href="{concat('ref-',$partialname,'.html')}">
          <xsl:value-of select="$link"/>
        </a>
      </xsl:when>

      <xsl:when test="$oddmode='pdf'">
        <fo:inline>
          <xsl:value-of select="$link"/>
        </fo:inline>
      </xsl:when>

      <xsl:when test="$oddmode='tei'">
        <tei:ref target="#{$partialname}">
          <xsl:value-of select="$link"/>
        </tei:ref>
      </xsl:when>

      <xsl:otherwise>
        <xsl:value-of select="$link"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <xsl:variable name="secnum">
      <xsl:call-template name="sectionNumber"/>
    </xsl:variable>
    <xsl:apply-templates mode="tangle"/>
  </xsl:template>


  <xsl:template name="make-ns-declaration">
    <xsl:param name="is-default"/>
    <xsl:param name="prefix"/>
    <xsl:param name="uri"/>
  </xsl:template>

  <xsl:template name="inhnamespace"/>

  <xsl:template match="tei:constraintSpec/tei:desc"/>
  <xsl:template match="tei:constraintSpec/tei:gloss"/>
  <xsl:template match="tei:constraintSpec/tei:equiv"/>


  <xsl:template match="tei:constraintSpec"/>

  <xsl:template match="tei:altIdent"/>

  <xsl:template match="a:*">
    <xsl:apply-templates mode="justcopy" select="."/>
  </xsl:template>

  <xsl:template match="tei:classSpec" mode="processDefaultAtts">
    <xsl:if test="$verbose='true'">
      <xsl:message> .. default attribute settings for <xsl:value-of select="@ident"/>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
        <Wrapper>
	  <xsl:variable name="c">
	    <xsl:choose>
	      <xsl:when test="@prefix">
		<xsl:value-of select="@prefix"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="$generalPrefix"/>
	      </xsl:otherwise>
	    </xsl:choose>
	    <xsl:value-of select="@ident"/>
	  </xsl:variable>
          <define xmlns="http://relaxng.org/ns/structure/1.0" combine="choice"
            name="{$c}.attributes">
            <empty/>
          </define>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element schemaSpec</desc>
  </doc>

  <xsl:template match="tei:schemaSpec">
    <xsl:call-template name="processSchemaFragment"/>
  </xsl:template>

  <xsl:template name="generateOutput">
    <xsl:param name="body"/>
    <xsl:param name="suffix"/>
    <xsl:param name="method">xml</xsl:param>
    <xsl:variable name="processor">
      <xsl:value-of select="system-property('xsl:vendor')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$outputDir='' or $outputDir='-'">
        <xsl:copy-of select="$body"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:result-document href="{$outputDir}/{@ident}{$suffix}" method="{$method}">
          <xsl:copy-of select="$body"/>
        </xsl:result-document>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="showDate">
    <xsl:choose>
      	<xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
	<xsl:otherwise>
    <xsl:value-of
	select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/>
	</xsl:otherwise>
    </xsl:choose>
</xsl:template>

  <xsl:template name="sectionNumber">
    <xsl:for-each
      select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4)[last()]">
      <xsl:for-each select="ancestor-or-self::tei:div1">
        <xsl:number from="tei:body" level="any"/>
        <xsl:text>.</xsl:text>
      </xsl:for-each>
      <xsl:number count="tei:div2|tei:div3|tei:div4" from="tei:div1" level="multiple"/>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="*" mode="expandSpecs">
    <xsl:apply-templates mode="justcopy" select="."/>
  </xsl:template>

  <xsl:template match="tei:specGrpRef" mode="expandSpecs">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
        <xsl:for-each select="id(substring(@target,2))">
          <xsl:apply-templates mode="expandSpecs"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="doc(resolve-uri(@target,$BASE))" mode="expandSpecs"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- list inside <desc> -->
  <xsl:template match="tei:desc/tei:list/tei:item">
    <xsl:text> * </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="makeTEIVersion">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::tei:TEI/processing-instruction()[name()='TEIVERSION']">
        <!-- JC Additions to form proper URL from version number -->
        <xsl:variable name="TEIVersion"
          select="ancestor-or-self::tei:TEI/processing-instruction()[name()='TEIVERSION'][1]"/>
        <xsl:variable name="TEIVersion-edition"
          select="substring-before($TEIVersion, ' Last')"/>
        <xsl:variable name="TEIVersion-datestring"
          select="concat(' Last',substring-after($TEIVersion, ' Last'))"/>
        <xsl:variable name="TEIVersionWithoutFullStop">
          <xsl:choose>
            <xsl:when
              test="substring($TEIVersion-edition,
              string-length($TEIVersion-edition)) =
              '.' and matches($TEIVersion-edition, '\d\d*\.\d\d*\.\d\d*\.')">
              <xsl:value-of
                select="substring($TEIVersion-edition,0,string-length($TEIVersion-edition))"
              />
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$TEIVersion-edition"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:variable name="versionURL"
          select="concat('http://www.tei-c.org/Vault/P5/', $TEIVersionWithoutFullStop, '/')"/>
        <xsl:text>&#10;Edition: </xsl:text>
        <xsl:value-of select="$TEIVersion"/>
        <xsl:text>&#10;Edition Location: </xsl:text>
        <xsl:value-of select="$versionURL"/>
        <xsl:text>&#10;</xsl:text>
        </xsl:when>
      <xsl:when
        test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
        <xsl:text>&#10;Edition: </xsl:text>
        <xsl:value-of
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
        <xsl:text>&#10;</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:gloss" mode="inLanguage">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="tei:desc" mode="inLanguage">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="processSchematron">
    <xsl:choose>
      <xsl:when test="ancestor::teix:egXML"/>
      <xsl:when test="self::s:ns">
        <ns prefix="{@prefix}" uri="{@uri}" xmlns="http://www.ascc.net/xml/schematron"/>
      </xsl:when>
      <xsl:when test="(self::s:report or self::s:assert) and ancestor::tei:elementSpec">
        <pattern xmlns="http://www.ascc.net/xml/schematron">
          <xsl:attribute name="name">
            <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
            <xsl:text>-constraint-</xsl:text>
            <xsl:value-of select="ancestor::tei:constraintSpec/@ident"/>
            <xsl:if test="count(../s:report|s:assert) &gt;1">
              <xsl:number/>
            </xsl:if>
          </xsl:attribute>
          <rule>
            <xsl:attribute name="context">
	      <xsl:sequence select="tei:generate-nsprefix-schematron(.)"/>
              <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
            </xsl:attribute>
            <xsl:apply-templates mode="justcopy" select="."/>
          </rule>
        </pattern>
      </xsl:when>
      <xsl:when test="self::s:pattern">
        <xsl:apply-templates mode="justcopy" select="."/>
      </xsl:when>
      <xsl:when test="self::s:rule">
        <pattern name="{ancestor::tei:constraintSpec/parent::*/@ident}-constraint-{ancestor::tei:constraintSpec/@ident}"
          xmlns="http://www.ascc.net/xml/schematron">
          <xsl:apply-templates mode="justcopy" select="."/>
        </pattern>
      </xsl:when>
      <xsl:when test="self::sch:ns">
        <ns prefix="{@prefix}" uri="{@uri}" xmlns="http://purl.oclc.org/dsdl/schematron"/>
      </xsl:when>
      <xsl:when test="self::sch:pattern">
        <xsl:apply-templates mode="justcopy" select="."/>
      </xsl:when>
      <xsl:when test="self::sch:rule">
        <pattern xmlns="http://purl.oclc.org/dsdl/schematron"
          id="{ancestor::tei:constraintSpec/parent::*/@ident}-constraint-{ancestor::tei:constraintSpec/@ident}">
          <xsl:apply-templates mode="justcopy" select="."/>
        </pattern>
      </xsl:when>
      <xsl:when test="(self::sch:report or self::sch:assert) and ancestor::tei:elementSpec">
        <pattern xmlns="http://purl.oclc.org/dsdl/schematron">
          <xsl:attribute name="id">
            <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
            <xsl:text>-constraint-</xsl:text>
            <xsl:value-of select="../../@ident"/>
            <xsl:if test="count(../sch:report|../sch:assert) &gt;1">
              <xsl:number/>
            </xsl:if>
          </xsl:attribute>
          <rule>
            <xsl:attribute name="context">
	      <xsl:sequence select="tei:generate-nsprefix-schematron(.)"/>
              <xsl:value-of select="ancestor::tei:elementSpec/@ident"/>
            </xsl:attribute>
            <xsl:apply-templates mode="justcopy" select="."/>
          </rule>
        </pattern>
      </xsl:when>
    </xsl:choose>
  </xsl:template>



</xsl:stylesheet>

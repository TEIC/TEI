<?xml version="1.0" encoding="utf-8"?>
<!--
    Copyright TEI Consortium.
    Dual-licensed under CC-by and BSD2 licences
    See the file COPYING.txt for details
    $Date$
    $Id$
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:n="www.example.com" xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0"
  exclude-result-prefixes="rng tei n"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">
  <!--
This software is dual-licensed:

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

$Id$

2008, TEI Consortium
-->
  <!-- typical usage:
   saxon -it:main -o:myodd /usr/share/xml/tei/stylesheet/tools/oddbyexample.xsl   corpus=`pwd`/

-->
  <!--
Read a corpus of TEI P5 documents and construct
an ODD customization file which expresses the subset
of the TEI you need to validate that corpus
-->
  <!-- How does this work?

1) start a variable and copy in all of the TEI

2) read the corpus and get a list of all the elements and their
attributes that it uses, put that in the same variable.

3) process the variable and read the TEI section. if an element or
 attribute is not present in the corpus section, put out a delete
 customization;
 if the attributes of an attribute class are never used
 that class may be deleted only if it doesn't claim membership in any other class
 or, if it does, none of the attributes from that other class is used.

4) for every attribute which is of type "enumerated", construct a
valList

-->
  <xsl:output indent="yes"/>
  <!-- name of odd -->
  <xsl:param name="schema">oddbyexample</xsl:param>
  <!-- whether to do all the global attributes -->
  <xsl:param name="keepGlobals">true</xsl:param><!-- was false -->
  <!-- the document corpus -->
  <xsl:param name="corpus">./</xsl:param>
  <!-- file names starting with what prefix? -->
  <xsl:param name="prefix"/>
  <!-- should elements in teiHeader be included?-->
  <xsl:param name="includeHeader">true</xsl:param>
  <!-- the source of the TEI (just needs *Spec)-->
  <xsl:param name="defaultSource"
    >https://www.tei-c.org/Vault/P5/current/xml/tei/odd/p5subset.xml</xsl:param>
  <!-- should we make valList for @rend and @rendition -->
  <xsl:param name="enumerateRend">false</xsl:param>
  <!-- should we make valList for @type -->
  <xsl:param name="enumerateType">true</xsl:param><!-- was false -->
  <!-- should we deal with non-TEI namespaces -->
  <xsl:param name="processNonTEI">false</xsl:param>
  <!-- which attributes should be make valLists for, regardless -->
  <xsl:param name="attributeList"/>
  <!-- do you want moduleRef generated with @include or @except? -->
  <xsl:param name="method">include</xsl:param> <!-- this doesn't seem to be used -->
  <!-- turn on debug messages -->
  <xsl:param name="debug">false</xsl:param>
  <!-- turn on messages -->
  <xsl:param name="verbose">false</xsl:param>
  <!-- which files to look at? provide suffix -->
  <xsl:param name="suffix">xml</xsl:param>
  <!-- provide specific list of files -->
  <xsl:param name="corpusList"/>
  <!-- should P4 files be considered? -->
  <xsl:param name="processP4">false</xsl:param>
  <!-- should P5 files be considered? -->
  <xsl:param name="processP5">true</xsl:param>
  <xsl:key name="Atts" match="@*" use="local-name(parent::*)"/>
  <xsl:key name="attVals" match="@*"
    use="concat(local-name(parent::*),local-name())"/>
  <xsl:key name="ELEMENTS" use="1" match="elementSpec"/>
  <xsl:key name="CLASSES" use="1" match="classSpec[@type='atts']"/>
  <xsl:key name="OTHERS" use="1"
    match="classSpec[not(@module='tei') and @type='model']"/>
  <xsl:key name="OTHERS" use="1" match="macroSpec[not(@module='tei')]"/>
  <xsl:key name="DOCIDENTS" use="@ident" match="docs/*[@ident]"/>
  <xsl:key name="IDENTS" use="@ident" match="*[@ident]"/>
  <xsl:key name="MEMBERS" use="@key" match="elementSpec/classes/memberOf"/>
  <xsl:key name="CLASSMEMBERS" use="@key" match="classSpec/classes/memberOf"/>
  <xsl:key name="Used" use="@ident" match="docs/elementSpec"/>
  <xsl:key name="UsedAtt" use="concat(@ident,'@',../@ident)"
    match="docs/elementSpec/attDef"/>
  <xsl:key name="All" match="*" use="1"/>
  <xsl:key name="AllTEI" match="tei:*" use="1"/>
  <xsl:key name="E" match="*" use="local-name()"/>
  <xsl:key name="EbyM" match="elementSpec" use="@module"/>
  <xsl:key name="deletedE" match="elementSpec[@mode='delete']" use="@ident"/>
  <xsl:key name="changedE" match="elementSpec[@mode='change']" use="@ident"/>
  <xsl:key name="changedE" match="elementSpec[@mode='replace']" use="@ident"/>
  <xsl:template name="main">
    <xsl:variable name="pathlist">
      <xsl:choose>
        <xsl:when test="$corpusList=''">
          <xsl:value-of
            select="concat($corpus,'?select=',$prefix, '*.',$suffix,';recurse=yes;on-error=warning')"
          />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$corpusList"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="$debug='true'">
      <xsl:message>Process <xsl:value-of select="$pathlist"/></xsl:message>
    </xsl:if>
    <xsl:variable name="docs" select="collection($pathlist)"/>
    <xsl:variable name="stage0">
      <n:ROOT>
        <xsl:if test="$processP4='true'">
          <xsl:for-each select="$docs/TEI.2">
            <xsl:if test="$verbose='true'">
              <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
              </xsl:message>
            </xsl:if>
            <TEI.2 xn="{base-uri(.)}">
              <xsl:apply-templates select="*|@*" mode="copy"/>
            </TEI.2>
          </xsl:for-each>
        </xsl:if>
        <xsl:if test="$processP5='true'">
          <xsl:for-each select="$docs/tei:*">
            <xsl:if test="$verbose='true'">
              <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
              </xsl:message>
            </xsl:if>
            <tei:TEI xn="{base-uri(.)}">
              <xsl:apply-templates select="*|@*" mode="copy"/>
            </tei:TEI>
          </xsl:for-each>
          <xsl:for-each select="$docs/tei:teiCorpus">
            <xsl:if test="$verbose='true'">
              <xsl:message>processing <xsl:value-of select="base-uri(.)"/>
              </xsl:message>
            </xsl:if>
            <tei:teiCorpus xn="{base-uri(.)}">
              <xsl:copy-of select="@*|*"/>
            </tei:teiCorpus>
          </xsl:for-each>
        </xsl:if>
      </n:ROOT>
    </xsl:variable>
    <xsl:for-each select="$stage0/*">
      <xsl:variable name="count">
        <xsl:value-of select="count(/n:ROOT/*)"/>
      </xsl:variable>
      <!-- assemble together all the TEI elements and attributes,
     followed by all the
     elements and attributes used in the corpus -->
      <xsl:variable name="stage1">
        <stage1>
          <tei>
            <xsl:for-each select="doc($defaultSource)">
              <xsl:if test="$verbose='true'">
                <xsl:message>reading main source from <xsl:value-of
                    select="$defaultSource"/></xsl:message>
              </xsl:if>
              <xsl:for-each select="key('OTHERS',1)">
                <xsl:copy>
                  <xsl:copy-of select="@ident"/>
                  <xsl:copy-of select="@type"/>
                  <xsl:copy-of select="@module"/>
                </xsl:copy>
              </xsl:for-each>
              <xsl:for-each select="key('CLASSES',1)">
                <classSpec>
                  <xsl:copy-of select="@type"/>
                  <xsl:copy-of select="@ident"/>
                  <xsl:copy-of select="@module"/>
                  <xsl:copy-of select="classes/memberOf"/>
                  <xsl:for-each select=".//attDef">
                    <attDef>
                      <xsl:copy-of select="@ident"/>
                      <xsl:call-template name="checktype"/>
                    </attDef>
                  </xsl:for-each>
                  <xsl:for-each select="classes/memberOf">
                    <classmember ident="{@key}"/>
                  </xsl:for-each>
                  <xsl:call-template name="classmembers"/>
                </classSpec>
              </xsl:for-each>
              <xsl:for-each select="key('ELEMENTS',1)">
                <elementSpec>
                  <xsl:copy-of select="@ident"/>
                  <xsl:copy-of select="@module"/>
                  <xsl:for-each select=".//tei:attDef">
                    <attDef>
                      <xsl:copy-of select="@ident"/>
                      <xsl:call-template name="checktype"/>
                    </attDef>
                  </xsl:for-each>
                  <xsl:call-template name="attributesFromClasses"/>
                </elementSpec>
              </xsl:for-each>
            </xsl:for-each>
          </tei>
          <docs>
            <xsl:for-each-group select="key('AllTEI',1)" group-by="local-name()">
              <xsl:sort select="current-grouping-key()"/>
              <xsl:variable name="ident" select="current-grouping-key()"/>
              <elementSpec>
                <xsl:attribute name="ident">
                  <xsl:value-of select="$ident"/>
                </xsl:attribute>
                <xsl:for-each-group select="key('Atts',$ident)"
                  group-by="name()">
                  <attDef ident="{current-grouping-key()}">
                    <valList type="closed">
                      <xsl:for-each-group
                        select="key('attVals',concat($ident,name()))"
                        group-by=".">
                        <xsl:sort select="."/>
                        <xsl:for-each
                          select="tokenize(current-grouping-key(),' ')">
                          <valItem ident="{.}"/>
                        </xsl:for-each>
                      </xsl:for-each-group>
                    </valList>
                  </attDef>
                </xsl:for-each-group>
              </elementSpec>
            </xsl:for-each-group>
          </docs>
        </stage1>
      </xsl:variable>
      <xsl:variable name="stage2">
        <stage2>
          <!-- copy over model classes and macros -->
          <xsl:copy-of select="$stage1/stage1/tei/classSpec[@type='model']"/>
          <xsl:copy-of select="$stage1/stage1/tei/macroSpec"/>
          <!-- for every attribute class, see if its attributes should be
	     deleted, by seeing if they are used anywhere-->
          <xsl:for-each select="$stage1/stage1/tei/classSpec[@type='atts']">
            <xsl:variable name="classatts">
              <xsl:if test="classmember">
                <keep/>
              </xsl:if>
              <xsl:for-each select="attDef">
                <xsl:variable name="this" select="@ident"/>
                <xsl:variable name="used">
                  <xsl:call-template name="checkUsed"/>
                </xsl:variable>
                <xsl:choose>
                  <xsl:when test="$keepGlobals='true' and $this='n'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='rendition'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='style'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='xml:id'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='xml:base'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='xml:space'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='xml:lang'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$keepGlobals='true' and $this='rend'">
                    <keep/>
                  </xsl:when>
                  <xsl:when test="$used=''">
                    <attDef ident="{$this}" mode="delete"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <keep/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>
            </xsl:variable>
            <xsl:choose>
              <xsl:when
                test="$classatts/attDef[@mode='delete'] and $classatts/keep">
                <classSpec ident="{@ident}" module="{@module}" type="atts"
                  mode="change">
                  <attList>
                    <xsl:copy-of select="$classatts/attDef"/>
                  </attList>
                </classSpec>
              </xsl:when>
              <xsl:when
                test="$classatts/attDef[@mode='delete'] and        not($classatts/keep)">
                <classSpec ident="{@ident}" module="{@module}" type="atts"
                  mode="delete">
                  <xsl:copy-of select="classmember"/>
                  <attList>
                    <xsl:copy-of select="$classatts/attDef"/>
                  </attList>
                </classSpec>
              </xsl:when>
              <xsl:when test="$classatts/keep">
                <classSpec ident="{@ident}" module="{@module}" type="atts"
                  mode="keep"/>
              </xsl:when>
            </xsl:choose>
          </xsl:for-each>
          <!-- for every TEI element, say if it is actually used or is to be deleted -->
          <xsl:for-each select="$stage1/stage1/tei/elementSpec">
            <xsl:choose>
              <xsl:when test="key('Used',@ident)">
                <elementSpec ident="{@ident}" module="{@module}" mode="keep">
                  <xsl:copy-of select="attDef"/>
                </elementSpec>
              </xsl:when>
              <xsl:otherwise>
                <elementSpec ident="{@ident}" module="{@module}" mode="delete"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:if test="$processNonTEI='true'">
            <xsl:for-each-group select="key('All',1)" group-by="local-name()">
              <xsl:sort/>
              <xsl:choose>
                <xsl:when test="self::n:ROOT"/>
                <xsl:when test="namespace-uri()='http://www.tei-c.org/ns/1.0'"/>
                <xsl:otherwise>
                  <!-- build new 'add' elementSpec -->
                  <elementSpec ident="{current-grouping-key()}" mode="add"
                    ns="{namespace-uri()}">
                    <xsl:text>&#10;</xsl:text>
                    <xsl:comment>add an &lt;equiv/&gt; to point to an named  template
		  in an XSLT file which will transform this to pure TEI</xsl:comment>
                    <xsl:text>&#10;</xsl:text>
                    <equiv filter="somefile.xsl" mimeType="text/xsl"
                      name="{current-grouping-key()}"/>
                    <desc>
                      <xsl:text>&#10;</xsl:text>
                      <xsl:comment> Describe the <xsl:value-of select="current-grouping-key()"/> element  here</xsl:comment>
                      <xsl:text>&#10;</xsl:text>
                    </desc>
                    <classes>
                      <xsl:text>&#10;</xsl:text>
                      <xsl:comment> Add a memberOf key="model.className"' stanza for whatever classes it belongs to  here</xsl:comment>
                      <xsl:text>&#10;</xsl:text>
                    </classes>
                    <content>
                      <xsl:text>&#10;</xsl:text>
                      <xsl:comment>Add RNG content model here</xsl:comment>
                      <xsl:text>&#10;</xsl:text>
                    </content>
                    <xsl:if test="key('Atts',local-name())">
                      <attList>
                        <xsl:comment>Add attDefs:</xsl:comment>
                        <xsl:text>&#10;</xsl:text>
                        <xsl:for-each-group select="key('Atts',local-name())"
                          group-by="local-name()">
                          <xsl:sort/>
                          <attDef ident="{local-name()}" mode="add"/>
                        </xsl:for-each-group>
                      </attList>
                    </xsl:if>
                  </elementSpec>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each-group>
          </xsl:if>
        </stage2>
      </xsl:variable>
      <!-- start writing the final ODD document -->
      <xsl:variable name="stage3">
        <TEI xml:lang="en">
          <teiHeader>
            <fileDesc>
              <titleStmt>
                <title type="short"><xsl:value-of select="$schema"/></title>
                <title>ODD by Example customization</title>
              </titleStmt>
              <publicationStmt>
                <p>Unpublished first draft </p>
              </publicationStmt>
              <sourceDesc>
                <p>Derived from <ref target="{$defaultSource}">base odd</ref> after an analysis of <xsl:value-of select="$count"/> files in <xsl:value-of select="$corpus"/></p>
              </sourceDesc>
            </fileDesc>
          </teiHeader>
          <text>
            <body>
              <schemaSpec ident="{$schema}">
                <xsl:attribute name="start">
                  <xsl:if
                    test="$stage2/stage2/elementSpec[@mode='keep' and @ident='TEI']"
                    >TEI</xsl:if>
                  <xsl:text> </xsl:text>
                  <xsl:if
                    test="$stage2/stage2/elementSpec[@mode='keep' and @ident='teiCorpus']"
                    >teiCorpus</xsl:if>
                </xsl:attribute>
                <!--
		 We need to list only modules from which elements or
		 classes have been used, but we always need tei module
	    -->
                <moduleRef key="tei"/>

                <xsl:for-each-group select="$stage2/stage2/*" group-by="@module">
                  <xsl:variable name="m" select="current-grouping-key()"/>
                  <xsl:choose>
                    <xsl:when test="$m='tei'"/>
                    <xsl:when test="count(current-group()/*)=0"/>
                    <xsl:otherwise>
                 <xsl:comment>Checking module <xsl:value-of select="$m"/></xsl:comment>

              <xsl:for-each select="current-group()[self::macroSpec]">
                        <xsl:sort select="@ident"/>
                        <xsl:if test="not(@mode='delete')">
                          <macroRef key="{@ident}"/>
                        </xsl:if>
                      </xsl:for-each>

                      <xsl:for-each select="current-group()[self::classSpec]">
                        <xsl:sort select="@ident"/>
                        <xsl:if test="not(@mode='delete')">
                          <classRef key="{@ident}"/>
                        </xsl:if>
                      </xsl:for-each>

                      <xsl:variable name="keepList">
                        <xsl:value-of select="current-group()[self::elementSpec][not(@mode='delete')]/@ident" separator=" "/>
                      </xsl:variable>
                     <xsl:if test='string-length($keepList) gt 1'> <moduleRef key="{$m}" include="{$keepList}"/>
                     </xsl:if>
                    </xsl:otherwise>
                  </xsl:choose>
                  <xsl:for-each select="current-group()[self::elementSpec]">
                    <xsl:variable name="e" select="@ident"/>
                    <!-- for every attribute, if its a class attribute, see if
	       its already deleted. if its a local attribute, see if its used. -->
                    <xsl:variable name="a">
                      <attList>
                        <xsl:for-each select="attDef">
                          <xsl:variable name="class" select="@class"/>
                          <xsl:variable name="ident" select="@ident"/>
                          <xsl:variable name="enumerated" select="@enumerated"/>
                          <xsl:for-each select="$stage1">
                            <xsl:choose>
                              <xsl:when
                                test="not($class='') and        $stage2/stage2/classSpec[@ident=$class]/attList/attDef[@ident=$ident and @mode='delete']"> </xsl:when>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='xml:id'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='xml:base'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='xml:space'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='xml:lang'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='n'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='rendition'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='style'"/>
                              <xsl:when
                                test="$keepGlobals='true' and $ident='rend'"/>
                              <xsl:when
                                test="not(key('UsedAtt',concat($ident,'@',$e)))">
                                <attDef ident="{$ident}" mode="delete"/>
                              </xsl:when>
                              <xsl:when test="$enumerated='true'">
                                <attDef ident="{$ident}" mode="change">
                                  <xsl:apply-templates
                                    select="key('UsedAtt',concat($ident,'@',$e))/valList"
                                  />
                                </attDef>
                              </xsl:when>
                            </xsl:choose>
                          </xsl:for-each>
                        </xsl:for-each>
                      </attList>
                    </xsl:variable>
                    <xsl:for-each select="$a">
                      <xsl:if test="attList/attDef">
                        <elementSpec ident="{$e}" mode="change">
                          <xsl:copy-of select="attList"/>
                        </elementSpec>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                  <xsl:copy-of
                    select="current-group()/elementSpec[@mode='delete']"/>
                  <xsl:apply-templates
                    select="current-group()[self::classSpec] "/>
                </xsl:for-each-group>
                <xsl:for-each select="$stage2/stage2/elementSpec[@mode='add']">
                  <xsl:text>


</xsl:text>
                  <xsl:comment>You've added an element '<xsl:value-of select="@ident"/>'
	   which does not seem to be a proper TEI element. This will make your TEI
	   documents non-conformant. If you really want to do this you should
	   add an elementSpec for it if you want your document to validate. It
	   would be better to use a separate namespace. </xsl:comment>
                  <xsl:text>

</xsl:text>
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </schemaSpec>
            </body>
          </text>
        </TEI>
      </xsl:variable>
      <xsl:apply-templates select="$stage3" mode="stage3"/>
<!--
    <xsl:result-document href="/tmp/stage0.xml">
    <xsl:copy-of select="$stage0"/>
  </xsl:result-document>
    <xsl:result-document href="/tmp/stage1.xml">
    <xsl:copy-of select="$stage1"/>
  </xsl:result-document>
    <xsl:result-document href="/tmp/stage2.xml">
    <xsl:copy-of select="$stage2"/>
  </xsl:result-document>
    <xsl:result-document href="/tmp/stage3.xml">
    <xsl:copy-of select="$stage3"/>
  </xsl:result-document>
-->
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="text()" mode="copy"/>
  <xsl:template match="@*" mode="copy">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="copy">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" mode="copy"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template name="checkUsed">
    <xsl:variable name="this" select="@ident"/>
    <xsl:for-each select="../member">
      <!--<xsl:message> CHECK       <xsl:value-of select="concat($this,'@',@ident)"/></xsl:message>-->
      <xsl:if test="key('UsedAtt',concat($this,'@',@ident))">
        <!--      <xsl:message>  ... FOUND  <xsl:value-of select="key('UsedAtt',concat($this,'@',@ident))/(ancestor-or-self::*[@ident]/@ident)"/></xsl:message>-->
        <xsl:value-of
          select="key('UsedAtt',concat($this,'@',@ident))/(ancestor-or-self::*[@ident]/@ident)"
        />
      </xsl:if>
    </xsl:for-each>
    <xsl:for-each select="../classmember">
      <xsl:for-each select="key('DOCIDENTS',@ident)/attDef">
        <xsl:call-template name="checkUsed"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="classmembers">
    <xsl:choose>
      <xsl:when test="@ident='att.global'">
        <xsl:for-each select="key('ELEMENTS',1)">
          <member ident="{@ident}"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="key('MEMBERS',@ident)">
          <member ident="{ancestor::elementSpec/@ident}"/>
        </xsl:for-each>
        <xsl:for-each select="key('CLASSMEMBERS',@ident)/ancestor::classSpec">
          <xsl:call-template name="classmembers"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="attributesFromClasses">
    <xsl:for-each select="classes/memberOf">
      <xsl:for-each select="key('IDENTS',@key)">
        <xsl:for-each select=".//tei:attDef">
          <attDef class="{ancestor::classSpec/@ident}">
            <xsl:copy-of select="@ident"/>
            <xsl:call-template name="checktype"/>
          </attDef>
        </xsl:for-each>
        <xsl:call-template name="attributesFromClasses"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="checktype">
    <xsl:attribute name="enumerated">
      <xsl:choose>
        <xsl:when test="ancestor::teiHeader and not($includeHeader='true')"
          >false</xsl:when>
        <xsl:when test="@ident = ($attributeList)">true</xsl:when>
        <xsl:when test="@ident='n'">false</xsl:when>
        <xsl:when test="@ident='rend' and $enumerateRend='true'">true</xsl:when>
        <xsl:when test="@ident='rendition' and $enumerateRend='true'"
          >true</xsl:when>
        <xsl:when test="@ident='type' and $enumerateType='true'">true</xsl:when>
        <xsl:when test="valList[@type='closed']">true</xsl:when>
        <xsl:when test="datatype/rng:ref[@name='data.enumerated']"
          >true</xsl:when>
        <!--
      <xsl:when
	  test="datatype/rng:ref[@name='data.word']">true</xsl:when>
      -->
        <xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="valList">
    <valList mode="add" type="closed">
      <xsl:for-each-group select="valItem" group-by="@ident">
        <xsl:sort select="@ident"/>
        <valItem ident="{current-grouping-key()}"/>
      </xsl:for-each-group>
    </valList>
  </xsl:template>
  <xsl:template match="classSpec">
    <xsl:choose>
      <xsl:when test="@mode='delete'"/>
      <xsl:when test="attList/attDef">
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:copy-of select="attList"/>
        </xsl:copy>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="@*|text()|comment()|processing-instruction()"
    mode="stage3">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*" mode="stage3">
    <xsl:copy>
      <xsl:apply-templates
        select="*|@*|processing-instruction()|comment()|text()" mode="stage3"/>
    </xsl:copy>
  </xsl:template>
  <!-- keep a class if someone else wants it -->
  <xsl:template match="classSpec/@module" mode="stage3"/>
  <xsl:template match="classSpec[@mode='delete']" mode="stage3">
    <xsl:copy>
      <xsl:copy-of select="@ident"/>
      <xsl:copy-of select="@type"/>
      <xsl:choose>
        <xsl:when
          test="../classSpec[not(@mode='delete') and @ident=current()/classmember/@ident]">
          <xsl:attribute name="mode">change</xsl:attribute>
          <xsl:copy-of select="attList"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="mode">delete</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>

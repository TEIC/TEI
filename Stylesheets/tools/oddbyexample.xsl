<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:n="www.example.com"
		xmlns:rng="http://relaxng.org/ns/structure/1.0"
		xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns="http://www.tei-c.org/ns/1.0"
		exclude-result-prefixes="rng tei n"
		xpath-default-namespace="http://www.tei-c.org/ns/1.0"
		version="2.0"><!--
This software is dual-licensed:

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

$Id$

2008, TEI Consortium
-->
  <!-- typical usage:
   saxon -it:main -o:myodd /usr/share/xml/tei/stylesheet/tools2/oddbyexample.xsl   corpus=`pwd`/

-->
  <!-- 
Read a corpus of TEI P5 documents and construct
an ODD customization file which expresses the subset
of the TEI you need to validate that corpus
-->

  <!-- How does this work?

1) start a variable and copy in all of the TEI 
2) read the corpus and get a list of all the elements and their
attributes that it uses, put that in the same variable
3) process the variable and read the TEI section. if an element or
 attribute is not present in the corpus section, put out a delete
 customization
4) for every attribute which is of type "enumerated", construct a
valList

-->

  <xsl:output indent="yes"/>
  <!-- name of odd -->
  <xsl:param name="schema">oddbyexample</xsl:param>
  <!-- whether to do all the global attributes -->
  <xsl:param name="keepGlobals">false</xsl:param>
  <!-- the document corpus -->
  <xsl:param name="corpus">./</xsl:param>
  <!-- the source of the TEI (just needs *Spec)-->
  <xsl:param name="defaultSource">http://www.tei-c.org/Vault/P5/current/xml/tei/odd/p5subset.xml</xsl:param>
  <!-- should we make valList for @rend -->
  <xsl:param name="enumerateRend">false</xsl:param>
  <xsl:param name="enumerateType">false</xsl:param>
  <!-- should we deal with non-TEI namespaces -->
  <xsl:param name="processNonTEI">false</xsl:param>
  <!-- which attributes should be make valLists for, regardless -->
  <xsl:param name="attributeList"/>
  <!-- do you want moduleRef generated with @include or @except? -->
  <xsl:param name="method">include</xsl:param>
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



  <xsl:variable name="checkAtts">
    <xsl:text>,</xsl:text>
    <xsl:value-of select="$attributeList"/>
    <xsl:text>,</xsl:text>
  </xsl:variable>
  <xsl:key name="Atts" match="@*" use="local-name(parent::*)"/>
  <xsl:key name="attVals" match="@*" use="concat(local-name(parent::*),local-name())"/>
  <xsl:key name="ELEMENTS" use="1" match="elementSpec"/>
  <xsl:key name="CLASSES" use="1" match="classSpec[@type='atts']"/>
  <xsl:key name="IDENTS" use="@ident" match="*[@ident]"/>
  <xsl:key name="MEMBERS" use="@key" match="elementSpec/classes/memberOf"/>
  <xsl:key name="CLASSMEMBERS" use="@key" match="classSpec/classes/memberOf"/>
  <xsl:key name="Used" use="@ident" match="docs/elementSpec"/>
  <xsl:key name="UsedAtt" use="concat(../@ident,@ident)" match="docs/elementSpec/attDef"/>
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
	     <xsl:value-of select="concat($corpus,'?select=*.',$suffix,';recurse=yes;on-error=warning')"/>
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
      <xsl:variable name="all">
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
      <xsl:variable name="stage3">
	<xsl:for-each select="$all/*">
	  <xsl:call-template name="processAll"/>
	</xsl:for-each>
      </xsl:variable>
      <xsl:variable name="stage4">
	<xsl:apply-templates select="$stage3" mode="stage3"/>
      </xsl:variable>
      <xsl:apply-templates select="$stage4" mode="stage4"/>
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
  

  <xsl:template name="processAll">
    <xsl:variable name="count">
      <xsl:value-of select="count(/n:ROOT/*)"/>
    </xsl:variable>
    <!-- assemble together all the TEI elements and attributes, 
     followed by all the
     elements and attributes used in the corpus -->
    <xsl:variable name="stage1">
      <stage1>
        <tei>
          <xsl:for-each select="document($defaultSource)">
            <xsl:for-each select="key('CLASSES',1)">
              <classSpec>
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
                <xsl:call-template name="classatts"/>
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
                <xsl:copy-of select="current-grouping-key()"/>
              </xsl:attribute>
              <xsl:for-each-group select="key('Atts',$ident)" group-by="local-name()">
                <attDef ident="{name()}">
                  <valList type="closed">
                    <xsl:for-each-group select="key('attVals',concat($ident,local-name()))" group-by=".">
                      <xsl:sort select="."/>
                      <xsl:choose>
                        <xsl:when test="contains(current-grouping-key(),' ')">
                          <xsl:for-each select="tokenize(current-grouping-key(),' ')">
                            <valItem ident="{.}"/>
                          </xsl:for-each>
                        </xsl:when>
                        <xsl:otherwise>
                          <valItem ident="{current-grouping-key()}"/>
                        </xsl:otherwise>
                      </xsl:choose>
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
        <!-- for every attribute class, see if its members should be
	     deleted, by seeing if they are used anywhere-->
        <xsl:for-each select="$stage1/stage1/tei/classSpec">
	  <xsl:variable name="classatts">
	    <xsl:for-each select="attDef">
	      <xsl:variable name="this" select="@ident"/>
	      <xsl:variable name="used">
		<xsl:for-each select="../member">
		  <xsl:if
		      test="key('UsedAtt',concat(@ident,$this))">
		  <xsl:text>true</xsl:text></xsl:if>
		</xsl:for-each>
	      </xsl:variable>
	      <xsl:choose>
		<xsl:when test="$keepGlobals='true' and $this='n'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='rendition'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='xml:id'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='xml:base'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='xml:space'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='xml:lang'"><keep/></xsl:when>
		<xsl:when test="$keepGlobals='true' and $this='rend'"><keep/></xsl:when>
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
	    <xsl:when test="$classatts/attDef[@mode='delete'] and not($classatts/keep)">
	      <classSpec ident="{@ident}" module="{@module}"
			 type="atts" mode="delete">
		<xsl:copy-of select="classmember"/>
		<attList>
		  <xsl:copy-of select="$classatts/attDef"/>
		</attList>
	      </classSpec>
	    </xsl:when>
	    <xsl:when test="$classatts/attDef[@mode='delete'] and $classatts/keep">
	      <classSpec ident="{@ident}" module="{@module}"
			 type="atts" mode="change">
		<attList>
		  <xsl:copy-of select="$classatts/attDef"/>
		</attList>
	      </classSpec>
	    </xsl:when>
	    <xsl:when test="$classatts/keep">
	      <classSpec ident="{@ident}" module="{@module}" type="atts" mode="keep"/>
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
                <elementSpec ident="{current-grouping-key()}" mode="add" ns="{namespace-uri()}">
                  <xsl:text>&#10;</xsl:text>
                  <xsl:comment>add an &lt;equiv/&gt; to point to an named  template
		  in an XSLT file which will transform this to pure TEI</xsl:comment>
                  <xsl:text>&#10;</xsl:text>
                  <equiv filter="somefile.xsl" mimeType="text/xsl" name="{current-grouping-key()}"/>
                  <desc>
                    <xsl:text>&#10;</xsl:text>
                    <xsl:comment> Describe the <xsl:value-of select="current-grouping-key()"/> element  here</xsl:comment>
                    <xsl:text>&#10;</xsl:text>
                  </desc>
                  <classes>
                    <xsl:text>&#10;</xsl:text>
                    <xsl:comment> Add a 'memberOf key="model.className"' stanza for whatever classes it belongs to  here</xsl:comment>
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
                      <xsl:for-each-group select="key('Atts',local-name())" group-by="local-name()">
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
    <TEI xml:lang="en">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>ODD by Example customization</title>
          </titleStmt>
          <editionStmt>
            <edition>generated on     
	   <date><xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[m02]:[s02]Z')"/></date>
	                 </edition>
          </editionStmt>
          <publicationStmt>
            <p> </p>
          </publicationStmt>
          <sourceDesc>
            <p>generated by oddbyexample.xsl, based on analyzing 
	    <xsl:value-of select="$count"/> files from
	    <xsl:value-of select="$corpus"/>
                  </p>
          </sourceDesc>
        </fileDesc>
      </teiHeader>
      <text>
        <body>
<!--
	  <xsl:result-document href="/tmp/stage1.xml">
	    <xsl:copy-of select="$stage1"/>
	  </xsl:result-document>
	  <xsl:result-document href="/tmp/stage2.xml">
	    <xsl:copy-of select="$stage2"/>
	  </xsl:result-document>
-->
          <schemaSpec ident="{$schema}">
            <xsl:attribute name="start">
              <xsl:if test="$stage2/stage2/elementSpec[@mode='keep' and @ident='TEI']">TEI</xsl:if>
              <xsl:text> </xsl:text>
              <xsl:if test="$stage2/stage2/elementSpec[@mode='keep' and @ident='teiCorpus']">teiCorpus</xsl:if>
            </xsl:attribute>
            <moduleRef key="tei"/>
            <xsl:apply-templates select="$stage2/stage2/classSpec[@module='tei']"/>
            <moduleRef key="core"/>
            <!-- we need to list only modules from which elements or classes have been used -->
            <xsl:for-each-group select="$stage2/stage2/*[@mode='keep']" group-by="@module">
              <xsl:sort select="current-grouping-key()"/>
              <xsl:choose>
                <xsl:when test="@module='core'"/>
                <xsl:otherwise>
                  <moduleRef key="{@module}"/>
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
                          <xsl:when test="not($class='') and
					  $stage2/stage2/classSpec[@ident=$class]/attList/attDef[@ident=$ident
					  and @mode='delete']">			 
		       </xsl:when>
		       <xsl:when test="$keepGlobals='true' and $ident='xml:id'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='xml:base'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='xml:space'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='xml:lang'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='n'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='rendition'"/>
		       <xsl:when test="$keepGlobals='true' and $ident='rend'"/>
		       <xsl:when test="not(key('UsedAtt',concat($e,$ident)))">
			 <attDef ident="{$ident}" mode="delete"/>
		       </xsl:when>
		       <xsl:when test="$enumerated='true'">
			 <attDef ident="{$ident}" mode="change">
			   <xsl:apply-templates select="key('UsedAtt',concat($e,$ident))/valList"/>
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
		  select="$stage2/stage2/elementSpec[@mode='delete' and @module=current-grouping-key()]"/>
              <xsl:apply-templates select="$stage2/stage2/classSpec[@module=current-grouping-key()]"/>
            </xsl:for-each-group>
            <xsl:for-each select="$stage2/stage2/elementSpec[@mode='add']">
              <xsl:text>


</xsl:text>
              <xsl:comment>You've added an element '<xsl:value-of select="@ident"/>'
	   which does not seem to be a proper TEI element. This will make your TEI
	   documents non-conformant, but if you really want to do this you should
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
  </xsl:template>

  <xsl:template name="classmembers">
    <xsl:choose>
      <xsl:when test="@ident='att.global'">
        <xsl:for-each select="key('ELEMENTS',1)">
          <xsl:sort select="@ident"/>
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
  <xsl:template name="classatts">
    <xsl:for-each select="classes/memberOf">
      <xsl:for-each select="key('IDENTS',@key)">
        <xsl:for-each select=".//tei:attDef">
          <attDef class="{ancestor::classSpec/@ident}">
            <xsl:copy-of select="@ident"/>
            <xsl:call-template name="checktype"/>
          </attDef>
        </xsl:for-each>
        <xsl:call-template name="classatts"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="checktype">
    <xsl:attribute name="enumerated">
      <xsl:choose>
        <xsl:when test="contains($checkAtts,concat(',',@ident,','))">true</xsl:when>
        <xsl:when test="@ident='n'">false</xsl:when>
        <xsl:when test="@ident='rend' and $enumerateRend='true'">true</xsl:when>
        <xsl:when test="@ident='type' and $enumerateType='true'">true</xsl:when>
        <xsl:when test="valList[@type='closed']">true</xsl:when>
        <xsl:when test="datatype/rng:ref[@name='data.enumerated']">true</xsl:when>
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
      <xsl:when test="@mode='delete'">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:copy-of select="classmember"/>
	  <xsl:copy-of select="attList"/>
	</xsl:copy>
      </xsl:when>
      <xsl:when test="attList/attDef">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:copy-of select="attList"/>
	</xsl:copy>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


<!-- odd 2 nuodd -->

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="stage3">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@*|text()|comment()|processing-instruction()" mode="stage4">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="stage3">
    <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()"  mode="stage3"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*" mode="stage4">
    <xsl:copy>
      <xsl:apply-templates
	  select="*|@*|processing-instruction()|comment()|text()" mode="stage4"/>
    </xsl:copy>
  </xsl:template>
  
  
  <!-- ignore elementSpec @mode='delete' -->
  <xsl:template match="elementSpec[@mode='delete']"  mode="stage3"/>
  <xsl:template match="classSpec[@mode='delete']"  mode="stage3">
    <xsl:copy>
      <xsl:copy-of select="@ident"/>
      <xsl:copy-of select="@module"/>
      <xsl:copy-of select="@type"/>
      <xsl:choose>
	<xsl:when
	    test="../classSpec[@ident=current()/classmember/@ident]">
	  <xsl:attribute name="mode">change</xsl:attribute>
	  <xsl:copy-of select="attList"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="mode">delete</xsl:attribute>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="moduleRef/@include"  mode="stage3"/>
  <xsl:template match="moduleRef/@except"  mode="stage3"/>

  <!-- for any moduleRef, look up all the members of it in P5;
       if they are not deleted by this odd, add them to a list to be
       included -->
  <xsl:template match="moduleRef[@key]" mode="stage3">
    <xsl:variable name="orig" select="/"/>
    <xsl:variable name="here" select="."/>
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="stage3"/>
      <xsl:variable name="module" select="@key"/>
      <xsl:choose>
	<xsl:when test="$method='include' and @include">
	  <xsl:copy-of select="@include"/>
	</xsl:when>	
	<xsl:when test="$method='except' and @except">
	  <xsl:copy-of select="@except"/>
	</xsl:when>	
	<xsl:when test="$method='include' and @except">
	  <xsl:variable name="not">
	    <xsl:for-each select="tokenize($here/@except,' ')">
	      <not ident="{.}"/>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:if test="not($not/not[@ident=$e])">
		  <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		</xsl:if>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($includelist='')">
	    <xsl:attribute name="include" select="normalize-space($includelist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='include'">
	  <xsl:variable name="includelist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:choose>
		  <xsl:when test="key('deletedE',$e)"/>
		  <xsl:otherwise>
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:otherwise>
		  </xsl:choose>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($includelist='')">
	    <xsl:attribute name="include"
			   select="normalize-space($includelist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='except' and @include">
	  <xsl:variable name="yes">
	    <xsl:for-each select="tokenize($here/@include,' ')">
	      <yes ident="{.}"/>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:variable name="exceptlist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="not($yes/yes[@ident=$e])">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($exceptlist='')">
	    <xsl:attribute name="except"  select="normalize-space($exceptlist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:when test="$method='except'">
	  <xsl:variable name="exceptlist">
	    <xsl:for-each select="document($defaultSource)">
	      <xsl:for-each select="key('EbyM',$module)">
		<xsl:sort select="@ident"/>
		<xsl:variable name="e" select="@ident"/>
		<xsl:for-each select="$orig">
		  <xsl:if test="key('deletedE',$e)">
		    <xsl:value-of select="$e"/>
		    <xsl:text> </xsl:text>
		  </xsl:if>
		</xsl:for-each>
	      </xsl:for-each>
	    </xsl:for-each>
	  </xsl:variable>
	  <xsl:if test="not($exceptlist='')">
	    <xsl:attribute name="except" select="normalize-space($exceptlist)"/>
	  </xsl:if>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:message terminate="yes">Method <xsl:value-of
	  select="$method"/> not supported</xsl:message>
	</xsl:otherwise>
      </xsl:choose>
	</xsl:copy>
      </xsl:template>

  <xsl:template mode="stage4" match="specGrp[not(*)]"/>

  <xsl:template mode="stage4" match="specGrpRef">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
	<xsl:for-each
	    select="id(substring(@target,2))">
	  <xsl:if test="*">
	    <specGrpRef target="#{@xml:id}"/>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:function name="tei:shallWeKeep"/>

</xsl:stylesheet>

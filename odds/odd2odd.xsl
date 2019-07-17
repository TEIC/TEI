<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:sch="http://purl.oclc.org/dsdl/schematron" 
    xmlns:s="http://www.ascc.net/xml/schematron" 
    xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="2.0" 
    exclude-result-prefixes="#all">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for merging TEI ODD specification with source to
      make a new source document. </p>
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

  <xsl:output encoding="utf-8" indent="no"/>
  <xsl:param name="autoGlobal">false</xsl:param>
  <xsl:param name="configDirectory"/>
  <xsl:param name="currentDirectory"/>
  <xsl:param name="defaultSource"/>
  <xsl:param name="defaultTEIServer">https://www.tei-c.org/Vault/P5/</xsl:param>
  <xsl:param name="defaultTEIVersion">current</xsl:param>
  <xsl:param name="doclang"/>
  <xsl:param name="selectedSchema"/>
  <xsl:param name="stripped">false</xsl:param>
  <xsl:param name="useVersionFromTEI">true</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <!-- following param, added 2016-06-06 by Syd Bauman for use by TEI
       in Libraries Best Practices Guidelines. If set to 'true' then
       all <exmplum> elements from TEI source or summarily dropped,
       whereas <exemplum> elements in ODD customization file are
       copied through. -->
  <xsl:param name="suppressTEIexamples">false</xsl:param>
  <xsl:key name="odd2odd-CHANGEATT" match="tei:attDef[@mode eq 'change']" use="concat(../../@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-CHANGECONSTRAINT" match="tei:constraintSpec[@mode eq 'change']" use="concat(../@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-CLASS_MEMBERED" use="tei:classes/tei:memberOf/@key" match="tei:classSpec"/>
  <xsl:key name="odd2odd-DELETEATT" match="tei:attDef[@mode eq 'delete']" use="concat(ancestor::tei:classSpec/@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-DELETEATT" match="tei:attDef[@mode eq 'delete']" use="concat(ancestor::tei:elementSpec/@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-DELETECONSTRAINT" match="tei:constraintSpec[@mode eq 'delete']" use="concat(../@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-ELEMENT_MEMBERED" use="tei:classes/tei:memberOf/@key" match="tei:elementSpec"/>
  <xsl:key name="odd2odd-IDENTS" match="tei:dataSpec" use="@ident"/>
  <xsl:key name="odd2odd-IDENTS" match="tei:macroSpec" use="@ident"/>
  <xsl:key name="odd2odd-IDENTS" match="tei:classSpec" use="@ident"/>
  <xsl:key name="odd2odd-IDENTS" match="tei:elementSpec" use="@ident"/>
  <xsl:key name="odd2odd-MACROS" use="@ident" match="tei:macroSpec"/>
  <xsl:key name="odd2odd-MEMBEROFADD" match="tei:memberOf[@mode eq 'add' or not (@mode)]" use="concat(../../@ident,@key)"/>
  <xsl:key name="odd2odd-MEMBEROFDELETE" match="tei:memberOf[@mode eq 'delete']" use="concat(../../@ident,@key)"/>
  <xsl:key name="odd2odd-MODULES" match="tei:moduleRef" use="@key"/>
  <xsl:key name="odd2odd-MODULE_MEMBERS_ELEMENT" match="tei:elementSpec" use="@module"/>
  <xsl:key name="odd2odd-MODULE_MEMBERS_NONELEMENT" match="tei:dataSpec"  use="@module"/>
  <xsl:key name="odd2odd-MODULE_MEMBERS_NONELEMENT" match="tei:macroSpec"  use="@module"/>
  <xsl:key name="odd2odd-MODULE_MEMBERS_NONELEMENT" match="tei:classSpec" use="@module"/>
  <xsl:key name="odd2odd-ATTREFED" use="substring-before(@name,'.attribute.')" match="tei:attRef"/>
  <xsl:key name="odd2odd-REFED" use="@name" match="rng:ref[ancestor::tei:elementSpec]"/>
  <xsl:key name="odd2odd-REFED" use="@name" match="rng:ref[ancestor::tei:macroSpec and not(@name=ancestor::tei:macroSpec/@ident)]"/>
  <xsl:key name="odd2odd-REFED" use="@name" match="rng:ref[ancestor::tei:datatype]"/>
  <xsl:key name="odd2odd-REFED" use="@class" match="tei:attRef"/>
  <xsl:key name="odd2odd-REFED" use="substring-before(@name,'_')" match="rng:ref[contains(@name,'_')]"/>
  <xsl:key name="odd2odd-REFED" use="@key" match="tei:dataRef"/>
  <xsl:key name="odd2odd-REFED" use="@key" match="tei:macroRef"/>
  <xsl:key name="odd2odd-REFED" use="@key" match="tei:classRef"/>
  <xsl:key name="odd2odd-REFED" use="@key" match="tei:elementRef"/>

  <xsl:key name="odd2odd-REFOBJECTS" use="@key" match="tei:schemaSpec/tei:macroRef[not(ancestor::tei:content)]"/>
  <xsl:key name="odd2odd-REFOBJECTS" use="@key" match="tei:schemaSpec/tei:classRef[not(ancestor::tei:content)]"/>
  <xsl:key name="odd2odd-REFOBJECTS" use="@key" match="tei:schemaSpec/tei:elementRef[not(ancestor::tei:content)]"/>
  <xsl:key name="odd2odd-REPLACECONSTRAINT" match="tei:constraintSpec[@mode eq 'replace']" use="concat(../@ident,'_',@ident)"/>
  <xsl:key name="odd2odd-SCHEMASPECS" match="tei:schemaSpec" use="@ident"/>
  <xsl:key match="tei:moduleSpec" name="odd2odd-MODULES" use="@ident"/>


   <!-- all of these use a combination of @ident _and_ @ns (where
   present), in case of duplication of names across schemes -->

  <xsl:key match="tei:schemaSpec" name="LISTSCHEMASPECS" use="1"/>
  
  <xsl:variable name="whichSchemaSpec">
    <xsl:choose>
      <xsl:when test="not($selectedSchema='')">
        <xsl:value-of select="$selectedSchema"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="key('LISTSCHEMASPECS',1)[1]/@ident"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:key name="odd2odd-CHANGE"     match="tei:classSpec[@mode eq 'change'][ancestor::tei:schemaSpec[@ident=$whichSchemaSpec]]" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-CHANGE"     match="tei:dataSpec[@mode eq 'change'][ancestor::tei:schemaSpec[@ident=$whichSchemaSpec]]"   use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-CHANGE"     match="tei:elementSpec[@mode eq 'change'][ancestor::tei:schemaSpec[@ident=$whichSchemaSpec]]" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-CHANGE"     match="tei:macroSpec[@mode eq 'change'][ancestor::tei:schemaSpec[@ident=$whichSchemaSpec]]"   use="tei:uniqueName(.)"/>

  <xsl:key name="odd2odd-DELETE"   match="tei:classSpec[@mode eq 'delete']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-DELETE"   match="tei:macroSpec[@mode eq 'delete']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-DELETE"   match="tei:dataSpec[@mode eq 'delete']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-DELETE"   match="tei:elementSpec[@mode eq 'delete']" use="tei:uniqueName(.)"/>

  <xsl:key name="odd2odd-REPLACE"  match="tei:classSpec[@mode eq 'replace']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-REPLACE"  match="tei:dataSpec[@mode eq 'replace']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-REPLACE" match="tei:elementSpec[@mode eq 'replace']" use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-REPLACE"  match="tei:macroSpec[@mode eq 'replace']"   use="tei:uniqueName(.)"/>
  <xsl:key name="odd2odd-REPLACEATT"     match="tei:attDef[@mode eq 'replace']" use="concat(../../@ident,'_',@ident)"/>


  <xsl:variable name="DEFAULTSOURCE">
    <xsl:choose>
      <xsl:when test="$defaultSource != ''">
        <xsl:choose>
          <xsl:when test="starts-with($defaultSource, '&quot;') and ends-with($defaultSource, '&quot;')">
            <xsl:value-of select="substring($defaultSource, 2, string-length($defaultSource)-2)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$defaultSource"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="$configDirectory != ''">
        <xsl:value-of select="$configDirectory"/>
        <xsl:text>odd/p5subset.xml</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$defaultTEIServer"/>
        <xsl:value-of select="$defaultTEIVersion"/>
        <xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- NOTE on functions added 2016-12-02 by Syd: -->
  <!-- Many, if not most, of the functions below duplicate in name -->
  <!-- functions that are in teiodds.xsl. odd2relax, odd2dtd, odd2html, -->
  <!-- and even odd2json & odd2lite import that file. But this one -->
  <!-- does not. I do not know if the functions are slightly different, -->
  <!-- or if there is some other reason this file does not import -->
  <!-- teiodds.xsl. Someday I hope to test this out and do the right -->
  <!-- thing (either import that file so there is only 1 definition -->
  <!-- of each function, or add documentation explaining why not and -->
  <!-- perhaps re-name the functions so the difference is clear). But -->
  <!-- for now, since I'm in a rush, I'm just following the lead of -->
  <!-- what's here already, and copying my new function from teiodds to -->
  <!-- here. -->

  <xsl:function name="tei:includeMember" as="xs:boolean">
    <xsl:param name="ident"  as="xs:string"/>
    <xsl:param name="exc" />
    <xsl:param name="inc" />
      <xsl:choose>
        <xsl:when test="not($exc) and not($inc)">true</xsl:when>
        <xsl:when test="$inc and $ident cast as xs:string  = tokenize($inc, ' ')">true</xsl:when>
        <xsl:when test="$inc">false</xsl:when>
        <xsl:when test="$exc and $ident cast as xs:string   = tokenize($exc, ' ')">false</xsl:when>
        <xsl:otherwise>true</xsl:otherwise>
      </xsl:choose>
  </xsl:function>


  <xsl:function name="tei:checkExclude" as="xs:boolean">
    <xsl:param name="ident"  as="xs:string"/>
    <xsl:param name="exc" />
      <xsl:choose>
        <xsl:when test="not($exc)">true</xsl:when>
        <xsl:when test="$exc and $ident cast as xs:string   = tokenize($exc, ' ')">false</xsl:when>
        <xsl:otherwise>true</xsl:otherwise>
      </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:workOutSource" as="xs:string*">
    <xsl:param name="e"/>
    <xsl:variable name="loc">
      <xsl:choose>
        <xsl:when test="$e/@source">
          <xsl:value-of select="$e/@source"/>
        </xsl:when>
        <xsl:when test="$e/ancestor::tei:schemaSpec/@source">
          <xsl:value-of select="$e/ancestor::tei:schemaSpec/@source"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$DEFAULTSOURCE"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="source">
      <xsl:choose>
        <xsl:when test="starts-with($loc,'file:')">
          <xsl:value-of select="$loc"/>
        </xsl:when>
        <xsl:when test="starts-with($loc,'http:')">
          <xsl:value-of select="$loc"/>
        </xsl:when>
        <xsl:when test="starts-with($loc,'https:')">
          <xsl:value-of select="$loc"/>
        </xsl:when>
        <xsl:when test="starts-with($loc,'/')">
          <xsl:value-of select="resolve-uri($loc, 'file:///')"/>
        </xsl:when>
        <xsl:when test="starts-with($loc,'tei:')">
          <xsl:value-of select="replace($loc,'tei:',$defaultTEIServer)"/>
          <xsl:text>/xml/tei/odd/p5subset.xml</xsl:text>
        </xsl:when>
        <xsl:when test="base-uri($top)=''">
          <xsl:value-of select="$currentDirectory"/>
          <xsl:value-of select="$loc"/>
        </xsl:when>
        <xsl:when test="$currentDirectory=''">
          <xsl:value-of select="resolve-uri($loc,base-uri($top))"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="resolve-uri(string-join(($currentDirectory, $loc), '/'),base-uri($top))"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="not(doc-available($source))">
        <xsl:call-template name="die">
          <xsl:with-param name="message">
            <xsl:text>Source </xsl:text>
            <xsl:value-of select='($source,$loc,name($top),base-uri($top))' separator=" + "/>
            <xsl:text> not readable</xsl:text>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Setting source document to <xsl:value-of select="$source"/></xsl:message>
        </xsl:if>
        <xsl:sequence select="$source"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:message" as="xs:string">
    <xsl:param name="message"/>
    <xsl:message><xsl:copy-of select="$message"/></xsl:message>
    <xsl:text/>
  </xsl:function>

  <xsl:function name="tei:uniqueName" as="xs:string">
    <xsl:param name="e"/>
    <xsl:for-each select="$e">
      <xsl:sequence select="concat(
        if (@ns eq 'http://www.tei-c.org/ns/1.0') then ''
        else if (@ns) then @ns
        else if (ancestor::tei:schemaSpec/@ns) then
        ancestor::tei:schemaSpec/@ns else '',@ident)"/>
    </xsl:for-each>
  </xsl:function>

  <xsl:template name="die">
    <xsl:param name="message"/>
    <xsl:message terminate="yes">
      <xsl:text>Error: odd2odd.xsl: </xsl:text> 
      <xsl:value-of select="$message"/>
    </xsl:message>
  </xsl:template>
  
  <xsl:function name="tei:minOmaxO" as="xs:integer+">
    <!-- Input: the string values of the attributes @minOccurs and -->
    <!--        @maxOccurs  -->
    <!-- Oputput: a sequence of 2 integers representing the integer -->
    <!--          values thereof with -1 used to indicate "unbounded" -->
    <xsl:param name="minOccurs"/>
    <xsl:param name="maxOccurs"/>
    <!-- get the value of @minOccurs, defaulting to "1" -->
    <xsl:variable name="minOccurs" select="( $minOccurs, '1')[1]"/>
    <!-- get the value of @maxOccurs, defaulting to "1" -->
    <xsl:variable name="maxOccurs" select="( $maxOccurs, '1')[1]"/>
    <!-- We now have two _string_ representations of the attrs, but -->
    <!-- we need integers. So cast them, converting "unbounded" to  -->
    <!-- a special flag value (-1): -->
    <xsl:variable name="min" select="xs:integer( $minOccurs )"/>
    <xsl:variable name="max">
      <xsl:choose>
        <xsl:when test="$maxOccurs castable as xs:integer">
          <xsl:value-of select="xs:integer( $maxOccurs )"/>
        </xsl:when>
        <xsl:otherwise>
          <!-- Must be "unbounded". -->
          <xsl:value-of select="-1"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:sequence select="( $min, $max )"/>
  </xsl:function>
  
  <xsl:variable name="ODD">
    <xsl:for-each select="/*">
      <xsl:copy>
              <xsl:attribute name="xml:base" select="document-uri(/)"/>
        <xsl:copy-of select="@*"/>
        <xsl:if test="$useVersionFromTEI='true'">
          <xsl:processing-instruction name="TEIVERSION">
            <!-- 
                 Generate a string based on the fileDesc/editionStmt/edition
                 element(s) in the p5subset.xml file. (Note that this would
                 look ugly if there were more than one editionStmt/edition,
                 but there is only one in that file.)
                 See ticket https://github.com/TEIC/Stylesheets/issues/355.
            -->
            <xsl:call-template name="odd2odd-getversion"/>
          </xsl:processing-instruction>
        </xsl:if>
        <xsl:apply-templates mode="pass0"/>
      </xsl:copy>
    </xsl:for-each>
  </xsl:variable>

  <xsl:variable name="top" select="/"/>

  <xsl:template match="/">
    <!--
        <xsl:result-document href="/tmp/odd2odd-pass0.xml">
          <xsl:copy-of select="$ODD"/>
        </xsl:result-document>
        -->
    <xsl:apply-templates mode="pass1" select="$ODD"/>
  </xsl:template>

  <!-- ******************* Pass 0, follow and expand specGrp ********************************* -->

  <xsl:template match="tei:specGrp" mode="pass0">
    <xsl:if test="$verbose='true'">
      <xsl:message>Phase 0: summarize specGrp <xsl:value-of select="@xml:id"/>
      </xsl:message>
    </xsl:if>
    <xsl:if test="tei:specGrpRef|tei:elementSpec|tei:classSpec|tei:macroSpec|tei:dataSpec|tei:moduleRef">
    <table xmlns="http://www.tei-c.org/ns/1.0" rend="specGrpSummary">
      <xsl:for-each select="tei:specGrpRef|tei:elementSpec|tei:classSpec|tei:macroSpec|tei:dataSpec|tei:moduleRef">
        <row>
          <xsl:choose>
            <xsl:when test="self::tei:specGrpRef">
              <cell>
                <ref target="#{@target}">reference <xsl:value-of select="@target"/></ref>
              </cell>
              <cell/>
            </xsl:when>
            <xsl:when test="self::tei:elementSpec">
              <cell>
                Element <gi><xsl:value-of select="@ident"/></gi>
              </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:classSpec">
              <cell>
                Class <ident type="class"><xsl:value-of select="@ident"/></ident>
              </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:dataSpec">
              <cell>
                Data <ident type="macro"><xsl:value-of select="@ident"/></ident>
              </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:macroSpec">
              <cell>
                Macro <ident type="macro"><xsl:value-of select="@ident"/></ident>
              </cell>
              <cell>
                <xsl:value-of select="@mode"/>
              </cell>
            </xsl:when>
            <xsl:when test="self::tei:moduleRef">
              <cell>
                Module <xsl:value-of select="@key"/>
              </cell>
              <cell/>
            </xsl:when>
          </xsl:choose>
        </row>
      </xsl:for-each>
    </table>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:schemaSpec" mode="pass0">
    <xsl:if test="@ident=$selectedSchema or ($selectedSchema='' and not(preceding-sibling::tei:schemaSpec))">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <!-- generate a @defaultExceptions attribute if it's not present -->
        <xsl:if test="not(@defaultExceptions)">
          <xsl:variable name="defval" select="document(tei:workOutSource(.))//tei:elementSpec[@ident='schemaSpec']//tei:attDef[@ident='defaultExceptions']/tei:defaultVal"/>
          <xsl:for-each select="tokenize($defval, '\s+')">
            <xsl:if test="matches(., '\w(\w|\d)+:(\w|\d)+')">
              <xsl:namespace name="{substring-before(., ':')}" select="namespace-uri-for-prefix(substring-before(., ':'), $defval)" ></xsl:namespace>
            </xsl:if>
          </xsl:for-each>
          <xsl:if test="$defval">
            <xsl:attribute name="defaultExceptions" select="$defval"/>
          </xsl:if>
        </xsl:if>
        <xsl:choose>
          <xsl:when test="@source">
            <xsl:if test="$verbose='true'">
              <xsl:message>Source for TEI is <xsl:value-of select="@source"/></xsl:message>
            </xsl:if>
          </xsl:when>
          <xsl:otherwise>
            <xsl:if test="$verbose='true'">
              <xsl:message>Source for TEI will be set to <xsl:value-of select="$DEFAULTSOURCE"/> </xsl:message>
            </xsl:if>
            <xsl:attribute name="source">
              <xsl:value-of select="$DEFAULTSOURCE"/>
            </xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates select="*|text()|processing-instruction()" mode="pass0"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:specGrpRef" mode="pass0">
    <xsl:sequence select="if ($verbose='true')then tei:message(concat('Phase 0: expand specGrpRef ',@target)) else ()"/>
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#')">
        <xsl:apply-templates  mode="pass0"
                              select="id(substring(@target,2))/*"/>
      </xsl:when>
      <xsl:otherwise>
          <xsl:if test="$verbose='true'">
            <xsl:sequence select="tei:message(concat('... read from ',resolve-uri(@target,base-uri($top))))"/>
          </xsl:if>
        <xsl:for-each 
            select="doc(resolve-uri(@target,base-uri($top)))">
          <xsl:choose>
            <xsl:when test="tei:specGrp">
              <xsl:apply-templates select="tei:specGrp/*" mode="pass0"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates  mode="pass0"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="@*|processing-instruction()|text()|comment()" mode="pass0">
      <xsl:copy/>
  </xsl:template>  
  <xsl:template match="*" mode="pass0">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="pass0"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:elementSpec[@mode eq 'change']|tei:classSpec[@mode eq 'change']|tei:macroSpec[@mode eq 'change']|tei:dataSpec[@mode eq 'change']" mode="pass0">    
    <xsl:choose>
      <xsl:when test="count(key('odd2odd-CHANGE',@ident))&gt;1">
        <xsl:if
          test="generate-id(.)=generate-id(key('odd2odd-CHANGE',@ident)[1])">
          <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:for-each select="key('odd2odd-CHANGE',@ident)">
              <xsl:apply-templates
                select="*|text()|comment()|processing-instruction()"
                mode="pass0"/>
            </xsl:for-each>
          </xsl:copy>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="*|text()|comment()|processing-instruction()" mode="pass0"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- ******************* Phase 1, expand schemaSpec ********************************* -->

  <xsl:template match="tei:schemaSpec" mode="pass1">
    <xsl:variable name="pass1">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:sequence select="if ($verbose='true')then
          tei:message(concat('Schema pass 1: ',@ident)) else ()"/>
        
        <!-- 
          it is important to process "tei" and "core" first 
          because of the order of declarations
        -->
        <xsl:for-each select="tei:moduleRef[@key eq 'tei']">
          <xsl:call-template name="odd2odd-expandModule"/>
        </xsl:for-each>
        
        <xsl:for-each select="tei:moduleRef[@key eq 'core']">
          <xsl:call-template name="odd2odd-expandModule"/>
        </xsl:for-each>
        
        <xsl:for-each select="tei:moduleRef[@key]">
          <xsl:if test="not(@key eq 'tei' or @key eq 'core')">
            <xsl:call-template name="odd2odd-expandModule"/>
          </xsl:if>
        </xsl:for-each>
        <xsl:for-each
          select="*[not(self::tei:moduleRef[@key])]">
          <xsl:apply-templates select="." mode="pass1"/>
        </xsl:for-each>
      </xsl:copy>
    </xsl:variable>
    <!--
        <xsl:result-document href="/tmp/odd2odd-pass1.xml">
          <xsl:copy-of select="$pass1"/>
        </xsl:result-document>
    -->
    <xsl:for-each select="$pass1">
      <xsl:apply-templates mode="pass2"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="*" mode="pass1">
    <xsl:copy>
      <xsl:apply-templates mode="pass1" select="@*|*|processing-instruction()|comment()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|processing-instruction()|text()|comment()" mode="pass1">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:elementSpec[@mode eq 'delete']|tei:classSpec[@mode eq 'delete']|tei:macroSpec[@mode eq 'delete']|tei:dataSpec[@mode eq 'delete']"
                mode="pass1">
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 1: remove <xsl:value-of select="@ident"/></xsl:message>
        </xsl:if>
  </xsl:template>

  <xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec|tei:dataSpec"
                mode="pass1">
    <xsl:variable name="specName" select="@ident"/>
    <xsl:choose>
      <xsl:when test="$ODD/key('odd2odd-DELETE',$specName)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 1: remove <xsl:value-of select="$specName"/></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 1: hang onto <xsl:value-of
          select="$specName"/> <xsl:if test="@mode"> in mode <xsl:value-of
          select="@mode"/></xsl:if></xsl:message>
        </xsl:if>
        <xsl:copy>
          <xsl:apply-templates mode="pass1" select="@*|*|processing-instruction()|comment()|text()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template mode="pass1"
                match="tei:schemaSpec//tei:classSpec[  @mode eq 'add' or not(@mode) ]
                     | tei:schemaSpec//tei:macroSpec[  @mode eq 'add' or not(@mode) ]
                     | tei:schemaSpec//tei:dataSpec [  @mode eq 'add' or not(@mode) ]
                     | tei:schemaSpec//tei:elementSpec[@mode eq 'add' or not(@mode) ]
                     ">
    <xsl:call-template name="odd2odd-createCopy"/>
  </xsl:template>

  <xsl:template match="tei:dataRef|tei:macroRef|tei:classRef|tei:elementRef"
    mode="pass1">       
    <xsl:choose>
      <xsl:when test="ancestor::tei:content">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="ancestor::tei:datatype">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="@name">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="sourceDoc" select="tei:workOutSource(.)"/>
        <xsl:variable name="name" select="@key"/>
        <xsl:variable name="id" select="ancestor::*[@ident]/@ident"/>
        <xsl:for-each select="document($sourceDoc,$top)">
          <xsl:choose>
            <xsl:when test="key('odd2odd-IDENTS',$name)">
              <xsl:for-each select="key('odd2odd-IDENTS',$name)">
                <xsl:if test="$verbose='true'">
                  <xsl:message>Phase 1: import <xsl:value-of  select="$name"/> by direct reference</xsl:message>
                </xsl:if>
                <xsl:apply-templates mode="pass1" select="."/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="die">
                <xsl:with-param name="message">
                  <xsl:text>Reference to </xsl:text>
                  <xsl:value-of select="$name"/>
                  <xsl:text> in </xsl:text>
                  <xsl:value-of select="$id"/>
                  <xsl:text>: not found in source</xsl:text>
                </xsl:with-param>
              </xsl:call-template>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="odd2odd-expandModule">
    <xsl:variable name="sourceDoc" select="tei:workOutSource(.)"/>
    <xsl:variable name="name" select="@key"/>
    <xsl:variable name="exc" select="@except"/>
    <xsl:variable name="inc"  select="@include"/>
    <xsl:sequence select="if ($verbose='true') then
                          tei:message(concat('Process module reference to [',@key,'] with exclusion/inclusion of [',@except,'/',@include,']')) else ()"/>
          <xsl:for-each select="document($sourceDoc,$top)">
            
            <!-- get model and attribute classes regardless -->
            <xsl:for-each select="key('odd2odd-MODULE_MEMBERS_NONELEMENT',$name)">
              <xsl:variable name="class" select="@ident"/>
              <xsl:if test="not($ODD/key('odd2odd-REFOBJECTS',$class))">
                <xsl:if test="$verbose='true'">
                  <xsl:message>Phase 1: import <xsl:value-of select="$class"/> by moduleRef</xsl:message>
                </xsl:if>
                <xsl:apply-templates mode="pass1" select="."/>
              </xsl:if>
            </xsl:for-each>

            <!-- now elements -->
            <xsl:for-each select="key('odd2odd-MODULE_MEMBERS_ELEMENT',$name)">
              <xsl:variable name="i" select="@ident"/>
              <xsl:if test="tei:includeMember(@ident,$exc,$inc)
                and not($ODD/key('odd2odd-REFOBJECTS',$i))">
                <xsl:if test="$verbose='true'">
                  <xsl:message>Phase 1: import <xsl:value-of
                    select="$i"/> by moduleRef</xsl:message>
                </xsl:if>
                <xsl:apply-templates mode="pass1" select="."/>
              </xsl:if>
            </xsl:for-each>
          </xsl:for-each>
  </xsl:template>

  <xsl:template match="tei:elementSpec[@mode = ('change','replace')]
                     | tei:classSpec[ @mode  = ('change','replace')]
                     | tei:macroSpec[ @mode  = ('change','replace')]
                     | tei:dataSpec[ @mode   = ('change','replace')]"
                mode="pass1"/>
  
  <xsl:template match="tei:classSpec/tei:attList/tei:attDef" mode="pass1">
    <xsl:variable name="c" select="ancestor::tei:classSpec/@ident"/>
    <xsl:variable name="a" select="@ident"/>
    <xsl:choose>
      <xsl:when test="$ODD/key('odd2odd-REFED',$c)[@include or @except]">
        <xsl:if test="tei:includeMember(@ident,$ODD/key('odd2odd-REFED',$c)/@except,$ODD/key('odd2odd-REFED',$c)/@include)">
          <xsl:if test="$verbose eq 'true'">
            <xsl:message>  keeping attribute <xsl:value-of
              select="(ancestor::tei:classSpec/@ident,@ident)" separator="/"/></xsl:message>
          </xsl:if>
          <xsl:copy>
            <xsl:apply-templates select="@*|node()" mode="pass1"/>
          </xsl:copy>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*|node()" mode="pass1"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  
  <!-- ******************* Phase 2, make the changes ********************************* -->
  <xsl:template match="@*|processing-instruction()|text()|comment()" mode="pass2">
    <xsl:copy/>
  </xsl:template>
  
  <xsl:template match="*" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates mode="#current" select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="comment()" mode="justcopy"/>

  <xsl:template match="@*|text()|processing-instruction()|tei:exemplum//comment()" mode="justcopy">
    <xsl:copy/>
  </xsl:template>
  
  <xsl:template match="*" mode="justcopy">
    <xsl:param name="rend"/>
    <xsl:copy>
      <xsl:if test="$rend">
        <xsl:attribute name="rend" select="$rend"/>
      </xsl:if>
      <xsl:apply-templates select="@*|node()" mode="#current"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="a:* | rng:*" mode="justcopy">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|text()" mode="#current"/>
    </xsl:copy>
  </xsl:template>
  
   <xsl:template match="tei:schemaSpec" mode="pass2">
     <xsl:variable name="oddsource">
       <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:sequence select="if ($verbose='true')then
           tei:message(concat('Schema pass 2: ',@ident))
           else ()"/>
         <xsl:for-each select="*">
           <xsl:call-template name="odd2odd-checkObject"/>
         </xsl:for-each>
       </xsl:copy>
     </xsl:variable>
    <xsl:for-each select="$oddsource">
      <xsl:apply-templates mode="pass3"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="odd2odd-checkObject">
    <!--
        for every object
         - if its in the ODD spec's REPLACE list, use that      
         - if its in ODD spec's CHANGE list  (do the hard merge bit)
               - if its duplicated by an existing spec, ignore
         - otherwise copy 
        done
  -->

    <xsl:variable name="specName" select="tei:uniqueName(.)"/>
    <xsl:variable name="N" select="local-name(.)"/>
    <xsl:choose>
      <xsl:when test="$ODD/key('odd2odd-DELETE',$specName)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 2: delete <xsl:value-of select="$specName"/></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="$ODD/key('odd2odd-REPLACE',$specName)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 2: replace <xsl:value-of select="$specName"/></xsl:message>
        </xsl:if>
        <xsl:apply-templates mode="odd2odd-copy" select="$ODD/key('odd2odd-REPLACE',$specName)"/>
      </xsl:when>
      <xsl:when test="$ODD/key('odd2odd-CHANGE',$specName)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 2: change <xsl:value-of select="$specName"/></xsl:message>
        </xsl:if>
        <xsl:apply-templates mode="odd2odd-change" select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Phase 2: keep <xsl:value-of  select="($N,$specName)"/></xsl:message>
        </xsl:if>
        <xsl:apply-templates mode="justcopy" select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template mode="odd2odd-change odd2odd-copy"
                match="@*|processing-instruction()|text()">
    <xsl:copy/>
  </xsl:template>

  <xsl:template match="*" mode="odd2odd-change">
    <xsl:copy>
      <xsl:apply-templates mode="odd2odd-change" select="@*|*|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>

<!--  <xsl:template match="rng:*" mode="odd2odd-change">
    <xsl:element xmlns="http://relaxng.org/ns/structure/1.0" name="{local-name()}">
      <xsl:apply-templates mode="odd2odd-change" select="@*|*|processing-instruction()|text()"/>
    </xsl:element>
  </xsl:template>
 USELESS template to be removed? see e-mail "copying craziness" of 2016-11-15 22:54Z -->

<!--  <xsl:template match="tei:elementSpec/@mode" mode="odd2odd-change">
    <xsl:copy/>
  </xsl:template>
 USELESS? same as above, I think. -->
  <xsl:variable name="key" select="@key"/>
  <xsl:variable name="whence" select="local-name()"/>
  
  <xsl:template match="tei:elementSpec" mode="odd2odd-change">
    <xsl:variable name="elementName" select="tei:uniqueName(.)"/>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:attribute name="rend">change</xsl:attribute>
      <xsl:apply-templates mode="odd2odd-change" select="@*"/>
      <!-- 
              For each element, go through most of the sections one by one
              and see if they are present in the change mode version.
              If so, use them as is. The constraints and attributes are identifiable
              for change individually.
      -->
      <xsl:for-each select="$ODD/key('odd2odd-CHANGE',$elementName)">
        <xsl:copy-of select="@ns"/>
        <!-- if there is an altIdent, use it -->
        <xsl:apply-templates mode="justcopy" select="tei:altIdent"/>
        <!-- equiv, gloss, desc trio -->
        <xsl:choose>
          <xsl:when test="tei:equiv">
            <xsl:apply-templates mode="justcopy"
              select="tei:equiv">
              <xsl:with-param name="rend">replace</xsl:with-param>
            </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:equiv"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:gloss">
              <xsl:apply-templates mode="justcopy" select="tei:gloss">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:gloss"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:desc">
              <xsl:apply-templates mode="justcopy" select="tei:desc">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:desc"/>
            </xsl:otherwise>
          </xsl:choose>
          <!-- classes -->
          <classes xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:classes[@mode eq 'change']">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:choose>
                    <xsl:when test="@mode eq 'delete'"/>
                    <xsl:when test="@mode eq 'add' or not (@mode)">
                      <memberOf key="{@key}">
                        <xsl:copy-of select="@min|@max"/>
                      </memberOf>
                    </xsl:when>
                  </xsl:choose>
                </xsl:for-each>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:variable name="metoo">
                      <xsl:value-of select="concat(../../@ident,@key)"/>
                    </xsl:variable>
                      <xsl:choose>
                        <xsl:when test="$ODD/key('odd2odd-DELETE',$me)"> </xsl:when>
                        <xsl:when test="$ODD/key('odd2odd-MEMBEROFDELETE',$metoo)"> </xsl:when>
                        <xsl:when test="$ODD/key('odd2odd-MEMBEROFADD',$metoo)"> </xsl:when>
                        <xsl:otherwise>
                          <memberOf key="{$me}"/>
                        </xsl:otherwise>
                      </xsl:choose>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="tei:classes">
                <xsl:for-each select="tei:classes/tei:memberOf[not(@mode eq 'delete')]">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:if test="not(key('odd2odd-DELETE',$me))">
                        <memberOf key="{$me}"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </classes>
          <!-- valList -->
          <xsl:choose>
            <xsl:when test="tei:valList[@mode eq 'delete']"/>
            <xsl:when test="tei:valList">
              <xsl:apply-templates mode="odd2odd-copy" select="tei:valList[1]"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:valList"/>
            </xsl:otherwise>
          </xsl:choose>
          <!-- element content -->
          <content xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:content and not(tei:content/*)"/>
              <xsl:when test="tei:content/rng:*">
                <xsl:apply-templates mode="odd2odd-copy" select="tei:content/*"/>
              </xsl:when>
              <xsl:when test="tei:content/tei:*">
                <xsl:apply-templates mode="odd2odd-copy" select="tei:content/@*"/>
                <xsl:apply-templates mode="odd2odd-copy" select="tei:content/*"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:content/@*"/>
                <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:content/*"/>
              </xsl:otherwise>
            </xsl:choose>
          </content>
          <!-- element constraints -->
          <xsl:call-template name="odd2odd-processConstraints">
            <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
            <xsl:with-param name="elementName" select="$elementName"/>
          </xsl:call-template>
          <!-- attList -->
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:apply-templates mode="justcopy" select="tei:attList/@org"/>
            <xsl:call-template name="odd2odd-processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="objectName" select="$elementName"/>
            </xsl:call-template>
          </attList>
          

          <!-- models -->
          <xsl:choose>
            <xsl:when test="tei:modelGrp|tei:model|tei:modelSequence">
              <xsl:apply-templates mode="justcopy" select="tei:modelGrp|tei:model|tei:modelSequence"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:modelGrp|$ORIGINAL/tei:model|$ORIGINAL/tei:modelSequence"/>
            </xsl:otherwise>
          </xsl:choose>

          <!-- exempla -->
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:exemplum">
              <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:exemplum"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:remarks">
              <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:remarks"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:listRef">
              <xsl:apply-templates mode="justcopy" select="tei:listRef"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:listRef"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:dataSpec|tei:macroSpec" mode="odd2odd-change">
    <xsl:variable name="specName" select="tei:uniqueName(.)"/>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:attribute name="rend">change</xsl:attribute>
      <xsl:apply-templates mode="odd2odd-change" select="@*"/>
      <!-- 
           For each macro, go through most of the sections one by one
           and see if they are present in the change mode version.
           If so, use them as is. 
      -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('odd2odd-CHANGE',$specName)">
          <!-- if there is an altIdent, use it -->
          <xsl:apply-templates mode="justcopy" select="tei:altIdent"/>
          <!-- equiv, gloss, desc trio -->
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:equiv">
              <xsl:apply-templates mode="justcopy"
                select="tei:equiv">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:equiv"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:gloss">
              <xsl:apply-templates mode="justcopy" select="tei:gloss">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:gloss"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:desc">
              <xsl:apply-templates mode="justcopy" select="tei:desc">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:desc"/>
            </xsl:otherwise>
          </xsl:choose>
          <!-- content -->
          <xsl:choose>
            <xsl:when test="tei:dataRef">
              <xsl:apply-templates mode="justcopy" select="tei:dataRef">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:when test="tei:content">
              <xsl:apply-templates mode="justcopy" select="tei:content">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:when test="$ORIGINAL/tei:dataRef">
              <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:dataRef">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:content"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:valList">
              <xsl:apply-templates mode="justcopy" select="tei:valList[1]">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
            </xsl:when>
            <xsl:when test="tei:stringVal">
              <xsl:apply-templates mode="justcopy" select="tei:stringVal"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="odd2odd-copy" select="$ORIGINAL/tei:stringVal"/>
            </xsl:otherwise>
          </xsl:choose>
          <!-- constraints -->
          <xsl:call-template name="odd2odd-processConstraints">
            <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
            <xsl:with-param name="elementName" select="$specName"/>
          </xsl:call-template>
          <!-- exemplum, remarks and listRef are either replacements or not -->
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:exemplum">
              <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:exemplum"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="tei:remarks">
              <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:remarks"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose>
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:listRef">
              <xsl:apply-templates mode="justcopy" select="tei:listRef"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:listRef"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="odd2odd-change">
    <xsl:variable name="className" select="tei:uniqueName(.)"/>
    <xsl:variable name="ORIGINAL" select="."/>
    <xsl:copy>
      <xsl:attribute name="rend">change</xsl:attribute>
      <xsl:apply-templates mode="odd2odd-change" select="@*"/>
      <!-- for each section of the class spec, 
     go through the sections one by one
     and see if they are present in the change mode version -->
      <xsl:for-each select="$ODD">
        <xsl:for-each select="key('odd2odd-CHANGE',$className)">
          <!-- context is now a classSpec in change mode in the ODD spec -->
          <!-- description -->
          <!-- For each non-identifiable element type (namely:
               <gloss>, <altIdent>, <equiv>, <desc>, then <remarks>,
               <exemplum>, and <listRef>) copy instances over from the
               customization ODD if it is present there, and from the
               original source ODD if it is not present in the context
               node; unless we are stripping out prose content stuff,
               in which case some are not copied no matter what. The
               <classes>, <constraintSpec>s, and <attList> are handled
               differently, as they are identifiable. -->
          <xsl:choose> <!-- maybe copy <gloss>s from ODD or ORIGINAL -->
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:gloss">
              <xsl:apply-templates mode="justcopy" select="tei:gloss"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:gloss"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose> <!-- copy <altIdent>s from ODD or ORIGINAL -->
            <xsl:when test="tei:altIdent">
              <xsl:apply-templates mode="justcopy" select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:altIdent"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose> <!-- copy <equiv>s from ODD or ORIGINAL -->
            <xsl:when test="tei:equiv">
              <xsl:apply-templates mode="justcopy" select="tei:equiv"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:equiv"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose> <!-- maybe copy <desc>s from ODD or ORIGINAL -->
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:desc">
              <xsl:apply-templates mode="justcopy" select="tei:desc"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:desc"/>
            </xsl:otherwise>
          </xsl:choose>
          <!-- classes -->
          <classes xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:choose>
              <xsl:when test="tei:classes[@mode eq 'change']">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:choose>
                    <xsl:when test="@mode eq 'delete'"/>
                    <xsl:when test="@mode eq 'add' or not (@mode)">
                      <memberOf key="{@key}">
                        <xsl:copy-of select="@min|@max"/>
                      </memberOf>
                    </xsl:when>
                  </xsl:choose>
                </xsl:for-each>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:variable name="metoo">
                      <xsl:value-of select="concat(../../@ident,@key)"/>
                    </xsl:variable>
                    <xsl:choose>
                      <xsl:when test="$ODD/key('odd2odd-DELETE',$me)"> </xsl:when>
                      <xsl:when test="$ODD/key('odd2odd-MEMBEROFDELETE',$metoo)"> </xsl:when>
                      <xsl:when test="$ODD/key('odd2odd-MEMBEROFADD',$metoo)"> </xsl:when>
                      <xsl:otherwise>
                        <memberOf key="{$me}"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:when>
              <xsl:when test="tei:classes">
                <xsl:for-each select="tei:classes/tei:memberOf">
                  <xsl:copy-of select="."/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$ORIGINAL">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:variable name="me">
                      <xsl:value-of select="@key"/>
                    </xsl:variable>
                    <xsl:for-each select="$ODD">
                      <xsl:if test="not(key('odd2odd-DELETE',$me))">
                        <memberOf key="{$me}"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:for-each>
              </xsl:otherwise>
            </xsl:choose>
          </classes>
          <!-- constraints -->
          <xsl:call-template name="odd2odd-processConstraints">
            <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
            <xsl:with-param name="elementName" select="$className"/>
          </xsl:call-template>
          <!-- attList -->
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:call-template name="odd2odd-processAttributes">
              <xsl:with-param name="ORIGINAL" select="$ORIGINAL"/>
              <xsl:with-param name="objectName" select="$className"/>
            </xsl:call-template>
          </attList>
          <xsl:choose> <!-- maybe copy <exemplum>s from ODD or ORIGINAL -->
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:exemplum">
              <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:exemplum"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose> <!-- maybe copy <remarks>s from ODD or ORIGINAL -->
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:remarks">
              <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:remarks"/>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:choose> <!-- maybe copy <listRef>s from ODD or ORIGINAL -->
            <xsl:when test="$stripped='true'"/>
            <xsl:when test="tei:listRef">
              <xsl:apply-templates mode="justcopy" select="tei:listRef"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$ORIGINAL/tei:listRef"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>
  <!-- TODO: Duplicate the functionality here for Pure ODD constructs -->
  <xsl:template match="rng:choice|rng:list|rng:group|rng:optional|rng:oneOrMore|rng:zeroOrMore" mode="odd2odd-copy">
    <xsl:call-template name="odd2odd-simplifyRelax"/>
  </xsl:template>
  <xsl:template match="tei:alternate|tei:sequence" mode="odd2odd-copy">
    <xsl:call-template name="odd2odd-simplifyODD"/>
  </xsl:template>
  <xsl:template match="tei:content//tei:valList" mode="odd2odd-copy">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template name="odd2odd-simplifyRelax">
    <xsl:variable name="element">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <!-- 
      for each RELAX NG content model,
      remove reference to any elements which have been
      deleted, or to classes which are empty.
      This may make the container empty,
      so that is only put back in if there is some content
    -->
    <xsl:variable name="contents">
      <WHAT>
        <xsl:for-each select="a:*|rng:*|processing-instruction()">
          <xsl:choose>
            <xsl:when test="self::a:*">
              <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:when test="self::processing-instruction()">
              <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:when test="self::rng:element | self::rng:name | self::rng:attribute | self::rng:data | self::rng:text ">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="odd2odd-copy"/>
              </xsl:copy>
            </xsl:when>
            <xsl:when test="self::rng:value">
              <value xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:apply-templates/>
              </value>
            </xsl:when>
            <xsl:when test="self::rng:ref">
              <xsl:variable name="N" select="@name"/>
              <xsl:for-each select="$ODD">
                <xsl:choose>
                  <xsl:when test="$stripped='true'">
                    <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$N}"/>
                  </xsl:when>
                  <xsl:when test="key('odd2odd-DELETE',$N)"/>
                  <xsl:otherwise>
                    <ref xmlns="http://relaxng.org/ns/structure/1.0" name="{$N}"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="odd2odd-simplifyRelax"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </WHAT>
    </xsl:variable>
    <xsl:variable name="entCount">
      <xsl:value-of select="count($contents/WHAT/*)"/>
    </xsl:variable>
    <xsl:for-each select="$contents/WHAT">
      <xsl:choose>
        <xsl:when test="$entCount=1     and local-name(*)=$element">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='optional'     and $entCount=1     and rng:zeroOrMore">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='optional'     and $entCount=1     and rng:oneOrMore">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element='oneOrMore'     and $entCount=1     and rng:zeroOrMore">
          <oneOrMore xmlns="http://relaxng.org/ns/structure/1.0">
            <xsl:copy-of select="rng:zeroOrMore/*"/>
          </oneOrMore>
        </xsl:when>
        <xsl:when test="self::rng:zeroOrMore/rng:ref/@name eq 'model.global'        and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name eq 'model.global']"/>
        <xsl:when test="$entCount&gt;0 or $stripped='true'">
          <xsl:element namespace="http://relaxng.org/ns/structure/1.0" name="{$element}">
            <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
          </xsl:element>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template name="odd2odd-simplifyODD">
    <xsl:variable name="element">
      <xsl:value-of select="local-name(.)"/>
    </xsl:variable>
    <xsl:variable name="minOmaxO" select="tei:minOmaxO( @minOccurs, @maxOccurs )"/>
    <xsl:variable name="min" select="$minOmaxO[1]"/>
    <xsl:variable name="max" select="$minOmaxO[2]"/>
    <!-- 
      for each Pure ODD content model,
      remove reference to any elements which have been
      deleted, or to classes which are empty.
      This may make the container empty,
      so that is only put back in if there is some content
    -->
    <xsl:variable name="contents">
      <WHAT>
        <xsl:for-each select="a:*|tei:*|rng:*|processing-instruction()">
          <xsl:choose>
            <xsl:when test="self::a:* | self::processing-instruction() ">
              <xsl:copy/>
            </xsl:when>
            <!-- Keep for now to support anyXML -->
            <xsl:when test="self::rng:element | self::rng:name | self::rng:attribute">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="odd2odd-copy"/>
              </xsl:copy>
            </xsl:when>
            <!-- end anyXML section -->
            <xsl:when test="self::tei:dataRef[@name] | self::tei:textNode | self::tei:valList |
              self::tei:anyElement">
              <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:when test="self::tei:classRef or self::tei:elementRef or self::tei:macroRef or self::tei:dataRef">
              <xsl:variable name="N" select="@key"/>
              <xsl:variable name="current" select="."/>
              <xsl:for-each select="$ODD">
                <xsl:choose>
                  <xsl:when test="$stripped='true'">
                    <xsl:copy-of select="$current"/>
                  </xsl:when>
                  <xsl:when test="key('odd2odd-DELETE',$N)"/>
                  <xsl:otherwise>
                    <xsl:copy-of select="$current"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="odd2odd-simplifyODD"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </WHAT>
    </xsl:variable>
    <xsl:variable name="entCount" select="count($contents/WHAT/*)"/>
    <xsl:for-each select="$contents/WHAT">
      <xsl:choose>
        <xsl:when test="$entCount eq 1 and local-name(*) eq $element">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <!-- sequence or alternate that's zero or one containing sequence or alternate that's zero or one-->
        <xsl:when test="$element=('sequence','alternate')
                    and $min eq 0 and $max eq 1 
                    and $entCount eq 1
                    and (tei:sequence|tei:alternate)[
                        tei:minOmaxO( @minOccurs, @maxOccurs )[1] eq 0
                        and
                        tei:minOmaxO( @minOccurs, @maxOccurs )[2] eq -1
                        ]">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <xsl:when test="$element=('sequence','alternate')
                    and $min eq 0 and $max eq 1 
                    and $entCount eq 1
                    and (tei:sequence|tei:alternate)[
                        tei:minOmaxO( @minOccurs, @maxOccurs )[1] eq 1
                        and
                        tei:minOmaxO( @minOccurs, @maxOccurs )[2] eq -1
                        ]">
          <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
        </xsl:when>
        <!-- sequence or alternate that's zero or more containing sequence or alternate that's zero or more  -->
        <xsl:when test="$element=('sequence','alternate')
                    and $min eq 1 and $max eq -1 
                    and $entCount eq 1
                    and (tei:sequence|tei:alternate)[
                        tei:minOmaxO( @minOccurs, @maxOccurs )[1] eq 0
                        and
                        tei:minOmaxO( @minOccurs, @maxOccurs )[2] eq -1
                        ]">
          <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:copy-of select="(tei:sequence|tei:alternate)/*"/>
          </xsl:copy>
        </xsl:when>
        <!-- classRef that's 0 or more immediately following a classRef that's 0 or more -->
        <xsl:when test="self::tei:classRef[
                              @key eq 'model.global'
                              and
                              tei:minOmaxO( @minOccurs, @maxOccurs )[1] eq 0
                              and
                              tei:minOmaxO( @minOccurs, @maxOccurs )[2] eq -1
                              ] and
                          preceding-sibling::tei:*[1][
                                self::tei:classRef/@key eq 'model.global'
                                and
                                tei:minOmaxO( @minOccurs, @maxOccurs )[1] eq 0
                                and
                                tei:minOmaxO( @minOccurs, @maxOccurs )[2] eq -1
                                ]"/>
        <xsl:when test="$entCount gt 0 or $stripped='true'">
          <xsl:element namespace="http://www.tei-c.org/ns/1.0" name="{$element}">
            <xsl:attribute name="minOccurs" select="$min"/>
            <xsl:attribute name="maxOccurs" select="if ($max eq -1) then 'unbounded' else $max"/>
            <xsl:copy-of select="@*|*|text()|processing-instruction()"/>
          </xsl:element>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tei:memberOf" mode="odd2odd-copy">
    <xsl:variable name="k" select="@key"/>
    <xsl:choose>
      <xsl:when test="key('odd2odd-DELETE',$k)"/>      
      <xsl:otherwise>
        <memberOf xmlns="http://www.tei-c.org/ns/1.0" key="{$k}">
          <xsl:copy-of select="@min|@max"/>
        </memberOf>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- following template, added 2016-06-06 by Syd Bauman, completely
       supresses <exemplum> elements from the TEI source iff
       $suppressTEIexamples (a parameter) is set to "true". Note that
       <exemplum> elements from the ODD customization file are still
       copied through. -->
  <xsl:template match="exemplum" mode="odd2odd-copy">
    <xsl:choose>
      <xsl:when test="$suppressTEIexamples eq 'true'"/>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates mode="odd2odd-copy" select="@*|node()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="*" mode="odd2odd-copy">
    <xsl:copy>
      <xsl:apply-templates mode="odd2odd-copy" select="@*|*|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:listRef" mode="odd2odd-copy"/>

  <xsl:template match="tei:elementSpec" mode="odd2odd-copy">
    <xsl:copy>
      <xsl:call-template name="odd2odd-copyElementSpec">
        <xsl:with-param name="n" select="'1'"/>
      </xsl:call-template>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="odd2odd-copyElementSpec">
    <xsl:param name="n"/>
    <xsl:variable name="orig" select="."/>
    <xsl:apply-templates mode="odd2odd-copy" select="@*"/>
    <xsl:apply-templates mode="justcopy" select="tei:altIdent"/>
    <xsl:if test="$stripped='false'">
      <xsl:apply-templates mode="odd2odd-copy" select="tei:equiv"/>
      <xsl:apply-templates mode="justcopy" select="tei:gloss"/>
      <xsl:apply-templates mode="justcopy" select="tei:desc"/>
    </xsl:if>
    <xsl:apply-templates mode="justcopy" select="tei:classes"/>
    <xsl:apply-templates mode="odd2odd-copy" select="tei:content"/>
    <xsl:apply-templates mode="odd2odd-copy" select="tei:constraintSpec"/>
    <xsl:if test="tei:attList">
      <attList xmlns="http://www.tei-c.org/ns/1.0">
        <xsl:choose>
          <xsl:when test="tei:attList[@org='choice']">
            <xsl:for-each select="tei:attList">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates mode="justcopy" select="tei:attDef"/>
                <xsl:apply-templates mode="justcopy" select="tei:attRef"/>
                <xsl:apply-templates mode="justcopy" select="tei:attList"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="justcopy" select="tei:attList/tei:attDef"/>
            <xsl:apply-templates mode="justcopy" select="tei:attList/tei:attRef"/>
            <xsl:apply-templates mode="justcopy" select="tei:attList/tei:attList"/>
          </xsl:otherwise>
        </xsl:choose>
      </attList>
    </xsl:if>
    <xsl:apply-templates mode="odd2odd-copy" select="tei:modelGrp|tei:model|tei:modelSequence"/>
    <xsl:if test="$stripped='false'">
      <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
      <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
      <xsl:apply-templates mode="justcopy" select="tei:listRef"/>
    </xsl:if>
  </xsl:template>

  <xsl:template name="odd2odd-addClassAttsToCopy">
  </xsl:template>

  <xsl:template name="odd2odd-processAttributes">
    <xsl:param name="ORIGINAL"/>
    <xsl:param name="objectName"/>
    <!-- we are sitting in the ODD -->
    <!-- first put in the ones we know take precedence as replacements -->
    <xsl:for-each select="tei:attList/tei:attDef[@mode eq 'replace' and @ident=$ORIGINAL/tei:attList//tei:attDef/@ident]">
      <attDef xmlns="http://www.tei-c.org/ns/1.0" >
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates mode="justcopy"/>
      </attDef>
    </xsl:for-each>
    <xsl:for-each select="tei:attList/tei:attDef[@mode eq 'add' or not(@mode)]">
      <attDef xmlns="http://www.tei-c.org/ns/1.0" >
        <xsl:apply-templates select="@*[not(name()='mode')]"/>
        <xsl:apply-templates mode="justcopy"/>
      </attDef>
    </xsl:for-each>
    <!-- class attributes are ones where there is no direct correspondence in
    the source for this element -->
    <xsl:apply-templates mode="justcopy"
                         select="tei:attList/tei:attDef[(@mode eq 'change'
                                 or @mode eq 'delete'
                                 or @mode eq 'replace') and
                                 not(@ident=$ORIGINAL/tei:attList//tei:attDef/@ident)]"/>
    <!-- any direct attRef elements -->
    <xsl:apply-templates mode="justcopy"
                           select="tei:attList/tei:attRef"/>
    <!-- now look at each of the original object's attributes and see
    if we have an update -->
    <xsl:for-each select="$ORIGINAL/tei:attList">
      <!-- original source  context -->
      <!-- first looking at nested attList -->
      <xsl:for-each select="tei:attList">
        <attList xmlns="http://www.tei-c.org/ns/1.0">
          <xsl:copy-of select="@org"/>
          <xsl:for-each select="tei:attDef">
            <xsl:variable name="ATT" select="."/>
            <xsl:variable name="lookingAt">
              <xsl:value-of select="concat(../../../@ident,'_',@ident)"/>
            </xsl:variable>
              <xsl:choose>
                <xsl:when test="ancestor::tei:classSpec and $ODD/key('odd2odd-DELETEATT',$lookingAt)"/>
                <xsl:when test="$ODD/key('odd2odd-DELETEATT',$lookingAt)">
                  <xsl:copy-of
                    select="$ODD/key('odd2odd-DELETEATT',$lookingAt)"/>
                </xsl:when>
                <xsl:when test="$ODD/key('odd2odd-REPLACEATT',$lookingAt)"/>
                <xsl:when test="$ODD/key('odd2odd-CHANGEATT',$lookingAt)">
                  <xsl:call-template name="odd2odd-mergeAttribute">
                    <xsl:with-param name="New" select="$ODD/key('odd2odd-CHANGEATT',$lookingAt)"/>
                    <xsl:with-param name="Old" select="$ATT"/>
                  </xsl:call-template>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:copy-of select="$ATT"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
        </attList>
      </xsl:for-each>
      <!-- now the normal attributes -->
      <xsl:variable name="atts">
        <xsl:for-each select="tei:attDef">
          <xsl:variable name="ATT" select="."/>
          <xsl:variable name="lookingAt">
            <xsl:value-of select="concat(../../@ident,'_',@ident)"/>
          </xsl:variable>
            <xsl:choose>
                <xsl:when test="ancestor::tei:classSpec and
                                $ODD/key('odd2odd-DELETEATT',$lookingAt)"/>
              <xsl:when
                  test="$ODD/key('odd2odd-DELETEATT',$lookingAt)">
                <xsl:copy-of
                    select="$ODD/key('odd2odd-DELETEATT',$lookingAt)"/>
              </xsl:when>
              <xsl:when  test="$ODD/key('odd2odd-REPLACEATT',$lookingAt)"/>
              <xsl:when test="$ODD/key('odd2odd-CHANGEATT',$lookingAt)">
                <xsl:call-template name="odd2odd-mergeAttribute">
                  <xsl:with-param name="New" select="$ODD/key('odd2odd-CHANGEATT',$lookingAt)"/>
                  <xsl:with-param name="Old" select="$ATT"/>
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <xsl:copy-of select="$ATT"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="@org">
          <attList xmlns="http://www.tei-c.org/ns/1.0">
            <xsl:copy-of select="@org"/>
            <xsl:copy-of select="$atts"/>
          </attList>
        </xsl:when>
        <xsl:otherwise>
          <xsl:copy-of select="$atts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="odd2odd-mergeAttribute">
    <xsl:param name="New"/>
    <xsl:param name="Old"/>
    <attDef xmlns="http://www.tei-c.org/ns/1.0">
      <xsl:attribute name="ident" select="$Old/@ident"/>
      <xsl:copy-of select="$Old/@mode"/>
      <xsl:if test="$Old/@mode">
        <xsl:attribute name="rend"><xsl:value-of select="$Old/@mode"/></xsl:attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="$New/@usage">
          <xsl:copy-of select="$New/@usage"/>
        </xsl:when>
        <xsl:when test="$Old/@usage">
          <xsl:copy-of select="$Old/@usage"/>
        </xsl:when>
      </xsl:choose>
      <xsl:if test="$New/tei:altIdent">
        <xsl:apply-templates mode="justcopy" select="$New/tei:altIdent"/>
      </xsl:if>
      <!-- equiv, gloss, desc trio -->
      <xsl:choose>
          <xsl:when test="$stripped='true'"/>
          <xsl:when test="$New/tei:equiv">
            <xsl:apply-templates mode="justcopy"
                                 select="$New/tei:equiv">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates mode="justcopy" select="$Old/tei:equiv"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:gloss">
            <xsl:apply-templates mode="justcopy"
                                 select="$New/tei:gloss">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$Old/tei:gloss"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$stripped='true'"/>
          <xsl:when test="$New/tei:desc">
            <xsl:apply-templates mode="justcopy"
                                 select="$New/tei:desc">
                <xsl:with-param name="rend">replace</xsl:with-param>
              </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
              <xsl:apply-templates mode="justcopy" select="$Old/tei:desc"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:constraintSpec">
            <xsl:apply-templates mode="justcopy" select="$New/tei:constraintSpec"/>
          </xsl:when>
          <xsl:when test="$Old/tei:constraintSpec">
            <xsl:copy-of select="$Old/tei:constraintSpec"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:datatype">
            <xsl:apply-templates mode="justcopy" select="$New/tei:datatype"/>
          </xsl:when>
          <xsl:when test="$Old/tei:datatype">
            <xsl:copy-of select="$Old/tei:datatype"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:defaultVal">
            <xsl:apply-templates mode="justcopy" select="$New/tei:defaultVal"/>
          </xsl:when>
          <xsl:when test="$Old/tei:defaultVal">
            <xsl:copy-of select="$Old/tei:defaultVal"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:valDesc">
            <xsl:apply-templates mode="justcopy" select="$New/tei:valDesc"/>
          </xsl:when>
          <xsl:when test="$Old/tei:valDesc">
            <xsl:copy-of select="$Old/tei:valDesc"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$New/tei:valList[@mode eq 'delete']"/>
          <xsl:when test="$New/tei:valList[@mode eq 'add' or @mode eq 'replace']">
            <xsl:for-each select="$New/tei:valList[1]">
              <xsl:copy>
                <xsl:copy-of select="@type"/>
                <xsl:copy-of select="@repeatable"/>
                <xsl:copy-of select="$Old/tei:valList/@mode"/>
                <xsl:copy-of select="*"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="$New/tei:valList[@mode eq 'change']">
            <xsl:for-each select="$New/tei:valList[1]">
              <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:for-each select="$Old/tei:valList/tei:valItem">
                  <xsl:variable name="thisme" select="@ident"/>
                  <xsl:choose>
                    <xsl:when test="$New/tei:valList[1]/tei:valItem[@ident eq $thisme and (@mode eq 'delete' or @mode eq 'replace')]"/>
                    <xsl:when test="$New/tei:valList[1]/tei:valItem[@ident eq $thisme and (@mode eq 'change')]">
                      <xsl:copy>
                        <xsl:copy-of select="@*"/>
                        <xsl:for-each select="$New/tei:valList[1]/tei:valItem[@ident=$thisme]">
                          <xsl:choose>
                            <xsl:when test="tei:equiv">
                              <xsl:apply-templates mode="odd2odd-copy" select="tei:equiv"/>
                            </xsl:when>
                            <xsl:otherwise>
                              <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                                <xsl:apply-templates mode="odd2odd-copy" select="tei:equiv"/>
                              </xsl:for-each>
                            </xsl:otherwise>
                          </xsl:choose>
                          <xsl:choose>
                            <xsl:when test="tei:gloss">
                              <xsl:apply-templates mode="justcopy" select="tei:gloss"/>
                            </xsl:when>
                            <xsl:otherwise>
                              <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                                <xsl:apply-templates mode="justcopy" select="tei:gloss"/>
                              </xsl:for-each>
                            </xsl:otherwise>
                          </xsl:choose>
                          <xsl:choose>
                            <xsl:when test="$stripped='true'"/>
                            <xsl:when test="tei:desc">
                              <xsl:apply-templates mode="justcopy" select="tei:desc"/>
                            </xsl:when>
                            <xsl:otherwise>
                              <xsl:for-each select="$Old/tei:valList/tei:valItem[@ident=$thisme]">
                                <xsl:apply-templates mode="justcopy" select="tei:desc"/>
                              </xsl:for-each>
                            </xsl:otherwise>
                          </xsl:choose>
                        </xsl:for-each>
                      </xsl:copy>
                    </xsl:when>
                    <xsl:when test="$New/tei:valList[1]/tei:valItem[@ident eq $thisme and (@mode eq 'add')]">
                      <xsl:message terminate="yes">Asked to add attr <xsl:value-of select="$thisme"/> of <xsl:value-of select="$Old/@ident"/> of <xsl:value-of select="$Old/ancestor::*[@ident][1]/@ident"/> but it already exists; perhaps use @mode of 'change' or 'replace' instead.</xsl:message>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:apply-templates mode="justcopy" select="."/>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:for-each>
                <xsl:apply-templates mode="justcopy" select="tei:valItem[@mode eq 'add']"/>
                <xsl:apply-templates mode="justcopy" select="tei:valItem[@mode eq 'replace']"/>
              </xsl:copy>
            </xsl:for-each>
          </xsl:when>
          <xsl:when test="$Old/tei:valList">
            <xsl:copy-of select="$Old/tei:valList"/>
          </xsl:when>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$stripped='true'"/>
          <xsl:when test="$New/tei:exemplum">
            <xsl:apply-templates mode="justcopy" select="$New/tei:exemplum"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:choose>
          <xsl:when test="$stripped='true'"/>
          <xsl:when test="$New/tei:remarks">
            <xsl:apply-templates mode="justcopy" select="$New/tei:remarks"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:for-each select="$Old">
              <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
    </attDef>
  </xsl:template>
  <xsl:template match="tei:specGrp">
    <xsl:choose>
      <xsl:when test="ancestor::tei:schemaSpec"> </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:specGrpRef"/>
  <xsl:template match="tei:dataSpec|tei:macroSpec|tei:classSpec">
    <xsl:if test="not(ancestor::tei:schemaSpec)">
      <xsl:copy-of select="."/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:attDef[@mode]"/>

  <xsl:template match="tei:elementSpec">
    <xsl:if test="not(//tei:schemaSpec)">
      <xsl:copy>
        <xsl:apply-templates mode="odd2odd-copy" select="@*"/>
        <xsl:apply-templates mode="justcopy" select="tei:altIdent"/>
        <xsl:if test="$stripped='false'">
          <xsl:apply-templates mode="odd2odd-copy" select="tei:equiv"/>
          <xsl:apply-templates mode="justcopy" select="tei:gloss"/>
          <xsl:apply-templates mode="justcopy" select="tei:desc"/>
        </xsl:if>
        <xsl:apply-templates mode="justcopy" select="tei:classes"/>
        <xsl:apply-templates mode="odd2odd-copy" select="tei:content"/>
        <xsl:apply-templates mode="odd2odd-copy" select="tei:constraintSpec"/>
        <attList xmlns="http://www.tei-c.org/ns/1.0">
          <xsl:apply-templates select="tei:attList"/>
        </attList>
        <xsl:if test="$stripped='false'">
          <xsl:apply-templates mode="justcopy" select="tei:exemplum"/>
          <xsl:apply-templates mode="justcopy" select="tei:remarks"/>
          <xsl:apply-templates mode="justcopy" select="tei:listRef"/>
        </xsl:if>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@url]">
    <p>Include external module <xsl:value-of select="@url"/>.</p>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@key]">
    <p>Internal module <xsl:value-of select="@key"/> was located and expanded.</p>
  </xsl:template>
  <xsl:template match="@*|processing-instruction()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="*">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template name="odd2odd-createCopy">
    <xsl:if test="$verbose='true'">
      <xsl:message>Create <xsl:value-of select="local-name()"/> named   <xsl:value-of select="@ident"/>   <xsl:sequence select="if
      (@module) then concat(' module: ',@module) else ''"/>         </xsl:message>
    </xsl:if>
    <xsl:element namespace="http://www.tei-c.org/ns/1.0"
                 name="{local-name()}">
      <xsl:attribute name="rend">add</xsl:attribute>
      <xsl:choose>
        <xsl:when test="@module"/>
        <xsl:when test="ancestor::tei:schemaSpec/@module">
          <xsl:copy-of select="ancestor::tei:schemaSpec/@module"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="module">
            <xsl:text>derived-module-</xsl:text>
            <xsl:value-of select="ancestor::tei:schemaSpec/@ident"/>
          </xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="local-name()='classSpec'">
          <xsl:if test="@type eq 'model' and not(@predeclare)">
            <xsl:attribute name="predeclare">true</xsl:attribute>
          </xsl:if>
          <xsl:apply-templates mode="odd2odd-copy" select="@*|*|processing-instruction()|text()"/>
        </xsl:when>
        <xsl:when test="local-name()='macroSpec'">
          <xsl:apply-templates mode="odd2odd-copy" select="@*|*|processing-instruction()|text()"/>
        </xsl:when>
        <xsl:when test="local-name()='dataSpec'">
          <xsl:apply-templates mode="odd2odd-copy" select="@*|*|processing-instruction()|text()"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="odd2odd-copyElementSpec">
            <xsl:with-param name="n" select="'2'"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>

  <xsl:template name="odd2odd-getversion">
    <!-- 
         Note:
         This template generates a single string which is the concatonation 
         of all the fileDesc/editionStmt/edition elements. Luckily it only
         reads p5subset.xml which has one and only one such element. See
         https://github.com/TEIC/Stylesheets/issues/355.
    -->
    <xsl:choose>
      <xsl:when test="key('odd2odd-SCHEMASPECS',$whichSchemaSpec)">
        <xsl:for-each
            select="key('odd2odd-SCHEMASPECS',$whichSchemaSpec)">
          <xsl:variable name="source" select="tei:workOutSource(.)"/>
          <xsl:for-each select="document($source)/*/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
            <xsl:value-of select="."/>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="document($DEFAULTSOURCE)/*/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
          <xsl:value-of select="."/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="odd2odd-processConstraints">
    <xsl:param name="ORIGINAL"/>
    <xsl:param name="elementName"/>
    <!-- first put in the ones we know take precedence -->
    <xsl:apply-templates mode="justcopy" select="tei:constraintSpec[@mode eq 'add' or not(@mode)]"/>
    <xsl:apply-templates mode="justcopy" select="tei:constraintSpec[@mode eq 'replace']"/>
    <xsl:apply-templates mode="justcopy" select="tei:constraintSpec[@mode eq 'change']"/>
    <xsl:for-each select="$ORIGINAL">
      <!-- original source  context -->
      <xsl:for-each select="tei:constraintSpec">
        <xsl:variable name="CONSTRAINT" select="."/>
        <xsl:variable name="lookingAt">
          <xsl:value-of select="concat(../@ident,'_',@ident)"/>
        </xsl:variable>
        <xsl:for-each select="$ODD">
          <xsl:choose>
            <xsl:when test="key('odd2odd-DELETECONSTRAINT',$lookingAt)"/>
            <xsl:when test="key('odd2odd-REPLACECONSTRAINT',$lookingAt)"/>
            <xsl:when test="key('odd2odd-CHANGECONSTRAINT',$lookingAt)"/>
            <xsl:otherwise>
              <xsl:copy-of select="$CONSTRAINT"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>



<!-- pass 3, clean up -->      
  <xsl:template match="rng:ref" mode="pass3">
    <xsl:variable name="N">
      <xsl:value-of select="@name"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="starts-with($N,'macro.') and $stripped='true'">
        <xsl:for-each select="key('odd2odd-MACROS',$N)/tei:content/*">
          <xsl:call-template name="odd2odd-simplifyRelax"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <ref xmlns="http://relaxng.org/ns/structure/1.0">
          <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass3"/>
        </ref>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:valDesc|tei:equiv|tei:gloss|tei:desc|tei:remarks|tei:exemplum|tei:modelGrp|tei:model|tei:modelSequence|tei:rendition|tei:listRef" mode="pass3">
    <xsl:choose>
      <xsl:when test="$stripped='true'"> </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass3"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:ptr | tei:listRef/tei:ref" mode="pass3">
    <xsl:choose>
      <xsl:when test="starts-with(@target,'#') and 
        (ancestor::tei:remarks or parent::tei:listRef or ancestor::tei:valDesc) and
        not(id(substring(@target,2)))">
        <xsl:variable name="target" select="substring(@target,2)"/>
        <xsl:variable name="sourceDoc" select="tei:workOutSource(.)"/>
        <!-- the chapter ID is on the highest ancestor or self div -->
        <xsl:variable name="chapter" select="document($sourceDoc)/id($target)/ancestor-or-self::tei:div[not(ancestor::tei:div)]/@xml:id"/>
        <xsl:choose>
          <xsl:when test="(string-length(normalize-space(.)) &gt; 0) or processing-instruction() or comment()">
            <ref  xmlns="http://www.tei-c.org/ns/1.0"           target="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/{$chapter}.html#{$target}">
              <xsl:apply-templates mode="#current"/>
            </ref>
          </xsl:when>
          <xsl:when test="document($sourceDoc)/id($target)">
            <ref  xmlns="http://www.tei-c.org/ns/1.0"
              target="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/{$chapter}.html#{$target}">
              <xsl:for-each select="document($sourceDoc)/id($target)">
                <xsl:number count="tei:div" format="1.1.1."
                  level="multiple"/>      
                <xsl:text> </xsl:text>
                <xsl:value-of select="tei:head"/>
              </xsl:for-each>
            </ref>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy>
              <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass3"/>
            </xsl:copy>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass3"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:schemaSpec" mode="pass3">
    <xsl:variable name="orig" select="tei:workOutSource(.)"/>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass3" select="text()|comment()|*"/>
      <xsl:copy-of select="document($orig)//tei:schemaSpec/tei:rendition"/>
      <xsl:for-each select="distinct-values(//*[@module]/@module)">
        <xsl:variable name="m" select="."/>
        <xsl:for-each select="document($orig)/key('odd2odd-MODULES',$m)">
          <xsl:copy>
            <xsl:attribute name="n"   select="ancestor::tei:div[last()]/@xml:id"/>
            <xsl:copy-of select="@*"/>
          </xsl:copy>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="@*|text()|comment()" mode="pass3">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="*" mode="pass3">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass3" select="text()|comment()|*"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:content" mode="pass3">
    <xsl:variable name="content">
      <xsl:copy>
        <xsl:copy-of select="@*"/>
        <xsl:apply-templates mode="pass3"/>
      </xsl:copy>
    </xsl:variable>
    <xsl:apply-templates select="$content" mode="pass4"/>
  </xsl:template>


  <xsl:template match="tei:classSpec" mode="pass3">
    <xsl:variable name="used">
      <xsl:call-template name="odd2odd-amINeeded"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$used=''">
        <xsl:if test="$verbose='true'">
          <xsl:message>Reject unused class <xsl:value-of select="@ident"/>
          </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates mode="pass3"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="odd2odd-amINeeded">
    <!--
        How can a class be ok?
        a) if an element is a member of it and
        b)  its referred to in a content model
        c) some other class is a member of it, and that class is OK
        d) its a member of some other class, and that class is OK
    -->
    <xsl:variable name="k" select="@ident"/>
    <xsl:choose>
      <xsl:when test="$autoGlobal='true' and starts-with(@ident,'att.global')">y</xsl:when>
      <xsl:when test="@type eq 'model' and  key('odd2odd-REFED',$k)">y</xsl:when>
      <xsl:when test="@type eq 'atts' and  key('odd2odd-ATTREFED',$k)">y</xsl:when>
      <xsl:when test="@type eq 'atts' and key('odd2odd-ELEMENT_MEMBERED',$k)">y</xsl:when>
      <xsl:when test="@type eq 'atts' and key('odd2odd-CLASS_MEMBERED',$k)">
        <xsl:for-each select="key('odd2odd-CLASS_MEMBERED',$k)">
          <xsl:call-template name="odd2odd-amINeeded"/>
        </xsl:for-each>
      </xsl:when>

      <xsl:when test="@type eq 'model' and tei:classes/tei:memberOf">
        <xsl:for-each
            select="tei:classes/tei:memberOf/key('odd2odd-IDENTS',@key)">
            <xsl:call-template name="odd2odd-amINeeded"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:memberOf" mode="pass3">
    <xsl:variable name="keep" select="."/>
    <xsl:choose>
      <xsl:when test="not(key('odd2odd-IDENTS',@key))">
        <xsl:if test="$verbose='true'">
          <xsl:message>Reject unused memberOf pointing to <xsl:value-of select="@ident"/> because that doesn't exist</xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="key('odd2odd-IDENTS',@key)[1]">
          <xsl:variable name="used">
            <xsl:call-template name="odd2odd-amINeeded"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="$used=''">
              <xsl:if test="$verbose='true'">
                <xsl:message>Reject unused memberOf pointing to <xsl:value-of select="@ident"/>  </xsl:message>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              <xsl:copy-of select="$keep"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:macroSpec" mode="pass3">
    <xsl:variable name="k">
      <xsl:value-of select="@prefix"/>
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$stripped='true' and starts-with($k,'macro.')"/>
      <xsl:when test="key('odd2odd-REFED',$k)">
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates mode="pass3"/>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Reject unused macro <xsl:value-of select="$k"/></xsl:message>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:dataSpec" mode="pass3">
    <xsl:variable name="k">
      <xsl:value-of select="@ident"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="key('odd2odd-REFED',$k)">
        <dataSpec xmlns="http://www.tei-c.org/ns/1.0" >
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates mode="pass3"/>
        </dataSpec>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message>Reject unused dataSpec <xsl:value-of select="$k"/></xsl:message>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="processing-instruction()" mode="pass3">
    <xsl:copy-of select="."/>
  </xsl:template>


  <!-- pass 4, more clean up -->
  <xsl:template match="processing-instruction()" mode="pass4">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@*|text()" mode="pass4">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="@mode" mode="pass4">
      <xsl:attribute name="rend" select="."/>
  </xsl:template>

  <xsl:template match="*" mode="pass4">
    <xsl:choose>
      <xsl:when test="self::rng:optional     and count(rng:zeroOrMore) eq 2    and count(*) eq 2">
        <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
      </xsl:when>
      <xsl:when test="count(*) eq 1">
        <xsl:variable name="element" select="local-name()"/>
        <xsl:choose>
          <xsl:when test="*[local-name() eq $element]">
            <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:when test="$element eq 'optional'         and rng:zeroOrMore">
            <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:when test="$element eq 'optional'         and rng:oneOrMore">
            <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy>
              <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
            </xsl:copy>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*|*|text()|processing-instruction()" mode="pass4"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

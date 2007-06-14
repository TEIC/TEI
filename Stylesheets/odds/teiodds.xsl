<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr rng edate teix fo a tei s xd xs"
  extension-element-prefixes="edate exsl estr" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:s="http://www.ascc.net/xml/schematron"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:import href="../common/verbatim.xsl"/>
  <xsl:import href="../common/i18n.xsl"/>
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for processing TEI ODD markup </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xsl:include href="RngToRnc.xsl"/>

  <xsl:param name="outputSuffix">.html</xsl:param>
  <xd:doc type="string" class="output">
    Public Doctype of output file(s).
  </xd:doc>
  <xsl:param name="outputDir"/>
  <xsl:param name="simplify">false</xsl:param>
  <xsl:param name="localsource"/>
  <xsl:param name="lang"/>
  <xsl:param name="doclang"/>
  <xsl:param name="patternPrefix"/>
  <xsl:param name="TEIC">false</xsl:param>
  <xsl:param name="lookupDatabase">false</xsl:param>
  <xsl:param name="TEISERVER">http://tei.oucs.ox.ac.uk/Query/</xsl:param>
  <xsl:param name="verbose">false</xsl:param>
  <xsl:param name="schemaBaseURL">http://localhost/schema/relaxng/</xsl:param>
  <xsl:key match="tei:*" name="LOCALIDENTS" use="@ident"/>
  <xsl:key match="tei:macroSpec" name="MACROS" use="@ident"/>
  <xsl:key match="tei:elementSpec" name="ELEMENTS" use="@ident"/>
  <xsl:key match="tei:classSpec" name="CLASSES" use="@ident"/>
  <xsl:key match="rng:ref" name="CLASSREFS" use="@name"/>
  <xsl:key match="tei:elementSpec|tei:classSpec" name="CLASSMEMBERS"
    use="tei:classes/tei:memberOf/@key"/>
  <xsl:key match="tei:elementSpec|tei:classSpec|tei:macroSpec" name="IDENTS"
    use="@ident"/>
  <xsl:key match="tei:*[@xml:id]" name="IDS" use="@xml:id"/>
  <xsl:key match="tei:macroSpec[@type='dt']" name="DATATYPES" use="1"/>
  <xsl:key match="tei:macroSpec" name="MACRODOCS" use="1"/>
  <xsl:key match="tei:classSpec" name="CLASSDOCS" use="1"/>
  <xsl:key match="tei:elementSpec" name="ELEMENTDOCS" use="1"/>
  <xsl:key match="tei:*" name="NameToID" use="@ident"/>
  <xsl:key match="tei:elementSpec" name="ElementModule" use="@module"/>
  <xsl:key match="tei:classSpec" name="ClassModule" use="@module"/>
  <xsl:key match="tei:macroSpec" name="MacroModule" use="@module"/>
  <xsl:key match="tei:moduleSpec" name="Modules" use="1"/>
  <xsl:key match="tei:classSpec[@predeclare='true']" name="predeclaredClasses"
    use="1"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']" name="PredeclareMacros"
    use="@ident"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']"
    name="PredeclareMacrosModule" use="@module"/>
  <xsl:key match="tei:macroSpec[@predeclare='true']" name="PredeclareAllMacros"
    use="1"/>
  <xsl:variable name="parameterize">
    <xsl:choose>
      <xsl:when test="$TEIC='false'">true</xsl:when>
      <xsl:when test="//tei:schemaSpec">false</xsl:when>
      <xsl:otherwise>true</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <!-- lookup table of element contents, and templates to access the result -->
  <xsl:key match="Contains" name="ELEMENTPARENTS" use="."/>
  <xsl:param name="wrapLength">65</xsl:param>

  <xsl:variable name="patternPrefixText">
    <xsl:choose>
      <xsl:when test="string-length($patternPrefix)&gt;0">
	<xsl:value-of select="$patternPrefix"/>
      </xsl:when>
      <xsl:when test="//tei:schemaSpec[@prefix]">
	<xsl:value-of select="//tei:schemaSpec/@prefix"/>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="targetLanguage">
    <xsl:choose>
      <xsl:when test="string-length($lang)&gt;0">
	<xsl:value-of select="$lang"/>
      </xsl:when>
      <xsl:when test="//tei:schemaSpec[@targetLang]">
	<xsl:value-of select="//tei:schemaSpec/@targetLang"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>


  <xsl:template name="generateDoc">
    <xsl:choose>
      <xsl:when test="string-length($doclang)&gt;0">
	<xsl:value-of select="$doclang"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:schemaSpec/@docLang">
	<xsl:value-of select="//tei:schemaSpec[1]/@docLang"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="processing-instruction()">
    <xsl:if test="name(.) = 'odds'">
      <xsl:choose>
        <xsl:when test=".='date'"> This formatted version of the Guidelines was
          created on <xsl:call-template name="showDate"/>. </xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>


  <xsl:template match="*" mode="literal">
    <xsl:text>&#10;</xsl:text>
    <xsl:for-each select="ancestor::rng:*">
      <xsl:text> </xsl:text>
    </xsl:for-each>
    <xsl:text>&lt;</xsl:text>
    <xsl:value-of select="local-name(.)"/>
    <xsl:for-each select="@*"><xsl:text> </xsl:text><xsl:value-of
        select="local-name(.)"/>="<xsl:value-of select="."/>"</xsl:for-each>
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
    <xsl:copy>
      <xsl:attribute name="name">
	<xsl:choose>
	  <xsl:when test="key('IDENTS',@name)">
	    <xsl:value-of select="$patternPrefixText"/>
	  </xsl:when>
	  <xsl:when test="key('IDENTS',substring-before(@name,'_'))">
	    <xsl:value-of select="$patternPrefixText"/>
	  </xsl:when>
	</xsl:choose>
	<xsl:value-of select="@name"/>
      </xsl:attribute>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="rng:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
    </xsl:copy>
  </xsl:template>


  <xsl:template match="rng:zeroOrMore">
    <xsl:choose>
      <xsl:when
	  test="rng:ref/@name='model.global'
		and preceding-sibling::rng:*[1][self::rng:zeroOrMore/rng:ref/@name='model.global']"
/>
      <xsl:when test="count(rng:*)=1 and rng:zeroOrMore">
        <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="rng:choice">
    <xsl:choose>
      <xsl:when test="count(rng:*)=1">
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
        </xsl:copy>
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
	    <xsl:copy>
	      <xsl:copy-of select="@*"/>
	      <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
	    </xsl:copy>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:copy-of select="@*"/>
          <xsl:apply-templates select="rng:*|tei:*|text()|comment()"/>
        </xsl:copy>
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
    <ref name="{@name}" xmlns="http://relaxng.org/ns/structure/1.0"/>
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
          <define name="{$element}.attribute.{translate(@ident,':','')}"
            xmlns="http://relaxng.org/ns/structure/1.0">
            <xsl:call-template name="makeAnAttribute"/>
          </define>
        </xsl:when>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tei:attList" mode="tangle">
    <xsl:param name="element"/>
    <xsl:choose>
      <xsl:when test="count(*)=0"/>
      <xsl:when test="@org='group' and
		      parent::tei:attList[@org='choice']">
	<rng:group>
	  <xsl:apply-templates mode="tangle" select="tei:*">
	    <xsl:with-param name="element" select="$element"/>
	  </xsl:apply-templates>
	</rng:group>
      </xsl:when>

      <xsl:when test="@org='choice'">
          <rng:choice>
            <xsl:apply-templates mode="tangle" select="tei:*">
              <xsl:with-param name="element" select="$element"/>
            </xsl:apply-templates>
          </rng:choice>
      </xsl:when>

      <xsl:otherwise>
        <xsl:apply-templates mode="tangle" select="tei:*">
          <xsl:with-param name="element" select="$element"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:author">
    <xsl:apply-templates/>
    <xsl:text>, </xsl:text>
  </xsl:template>


  <xsl:template match="tei:classSpec" mode="tangle">
    <xsl:variable name="c" select="@ident"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> classSpec <xsl:value-of 
      select="@ident"/> (type <xsl:value-of select="@type"/>)</xsl:message>
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
              <define name="{$patternPrefixText}{@ident}.attributes"
                xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:if test="$parameterize='true'">
                  <xsl:for-each select="tei:classes/tei:memberOf">
                    <xsl:for-each select="key('IDENTS',@key)[1]">
                      <xsl:if test="@type='atts'">
                        <ref name="{@ident}.attributes"
                          xmlns="http://relaxng.org/ns/structure/1.0"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:for-each>
                </xsl:if>
                <xsl:choose>
                  <xsl:when test="tei:attList//tei:attDef">
                    <xsl:for-each select="tei:attList//tei:attDef">
                      <xsl:if test="not(starts-with(@ident,'xmlns'))">
                        <ref name="{$c}.attribute.{translate(@ident,':','')}"
                          xmlns="http://relaxng.org/ns/structure/1.0"/>
                      </xsl:if>
                    </xsl:for-each>
                  </xsl:when>
		  <xsl:when test="tei:classes/tei:memberOf"/>
                  <xsl:otherwise>
                    <notAllowed xmlns="http://relaxng.org/ns/structure/1.0"/>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:if test="$TEIC='true'">
                  <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
                </xsl:if>
              </define>
              <xsl:apply-templates mode="tangle" select="tei:attList">
                <xsl:with-param name="element" select="@ident"/>
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
      <xsl:message> .... model class <xsl:value-of select="@ident"
      /></xsl:message>
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
		  <xsl:text>alternation
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
	<xsl:with-param name="type">alternation</xsl:with-param>
	<xsl:with-param name="declare" select="$declare"/>
      </xsl:call-template>
    </xsl:when>

    <xsl:when test="contains($Type,' ')">
      <xsl:call-template name="makeClassDefinition">
	<xsl:with-param name="type" select="substring-before($Type,' ')"/>
	    <xsl:with-param name="declare" select="$declare"/>
      </xsl:call-template>
      <xsl:call-template name="processClassDefinition">
	<xsl:with-param 
	    name="type" 
	    select="substring-after($Type,' ')"/>
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
  <xsl:variable name="suffix">
    <xsl:choose>
      <xsl:when test="$type='alternation'"/>
      <xsl:otherwise>
	<xsl:text>_</xsl:text>
	<xsl:value-of select="$type"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$declare='true'">
      <xsl:apply-templates mode="tangleModel"
			   select="tei:classes/tei:memberOf"/>
      <define name="{$patternPrefixText}{$thisClass}{$suffix}"
	      xmlns="http://relaxng.org/ns/structure/1.0">
	<xsl:if test="@predeclare='true'">
	  <xsl:attribute name="combine">choice</xsl:attribute>
	</xsl:if>
	<notAllowed xmlns="http://relaxng.org/ns/structure/1.0"/>
      </define>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="makeDecls">
	<xsl:call-template name="findUses">
	  <xsl:with-param name="pattern" select="$suffix"/>
	  <xsl:with-param name="class" select="$thisClass"/>
	</xsl:call-template>
      </xsl:variable>
<!--
<xsl:message><xsl:value-of select="$thisClass"/><xsl:value-of
select="$suffix"/> generated <xsl:value-of
select="$makeDecls"/></xsl:message>
-->
      <xsl:choose>
	<xsl:when test="$makeDecls=''">
	  <xsl:if test="$verbose='true'">
	    <xsl:message>ZAP <xsl:value-of select="$thisClass"/><xsl:value-of select="$suffix"/>
	  </xsl:message>
	  </xsl:if>
	</xsl:when> 
	<xsl:when
	    test="count(key('CLASSMEMBERS',$thisClass))&gt;0">
	  <xsl:if test="$verbose='true'">
	    <xsl:message> .... ... generate model <xsl:value-of
	    select="$thisClass"/><xsl:value-of select="$suffix"/>
	    </xsl:message>
	  </xsl:if>
	  <define name="{$patternPrefixText}{$thisClass}{$suffix}"
		  xmlns="http://relaxng.org/ns/structure/1.0">
	    <xsl:choose>
	      <xsl:when test="$type='sequence'">
		<xsl:for-each
		    select="key('CLASSMEMBERS',$thisClass)">
		  <ref name="{$patternPrefixText}{@ident}"
		       xmlns="http://relaxng.org/ns/structure/1.0"/>
		</xsl:for-each>
	      </xsl:when>
	      <xsl:when test="$type='sequenceOptional'">
		<xsl:for-each
		    select="key('CLASSMEMBERS',$thisClass)">
		  <optional xmlns="http://relaxng.org/ns/structure/1.0">
		    <ref name="{$patternPrefixText}{@ident}"
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </optional>
		</xsl:for-each>
	      </xsl:when>
	      
	      <xsl:when test="$type='sequenceRepeatable'">
		<xsl:for-each
		    select="key('CLASSMEMBERS',$thisClass)">
		  <oneOrMore xmlns="http://relaxng.org/ns/structure/1.0">
		    <ref name="{$patternPrefixText}{@ident}"
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </oneOrMore>
		</xsl:for-each>
	      </xsl:when>
	      
	      <xsl:when test="$type='sequenceOptionalRepeatable'">
		<xsl:for-each
		    select="key('CLASSMEMBERS',$thisClass)">
		  <zeroOrMore xmlns="http://relaxng.org/ns/structure/1.0">
		    <ref name="{$patternPrefixText}{@ident}"
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </zeroOrMore>
		</xsl:for-each>
	      </xsl:when>
	      
	      <xsl:otherwise>
		<rng:choice>
		  <xsl:for-each
		      select="key('CLASSMEMBERS',$thisClass)">
		    <ref name="{$patternPrefixText}{@ident}"
			 xmlns="http://relaxng.org/ns/structure/1.0"/>
		  </xsl:for-each>
		</rng:choice>
	      </xsl:otherwise>
	    </xsl:choose>
	    </define>
	  </xsl:when>
	  <xsl:otherwise>
	    <define name="{$patternPrefixText}{$thisClass}{$suffix}"
	      xmlns="http://relaxng.org/ns/structure/1.0">
	      <xsl:choose>
		<xsl:when test="$type='sequence' or
				$type='sequenceOptional' or 
				$type='sequenceOptionalRepeatable'">
		  <empty
		      xmlns="http://relaxng.org/ns/structure/1.0"/>
		</xsl:when>
		<xsl:otherwise>
		  <notAllowed
		      xmlns="http://relaxng.org/ns/structure/1.0"/>
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
  <xsl:choose>
    <xsl:when test="not(ancestor::tei:schemaSpec)">x</xsl:when>
    <xsl:when test="key('CLASSREFS',concat($class,$pattern))">x</xsl:when>
    <xsl:when test="not($pattern='')"></xsl:when>
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
    <xsl:otherwise></xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="tei:classSpec" mode="tangleadd">
  <xsl:apply-templates mode="tangleadd"/>
</xsl:template>


<xsl:template match="tei:classSpec/@ident"/>


<xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec" mode="weave">
  <xsl:call-template name="refdoc"/>
</xsl:template>


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


<xsl:template match="tei:divGen[@type='classcat']">
    <xsl:apply-templates mode="weave" select="key('CLASSDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>


  <xsl:template match="tei:divGen[@type='macrocat']">
    <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>


  <xsl:template match="tei:divGen[@type='tagcat']">
    <xsl:apply-templates mode="weave" select="key('ELEMENTDOCS',1)">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:template>


  <xsl:template match="tei:editor"><xsl:apply-templates/>: </xsl:template>


  <xsl:template match="tei:elementSpec" mode="tangle">
    <xsl:if test="$verbose='true'">
      <xsl:message> elementSpec <xsl:value-of select="@ident"/>
        <xsl:if test="@xml:id">: <xsl:value-of select="@xml:id"/></xsl:if>
      </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar"/>
      <xsl:with-param name="content">
        <Wrapper>
          <xsl:variable name="name">
            <xsl:choose>
	      <xsl:when test= "tei:altIdent=@ident">
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
              <define name="{$patternPrefixText}{@ident}"
                xmlns="http://relaxng.org/ns/structure/1.0">
                <notAllowed xmlns="http://relaxng.org/ns/structure/1.0"/>
              </define>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="Attributes">
                <xsl:call-template name="summarizeAttributes"/>
              </xsl:variable>
              <define name="{$patternPrefixText}{@ident}"
                xmlns="http://relaxng.org/ns/structure/1.0">
                <element name="{$name}"
                  xmlns="http://relaxng.org/ns/structure/1.0">
                  <xsl:if test="@ns">
                    <xsl:attribute name="ns">
                      <xsl:value-of select="@ns"/>
                    </xsl:attribute>
                  </xsl:if>
                  <xsl:if test="not($oddmode='tei')">
                    <a:documentation>
		      <xsl:call-template name="makeDescription">
			<xsl:with-param name="includeValList">true</xsl:with-param>
		      </xsl:call-template>
                    </a:documentation>
                  </xsl:if>
                  <xsl:choose>
                    <xsl:when test="$simplify='true'">
                      <xsl:call-template name="defineContent"/>
                      <xsl:if test="not($Attributes='') or $TEIC='true'">
                        <xsl:call-template name="defineAttributes"/>
                      </xsl:if>
                    </xsl:when>
                    <xsl:otherwise>
                      <ref name="{$patternPrefixText}{@ident}.content"
                        xmlns="http://relaxng.org/ns/structure/1.0"/>
                      <xsl:if test="not($Attributes='') or $TEIC='true'">
                        <ref name="{$patternPrefixText}{@ident}.attributes"
                          xmlns="http://relaxng.org/ns/structure/1.0"/>
                      </xsl:if>
                    </xsl:otherwise>
                  </xsl:choose>
                </element>
              </define>
              <xsl:if test="$simplify='false'">
                <define name="{$patternPrefixText}{@ident}.content"
                  xmlns="http://relaxng.org/ns/structure/1.0">
                  <xsl:call-template name="defineContent"/>
                </define>
                <xsl:if test="not($Attributes='') or $TEIC='true'">
                  <define name="{$patternPrefixText}{@ident}.attributes"
                    xmlns="http://relaxng.org/ns/structure/1.0">
                    <xsl:call-template name="defineAttributes"/>
                  </define>
                </xsl:if>
              </xsl:if>
              <xsl:if test="$parameterize='true'">
                <xsl:apply-templates mode="tangleModel"
                  select="tei:classes/tei:memberOf"/>
              </xsl:if>
            </xsl:otherwise>
          </xsl:choose>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="summarizeAttributes">
    <xsl:for-each select=".//tei:attDef">x</xsl:for-each>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('CLASSES',@key)">
        <xsl:if test="@type='atts'">x</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="defineAttributes">
    <xsl:variable name="name" select="@ident"/>
    <xsl:if test="$parameterize='true'">
      <xsl:if test="$TEIC='true'">
        <rng:ref name="{$patternPrefixText}att.global.attributes"/>
      </xsl:if>
      <xsl:for-each select="tei:classes/tei:memberOf">
        <xsl:for-each select="key('CLASSES',@key)">
          <xsl:if test="@type='atts'">
            <ref name="{$patternPrefixText}{@ident}.attributes"
              xmlns="http://relaxng.org/ns/structure/1.0"/>
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
    <xsl:if test="$TEIC='true'">
      <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
    </xsl:if>
    <!--
    <xsl:choose>
      <xsl:when test="$TEIC='true'">
	<optional xmlns="http://relaxng.org/ns/structure/1.0">
	  <attribute name="TEIform" a:defaultValue="{@ident}" xmlns="http://relaxng.org/ns/structure/1.0">
	    <text xmlns="http://relaxng.org/ns/structure/1.0"/>
	  </attribute>
	</optional>
      </xsl:when>
      <xsl:otherwise>
	<empty xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:otherwise>
    </xsl:choose>
-->
  </xsl:template>

  <xsl:template name="defineContent">
    <xsl:variable name="Contents">
      <BLAH>
        <xsl:choose>
	  <xsl:when test="tei:content/tei:valList[@type='closed' and @repeatable='true']">
	    <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
	      <rng:oneOrMore>
		<rng:choice>
		  <xsl:for-each select="tei:content">
		    <xsl:call-template name="valListChildren"/>
		  </xsl:for-each>
		</rng:choice>
	      </rng:oneOrMore>
	    </rng:list>
	  </xsl:when>
          <xsl:when test="tei:content/tei:valList[@type='closed']">
	    <xsl:for-each select="tei:content">
	      <xsl:call-template name="valListChildren"/>
	    </xsl:for-each>
          </xsl:when>
          <xsl:when test="tei:content">
            <xsl:apply-templates select="tei:content/*"/>
          </xsl:when>
          <xsl:otherwise>
            <rng:empty/>
          </xsl:otherwise>
        </xsl:choose>
      </BLAH>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="count(exsl:node-set($Contents)/BLAH/*)=0">
        <rng:empty/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="exsl:node-set($Contents)/BLAH">
          <xsl:copy-of select="*"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="valListChildren">
    <rng:choice>
      <xsl:for-each select="tei:valList/tei:valItem">
	<rng:value>
	  <xsl:choose>
	    <xsl:when test= "tei:altIdent=@ident">
	      <xsl:value-of select="@ident"/>
	    </xsl:when>
	    <xsl:when test="tei:altIdent">
	      <xsl:value-of select="normalize-space(tei:altIdent)"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@ident"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</rng:value>
	<xsl:if test="not($oddmode='tei')">
	  <a:documentation>
	    <xsl:call-template name="makeDescription">
	      <xsl:with-param name="includeValList">true</xsl:with-param>
	    </xsl:call-template>
	  </a:documentation>
	</xsl:if>
      </xsl:for-each>
    </rng:choice>
  </xsl:template>


  <xsl:template match="tei:elementSpec/@ident"/>

  <xd:doc>
    <xd:short>Process elements tei:exemplum</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="tei:exemplum" mode="weave">
    <xsl:if test="teix:egXML/* or teix:egXML/text() or text()">
      <xsl:apply-templates select="." mode="doc"/>
    </xsl:if>
  </xsl:template>

  <xd:doc>
    <xd:short>No-op processing of elements tei:gloss and tei:desc in
    normal modes, as they will always be called explicitly if
    needed.</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="tei:desc|tei:gloss" mode="weave"/>

  <xsl:template match="tei:elementSpec/tei:desc"/>

  <xsl:template match="tei:classSpec/tei:desc"/>

  <xsl:template match="tei:macroSpec/tei:desc"/>

  <xsl:template match="tei:elementSpec/tei:gloss"/>

  <xsl:template match="tei:classSpec/tei:gloss"/>

  <xsl:template match="tei:macroSpec/tei:gloss"/>


  <xsl:template match="tei:index">
    <xsl:call-template name="makeAnchor">
      <xsl:with-param name="name">IDX-<xsl:number level="any"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>


  <xsl:template match="tei:macroSpec" mode="tangle">
    <xsl:param name="msection"/>
    <xsl:param name="filename"/>
    <xsl:variable name="entCont">
      <BLAH>
        <xsl:choose>
          <xsl:when test="not($msection='') and tei:content/rng:group">
            <rng:choice>
              <xsl:apply-templates select="tei:content/rng:group/rng:*"/>
            </rng:choice>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="tei:content/rng:*"/>
          </xsl:otherwise>
        </xsl:choose>
      </BLAH>
    </xsl:variable>
    <xsl:variable name="entCount">
      <xsl:for-each select="exsl:node-set($entCont)/BLAH">
        <xsl:value-of select="count(rng:*)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="@ident=&quot;TEI.singleBase&quot;"/>
      <xsl:when test="starts-with($entCont,&quot;'&quot;)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Omit <xsl:value-of select="$entCont"/> for <xsl:value-of
              select="@ident"/></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:when test="starts-with($entCont,&quot;-&quot;)">
        <xsl:if test="$verbose='true'">
          <xsl:message>Omit <xsl:value-of select="$entCont"/> for <xsl:value-of
              select="@ident"/></xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$verbose='true'">
          <xsl:message> macroSpec <xsl:value-of select="@ident"/></xsl:message>
        </xsl:if>
        <xsl:call-template name="bitOut">
          <xsl:with-param name="grammar">true</xsl:with-param>
          <xsl:with-param name="content">
            <Wrapper>
              <define name="{$patternPrefixText}{@ident}"
                xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:if test="$parameterize='true'">
                  <xsl:if
                    test="starts-with(@ident,'macro.component')     or @predeclare='true'">
                    <xsl:attribute name="combine">choice</xsl:attribute>
                  </xsl:if>
                </xsl:if>
                <xsl:choose>
                  <xsl:when test="starts-with(@ident,'type')">
                    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
                  </xsl:when>
                  <xsl:when test="$entCount=0">
                    <rng:choice>
                      <empty xmlns="http://relaxng.org/ns/structure/1.0"/>
                    </rng:choice>
                  </xsl:when>
                  <xsl:when test="$entCount=1">
                    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
                  </xsl:when>
                  <xsl:when test="tei:content/rng:text|tei:content/rng:ref">
                    <rng:choice>
                      <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
                    </rng:choice>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:copy-of select="exsl:node-set($entCont)/BLAH/rng:*"/>
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
          <ref name="{$patternPrefixText}{$owner}"
            xmlns="http://relaxng.org/ns/structure/1.0"/>
        </define>
      </xsl:if>
    </xsl:for-each>
-->
  </xsl:template>


  <xsl:template match="tei:moduleRef" mode="tangle">
    <xsl:variable name="This" select="@key"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> .... import module [<xsl:value-of select="$This"/>
        <xsl:value-of select="@url"/>] </xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
        <Wrapper>
          <xsl:choose>
            <xsl:when test="@url and $parameterize='true'">
              <include href="{@url}" xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:copy-of select="tei:content/*"/>
              </include>
            </xsl:when>
            <xsl:when test="@url and $parameterize='false'">
              <xsl:comment>Start of import of <xsl:value-of select="@url"/></xsl:comment>
              <rng:div>
                <xsl:for-each select="document(@url)/rng:grammar">
                  <xsl:apply-templates mode="expandRNG"
                    select="*|@*|text()|comment()|processing-instruction()"/>
                </xsl:for-each>
                <xsl:copy-of select="tei:content/*"/>
              </rng:div>
              <xsl:comment>End of import of <xsl:value-of select="@url"/>
              </xsl:comment>
            </xsl:when>
            <xsl:otherwise>
              <include href="{$schemaBaseURL}{$This}.rng"
                xmlns="http://relaxng.org/ns/structure/1.0">
                <xsl:attribute name="ns">
                  <xsl:choose>
                    <xsl:when test="ancestor::tei:schemaSpec/@ns">
                      <xsl:value-of select="ancestor::tei:schemaSpec/@ns"/>
                    </xsl:when>
                    <xsl:otherwise>http://www.tei-c.org/ns/1.0</xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
                <xsl:for-each
                  select="../tei:*[@module=$This and not(@mode='add')]">
                  <xsl:apply-templates mode="tangle" select="."/>
                </xsl:for-each>
              </include>
            </xsl:otherwise>
          </xsl:choose>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>


  <xsl:template match="@*|text()|comment()|processing-instruction"
    mode="expandRNG">
    <xsl:copy/>
  </xsl:template>


  <xsl:template match="*" mode="expandRNG">
    <xsl:choose>
      <xsl:when test="local-name(.)='start'"/>
      <xsl:when test="local-name(.)='include'">
        <xsl:if test="$verbose='true'">
          <xsl:message> .... import <xsl:value-of select="@href"/></xsl:message>
        </xsl:if>
        <xsl:comment>Start of import of <xsl:value-of select="@href"/>
        </xsl:comment>
        <rng:div>
          <xsl:for-each select="document(@href)/rng:grammar">
            <xsl:apply-templates mode="expandRNG"
              select="*|@*|text()|comment()|processing-instruction()"/>
          </xsl:for-each>
        </rng:div>
        <xsl:comment>End of import of <xsl:value-of select="@href"/>
        </xsl:comment>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates mode="expandRNG"
            select="*|@*|text()|comment()|processing-instruction()"/>
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:remarks" mode="tangle"/>


  <xsl:template match="tei:remarks" mode="weave">
    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDoc"/>
    </xsl:variable>

    <xsl:variable name="langs">
      <xsl:value-of
	  select="concat(normalize-space($documentationLanguage),' ')"/>
    </xsl:variable>
    <xsl:variable name="firstLang">
      <xsl:value-of select="substring-before($langs,' ')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="preceding-sibling::tei:remarks"/>
      <xsl:when test="count(../tei:remarks)=1">
	<xsl:apply-templates select="." mode="doc"/>
      </xsl:when>
      <xsl:when test="@xml:lang=$firstLang">
	<xsl:apply-templates select="." mode="doc"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:for-each select="../tei:remarks">
	  <xsl:variable name="currentLang">
	    <xsl:call-template name="findLanguage"/>
	  </xsl:variable>
	  <xsl:if test="contains($langs,concat($currentLang,' '))">
	    <xsl:apply-templates select="." mode="doc"/>
	  </xsl:if>
	</xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template match="tei:specGrp" mode="ok">
    <xsl:param name="filename"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> processing spec grp <xsl:value-of select="@ident"
      /></xsl:message>
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

  <xsl:template match="tei:title">
    <xsl:choose>
      <xsl:when test="parent::tei:titleStmt">
        <xsl:if test="preceding-sibling::tei:title">
          <br/>
        </xsl:if>
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@level='A'"> ‘<xsl:apply-templates/>' </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="italicize">
          <xsl:with-param name="text">
            <xsl:apply-templates/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="token" mode="commentline">
    <xsl:call-template name="italicize">
      <xsl:with-param name="text">
        <xsl:value-of select="translate(.,'&#10;','')"/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:if test="following-sibling::token">
      <xsl:text>&#10;</xsl:text>
      <xsl:choose>
        <xsl:when test="contains(.,'--&gt;')">
          <xsl:apply-templates mode="normalline"
            select="following-sibling::token[1]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="commentline"
            select="following-sibling::token[1]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="token" mode="normalline">
    <xsl:choose>
      <xsl:when test="contains(.,'&lt;!--')">
        <xsl:call-template name="italicize">
          <xsl:with-param name="text">
            <xsl:value-of select="translate(.,'&#10;','')"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="following-sibling::token">
          <xsl:text>&#10;	  </xsl:text>
          <xsl:choose>
            <xsl:when test="contains(.,'--&gt;')">
              <xsl:apply-templates mode="normalline"
                select="following-sibling::token[1]"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="commentline"
                select="following-sibling::token[1]"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="breakline"/>
        <xsl:if test="following-sibling::token">
          <xsl:text>&#10;	  </xsl:text>
          <xsl:apply-templates mode="normalline"
            select="following-sibling::token[1]"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="token" mode="verbatimline">
    <xsl:call-template name="breakline"/>
    <xsl:if test="following-sibling::token">
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates mode="verbatimline"
        select="following-sibling::token[1]"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="token" mode="word">
    <xsl:param name="len"/>
    <xsl:choose>
      <xsl:when test="$len +string-length(.) &gt; $wrapLength">
        <xsl:text>&#10;	</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text> </xsl:text>
        <xsl:if test="following-sibling::token">
          <xsl:apply-templates mode="word" select="following-sibling::token[1]">
            <xsl:with-param name="len" select="8"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
        <xsl:text> </xsl:text>
        <xsl:if test="following-sibling::token">
          <xsl:apply-templates mode="word" select="following-sibling::token[1]">
            <xsl:with-param name="len" select="$len + string-length(.)"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="breakline">
    <xsl:choose>
      <xsl:when test="string-length(.)&lt;$wrapLength">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="words" select="estr:tokenize(.)"/>
        <xsl:apply-templates mode="word" select="$words[1]">
          <xsl:with-param name="len" select="0"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="compositeNumber">
    <xsl:choose>
      <xsl:when test="ancestor::tei:div0">
        <xsl:for-each select="ancestor::tei:div0">
          <xsl:number/>
        </xsl:for-each>
	<xsl:text>-</xsl:text>
        <xsl:for-each select="ancestor::tei:div1">
          <xsl:number/>
        </xsl:for-each>
        <xsl:text>.</xsl:text>
        <xsl:number from="tei:div1" level="any"/>
      </xsl:when>
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

  <xsl:template name="copyright">
    <xsl:apply-templates mode="copyrighttext"
      select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability"
    />
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
	<rng:choice>
	  <xsl:for-each select="tei:valList/tei:valItem">
	    <rng:value>
	      <xsl:choose>
		<xsl:when test= "tei:altIdent=@ident">
		  <xsl:value-of select="@ident"/>
		</xsl:when>
		<xsl:when test="tei:altIdent">
		  <xsl:value-of select="normalize-space(tei:altIdent)"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@ident"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </rng:value>
	    <xsl:if test="not($oddmode='tei')">
	      <a:documentation>
		<xsl:call-template name="makeDescription">
		  <xsl:with-param name="includeValList">true</xsl:with-param>
		</xsl:call-template>
	      </a:documentation>
	    </xsl:if>
	  </xsl:for-each>
	</rng:choice>
      </xsl:when>
      <xsl:when test="tei:valList[@type='semi']">
	<rng:choice>
	  <xsl:for-each select="tei:valList/tei:valItem">
	    <rng:value>
	      <xsl:choose>
		<xsl:when test= "tei:altIdent=@ident">
		  <xsl:value-of select="@ident"/>
		</xsl:when>
		<xsl:when test="tei:altIdent">
		  <xsl:value-of select="normalize-space(tei:altIdent)"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@ident"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </rng:value>
	    <xsl:if test="not($oddmode='tei')">
	      <a:documentation>
		<xsl:call-template name="makeDescription">
		  <xsl:with-param name="includeValList">true</xsl:with-param>
		</xsl:call-template>
	      </a:documentation>
	    </xsl:if>
	  </xsl:for-each>
	  <xsl:choose>
	    <xsl:when
		test="tei:datatype/rng:ref[@name='data.enumerated']">
	      <rng:data type="token"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates mode="forceRNG"
				   select="tei:datatype/rng:*"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</rng:choice>
      </xsl:when>
      <xsl:when test="tei:datatype/rng:*">
        <xsl:apply-templates mode="forceRNG" select="tei:datatype/rng:*"/>
      </xsl:when>
      <xsl:otherwise>
        <text xmlns="http://relaxng.org/ns/structure/1.0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- occursOutOfBounds is a template that is called only as -->
  <!-- a subroutine of makeSimpleAttribute. It is used iff the -->
  <!-- big <choose> that handles minOccurs= and maxOccurs= runs -->
  <!-- into values of those attributes that it doesn't know how -->
  <!-- to handle properly. -->

  <xsl:template name="occursOutOfBounds">
    <xsl:param name="min"/>
    <xsl:param name="max"/>
    <!-- $myMin = min( $min, 3 ) -->
    <xsl:variable name="myMin">
      <xsl:choose>
        <xsl:when test="$min > 3" >3</xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$min"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="message">
      <xsl:text>Sorry, unable to create schema that uses actual counts minOccurs=</xsl:text>
      <xsl:value-of select="$min"/>
      <xsl:text> and maxOccurs=</xsl:text>
      <xsl:value-of select="$max"/>
      <xsl:text>; approximating to minOccurs=</xsl:text>
      <xsl:value-of select="$myMin"/>
      <xsl:text> and maxOccurs=unbounded.</xsl:text>
    </xsl:variable>
    <xsl:if test="$verbose='true'">
      <xsl:message><xsl:value-of select="$message"/></xsl:message>
    </xsl:if>
    <a:documentation><xsl:value-of select="$message"/></a:documentation>
    <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
      <xsl:if test="$myMin > 0">
        <xsl:call-template name="attributeData"/>
      </xsl:if>
      <xsl:if test="$myMin > 1">
        <xsl:call-template name="attributeData"/>
      </xsl:if>
      <xsl:if test="$myMin > 2">
        <xsl:call-template name="attributeData"/>
      </xsl:if>
      <rng:zeroOrMore>
        <xsl:call-template name="attributeData"/>
      </rng:zeroOrMore>
    </rng:list>
  </xsl:template>
  

  <xsl:template name="makeSimpleAttribute">
    <xsl:variable name="name">
      <xsl:choose>
	<xsl:when test= "tei:altIdent=@ident">
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
    <rng:attribute name="{$name}">
      <xsl:if test="@ns">
        <xsl:copy-of select="@ns"/>
      </xsl:if>
      <xsl:if test="tei:defaultVal">
        <xsl:attribute name="a:defaultValue">
          <xsl:value-of select="normalize-space(tei:defaultVal)"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="not($oddmode='tei')">
        <a:documentation>
	  <xsl:call-template name="makeDescription">
	    <xsl:with-param name="includeValList">true</xsl:with-param>
	  </xsl:call-template>
        </a:documentation>
      </xsl:if>
      <xsl:variable name="minOccurs">
	<xsl:choose>
	  <xsl:when test="tei:datatype/@minOccurs">
	    <xsl:value-of select="tei:datatype/@minOccurs"/>
	  </xsl:when>
	  <xsl:otherwise>1</xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:variable name="maxOccurs">
	<xsl:choose>
	  <xsl:when test="tei:datatype/@maxOccurs">
	    <xsl:value-of select="tei:datatype/@maxOccurs"/>
	  </xsl:when>
	  <xsl:otherwise>1</xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:choose>
        <!-- This huge <xsl:choose> handles minOccurs= and -->
        <!-- maxOccurs= by brute force. It is currently -->
        <!-- set to handle: -->
        <!--
          ** min=0, max=1
          ** min=0, max=2
          ** min=0, max=[anything else]  # approximates & issues warning
          ** min=0, max=unbounded
          ** min=1, max=1
          ** min=1, max=2
          ** min=1, max=[anything else]  # approximates & issues warning
          ** min=1, max=unbounded
          ** min=2, max=2
          ** min=2, max=[anything else]  # approximates & issues warning
          ** min=2, max=unbounded
          ** min>2, max=[anything]  # approximates & issues warning
          ** anything else  # approximates, issues error msg & warning
        -->
        <!-- We don't provide for min=0 max=0, as that's -->
        <!-- the same as using <rng:empty> as content of -->
        <!-- <tei:datatype>. -->
        <xsl:when test="$minOccurs=0 and $maxOccurs=1">
          <rng:optional xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <xsl:call-template name="attributeData"/>
          </rng:optional>
        </xsl:when>
        <xsl:when test="$minOccurs=0 and $maxOccurs=2">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <rng:optional>
              <xsl:call-template name="attributeData"/>
            </rng:optional>
            <rng:optional>
              <xsl:call-template name="attributeData"/>
            </rng:optional>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs=0 and $maxOccurs > 2">
          <xsl:call-template name="occursOutOfBounds">
            <xsl:with-param name="min"><xsl:value-of select="$minOccurs"/></xsl:with-param>
            <xsl:with-param name="max"><xsl:value-of select="$maxOccurs"/></xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="$minOccurs=0 and $maxOccurs='unbounded'">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <rng:zeroOrMore>
              <xsl:call-template name="attributeData"/>
            </rng:zeroOrMore>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs=1 and $maxOccurs=1">
          <xsl:call-template name="attributeData"/>
        </xsl:when>
        <xsl:when test="$minOccurs=1 and $maxOccurs=2">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <xsl:call-template name="attributeData"/>
            <rng:optional>
              <xsl:call-template name="attributeData"/>
            </rng:optional>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs=1 and $maxOccurs > 2">
          <xsl:call-template name="occursOutOfBounds">
            <xsl:with-param name="min"><xsl:value-of select="$minOccurs"/></xsl:with-param>
            <xsl:with-param name="max"><xsl:value-of select="$maxOccurs"/></xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="$minOccurs=1 and $maxOccurs='unbounded'">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <rng:oneOrMore>
              <xsl:call-template name="attributeData"/>
            </rng:oneOrMore>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs=2 and $maxOccurs=2">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <xsl:call-template name="attributeData"/>
            <xsl:call-template name="attributeData"/>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs=2 and $maxOccurs > 2">
          <xsl:call-template name="occursOutOfBounds">
            <xsl:with-param name="min"><xsl:value-of select="$minOccurs"/></xsl:with-param>
            <xsl:with-param name="max"><xsl:value-of select="$maxOccurs"/></xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="$minOccurs=2 and $maxOccurs='unbounded'">
          <rng:list xmlns:rng="http://relaxng.org/ns/structure/1.0">
            <xsl:call-template name="attributeData"/>
            <rng:oneOrMore>
              <xsl:call-template name="attributeData"/>
            </rng:oneOrMore>
          </rng:list>
        </xsl:when>
        <xsl:when test="$minOccurs > 2">
          <xsl:call-template name="occursOutOfBounds">
            <xsl:with-param name="min"><xsl:value-of select="$minOccurs"/></xsl:with-param>
            <xsl:with-param name="max"><xsl:value-of select="$maxOccurs"/></xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message>
            <xsl:text>Internal error: minOccurs=</xsl:text>
            <xsl:value-of select="$minOccurs"/>
            <xsl:text> maxOccurs=</xsl:text>
            <xsl:value-of select="$maxOccurs"/>
            <xsl:text> is an unanticipated combination.</xsl:text>
          </xsl:message>
          <xsl:call-template name="occursOutOfBounds">
            <xsl:with-param name="min"><xsl:value-of select="$minOccurs"/></xsl:with-param>
            <xsl:with-param name="max"><xsl:value-of select="$maxOccurs"/></xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </rng:attribute>
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
        <rng:optional>
          <xsl:call-template name="makeSimpleAttribute"/>
        </rng:optional>
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
                <xsl:call-template name="makeLink">
                  <xsl:with-param name="class">classlink</xsl:with-param>
                  <xsl:with-param name="name">
                    <xsl:value-of select="@ident"/>
                  </xsl:with-param>
                  <xsl:with-param name="text">
                    <xsl:value-of select="@ident"/>
                  </xsl:with-param>
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


  <xsl:template name="showAttClasses">
    <xsl:param name="minimal">false</xsl:param>
    <xsl:variable name="clatts">
      <xsl:for-each
        select="ancestor-or-self::tei:elementSpec|ancestor-or-self::tei:classSpec">
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:choose>
            <xsl:when test="key('CLASSES',@key)">
              <xsl:for-each select="key('CLASSES',@key)">
                <xsl:if test="@type='atts'">
                  <xsl:call-template name="makeLink">
                    <xsl:with-param name="class">classlink</xsl:with-param>
                    <xsl:with-param name="name">
                      <xsl:value-of select="@ident"/>
                    </xsl:with-param>
                    <xsl:with-param name="text">
                      <xsl:value-of select="@ident"/>
                    </xsl:with-param>
                  </xsl:call-template>
                  <xsl:text> </xsl:text>
                </xsl:if>
              </xsl:for-each>
            </xsl:when>
	    <xsl:when test="ancestor::tei:schemaSpec">
	    </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="@key"/>
              <xsl:text> </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:for-each>
      </xsl:for-each>
    </xsl:variable>
    <xsl:if test="$minimal='true'">
      <xsl:text> — </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$minimal='true' and not($clatts='')">
        <xsl:text> [+ </xsl:text>
        <xsl:copy-of select="$clatts"/>
        <xsl:text>]</xsl:text>
      </xsl:when>
      <xsl:when test="not($clatts='')">
      <xsl:if test="ancestor::tei:schemaSpec and key('CLASSES','att.global')">

        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:choose>
              <xsl:when test=".//tei:attDef">
                <xsl:text>In addition to global attributes and those inherited from</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>Global attributes and those inherited from</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:if>
        <xsl:text> </xsl:text>
        <xsl:text> [</xsl:text>
        <xsl:copy-of select="$clatts"/>
        <xsl:text>] </xsl:text>
      </xsl:when>
      <xsl:when test="ancestor::tei:schemaSpec and not(key('CLASSES','att.global'))"/>
      <xsl:otherwise>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:choose>
              <xsl:when test=".//tei:attDef">
                <xsl:text>In addition to global attributes</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>Global attributes only</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="generateMembers">
    <xsl:variable name="this" select="@ident"/>
    <xsl:choose>
      <xsl:when test="key('CLASSMEMBERS',$this)">
        <xsl:for-each select="key('CLASSMEMBERS',$this)">
          <xsl:sort select="@ident"/>
          <xsl:call-template name="linkTogether">
            <xsl:with-param name="name" select="@ident"/>
          </xsl:call-template>
          <xsl:text>  </xsl:text>
          <xsl:if test="count(key('CLASSMEMBERS',@ident))&gt;0">
            <xsl:text>  [</xsl:text>
            <xsl:variable name="Key" select="@ident"/>
            <xsl:for-each select="key('CLASSMEMBERS',@ident)">
              <xsl:sort select="@ident"/>
              <xsl:call-template name="showElement">
                <xsl:with-param name="name" select="@ident"/>
              </xsl:call-template>
              <xsl:text>  </xsl:text>
            </xsl:for-each>
            <xsl:text>] </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="$lookupDatabase='true'">
        <xsl:choose>
          <xsl:when test="not($localsource='')">
            <xsl:for-each select="document($localsource)/tei:TEI">
              <xsl:for-each
                select="tei:elementSpec[tei:classes/tei:memberOf[@key=$this]]">
                <xsl:call-template name="showElement">
                  <xsl:with-param name="name" select="@ident"/>
                </xsl:call-template>
                <xsl:text> </xsl:text>
              </xsl:for-each>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="address">
              <xsl:value-of select="$TEISERVER"/>
              <xsl:text>classmembers.xq?class=</xsl:text>
              <xsl:value-of select="@ident"/>
            </xsl:variable>
            <xsl:if test="$verbose='true'">
              <xsl:message>Accessing TEISERVER: <xsl:value-of select="$address"
                /></xsl:message>
            </xsl:if>
            <xsl:for-each select="document($address)/list/item">
              <xsl:call-template name="showElement">
                <xsl:with-param name="name" select="."/>
              </xsl:call-template>
              <xsl:if test="following::item">
                <xsl:text> &#10;</xsl:text>
              </xsl:if>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="showElement">
    <xsl:param name="name"/>
    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDoc"/>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$oddmode='tei'">
        <tei:ref target="#{$name}">
          <xsl:value-of select="$name"/>
        </tei:ref>
      </xsl:when>
      <xsl:when test="$oddmode='html'">
        <xsl:choose>
          <xsl:when test="key('IDENTS',$name) and $splitLevel=-1">
            <a class="link_element" href="#{$name}"
              xmlns="http://www.w3.org/1999/xhtml">
              <xsl:value-of select="$name"/>
            </a>
          </xsl:when>
          <xsl:when test="key('IDENTS',$name) and $STDOUT='true'">
            <a class="link_element"
	       xmlns="http://www.w3.org/1999/xhtml">
	      <xsl:attribute name="href">
		<xsl:call-template name="getSpecURL">
		  <xsl:with-param name="name">
		    <xsl:value-of select="$name"/>
		  </xsl:with-param>
		  <xsl:with-param name="type">
		    <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:attribute>
	      <xsl:value-of select="$name"/>
            </a>
          </xsl:when>
          <xsl:when test="key('IDENTS',$name)">
            <a class="link_element" href="ref-{$name}{$outputSuffix}"
              xmlns="http://www.w3.org/1999/xhtml">
              <xsl:value-of select="$name"/>
            </a>
          </xsl:when>
          <xsl:otherwise>
	    <a xmlns="http://www.w3.org/1999/xhtml">
	      <xsl:attribute name="href">
		<xsl:value-of select="$TEISERVER"/>
		<xsl:text>tag.xq?name=</xsl:text>
		<xsl:value-of select="$name"/>
		<xsl:text>&amp;documentationLanguage=</xsl:text>
		<xsl:value-of select="$documentationLanguage"/>
	      </xsl:attribute>
              <xsl:value-of select="$name"/>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="$oddmode='pdf'">
        <fo:inline font-style="italic">
          <xsl:value-of select="$name"/>
        </fo:inline>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="linkStyle"/>


  <xsl:template name="getSpecURL">
    <xsl:param name="name"/>
    <xsl:param name="type"/>
    <xsl:choose>
      <xsl:when test="$type='macro'">
	<xsl:for-each select="key('IDS','REFENT')">
	  <xsl:apply-templates mode="generateLink" select="."/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="$type='element'">
	<xsl:for-each select="key('IDS','REFTAG')">
	  <xsl:apply-templates mode="generateLink" select="."/>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="$type='class'">
	<xsl:for-each select="key('IDS','REFCLA')">
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
    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDoc"/>
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
        <xsl:choose>
          <xsl:when test="$oddmode='tei'">
            <tei:ref>
              <xsl:attribute name="target">
		<xsl:value-of select="$TEISERVER"/>
	        <xsl:text>tag.xq?name=</xsl:text>
		<xsl:value-of select="$partialname"/>
		<xsl:text>&amp;documentationLanguage=</xsl:text>
		<xsl:value-of select="$documentationLanguage"/>
	      </xsl:attribute>
              <xsl:value-of select="$link"/>
            </tei:ref>
          </xsl:when>
          <xsl:otherwise>
            <a xmlns="http://www.w3.org/1999/xhtml">
              <xsl:attribute name="href">
		<xsl:value-of select="$TEISERVER"/>
	        <xsl:text>tag.xq?name=</xsl:text>
		<xsl:value-of select="$partialname"/>
		<xsl:text>&amp;documentationLanguage=</xsl:text>
		<xsl:value-of select="$documentationLanguage"/>
	      </xsl:attribute>
              <xsl:value-of select="$link"/>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="$oddmode='html' and $splitLevel=-1">
        <a class="link_odd" href="#{$partialname}" xmlns="http://www.w3.org/1999/xhtml">
          <xsl:value-of select="$link"/>
        </a>
      </xsl:when>
      <xsl:when test="$oddmode='html' and $STDOUT='true'">
	<a class="link_odd" xmlns="http://www.w3.org/1999/xhtml">
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
        <a class="link_odd" href="{concat('ref-',$partialname,'.html')}"
          xmlns="http://www.w3.org/1999/xhtml">
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


  <xsl:template name="makeTagsetInfo">
    <xsl:value-of select="@module"/>
    <xsl:if test="$verbose='true'">
      <xsl:message> tagset <xsl:value-of select="@xml:id"/>: <xsl:value-of
          select="@module"/></xsl:message>
    </xsl:if>
  </xsl:template>


  <xsl:template name="processSchemaFragment">
    <xsl:param name="filename"/>
    <xsl:variable name="secnum">
      <xsl:call-template name="sectionNumber"/>
    </xsl:variable>
    <!--
	<xsl:if test="@xml:id">
	<xsl:comment>[<xsl:value-of select="@xml:id"/>] <xsl:value-of
	select="$secnum"/>
	<xsl:if test="@n">
	<xsl:text>: </xsl:text>
	<xsl:value-of select="@n"/>
	</xsl:if>
	</xsl:comment>
	</xsl:if>
    -->
    <xsl:apply-templates mode="tangle"/>
    <!--
	<xsl:if test="@xml:id">
	<xsl:comment> end of [<xsl:value-of select="@xml:id"/>]  <xsl:value-of select="$secnum"/>    
	</xsl:comment>
	</xsl:if>
    -->
  </xsl:template>

  <xsl:template name="processSpecDesc">
    <xsl:variable name="name">
      <xsl:value-of select="@key"/>
    </xsl:variable>
    <xsl:variable name="atts">
      <xsl:choose>
        <xsl:when test="@rend='noatts'">-</xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="normalize-space(@atts)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$name=''">
        <xsl:message>ERROR: no key attribute on specDesc</xsl:message>
      </xsl:when>
      <xsl:when test="key('IDENTS',$name)">
        <xsl:apply-templates mode="show" select="key('IDENTS',$name)">
          <xsl:with-param name="atts" select="$atts"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:when test="not($localsource='')">
        <xsl:for-each select="document($localsource)/tei:TEI">
          <xsl:apply-templates mode="show" select="tei:*[@ident=$name]">
            <xsl:with-param name="atts" select="$atts"/>
          </xsl:apply-templates>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="loc">
          <xsl:value-of select="$TEISERVER"/>
          <xsl:text>copytag.xq?name=</xsl:text>
          <xsl:value-of select="$name"/>
        </xsl:variable>
        <xsl:if test="$verbose='true'">
          <xsl:message>Accessing TEISERVER: <xsl:value-of select="$loc"
          /></xsl:message>
        </xsl:if>
        <xsl:apply-templates mode="show" select="document($loc)/tei:*">
          <xsl:with-param name="atts" select="$atts"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="processatts">
    <xsl:param name="values"/>
    <xsl:if test="not($values = '')">
      <xsl:apply-templates select="key('IDENTS',substring-before($values,' '))"/>
      <xsl:call-template name="processatts">
        <xsl:with-param name="values" select="substring-after($values,' ')"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="sectionNumber">
    <xsl:for-each
      select="(ancestor::tei:div1|ancestor::tei:div2|ancestor::tei:div3|ancestor::tei:div4)[last()]">
      <xsl:for-each select="ancestor-or-self::tei:div1">
        <xsl:number from="tei:body" level="any"/>
        <xsl:text>.</xsl:text>
      </xsl:for-each>
      <xsl:number count="tei:div2|tei:div3|tei:div4" from="tei:div1"
        level="multiple"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="make-ns-declaration">
    <xsl:param name="is-default"/>
    <xsl:param name="prefix"/>
    <xsl:param name="uri"/>
  </xsl:template>


  <xsl:template name="inhnamespace"/>


  <xsl:template match="s:*"/>


  <xsl:template match="tei:altIdent"/>

  <xsl:template match="a:*">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="tei:classSpec" mode="processDefaultAtts">
    <xsl:if test="$verbose='true'">
      <xsl:message> .. default attribute settings for <xsl:value-of
          select="@ident"/></xsl:message>
    </xsl:if>
    <xsl:call-template name="bitOut">
      <xsl:with-param name="grammar">true</xsl:with-param>
      <xsl:with-param name="content">
        <Wrapper>
          <rng:define combine="choice"
            name="{$patternPrefixText}{@ident}.attributes">
            <rng:empty/>
          </rng:define>
        </Wrapper>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <!-- Force an output element in the RNG namespace. 
       I don't see why this is necessary, but xsltproc gets
       it wrong otherwise. I suspect a bug there somewhere.
  -->

  <xsl:template match="*" mode="forceRNG">
    <xsl:element name="{local-name(.)}"
      xmlns="http://relaxng.org/ns/structure/1.0">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="forceRNG"/>
    </xsl:element>
  </xsl:template>


  <xsl:template match="rng:ref" mode="forceRNG">
    <xsl:choose>
      <xsl:when test="key('IDENTS',@name)">
	<xsl:element name="ref"
		     xmlns="http://relaxng.org/ns/structure/1.0">
	  <xsl:attribute name="name">
	    <xsl:value-of select="$patternPrefixText"/>
	    <xsl:value-of select="@name"/>
	  </xsl:attribute>
	</xsl:element>
      </xsl:when>
      <xsl:when test="key('IDENTS',substring-before(@name,'_'))">
	<xsl:element name="ref"
		     xmlns="http://relaxng.org/ns/structure/1.0">
	  <xsl:attribute name="name">
	    <xsl:value-of select="$patternPrefixText"/>
	    <xsl:value-of select="@name"/>
	  </xsl:attribute>
	</xsl:element>
      </xsl:when>
      <xsl:otherwise>
	<xsl:element name="ref"
		     xmlns="http://relaxng.org/ns/structure/1.0">
	  <xsl:attribute name="name">
	    <xsl:value-of select="@name"/>
	  </xsl:attribute>
	</xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:schemaSpec</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>

  <xsl:template match="tei:schemaSpec">
    <xsl:call-template name="processSchemaFragment"/>
  </xsl:template>

  <xsl:template name="typewriter"/>

  <xsl:template name="refdoc"/>

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
      <xsl:when test="contains($processor,'SAXON')">
        <xsl:copy-of select="$body"/>
      </xsl:when>
      <xsl:when test="$method='text' and element-available('exsl:document')">
        <xsl:if test="$verbose='true'">
          <xsl:message> File [<xsl:value-of select="$outputDir"/>/<xsl:value-of
              select="@ident"/><xsl:value-of select="$suffix"/>] </xsl:message>
        </xsl:if>
        <xsl:if test="element-available('exsl:document')">
          <exsl:document href="{$outputDir}/{@ident}{$suffix}" method="text">
            <xsl:copy-of select="$body"/>
          </exsl:document>
        </xsl:if>
      </xsl:when>
      <xsl:when test="element-available('exsl:document')">
        <xsl:if test="$verbose='true'">
          <xsl:message> File [<xsl:value-of select="$outputDir"/>/<xsl:value-of
              select="@ident"/><xsl:value-of select="$suffix"/>] </xsl:message>
        </xsl:if>
        <xsl:if test="element-available('exsl:document')">
          <exsl:document href="{$outputDir}/{@ident}{$suffix}" indent="yes"
            method="xml">
            <xsl:copy-of select="$body"/>
          </exsl:document>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$body"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="showDate">
    <xsl:variable name="processor">
      <xsl:value-of select="system-property('xsl:vendor')"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="function-available('edate:date-time')">
        <xsl:value-of select="edate:date-time()"/>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON')">
        <xsl:value-of select="Date:toString(Date:new())"
          xmlns:Date="/java.util.Date"/>
      </xsl:when>
      <xsl:otherwise> (unknown date) </xsl:otherwise>
    </xsl:choose>
  </xsl:template>



  <xsl:template name="makeDescription">
    <xsl:param name="includeValList">false</xsl:param>
    <xsl:variable name="documentationLanguage">
      <xsl:call-template name="generateDoc"/>
    </xsl:variable>
    <xsl:variable name="langs">
      <xsl:value-of
	  select="concat(normalize-space($documentationLanguage),' ')"/>
    </xsl:variable>
    <xsl:variable name="firstLang">
      <xsl:value-of select="substring-before($langs,' ')"/>
    </xsl:variable>
    <!-- first the gloss -->
    <xsl:choose>
      <xsl:when test="not(tei:gloss)"/>
      <xsl:when test="string-length(tei:gloss)=0"/>
      <xsl:when test="count(tei:gloss)=1 and not(tei:gloss[@xml:lang])">
	<xsl:text>(</xsl:text>
	<xsl:value-of select="tei:gloss"/>
	<xsl:text>) </xsl:text>
      </xsl:when>
      <xsl:when test="tei:gloss[@xml:lang=$firstLang]">
	<xsl:text>(</xsl:text>
	<xsl:value-of select="tei:gloss[@xml:lang=$firstLang]"/>
	<xsl:text>) </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="G">
	<xsl:for-each select="tei:gloss">
	  <xsl:variable name="currentLang">
	    <xsl:call-template name="findLanguage"/>
	  </xsl:variable>
	  <xsl:if test="contains($langs,concat($currentLang,' '))">
	    <xsl:text>(</xsl:text>
	    <xsl:value-of select="."/>
	    <xsl:text>) </xsl:text>
	  </xsl:if>
	</xsl:for-each>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="$G='' and tei:gloss[not(@xml:lang)]">
	    <xsl:text>(</xsl:text>
	    <xsl:value-of select="tei:gloss[not(@xml:lang)]"/>
	    <xsl:text>) </xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="$G"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>

    </xsl:choose>
<!-- now the description -->
    <xsl:choose>
      <xsl:when test="not(tei:desc)"/>
      <xsl:when test="count(tei:desc)=1">
	<xsl:value-of select="tei:desc"/>
      </xsl:when>
      <xsl:when test="tei:desc[@xml:lang=$firstLang]">
	<xsl:value-of select="tei:desc[@xml:lang=$firstLang]"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="D">
	  <xsl:for-each select="tei:desc">
	    <xsl:variable name="currentLang">
	      <xsl:call-template name="findLanguage"/>
	    </xsl:variable>
	    <xsl:if test="contains($langs,concat($currentLang,' '))">
	      <xsl:value-of select="."/>
	    </xsl:if>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="$D='' and tei:desc[not(@xml:lang)]">
	    <xsl:text>(</xsl:text>
	      <xsl:value-of select="tei:desc[not(@xml:lang)]"/>
	      <xsl:text>) </xsl:text>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:copy-of select="$D"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="$includeValList='false'"/>
      <xsl:when test="tei:valList[@type='open']">
	<xsl:text>&#10;</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
	    <xsl:text>Sample values include</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
	<xsl:for-each select="tei:valList/tei:valItem">
	  <xsl:number/>
	  <xsl:text>] </xsl:text>
	  <xsl:choose>
	    <xsl:when test= "tei:altIdent=@ident">
	      <xsl:value-of select="@ident"/>
	    </xsl:when>
	    <xsl:when test="tei:altIdent">
	      <xsl:value-of select="normalize-space(tei:altIdent)"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@ident"/>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test="tei:gloss">
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="tei:gloss"/>
	    <xsl:text>)</xsl:text>
	  </xsl:if>
	  <xsl:if test="following-sibling::tei:valItem">
	    <xsl:text>; </xsl:text>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
      <xsl:when test="tei:valList[@type='semi']">
	<xsl:text>&#10;</xsl:text>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
	    <xsl:text>Suggested values include</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
	<xsl:for-each select="tei:valList/tei:valItem">
	  <xsl:number/>
	  <xsl:text>] </xsl:text>
	  <xsl:choose>
	    <xsl:when test= "tei:altIdent=@ident">
	      <xsl:value-of select="@ident"/>
	    </xsl:when>
	    <xsl:when test="tei:altIdent">
	      <xsl:value-of select="normalize-space(tei:altIdent)"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@ident"/>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:if test="tei:gloss">
	    <xsl:text> (</xsl:text>
	    <xsl:value-of select="tei:gloss"/>
	    <xsl:text>)</xsl:text>
	  </xsl:if>
	  <xsl:if test="following-sibling::tei:valItem">
	    <xsl:text>; </xsl:text>
	  </xsl:if>
	</xsl:for-each>
      </xsl:when>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="findLanguage">
    <xsl:choose>
      <xsl:when test="@xml:lang">
	<xsl:value-of select="@xml:lang"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:*[@xml:lang]">
	  <xsl:value-of select="(ancestor::tei:*[@xml:lang])[1]/@xml:lang"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>en</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>

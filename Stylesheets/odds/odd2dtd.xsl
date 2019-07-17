<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a t tei fo rng xs"
                version="2.0">
  <xsl:import href="teiodds.xsl"/>
  <xsl:import href="classatts.xsl"/>
  <xsl:import href="../common/i18n.xsl"/>
  <xsl:import href="../common/functions.xsl"/>
  <xsl:import href="../common/common_param.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making DTD from ODD </p>
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
  <xsl:output method="text"/>
  <xsl:param name="autoGlobal">false</xsl:param>
  <xsl:param name="verbose"/>
  <xsl:param name="outputDir"/>
  <xsl:param name="appendixWords"> </xsl:param>
  <xsl:param name="filesuffix"/>
  <xsl:template name="headingNumberSuffix">
      <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:param name="numberBackHeadings"/>
  <xsl:param name="numberFrontHeadings"/>
  <xsl:param name="numberHeadings"/>
  <xsl:param name="numberHeadingsDepth"/>
  <xsl:param name="oddmode">dtd</xsl:param>
  <xsl:param name="prenumberedHeadings">false</xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="generateNamespacePrefix">false</xsl:param>
  <xsl:param name="namespacePrefix"/>
  <xsl:key match="tei:moduleSpec" name="Modules" use="1"/>
  <xsl:variable name="nsPrefix">
      <xsl:choose>
         <xsl:when test="generateNamespacePrefix='false'"/>
         <xsl:when test="not($namespacePrefix='')">
            <xsl:value-of select="$namespacePrefix"/>
         </xsl:when>
         <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)/@ns">
            <xsl:variable name="n" select="key('LISTSCHEMASPECS',whichSchemaSpec)/@ns"/>
            <xsl:choose>
               <xsl:when test="$n='http://www.w3.org/2005/11/its'">its:</xsl:when>
               <xsl:when test="$n='http://www.tei-c.org/ns/1.0'">tei:</xsl:when>
            </xsl:choose>
         </xsl:when>
      </xsl:choose>
  </xsl:variable>
  <xsl:key name="NSELEMENTS" match="tei:elementSpec[@ns]|tei:attDef[@ns]" use="1"/>
  <xsl:key match="tei:moduleSpec[@ident]" name="FILES" use="@ident"/>
  <xsl:template match="/">
    <xsl:variable name="wrappedClassRefs">
      <xsl:apply-templates mode="preprocess"/>
    </xsl:variable>
    <xsl:variable name="resolvedClassatts">
      <xsl:apply-templates select="$wrappedClassRefs"  mode="classatts"/>
    </xsl:variable>
    <xsl:for-each select="$resolvedClassatts">
      <xsl:choose>
         <xsl:when test="key('SCHEMASPECS',1)">
            <xsl:apply-templates select="key('LISTSCHEMASPECS',$whichSchemaSpec)"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="byModule"/>
         </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="byModule">
      <xsl:for-each select="key('Modules',1)">
         <xsl:sort order="descending" select="@ident"/>
         <xsl:if test="$verbose='true'">
            <xsl:message> File [<xsl:value-of select="@ident"/>] </xsl:message>
         </xsl:if>
         <xsl:call-template name="generateOutput">
            <xsl:with-param name="suffix">.dtd</xsl:with-param>
            <xsl:with-param name="method">text</xsl:with-param>
            <xsl:with-param name="body">
               <xsl:call-template name="dtdComment">
                  <xsl:with-param name="text"> 
	                    <xsl:text>DTD module </xsl:text>
	                    <xsl:value-of select="@ident"/>
	                    <xsl:text>. generated from ODD source </xsl:text>
	                    <xsl:sequence select="tei:whatsTheDate()"/>
	                    <xsl:text>. </xsl:text>
			    <xsl:call-template name="copyright"/>
	                    <xsl:call-template name="makeTEIVersion"/>
	                    <xsl:sequence select="tei:makeDescription(., true(), true())"/>
	                 </xsl:with-param>
               </xsl:call-template>
               <xsl:choose>
                  <xsl:when test="@type='core'">
		    <xsl:text>&lt;!ENTITY % TEI.extensions.ent '' &gt;&#10;</xsl:text>
		    <xsl:text>%TEI.extensions.ent;&#10;</xsl:text>
		    <xsl:call-template name="dtdComment">
		      <xsl:with-param name="text">
			<xsl:text>list of element names</xsl:text>
                        </xsl:with-param>
		    </xsl:call-template>
		    <xsl:if test="$parameterize='true' and count(key('NSELEMENTS',1))&gt;0">
		      <xsl:text>&lt;!ENTITY % NS '</xsl:text>
		      <xsl:value-of select="$nsPrefix"/>
		      <xsl:text>' &gt;&#10;</xsl:text>
		    </xsl:if>
		    <xsl:if test="$parameterize='true'">
		      <xsl:call-template name="NameList"/>
		    </xsl:if>
		    <xsl:call-template name="datatypeMacros"/>
		    <xsl:call-template name="omissability"/>
		    <xsl:call-template name="entityModules"/>
		    <xsl:call-template name="predeclaredClasses"/>
		    <xsl:call-template name="predeclaredMacros"/>
		    <xsl:call-template name="normalClasses"/>
		    <xsl:call-template name="normalMacros"/>
		    <xsl:text>&#10;&lt;!ENTITY % TEI.extensions.dtd '' &gt;&#10;</xsl:text>
		    <xsl:text>%TEI.extensions.dtd;&#10;</xsl:text>
		    <xsl:apply-templates mode="tangle" select="key('ElementModule',@ident)">
		      <xsl:sort select="@ident"/>
		    </xsl:apply-templates>
		    <xsl:call-template name="elementModules"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:apply-templates mode="tangle" select="key('ElementModule',@ident)">
                        <xsl:sort select="@ident"/>
                     </xsl:apply-templates>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:with-param>
         </xsl:call-template>
         <xsl:if test="not(@type='core')">
            <xsl:call-template name="generateOutput">
               <xsl:with-param name="suffix">-decl.dtd</xsl:with-param>
               <xsl:with-param name="method">text</xsl:with-param>
               <xsl:with-param name="body">
                  <xsl:call-template name="dtdComment">
                     <xsl:with-param name="text">
		                      <xsl:text>TEI P5 entity declaration module for </xsl:text>
		                      <xsl:value-of select="@ident"/>
		                      <xsl:text>. generated from ODD source </xsl:text>
		                      <xsl:sequence select="tei:whatsTheDate()"/>
		                      <xsl:text>.&#10;</xsl:text>
				      <xsl:call-template name="copyright"/>
		                      <xsl:call-template name="makeTEIVersion"/>
		                      <xsl:sequence select="tei:makeDescription(., true(), true())"/>
	                    </xsl:with-param>
                  </xsl:call-template>
                  <xsl:call-template name="datatypeMacros"/>
                  <xsl:call-template name="normalClasses"/>
                  <xsl:call-template name="normalMacros"/>
                  <xsl:call-template name="predeclaredMacros"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:if>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="elementModules">
      <xsl:for-each select="key('Modules',1)">
         <xsl:sort order="descending" select="@ident"/>
         <xsl:if test="not(@type='core')">
            <xsl:text>&#10;&lt;![%TEI.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>;[
	&lt;!ENTITY % file.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text> PUBLIC '-//TEI P5//ELEMENTS </xsl:text>
            <xsl:value-of select="tei:altIdent[@type='FPI']"/>
            <xsl:text>//EN' '</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>.dtd' &gt;
	%file.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>;
	]]&gt;&#10;</xsl:text>
         </xsl:if>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="entityModules">
    <xsl:sequence select="tei:dtdcomment('Module declarations')"/>
      <xsl:for-each select="key('Modules',1)">
         <xsl:sort order="descending" select="@ident"/>
         <xsl:if test="not(@type='core')">
            <xsl:text>&lt;!ENTITY % TEI.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text> 'IGNORE' &gt;&#10;</xsl:text>
            <xsl:text>&lt;![%TEI.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>;[
	&lt;!ENTITY % file.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>-decl PUBLIC '-//TEI P5//ENTITIES </xsl:text>
            <xsl:value-of select="tei:altIdent[@type='FPI']"/>
            <xsl:text>//EN' '</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>-decl.dtd' &gt;
	%file.</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>-decl;
	]]&gt;&#10;</xsl:text>
         </xsl:if>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="omissability">
    <xsl:sequence select="tei:dtdcomment('legacy declaration of omissability indicators')"/>
      <xsl:text>&lt;!ENTITY % TEI.XML 'INCLUDE' &gt;&#10;</xsl:text>
      <xsl:text>&lt;![%TEI.XML;[&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RO '' &gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RR '' &gt;&#10;</xsl:text>
      <xsl:text>]]&gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RO '- o' &gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RR '- -' &gt;&#10;</xsl:text>
  </xsl:template>
  <xsl:template name="datatypeMacros">
      <xsl:if test="key('DataMacroModule',@ident)">
	<xsl:sequence select="tei:dtdcomment('Start datatype macro declarations')"/>
        <xsl:apply-templates mode="tangle" select="key('DataMacroModule',@ident)"/>
	<xsl:sequence select="tei:dtdcomment('End of datatype macro declarations')"/>
      </xsl:if>
  </xsl:template>
  <xsl:template name="predeclaredMacros">
    <xsl:if test="key('PredeclareMacrosModule',@ident)">
      <xsl:sequence select="tei:dtdcomment('Start of pre-declared macros')"/>
      <xsl:for-each select="key('PredeclareMacrosModule',@ident)">
         <xsl:apply-templates mode="tangle" select="."/>
      </xsl:for-each>
      <xsl:sequence select="tei:dtdcomment('End of pre-declared macros')"/>
    </xsl:if>
  </xsl:template>
  <xsl:template name="normalClasses">
    <xsl:if test="key('ClassModule',@ident)">
      <xsl:sequence select="tei:dtdcomment('Start of classes')"/>
      <xsl:apply-templates mode="tangle" select="key('ClassModule',@ident)"/>
      <xsl:sequence select="tei:dtdcomment('End of classes')"/>
    </xsl:if>
  </xsl:template>
  <xsl:template name="predeclaredClasses">
    <xsl:if test="key('predeclaredClasses',1)">
      <xsl:sequence select="tei:dtdcomment('Start of pre-declared classes')"/>
      <xsl:for-each select="key('predeclaredClasses',1)">
        <xsl:choose>
          <xsl:when test="@type='atts'">
               <xsl:call-template name="classAtt">
                 <xsl:with-param name="declare">false</xsl:with-param>
               </xsl:call-template>
          </xsl:when>
          <xsl:when test="@type='model'">
            <xsl:call-template name="classModel"/>
            </xsl:when>
        </xsl:choose>
      </xsl:for-each>
      <xsl:sequence select="tei:dtdcomment('End of pre-declared classes')"/>
    </xsl:if>
  </xsl:template>
  <xsl:template name="normalMacros">
      <xsl:if test="@type='core' and key('PredeclareAllMacros',1)">
	<xsl:sequence select="tei:dtdcomment('Global pre-declared macros')"/>
         <xsl:for-each select="key('PredeclareAllMacros',1)">
            <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text> ''</xsl:text>
            <xsl:text>&gt;&#10;</xsl:text>
         </xsl:for-each>
      </xsl:if>
      <xsl:sequence select="tei:dtdcomment('Start rest of  macro declarations')"/>
      <xsl:for-each select="key('MacroModule',@ident)">
        <xsl:choose>
          <xsl:when test="@predeclare='true'"/>
          <!--	    <xsl:when test="key('PredeclareMacros',@ident)"/>-->
	  <xsl:otherwise>
	    <xsl:apply-templates mode="tangle" select="."/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
      <xsl:sequence select="tei:dtdcomment('End macros')"/>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
      <xsl:if test="$verbose='true'">
         <xsl:message>schemaSpec <xsl:value-of select="@ident"/>
			      </xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="$outputDir='' or $outputDir='-'">
            <xsl:call-template name="writeSchema"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="generateOutput">
               <xsl:with-param name="suffix">.dtd</xsl:with-param>
               <xsl:with-param name="method">text</xsl:with-param>
               <xsl:with-param name="body">
                  <xsl:call-template name="writeSchema"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="writeSchema">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>DTD generated from ODD source </xsl:text>
            <xsl:sequence select="tei:whatsTheDate()"/>
            <xsl:text>. </xsl:text>
		  <xsl:value-of
		      select="(/tei:TEI/tei:text/tei:front/tei:titlePage/tei:docDate,'.')"
		      separator=""/>
	    <xsl:call-template name="copyright"/>
            <xsl:call-template name="makeTEIVersion"/>
            <xsl:sequence select="tei:makeDescription(., true(), true())"/>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:choose>
         <xsl:when test="tei:specGrpRef">
            <xsl:variable name="SPECS">
               <tei:schemaSpec>
                  <xsl:copy-of select="@*"/>
                  <xsl:apply-templates mode="expandSpecs"/>
               </tei:schemaSpec>
            </xsl:variable>
            <xsl:for-each select="$SPECS/tei:schemaSpec">
               <xsl:call-template name="schemaSpecBody"/>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="schemaSpecBody"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="schemaSpecBody">
      <xsl:if test="$parameterize='true' and count(key('NSELEMENTS',1))&gt;0">
         <xsl:text>&lt;!ENTITY % NS '</xsl:text>
         <xsl:value-of select="$nsPrefix"/>
         <xsl:text>' &gt;&#10;</xsl:text>
      </xsl:if>
      <xsl:if test="$parameterize='true'">
         <xsl:call-template name="NameList"/>
      </xsl:if>

      <xsl:if test="tei:dataSpec">
	<xsl:sequence select="tei:dtdcomment('start datatypes')"/>
	<xsl:apply-templates mode="tangle" select="tei:dataSpec"/>
	<xsl:sequence select="tei:dtdcomment('end datatypes')"/>
      </xsl:if>

      <xsl:if test="tei:classSpec[@predeclare='true']">
	<xsl:sequence select="tei:dtdcomment('predeclared classes')"/>
         <xsl:for-each select="tei:classSpec[@predeclare='true']">
	   <xsl:choose>
	     <xsl:when test="@type='atts'">
	       <xsl:call-template name="classAtt">
		 <xsl:with-param name="declare">true</xsl:with-param>
	       </xsl:call-template>
	     </xsl:when>
	     <xsl:when test="@type='model' and $parameterize='false'">
	       <xsl:call-template name="classModel">
		 <xsl:with-param name="declare">true</xsl:with-param>
	       </xsl:call-template>
	     </xsl:when>
	   </xsl:choose>
         </xsl:for-each>
	 <xsl:sequence select="tei:dtdcomment('end of predeclared classes')"/>
         <xsl:apply-templates mode="tangle" select="tei:classSpec"/>
      </xsl:if>

      <xsl:if test="tei:macroSpec[@predeclare='true']">
	<xsl:sequence select="tei:dtdcomment('start predeclared patterns')"/>
	<xsl:apply-templates mode="tangle" select="tei:macroSpec[@predeclare='true']"/>
	<xsl:sequence select="tei:dtdcomment('end predeclared patterns')"/>
      </xsl:if>

      <xsl:if test="tei:macroSpec[ not( @predeclare eq 'true' ) ]">
	<xsl:sequence select="tei:dtdcomment('start rest of patterns')"/>
        <xsl:apply-templates mode="tangle" select="tei:macroSpec[ not( @predeclare eq 'true' ) ]"/>
	<xsl:sequence select="tei:dtdcomment('end patterns')"/>
      </xsl:if>

      <xsl:if test="not(tei:classSpec[@predeclare='true'])">
	<xsl:sequence select="tei:dtdcomment('start classes')"/>
         <xsl:apply-templates mode="tangle"  select="tei:classSpec[@type='atts']"/>
         <xsl:apply-templates mode="tangle" select="tei:classSpec[@type='model']"/>
	 <xsl:sequence select="tei:dtdcomment('stop classes')"/>
      </xsl:if>
      
      <xsl:sequence select="tei:dtdcomment('start elements')"/>
      <xsl:apply-templates mode="tangle" select="tei:elementSpec">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
      <xsl:sequence select="tei:dtdcomment('end elements')"/>
  </xsl:template>

  <xsl:template match="tei:macroSpec[@xml:id='TEIGIS']" mode="tangle"/>
  <xsl:template name="NameList">
      <xsl:choose>
         <xsl:when test="self::tei:schemaSpec">
            <xsl:for-each select="tei:elementSpec">
               <xsl:sort select="@ident"/>
               <xsl:call-template name="declareAnElement"/>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="key('ELEMENTDOCS',1)">
               <xsl:sort select="@ident"/>
               <xsl:call-template name="declareAnElement"/>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
      <!-- walk over all the elementSpec elements and make list of 
	 elements -->
  </xsl:template>
  <xsl:template name="declareAnElement">
      <xsl:if test="not(starts-with(@ident,'%'))">
         <xsl:text>&lt;!ENTITY % n.</xsl:text>
         <xsl:value-of select="@ident"/>
         <xsl:text> "</xsl:text>
         <xsl:if test="$parameterize='true' and (@ns)">
            <xsl:text>%NS;</xsl:text>
         </xsl:if>
	<xsl:value-of select="tei:createSpecName(.)"/>
         <xsl:text>"&gt;&#10;</xsl:text>
      </xsl:if>
  </xsl:template>
  <xsl:template name="linkStyle"/>
  <xsl:template name="makeAnchor">
      <xsl:param name="name"/>
  </xsl:template>
  <xsl:template match="rng:element[rng:anyName]">
      <xsl:text>#PCDATA</xsl:text>
  </xsl:template>
  <xsl:template match="rng:zeroOrMore">
      <xsl:variable name="body">
         <xsl:call-template name="content">
            <xsl:with-param name="sep" select="','"/>
         </xsl:call-template>
      </xsl:variable>
      <xsl:if test="not($body='')">
         <xsl:call-template name="checkStart"/>
         <xsl:value-of select="$body"/>
         <xsl:text>*</xsl:text>
         <xsl:call-template name="checkEnd"/>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:oneOrMore">
      <xsl:variable name="body">
         <xsl:call-template name="content">
            <xsl:with-param name="sep" select="','"/>
         </xsl:call-template>
      </xsl:variable>
      <xsl:if test="not($body='')">
         <xsl:call-template name="checkStart"/>
         <xsl:value-of select="$body"/>
         <xsl:text>+</xsl:text>
         <xsl:call-template name="checkEnd"/>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:optional">
      <xsl:variable name="body">
         <xsl:call-template name="content">
            <xsl:with-param name="sep" select="','"/>
         </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$body=''"> </xsl:when>
	 <xsl:when test="matches($body,'\*$')">
            <xsl:value-of select="$body"/>
	 </xsl:when>
	 <xsl:otherwise>
            <xsl:call-template name="checkStart"/>
            <xsl:value-of select="$body"/>
            <xsl:text>?</xsl:text>
            <xsl:call-template name="checkEnd"/>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:choice">
      <xsl:call-template name="checkStart"/>
      <xsl:choose>
         <xsl:when test="rng:value and (ancestor::tei:elementSpec or ancestor::tei:macroSpec)">
            <xsl:text>(#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:when test="rng:value">
            <xsl:text> (</xsl:text>
            <xsl:for-each select="rng:value">
               <xsl:value-of select="."/>
               <xsl:if test="following-sibling::rng:value">
                  <xsl:text>|</xsl:text>
               </xsl:if>
            </xsl:for-each>) </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="content">
               <xsl:with-param name="sep" select="'|'"/>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="checkEnd"/>
  </xsl:template>
  <xsl:template match="rng:group|rng:mixed">
      <xsl:call-template name="content">
         <xsl:with-param name="sep" select="','"/>
      </xsl:call-template>
  </xsl:template>
  <xsl:template match="rng:interleave">
      <xsl:call-template name="content">
         <xsl:with-param name="sep" select="','"/>
      </xsl:call-template>
  </xsl:template>
  <xsl:template name="content">
      <xsl:param name="sep"/>
        <xsl:variable name="parent" select="local-name(..)"/>
        <xsl:variable name="contentbody">
          <xsl:variable name="members">
            <M>
              <xsl:for-each select="rng:*|processing-instruction()">
                  <xsl:variable name="F">
                     <xsl:choose>
                        <xsl:when test="self::processing-instruction()">
                           <xsl:apply-templates select="."/>
                        </xsl:when>
                        <xsl:when test="self::rng:ref and $parameterize='false'">
                           <xsl:choose>
                              <xsl:when test="key('CLASSES',@name)">
                                 <xsl:variable name="exists">
                                    <xsl:call-template name="checkClass">
                                       <xsl:with-param name="id" select="@name"/>
                                    </xsl:call-template>
                                 </xsl:variable>
                                 <xsl:choose>
                                    <xsl:when test="$exists=''">
                                       <xsl:text>_DUMMY_</xsl:text>
                                       <xsl:value-of select="@name"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                       <xsl:apply-templates select="."/>
                                    </xsl:otherwise>
                                 </xsl:choose>
                              </xsl:when>
                              <xsl:when test="key('CLASSES',substring-before(@name,'_'))">
                                 <xsl:variable name="exists">
                                    <xsl:call-template name="checkClass">
                                       <xsl:with-param name="id" select="substring-before(@name,'_')"/>
                                    </xsl:call-template>
                                 </xsl:variable>
                                 <xsl:choose>
                                    <xsl:when test="$exists=''">
                                       <xsl:text>_DUMMY_</xsl:text>
                                       <xsl:value-of select="@name"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                       <xsl:apply-templates select="."/>
                                    </xsl:otherwise>
                                 </xsl:choose>
                              </xsl:when>
                              <xsl:when test="key('MACROS',@name)">
                                 <xsl:apply-templates select="."/>
                              </xsl:when>
                              <xsl:when test="key('ELEMENTS',@name)">
                                 <xsl:apply-templates select="."/>
                              </xsl:when>
                           </xsl:choose>
                        </xsl:when>
                        <xsl:otherwise>
                           <xsl:apply-templates select="."/>
                        </xsl:otherwise>
                     </xsl:choose>
                  </xsl:variable>
                  <xsl:if test="not($F='')">
                     <N>
                        <xsl:value-of select="$F"/>
                     </N>
                  </xsl:if>
              </xsl:for-each>
            </M>
          </xsl:variable>
          <xsl:for-each select="$members/M">
            <xsl:choose>
              <xsl:when test="starts-with(N[1],'(') and count(N)=1"/>
              <xsl:when test="$parent='content' and count(N)=1 and starts-with(N[1],'%macro.')"/>
              <xsl:when test="$parent='content' and count(N)=1">
                  <xsl:text>(</xsl:text>
              </xsl:when>
              <xsl:when test="count(N)=1 and starts-with(N,'%')">
                  <xsl:text>(</xsl:text>
              </xsl:when>
              <xsl:when test="count(N)&gt;1">
                  <xsl:text>(</xsl:text>
              </xsl:when>
            </xsl:choose>
            <xsl:for-each select="N">
              <xsl:choose>
                  <xsl:when test="starts-with(.,'_DUMMY_') and .=preceding-sibling::N[1]"/>
                  <xsl:otherwise>
                     <xsl:value-of select="."/>
                     <xsl:choose>
                        <xsl:when test="self::N[1]='|'"/>
                        <xsl:when test="self::N[1]='('"/>
                        <xsl:when test="self::N[1]=')' and position() &lt; last()">
                           <xsl:value-of select="$sep"/>
                        </xsl:when>
                        <xsl:when test="following-sibling::N[1]='('"/>
                        <xsl:when test="following-sibling::N[1]=')'"/>
                        <xsl:when test="following-sibling::N[1]='|'"/>
                        <xsl:when test="position() &lt; last()">
                           <xsl:value-of select="$sep"/>
                        </xsl:when>
                     </xsl:choose>
                  </xsl:otherwise>
              </xsl:choose>
            </xsl:for-each>
            <xsl:choose>
              <xsl:when test="starts-with(N[1],'(') and count(N)=1"/>
              <xsl:when test="$parent='content' and count(N)=1 and starts-with(N[1],'%macro.')"/>
              <xsl:when test="$parent='content' and count(N)=1">
                  <xsl:text>)</xsl:text>
              </xsl:when>
              <xsl:when test="count(N)=1 and starts-with(N,'%')">
                  <xsl:text>)</xsl:text>
              </xsl:when>
              <xsl:when test="count(N)&gt;1">
                  <xsl:text>)</xsl:text>
              </xsl:when>
            </xsl:choose>
          </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$contentbody=''"/>
          <xsl:when test="$contentbody='()'"/>
	        <!-- some special cases of known evil -->
          <xsl:when test="$contentbody='(#PCDATA|#PCDATA)'">
            <xsl:text>(#PCDATA)</xsl:text>
          </xsl:when>
          <xsl:when test="contains($contentbody,'#PCDATA') and contains($contentbody,'%macro.anyXML;')">
            <xsl:text>(#PCDATA)</xsl:text>
          </xsl:when>
          <xsl:when test="contains($contentbody, '| ()')">
            <xsl:value-of select="substring-before($contentbody,'| ()')"/>
            <xsl:value-of select="substring-after($contentbody,'| ()')"/>
          </xsl:when>
          <xsl:when test="contains($contentbody, ',)')">
            <xsl:value-of select="substring-before($contentbody,',)')"/>
	           <xsl:text>)</xsl:text>
            <xsl:value-of select="substring-after($contentbody,',)')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$contentbody"/>
          </xsl:otherwise>
        </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:datatype">
      <xsl:choose>
         <xsl:when test="tei:dataRef/@name='ID'">
            <xsl:text> ID</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='ID'">
            <xsl:text> ID</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='IDREF'">
            <xsl:text> IDREF</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='IDREFS'">
            <xsl:text> IDREFS</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='NMTOKEN'">
            <xsl:text> NMTOKEN</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='NMTOKENS'">
            <xsl:text> NMTOKENS</xsl:text>
         </xsl:when>
         <xsl:when test="rng:data/@type='boolean'">
            <xsl:text> (true | false) </xsl:text>
         </xsl:when>
         <xsl:when test="rng:ref">
            <xsl:text> %</xsl:text>
            <xsl:value-of select="rng:ref/@name"/>
            <xsl:text>; </xsl:text>
         </xsl:when>
        <xsl:when test="tei:dataRef[@key]">
          <xsl:text> %</xsl:text>
          <xsl:value-of select="tei:dataRef/@key"/>
          <xsl:text>; </xsl:text>
        </xsl:when>
         <xsl:otherwise>
            <xsl:text> CDATA</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:element">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec">
            <xsl:call-template name="topLevel"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="@name"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
   <!--  
       <xsl:template match="tei:text()"/>-->
  <xsl:template match="rng:empty">
      <xsl:text>EMPTY</xsl:text>
  </xsl:template>
  <xsl:template match="tei:content[normalize-space(.) = '']">
    <xsl:text>EMPTY</xsl:text>
  </xsl:template>
  <xsl:template match="rng:data">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:dataSpec">
            <xsl:text> CDATA </xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:content">
            <xsl:text> (#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:when test="@type='ID'"> ID </xsl:when>
         <xsl:when test="@type='IDREF'"> IDREF </xsl:when>
         <xsl:when test="@type='IDREFS'"> IDREFS </xsl:when>
         <xsl:otherwise> CDATA </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:dataRef">
    <token><xsl:choose>
      <xsl:when test="parent::tei:content/parent::tei:dataSpec">
        <xsl:text> CDATA </xsl:text>
      </xsl:when>
      <xsl:when test="parent::tei:content">
        <xsl:text> (#PCDATA)</xsl:text>
      </xsl:when>
      <xsl:otherwise> CDATA </xsl:otherwise>
    </xsl:choose></token>
  </xsl:template>
  <xsl:template match="rng:text">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec">
            <xsl:call-template name="topLevel"/>
         </xsl:when>
         <xsl:when test="parent::tei:content">
            <xsl:text> (#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>#PCDATA</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:dataSpec/tei:content/tei:textNode | tei:dataSpec/tei:content/rng:text">
    <xsl:text> CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='epe']/tei:content/rng:text">
      <xsl:text>CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:dataSpec/tei:content/tei:valList[not(@type='closed')]">
      <xsl:text> CDATA</xsl:text>
  </xsl:template>

  <xsl:template match="tei:dataSpec/tei:content/rng:choice">
      <xsl:choose>
         <xsl:when test="rng:value and rng:data">
            <xsl:text> CDATA</xsl:text>
	 </xsl:when>
         <xsl:when test="rng:value">
            <xsl:text>(</xsl:text>
            <xsl:for-each select="rng:value">
               <xsl:value-of select="."/>
               <xsl:if test="following-sibling::rng:value">|</xsl:if>
            </xsl:for-each>
            <xsl:if test="rng:data/@type='boolean'">
               <xsl:text> | true | false</xsl:text>
            </xsl:if>
            <xsl:text>)</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text> CDATA</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template
      match="tei:dataSpec/tei:content/tei:valList[@type='closed']"
      mode="#default tangle">
    <xsl:text>(</xsl:text>
    <xsl:value-of select="tei:valItem/@ident" separator="|"/>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template match="rng:text|tei:textNode" mode="simple">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:dataSpec">
            <xsl:text> CDATA</xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec">
            <xsl:text>#PCDATA</xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
            <xsl:text>(#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:content and not(following-sibling::rng:*) and not (preceding-sibling::rng:*)">
            <xsl:text>(#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>#PCDATA</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="processing-instruction()[name(.)='teidtd']">
      <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="processing-instruction()[name(.)='teidtd']" mode="tangle">
      <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="rng:ref">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec">
            <xsl:call-template name="topLevel"/>
         </xsl:when>
         <xsl:when test="count(parent::tei:content/rng:*)&gt;1">
            <xsl:choose>
               <xsl:when test="not(preceding-sibling::rng:*)">
                  <xsl:text>(</xsl:text>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:text>,</xsl:text>
               </xsl:otherwise>
            </xsl:choose>
            <xsl:call-template name="refbody"/>
            <xsl:if test="not(following-sibling::rng:*)">
               <xsl:text>)</xsl:text>
            </xsl:if>
         </xsl:when>
         <xsl:when test="key('MACROS',@name)">
            <xsl:call-template name="refbody"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:if test="parent::tei:content">(</xsl:if>
            <xsl:call-template name="refbody"/>
            <xsl:if test="parent::tei:content">)</xsl:if>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="rng:ref" mode="simple">
      <xsl:call-template name="refbody"/>
  </xsl:template>
  <xsl:template name="refbody">
      <xsl:variable name="R">
         <xsl:choose>
            <xsl:when test="$parameterize='false'">
               <xsl:choose>
                  <xsl:when test="key('CLASSES',@name)">
                     <xsl:variable name="exists">
                        <xsl:call-template name="checkClass">
                           <xsl:with-param name="id" select="@name"/>
                        </xsl:call-template>
                     </xsl:variable>
                     <xsl:choose>
                        <xsl:when test="$exists=''">
                           <xsl:text>_DUMMY_</xsl:text>
                           <xsl:value-of select="@name"/>
                        </xsl:when>
                        <xsl:otherwise>
                           <xsl:text>%</xsl:text>
                           <xsl:value-of select="@name"/>
                           <xsl:text>;</xsl:text>
                        </xsl:otherwise>
                     </xsl:choose>
                  </xsl:when>
                  <xsl:when test="key('CLASSES',substring-before(@name,'_'))">
                     <xsl:variable name="exists">
                        <xsl:call-template name="checkClass">
                           <xsl:with-param name="id" select="substring-before(@name,'_')"/>
                        </xsl:call-template>
                     </xsl:variable>
                     <xsl:choose>
                        <xsl:when test="$exists=''">
                           <xsl:text>_DUMMY_</xsl:text>
                           <xsl:value-of select="@name"/>
                        </xsl:when>
                        <xsl:otherwise>
                           <xsl:text>%</xsl:text>
                           <xsl:value-of select="@name"/>
                           <xsl:text>;</xsl:text>
                        </xsl:otherwise>
                     </xsl:choose>
                  </xsl:when>
                  <xsl:when test="key('MACROS',@name)">
                     <xsl:text>%</xsl:text>
                     <xsl:value-of select="@name"/>
                     <xsl:text>;</xsl:text>
                  </xsl:when>
                  <xsl:when test="key('ELEMENTS',@name)">
                     <xsl:for-each select="key('ELEMENTS',@name)">
		       <xsl:value-of select="tei:createSpecName(.)"/>
                     </xsl:for-each>
                  </xsl:when>
               </xsl:choose>
            </xsl:when>
            <xsl:when test="@name='IGNORE' or @name='INCLUDE'">
               <xsl:value-of select="@name"/>
            </xsl:when>
            <xsl:when test="contains(@name, '.')">
               <xsl:text>%</xsl:text>
               <xsl:value-of select="@name"/>
               <xsl:text>;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>%n.</xsl:text>
               <xsl:value-of select="@name"/>
               <xsl:text>;</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:value-of select="$R"/>
  </xsl:template>
  <xsl:template name="topLevel">
      <xsl:variable name="unit">
         <xsl:apply-templates mode="simple" select="."/>
      </xsl:variable>
      <xsl:if test="not($unit='')">
         <xsl:if test="preceding-sibling::rng:*">
            <xsl:choose>
               <xsl:when test="preceding-sibling::processing-instruction()"/>
               <xsl:otherwise>
                  <xsl:text>|</xsl:text>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:if>
         <xsl:value-of select="$unit"/>
      </xsl:if>
  </xsl:template>
  <xsl:template match="rng:element" mode="simple">
      <xsl:choose>
         <xsl:when test="@name">
            <xsl:value-of select="@name"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>ANY</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:macroSpec" mode="tangle">
      <xsl:choose>
         <xsl:when test="@depend and $parameterize='true'">
            <xsl:if test="$verbose='true'">
               <xsl:message>Dependency on <xsl:value-of select="@depend"/> for <xsl:value-of select="@ident"/>
               </xsl:message>
            </xsl:if>
            <xsl:text>&#10; &lt;![%TEI.</xsl:text>
            <xsl:value-of select="@depend"/>;[ <xsl:call-template name="macroBody"/>
            <xsl:text>&#10;]]&gt;</xsl:text>
         </xsl:when>
         <xsl:when test="@depend and count(key('ElementModule',@depend))=0">
            <xsl:if test="$verbose='true'">
               <xsl:message>Dependency on <xsl:value-of select="@depend"/>, but not used in
						this schema </xsl:message>
            </xsl:if>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="macroBody"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:dataSpec" mode="tangle">
    <xsl:choose>
      <xsl:when test="@depend and $parameterize='true'">
        <xsl:if test="$verbose='true'">
          <xsl:message>Dependency on <xsl:value-of select="@depend"/> for <xsl:value-of select="@ident"/>
          </xsl:message>
        </xsl:if>
        <xsl:text>&#10; &lt;![%TEI.</xsl:text>
        <xsl:value-of select="@depend"/>;[ <xsl:call-template name="dataBody"/>
        <xsl:text>&#10;]]&gt;</xsl:text>
      </xsl:when>
      <xsl:when test="@depend and count(key('ElementModule',@depend))=0">
        <xsl:if test="$verbose='true'">
          <xsl:message>Dependency on <xsl:value-of select="@depend"/>, but not used in
            this schema </xsl:message>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="macroBody"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="macroBody">
    <xsl:text>&#10;&lt;!ENTITY </xsl:text>
    <xsl:if test="not(@type) or @type=('defaultpe','pe','epe')">
      <xsl:text>%</xsl:text>
    </xsl:if>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@ident"/>
    <xsl:text> '</xsl:text>
    <xsl:for-each select="tei:content">
      <xsl:choose>
        <xsl:when test=".//rng:anyName">
          <xsl:text> ANY</xsl:text>
        </xsl:when>
        <xsl:when test=".//tei:anyElement">
          <xsl:text> ANY</xsl:text>
        </xsl:when>
        <xsl:when test="tei:textNode">
          <xsl:text> CDATA</xsl:text>
        </xsl:when>
        <xsl:when test="processing-instruction()[name()='NameList']">
          <xsl:text> ANY</xsl:text>
        </xsl:when>
        <!-- un-closed value lists just resolve to CDATA -->
        <xsl:when test="valList[not(@type='closed')]">
          <xsl:text> CDATA </xsl:text>
        </xsl:when>
        <!-- non-TEI datatypes mean CDATA -->
        <xsl:when test=".//tei:dataRef[@name]">
          <xsl:text> CDATA </xsl:text>
        </xsl:when>
        <!-- can't resolve alternation between TEI datatypes, so CDATA -->
        <xsl:when test="tei:alternate/tei:dataRef[@key]">
          <xsl:text> CDATA </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates
            select="*|processing-instruction()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:text>' &gt;&#10;</xsl:text>
  </xsl:template>
  <xsl:template name="dataBody">
    <xsl:text>&#10;&lt;!ENTITY </xsl:text>
    <xsl:if test="not(@type) or @type=('defaultpe','pe','epe')">
      <xsl:text>%</xsl:text>
    </xsl:if>
    <xsl:text> </xsl:text>
    <xsl:value-of select="@ident"/>
    <xsl:text> '</xsl:text>
    <xsl:for-each select="tei:content">
      <xsl:choose>
        <xsl:when test="tei:textNode">
          <xsl:text> CDATA</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates
            select="*|processing-instruction()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:text>' &gt;&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="tei:elementSpec" mode="tangle">
      <xsl:if test="$verbose='true'">
         <xsl:message> .... elementSpec <xsl:value-of select="@ident"/>
         </xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="starts-with(@ident,'%')">
            <xsl:call-template name="elementBody"/>
         </xsl:when>
         <xsl:when test="$parameterize='false'">
            <xsl:call-template name="elementBody"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text> 'INCLUDE' &gt;&#10;&lt;![ %</xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text>; [&#10;</xsl:text>
            <xsl:call-template name="elementBody"/>
            <xsl:text>&#10;]]&gt;&#10;</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="elementBody">
      <xsl:variable name="ename">
         <xsl:choose>
            <xsl:when test="$parameterize='false'">
               <xsl:choose>
                  <xsl:when test="@ns='http://www.w3.org/1999/xhtml'">
                     <xsl:text>html:</xsl:text>
                     <xsl:value-of select="@ident"/>
                  </xsl:when>
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
            </xsl:when>
            <xsl:when test="starts-with(@ident,'%')">
               <xsl:value-of select="@ident"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>%n.</xsl:text>
               <xsl:value-of select="@ident"/>
               <xsl:text>;</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:text>&#10;&lt;!--doc:</xsl:text>
      <xsl:sequence select="tei:makeDescription(., true(), true())"/>
      <xsl:text> --&gt;&#10;</xsl:text>
      <xsl:text>&lt;!ELEMENT </xsl:text>
      <xsl:value-of select="$ename"/>
      <xsl:if test="$parameterize='true'">
         <xsl:text> %om.RR;</xsl:text>
      </xsl:if>
      <xsl:text> </xsl:text>
      <xsl:variable name="Contents">
	<Contents>
	<xsl:choose>
	  <xsl:when test="tei:content/rng:element">
	    <xsl:text> ANY</xsl:text>
	  </xsl:when>
	  <xsl:when test="tei:content/rng:element[rng:anyName]">
	    <xsl:text> (#PCDATA)</xsl:text>
	  </xsl:when>
	  <xsl:when test="tei:content/rng:ref/@name='data.name'">
	    <xsl:text> (#PCDATA)</xsl:text>
	  </xsl:when>
	  <xsl:when test="tei:content/tei:macroRef[@key='data.name']
			| tei:content/tei:dataRef[@key='teidata.name']">
	    <xsl:text> (#PCDATA)</xsl:text>
	  </xsl:when>
	  <xsl:when test="tei:valList[@type='closed']">
	    <xsl:text> (#PCDATA)</xsl:text>
	  </xsl:when>
          <xsl:when test="tei:content/tei:textNode and count(tei:content/*)=1">
            <xsl:text>(#PCDATA)</xsl:text>
	  </xsl:when>
          <xsl:when test="tei:content/*">
	      <xsl:apply-templates select="tei:content/*"/>
	  </xsl:when>
	</xsl:choose>
	</Contents>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$Contents=''">
            <xsl:text> EMPTY</xsl:text>
         </xsl:when>
         <xsl:when test="$Contents='()'">
            <xsl:text> EMPTY</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$Contents"/>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:text>&gt;</xsl:text>
      <xsl:choose>
         <xsl:when test="@ns and @ns=''"> </xsl:when>
         <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)/@ns=''"> </xsl:when>
         <xsl:when test="@ns">
            <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
            <xsl:value-of select="$ename"/>
            <xsl:text> xmlns CDATA "</xsl:text>
            <xsl:value-of select="@ns"/>
            <xsl:text>"</xsl:text>
            <xsl:text>&gt;</xsl:text>
         </xsl:when>
         <xsl:when test="key('LISTSCHEMASPECS',$whichSchemaSpec)/@ns">
            <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
            <xsl:value-of select="$ename"/>
            <xsl:text> xmlns CDATA "</xsl:text>
            <xsl:value-of select="ancestor::tei:schemaSpec/@ns"/>
            <xsl:text>"</xsl:text>
            <xsl:text>&gt;</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
            <xsl:value-of select="$ename"/>
            <xsl:text> xmlns CDATA "http://www.tei-c.org/ns/1.0"</xsl:text>
            <xsl:text>&gt;</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:variable name="maybeatts">
         <xsl:if test="$parameterize='true' and $autoGlobal='true'">
            <xsl:text>&#10; %att.global.attributes;</xsl:text>
         </xsl:if>
         <xsl:if test="$parameterize='true'">
            <xsl:apply-templates mode="tangleAtts" select="tei:classes/tei:memberOf"/>
         </xsl:if>
         <xsl:call-template name="attributeList"/>
    </xsl:variable>
      <xsl:if test="string-length($maybeatts) &gt;0">
         <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
         <xsl:value-of select="$ename"/>
         <xsl:value-of select="$maybeatts"/>
         <xsl:text> &gt;</xsl:text>
      </xsl:if>
      <xsl:if test="@ident='TEI' or @ident='teiCorpus'">
         <xsl:text>&#10;&lt;!ATTLIST </xsl:text>
         <xsl:value-of select="$ename"/>
         <xsl:text> xsi:schemaLocation CDATA #IMPLIED&#10;</xsl:text>
         <xsl:text> xmlns:xsi CDATA #FIXED 'http://www.w3.org/2001/XMLSchema-instance'&#10;</xsl:text>
         <xsl:text> &gt;</xsl:text>
      </xsl:if>
  </xsl:template>
  <xsl:template name="attclasses">
      <xsl:for-each select="tei:classes/tei:memberOf">
         <xsl:for-each select="key('IDENTS',@key)[1]">
            <xsl:if test="@type='atts'">
	      <xsl:text>%</xsl:text>
	      <xsl:value-of select="concat(if (@prefix) then @prefix
				    else $generalPrefix, @ident)"/>
	      <xsl:text>.attributes;</xsl:text>
	    </xsl:if>
         </xsl:for-each>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="classAtt">
      <xsl:param name="declare">true</xsl:param>
      <xsl:if test="$verbose='true'">
         <xsl:message> .... <xsl:value-of select="@ident"/>.attributes</xsl:message>
      </xsl:if>
      <xsl:variable name="thisclass"
		    select="concat(if (@prefix) then @prefix else
			    $generalPrefix, @ident)"/>
      <xsl:choose>
         <xsl:when test="$declare='false'">
            <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
            <xsl:value-of select="$thisclass"/>
            <xsl:text>.attributes</xsl:text>
            <xsl:text> ''&gt;</xsl:text>
	   <xsl:for-each select="tei:attList/tei:attDef[not(@mode='delete')]">
	     <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
	     <xsl:value-of select="$thisclass"/>
	     <xsl:text>.attribute.</xsl:text>
	     <xsl:value-of select="translate(@ident,':','')"/>
	     <xsl:text> ''&gt;&#10;</xsl:text>
	   </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:for-each select="tei:attList/tei:attDef[not(@mode='delete')]">
	     <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
	     <xsl:value-of select="$thisclass"/>
	     <xsl:text>.attribute.</xsl:text>
	     <xsl:value-of select="translate(@ident,':','')"/>
	     <xsl:text> '</xsl:text>
	     <xsl:apply-templates mode="tangle" select="."/>
	     <xsl:text>'&gt;&#10;</xsl:text>
	   </xsl:for-each>
	   <xsl:text>&lt;!ENTITY % </xsl:text>
	   <xsl:value-of select="$thisclass"/>
	   <xsl:text>.attributes '</xsl:text>
	   <xsl:call-template name="attclasses"/>
	   <xsl:for-each select="tei:attList/tei:attDef[not(@mode='delete')]">
	     <xsl:text>&#10; %</xsl:text>
	     <xsl:value-of select="$thisclass"/>
	     <xsl:text>.attribute.</xsl:text>
	     <xsl:value-of select="translate(@ident,':','')"/>
	     <xsl:text>;</xsl:text>
	   </xsl:for-each>
	   <xsl:text>'&gt; </xsl:text>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="tagatts">
      <xsl:if test="@type='atts'">
         <xsl:if test="$verbose='true'">
            <xsl:message> .... added contents of [%<xsl:value-of select="@ident"/>.attributes;]</xsl:message>
         </xsl:if>
         <xsl:text>&#10; %</xsl:text>
         <xsl:value-of select="@ident"/>
         <xsl:text>.attributes;</xsl:text>
      </xsl:if>
  </xsl:template>
  <xsl:template match="tei:moduleRef[@key]" mode="tangle">
      <xsl:if test="$verbose='true'">
         <xsl:message> moduleRef to <xsl:value-of select="@key"/>
			      </xsl:message>
      </xsl:if>
      <xsl:text>&#10;&lt;!ENTITY %</xsl:text>
      <xsl:text> file.</xsl:text>
      <xsl:value-of select="@key"/>
      <xsl:text> </xsl:text>
      <xsl:text>PUBLIC</xsl:text>
      <xsl:text> '-//TEI P5//</xsl:text>
      <xsl:value-of select="key('FILES',@key)/tei:altIdent[@type='FPI']"/>
      <xsl:text>//EN' '</xsl:text>
      <xsl:value-of select="@key"/>
      <xsl:text>.dtd'</xsl:text>
      <xsl:text> &gt;&#10;</xsl:text>
      <xsl:text>%file.</xsl:text>
      <xsl:value-of select="@key"/>
      <xsl:text>;&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="tangle">
      <xsl:if test="$verbose='true'">
         <xsl:message> .. classSpec <xsl:value-of select="@ident"/>,<xsl:value-of select="@type"/></xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="@type='atts'">
            <xsl:call-template name="classAtt"/>
         </xsl:when>
         <xsl:when test="@type='model'">
            <xsl:call-template name="classModel"/>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:classSpec" mode="process">
      <xsl:choose>
         <xsl:when test="@type='atts'">
            <xsl:call-template name="classAtt"/>
         </xsl:when>
         <xsl:when test="@type='model'">
            <xsl:call-template name="classModel"/>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="classModel">
      <xsl:param name="declare">false</xsl:param>
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
  </xsl:template>
  <xsl:template name="makeClassDefinition">
      <xsl:param name="type">alternation</xsl:param>
      <xsl:param name="declare"/>
      <xsl:variable name="IDENT">
         <xsl:value-of select="@ident"/>
         <xsl:choose>
            <xsl:when test="$type='alternation'"/>
            <xsl:otherwise>
               <xsl:text>_</xsl:text>
               <xsl:value-of select="$type"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="$verbose='true'">
         <xsl:message> .... ... generate model <xsl:value-of select="$type"/> for <xsl:value-of select="@ident"/>
			      </xsl:message>
      </xsl:if>
      <xsl:if test="$parameterize='true'">
         <xsl:text>&#10;&lt;!ENTITY % x.</xsl:text>
         <xsl:value-of select="$IDENT"/>
         <xsl:text> "" &gt;</xsl:text>
      </xsl:if>
      <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
      <xsl:value-of select="$IDENT"/>
      <xsl:text> "</xsl:text>
      <xsl:if test="$parameterize='true'">
         <xsl:text>%x.</xsl:text>
         <xsl:value-of select="$IDENT"/>
         <xsl:text>; </xsl:text>
      </xsl:if>
      <xsl:variable name="generate" select="@generate"/>
      <xsl:variable name="members">
         <M>
            <xsl:for-each select="key('CLASSMEMBERS',@ident)">
               <xsl:variable name="N">
                  <xsl:choose>
                     <xsl:when test="$parameterize='false'">
                        <xsl:choose>
                           <xsl:when test="key('CLASSES',@ident)">
                              <xsl:variable name="exists">
                                 <xsl:call-template name="checkClass">
                                    <xsl:with-param name="id" select="@ident"/>
                                 </xsl:call-template>
                              </xsl:variable>
                              <xsl:if test="not($exists='')">
                                 <xsl:text>%</xsl:text>
                                 <xsl:value-of select="@ident"/>
                                 <xsl:text>;</xsl:text>
                              </xsl:if>
                           </xsl:when>
                           <xsl:when test="key('MACROS',@ident)">
                              <xsl:text>%</xsl:text>
                              <xsl:value-of select="@ident"/>
                              <xsl:text>;</xsl:text>
                           </xsl:when>
                           <xsl:when test="key('ELEMENTS',@ident)">
			     <xsl:value-of select="tei:createSpecName(.)"/>
                           </xsl:when>
                        </xsl:choose>
                     </xsl:when>
                     <xsl:when test="self::tei:elementSpec">
                        <xsl:if test="$parameterize='true'">
                           <xsl:text>%n.</xsl:text>
                        </xsl:if>
                        <xsl:value-of select="@ident"/>
                        <xsl:if test="$parameterize='true'">
                           <xsl:text>;</xsl:text>
                        </xsl:if>
                     </xsl:when>
                     <xsl:when test="self::tei:classSpec">
                        <xsl:text>%</xsl:text>
                        <xsl:value-of select="@ident"/>
                        <xsl:text>;</xsl:text>
                     </xsl:when>
                     <xsl:otherwise>
                        <xsl:value-of select="@ident"/>
                     </xsl:otherwise>
                  </xsl:choose>
               </xsl:variable>
               <xsl:if test="not($N='')">
                  <N>
                     <xsl:if test="starts-with($N,'%tei.')">
                        <xsl:attribute name="type">class</xsl:attribute>
                     </xsl:if>
                     <xsl:if test="starts-with($N,'%model.')">
                        <xsl:attribute name="type">class</xsl:attribute>
                     </xsl:if>
                     <xsl:value-of select="$N"/>
                  </N>
               </xsl:if>
            </xsl:for-each>
         </M>
      </xsl:variable>
      <xsl:for-each select="$members/M">
         <xsl:call-template name="memberOfClassDefinition">
            <xsl:with-param name="type" select="$type"/>
         </xsl:call-template>
      </xsl:for-each>
      <xsl:text>"&gt; </xsl:text>
  </xsl:template>
  <xsl:template name="memberOfClassDefinition">
      <xsl:param name="type"/>
      <!-- does a class model need bracketing if all its members are also classes?
	 <xsl:if test="count(N[@type='class'])=count(N) and count(N) &gt; 2">
	 <xsl:text>(</xsl:text>
	 </xsl:if>
    -->
    <xsl:for-each select="N">
         <xsl:value-of select="."/>
         <xsl:choose>
            <xsl:when test="$type='sequence'">
               <xsl:if test="position() &lt; last()">, </xsl:if>
            </xsl:when>
            <xsl:when test="$type='sequenceOptional'">
               <xsl:text>?</xsl:text>
               <xsl:if test="position() &lt; last()">, </xsl:if>
            </xsl:when>
            <xsl:when test="$type='sequenceRepeatable'">
               <xsl:text>+</xsl:text>
               <xsl:if test="position() &lt; last()">, </xsl:if>
            </xsl:when>
            <xsl:when test="$type='sequenceOptionalRepeatable'">
               <xsl:text>*</xsl:text>
               <xsl:if test="position() &lt; last()">, </xsl:if>
            </xsl:when>
            <xsl:otherwise>
               <xsl:if test="position() &lt; last()"> |</xsl:if>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:for-each>
      <!-- see above
	 <xsl:if test="count(N[@type='class'])=count(N) and count(N) &gt; 2">
	 <xsl:text>)</xsl:text>
	 </xsl:if>
    -->
  </xsl:template>
  <xsl:template match="tei:commDecl" mode="tangle">
      <xsl:text>&#10;  &lt;!--</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>--&gt;</xsl:text>
  </xsl:template>
  <xsl:template name="attributeList">
      <xsl:apply-templates mode="tangle" select="tei:attList/tei:*"/>
  </xsl:template>

  <xsl:template match="tei:attDef" mode="tangle">
    <xsl:text>&#10;</xsl:text>
    <xsl:choose>
      <xsl:when test="@ns='http://www.w3.org/XML/1998/namespace'">
        <xsl:text>xml:</xsl:text>
      </xsl:when>
      <xsl:when test="@ns='http://www.w3.org/1999/xlink'">
        <xsl:text>xlink:</xsl:text>
      </xsl:when>
      <xsl:when test="$parameterize='true' and (@ns) and not($nsPrefix='') and not(starts-with(@ident,'xmlns'))">
        <xsl:text>%NS;</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:value-of select="tei:createSpecName(.)"/>
    <xsl:variable name="datminOmaxO" select="tei:minOmaxO( tei:datatype/@minOccurs, tei:datatype/@maxOccurs )"/>
    <xsl:variable name="datmin" select="$datminOmaxO[1]"/>
    <xsl:variable name="datmax" select="$datminOmaxO[2]"/>
    <xsl:choose>
      <xsl:when test="( $datmax gt 1  or  $datmax eq -1 )  and  (
        tei:datatype/rng:ref/@name = ('data.enumerated','data.name')
        or
        tei:datatype/tei:dataRef/@key = ('teidata.enumerated','teidata.name')
        or
        'Name' = ( tei:datatype/rng:data/@type, tei:datatype/tei:dataRef/@name )
        )">
        <xsl:text> NMTOKENS </xsl:text>
      </xsl:when>
      <xsl:when test="tei:valList[@type eq 'closed']">
        <xsl:text> (</xsl:text>
        <xsl:for-each select="tei:valList/tei:valItem">
          <xsl:value-of select="@ident"/>
          <xsl:if test="following-sibling::tei:valItem">
            <xsl:text>|</xsl:text>
          </xsl:if>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </xsl:when>
      <xsl:when test="tei:datatype/rng:data[@type eq 'Name']">
        <xsl:text> NMTOKEN </xsl:text>
      </xsl:when>
      <xsl:when test="( $datmin, $datmax ) != 1">
        <xsl:text> CDATA </xsl:text>
      </xsl:when>
      <xsl:when test="tei:datatype/*">
        <xsl:apply-templates select="tei:datatype"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> CDATA </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="string-length(tei:defaultVal) gt 0">
        <xsl:text> "</xsl:text>
        <xsl:value-of select="tei:defaultVal"/>
        <xsl:text>" </xsl:text>
      </xsl:when>
      <xsl:when test="parent::tei:attList[@org='choice']">
        <xsl:text> #IMPLIED</xsl:text>
      </xsl:when>
      <xsl:when test="@usage='req'">
        <xsl:text> #REQUIRED</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> #IMPLIED</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="schemaOut">
      <xsl:param name="grammar"/>
      <xsl:param name="element"/>
      <xsl:param name="content"/>
      <xsl:copy-of select="$content"/>
  </xsl:template>
  <xsl:template name="CR">
      <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="tei:memberOf" mode="tangleAtts">
      <xsl:variable name="ident">
         <xsl:value-of select="ancestor::tei:elementSpec/@ident|ancestor::tei:classSpec/@ident"/>
      </xsl:variable>
      <xsl:variable name="K" select="@key"/>
      <xsl:choose>
         <xsl:when test="key('IDENTS',$K)">
            <xsl:for-each select="key('IDENTS',@key)[1]">
               <xsl:apply-templates mode="tagatts" select="."/>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:for-each select="document($localsource)/tei:TEI">
	     <xsl:apply-templates mode="tagatts" select="key('LOCALIDENTS',$K)"/>
	   </xsl:for-each>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:attRef" mode="tangle">
      <xsl:text>&#10; %</xsl:text>
      <xsl:sequence select="tei:generateAttRef(.,$generalPrefix)"/>
      <xsl:text>;</xsl:text>
  </xsl:template>
  <xsl:template name="checkClass">
      <xsl:param name="id"/>
      <!-- should a class be included?
	 * it must exist in the ODD spec
	 * it must have other classes which point to it
	 * whatever it points to must have members which are in the ODD
    -->
    <xsl:for-each select="key('CLASSMEMBERS',$id)">
         <xsl:choose>
            <xsl:when test="self::tei:elementSpec and key('ELEMENTS',@ident)">
               <xsl:text>true</xsl:text>
            </xsl:when>
            <xsl:when test="self::tei:macroSpec and key('MACROS',@ident)">
               <xsl:text>true</xsl:text>
            </xsl:when>
            <xsl:when test="self::tei:classSpec and key('CLASSES',@ident)">
               <xsl:call-template name="checkClass">
                  <xsl:with-param name="id" select="@ident"/>
               </xsl:call-template>
            </xsl:when>
         </xsl:choose>
      </xsl:for-each>
  </xsl:template>


  <xsl:template match="tei:sequence">
    <xsl:variable name="innards">
      <token>
        <xsl:choose>
          <xsl:when test="tei:textNode or parent::*/tei:textNode">
            <xsl:call-template name="innards"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>(</xsl:text>
            <xsl:call-template name="innards">
              <xsl:with-param name="sep">,</xsl:with-param>
            </xsl:call-template>
            <xsl:text>)</xsl:text>
            <xsl:value-of select="tei:generateIndicators(., @minOccurs, @maxOccurs)"/>
          </xsl:otherwise>
        </xsl:choose>
      </token>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="parent::tei:content">
        <xsl:text>(</xsl:text>
        <xsl:value-of select="$innards/*" separator=","/>
        <xsl:text>)</xsl:text>
        <xsl:value-of select="tei:generateIndicators(., @minOccurs, @maxOccurs)"/>
      </xsl:when>
      <xsl:otherwise>
        <token>
          <xsl:copy-of select="$innards"/>
        </token>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:alternate">
    <token>
      <xsl:variable name="suffix" select="tei:generateIndicators(.,@minOccurs,@maxOccurs)"/>
      <xsl:text>(</xsl:text>
      <xsl:call-template name="innards"/>
      <xsl:text>)</xsl:text>
      <xsl:value-of select="$suffix"/>
    </token>
  </xsl:template>

  <xsl:template match="tei:textNode">
    <token>#PCDATA</token>
  </xsl:template>

  <xsl:template match="tei:interleave">
    <xsl:message>met an interleave</xsl:message>
  </xsl:template>
  
  <xsl:template match="tei:elementRef|tei:classRef|tei:macroRef|tei:dataRef[@key]">
    <xsl:variable name="except" select="@except"/>
    <xsl:variable name="include" select="@include"/>
    <xsl:variable name="minOmaxO" select="tei:minOmaxO( @minOccurs, @maxOccurs )"/>
    <xsl:variable name="min" select="$minOmaxO[1]"/>
    <xsl:variable name="max" select="$minOmaxO[2]"/>
    <xsl:variable name="exists">
      <xsl:call-template name="checkClass">
        <xsl:with-param name="id" select="@key"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="suffix"
		  select="tei:generateIndicators(.,@minOccurs,@maxOccurs)"/>
    <xsl:variable name="ename">
      <xsl:choose>
        <xsl:when test="self::tei:classRef and $exists=''">
          <xsl:text>_DUMMY_</xsl:text>
          <xsl:value-of select="@key"/>
        </xsl:when>
        <xsl:when test="self::tei:elementRef and $parameterize='true'">
          <xsl:value-of select="concat('%n.',@key,';')"/>
        </xsl:when>
        <xsl:when test="self::tei:elementRef">
          <xsl:value-of select="@key"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat('%',@key,';')"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="this" select="@key"/>

    <token>
      <xsl:choose>
        <xsl:when test="ancestor::tei:macroSpec">
          <xsl:copy-of select="$ename"/>
        </xsl:when>
        <xsl:when test="ancestor::tei:dataSpec">
          <xsl:copy-of select="$ename"/>
        </xsl:when>
        <xsl:when test="not(@expand) and (@include or @except)">
          <xsl:variable name="members">
            <xsl:for-each select="key('CLASSMEMBERS',$this)">
              <xsl:if test="key('IDENTS',@ident) and
                tei:includeMember(@ident,$except,$include)">
                <token>
                  <xsl:value-of select="@ident"/>
                </token>
              </xsl:if>
            </xsl:for-each>
          </xsl:variable>
          <xsl:value-of select="$members/*" separator="|"/>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
        </xsl:when>
        <xsl:when test="parent::*/tei:textNode">
          <xsl:value-of select="$ename"/>
        </xsl:when>
        <xsl:when test="@expand">
          <xsl:text>(%</xsl:text>
          <xsl:value-of select="(@key,@expand)" separator="_"/>
          <xsl:text>;)</xsl:text>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
        </xsl:when>
        <xsl:when test="$min gt 1 and $max = -1">
          <xsl:text>(</xsl:text>
          <xsl:for-each select="1 to $min">
            <xsl:if test="starts-with( $ename,'%')">(</xsl:if>
            <xsl:value-of select="$ename"/>
            <xsl:if test="starts-with( $ename,'%')">)</xsl:if>
            <xsl:if test="position() lt last()"><xsl:text>,</xsl:text></xsl:if>
          </xsl:for-each>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:when test="$max gt 1  and  $max lt $maxint">
          <xsl:text>(</xsl:text>
          <xsl:for-each select="1 to $max">
            <xsl:if test="starts-with( $ename,'%')">(</xsl:if>
            <xsl:value-of select="$ename"/>
            <xsl:if test="starts-with( $ename,'%')">)</xsl:if>
            <xsl:if test="position() gt $min"><xsl:text>?</xsl:text></xsl:if>
            <xsl:if test="position() lt last()"><xsl:text>,</xsl:text></xsl:if>
          </xsl:for-each>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
          <xsl:text>)</xsl:text>
        </xsl:when>
        <xsl:when test="string-length($suffix) gt 0">
          <xsl:text>(</xsl:text>
          <xsl:value-of select="$ename"/>
          <xsl:text>)</xsl:text>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$ename"/>
          <xsl:if test="not(parent::*/tei:textNode)">
            <xsl:value-of select="$suffix"/>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
      
    </token>
  </xsl:template>
  
  <xsl:template name="innards">
    <xsl:param name="sep">|</xsl:param>
    <xsl:variable name="innards">
      <xsl:apply-templates/>
    </xsl:variable>
    <xsl:value-of select="$innards/*" separator="{$sep}"/>
  </xsl:template>

  <xsl:template name="dtdComment">
      <xsl:param name="text"/>
      <xsl:text>&#10;&lt;!--&#10;</xsl:text>
      <xsl:value-of select="$text"/>
      <xsl:text>&#10;--&gt;&#10;</xsl:text>
  </xsl:template>
  
  <xsl:template name="checkEnd">
      <xsl:if test="count(parent::tei:content[parent::tei:elementSpec]/rng:*)&gt;1 and not(following-sibling::rng:*)">
         <xsl:text>)</xsl:text>
      </xsl:if>
  </xsl:template>

  <xsl:template name="checkStart">
      <xsl:if test="count(parent::tei:content[parent::tei:elementSpec]/rng:*)
        &gt; 1">
         <xsl:choose>
            <xsl:when test="preceding-sibling::rng:*">
               <xsl:text>,</xsl:text>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>(</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:if>
  </xsl:template>

  <xsl:template name="copyright">
    <xsl:for-each
	select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability">
      <xsl:if test="count(tei:licence)&gt;1">
	<xsl:text>This material is dual-licensed.&#10;</xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tei:licence">
    <xsl:if test="@target">
      <xsl:text>[</xsl:text>
      <xsl:value-of select="@target"/>
      <xsl:text>] </xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="typewriter">
    <xsl:param name="text"/>
  </xsl:template>
  
  <xsl:template match="tei:content/tei:classRef | tei:content//tei:sequence/tei:classRef" mode="preprocess">
    <xsl:variable name="minOmaxO" select="tei:minOmaxO( @minOccurs, @maxOccurs )"/>
    <xsl:variable name="min" select="$minOmaxO[1]"/>
    <xsl:variable name="max" select="$minOmaxO[2]"/>
    <xsl:choose>
      <xsl:when test="$max eq -1">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="$max gt 1">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="parent::tei:sequence/count(*) eq 1">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="contains(@expand, 'sequence')">
        <xsl:copy-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <tei:sequence>
          <xsl:copy-of select="."/>
        </tei:sequence>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="*" mode="preprocess">
    <xsl:copy>
      <xsl:apply-templates select="* | @* | comment() | processing-instruction() | text()" mode="preprocess"/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="@* | text() | comment() | processing-instruction()" mode="preprocess">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:function name="tei:generateIndicators">
    <xsl:param name="context"/>
    <xsl:param name="minOccurs"/>
    <xsl:param name="maxOccurs"/>
    <xsl:variable name="minOmaxO" select="tei:minOmaxO( $minOccurs, $maxOccurs )"/>
    <xsl:variable name="min" select="$minOmaxO[1]"/>
    <xsl:variable name="max" select="$minOmaxO[2]"/>
    <xsl:choose>
      <xsl:when test="$context/tei:textNode">*</xsl:when>
      <xsl:when test="$min eq 0  and  $max eq  1">?</xsl:when>
      <xsl:when test="$min eq 0  and  $max eq -1">*</xsl:when>
      <xsl:when test="$min ge 1  and  $max eq -1">+</xsl:when>
      <xsl:otherwise></xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="tei:dtdcomment">
    <xsl:param name="text"/>
    <xsl:value-of select="concat('&#10;&lt;!-- ',$text,' --&gt;&#10;')"/>
  </xsl:function>

</xsl:stylesheet>

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
  <xsl:import href="../common2/i18n.xsl"/>
  <xsl:import href="../common2/tei-param.xsl"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making DTD from ODD </p>
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
  <xsl:output method="text"/>
  <xsl:param name="autoGlobal">false</xsl:param>
  <xsl:param name="verbose"/>
  <xsl:param name="outputDir"/>
  <xsl:param name="appendixWords"> </xsl:param>
  <xsl:param name="filesuffix"/>
  <xsl:template name="headingNumberSuffix">
      <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:param name="numberBackHeadings"> </xsl:param>
  <xsl:param name="numberFrontHeadings"> </xsl:param>
  <xsl:param name="numberHeadings"> </xsl:param>
  <xsl:param name="numberHeadingsDepth"> </xsl:param>
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
      <xsl:choose>
         <xsl:when test="key('SCHEMASPECS',1)">
            <xsl:apply-templates select="key('LISTSCHEMASPECS',$whichSchemaSpec)"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="byModule"/>
         </xsl:otherwise>
      </xsl:choose>
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
	                    <xsl:text>. Generated </xsl:text>
	                    <xsl:call-template name="showDate"/>
	                    <xsl:text>. </xsl:text>
			    <xsl:call-template name="copyright"/>
	                    <xsl:call-template name="makeTEIVersion"/>
	                    <xsl:call-template name="makeDescription"/>
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
		                      <xsl:text>. Generated </xsl:text>
		                      <xsl:call-template name="showDate"/>
		                      <xsl:text>.&#10;</xsl:text>
				      <xsl:call-template name="copyright"/>
		                      <xsl:call-template name="makeTEIVersion"/>
		                      <xsl:call-template name="makeDescription"/>
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
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Module declarations</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
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
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>legacy declaration of omissability indicators</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:text>&lt;!ENTITY % TEI.XML 'INCLUDE' &gt;&#10;</xsl:text>
      <xsl:text>&lt;![%TEI.XML;[&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RO '' &gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RR '' &gt;&#10;</xsl:text>
      <xsl:text>]]&gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RO '- o' &gt;&#10;</xsl:text>
      <xsl:text>&lt;!ENTITY % om.RR '- -' &gt;&#10;</xsl:text>
  </xsl:template>
  <xsl:template name="datatypeMacros">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Start datatype macro declarations</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('MacroModule',@ident)">
         <xsl:if test="@type='dt'">
            <xsl:apply-templates mode="tangle" select="."/>
         </xsl:if>
      </xsl:for-each>
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>End of datatype macro declarations</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <xsl:template name="predeclaredMacros">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Start of pre-declared macros</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('PredeclareMacrosModule',@ident)">
         <xsl:apply-templates mode="tangle" select="."/>
      </xsl:for-each>
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>End of pre-declared macros</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <xsl:template name="normalClasses">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Start of classes</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates mode="tangle" select="key('ClassModule',@ident)"/>
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>End of classes</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <xsl:template name="predeclaredClasses">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Start of pre-declared classes</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
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
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>End of pre-declared classes</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <xsl:template name="normalMacros">
      <xsl:if test="@type='core'">
         <xsl:call-template name="dtdComment">
            <xsl:with-param name="text">
               <xsl:text>Global pre-declared macros</xsl:text>
            </xsl:with-param>
         </xsl:call-template>
         <xsl:for-each select="key('PredeclareAllMacros',1)">
            <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
            <xsl:value-of select="@ident"/>
            <xsl:text> ''</xsl:text>
            <xsl:text>&gt;&#10;</xsl:text>
         </xsl:for-each>
      </xsl:if>
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>Start rest of  macro declarations</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:for-each select="key('MacroModule',@ident)">
         <xsl:if test="not(@type='dt')">
            <xsl:choose>
               <xsl:when test="@predeclare='true'"/>
               <!--	    <xsl:when test="key('PredeclareMacros',@ident)"/>-->
	       <xsl:otherwise>
		 <xsl:apply-templates mode="tangle" select="."/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:if>
      </xsl:for-each>
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>End macros</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>
  <xsl:template match="tei:schemaSpec">
      <xsl:if test="$verbose='true'">
         <xsl:message>schemaSpec <xsl:value-of select="@ident"/>
			      </xsl:message>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="$outputDir='' or $outputDir='-'">
            <xsl:call-template name="schemaOut"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="generateOutput">
               <xsl:with-param name="suffix">.dtd</xsl:with-param>
               <xsl:with-param name="method">text</xsl:with-param>
               <xsl:with-param name="body">
                  <xsl:call-template name="schemaOut"/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="schemaOut">
      <xsl:call-template name="dtdComment">
         <xsl:with-param name="text">
            <xsl:text>DTD Generated </xsl:text>
            <xsl:call-template name="showDate"/>
            <xsl:text>. </xsl:text>
	    <xsl:call-template name="copyright"/>
            <xsl:call-template name="makeTEIVersion"/>
            <xsl:call-template name="makeDescription"/>
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
      <xsl:if test="$parameterize='true' and      count(key('NSELEMENTS',1))&gt;0">
         <xsl:text>&lt;!ENTITY % NS '</xsl:text>
         <xsl:value-of select="$nsPrefix"/>
         <xsl:text>' &gt;&#10;</xsl:text>
      </xsl:if>
      <xsl:if test="$parameterize='true'">
         <xsl:call-template name="NameList"/>
      </xsl:if>
      <xsl:text>&#10;&lt;!-- start datatypes --&gt;&#10;</xsl:text>
      <xsl:apply-templates mode="tangle" select="tei:macroSpec[@type='dt']"/>
      <xsl:text>&#10;&lt;!-- end datatypes --&gt;&#10;</xsl:text>
      <xsl:if test="tei:classSpec[@predeclare='true']">
         <xsl:text>&#10;&lt;!--predeclared classes --&gt;&#10;</xsl:text>
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
         <xsl:text>&#10;&lt;!--end of predeclared classes --&gt;&#10;</xsl:text>
         <xsl:apply-templates mode="tangle" select="tei:classSpec"/>
      </xsl:if>
      <xsl:text>&#10;&lt;!-- start predeclared patterns --&gt;&#10;</xsl:text>
      <xsl:for-each select="tei:macroSpec">
	<xsl:if test="@predeclare='true'">
	  <xsl:apply-templates mode="tangle" select="."/>
	</xsl:if>
      </xsl:for-each>
      <xsl:text>&#10;&lt;!-- end predeclared patterns --&gt;&#10;</xsl:text>
      <xsl:text>&#10;&lt;!-- start rest of patterns --&gt;&#10;</xsl:text>
      <xsl:for-each select="tei:macroSpec">
	<xsl:choose>
	  <xsl:when test="@predeclare='true'"/>
	  <xsl:when test="@type='dt'"/>
	  <xsl:otherwise>
	    <xsl:apply-templates mode="tangle" select="."/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each>
      <xsl:text>&#10;&lt;!-- end patterns --&gt;&#10;</xsl:text>
      <xsl:if test="not(tei:classSpec[@predeclare='true'])">
         <xsl:text>&#10;&lt;!-- start classes --&gt;&#10;</xsl:text>
         <xsl:apply-templates mode="tangle" select="tei:classSpec[@type='atts']"/>
         <xsl:apply-templates mode="tangle" select="tei:classSpec[@type='model']"/>
         <xsl:text>&#10;&lt;!-- stop classes --&gt;&#10;</xsl:text>
      </xsl:if>
      <xsl:text>&#10;&lt;!-- start elements --&gt;&#10;</xsl:text>
      <xsl:apply-templates mode="tangle" select="tei:elementSpec">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
      <xsl:text>&#10;&lt;!-- end elements --&gt;&#10;</xsl:text>
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
         <xsl:choose>
            <xsl:when test="tei:altIdent=@ident">
               <xsl:value-of select="@ident"/>
            </xsl:when>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
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
         <xsl:when test="rng:value and ancestor::tei:elementSpec">
            <xsl:text>(#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:when test="rng:value">
            <xsl:text> (</xsl:text>
            <xsl:for-each select="rng:value">
               <xsl:value-of select="."/>
               <xsl:if test="following-sibling::rng:value">
                  <xsl:text>|&#10;</xsl:text>
               </xsl:if>
            </xsl:for-each>) </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="content">
               <xsl:with-param name="sep" select="' |&#xA; '"/>
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
                        <xsl:when test="self::N[1]='|&#xA;'"/>
                        <xsl:when test="self::N[1]='('"/>
                        <xsl:when test="self::N[1]=')' and position() &lt; last()">
                           <xsl:value-of select="$sep"/>
                        </xsl:when>
                        <xsl:when test="following-sibling::N[1]='('"/>
                        <xsl:when test="following-sibling::N[1]=')'"/>
                        <xsl:when test="following-sibling::N[1]='|&#xA;'"/>
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
          <xsl:when test="$contentbody='(#PCDATA |  #PCDATA)'">
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
  <xsl:template match="rng:data">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec[@type='dt']">
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
  <xsl:template match="rng:text">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec">
            <xsl:call-template name="topLevel"/>
         </xsl:when>
         <xsl:when test="parent::tei:content">
            <xsl:text> (#PCDATA)</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text> #PCDATA</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:text">
      <xsl:text> CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:data">
      <xsl:text> CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='epe']/tei:content/rng:text">
      <xsl:text>CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:list">
      <xsl:text> CDATA</xsl:text>
  </xsl:template>
  <xsl:template match="tei:macroSpec[@type='dt']/tei:content/rng:choice">
      <xsl:choose>
         <xsl:when test="rng:value">
            <xsl:text>(</xsl:text>
            <xsl:for-each select="rng:value">
               <xsl:value-of select="."/>
               <xsl:if test="following-sibling::rng:value">|
</xsl:if>
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
  <xsl:template match="rng:text" mode="simple">
      <xsl:choose>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec[@type='dt']">
            <xsl:text> CDATA</xsl:text>
         </xsl:when>
         <xsl:when test="parent::tei:content/parent::tei:macroSpec[@type='pe']">
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
                  <xsl:text>|&#10;</xsl:text>
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
  <xsl:template name="macroBody">
      <xsl:text>&#10;&lt;!ENTITY </xsl:text>
      <xsl:if test="not(@type) or @type='defaultpe' or @type='pe' or @type='epe' or @type='dt'">
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
	  <xsl:otherwise>
	    <xsl:apply-templates
		select="tei:*|rng:*|processing-instruction()"/>
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
      <xsl:call-template name="makeDescription">
         <xsl:with-param name="includeValList">true</xsl:with-param>
         <xsl:with-param name="coded">false</xsl:with-param>
      </xsl:call-template>
      <xsl:text> --&gt;&#10;</xsl:text>
      <xsl:text>&lt;!ELEMENT </xsl:text>
      <xsl:value-of select="$ename"/>
      <xsl:if test="$parameterize='true'">
         <xsl:text> %om.RR;</xsl:text>
      </xsl:if>
      <xsl:text> </xsl:text>
      <xsl:variable name="Contents">
         <BLAH>
            <xsl:choose>
	      <xsl:when test="tei:content/rng:element[rng:anyName]">
		<xsl:text> (#PCDATA)</xsl:text>
	      </xsl:when>
	      <xsl:when test="tei:content/rng:ref/@name='data.name'">
		<xsl:text> (#PCDATA)</xsl:text>
	      </xsl:when>
	      <xsl:when test="tei:content/rng:element">
                  <xsl:text> ANY</xsl:text>
	      </xsl:when>
	      <xsl:when test="tei:valList[@type='closed']">
		<xsl:text> (#PCDATA)</xsl:text>
	      </xsl:when>
	      <xsl:when test="tei:content">
		<xsl:apply-templates select="tei:content/*"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:text/>
	      </xsl:otherwise>
            </xsl:choose>
         </BLAH>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$Contents=''">
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
            <xsl:if test="@type='atts'"> %<xsl:value-of select="@ident"/>
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
      <xsl:variable name="thisclass">
         <xsl:value-of select="@ident"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$declare='false'">
            <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
            <xsl:value-of select="$thisclass"/>
            <xsl:text>.attributes</xsl:text>
            <xsl:text> ''&gt;</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="$parameterize='true'">
                  <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
                  <xsl:value-of select="$thisclass"/>
                  <xsl:text>.attributes '</xsl:text>
                  <xsl:call-template name="attclasses"/>
                  <xsl:call-template name="attributeList"/>
                  <xsl:text>'&gt; </xsl:text>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:text>&lt;!ENTITY % </xsl:text>
                  <xsl:value-of select="$thisclass"/>
                  <xsl:text>.attributes '</xsl:text>
                  <xsl:call-template name="attclasses"/>
                  <xsl:call-template name="attributeList"/>
                  <xsl:text>'&gt; </xsl:text>
                  <xsl:for-each select="tei:attList/tei:attDef">
                     <xsl:text>&#10;&lt;!ENTITY % </xsl:text>
                     <xsl:value-of select="$thisclass"/>
                     <xsl:text>.attribute.</xsl:text>
                     <xsl:value-of select="translate(@ident,':','')"/>
                     <xsl:text> '</xsl:text>
                     <xsl:apply-templates mode="tangle" select="."/>
                     <xsl:text>'&gt;&#10;</xsl:text>
                  </xsl:for-each>
               </xsl:otherwise>
            </xsl:choose>
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
               <xsl:if test="position() &lt; last()"> |
 </xsl:if>
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
      <xsl:choose>
         <xsl:when test="(number(tei:datatype/@maxOccurs) &gt; 1 or tei:datatype/@maxOccurs='unbounded') and tei:datatype/rng:ref[@name='data.enumerated']">
            <xsl:text> NMTOKENS </xsl:text>
         </xsl:when>
         <xsl:when test="(number(tei:datatype/@maxOccurs) &gt; 1 or  tei:datatype/@maxOccurs='unbounded') and tei:datatype/rng:ref[@name='data.name']">
            <xsl:text> NMTOKENS </xsl:text>
         </xsl:when>
         <xsl:when test="(number(tei:datatype/@maxOccurs) &gt; 1 or  tei:datatype/@maxOccurs='unbounded') and tei:datatype/rng:data[@type='Name']">
            <xsl:text> NMTOKENS </xsl:text>
         </xsl:when>
         <xsl:when test="tei:valList[@type='closed']">
            <xsl:text> (</xsl:text>
            <xsl:for-each select="tei:valList/tei:valItem">
               <xsl:value-of select="@ident"/>
               <xsl:if test="following-sibling::tei:valItem">
		 <xsl:text>|</xsl:text>
	       </xsl:if>
            </xsl:for-each>
            <xsl:text>)</xsl:text>
         </xsl:when>
         <xsl:when test="tei:datatype/rng:data[@type='Name']">
            <xsl:text> NMTOKEN </xsl:text>
         </xsl:when>
         <xsl:when test="tei:datatype/@minOccurs or tei:datatype/@maxOccurs">
            <xsl:text> CDATA </xsl:text>
         </xsl:when>
         <xsl:when test="tei:datatype/rng:*">
            <xsl:apply-templates select="tei:datatype"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text> CDATA </xsl:text>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test="string-length(tei:defaultVal) &gt;0">
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
  <xsl:template name="bitOut">
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
      <xsl:value-of select="@name"/>
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
  <xsl:template name="dtdComment">
      <xsl:param name="text"/>
      <xsl:text>&#10;&lt;!--&#10;</xsl:text>
      <xsl:value-of select="$text"/>
      <xsl:text>&#10;--&gt;&#10;</xsl:text>
  </xsl:template>
  <xsl:template name="checkEnd">
      <xsl:if test="count(parent::tei:content[parent::tei:elementSpec]/rng:*)&gt;1 and     not(following-sibling::rng:*)">
         <xsl:text>)</xsl:text>
      </xsl:if>
  </xsl:template>
  <xsl:template name="checkStart">
      <xsl:if test="count(parent::tei:content[parent::tei:elementSpec]/rng:*)&gt;1">
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


</xsl:stylesheet>

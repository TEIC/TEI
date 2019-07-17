<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:custprops="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"
                xmlns:html="http://www.w3.org/1999/xhtml"
                version="2.0"
                exclude-result-prefixes="a pic rel ve o teidocx r m v wp w10 w wne mml vt cals tbx iso custprops">

    <!-- import conversion style -->

    <xsl:import href="../../../docx/from/docxtotei.xsl"/>

    <!-- import special iso functions -->
    <xsl:include href="iso-functions.xsl"/>
    <xsl:include href="../isoutils.xsl"/>


    <xsl:include href="from-pass2.xsl"/>
    <xsl:include href="from-pass3.xsl"/>

    <xsl:variable name="standardsBodies">
      <xsl:text>ISO/ASTM|ISO/CEI|ISO/CIE|ISO/HL7|ISO/IEC/IEEE|ISO/IEC|ISO/IEEE|ISO/OCDE|ISO/OECD|CEI|IEC|ISO</xsl:text>
    </xsl:variable>
    <xsl:variable name="standardsTypes">
      <xsl:text>Data|Guide|ISP|IWA|PAS|R|TR|TS|TTA</xsl:text>
    </xsl:variable>

    <xsl:key name="WordTables" match="w:tbl" use="1"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert Word DOCX XML to TEI XML.</p>
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

    <xsl:key name="Sdt" match="w:sdt" use="w:sdtPr/w:tag/@w:val"/>
    <xsl:key name="AllSdt" match="w:sdtPr/w:tag/@w:val" use="1"/>

    <xsl:param name="preserveWordSections">true</xsl:param>
    <xsl:param name="preserveWordHeadersFooters">true</xsl:param>
    <xsl:param name="preserveEffects">true</xsl:param>
    <xsl:param name="preserveObject">true</xsl:param>	  

    <xsl:output indent="no"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	<p> Param defining whether to use a custom metadata file or to extract
	the metadata from the document </p>
      </desc>
    </doc>
    <xsl:param name="metadata-file"/>

    <xsl:param name="tableMethod">cals</xsl:param>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	<p>  Ignore existing title pages </p>
      </desc>
    </doc>
    <xsl:template match="w:p[.//w:sdt and not (w:pPr/w:pStyle/@w:val='zzSTDTitle')]" priority="1001"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='cover_warning']" priority="1002"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzCopyright']" priority="1002"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='idno']" priority="1002"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='copyrightdetails']" priority="1002"/>
    <xsl:template match="w:p[.//w:sdt and not
			 (w:pPr/w:pStyle/@w:val='zzSTDTitle')]"
		  priority="1001" mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='cover_warning']"
		  priority="1002" mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzCopyright']"
		  priority="1002" mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='idno']"
		  priority="1002" mode="paragraph"/>
    <xsl:template
	match="w:p[w:pPr/w:pStyle/@w:val='copyrightdetails']"
	priority="1002" mode="paragraph"/>


         <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Overwriting the creation of the teiHeader.
	Construct the TEI Header either by copying the passed metadata or extracting
      the metadata from the document </desc>
	 </doc>

    <xsl:template name="create-tei-header">
      <xsl:variable name="getCustomLang">
	<xsl:if test="doc-available($customFile)">
	<xsl:value-of
	    select="doc($customFile)//*[@name='DocIdentLanguage']/vt:lpwstr"/>
	</xsl:if>
      </xsl:variable>
      <xsl:variable name="getSdtLang">
	<xsl:call-template name="getSdt">
	  <xsl:with-param name="tag">doclanguage</xsl:with-param>
	</xsl:call-template>
      </xsl:variable>
      <xsl:attribute name="xml:lang">
	<xsl:choose>
	  <xsl:when test="$getCustomLang='' and
			  $getSdtLang=''">en</xsl:when>
	  <xsl:when test="not($getCustomLang='')">
	    <xsl:value-of select="$getCustomLang"/>
	  </xsl:when>
	  <xsl:when test="$getSdtLang='Russian'">ru</xsl:when>
	  <xsl:when test="$getSdtLang='French'">fr</xsl:when>
	  <xsl:otherwise>
	    <xsl:text>en</xsl:text>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:attribute>
       <xsl:choose>
            <xsl:when test="$metadata-file=''">
                <xsl:call-template name="teiHeader-extract-from-doc"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="teiHeader-copy-from-metadata-file"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <xsl:template name="teiHeader-copy-from-metadata-file">
        <xsl:copy-of select="document($metadata-file)/tei:teiHeader"/>
    </xsl:template>

    <xsl:template name="teiHeader-extract-from-doc">
        <xsl:variable name="customProps">
	  <xsl:if test="doc-available($customFile)">
	    <xsl:copy-of select="doc($customFile)"/>
	</xsl:if>
	</xsl:variable>
        <teiHeader>
            <fileDesc>
                <titleStmt>
                    <title iso:meta="introductoryTitle" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">introductoryTitle</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="mainTitle" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">mainTitle</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="complementaryTitle" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">complementaryTitle</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="supplementTitle" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">supplementTitle</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="fullTitle" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">fullTitle</xsl:with-param>
                        </xsl:call-template>
                    </title>

                    <title iso:meta="introductoryTitle_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">introductoryTitle_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="mainTitle_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">mainTitle_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="complementaryTitle_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">complementaryTitle_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="supplementTitle_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">supplementTitle_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="fullTitle_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">fullTitle_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>

                    <respStmt>
                        <resp>Committee</resp>
                        <name>
                            <xsl:call-template name="getSdt">
                                <xsl:with-param name="tag">committeeReference</xsl:with-param>
                            </xsl:call-template>
                        </name>
                    </respStmt>
                </titleStmt>
                <editionStmt>
                    <edition>
                        <xsl:attribute name="n">1</xsl:attribute>
                    </edition>
                </editionStmt>
                <publicationStmt>
                    <date iso:meta="docDate">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">docDate</xsl:with-param>
                        </xsl:call-template>
                    </date>
                    <publisher iso:meta="organization">
                        <xsl:value-of select="$customProps//*[@name='DocIdentSDO']/vt:lpwstr"/>
                    </publisher>
                    <authority iso:meta="secretariat">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">secretariat</xsl:with-param>
                        </xsl:call-template>
                    </authority>

                    <xsl:for-each-group select="key('AllSdt',1)" group-by=".">
                        <xsl:sort select="."/>
                        <xsl:variable name="thisSdt" select="current-grouping-key()"/>
                        <xsl:choose>
                            <xsl:when test="$thisSdt='committee'"/>
                            <xsl:when test="$thisSdt='complementaryTitle'"/>
                            <xsl:when test="$thisSdt='complementaryTitle_fr'"/>
                            <xsl:when test="$thisSdt='introductoryTitle'"/>
                            <xsl:when test="$thisSdt='introductoryTitle_fr'"/>
                            <xsl:when test="$thisSdt='mainTitle'"/>
                            <xsl:when test="$thisSdt='mainTitle_fr'"/>
                            <xsl:when test="$thisSdt='fullTitle'"/>
                            <xsl:when test="$thisSdt='fullTitle_fr'"/>
                            <xsl:when test="$thisSdt='supplementTitle'"/>
                            <xsl:when test="$thisSdt='supplementTitle_fr'"/>
                            <xsl:when test="$thisSdt='complementary_title'"/>
                            <xsl:when test="$thisSdt='complementary_title_fr'"/>
                            <xsl:when test="$thisSdt='introductory_title'"/>
                            <xsl:when test="$thisSdt='introductory_title_fr'"/>
                            <xsl:when test="$thisSdt='main_title'"/>
                            <xsl:when test="$thisSdt='main_title_fr'"/>
                            <xsl:when test="$thisSdt='docDate'"/>
                            <xsl:when test="$thisSdt='docdate'"/>
                            <xsl:when test="$thisSdt='organization'"/>
                            <xsl:when test="$thisSdt='secretariat'"/>
                            <xsl:when test="$thisSdt='docStage'"/>
                            <xsl:when test="starts-with($thisSdt, 'fw_')"/>
                            <xsl:otherwise>
                                <idno iso:meta="{$thisSdt}">
                                    <xsl:call-template name="getSdt">
                                        <xsl:with-param name="tag" select="$thisSdt"/>
                                    </xsl:call-template>
                                </idno>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:for-each-group>
                    <idno iso:meta="projectId">
                        <xsl:value-of select="$customProps//*[@name='DocIdentProjectId']/vt:lpwstr"/>
                    </idno>
                    <idno iso:meta="stage">
                        <xsl:value-of select="$customProps//*[@name='DocIdentStage']/vt:lpwstr"/>
                    </idno>
                    
                    <xsl:variable name="excludedPropNames">
                      committee committeeReference docDate docdate organization secretariat docNumber docPartNumber             
                    </xsl:variable>
                    <xsl:for-each select="$customProps//custprops:property">
                      <xsl:variable name="propName" select="@name"/>
                      <xsl:choose>
                        <xsl:when test="starts-with($propName, 'DocIdent')"/>
                        <xsl:when test="starts-with($propName, 'ISO_')"/>
                        <xsl:when test="starts-with($propName, 'TEI_')"/>
                        <xsl:when test="starts-with($propName, 'Word')"/>
                        <xsl:when test="contains($propName,'Title')"/>
                        <xsl:when test="contains($excludedPropNames, concat(' ',$propName,' '))"/>
                        <xsl:otherwise>
			  <xsl:if test="$debug='true'">
			    <xsl:message>
			      <xsl:text>create idno from property </xsl:text>
			      <xsl:value-of
				  select="$propName,'=',vt:lpwstr"/>
			    </xsl:message>
			  </xsl:if>
                          <idno iso:meta="{$propName}">
                            <xsl:value-of select="vt:lpwstr"/>
                          </idno>
                        </xsl:otherwise>
                      </xsl:choose>
                    </xsl:for-each>

                    <xsl:if test="w:body/w:p[w:pPr/w:pStyle/@w:val='zzCopyright']">
                        <availability>
                            <xsl:apply-templates select="w:body/w:p[w:pPr/w:pStyle/@w:val='zzCopyright']" mode="teiHeader"/>
                        </availability>
                    </xsl:if>

                    <xsl:if test="w:body/w:p[w:pPr/w:pStyle/@w:val='cover_warning'                             or w:pPr/w:pStyle/@w:val='coverWarning']">
                        <availability>
                            <xsl:apply-templates select="w:body/w:p[w:pPr/w:pStyle/@w:val='cover_warning'             or w:pPr/w:pStyle/@w:val='coverWarning']"
                                          mode="teiHeader"/>
                        </availability>
                    </xsl:if>

                </publicationStmt>
                <sourceDesc>
                    <p>Processed by ISOTEI on <xsl:value-of select="tei:whatsTheDate()"/>
                    </p>
                </sourceDesc>
            </fileDesc>
            <profileDesc>
                <textClass>
                    <classCode>
                        <xsl:attribute name="scheme">#TYPE</xsl:attribute>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">docType</xsl:with-param>
                            <xsl:with-param name="oldtag">docType</xsl:with-param>
                        </xsl:call-template>
                    </classCode>
                    <classCode>
                        <xsl:attribute name="scheme">#SUPPLTYPE</xsl:attribute>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">docSubtype</xsl:with-param>
                            <xsl:with-param name="oldtag">docSubtype</xsl:with-param>
                        </xsl:call-template>
                    </classCode>
                </textClass>
            </profileDesc>
            <encodingDesc>
                <xsl:call-template name="generateAppInfo"/>
                <classDecl>
                    <taxonomy>
                        <category>
                            <xsl:attribute name="xml:id">TYPE</xsl:attribute>
                        </category>
                        <category>
                            <xsl:attribute name="xml:id">SUPPLTYPE</xsl:attribute>
                        </category>
                    </taxonomy>
                </classDecl>
            </encodingDesc>
            <revisionDesc>
                <change/>
            </revisionDesc>
        </teiHeader>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Construct the TEI Header either by copying the passed metadata or extracting
	the metadata from the document simple templates for the info
	that goes into the teiHeader</desc>
    </doc>
    <xsl:template match="w:p" mode="teiHeader">
        <xsl:choose>
            <xsl:when test="w:pPr/w:pStyle[@w:val='document_title']">
                <xsl:for-each select="w:sdt">
                    <xsl:if test="contains(w:sdtPr/w:tag/@w:val,'title')">
                        <title>
                            <xsl:choose>
                                <xsl:when test="contains(w:sdtPr/w:tag/@w:val,'_fr')">
                                    <xsl:attribute name="xml:lang">fr</xsl:attribute>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:attribute name="xml:lang">en</xsl:attribute>
                                </xsl:otherwise>
                            </xsl:choose>
                            <xsl:attribute name="type">
                                <xsl:value-of select="substring-before(w:sdtPr/w:tag/@w:val,'_')"/>
                            </xsl:attribute>
                            <xsl:apply-templates select="w:sdtContent/w:r"/>
                        </title>
                    </xsl:if>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <p>
                    <xsl:apply-templates/>
                </p>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Override mechanism for deciding on what subsections are</desc>
    </doc>
    <xsl:template name="group-by-section">
      <xsl:variable name="Style" select="w:pPr/w:pStyle/@w:val"/>
      <xsl:variable name="NextLevel" select="number(@LEVEL) + 1"/>
      <div>
	<!-- generate the head -->
	<xsl:call-template name="generate-section-heading">
	  <xsl:with-param name="Style" select="$Style"/>
	</xsl:call-template>
	<!-- Process sub-sections -->
	<xsl:for-each-group select="current-group() except ."
			    group-starting-with="w:p[@LEVEL=$NextLevel]">
	  <xsl:choose>
	    <xsl:when test="tei:is-heading(.)">
	      <xsl:call-template name="group-by-section"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="." mode="inSectionGroup"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
		    </div>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Some specific section headers 
      </desc>
    </doc>
    <xsl:template name="generate-section-heading">
        <xsl:param name="Style"/>
        <xsl:variable name="divname">
            <xsl:for-each select=".//w:t">
                <xsl:if test="string(number(translate(., '.', ''))) = 'NaN' and string(number(translate(translate(.,'A',''),'.','')))='NaN'">
                    <xsl:value-of select="."/>
                </xsl:if>
            </xsl:for-each>
        </xsl:variable>
        <xsl:choose>
            <!-- if we are dealing with a normal header (Style='heading [123456..]')-->
            <xsl:when test="starts-with($Style,'heading')">
                <xsl:variable name="type">
                    <xsl:choose>
                        <xsl:when test="$divname=$frScope">scope</xsl:when>
                        <xsl:when test="$divname='Références normatives'">normativeReferences</xsl:when>
                        <xsl:when test="$divname='Termes et définitions'">termsAndDefinitions</xsl:when>
                        <xsl:when test="$divname='Symboles'">symbolsAndTerms</xsl:when>
                        <xsl:when test="$divname='Foreword'">foreword</xsl:when>
                        <xsl:when test="$divname='zzIntroduction'">introduction</xsl:when>
                        <xsl:when test="$divname='Introduction'">introduction</xsl:when>
                        <xsl:when test="$divname='Scope'">scope</xsl:when>
                        <xsl:when test="$divname='Normative references'">normativeReferences</xsl:when>
                        <xsl:when test="$divname='Terms and definitions'">termsAndDefinitions</xsl:when>
                        <xsl:when test="$divname='Symbols and abbreviated terms'">symbolsAndTerms</xsl:when>
                        <xsl:when test="$divname='Requirements'">requirements</xsl:when>
                        <xsl:when test="$divname='Sampling'">sampling</xsl:when>
                        <xsl:when test="$divname='Test methods'">test</xsl:when>
                        <xsl:when test="$divname='Classification, designation and coding'">classification</xsl:when>
                        <xsl:when test="$divname='Marking, labelling and packaging'">marking</xsl:when>
                    </xsl:choose>
                </xsl:variable>
                <xsl:if test="not($type='')">
                    <xsl:attribute name="type">
                        <xsl:value-of select="$type"/>
                    </xsl:attribute>
                </xsl:if>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>
            <xsl:when test="contains($Style,'ANNEX')">
                <xsl:attribute name="type">annex</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>
            <xsl:when test="contains($Style,'termHeading')">
                <xsl:attribute name="type">termHeading</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>
            <xsl:when test="$Style=$BibliographyHeading">
                <xsl:attribute name="type">bibliography</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>
            <xsl:when test="$Style=$ForewordHeading">
                <xsl:attribute name="type">foreword</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>
            <xsl:when test="$Style=$zzIntroductionHeading">
                <xsl:attribute name="type">introduction</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>

            <xsl:when test="$Style=$IntroductionHeading">
                <xsl:attribute name="type">introduction</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>

            <xsl:when test="$Style='a2' or $Style='a3' or $Style='a4' or $Style='a5' or $Style='a6'">
                <xsl:attribute
                name="type">annexSection</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>

            <xsl:when test="$Style='pA2' or $Style='pA3' or $Style='pA4' or $Style='pA5' or $Style='pA6'">
                <xsl:attribute
                name="type">headless</xsl:attribute>
                <xsl:if test="string(normalize-space($divname))">
                    <p>
		      <xsl:call-template name="process-checking-for-crossrefs"/>
                    </p>
                </xsl:if>
            </xsl:when>

            <xsl:otherwise>
                <xsl:attribute name="type">headless</xsl:attribute>
                <xsl:if test="string(normalize-space($divname))">
                    <p>
		      <xsl:call-template name="process-checking-for-crossrefs"/>
                    </p>
                </xsl:if>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	<p>
        We are now working on a group of all elements inside some group bounded by
        headings. These need to be further split up into smaller groups for figures,
        list etc. and into individual groups for simple paragraphs.
        Be careful about the position(), since it might be 1,2,3.           
	For each defined grouping call a specific
	template. If there is no grouping defined, apply
	templates with mode paragraph.
	</p>
      </desc>
    </doc>
    <xsl:template match="w:p" mode="inSectionGroup" priority="-100">
        <xsl:for-each-group select="current-group()"
                          group-adjacent="if
					  (contains(w:pPr/w:pStyle/@w:val,'Figure'))
					  then 0 else
                           if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else
             if ((w:pPr/w:pStyle/@w:val='Note') and
	     (contains(preceding-sibling::w:p[1]/w:pPr/w:pStyle/@w:val,'List'))) then 1 else
	     if (w:pPr/w:pStyle/@w:val=$BibliographyItem) then 4 else
	     if (w:pPr/w:pStyle/@w:val=$DefinitionList) then 5 else
	     if (w:pPr/w:pStyle/@w:val='Definition') then 3 else
	     if (w:pPr/w:pStyle/@w:val='RefNorm') then 2 else
             if (w:pPr/w:pStyle/@w:val='Example numbered') then 3 else
             if (w:pPr/w:pStyle/@w:val='Example list') then 3 else
             if (w:pPr/w:pStyle/@w:val='Example') then 3 else
             if (w:pPr/w:pStyle/@w:val='TermNum') then 3 else
             if (w:pPr/w:pStyle/@w:val='entrySource') then 3 else
             if (w:pPr/w:pStyle/@w:val='nonVerbalRepresentation') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteDefinition') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteExample') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteNonVerbalRepresentation') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteSymbol') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteTerm') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteTermEntry') then 3 else
             if (w:pPr/w:pStyle/@w:val='symbol') then 3 else
             if (w:pPr/w:pStyle/@w:val='termAdmitted') then 3 else
             if (w:pPr/w:pStyle/@w:val='termDeprecated') then 3 else
             if (w:pPr/w:pStyle/@w:val='termPreferred') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum1') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum2') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum3') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum4') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum5') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNum6') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNumA2') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNumA3') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNumA4') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNumA5') then 3 else
	     if (w:pPr/w:pStyle/@w:val='autoTermNumA6') then 3 else
	     if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 6 else
	     if (w:pPr/w:pStyle/@w:val='Special') then 7 else
	     position() + 100">

            <xsl:choose>
                <xsl:when test="current-grouping-key()=0">
                    <xsl:call-template name="figureSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=1">
                    <xsl:call-template name="listSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=2">
                    <xsl:call-template name="normativeReferencesSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=3">
                    <xsl:call-template name="termsAndDefinitionsSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=4">
                    <xsl:call-template name="bibliographySection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=5">
                    <xsl:call-template name="definitionListSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=6">
                    <xsl:call-template name="tocSection"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=7">
                    <xsl:call-template name="doSpecialStyle"/>		  
                </xsl:when>

                <!-- it is not a defined grouping .. apply templates -->
                <xsl:otherwise>
                    <xsl:apply-templates select="." mode="paragraph"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each-group>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
        <desc>
            <p>Handle 'Special' style-override paragraphs, converting Word's styles to CSS</p>
        </desc>
    </doc>
    <xsl:template name="doSpecialStyle">
        <xsl:for-each-group select="current-group()" group-starting-with="w:p">
          <xsl:variable name="css">
            <xsl:if test="w:pPr/w:ind">
                <!-- this is a block indent, ie (in CSS terms) a margin -->
                <xsl:if test="w:pPr/w:ind/@w:left">
                    <!-- margin-left: w:pPr/w:ind/@w:left -->
                    <!-- units: px? -->
                    <xsl:text>margin-left: </xsl:text>
                    <xsl:value-of select="w:pPr/w:ind/@w:left"/>
                    <xsl:text>; </xsl:text>
                </xsl:if>
                <xsl:if test="w:pPr/w:ind/@w:right">
                    <!-- margin-right: w:pPr/w:ind/@w:right -->
                    <!-- units: px? -->
                    <xsl:text>margin-right: </xsl:text>
                    <xsl:value-of select="w:pPr/w:ind/@w:right"/>
                    <xsl:text>; </xsl:text>
                </xsl:if>
            </xsl:if>
            <xsl:if test="w:pPr/w:spacing">
                <xsl:if test="w:pPr/w:spacing/@w:before">
                    <!-- margin-top: w:pPr/w:spacing/@w:before -->
                    <!-- units: px? -->
                    <xsl:text>margin-top: </xsl:text>
                    <xsl:value-of select="w:pPr/w:spacing/@w:before"/>
                    <xsl:text>; </xsl:text>                    
                </xsl:if>
                <xsl:if test="w:pPr/w:spacing/@w:after">
                    <!-- margin-bottom: w:pPr/w:spacing/@w:after -->
                    <!-- units: px? -->
                    <xsl:text>margin-bottom: </xsl:text>
                    <xsl:value-of select="w:pPr/w:spacing/@w:after"/>
                    <xsl:text>; </xsl:text>
                </xsl:if>                
            </xsl:if>
            <xsl:if test="w:pPr/w:jc">
                <!-- text-align: w:pPr/w:jc/@w:val -->
                <xsl:text>text-align: </xsl:text>
                <xsl:value-of select="w:pPr/w:jc/@w:val"/>
                <xsl:text>; </xsl:text>
            </xsl:if>
            <xsl:if test="w:pPr/w:rPr/w:rFonts">
                <!-- font-family: w:pPr/w:rPr/w:rFonts/@w:ascii -->
                <xsl:text>font-family: </xsl:text>
                <xsl:value-of select="w:pPr/w:rPr/w:rFonts/@w:ascii"/>
                <xsl:text>; </xsl:text>
            </xsl:if>
            <xsl:if test="w:pPr/w:rPr/w:sz">
                <!-- font-size: w:pPr/w:rPr/w:sz/@w:val -->
                <!-- units: pt? -->
                <xsl:text>font-size: </xsl:text>
                <xsl:value-of select="w:pPr/w:rPr/w:sz/@w:val"/>
                <xsl:text>; </xsl:text>
            </xsl:if>
        </xsl:variable>
        <p rend="Special">
            <xsl:attribute name="iso:style">
	      <xsl:value-of select="$css"/>
	    </xsl:attribute>
	    <xsl:apply-templates/>
        </p>
     </xsl:for-each-group>
    </xsl:template>
    
         
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>override handling of runs 
      </desc>
    </doc>

    <xsl:template match="w:r/w:tab">
      <g ref="x:tab"/>
    </xsl:template>

    <xsl:template name="processTextrun">
        <xsl:variable name="style">
            <xsl:value-of select="w:rPr/w:rStyle/@w:val"/>
        </xsl:variable>
        <xsl:variable name="pstyle">
            <xsl:value-of select="parent::w:p/w:pPr/w:pStyle/@w:val"/>
        </xsl:variable>
        <xsl:choose>
            <!-- 
	     ignore some headingnotechars
	-->
            <!--
	    <xsl:when test="$style=$ExampleHeadingChar"/>
	    <xsl:when test="$style=$NoteHeadingChar"/>
	    <xsl:when
	    test="$style=$TableNoteHeadingChar"/>
	-->

	    <!-- stored elsewhere in TBX -->
            <xsl:when test="$style='gender'"/>
	    <xsl:when test="$style='number'"/> 
	    <xsl:when test="$style='pronunciation'"/>
            <xsl:when test="$style='partOfSpeech'"/>
            <xsl:when test="$style='geographicalUse'"/>
            <xsl:when test="$style='script'"/>
            <xsl:when test="$style='language' and ancestor::w:p/w:pPr/w:pStyle/@w:val='termPreferred'"/>
            <xsl:when test="$style='language'">
                <hi rend="language">
                    <xsl:apply-templates/>
		</hi>
            </xsl:when>

	    <xsl:when test="$style='CommentReference'">
	      <xsl:apply-templates/>
	    </xsl:when>
            <xsl:when test="$style=$HeadingChar"/>
            <xsl:when test="$style=$HeadingCharFr"/>
            <xsl:when test="$style=$BibliographyReference"/>
            <xsl:when test="$style=$TableHeadingChar"/>
            <xsl:when test="$style='mentioned'">
                <mentioned>
                    <xsl:apply-templates/>
                </mentioned>
            </xsl:when>

	    <xsl:when test="$style='Hyperlink' and ancestor::w:hyperlink">
	      <xsl:call-template name="basicStyles"/>
	    </xsl:when>
	    
	    <xsl:when test="$style='Hyperlink' and ancestor::w:fldSimple">
	      <xsl:call-template name="basicStyles"/>
	    </xsl:when>
	    
            <xsl:when test="$style='Hyperlink'">
	      <xsl:variable name="ref">
		<xsl:value-of
		    select="preceding-sibling::w:r[w:instrText][1]/w:instrText">
		</xsl:value-of>
	      </xsl:variable>
	      <xsl:variable name="rends">
		<xsl:choose>
		  <xsl:when test="starts-with($ref,' REF') or starts-with($ref,'REF')"><r>ref</r></xsl:when>
		  <xsl:when test="starts-with($ref,' NOTEREF') or starts-with($ref,'NOTEREF')"><r>noteref</r></xsl:when>
		</xsl:choose>
		<xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\r')"><r>instr_r</r></xsl:if>
		<xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\f')"><r>instr_f</r></xsl:if>
		<xsl:if test="contains(following-sibling::w:r[w:instrText][1],'\n')"><r>instr_n</r></xsl:if>
		<xsl:if test="contains(following-sibling::w:r[w:instrText][1],'MERGEFORMAT')"><r>mergeformat</r></xsl:if>
	      </xsl:variable>
	      <xsl:choose>
		<xsl:when test="starts-with($ref,' REF') or starts-with($ref,'REF')">
		  <xsl:call-template name="basicStyles"/>
		</xsl:when>
		<xsl:otherwise>
		  <ref>
		    <xsl:attribute name="rend">
		      <xsl:value-of select="string-join(($rends/r),' ')"/>
		    </xsl:attribute>
		    <xsl:attribute name="target">
		      <xsl:value-of
			  select="substring-before(substring-after($ref,'&#x0022;'),'&#x0022;')"/>
		    </xsl:attribute>
		    <xsl:call-template name="basicStyles"/>
		  </ref>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:when>
	    
            <xsl:when test="$pstyle='RefNorm'">
	      <xsl:apply-templates/>
            </xsl:when>

            <xsl:when test="$style='TableFootnoteXref'">
                <ref rend="TableFootnoteXref">
                    <xsl:apply-templates/>
                </ref>
            </xsl:when>

            <xsl:when test="$style='ref'">
                <ref>
                    <xsl:apply-templates/>
                </ref>
            </xsl:when>

            <xsl:when test="$style='date'">
                <date>
                    <xsl:apply-templates/>
                </date>
            </xsl:when>

            <xsl:when test="$style='isonumber'">
                <num>
                    <xsl:apply-templates/>
                </num>
            </xsl:when>

            <xsl:when test="$style='isononumber'">
                <seg rend="nonumber">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>

            <xsl:when test="$style=$FormulaReference">
                <!--<seg rend="FormulaReference">
	      <xsl:apply-templates/>
	      </seg>-->
            </xsl:when>

            <xsl:when test="$style=$ExtXref">
                <ref>
                    <xsl:apply-templates/>
                </ref>
            </xsl:when>

            <xsl:when test="$style='statement'">
                <seg iso:provision="statement">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>

            <xsl:when test="$style='FootnoteReference'">
	      <hi rend="FootnoteReference">
		<xsl:apply-templates/>
	      </hi>
	    </xsl:when>

            <xsl:when test="$style='recommendation'">
                <seg iso:provision="recommendation">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>

	    <xsl:when test="$style= 'domain' or $style ='source'">
	      <hi rend="{$style}">
		<xsl:apply-templates/>
	      </hi>
	    </xsl:when>

	    <xsl:when test="$style='termRef'">
	      <hi type="entailedTerm" xmlns="http://www.lisa.org/TBX-Specification.33.0.html">
		<xsl:apply-templates/>
	      </hi>
	    </xsl:when>

            <xsl:when test="$style='requirement'">
                <seg iso:provision="requirement">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>

            <xsl:when test="$style='possibility_and_capability'">
                <seg iso:provision="possibilityandcapability">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>

            <xsl:when test="$style='FigureFootnoteXref'">
                <hi rend="FigureFootnoteXref">
                    <xsl:apply-templates/>
                </hi>
            </xsl:when>

            <xsl:otherwise>
	      <xsl:call-template name="basicStyles"/>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Table titles. we deal with them inside the table</desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]" mode="paragraph"/>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='TableTitle']" mode="paragraph"/>

     <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>When creating a CALS table, put in an attribute linking it to
	the original Word markup in an external file</desc>
   </doc>

	  <xsl:template match="cals:table" mode="innerTable">
	    <xsl:param name="n"  tunnel="yes"/>
	     <xsl:copy>
	        <xsl:copy-of select="@*"/>
		<xsl:attribute name="tei:corresp">
		  <xsl:text>embeddings/table</xsl:text>
		  <xsl:value-of select="$n"/>
		  <xsl:text>.xml</xsl:text>
		</xsl:attribute>
	        <xsl:apply-templates mode="innerTable"/>	    
	     </xsl:copy>
	  </xsl:template>

     <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>For every table in Word, copy the contents to an external file.</desc>
     </doc>
	  <xsl:template name="fromDocxFinalHook">
	    <xsl:for-each select="key('WordTables',1)">
		<xsl:variable name="filename">
		  <xsl:value-of select="$word-directory"/>
		  <xsl:text>/word/embeddings/table</xsl:text>
		  <xsl:number level="any"/>
		  <xsl:text>.xml</xsl:text>
		</xsl:variable>
		<xsl:result-document href="{$filename}">
		  <xsl:copy>
		    <xsl:copy-of select="@*"/>
		    <xsl:variable name="tablePass1">
		      <xsl:apply-templates mode="copytable">
			<xsl:with-param name="n" tunnel="yes">
			  <xsl:number level="any"/>
			</xsl:with-param>
		      </xsl:apply-templates>
		    </xsl:variable>
		    <xsl:for-each select="$tablePass1">
		      <xsl:apply-templates mode="pass2"/>
		    </xsl:for-each>
		  </xsl:copy>
		</xsl:result-document>
	    </xsl:for-each>
	  </xsl:template>

	  <xsl:template match="*" mode="copytable">
	    <xsl:apply-templates select="."/>
	  </xsl:template>

	  <xsl:template
	      match="
	       w:bottom | 
	       w:gridCol | 
	       w:gridSpan | 
	       w:insideH | 
	       w:insideV | 
	       w:jc | 
	       w:pPr |
	       w:p |
	       w:left | 
	       w:pStyle |
	       w:right | 
	       w:spacing | 
	       w:tbl | 
	       w:tblBorders | 
	       w:tblCellMar | 
	       w:tblGrid | 
	       w:tblLayout | 
	       w:tblLook | 
	       w:tblPr | 
	       w:tblStyle | 
	       w:tblW | 
	       w:tc | 
	       w:tcBorders | 
	       w:tcPr | 
	       w:tcW | 
	       w:top | 
	       w:tr | 
	       w:trPr | 
	       w:vAlign "
	      mode="copytable">
	    <xsl:copy>
	      <xsl:copy-of select="@*"/>
	      <xsl:apply-templates mode="copytable"/>
	    </xsl:copy>
	  </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Working with figure headings</desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Figuretitle]" mode="paragraph">
        <head>
            <xsl:apply-templates/>
        </head>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Figure subtitle']" mode="paragraph">
        <head type="subtitle">
            <xsl:apply-templates/>
        </head>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Text in figure heading</desc>
    </doc>
    <xsl:template
	match="w:p[w:pPr/w:pStyle/@w:val=$Figuretitle]/w:r/w:t">
      <xsl:choose>
	<xsl:when test="starts-with(.,'— ')">
	  <xsl:value-of select="substring(.,3)"/>
	</xsl:when>
	<xsl:when test="starts-with(.,' — ')">
	  <xsl:value-of select="substring(.,4)"/>
	</xsl:when>
	<xsl:when test="starts-with(.,'—')">
	  <xsl:value-of select="substring(.,2)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="."/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Text in table heading</desc>
    </doc>
    <xsl:template
	match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]/w:r/w:t">
      <xsl:apply-templates/>
    </xsl:template>

    <xsl:template
	match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]/w:r/w:t/text()">
      <xsl:analyze-string select="." regex="^\s?[\-—]+\s?">
	<xsl:matching-substring>
	</xsl:matching-substring>
	<xsl:non-matching-substring>
	  <xsl:value-of select="."/>
	</xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Figure text']" mode="paragraph">
        <p>
	  <xsl:if test="w:pPr/w:jc/@w:val">
	    <xsl:attribute name="iso:style">
	      <xsl:text>text-align:</xsl:text>
	      <xsl:value-of select="w:pPr/w:jc/@w:val"/>
	    </xsl:attribute>
	  </xsl:if>
	  <xsl:attribute name="rend">
	    <xsl:text>Figure text</xsl:text>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</p>
    </xsl:template>
    <xsl:template name="figureSection">
        <figure>
            <xsl:for-each select="current-group()">
                <xsl:apply-templates select="." mode="paragraph"/>
            </xsl:for-each>
        </figure>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Dealing with Normative References</desc>
	</doc>
    <xsl:template name="normativeReferencesSection">
        <listBibl type="normativeReferences">
	  <xsl:for-each select="current-group()">
	    <bibl>
	      <xsl:call-template name="parseReference"/>
	    </bibl>
	  </xsl:for-each>
        </listBibl>
    </xsl:template>


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	<p>Terms and definitions. The following styles are block-level
	objects in a term entry</p>
	<ul>
	  <li>TermNum</li>
	  <li>autoTermNum</li>
	  <li>Example</li>
	  <li>Example List</li>
	  <li>Example numbered</li>
	  <li>symbol</li>
	  <li>termAdmitted</li>
	  <li>termDeprecated</li>
	  <li>termPreferred</li>
	  <li>abbreviatedForm</li>
	</ul>
	Though it should be noted that <gi>Example</gi> can also occur
	on its own.</desc>
    </doc>
    <xsl:template name="termsAndDefinitionsSection">
      <xsl:for-each-group select="current-group()"
			  group-starting-with="w:p[w:pPr/w:pStyle/@w:val='TermNum' or w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]]">
	<xsl:choose>
	  <xsl:when test="self::w:p[w:pPr/w:pStyle[
			  @w:val='Example numbered' 
			  or @w:val='Example list' 
			  or @w:val='Example']]">
	    <xsl:for-each select="current-group()">
	      <xsl:variable name="Style">
		<xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	      </xsl:variable>
	      <p rend="{$Style}"> 
		<xsl:call-template name="process-checking-for-crossrefs"/>
<!--		<xsl:apply-templates/> -->
	      </p>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:when test="not(self::w:p[w:pPr/w:pStyle/@w:val='TermNum'  or  w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]])">

	    <xsl:if test="string-length(.)&gt;0">
	      <iso:error>
	      <xsl:text>A badly-structured terminology entry group
	      here, starting with a "</xsl:text>
	      <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	      <xsl:text>" (</xsl:text>
	      <xsl:value-of select="."/>
	      <xsl:text>). </xsl:text>
	      <xsl:text>Term entries must start with     a number</xsl:text>
	      </iso:error>
	    </xsl:if>
	    <xsl:for-each select="current-group()">
	      <xsl:variable name="Style">
		<xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	      </xsl:variable>
	      <p rend="{$Style}"> 
		<xsl:call-template name="process-checking-for-crossrefs"/>
<!--		<xsl:apply-templates/>-->
	      </p>
	    </xsl:for-each>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:variable name="Style">
	      <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	    </xsl:variable>
	    <xsl:variable name="ID">
	      <xsl:choose>
		<xsl:when test="$Style='TermNum'">
		  <xsl:text>user_</xsl:text>
		  <xsl:value-of select="."/>
		</xsl:when>
		<xsl:when
		    test="starts-with($Style,'autoTermNum')">
		  <xsl:value-of select="$Style"/>
		  <xsl:text>_</xsl:text>
		  <xsl:number level="any"/>
		</xsl:when>
	      </xsl:choose>
	    </xsl:variable>
<!-- oucs0037: are there some types of tbx that we don't allow refs
     in? or should all of these actually be calling process-checking-for-crossrefs? -->
	    <termEntry xmlns="http://www.lisa.org/TBX-Specification.33.0.html" id="{$ID}">
	      <xsl:if test="w:bookmarkStart">
		<xsl:attribute name="xml:id" select="substring-after(w:bookmarkStart/@w:name,'_')"/>
	      </xsl:if>
	      <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteTermEntry'] except .">
		<note>
		  <xsl:apply-templates/>
		</note>
	      </xsl:for-each>
	      <descripGrp>
		<descrip type="definition">
		  <xsl:for-each
		      select="current-group()[w:pPr/w:pStyle/@w:val='Definition']
			      except .">
		      <xsl:call-template name="process-checking-for-crossrefs"/>
		  </xsl:for-each>
		</descrip>
		<xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteDefinition'] except .">
		  <note>
		    <xsl:apply-templates/>
		  </note>
		</xsl:for-each>
		<xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='entrySource'] except .">
		  <admin type="entrySource">
<!--		    <xsl:apply-templates/> -->
		      <xsl:call-template name="process-checking-for-crossrefs"/>
		  </admin>
		</xsl:for-each>
	      </descripGrp>
		<xsl:for-each
		    select="current-group()[w:pPr/w:pStyle/@w:val='noteSymbol']
			    except .">
		  <descripGrp>
		    <descrip type="symbol"></descrip>
		      <note>
			<xsl:apply-templates/> 
		      </note>
		  </descripGrp>
		</xsl:for-each>

		<xsl:variable name="nn">
		  <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteExample'] except .">
		    <note>
		      <xsl:apply-templates/>
		    </note>
		  </xsl:for-each>
		</xsl:variable>

	      <xsl:for-each select="current-group()[w:pPr/w:pStyle[starts-with(@w:val,'Example')]]">
		<descripGrp>
		  <descrip type="example">
		    <xsl:choose>
		      <xsl:when test="w:pPr/w:pStyle/@w:val='Example numbered'">
			<xsl:attribute name="iso:class">numbered</xsl:attribute>
		      </xsl:when>
		      <xsl:when test="w:pPr/w:pStyle/@w:val='Example list'">
			<xsl:attribute name="iso:class">list</xsl:attribute>
		      </xsl:when>
		    </xsl:choose>
		      <xsl:apply-templates/>
		  </descrip>
		  <xsl:copy-of select="$nn"/>
		</descripGrp>
	      </xsl:for-each>

	      <xsl:if
		  test="current-group()[w:pPr/w:pStyle/@w:val='nonVerbalRepresentation']">
		  <descripGrp>
		  <descrip type="figure">
		    <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='nonVerbalRepresentation'] except .">
		      <xsl:apply-templates/>
		    </xsl:for-each>
		  </descrip>
		  <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteNonVerbalRepresentation'] except .">
		    <note>
		      <xsl:apply-templates/>
		    </note>
		  </xsl:for-each>
		</descripGrp>
	      </xsl:if>
	      <langSet>
		<xsl:attribute name="xml:lang">
		  <xsl:choose>
		    <xsl:when test="current-group()[w:r/w:rPr/w:lang/@w:val]">
		      <xsl:value-of select="subsequence(current-group()/w:r[w:rPr/w:lang][1]/w:rPr/w:lang/@w:val,1,1)"/>
		    </xsl:when>
		    <xsl:when test="w:pPr/w:rPr/w:lang/w:val">
		      <xsl:value-of select="w:pPr/w:rPr/w:lang/@w:val"/>
		    </xsl:when>
		    <xsl:otherwise>en</xsl:otherwise>
		  </xsl:choose>
		</xsl:attribute>
		<xsl:for-each select="current-group()">
		  <xsl:variable name="Thing">
		    <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
		  </xsl:variable>
		  <xsl:choose>
		    <xsl:when test="$Thing='autoTermNum1'"/>
		    <xsl:when test="$Thing='autoTermNum2'"/>
		    <xsl:when test="$Thing='autoTermNum3'"/>
		    <xsl:when test="$Thing='autoTermNum4'"/>
		    <xsl:when test="$Thing='autoTermNum5'"/>
		    <xsl:when test="$Thing='autoTermNum6'"/>
		    <xsl:when test="$Thing='autoTermNumA2'"/>
		    <xsl:when test="$Thing='autoTermNumA3'"/>
		    <xsl:when test="$Thing='autoTermNumA4'"/>
		    <xsl:when test="$Thing='autoTermNumA5'"/>
		    <xsl:when test="$Thing='autoTermNumA6'"/>
		    <xsl:when test="$Thing='entrySource'"/>
		    <xsl:when test="$Thing='TermNum'"/>
		    <xsl:when test="$Thing='Definition'"/>
		    <xsl:when test="$Thing='Example numbered'"/>
		    <xsl:when test="$Thing='Example list'"/>
		    <xsl:when test="$Thing='Example'"/>
		    <xsl:when test="$Thing='nonVerbalRepresentation'"/>
		    <xsl:when test="$Thing='noteDefinition'"/>
		    <xsl:when test="$Thing='noteExample'"/>
		    <xsl:when test="$Thing='noteNonVerbalRepresentation'"/>
		    <xsl:when test="$Thing='noteSymbol'"/>
		    <xsl:when test="$Thing='noteTerm'"/>
		    <xsl:when test="$Thing='noteTermEntry'"/>
		    <xsl:otherwise>
		      <ntig>
			<termGrp>
			  <term>
			    <xsl:apply-templates/>
			  </term>
			  <termNote type="partOfSpeech">
			    <xsl:choose>
			      <xsl:when test="w:r/w:rPr/w:rStyle/@w:val='partOfSpeech'">
				<xsl:value-of select="w:r[w:rPr/w:rStyle/@w:val='partOfSpeech']"/>
			      </xsl:when>
			      <xsl:otherwise>
				<xsl:text>noun</xsl:text>
			      </xsl:otherwise>
			    </xsl:choose>
			  </termNote>
			  <termNote type="administrativeStatus">
			      <xsl:if
				  test="w:r/w:rPr/w:rStyle/@w:val='symbol'
					or m:oMath/m:r/w:rPr/w:rStyle/@w:val='symbol'">
				<xsl:attribute
				    name="iso:style">symbol</xsl:attribute>
			      </xsl:if>
			      <xsl:choose>
				<xsl:when test="$Thing='termPreferred'">preferredTerm-admn-sts</xsl:when>
				<xsl:when test="$Thing='termDeprecated'">deprecatedTerm-admn-sts</xsl:when>
				<xsl:when test="$Thing='termAdmitted'">admittedTerm-admn-sts</xsl:when>
				<xsl:otherwise>UNKNOWN</xsl:otherwise>
			      </xsl:choose>
			  </termNote>
			  <xsl:if test="w:r/w:rPr/w:rStyle/@w:val='abbreviatedForm'">
			    <termNote type="termType">abbreviation</termNote>
			  </xsl:if>

			  <xsl:choose>
			  <xsl:when
			      test="w:r/w:rPr/w:rStyle/@w:val='geographicalUse'">
			    <termNote type="geographicalUsage">
			      <xsl:if test="w:r[w:rPr/w:rStyle/@w:val='language']">
				<xsl:value-of
				    select="w:r[w:rPr/w:rStyle/@w:val='language']"/>
				<xsl:text>-</xsl:text>
			      </xsl:if>
			      <xsl:for-each
				  select="w:r[w:rPr/w:rStyle/@w:val='geographicalUse']">
				<xsl:value-of select="normalize-space(.)"/>
			      </xsl:for-each>
			      <xsl:if test="w:r[w:rPr/w:rStyle/@w:val='script']">
				<xsl:text>-x-</xsl:text>
				<xsl:value-of
				    select="normalize-space(w:r[w:rPr/w:rStyle/@w:val='script'])"/>
			      </xsl:if>
			    </termNote>
			  </xsl:when>
			  <xsl:otherwise>
			    <xsl:if test="w:r[w:rPr/w:rStyle/@w:val='language']">
			      <tei:hi rend="language">
				<xsl:value-of
				  select="w:r[w:rPr/w:rStyle/@w:val='language']"/>
			      </tei:hi>
			    </xsl:if>
			  </xsl:otherwise>
			  </xsl:choose>

			  <xsl:apply-templates select="w:r[w:rPr/w:rStyle/@w:val='gender']" mode="inTerm"/>
			  <xsl:apply-templates select="w:r[w:rPr/w:rStyle/@w:val='number']" mode="inTerm"/>

			  <xsl:if test="w:r[w:rPr/w:rStyle/@w:val='pronunciation']">
			    <termNote type="pronunciation"
				      xmlns="http://www.lisa.org/TBX-Specification.33.0.html">
			      <xsl:for-each select="w:r[w:rPr/w:rStyle/@w:val='pronunciation']">
				    <xsl:value-of 
					select="replace(replace(.,'/ ',''),' /','')"/>
			      </xsl:for-each>
			    </termNote>
			  </xsl:if>
			</termGrp>
			<xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteTerm'] except .">
			  <note type="noteTerm">
			    <xsl:apply-templates/>
			  </note>
			</xsl:for-each>
		      </ntig>
		    </xsl:otherwise>
		  </xsl:choose>
		</xsl:for-each>
	      </langSet>
	    </termEntry>
<!--
	    <xsl:for-each
		select="current-group()[w:pPr/w:pStyle/@w:val='Example']
			except .">
	      <p rend="Example"><xsl:apply-templates/></p>
	    </xsl:for-each>
-->
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:for-each-group>
    </xsl:template>


    <xsl:template match="w:r[w:rPr/w:rStyle/@w:val='gender']"  mode="inTerm">
      <termNote type="grammaticalGender" xmlns="http://www.lisa.org/TBX-Specification.33.0.html">
	<xsl:choose>
	  <xsl:when test=".='m'">masculine</xsl:when>
	  <xsl:when test=".='f'">feminine</xsl:when>
	  <xsl:when test=".='n'">neuter</xsl:when>
	  <xsl:otherwise>otherGender</xsl:otherwise>
	</xsl:choose>
      </termNote>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>inline style 'number' turns into a <gi>termNote</gi>
      </desc>
    </doc>

    <xsl:template match="w:r[w:rPr/w:rStyle/@w:val='number']"  mode="inTerm">
      <termNote type="grammaticalNumber" xmlns="http://www.lisa.org/TBX-Specification.33.0.html">
	<xsl:value-of select="."/>
      </termNote>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Special check to see if a cell starts with a terminology entry</desc>
    </doc>
    <xsl:template name="cellContents">
      <xsl:choose>
	<xsl:when test="w:p/w:pPr/w:pStyle[@w:val='TermNum']">
	  <xsl:for-each-group select="w:p" group-by="1">
	    <xsl:call-template name="termsAndDefinitionsSection"/>
	  </xsl:for-each-group>
	</xsl:when>
	<xsl:when
	    test="w:p/w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]">
	  <xsl:for-each-group select="w:p" group-by="1">
	    <xsl:call-template name="termsAndDefinitionsSection"/>
	  </xsl:for-each-group>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates select="w:p" mode="inTable"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Definition Lists</desc>
    </doc>
    <xsl:template name="definitionListSection">
        <list type="gloss">
            <xsl:for-each-group select="current-group()" group-starting-with="w:p">
                <xsl:for-each-group select="current-group()/*"
                                group-starting-with="w:r[w:tab or w:sym[@w:font='Symbol']]">
                    <xsl:choose>
                        <xsl:when test="self::w:r[w:tab  or w:sym[@w:font='Symbol']]">
                            <item>
                                <xsl:for-each select="current-group()">
                                    <xsl:apply-templates select="."/>
                                </xsl:for-each>
                            </item>
                        </xsl:when>
                        <xsl:otherwise>
                            <label>
                                <xsl:for-each select="current-group()">
                                    <xsl:apply-templates select="."/>
                                </xsl:for-each>
                            </label>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each-group>
            </xsl:for-each-group>
        </list>
    </xsl:template>


          <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Dealing with references</desc>
	  </doc>
    <xsl:template name="bibliographySection">
        <listBibl>
	  <xsl:for-each select="current-group()">	      
	    <bibl>
	      <xsl:call-template name="parseReference"/>
	    </bibl>
	  </xsl:for-each>
        </listBibl>
    </xsl:template>


          <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>  See if paragraph is the main title of the document </desc>
	  </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzSTDTitle']" mode="paragraph">
        <!-- throw it away ; alternative is
            
            <head>
            <xsl:apply-templates select="descendant::w:t"/>
            </head>
        -->
    </xsl:template>


          <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Table of contents</desc>
	  </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzContents']"
		  mode="paragraph" priority="100">
      <div type="toc">
	<head>Contents</head>
	<divGen type="toc">
	  <xsl:for-each select="following-sibling::*[starts-with(w:pPr/w:pStyle/@w:val, 'toc')]//w:instrText">

	    <xsl:if test="starts-with(text(), ' TOC ')">
	      <xsl:processing-instruction name="isotoc">
		<xsl:value-of select="text()"/>
	      </xsl:processing-instruction>
	    </xsl:if>

	  </xsl:for-each>
	</divGen>
      </div>
    </xsl:template>
    <!-- 
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 9']"
        mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 1']"
        mode="paragraph"/>
    -->


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Dealing with examples</desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Example']" mode="paragraph">
        <note rend="Example">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Exampleparagraph']" mode="paragraph">
        <note rend="Exampleparagraph">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Examplenumbered']" mode="paragraph">
        <note rend="Examplenumbered">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Dealing with notes</desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note']" mode="paragraph">
        <note rend="Note">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note paragraph']" mode="paragraph">
      <note rend="Noteparagraph" >
	<xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note numbered']"
		  mode="paragraph">
        <note rend="Notenumbered">
	  <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Table note']"
		  mode="paragraph">
        <note rend="Tablenote">
	  <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Figure note']"
		  mode="paragraph">
        <note rend="Figurenote">
	  <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Figure footnote']"
		  mode="paragraph">
      <note rend="Figurefootnote">
	<xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note list']" mode="paragraph">
        <note rend="Notelist" >
	  <xsl:call-template name="process-checking-for-crossrefs"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableFootnote]">
        <note place="tablefoot">
            <xsl:attribute name="n">
                <xsl:value-of select="w:r[w:rPr/w:rStyle[@w:val='TableFootnoteXref']]/w:t"/>
	    </xsl:attribute>
            <xsl:apply-templates select="w:r[not(w:rPr/w:rStyle[@w:val='TableFootnoteXref'])]"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableNote]">
      <note place="inline">
	<xsl:call-template name="process-checking-for-crossrefs"/>		    
        </note>
    </xsl:template>

    <xsl:template match="w:p" mode="inTable">
      <xsl:choose>
	<xsl:when test="w:pPr/w:pStyle/@w:val=$TableFootnote">
	  <note place="tablefoot">
	    <xsl:attribute name="n">
	      <xsl:value-of select="w:r[w:rPr/w:rStyle[@w:val='TableFootnoteXref']]/w:t"/>
	    </xsl:attribute>
	    <xsl:apply-templates select="w:r[not(w:rPr/w:rStyle[@w:val='TableFootnoteXref'])]"/>
	  </note>
	</xsl:when>
	<xsl:when test="w:pPr/w:pStyle/@w:val=$TableNote">
	  <note place="inline">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
	  </note>
	</xsl:when>
	<xsl:otherwise>
	  <p rend="{w:pPr/w:pStyle/@w:val}">
	    <xsl:call-template name="process-checking-for-crossrefs"/>
	  </p>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Index1']">
      <p rend='Index1'>
	<xsl:call-template name="process-checking-for-crossrefs"/>		    
      </p>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='IndexHeading']">
      <p rend='IndexHeading'>
	<xsl:call-template name="process-checking-for-crossrefs"/>		    
      </p>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> Override handling of standard Word paragraphs </desc>
    </doc>

    <xsl:template name="paragraph-wp">
      <xsl:param name="style"/>
        <p>
            <!-- put style in rend, if there is a style -->
            <xsl:if test="w:pPr/w:pStyle/@w:val and tei:is-supported-style(w:pPr/w:pStyle/@w:val)">
                <xsl:attribute name="rend">
                    <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
                </xsl:attribute>
            </xsl:if>

            <!-- Store information about spacing  -->
            <xsl:for-each select="w:pPr/w:spacing">
	      <xsl:attribute name="iso:style">
		<xsl:if test="@w:before">
		  <xsl:text>margin-top:</xsl:text>
		  <xsl:value-of select="@w:before"/>
		  <xsl:text>;</xsl:text>
		</xsl:if>
		<xsl:if test="@w:after">
		  <xsl:text>margin-bottom:</xsl:text>
		  <xsl:value-of select="@w:after"/>
		  <xsl:text>;</xsl:text>
		</xsl:if>
	      </xsl:attribute>
	    </xsl:for-each>
	    <xsl:call-template name="process-checking-for-crossrefs"/>
        </p>
    </xsl:template>

    <!--  UTILITIES -->
    <xsl:template name="getSdt">
        <xsl:param name="tag"/>
        <xsl:param name="oldtag"/>
        <xsl:variable name="result1">
            <xsl:for-each select="key('Sdt',$tag)[1]">
                <xsl:value-of select="w:sdtContent/w:r/w:t/text()"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="result">
            <xsl:choose>
                <xsl:when test="string-length($result1)=0 and not($oldtag='')">
                    <xsl:for-each select="key('Sdt',$oldtag)[1]">
                        <xsl:value-of select="w:sdtContent/w:r/w:t/text()"/>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$result1"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="$result='Click here to enter text.'"> </xsl:when>
            <xsl:when test="$result='Choose an item.'"> </xsl:when>
            <!--
	    <xsl:when test="$tag='documentstage'">
	    <xsl:value-of select="substring-before(substring-after($result,'('),')')"/>
	    </xsl:when>
	-->
            <xsl:otherwise>
                <xsl:value-of select="$result"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> Deal with special case of text paragraphs in an SDT </desc></doc>
    <xsl:template match="w:sdt" mode="paragraph">
      <q type="sdt" iso:meta="{w:sdtPr/w:tag/@w:val}">
	        <xsl:for-each-group select="w:sdtContent/w:p"
                             group-adjacent="if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else   position() + 100">
	           <xsl:choose>
	              <xsl:when test="current-grouping-key()=1">
	                 <xsl:call-template name="listSection"/>
	              </xsl:when>
	              <!-- it is not a defined grouping .. apply templates -->
	    <xsl:otherwise>
	                 <xsl:apply-templates select="." mode="paragraph"/>
	              </xsl:otherwise>
	           </xsl:choose>
	        </xsl:for-each-group>
      </q>
    </xsl:template>

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> Para-level ins/del </desc></doc>
      <xsl:template match="w:p[w:pPr/w:rPr/w:ins]" mode="paragraph"
		    priority="42">
	  <addSpan spanTo="{generate-id()}"
		   when="{w:pPr/w:rPr/w:ins/@w:date}">
	    <xsl:call-template name="identifyChange">
	      <xsl:with-param name="who" select="w:pPr/w:rPr/w:ins/@w:author"/>
	    </xsl:call-template>
	  </addSpan>
	<xsl:apply-imports/>
	<anchor>
	  <xsl:attribute name="xml:id">
	    <xsl:value-of select="generate-id()"/>
	  </xsl:attribute>
	</anchor>
      </xsl:template>
      
      <xsl:template match="w:instrText" mode="paragraph" priority="42"/>
      <xsl:template match="w:instrText" priority="42"/>
      
      <xsl:template match="w:p[w:pPr/w:rPr/w:del]" mode="paragraph" priority="42">
	<delSpan spanTo="{generate-id()}" when="{w:pPr/w:rPr/w:del/@w:date}">
	  <xsl:call-template name="identifyChange">
	    <xsl:with-param name="who" select="w:pPr/w:rPr/w:del/@w:author"/>
	  </xsl:call-template>
	</delSpan>
	<xsl:apply-imports/>
	<anchor>
	  <xsl:attribute name="xml:id">
	    <xsl:value-of select="generate-id()"/>
	  </xsl:attribute>
	</anchor>
      </xsl:template>


   <xsl:template match="tei:p[starts-with(@rend,'termHeading')]" mode="group">
      <xsl:variable name="next" select="translate(@rend, '23456', '34567')"/>
      <div type="termHeading">
         <head>
	   <xsl:apply-templates/>
         </head>
         <xsl:for-each-group select="current-group() except ." group-starting-with="tei:p[@rend = $next]">
	           <xsl:choose>
	              <xsl:when test="self::tei:p[@rend=$next]">
	                 <xsl:apply-templates select="." mode="group"/>
	              </xsl:when>
	              <xsl:otherwise>
	                 <xsl:apply-templates select="current-group()" mode="pass2"/>
	              </xsl:otherwise>
	           </xsl:choose>
         </xsl:for-each-group>
      </div>
  </xsl:template>


  <xsl:template match="*" mode="group">
      <xsl:apply-templates select="." mode="pass2"/>
  </xsl:template>


    


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Check whether paragraph styles and block styles are
    supported; identify headings</desc>
    </doc>

    <xsl:template match="w:p|w:r" mode="pass0">
      <xsl:if test="w:pPr/w:pStyle/@w:val or w:rPr/w:rStyle/@w:val">
	<xsl:variable name="old">
	  <xsl:value-of select="w:pPr/w:pStyle/@w:val|w:rPr/w:rStyle/@w:val"/>
	</xsl:variable>
	<xsl:variable name="new">
	  <xsl:for-each select="document(concat($word-directory,'/word/styles.xml'),/)">
	    <xsl:value-of select="key('STYLES',$old)/w:name/@w:val"/>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:choose>
	  <xsl:when test="tei:is-supported-style($old)"/>
	  <xsl:when test="tei:is-supported-style($new)"/>
	  <xsl:otherwise>
	    <w:p>
	      <iso:error>
		<xsl:text>Word Style [</xsl:text>
		<xsl:value-of select="$old"/>
		<xsl:text>] on </xsl:text>
		<xsl:value-of select="name()"/>
		<xsl:text> not supported</xsl:text>
	      </iso:error>
	    </w:p>
	  </xsl:otherwise>
	</xsl:choose>
    </xsl:if>
    <xsl:copy>
      <xsl:if test="self::w:p">
	<xsl:if test="tei:is-heading(.)">
	  <xsl:attribute name="LEVEL"
			 select="tei:heading-level(.)"/>
	</xsl:if>
      </xsl:if>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass0"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="iso:error">
      <xsl:copy-of select="."/>
    </xsl:template>


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Handles w:pict by generating a copy, but also a comment</p>
      </desc>
   </doc>
    <xsl:template match="w:pict|w:object" mode="pass0">
      <iso:error>Deprecated  &lt;<xsl:value-of select="name()"/>&gt; found here</iso:error>
     <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template name="identifyChange">
      <xsl:param name="who"/>
      <xsl:variable name="W">
	<xsl:choose>
	  <xsl:when test="$who='Wanner Claude'">AMD.1</xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select="$who"/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="starts-with($W,'AMD.') or starts-with($W,'COR.')">
	    <xsl:attribute name="type">
	      <xsl:value-of select="substring-before($W,'.')"/>
	    </xsl:attribute>
	    <xsl:attribute name="n">
	      <xsl:value-of select="substring-after($W,'.')"/>
	    </xsl:attribute>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:attribute name="type">
	    <xsl:value-of select="translate($who,' ','_')"/>
	  </xsl:attribute>
	  </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="parseReference">
      <!-- see Test2010-13 for examples of references -->
      <!--<xsl:comment>PARSE <xsl:value-of select="."/></xsl:comment>-->
      <xsl:variable name="cheese">
	<xsl:apply-templates/>
      </xsl:variable>
      <xsl:for-each select="$cheese">
	<xsl:apply-templates mode="parseRef"/>
      </xsl:for-each>
    </xsl:template>

    <xsl:template match="*" mode="parseRef">
      <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="tei:seg[.=' ']" mode="parseRef">
      <xsl:text> </xsl:text>
    </xsl:template>

    <xsl:template match="text()" mode="parseRef">
      <idno type="orgref">
        <xsl:choose>
	        <xsl:when test="contains(.,',')">
	          <xsl:value-of select="substring-before(.,',')"/>
	        </xsl:when>
	        <xsl:otherwise>
	          <xsl:value-of select="."/>
	        </xsl:otherwise>
        </xsl:choose>
      </idno>
      <xsl:variable name="ISOPATT">
	<xsl:text>^</xsl:text>
	<xsl:text>(</xsl:text><!-- body -->
	<xsl:value-of select="$standardsBodies"/>
	<xsl:text>)</xsl:text>
	<xsl:text>[\s/]?</xsl:text>
	<xsl:text>(</xsl:text><!-- type -->
	<xsl:value-of select="$standardsTypes"/>
	<xsl:text>)?</xsl:text>
	<xsl:text>\s</xsl:text>
	<xsl:text>([0-9]+)</xsl:text><!-- document number -->
	<xsl:text>[-]?([0-9A-Z]+|[0-9]-[0-9]+| \(all parts\))?</xsl:text><!-- part(s) -->
	<xsl:text>((:)([0-9]{4}|[\-—]+))?</xsl:text><!-- year -->
	<xsl:text>(</xsl:text> <!-- start suppl -->
	<xsl:text>/</xsl:text>
	<xsl:text>(Cor|Amd|Add|Suppl)</xsl:text> <!-- suppl type -->
	<xsl:text>\.</xsl:text>
	<xsl:text>([0-9]+)?</xsl:text> <!-- suppl number -->
	<xsl:text>((:)([0-9]{4}))?</xsl:text>
	<xsl:text>)?</xsl:text> <!-- end suppl -->
	<xsl:text>(</xsl:text> <!-- start corr -->
	<xsl:text>/</xsl:text>
	<xsl:text>(Cor)</xsl:text>
	<xsl:text>\.</xsl:text>
	<xsl:text>([0-9]+)</xsl:text>
	<xsl:text>((:)([0-9]{4}))?</xsl:text>
	<xsl:text>)?</xsl:text> <!-- end corr -->
	<xsl:text>(,(\s)*(.*))?</xsl:text>
	<xsl:text>$</xsl:text>
      </xsl:variable>
      <xsl:choose>
	<xsl:when test="starts-with(.,'ISO') or starts-with(.,'IEC')
			or starts-with(.,'CIE')">
	      <!--docoriginator [[punctuation] doctype] "#" docnumber ["-"
		  partnumber] ":" pubyear ["/"suppltype "." supplnumber ":" pubyear]
		  ["/"suppltype "." supplnumber ":" pubyear] "," title ["/" suppltitle]
		  ["/" suppltitle] -->
	      <xsl:analyze-string
		  regex="{$ISOPATT}"
		  select="translate(.,'&#160;',' ')">
		<xsl:matching-substring>
		  <xsl:variable name="Part" select="regex-group(4)"/>
		  <xsl:variable name="Suppltype" select="regex-group(9)"/>
		  <xsl:variable name="Corrtype" select="regex-group(15)"/>
<!-- 
		      <xsl:message>
		      1 publisher :<xsl:value-of select="regex-group(1)"/>
		      2 documenttype :<xsl:value-of select="regex-group(2)"/>
		      3 docnumber  :<xsl:value-of select="regex-group(3)"/>
		      4 part :<xsl:value-of select="regex-group(4)"/>
		      5 edition :<xsl:value-of select="regex-group(7)"/>
		      6 suppltype :<xsl:value-of select="regex-group(9)"/>
		      7 supplnum :<xsl:value-of select="regex-group(10)"/>
		      8 suppllyear  :<xsl:value-of select="regex-group(13)"/>
		      9 corrtype :<xsl:value-of select="regex-group(15)"/>
		      10 corrnum :<xsl:value-of select="regex-group(16)"/>
		      11 corryear :<xsl:value-of select="regex-group(19)"/>
		      12 title :<xsl:value-of select="regex-group(22)"/>
		      </xsl:message>
-->
		  <publisher>
		    <xsl:value-of select="regex-group(1)"/>
		  </publisher>
		  
		  <xsl:if test="not(regex-group(2)='')">
		    <idno type="documentType">
		      <xsl:value-of select="regex-group(2)"/>
		    </idno>
		  </xsl:if>
		  
		  <idno type="docNumber">
		    <xsl:value-of select="regex-group(3)"/>
		  </idno>
		  
		  <xsl:choose>
		    <xsl:when test="$Part=''"/>
		    <xsl:when test="$Part=' (all parts)'">
		      <idno type="parts">(all parts)</idno>
		    </xsl:when>
		    <xsl:otherwise>
		      <idno type="docPartNumber">
			<xsl:value-of select="$Part"/>
		      </idno>
		    </xsl:otherwise>
		  </xsl:choose>

		  <xsl:choose>
		    <xsl:when test="regex-group(7)=''"/>
		    <xsl:otherwise>
		      <edition>
			<xsl:value-of select="regex-group(7)"/>
		      </edition>
		    </xsl:otherwise>
		  </xsl:choose>
		  <xsl:if test="not($Suppltype='')">
		    <idno type="supplType">
		      <xsl:value-of select="$Suppltype"/>
		    </idno>
		    <idno type="supplNumber">
		      <xsl:value-of select="regex-group(10)"/>
		    </idno>
		    <idno type="supplYear">
		      <xsl:value-of select="regex-group(13)"/>
		    </idno>
		  </xsl:if>
		  <xsl:if test="not($Corrtype='')">
		    <idno type="corrType">
		      <xsl:value-of select="$Corrtype"/>
		    </idno>
		    <idno type="corrNumber">
		      <xsl:value-of select="regex-group(16)"/>
		    </idno>
		    <xsl:if test="not(regex-group(19)='')">
		      <idno type="corrYear">
			<xsl:value-of select="regex-group(19)"/>
		      </idno>
		    </xsl:if>
		  </xsl:if>
		  <xsl:if test="not(regex-group(22)='')">
		    <title>
			<xsl:value-of select="regex-group(22)"/>
		    </title>
		  </xsl:if>
		</xsl:matching-substring>
		<xsl:non-matching-substring>
		  <xsl:if test="$debug='true'">
		    <xsl:message>
		      <xsl:text>Invalid format of reference </xsl:text>
		      <xsl:value-of select="."/>
		    </xsl:message>
		  </xsl:if>
		  <xsl:value-of select="."/>
		</xsl:non-matching-substring>
	      </xsl:analyze-string>	      
	</xsl:when>
	<xsl:when test=".='),'"/>
	<xsl:when test=".='), '"/>
	<xsl:when test=".=') '"/>
	<!--<xsl:when test=".=')'"/>-->
	<xsl:when test=".=','"/>
	<xsl:when test="starts-with(.,'), ')">
	  <title>
	    <xsl:value-of select="substring(.,4)"/>
	  </title>
	</xsl:when>
<!--	<xsl:when test="preceding-sibling::node()">-->
<!--	  <title>-->
<!--	    <xsl:value-of select="."/>-->
<!--	  </title>-->
<!--	</xsl:when>-->
	<xsl:otherwise>
	  <xsl:value-of select="."/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>    

 <xsl:template name="block-element">
     <xsl:param name="select"/>
     <xsl:param name="style"/>
     <xsl:param name="pPr" as="node()*"/>
     <xsl:param name="nop"/>
     <xsl:param name="bookmark-name"/>
     <xsl:param name="bookmark-id"/>
   </xsl:template>

   <xsl:template name="termNum"/>
 </xsl:stylesheet>

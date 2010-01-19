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
                version="2.0"
                exclude-result-prefixes="a pic rel ve o teidocx r m v wp w10 w wne mml vt cals tbx iso custprops">

    <!-- import conversion style -->
    <xsl:import href="../../../docx/utils/functions.xsl"/>
    <xsl:import href="../../../docx/from/from.xsl"/>

    <!-- import special iso functions -->
    <xsl:include href="iso-functions.xsl"/>

    <xsl:key name="WordTables" match="w:tbl" use="1"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p>TEI stylesheet to convert Word DOCX XML to TEI XML.</p>
         <p> This library is free software; you can redistribute it and/or modify it under the
            terms of the GNU Lesser General Public License as published by the Free Software
            Foundation; either version 2.1 of the License, or (at your option) any later version.
            This library is distributed in the hope that it will be useful, but WITHOUT ANY
            WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
            should have received a copy of the GNU Lesser General Public License along with this
            library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
            330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

    <xsl:key name="Sdt" match="w:sdt" use="w:sdtPr/w:tag/@w:val"/>
    <xsl:key name="AllSdt" match="w:sdtPr/w:tag/@w:val" use="1"/>

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
    <xsl:template match="w:p[.//w:sdt and not (w:pPr/w:pStyle/@w:val='zzSTDTitle')]"
                 priority="1001">

        <!--<xsl:message>fail 1: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='cover_warning']" priority="1002">
        <!--<xsl:message>fail 3: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzCopyright']" priority="1002">
        <!--<xsl:message>fail 4: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='idno']" priority="1002">
        <!--<xsl:message>fail 5: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='copyrightdetails']" priority="1002">
        <!--<xsl:message>fail 6: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

         <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	Overwriting the creation of the teiHeader.
	Construct the TEI Header either by copying the passed metadata or extracting
      the metadata from the document </desc>
	 </doc>

    <xsl:template name="create-tei-header">
        <xsl:attribute name="xml:lang">
            <xsl:variable name="l">
                <xsl:call-template name="getSdt">
                    <xsl:with-param name="tag">doclanguage</xsl:with-param>
                </xsl:call-template>
            </xsl:variable>
            <xsl:choose>
                <xsl:when test="$l='Russian'">ru</xsl:when>
                <xsl:when test="$l='French'">fr</xsl:when>
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
        <xsl:variable name="customProps"
                    select="document(concat($word-directory,'/docProps/custom.xml'))"/>
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
                    <p>Processed by ISOTEI on <xsl:value-of select="teidocx:whatsTheDate()"/>
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
      <desc>
	Construct the TEI Header either by copying the passed metadata or extracting
	the metadata from the document simple templates for the info
	that goes into the teiHeader
      </desc>
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
      <desc>
	Some specific section headers 
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

            <xsl:when test="$Style='a2' or                  $Style='a3' or                  $Style='a4' or                  $Style='a5' or                  $Style='a6'">
                <xsl:attribute name="type">annexSection</xsl:attribute>
                <head>
                    <xsl:apply-templates/>
                </head>
            </xsl:when>


            <xsl:otherwise>
                <xsl:attribute name="type">nohead</xsl:attribute>
                <xsl:if test="string(normalize-space($divname))">
                    <p>
                        <xsl:apply-templates/>
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
	     if (w:pPr/w:pStyle/@w:val='RefNorm') then 2 else
	     if (w:pPr/w:pStyle/@w:val='Definition') then 3 else
             if (w:pPr/w:pStyle/@w:val='Example') then 3 else
             if (w:pPr/w:pStyle/@w:val='TermNum') then 3 else
             if (w:pPr/w:pStyle/@w:val='nonVerbalRepresentation') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteDefinition') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteNonVerbalRepresentation') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteTerm') then 3 else
             if (w:pPr/w:pStyle/@w:val='entrySource') then 3 else
             if (w:pPr/w:pStyle/@w:val='noteTermEntry') then 3 else
             if (w:pPr/w:pStyle/@w:val='symbol') then 3 else
             if (w:pPr/w:pStyle/@w:val='termAdmitted') then 3 else
             if (w:pPr/w:pStyle/@w:val='termDeprecated') then 3 else
             if (w:pPr/w:pStyle/@w:val='termPreferred') then 3 else
             if (w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]) then 3 else
	     if (w:pPr/w:pStyle/@w:val=$BibliographyItem) then 4 else
	     if (w:pPr/w:pStyle/@w:val=$DefinitionList) then 5 else
	     if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 6 else
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

                <!-- it is not a defined grouping .. apply templates -->
                <xsl:otherwise>
                    <xsl:apply-templates select="." mode="paragraph"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each-group>
    </xsl:template>

            
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	override handling of runs 
      </desc>
    </doc>

    <xsl:template match="w:r/w:tab">
      <c rend="tab">
            <xsl:text>	</xsl:text>
        </c>
    </xsl:template>

    <xsl:template name="processTextrun">
        <xsl:variable name="style">
            <xsl:value-of select="w:rPr/w:rStyle/@w:val"/>
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

            <xsl:when test="$style='Hyperlink'">
                <ref>
		  <xsl:attribute name="target">
		    <xsl:for-each
			select="preceding-sibling::w:r[w:instrText][1]/w:instrText">
		      <xsl:value-of select="substring-before(substring-after(.,'&#x0022;'),'&#x0022;')"/>
		    </xsl:for-each>
		  </xsl:attribute>
                    <xsl:apply-templates/>
                </ref>
            </xsl:when>

            <xsl:when test="$style='RefNorm' and starts-with(.,'ISO')">
                <idno type="ISO">
                    <xsl:apply-templates/>
                </idno>
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

            <xsl:when test="$style='recommendation'">
                <seg iso:provision="recommendation">
                    <xsl:apply-templates/>
                </seg>
            </xsl:when>
	        <xsl:when test="$style = 'domain'        or $style = 'gender'        or $style = 'geographicalUse'        or $style = 'language'        or $style = 'partOfSpeech'        or $style = 'pronunciation'        or $style = 'source'        or $style = 'termRef'">
	           <hi rend="{$style}">
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

            <xsl:otherwise>
	      <xsl:call-template name="basicStyles"/>
            </xsl:otherwise>
        </xsl:choose>

    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
    Table titles.. we deal with them inside the table
      </desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]" mode="paragraph"/>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='TableTitle']" mode="paragraph"/>

     <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        When creating a CALS table, put in an attribute linking it to
	the original Word markup in an external file
    </desc>
   </doc>

	  <xsl:template match="cals:table" mode="innerTable">
	    <xsl:param name="n"  tunnel="yes"/>
	     <xsl:copy>
	        <xsl:copy-of select="@*"/>
		<xsl:attribute name="tei:corresp">
		  <xsl:text>media/table</xsl:text>
		  <xsl:value-of select="$n"/>
		  <xsl:text>.xml</xsl:text>
		</xsl:attribute>
	        <xsl:apply-templates mode="innerTable"/>	    
	     </xsl:copy>
	  </xsl:template>

     <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        For every table in Word, copy the contents to an external file.
      </desc>
     </doc>
	  <xsl:template name="fromDocxFinalHook">
	    <xsl:for-each select="key('WordTables',1)">
		<xsl:variable name="n">
		  <xsl:value-of select="$word-directory"/>
		  <xsl:text>/word/media/table</xsl:text>
		  <xsl:number level="any"/>
		  <xsl:text>.xml</xsl:text>
		</xsl:variable>
		<xsl:result-document href="{$n}">
		  <xsl:copy>
		    <xsl:copy-of select="@*"/>
		    <xsl:apply-templates mode="copytable"/>
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
	       w:left | 
	       w:pPr |
	       w:p |
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
      <desc>
        Working with figures
      </desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Figuretitle]" mode="paragraph">
        <head>
            <xsl:apply-templates/>
        </head>
    </xsl:template>
    <xsl:template name="figureSection">
        <figure>
            <xsl:for-each select="current-group()">
                <xsl:apply-templates select="." mode="paragraph"/>
            </xsl:for-each>
        </figure>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
        Dealing with Normative References
      </desc>
	</doc>
    <xsl:template name="normativeReferencesSection">
        <listBibl type="normativeReferences">
            <xsl:for-each select="current-group()">
                <xsl:choose>
                    <xsl:when test="starts-with(.,'ISO')">
                        <bibl>
                            <xsl:apply-templates/>
                        </bibl>
                    </xsl:when>
                    <xsl:otherwise>
                        <p>
                            <xsl:apply-templates/>
                        </p>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each>
        </listBibl>
    </xsl:template>


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	<p>Terms and definitions. The following styles are block-level
	objects in a term entry</p>
	<ul>
	  <li>TermNum</li>
	  <li>AutoTermNum</li>
	  <li>Example</li>
	  <li>symbol</li>
	  <li>termAdmitted</li>
	  <li>termDeprecated</li>
	  <li>termPreferred</li>
	  <li>abbreviatedForm</li>
	</ul>
      </desc>
    </doc>
    <xsl:template name="termsAndDefinitionsSection">
        <xsl:for-each-group select="current-group()"
                          group-starting-with="w:p[w:pPr/w:pStyle/@w:val='TermNum'      or      w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]]">
	        <xsl:choose>
	           <xsl:when test="not(self::w:p[w:pPr/w:pStyle/@w:val='TermNum'      or      w:pPr/w:pStyle[starts-with(@w:val,'autoTermNum')]])">
	              <iso:error>
			<xsl:text>Terminology entry here does not have
			a number style, but starts with with </xsl:text>
			<xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
		      </iso:error>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:variable name="Style">
	                 <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	              </xsl:variable>
	              <xsl:variable name="ID">
	                 <xsl:choose>
		                   <xsl:when test="$Style='TermNum'">
		                      <xsl:text>CDB_</xsl:text>
		                      <xsl:value-of select="."/>
		                   </xsl:when>
		                   <xsl:when test="starts-with($Style,'autoTermNum')">
		                      <xsl:value-of select="$Style"/>
		                      <xsl:text>_</xsl:text>
		                      <xsl:number level="any" count="w:p[w:pPr/w:pStyle/@w:val=$Style]"/>
		                   </xsl:when>
	                 </xsl:choose>
	              </xsl:variable>
               <termEntry xmlns="http://www.lisa.org/TBX-Specification.33.0.html" id="{$ID}">
	                 <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteTermEntry'] except .">
		                   <note>
		                      <xsl:apply-templates/>
		                   </note>
	                 </xsl:for-each>
	                 <descripGrp>
			   <descrip type="definition">
			     <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='Definition'] except .">
			       <xsl:apply-templates/>
			     </xsl:for-each>
			   </descrip>
			   <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='entrySource'] except .">
			     <admin type="entrySource">
			       <xsl:apply-templates/>
			     </admin>
			   </xsl:for-each>
	                 </descripGrp>
	                 <langSet>
		                   <xsl:attribute name="xml:lang">
		                      <xsl:choose>
		                         <xsl:when test="current-group()[w:r/w:rPr/w:lang]">
			                           <xsl:value-of select="subsequence(current-group()/w:r[w:rPr/w:lang][1]/w:rPr/w:lang/@w:val,1,1)"/>
		                         </xsl:when>
		                         <xsl:when test="w:pPr/w:rPr/w:lang">
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
			                        <xsl:when test="$Thing='TermNum'"/>
			                        <xsl:when test="$Thing='Definition'"/>
			                        <xsl:when test="$Thing='noteTermEntry'"/>
			                        <xsl:otherwise>
			                           <ntig>
			                              <termGrp>
			                                 <term>
				                                   <xsl:apply-templates/>
			                                 </term>
			                                 <termNote type="partOfSpeech">noun</termNote>
			                                 <termNote type="administrativeStatus">
				                                   <xsl:choose>
				                                      <xsl:when test="$Thing='termPreferred'">preferredTerm-admn-sts</xsl:when>
				                                      <xsl:when test="$Thing='termDeprecated'">deprecatedTerm-admn-sts</xsl:when>
				                                      <xsl:when test="$Thing='termAdmitted'">admittedTerm-admn-sts</xsl:when>
				                                      <xsl:when test="$Thing='symbol'">symbol-admn-sts</xsl:when>
				                                      <xsl:otherwise>UNKNOWN</xsl:otherwise>
				                                   </xsl:choose>
			                                 </termNote>
			                                 <xsl:if test="w:r/w:rPr/w:rStyle/@w:val='abbreviatedForm'">
				                                   <termNote type="termType">abbreviation</termNote>
			                                 </xsl:if>
			                                 <xsl:if test="w:r/w:rPr/w:rStyle/@w:val='gender'">
				                                   <termNote type="grammaticalGender">
				                                      <xsl:for-each select="w:r[w:rPr/w:rStyle/@w:val='gender']">
				                                         <xsl:choose>
				                                            <xsl:when test=".='m'">masculine</xsl:when>
				                                            <xsl:when test=".='f'">feminine</xsl:when>
				                                            <xsl:otherwise>otherGender</xsl:otherwise>
				                                         </xsl:choose>
				                                      </xsl:for-each>
				                                   </termNote>
			                                 </xsl:if>
			                              </termGrp>
			                           </ntig>
			                        </xsl:otherwise>
		                      </xsl:choose>
		                   </xsl:for-each>
		                </langSet>
	              </termEntry>
	           </xsl:otherwise>
	        </xsl:choose>
	     </xsl:for-each-group>
    </xsl:template>
    <xsl:template name="cellContents">
      <xsl:choose>
	        <xsl:when test="w:p/w:pPr/w:pStyle[@w:val='TermNum']">
	           <xsl:for-each-group select="w:p" group-by="1">
	              <xsl:call-template name="termsAndDefinitionsSection"/>
	           </xsl:for-each-group>
	        </xsl:when>
	        <xsl:when test="w:p/w:pPr/w:pStyle[starts-with(@w:val,'AutoTermNum')]">
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
      <desc>  
	Definition Lists
      </desc>
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
      <desc>  
        Dealing with Normative References
      </desc>
	  </doc>
    <xsl:template name="bibliographySection">
        <listBibl>
            <xsl:for-each select="current-group()">
                <bibl>
                    <xsl:apply-templates/>
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
      <desc>  
        Table of contents
      </desc>
	  </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzContents']" mode="paragraph" priority="100"/>
    <!-- 
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 9']"
        mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 1']"
        mode="paragraph"/>
    -->


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>  
        Dealing with examples
      </desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Example']" mode="paragraph">
        <note rend="Example">
            <xsl:apply-templates/>
        </note>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Exampleparagraph']" mode="paragraph">
        <note rend="Exampleparagraph">
            <xsl:apply-templates/>
        </note>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Examplenumbered']" mode="paragraph">
        <note rend="Examplenumbered">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>  
	Dealing with notes
      </desc>
    </doc>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note']" mode="paragraph">
        <note rend="Note">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note paragraph']" mode="paragraph">
        <note rend="Noteparagraph" >
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note numbered']"
		  mode="paragraph">
        <note rend="Notenumbered">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Table note']"
		  mode="paragraph">
        <note rend="Tablenote">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Figure note']"
		  mode="paragraph">
        <note rend="Figurenote">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note list']" mode="paragraph">
        <note rend="Notelist" >
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableFootnote]">
        <note place="foot">
            <xsl:attribute name="n">
                <xsl:value-of select="w:r[w:rPr/w:rStyle[@w:val='TableFootnoteXref']]/w:t"/>
	        </xsl:attribute>
            <xsl:apply-templates select="w:r[not(w:rPr/w:rStyle[@w:val='TableFootnoteXref'])]"/>
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableNote]">
        <note place="inline">
            <xsl:apply-templates/>
        </note>
    </xsl:template>

    <xsl:template match="w:p" mode="inTable">
      <xsl:choose>
	        <xsl:when test="w:pPr/w:pStyle/@w:val=$TableFootnote">
	           <note place="foot">
	              <xsl:attribute name="n">
	                 <xsl:value-of select="w:r[w:rPr/w:rStyle[@w:val='TableFootnoteXref']]/w:t"/>
	              </xsl:attribute>
	              <xsl:apply-templates select="w:r[not(w:rPr/w:rStyle[@w:val='TableFootnoteXref'])]"/>
	           </note>
	        </xsl:when>
	        <xsl:when test="w:pPr/w:pStyle/@w:val=$TableNote">
	           <note place="inline">
	              <xsl:apply-templates/>
	           </note>
	        </xsl:when>
	        <xsl:otherwise>
	           <p>
	              <xsl:apply-templates/>
	           </p>
	        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <!-- ******************************************************************************************* -->
    <!-- second stage processing -->

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
	<desc> Analyze numbers, marking them up to allow for decimal
	character changing </desc>
      </doc>
    <xsl:template match="text()" mode="part2">
        <xsl:choose>
            <xsl:when test="parent::tei:num">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:when test="parent::tei:seg[@rend='nonumber']">
                <xsl:value-of select="."/>
            </xsl:when>
            <!-- do not search for numbers inside math -->
            <xsl:when test="ancestor-or-self::m:t">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:when test="ancestor::mml:math">
                <xsl:value-of select="."/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:analyze-string select="." regex="((\d+ \d+)+,?\d*|\d+,\d+)">
                    <xsl:matching-substring>
                        <num>
                            <xsl:value-of select="regex-group(1)"/>
                        </num>
                    </xsl:matching-substring>
                    <xsl:non-matching-substring>
                        <xsl:value-of select="."/>
                    </xsl:non-matching-substring>
                </xsl:analyze-string>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> look at the sections we have generated, and put
        them in &lt;front&gt; or &lt;body&gt; as appropriate</desc></doc>
    <xsl:template match="tei:text" mode="part2">
        <text>
            <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
            </xsl:for-each>
            <front>
                <xsl:apply-templates select="tei:body/tei:div[@type='foreword']" mode="part2"/>
                <xsl:apply-templates select="tei:body/tei:div[@type='introduction']" mode="part2"/>
            </front>
            <body>
                <xsl:for-each select="tei:body">
                    <xsl:for-each select="tei:div|tei:p|tei:table|cals:table">
                        <xsl:choose>
                            <xsl:when test="self::tei:div[@type='foreword']"/>
                            <xsl:when test="self::tei:div[@type='introduction']"/>
                            <xsl:when test="self::tei:div[@type='bibliography']"/>
                            <xsl:when test="self::tei:div[@type='annex']"/>
                            <xsl:otherwise>
                                <xsl:apply-templates select="." mode="part2"/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:for-each>
                </xsl:for-each>
            </body>
            <back>
                <xsl:apply-templates select="tei:body/tei:div[@type='bibliography'                     or @type='annex']"
                                 mode="part2"/>
            </back>

            <!-- copy last milestone -->
            <xsl:apply-templates select="tei:body/tei:milestone[count(//tei:body/tei:milestone)]" mode="part2"/>
        </text>
    </xsl:template>

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> inner lists and notes in lists must be moved to inside items </desc></doc>
    <xsl:template match="tei:list/tei:list" mode="part2"/>
    <xsl:template match="tei:list/tei:note" mode="part2"/>

    <xsl:template match="tei:item" mode="part2">
        <item>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="me" select="generate-id()"/>
            <xsl:apply-templates mode="part2"/>
            <!-- find following sibling lists and notes -->
            <xsl:for-each select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <list>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
                </list>
            </xsl:for-each>
            <xsl:for-each select="following-sibling::tei:note[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <note>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part2"/>
                </note>
            </xsl:for-each>
        </item>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> Override handling of standard Word paragraphs </desc>
    </doc>

    <xsl:template name="paragraph-wp">
        <p>
            <!-- put style in rend, if there is a style -->
            <xsl:if test="w:pPr/w:pStyle/@w:val and teidocx:is-supported-style(w:pPr/w:pStyle/@w:val)">
                <xsl:attribute name="rend">
                    <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
                </xsl:attribute>
            </xsl:if>

            <!-- Store information about spacing  -->
            <xsl:if test="w:pPr/w:spacing/@w:before">
                <xsl:attribute name="iso:spaceBefore">
                    <xsl:value-of select="w:pPr/w:spacing/@w:before"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:if test="w:pPr/w:spacing/@w:after">
                <xsl:attribute name="iso:spaceAfter">
                    <xsl:value-of select="w:pPr/w:spacing/@w:after"/>
                </xsl:attribute>
            </xsl:if>

            <xsl:apply-templates select="."/>
        </p>
    </xsl:template>

    <!--  UTILITIES -->
    <xsl:template name="getSdt">
        <xsl:param name="tag"/>
        <xsl:param name="oldtag"/>
        <xsl:variable name="result1">
            <xsl:for-each select="key('Sdt',$tag)[1]">
                <xsl:value-of select="w:sdtContent/w:r/w:t"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="result">
            <xsl:choose>
                <xsl:when test="string-length($result1)=0 and not($oldtag='')">
                    <xsl:for-each select="key('Sdt',$oldtag)[1]">
                        <xsl:value-of select="w:sdtContent/w:r/w:t"/>
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


    <xsl:template name="extract-forme-work"/>

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
	<anchor xml:id="{generate-id()}"/>
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
	<anchor xml:id="{generate-id()}"/>
      </xsl:template>

    <!-- overrides for part 2 -->
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap &lt;div&gt; with head only </desc></doc>
    
    <xsl:template match="tei:div[count(*)=1 and tei:head]" mode="part2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;availability&gt; </desc></doc>
   
    <xsl:template match="tei:availability[not(*) or not(text())]"
		  mode="part2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;note&gt; </desc></doc>
   
    <xsl:template match="tei:note[not(*) and string-length(.)=0]"
		  mode="part2">
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap spurious page break </desc></doc>
    <xsl:template match="tei:body/tei:p[count(*)=1 and tei:pb]" mode="part2"/>

    <xsl:template match="w:bookmarkEnd" mode="part0"/>

    <xsl:template match="tei:div[@type='termsAndDefinitions']" mode="part2">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:variable name="name">
	  <xsl:text>termHeading2</xsl:text>
	</xsl:variable>
	<xsl:for-each-group select="*" group-starting-with="tei:p[@rend=$name]">
	  <xsl:choose>
	    <xsl:when test="self::tei:p[@rend=$name]">
	      <xsl:apply-templates select="." mode="group"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="current-group()" mode="part2"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
      </xsl:copy>
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
	                 <xsl:apply-templates select="current-group()" mode="part2"/>
	              </xsl:otherwise>
	           </xsl:choose>
         </xsl:for-each-group>
      </div>
  </xsl:template>


  <xsl:template match="*" mode="group">
      <xsl:apply-templates select="." mode="part2"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Merge adjacent &lt;hi&gt; </desc>
  </doc>

  <xsl:template match="tei:hi[@rend]" mode="part2">
    <xsl:variable name="r" select="@rend"/>
    <xsl:choose>
      <xsl:when test="preceding-sibling::node()[1][self::tei:hi[@rend=$r]]">
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="part2"/>
	  <xsl:call-template name="nextHi">
	    <xsl:with-param name="r" select="$r"/>
	  </xsl:call-template>
	</xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
   </xsl:template>

   <xsl:template name="nextHi">
      <xsl:param name="r"/>
      <xsl:for-each select="following-sibling::node()[1]">
         <xsl:if test="self::tei:hi[@rend=$r]">
            <xsl:apply-templates mode="part2"/>
            <xsl:call-template name="nextHi">
	              <xsl:with-param name="r" select="$r"/>
            </xsl:call-template>
         </xsl:if>
      </xsl:for-each>
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Merge adjacent &lt;termEntry&gt; with same ID</desc></doc>
   <xsl:template match="tbx:termEntry" mode="part2">
      <xsl:variable name="ID" select="@id"/>
      <xsl:choose>
         <xsl:when test="$ID=preceding-sibling::tbx:termEntry/@id"/>
         <xsl:when test="not(following-sibling::tbx:termEntry[@id=$ID])">
            <xsl:copy>
	              <xsl:copy-of select="@*"/>
	              <xsl:apply-templates mode="part2"/>
            </xsl:copy>
         </xsl:when>
         <xsl:otherwise>
            <xsl:copy>
	              <xsl:copy-of select="@*"/>
	              <xsl:for-each select="tbx:langSet">
	                 <xsl:copy>
	                    <xsl:copy-of select="@*"/>
	                    <xsl:apply-templates mode="part2" select="../tbx:note"/>
	                    <xsl:apply-templates mode="part2" select="../tbx:descripGrp"/>
	                    <xsl:apply-templates mode="part2"/>
	                 </xsl:copy>
	              </xsl:for-each>
	
	              <xsl:for-each select="following-sibling::tbx:termEntry[@id=$ID]/tbx:langSet">
	                 <xsl:copy>
	                    <xsl:copy-of select="@*"/>
	                    <xsl:apply-templates mode="part2" select="tbx:note"/>
	                    <xsl:apply-templates mode="part2" select="../tbx:descripGrp"/>
	                    <xsl:apply-templates mode="part2"/>
	                 </xsl:copy>
	              </xsl:for-each>
            </xsl:copy>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="tbx:descrip" mode="part2">
      <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:apply-templates mode="part2"/>
         <xsl:for-each select="ancestor::tbx:termEntry/following-sibling::*[1][self::tei:list]">
            <xsl:copy>
	              <xsl:copy-of select="@*"/>
	              <xsl:apply-templates mode="part2"/>
            </xsl:copy>
         </xsl:for-each>
      </xsl:copy>
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty &lt;list&gt; and one after termEntry </desc></doc>

   <xsl:template match="tei:list" mode="part2">
     <xsl:choose>
       <xsl:when test="preceding-sibling::*[1][self::tbx:termEntry]">
       </xsl:when>
       <xsl:when test="count(*)=0">
       </xsl:when>
       <xsl:otherwise>
	 <xsl:copy>
	   <xsl:copy-of select="@*"/>
	   <xsl:apply-templates mode="part2"/>
	 </xsl:copy>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <xsl:template match="tbx:term" mode="part2">
      <xsl:copy>
         <xsl:attribute name="id">
            <xsl:value-of select="ancestor::tbx:termEntry/@id"/>
            <xsl:text>-</xsl:text>
            <xsl:number level="any" from="tbx:termEntry"/>
         </xsl:attribute>
         <xsl:apply-templates mode="part2"/>
      </xsl:copy>
   </xsl:template>

   <!--
  <xsl:template match="tbx:descripGrp" mode="part2">
    <xsl:copy>
      <xsl:apply-templates select="tbx:descrip"/>
      <xsl:if test="tbx:descrip/tei:hi[@rend='source']">
	<termNote type="source">
	  <xsl:copy-of select="tbx:descrip/tei:hi[@rend='source']"/>
	</termNote>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tbx:descrip[@type='definition']/tei:source" mode="part2"/>
   -->

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Check whether paragraph styles and block styles are supported</desc>
    </doc>

    <xsl:template match="w:p|w:r" mode="part0">
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
	  <xsl:when test="teidocx:is-supported-style($old)"/>
	  <xsl:when test="teidocx:is-supported-style($new)"/>
	  <xsl:otherwise>
	    <w:p>
	      <iso:error>
		<xsl:text>Word Style </xsl:text>
		<xsl:value-of select="$old"/>
		<xsl:text> not supported</xsl:text>
	      </iso:error>
	    </w:p>
	  </xsl:otherwise>
	</xsl:choose>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="part0"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="w:noBreakHyphen">
    <xsl:text>&#2011;</xsl:text>
  </xsl:template>

  <xsl:template match="iso:error">
      <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="iso:error" mode="part2">
      <xsl:processing-instruction name="ISOerror">
	<xsl:value-of select="."/>
      </xsl:processing-instruction>
    </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Handles a w:object by generating an error</p>
      </desc>
   </doc>
    <xsl:template match="w:object">
      <iso:error>
	Invalid Word object found
      </iso:error>
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

</xsl:stylesheet>
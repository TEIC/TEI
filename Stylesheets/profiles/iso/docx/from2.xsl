<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
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
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    exclude-result-prefixes="a pic rel ve o teidocx r m v wp w10 w wne mml cals tbx iso xd">
    
    <!-- import conversion style -->
    <xsl:import href="../../../docx/utils/functions.xsl"/>
    <xsl:import href="../../../docx/from/from.xsl"/>
    
    <!-- import special iso functions -->
    <xsl:include href="iso-functions.xsl"/>
    
    <doc type="stylesheet" xmlns="http://www.pnp-software.com/XSLTdoc">
    <short>TEI stylesheet to convert Word DOCX XML to TEI XML.</short>
    <detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  
      </detail>
    <author>See AUTHORS</author>
    <cvsId>$Id: from.xsl 6894 2009-10-28 13:35:07Z rahtz $</cvsId>
    <copyright>2008, TEI Consortium</copyright>
    </doc>
    
    <xsl:key name="Sdt" match="w:sdt" use="w:sdtPr/w:tag/@w:val"/>
    <xsl:key name="AllSdt" match="w:sdtPr/w:tag/@w:val" use="1"/>

    <!-- param defining whether to use a custom metadata file or to extract
    the metadata from the document -->
    <xsl:param name="metadata-file"/>
    <xsl:param name="tableMethod">cals</xsl:param>    
    
    <!-- ignore existing title pages -->
    <xsl:template match="w:p[.//w:sdt and not (w:pPr/w:pStyle/@w:val='zzSTDTitle')]" priority="1001">
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
    <xsl:template
	match="w:p[w:pPr/w:pStyle/@w:val='copyrightdetails']" priority="1002">
      <!--<xsl:message>fail 6: <xsl:value-of select="normalize-space(.)"/></xsl:message>-->
    </xsl:template>

    <!-- Overwriting the creation of the teiHeader -->
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
        
        <!-- construct the TEI Header either by copying the passed metadata or extracting
            the metadata from the document -->
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
        <teiHeader xmlns:iso="http://www.iso.org/ns/1.0">
            <fileDesc>
                <titleStmt>
                    <title iso:meta="introductory_title" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">introductory_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="main_title" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">main_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="complementary_title" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">complementary_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    
                    <title iso:meta="introductory_title_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">introductory_title_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="main_title_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">main_title_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title iso:meta="complementary_title_fr" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">complementary_title_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    
                    <respStmt>
                        <resp>Committee</resp>
                        <name>
			  <xsl:text>TC </xsl:text>
                            <xsl:call-template name="getSdt">
                                <xsl:with-param name="tag">tcnum</xsl:with-param>
                            </xsl:call-template>
			  <xsl:text> /SC </xsl:text>
                            <xsl:call-template name="getSdt">
                                <xsl:with-param name="tag">scnum</xsl:with-param>
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
                    <date iso:meta="docdate">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">docdate</xsl:with-param>
                        </xsl:call-template>
                    </date>
                    <publisher iso:meta="organization">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">organization</xsl:with-param>
                        </xsl:call-template>
                    </publisher>
                    <authority iso:meta="secretariat">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">secretariat</xsl:with-param>
                        </xsl:call-template>
                    </authority>
		    <xsl:for-each-group select="key('AllSdt',1)"
					group-by=".">
		      <xsl:sort select="."/>
		      <xsl:variable name="thisSdt"
				    select="current-grouping-key()"/>
		      <xsl:choose>
			<xsl:when test="$thisSdt='committee'"/>
			<xsl:when test="$thisSdt='complementary_title'"/>
			<xsl:when test="$thisSdt='complementary_title_fr'"/>
			<xsl:when test="$thisSdt='introductory_title'"/>
			<xsl:when test="$thisSdt='introductory_title_fr'"/>
			<xsl:when test="$thisSdt='main_title'"/>
			<xsl:when test="$thisSdt='main_title_fr'"/>
			<xsl:when test="$thisSdt='docdate'"/>
			<xsl:when test="$thisSdt='organization'"/>
			<xsl:when test="$thisSdt='secretariat'"/>
			<xsl:otherwise>
			  <idno iso:meta="{$thisSdt}">
			    <xsl:call-template name="getSdt">
			      <xsl:with-param name="tag" select="$thisSdt"/>
			    </xsl:call-template>
			  </idno>
			</xsl:otherwise>
		      </xsl:choose>
		    </xsl:for-each-group>
                    <xsl:if test="w:body/w:p[w:pPr/w:pStyle/@w:val='zzCopyright']">
		      <availability>
			<xsl:apply-templates
			    select="w:body/w:p[w:pPr/w:pStyle/@w:val='zzCopyright']" mode="teiHeader"/>
		      </availability>
		    </xsl:if>
                    
		    <xsl:if test="w:body/w:p[w:pPr/w:pStyle/@w:val='cover_warning'
                            or w:pPr/w:pStyle/@w:val='coverWarning']">
		      <availability>
			<xsl:apply-templates
			    select="w:body/w:p[w:pPr/w:pStyle/@w:val='cover_warning'
				    or w:pPr/w:pStyle/@w:val='coverWarning']"
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
                            <xsl:with-param
                                name="tag">documenttype</xsl:with-param>
                            <xsl:with-param name="oldtag">doctype</xsl:with-param>
                        </xsl:call-template>
                    </classCode>
                    <classCode>
                        <xsl:attribute name="scheme">#SUPPLTYPE</xsl:attribute>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">documentsubtype</xsl:with-param>
                            <xsl:with-param name="oldtag">docsubtype</xsl:with-param>
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
    
    
    <!-- simple templates for the info that goes into the teiHeader -->
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
    
    <!-- some specific section headers -->
    <xsl:template name="generate-section-heading">
        <xsl:param name="Style"/>
        
        <xsl:variable name="divname">
            <xsl:for-each select=".//w:t">
                <xsl:if
                    test="string(number(translate(., '.', ''))) = 'NaN' and string(number(translate(translate(.,'A',''),'.','')))='NaN'">
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
            
            <xsl:when test="$Style='a2' or 
                $Style='a3' or 
                $Style='a4' or 
                $Style='a5' or 
                $Style='a6'">
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
    
    <!-- 
        We are now working on a group of all elements inside some group bounded by
        headings. These need to be further split up into smaller groups for figures,
        list etc. and into individual groups for simple paragraphs...
        
        Be careful about the position(), since it might be 1,2,3
    -->
    <xsl:template match="w:p" mode="inSectionGroup" priority="-100">
        <xsl:for-each-group select="current-group()"
            group-adjacent="if (contains(w:pPr/w:pStyle/@w:val,'Figure')) then 0 else 
            
            if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else
            if ((w:pPr/w:pStyle/@w:val='Note') and 
            (contains(preceding-sibling::w:p[1]/w:pPr/w:pStyle/@w:val,'List'))) then 1 else
            
            if (w:pPr/w:pStyle/@w:val='RefNorm') then 2 else
            
            if (w:pPr/w:pStyle/@w:val='TermNum') then 3 else
            if (w:pPr/w:pStyle[starts-with(@w:val,'AutoTermNum')]) then 3 else
            if (w:pPr/w:pStyle/@w:val='symbol') then 3 else
            if (w:pPr/w:pStyle/@w:val='termAdmitted') then 3 else
            if (w:pPr/w:pStyle/@w:val='termDeprecated') then 3 else
            if (w:pPr/w:pStyle/@w:val='termPreferred') then 3 else
            if (w:pPr/w:pStyle/@w:val='termRef') then 3 else
            if (w:pPr/w:pStyle/@w:val='Definition') then 3 else
            if (w:pPr/w:pStyle/@w:val='noteTermEntry') then 3 else
            
            if (w:pPr/w:pStyle/@w:val=$BibliographyItem) then 4 else
            
            if (w:pPr/w:pStyle/@w:val=$DefinitionList) then 5 else
            
            if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 6 else
            
            position() + 100">
            
            <!-- For each defined grouping call a specific template. If there is no
                grouping defined, apply templates with mode paragraph -->
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
    
    <!-- override handling of runs -->
    <xsl:template match="w:r/w:tab">
        <c rend="tab">
            <xsl:text>&#009;</xsl:text>
        </c>
    </xsl:template>

	<xsl:template match="w:r|w:ins">
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
			<xsl:when test="$style=$HeadingChar"/>
			<xsl:when test="$style=$HeadingCharFr"/>
			<xsl:when test="$style=$BibliographyReference"/>
			<xsl:when test="$style=$TableHeadingChar"/>
			<xsl:when test="$style='mentioned'">
				<mentioned>
					<xsl:apply-templates/>
				</mentioned>
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

			<xsl:when test="$style='orgName'">
				<orgName>
					<xsl:apply-templates/>
				</orgName>
			</xsl:when>
			<xsl:when test="$style='isonumber'">
				<num>
					<xsl:apply-templates/>
				</num>
			</xsl:when>
			
			<xsl:when test="$style='isononumber'">
				<seg rend='nonumber'>
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

			<xsl:when
			    test="w:rPr/w:position[number(@w:val)&lt;-2]">
				<hi rend="subscript">
					<xsl:apply-templates/>
				</hi>
			</xsl:when>

			<xsl:when
			    test="w:rPr/w:position[number(@w:val)&gt;2]">
				<hi rend="superscript">
					<xsl:apply-templates/>
				</hi>
			</xsl:when>

			<xsl:when test="w:rPr/w:vertAlign">
				<hi>
					<xsl:attribute name="rend">
						<xsl:value-of select="w:rPr/w:vertAlign/@w:val"/>
					</xsl:attribute>
					<xsl:apply-templates/>
				</hi>
			</xsl:when>

			<xsl:when test="w:rPr/w:i">
				<hi rend="italic">
					<xsl:apply-templates/>
				</hi>
			</xsl:when>

			<xsl:when test="w:rPr/w:b">
				<hi rend="bold">
					<xsl:apply-templates/>
				</hi>
			</xsl:when>
			<xsl:when test="$style='requirement'">
			  <seg iso:provision="requirement">
			    <xsl:apply-templates/>
			  </seg>
			</xsl:when>

			<xsl:when
			    test="$style='possibility_and_capability'">
			  <seg iso:provision="possibilityandcapability">
			    <xsl:apply-templates/>
			  </seg>
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

			<xsl:otherwise>
				<xsl:apply-templates/>
			</xsl:otherwise>
		</xsl:choose>

	</xsl:template>
	
    
	<!-- table titles.. we deal with them inside the table -->
	
	<xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]" mode="paragraph"/>
	
	<xsl:template match="w:p[w:pPr/w:pStyle/@w:val='TableTitle']" mode="paragraph"/>

    <!--
        Working with figures
    -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Figuretitle]"
		  mode="paragraph">
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
    
    <!-- 
        Dealing with Normative References
    -->
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
    
    
    <!-- 
        Terms and definitions
    -->
    <xsl:template name="termsAndDefinitionsSection">
      <xsl:for-each-group select="current-group()"
			  group-starting-with="w:p[w:pPr/w:pStyle/@w:val='TermNum'
					       or w:pPr/w:pStyle[starts-with(@w:val,'AutoTermNum')]]">
	<!--
	    symbol
	    termAdmitted
	    termDeprecated
	    termPreferred
	    termRef
	-->
	<tbx:termEntry id="{.}">
	  <tbx:descripGrp>
	    <tbx:descrip type="definition">
	      <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='Definition'] except .">
		<xsl:apply-templates/>
	      </xsl:for-each>
	    </tbx:descrip>
	    <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='noteTermEntry'] except .">
	      <tbx:note>
		<xsl:apply-templates/>
	      </tbx:note>
	    </xsl:for-each>
	  </tbx:descripGrp>
	  <tbx:langSet xml:lang="en">
	    <xsl:for-each  select="current-group()">
	      <xsl:variable name="Thing">
		<xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
	      </xsl:variable>
	      <xsl:choose>
		<xsl:when test="$Thing='TermNum'"/>
		<xsl:when test="w:pPr/w:pStyle/@w:val='Definition'"/>
		<xsl:when test="w:pPr/w:pStyle/@w:val='noteTermEntry'"/>
		<xsl:otherwise>
		  <tbx:ntig>
		    <tbx:termGrp>
		      <tbx:term id="{.}-{position()}">
			<xsl:apply-templates/>
		      </tbx:term>
		      <tbx:termNote type="partOfSpeech">noun</tbx:termNote>
		      <tbx:termNote type="administrativeStatus">
			<xsl:choose>
			  <xsl:when
			      test="$Thing='preferredTerm'">preferredTerm-admn-sts</xsl:when>
			  <xsl:otherwise>
			    <xsl:value-of select="$Thing"/>
			  </xsl:otherwise>
			</xsl:choose>
		      </tbx:termNote>
		    </tbx:termGrp>
		  </tbx:ntig>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	  </tbx:langSet>
	</tbx:termEntry>
      </xsl:for-each-group>
    </xsl:template>
    
    <!-- 
        Definition Lists
    -->
    <xsl:template name="definitionListSection">
        <list type="gloss">
            <xsl:for-each-group select="current-group()" group-starting-with="w:p">
                <xsl:for-each-group
                    select="current-group()/*"
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
    
    
    <!-- 
        Dealing with Normative References
    -->
    <xsl:template name="bibliographySection">
        <listBibl>
            <xsl:for-each select="current-group()">
                <bibl>
                    <xsl:apply-templates/>
                </bibl>
            </xsl:for-each>
        </listBibl>
    </xsl:template>
    
    
    <!-- If it is the main title of the document -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzSTDTitle']"
        mode="paragraph">
        <!-- throw it away ; alternative is
            
            <head>
            <xsl:apply-templates select="descendant::w:t"/>
            </head>
        -->
    </xsl:template>
    
    
    <!-- 
        Table of contents
    -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='zzContents']"
        mode="paragraph" priority="100"/>
    <!-- 
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 9']"
        mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='toc 1']"
        mode="paragraph"/>
    -->
    
    
    <!-- 
        Dealing with examples
    -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Example']" mode="paragraph">
        <note type="Example">
            <xsl:apply-templates/>
        </note>
    </xsl:template>
    
    <!-- 
        Dealing with notes
    -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Note']" mode="paragraph">
        <note>
            <xsl:apply-templates/>
        </note>
    </xsl:template>    
    
    <!-- notes -->
    <!--<xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Note]">
        <note>
            <xsl:call-template name="noteRend"/>
            <xsl:apply-templates/>
        </note>
    </xsl:template>
   
   <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Example']">
        <xsl:apply-templates select="." mode="paragraph"/>
    </xsl:template>    
    -->


    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableFootnote]">
        <note place="foot">
            <xsl:attribute name="n">
                <xsl:value-of
                    select="w:r[w:rPr/w:rStyle[@w:val='TableFootnoteXref']]/w:t"
                />
            </xsl:attribute>
            <xsl:apply-templates
                select="w:r[not(w:rPr/w:rStyle[@w:val='TableFootnoteXref'])]"
            />
        </note>
    </xsl:template>

    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableNote]">
        <note place="inline">
            <xsl:apply-templates/>
        </note>
    </xsl:template>
    
    
    <!-- ******************************************************************************************* -->
    <!-- second stage processing -->
    
    <!-- take care of numbers -->
    <xsl:template match="text()" mode="pass2">
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
                <xsl:analyze-string select="." regex="((\d+&#160;\d+)+,?\d*|\d+,\d+)">
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
    
    
    <!-- look at the sections we have generated, and put
        them in <front> or <body> as appropriate-->
    <xsl:template match="tei:text" mode="pass2">
        <text>
            <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
            </xsl:for-each>
            <front>
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='foreword']" mode="pass2"/>
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='introduction']" mode="pass2"/>
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
                            <xsl:apply-templates select="." mode="pass2"/>
                        </xsl:otherwise>
                    </xsl:choose>
		  </xsl:for-each>
		</xsl:for-each>
            </body>
            <back>
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='bibliography'
                    or @type='annex']" mode="pass2"/>
            </back>
            
            <!-- copy last milestone -->
            <xsl:apply-templates select="tei:body/tei:milestone[count(//tei:body/tei:milestone)]" mode="pass2"/>
        </text>
    </xsl:template>
    
    <!-- inner lists and notes in lists must be moved to inside items -->
    <xsl:template match="tei:list/tei:list" mode="pass2"/>
    <xsl:template match="tei:list/tei:note" mode="pass2"/>
    
    <xsl:template match="tei:item" mode="pass2">
        <item>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="me" select="generate-id()"/>
            <xsl:apply-templates mode="pass2"/>
            <!-- find following sibling lists and notes -->
            <xsl:for-each
                select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <list>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
                        mode="pass2"/>
                </list>
            </xsl:for-each>
            <xsl:for-each
                select="following-sibling::tei:note[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <note>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
                        mode="pass2"/>
                </note>
            </xsl:for-each>
        </item>
    </xsl:template>
    
    
    <xsl:template name="paragraph-wp">
    	<p>
	  <!-- put style in rend, if there is a style -->
	  <xsl:if test="w:pPr/w:pStyle/@w:val">
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
 
    <!-- override for part 2 -->

    <!-- div with head only -->
    
    <xsl:template match="tei:div[count(*)=1 and tei:head]" mode="pass2"/>

    <!-- spurious page break -->
    <xsl:template match="tei:body/tei:p[count(*)=1 and tei:pb]" mode="pass2"/>

</xsl:stylesheet>
<!-- 

for future ref , word styles which change when looked up by name:

! Bibliography0 ... CHANGED ...  Bibliography
! DefaultParagraphFont ... CHANGED ...  Default Paragraph Font
! ExampleHeadingChar ... CHANGED ...  Example Heading Char
! FigureHeadingChar ... CHANGED ...  Figure Heading Char
! FigureTitle0 ... CHANGED ...  Figure Title
! FootnoteReference ... CHANGED ...  footnote reference
! Heading1 ... CHANGED ...  heading 1

! Heading3 ... CHANGED ...  heading 3
! Heading4 ... CHANGED ...  heading 4
! List2 ... CHANGED ...  List 2
! ListBullet ... CHANGED ...  List Bullet
! ListNumber ... CHANGED ...  List Number
! ListNumber3 ... CHANGED ...  List Number 3
! NoteHeadingChar ... CHANGED ...  Note Heading Char
! PlaceholderText ... CHANGED ...  Placeholder Text
! TableHeadingChar ... CHANGED ...  Table Heading Char
! TableNoteHeadingChar ... CHANGED ...  TableNoteHeading Char
! TableTitle0 ... CHANGED ...  TableTitle
! Terms ... CHANGED ...  Term(s)
! committeeid ... CHANGED ...  committee_id
! copyrightdetails ... CHANGED ...  copyright_details
! docdetails ... CHANGED ...  doc_details
! refnumworking ... CHANGED ...  working_reference_number
-->

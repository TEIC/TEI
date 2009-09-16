<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:cals="http://www.oasis-open.org/specs/tm9901"
    xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    exclude-result-prefixes="ve o r m v wp w10 w wne mml cals tbx iso xd">
    
    <!-- import conversion style -->
    <xsl:import href="../../../docx/docx-tei.xsl"/>
    
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
    <cvsId>$Id$</cvsId>
    <copyright>2008, TEI Consortium</copyright>
</doc>
    <!-- param defining whether to use a custom metadata file or to extract
    the metadata from the document -->
    <xsl:param name="metadata-file"/>
    <xsl:param name="tableMethod">cals</xsl:param>    
    
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
            <xsl:when test="$metadata-file!=''">
                <xsl:call-template name="teiHeader-copy-from-metadata-file"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="teiHeader-extract-from-doc"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <xsl:template name="teiHeader-copy-from-metadata-file">
        <xsl:copy-of select="document($metadata-file)/tei:teiHeader"/>
    </xsl:template>
    
    <xsl:template name="teiHeader-extract-from-doc">
        <teiHeader>
            <fileDesc>
                <titleStmt>
                    <title type="introductory" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">introductory_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title type="main" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">main_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title type="complementary" xml:lang="en">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">complementary_title</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    
                    <title type="introductory" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">introductory_title_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title type="main" xml:lang="fr">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param
                                name="tag">main_title_fr</xsl:with-param>
                        </xsl:call-template>
                    </title>
                    <title type="complementary" xml:lang="fr">
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
                            <xsl:text>/SC </xsl:text>
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
                    <!-- where do we get the edition number from? 
                        "w:body/w:p/w:sdt/w:sdtPr[w:tag/@w:val='docstage']/w:dropDownList/w:listItem[@w:displayText=$displayText]/@w:value"/>
                        First <xsl:value-of select="substring-after(normalize-space(w:body/w:p/w:sdt[w:sdtPr/w:tag/@w:val='docstage']/w:sdtContent/w:r/w:t),' ')"/>-->
                </editionStmt>
                <publicationStmt>
                    <date>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">documentdate</xsl:with-param>
                            <xsl:with-param name="oldtag">docdate</xsl:with-param>
                        </xsl:call-template>
                    </date>
                    <publisher>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">organization</xsl:with-param>
                        </xsl:call-template>
                    </publisher>
                    <authority>
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">secretariat</xsl:with-param>
                        </xsl:call-template>
                    </authority>
                    <idno type="wgNumber">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">wgNumber</xsl:with-param>
                        </xsl:call-template>
                    </idno>
                    <idno type="serialNumber">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">serialNumber</xsl:with-param>
                        </xsl:call-template>
                    </idno>
                    <idno type="documentNumber">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">documentNumber</xsl:with-param>
                        </xsl:call-template>
                    </idno>
                    <idno type="partNumber">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">partNumber</xsl:with-param>
                        </xsl:call-template>
                    </idno>
                    <idno type="draftNumber">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">draftNumber</xsl:with-param>
                        </xsl:call-template>
                    </idno>
                    <idno type="stage">
                        <xsl:call-template name="getSdt">
                            <xsl:with-param name="tag">documentstage</xsl:with-param>
                            <xsl:with-param name="oldtag">docstage</xsl:with-param>
                        </xsl:call-template>
                    </idno>
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
                    <p>Processed by ISOTEI on <xsl:call-template name="whatsTheDate"/>
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
    <xsl:template match="w:p" mode="headings" priority="-100">
        <xsl:for-each-group select="current-group()"
            group-adjacent="if (contains(w:pPr/w:pStyle/@w:val,'Figure')) then 0 else 
            
            if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else
            if ((w:pPr/w:pStyle/@w:val='Note') and 
            (contains(preceding-sibling::w:p[1]/w:pPr/w:pStyle/@w:val,'List'))) then 1 else
            
            if (w:pPr/w:pStyle/@w:val='RefNorm') then 2 else
            
            if (w:pPr/w:pStyle/@w:val='TermNum') then 3 else
            if (w:pPr/w:pStyle/@w:val='Term(s)') then 3 else
            if (w:pPr/w:pStyle/@w:val='GlossText')	then 3 else
            if (w:pPr/w:pStyle/@w:val='Definition') then 3 else
            if ((w:pPr/w:pStyle/@w:val='Note') and 
            (preceding-sibling::w:p[w:pPr/w:pStyle/@w:val!='Note' or not(w:pPr/w:pStyle/@w:val)][1][w:pPr/w:pStyle/@w:val='TermNum'] or
            preceding-sibling::w:p[w:pPr/w:pStyle/@w:val!='Note' or not(w:pPr/w:pStyle/@w:val)][1][w:pPr/w:pStyle/@w:val='Terms'] or
            preceding-sibling::w:p[w:pPr/w:pStyle/@w:val!='Note' or not(w:pPr/w:pStyle/@w:val)][1][w:pPr/w:pStyle/@w:val='GlossText'] or
            preceding-sibling::w:p[w:pPr/w:pStyle/@w:val!='Note' or not(w:pPr/w:pStyle/@w:val)][1][w:pPr/w:pStyle/@w:val='Definition'])) then 3 else
            
            if (w:pPr/w:pStyle/@w:val=$BibliographyItem) then 4 else
            
            if (w:pPr/w:pStyle/@w:val=$DefinitionList) then 5 else
            
            if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 6 else
            
            position() + 100">
            
            <!-- For each defined grouping call a specific template. If there is no
                grouping defined, apply templates with mode paragraph -->
            <xsl:choose>
                <xsl:when test="current-grouping-key()=0">
                    <xsl:call-template name="figures"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=1">
                    <xsl:call-template name="lists"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=2">
                    <xsl:call-template name="normativeReferences"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=3">
                    <xsl:call-template name="termsAndDefinitions"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=4">
                    <xsl:call-template name="bibliography"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=5">
                    <xsl:call-template name="definitionLists"/>
                </xsl:when>
                <xsl:when test="current-grouping-key()=6">
                    <xsl:call-template name="toc"/>
                </xsl:when>
                
                <!-- it is not a defined grouping .. apply templates -->
                <xsl:otherwise>
                    <xsl:apply-templates select="." mode="paragraph"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each-group>
    </xsl:template>
    
    
    
    <!--
        Working with figures
    -->
    <xsl:template name="figures">
        <figure>
            <xsl:for-each select="current-group()">
                <xsl:apply-templates select="." mode="paragraph"/>
            </xsl:for-each>
        </figure>
    </xsl:template>    
    
    <!-- 
        Dealing with Normative References
    -->
    <xsl:template name="normativeReferences">
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
    <xsl:template name="termsAndDefinitions">
        <list type="termlist" rend="other">
            <xsl:for-each-group select="current-group()"
                group-starting-with="w:p[w:pPr/w:pStyle/@w:val='TermNum']">
                <!-- we have identified a term-->
                <item>
                    <xsl:attribute name="n">
                        <xsl:apply-templates/>
                    </xsl:attribute>
                    
                    <term>
                        <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='Term(s)'] except .">
                            <xsl:if test="position()>1">
                                <lb/>
                            </xsl:if>
                            <xsl:apply-templates/>
                        </xsl:for-each>
                    </term>
                    <gloss>
                        <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='GlossText'
                            or w:pPr/w:pStyle/@w:val='Definition'] except .">
                            <xsl:apply-templates/>
                        </xsl:for-each>
                    </gloss>
                    <!-- look out for notes -->
                    <xsl:for-each select="current-group()[w:pPr/w:pStyle/@w:val='Note'] except .">
                        <note>
                            <xsl:apply-templates/>
                        </note>
                    </xsl:for-each>
                </item>
            </xsl:for-each-group>
        </list>
    </xsl:template>
    
    <!-- 
        Definition Lists
    -->
    <xsl:template name="definitionLists">
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
    <xsl:template name="bibliography">
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
    
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$TableFootnote]">
        <note place="foot">
            <xsl:attribute name="n">
                <xsl:value-of
                    select="w:r[w:rPr/w:rStyle[@w:val='FigureFootnoteXref' or @w:val='TableFootnoteXref']]/w:t"
                />
            </xsl:attribute>
            <xsl:apply-templates
                select="w:r[not(w:rPr/w:rStyle[@w:val='FigureFootnoteXref' or @w:val='TableFootnoteXref'])]"
            />
        </note>
    </xsl:template>
    
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Example']">
        <xsl:apply-templates select="." mode="paragraph"/>
    </xsl:template>    -->
    
    <!-- ******************************************************************************************* -->
    <!-- second stage processing -->
    
    <!-- take care of numbers -->
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
    <xsl:template match="tei:text" mode="part2">
        <text>
            <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
            </xsl:for-each>
            <front>
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='foreword']" mode="part2"/>
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='introduction']" mode="part2"/>
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
                <xsl:apply-templates
                    select="tei:body/tei:div[@type='bibliography'
                    or @type='annex']" mode="part2"/>
            </back>
            
            <!-- copy last milestone -->
            <xsl:apply-templates select="tei:body/tei:milestone[count(//tei:body/tei:milestone)]" mode="part2"/>
        </text>
    </xsl:template>
    
    <!-- inner lists and notes in lists must be moved to inside items -->
    <xsl:template match="tei:list/tei:list" mode="part2"/>
    <xsl:template match="tei:list/tei:note" mode="part2"/>
    
    <xsl:template match="tei:item" mode="part2">
        <item>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="me" select="generate-id()"/>
            <xsl:apply-templates mode="part2"/>
            <!-- find following sibling lists and notes -->
            <xsl:for-each
                select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <list>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
                        mode="part2"/>
                </list>
            </xsl:for-each>
            <xsl:for-each
                select="following-sibling::tei:note[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <note>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
                        mode="part2"/>
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
            <xsl:when test="$tag='documentstage'">
                <xsl:value-of select="substring-before(substring-after($result,'('),')')"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$result"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
 
    <!-- override for part 2 -->

	<!-- div with head only -->

	<xsl:template match="tei:div[count(*)=1 and tei:head]" mode="part2">
	</xsl:template>
    
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
! Heading2 ... CHANGED ...  heading 2
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

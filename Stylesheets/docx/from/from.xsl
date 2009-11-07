<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
	xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
	xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
	xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/"
	xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:iso="http://www.iso.org/ns/1.0"
	xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
	xmlns:mml="http://www.w3.org/1998/Math/MathML"
	xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
	xmlns:mv="urn:schemas-microsoft-com:mac:vml" xmlns:o="urn:schemas-microsoft-com:office:office"
	xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
	xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
	xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
	xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
	xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
	xmlns:v="urn:schemas-microsoft-com:vml"
	xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
	xmlns:w10="urn:schemas-microsoft-com:office:word"
	xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
	xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
	xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
	xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns="http://www.tei-c.org/ns/1.0"
	exclude-result-prefixes="a cp dc dcterms dcmitype prop
					 iso m mml mo mv o pic r rel
					 tbx tei teidocx v xs ve w10 w wne wp xd">

	<xsl:import href="../utils/maths/omml2mml.xsl"/>
	<xsl:import href="../utils/functions.xsl"/>
	<xsl:import href="../utils/variables.xsl"/>
	<xsl:import href="../utils/identity/identity.xsl"/>
	<xsl:import href="parameters.xsl"/>

	<xsl:include href="part2/part2.xsl"/>
	<xsl:include href="part0/part0.xsl"/>
	
	<xsl:include href="dynamic/fields.xsl"/>
	<xsl:include href="dynamic/toc.xsl"/>
	<xsl:include href="graphics/graphics.xsl"/>
	<xsl:include href="lists/lists.xsl"/>
	<xsl:include href="marginals/marginals.xsl"/>
	<xsl:include href="maths/maths.xsl"/>
	<xsl:include href="paragraphs/paragraphs.xsl"/>
	<xsl:include href="tables/tables.xsl"/>
	<xsl:include href="templates/tei-templates.xsl"/>
	<xsl:include href="textruns/textruns.xsl"/>
	<xsl:include href="utils/utility-templates.xsl"/>
	<xsl:include href="wordsections/wordsections.xsl"/>
	
	
	<xd:doc type="stylesheet">
		<xd:short> TEI stylesheet for converting Word docx files to TEI </xd:short>
		<xd:detail> This library is free software; you can redistribute it and/or modify it under
			the terms of the GNU Lesser General Public License as published by the Free Software
			Foundation; either version 2.1 of the License, or (at your option) any later version.
			This library is distributed in the hope that it will be useful, but WITHOUT ANY
			WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
			PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You
			should have received a copy of the GNU Lesser General Public License along with this
			library; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite
			330, Boston, MA 02111-1307 USA </xd:detail>
		<xd:author>See AUTHORS</xd:author>
		<xd:cvsId>$Id$</xd:cvsId>
		<xd:copyright>2008, TEI Consortium</xd:copyright>
	</xd:doc>

	<xsl:variable name="processor">
		<xsl:value-of select="system-property('xsl:vendor')"/>
	</xsl:variable>
	<xsl:variable name="lowercase">abcdefghijklmnopqrstuvwxyz</xsl:variable>
	<xsl:variable name="uppercase">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
	<xsl:variable name="digits">1234567890</xsl:variable>
	<xsl:variable name="characters">~!@#$%^&amp;*()&lt;&gt;{}[]|:;,.?`'"=+-_</xsl:variable>


	<!-- 
		IMPORTING STYLESHEETS AND OVERRIDING MATCHED TEMPLATES:
		
		When importing a stylesheet (xsl:import) all the templates in the imported stylesheet
		get a lower import-precedence than the ones in the importing stylesheet. If the importing
		stylesheet now wants to override, let's say a general template to match all <w:p> elements
		where no more specialized rule applies it can't since it will automatically override
		all w:p[someprediceat] template in the imported stylesheet as well. 
		In this case we have outsourced the processing of the general template into a named template
		and all the imported stylesheet does is to call the named template. Now, the importing
		stylesheet can simply override the named template, and everything works out fine.
		
		See templates:
			- w:p (mode: paragraph)
	
		Modes:
			- part0:	
						a normalization process.
						
			- part2: 	
						templates that are relevant in the second stage of the conversion are 
						defined in mode "part2"
					
			- inSectionGroup:
						Defines a template that is working o a group of consecutive elements (w:ps or w:tbls)
						that form a section (a normal section not to be confused with w:sectPr).
						
			- paragraph:
						Defines that the template works on an individual element (usually starting with a
						w:p element).  

			- iden:
			
			
	-->


	<xsl:strip-space elements="*"/>
	<xsl:preserve-space elements="w:t"/>
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<xd:doc>
		<xd:short>The main template that starts the conversion from docx to TEI</xd:short>
		<xd:detail> The conversion process is split in two parts. Part one does the main conversion
			but does not necessarily create correct TEI. Part2 cleans up the mess produced by Part1
			and ensures the creation of valid TEI. </xd:detail>
	</xd:doc>
	<xsl:template match="/">
		<!-- Do an initial normalization and store everything in $part0 -->
		<xsl:variable name="part0">
		  <xsl:apply-templates mode="part0"/>
		</xsl:variable>

		<!-- Do the main transformation and store everything in the variable part1 -->
		<xsl:variable name="part1">
		  <xsl:for-each select="$part0">
		    <xsl:apply-templates/>
		  </xsl:for-each>
		</xsl:variable>
		  
		<!-- uncomment the following line to get the output from Part 1 -->
		<!--
		<xsl:copy-of select="$part1"/>
		-->

		<!-- Do the final parse and create valid TEI -->
		<xsl:apply-templates select="$part1" mode="part2"/>
	</xsl:template>

	<xd:doc>
		<xd:short>main document template</xd:short>
	</xd:doc>
	<xsl:template match="w:document">
		<TEI xmlns:iso="http://www.iso.org/ns/1.0">
			<!-- create teiHeader -->
			<xsl:call-template name="create-tei-header"/>

			<!-- convert main and back matter -->
			<xsl:apply-templates select="w:body"/>
		</TEI>
	</xsl:template>


	<xd:doc>
		<xd:short>create the basic text; worry later about dividing it up</xd:short>
	</xd:doc>
	<xsl:template match="w:body">
		<text>
			<!-- Create forme work -->
			<xsl:call-template name="extract-forme-work"/>

			<!-- create TEI body -->
			<body>
				<!-- 
					group all paragraphs that form a first level section.
				-->
				<xsl:for-each-group select="w:sdt|w:p|w:tbl"
					group-starting-with="w:p[teidocx:is-firstlevel-heading(.)]">

					<xsl:choose>
						
						<!-- We are dealing with a first level section, we now have
						to further divide the section into subsections that we can then
						finally work on -->
						<xsl:when test="teidocx:is-heading(.)">
							<xsl:call-template name="group-by-section"/>
						</xsl:when>
						
						<!-- We have found some loose paragraphs. These are most probably
						front matter paragraps. We can simply convert them without further
						trying to split them up into sub sections. -->
						<xsl:otherwise>
							<xsl:apply-templates select="." mode="inSectionGroup"/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each-group>
				
				<!-- I have no idea why I need this, but I apparently do. 
				//TODO: find out what is going on-->
				<xsl:apply-templates select="w:sectPr" mode="paragraph"/>
			</body>
		</text>
	</xsl:template>

	<xd:doc>
		<xd:short>Ignore bookmarks</xd:short>
		<xd:detail>
		There are certain elements, that we don't really care about, but that
		force us to regroup everything from the next sibling on.
		
		@see grouping in construction of headline outline.
		</xd:detail>
	</xd:doc>
	<xsl:template match="w:bookmarkStart|w:bookmarkEnd" mode="inSectionGroup">
		<xsl:for-each-group select="current-group() except ." group-adjacent="1">
			<xsl:apply-templates select="." mode="inSectionGroup"/>
		</xsl:for-each-group>
	</xsl:template>

	<xd:doc>
		<xd:short>Grouping consecutive elements that belong together</xd:short>
		<xd:detail>
		We are now working on a group of all elements inside some group bounded by
		headings. These need to be further split up into smaller groups for figures,
		list etc. and into individual groups for simple paragraphs...
		</xd:detail>
	</xd:doc>
	<xsl:template match="w:tbl|w:p" mode="inSectionGroup">

		<!-- 
			We are looking for:
				- Lists -> 1
				- Table of Contents -> 2
			Anything else is assigned a number of position()+100. This should be
			sufficient even if we find lots more things to group.
		-->
		<xsl:for-each-group select="current-group()"
			group-adjacent="if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else
							
							if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 2 else
							
							position() + 100">

			<!-- For each defined grouping call a specific template. If there is no
				grouping defined, apply templates with mode paragraph -->
			<xsl:choose>
				<xsl:when test="current-grouping-key()=1">
					<xsl:call-template name="listSection"/>
				</xsl:when>
				<xsl:when test="current-grouping-key()=2">
					<xsl:call-template name="tocSection"/>
				</xsl:when>


				<!-- it is not a defined grouping .. apply templates -->
				<xsl:otherwise>
					<xsl:apply-templates select="." mode="paragraph"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each-group>
	</xsl:template>


	<xd:doc>
		<xd:short>Groups the document by headings and thereby creating the document structure.</xd:short> 
	</xd:doc>
	<xsl:template name="group-by-section">
		<xsl:variable name="Style" select="w:pPr/w:pStyle/@w:val"/>
		<xsl:variable name="NextHeader" select="teidocx:get-nextlevel-header($Style)"/>
		<div>
			<!-- generate the head -->
			<xsl:call-template name="generate-section-heading">
				<xsl:with-param name="Style" select="$Style"/>
			</xsl:call-template>
			
			<!-- Process subheadings -->
			<xsl:for-each-group select="current-group() except ."
				group-starting-with="w:p[w:pPr/w:pStyle/@w:val=$NextHeader]">
				<xsl:choose>
					<xsl:when test="teidocx:is-heading(.)">
						<xsl:call-template name="group-by-section"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:apply-templates select="." mode="inSectionGroup"/>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each-group>
		</div>
	</xsl:template>
	

	<xd:doc>
		<xd:short>Looks through the document to find forme work related sections.</xd:short>
		<xd:detail>
			Creates a &lt;fw&gt; element for each forme work related section. These include
			running headers and footers. The corresponding elements in OOXML are w:headerReference
			and w:footerReference. These elements only define a reference that to a header or
			footer definition file. The reference itself is resolved in the file word/_rels/document.xml.rels.
		</xd:detail>
	</xd:doc>
	<xsl:template name="extract-forme-work">
		<xsl:for-each-group select="//w:headerReference|//w:footerReference" group-by="@r:id">
			<fw>
				<xsl:attribute name="xml:id">
					<xsl:value-of select="@r:id"/>
				</xsl:attribute>
				<xsl:attribute name="type">
					<xsl:choose>
						<xsl:when test="self::w:headerReference">header</xsl:when>
						<xsl:otherwise>footer</xsl:otherwise>
					</xsl:choose>
				</xsl:attribute>

				<xsl:variable name="rid" select="@r:id"/>
				<xsl:variable name="h-file">
					<xsl:value-of
						select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
					/>
				</xsl:variable>

				<!-- for the moment, just copy content -->
				<xsl:if test="doc-available(concat($word-directory,'/word/', $h-file))">
					<xsl:for-each-group
						select="document(concat($word-directory,'/word/', $h-file))/*[1]/w:*"
						group-adjacent="1">
						<xsl:apply-templates select="." mode="inSectionGroup"/>
					</xsl:for-each-group>
				</xsl:if>

			</fw>
		</xsl:for-each-group>

	</xsl:template>

<xsl:template match="w:hyperlink">
  <ptr target="{w:r/w:t}"/>
</xsl:template>

<xsl:template match="w:instrText">
<xsl:choose>
  <xsl:when test="contains(.,'REF _Ref')">
  </xsl:when>
  <xsl:otherwise>
    <xsl:value-of select="."/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>
</xsl:stylesheet>

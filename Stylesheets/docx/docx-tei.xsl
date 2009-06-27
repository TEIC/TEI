<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
		xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" 
		xmlns:dc="http://purl.org/dc/elements/1.1/" 
		xmlns:dcterms="http://purl.org/dc/terms/" 
		xmlns:dcmitype="http://purl.org/dc/dcmitype/"	
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
		xmlns:mml="http://www.w3.org/1998/Math/MathML"
		xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main" 
		xmlns:mv="urn:schemas-microsoft-com:mac:vml" 
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
		xmlns="http://www.tei-c.org/ns/1.0"
		exclude-result-prefixes="a cp dc dcterms dcmitype  iso m mml mo mv o pic r rel tbx tei teidocx v ve w10 w wne wp xd">
  
  <xsl:import href="omml2mml.xsl"/>
  <xsl:import href="tei-docx-functions.xsl"/>
  
  <xsl:variable name="convert-graphics">true</xsl:variable>
  <xsl:variable name="convert-headers">true</xsl:variable>
  
  <xsl:variable name="processor">
    <xsl:value-of select="system-property('xsl:vendor')"/>
  </xsl:variable>
  <xsl:variable name="lowercase">abcdefghijklmnopqrstuvwxyz</xsl:variable>
  <xsl:variable name="uppercase">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
  <xsl:variable name="digits">1234567890</xsl:variable>
  <xsl:variable name="characters">~!@#$%^&amp;*()&lt;&gt;{}[]|:;,.?`'"=+-_</xsl:variable>
  
  <xsl:param name="mathMethod">mml</xsl:param>
  <xsl:param name="termMethod">tei</xsl:param>
  <xsl:param name="tableMethod">tei</xsl:param>
  
  <xsl:param name="word-directory">.</xsl:param>
  
  
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet for converting Word docx files to TEI </xd:short>
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
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>
	
	
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
	
	-->
	
	
	<xsl:key name="Sdt" match="w:sdt" use="w:sdtPr/w:tag/@w:val"/>
	<xsl:key name="AllSdt" match="w:sdt" use="1"/>

	<xsl:strip-space elements="*"/>
	<xsl:preserve-space elements="w:t"/>
	
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

	<!-- create root element -->
	<xsl:template match="/">
		<xsl:variable name="part1">
			<xsl:apply-templates/>
		</xsl:variable>
		
		<!--
		<xsl:copy-of select="$part1"/>
		-->
		<xsl:apply-templates select="$part1" mode="part2"/>
	</xsl:template>

	<!-- main document template-->
	<xsl:template match="w:document">
		<TEI xmlns:iso="http://www.iso.org/ns/1.0">
			<!-- create teiHeader -->
			<xsl:call-template name="create-tei-header"/>

			<!-- convert main and back matter -->
			<xsl:apply-templates select="w:body"/>
		</TEI>
	</xsl:template>
	

	<!-- create the basic text; worry later about dividing it up -->
	<xsl:template match="w:body">
		<text>
		  <xsl:call-template name="extract-headers-and-footers"/>
		  
		<!-- 
		     look for headings of various kinds, from which to
		     generate sections
			-->
			<body>
			  <xsl:choose>
			    <xsl:when test="w:p[w:pPr/w:pStyle/@w:val='heading 1']">
			      <xsl:for-each-group select="w:p|w:tbl"
					group-starting-with="w:p[teidocx:is-firstlevel-heading(.)]">
					<xsl:choose>
						<xsl:when test="teidocx:is-heading(.)">
							<xsl:call-template name="group-headings"/>		
						</xsl:when>
						<xsl:otherwise>
							<xsl:apply-templates select="." mode="headings"/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each-group>
			    </xsl:when>
			    <xsl:otherwise>
			      <xsl:apply-templates select="w:p|w:tbl" mode="paragraph"/>
			    </xsl:otherwise>
			  </xsl:choose>
			  <xsl:apply-templates select="w:sectPr" mode="paragraph"/>
			</body>
		</text>
	</xsl:template>

	<!-- 
		There are certain elements, that we don't really care about, but that
		force us to regroup everything from the next sibling on.
		@see grouping in construction of headline outline.
	-->
	<xsl:template match="w:bookmarkStart|w:bookmarkEnd" mode="headings">
		<xsl:for-each-group select="current-group() except ." group-adjacent="1">
			<xsl:apply-templates select="." mode="headings"/>
		</xsl:for-each-group>
	</xsl:template>

	<!-- 
		We are now working on a group of all elements inside some group bounded by
		headings. These need to be further split up into smaller groups for figures,
		list etc. and into individual groups for simple paragraphs...
		
		Be careful about the position(), since it might be 1,2,3
	-->
	<xsl:template match="w:p|w:tbl" mode="headings">
		<!-- Trying to split everything up. A note adjacent to
		a list item is grouped with it-->
		<xsl:for-each-group select="current-group()"
			group-adjacent="if (contains(w:pPr/w:pStyle/@w:val,'List')) then 1 else
							
							if (starts-with(w:pPr/w:pStyle/@w:val,'toc')) then 2 else
							
							position() + 100">

			<!-- For each defined grouping call a specific template. If there is no
				grouping defined, apply templates with mode paragraph -->
			<xsl:choose>
				<xsl:when test="current-grouping-key()=1">
					<xsl:call-template name="lists"/>
				</xsl:when>
				<xsl:when test="current-grouping-key()=2">
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
		Footnotes
		-->
	<xsl:template match="w:footnoteReference">
		<note place="foot">
			<xsl:variable name="referenced-id" select="@w:id"/>
			<xsl:for-each select="document(concat($word-directory,'/word/footnotes.xml'))//w:footnote[@w:id=$referenced-id]">
				<xsl:apply-templates mode="paragraph"/>			
			</xsl:for-each>
		</note>
	</xsl:template>
	
	
	<!--
		Endnotes
	-->
	<xsl:template match="w:endnoteReference">
		<note place="end">
			<xsl:variable name="referenced-id" select="@w:id"/>
			<xsl:for-each select="document(concat($word-directory,'/word/endnotes.xml'))//w:endnote[@w:id=$referenced-id]">
				<xsl:apply-templates mode="paragraph"/>			
			</xsl:for-each>
		</note>
	</xsl:template>	

	<!-- 
		Handle Math 
		-->

	<xsl:template match="w:p[m:oMathPara]" mode="paragraph" priority="-1">
		<p>
			<formula>
				<xsl:apply-templates select="m:oMathPara/m:oMath"/>
			</formula>
		</p>
	</xsl:template>

	<xsl:template match="w:p[m:oMath]" mode="paragraph" priority="-1">
		<p>
			<formula>
				<xsl:apply-templates select="m:oMath"/>
			</formula>
		</p>
	</xsl:template>


	<!-- 
		See comment at begin of document to understand why this template is calling
		a named template.
		
		match strange word sections 
	-->
	<xsl:template match="w:p[w:pPr/w:sectPr]|w:sectPr" mode="paragraph">
		<xsl:call-template name="paragraph-sectpr"/>
	</xsl:template>

	<!-- 
		See comment at begin of document to understand why this template is calling
		a named template.
		
		This stylesheet is handling simple paragraphs that we know nothing else
		about.
		-->
	<xsl:template match="w:p" mode="paragraph">
		<xsl:call-template name="paragraph-wp"/>
	</xsl:template>
	

	<!-- handle graphics -->
	<xsl:template match="w:drawing">
		<xsl:choose>
			<xsl:when test="$convert-graphics='true'">
				<xsl:choose>
					<xsl:when test="descendant::a:blip[1]/@r:embed">
						<graphic>
							<xsl:attribute name="width" select="concat(number(descendant::wp:extent[1]/@cx) div 360000,'cm')"/>
							<xsl:attribute name="height" select="concat(number(descendant::wp:extent[1]/@cy) div 360000,'cm')"/>
							<xsl:attribute name="url">
								<xsl:variable name="rid" select="descendant::a:blip[1]/@r:embed"/>
								<xsl:value-of
									select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
								/>
							</xsl:attribute>
							
							<!-- inline or block -->
							<xsl:attribute name="rend">
								<xsl:choose>
									<xsl:when test="wp:anchor">block</xsl:when>
									<xsl:otherwise>inline</xsl:otherwise>
								</xsl:choose>
							</xsl:attribute>
							
						</graphic>
					</xsl:when>
					<xsl:otherwise>
						<xsl:message>Linked Image</xsl:message>
						<graphic>
							Linked Graphic: 
							<xsl:variable name="rid" select="@r:link"/>
							<xsl:value-of
								select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
							/>
						</graphic>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<w:drawing>
					<xsl:apply-templates mode="iden"/>
				</w:drawing>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="a:blip" mode="iden">
		<a:blip>
			<xsl:choose>
				<xsl:when test="@r:embed">
					<xsl:variable name="rid" select="@r:embed"/>
					<xsl:attribute name="r:embed">
						<xsl:value-of
							select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
						/>
					</xsl:attribute>
				</xsl:when>
				<xsl:otherwise>
					<xsl:variable name="rid" select="@r:link"/>
					<xsl:attribute name="r:link">
						<xsl:value-of
							select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
						/>
					</xsl:attribute>
				</xsl:otherwise>
			</xsl:choose>
			
		</a:blip>
	</xsl:template>

	<!-- Handle Text Runs -->
	<xsl:template match="w:del"/>
	
	

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

			<xsl:when test="w:rPr/w:position[@w:val='-6']">
				<hi rend="subscript">
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
	
	<!-- capture line breaks -->
	<xsl:template match="w:br">
		<xsl:choose>
			<xsl:when test="@w:type='page'">
				<pb/>
			</xsl:when>
			<xsl:otherwise>
				<lb/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- Word fields -->
	<xsl:template match="w:fldSimple">
		<xsl:choose>
			<!-- Page number -->
			<xsl:when test="contains(@w:instr,'PAGE')">
				<teidocx:dynamicContent type="pagenumber"/>
			</xsl:when>
		</xsl:choose>
		
	</xsl:template>

	<!-- Handle Text, Comments, Tabs, Symbols etc. -->
	<xsl:template match="w:t">
		<xsl:variable name="t">
			<xsl:choose>
				<xsl:when test="@xml:space='preserve' and string-length(normalize-space(.))=0">
					<seg>
						<xsl:value-of select="."/>
					</seg>
				</xsl:when>
				<xsl:when test="@xml:space='preserve'">
					<xsl:value-of select="."/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="normalize-space(.)"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:choose>
			<xsl:when test="parent::w:r/w:rPr/w:rFonts[starts-with(@w:ascii,'ISO')]">
				<seg iso:font="{parent::w:r/w:rPr/w:rFonts/@w:ascii}">
					<xsl:value-of select="$t"/>
				</seg>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="$t"/>
			</xsl:otherwise>

		</xsl:choose>
	</xsl:template>

	<xsl:template match="w:sym">
		<c iso:font="{@w:font}" n="{@w:char}"/>
	</xsl:template>

	<!-- handle tabs -->
	<xsl:template match="w:r/w:tab">
		<c rend="tab">
			<xsl:text>&#009;</xsl:text>
		</c>
	</xsl:template>

	<!-- handle ptabs (absolute position tab character) -->
	<xsl:template match="w:r/w:ptab">
		<c rend="ptab" type="{@w:alignment}">
			<xsl:text>&#009;</xsl:text>
		</c>
	</xsl:template>
	

	<!-- Handle Tables -->
	<xsl:template match="w:tbl" mode="paragraph">
	  <xsl:choose>
	    <xsl:when test="$tableMethod='word'">
	      <xsl:copy>
		<xsl:copy-of select="@*"/>
		<xsl:apply-templates/>
	      </xsl:copy>
	    </xsl:when>
	    <xsl:when test="$tableMethod='cals'">
	      <table xmlns="http://http://www.oasis-open.org/specs/tm9901">
		<xsl:call-template name="cals-table-header"/>
		<tgroup>
		  <tbody>
		    <xsl:for-each select="w:tr">
		      <row xmlns="http://http://www.oasis-open.org/specs/tm9901">
			<xsl:for-each select="w:tc">
			  <entry xmlns="http://http://www.oasis-open.org/specs/tm9901">
			    <xsl:if test="w:p/w:pPr/w:jc">
			      <xsl:attribute name="align">
				<xsl:value-of select="w:p[1]/w:pPr/w:jc/@w:val"/>
			      </xsl:attribute>
			    </xsl:if>
			    <xsl:if test="w:p[1]/w:pPr/w:pStyle">
			      <xsl:attribute name="rend">
				<xsl:value-of select="w:p[1]/w:pPr/w:pStyle/@w:val"/>
			      </xsl:attribute>
			    </xsl:if>
			    <xsl:if test="w:tcPr/w:gridSpan">
			      <xsl:attribute name="cols">
				<xsl:value-of select="w:tcPr/w:gridSpan/@w:val"/>
			      </xsl:attribute>
			    </xsl:if>
			    <xsl:apply-templates/>
			  </entry>
			</xsl:for-each>
		      </row>
		    </xsl:for-each>
		  </tbody>
		</tgroup>
	      </table>
	    </xsl:when>
	    <xsl:otherwise>
	      <table rend="rules">
		<xsl:call-template name="table-header"/>
		<xsl:for-each select="w:tr">
		  <row>
		    <xsl:for-each select="w:tc">
		      <cell>
			<xsl:if test="w:p/w:pPr/w:jc">
			  <xsl:attribute name="align">
			    <xsl:value-of select="w:p[1]/w:pPr/w:jc/@w:val"/>
			  </xsl:attribute>
			</xsl:if>
			<xsl:if test="w:p[1]/w:pPr/w:pStyle">
			  <xsl:attribute name="rend">
			    <xsl:value-of select="w:p[1]/w:pPr/w:pStyle/@w:val"/>
			  </xsl:attribute>
			</xsl:if>
			<xsl:if test="w:tcPr/w:gridSpan">
			  <xsl:attribute name="cols">
			    <xsl:value-of select="w:tcPr/w:gridSpan/@w:val"/>
			  </xsl:attribute>
			</xsl:if>
			<xsl:apply-templates/>
		      </cell>
		    </xsl:for-each>
		  </row>
		</xsl:for-each>
	      </table>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:template>

	<!-- table titles.. we deal with them inside the table -->

	<xsl:template match="w:p[w:pPr/w:pStyle/@w:val=$Tabletitle]" mode="paragraph"/>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='TableTitle']" mode="paragraph"/>

	<!-- getting the basic table structure -->

	<xsl:template
		match="w:gridCol|w:tblBorders|w:tblCellMar|w:tblGrid|w:tblLayout|w:tblLook|w:tblPr|w:tblW|w:tc|w:tcBorders|w:tcPr|w:tcW|w:tr|w:trPr|w:vAlign|w:top|w:left|w:bottom|w:right|w:insideH|w:insideV">

		<xsl:choose>
			<xsl:when test="$tableMethod='word'">
				<xsl:copy>
					<xsl:copy-of select="@*"/>
					<xsl:apply-templates/>
				</xsl:copy>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- identity transform -->

	<xsl:template match="@*|text()|comment()|processing-instruction()" mode="iden">
		<xsl:copy-of select="."/>
	</xsl:template>

	<xsl:template match="*" mode="iden">
	  <xsl:element name="{name()}">
	    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="iden"/>
	  </xsl:element>
	</xsl:template>


	<!-- Work with maths -->

	<xsl:template match="w:p[w:pPr/w:pStyle/@w:val='Formula']" mode="paragraph">
		<xsl:call-template name="paragraph-formula"/>
	</xsl:template>

	<xsl:template match="w:object">
	  <iso:wordObject>
	    <xsl:apply-templates select="." mode="iden"/>
	  </iso:wordObject>
	</xsl:template>
	
	<xsl:template match="v:imagedata" mode="iden">
		<v:imagedata>
			<xsl:variable name="rid" select="@r:id"/>
			<xsl:attribute name="r:id">
				<xsl:value-of
					select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
				/>
			</xsl:attribute>
		</v:imagedata>
	</xsl:template>
	
	<xsl:template match="o:OLEObject" mode="iden">
		<o:OLEObject>
			<xsl:copy-of select="@*"/>
			<xsl:variable name="rid" select="@r:id"/>
			<xsl:attribute name="r:id">
				<xsl:value-of
					select="document(concat($word-directory,'/word/_rels/document.xml.rels'))//rel:Relationship[@Id=$rid]/@Target"
				/>
			</xsl:attribute>
		</o:OLEObject>
	</xsl:template>
	

	<xsl:template match="m:oMath">
		<xsl:choose>
			<xsl:when test="$mathMethod='omml'">
				<xsl:apply-templates select="." mode="iden"/>
			</xsl:when>
			<xsl:otherwise>
				<mml:math>
					<xsl:apply-templates/>
				</mml:math>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

<!-- ******************************************************************************************* -->
	<!-- second stage processing -->
	<xsl:template match="@*|comment()|processing-instruction()" mode="part2">
		<xsl:copy-of select="."/>
	</xsl:template>

	<xsl:template match="*" mode="part2">
		<xsl:copy>
			<xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
				mode="part2"/>
		</xsl:copy>
	</xsl:template>


	<xsl:template match="text()" mode="part2">
		<xsl:value-of select="."/>
	</xsl:template>

	<!-- zap empty hi, p and item -->
	<xsl:template match="tei:hi[not(*) and string-length(.)=0]" mode="part2"/>
	<xsl:template match="tei:item[not(*) and string-length(.)=0]" mode="part2"/>
	<xsl:template match="tei:p[not(*) and string-length(.)=0]" mode="part2"/>

	<!-- inner lists in lists must be moved to inside items -->
	<xsl:template match="tei:list/tei:list" mode="part2"/>
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
		</item>
	</xsl:template>

	<!-- bold emdash in title, forget it -->
	<xsl:template match="tei:head/tei:hi[.=' ']" mode="part2"/>

	<!-- zap emdashes at start of head -->
	<xsl:template match="tei:head/text()" mode="part2">
		<xsl:choose>
			<xsl:when test="starts-with(.,'— ')">
				<xsl:value-of select="substring(.,3)"/>
			</xsl:when>
			<xsl:when test="starts-with(.,' — ')">
				<xsl:value-of select="substring(.,4)"/>
			</xsl:when>
			<xsl:when test="starts-with(.,' — ')">
				<xsl:value-of select="substring(.,4)"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="."/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- forget boldness in figure drawing -->
	<!--<xsl:template match="tei:p[@rend='FigureSpecial']/tei:hi" mode="part2">
		<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="part2"/>
	</xsl:template>-->


	<!-- a <seg> which does nothing isnt worth having -->
	<xsl:template match="tei:seg[not(@*)]" mode="part2">
		<xsl:choose>
		        <xsl:when test="parent::tei:formula and normalize-space(.)=''"/>
			<xsl:when test="parent::*/text()">
				<xsl:value-of select="."/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="."/>
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
	    <body>
	      <xsl:for-each select="tei:body/tei:*">
		<xsl:apply-templates select="." mode="part2"/>
	      </xsl:for-each>
	    </body>
	  </text>
	</xsl:template>

	<!-- a <p> inside a listBibl is moved out-->
	<xsl:template match="tei:listBibl/tei:p" mode="part2"/>

	<xsl:template match="tei:listBibl" mode="part2">
	  <xsl:for-each select="tei:p">
	    <p>
	      <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
				 mode="part2"/>
	    </p>
	  </xsl:for-each>
	  <listBibl>
	    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"
				 mode="part2"/>
	  </listBibl>
	</xsl:template>
	
	<!-- a <tab> (in a hi)? in a gloss list -->
	<xsl:template
	    match="tei:list[@type='gloss']/tei:item/tei:hi[tei:c[@rend='tab']]"
	    mode="part2"/>
	<xsl:template
	    match="tei:list[@type='gloss']/tei:item/tei:c[@rend='tab']"
	    mode="part2"/>

	<!-- top of a weird gloss list -->
	<xsl:template
	    match="tei:list[@type='gloss']/tei:label[.='where']"
	    mode="part2">
	  <head>
	    <xsl:apply-templates/>
	  </head>
	</xsl:template>

	<!-- a <tab> in a <bibl>? no. -->
	<xsl:template match="tei:bibl/tei:c[@rend='tab']"
		      mode="part2"/>

	<!-- a <tab> in a <gloss>? no. -->
	<xsl:template match="tei:gloss//tei:c[@rend='tab']"
		mode="part2"/>
	
	
	<!-- a <tab> in a <formula>? no. -->
	<xsl:template match="tei:formula//tei:c[@rend='tab']"
		mode="part2"/>
	

	<!-- a bold line break??? -->
	<xsl:template match="tei:hi[count(*)=1 and not(text()) and tei:lb]"
		      mode="part2">
	  <tei:lb/>
	</xsl:template>


	<!-- a <tab> in a <head>? no. -->
	<xsl:template match="tei:head/tei:c[@rend='tab']"
		      mode="part2"/>


    <!--
        Groups the document by headings and thereby creating the document structure. 
    -->
    <xsl:template name="group-headings">
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
                        <xsl:call-template name="group-headings"/>		
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:apply-templates select="." mode="headings"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:for-each-group>
        </div>
    </xsl:template>
    
    <!-- generates a section heading. If you need something specific, feel free
        to overwrite this template -->
    <xsl:template name="generate-section-heading">
        <xsl:param name="Style"/>
        
        <head>
            <xsl:apply-templates/>
        </head>
    </xsl:template>
    
    <xsl:template name="extract-headers-and-footers">
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
                    <xsl:for-each-group select="document(concat($word-directory,'/word/', $h-file))/*[1]/w:*" group-adjacent="1">
                        <xsl:apply-templates select="." mode="headings"/>					
                    </xsl:for-each-group>
                </xsl:if>
                
            </fw>
        </xsl:for-each-group>
        
    </xsl:template>
    
    
    <!-- simple teiHeader. For a more sophisticated header, think about overwriting
        this template -->
    <xsl:template name="create-tei-header">
        <teiHeader>
            <fileDesc>
                <titleStmt>
                    <title>
                        <xsl:call-template name="getDocTitle"/>
                    </title>
                    <author>
                        <xsl:call-template name="getDocAuthor"/>
                    </author>
                </titleStmt>
                <editionStmt>
                    <edition>
                        <date>
                            <xsl:call-template name="getDocDate"/>
                        </date>
                    </edition>
                </editionStmt>
                <publicationStmt>
                    <p></p>
                </publicationStmt>
                <sourceDesc>
                    <p>Converted from a Word document </p>
                </sourceDesc>
            </fileDesc>
	    <encodingDesc>
	      <appInfo>
		<application ident="docxtotei">
		  <label>$Id$</label>
		</application>
	      </appInfo> 
	    </encodingDesc>
            <revisionDesc>
                <change>
                    <date>
                        <xsl:text>$LastChangedDate: </xsl:text>
                        <xsl:call-template name="whatsTheDate"/>
                        <xsl:text>$</xsl:text>
                    </date>
                    <respStmt>
                        <name>$LastChangedBy$</name>
                    </respStmt>
                    <item>$LastChangedRevision$</item>
                </change>
            </revisionDesc>
        </teiHeader>
    </xsl:template>
    
    
    
    <!-- 
        This template handles lists and takes care of nested lists.
    -->
    <xsl:template name="lists">
        <xsl:variable name="level">
            <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
        </xsl:variable>
        <list>
            <xsl:call-template name="listType"/>
            
            <!-- Notes should be handled by a specific ISO handler -->
            <xsl:for-each-group select="current-group()"
                group-adjacent="if(w:pPr/w:pStyle/@w:val=$level) then 0 else
                if(w:pPr/w:pStyle/@w:val='Note') then 0
                else 1">
                <xsl:choose>
                    <!-- we are still on the same level -->
                    <xsl:when test="current-grouping-key()=0">
                        <xsl:for-each select="current-group()">
                            <!-- put items and notes as siblings  for now -->
                            <xsl:choose>
                                <xsl:when test="contains(w:pPr/w:pStyle/@w:val,'List')">
                                    <item>
                                        <xsl:apply-templates/>
                                    </item>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:apply-templates select="." mode="paragraph"/>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="lists"/>
                    </xsl:otherwise>
                </xsl:choose>
                
            </xsl:for-each-group>
        </list>
    </xsl:template>
    
    <!-- 
        named Template for w:p
    -->
    <xsl:template name="paragraph-wp">
        <p>
            <!-- put style in rend, if there is a style -->
            <xsl:if test="w:pPr/w:pStyle/@w:val">
                <xsl:attribute name="rend">
                    <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
                </xsl:attribute>
            </xsl:if>
            
            <xsl:apply-templates select="."/>
        </p>
    </xsl:template>
    
    <!-- 
        named template for w:p[w:pPr/w:pStyle/@w:val='Formula']
    -->
    <xsl:template name="paragraph-formula">
        <p>
            <formula>
                <xsl:if test="w:r/w:rPr/w:rStyle/@w:val='FormulaReference'">
                    <xsl:attribute name="n">
                        <xsl:value-of
                            select="w:r[w:rPr/w:rStyle/@w:val='FormulaReference']/w:t" />
                    </xsl:attribute>
                </xsl:if>
                <xsl:apply-templates/>
            </formula>
        </p>
    </xsl:template>
    
    <!--
        template for w:p[w:pPr/w:sectPr]|w:sectPr
        -->
    <xsl:template name="paragraph-sectpr">
        <xsl:for-each select="descendant-or-self::w:sectPr">
            <milestone unit="section">
                <xsl:for-each select="w:headerReference">
                    <teidocx:header type="{@w:type}" ref="{@r:id}"/> 
                </xsl:for-each>
                <xsl:for-each select="w:footerReference">
                    <teidocx:footer type="{@w:type}" ref="{@r:id}"/> 
                </xsl:for-each>
                <xsl:if test="w:pgSz/@w:orient='landscape'">
                    <teidocx:orientation type="landscape"/>
                </xsl:if>
                <xsl:if test="w:pgNumType">
                    <teidocx:pageNumbering>
                        <xsl:if test="w:pgNumType/@w:start">
                            <xsl:attribute name="start" select="w:pgNumType/@w:start"/>
                        </xsl:if>
                        <xsl:if test="w:pgNumType/@w:fmt">
                            <xsl:attribute name="type" select="w:pgNumType/@w:fmt"/>
                        </xsl:if>
                    </teidocx:pageNumbering>
                </xsl:if>
            </milestone>
        </xsl:for-each>
        
        <xsl:next-match/>
    </xsl:template>
    
    <xsl:template name="table-header">
        <xsl:variable name="precedingTableTitle" select="preceding-sibling::w:p[w:pPr/w:pStyle/@w:val='TableTitle'
            or w:pPr/w:pStyle/@w:val=$Tabletitle][1]"/>
        <xsl:if test="$precedingTableTitle and $precedingTableTitle/following-sibling::w:tbl[1] and generate-id()=generate-id($precedingTableTitle/following-sibling::w:tbl[1])">
            <head>
                <xsl:apply-templates select="$precedingTableTitle"/>
            </head>
        </xsl:if>
    </xsl:template>

    <xsl:template name="cals-table-header">
        <xsl:variable name="precedingTableTitle" select="preceding-sibling::w:p[w:pPr/w:pStyle/@w:val='TableTitle'
            or w:pPr/w:pStyle/@w:val=$Tabletitle][1]"/>
        <xsl:if test="$precedingTableTitle and $precedingTableTitle/following-sibling::w:tbl[1] and generate-id()=generate-id($precedingTableTitle/following-sibling::w:tbl[1])">
            <title>
                <xsl:apply-templates select="$precedingTableTitle"/>
            </title>
        </xsl:if>
    </xsl:template>
    
    
    <!--
        Trying to figure out the style of a list.
    -->
    <xsl:template name="listType">
        <xsl:variable name="style">
            <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
        </xsl:variable>
        <xsl:variable name="type" select="teidocx:get-listtype($style)"/>
        
        <xsl:attribute name="type">
            <xsl:choose>
                <xsl:when test="string-length($type) &gt; 0">
                    <xsl:value-of select="$type"/>
                </xsl:when>
                
                <!-- try to figure it out by looking at the corresponding numbering file -->
                <xsl:otherwise>
                    
                    <!-- look up the numbering definition .. either in document.xml or in styles.xml  -->
                    <xsl:variable name="numbering-def">
                        <xsl:choose>
                            <xsl:when test="w:pPr/w:numPr/w:numId/@w:val">
                                <xsl:value-of select="w:pPr/w:numPr/w:numId/@w:val"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <!-- we might want to follow the basedOn reference, but not at the moment -->
                                <xsl:value-of select="document(concat($word-directory,'/word/styles.xml'))//w:style[w:name/@w:val=$style]/w:pPr/w:numPr/w:numId/@w:val"/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>
                    
                    <!-- look up the level .. either in document.xml or in styles.xml  -->
                    <xsl:variable name="numbering-level">
                        <xsl:choose>
                            <xsl:when test="w:pPr/w:numPr/w:ilvl/@w:val">
                                <xsl:value-of select="w:pPr/w:numPr/w:ilvl/@w:val"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <!-- we might want to follow the basedOn reference, but not at the moment -->
                                <xsl:value-of select="document(concat($word-directory,'/word/styles.xml'))//w:style[w:name/@w:val=$style]/w:pPr/w:numPr/w:ilvl/@w:val"/>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:variable>
                    
                    <!-- find the abstract numbering definition and then the corresponding numfmt -->
                    <xsl:variable name="abstract-def" select="document(concat($word-directory,'/word/numbering.xml'))//w:num[@w:numId=$numbering-def]/w:abstractNumId/@w:val"/>
                    <xsl:variable name="numfmt">
                        <xsl:value-of select="document(concat($word-directory,'/word/numbering.xml'))//w:abstractNum[@w:abstractNumId=$abstract-def]/w:lvl[@w:ilvl=$numbering-level]/w:numFmt/@w:val"/>
                    </xsl:variable>
                    
                    
                    <!-- figure out what numbering scheme to use -->
                    <xsl:choose>
                        <xsl:when test="string-length($numfmt)=0">unordered</xsl:when>
                        <xsl:when test="$numfmt='bullet'">unordered</xsl:when>
                        <xsl:otherwise>ordered</xsl:otherwise>
                    </xsl:choose>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:attribute>
    </xsl:template>
    
    
    <!-- 
        Handle TOC
    -->
    <xsl:template name="toc">
        <divGen type="toc"/>
    </xsl:template>
    
    
    
    <xsl:template name="getDocTitle">
        <xsl:for-each select="document('docProps/core.xml',/)">
            <xsl:value-of select="cp:coreProperties/dc:title"/>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="getDocAuthor">
        <xsl:for-each select="document('docProps/core.xml',/)">
            <xsl:value-of select="cp:coreProperties/dc:creator"/>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="getDocDate">
        <xsl:for-each select="document('docProps/core.xml',/)">
            <xsl:value-of select="substring-before(cp:coreProperties/dcterms:created,'T')"/>
        </xsl:for-each>
    </xsl:template>
    
    <xsl:template name="whatsTheDate">
        <xsl:value-of
            select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[M02]:[s02]Z')"/>
    </xsl:template>

</xsl:stylesheet>

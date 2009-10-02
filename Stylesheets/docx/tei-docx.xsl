<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 		
    version="2.0" 
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:cals="http://www.oasis-open.org/specs/tm9901"
    xmlns:contypes="http://schemas.openxmlformats.org/package/2006/content-types"
    xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
    xmlns:dc="http://purl.org/dc/elements/1.1/" 
    xmlns:dcmitype="http://purl.org/dc/dcmitype/"
    xmlns:dcterms="http://purl.org/dc/terms/"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:its="http://www.w3.org/2005/11/its"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="cp ve o r m v wp w10 w wne mml tbx iso its
			     tei a xs pic fn xsi dc dcterms dcmitype
			     contypes teidocx teix html cals xd">

  <xsl:import href="tei-docx-functions.xsl"/>
  <xsl:import href="tei-docx-verbatim.xsl"/>
  <xsl:import href="variables.xsl"/>
  <xsl:import href="mml2omml.xsl"/>
<!--
A4 is 210mm x 297mm; leaving 1in margin (25mm),
gives 160 x 247 approx useable area.  In Microsoft speak, 
1mm = 35998 units. so page size is 6659630 x 9791456
Divide by 100 to avoid overflow.
-->
   <xsl:param name="pageWidth">57596.80</xsl:param>
   <xsl:param name="pageHeight">88915.06</xsl:param>
   <xsl:param name="defaultHeaderFooterFile">default.xml</xsl:param>
   <xsl:param name="postQuote">’</xsl:param>
   <xsl:param name="preQuote">‘</xsl:param>
   <xsl:param name="bulletOne">&#xF0BE;</xsl:param>
   <xsl:param name="bulletTwo">•</xsl:param>
   <xsl:param name="bulletThree">*</xsl:param>
   <xsl:param name="bulletFour">+</xsl:param>
   <xsl:param name="bulletFive">•</xsl:param>
   <xsl:param name="bulletSix">•</xsl:param>
   <xsl:param name="bulletSeven">•</xsl:param>
   <xsl:param name="bulletEight">•</xsl:param>


    <xd:doc type="stylesheet">
        <xd:short> TEI stylesheet for making Word docx files from TEI XML </xd:short>
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
    
    

    <xsl:variable name="align">right</xsl:variable>

    <xsl:strip-space elements="*"/>
    <xsl:preserve-space elements="tei:text"/>
    <xsl:output method="xml" version="1.0" encoding="UTF-8"/>

    <xsl:key name="FOOTERS" match="tei:fw[@type='footer']" use="@xml:id"/>
    <xsl:key name="HEADERS" match="tei:fw[@type='header']" use="@xml:id"/>
    <xsl:key name="ALLFOOTERS" match="tei:fw[@type='footer']" use="1"/>
    <xsl:key name="ALLHEADERS" match="tei:fw[@type='header']" use="1"/>

    <xsl:key name='ENDNOTES' match="tei:note[@place='end']" use="1"/>
    <xsl:key name='FOOTNOTES' match="tei:note[@place='foot' or @place='bottom' ]" use="1"/>

    <xsl:key name="IDS" match="tei:*[@xml:id]" use="@xml:id"/>

    <xsl:key name='OL' match="tei:list[@type='ordered']" use="1"/>
    <xsl:key name='BLIP' match="a:blip" use="1"/>

    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="debug">false</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($word-directory, '/word/styles.xml')"/>
    </xsl:param>
    <xsl:param name="docDoc">
        <xsl:value-of select="concat($word-directory, '/word/document.xml')"/>
    </xsl:param>

    <xsl:variable name="lowercase">abcdefghijklmnopqrstuvwxyz</xsl:variable>
    <xsl:variable name="uppercase">ABCDEFGHIJKLMNOPQRSTUVWXYZ</xsl:variable>
    <xsl:key name="Styles" match="w:style/w:name" use="@w:val"/>


    <xsl:template match="/tei:TEI">


        <xsl:call-template name="write-header-files"/>

        <!--  Write out footer files -->
        <xsl:call-template name="write-footer-files"/>

        <!-- Write out numbering file -->
        <xsl:call-template name="write-numbering-definition"/>
        
        <!-- Write out footnotes file -->
        <xsl:call-template name="write-footnotes-file"/>
        
        <!-- Write out endnotes file -->
        <xsl:call-template name="write-endnotes-file"/>
        
        <!-- main relationships -->
        <xsl:call-template name="write-main-relationships"/>
        
        <!-- relationships -->
        <xsl:call-template name="write-relationships"/>
        
        <!-- write Content Types -->
        <xsl:call-template name="write-content-types"/>

        <!-- Write out settings -->
        <xsl:call-template name="write-settings"/>

        <!-- app files -->

        <xsl:call-template name="write-appFiles"/>

        <w:document xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
            xmlns:o="urn:schemas-microsoft-com:office:office"
            xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
            xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
            xmlns:v="urn:schemas-microsoft-com:vml"
            xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
            xmlns:w10="urn:schemas-microsoft-com:office:word"
            xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
            xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml">

            <w:body>

                <!-- are there specific titlepages -->
                <xsl:call-template name="titlepages"/>

		<!-- header components -->

		<xsl:call-template name="headerParts"/>

                <!-- The front matter -->
                <xsl:apply-templates select="tei:text/tei:front"/>

                <!-- document title -->
                <xsl:call-template name="document-title"/>

                <!-- Describes the main part of the document -->
                <xsl:apply-templates select="tei:text/tei:body"/>

                <!-- Describes the back matter of the document -->
                <xsl:apply-templates select="tei:text/tei:back"/>

                <!-- write out final sectPr .. if exists -->
		<xsl:choose>
		  <xsl:when test="tei:text/tei:milestone">
		    <xsl:apply-templates select="tei:text/tei:milestone[1]">
		      <xsl:with-param name="final-section">true</xsl:with-param>
		    </xsl:apply-templates>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:apply-templates
			select="document($defaultHeaderFooterFile)/tei:TEI/tei:text/tei:milestone">
		      <xsl:with-param
			  name="final-section">true</xsl:with-param>
		    </xsl:apply-templates>
		  </xsl:otherwise>
		</xsl:choose>
            </w:body>
        </w:document>
    </xsl:template>


    <!--
        Template used to process block elements:
        @param style          A style for all the <w:p>s
        @param pPr            An entire pPr element to use
        @param nop            a w:p has already been created and no new w:p is to be created
        @param bookmark-id    if present in conjunction with bookmark-name, a bookmark is created around the current element
        @param bookmark-name  @see bookmark-id
        
        
    -->
    <xsl:template name="block-element">
        <xsl:param name="style"/>
        <xsl:param name="select" select="."/>
        <xsl:param name="pPr"/>
        <xsl:param name="nop"/>
        <xsl:param name="bookmark-id"/>
        <xsl:param name="bookmark-name"/>

        <xsl:for-each select="$select">
            <xsl:for-each-group select="*|processing-instruction()|text()" group-adjacent="1">
                <xsl:call-template name="_process-blockelement">
                    <xsl:with-param name="style" select="$style"/>
                    <xsl:with-param name="pPr" select="$pPr"/>
                    <xsl:with-param name="nop" select="$nop"/>
                    <xsl:with-param name="bookmark-id" select="$bookmark-id"/>
                    <xsl:with-param name="bookmark-name" select="$bookmark-name"/>
                </xsl:call-template>
            </xsl:for-each-group>
        </xsl:for-each>

    </xsl:template>
    <!-- end template block-element -->

    <!-- 
        This template processes block elements (or better to say the children of a block element)
        and should never be called directly 
        (call block-element instead). The function processes all children and puts
        all inline elements into one w:p. If it encounters a nested block element
        (e.g. a note inside a p) then it closes the previous w:p processes that block
        element and then again starts putting all following inline elements into another
        w:p element.
        
        @see block-element
    -->
    <xsl:template name="_process-blockelement">
        <xsl:param name="style"/>
        <xsl:param name="pPr"/>
        <xsl:param name="nop"/>
        <xsl:param name="bookmark-id"/>
        <xsl:param name="bookmark-name"/>

        <!-- bookmark -->
        <xsl:if
            test="string-length($bookmark-name) &gt; 0 and string-length($bookmark-id) &gt; 0">
            <w:bookmarkStart w:id="{$bookmark-id}" w:name="{$bookmark-name}"/>
        </xsl:if>

        <!-- Process Child elements -->
        <xsl:for-each-group select="current-group()"
            group-starting-with="*[not(teidocx:is-inline(.))]">
            <xsl:choose>
                <!-- if the current item is a block element, we process that one,
                     and then take call this function recursively was all the other
                     elements -->
                <xsl:when test="self::*[not(teidocx:is-inline(.))]">
                    <!-- process block element -->
                    <xsl:apply-templates select=".">
                        <xsl:with-param name="style" select="$style"/>
                        <xsl:with-param name="pPr" select="$pPr"/>
                        <xsl:with-param name="nop" select="$nop"/>
                    </xsl:apply-templates>

                    <!-- process all the other elements in the current group -->
                    <xsl:for-each-group select="current-group() except ." group-adjacent="1">
                        <xsl:call-template name="_process-blockelement">
                            <xsl:with-param name="style" select="$style"/>
                            <xsl:with-param name="pPr" select="$pPr"/>
                            <xsl:with-param name="nop" select="$nop"/>
                        </xsl:call-template>
                    </xsl:for-each-group>
                </xsl:when>
                
                <!-- we encountered an inline element. This means that the current group only
                     contains inline elements -->
                <xsl:otherwise>
                    <!-- create all text runs for each item in the current group. we will later
                         on decide whether we are grouping them together in a w:p or not. -->
                    <xsl:variable name="innerRuns">
                        
                        <!-- add paragraph properties (if nobody else created a w:p ($nop)) -->
                        <xsl:if test="$nop!='true'">
                            <xsl:choose>
                                <xsl:when test="string-length($style) &gt; 0">
                                    <w:pPr>
                                        <w:pStyle>
                                            <xsl:attribute name="w:val" select="$style"/>
                                        </w:pStyle>
                                    </w:pPr>
                                </xsl:when>
                                <xsl:when test="not(empty($pPr))">
                                    <xsl:copy-of select="$pPr"/>
                                </xsl:when>
                                <xsl:otherwise/>
                            </xsl:choose>
                        </xsl:if>
                        	
                        <!-- Create text runs -->
                        <xsl:for-each select="current-group()">
                            <xsl:apply-templates select=".">
                                <xsl:with-param name="style" select="$style"/>
                                <xsl:with-param name="pPr" select="$pPr"/>
                            </xsl:apply-templates>
                        </xsl:for-each>
                    </xsl:variable>

                    <!-- write out text runs.
                         if someone has already created a w:p ($nop) we may not create another one. -->
                    <xsl:choose>
                        <xsl:when test="$nop='true'">
                            <xsl:copy-of select="$innerRuns"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <w:p>
                                <xsl:copy-of select="$innerRuns"/>
                            </w:p>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each-group>
        <!-- end process children -->

        <!-- bookmark end-->
        <xsl:if
            test="string-length($bookmark-name) &gt; 0 and string-length($bookmark-id) &gt; 0">
            <w:bookmarkEnd w:id="{$bookmark-id}"/>
        </xsl:if>

    </xsl:template>
    <!-- end template _process-blockelement -->


    <!-- 
        Template for all simple block elements.
        This template looks for a style definition template (mode="get-style") that
        matches the block element that is currently processed. If none is specified
        it copies the style definition from the parent element.
        
        If some special rendering is required you should overwrite this template.
    -->
    <xsl:template match="*[not(teidocx:is-inline(.))]" priority="-10">
        <xsl:param name="style"/>
        <xsl:param name="pPr"/>
        <xsl:param name="nop"/>

        <!-- calculate style definition -->
        <xsl:variable name="newStyle">
            <xsl:apply-templates select="." mode="get-style"/>
        </xsl:variable>
        <xsl:variable name="styleToPassOn">
            <xsl:choose>
                <xsl:when test="string-length($newStyle) &gt; 0">
                    <xsl:value-of select="$newStyle"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$style"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- process children  -->
        <xsl:call-template name="block-element">
            <xsl:with-param name="style" select="$styleToPassOn"/>
            <xsl:with-param name="pPr" select="$pPr"/>
            <xsl:with-param name="nop" select="$nop"/>
        </xsl:call-template>
    </xsl:template>
    <!-- end template simple block elements: *[not(teidocx:is-inline(.))] -->
    
    
    
    <!--
        Template for all simple inline elements
        This template looks for a character style definition template (mode="get-style")
        for the currently processed element.
        -->
    <xsl:template match="*[teidocx:is-inline(.)]" priority="-10">
        <xsl:param name="character-style"/>
        
        <xsl:variable name="style">
            <xsl:apply-templates select="." mode="get-style"/>
        </xsl:variable>

        <xsl:variable name="character-style">
            <xsl:choose>
                <xsl:when test="(string-length($style) &gt; 0)">
                    <xsl:value-of select="$style"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$character-style"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:apply-templates>
            <xsl:with-param name="character-style" select="$character-style"/>
        </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="text()">
        <xsl:param name="character-style"/>
	<xsl:if test="parent::tei:head/parent::tei:div[@iso:status]">
	  <w:r>
	    <w:t>
	      <xsl:attribute name="xml:space">preserve</xsl:attribute>
	      <xsl:text> (</xsl:text>
	      <xsl:value-of select="../../@iso:status"/>
	      <xsl:text>) </xsl:text>
	    </w:t>
	  </w:r>
	</xsl:if>

        <w:r>
            <!-- if no specific style is assigned we might check for any other indication to assign 
                some style ... -->
            <xsl:variable name="renderingProperties">
                <xsl:call-template name="applyRend"/>
            </xsl:variable>
            
            <xsl:if test="string-length($character-style) &gt; 0 or not(empty($renderingProperties))">
            <w:rPr>
                <xsl:if test="string-length($character-style) &gt; 0">
                    <w:rStyle>
                        <xsl:attribute name="w:val" select="$character-style"/>
                    </w:rStyle>
                </xsl:if>
                <xsl:copy-of select="$renderingProperties"/>
            </w:rPr>
            </xsl:if>

            <xsl:call-template name="Text"/>
        </w:r>
    </xsl:template>

    <!-- Style definition templates: 
        No default Style for any block or inline element -->
    <xsl:template match="*" mode="get-style"/>


    <!-- to a given style name, this template returns the correct style id
        looking it up in styles.xml -->
    <xsl:template name="getStyleName">
        <xsl:param name="in"/>
        
        <xsl:for-each select="document($styleDoc,/)">
            <xsl:for-each select="key('Styles',$in)">
                <xsl:value-of select="parent::w:style/@w:styleId"/>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:template>

    <!--
        Tests whether to add rendering attributes to a run
        Styles may not be added in applyRend. If you want to add
        a style go for a get-style template..
        -->

    <xsl:template name="applyRend">
        <xsl:for-each select="..">

            <!-- use a custom font -->
            <xsl:if test="@iso:font">
                <w:rFonts w:ascii="{@iso:font}" w:hAnsi="{@iso:font}"/>
            </xsl:if>

            <!-- bold? -->
            <xsl:choose>
                <xsl:when test="teidocx:render-bold(.)">
                    <w:b/>
                </xsl:when>
                <xsl:when test="self::tei:hi[not(@rend)]">
                    <w:b/>
                </xsl:when>
            </xsl:choose>

            <!-- italic -->
            <xsl:choose>
                <xsl:when test="@rend='italic' or teidocx:render-italic(.)">
                    <w:i/>
                </xsl:when>
                <xsl:when test="self::tei:emph">
                    <w:i/>
                </xsl:when>
            </xsl:choose>

	    <!-- typewriter -->
            <xsl:choose>
                <xsl:when test="@rend='typewriter' or teidocx:render-typewriter(.)">
		  <w:rFonts w:ascii="Courier" w:hAnsi="Courier"/>
		</xsl:when>
	    </xsl:choose>

            <xsl:if test="@rend='subscript'">
                <w:vertAlign w:val="subscript"/>
            </xsl:if>

            <xsl:if test="@rend='superscript'">
                <w:vertAlign w:val="superscript"/>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>


    <!-- 
        Templates to handle text.
        -->
    <!--
    <xsl:template match="text()">
        <xsl:choose>
            <xsl:when test="ancestor::tei:titleStmt">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:otherwise>
                <w:r>
                    <w:rPr>
                        <xsl:call-template name="applyRend"/>
                    </w:rPr>
                    <xsl:call-template name="Text"/>
                </w:r>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    -->

    <xsl:template name="Text">
        <xsl:choose>
            <xsl:when test="parent::w:body">
                <xsl:message terminate="yes">CDATA found in body! [<xsl:value-of select="."/>]</xsl:message>
            </xsl:when>
            <xsl:otherwise>
	      <w:t>
		<xsl:attribute name="xml:space">preserve</xsl:attribute>
		<xsl:choose>
		  <xsl:when test=".=' ' or ../@xml:space='preserve'">
		    <xsl:value-of select="."/>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:if test="starts-with(.,' ') or starts-with(.,'&#10;')">
		      <xsl:text> </xsl:text>
		    </xsl:if>
		    <xsl:value-of select="normalize-space(.)"/>
		    <xsl:if test="substring(.,string-length(.),1)=' '">
		      <xsl:text> </xsl:text>
		    </xsl:if>
		    <xsl:if test="substring(.,string-length(.),1)='&#10;'">
		      <xsl:text> </xsl:text>
		    </xsl:if>
		  </xsl:otherwise>
		</xsl:choose>
                </w:t>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- 
        Dealing with divGens
    -->
    <xsl:template match="tei:divGen">
        <xsl:choose>
            <xsl:when test="@type='toc'">
                <xsl:call-template name="generate-toc"/>
            </xsl:when>
        </xsl:choose>
        
    </xsl:template>

    <!-- 
        Table of Contents:
        Feel free to overwrite this one.
    -->
    <xsl:template name="generate-toc">
        <w:p>
            <w:pPr>
                <w:pStyle w:val="TOC1"/>
                <w:tabs>
                    <w:tab w:val="right" w:leader="dot" w:pos="9350"/>
                </w:tabs>
            </w:pPr>
            <w:r>
                <w:fldChar w:fldCharType="begin"/>
            </w:r>
            <w:r>
                <w:rPr>
                    <w:noProof/>
                </w:rPr>
                <w:instrText xml:space="preserve"> TOC \o "1-6" \h \z </w:instrText>
            </w:r>
            <w:r>
                <w:fldChar w:fldCharType="separate"/>
            </w:r>
            <w:r>
                <w:fldChar w:fldCharType="end"/>
            </w:r>
        </w:p>
    </xsl:template>
    

    <!-- 
        Footnotes
    -->
    <xsl:template name="create-footnote">
        <xsl:variable name="num">
            <xsl:number count="tei:note[@place='foot' or @place='bottom' ]" level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$num+1"/>
        <w:r>
            <w:rPr>
                <w:rStyle w:val="FootnoteReference"/>
            </w:rPr>
            <w:footnoteReference w:id="{$id}"/>
        </w:r>
        <w:r>
            <w:t xml:space="preserve"> </w:t>
        </w:r>
    </xsl:template>
    
    <!-- 
        Endnotes
    -->
    <xsl:template name="create-endnote">
        <xsl:variable name="num">
            <xsl:number count="tei:note[@place='end']" level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$num+1"/>
        <w:r>
            <w:rPr>
                <w:rStyle w:val="EndnoteReference"/>
                <w:noProof/>
            </w:rPr>
            <w:endnoteReference w:id="{$id}"/>
        </w:r>
        <w:r>
            <w:t xml:space="preserve"> </w:t>
        </w:r>
    </xsl:template>

    <!-- 
        Dealing with sections
    -->
    <xsl:template match="tei:milestone">
        <xsl:param name="final-section">false</xsl:param>
	

        <!-- construct sectPr -->
        <xsl:variable name="sectPr">
            <w:sectPr>
                <xsl:for-each select="teidocx:footer">
                    <xsl:variable name="ref" select="@ref"/>
                    <xsl:variable name="footernum">
		              <xsl:for-each select="key('FOOTERS',$ref)">
			            <xsl:number level="any"/>
		              </xsl:for-each>
		            </xsl:variable>
                    <w:footerReference w:type="{@type}" r:id="{concat('rId',100+$footernum)}"/>
                </xsl:for-each>

                <xsl:for-each select="teidocx:header">
                    <xsl:variable name="ref" select="@ref"/>
                    <xsl:variable name="headernum">
                    <xsl:for-each select="key('HEADERS',$ref)">
			            <xsl:number level="any"/>
			        </xsl:for-each>
                    </xsl:variable>
                    <w:headerReference w:type="{@type}" r:id="{concat('rId',100+$headernum)}"/>
                </xsl:for-each>

                <w:pgSz>
                    <xsl:choose>
                        <!-- landscape -->
                        <xsl:when test="teidocx:orientation/@type='landscape'">
                            <xsl:attribute name="w:orient">landscape</xsl:attribute>
                            <xsl:attribute name="w:w">15840</xsl:attribute>
                            <xsl:attribute name="w:h">12240</xsl:attribute>
                        </xsl:when>
                        <!-- portrait -->
                        <xsl:otherwise>
                            <xsl:attribute name="w:w">12240</xsl:attribute>
                            <xsl:attribute name="w:h">15840</xsl:attribute>
                        </xsl:otherwise>
                    </xsl:choose>
                </w:pgSz>
                <w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:gutter="0"
                    w:footer="720" w:header="720"/>
                <xsl:if test="teidocx:pageNumbering">
                    <w:pgNumType>
                        <xsl:if test="teidocx:pageNumbering/@start">
                            <xsl:attribute name="w:start" select="teidocx:pageNumbering/@start"/>
                        </xsl:if>
                        <xsl:if test="teidocx:pageNumbering/@type">
                            <xsl:attribute name="w:fmt" select="teidocx:pageNumbering/@type"/>
                        </xsl:if>
                    </w:pgNumType>
                </xsl:if>
                <xsl:if test="teidocx:header/@type='first' or teidocx:footer/@type='first'">
                    <w:titlePg/>
                </xsl:if>
                <w:docGrid w:linePitch="360"/>
            </w:sectPr>
        </xsl:variable>

        <!-- write out sectPr -->
        <xsl:choose>
            <xsl:when test="$final-section='false'">
                <w:p>
                    <w:pPr>
                        <xsl:copy-of select="$sectPr"/>
                    </w:pPr>
                </w:p>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="$sectPr"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--
        Headers and Sections  
    -->
    <xsl:template
        match="tei:head[parent::tei:div or 
                                            parent::tei:div1 or
                                            parent::tei:div2 or
                                            parent::tei:div3 or
                                            parent::tei:div4 or
                                            parent::tei:div5 or
                                            parent::tei:div6 or
                                            parent::tei:div7]">

        <!-- find out what level we are at -->
        <xsl:variable name="level">
            <xsl:value-of
                select="count(ancestor-or-self::tei:div|
                                        ancestor-or-self::tei:div1|
                                        ancestor-or-self::tei:div2|
                                        ancestor-or-self::tei:div3|
                                        ancestor-or-self::tei:div4|
                                        ancestor-or-self::tei:div5|
                                        ancestor-or-self::tei:div6|
                                        ancestor-or-self::tei:div7)"
            />
        </xsl:variable>

        <!-- grep all previous headings to get some id -->
        <xsl:variable name="number">
            <xsl:number level="any"/>
        </xsl:variable>

        <xsl:call-template name="block-element">
            <!-- we want a bookmark for referencing this section -->
            <xsl:with-param name="bookmark-id">
                <xsl:value-of select="1000+$number"/>
            </xsl:with-param>
            <xsl:with-param name="bookmark-name">
                <xsl:text>_SECTION_</xsl:text>
                <xsl:value-of select="1000+$number"/>
            </xsl:with-param>

            <!-- find the correct header style -->
            <xsl:with-param name="style">
                <xsl:variable name="style" select="teidocx:get-headingstyle(.,$level)"/>
                <xsl:choose>
                    <xsl:when test="string-length($style) &gt; 0">
                        <xsl:call-template name="getStyleName">
                            <xsl:with-param name="in" select="$style"/>
                        </xsl:call-template>
                    </xsl:when>

                    <xsl:otherwise>
                        <xsl:call-template name="getStyleName">
                            <xsl:with-param name="in">
                                <xsl:text>heading </xsl:text>
                                <xsl:value-of select="$level"/>
                            </xsl:with-param>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>

            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>

    <!-- 
        Handle value lists
    -->
    <xsl:template match="tei:label[following-sibling::tei:*[1]/self::tei:item]">
        <xsl:param name="nop"/>
       
       <w:p>
           <w:pPr>
               <w:pStyle w:val="dl"/>
               <w:ind w:left="567" w:hanging="567"/>
           </w:pPr>
           <xsl:apply-templates>
               <xsl:with-param name="nop">true</xsl:with-param>
           </xsl:apply-templates>
           <w:r>
               <w:tab/>
           </w:r>
           <xsl:for-each select="following-sibling::tei:item[1]">
	     <xsl:apply-templates>
               <xsl:with-param name="nop">true</xsl:with-param>
           </xsl:apply-templates>
	   </xsl:for-each>           
       </w:p>
    </xsl:template>
    
    <xsl:template match="tei:item/tei:list">
      <xsl:param name="nop"/>
      <xsl:apply-templates>
               <xsl:with-param name="nop">false</xsl:with-param>
      </xsl:apply-templates>
    </xsl:template>

    <xsl:template match="tei:item[preceding-sibling::tei:*[1]/self::tei:label]"/>
    

    <xsl:template match="comment()"/>
    <!-- 
        Handle list items
    -->
    <xsl:template match="tei:item">
        <xsl:param name="nop"/>


        <xsl:variable name="listStyle">
            <xsl:choose>
                <xsl:when test="../@type='unordered' or ../@type='simple' or not(../@type)">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>List Continue</xsl:text>
			    <xsl:call-template name="listNumberDepth"/>
			</xsl:with-param>
		    </xsl:call-template>
		</xsl:when>
                <xsl:when test="../@type='ordered'">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>List Number</xsl:text>
			    <xsl:call-template name="listNumberDepth"/>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:when>
                <xsl:when test="../@type='termlist'"/>
                <xsl:otherwise>
                    <xsl:text>ListParagraph</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        <xsl:variable name="pPr">
            <w:pPr>
                <xsl:if test="string-length($listStyle) &gt; 1">
                    <w:pStyle w:val="{$listStyle}"/>
                </xsl:if>
                <xsl:choose>
                    <xsl:when test="../@type='unordered' or not(../@type)">
                        <w:numPr>
                            <w:ilvl>
                                <xsl:attribute name="w:val">
                                    <xsl:value-of select="count(ancestor::tei:list) - 1"/>
                                </xsl:attribute>
                            </w:ilvl>
                            <w:numId w:val="2"/>
                        </w:numPr>
                    </xsl:when>
                    <xsl:when test="../@type='ordered'">
                        <w:numPr>
                            <w:ilvl>
                                <xsl:attribute name="w:val">
                                    <xsl:value-of select="count(ancestor::tei:list) - 1"/>
                                </xsl:attribute>
                            </w:ilvl>
                            <w:numId>
                                <!-- @see template: numbering-definition ordered lists -->
                                <xsl:variable name="CurrentList">
                                    <xsl:value-of select="generate-id(..)"/>
                                </xsl:variable>
                                <xsl:attribute name="w:val">
                                    <xsl:for-each select="key('OL',1)">
                                        <xsl:if test="$CurrentList=generate-id(.)">
                                            <xsl:value-of select="position()+100"/>
                                        </xsl:if>
                                    </xsl:for-each>
                                </xsl:attribute>
                            </w:numId>
                        </w:numPr>
                    </xsl:when>
                </xsl:choose>
            </w:pPr>
        </xsl:variable>
<!--
<xsl:message>List item <xsl:value-of select="."/>, <xsl:value-of
select="$nop"/>, <xsl:value-of select="$listStyle"/></xsl:message>
-->

        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr" select="$pPr"/>
            <xsl:with-param name="nop" select="$nop"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template name="listNumberDepth">
      <xsl:choose>
	<xsl:when
	    test="ancestor::tei:glossListEntry">
	  <xsl:value-of
	      select="count(ancestor::tei:list)
		      + ancestor::tei:glossListEntry/@count"/>
	</xsl:when>
	<xsl:when test="parent::tei:list/ancestor::tei:list">
	  <xsl:text> </xsl:text>
	  <xsl:value-of select="count(ancestor::tei:list)"/>
	</xsl:when>
      </xsl:choose>
    </xsl:template>

      <!-- 
	   Handle figures 
      -->
    
    <xsl:template match="tei:figure[not(@rend)]">
      <xsl:call-template name="block-element">
	<xsl:with-param name="pPr">
	  <w:pPr>
	    <w:spacing w:before="240"/>
	    <w:jc w:val="left"/>
	  </w:pPr>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
    <xsl:template match="tei:figure/tei:figDesc"/>

    <xsl:template match="tei:figure/tei:head">
      <xsl:call-template name="block-element">
	<xsl:with-param name="style">Figuretitle</xsl:with-param>
      </xsl:call-template>
    </xsl:template>

    <xsl:template match="tei:graphic">

        <!-- perform some tests on the graphic -->
        <xsl:if
            test="@url and 
		  (
		  (@teidocx:width and @teidocx:height)
		  or
		  (@width and @height))">
	  
<!--

is there a number present?

                  not(number(substring(@width,0,string-length(@width)-1))=NAN) and 
                  not(number(substring(@height,0,string-length(@height)-1))=NAN)">

-->

            <xsl:variable name="imageWidth">
	      <xsl:choose>
		<xsl:when test="contains(@width,'%')">
		  <xsl:value-of select="number($pageWidth * number(substring-before(@width,'%'))) cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@width">
		  <xsl:value-of select="teidocx:convert-dim-emu(@width)"/>
		</xsl:when>
		<xsl:when test="@scale and @teidocx:width">
		  <xsl:value-of select="(@teidocx:width *  number(@scale)) cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@height and @teidocx:height">
		  <xsl:variable name="h">
		    <xsl:value-of
			select="teidocx:convert-dim-emu(@height)"/>
		  </xsl:variable>
		  <xsl:value-of select="(@teidocx:width *
					($h div number(@teidocx:height)))
					cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@teidocx:width">
		  <xsl:value-of select="@teidocx:width"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:message terminate="yes">no way to work out
		  image width for <xsl:value-of select="@url"/>
		  </xsl:message>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:variable>

            <xsl:variable name="imageHeight">
	      <xsl:choose>
		<xsl:when test="contains(@height,'%')">
		  <xsl:value-of select="number($pageHeight * (number(substring-before(@height,'%')))) cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@height">
		  <xsl:value-of select="teidocx:convert-dim-emu(@height)"/>
		</xsl:when>
		<xsl:when test="@scale and @teidocx:height">
		  <xsl:value-of select="(@teidocx:height *  number(@scale)) cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@width and @teidocx:height and
				@teidocx:width">
		  <xsl:value-of select="(($imageWidth *
		    @teidocx:height) div @teidocx:width) cast as xs:integer"/>
		</xsl:when>
		<xsl:when test="@teidocx:height">
		  <xsl:value-of select="@teidocx:height"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:message terminate="yes">no way to work out
		  image height for <xsl:value-of select="@url"/>
		  </xsl:message>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:variable>

<!--
	    <xsl:message> arrived at <xsl:value-of
	    select="$imageWidth"/> x <xsl:value-of
	    select="$imageHeight"/> from <xsl:value-of select="@teidocx:width"/>x<xsl:value-of select="@teidocx:height"/>
	    </xsl:message>
-->
            <!-- prepare actual graphic -->
            <xsl:variable name="graphic-element">
                <a:graphic>
                    <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
                        <pic:pic>
                            <pic:nvPicPr>
                                <pic:cNvPr name="{tokenize(@url, '/')[last()]}">
                                    <xsl:attribute name="id">
                                        <xsl:number level="any"/>
                                    </xsl:attribute>
                                </pic:cNvPr>
                                <pic:cNvPicPr/>
                            </pic:nvPicPr>
                            <pic:blipFill>
                                <a:blip>
                                    <xsl:attribute name="r:embed">
                                        <xsl:variable name="newID">
                                            <xsl:number level="any"/>
                                        </xsl:variable>
                                        <xsl:value-of
                                            select="concat('rId', string(300 + number($newID)))"/>
                                    </xsl:attribute>
                                </a:blip>
                                <a:stretch>
                                    <a:fillRect/>
                                </a:stretch>
                            </pic:blipFill>
                            <pic:spPr>
                                <a:xfrm>
                                    <a:off x="0" y="0"/>
                                    <a:ext cx="{$imageWidth}00"
					   cy="{$imageHeight}00"/>
                                </a:xfrm>
                                <a:prstGeom prst="rect">
                                    <a:avLst/>
                                </a:prstGeom>
                            </pic:spPr>
                        </pic:pic>
                    </a:graphicData>
                </a:graphic>
            </xsl:variable>
            <!-- end graphic element -->

            <w:r>
                <w:drawing>
                    <!-- choose between inline and block -->
                    <xsl:choose>
		      <xsl:when test="parent::tei:figure[@rend='display']">
                        <!-- render  as block -->
                            <wp:anchor simplePos="0" relativeHeight="10" behindDoc="0" locked="0"
                                layoutInCell="1" allowOverlap="1">
                                <wp:simplePos x="0" y="0"/>
                                <wp:positionH relativeFrom="margin">
                                    <wp:align>center</wp:align>
                                </wp:positionH>
                                <wp:positionV relativeFrom="paragraph">
                                    <wp:align>center</wp:align>
                                </wp:positionV>
                                <wp:extent cx="{$imageWidth}00"
					   cy="{$imageHeight}00"/>
                                <wp:wrapTopAndBottom/>
                                <wp:docPr name="Some Image">
                                    <xsl:attribute name="id">
                                        <xsl:number level="any"/>
                                    </xsl:attribute>
                                </wp:docPr>

                                <xsl:copy-of select="$graphic-element"/>
                            </wp:anchor>
		      </xsl:when>
		      <xsl:otherwise>
                            <wp:inline>
                                <wp:extent cx="{$imageWidth}00"
					   cy="{$imageHeight}00"/>
                                <wp:docPr name="{tokenize(@url, '/')[last()]}">
                                    <xsl:attribute name="id">
                                        <xsl:number level="any"/>
                                    </xsl:attribute>
                                </wp:docPr>
                                <xsl:copy-of select="$graphic-element"/>
                            </wp:inline>
		      </xsl:otherwise>
                    </xsl:choose>
                    <!-- end inline/block -->


                </w:drawing>
            </w:r>
        </xsl:if>
    </xsl:template>

    <!-- dynamic content -->
    <xsl:template match="teidocx:dynamicContent">
        <xsl:choose>
            <xsl:when test="@type='pagenumber'">
                <w:fldSimple w:instr=" PAGE \* MERGEFORMAT ">
                    <w:r>
                        <w:rPr>
                            <w:noProof/>
                        </w:rPr>
                        <w:t>1</w:t>
                    </w:r>
                </w:fldSimple>
            </xsl:when>
        </xsl:choose>
    </xsl:template>

    <!-- quoted text -->
    <xsl:template match="tei:q">
        <w:r>
	  <w:t>
	    <xsl:value-of select="$preQuote"/>
	  </w:t>
	</w:r>
	<xsl:apply-templates/>
        <w:r>
	  <w:t>
	    <xsl:value-of select="$postQuote"/>
	  </w:t>
	</w:r>
    </xsl:template>

    <!-- TBX -->

    <xsl:template match="tbx:termEntry">
        <w:p>
            <w:pPr>
                <w:pStyle w:val="TermNum"/>
            </w:pPr>
            <w:r>
                <w:t>
                    <xsl:value-of select="translate(@id,'eid-','')"/>
                </w:t>
            </w:r>
        </w:p>
        <w:p>
            <w:pPr>
                <w:pStyle w:val="Terms"/>
            </w:pPr>
            <w:r>
                <w:t>
                    <xsl:value-of select=".//tbx:term"/>
                </w:t>
            </w:r>
        </w:p>
        <w:p>
            <w:pPr>
                <w:pStyle w:val="Definition"/>
            </w:pPr>
            <w:r>
                <w:t>
                    <xsl:value-of select="tbx:descrip[@type='definition']"/>
                </w:t>
            </w:r>
        </w:p>
    </xsl:template>

    <!-- 
        GI
    -->

    <xsl:template match="tei:hi[@rend='specList-elementSpec']">
        <w:r>
            <w:t>&lt;</w:t>
        </w:r>
        <xsl:apply-templates/>
        <w:r>
            <w:t>&gt;</w:t>
        </w:r>
    </xsl:template>

    <xsl:template match="tei:gi">
        <w:r>
            <w:t>&lt;</w:t>
        </w:r>
        <xsl:apply-templates/>
        <w:r>
            <w:t>&gt;</w:t>
        </w:r>
    </xsl:template>
    

    <!-- 
        Handle examples
    -->
    <xsl:template match="teix:egXML">
    <xsl:param name="simple">false</xsl:param>
    <xsl:param name="highlight"></xsl:param>
        <xsl:call-template name="block-element">
            <xsl:with-param name="style">
	    <xsl:choose>
	      <xsl:when test="ancestor::tei:cell">egXMLTable</xsl:when>
	      <xsl:otherwise>egXML</xsl:otherwise>
	    </xsl:choose>
	    </xsl:with-param>
            <xsl:with-param name="select">
                <tei:p>
                    <xsl:call-template name="create-egXML-section"/>
                </tei:p>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="tei:eg">
        <xsl:call-template name="block-element">
	  <xsl:with-param name="style">
	    <xsl:choose>
	      <xsl:when test="ancestor::tei:cell">egXMLTable</xsl:when>
	      <xsl:otherwise>egXML</xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
            <xsl:with-param name="select">
                <tei:p>
                    <xsl:attribute name="xml:space">preserve</xsl:attribute>
                    <xsl:for-each select="tokenize(.,'\n')">
		      <xsl:choose>
			<xsl:when test="position()=last()">
			  <xsl:value-of select="."/>
			</xsl:when>
			<xsl:when test=".='' and position()=1"/>
			<xsl:otherwise>
			  <xsl:value-of select="."/>
			  <tei:lb/>
			</xsl:otherwise>
		      </xsl:choose>
		    </xsl:for-each>                    
                </tei:p>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>


    <!-- 
        Handle TEI tables 
    -->
    <xsl:template match="tei:table">
        <xsl:call-template name="table-header"/>
        <w:tbl>
            <xsl:choose>
                <xsl:when test="w:tblPr">
                    <xsl:copy-of select="w:tblPr"/>
                </xsl:when>
                <xsl:otherwise>
                    <w:tblPr>
                        <w:tblW w:w="0" w:type="auto"/>
			<w:jc w:val="center"/>
			<w:tblBorders>
			  <w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			  <w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			  <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			  <w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			  <w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			  <w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>
			</w:tblBorders>
                    </w:tblPr>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:choose>
                <xsl:when test="html:colgroup">
                    <w:tblGrid>
                        <xsl:for-each select="html:colgroup/html:col">
                            <w:gridCol>
                                <xsl:attribute name="w:w" select="teidocx:convert-dim-pt20(@width)"/>
                            </w:gridCol>
                        </xsl:for-each>
                    </w:tblGrid>
                </xsl:when>
                <!-- if it is defined in word's namespace -->
                <xsl:when test="w:tblGrid">
                    <xsl:copy-of select="w:tblGrid"/>
                </xsl:when>
		<xsl:otherwise>
                    <w:tblGrid>
		      <xsl:for-each select="tei:row[1]/tei:cell">
			<w:gridCol w:w="500"/> <!-- notional amount -->
		      </xsl:for-each>
		    </w:tblGrid>		  
		</xsl:otherwise>
            </xsl:choose>
            <xsl:apply-templates select="tei:row"/>
        </w:tbl>
        <w:p/>
    </xsl:template>

    <xsl:template name="table-header">
        <xsl:if test="tei:head">
            <xsl:for-each select="tei:head[1]">
                <xsl:call-template name="block-element">
                    <xsl:with-param name="style">Tabletitle</xsl:with-param>
                </xsl:call-template>
            </xsl:for-each>
        </xsl:if>
    </xsl:template>
    <xsl:template match="tei:row">
        <w:tr>
            <xsl:choose>
	      <xsl:when test="w:trPr">
		<xsl:copy-of select="w:trPr"/>
	      </xsl:when>
	      <xsl:otherwise> 
	      </xsl:otherwise>
            </xsl:choose>
            <w:tblPrEx>
                <w:tblLayout w:type="autofit"/> 
            </w:tblPrEx>
            <xsl:apply-templates select="tei:cell"/>
        </w:tr>
    </xsl:template>

    <xsl:template match="tei:cell">
        <w:tc>
            <xsl:choose>
                <xsl:when test="w:tcPr">
                    <xsl:copy-of select="w:tcPr"/>
                </xsl:when>
                <xsl:otherwise>
                    <w:tcPr>
                        <!--w:tcW w:w="1915" w:type="dxa"/-->
                        <xsl:if test="@cols">
                            <w:gridSpan w:val="{@cols}"/>
                        </xsl:if>
                    </w:tcPr>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:choose>
                <xsl:when test="tei:note">
                    <xsl:call-template name="block-element"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:call-template name="block-element">
		      <xsl:with-param name="pPr">
			<w:pPr>
			  <xsl:choose>
			    <xsl:when test="@rend">
			      <xsl:variable name="sName">
				<xsl:call-template name="getStyleName">
				  <xsl:with-param name="in" select="@rend"/>
				</xsl:call-template>
			      </xsl:variable>
			      <xsl:choose>
				<xsl:when test="$sName=''">
				  <w:pStyle w:val="{$TableText}"/>
				</xsl:when>
				<xsl:otherwise>
				  <w:pStyle w:val="{$sName}"/>
				</xsl:otherwise>
			      </xsl:choose>
			    </xsl:when>
			    <xsl:otherwise>
			      <w:pStyle w:val="{$TableText}"/>
			    </xsl:otherwise>
			  </xsl:choose>
			  <xsl:choose>
			    <xsl:when test="@align">
			      <w:jc w:val="{@align}"/>
			    </xsl:when>
			    <xsl:when test="parent::tei:row[@role='label']
					    or @role='label'">
			      <w:jc w:val="left"/>
			    </xsl:when>
			    <xsl:when test="starts-with(.,'[0-9]')">
			      <w:jc w:val="right"/>
			    </xsl:when>
			    <xsl:otherwise>
			      <w:jc w:val="left"/>
			    </xsl:otherwise>
			  </xsl:choose>
			</w:pPr>
		      </xsl:with-param>
		    </xsl:call-template>
		</xsl:otherwise>
            </xsl:choose>
            <!-- If we have no children, put an empty p here -->
            <xsl:if test="not(descendant::text())">
                <w:p>
                    <w:r>
                        <w:t/>
                    </w:r>
                </w:p>
            </xsl:if>
        </w:tc>
    </xsl:template>

    <!-- Handle CALS tables -->
    <xsl:template match="cals:table">
        <xsl:call-template name="cals-table-header"/>
        <w:tbl>
	  <w:tblPr>
	    <w:tblW w:w="0" w:type="auto"/>
	    <w:jc w:val="center"/>
	    <w:tblBorders>
	      <w:top w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	      <w:left w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	      <w:bottom w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	      <w:right w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	      <w:insideH w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	      <w:insideV w:val="single" w:sz="4" w:space="0" w:color="auto"/>
	    </w:tblBorders>
	  </w:tblPr>
	  <xsl:choose>
	    <xsl:when test="cals:tgroup">
	      <w:tblGrid>
		<xsl:for-each select="cals:tgroup/cals:colspec">
		  <w:gridCol>
		    <xsl:attribute name="w:w" select="teidocx:convert-dim-pt20(@colwidth)"/>
		  </w:gridCol>
		</xsl:for-each>
	      </w:tblGrid>
	    </xsl:when>
	    <xsl:otherwise>
	      <w:tblGrid>
		<xsl:for-each select="cals:row[1]/cals:entry">
		  <w:gridCol w:w="500"/> <!-- notional amount -->
		</xsl:for-each>
	      </w:tblGrid>		  
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:apply-templates select="cals:tgroup"/>
	</w:tbl>
	<w:p/>
    </xsl:template>

    <xsl:template match="cals:tgroup">
      <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="cals:tbody">
      <xsl:apply-templates/>
    </xsl:template>

    <xsl:template name="cals-table-header">
        <xsl:if test="cals:title">
            <xsl:for-each select="cals:title[1]">
                <xsl:call-template name="block-element">
                    <xsl:with-param name="style">Tabletitle</xsl:with-param>
                </xsl:call-template>
            </xsl:for-each>
        </xsl:if>
    </xsl:template>
    <xsl:template match="cals:row">
        <w:tr>
            <w:tblPrEx>
                <w:tblLayout w:type="autofit"/> 
            </w:tblPrEx>
            <xsl:apply-templates select="cals:entry"/>
        </w:tr>
    </xsl:template>

    <xsl:template match="cals:entry">
      <xsl:variable name="colpos" select="position()"/>
        <w:tc>
	  <w:tcPr>
	    <xsl:if test="@namest">
	      <xsl:variable name="start">
		<xsl:value-of
		    select="ancestor::cals:table/cals:tgroup/cals:colspec[@colname=current()/@namest]/@colnum"/>
	      </xsl:variable>
	      <xsl:variable name="end">
		<xsl:value-of
		    select="ancestor::cals:table/cals:tgroup/cals:colspec[@colname=current()/@nameend]/@colnum"/>
	      </xsl:variable>
	      <w:gridSpan w:val="{number($end)-number($start)+1}"/>
	    </xsl:if>
	    <xsl:variable name="borders">
	    <xsl:choose>
	      <xsl:when test="@rowsep='0'">
		  <w:bottom w:val="nil"/>
	      </xsl:when>
	      <xsl:when test="parent::cals:row/preceding-sibling::cals:row[1]/cals:entry[$colpos]/@rowsep=0">
		  <w:top w:val="nil"/>
	      </xsl:when>
	    </xsl:choose>
	    </xsl:variable>
	    <xsl:if test="$borders/*">
		<w:tcBorders>
		  <xsl:copy-of select="$borders/*"/>
		</w:tcBorders>
	    </xsl:if>
	  </w:tcPr>
	  <xsl:choose>
	    <xsl:when test="tei:note">
	      <xsl:call-template name="block-element"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:call-template name="block-element">
		<xsl:with-param name="pPr">
		  <w:pPr>
		    <xsl:choose>
		      <xsl:when test="@rend">
			<xsl:variable name="sName">
			  <xsl:call-template name="getStyleName">
			    <xsl:with-param name="in" select="@rend"/>
			  </xsl:call-template>
			</xsl:variable>
			<xsl:choose>
			  <xsl:when test="$sName=''">
			    <w:pStyle w:val="{$TableText}"/>
			  </xsl:when>
			  <xsl:otherwise>
			    <w:pStyle w:val="{$sName}"/>
			  </xsl:otherwise>
			</xsl:choose>
		      </xsl:when>
		      <xsl:otherwise>
			<w:pStyle w:val="{$TableText}"/>
		      </xsl:otherwise>
		    </xsl:choose>
		    <xsl:choose>
		      <xsl:when test="@align">
			<w:jc w:val="{@align}"/>
		      </xsl:when>
		      <xsl:when test="parent::tei:row[@role='label']
				      or @role='label'">
			<w:jc w:val="left"/>
		      </xsl:when>
		      <xsl:when test="starts-with(.,'[0-9]')">
			<w:jc w:val="right"/>
		      </xsl:when>
		      <xsl:otherwise>
			<w:jc w:val="left"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </w:pPr>
		</xsl:with-param>
	      </xsl:call-template>
	    </xsl:otherwise>
	  </xsl:choose>
	  <!-- If we have no children, put an empty p here -->
	  <xsl:if test="not(descendant::text())">
	    <w:p>
	      <w:r>
		<w:t/>
	      </w:r>
	    </w:p>
	  </xsl:if>
	</w:tc>
    </xsl:template>
    
    <!-- 
	 Inline Templates:
    -->
    <xsl:template match="tei:c[@rend='tab']">
        <w:r>
            <w:tab/>
        </w:r>
    </xsl:template>

    <xsl:template match="tei:c[@rend='ptab']">
        <w:r>
            <w:ptab w:relativeTo="margin" w:alignment="{@type}" w:leader="none"/>
        </w:r>
    </xsl:template>


    <xsl:template match="tei:lb">
        <w:r>
            <w:br/>
        </w:r>
    </xsl:template>

    <!-- hyperlink -->
    <xsl:template match="tei:ptr">
      <w:r>
	<w:fldChar w:fldCharType="begin"/>
      </w:r>
      <w:r>
	<w:instrText>HYPERLINK "<xsl:value-of select="@target"/>" \h</w:instrText>
      </w:r>
      <w:r>
	<w:fldChar w:fldCharType="separate"/>
      </w:r>
      <w:r w:rsidR="00765EBE">
	<w:rPr>
	  <w:rStyle w:val="Hyperlink"/>
	  <w:u w:val="none"/>
	</w:rPr>
	<w:t>
	  <xsl:choose>
	    <xsl:when test="starts-with(@target,'#')">
	      <xsl:apply-templates
		  select="key('IDS',substring-after(@target,'#'))" mode="xref"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@target"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</w:t>
      </w:r>
      <w:r>
	<w:fldChar w:fldCharType="end"/>
      </w:r>
<!--
      <w:r>
          <w:rPr>
            <w:rStyle w:val="Hyperlink"/>
          </w:rPr>

	<w:instrText>HYPERLINK "<xsl:value-of select="@target"/>"\h</w:instrText>
      </w:r>
-->
<!--
	<xsl:variable name="rid">
	  <xsl:text>rId</xsl:text>
	  <xsl:variable name="n">
	    <xsl:number level="any"/>
	  </xsl:variable>
	  <xsl:value-of select="$n + 3000"/>
	</xsl:variable>
      <w:hyperlink r:id="{$rid}">
        <w:r>
          <w:rPr>
            <w:rStyle w:val="Hyperlink"/>
          </w:rPr>
          <w:t>
	    <xsl:value-of select="@target"/>
	  </w:t>
	</w:r>
      </w:hyperlink>
-->
    </xsl:template>

    <xsl:template match="tei:note|tei:figure|tei:table|tei:item" mode="xref">
      <xsl:number/>
    </xsl:template>

    <xsl:template match="tei:div" mode="xref">
      <xsl:number
	    count="tei:div"
	    from="tei:body|tei:front|tei:back" 
	    level="multiple"/>
    </xsl:template>

    <!-- 
       §< Handle elements from different namespaces, such as wordML, wordMathML, MathML ...
        -->

    <xsl:template match="m:oMath">
        <xsl:apply-templates select="." mode="iden"/>
    </xsl:template>

    <xsl:template match="w:drawing">
        <w:r>
            <xsl:apply-templates select="." mode="iden"/>
        </w:r>
    </xsl:template>

    <xsl:template match="iso:wordObject">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="w:object">
        <w:r>
            <xsl:apply-templates select="." mode="iden"/>
        </w:r>
    </xsl:template>

    <xsl:template match="w:*">
        <xsl:if test="ancestor::w:tbl">
            <xsl:copy>
                <xsl:copy-of select="@*"/>
                <xsl:apply-templates/>
            </xsl:copy>
        </xsl:if>
    </xsl:template>

    <xsl:template match="mml:math">
      <m:oMath>
	<xsl:apply-templates mode="mml"/>
      </m:oMath>
    </xsl:template>



    <!-- HEADER / FOOTER TEMPLATES -->
    <xsl:template name="write-header-files">
      <xsl:choose>
	<xsl:when test="count(key('ALLHEADERS',1))=0">
	  <xsl:for-each select="document($defaultHeaderFooterFile)">
	    <xsl:call-template name="the-header-files"/>
	  </xsl:for-each>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="the-header-files"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template name="the-header-files">
      <xsl:for-each select="key('ALLHEADERS',1)">
	      <xsl:result-document href="{concat($word-directory,'/word/header',position(),'.xml')}">
        	  <w:hdr xmlns:mv="urn:schemas-microsoft-com:mac:vml"
        		 xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
        		 xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
        		 xmlns:o="urn:schemas-microsoft-com:office:office"
        		 xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
        		 xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
        		 xmlns:v="urn:schemas-microsoft-com:vml"
        		 xmlns:w10="urn:schemas-microsoft-com:office:word"
        		 xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
        		 xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
        		 xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">
        		  <xsl:apply-templates select="."/>
        	  </w:hdr>
	    </xsl:result-document>
      </xsl:for-each>
    </xsl:template>

    <xsl:template name="write-footer-files">
      <xsl:choose>
    	<xsl:when test="count(key('ALLFOOTERS',1))=0">
    	  <xsl:for-each select="document($defaultHeaderFooterFile)">
    	    <xsl:call-template name="the-footer-files"/>
    	  </xsl:for-each>
    	</xsl:when>
    	<xsl:otherwise>
    	    <xsl:call-template name="the-footer-files"/>
    	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    
    <xsl:template name="the-footer-files">
      <xsl:for-each select="key('ALLFOOTERS',1)">
    	<xsl:result-document href="{concat($word-directory,'/word/footer',position(),'.xml')}">
    	  <w:ftr xmlns:mv="urn:schemas-microsoft-com:mac:vml"
    		 xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
    		 xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    		 xmlns:o="urn:schemas-microsoft-com:office:office"
    		 xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    		 xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    		 xmlns:v="urn:schemas-microsoft-com:vml"
    		 xmlns:w10="urn:schemas-microsoft-com:office:word"
    		 xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    		 xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    		 xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">
    	    <xsl:apply-templates select="."/>
    	  </w:ftr>
    	</xsl:result-document>
      </xsl:for-each>
    </xsl:template>



    <!-- Write out the numbering definition file -->
    <xsl:template name="write-numbering-definition">
        <xsl:result-document href="{concat($word-directory,'/word/numbering.xml')}">
            <w:numbering>

                <!-- for headlines -->
                <w:abstractNum w:abstractNumId="1">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:name w:val="heading"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading1"/>
                        <w:lvlText w:val="%1"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="432" w:hanging="432"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading2"/>
                        <w:lvlText w:val="%1.%2"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="648" w:hanging="648"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading3"/>
                        <w:lvlText w:val="%1.%2.%3"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="864" w:hanging="864"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading4"/>
                        <w:lvlText w:val="%1.%2.%3.%4"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="1080"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading5"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1296" w:hanging="1296"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading6"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1512" w:hanging="1512"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading7"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1728" w:hanging="1728"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading8"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1944" w:hanging="1944"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Heading9"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8.%9"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="2160"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                </w:abstractNum>


                <!-- unordered lists -->
		<xsl:call-template name="defineUnorderedLists"/>

                <!-- ordered lists -->
		<xsl:call-template name="defineOrderedLists"/>


                <!-- for sections in Annex -->
                <w:abstractNum w:abstractNumId="4">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="upperLetter"/>
                        <w:pStyle w:val="ANNEX"/>
                        <w:suff w:val="nothing"/>
                        <w:lvlText w:val="Annex %1: "/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="0" w:firstLine="0"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a2"/>
                        <w:lvlText w:val="%1.%2"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="720"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a3"/>
                        <w:lvlText w:val="%1.%2.%3"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="936" w:hanging="936"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a4"/>
                        <w:lvlText w:val="%1.%2.%3.%4"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1152" w:hanging="1152"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a5"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1368" w:hanging="1368"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="a6"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1584" w:hanging="1584"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1800" w:hanging="1800"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2016" w:hanging="2016"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%1.%2.%3.%4.%5.%6.%7.%8.%9"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2232" w:hanging="2232"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for tables -->
                <w:abstractNum w:abstractNumId="5">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Tabletitle"/>
                        <w:suff w:val="space"/>
                        <w:lvlText w:val="Table %1 —"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for bibliography -->
                <w:abstractNum w:abstractNumId="6">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Bibliography"/>
                        <w:lvlText w:val="[%1]"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="1080"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for figures -->
                <w:abstractNum w:abstractNumId="7">
                    <w:multiLevelType w:val="hybridMultilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="Figuretitle"/>
                        <w:suff w:val="space"/>
                        <w:lvlText w:val="Figure %1 —"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="0" w:firstLine="0"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%2."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%3."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%4."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%5."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3600" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%6."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="4320" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5040" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="5760" w:hanging="360"/>
                        </w:pPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="right"/>
                        <w:pPr>
                            <w:ind w:left="6480" w:hanging="180"/>
                        </w:pPr>
                    </w:lvl>
                </w:abstractNum>

                <!-- for headlines -->
                <w:num w:numId="1">
                    <w:abstractNumId w:val="1"/>
                </w:num>

                <!-- unordered lists -->
                <w:num w:numId="2">
                    <w:abstractNumId w:val="2"/>
                </w:num>

                <!-- for annex sections -->
                <w:num w:numId="3">
                    <w:abstractNumId w:val="4"/>
                </w:num>

                <!-- for tables -->
                <w:num w:numId="5">
                    <w:abstractNumId w:val="5"/>
                </w:num>

                <!-- for bibliography -->
                <w:num w:numId="6">
                    <w:abstractNumId w:val="6"/>
                </w:num>

                <!-- for figures -->
                <w:num w:numId="7">
                    <w:abstractNumId w:val="7"/>
                </w:num>

                <!-- for the ordered lists style -->
                <w:num w:numId="8">
                    <w:abstractNumId w:val="3"/>
                    <w:lvlOverride w:ilvl="0">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="1">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="2">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="3">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="4">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="5">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="6">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="7">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                    <w:lvlOverride w:ilvl="8">
                        <w:startOverride w:val="1"/>
                    </w:lvlOverride>
                </w:num>

                <!-- for the unordered lists style -->
                <w:num w:numId="9">
                    <w:abstractNumId w:val="2"/>
                </w:num>


                <!-- ordered lists -->
                <!-- 
                    We have to generate an instance for each list present in the
                    document.
                -->
                <xsl:for-each select="key('OL',1)">
                    <w:num>
                        <xsl:attribute name="w:numId">
                            <xsl:value-of select="position()+100"/>
                        </xsl:attribute>
                        <w:abstractNumId w:val="3"/>
                        <w:lvlOverride w:ilvl="0">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="1">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="2">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="3">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="4">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="5">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="6">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="7">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                        <w:lvlOverride w:ilvl="8">
                            <w:startOverride w:val="1"/>
                        </w:lvlOverride>
                    </w:num>
                </xsl:for-each>


            </w:numbering>
        </xsl:result-document>
    </xsl:template>

    <xsl:template name="defineUnorderedLists">
                <w:abstractNum w:abstractNumId="2">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletOne}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="360" w:hanging="0"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletTwo}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletThree}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletFour}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletFive}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1800" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletSix}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletSeven}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2520" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="bullet"/>
                        <w:pStyle w:val="ListBullet"/>
                        <w:lvlText w:val="{$bulletEight}"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:ascii="Symbol" w:hAnsi="Symbol" w:hint="default"/>
                            <w:color w:val="auto"/>
                        </w:rPr>
                    </w:lvl>

                </w:abstractNum>
    </xsl:template>
    
    <!-- ordered lists -->
    <xsl:template name="defineOrderedLists">
      <w:abstractNum w:abstractNumId="3">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:pStyle w:val="ListNumber"/>
                        <w:lvlText w:val="%1)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="360" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="1">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="ListNumber2"/>
                        <w:lvlText w:val="%2)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="720" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="2">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:pStyle w:val="ListNumber3"/>
                        <w:lvlText w:val="%3)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1080" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="3">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="upperRoman"/>
                        <w:pStyle w:val="ListNumber4"/>
                        <w:lvlText w:val="%4)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1440" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="4">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="(%5)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="1800" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="5">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="(%6)"/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2160" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="6">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:lvlText w:val="%7."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2520" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="7">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerLetter"/>
                        <w:lvlText w:val="%8."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="2880" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                    <w:lvl w:ilvl="8">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="lowerRoman"/>
                        <w:lvlText w:val="%9."/>
                        <w:lvlJc w:val="left"/>
                        <w:pPr>
                            <w:ind w:left="3240" w:hanging="360"/>
                        </w:pPr>
                        <w:rPr>
                            <w:rFonts w:hint="default"/>
                        </w:rPr>
                    </w:lvl>
                </w:abstractNum>

    </xsl:template>

    <xsl:template name="write-appFiles">
        <xsl:variable name="now">
            <xsl:value-of
                select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]T[H02]:[M02]:[s02]Z')"/>
        </xsl:variable>

        <xsl:variable name="coreFile">
            <xsl:value-of select="$word-directory"/>
            <xsl:text>/docProps/core.xml</xsl:text>
        </xsl:variable>

        <xsl:variable name="createdDate">
            <xsl:choose>
                <xsl:when test="doc-available($coreFile)">
                    <xsl:for-each select="document($coreFile)">
                        <xsl:value-of select="cp:coreProperties/dcterms:created"/>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$now"/>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <xsl:variable name="revision">
            <xsl:choose>
                <xsl:when test="doc-available($coreFile)">
                    <xsl:for-each select="document($coreFile)">
                        <xsl:value-of select="cp:coreProperties/cp:revision + 1"/>
                    </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>1</xsl:otherwise>
            </xsl:choose>
        </xsl:variable>

        <!-- after opening core.xml, we cannot write back to it; so save
under new name -->
        <xsl:result-document href="{concat($word-directory,'/docProps/newcore.xml')}"
            standalone="yes">
            <cp:coreProperties>
                <dc:title>
                    <xsl:call-template name="generateTitle"/>
                </dc:title>
                <dc:creator>
                    <xsl:call-template name="created-by"/>
                </dc:creator>
                <cp:lastModifiedBy>TEIISO</cp:lastModifiedBy>
                <cp:revision>
                    <xsl:value-of select="$revision"/>
                </cp:revision>
                <dcterms:created xsi:type="dcterms:W3CDTF">
                    <xsl:value-of select="$createdDate"/>
                </dcterms:created>
                <dcterms:modified xsi:type="dcterms:W3CDTF">
                    <xsl:value-of select="$now"/>
                </dcterms:modified>
            </cp:coreProperties>
        </xsl:result-document>

        <xsl:result-document href="{concat($word-directory,'/docProps/app.xml')}" standalone="yes">
            <Properties
                xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
                xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
                <Template>Iso_Template.dotx</Template>
                <Application>TEIISO tei-docx.xsl</Application>
                <DocSecurity>0</DocSecurity>
                <SharedDoc>true</SharedDoc>
                <AppVersion>1.0</AppVersion>
            </Properties>
        </xsl:result-document>

        <xsl:result-document
	    href="{concat($word-directory,'/docProps/custom.xml')}"
	    standalone="yes">
	  <Properties
	      xmlns="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties" 
	      xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
	    <property pid="1001" name="TEI_toDOCX">
	      <xsl:attribute name="fmtid">
		<xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
	      </xsl:attribute>
	      <vt:lpwstr>2.2.0</vt:lpwstr>
	    </property>
	    <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:appInfo/tei:application">
	      <xsl:if test="not(@ident='TEI_toDOCX')">
		<property name="{@ident}">
		  <xsl:attribute name="pid">
		    <xsl:value-of select="position()+1001"/>
		  </xsl:attribute>
		  <xsl:attribute name="fmtid">
		    <xsl:text>{D5CDD505-2E9C-101B-9397-08002B2CF9AE}</xsl:text>
		  </xsl:attribute>
		  <vt:lpwstr>
		    <xsl:value-of select="@version"/>
		  </vt:lpwstr>
		</property>
	      </xsl:if>
	    </xsl:for-each>
	  </Properties>
        </xsl:result-document>
    </xsl:template>

    <!-- 
        Write word/endnotes
    -->
    <xsl:template name="write-endnotes-file">
        <xsl:result-document href="{concat($word-directory,'/word/endnotes.xml')}">
            <w:endnotes xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml">
                <w:endnote w:type="separator" w:id="0">
                    <w:p>
                        <w:pPr>
                            <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
                        </w:pPr>
                        <w:r>
                            <w:separator/>
                        </w:r>
                    </w:p>
                    <w:p/>
                    <w:p/>
                </w:endnote>
                <w:endnote w:type="continuationSeparator" w:id="1">
                    <w:p>
                        <w:pPr>
                            <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
                        </w:pPr>
                        <w:r>
                            <w:continuationSeparator/>
                        </w:r>
                    </w:p>
                    <w:p/>
                    <w:p/>
                </w:endnote>
                
                <xsl:for-each select="key('ENDNOTES',1)">
                    <xsl:variable name="id" select="position()+1"/>
                    <w:endnote w:id="{$id}">
                        <xsl:call-template name="block-element">
                            <xsl:with-param name="pPr">
                                <w:pPr>
                                    <w:pStyle w:val="EndnoteText"/>
                                </w:pPr>
                                <w:r>
                                    <w:rPr>
                                        <w:rStyle w:val="EndnoteReference"/>
                                    </w:rPr>
                                    <w:endnoteRef/>
                                </w:r>
                            </xsl:with-param>
                        </xsl:call-template>
                    </w:endnote>
                </xsl:for-each>
                
            </w:endnotes>
        </xsl:result-document>
    </xsl:template>

    <!-- 
        Write word/footnotes.xml
    -->
    <xsl:template name="write-footnotes-file">
        <xsl:result-document href="{concat($word-directory,'/word/footnotes.xml')}">
            <w:footnotes xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml">
                <w:footnote w:type="separator" w:id="0">
                    <w:p>
                        <w:pPr>
                            <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
                        </w:pPr>
                        <w:r>
                            <w:separator/>
                        </w:r>
                    </w:p>
                </w:footnote>
                <w:footnote w:type="continuationSeparator" w:id="1">
                    <w:p>
                        <w:pPr>
                            <w:spacing w:after="0" w:line="240" w:lineRule="auto"/>
                        </w:pPr>
                        <w:r>
                            <w:continuationSeparator/>
                        </w:r>
                    </w:p>
                    <w:p/>
                    <w:p/>
                </w:footnote>
                
                <xsl:for-each select="key('FOOTNOTES',1)">
                    <xsl:variable name="id" select="position()+1"/>
                    <w:footnote w:id="{$id}">
                        <xsl:call-template name="block-element">
                            <xsl:with-param name="pPr">
                                <w:pPr>
                                    <w:pStyle w:val="FootnoteText"/>
                                </w:pPr>
                                <w:r>
                                    <w:rPr>
                                        <w:rStyle w:val="FootnoteReference"/>
                                    </w:rPr>
                                    <w:footnoteRef/>
                                </w:r>
                                <w:r>
                                    <w:t xml:space="preserve"> </w:t>
                                </w:r>
                            </xsl:with-param>
                        </xsl:call-template>
                    </w:footnote>
                </xsl:for-each>
                
            </w:footnotes>
        </xsl:result-document>
    </xsl:template>

    <!-- 
        Write word/_rels/document.xml.rels 
    -->
    <xsl:template name="write-content-types">
        <xsl:result-document href="{concat($word-directory,'/%5BContent_Types%5D.xml')}">
           
            <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">

                <Default Extension="jpeg" ContentType="image/jpeg"/>
                <Default Extension="jpg" ContentType="image/jpeg"/>
                <Default Extension="png" ContentType="image/png"/>
                <Default Extension="tiff" ContentType="image/tiff"/>
                <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
                <Default Extension="xml" ContentType="application/xml"/>
                <Default Extension="bin" ContentType="application/vnd.openxmlformats-officedocument.oleObject"/>
                <Default Extension="wmf" ContentType="image/x-wmf"/>
                <Default Extension="emf" ContentType="image/x-emf"/>
               
                
                <!-- docprops -->
                <Override PartName="/docProps/core.xml"
                    ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>
                <Override PartName="/docProps/app.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>
		<Override PartName="/docProps/custom.xml" 
			  ContentType="application/vnd.openxmlformats-officedocument.custom-properties+xml"/>
                
                <!-- word -->
                <Override PartName="/word/document.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
                <Override PartName="/word/styles.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
                <Override PartName="/word/numbering.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/>
                <Override PartName="/word/webSettings.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml"/>
                
                <Override PartName="/word/endnotes.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml"/>
                <Override PartName="/word/fontTable.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"/>
                <Override PartName="/word/footnotes.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml"/>
                <Override PartName="/word/settings.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>
                
                <!-- others -->
                <Override PartName="/word/theme/theme1.xml"
                    ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/>
                
                <!-- headers -->
        		<xsl:choose>
        		  <xsl:when test="count(key('ALLHEADERS',1))=0">
        		    <xsl:for-each select="document($defaultHeaderFooterFile)">
        		      <xsl:call-template name="headerrefs"/>
        		    </xsl:for-each>
        		  </xsl:when>
        		  <xsl:otherwise>
        		    <xsl:call-template name="headerrefs"/>
        		  </xsl:otherwise>
        		</xsl:choose>

                
                <!-- footers -->
        		<xsl:choose>
        		  <xsl:when test="count(key('ALLFOOTERS',1))=0">
        		    <xsl:for-each select="document($defaultHeaderFooterFile)">
        		      <xsl:call-template name="footerrefs"/>
        		    </xsl:for-each>
        		  </xsl:when>
        		  <xsl:otherwise>
        		    <xsl:call-template name="footerrefs"/>
        		  </xsl:otherwise>
        		</xsl:choose>
            </Types>
            
        </xsl:result-document>
    </xsl:template>

    <xsl:template name="footerrefs">
      <xsl:for-each select="key('ALLFOOTERS',1)">
    	<Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
    	    ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml">
    	  <xsl:attribute name="PartName" select="concat('/word/footer', position(), '.xml')"/>
    	</Override>            
      </xsl:for-each>
    </xsl:template>

    <xsl:template name="headerrefs">
      <xsl:for-each select="key('ALLHEADERS',1)">
	    <Override xmlns="http://schemas.openxmlformats.org/package/2006/content-types"
	        ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml">
	      <xsl:attribute name="PartName" select="concat('/word/header', position(), '.xml')"/>
	    </Override>
      </xsl:for-each>
    </xsl:template>

    <!-- 
        Write _rels/.rels
    -->
    <xsl:template name="write-main-relationships">
        <xsl:result-document href="{concat($word-directory,'/_rels/.rels')}"
            standalone="yes">
            <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
                <Relationship Id="rId1"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
                    Target="word/document.xml"/>
                <Relationship Id="rId2"
                    Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
                    Target="docProps/core.xml"/>
                <Relationship Id="rId3"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"
                    Target="docProps/app.xml"/>
		<Relationship Id="rId4"
			      Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties" 
			      Target="docProps/custom.xml"/>
            </Relationships>
        </xsl:result-document>
    </xsl:template>

    <!-- 
        Write word/_rels/document.xml.rels 
    -->
    <xsl:template name="write-relationships">
        <xsl:result-document href="{concat($word-directory,'/word/_rels/document.xml.rels')}"
            standalone="yes">
            <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">

                <!-- Mandatory Relationships -->
                <Relationship Id="rId3"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering"
                    Target="numbering.xml"/>
                <Relationship Id="rId4"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
                    Target="styles.xml"/>
                <Relationship Id="rId5"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings"
                    Target="settings.xml"/>
                <Relationship Id="rId7"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes"
                    Target="footnotes.xml"/>
                <Relationship Id="rId8"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes"
                    Target="endnotes.xml"/>
                <!-- odd stuff -->
                <Relationship Id="rId18"
                    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable"
                    Target="fontTable.xml"/>
                
                <!-- Images -->

                <xsl:for-each select="key('BLIP',1)">
                    <xsl:choose>
                        <xsl:when test="@r:embed">
                            <Relationship Id="rId{position() + 200}"
                                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                                Target="{@r:embed}"/>
                        </xsl:when>
                        <xsl:otherwise>
                            <Relationship Id="rId{position() + 200}"
                                Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                                Target="{@r:link}" TargetMode="External"/>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:for-each>

		<!-- hyperlinks -->
<!--
		<xsl:for-each select="//tei:ptr">
		  <Relationship 
		      Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"
		      Target="{@target}" 
		      TargetMode="External">
<xsl:message>H <xsl:value-of select="@target"/></xsl:message>
		     <xsl:attribute name="Id">
		       <xsl:text>rId</xsl:text>
		       <xsl:variable name="n">
			 <xsl:number level="any"/>
		       </xsl:variable>
		       <xsl:value-of select="$n + 3000"/>
		     </xsl:attribute>
		  </Relationship>
		</xsl:for-each>
-->
                <xsl:for-each select="//tei:graphic[@url]">
                    <Relationship Id="rId{position() + 300}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                        Target="{@url}"/>
                </xsl:for-each>

                <!-- Formulas -->
                <xsl:for-each select="//v:imagedata">
                    <Relationship Id="rId{position() + 1000}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
                        Target="{@r:id}"/>
                </xsl:for-each>

                <xsl:for-each select="//o:OLEObject">
                    <Relationship Id="rId{position() + 2000}"
                        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/oleObject"
                        Target="{@r:id}"/>
                </xsl:for-each>



                <!-- our headers and footers -->
		<xsl:choose>
		  <xsl:when test="count(key('ALLHEADERS',1))=0 and count(key('ALLFOOTERS',1))=0">
		    <xsl:for-each select="document($defaultHeaderFooterFile)">
		      <xsl:call-template name="headfootrels"/>
		    </xsl:for-each>
		  </xsl:when>
		  <xsl:otherwise>
		      <xsl:call-template name="headfootrels"/>
		  </xsl:otherwise>
		</xsl:choose>



            </Relationships>
        </xsl:result-document>

    </xsl:template>
    
    
    <xsl:template name="headfootrels">
      <xsl:for-each select="key('ALLFOOTERS',1)">
          <xsl:variable name="num">
              <xsl:number/>
          </xsl:variable>
          <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships"
	        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
	        Target="{concat('footer',position(),'.xml')}"
	        Id="{concat('rId',100+$num)}"/>
	    </xsl:for-each>
      
      <!-- count all footers -->
      <xsl:for-each select="key('ALLHEADERS',1)">
	    <xsl:variable name="num">
	        <xsl:number/>
	    </xsl:variable>
        <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships"
	        Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
	        Target="{concat('header',position(),'.xml')}"
	        Id="{concat('rId',100+$num)}"
	        />
      </xsl:for-each>
    </xsl:template>
    <!-- 
        write: word/settings.xml 
    -->
    <xsl:template name="write-settings">
        <xsl:result-document href="{concat($word-directory,'/word/settings.xml')}"
            standalone="yes">
            
            <w:settings xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:sl="http://schemas.openxmlformats.org/schemaLibrary/2006/main">
                <w:zoom w:percent="100"/>
                <w:stylePaneSortMethod w:val="0000"/>
                <w:defaultTabStop w:val="720"/>
                <w:evenAndOddHeaders/>
                <w:characterSpacingControl w:val="doNotCompress"/>
                <w:footnotePr>
                    <w:footnote w:id="0"/>
                    <w:footnote w:id="1"/>
                </w:footnotePr>
                <w:endnotePr>
                    <w:endnote w:id="0"/>
                    <w:endnote w:id="1"/>
                </w:endnotePr>
                <w:compat/>
                <m:mathPr>
                    <m:mathFont m:val="Cambria Math"/>
                    <m:brkBin m:val="before"/>
                    <m:brkBinSub m:val="--"/>
                    <m:smallFrac/>
                    <m:dispDef/>
                    <m:lMargin m:val="432"/>
                    <m:rMargin m:val="0"/>
                    <m:defJc m:val="left"/>
                    <m:wrapIndent m:val="1440"/>
                    <m:intLim m:val="subSup"/>
                    <m:naryLim m:val="undOvr"/>
                </m:mathPr>
                <w:attachedSchema w:val="ActionsPane3"/>
                <w:themeFontLang w:val="en-US"/>
                <w:clrSchemeMapping w:bg1="light1" w:t1="dark1" w:bg2="light2" w:t2="dark2" w:accent1="accent1"
                    w:accent2="accent2" w:accent3="accent3" w:accent4="accent4" w:accent5="accent5"
                    w:accent6="accent6" w:hyperlink="hyperlink" w:followedHyperlink="followedHyperlink"/>
                <w:decimalSymbol w:val="."/>
                <w:listSeparator w:val=","/>
            </w:settings>
        </xsl:result-document>
    </xsl:template>
    

    <!-- identity transform -->

    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="iden">
        <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="*" mode="iden">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="iden"
            />
        </xsl:copy>
    </xsl:template>

    <!-- drawings -->
    <xsl:template match="a:blip" mode="iden">
        <xsl:variable name="me" select="generate-id()"/>
        <a:blip>
            <xsl:variable name="rId">
                <xsl:for-each select="key('BLIP',1)">
                    <xsl:if test="generate-id()=$me">
                        <xsl:value-of select="concat('rId', string(200 + position()))"/>
                    </xsl:if>
                </xsl:for-each>
            </xsl:variable>
            <xsl:choose>
                <xsl:when test="@r:embed">
                    <xsl:attribute name="r:embed" select="$rId"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:attribute name="r:link" select="$rId"/>
                </xsl:otherwise>
            </xsl:choose>
        </a:blip>
    </xsl:template>

    <!-- formulas -->
    <xsl:template match="v:imagedata" mode="iden">
        <xsl:variable name="me" select="generate-id()"/>
        <v:imagedata>
            <xsl:attribute name="r:id">
                <xsl:for-each select="//v:imagedata">
                    <xsl:if test="generate-id()=$me">
                        <xsl:value-of select="concat('rId', string(1000 + position()))"/>
                    </xsl:if>
                </xsl:for-each>
            </xsl:attribute>
        </v:imagedata>
    </xsl:template>

    <xsl:template match="o:OLEObject" mode="iden">
        <xsl:variable name="me" select="generate-id()"/>
        <o:OLEObject>
            <!-- copy all attributes -->
            <xsl:copy-of select="@*"/>

            <!-- set rId -->
            <xsl:attribute name="r:id">
                <xsl:for-each select="//o:OLEObject">
                    <xsl:if test="generate-id()=$me">
                        <xsl:value-of select="concat('rId', string(2000 + position()))"/>
                    </xsl:if>
                </xsl:for-each>
            </xsl:attribute>
        </o:OLEObject>
    </xsl:template>

    <!-- Document title -->
    <xsl:template name="document-title">
      <xsl:choose>
	<xsl:when test="/tei:TEI/tei:text/tei:front/tei:titlePage">
	</xsl:when>
	<xsl:otherwise>
	  <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='main']">
	    <xsl:call-template name="block-element">
	      <xsl:with-param name="style">Title</xsl:with-param>
	    </xsl:call-template>
	  </xsl:for-each>
	  <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='sub']">
	    <xsl:call-template name="block-element">
	      <xsl:with-param name="style">Subtitle</xsl:with-param>
	    </xsl:call-template>
	  </xsl:for-each>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>

    <xsl:template match="tei:titlePage/tei:docTitle/tei:titlePart[@type='main']">
      <xsl:call-template name="block-element">
	<xsl:with-param name="style">Title</xsl:with-param>
      </xsl:call-template>
    </xsl:template>

    <xsl:template match="tei:titlePage/tei:docTitle/tei:titlePart[@type='sub']">
      <xsl:call-template name="block-element">
	<xsl:with-param name="style">Subtitle</xsl:with-param>
      </xsl:call-template>
    </xsl:template>

    <!-- place holders, used by ISO profile -->

    <xsl:template name="titlepages"/>
    <xsl:template name="generateTitle"/>
    <xsl:template name="created-by"/>
    <xsl:template name="headerParts"/>

    <!-- no handling of index terms -->

    <xsl:template match="tei:index"/>
</xsl:stylesheet>

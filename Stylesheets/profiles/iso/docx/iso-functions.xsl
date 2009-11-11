<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:cals="http://www.oasis-open.org/specs/tm9901"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">

    <xsl:import href="iso-variables.xsl"/>
    
    <!-- returns a listtype for a given stylename (return empty string to figure it out dynamically)-->
    <xsl:function name="teidocx:get-listtype" as="xs:string">
        <xsl:param name="style"/>
        <xsl:choose>
            <xsl:when test="$style=$Terms">
                <xsl:text>termlist</xsl:text>
            </xsl:when>
            <xsl:when test="$style=$TermNum">
                <xsl:text>termlist</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListBullet)">
                <xsl:text>unordered</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListContinue)">
                <xsl:text>unordered</xsl:text>
            </xsl:when>
            <xsl:when test="starts-with($style,$ListNumber)">
                <xsl:text>ordered</xsl:text>
            </xsl:when>
            <xsl:when test="$style=$List">
                <xsl:text>ordered</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text></xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- returns the correct heading style (return empty string to figure it out dynamically)-->
    <xsl:function name="teidocx:get-headingstyle" as="xs:string">
        <xsl:param name="element"/>
        <xsl:param name="level"/>
        
        <xsl:choose>
            <xsl:when test="$element/../@type='annex'">
                <xsl:value-of select="$ANNEX"/>
            </xsl:when>
            <xsl:when test="$element/parent::tei:div/@type='annexSection'">
                <xsl:value-of select="concat('a',$level)"/>
            </xsl:when>
            <xsl:when test="$element/ancestor::tei:div/@type='annex'">
                <xsl:value-of select="concat('a',$level)"/>
            </xsl:when>
            <xsl:when test="$element/../@type='bibliography'">
                <xsl:value-of select="$BibliographyHeading"/>
            </xsl:when>
            <xsl:when test="$element/../@type='foreword'">
                <xsl:value-of select="$ForewordHeading"/>
            </xsl:when>
            <xsl:when test="$element/../@type='introduction'">
                <xsl:value-of select="$IntroductionHeading"/>
            </xsl:when>
            
            <xsl:otherwise>
                <xsl:text></xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!--
        -->
    <xsl:function name="teidocx:is-supported-style" as="xs:boolean">
        <xsl:param name="style"/>
        <xsl:variable name="styles">
	  <names>
	    <name>ANNEX</name>
	    <name>Bibliography</name>
	    <name>Definition</name>
	    <name>Defterms</name>
	    <name>Example</name>
	    <name>ExtXref</name>
	    <name>Figure footnote</name>
	    <name>Figure title</name>
	    <name>FigureFootnoteXref</name>
	    <name>Footnote Reference</name>
	    <name>Footnote Text</name>
	    <name>Foreword</name>
	    <name>Formula</name>
	    <name>Index 1</name>
	    <name>Index heading</name>
	    <name>Introduction</name>
	    <name>List Continue 2</name>
	    <name>List Continue 3</name>
	    <name>List Continue 4</name>
	    <name>List Continue</name>
	    <name>List Number 2</name>
	    <name>List Number 3</name>
	    <name>List Number 4</name>
	    <name>List Number</name>
	    <name>Normal</name>
	    <name>Normal</name>
	    <name>Note</name>
	    <name>Note</name>
	    <name>Note</name>
	    <name>RefNorm</name>
	    <name>Special</name>
	    <name>Table footnote</name>
	    <name>Table text (10)</name>
	    <name>Table text (7)</name>
	    <name>Table text (8)</name>
	    <name>Table text (9)</name>
	    <name>Table title</name>
	    <name>TableFootNoteXref</name>
	    <name>TermNum</name>
	    <name>a2</name>
	    <name>a3</name>
	    <name>a4</name>
	    <name>a5</name>
	    <name>a6</name>
	    <name>committee_id</name>
	    <name>copyright_details</name>
	    <name>cover_warning</name>
	    <name>dl</name>
	    <name>document_details</name>
	    <name>document_title</name>
	    <name>heading 1</name>
	    <name>heading 2</name>
	    <name>heading 3</name>
	    <name>heading 4</name>
	    <name>heading 5</name>
	    <name>heading 6</name>
	    <name>id_no</name>
	    <name>isononumber</name>
	    <name>isonumber</name>
	    <name>p2</name>
	    <name>p3</name>
	    <name>p4</name>
	    <name>p5</name>
	    <name>p6</name>
	    <name>permission</name>
	    <name>possibility_and_capability</name>
	    <name>recommendation</name>
	    <name>reference_number</name>
	    <name>requirement</name>
	    <name>statement</name>
	    <name>termPreferred</name>
	    <name>toc 1</name>
	    <name>toc 2</name>
	    <name>toc 3</name>
	    <name>toc 4</name>
	    <name>toc 5</name>
	    <name>toc 6</name>
	    <name>toc 9</name>
	    <name>working_reference_number</name>
	    <name>zzBiblio</name>
	    <name>zzContents</name>
	    <name>zzCopyright</name>
	    <name>zzCover</name>
	    <name>zzForeword</name>
	    <name>zzIndex</name>
	    <name>zzSTDTitle</name>
	  </names>
	</xsl:variable>

        <xsl:variable name="realstyle">
            <xsl:value-of select="$style"/>
        </xsl:variable>
        
        <xsl:choose>
            <xsl:when test="$styles/names[name=$realstyle]">
                true
            </xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- defines whether or not a word paragraph is a first level heading -->
    <xsl:function name="teidocx:is-firstlevel-heading" as="xs:boolean">
        <xsl:param name="p"/>
        
        <xsl:choose>
            <xsl:when test="$p[w:pPr/w:pStyle/@w:val=$Heading1
                or w:pPr/w:pStyle/@w:val=$ANNEX
                or w:pPr/w:pStyle/@w:val=$ForewordHeading
                or w:pPr/w:pStyle/@w:val=$zzIntroductionHeading
                or w:pPr/w:pStyle/@w:val=$IntroductionHeading
                or w:pPr/w:pStyle/@w:val=$BibliographyHeading]">
                true
            </xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- defines whether or not a word paragraph is a  heading -->
    <xsl:function name="teidocx:is-heading" as="xs:boolean">
        <xsl:param name="p"/>
        <xsl:choose>
            <xsl:when
		test="$p[starts-with(w:pPr/w:pStyle/@w:val,'heading') 
                                or w:pPr/w:pStyle/@w:val=$ANNEX 
                                or w:pPr/w:pStyle/@w:val=$a2
                                or w:pPr/w:pStyle/@w:val=$a3
                                or w:pPr/w:pStyle/@w:val=$a4
                                or w:pPr/w:pStyle/@w:val=$a5
                                or w:pPr/w:pStyle/@w:val=$a6
                                or w:pPr/w:pStyle/@w:val=$BibliographyHeading 
                                or w:pPr/w:pStyle/@w:val=$ForewordHeading  
                                or w:pPr/w:pStyle/@w:val=$IntroductionHeading  
                                or w:pPr/w:pStyle/@w:val=$zzIntroductionHeading]">
                true
            </xsl:when>
            <xsl:otherwise>false</xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- Is given a header style and returns the style for the next level header -->
    <xsl:function name="teidocx:get-nextlevel-header" as="xs:string">
        <xsl:param name="current-header"/>
        <xsl:choose>
            <xsl:when test="$current-header=$ANNEX">
                <xsl:text>a2</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="translate($current-header,'12345678','23456789')"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>
    
    <!-- define special rendering -->
    <xsl:function name="teidocx:render-bold" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="starts-with(@rend,'specList-')">true</xsl:when>
                <xsl:when test="starts-with(parent::tei:hi/@rend,'specList-')">true</xsl:when>
                <xsl:when test="@rend='label'">true</xsl:when>
                <xsl:when test="contains(@rend,'bold')">true</xsl:when>
                <xsl:when test="parent::tei:hi[contains(@rend,'bold')]">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    <xsl:function name="teidocx:render-italic" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="parent::tei:item and self::tei:gloss">false</xsl:when>
                <xsl:when test="parent::tei:item and self::tei:term">false</xsl:when>
                <xsl:when test="self::tei:gloss">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    
    
    <xsl:function name="teidocx:is-inline" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
                <xsl:when test="self::m:oMath or
				self::mml:math or
				self::teidocx:dynamicContent or
				
                    self::w:drawing or
                    self::w:object or
                    
		    self::tei:formula[parent::cals:entry] or

		    self::tei:formula[parent::tei:title] or

                    self::iso:wordObject[w:object] or
                    
		    self::tbx:term or
		    self::tbx:descrip or

                    self::tei:abbr or
                    self::tei:affiliation or
                    self::tei:att or
                    self::tei:author or
                    self::tei:br or
                    self::tei:c or
                    self::tei:code or
                    self::tei:editionStmt or
                    self::tei:date or
                    self::tei:emph or
                    
                    self::tei:foreign or
                    self::tei:forename or
                    self::tei:graphic or
                    self::tei:gi or
                    self::tei:gloss[not(ancestor-or-self::*/@type='termsAndDefinitions')] or
                    self::tei:hi[not(w:*)] or
                    self::tei:idno or
                    self::tei:ident or
                    self::tei:lb or
                    self::tei:name or
                    self::tei:note[@place='foot'  or @place='bottom' ] or
                    self::tei:note[@place='end'] or
                    self::tei:num or
                    self::tei:mentioned or
                    self::tei:orgName or
                    self::tei:placeName or
                    self::tei:persName or
                    self::tei:publisher or
                    self::tei:pb or
                    self::tei:ptr or
                    self::tei:q[not(@type='sdt')] or
                    self::tei:ref or
                    self::tei:seg or
                    self::tei:surname or 
                    self::tei:soCalled or
                    self::tei:term[not(ancestor-or-self::*/@type='termsAndDefinitions')] or
                    self::tei:val or
                    self::tei:title">
                    true
                </xsl:when>
                <xsl:otherwise>
                    <xsl:choose>
                        <xsl:when test="empty($element/..)">false</xsl:when>
                        <xsl:when test="teidocx:is-inline($element/..)">true</xsl:when>
                        <xsl:otherwise>false</xsl:otherwise>
                    </xsl:choose>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>

</xsl:stylesheet>

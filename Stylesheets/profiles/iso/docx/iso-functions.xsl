<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" xmlns:iso="http://www.iso.org/ns/1.0"
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
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">

    <xsl:import href="variables.xsl"/>
    
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
        <xsl:variable name="styles">|ANNEX|Bibliography|committee_id|copyright_details|cover_warning|document_details|heading 1|a2|Definition|Formula|dl|document_title|Example|Note|Figure title|FigureFootnoteXref|Figure footnote|Footnote Reference|Footnote Text|Foreword|zzForeword|id_no|Index 1|Index heading|zzIndex|isonumber|isononumber|Normal|Introduction|List Number|List Number 2|List Number 3|List Number 4|List Continue|List Continue 2|List Continue 3|List Continue 4|ExtXref|RefNorm|Note|Normal|a3|a4|a5|a6|heading 2|heading 3|heading 4|heading 5|heading 6|p2|p3|p4|p5|p6|Note|permission|possibility_and_capability|reference_number|requirement|recommendation|Special|statement|Table text (9)|Table text (8)|Table text (7)|Table text (10)|Table title|TableFootNoteXref|TableFootNoteXref|Table footnote|zzContents|toc 1|toc 2|toc 3|toc 4|toc 5|toc 6|toc 9|Defterms|Term(s)|TermNum|working_reference_number|zzBiblio|zzCopyright|zzCover|zzSTDTitle|</xsl:variable>
        <xsl:variable name="realstyle">
            <xsl:value-of select="$style"/>
        </xsl:variable>
        
        <xsl:choose>
            <xsl:when test="contains($styles,concat('|',$realstyle,'|'))">
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
            <xsl:when test="$p[starts-with(w:pPr/w:pStyle/@w:val,'heading') 
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
                <xsl:when test="@rend='bold'">true</xsl:when>
                <xsl:when test="self::tei:term">true</xsl:when>
                <xsl:otherwise>false</xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:function>
    
    <xsl:function name="teidocx:render-italic" as="xs:boolean">
        <xsl:param name="element"/>
        <xsl:for-each select="$element">
            <xsl:choose>
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
                    
                    self::teidocx:dynamicContent or
                    
                    self::w:drawing or
                    self::w:object or
                    
                    self::iso:wordObject[w:object] or
                    
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
                    self::tei:q or
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

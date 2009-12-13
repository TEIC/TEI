<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="cals ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">

    <xsl:import href="iso-variables.xsl"/>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Returns a listtype for a given stylename (return empty string to
	figure it out dynamically)
      </desc>
    </doc>
    
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
	  <xsl:text/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Returns the correct heading style (return empty string to figure it
	out dynamically)
      </desc>
    </doc>
    
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
	  <xsl:text/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Defines whether or not a word paragraph is a first level heading 
      </desc>
    </doc>
    
    <xsl:function name="teidocx:is-firstlevel-heading" as="xs:boolean">
      <xsl:param name="p"/>
      
      <xsl:choose>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$Heading1]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ANNEX]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ForewordHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$zzIntroductionHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$IntroductionHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$BibliographyHeading]">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Defines whether or not a word paragraph is a  heading 
      </desc>
    </doc>
    
    <xsl:function name="teidocx:is-heading" as="xs:boolean">
      <xsl:param name="p"/>
      <xsl:choose>
	<xsl:when test="$p[starts-with(w:pPr/w:pStyle/@w:val,'heading')]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ANNEX]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$a2]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$a3]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$a4]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$a5]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$a6]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$BibliographyHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$ForewordHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$IntroductionHeading]">true</xsl:when>
	<xsl:when test="$p[w:pPr/w:pStyle/@w:val=$zzIntroductionHeading]">true</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Is given a header style and returns the style for the next level
	header 
      </desc>
    </doc>
    
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
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Define special rendering 
      </desc>
    </doc>
    
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
    
    <doc type="function" xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>
	Whether to render an element in italic 
      </desc>
    </doc>
    
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
    
    
    <!-- whether an element is block-level or inline -->
    <xsl:function name="teidocx:is-inline" as="xs:boolean">
      <xsl:param name="element"/>
      <xsl:for-each select="$element">
	<xsl:choose>
	  <xsl:when test="self::m:oMath">true</xsl:when>
	  <xsl:when test="self::mml:math">true</xsl:when>
	  <xsl:when
	      test="self::teidocx:dynamicContent">true</xsl:when>
	  <xsl:when test="self::w:drawing">true</xsl:when>
	  <xsl:when
	      test="self::tei:formula[parent::cals:entry]">true</xsl:when>
	  <xsl:when
	      test="self::tei:formula[parent::tei:title]">true</xsl:when>
	  <xsl:when test="self::tei:abbr">true</xsl:when>
	  <xsl:when test="self::tei:add">true</xsl:when>
	  <xsl:when test="self::tei:affiliation">true</xsl:when>
	  <xsl:when test="self::tei:att">true</xsl:when>
	  <xsl:when test="self::tei:author">true</xsl:when>
	  <xsl:when test="self::tei:br">true</xsl:when>
	  <xsl:when test="self::tei:c">true</xsl:when>
	  <xsl:when test="self::tei:code">true</xsl:when>
	  <xsl:when test="self::tei:del">true</xsl:when>
	  <xsl:when test="self::tei:editionStmt">true</xsl:when>
	  <xsl:when test="self::tei:date">true</xsl:when>
	  <xsl:when test="self::tei:emph">true</xsl:when>
	  <xsl:when test="self::tei:foreign">true</xsl:when>
	  <xsl:when test="self::tei:forename">true</xsl:when>
	  <xsl:when test="self::tei:graphic">true</xsl:when>
	  <xsl:when test="self::tei:gi">true</xsl:when>
	  <xsl:when
	      test="self::tei:gloss[not(ancestor-or-self::*/@type='termsAndDefinitions')]">true</xsl:when>
	  <xsl:when test="self::tei:hi[not(w:*)]">true</xsl:when>
	  <xsl:when test="self::tei:idno">true</xsl:when>
	  <xsl:when test="self::tei:ident">true</xsl:when>
	  <xsl:when test="self::tei:lb">true</xsl:when>
	  <xsl:when test="self::tei:name">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='foot']">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='bottom']">true</xsl:when>
	  <xsl:when test="self::tei:note[@place='end']">true</xsl:when>
	  <xsl:when test="self::tei:num">true</xsl:when>
	  <xsl:when test="self::tei:mentioned">true</xsl:when>
	  <xsl:when test="self::tei:orgName">true</xsl:when>
	  <xsl:when test="self::tei:placeName">true</xsl:when>
	  <xsl:when test="self::tei:persName">true</xsl:when>
	  <xsl:when test="self::tei:publisher">true</xsl:when>
	  <xsl:when test="self::tei:pb">true</xsl:when>
	  <xsl:when test="self::tei:ptr">true</xsl:when>
	  <xsl:when
	      test="self::tei:q[not(@type='sdt')]">true</xsl:when>
	  <xsl:when test="self::tei:ref">true</xsl:when>
	  <xsl:when test="self::tei:seg">true</xsl:when>
	  <xsl:when test="self::tei:surname">true</xsl:when>
	  <xsl:when test="self::tei:soCalled">true</xsl:when>
	  <xsl:when
	      test="self::tei:term[not(ancestor-or-self::*/@type='termsAndDefinitions')]">true</xsl:when>
	  <xsl:when test="self::tei:val">true</xsl:when>
	  <xsl:when test="self::tei:title">true</xsl:when>
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
    
    <!-- whether a Word style is supported -->
    <xsl:function name="teidocx:is-supported-style" as="xs:boolean">
      <xsl:param name="style"/>
      <xsl:variable name="styles">
	<names>
	  <name>a2</name>
	  <name>a3</name>
	  <name>a4</name>
	  <name>a5</name>
	  <name>a6</name>
	  <name>abbreviatedForm</name>
	  <name>ANNEX</name>
	  <name>autoTermNum1</name>
	  <name>autoTermNum2</name>
	  <name>autoTermNum3</name>
	  <name>autoTermNum4</name>
	  <name>autoTermNum5</name>
	  <name>autoTermNum6</name>
	  <name>autoTermNumA2</name>
	  <name>autoTermNumA3</name>
	  <name>autoTermNumA4</name>
	  <name>autoTermNumA5</name>
	  <name>autoTermNumA6</name>
	  <name>Bibliography Heading</name>
	  <name>Bibliography</name>
	  <name>committee_id</name>
	  <name>copyright_details</name>
	  <name>cover_warning</name>
	  <name>date</name>
	  <name>Definition</name>
	  <name>dl</name>
	  <name>document_details</name>
	  <name>document_edition</name>
	  <name>document_language</name>
	  <name>document_stage</name>
	  <name>document_subtype</name>
	  <name>document_title</name>
	  <name>document_type</name>
	  <name>domain</name>
	  <name>draft_number</name>
	  <name>entrySource</name>
	  <name>Example list</name>
	  <name>Example</name>
	  <name>ExtXref</name>
	  <name>Figure footnote</name>
	  <name>Figure key</name>
	  <name>Figure note</name>
	  <name>Figure subtitle</name>
	  <name>Figure text</name>
	  <name>Figure title</name>
	  <name>Figure units</name>
	  <name>FigureFootnoteXref</name>
	  <name>Footer</name>
	  <name>Footnote Reference</name>
	  <name>Footnote Text</name>
	  <name>Foreword</name>
	  <name>Formula</name>
	  <name>gender</name>
	  <name>geographicalUse</name>
	  <name>Header</name>
	  <name>Heading1</name>
	  <name>Heading2</name>
	  <name>Heading3</name>
	  <name>Heading4</name>
	  <name>Heading5</name>
	  <name>Heading6</name>
	  <name>Hyperlink</name>
	  <name>id_no</name>
	  <name>Index 1</name>
	  <name>Index Heading</name>
	  <name>Introduction</name>
	  <name>language</name>
	  <name>List Continue 2</name>
	  <name>List Continue 3</name>
	  <name>List Continue 4</name>
	  <name>List Continue</name>
	  <name>List Number 2</name>
	  <name>List Number 3</name>
	  <name>List Number 4</name>
	  <name>List Number</name>
	  <name>nonVerbalRepresentation</name>
	  <name>Normal</name>
	  <name>Note list</name>
	  <name>Note</name>
	  <name>noteDefinition</name>
	  <name>noteExample</name>
	  <name>noteNonVerbalRepresentation</name>
	  <name>noteSymbol</name>
	  <name>noteTerm</name>
	  <name>noteTermEntry</name>
	  <name>number</name>
	  <name>p2</name>
	  <name>p3</name>
	  <name>p4</name>
	  <name>p5</name>
	  <name>p6</name>
	  <name>partOfSpeech</name>
	  <name>pronunciation</name>
	  <name>RefNorm</name>
	  <name>script</name>
	  <name>source</name>
	  <name>Special</name>
	  <name>symbol</name>
	  <name>Table footnote</name>
	  <name>Table note</name>
	  <name>Table text (10)</name>
	  <name>Table text (7)</name>
	  <name>Table text (8)</name>
	  <name>Table text (9)</name>
	  <name>Table title</name>
	  <name>Table units</name>
	  <name>TableFootnoteXref</name>
	  <name>termAdmitted</name>
	  <name>termDeprecated</name>
	  <name>termHeading2</name>
	  <name>termHeading3</name>
	  <name>termHeading4</name>
	  <name>termHeading5</name>
	  <name>termHeading6</name>
	  <name>TermNum</name>
	  <name>termPreferred</name>
	  <name>termRef</name>
	  <name>TOC1</name>
	  <name>TOC2</name>
	  <name>TOC3</name>
	  <name>TOC4</name>
	  <name>TOC5</name>
	  <name>TOC6</name>
	  <name>TOC9</name>
	  <name>zzBiblio</name>
	  <name>zzContents</name>
	  <name>zzCopyright</name>
	  <name>zzCover</name>
	  <name>zzForeword</name>
	  <name>zzHelp</name>
	  <name>zzIndex</name>
	  <name>zzSTDTitle</name>
	</names>
      </xsl:variable>
      
      <xsl:choose>
	<xsl:when test="$styles/names[name=$style]">
	  true
	</xsl:when>
	<xsl:otherwise>false</xsl:otherwise>
      </xsl:choose>
    </xsl:function>
    
    
  </xsl:stylesheet>
  
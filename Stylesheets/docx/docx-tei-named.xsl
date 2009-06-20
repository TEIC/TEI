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
    
    <!--xsl:import href="omml2mml.xsl"/-->
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
    
    <xsl:param name="mathMethod">omml</xsl:param>
    <xsl:param name="termMethod">tei</xsl:param>
    <xsl:param name="tableMethod">tei</xsl:param>
    
    <xsl:param name="word-directory">.</xsl:param>
    
    
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
        <xsl:variable name="preceedingTableTitle" select="preceding-sibling::w:p[w:pPr/w:pStyle/@w:val='TableTitle'
            or w:pPr/w:pStyle/@w:val=$Tabletitle][1]"/>
        <xsl:if test="$preceedingTableTitle and $preceedingTableTitle/following-sibling::w:tbl[1] and generate-id()=generate-id($preceedingTableTitle/following-sibling::w:tbl[1])">
            <head>
                <xsl:apply-templates select="$preceedingTableTitle"/>
            </head>
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
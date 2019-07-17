<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
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
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:con="http://schemas.openxmlformats.org/package/2006/content-types"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="cp con ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn xsi dc dcterms dcmitype">
    
    <xsl:output method="xml" version="1.0" encoding="UTF-8"/>
    
    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($wordDirectory, '/word/styles.xml')"/>
    </xsl:param>
    
    <xsl:key name="Styles" match="w:style/w:name" use="@w:val"/>
    
    <xsl:template match="/">
        <xsl:variable name="comments">
            <xsl:apply-templates mode="comments"/>
        </xsl:variable>
        <xsl:variable name="relations">
            <xsl:apply-templates select="doc(concat($wordDirectory,'/word/_rels/document.xml.rels'))"
                              mode="relations"/>
        </xsl:variable>
        <xsl:variable name="content-typeS">
            <xsl:apply-templates mode="content-types" select="doc(concat($wordDirectory,'/Content_Types.xml'))"/>
        </xsl:variable>
        
        <xsl:message>produce actual document</xsl:message>
        <xsl:apply-templates mode="document"/>    
        
        <xsl:message>write out relations</xsl:message>
        <xsl:call-template name="write-out-relations">
            <xsl:with-param name="relations" select="$relations"/>
        </xsl:call-template>

        <xsl:message>write out content types</xsl:message>
        <xsl:call-template name="write-out-content-types">
            <xsl:with-param name="types" select="$content-types"/>
        </xsl:call-template>
        

        <xsl:message>write out comments</xsl:message>
        <xsl:call-template name="write-out-comments">
            <xsl:with-param name="comments" select="$comments"/>
        </xsl:call-template>
        
        <xsl:message>lock down document</xsl:message>
        <xsl:call-template name="lock-down-document"/>
    </xsl:template>
    
    <!-- identity transform  document-->
    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="document">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="document">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="document"/>
        </xsl:copy>
    </xsl:template>
    
    <!-- identity transform relations -->
    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="relations">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="relations">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="document"/>
        </xsl:copy>
    </xsl:template>
    
    <!-- copy nothing for comments -->
    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="comments"/>
        
    
    <xsl:template match="*" mode="comments">
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="comments"/>
    </xsl:template>
    
    <!-- identity transform content types -->
    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="content-types">
        <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="content-types">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="content-types"/>
        </xsl:copy>
    </xsl:template>    

    <!-- comment helper -->
    <xsl:template name="commentNote">
        <xsl:param name="id"/>
        <xsl:param name="text"/>
        <w:comment w:id="{$id}" w:author="TEIISO" w:date="2008-09-28T14:17:00Z" w:initials="SPQR">
            <w:p>
                <w:pPr>
                    <w:pStyle w:val="CommentText"/>
                </w:pPr>
                <w:r>
                    <w:annotationRef/>
		</w:r>
		<xsl:apply-templates select="$text"/>
            </w:p>
        </w:comment>
    </xsl:template>
    
    <!-- put back correct styles -->
    <xsl:template match="w:pStyle" mode="document">
        <w:pStyle>
            <xsl:attribute name="w:val">
                <xsl:call-template name="getStyleName">
                    <xsl:with-param name="in" select="@w:val"/>
                </xsl:call-template>
            </xsl:attribute>
        </w:pStyle>
    </xsl:template>
    
    <xsl:template match="w:rStyle" mode="document">
        <w:rStyle>
            <xsl:attribute name="w:val">
                <xsl:call-template name="getStyleName">
                    <xsl:with-param name="in" select="@w:val"/>
                </xsl:call-template>
            </xsl:attribute>
        </w:rStyle>
    </xsl:template>
    

    <xsl:template name="getStyleName">
        <xsl:param name="in"/>
        <xsl:for-each select="document($styleDoc,/)">
            <xsl:for-each select="key('Styles',$in)">
                <xsl:value-of select="parent::w:style/@w:styleId"/>
            </xsl:for-each>
        </xsl:for-each>
    </xsl:template>
    
    
    <!-- content types -->
    <xsl:template match="con:Override[@PartName='/word/comments.xml']"/>
    <!-- write out content-types -->
    <xsl:template name="write-out-content-types">
        <xsl:param name="types"/>
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'/Content_Types_new.xml')"/>
         </xsl:message>
	     </xsl:if>
        <xsl:result-document href="{concat($wordDirectory,'/Content_Types_new.xml')}" standalone="yes">
            <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
                <xsl:for-each select="$types/child::node()/*">
                    <xsl:copy-of select="."/>
                </xsl:for-each>
                <Override PartName="/word/comments.xml"
                      ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml"/>
            </Types>
        </xsl:result-document>
    </xsl:template>
    
    
    <!-- write out relations -->
    <xsl:template name="write-out-relations">
        <xsl:param name="relations"/>
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'word/_rels/document_new.xml.rels')"/>
         </xsl:message>
	     </xsl:if>
        <xsl:result-document href="{concat($wordDirectory,'/word/_rels/document_new.xml.rels')}"
                           standalone="yes">
            <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
                <xsl:for-each select="$relations/child::node()/*">
                    <xsl:copy-of select="."/>
                </xsl:for-each>
                <Relationship Id="rId9999"
                          Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments"
                          Target="comments.xml"/>
            </Relationships>
        </xsl:result-document>
    </xsl:template>
    
    <!-- write out comments -->
    <xsl:template name="write-out-comments">
        <xsl:param name="comments"/>
	     <xsl:if test="$debug='true'">
	        <xsl:message>Writing out <xsl:value-of select="concat($wordDirectory,'word/_rels/comments.xml')"/>
         </xsl:message>
	     </xsl:if>
        <xsl:result-document href="{concat($wordDirectory,'/word/comments.xml')}" standalone="yes">
            <w:comments xmlns:mv="urn:schemas-microsoft-com:mac:vml"
                     xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main">
                <xsl:copy-of select="$comments"/>
            </w:comments>
        </xsl:result-document>
    </xsl:template>
    
    <!-- writes settings.xml and app.xml to lock down the document -->
    <xsl:template name="lock-down-document">
        <xsl:result-document href="{concat($wordDirectory,'/docProps/app.xml')}" standalone="yes">
            <Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
                     xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">
                <Template>STD_3_0_0.dotx</Template>
                <Application>TEIISO tei-docx.xsl</Application>
                <DocSecurity>8</DocSecurity>
                <SharedDoc>true</SharedDoc>
                <AppVersion>1.0</AppVersion>
            </Properties>
        </xsl:result-document>
        
        <xsl:result-document href="{concat($wordDirectory,'/word/settings.xml')}" standalone="yes">
            <w:settings xmlns:sl="http://schemas.openxmlformats.org/schemaLibrary/2006/main">
                <w:zoom w:percent="100"/>
                <w:attachedTemplate r:id="rId1"/>
                <w:linkStyles/>
                <w:doNotTrackMoves/>
                <w:documentProtection w:edit="readOnly" w:enforcement="1"/>
                <w:defaultTabStop w:val="720"/>
                <w:hyphenationZone w:val="425"/>
                <w:evenAndOddHeaders/>
                <w:drawingGridHorizontalSpacing w:val="110"/>
                <w:displayHorizontalDrawingGridEvery w:val="2"/>
                <w:characterSpacingControl w:val="doNotCompress"/>
                <w:hdrShapeDefaults>
                    <o:shapedefaults v:ext="edit" spidmax="2050"/>
                </w:hdrShapeDefaults>
                <w:compat/>
                <w:rsids/>
                <m:mathPr>
                    <m:mathFont m:val="MT Symbol"/>
                    <m:brkBin m:val="before"/>
                    <m:brkBinSub m:val="--"/>
                    <m:smallFrac m:val="off"/>
                    <m:dispDef/>
                    <m:lMargin m:val="0"/>
                    <m:rMargin m:val="0"/>
                    <m:defJc m:val="centerGroup"/>
                    <m:wrapIndent m:val="1440"/>
                    <m:intLim m:val="subSup"/>
                    <m:naryLim m:val="undOvr"/>
                </m:mathPr>
                <w:attachedSchema w:val="ActionsPane3"/>
                <w:themeFontLang w:val="en-GB"/>
                <w:clrSchemeMapping w:bg1="light1" w:t1="dark1" w:bg2="light2" w:t2="dark2" w:accent1="accent1"
                                w:accent2="accent2"
                                w:accent3="accent3"
                                w:accent4="accent4"
                                w:accent5="accent5"
                                w:accent6="accent6"
                                w:hyperlink="hyperlink"
                                w:followedHyperlink="followedHyperlink"/>
                <w:shapeDefaults>
                    <o:shapedefaults v:ext="edit" spidmax="2050"/>
                    <o:shapelayout v:ext="edit">
                        <o:idmap v:ext="edit" data="1"/>
                    </o:shapelayout>
                </w:shapeDefaults>
                <w:decimalSymbol w:val="."/>
                <w:listSeparator w:val=","/>
            </w:settings>
            
        </xsl:result-document>
    </xsl:template>
</xsl:stylesheet>
<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:iso="http://www.iso.org/ns/1.0"
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
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                version="2.0"
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn">
    
    <xsl:import href="docx/iso-functions.xsl"/>
    <xsl:import href="../../docx/check-docx-and-annotate.xsl"/>
    
    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($word-directory, '/word/styles.xml')"/>
    </xsl:param>
    
    
    <!-- use only iso styles for paragraphs-->
    <xsl:template match="w:p[w:pPr/w:pStyle and not(teidocx:is-supported-style(w:pPr/w:pStyle/@w:val))]"
                 mode="document">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number"/>
        <w:p>
            <xsl:for-each select="*">
                <xsl:apply-templates select="." mode="document"/>
                <xsl:if test="self::w:pPr">
                    <w:commentRangeStart w:id="{$id}"/>        
                </xsl:if>
            </xsl:for-each>
            <w:commentRangeEnd w:id="{$id}"/>
            <w:r>
                <w:commentReference w:id="{$id}"/>
            </w:r>
        </w:p>
    </xsl:template>
    
    <xsl:template match="w:p[w:pPr/w:pStyle and not(teidocx:is-supported-style(w:pPr/w:pStyle/@w:val))]"
                 mode="comments">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number"/>
        <xsl:call-template name="create-comment">
            <xsl:with-param name="id" select="$id"/>
            <xsl:with-param name="text">Use of unsupported style detected: <xsl:value-of select="w:pPr/w:pStyle/@w:val"/>
         </xsl:with-param>
        </xsl:call-template>

        <xsl:apply-templates mode="comments"/>
    </xsl:template>
    
    <!-- use only iso styles for runs-->
    <xsl:template match="w:p/w:r[w:rPr/w:rStyle and not(teidocx:is-supported-style(w:rPr/w:rStyle/@w:val))]"
                 mode="document">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number+5000"/>
        <w:commentRangeStart w:id="{$id}"/>
        <w:r>
            <xsl:apply-templates mode="document"/>
        </w:r>
        <w:commentRangeEnd w:id="{$id}"/>
        <w:r>
            <w:commentReference w:id="{$id}"/>
        </w:r>
    </xsl:template>
    
    <xsl:template match="w:p/w:r[w:rPr/w:rStyle and not(teidocx:is-supported-style(w:rPr/w:rStyle/@w:val))]"
                 mode="comments">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number+5000"/>
        <xsl:call-template name="create-comment">
            <xsl:with-param name="id" select="$id"/>
            <xsl:with-param name="text">Use of unsupported style detected: <xsl:value-of select="w:rPr/w:rStyle/@w:val"/>
         </xsl:with-param>
        </xsl:call-template>
        <xsl:next-match/>
    </xsl:template>
    
    <!-- check for wrongly numbered headings -->
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='a2' or w:pPr/w:pStyle/@w:val='a3' or w:pPr/w:pStyle/@w:val='a4' or w:pPr/w:pStyle/@w:val='a5']"
                 mode="document">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number+10000"/>
        <xsl:variable name="text">
            <xsl:value-of select="."/>
        </xsl:variable>
        <xsl:variable name="me" select="."/>
        
        <w:p>
            <xsl:analyze-string select="$text" regex="^\w\.\d">
                <xsl:matching-substring>
                    <xsl:for-each select="$me/*">
                        <xsl:apply-templates select="." mode="document"/>
                        <xsl:if test="self::w:pPr">
                            <w:commentRangeStart w:id="{$id}"/>        
                        </xsl:if>
                    </xsl:for-each>
                    <w:commentRangeEnd w:id="{$id}"/>
                    <w:r>
                        <w:commentReference w:id="{$id}"/>
                    </w:r>
                </xsl:matching-substring>
                <xsl:non-matching-substring>
                    <xsl:apply-templates select="$me/*" mode="document"/>--&gt;
                </xsl:non-matching-substring>
            </xsl:analyze-string>
        </w:p>
    </xsl:template>
    <xsl:template match="w:p[w:pPr/w:pStyle/@w:val='a2' or w:pPr/w:pStyle/@w:val='a3' or w:pPr/w:pStyle/@w:val='a4' or w:pPr/w:pStyle/@w:val='a5']"
                 mode="comments">
        <xsl:variable name="number">
            <xsl:number select="." level="any"/>
        </xsl:variable>
        <xsl:variable name="id" select="$number+10000"/>
        <xsl:variable name="text">
            <xsl:value-of select="."/>
        </xsl:variable>
        <xsl:analyze-string select="$text" regex="^\w\.\d">
            <xsl:matching-substring>
                <xsl:call-template name="create-comment">
                    <xsl:with-param name="id" select="$id"/>
                    <xsl:with-param name="text">Probably wrong use of numbering</xsl:with-param>
                </xsl:call-template>
            </xsl:matching-substring>
        </xsl:analyze-string>
        <xsl:apply-templates mode="comments"/>
    </xsl:template>
</xsl:stylesheet>
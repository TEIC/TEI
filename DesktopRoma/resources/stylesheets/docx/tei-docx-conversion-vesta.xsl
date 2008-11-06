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
    <!-- import conversion style -->
    <xsl:import href="tei-docx.xsl"/>
    
    <!-- import functions -->
    <xsl:include href="vesta-functions.xsl"/>
    
    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="debug">false</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($word-directory, '/word/styles.xml')"/>
    </xsl:param>
    
    <!-- Styles -->
    
    <xsl:template match="tei:abbr" mode="get-style">abbr</xsl:template>
    <xsl:template match="tei:cit" mode="get-style">Quote</xsl:template>
    <xsl:template match="tei:date" mode="get-style">date</xsl:template>
    <xsl:template match="tei:formula" mode="get-style">Formula</xsl:template>
    <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:abbr" mode="get-style">ExtRef</xsl:template>
    <xsl:template match="tei:mentioned" mode="get-style">mentioned</xsl:template>
    <xsl:template match="tei:orgName" mode="get-style">orgName</xsl:template>
    <xsl:template match="tei:p[@rend]" mode="get-style">
        <xsl:call-template name="getStyleName">
            <xsl:with-param name="in" select="@rend"/>
        </xsl:call-template>
    </xsl:template>
    <xsl:template match="tei:quote" mode="get-style">Quote</xsl:template>
    <xsl:template match="tei:ref" mode="get-style">ExtXref</xsl:template>
    <xsl:template match="tei:seg[@rend='FormulaReference']">FormulaReference</xsl:template>
    <xsl:template match="tei:seg[@iso:provision]" mode="get-style"><xsl:value-of select="@iso:provision"/></xsl:template>
    <xsl:template match="tei:seg[@rend]" mode="get-style"><xsl:value-of select="@rend"/></xsl:template>
    
    <!-- 
        Inline Templates:
        Here we can overwrite how inline elements are rendered
    -->

    <xsl:template match="tei:c[@iso:font and @n]">
        <w:r>
            <w:sym w:font="{@iso:font}" w:char="{@n}"/>
        </w:r>
    </xsl:template>
    
    <xsl:template match="tei:editionStmt">
        <w:r>
            <w:t><xsl:value-of select="tei:edition"/> Edition</w:t>
        </w:r>
    </xsl:template>

    
    
    <xsl:template match="tei:pb">
        <w:r>
            <w:br w:type="page"/>
        </w:r>
    </xsl:template>
    
    <xsl:template match="tei:seg[not(@*) and normalize-space(.)='']">
        <w:r>
            <w:t>
                <xsl:attribute name="xml:space">preserve</xsl:attribute>
                <xsl:text> </xsl:text>
            </w:t>
        </w:r>
    </xsl:template>

    
    <!-- 
        Block Templates:
        Here we can overwrite how block elements are rendered
    -->
  
    
    <!-- Dates -->
    <xsl:template match="tei:date">
        <w:r>
            <w:rPr>
                <w:rStyle w:val="date"/>
            </w:rPr>
            <w:t>
                <xsl:value-of select="."/>
            </w:t>
        </w:r>
    </xsl:template>

    <!-- formulas -->
    <xsl:template match="tei:formula">
        <w:p>    
            <w:pPr>
                <w:pStyle w:val="Formula"/>
            </w:pPr>
            <xsl:call-template name="block-element">                   
                <xsl:with-param name="nop">true</xsl:with-param>
            </xsl:call-template>
            <xsl:if test="@n">
                <w:r>
                    <w:tab/>
                </w:r>
                <w:r>
                    <w:rPr>
                        <w:rStyle w:val="FormulaReference"/>
                    </w:rPr>
                    <w:t xml:space="preserve"><xsl:value-of select="@n"/></w:t>
                </w:r>
            </xsl:if>
        </w:p>
    </xsl:template>
    
    <!-- List Bibl -->
    <xsl:template match="tei:listBibl">
        <xsl:choose>
            <xsl:when test="ancestor-or-self::*[@type='normativeReferences']">
                <xsl:for-each select="tei:bibl">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="pPr">
                            <w:pPr>
                                <w:pStyle>
                                    <xsl:attribute name="w:val">
                                        <xsl:call-template name="getStyleName">
                                            <xsl:with-param name="in">
                                                <xsl:text>RefNorm</xsl:text>
                                            </xsl:with-param>
                                        </xsl:call-template>
                                    </xsl:attribute>
                                </w:pStyle>
                            </w:pPr>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <xsl:for-each select="tei:bibl">
                    <xsl:call-template name="block-element">
                        <xsl:with-param name="style">Bibliography</xsl:with-param>
                    </xsl:call-template>
                </xsl:for-each>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <!-- 
        Special Notes (Footnotes) .. 
        @TODO: Ideally this should go into the general template, but for some
        reason xsl always calls the less specific tei:note template in here. 
    -->
    <xsl:template match="tei:note[@place]">
        <xsl:choose>
            <xsl:when test="@place='foot'">
                <xsl:call-template name="create-footnote"/>
            </xsl:when>
            <xsl:when test="@place='end'">
                <xsl:call-template name="create-endnote"/>
            </xsl:when>
        </xsl:choose>
    </xsl:template>
    
    
    
    <!-- Paragraphs in the front matter -->
    <xsl:template match="tei:front/tei:div/tei:p">
        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr">
                <w:pPr>
                    <w:pStyle>
                        <xsl:attribute name="w:val">
                            <xsl:value-of
                                select="concat(translate(substring(parent::tei:div/@type,1,1),$lowercase,$uppercase),substring(parent::tei:div/@type,2))"/>
                        </xsl:attribute>
                    </w:pStyle>
                </w:pPr>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>
   
    
    
    <!-- who created this document -->
    <xsl:template name="created-by">
        <xsl:text>Vesta</xsl:text>
    </xsl:template>


    <!-- fake listPerson into an unordered list -->
  <xsl:template match="tei:listPerson">
    <xsl:variable name="mylist">
      <list type="ordered">
	<xsl:apply-templates/>
      </list>
    </xsl:variable>
    <xsl:apply-templates select="$mylist"/>
  </xsl:template>

  <xsl:template match="tei:person">
    <tei:item>
      <xsl:copy-of select="*|text()"/>
    </tei:item>
  </xsl:template>

  <xsl:template match="tei:affiliation">
    <w:r>
      <w:br/>
    </w:r>
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
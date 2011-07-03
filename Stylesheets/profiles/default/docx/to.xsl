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
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn tei teidocx">
    <!-- import conversion style -->

    <xsl:import href="../../../docx/to/teitodocx.xsl"/>
    
    <!-- import functions -->
    <xsl:import href="default-functions.xsl"/>
    
    <xsl:param name="renderAddDel">true</xsl:param>
    <xsl:param name="addColour">red</xsl:param>

    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for making Word docx files from TEI XML (see tei-docx.xsl)</p>
         <p> This library is free software; you can redistribute it and/or
            modify it under the terms of the GNU Lesser General Public License as
            published by the Free Software Foundation; either version 2.1 of the
            License, or (at your option) any later version. This library is
            distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
            without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
            PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
            details. You should have received a copy of the GNU Lesser General Public
            License along with this library; if not, write to the Free Software
            Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
    
    <xsl:param name="bulletOne">•</xsl:param>
    <xsl:param name="bulletTwo"></xsl:param>

    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="debug">false</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($word-directory, '/word/styles.xml')"/>
    </xsl:param>
    
    <!-- Styles -->
    
    <xsl:template match="tei:abbr" mode="get-style">abbr</xsl:template>
    <xsl:template match="tei:cit" mode="get-style">Quote</xsl:template>
    <xsl:template match="tei:date" mode="get-style">date</xsl:template>
    <xsl:template match="tei:foreign" mode="get-style">teiforeign</xsl:template>
    <xsl:template match="tei:formula" mode="get-style">Formula</xsl:template>
    <xsl:template match="tei:mentioned" mode="get-style">teimentioned</xsl:template>
    <xsl:template match="tei:orgName" mode="get-style">orgName</xsl:template>
    <xsl:template match="tei:quote" mode="get-style">Quote</xsl:template>
    <xsl:template match="tei:ref[@rend and not(@target)]" mode="get-style"><xsl:value-of select="@rend"/></xsl:template>
    <xsl:template match="tei:seg[@rend]" mode="get-style"><xsl:value-of select="@rend"/></xsl:template>

    <xsl:template match="tei:p[@rend]" mode="get-style">
        <xsl:call-template name="getStyleName">
            <xsl:with-param name="in" select="@rend"/>
        </xsl:call-template>
    </xsl:template>

       <xsl:template match="tei:editionStmt">
        <w:r>
            <w:t>
            <xsl:value-of select="tei:edition"/> Edition</w:t>
        </w:r>
    </xsl:template>

    <xsl:template match="tei:label[following-sibling::tei:*[1]/self::tei:item]">
        <xsl:param name="nop"/>   
	<xsl:variable name="pair">
	  <tei:list>
	    <tei:glossListEntry count="{count(ancestor::tei:list)}">
	      <tei:hi rend="bold">
		<xsl:apply-templates mode="iden"/>
	      </tei:hi>
	      <tei:lb/>
	      <xsl:for-each select="following-sibling::tei:item[1]">
		<xsl:apply-templates mode="iden"/>
	      </xsl:for-each>
	    </tei:glossListEntry>
	  </tei:list>
	</xsl:variable>
	<xsl:apply-templates select="$pair/tei:list/tei:glossListEntry"/>
    </xsl:template>

    <xsl:template match="tei:glossListEntry">
      <xsl:call-template name="block-element">
	<xsl:with-param name="style">dl</xsl:with-param>
	<xsl:with-param name="pPr">
	  <w:pPr>
	    <w:pStyle w:val="dl"/>
	    <w:ind w:left="360" w:hanging="360"/>
	  </w:pPr>
	</xsl:with-param>
      </xsl:call-template>
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
    <xsl:template match="tei:date[ancestor::tei:teiHeader]">
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
    
     
    <!-- 
        Special Notes (Footnotes) .. 
        @TODO: Ideally this should go into the general template, but for some
        reason xsl always calls the less specific tei:note template in here. 
    -->
    <xsl:template match="tei:note[@place]">
        <xsl:choose>
            <xsl:when test="@place='foot'  or @place='bottom' or @place='tablefoot' ">
                <xsl:call-template name="create-footnote"/>
            </xsl:when>
            <xsl:when test="@place='end'">
                <xsl:call-template name="create-endnote"/>
            </xsl:when>
	    <xsl:when test="@place='comment'">
	      <xsl:call-template name="create-comment"/>
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
                            <xsl:value-of select="concat(upper-case(substring(parent::tei:div/@type,1,1)),substring(parent::tei:div/@type,2))"/>
                        </xsl:attribute>
                    </w:pStyle>
                </w:pPr>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>
   
    
    
    <!-- who created this document -->
    <xsl:template name="created-by">
        <xsl:text>TEI XSL</xsl:text>
    </xsl:template>


    <!-- fake listPerson into an unordered list -->
  <xsl:template match="tei:listPerson">
      <xsl:variable name="mylist">
         <tei:list type="unordered">
	           <xsl:apply-templates/>
         </tei:list>
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


    <xsl:template name="defineOrderedLists">
      <w:abstractNum w:abstractNumId="3">
                    <w:multiLevelType w:val="multilevel"/>
                    <w:lvl w:ilvl="0">
                        <w:start w:val="1"/>
                        <w:numFmt w:val="decimal"/>
                        <w:pStyle w:val="ListNumber"/>
                        <w:lvlText w:val="%1."/>
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
                        <w:numFmt w:val="lowerLetter"/>
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

    <xsl:template match="@*|text()|comment()|processing-instruction()" mode="iden">
      <xsl:copy-of select="."/>
    </xsl:template>
    
    <xsl:template match="*" mode="iden">
      <xsl:copy>
	        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="iden"/>
      </xsl:copy>
    </xsl:template>
 

</xsl:stylesheet>

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:cals="http://www.oasis-open.org/specs/tm9901"
    xmlns:iso="http://www.iso.org/ns/1.0"
    xmlns:its="http://www.w3.org/2005/11/its"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" 
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:v="urn:schemas-microsoft-com:vml" xmlns:fn="http://www.w3.org/2005/02/xpath-functions"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="teidocx cals xd ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn its">
    <!-- import conversion style -->
    <xsl:import href="../../../docx/to/to.xsl"/>
    <xsl:import href="../isoutils.xsl"/>
    
    <!-- import functions -->
    <xsl:include href="iso-functions.xsl"/>

<doc type="stylesheet" xmlns="http://www.pnp-software.com/XSLTdoc">
    <short>TEI stylesheet to convert TEI XML to Word DOCX XML.</short>
    <detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  
      </detail>
    <author>See AUTHORS</author>
    <cvsId>$Id$</cvsId>
    <copyright>2008, TEI Consortium</copyright>
</doc>

    <xsl:param name="template">ISO</xsl:param>

    <xsl:variable name="align">
      <xsl:choose>
    	<xsl:when test="$template='ISO'">left</xsl:when>
	    <xsl:otherwise>right</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="debug">false</xsl:param>   
    <xsl:param name="numberFormat">fr</xsl:param>
    
    <xsl:variable name="orig" select="/"/>

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
    <xsl:template match="tei:ref[@rend]" mode="get-style"><xsl:value-of select="@rend"/></xsl:template>
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

    <!-- 
        Handle Numbers 
    -->
    <xsl:template match="tei:num">
        <w:r>
            <w:rPr>
                <w:rStyle w:val="isonumber"/>
            </w:rPr>
            <w:t>
                <xsl:choose>
                    <xsl:when test="$numberFormat='fr'">
                        <xsl:value-of select="."/>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="translate(.,',&#160;','.,')"/>
                    </xsl:otherwise>
                </xsl:choose>
            </w:t>
        </w:r>
    </xsl:template>
    
    <xsl:template match="tei:num" mode="iden">
        <xsl:apply-templates/>
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
      <xsl:choose>
	<xsl:when test="parent::cals:entry or parent::tei:title">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
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
	</xsl:otherwise>
      </xsl:choose>
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
    
    
    <!-- Notes -->
    <xsl:template match="tei:note[@place]">
      <xsl:choose>
	<xsl:when test="@place='foot'  or @place='bottom' ">
	  <xsl:call-template name="create-footnote"/>
	</xsl:when>
	<xsl:when test="@place='end'">
	  <xsl:call-template name="create-endnote"/>
	</xsl:when>
	<xsl:when test="ancestor::tei:cell or ancestor::cals:entry">
	  <xsl:call-template name="create-inlinenote"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
    

    <xsl:template name="create-footnote">           
      <xsl:variable name="pPr">
	<xsl:choose>
	  <xsl:when test="(@place='foot'  or @place='bottom') and (parent::tei:cell or parent::cals:entry)">
	    <w:pPr>
	      <w:pStyle w:val="Table footnote"/>
	    </w:pPr>
	    <w:r>
	      <w:rPr>
		<w:rStyle w:val="TableFootnoteXref"/>
		<w:position w:val="6"/>
		<w:sz w:val="16"/>
	      </w:rPr>
	      <w:t>
		<xsl:value-of select="@n"/>
	      </w:t>
	    </w:r>
	    <w:r>
	      <w:t>
		<xsl:text> </xsl:text>
	      </w:t>
	    </w:r>
	  </xsl:when>
	  <xsl:when test="@type='Example'">
	    <w:pPr>
	      <w:pStyle w:val="Example"/>
	    </w:pPr>
	  </xsl:when>
	  <xsl:when test="parent::tei:cell or parent::cals:entry">	    
	    <w:pPr>
	      <xsl:variable name="Tablenote">
		<xsl:call-template name="getStyleName">
		  <xsl:with-param name="in">
		    <xsl:value-of select="$Note"/>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:variable>
	      <w:pStyle w:val="{$TableNote}"/>
	    </w:pPr>
	  </xsl:when>
	  <xsl:otherwise>
	    <w:pPr>
	      <w:pStyle w:val="Footnote"/>
	    </w:pPr>	    
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:variable>
      
      <xsl:choose>
	<xsl:when test="$pPr=''">
	  <xsl:variable name="num">
	    <xsl:number count="tei:note[@place='foot' or @place='bottom'][not(ancestor::cals:entry)]" level="any"/>
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
	</xsl:when>
	<xsl:otherwise>
	  <xsl:call-template name="block-element">
	    <xsl:with-param name="pPr" select="$pPr"/>
	    <xsl:with-param name="nop">false</xsl:with-param>
	  </xsl:call-template>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
      
    <xsl:template name="create-inlinenote">           
      <xsl:variable name="pPr">
	<w:pPr>
	  <w:pStyle w:val="{$TableNote}"/>
	</w:pPr>
      </xsl:variable>
      
      <xsl:call-template name="block-element">
	<xsl:with-param name="pPr" select="$pPr"/>
	<xsl:with-param name="nop">false</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
    
    
    <!-- Paragraphs in the front matter -->
    <xsl:template match="tei:front/tei:div/tei:p[@type='foreword']">
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
    
    <!-- Definition lists -->
    <xsl:template match="tei:list[@type='gloss']">
      <xsl:for-each select="tei:head">
        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr">
                <w:pPr>
                        <w:pStyle w:val="dl"/>
                        <w:tabs>
                            <w:tab w:val="left" w:pos="403"/>
                        </w:tabs>
                </w:pPr>
            </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
        
        
      <xsl:for-each select="tei:label">
        <w:p>
            <w:pPr>
                <w:pStyle w:val="dl"/>
                <w:tabs>
                    <w:tab w:val="left" w:pos="403"/>
                </w:tabs>
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
      </xsl:for-each>
    </xsl:template>
    
    <!-- Terms and Definitions -->
    <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:term">
        <w:p>
            <w:pPr>
                <w:pStyle>
                    <xsl:attribute name="w:val">
                        <xsl:call-template name="getStyleName">
                            <xsl:with-param name="in">
                                <xsl:text>TermNum</xsl:text>
                            </xsl:with-param>
                        </xsl:call-template>
                    </xsl:attribute>
                </w:pStyle>
            </w:pPr>
            <w:r>
                <w:t>
                    <xsl:value-of select="ancestor-or-self::*[@n][1]/@n"/>
                </w:t>
            </w:r>
        </w:p>
        
        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr">
                <w:pPr>
                    <w:pStyle>
                        <xsl:attribute name="w:val">
                            <xsl:call-template name="getStyleName">
                                <xsl:with-param name="in">
                                    <xsl:text>termPreferred</xsl:text>
                                </xsl:with-param>
                            </xsl:call-template>
                        </xsl:attribute>
                    </w:pStyle>
                </w:pPr>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="tei:list[@type='termlist' and ancestor-or-self::*/@type='termsAndDefinitions']/tei:item/tei:gloss">
        <xsl:call-template name="block-element">
            <xsl:with-param name="style">
                <xsl:variable name="style">
                    <xsl:call-template name="getStyleName">
                        <xsl:with-param name="in">
                            <xsl:text>GlossText</xsl:text>
                        </xsl:with-param>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:choose>
                    <xsl:when test="$style=''">
                        <xsl:text>Definition</xsl:text>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:value-of select="$style"/>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:template>
    
    <!-- titleStmt -->
    <xsl:template match="tei:titleStmt">
        <xsl:param name="language" as="xs:string"/>
        <xsl:param name="id_start" as="xs:integer"/>
        <xsl:for-each select="tei:title">
            <xsl:if test="@xml:lang=$language">
                <xsl:variable name="title_type"><xsl:value-of select="@type"/>_title</xsl:variable>
                <xsl:variable name="id">
                    <xsl:value-of select="position()+$id_start"/>
                </xsl:variable>
                <w:sdt>
                    <w:sdtPr>
                        <w:alias w:val="{$title_type}"/>
                        <w:tag w:val="{$title_type}"/>
                        <w:id w:val="{$id}"/>
                        <w:lock w:val="sdtLocked"/>
                        <w:placeholder>
                            <w:docPart w:val="0949DFCE5F58405499B2741B51A42BCD"/>
                        </w:placeholder>
                        <w:text/>
                    </w:sdtPr>
                    <w:sdtContent>
                        <w:r>
                            <w:t>
                                <xsl:value-of select="normalize-space(.)"/>
                            </w:t>
                        </w:r>
                    </w:sdtContent>
                </w:sdt>
                <xsl:choose>
                    <xsl:when test="position()=1 or position()=4">
                        <w:r>
                            <w:t xml:space="preserve"> </w:t>
                        </w:r>
                        <w:r>
                            <w:t xml:space="preserve">– </w:t>
                        </w:r>
                    </xsl:when>
                    <xsl:when test="position()=2 or position()=5">
                        <w:r>
                            <w:t xml:space="preserve"> </w:t>
                        </w:r>
                        <w:r>
                            <xsl:choose>
                                <xsl:when test="$language='en'">
                                    <w:t xml:space="preserve">– Part </w:t>
                                </xsl:when>
                                <xsl:otherwise>
                                    <w:t xml:space="preserve">– Partie </w:t>
                                </xsl:otherwise>
                            </xsl:choose>
                        </w:r>
                        <w:sdt>
                            <w:sdtPr>
                                <w:alias w:val="partNumber"/>
                                <w:tag w:val="partNumber"/>
                                <xsl:choose>
                                    <xsl:when test="$language='en'">
                                        <w:id w:val="680634476"/>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <w:id w:val="680634477"/>
                                    </xsl:otherwise>
                                </xsl:choose>
                                <w:lock w:val="sdtLocked"/>
                                <w:placeholder>
                                    <w:docPart w:val="0949DFCE5F58405499B2741B51A42BCD"/>
                                </w:placeholder>
                                <w:text/>
                            </w:sdtPr>
                            <w:sdtContent>
                                <w:r>
                                    <w:t>
                                        <xsl:value-of
                                            select="//tei:publicationStmt/tei:idno[@type='partNumber']"
                                        />
                                    </w:t>
                                </w:r>
                            </w:sdtContent>
                        </w:sdt>
                        <w:r>
                            <w:t xml:space="preserve">: </w:t>
                        </w:r>
                    </xsl:when>
                    <xsl:otherwise/>
                </xsl:choose>
            </xsl:if>
        </xsl:for-each>
    </xsl:template>

    <!-- document title -->
    <xsl:template name="document-title">
        <w:p>
            <w:pPr>
                <w:pStyle w:val="zzSTDTitle"/>
            </w:pPr>
            <w:r>
                <w:t>
                    <xsl:call-template name="generateTitle"/>
                </w:t>
            </w:r>
        </w:p>
    </xsl:template>
    
    
    <!-- 
        Table of Contents:
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
                <w:instrText xml:space="preserve"> TOC \o "1-6" \h \z \t "ANNEX;1;a2;2;a3;3;a4;4;a5;5;a6;6;zzForeword;1;Introduction;1;zzBiblio;1;zzIndex;1" </w:instrText>
            </w:r>
            <w:r>
                <w:fldChar w:fldCharType="separate"/>
            </w:r>
            <w:r>
                <w:fldChar w:fldCharType="end"/>
            </w:r>
        </w:p>
    </xsl:template>
    
    <!-- who created this document -->
    <xsl:template name="created-by">
        <xsl:value-of select="key('ISOMETA','secretariat')"/>
    </xsl:template>


  <xsl:template match="tei:availability" mode="titlepage">
  <xsl:param name="style"/>
  <xsl:for-each select="tei:p">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style" select="$style"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>



<!-- TBX -->

<xsl:template match="tbx:termEntry">
    <w:p>
      <w:pPr>
	<w:pStyle>
	  <xsl:attribute name="w:val">
	    <xsl:call-template name="getStyleName">
	      <xsl:with-param name="in">
		<xsl:text>TermNum</xsl:text>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:attribute>
	</w:pStyle>
      </w:pPr>
      <w:r>
	<w:t>
	  <xsl:value-of select="@id"/>
	</w:t>
      </w:r>
    </w:p>
  <xsl:for-each select="tbx:langSet/tbx:ntig">
    <w:p>
      <w:pPr>
	<w:pStyle>
	  <xsl:attribute name="w:val">
	    <xsl:call-template name="getStyleName">
	      <xsl:with-param name="in">
		<xsl:value-of select="substring-before(tbx:termGrp/tbx:termNote[@type='administrativeStatus'],'-adm-status')"/>
	      </xsl:with-param>
	    </xsl:call-template>
	  </xsl:attribute>
	</w:pStyle>
      </w:pPr>
      <w:r>
	<w:t>
	  <xsl:apply-templates select="tbx:termGrp/tbx:term"/>
	</w:t>
      </w:r>
    </w:p>
  </xsl:for-each>
  <w:p>
    <w:pPr>
      <w:pStyle>
	<xsl:attribute name="w:val">
	  <xsl:call-template name="getStyleName">
	    <xsl:with-param name="in">
	      <xsl:text>Definition</xsl:text>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:attribute>
      </w:pStyle>
    </w:pPr>
    <w:r>
      <w:t>
	<xsl:value-of select="tbx:descripGrp/tbx:descrip[@type='definition']"/>
      </w:t>
    </w:r>
  </w:p>
  <xsl:apply-templates select="tbx:descripGrp/tbx:note"/>

</xsl:template>


</xsl:stylesheet>

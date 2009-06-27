<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0" version="2.0" 
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:its="http://www.w3.org/2005/11/its"
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
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso tei a xs pic fn its">
    <!-- import conversion style -->
    <xsl:import href="../../../docx/tei-docx.xsl"/>
    <xsl:import href="../isoutils.xsl"/>
    
    <!-- import functions -->
    <xsl:include href="iso-functions.xsl"/>

    <xsl:param name="tableMethod">cals</xsl:param>    

    <xsl:param name="template">ISO</xsl:param>

    <xsl:variable name="align">
      <xsl:choose>
    	<xsl:when test="$template='ISO'">left</xsl:when>
	    <xsl:otherwise>right</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    
    <xsl:param name="word-directory">..</xsl:param>
    <xsl:param name="debug">false</xsl:param>
    <xsl:param name="styleDoc">
        <xsl:value-of select="concat($word-directory, '/word/styles.xml')"/>
    </xsl:param>
    
    <xsl:param name="numberFormat">fr</xsl:param>
    
    
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
    
    
    <!-- Notes -->
    <xsl:template match="tei:note">
        <xsl:param name="nop"></xsl:param>
           
        <xsl:variable name="pPr">
            <xsl:choose>
                <xsl:when test="(@place='foot'  or @place='bottom') and parent::tei:cell">
                    <w:pPr>
                        <w:pStyle w:val="TableFootnoteText"/>
                    </w:pPr>
                    <w:r>
                        <w:rPr>
                            <w:rStyle w:val="TableFootnoteReference"/>
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
                <xsl:when test="parent::tei:cell">
                    
                    <w:pPr>
                        <xsl:variable name="Tablenote">
                            <xsl:call-template name="getStyleName">
                                <xsl:with-param name="in">
                                    <xsl:value-of select="$Note"/>
                                </xsl:with-param>
                            </xsl:call-template>
                        </xsl:variable>
                        <w:pStyle w:val="{$Tablenote}"/>
                    </w:pPr>
                </xsl:when>
                <xsl:otherwise>
                    <w:pPr>
                        <w:pStyle w:val="Note"/>
                    </w:pPr>
                    
                </xsl:otherwise>
            </xsl:choose>
        </xsl:variable>
        
        <xsl:call-template name="block-element">
            <xsl:with-param name="pPr" select="$pPr"/>
            <xsl:with-param name="nop" select="$nop"/>
        </xsl:call-template>
    </xsl:template>
    
    <!-- 
        Special Notes (Footnotes) .. 
        @TODO: Ideally this should go into the general template, but for some
        reason xsl always calls the less specific tei:note template in here. 
    -->
    <xsl:template match="tei:note[@place]">
        <xsl:choose>
            <xsl:when test="@place='foot'  or @place='bottom' ">
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
                                    <xsl:text>Term(s)</xsl:text>
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
        <xsl:call-template name="getiso_authority"/>
    </xsl:template>


    

<!-- cover pages -->
<xsl:template name="titlepages">
  <xsl:variable name="committee">
    <xsl:call-template name="getiso_committee"/>
  </xsl:variable>
  <xsl:variable name="tc">
    <xsl:value-of select="substring-after(substring-before($committee,'/'),
			  ' ')"/>
  </xsl:variable>
  <xsl:variable name="sc">
    <xsl:value-of select="substring-after(substring-after($committee,'/'),
			  ' ')"/>
  </xsl:variable>

  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Reference number of working document: </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="workingreferencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:call-template name="getiso_publisher"/>
	<xsl:text>/TC </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="tcnum"/>
	<w:tag w:val="tcnum"/>
	<w:id w:val="680634442"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="workingreferencenumber"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:attribute name="xml:space">preserve</xsl:attribute>
	    <xsl:value-of select="$tc"/>
	  </w:t>	  
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="workingreferencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>/SC </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="scnum"/>
	<w:tag w:val="scnum"/>
	<w:id w:val="680634443"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="workingreferencenumber"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:attribute name="xml:space">preserve</xsl:attribute>
	    <xsl:value-of select="$sc"/>
	  </w:t>
	  
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="workingreferencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> N </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="workingreferencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="documentNumber"/>
	<w:tag w:val="documentNumber"/>
	<w:id w:val="680634444"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="workingreferencenumber"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="workingreferencenumber"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:attribute name="xml:space">preserve</xsl:attribute>
	    <xsl:call-template name="getiso_documentNumber"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="zzCover"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>Date: </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="date"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="docdate"/>
	<w:tag w:val="docdate"/>
	<w:id w:val="680634480"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="D55D639ECBC47B41AF3E6A91E06A6021"/>
	</w:placeholder>
	<w:date>
	  <w:dateFormat w:val="yyyy-MM-dd"/>
	  <w:lid w:val="en-US"/>
	  <w:storeMappedDataAs w:val="dateTime"/>
	  <w:calendar w:val="gregorian"/>
	</w:date>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="date"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_date"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>Reference number of document: </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="referencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>ISO/</xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="referencenumber"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>WD </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="referencenumber"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="serialNumber"/>
	<w:tag w:val="serialNumber"/>
	<w:id w:val="680634449"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="referencenumber"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="referencenumber"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:attribute name="xml:space">preserve</xsl:attribute>
	    <xsl:text> </xsl:text>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>Committee identification: </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="committeeid"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:call-template name="getiso_publisher"/>
	<xsl:text>/TC </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="tcnum"/>
	<w:tag w:val="tcnum"/>
	<w:id w:val="680634453"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="committeeid"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:value-of select="$tc"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="committeeid"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> /SC </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="scnum"/>
	<w:tag w:val="scnum"/>
	<w:id w:val="680634454"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="committeeid"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:value-of select="$sc"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:rStyle w:val="committeeid"/>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>/WG </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="wgNumber"/>
	<w:tag w:val="wgNumber"/>
	<w:id w:val="680634455"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="committeeid"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="committeeid"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_wgNumber"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:rStyle w:val="secretariat"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Secretariat: </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="secretariat"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="secretariat"/>
	<w:tag w:val="secretariat"/>
	<w:id w:val="680634456"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="secretariat"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="secretariat"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_authority"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:rStyle w:val="organization"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Organization:  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="organization"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="organization"/>
	<w:tag w:val="organization"/>
	<w:id w:val="72302846"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7AA4B62B517B5046B97F8F98C4AADF0D"/>
	</w:placeholder>
	<w:dropDownList>
	  <w:listItem w:displayText="ISO" w:value="ISO"/>
	  <w:listItem w:displayText="ISO/IEC" w:value="ISO/IEC"/>
	</w:dropDownList>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="organization"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_publisher"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="idno"/>
      <w:jc w:val="{$align}"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
  </w:p>
  
  
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documenttitle"/>
      <w:jc w:val="left"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:noProof/>
	  <w:color w:val="808080"/>
	  <w:sz w:val="22"/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="introductory_title"/>
	<w:tag w:val="introductory_title"/>
	<w:id w:val="680634464"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_introductory_en"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> –  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="main_title"/>
	<w:tag w:val="main_title"/>
	<w:id w:val="680634465"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_main_en"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> –  Part </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="partNumber"/>
	<w:tag w:val="partNumber"/>
	<w:id w:val="680634466"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_partNumber"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>: </xsl:text>
      </w:t>
    </w:r>
    
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="complementary_title"/>
	<w:tag w:val="complementary_title"/>
	<w:id w:val="1422197601"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_complementary_en">
	      <xsl:with-param name="withpart">false</xsl:with-param>
	    </xsl:call-template>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documenttitle"/>
      <w:jc w:val="left"/>
      <w:rPr>
	<w:i/>
	<w:noProof/>
	<w:sz w:val="24"/>
	<w:szCs w:val="24"/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:i/>
	  <w:noProof/>
	  <w:sz w:val="24"/>
	  <w:szCs w:val="24"/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="introductory_title_fr"/>
	<w:tag w:val="introductory_title_fr"/>
	<w:id w:val="32217405"/>
	<w:placeholder>
	  <w:docPart w:val="93A60E88754D3F47894219F2131A4DAA"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:i/>
	    <w:noProof/>
	    <w:sz w:val="24"/>
	    <w:szCs w:val="24"/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_introductory_fr"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:i/>
	<w:noProof/>
	<w:sz w:val="24"/>
	<w:szCs w:val="24"/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> –  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:i/>
	  <w:noProof/>
	  <w:sz w:val="24"/>
	  <w:szCs w:val="24"/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="main_title_fr"/>
	<w:tag w:val="main_title_fr"/>
	<w:id w:val="32217406"/>
	<w:placeholder>
	  <w:docPart w:val="93A60E88754D3F47894219F2131A4DAA"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:i/>
	    <w:noProof/>
	    <w:sz w:val="24"/>
	    <w:szCs w:val="24"/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_main_fr"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:i/>
	<w:noProof/>
	<w:sz w:val="24"/>
	<w:szCs w:val="24"/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> –  Partie </xsl:text>
      </w:t>
    </w:r>
    
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:i/>
	  <w:noProof/>
	  <w:sz w:val="24"/>
	  <w:szCs w:val="24"/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="partNumber"/>
	<w:tag w:val="partNumber"/>
	<w:id w:val="32217407"/>
	<w:placeholder>
	  <w:docPart w:val="93A60E88754D3F47894219F2131A4DAA"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:i/>
	    <w:noProof/>
	    <w:sz w:val="24"/>
	    <w:szCs w:val="24"/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_partNumber"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:i/>
	  <w:noProof/>
	  <w:sz w:val="24"/>
	  <w:szCs w:val="24"/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="complementary_title_fr"/>
	<w:tag w:val="complementary_title_fr"/>
	<w:id w:val="32217408"/>
	<w:placeholder>
	  <w:docPart w:val="93A60E88754D3F47894219F2131A4DAA"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr/>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:i/>
	    <w:noProof/>
	    <w:sz w:val="24"/>
	    <w:szCs w:val="24"/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_title_complementary_fr">
	      <xsl:with-param name="withpart">false</xsl:with-param>
	    </xsl:call-template>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  
  <xsl:call-template name="getiso_coverWarning"/>
  
  <w:p>
    <w:pPr>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
  </w:p>
  <w:p>
    <w:pPr>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documentdetails"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Document draft number:  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="draftnumber"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="draftNumber"/>
	<w:tag w:val="draftNumber"/>
	<w:id w:val="77933780"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="7315DFB3257B6741B0A1F2B19BC52BEF"/>
	</w:placeholder>
	<w:text/>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="draftnumber"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t> </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documentdetails"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Document type: </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="documenttype"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="doctype"/>
	<w:tag w:val="doctype"/>
	<w:id w:val="2485807"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="FE8938EB6F2C8048961AC56BE9778F3B"/>
	</w:placeholder>
	<w:dropDownList>
	  <w:listItem w:displayText="International Standard"
		      w:value="International Standard"/>
	  <w:listItem w:displayText="International Standardized Profile"
		      w:value="International Standardized Profile"/>
	  <w:listItem w:displayText="Technical Report"
		      w:value="Technical Report"/>
	  <w:listItem w:displayText="Publicly Available Specification"
		      w:value="Publicly Available Specification"/>
	  <w:listItem w:displayText="Technical Specification"
		      w:value="Technical Specification"/>
	  <w:listItem w:displayText="Guide" w:value="Guide"/>
	  <w:listItem w:displayText="International Workshop Agreement"
		      w:value="International Workshop Agreement"/>
	  <w:listItem w:displayText="Technology Trends Assessment"
		      w:value="Technology Trends Assessment"/>
	</w:dropDownList>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="documenttype"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_doctype"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documentdetails"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Document subtype: </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="documentsubtype"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="docsubtype"/>
	<w:tag w:val="docsubtype"/>
	<w:id w:val="2485813"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="FE8938EB6F2C8048961AC56BE9778F3B"/>
	</w:placeholder>
	<w:dropDownList>
	  <w:listItem w:displayText="Amendment" w:value="Amendment"/>
	  <w:listItem w:displayText="Technical Corrigendum"
		      w:value="Technical Corrigendum"/>
	  <w:listItem w:displayText=" " w:value=" "/>
	</w:dropDownList>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="documentsubtype"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_docsubtype"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documentdetails"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Document stage:  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="documentstage"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="docstage"/>
	<w:tag w:val="docstage"/>
	<w:id w:val="2485817"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="FE8938EB6F2C8048961AC56BE9778F3B"/>
	</w:placeholder>
	<w:dropDownList>
	  <w:listItem w:displayText="(00) Preliminary" w:value="00"/>
	  <w:listItem w:displayText="(20) Preparation" w:value="20"/>
	  <w:listItem w:displayText="(30) Committee" w:value="30"/>
	  <w:listItem w:displayText="(40) Enquiry" w:value="40"/>
	  <w:listItem w:displayText="(50) Approval" w:value="50"/>
	  <w:listItem w:displayText="(60) Publication" w:value="60"/>
	</w:dropDownList>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="documentstage"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:call-template name="getiso_stage"/>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
  </w:p>
  <w:p>
    <w:pPr>
      <w:pStyle w:val="documentdetails"/>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:t>
	<xsl:attribute name="xml:space">preserve</xsl:attribute>
	<xsl:text>Document language:  </xsl:text>
      </w:t>
    </w:r>
    <w:sdt>
      <w:sdtPr>
	<w:rPr>
	  <w:rStyle w:val="documentlanguage"/>
	  <w:noProof/>
	  <w:lang w:val="en-GB"/>
	</w:rPr>
	<w:alias w:val="doclanguage"/>
	<w:tag w:val="doclanguage"/>
	<w:id w:val="2485820"/>
	<w:lock w:val="sdtLocked"/>
	<w:placeholder>
	  <w:docPart w:val="FE8938EB6F2C8048961AC56BE9778F3B"/>
	</w:placeholder>
	<w:dropDownList>
	  <w:listItem w:displayText="English" w:value="EN"/>
	  <w:listItem w:displayText="Français" w:value="FR"/>
	</w:dropDownList>
      </w:sdtPr>
      <w:sdtEndPr>
	<w:rPr>
	  <w:rStyle w:val="DefaultParagraphFont"/>
	  <w:color w:val="1F497D" w:themeColor="text2"/>
	  <w:sz w:val="24"/>
	</w:rPr>
      </w:sdtEndPr>
      <w:sdtContent>
	<w:r>
	  <w:rPr>
	    <w:rStyle w:val="documentlanguage"/>
	    <w:noProof/>
	    <w:lang w:val="en-GB"/>
	  </w:rPr>
	  <w:t>
	    <xsl:choose>
	      <xsl:when test="ancestor-or-self::tei:TEI/@xml:lang='ru'"
			>Russian</xsl:when>
	      <xsl:when test="ancestor-or-self::tei:TEI/@xml:lang='fr'"
			>French</xsl:when>
	      <xsl:otherwise>English</xsl:otherwise>
	    </xsl:choose>
	  </w:t>
	</w:r>
      </w:sdtContent>
    </w:sdt>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:br w:type="page"/>
    </w:r>
  </w:p>
  
  <xsl:call-template name="getiso_copyright"/>
  
  <w:p>
    <w:pPr>
      <w:ind w:left="1800" w:hanging="360"/>
      <w:rPr>
	<w:b/>
	<w:noProof/>
	<w:color w:val="1F497D" w:themeColor="text2"/>
	<w:sz w:val="24"/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
    </w:pPr>
    <w:r>
      <w:rPr>
	<w:noProof/>
	<w:lang w:val="en-GB"/>
      </w:rPr>
      <w:br w:type="page"/>
    </w:r>
  </w:p>
</xsl:template>

<xsl:template match="tei:availability" mode="titlepage">
  <xsl:param name="style"/>
  <xsl:for-each select="tei:p">
    <xsl:call-template name="block-element">
      <xsl:with-param name="style" select="$style"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:edate="http://exslt.org/dates-and-times" xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:iso="http://www.iso.org/ns/1.0" 
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
    xmlns:o="urn:schemas-microsoft-com:office:office"
    xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
    xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
    xmlns:v="urn:schemas-microsoft-com:vml"
    xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
    xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
    xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
    xmlns:w10="urn:schemas-microsoft-com:office:word"
    xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
    xmlns:mml="http://www.w3.org/1998/Math/MathML"
    xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
    exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx pic rel a tei teidocx iso edate">
    
    <!-- import base conversion style -->

    <xsl:import href="../../../docx/from/from.xsl"/>
    
	<xsl:template match="w:body">
		<text> <!--
		     look for headings of various kinds, from which to
		     generate sections
			-->
			<body>
			  <msDesc>
			  <xsl:choose>
			    <xsl:when test="w:p[w:pPr/w:pStyle/@w:val='heading 1']">
			      <xsl:for-each-group select="w:p|w:tbl"
					group-starting-with="w:p[teidocx:is-firstlevel-heading(.)]">
					<xsl:choose>
						<xsl:when test="teidocx:is-heading(.)">
							<xsl:call-template name="group-headings"/>		
						</xsl:when>
						<xsl:otherwise>
							<xsl:apply-templates select="." mode="headings"/>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each-group>
			    </xsl:when>
			    <xsl:otherwise>
			      <xsl:apply-templates select="w:p|w:tbl" mode="paragraph"/>
			    </xsl:otherwise>
			  </xsl:choose>
			  </msDesc>
			</body>
		</text>
	</xsl:template>

    <!-- some specific section headers -->
    <xsl:template name="group-headings">
        <xsl:variable name="Style" select="w:pPr/w:pStyle/@w:val"/>
        <xsl:variable name="NextHeader"
		      select="teidocx:get-nextlevel-header($Style)"/>
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

    <xsl:template match="tei:div[tei:head[string-length(.)=0] and count(*)=1]"
		  mode="part2"/>

    <xsl:template match="tei:div[tei:head='Decoration']"
		  mode="part2">
      <decoDesc>
	<xsl:apply-templates mode="part2"/>
      </decoDesc>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Binding']"
		  mode="part2">
      <bindingDesc>
	<xsl:apply-templates mode="part2"/>
      </bindingDesc>
    </xsl:template>

    <xsl:template match="tei:div[tei:head='Physical Description']"
		  mode="part2">
      <physDesc>
	<xsl:apply-templates mode="part2"/>
      </physDesc>
    </xsl:template>

    <xsl:template match="tei:head[.='Decoration']" mode="part2"/>
    <xsl:template match="tei:head[.='Binding']" mode="part2"/>
    <xsl:template match="tei:head[.='Physical Description']"
		  mode="part2"/>

    <xsl:template match="@rend[.='Body Text']" mode="part2"/>
    <xsl:template match="@rend[.='Body Text Indent']" mode="part2"/>

    <xsl:template match="tei:hi/@rend[.='superscript']" mode="part2">
      <xsl:attribute name="rend">
	<xsl:text>sup</xsl:text>
      </xsl:attribute>
    </xsl:template>

    <xsl:template match="tei:publicationStmt[.='']" mode="part2">
      <publicationStmt>
	<p>Unpublished </p>
      </publicationStmt>
    </xsl:template>

    <xsl:template match="tei:c[@rend='tab']" mode="part2">
      <xsl:text> </xsl:text>
    </xsl:template>

    <xsl:template match="tei:c[@iso:font='Symbol']" mode="part2">
      <xsl:choose>
	<xsl:when test="@n='F0B4'">×</xsl:when>
	<xsl:when test="@n='F05B'">[</xsl:when>
	<xsl:when test="@n='F05D'">]</xsl:when>
	<xsl:otherwise>
	  <xsl:message>Panic: character <xsl:value-of select="@n"/> unknown</xsl:message>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:template>
</xsl:stylesheet>

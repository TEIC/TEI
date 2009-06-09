<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:edate="http://exslt.org/dates-and-times" xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:iso="http://www.iso.org/ns/1.0"
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
    exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx iso edate">
    
    <!-- import base conversion style -->

    <xsl:import href="../../../docx/docx-tei.xsl"/>
    
	<xsl:template match="w:body">
		<text>
		  <xsl:call-template name="extract-headers-and-footers"/>
		  
		<!-- 
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
			  <xsl:apply-templates select="w:sectPr"
					       mode="paragraph"/>
			  </msDesc>
			</body>
		</text>
	</xsl:template>

</xsl:stylesheet>

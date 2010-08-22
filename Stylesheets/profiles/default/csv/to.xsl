<?xml version="1.0" encoding="UTF-8"?>
	<!-- 
		Usage:  saxon filename.xsl tei2csv.xsl 
		Notes:
		1) This produces files named tableX.csv
		where X is the number of the table in the 
		document as a whole.
		2) All other content is discarded
		3) All output is in UTF-8, input-encoding is 
		based on XML declaration in the original file.
		
		-James Cummings 
		2010-08-20
		
	-->

<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:tei="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="xs tei">
	<xsl:output omit-xml-declaration="yes"/>
	<xsl:template match="/">
		<xsl:for-each select="//tei:table">
			<xsl:variable name="tableNum">
				<xsl:number from="/"/>
			</xsl:variable>
			<xsl:result-document href="{concat('table',$tableNum, '.csv')}"
				encoding="UTF-8" method="text">
				<xsl:for-each select="tei:row">
					<xsl:for-each select="tei:cell">
						<xsl:value-of select="concat('&quot;',.,'&quot;')"/>
						<xsl:if test="following-sibling::tei:cell[1]">, </xsl:if>
					</xsl:for-each>
					<xsl:text>&#xa;</xsl:text>
				</xsl:for-each>
			</xsl:result-document>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>

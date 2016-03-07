<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
     exclude-result-prefixes="rng"
      version="2.0">
    
    <xsl:output 
        method="xml"
        encoding="utf-8"
        indent="yes"
        omit-xml-declaration="yes"/>
    
    <xsl:template match="tei:macroSpec[@type='dt']">
    
        <xsl:result-document href="{concat('tei',@ident,'.xml')}">

<xsl:comment>
Copyright TEI Consortium. 
Dual-licensed under CC-by and BSD2 licences 
See the file COPYING.txt for details
</xsl:comment><xsl:text>
</xsl:text>
<xsl:processing-instruction name="xml-model">
 href="http://tei.it.ox.ac.uk/jenkins/job/TEIP5/lastSuccessfulBuild/artifact/release/xml/tei/odd/p5.nvdl" type="application/xml" schematypens="http://purl.oclc.org/dsdl/nvdl/ns/structure/1.0"
</xsl:processing-instruction>
        <dataSpec  xmlns="http://www.tei-c.org/ns/1.0">
          <xsl:attribute name="module">
              <xsl:value-of select="@module"/>
          </xsl:attribute>
          <xsl:attribute name="ident">
              <xsl:value-of select="concat('tei',@ident)"/>
          </xsl:attribute>         
            <xsl:apply-templates />
      </dataSpec>
        </xsl:result-document>
        
        <!-- output an attdef using this datatype -->
        
        <attDef ident="{concat('A_', substring-after(@ident,'data.'))}">
            <datatype><ref xmlns="http://relaxng.org/ns/structure/1.0" name="{@ident}"/></datatype>
        </attDef>
  
    </xsl:template>
    
    <xsl:template match="tei:content">
<xsl:copy>
        <xsl:choose>
            <xsl:when test="rng:choice">
                <alternate>
                    <xsl:for-each select="rng:choice/rng:data">
                        <dataRef name="{@type}">
                            <xsl:if test="rng:param/@name='pattern'">
                                <xsl:attribute name="restriction">
                                    <xsl:value-of select="rng:param"/>
                                </xsl:attribute>
                            </xsl:if>
                        </dataRef>
                    </xsl:for-each>
                    <xsl:if test="rng:choice/rng:value">
                        <valList>
                            <xsl:for-each select="rng:choice/rng:value">
                                <valItem ident="{.}"/>
                            </xsl:for-each>
                        </valList>
                    </xsl:if>
                </alternate>
            </xsl:when>
            <xsl:when test="rng:data/rng:param/@name='pattern'">
                <dataRef name="{rng:data/@type}"
                    restriction="{rng:data/rng:param}"/>
            </xsl:when>
            <xsl:when test="rng:data">
                <dataRef name="{rng:data/@type}"/>
            </xsl:when>
            <xsl:when test="rng:ref">
                <dataRef key="{concat('tei',rng:ref/@name)}"/>
            </xsl:when>
            <xsl:otherwise>
                <textNode/>
            </xsl:otherwise>
        </xsl:choose>
</xsl:copy>
    </xsl:template>
  
    <!-- copy everything else -->
    
    <xsl:template match="*">
        <xsl:copy>
            <xsl:apply-templates select="@*"/>
            <xsl:apply-templates 
                select="*|comment()|processing-instruction()|text()"/>
        </xsl:copy>
    </xsl:template>
    
    <xsl:template match="@*|processing-instruction()|text()">
        <xsl:copy/>
    </xsl:template>
    
    <xsl:template match="comment()">
        <xsl:copy/>
    </xsl:template>
</xsl:stylesheet>
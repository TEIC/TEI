<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    exclude-result-prefixes="tei rng"
    version="2.0">
<!--<xsl:variable name="elements">
        <xsl:value-of select='document("out/tei-simple-new.rng")//rng:element/@name'/>
    </xsl:variable>-->
    
    <xsl:variable name="context">
        <xsl:value-of select="/"/>
    </xsl:variable>
  
    <xsl:key name="specced"  match="tei:specDesc" use="@key"/>
    <xsl:key name="modelled" match="tei:elementSpec[tei:model]" use="@ident"/>   
    <xsl:key name="reffed" match="tei:elementRef" use="@key"/>
    
     <xsl:template match="/">
        <table>
            <row role="label"><cell>Reffed?</cell>
                <cell>Specced?</cell><cell>Modelled?</cell><cell>Element</cell></row>
       <xsl:for-each select="//tei:elementRef">
           <xsl:sort select="@key"/>
            <xsl:variable name="theKey">
                <xsl:value-of select="@key"/>
            </xsl:variable>
            <row><cell>
               <xsl:choose> <xsl:when test="key('specced', $theKey)">YES</xsl:when>
                <xsl:otherwise>NO</xsl:otherwise></xsl:choose>
            </cell>
               <cell>
                   <xsl:choose> <xsl:when test="key('modelled', $theKey)">YES</xsl:when>
                       <xsl:otherwise>NO</xsl:otherwise></xsl:choose>
               </cell>
                <cell> <xsl:value-of select="@key"/></cell></row><xsl:text>
</xsl:text>
     </xsl:for-each>
        </table>
    </xsl:template>
</xsl:stylesheet>
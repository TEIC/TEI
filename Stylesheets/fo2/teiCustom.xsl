<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fotex="http://www.tug.org/fotex"
                xmlns="http://www.w3.org/1999/XSL/Format"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                version="1.0">
    <xsl:import href="tei.xsl"/>
    
    <!-- oXygen begin change -->
    <!--
        The default value of the 'tableAlign' param set in common/ tei-param.xsl is 
        'leftTable' which is not a standard XSL-FO value for this param. It is reported 
        as error by both Apache FOP and XEP. We set the default value to a 
        standard one.
    -->
    <xsl:param name="tableAlign">left</xsl:param>
    <!-- oXygen end change -->
    
    <xsl:template match="tei:table">
        <xsl:choose>
            <xsl:when test="@rend='eqnarray' and $foEngine='passivetex'">
                <fotex:eqnarray>
                    <xsl:apply-templates select=".//tei:formula"/>
                </fotex:eqnarray>
            </xsl:when>
            <xsl:when test=".//tei:formula[@type='subeqn'] and $foEngine='passivetex'">
                <fotex:eqnarray>
                    <xsl:apply-templates select=".//tei:formula"/>
                </fotex:eqnarray>
            </xsl:when>
            <xsl:when test="$inlineTables or @rend='inline'">
                <xsl:if test="tei:head">
                    <block>
                        <xsl:call-template name="tableCaptionstyle"/>
                        <!-- oXygen begin change -->
                        <!-- 
                            The same ID attribute value generated from the same node of the XML
                            source cannot be added to two different elements of the XSL-FO result,
                            that is an fo:block and a child element fo:table. Here we skip adding the ID
                            attribute for the child element.
                        -->
                        <!-- <xsl:call-template name="addID"/> -->
                        <!-- oXygen end change -->
                        <xsl:if test="$makeTableCaption='true'">
                            <xsl:call-template name="calculateTableNumber"/>
                            <xsl:text>. </xsl:text>
                        </xsl:if>
                        <xsl:apply-templates select="tei:head"/>
                    </block>
                </xsl:if>
                <xsl:call-template name="blockTable"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="floatTable"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
</xsl:stylesheet>
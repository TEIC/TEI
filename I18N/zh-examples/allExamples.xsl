<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:ex="http://www.tei-c.org/ns/Examples"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">
    <xsl:template match="/">
        <html>
            <head>
                <title>all examples now right here</title>
            </head>
            <body>
                <xsl:apply-templates/>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="elementSpec">
        <xsl:value-of select="@ident"/>
        <br/>
        <table border="1">
            <xsl:for-each select="ex:egXML[not(@xml:lang)]">
                <tr>
                <td style="background:lavender; width:45%;">
                    <xsl:apply-templates select=".[not(@xml:lang)]"/>
                </td>
                    <td style="background:beige; width:45%;">
                    <xsl:apply-templates select="following-sibling::ex:egXML[@xml:lang][1]"/>
                </td>
                </tr>
            </xsl:for-each>
        </table>
    </xsl:template>
   
  
    <xsl:template match="ex:egXML[not(@xml:lang)]">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="ex:egXML[not(@xml:lang)]/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/> 
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>        
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment as in interpGrp-->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[not(@xml:lang)]/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/>
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[not(@xml:lang)]/*/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[not(@xml:lang)]/*/*/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <!-- from here on the Chinese egXML -->
    <xsl:template match="ex:egXML[@xml:lang]">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="ex:egXML[@xml:lang]/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/> 
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>        
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment as in interpGrp-->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[@xml:lang]/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/>
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[@xml:lang]/*/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    <xsl:template match="ex:egXML[@xml:lang]/*/*/*/*">
        <xsl:if test="preceding-sibling::*"><br/></xsl:if>&lt;<xsl:value-of select="name()"/>
        <xsl:for-each select="attribute::*">
            <xsl:text> </xsl:text><xsl:value-of select="name()"/><xsl:text>="</xsl:text>
            <xsl:value-of select="."/><xsl:text>"</xsl:text>
        </xsl:for-each>
        <xsl:choose> 
            <xsl:when test="text()">&gt;</xsl:when>
            <xsl:otherwise></xsl:otherwise>
        </xsl:choose>
        <xsl:apply-templates/> 
        <!-- This in case the content is a comment -->
        <xsl:if test="comment()">
            <xsl:text>&lt;!-- </xsl:text><xsl:value-of select="comment()"/><xsl:text>--></xsl:text>
        </xsl:if>
        <xsl:choose> 
            <xsl:when test="text()">&lt;/<xsl:value-of select="name()"/>&gt;</xsl:when>
            <xsl:otherwise>/&gt;</xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
</xsl:stylesheet>

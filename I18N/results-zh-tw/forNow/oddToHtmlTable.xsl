<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xpath-default-namespace="http://www.tei-c.org/ns/1.0" version="2.0">

  <xsl:template match="/">
      <html>
          <head><title>some html view of an odd file</title></head>
          <body>
              <xsl:apply-templates/>
          </body>
      </html>
  </xsl:template>
    
    <xsl:template match="moduleSpec">
        <table border="1">
            <xsl:for-each select="elementSpec">
                <xsl:sort select="@ident"/>
                <tr>
                    <td><table><tr>
                    <td>
                        <b><xsl:text>&lt;</xsl:text>
                        <xsl:value-of select="@ident"/>
                        <xsl:text>&gt;</xsl:text></b>
                    </td>
                        <td></td>
                    </tr>
                    <tr> 
                            <td> <xsl:value-of select="gloss[not(@xml:lang)]"/></td>
                            <td> <xsl:value-of select="gloss[@xml:lang='zh-tw']"/></td>
                    </tr>
                        <tr> 
                            <td> <xsl:value-of select="desc[not(@xml:lang)]"/></td>
                            <td> <xsl:value-of select="desc[@xml:lang='zh-tw']"/></td>
                        </tr>
                        <xsl:if test="attList">
                            <tr>
                                <td colspan="2"><table border="1">
                                    <thead><b>Attributes:</b></thead>
                                    <xsl:for-each select="attList/attDef">
                                        <tr><td><xsl:value-of select="./@ident"/><br/>
                                                      <xsl:value-of select="desc[not(@xml:lang)]"/>
                                            <xsl:if test="valDesc[not(@xml:lang)]">
                                                <br/>(<xsl:value-of select="valDesc[not(@xml:lang)]"/>)
                                            </xsl:if>
                                            <xsl:if test="valList">
                                                <br/><span style="color:grey">value list:</span>
                                                <xsl:for-each select="valList/valItem">
                                                <br/><xsl:value-of select="./@ident"/>
                                                    <br/><xsl:value-of select="gloss[not(@xml:lang)] | desc[not(@xml:lang)]"/>
                                                </xsl:for-each>
                                            </xsl:if>
                                </td>
                                <td><xsl:value-of select="./@ident"/><br/>
                                    <xsl:value-of select="desc[@xml:lang='zh-tw']"/>
                                    <xsl:if test="valDesc[@xml:lang='zh-tw']">
                                        <br/>(<xsl:value-of select="valDesc[@xml:lang='zh-tw']"/>)
                                    </xsl:if>
                                    <xsl:if test="valList">
                                        <br/><span style="color:grey">value list:</span>
                                        <xsl:for-each select="valList/valItem">
                                            <br/><xsl:value-of select="./@ident"/>
                                            <br/><xsl:value-of select="gloss[@xml:lang='zh-tw'] | desc[@xml:lang='zh-tw']"/>
                                        </xsl:for-each>
                                    </xsl:if>
                                </td>
                                        </tr>
                                        
                                    </xsl:for-each>
                                </table>
                                </td>
                            </tr>
                         </xsl:if>
                </table></td></tr>
            </xsl:for-each>
        </table>
    </xsl:template>
    
   
    
</xsl:stylesheet>
